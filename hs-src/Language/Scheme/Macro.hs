{- |
Module      : Language.Scheme.Macro
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module contains code for hygienic macros.

During transformation, the following components are considered:
 - Pattern (part of a rule that matches input)
 - Transform (what the macro "expands" into)
 - Input (the actual code in the user's program)

At a high level, macro transformation is broken down into the following steps:

 1) Search for a rule that matches the input.
    During this process, any variables in the input are loaded into a temporary environment
 2) If a rule matches,
 3) Transform by walking the transform, inserting variables as needed

Remaining Work:

* Dotted lists are not 100% correctly implemented. In particular, the transformation should
  take into account whether the input was presented as a list or a pair, and replicate that
   in the output.

-}

module Language.Scheme.Macro
    (
      macroEval
    ) where
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error
import Data.Array
import Debug.Trace -- Only req'd to support trace, can be disabled at any time...

{- Nice FAQ regarding macro's, points out some of the limitations of current implementation
http://community.schemewiki.org/?scheme-faq-macros -}

{- |macroEval
Search for macro's in the AST, and transform any that are found.
There is also a special case (define-syntax) that loads new rules. -}
macroEval :: Env -> LispVal -> IOThrowsError LispVal

-- Special case, just load up the syntax rules
macroEval env (List [Atom "define-syntax", Atom keyword, syntaxRules@(List (Atom "syntax-rules" : (List _ : _)))]) = do
  {-
   - FUTURE: Issue #15: there really ought to be some error checking of the syntax rules, 
   -                    since they could be malformed...
  - As it stands now, there is no checking until the code attempts to perform a macro transformation.
  - At a minimum, should check identifiers to make sure each is an atom (see findAtom) 
  -}
  _ <- defineNamespacedVar env macroNamespace keyword syntaxRules
  return $ Nil "" -- Sentinal value

-- Inspect a list of code, and transform as necessary
macroEval env (List (x@(List _) : xs)) = do
  first <- macroEval env x
  rest <- mapM (macroEval env) xs
  return $ List $ first : rest

{- Inspect code for macros
 -
 - Only a list form is required because a pattern may only consist
 - of a list here. From the spec:
 -
 - "The <pattern> in a <syntax rule> is a list <pattern> that
begins with the keyword for the macro." 
 -
 -}
macroEval env lisp@(List (Atom x : xs)) = do
  isDefined <- liftIO $ isNamespacedBound env macroNamespace x
  if isDefined
     then do
       (List (Atom "syntax-rules" : (List identifiers : rules))) <- getNamespacedVar env macroNamespace x
       -- Transform the input and then call macroEval again, since a macro may be contained within...
       macroEval env =<< macroTransform env (List identifiers) rules lisp
     else do
       rest <- mapM (macroEval env) xs
       return $ List $ (Atom x) : rest

-- No macro to process, just return code as it is...
macroEval _ lisp@(_) = return lisp

{-
 - Given input and syntax-rules, determine if any rule is a match and transform it.
 -
 - FUTURE: validate that the pattern's template and pattern are consistent 
 - (IE: no vars in transform that do not appear in matching pattern - csi "stmt1" case)
 -
 - Parameters:
 -  env - Higher level LISP environment
 -  identifiers - Literal identifiers - IE, atoms that should not be transformed
 -  rules - pattern/transform pairs to compare to input
 -  input - Code from the scheme application 
 -}
macroTransform :: Env -> LispVal -> [LispVal] -> LispVal -> IOThrowsError LispVal
macroTransform env identifiers (rule@(List _) : rs) input = do
  localEnv <- liftIO $ nullEnv -- Local environment used just for this invocation
  result <- matchRule env identifiers localEnv rule input
  case result of
    Nil _ -> macroTransform env identifiers rs input
    _ -> return result
-- Ran out of rules to match...
macroTransform _ _ _ input = throwError $ BadSpecialForm "Input does not match a macro pattern" input

-- Determine if the next element in a list matches 0-to-n times due to an ellipsis
macroElementMatchesMany :: LispVal -> Bool
macroElementMatchesMany (List (_ : ps)) = do
  if not (null ps)
     then case (head ps) of
                Atom "..." -> True
                _ -> False
     else False
macroElementMatchesMany _ = False

{- Given input, determine if that input matches any rules
@return Transformed code, or Nil if no rules match -}
matchRule :: Env -> LispVal -> Env -> LispVal -> LispVal -> IOThrowsError LispVal
matchRule outerEnv identifiers localEnv (List [pattern, template]) (List inputVar) = do
   let is = tail inputVar
   let p = case pattern of
              DottedList ds d -> case ds of
                                  (Atom l : ls) -> List [Atom l, DottedList ls d]
                                  _ -> pattern
              _ -> pattern
   case p of
      List (Atom _ : ps) -> do
        match <- loadLocal outerEnv localEnv identifiers (List ps) (List is) False False
        case match of
           Bool False -> return $ Nil ""
           _ -> transformRule localEnv 0 (List []) template (List [])
      _ -> throwError $ BadSpecialForm "Malformed rule in syntax-rules" p

matchRule _ _ _ rule input = do
  throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ List [Atom "rule: ", rule, Atom "input: ", input]

{- loadLocal - Determine if pattern matches input, loading input into pattern variables as we go,
in preparation for macro transformation. -}
loadLocal :: Env -> Env -> LispVal -> LispVal -> LispVal -> Bool -> Bool -> IOThrowsError LispVal
loadLocal outerEnv localEnv identifiers pattern input hasEllipsis outerHasEllipsis = do
  case (pattern, input) of

       {- For vectors, just use list match for now, since vector input matching just requires a
       subset of that behavior. Should be OK since parser would catch problems with trying
       to add pair syntax to a vector declaration. -}
       ((Vector p), (Vector i)) -> do
         loadLocal outerEnv localEnv identifiers (List $ elems p) (List $ elems i) False outerHasEllipsis

       ((DottedList ps p), (DottedList is i)) -> do
         result <- loadLocal outerEnv localEnv identifiers (List ps) (List is) False outerHasEllipsis
         case result of
            Bool True -> loadLocal outerEnv localEnv identifiers p i False outerHasEllipsis
            _ -> return $ Bool False

       (List (p : ps), List (i : is)) -> do -- check first input against first pattern, recurse...

         let localHasEllipsis = macroElementMatchesMany pattern

         {- FUTURE: error if ... detected when there is an outer ... ????
         no, this should (eventually) be allowed. See scheme-faq-macros -}

         status <- checkLocal outerEnv localEnv identifiers (localHasEllipsis || outerHasEllipsis) p i
         case status of
              -- No match
              Bool False -> if localHasEllipsis
                                {- No match, must be finished with ...
                                Move past it, but keep the same input. -}
                                then do
                                        loadLocal outerEnv localEnv identifiers (List $ tail ps) (List (i : is)) False outerHasEllipsis
                                else return $ Bool False
              -- There was a match
              _ -> if localHasEllipsis
                      then loadLocal outerEnv localEnv identifiers pattern (List is) True outerHasEllipsis
                      else loadLocal outerEnv localEnv identifiers (List ps) (List is) False outerHasEllipsis

       -- Base case - All data processed
       (List [], List []) -> return $ Bool True

       -- Ran out of input to process
       (List (_ : ps), List []) -> do
                                 {- Ensure any patterns that are not present in the input still
                                 have their variables initialized so they are ready during trans. -}
                                 _ <- initializePatternVars localEnv "list" identifiers pattern
                                 if (macroElementMatchesMany pattern) && ((length ps) == 1)
                                           then return $ Bool True
                                           else return $ Bool False

       -- Pattern ran out, but there is still input. No match.
       (List [], _) -> return $ Bool False

       -- Check input against pattern (both should be single var)
       (_, _) -> checkLocal outerEnv localEnv identifiers (hasEllipsis || outerHasEllipsis) pattern input

{- Check pattern against input to determine if there is a match
 -
 - @param localEnv - Local variables for the macro, used during transform
 - @param hasEllipsis - Determine whether we are in a zero-or-many match.
 -                      Used for loading local vars and NOT for purposes of matching.
 - @param pattern - Pattern to match
 - @param input - Input to be matched
 -}
checkLocal :: Env -> Env -> LispVal -> Bool -> LispVal -> LispVal -> IOThrowsError LispVal
checkLocal _ _ _ _ (Bool pattern) (Bool input) = return $ Bool $ pattern == input
checkLocal _ _ _ _ (Number pattern) (Number input) = return $ Bool $ pattern == input
checkLocal _ _ _ _ (Float pattern) (Float input) = return $ Bool $ pattern == input
checkLocal _ _ _ _ (String pattern) (String input) = return $ Bool $ pattern == input
checkLocal _ _ _ _ (Char pattern) (Char input) = return $ Bool $ pattern == input
checkLocal outerEnv localEnv identifiers hasEllipsis (Atom pattern) input = do
  if hasEllipsis
     {- FUTURE: may be able to simplify both cases below by using a
     lambda function to store the 'save' actions -}

             -- Var is part of a 0-to-many match, store up in a list...
     then do isDefined <- liftIO $ isBound localEnv pattern
             isLexicallyDefinedVar <- liftIO $ isBound outerEnv pattern
             --
             -- If pattern is a literal identifier, need to ensure
             -- input matches that literal, or that (in this case)
             -- the literal is missing from the input (0 match)
             --
             isIdent <- findAtom (Atom pattern) identifiers
             case isIdent of
                Bool True -> do
                    case input of
                        Atom inpt -> do
                            if (pattern == inpt)  
                               then if (True) --isLexicallyDefinedVar == False 
                                       -- Var is not bound in outer code; proceed
                                       then do
                                         -- Set variable in the local environment
                                         _ <- addPatternVar isDefined $ Atom pattern
                                         return $ Bool True
                                       -- Var already bound in enclosing environment prior to evaluating macro.
                                       -- So... do not match it here.
                                       --
                                       -- See section 4.3.2 of R5RS, in particular:
                                       -- " If a literal identifier is inserted as a bound identifier then it is 
                                       --   in effect renamed to prevent inadvertent captures of free identifiers "
                                       else return $ Bool False
                               else return $ Bool False
                        -- Pattern/Input cannot match because input is not an atom
                        _ -> return $ Bool False
                -- No literal identifier, just load up the var
                _ -> do _ <- addPatternVar isDefined input
                        return $ Bool True
     --
     -- Simple var, try to load up into macro env
     --
     else do
         isIdent <- findAtom (Atom pattern) identifiers
         isLexicallyDefinedPatternVar <- liftIO $ isBound outerEnv pattern -- Var defined in scope outside macro
         case (isIdent) of
            -- Fail the match if pattern is a literal identifier and input does not match
            Bool True -> do
                case input of
                    Atom inpt -> do
                        -- Pattern/Input are atoms; both must match
-- 
-- TODO: this is broken because we call macroEval only *once* before evaluating a tree of expressions.
-- macroEval needs to be called each time prior to eval
-- need to consider the performance implications of this...
--
                        if (pattern == inpt && (not isLexicallyDefinedPatternVar)) -- TODO: Need to test this after macro code is reorganized
                           then do _ <- defineVar localEnv pattern input
                                   return $ Bool True
                           else return $ (Bool False)
                    -- Pattern/Input cannot match because input is not an atom
                    _ -> return $ (Bool False)

            -- No literal identifier, just load up the var
            _ -> do _ <- defineVar localEnv pattern input
                    return $ Bool True
    where
      addPatternVar isDefined val = do
             if isDefined
                then do v <- getVar localEnv pattern
                        case v of
                          (List vs) -> setVar localEnv pattern (List $ vs ++ [val])
                          _ -> throwError $ Default "Unexpected error in checkLocal (Atom)"
                else defineVar localEnv pattern (List [val])

checkLocal outerEnv localEnv identifiers hasEllipsis pattern@(Vector _) input@(Vector _) =
  loadLocal outerEnv localEnv identifiers pattern input False hasEllipsis

checkLocal outerEnv localEnv identifiers hasEllipsis pattern@(DottedList _ _) input@(DottedList _ _) =
  loadLocal outerEnv localEnv identifiers pattern input False hasEllipsis
-- throwError $ BadSpecialForm "Test" input
checkLocal outerEnv localEnv identifiers hasEllipsis pattern@(DottedList ps p) input@(List (i : is)) = do
  if (length ps) == (length is)
          {- Lists are same length, implying elements in both should be the same.
          Cast pair to a List for further processing -}
     then loadLocal outerEnv localEnv identifiers (List $ ps ++ [p]) input False hasEllipsis
          {- Idea here is that if we have a dotted list, the last component does not have to be provided
          in the input. So in that case just fill in an empty list for the missing component. -}
     else loadLocal outerEnv localEnv identifiers pattern (DottedList (i : is) (List [])) False hasEllipsis
checkLocal outerEnv localEnv identifiers hasEllipsis pattern@(List _) input@(List _) =
  loadLocal outerEnv localEnv identifiers pattern input False hasEllipsis

checkLocal _ _ _ _ _ _ = return $ Bool False

{- Transform input by walking the tranform structure and creating a new structure
with the same form, replacing identifiers in the tranform with those bound in localEnv -}
transformRule :: Env -> Int -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal

{-
 - Recursively transform a list
 -
 - Parameters:
 -
 - localEnv - Local variable environment
 - ellipsisIndex - Zero-or-more match variables are stored as a list.
 -     This is the index into the current value to read from list
 - result - Resultant value, must be a parameter as it mutates with each function call, so we pass it using CPS
 - transform - The macro transformation, we read it out one atom at a time, and rewrite it into result
 - ellipsisList - Temporarily holds value of the "outer" result while we process the
 -     zero-or-more match. Once that is complete we swap this value back into it's rightful place 
 -}
transformRule localEnv ellipsisIndex (List result) transform@(List (List l : ts)) (List ellipsisList) = do
  if macroElementMatchesMany transform
     then do
             curT <- transformRule localEnv (ellipsisIndex + 1) (List []) (List l) (List result)
             case curT of
               Nil _ -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                           else transformRule localEnv 0 (List $ ellipsisList ++ result) (List $ tail ts) (List [])
               -- Dotted list transform returned during processing...
               List [Nil _, List _] -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                          else transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
               List _ -> transformRule localEnv (ellipsisIndex + 1) (List $ result ++ [curT]) transform (List ellipsisList)
               _ -> throwError $ Default "Unexpected error"
     else do
             lst <- transformRule localEnv ellipsisIndex (List []) (List l) (List ellipsisList)
             case lst of
                  List [Nil _, _] -> return lst
                  List _ -> transformRule localEnv ellipsisIndex (List $ result ++ [lst]) (List ts) (List ellipsisList)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "Macro transform error" $ List [lst, (List l), Number $ toInteger ellipsisIndex]

transformRule localEnv ellipsisIndex (List result) transform@(List ((Vector v) : ts)) (List ellipsisList) = do
  if macroElementMatchesMany transform
     then do
             -- Idea here is that we need to handle case where you have (vector ...) - EG: (#(var step) ...)
             curT <- transformRule localEnv (ellipsisIndex + 1) (List []) (List $ elems v) (List result)
             case curT of
               Nil _ -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                           else transformRule localEnv 0 (List $ ellipsisList ++ result) (List $ tail ts) (List [])
               List t -> transformRule localEnv (ellipsisIndex + 1) (List $ result ++ [asVector t]) transform (List ellipsisList)
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformRule localEnv ellipsisIndex (List []) (List $ elems v) (List ellipsisList)
             case lst of
                  List l -> do
                      transformRule localEnv ellipsisIndex (List $ result ++ [asVector l]) (List ts) (List ellipsisList)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [(List ellipsisList), lst, (List [Vector v]), Number $ toInteger ellipsisIndex]

 where asVector lst = (Vector $ (listArray (0, length lst - 1)) lst)

transformRule localEnv ellipsisIndex (List result) transform@(List (dl@(DottedList _ _) : ts)) (List ellipsisList) = do
  if macroElementMatchesMany transform
     then do
     -- Idea here is that we need to handle case where you have (pair ...) - EG: ((var . step) ...)
             curT <- transformDottedList localEnv (ellipsisIndex + 1) (List []) (List [dl]) (List result)
             case curT of
               Nil _ -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                           else transformRule localEnv 0 (List $ ellipsisList ++ result) (List $ tail ts) (List [])
               {- This case is here because we need to process individual components of the pair to determine
               whether we are done with the match. It is similar to above but not exact... -}
               List [Nil _, List _] -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                           else transformRule localEnv 0 (List $ result) (List $ tail ts) (List [])
               List t -> transformRule localEnv (ellipsisIndex + 1) (List $ result ++ t) transform (List ellipsisList)
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformDottedList localEnv ellipsisIndex (List []) (List [dl]) (List ellipsisList)
             case lst of
                  List [Nil _, List _] -> return lst
                  List l -> transformRule localEnv ellipsisIndex (List $ result ++ l) (List ts) (List ellipsisList)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [(List ellipsisList), lst, (List [dl]), Number $ toInteger ellipsisIndex]


-- Transform an atom by attempting to look it up as a var...
transformRule localEnv ellipsisIndex (List result) transform@(List (Atom a : ts)) unused = do
  let hasEllipsis = macroElementMatchesMany transform
  isDefined <- liftIO $ isBound localEnv a
  if hasEllipsis
     then if isDefined
             then do
                  -- get var
                  var <- getVar localEnv a
                  -- ensure it is a list
                  case var of
                    -- add all elements of the list into result
                    List v -> transformRule localEnv ellipsisIndex (List $ result ++ v) (List $ tail ts) unused
                    v@(_) -> transformRule localEnv ellipsisIndex (List $ result ++ [v]) (List $ tail ts) unused
             else -- Matched 0 times, skip it
                  transformRule localEnv ellipsisIndex (List result) (List $ tail ts) unused
     else do t <- if isDefined
                     then do var <- getVar localEnv a
                             if ellipsisIndex > 0
                                then do case var of
                                          List v -> if (length v) > (ellipsisIndex - 1)
                                                       then return $ v !! (ellipsisIndex - 1)
                                                       else return $ Nil ""
                                          _ -> throwError $ Default "Unexpected error in transformRule"
                                else return var
                     else return $ Atom a
             case t of
               Nil _ -> return t
               _ -> transformRule localEnv ellipsisIndex (List $ result ++ [t]) (List ts) unused

-- Transform anything else as itself...
transformRule localEnv ellipsisIndex (List result) (List (t : ts)) (List ellipsisList) = do
  transformRule localEnv ellipsisIndex (List $ result ++ [t]) (List ts) (List ellipsisList)

-- Base case - empty transform
transformRule _ _ result@(List _) (List []) _ = do
  return result

transformRule _ ellipsisIndex result transform unused = do
  throwError $ BadSpecialForm "An error occurred during macro transform" $ List [(Number $ toInteger ellipsisIndex), result, transform, unused]

transformDottedList :: Env -> Int -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
transformDottedList localEnv ellipsisIndex (List result) (List (DottedList ds d : ts)) (List ellipsisList) = do
          lsto <- transformRule localEnv ellipsisIndex (List []) (List ds) (List ellipsisList)
          case lsto of
            List lst -> do
                           r <- transformRule localEnv ellipsisIndex (List []) (List [d]) (List ellipsisList)
                           case r of
                                -- Trailing symbol in the pattern may be neglected in the transform, so skip it...
                                List [List []] -> transformRule localEnv ellipsisIndex (List $ result ++ [List lst]) (List ts) (List ellipsisList)
                                --
                                -- FUTURE: Issue #9 - the transform needs to be as follows:
                                --
                                {- - transform into a list if original input was a list - code is below but commented-out
                                - transform into a dotted list if original input was a dotted list -}
                                --
                                {- Could implement this by calling a new function on input (ds?) that goes through it and
                                looks up each atom that it finds, looking for its src. The src (or Nil?) would then be returned
                                and used here to determine what type of transform is used. -}
                                --
{- List [rst] -> transformRule localEnv ellipsisIndex (List $ result ++ [List $ lst ++ [rst]]) (List ts) (List ellipsisList)
List [rst] -> transformRule localEnv ellipsisIndex (List $ result ++ [DottedList lst rst]) (List ts) (List ellipsisList) -}
                                List [rst] -> do
                                                 src <- lookupPatternVarSrc localEnv $ List ds
                                                 case src of
                                                    String "pair" -> transformRule localEnv ellipsisIndex (List $ result ++ [DottedList lst rst]) (List ts) (List ellipsisList)
                                                    _ -> transformRule localEnv ellipsisIndex (List $ result ++ [List $ lst ++ [rst]]) (List ts) (List ellipsisList)
                                _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
            Nil _ -> return $ List [Nil "", List ellipsisList]
            _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d

transformDottedList _ _ _ _ _ = throwError $ Default "Unexpected error in transformDottedList"

-- Find an atom in a list; non-recursive (IE, a sub-list will not be inspected)
findAtom :: LispVal -> LispVal -> IOThrowsError LispVal
findAtom (Atom target) (List (Atom a : as)) = do
  if target == a
     then return $ Bool True
     else findAtom (Atom target) (List as)
findAtom _ (List (badtype : _)) = throwError $ TypeMismatch "symbol" badtype
findAtom _ _ = return $ Bool False

{- Initialize any pattern variables as an empty list.
 - That way a zero-match case can be identified later during transformation.
 -
 - Input:
 -  localEnv - Local environment that contains variables
 -  src - Input source, required because a pair in the pattern may be matched by either a list or a pair,
 -        and the transform needs to know this...
 -  identifiers - Literal identifiers that are transformed as themselves
 -  pattern - Pattern portion of the syntax rule 
 -}
initializePatternVars :: Env -> String -> LispVal -> LispVal -> IOThrowsError LispVal
initializePatternVars localEnv src identifiers pattern@(List _) = do
    case pattern of
        List (p : ps) -> do _ <- initializePatternVars localEnv src identifiers p
                            initializePatternVars localEnv src identifiers $ List ps
        List [] -> return $ Bool True
        _ -> return $ Bool True

initializePatternVars localEnv src identifiers (DottedList ps p) = do
    _ <- initializePatternVars localEnv src identifiers $ List ps
    initializePatternVars localEnv src identifiers p

initializePatternVars localEnv src identifiers (Vector v) = do
    initializePatternVars localEnv src identifiers $ List $ elems v

initializePatternVars localEnv src identifiers (Atom pattern) =
       {- FUTURE:
       there is code to attempt to flag "src" here, but it is not
       wire up correctly. In fact, the whole design here probably
       needs to be rethinked. -}
    do _ <- defineNamespacedVar localEnv "src" pattern $ String src
       isDefined <- liftIO $ isBound localEnv pattern
       found <- findAtom (Atom pattern) identifiers
       case found of
            (Bool False) -> if not isDefined -- Set variable in the local environment
                               then do
                                        defineVar localEnv pattern (List [])
                               else do
                                        return $ Bool True
             -- Ignore identifiers since they are just passed along as-is
            _ -> return $ Bool True

initializePatternVars _ _ _ _ =
    return $ Bool True

-- Find the first pattern var that reports being from a src, or False if none
lookupPatternVarSrc :: Env -> LispVal -> IOThrowsError LispVal
lookupPatternVarSrc localEnv pattern@(List _) = do
    case pattern of
        List (p : ps) -> do result <- lookupPatternVarSrc localEnv p
                            case result of
                              Bool False -> lookupPatternVarSrc localEnv $ List ps
                              _ -> return result
        List [] -> return $ Bool False
        _ -> return $ Bool False

lookupPatternVarSrc localEnv (DottedList ps p) = do
    result <- lookupPatternVarSrc localEnv $ List ps
    case result of
        Bool False -> lookupPatternVarSrc localEnv p
        _ -> return result

lookupPatternVarSrc localEnv (Vector v) = do
    lookupPatternVarSrc localEnv $ List $ elems v

lookupPatternVarSrc localEnv (Atom pattern) =
    do isDefined <- liftIO $ isNamespacedBound localEnv "src" pattern
       if isDefined then getNamespacedVar localEnv "src" pattern
                    else return $ Bool False

lookupPatternVarSrc _ _ =
    return $ Bool False
