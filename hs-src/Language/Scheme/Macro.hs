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
import qualified Language.Scheme.Macro.Matches as Matches
import Control.Monad.Error
import Data.Array
import Debug.Trace -- Only req'd to support trace, can be disabled at any time...

{- Nice FAQ regarding macro's, points out some of the limitations of current implementation
http://community.schemewiki.org/?scheme-faq-macros -}


{- Consider high-level ideas from these articles (of all places):
 -
 -  http://en.wikipedia.org/wiki/Scheme_(programming_language)#Hygienic_macros
 -  http://en.wikipedia.org/wiki/Hygienic_macro
 - -}

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

{-
-- Inspect a list of code, and transform as necessary
macroEval env (List (x@(List _) : xs)) = do
  first <- macroEval env x
  rest <- mapM (macroEval env) xs
  return $ List $ first : rest
-}

{- Inspect code for macros
 -
 - Only a list form is required because a pattern may only consist
 - of a list here. From the spec:
 -
 - "The <pattern> in a <syntax rule> is a list <pattern> that
begins with the keyword for the macro." 
 -
 -}
macroEval env lisp@(List (Atom x : _)) = do
  isDefined <- liftIO $ isNamespacedRecBound env macroNamespace x
  isDefinedAsVar <- liftIO $ isBound env x -- TODO: Not entirely correct; for example if a macro and var 
                                           -- are defined in same env with same name, which one should be selected?
  if isDefined && not isDefinedAsVar --(trace (show "mEval [" ++ show lisp ++ ", " ++ show x ++ "]: " ++ show isDefined) isDefined)
     then do
       (List (Atom "syntax-rules" : (List identifiers : rules))) <- getNamespacedVar env macroNamespace x
       -- Transform the input and then call macroEval again, since a macro may be contained within...
       macroEval env =<< macroTransform env (List identifiers) rules lisp
     else return lisp

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
        match <- loadLocal outerEnv localEnv identifiers (List ps) (List is) 0 [] [] 
        case match of
           Bool False -> return $ Nil ""
           _ -> do
--                bindings <- findBindings localEnv pattern
                transformRule outerEnv localEnv 0 [] (List []) template
      _ -> throwError $ BadSpecialForm "Malformed rule in syntax-rules" p

matchRule _ _ _ rule input = do
  throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ List [Atom "rule: ", rule, Atom "input: ", input]

-- Issue #30
{-------------------------
-- Just some test code, this needs to be more sophisticated than simply finding a list of them.
-- because we probably need to know the context - IE, (begin ... (lambda ...) (define ...) x) - x should
-- not be rewritten if it is the name of one of the lambda arguments
findBindings :: Env -> LispVal -> IOThrowsError LispVal
findBindings localEnv pattern@(List (_ : ps)) = searchForBindings localEnv (List ps) [] 

searchForBindings :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
--env pattern@(List (List (Atom "lambda" : List bs : _) : ps)) bindings = searchForBindings env (List ps) (bindings ++ bs) 
--searchForBindings env pattern@(List (List (Atom "lambda" : List bs : _) : ps)) bindings = searchForBindings env (List ps) (bindings ++ bs) 
searchForBindings env pattern@(List (p : ps)) bindings = do
  newBindings <- searchForBindings env (List [p]) []
  case newBindings of
    List n -> searchForBindings env (List ps) (bindings ++ n)
    _ -> throwError $ Default "Unexpected error in searchForBindings" 
searchForBindings _ _ bindings = return $ List bindings
-------------------------}

{- loadLocal - Determine if pattern matches input, loading input into pattern variables as we go,
in preparation for macro transformation. -}
loadLocal :: Env -> Env -> LispVal -> LispVal -> LispVal -> Int -> [Int] -> [(Bool, Bool)] -> IOThrowsError LispVal
loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex listFlags = do
  case (pattern, input) of

       {- For vectors, just use list match for now, since vector input matching just requires a
       subset of that behavior. Should be OK since parser would catch problems with trying
       to add pair syntax to a vector declaration. -}
       ((Vector p), (Vector i)) -> do
         loadLocal outerEnv localEnv identifiers (List $ elems p) (List $ elems i) ellipsisLevel ellipsisIndex listFlags

-- TODO: store somewhere whether input was a list or pair? Not quite sure how to make that happen, or if we
--       even need to per R5RS. See Issue 9
-- TODO: below should have a lot in common with pair/pair - roll common parts into a single function before we are finished
       ((DottedList ps p), (List (iRaw : isRaw))) -> do
         -- Split input into two sections: 
         --   is - required inputs that must be present
         --   i  - variable length inputs to each compare against p 
         let isSplit = splitAt (length ps) (iRaw : isRaw)
         let is = fst isSplit
         let i = (snd isSplit)

         result <- loadLocal outerEnv localEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags
         case result of
            Bool True -> --  By matching on an elipsis we force the code 
                         --  to match pagainst all elements in i. 
                         loadLocal outerEnv localEnv identifiers 
                                  (List $ [p, Atom "..."])
                                  (List i)
                                   ellipsisLevel -- Incremented in the list/list match below
                                   ellipsisIndex
                                   (flagDottedLists listFlags (True, False) $ length ellipsisIndex)
            _ -> return $ Bool False

-- TODO: something is wrong here, since one of the examples causes a hang. But I'm not seeing it at the moment...
-- detailed debugging is required

       ((DottedList ps p), (DottedList isRaw iRaw)) -> do
         
         -- Split input into two sections: 
         --   is - required inputs that must be present
         --   i  - variable length inputs to each compare against p 
         let isSplit = splitAt (length ps) isRaw
         let is = fst isSplit
         let i = (snd isSplit) ++ [iRaw]

         result <- loadLocal outerEnv localEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags
         case result of
            Bool True -> --  By matching on an elipsis we force the code 
                         --  to match pagainst all elements in i. 
                         loadLocal outerEnv localEnv identifiers 
                                  (List $ [p, Atom "..."]) 
                                  (List i)
                                   ellipsisLevel -- Incremented in the list/list match below
                                   ellipsisIndex
                                   (flagDottedLists listFlags (True, True) $ length ellipsisIndex)
            _ -> return $ Bool False

       (List (p : ps), List (i : is)) -> do -- check first input against first pattern, recurse...

         let nextHasEllipsis = macroElementMatchesMany pattern
         let level = if nextHasEllipsis then ellipsisLevel + 1
                                        else ellipsisLevel
         let idx = if nextHasEllipsis 
                      then if (length ellipsisIndex == level)
                              -- This is not the first match, increment existing index
                              then do
                                let l = splitAt (level - 1) ellipsisIndex
                                (fst l) ++ [(head (snd l)) + 1]
                              -- First input element that matches pattern; start at 0
                              else ellipsisIndex ++ [0]
                      else ellipsisIndex

         -- At this point we know if the input is part of an ellipsis, so set the level accordingly 
--         status <- checkLocal outerEnv (trace ("chkLocal i = " ++ show i ++ " lvl = " ++ show level ++ " idx = " ++ show idx ++ " p = " ++ show p) localEnv) identifiers level idx p i
         status <- checkLocal outerEnv (localEnv) identifiers level idx p i listFlags
         case (status) of
              -- No match
              Bool False -> if nextHasEllipsis
                                {- No match, must be finished with ...
                                Move past it, but keep the same input. -}
                                then do
                                        loadLocal outerEnv localEnv identifiers (List $ tail ps) (List (i : is)) ellipsisLevel ellipsisIndex listFlags
                                else return $ Bool False
              -- There was a match
              _ -> if nextHasEllipsis
                      then 
                           loadLocal outerEnv localEnv identifiers pattern (List is)
                            ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                            idx -- Must keep index since it is incremented each time
                            listFlags
                      else loadLocal outerEnv localEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags

       -- Base case - All data processed
       (List [], List []) -> return $ Bool True

       -- Ran out of input to process
       (List (_ : _), List []) -> do
                                 {- Ensure any patterns that are not present in the input still
                                 have their variables initialized so they are ready during trans. -}
                                 if (macroElementMatchesMany pattern)
--                                 if (trace ("m = " ++ show (Bool $ macroElementMatchesMany pattern)) (macroElementMatchesMany pattern)) -- TODO: is prev good enough? && ((length (trace ("no more input, pat = " ++ show pattern) ps)) == 1)
                                           then flagUnmatchedVars outerEnv localEnv identifiers pattern 
                                           else return $ Bool False
--TODO: returning a boolean is not sufficient; need to go through the remaining pattern and flag any vars as nil (?) if they are not defined
-- Consider output code from the simple let* example (pat is the remaining pattern, but in general case it could be anything):
--
-- no more input, pat = ((vars vals) ...)
-- a = "let"isDefined = False


       -- Pattern ran out, but there is still input. No match.
       (List [], _) -> return $ Bool False

       -- Check input against pattern (both should be single var)
       (_, _) -> checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern input listFlags

{-
 - Utility function to flag pattern variables as 'no match' that exist in the pattern after input has run out.
 - Note that this can only happen if the remaining pattern is part of a zero-or-more match, that matches 0 
 - (or no more) times.
 -}
flagUnmatchedVars :: Env -> Env -> LispVal -> LispVal -> IOThrowsError LispVal 

flagUnmatchedVars outerEnv localEnv identifiers (DottedList ps p) = do
  flagUnmatchedVars outerEnv localEnv identifiers $ List $ ps ++ [p]

flagUnmatchedVars outerEnv localEnv identifiers (Vector p) = do
  flagUnmatchedVars outerEnv localEnv identifiers $ List $ elems p

flagUnmatchedVars _ _ _ (List []) = return $ Bool True 

flagUnmatchedVars outerEnv localEnv identifiers (List (p : ps)) = do
  _ <- flagUnmatchedVars outerEnv localEnv identifiers p
  flagUnmatchedVars outerEnv localEnv identifiers $ List ps

flagUnmatchedVars _ _ _ (Atom "...") = return $ Bool True 
flagUnmatchedVars outerEnv localEnv identifiers (Atom p) = do
  isDefined <- liftIO $ isBound localEnv p
  isLexicallyDefinedVar <- liftIO $ isBound outerEnv p
  isIdent <- findAtom (Atom p) identifiers
  if isDefined 
     -- Var already defined, skip it...
     then continueFlagging
     else case isIdent of
             Bool True -> if isLexicallyDefinedVar   -- Is this good enough?
                             then return $ Bool True
                             else do _ <- flagUnmatchedVar localEnv p
                                     continueFlagging
             _ -> do _ <- flagUnmatchedVar localEnv p 
                     continueFlagging
 where continueFlagging = return $ Bool True 

flagUnmatchedVars _ _ _ _ = return $ Bool True 

flagUnmatchedVar :: Env -> String -> IOThrowsError LispVal
flagUnmatchedVar localEnv var = do
  defineVar localEnv var $ Nil "" -- Empty nil will signify the empty match

{- 
 - Utility function to insert a True flag to the proper trailing position of the DottedList indicator list
 - to indicate a dotted (improper) list in the pattern (fst) or input (snd)
 - -}
flagDottedLists :: [(Bool, Bool)] -> (Bool, Bool) -> Int -> [(Bool, Bool)]
flagDottedLists listFlags status lengthOfEllipsisIndex
 | length listFlags == lengthOfEllipsisIndex = listFlags ++ [status]
   -- Pad the original list with False flags, and append our status flags at the end
 | otherwise = listFlags ++ (replicate ((lengthOfEllipsisIndex) - (length listFlags)) (False, False)) ++ [status]

{- Check pattern against input to determine if there is a match
 -
 - @param outerEnv - Local variables in play outside the macro
 - @param localEnv - Local variables for the macro, used during transform
 - @param identifiers  - List of identifiers passed into the macro
 - @param ellipsisLevel - Determine nesting level of the zero-or-many match, if there is any
 - @param pattern - Pattern to match
 - @param input - Input to be matched
 -}
checkLocal :: Env -> Env -> LispVal -> Int -> [Int] -> LispVal -> LispVal -> [(Bool, Bool)]-> IOThrowsError LispVal
checkLocal _ _ _ _ _ (Bool pattern) (Bool input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ (Number pattern) (Number input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ (Float pattern) (Float input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ (String pattern) (String input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ (Char pattern) (Char input) _ = return $ Bool $ pattern == input
checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex (Atom pattern) input listFlags = do
  if (ellipsisLevel) > 0
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
                               then if isLexicallyDefinedVar == False 
                                       -- Var is not bound in outer code; proceed
                                       then do
                                         -- Set variable in the local environment
                                         _ <- addPatternVar isDefined ellipsisLevel ellipsisIndex pattern $ Atom pattern
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
                _ -> do _ <- addPatternVar isDefined ellipsisLevel ellipsisIndex pattern input
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
                        if (pattern == inpt && (not isLexicallyDefinedPatternVar)) -- Regarding lex binding; see above, sec 4.3.2 from spec
                           then do _ <- defineVar localEnv pattern input
                                   return $ Bool True
                           else return $ (Bool False)
                    -- Pattern/Input cannot match because input is not an atom
                    _ -> return $ (Bool False)

            -- No literal identifier, just load up the var
            _ -> do _ <- defineVar localEnv pattern input
                    return $ Bool True
{- TODO:
 the issue with this is that sometimes the var needs to be preserved and not have its value directly inserted.
 the real fix is to rename any identifiers bound in the macro transform. for example, (let ((temp ...)) ...) should
 have the variable renamed to temp-1 (for example)


            _ -> case input of
                    Atom inpt -> do
                       isLexicallyDefinedInput <- liftIO $ isBound outerEnv inpt -- Var defined in scope outside macro
                       if isLexicallyDefinedInput
                           then do _ <- defineVar localEnv pattern ((Nil inpt)) -- Var defined outside macro, flag as such for transform code
-- TODO: flag as such from the above ellipsis code as well            
                                   return $ Bool True
                           else do _ <- defineVar localEnv pattern input
                                   return $ Bool True
                    _ -> do _ <- defineVar localEnv pattern input
                            return $ Bool True
-}                            
    where
      -- Store pattern variable in a nested list
      -- TODO: ellipsisLevel should probably be used here for validation.
      -- 
      --  some notes: TODO (above): need to flag the ellipsisLevel of this variable.
      --              also, it is an error if, for an existing var, ellipsisLevel input does not match the var's stored level
      --
      addPatternVar isDefined ellipLevel ellipIndex pat val
        | isDefined = do v <- getVar localEnv pat
                         setVar localEnv pat (Matches.setData v ellipIndex val)
        | otherwise = do
                  let flags = getListFlags ellipIndex listFlags 
                  _ <- defineVar localEnv pat (Matches.setData (List []) ellipIndex val)
                  _ <- defineNamespacedVar localEnv "improper pattern" pat $ Bool $ fst flags
                  defineNamespacedVar localEnv "improper input" pat $ Bool $ snd flags
      -- Get pair of list flags that are at depth of ellipIndex, or False if flags do not exist (means improper not flagged)
      getListFlags elIndices flags 
        | length flags >= length elIndices = flags !! ((length elIndices) - 1)
        | otherwise = (False, False)

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern@(Vector _) input@(Vector _) flags =
  loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern@(DottedList _ _) input@(DottedList _ _) flags =
  loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern@(DottedList _ _) input@(List (_ : _)) flags = do
  loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern@(List _) input@(List _) flags =
  loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal _ _ _ _ _ _ _ _ = return $ Bool False

{- |Transform input by walking the tranform structure and creating a new structure
    with the same form, replacing identifiers in the tranform with those bound in localEnv -}
transformRule :: Env        -- ^ Outer, enclosing environment
              -> Env        -- ^ Environment local to the macro
              -> Int        -- ^ ellipsisLevel - Nesting level of the zero-to-many match, or 0 if none
              -> [Int]      -- ^ ellipsisIndex - The index at each ellipsisLevel. This is used to read data stored in
                            --                   pattern variables.
              -> LispVal    -- ^ Resultant (transformed) value. 
                            -- ^ Must be a parameter as it mutates with each transform call
              -> LispVal    -- ^ The macro transformation, read out one atom at a time and rewritten to result
-- FUTURE: re-arrange above parameters so they make more sense. Right now everything seems a little
-- too much like it is just thrown together.
              -> IOThrowsError LispVal
{- Notes
 -
 - the more I think about it, the more ellipsisList seems unnecessary. When transforming a zero-or-many
 - match the code can just start with an empty list and can append the transformed code to the result at
 - the current level. I think that will take care of everything
 -
 - simple example:
 -
 - ((list a ...) ...)
 -
 - -}

{-
 - Recursively transform a list
 -
 - OLD Parameters:
 -
 - localEnv - Local variable environment
 - ellipsisIndex - Zero-or-more match variables are stored as a list.
 -     This is the index into the current value to read from list
 - result - Resultant value, must be a parameter as it mutates with each function call, so we pass it using CPS
 - transform - The macro transformation, we read it out one atom at a time, and rewrite it into result
 - ellipsisList - Temporarily holds value of the "outer" result while we process the
 -     zero-or-more match. Once that is complete we swap this value back into it's rightful place 
 -}
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List (List l : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
--  if (trace ("trans List: " ++ show transform ++ " lvl = " ++ show ellipsisLevel ++ " idx = " ++ show ellipsisIndex) nextHasEllipsis)
  if (nextHasEllipsis)
     then do
             curT <- transformRule outerEnv localEnv level idx (List []) (List l)
--             case (trace ("curT = " ++ show curT) curT) of
             case (curT) of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result $ tail ts

{- TODO: refactor this code once list transformation works                               
               -- Dotted list transform returned during processing...
               List [Nil _, List _] -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule outerEnv localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                          else transformRule outerEnv localEnv 0 (List $ result) (List $ tail ts) (List [])
-}
               List _ -> transformRule outerEnv localEnv 
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [curT]) transform
               _ -> throwError $ Default "Unexpected error"
     else do
             lst <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List l)
             case lst of
-- OBSOLETE?                  List [Nil _, _] -> return lst
                  List _ -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [lst]) (List ts)
                  Nil _ -> return lst
-- TODO? seemed like a good idea but second-guessing this now:
--                  Nil _ -> --return (trace ("RETURNING NULL " ++ show transform ++ " , " ++ show lst) lst) -- TODO: seems wrong in some cases
--                           continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex (result ++ [List []]) (trace ("cont, ts = " ++ show ts ++ " lvl = " ++ show ellipsisLevel) ts)
                  _ -> throwError $ BadSpecialForm "Macro transform error" $ List [lst, (List l), Number $ toInteger ellipsisLevel]

transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List ((Vector v) : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
     then do
             -- Idea here is that we need to handle case where you have (vector ...) - EG: (#(var step) ...)
             curT <- transformRule outerEnv localEnv level idx (List []) (List $ elems v)
--             case (trace ("curT = " ++ show curT) curT) of
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result $ tail ts
               List t -> transformRule outerEnv localEnv 
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [asVector t]) transform
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List $ elems v)
             case lst of
                  List l -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [asVector l]) (List ts)
                  Nil _ -> return lst
-- TODO? seemed like a good idea but second-guessing this now:
--                  Nil _ -> return (trace ("RETURNING NULL " ++ show transform ++ " , " ++ show v) lst) -- TODO: seems wrong in some cases
--                           --continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result ts
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [Vector v]), Number $ toInteger ellipsisLevel]

 where asVector lst = (Vector $ (listArray (0, length lst - 1)) lst)

transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List (dl@(DottedList _ _) : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
     then do
             -- Idea here is that we need to handle case where you have (pair ...) - EG: ((var . step) ...)
             curT <- transformDottedList outerEnv localEnv level idx (List []) (List [dl])
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result $ tail ts 
{- TODO: needed?               -- This case is here because we need to process individual components of the pair to determine
               -- whether we are done with the match. It is similar to above but not exact...
               List [Nil _, List _] -> if ellipsisIndex == 0
                                -- First time through and no match ("zero" case). Use tail to move past the "..."
                           then transformRule outerEnv localEnv 0 (List $ result) (List $ tail ts) (List [])
                                -- Done with zero-or-more match, append intermediate results (ellipsisList) and move past the "..."
                           else transformRule outerEnv localEnv 0 (List $ result) (List $ tail ts) (List []) -}
               List t -> transformRule outerEnv localEnv 
                          ellipsisLevel -- Do not increment level, just wait until next iteration where incremented above
                          idx -- Keep incrementing each time
                         (List $ result ++ t) transform -- TODO: does t have to be converted to a pair?
    -- TODO: dotted list case?
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformDottedList outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List [dl])
             case lst of
-- TODO: needed?                  List [Nil _, List _] -> return lst
                  List l -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ l) (List ts) -- TODO: should l be a dotted list?
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [dl]), Number $ toInteger ellipsisLevel]

-- Transform an atom by attempting to look it up as a var...
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List (Atom a : ts)) = do
  isDefined <- liftIO $ isBound localEnv a


--TODO: is this really what we want? Do we handle case where we are in a 0-to-many match but not at this level?
--  if (trace ("trans Atom: " ++ show transform ++ " lvl = " ++ show ellipsisLevel ++ " idx = " ++ show ellipsisIndex ++ " hasEllip = " ++ show hasEllipsis) hasEllipsis)
  if hasEllipsis
    then ellipsisHere isDefined
    else noEllipsis isDefined

  where
    -- A temporary function to use input flags to append a '() to a list if necessary
-- TODO: only makes sense if the *transform* is a dotted list
    appendNil d (Bool isImproperPattern) (Bool isImproperInput) =
      case d of
         List lst -> if isImproperPattern && not isImproperInput
                        then List $ lst ++ [List []]
                        else List lst
         _ -> d
    -- TODO: appendNil _ _ _ = throwError $ Default "Unexpected error in appendNil"

    loadNamespacedBool namespc = do
        isDef <- liftIO $ isNamespacedBound localEnv namespc a
        if isDef
           then getNamespacedVar localEnv namespc a
           else return $ Bool False

    hasEllipsis = macroElementMatchesMany transform
    ellipsisHere isDefined = do
        if isDefined
             then do 
                    isImproperPattern <- loadNamespacedBool "improper pattern"
                    isImproperInput <- loadNamespacedBool "improper input"
                    -- get var
                    var <- getVar localEnv a
                    -- ensure it is a list
                    case var of
                      -- add all elements of the list into result
                      List _ -> do case (appendNil (Matches.getData var ellipsisIndex) isImproperPattern isImproperInput) of
                                     List aa -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ aa) (List $ tail ts)
                                     _ -> -- No matches for var
                                          continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result $ tail ts

{- TODO:                      Nil input -> do -- Var lexically defined outside of macro, load from there
--
-- TODO: this could be a problem, because we need to signal the end of the ... and do not want an infinite loop.
--       but we want the lexical value as well. need to think about this in more detail to get a truly workable solution
--
                                  v <- getVar outerEnv input
                                  transformRule outerEnv localEnv ellipsisIndex (List $ result ++ [v]) (List $ tail ts) unused -}
                      Nil "" -> -- No matches, keep going
                                continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result $ tail ts
                      v@(_) -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [v]) (List $ tail ts)
             else -- Matched 0 times, skip it
                  transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) (List $ tail ts)

    noEllipsis isDefined = do
      isImproperPattern <- loadNamespacedBool "improper pattern"
      isImproperInput <- loadNamespacedBool "improper input"
--      t <- if (trace ("a = " ++ show a ++ "isDefined = " ++ show isDefined) isDefined)
      t <- if (isDefined)
              then do
                   var <- getVar localEnv a
--                   case (trace ("var = " ++ show var) var) of
                   case (var) of
                     Nil "" -> return $ Nil "" -- A 0 match case (input ran out in pattern), flag it to calling code
                     Nil input -> do v <- getVar outerEnv input
                                     return v
--                     _ -> case (trace ("a = " ++ show a ++ " var = " ++ show var) var) of
-- TODO: why the hell is there another 'case' here when it is the same as the above one?
-- fix this up before the next release
                     _ -> case (var) of
                          List v -> do
                               if ellipsisLevel > 0
                                       then return $ appendNil (Matches.getData var ellipsisIndex) isImproperPattern isImproperInput -- Take all elements, instead of one-at-a-time 
                                       else if length v > 0 
                                               then return var -- Just return the elements directly, so all can be appended
                                               else return $ Nil "" -- A 0 match case, flag it to calling code
                          _ -> if ellipsisLevel > 0
                                  then throwError $ Default "Unexpected error processing data in transformRule" -- List req'd for 0-or-n match
                                  else return var
              else do
                -- TODO: need to determine if atom was within an nary-match in the pattern
                -- the problem is that the pattern matching code will simply skip the pattern since input ends
                -- will probably need to add code up there in order to flag it correctly down here
                --
                -- once we can flag it, we could return a special nil here such as "Continue" and could use that
                -- to signal to the code below to simply skip over this atom (just like how the 0-match case is
                -- handled above)
--                if ellipsisLevel > 0
--                   then return $ Nil "" -- No match, signal to the outer code... 
--                   else return $ Atom a
-- simple test cases:
-- (letrec ((x 1)) x)
-- (let* ((x 1)) x)
                  return $ Atom a
      case t of
         Nil _ -> return t -- TODO: is this correct? don't we need to keep going, a-la: continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result ts
         List l -> do
        {- What's going on here is that if the pattern was a dotted list but the transform is not, we
           need to "lift" the input up out of a list.
TODO:
some questions:
- What if the transform is (a b c 1) or such? is this handled correctly? (maybe due to how pattern flag is set, not sure)
- The list could be part of an ellipsis, right? I think similar logic probably needs to be above in the ellipsis case as well.
  that complicates things because we would need to distinguish between an ellipsis and a dotted list in the transform, in
  order to be able to figure out what to do...
-}
            if (eqVal isImproperPattern $ Bool True) && (eqVal isImproperInput $ Bool True)
              then continueTransformWith $ result ++ (buildImproperList l)
              else continueTransformWith $ result ++ [t]
         _ -> continueTransformWith $ result ++ [t]

    -- Transformed code should be an improper list, but may need to "promote" it to a proper list
    buildImproperList lst 
      | length lst > 1 = [DottedList (init lst) (last lst)]
      | otherwise      = lst

    -- Continue calling into transformRule
    continueTransformWith results = 
      transformRule outerEnv 
                    localEnv 
                    ellipsisLevel 
                    ellipsisIndex 
                   (List $ results)
                   (List ts)

-- Transform anything else as itself...
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) (List (t : ts)) = do
  transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [t]) (List ts) 

-- Base case - empty transform
transformRule _ _ _ _ result@(List _) (List []) = do
  return result

-- Transform is a single var, just look it up.
transformRule _ localEnv _ _ _ (Atom transform) = do
  v <- getVar localEnv transform
  return v

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
transformRule _ _ _ _ _ transform = return transform

transformDottedList :: Env -> Env -> Int -> [Int] -> LispVal -> LispVal -> IOThrowsError LispVal
transformDottedList outerEnv localEnv ellipsisLevel ellipsisIndex (List result) (List (DottedList ds d : ts)) = do
          lsto <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List ds)
          case lsto of
            List lst -> do

 -- TODO below: perhaps similar logic to the parser needs to be applied here, where
 --      the results are transformed into either a list or pair depending upon whether
 --      they form a proper list or not. think about this, I think this is what is supposed
 --      to happen, but the input in the pattern analysis step will need to change to 
 --      capture whether the input was a proper or improper list. hopefully the data stored to the
 --      input var can be altered to indicate this...
 --

                           -- TODO: d is an n-ary match, per Issue #34
                           r <- transformRule outerEnv localEnv 
                                              ellipsisLevel -- OK not to increment here, this is accounted for later on
                                              ellipsisIndex -- Same as above 
                                              (List []) 
                                              (List [d, Atom "..."])
                           case r of
                                -- Trailing symbol in the pattern may be neglected in the transform, so skip it...
--                                List [List []] -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts) -- TODO: is this form still applicable, post Issue #34?

                                List [] ->
                                    transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts)
                                Nil _ ->  -- Same as above, no match for d, so skip it 
                                    transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts)
                                {--
                                -- FUTURE: Issue #9 - the transform needs to be as follows:
                                --
                                -- - transform into a list if original input was a list - code is below but commented-out
                                - transform into a dotted list if original input was a dotted list 
                                --
                                -- Could implement this by calling a new function on input (ds?) that goes through it and
                                -- looks up each atom that it finds, looking for its src. The src (or Nil?) would then be returned
                                -- and used here to determine what type of transform is used. 
                                --
-- List [rst] -> transformRule localEnv ellipsisIndex (List $ result ++ [List $ lst ++ [rst]]) (List ts) (List ellipsisList)
--List [rst] -> transformRule localEnv ellipsisIndex (List $ result ++ [DottedList lst rst]) (List ts) (List ellipsisList) 
-}
-- TODO: both cases below do not take issue #9 into account
{-                                List [rst] -> do
                                    transformedCode <- buildTransformedCode result lst rst
                                    transformRule outerEnv localEnv ellipsisLevel ellipsisIndex 
                                                 (List transformedCode {-$ result ++ [DottedList lst rst]-}) (List ts)-}
                                {- TODO: OBSOLETE CODE:
                                   This method is broken, need to have more sophisticated pair/list processing
                                                 src <- lookupPatternVarSrc localEnv $ List ds
                                                 case src of
                                                    String "pair" -> transformRule outerEnv localEnv ellipsisIndex (List $ result ++ [DottedList lst rst]) (List ts) (List ellipsisList)
                                                    _ -> transformRule outerEnv localEnv ellipsisIndex (List $ result ++ [List $ lst ++ [rst]]) (List ts) (List ellipsisList)
                                                    -}
                                List rst -> do
                                    transformRule outerEnv localEnv ellipsisLevel ellipsisIndex 
                                                 (buildTransformedCode result lst rst) (List ts)
                                     -- OLD:(List $ result ++ [List $ lst ++ rst]) (List ts)
                                _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
            Nil _ -> return $ Nil ""
            _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
 where 
   -- Transform code as either a proper or improper list depending upon the data
   -- These are rather crude methods of 'cons'-ing everything together... are all cases accounted for?
   buildTransformedCode results ps p = do 
     case p of
        [List []] -> List $ results ++ [List ps]         -- Proper list has null list at the end
        [List ls] -> List $ results ++ [List $ ps ++ ls] -- Again, convert to proper list because a proper list is at end
        [l] -> List $ results ++ [DottedList ps l]
        ls -> do
            -- Same concepts as above, but here we check the last entry of a list of elements
            -- TODO: should be able to use a common function to encapsulate logic above and below
            case last ls of
              List [] -> List $ results ++ [List $ ps ++ init ls]
              List lls -> List $ results ++ [List $ ps ++ (init ls) ++ lls]
              t -> List $ results ++ [DottedList (ps ++ init ls) t]


transformDottedList _ _ _ _ _ _ = throwError $ Default "Unexpected error in transformDottedList"

-- |Continue transforming after a preceding match has ended 
continueTransform :: Env -> Env -> Int -> [Int] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result remaining = do
    if not (null remaining)
       then transformRule outerEnv 
                          localEnv 
                          ellipsisLevel 
                          ellipsisIndex 
                         (List result) 
                         (List $ remaining)
       else if length result > 0 
               then return $ List result
               else if ellipsisLevel > 0 
                       then return $ Nil ""  -- Nothing remains, no match
                       else return $ List [] -- Nothing remains, return empty list

-- Find an atom in a list; non-recursive (IE, a sub-list will not be inspected)
findAtom :: LispVal -> LispVal -> IOThrowsError LispVal
findAtom (Atom target) (List (Atom a : as)) = do
  if target == a
     then return $ Bool True
     else findAtom (Atom target) (List as)
findAtom _ (List (badtype : _)) = throwError $ TypeMismatch "symbol" badtype
findAtom _ _ = return $ Bool False

-- |Increment ellipsis level based on whether a new ellipsis is present
calcEllipsisLevel :: Bool -> Int -> Int
calcEllipsisLevel  nextHasEllipsis ellipsisLevel =
    if nextHasEllipsis then ellipsisLevel + 1
                       else ellipsisLevel

-- |Increment ellipsis index information based on given parameters
calcEllipsisIndex :: Bool -> Int -> [Int] -> [Int]
calcEllipsisIndex nextHasEllipsis ellipsisLevel ellipsisIndex =
    if nextHasEllipsis 
       then if (length ellipsisIndex == ellipsisLevel)
               -- This is not the first match, increment existing index
               then do
                 let l = splitAt (ellipsisLevel - 1) ellipsisIndex
                 (fst l) ++ [(head (snd l)) + 1]
               -- First input element that matches pattern; start at 0
               else ellipsisIndex ++ [0]
       else ellipsisIndex
