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

-}

module Language.Scheme.Macro
    (
      macroEval
    ) where
import Language.Scheme.Types
import Language.Scheme.Variables
import qualified Language.Scheme.Macro.Matches as Matches
import Language.Scheme.Primitives (_gensym)
import Control.Monad.Error
import Data.Array
import Data.Bits
import Debug.Trace -- Only req'd to support trace, can be disabled at any time...

-- |The bit for function application
modeFlagIsFuncApp = 0 
modeFlagIsQuoted = 1
setModeFlagIsFuncApp x = x .|. bit modeFlagIsFuncApp
--setModeFlag x = x .|. bit
--clearModeFlag x = x `clearBit`
clearFncFlg x = x `clearBit` modeFlagIsFuncApp -- Shortcut, since this will be called in many places

{-
 Implementation notes:

 Nice FAQ regarding macro's, points out some of the limitations of current implementation
 http://community.schemewiki.org/?scheme-faq-macros

 Consider high-level ideas from these articles (of all places):
 
 -  http://en.wikipedia.org/wiki/Scheme_(programming_language)#Hygienic_macros
 -  http://en.wikipedia.org/wiki/Hygienic_macro
 -}

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

{- Inspect code for macros
 -
 - Only a list form is required because a pattern may only consist
 - of a list here. From the spec:
 -
 - "The <pattern> in a <syntax rule> is a list <pattern> that 
 -  begins with the keyword for the macro." 
 -
 -}
macroEval env lisp@(List (Atom x : _)) = do
  isDefined <- liftIO $ isNamespacedRecBound env macroNamespace x
  isDefinedAsVar <- liftIO $ isBound env x -- TODO: Not entirely correct; for example if a macro and var 
                                           -- are defined in same env with same name, which one should be selected?

-- TODO: interesting that a simple (+) results in macroEval being called twice...
--       should see if the performance can be improved, at some point
--  if (trace ("entering macroEval. lisp = " ++ show lisp) isDefined) && not isDefinedAsVar 
  if isDefined && not isDefinedAsVar 
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
                                  -- Fix for Issue #44 - detect when pattern's match should 
                                  -- be modified from a pair to an ellipsis
                                  (Atom l : ls) -> (List [Atom l, DottedList ls d], True)
                                  _ -> (pattern, False)
              _ -> (pattern, False)
   case p of
      ((List (Atom _ : ps)), flag) -> do
        match <- checkPattern ps is flag 
        case match of
           Bool False -> return $ Nil ""
           _ -> do
                expandedLisp <- transformRule outerEnv localEnv 0 [] (List []) template $ setModeFlagIsFuncApp 0 
                case expandedLisp of
                    SyntaxResult r _ -> return r
                    x -> throwError $ BadSpecialForm "Unexpected code expansion from syntax-rules: " $ String $ show x
      _ -> throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ String $ show p

 where
   -- A pair at the outmost level must be transformed to use the ellipsis, 
   -- or else its nary match will not work properly during pattern matching. 
   checkPattern ps@(DottedList ds d : _) is True = do
     case is of
       (DottedList _ _ : _) -> do 
         loadLocal outerEnv localEnv identifiers 
                                  (List $ ds ++ [d, Atom "..."])
                                  (List is)
                                   0 []
                                  (flagDottedLists [] (False, False) 0)
       (List _ : _) -> do 
         loadLocal outerEnv localEnv identifiers 
                                  (List $ ds ++ [d, Atom "..."])
                                  (List is)
                                   0 []
                                  (flagDottedLists [] (True, False) 0)
       _ -> loadLocal outerEnv localEnv identifiers (List ps) (List is) 0 [] []

   -- No pair, immediately begin matching
   checkPattern ps is _ = loadLocal outerEnv localEnv identifiers (List ps) (List is) 0 [] [] 

matchRule _ _ _ rule input = do
  throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ List [Atom "rule: ", rule, Atom "input: ", input]

{- loadLocal - Determine if pattern matches input, loading input into pattern variables as we go,
in preparation for macro transformation. -}
loadLocal :: Env -> Env -> LispVal -> LispVal -> LispVal -> Int -> [Int] -> [(Bool, Bool)] -> IOThrowsError LispVal
loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex listFlags = do
  case (pattern, input) of

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
         status <- checkLocal outerEnv (localEnv) identifiers level idx p i listFlags
         case (status) of
              -- No match
              Bool False -> if nextHasEllipsis
                                {- No match, must be finished with ...
                                Move past it, but keep the same input. -}
                                then do
                                        case ps of
                                          [Atom "..."] -> return $ Bool True -- An otherwise empty list, so just let the caller know match is done
                                          _ -> loadLocal outerEnv localEnv identifiers (List $ tail ps) (List (i : is)) ellipsisLevel ellipsisIndex listFlags
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
         if (macroElementMatchesMany pattern)
            then do
              -- Ensure any patterns that are not present in the input still
              -- have their variables initialized so they are ready during transformation
              -- Note:
              -- Appending to eIndex to compensate for fact we are outside the list containing the nary match 
              let flags = getListFlags (ellipsisIndex ++ [0]) listFlags
              flagUnmatchedVars outerEnv localEnv identifiers pattern $ fst flags
            else return $ Bool False

       -- Pattern ran out, but there is still input. No match.
       (List [], _) -> return $ Bool False

       -- Check input against pattern (both should be single var)
       (_, _) -> checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern input listFlags

--
-- |Utility function to flag pattern variables as 'no match' that exist in the 
--  pattern after input has run out. Note that this can only happen if the 
--  remaining pattern is part of a zero-or-more match.
--
-- Extended for Issue #42 -
-- Flag whether an unmatched pattern variable was part of an improper list in the pattern
-- This information is necessary for use during transformation, where the output may
-- change depending upon the form of the input.
--
flagUnmatchedVars :: Env -> Env -> LispVal -> LispVal -> Bool -> IOThrowsError LispVal 

flagUnmatchedVars outerEnv localEnv identifiers (DottedList ps p) partOfImproperPattern = do
  flagUnmatchedVars outerEnv localEnv identifiers (List $ ps ++ [p]) partOfImproperPattern

flagUnmatchedVars outerEnv localEnv identifiers (Vector p) partOfImproperPattern = do
  flagUnmatchedVars outerEnv localEnv identifiers (List $ elems p) partOfImproperPattern

flagUnmatchedVars _ _ _ (List []) _ = return $ Bool True 

flagUnmatchedVars outerEnv localEnv identifiers (List (p : ps)) partOfImproperPattern = do
  _ <- flagUnmatchedVars outerEnv localEnv identifiers p partOfImproperPattern
  flagUnmatchedVars outerEnv localEnv identifiers (List ps) partOfImproperPattern

flagUnmatchedVars _ _ _ (Atom "...") _ = return $ Bool True 

flagUnmatchedVars outerEnv localEnv identifiers (Atom p) partOfImproperPattern =
  flagUnmatchedAtom outerEnv localEnv identifiers p partOfImproperPattern

flagUnmatchedVars _ _ _ _ _ = return $ Bool True 

-- |Flag an atom that did not have any matching input
--
--  Note that an atom may not be flagged in certain cases, for example if
--  the var is lexically defined in the outer environment. This logic
--  matches that in the pattern matching code.
flagUnmatchedAtom :: Env -> Env -> LispVal -> String -> Bool -> IOThrowsError LispVal 
flagUnmatchedAtom outerEnv localEnv identifiers p improperListFlag = do
  isDefined <- liftIO $ isBound localEnv p
  isLexicallyDefinedVar <- liftIO $ isBound outerEnv p
  isIdent <- findAtom (Atom p) identifiers
  if isDefined 
     -- Var already defined, skip it...
     then continueFlagging
     else case isIdent of
             Bool True -> if isLexicallyDefinedVar   -- Is this good enough?
                             then return $ Bool True
                             else do _ <- flagUnmatchedVar localEnv p improperListFlag
                                     continueFlagging
             _ -> do _ <- flagUnmatchedVar localEnv p improperListFlag 
                     continueFlagging
 where continueFlagging = return $ Bool True 

-- |Flag a pattern variable that did not have any matching input
flagUnmatchedVar :: Env -> String -> Bool -> IOThrowsError LispVal
flagUnmatchedVar localEnv var improperListFlag = do
  _ <- defineVar localEnv var $ Nil "" -- Empty nil will signify the empty match
  defineNamespacedVar localEnv "unmatched nary pattern variable" var $ Bool $ improperListFlag

{- 
 - Utility function to insert a True flag to the proper trailing position of the DottedList indicator list
 - to indicate a dotted (improper) list in the pattern (fst) or input (snd)
 - -}
flagDottedLists :: [(Bool, Bool)] -> (Bool, Bool) -> Int -> [(Bool, Bool)]
flagDottedLists listFlags status lengthOfEllipsisIndex
 | length listFlags == lengthOfEllipsisIndex = listFlags ++ [status]
   -- Pad the original list with False flags, and append our status flags at the end
 | otherwise = listFlags ++ (replicate ((lengthOfEllipsisIndex) - (length listFlags)) (False, False)) ++ [status]

-- Get pair of list flags that are at depth of ellipIndex, or False if flags do not exist (means improper not flagged)
getListFlags :: [Int] -> [(Bool, Bool)] -> (Bool, Bool)
getListFlags elIndices flags 
  | length elIndices > 0 && length flags >= length elIndices = flags !! ((length elIndices) - 1)
  | otherwise = (False, False)

-- Check pattern against input to determine if there is a match
checkLocal :: Env            -- Outer environment where this macro was called
           -> Env            -- Local environment used to store temporary variables for macro processing
           -> LispVal        -- List of identifiers specified in the syntax-rules
           -> Int            -- Current nary (ellipsis) level
           -> [Int]          -- Ellipsis Index, keeps track of the current nary (ellipsis) depth at each level 
           -> LispVal        -- Pattern to match
           -> LispVal        -- Input to be matched
           -> [(Bool, Bool)] -- Flags to determine whether input pattern/variables are proper lists
           -> IOThrowsError LispVal
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
                                         addPatternVar isDefined ellipsisLevel ellipsisIndex pattern $ Atom pattern
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
                _ -> addPatternVar isDefined ellipsisLevel ellipsisIndex pattern input
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
      -- FUTURE: ellipsisLevel should probably be used here for validation.
      -- 
      --         some notes: (above): need to flag the ellipsisLevel of this variable.
      --                     also, it is an error if, for an existing var, ellipsisLevel input does not match the var's stored level
      --
      addPatternVar isDefined ellipLevel ellipIndex pat val
        | isDefined = do v <- getVar localEnv pat
--                         case (trace ("addPV pat = " ++ show pat ++ " v = " ++ show v) v) of
                         case (v) of
                            Nil _ -> do
                              -- What's going on here is that the pattern var was found
                              -- before but not set as a pattern variable because it
                              -- was flagged as an unmatched var because input ran out
                              -- before it was found. So we need to define it at this step.
                              --
                              -- This feels like a special case that should be handled
                              -- in a more generic way. Anyhow, it seems to work fine for
                              -- the moment, but we may need to revisit this down the road.
                              _ <- initializePatternVar ellipLevel ellipIndex pat val
                              return $ Bool False
                            _ -> do _ <- setVar localEnv pat (Matches.setData v ellipIndex val)
                                    return $ Bool True
        | otherwise = do
            _ <- initializePatternVar ellipLevel ellipIndex pat val
            return $ Bool True

      -- Define a pattern variable that is seen for the first time
      initializePatternVar _ ellipIndex pat val = do
        let flags = getListFlags ellipIndex listFlags 
        _ <- defineVar localEnv pat (Matches.setData (List []) ellipIndex val)
        _ <- defineNamespacedVar localEnv "improper pattern" pat $ Bool $ fst flags
        defineNamespacedVar localEnv "improper input" pat $ Bool $ snd flags

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex (Vector p) (Vector i) flags =
  -- For vectors, just use list match for now, since vector input matching just requires a
  -- subset of that behavior. Should be OK since parser would catch problems with trying
  -- to add pair syntax to a vector declaration. -}
  loadLocal outerEnv localEnv identifiers (List $ elems p) (List $ elems i) ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern@(DottedList _ _) input@(DottedList _ _) flags =
  loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex (DottedList ps p) input@(List (_ : _)) flags = do
  loadLocal outerEnv localEnv identifiers 
                                  (List $ ps ++ [p, Atom "..."])
                                  input
                                   ellipsisLevel -- Incremented in the list/list match below
                                   ellipsisIndex
                                   (flagDottedLists flags (True, False) $ length ellipsisIndex)
checkLocal outerEnv localEnv identifiers ellipsisLevel ellipsisIndex pattern@(List _) input@(List _) flags =
  loadLocal outerEnv localEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal _ _ _ _ _ _ _ _ = return $ Bool False



{- TODO:
TODO: to start, will need some way for the transformer to know that it is processing the head component of a list.
so... will probably have to pass a flag around or something.   is transform the right place to do this? I think so, but need
to think about it, since transform is done after the whole pattern is read but prior to the full expansion of the macro.
does this equate to the paper's algorithm?

perhaps use Data.Bits to use an int to pass around this flag an potentially others
-}



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
              -> Int        -- ^ Mode flags
              -> IOThrowsError LispVal

-- Recursively transform a list
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List (List l : ts)) modeFlags = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if (nextHasEllipsis)
     then do
             curT <- transformRule outerEnv localEnv level idx (List []) (List l) $ setModeFlagIsFuncApp modeFlags
--             case (curT) of
             case (trace ("curT = " ++ show curT ++ " transform = " ++ show transform) curT) of
               SyntaxResult (Nil _) True -> throwError $ Default "should never happen" 
               SyntaxResult (Nil _) False -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result (tail ts) (clearFncFlg modeFlags)
               SyntaxResult lst {-@(List _)-} True -> transformRule outerEnv localEnv 
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [lst]) transform (clearFncFlg modeFlags)
               _ -> throwError $ Default $ "Unexpected error transforming list. Received: " ++ show curT
     else do
             lst <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List l) $ setModeFlagIsFuncApp modeFlags
             case lst of
-- TODO: see below, am trying to make this more general to support nested macro expansion. this line should become obsolete                  SyntaxResult lstResult@(List _) True -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [lstResult]) (List ts) 
                  SyntaxResult lstResult True -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [lstResult]) (List ts) (clearFncFlg modeFlags)
                  SyntaxResult (Nil _) False -> return lst
                  _ -> throwError $ BadSpecialForm "Macro transform error" $ List [lst, (List l), Number $ toInteger ellipsisLevel]

-- Recursively transform a vector by processing it as a list
-- FUTURE: can this code be consolidated with the list code?
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List ((Vector v) : ts)) modeFlags = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
     then do
             -- Idea here is that we need to handle case where you have (vector ...) - EG: (#(var step) ...)
             curT <- transformRule outerEnv localEnv level idx (List []) (List $ elems v) (clearFncFlg modeFlags) -- TODO: flag does not properly handle lists nested inside vectors, or dotted lists (see below)
--             case (trace ("curT = " ++ show curT) curT) of
             case curT of
               SyntaxResult (Nil _) False -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result (tail ts) (clearFncFlg modeFlags)
               SyntaxResult (List t) True -> transformRule outerEnv localEnv 
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [asVector t]) transform (clearFncFlg modeFlags)
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List $ elems v) (clearFncFlg modeFlags)
             case lst of
                  SyntaxResult (List l) True -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [asVector l]) (List ts) (clearFncFlg modeFlags)
                  SyntaxResult (Nil _) False -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [Vector v]), Number $ toInteger ellipsisLevel]

 where asVector lst = (Vector $ (listArray (0, length lst - 1)) lst)

-- Recursively transform an improper list
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List (dl@(DottedList _ _) : ts)) modeFlags = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
--  if (trace ("trans Pair: " ++ show transform ++ " lvl = " ++ show ellipsisLevel ++ " idx = " ++ show ellipsisIndex) nextHasEllipsis)
     then do
             -- Idea here is that we need to handle case where you have (pair ...) - EG: ((var . step) ...)
             curT <- transformDottedList outerEnv localEnv level idx (List []) (List [dl]) (clearFncFlg modeFlags)
             case curT of
               SyntaxResult (Nil _) False -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result (tail ts) (clearFncFlg modeFlags)
               SyntaxResult (List t) True -> transformRule outerEnv localEnv 
                          ellipsisLevel -- Do not increment level, just wait until next iteration where incremented above
                          idx -- Keep incrementing each time
                         (List $ result ++ t) transform (clearFncFlg modeFlags)
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformDottedList outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List [dl]) (clearFncFlg modeFlags)
             case lst of
                  SyntaxResult (List l) True -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ l) (List ts) (clearFncFlg modeFlags)
                  SyntaxResult (Nil _) False -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [dl]), Number $ toInteger ellipsisLevel]

-- Transform an atom by attempting to look it up as a var...
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) transform@(List (Atom a : rst)) inputModeFlags = do

  isDefinedAsMacro <- liftIO $ isNamespacedRecBound outerEnv macroNamespace a

  -- The macro subsystem needs to actively scan the template for binding constructs
  -- and macros. These only matter if they are at the start of a list (function application) 
  -- since otherwise they would not be executed as such.
  --
  -- Function application only matters if we find:
  --  - lambda, to handle as procedure abstraction (TODO: code this)
  --  - a macro, to handle as a macro call
  --  - quote marks, so we skip any quoted (literal) constructs - TODO: is this always the case that we skip? or is it not that simple??
  --
-- TODO: what about quasi-quotation? This probably is not handled correctly, but need to figure out what the
--       correct behavior is for a splice within a quasiquoted section of a template.
  if testBit inputModeFlags modeFlagIsFuncApp && not (testBit inputModeFlags modeFlagIsQuoted) -- Do not expand quoted macros 
     then if isDefinedAsMacro
             then expandMacro
             else expandFuncApp a rst
     else expandLisp rst inputModeFlags

  where
    -- Expand from an atom in the function application position
    expandFuncApp :: String -> [LispVal] -> IOThrowsError LispVal
    expandFuncApp "quote" ts = expandLisp ts $ setBit inputModeFlags modeFlagIsQuoted -- Set the quoted flag
    expandFuncApp "lambda" ts@(List vars : body) = do
        rawExpandedVars <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List vars) inputModeFlags

-- STILL WORKING THROUGH THIS SECTION!!

-- TODO: expand the vars section of ts - maybe cheating, but good enough for now
-- trouble is code needs to be restructured
        case rawExpandedVars of
          SyntaxResult (List expandedVars) True -> do
            _ <- markBoundIdentifiers localEnv expandedVars
            expandLisp (List expandedVars : body) inputModeFlags
          otherwise -> throwError $ BadSpecialForm "Unexpected error in expandFuncApp" otherwise
    expandFuncApp _ ts = expandLisp ts inputModeFlags

    -- Expand basic Lisp code using the normal means...
    expandLisp ts modeFlags = do
      isDefined <- liftIO $ isBound localEnv a
      if hasEllipsis
        then ellipsisHere isDefined ts modeFlags
        else noEllipsis isDefined ts modeFlags


--
-- TODO: the 'real' version from Clinger is much more involved, and has a match and rewrite step.
--       although... do those have to be done anyway, even if only 1 macro is being expanded (the one detected 'by eval')??
--
    -- Expand a macro inline
    -- This is more efficient than waiting until after expansion to expand an inner macro,
    -- and is also required for Clinger's hygiene algorithm.
    expandMacro = do
      expandedTransform <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) transform (clearFncFlg inputModeFlags)
      case expandedTransform of
         SyntaxResult r True -> do
             expanded <- macroEval outerEnv $ getSyntaxResult expandedTransform
             -- TODO: may not be this simple, since we discard the value of result. Probably OK though since the only way into this
             --       code block is if atom is in the head position, in which case we are guaranteed (?) to have an empty result list anyway
             return $ SyntaxResult ((trace ("a = " ++ a ++ " t = " ++ show transform ++ " ex = " ++ show expandedTransform ++ " expanded = " ++ show expanded ) expanded)) True
--             return $ SyntaxResult expanded True
         SyntaxResult _ False -> return expandedTransform

    -- A function to use input flags to append a '() to a list if necessary
    -- Only makes sense to do this if the *transform* is a dotted list
    appendNil d (Bool isImproperPattern) (Bool isImproperInput) =
      case d of
         List lst -> if isImproperPattern && not isImproperInput
                        then List $ lst ++ [List []]
                        else List lst
         _ -> d
    appendNil d _ _ = d -- Should never be reached...

    loadNamespacedBool namespc = do
        isDef <- liftIO $ isNamespacedBound localEnv namespc a
        if isDef
           then getNamespacedVar localEnv namespc a
           else return $ Bool False

    hasEllipsis = macroElementMatchesMany transform
    ellipsisHere isDefined ts modeFlags = do
        if isDefined
             then do 
                    isImproperPattern <- loadNamespacedBool "improper pattern"
                    isImproperInput <- loadNamespacedBool "improper input"
                    -- Load variable and ensure it is a list
                    var <- getVar localEnv a
                    case var of
                      -- add all elements of the list into result
                      List _ -> do case (appendNil (Matches.getData var ellipsisIndex) isImproperPattern isImproperInput) of
                                     List aa -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ aa) (List $ tail ts) (clearFncFlg modeFlags)
                                     _ -> -- No matches for var
                                          continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result (tail ts) (clearFncFlg modeFlags)

{- TODO:                      Nil input -> do -- Var lexically defined outside of macro, load from there
--
-- notes: this could be a problem, because we need to signal the end of the ... and do not want an infinite loop.
--        but we want the lexical value as well. need to think about this in more detail to get a truly workable solution
--
                                  v <- getVar outerEnv input
                                  transformRule outerEnv localEnv ellipsisIndex (List $ result ++ [v]) (List $ tail ts) unused -}
                      Nil "" -> -- No matches, keep going
                                continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result (tail ts) (clearFncFlg modeFlags)
                      v@(_) -> transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [v]) (List $ tail ts) (clearFncFlg modeFlags)
             else -- Matched 0 times, skip it
                  transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) (List $ tail ts) (clearFncFlg modeFlags)

    noEllipsis isDefined ts modeFlags = do
      isImproperPattern <- loadNamespacedBool "improper pattern"
      isImproperInput <- loadNamespacedBool "improper input"
--      t <- if (trace ("a = " ++ show a ++ "isDefined = " ++ show isDefined) isDefined)
      t <- if (isDefined)
              then do
                   var <- getVar localEnv a
--                   case (trace ("var = " ++ show var) var) of
                   case (var) of
                     Nil "" -> do 
                        -- Fix for issue #42: A 0 match case for var (input ran out in pattern), flag to calling code
                        --
                        -- What's happening here is that the pattern was flagged because it was not matched in
                        -- the pattern. We pick it up and in turn pass a special flag to the outer code (as t)
                        -- so that it can finally be processed correctly.
                        wasPair <- getNamespacedVar localEnv "unmatched nary pattern variable" a
                        case wasPair of
                            Bool True -> return $ Nil "var (pair) not defined in pattern"
                            _ -> return $ Nil "var not defined in pattern"
                     Nil input -> do v <- getVar outerEnv input
                                     return v
                     List v -> do
                          if ellipsisLevel > 0
                                  then -- Take all elements, instead of one-at-a-time
                                       return $ appendNil (Matches.getData var ellipsisIndex) 
                                                           isImproperPattern 
                                                           isImproperInput 
                                  else if length v > 0 
                                          then return var -- Just return the elements directly, so all can be appended
                                          else return $ Nil "" -- A 0 match case, flag it to calling code
                     _ -> if ellipsisLevel > 0
                             then -- List req'd for 0-or-n match
                                  throwError $ Default "Unexpected error processing data in transformRule" 
                             else return var
              else do
    -- TODO: this was the experimental code from findBindings...
--                isRenameDefined <- liftIO $ isNamespacedBound localEnv "renamed vars" a
--                case (trace ("isRenamed: " ++ show isRenameDefined ++ " var: " ++ show a) isRenameDefined) of
--                  True -> getNamespacedVar localEnv "renamed vars" a
 --                 _ -> return $ Atom a
                  return $ Atom a
      case t of
         Nil "var not defined in pattern" -> 
            if ellipsisLevel > 0
               then return $ SyntaxResult t False
               else continueTransformWith result ts modeFlags -- nary match in the pattern but used as list in transform; keep going
         Nil "var (pair) not defined in pattern" -> 
            if ellipsisLevel > 0
               then return $ SyntaxResult t False
                    -- nary match in pattern as part of an improper list but used as list here; append the empty list
               else continueTransformWith (result ++ [List []]) ts modeFlags
         Nil _ -> return  $ SyntaxResult t False
         List l -> do
            -- What's going on here is that if the pattern was a dotted list but the transform is not, we
            -- need to "lift" the input up out of a list.
            if (eqVal isImproperPattern $ Bool True) && (eqVal isImproperInput $ Bool True)
              then continueTransformWith (result ++ (buildImproperList l)) ts modeFlags
              else continueTransformWith (result ++ [t]) ts modeFlags
         _ -> continueTransformWith (result ++ [t]) ts modeFlags

    -- Transformed code should be an improper list, but may need to "promote" it to a proper list
    buildImproperList lst 
      | length lst > 1 = [DottedList (init lst) (last lst)]
      | otherwise      = lst

    -- Continue calling into transformRule
    continueTransformWith results ts modeFlags = 
      transformRule outerEnv 
                    localEnv 
                    ellipsisLevel 
                    ellipsisIndex 
                   (List $ results)
                   (List ts)
                   (clearFncFlg modeFlags)

-- Transform anything else as itself...
transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List result) (List (t : ts)) modeFlags = do
  transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [t]) (List ts) (clearFncFlg modeFlags)

-- Base case - empty transform
transformRule _ _ _ _ result@(List _) (List []) _ = do
  return $ normalSyntaxResult result

-- Transform is a single var, just look it up.
transformRule _ localEnv _ _ _ (Atom transform) _ = do
  v <- getVar localEnv transform
  return $ normalSyntaxResult (trace ("v = " ++ show v) v) -- Nil?

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
transformRule _ _ _ _ _ transform _ = return $ normalSyntaxResult (trace ("transform = " ++ show transform) transform)

-- | A helper function for transforming an improper list
transformDottedList :: Env -> Env -> Int -> [Int] -> LispVal -> LispVal -> Int -> IOThrowsError LispVal
transformDottedList outerEnv localEnv ellipsisLevel ellipsisIndex (List result) (List (DottedList ds d : ts)) modeFlags = do
          lsto <- transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List []) (List ds) (clearFncFlg modeFlags)
          case lsto of
            SyntaxResult (List lst) True -> do
              -- Similar logic to the parser is applied here, where
              -- results are transformed into either a list or pair depending upon whether
              -- they form a proper list
              --
              -- d is an n-ary match, per Issue #34
              r <- transformRule outerEnv localEnv 
                                 ellipsisLevel -- OK not to increment here, this is accounted for later on
                                 ellipsisIndex -- Same as above 
                                 (List []) 
                                 (List [d, Atom "..."])
                                 (clearFncFlg modeFlags)
              case r of
                   SyntaxResult (List []) True ->
                       -- Trailing symbol in the pattern may be neglected in the transform, so skip it...
                       transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts) (clearFncFlg modeFlags)
                   SyntaxResult _ False ->  -- Same as above, no match for d, so skip it 
                       transformRule outerEnv localEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts) (clearFncFlg modeFlags)
                   SyntaxResult (List rst) True -> do
                       transformRule outerEnv localEnv ellipsisLevel ellipsisIndex 
                                    (buildTransformedCode result lst rst) (List ts) (clearFncFlg modeFlags)
                   _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
            SyntaxResult _ False -> return lsto -- No match
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
            -- FUTURE: should be able to use a common function to encapsulate logic above and below
            case last ls of
              List [] -> List $ results ++ [List $ ps ++ init ls]
              List lls -> List $ results ++ [List $ ps ++ (init ls) ++ lls]
              t -> List $ results ++ [DottedList (ps ++ init ls) t]


transformDottedList _ _ _ _ _ _ _ = throwError $ Default "Unexpected error in transformDottedList"

-- |Continue transforming after a preceding match has ended 
continueTransform :: Env -> Env -> Int -> [Int] -> [LispVal] -> [LispVal] -> Int -> IOThrowsError LispVal
continueTransform outerEnv localEnv ellipsisLevel ellipsisIndex result remaining modeFlags = do
    if not (null remaining)
       then transformRule outerEnv 
                          localEnv 
                          ellipsisLevel 
                          ellipsisIndex 
                         (List result) 
                         (List $ remaining)
                         modeFlags
       else if length result > 0 
               then return $ SyntaxResult (List result) True
               else if ellipsisLevel > 0 
                       then return $ SyntaxResult (Nil "") False -- Nothing remains, no match
                       else return $ SyntaxResult (List []) True -- Nothing remains, return empty list

-- |Find an atom in a list; non-recursive (IE, a sub-list will not be inspected)
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

markBoundIdentifiers :: Env -> [LispVal] -> IOThrowsError LispVal
markBoundIdentifiers env vars = do
  saveVars env vars
markBoundIdentifiers _ input = throwError $ BadSpecialForm "Unexpected input to markBoundIdentifiers" $ List input 

-- TODO: does the code need to be expanded first? for example, if the template says (x y ...) what to do?
-- ^ perhaps the caller can read the pattern vars out and pass them along to this function... That probably makes the most sense
-- TODO: rename function to something more meaningful
saveVars :: Env -> [LispVal] -> IOThrowsError LispVal
-- TODO: the following should not be necessary
saveVars env (Atom "..." : vs) = saveVars env vs -- The ellipsis is reserved; skip it
saveVars env (Atom v : vs) = do
  renamed <- _gensym v
  _ <- defineNamespacedVar (trace ("renamed var:" ++ v ++ " to: " ++ show renamed) env) "renamed vars" v renamed -- TODO: a temporary rename used for testing  
--  defineNamespacedVar localEnv "renamed vars" v renamed -- TODO: a temporary rename used for testing  
  saveVars env vs 
saveVars env (_: vs) = saveVars env vs
saveVars _ [] = return $ Bool True --List renamedVars

{- findBindings :: Env -> Env -> LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal

findBindings outerEnv localEnv identifiers (List (Atom "lambda" : List vars : ls)) bindings = do
  renamedVars <- saveVars vars []
  findBindings outerEnv localEnv identifiers (List ls) (appendBindings bindings renamedVars) 
 where
  appendBindings b (List vars) = b ++ vars
  appendBindings b _ = b


findBindings outerEnv localEnv identifiers (List (List l : ls)) bindings = do
  b <- findBindings outerEnv localEnv identifiers (List l) bindings
  case b of
    List bb -> findBindings outerEnv localEnv identifiers (List ls) bb

findBindings outerEnv localEnv identifiers (List (_ : ls)) bindings = do
  findBindings outerEnv localEnv identifiers (List ls) bindings

findBindings outerEnv localEnv _ val b = return $ List b
-}
