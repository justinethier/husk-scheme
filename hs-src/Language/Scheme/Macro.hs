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

TODO: but the above is changing to use Clinger's algorithm!

macros will change to have a step 0 that walks the macro code searching for 
the 4 criteria noted in the paper. if any of the criteria are found (procedure abstractions,
macro calls, etc) then the appropriate handler will be called to deal with it
-}

module Language.Scheme.Macro
    (
      macroEval
    , needToExtendEnv
    ) where
import Language.Scheme.Types
import Language.Scheme.Variables
import qualified Language.Scheme.Macro.Matches as Matches
import Language.Scheme.Primitives (_gensym)
import Control.Monad.Error
import Data.Array
import Debug.Trace -- Only req'd to support trace, can be disabled at any time...

{-
 Implementation notes:

 Nice FAQ regarding macro's, points out some of the limitations of current implementation
 http://community.schemewiki.org/?scheme-faq-macros

 Consider high-level ideas from these articles (of all places):
 
 -  http://en.wikipedia.org/wiki/Scheme_(programming_language)#Hygienic_macros
 -  http://en.wikipedia.org/wiki/Hygienic_macro
 -}

-- TODO:
--
-- Notes regarding other side of hygiene.
-- In order to handle the 'other side', the env at macro definition needs to be saved. It
-- will be used again when a macro is expanded. The pattern matcher will compare any named
-- identifiers it finds against both environments to ensure identifiers were not redefined.
--
-- Also, during rewrite identifiers are supposed to be read out of envDef. They are then 
-- diverted into envUse at the end of the macro transcription (in other words, once an
-- instance of rewrite is finished).
--
-- So... how do we preserve envDef? One idea is to create a deep copy of the env during
-- macro definition, but this could be error prone and expensive. Another idea is to
-- call extendEnv to create a new environment on top of envDef. This new environment
-- would then need to be passed along to eval (and presumably its current/next continuations).
--
-- This should work because any env changes would only affect the new environment and not
-- the parent one. The disadvantage is that macroEval is called in several places in Core.
-- It's calls will need to be modified to use a new function that will pass along the
-- extended env if necessary. I am a bit concerned about suble errors occuring if any
-- continuations in the chain are not updated and still have the old environment in them.
-- It may be tricky to get this right. But otherwise the change *should* be straightforward.


-- A support function for Core that will be used as part of the above...
needToExtendEnv :: LispVal -> Bool --IOThrowsError LispVal
needToExtendEnv (List [Atom "define-syntax", Atom _, (List (Atom "syntax-rules" : (List _ : _)))]) = True
needToExtendEnv _ = False 

{- |macroEval
Search for macro's in the AST, and transform any that are found.
There is also a special case (define-syntax) that loads new rules. -}
macroEval :: Env -> LispVal -> IOThrowsError LispVal

-- Special case, just load up the syntax rules
macroEval env (List [Atom "define-syntax", Atom keyword, syntaxRules@(List (Atom "syntax-rules" : (List identifiers : rules)))]) = do
  {-
   - FUTURE: Issue #15: there really ought to be some error checking of the syntax rules, 
   -                    since they could be malformed...
  - As it stands now, there is no checking until the code attempts to perform a macro transformation.
  - At a minimum, should check identifiers to make sure each is an atom (see findAtom) 
  -}
  _ <- do
    -- TBD: do we need to store a copy of this Env?
    defineNamespacedVar env macroNamespace keyword $ Syntax env identifiers rules
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
  if isDefined && not isDefinedAsVar 
     then do
       Syntax defEnv identifiers rules <- getNamespacedVar env macroNamespace x
       renameEnv <- liftIO $ nullEnv -- Local environment used just for this invocation
                                     -- to hold renamed variables
       cleanupEnv <- liftIO $ nullEnv -- Local environment used just for this invocation
                                      -- to hold new symbols introduced by renaming. We
                                      -- can use this to clean up any left after transformation

-- TODO: this is just test code for comparing use/def environments
--       isUseDef <- liftIO $ isNamespacedRecBoundWUpper defEnv env varNamespace "=>"
       isCleanDef <- liftIO $ isRecBound cleanupEnv "=>"
       isUseDef <- liftIO $ isRecBound env "=>"
       isDefDef <- liftIO $ isRecBound defEnv "=>"
--

       -- Transform the input and then call macroEval again, since a macro may be contained within...
       expanded <- macroTransform env renameEnv cleanupEnv (List identifiers) rules --lisp
         (trace ("cleanDef = " ++ show isCleanDef ++ " useDef = " ++ show isUseDef ++ " defDef = " ++ show isDefDef) lisp) -- TODO: w/Clinger, may not need to call macroEval again
--       macroEval env =<< cleanExpanded cleanupEnv (List []) expanded 
       macroEval env expanded -- TODO: disabling this for now: =<< cleanExpanded cleanupEnv (List []) expanded 
        -- let's figure out why cond and iteration are failing, then circle around back to this...
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
macroTransform :: Env -> Env -> Env -> LispVal -> [LispVal] -> LispVal -> IOThrowsError LispVal
macroTransform env renameEnv cleanupEnv identifiers (rule@(List _) : rs) input = do
  localEnv <- liftIO $ nullEnv -- Local environment used just for this invocation
                               -- to hold pattern variables
  result <- matchRule env identifiers localEnv renameEnv cleanupEnv rule input
  case result of
    Nil _ -> macroTransform env renameEnv cleanupEnv identifiers rs input
    _ -> do
        -- Walk the resulting code, performing the Clinger algorithm's 4 components
        -- TODO: see below: expanded <-
        walkExpanded env env renameEnv cleanupEnv True False (List []) (trace ("macroT, result = " ++ show result) result)

-- Ran out of rules to match...
macroTransform _ _ _ _ _ input = throwError $ BadSpecialForm "Input does not match a macro pattern" input

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
matchRule :: Env -> LispVal -> Env -> Env -> Env -> LispVal -> LispVal -> IOThrowsError LispVal
matchRule outerEnv identifiers localEnv renameEnv cleanupEnv (List [pattern, template]) (List inputVar) = do
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
--                bindings <- findBindings localEnv pattern
                transformRule outerEnv localEnv renameEnv cleanupEnv 0 [] (List []) template
      _ -> throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ String $ show p

 where
   -- A pair at the outmost level must be transformed to use the ellipsis, 
   -- or else its nary match will not work properly during pattern matching. 
   checkPattern ps@(DottedList ds d : _) is True = do
     case is of
       (DottedList _ _ : _) -> do 
         loadLocal outerEnv localEnv renameEnv identifiers 
                                  (List $ ds ++ [d, Atom "..."])
                                  (List is)
                                   0 []
                                  (flagDottedLists [] (False, False) 0)
       (List _ : _) -> do 
         loadLocal outerEnv localEnv renameEnv identifiers 
                                  (List $ ds ++ [d, Atom "..."])
                                  (List is)
                                   0 []
                                  (flagDottedLists [] (True, False) 0)
       _ -> loadLocal outerEnv localEnv renameEnv identifiers (List ps) (List is) 0 [] []

   -- No pair, immediately begin matching
   checkPattern ps is _ = loadLocal outerEnv localEnv renameEnv identifiers (List ps) (List is) 0 [] [] 

matchRule _ _ _ _ _ rule input = do
  throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ List [Atom "rule: ", rule, Atom "input: ", input]

{- loadLocal - Determine if pattern matches input, loading input into pattern variables as we go,
in preparation for macro transformation. -}
loadLocal :: Env -> Env -> Env -> LispVal -> LispVal -> LispVal -> Int -> [Int] -> [(Bool, Bool)] -> IOThrowsError LispVal
loadLocal outerEnv localEnv renameEnv identifiers pattern input ellipsisLevel ellipsisIndex listFlags = do
  case (pattern, input) of

       ((DottedList ps p), (DottedList isRaw iRaw)) -> do
         
         -- Split input into two sections: 
         --   is - required inputs that must be present
         --   i  - variable length inputs to each compare against p 
         let isSplit = splitAt (length ps) isRaw
         let is = fst isSplit
         let i = (snd isSplit) ++ [iRaw]

         result <- loadLocal outerEnv localEnv renameEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags
         case result of
            Bool True -> --  By matching on an elipsis we force the code 
                         --  to match pagainst all elements in i. 
                         loadLocal outerEnv localEnv renameEnv identifiers 
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
         status <- checkLocal outerEnv (localEnv) renameEnv identifiers level idx p i listFlags
         case (status) of
              -- No match
              Bool False -> if nextHasEllipsis
                                {- No match, must be finished with ...
                                Move past it, but keep the same input. -}
                                then do
                                        case ps of
                                          [Atom "..."] -> return $ Bool True -- An otherwise empty list, so just let the caller know match is done
                                          _ -> loadLocal outerEnv localEnv renameEnv identifiers (List $ tail ps) (List (i : is)) ellipsisLevel ellipsisIndex listFlags
                                else return $ Bool False
              -- There was a match
              _ -> if nextHasEllipsis
                      then 
                           loadLocal outerEnv localEnv renameEnv identifiers pattern (List is)
                            ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                            idx -- Must keep index since it is incremented each time
                            listFlags
                      else loadLocal outerEnv localEnv renameEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags

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
       (_, _) -> checkLocal outerEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex pattern input listFlags

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
           -> Env            -- Local environment used to store vars that have been renamed by the macro subsystem 
           -> LispVal        -- List of identifiers specified in the syntax-rules
           -> Int            -- Current nary (ellipsis) level
           -> [Int]          -- Ellipsis Index, keeps track of the current nary (ellipsis) depth at each level 
           -> LispVal        -- Pattern to match
           -> LispVal        -- Input to be matched
           -> [(Bool, Bool)] -- Flags to determine whether input pattern/variables are proper lists
           -> IOThrowsError LispVal
checkLocal _ _ _ _ _ _ (Bool pattern) (Bool input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ (Number pattern) (Number input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ (Float pattern) (Float input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ (String pattern) (String input) _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ (Char pattern) (Char input) _ = return $ Bool $ pattern == input
checkLocal outerEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex (Atom pattern) input listFlags = do

  -- TODO: 
  --
  -- The code below uses this rename boolean as a factor to determine whether a named
  -- identifier has been redefined and thus should not match itself in the input. But the
  -- thing is, the actual code is supposed to compare the value at macro definition
  -- time with the value in the environment of use (outerEnv) to make this determination.
  -- So what is below is close but not truly correct.
  --
  isRenamed <- liftIO $ isRecBound renameEnv (trace ("pattern = " ++ pattern) pattern)

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
                               then if isLexicallyDefinedVar == False -- && not isRenamed 
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
                        if (pattern == inpt && (not isLexicallyDefinedPatternVar)) && (not isRenamed) -- Regarding lex binding; see above, sec 4.3.2 from spec
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

checkLocal outerEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex (Vector p) (Vector i) flags =
  -- For vectors, just use list match for now, since vector input matching just requires a
  -- subset of that behavior. Should be OK since parser would catch problems with trying
  -- to add pair syntax to a vector declaration. -}
  loadLocal outerEnv localEnv renameEnv identifiers (List $ elems p) (List $ elems i) ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex pattern@(DottedList _ _) input@(DottedList _ _) flags =
  loadLocal outerEnv localEnv renameEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal outerEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex (DottedList ps p) input@(List (_ : _)) flags = do
  loadLocal outerEnv localEnv renameEnv identifiers 
                                  (List $ ps ++ [p, Atom "..."])
                                  input
                                   ellipsisLevel -- Incremented in the list/list match below
                                   ellipsisIndex
                                   (flagDottedLists flags (True, False) $ length ellipsisIndex)
checkLocal outerEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex pattern@(List _) input@(List _) flags =
  loadLocal outerEnv localEnv renameEnv identifiers pattern input ellipsisLevel ellipsisIndex flags

checkLocal _ _ _ _ _ _ _ _ _ = return $ Bool False

-- |Walk expanded code per Clinger
walkExpanded :: Env -> Env -> Env -> Env -> Bool -> Bool -> LispVal -> LispVal -> IOThrowsError LispVal
walkExpanded defEnv useEnv renameEnv cleanupEnv _ isQuoted (List result) expanded@(List (List l : ls)) = do
  lst <- walkExpanded defEnv useEnv renameEnv cleanupEnv True isQuoted (List []) (List l)
  walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List $ result ++ [lst]) (List ls)

walkExpanded defEnv useEnv renameEnv cleanupEnv _ isQuoted (List result) transform@(List ((Vector v) : vs)) = do
  List lst <- walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List []) (List $ elems v)
  walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List $ result ++ [asVector lst]) (List vs)

walkExpanded defEnv useEnv renameEnv cleanupEnv _ isQuoted (List result) transform@(List ((DottedList ds d) : ts)) = do
  List ls <- walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List []) (List ds)
  l <- walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List []) d
  walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List $ result ++ [DottedList ls l]) (List ts)

walkExpanded defEnv useEnv renameEnv cleanupEnv startOfList inputIsQuoted (List result) transform@(List (Atom aa : ts)) = do
  
 Atom a <- expandAtom renameEnv (Atom aa)

 {- TODO: need to work through how to handle quoting an atoms that are renamed
 Atom aTmp <- expandAtom renameEnv (Atom aa)
 let a = if inputIsQuoted
            then aa
            else aTmp
-}

 -- If a macro is quoted, keep track of it and do not invoke rules below for
 -- procedure abstraction or macro calls 
 let isQuoted = inputIsQuoted || (a == "quote")

 isDefinedAsMacro <- liftIO $ isNamespacedRecBound useEnv macroNamespace a

 if a == "lambda" && not isQuoted -- Placed here, the lambda primitive trumps a macro of the same name... (desired behavior?)
     then do
       case transform of
         List (Atom _ : List vars : body) -> do
           -- Create a new Env for this, so args of the same name do not overwrite those in the current Env
           env <- liftIO $ extendEnv renameEnv []
           renamedVars <- markBoundIdentifiers env cleanupEnv vars []
           walkExpanded defEnv useEnv env cleanupEnv False isQuoted (List [Atom "lambda", renamedVars]) (List body)
         -- lambda is malformed, just transform as normal atom...
         otherwise -> walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List $ result ++ [Atom a]) (List ts)
     else if isDefinedAsMacro && not isQuoted
             then do
               Syntax _ identifiers rules <- getNamespacedVar useEnv macroNamespace a
{- TODO: below should be defEnv instead of useEnv, will be switching over later -}

               -- A child renameEnv is not created because for a macro call there is no way an
               -- renamed identifier inserted by the macro could override one in the outer env.
               --
               -- This is because the macro renames non-matched identifiers and stores mappings
               -- from the {rename ==> original}. Each new name is unique by definition, so
               -- no conflicts are possible.
               macroTransform useEnv renameEnv cleanupEnv (List identifiers) rules (List (Atom a : ts))
             else walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List $ result ++ [Atom a]) (List ts)
-- TODO: use startOfList for lambda/macro call processing
{- OLD CODE: 
  -- TODO: more to come w/Clinger's algorithm...
  isDefined <- liftIO $ isBound renameEnv aa -- TODO: Not entirely correct; for example if a macro and var 
  case (trace ("walker found atom: " ++ aa ++ " isRenamed = " ++ show isDefined) isDefined) of
    True -> do
      expanded <- getVar renameEnv aa
      walkExpanded defEnv useEnv renameEnv False (List $ result ++ [expanded]) (List ts)
    False -> walkExpanded defEnv useEnv renameEnv False (List $ result ++ [Atom aa]) (List ts)
-}

-- Transform anything else as itself...
walkExpanded defEnv useEnv renameEnv cleanupEnv _ isQuoted (List result) (List (t : ts)) = do
  walkExpanded defEnv useEnv renameEnv cleanupEnv False isQuoted (List $ result ++ [t]) (List ts)

-- Base case - empty transform
walkExpanded defEnv useEnv renameEnv cleanupEnv _ _ result@(List _) (List []) = do
  return (trace ("returning = " ++ show result) result)

-- Single atom, rename (if necessary) and return
walkExpanded defEnv useEnv renameEnv cleanupEnv _ isQuoted _ (Atom a) = do
  expandAtom renameEnv (Atom a)

-- TODO (?):
-- Transform is a single var, just look it up.
--walkExpanded defEnv useEnv renameEnv _ (Atom transform) = do

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
walkExpanded defEnv useEnv renameEnv cleanupEnv _ _ _ transform = return transform

-- |Accept a list of bound identifiers from a lambda expression, and rename them
--  Returns a list of the renamed identifiers as well as marking those identifiers
--  in the given environment, so they can be renamed during expansion.
markBoundIdentifiers :: Env -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
markBoundIdentifiers env cleanupEnv (Atom v : vs) renamedVars = do
  Atom renamed <- _gensym v
  _ <- defineVar (trace ("renamed var:" ++ v ++ " to: " ++ show renamed) env) v $ Atom renamed
  _ <- defineVar cleanupEnv renamed $ Atom v
  markBoundIdentifiers env cleanupEnv vs $ renamedVars ++ [Atom renamed]
markBoundIdentifiers env cleanupEnv (_: vs) renamedVars = markBoundIdentifiers env cleanupEnv vs renamedVars
markBoundIdentifiers _ _ [] renamedVars = return $ List renamedVars
--markBoundIdentifiers _ input _ = throwError $ BadSpecialForm "Unexpected input to markBoundIdentifiers" $ List input 

-- |Recursively expand an atom that may have been renamed multiple times
expandAtom :: Env -> LispVal -> IOThrowsError LispVal
expandAtom renameEnv (Atom a) = do
  isDefined <- liftIO $ isRecBound renameEnv a -- Search parent Env's also
  if isDefined 
     then do
       expanded <- getVar renameEnv a
       return expanded -- TODO: temporarily disabling this; just expand once. expandAtom renameEnv expanded -- Recursively expand
     else return $ Atom a 
expandAtom renameEnv a = return a

-- |Clean up any remaining renamed variables in the expanded code
--  This is ugly but seems like a simple way to verify any remaining renames are cleaned up. However, it would
--  be nice if this was unnecessary. 
--
--  TODO: IMPORTANT!!!!!
--
--        this will never work when using the renameEnv from walk, because that env binds
--        (old name => new name) in order to clean up any new names prior to eval, there would
--        need to be another environment with the reverse mappings
--   ALSO, due to parent Env logic going on, these bindings need to be in some sort of
--   'master' env that transcends those env's and maps all gensyms back to their original symbols
--
cleanExpanded :: Env -> LispVal -> LispVal -> IOThrowsError LispVal

cleanExpanded renameEnv (List result) expanded@(List (List l : ls)) = do
  lst <- cleanExpanded renameEnv (List []) (List l)
  cleanExpanded renameEnv (List $ result ++ [lst]) (List ls)

cleanExpanded renameEnv (List result) transform@(List ((Vector v) : vs)) = do
  List lst <- cleanExpanded renameEnv (List []) (List $ elems v)
  cleanExpanded renameEnv (List $ result ++ [asVector lst]) (List vs)

cleanExpanded renameEnv (List result) transform@(List ((DottedList ds d) : ts)) = do
  List ls <- cleanExpanded renameEnv (List []) (List ds)
  l <- cleanExpanded renameEnv (List []) d
  cleanExpanded renameEnv (List $ result ++ [DottedList ls l]) (List ts)

cleanExpanded renameEnv (List result) transform@(List (Atom a : ts)) = do
  expanded <- tmpexpandAtom renameEnv $ Atom a
--  expanded <- expandAtom renameEnv $ Atom a
  cleanExpanded (trace ("cleanup, a = " ++ show a ++ " exp = " ++ show expanded) renameEnv) (List $ result ++ [expanded]) (List ts)
 where
  -- TODO: if this works, figure out a way to simplify this code (perhaps consolidate with expandAtom)
  tmpexpandAtom :: Env -> LispVal -> IOThrowsError LispVal
  tmpexpandAtom renameEnv (Atom a) = do
    isDefined <- liftIO $ isRecBound renameEnv a -- Search parent Env's also
    if isDefined 
       then do
         expanded <- getVar renameEnv a
         tmpexpandAtom renameEnv expanded -- Recursively expand
       else return $ Atom a 
  tmpexpandAtom renameEnv a = return a

-- Transform anything else as itself...
cleanExpanded renameEnv (List result) (List (t : ts)) = do
  cleanExpanded renameEnv (List $ result ++ [t]) (List ts)

-- Base case - empty transform
cleanExpanded renameEnv result@(List _) (List []) = do
  return result

-- TODO (?):
-- Transform is a single var, just look it up.
--cleanExpanded renameEnv _ (Atom transform) = do

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
cleanExpanded renameEnv _ transform = return transform


{- |Transform input by walking the tranform structure and creating a new structure
    with the same form, replacing identifiers in the tranform with those bound in localEnv 

TODO: this is essentially Clinger's rewrite step, however it needs to be extended to do all that is req'd, including:

 - renaming of free variables
 - collecting a list of variables that are renamed (perhaps in a new Env parameter)
 - divert-ing bindings back into the resultant Env (not 100% clear on this, need to be careful)
 - TODO: anything else?
-}
transformRule :: Env        -- ^ Outer, enclosing environment
              -> Env        -- ^ Environment local to the macro containing pattern variables
              -> Env        -- ^ Environment local to the macro containing renamed variables
              -> Env        -- ^ Environment local to the macro used to cleanup any left-over renamed vars 
              -> Int        -- ^ ellipsisLevel - Nesting level of the zero-to-many match, or 0 if none
              -> [Int]      -- ^ ellipsisIndex - The index at each ellipsisLevel. This is used to read data stored in
                            --                   pattern variables.
              -> LispVal    -- ^ Resultant (transformed) value. 
                            -- ^ Must be a parameter as it mutates with each transform call
              -> LispVal    -- ^ The macro transformation, read out one atom at a time and rewritten to result
              -> IOThrowsError LispVal

-- Recursively transform a list
transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) transform@(List (List l : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if (nextHasEllipsis)
     then do
             curT <- transformRule outerEnv localEnv renameEnv cleanupEnv level idx (List []) (List l)
             case (curT) of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex result $ tail ts
               List _ -> transformRule outerEnv localEnv renameEnv cleanupEnv 
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [curT]) transform
               _ -> throwError $ Default "Unexpected error"
     else do
             lst <- transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List []) (List l)
             case lst of
                  List _ -> transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ [lst]) (List ts)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "Macro transform error" $ List [lst, (List l), Number $ toInteger ellipsisLevel]

-- Recursively transform a vector by processing it as a list
-- FUTURE: can this code be consolidated with the list code?
transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) transform@(List ((Vector v) : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
     then do
             -- Idea here is that we need to handle case where you have (vector ...) - EG: (#(var step) ...)
             curT <- transformRule outerEnv localEnv renameEnv cleanupEnv level idx (List []) (List $ elems v)
--             case (trace ("curT = " ++ show curT) curT) of
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex result $ tail ts
               List t -> transformRule outerEnv localEnv renameEnv cleanupEnv 
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [asVector t]) transform
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List []) (List $ elems v)
             case lst of
                  List l -> transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ [asVector l]) (List ts)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [Vector v]), Number $ toInteger ellipsisLevel]

-- Recursively transform an improper list
transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) transform@(List (dl@(DottedList _ _) : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
--  if (trace ("trans Pair: " ++ show transform ++ " lvl = " ++ show ellipsisLevel ++ " idx = " ++ show ellipsisIndex) nextHasEllipsis)
     then do
             -- Idea here is that we need to handle case where you have (pair ...) - EG: ((var . step) ...)
             curT <- transformDottedList outerEnv localEnv renameEnv cleanupEnv level idx (List []) (List [dl])
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex result $ tail ts 
               List t -> transformRule outerEnv localEnv renameEnv cleanupEnv 
                          ellipsisLevel -- Do not increment level, just wait until next iteration where incremented above
                          idx -- Keep incrementing each time
                         (List $ result ++ t) transform
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformDottedList outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List []) (List [dl])
             case lst of
                  List l -> transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ l) (List ts)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [dl]), Number $ toInteger ellipsisLevel]

-- Transform an atom by attempting to look it up as a var...
transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) transform@(List (Atom a : ts)) = do
  isDefined <- liftIO $ isBound localEnv a
  if hasEllipsis
    then ellipsisHere isDefined
    else noEllipsis isDefined

  where
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
    ellipsisHere isDefined = do
        if isDefined
             then do 
                    isImproperPattern <- loadNamespacedBool "improper pattern"
                    isImproperInput <- loadNamespacedBool "improper input"
                    -- Load variable and ensure it is a list
                    var <- getVar localEnv a
                    case var of
                      -- add all elements of the list into result
                      List _ -> do case (appendNil (Matches.getData var ellipsisIndex) isImproperPattern isImproperInput) of
                                     List aa -> transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ aa) (List $ tail ts)
                                     _ -> -- No matches for var
                                          continueTransform outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex result $ tail ts

{- TODO:                      Nil input -> do -- Var lexically defined outside of macro, load from there
--
-- notes: this could be a problem, because we need to signal the end of the ... and do not want an infinite loop.
--        but we want the lexical value as well. need to think about this in more detail to get a truly workable solution
--
                                  v <- getVar outerEnv input
                                  transformRule outerEnv localEnv renameEnv ellipsisIndex (List $ result ++ [v]) (List $ tail ts) unused -}
                      Nil "" -> -- No matches, keep going
                                continueTransform outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex result $ tail ts
                      v@(_) -> transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ [v]) (List $ tail ts)
             else -- Matched 0 times, skip it
                  transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) (List $ tail ts)

    noEllipsis isDefined = do
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
-- TODO: I think the outerEnv should be accessed by the walker, and not within rewrite as is done below...
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
                  -- Rename each encountered symbol, but the trick is that we want to give
                  -- the same symbol the same new name if it is found more than once, so...
                  -- we need to keep track of the var in two environments to map both ways 
                  -- between the original name and the new name.
                  isAlreadyRenamed <- liftIO $ isNamespacedBound localEnv "renamed" a
                  if isAlreadyRenamed
                     then do
                       renamed <- getNamespacedVar localEnv "renamed" a
                       return (trace ("macro renamed var:" ++ a ++ " to: " ++ show renamed) renamed)
                     else do
                       Atom renamed <- _gensym a
                       _ <- defineNamespacedVar localEnv "renamed" a $ Atom renamed -- Keep track of vars that are renamed; maintain reverse mapping
                       _ <- defineVar cleanupEnv renamed $ Atom a -- Global record for final cleanup of macro
                       _ <- defineVar (trace ("macro renamed var:" ++ a ++ " to: " ++ show renamed) renameEnv) renamed $ Atom a -- Keep for Clinger
                       return $ Atom renamed
      case t of
         Nil "var not defined in pattern" -> 
            if ellipsisLevel > 0
               then return t
               else continueTransformWith result -- nary match in the pattern but used as list in transform; keep going
         Nil "var (pair) not defined in pattern" -> 
            if ellipsisLevel > 0
               then return t
                    -- nary match in pattern as part of an improper list but used as list here; append the empty list
               else continueTransformWith $ result ++ [List []]
         Nil _ -> return t
         List l -> do
            -- What's going on here is that if the pattern was a dotted list but the transform is not, we
            -- need to "lift" the input up out of a list.
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
                    renameEnv cleanupEnv 
                    ellipsisLevel 
                    ellipsisIndex 
                   (List $ results)
                   (List ts)

-- Transform anything else as itself...
transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) (List (t : ts)) = do
  transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ [t]) (List ts) 

-- Base case - empty transform
transformRule _ _ _ _ _ _ result@(List _) (List []) = do
  return result

-- Transform is a single var, just look it up.
transformRule _ localEnv renameEnv cleanupEnv _ _ _ (Atom transform) = do
-- TODO: really? What if the atom is an identifier? Don't we need to rename it?
  v <- getVar localEnv (trace ("tR - single atom = " ++ transform) transform)
  return v

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
transformRule _ _ _ _ _ _ _ transform = return transform

-- | A helper function for transforming an improper list
transformDottedList :: Env -> Env -> Env -> Env -> Int -> [Int] -> LispVal -> LispVal -> IOThrowsError LispVal
transformDottedList outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List result) (List (DottedList ds d : ts)) = do
          lsto <- transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List []) (List ds)
          case lsto of
            List lst -> do
              -- Similar logic to the parser is applied here, where
              -- results are transformed into either a list or pair depending upon whether
              -- they form a proper list
              --
              -- d is an n-ary match, per Issue #34
              r <- transformRule outerEnv localEnv renameEnv cleanupEnv 
                                 ellipsisLevel -- OK not to increment here, this is accounted for later on
                                 ellipsisIndex -- Same as above 
                                 (List []) 
                                 (List [d, Atom "..."])
              case r of
                   -- Trailing symbol in the pattern may be neglected in the transform, so skip it...
                   List [] ->
                       transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts)
                   Nil _ ->  -- Same as above, no match for d, so skip it 
                       transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts)
                   List rst -> do
                       transformRule outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex 
                                    (buildTransformedCode result lst rst) (List ts)
                   _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
            Nil _ -> return $ Nil ""
            _ -> throwError $ BadSpecialForm "Macro transform error processing pair" $ DottedList ds d
 where 
   -- Transform code as either a proper or improper list depending upon the data
   -- These are rather crude methods of 'cons'-ing everything together... are all cases accounted for?
   buildTransformedCode results ps p = do 
     case p of
        [List []] -> List $ results ++ [List ps]         -- Proper list has null list at the end
        [List l@(Atom "unquote" : _ )] -> List $ results ++ [DottedList ps $ List l] -- Special case from parser. 
        [List ls] -> List $ results ++ [List $ ps ++ ls] -- Again, convert to proper list because a proper list is at end
        [l] -> List $ results ++ [DottedList ps l]
        ls -> do
            -- Same concepts as above, but here we check the last entry of a list of elements
            -- FUTURE: should be able to use a common function to encapsulate logic above and below
            case last ls of
              List [] -> List $ results ++ [List $ ps ++ init ls]
              List lls -> List $ results ++ [List $ ps ++ (init ls) ++ lls]
              t -> List $ results ++ [DottedList (ps ++ init ls) t]


transformDottedList _ _ _ _ _ _ _ _ = throwError $ Default "Unexpected error in transformDottedList"

-- |Continue transforming after a preceding match has ended 
continueTransform :: Env -> Env -> Env -> Env -> Int -> [Int] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
continueTransform outerEnv localEnv renameEnv cleanupEnv ellipsisLevel ellipsisIndex result remaining = do
    if not (null remaining)
       then transformRule outerEnv 
                          localEnv 
                          renameEnv
                          cleanupEnv
                          ellipsisLevel 
                          ellipsisIndex 
                         (List result) 
                         (List $ remaining)
       else if length result > 0 
               then return $ List result
               else if ellipsisLevel > 0 
                       then return $ Nil ""  -- Nothing remains, no match
                       else return $ List [] -- Nothing remains, return empty list

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

-- |Convert a list of lisp values to a vector
asVector :: [LispVal] -> LispVal
asVector lst = (Vector $ (listArray (0, length lst - 1)) lst)

