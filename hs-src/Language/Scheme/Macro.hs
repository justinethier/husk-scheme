{- |
Module      : Language.Scheme.Macro
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains code for hygienic macros.

Hygienic macros are implemented using the algorithm from the paper
Macros That Work by William Clinger and Jonathan Rees. During 
transformation, the following components are considered:

 - Pattern (part of a rule that matches input)

 - Transform (what the macro expands into)

 - Literal Identifiers (from the macro definition)

 - Input (the actual code in the user's program)

 - Environments of macro definition and macro use

At a high level, macro transformation is broken down into the following steps:

 (0) Walk the input code looking for a macro definition or macro call.
 
 (1) If a macro call is found, search for a rule that matches the input.
     During this process any pattern variables in the input are loaded 
     into a temporary environment

 (2) If a rule matches, transcribe the rule's template by walking the 
     template, inserting pattern variables and renaming free identifiers 
     as needed.

 (3) Walk the expanded code, checking for each of the cases from Macros That Work. If a 
     case is found (such as a macro call or procedure abstraction) then the appropriate 
     handler will be called to deal with it.
-}

module Language.Scheme.Macro
    (
      expand
    , macroEval
    , loadMacros  
    , getDivertedVars 
    ) where
import Language.Scheme.Types
import Language.Scheme.Variables
import Language.Scheme.Macro.ExplicitRenaming
import qualified Language.Scheme.Macro.Matches as Matches
import Language.Scheme.Primitives (_gensym)
import Control.Monad.Error
import Data.Array
-- import Debug.Trace -- Only req'd to support trace, can be disabled at any time...

{-
 Implementation notes:

 Nice FAQ regarding macro's, points out some of the limitations of current implementation
 http://community.schemewiki.org/?scheme-faq-macros

 -}

--
-- Notes regarding other side of hygiene.
--
-- !!!
-- Turns out this was unnecessary because it is sufficient to simply save the environment of
-- definition directly. Even though this causes problems with define, it seems that is how
-- other Schemes work, so it will stay that way for now. This note is being kept for the 
-- moment although it should probably go away... in any case only take it as brainstorming
-- notes and nothing further:
-- !!!
--
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
-- extended env if necessary. I am a bit concerned about subtle errors occurring if any
-- continuations in the chain are not updated and still have the old environment in them.
-- It may be tricky to get this right. But otherwise the change *should* be straightforward.


-- |Get a list of variables that the macro hygiene 
--  subsystem diverted back into the calling environment.
--
--  This is a specialized function that is only
--  mean to be used by the husk compiler.
getDivertedVars :: Env -> IOThrowsError [LispVal]
getDivertedVars env = do
  List tmp <- getNamespacedVar env ' ' "diverted"
  return tmp

clearDivertedVars :: Env -> IOThrowsError LispVal
clearDivertedVars env = defineNamespacedVar env ' ' "diverted" $ List []

-- |Examines the input AST to see if it is a macro call. 
--  If a macro call is found, the code is expanded.
--  Otherwise the input is returned unchanged.
macroEval :: Env        -- ^Current environment for the AST
          -> LispVal    -- ^AST to search
          -> (LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal) -- ^Eval func

          -> IOThrowsError LispVal -- ^Transformed AST containing an
                                   -- expanded macro if found

{- Inspect code for macros
 -
 - Only a list form is required because a pattern may only consist
 - of a list here. From the spec:
 -
 - "The <pattern> in a <syntax rule> is a list <pattern> that 
 -  begins with the keyword for the macro." 
 -
 -}
macroEval env lisp@(List (Atom x : _)) apply = do
  -- Keep track of diverted variables
  _ <- clearDivertedVars env
  _macroEval env lisp apply
macroEval env lisp apply = _macroEval env lisp apply

-- |Do the actual work for the 'macroEval' wrapper func
_macroEval env lisp@(List (Atom x : _)) apply = do
  -- Note: If there is a procedure of the same name it will be shadowed by the macro.
  var <- getNamespacedVar' env macroNamespace x
  -- DEBUG: var <- (trace ("expand: " ++ x) getNamespacedVar') env macroNamespace x
  case var of
    -- Explicit Renaming
    Just (SyntaxExplicitRenaming transformer@(Func _ _ _ _)) -> do
      renameEnv <- liftIO $ nullEnv -- Local environment used just for this
      expanded <- explicitRenamingTransform env renameEnv 
                                          lisp transformer apply
      _macroEval env expanded apply

    -- Syntax Rules
    Just (Syntax (Just defEnv) _ definedInMacro ellipsis identifiers rules) -> do
      renameEnv <- liftIO $ nullEnv -- Local environment used just for this
                                    -- invocation to hold renamed variables
      cleanupEnv <- liftIO $ nullEnv -- Local environment used just for 
                                     -- this invocation to hold new symbols
                                     -- introduced by renaming. We can use
                                     -- this to clean up any left after 
                                     -- transformation

      -- Transform the input and then call macroEval again, 
      -- since a macro may be contained within...
      expanded <- macroTransform defEnv env env renameEnv cleanupEnv 
                                 definedInMacro 
                                (List identifiers) rules lisp apply
                                ellipsis
      _macroEval env expanded apply
      -- Useful debug to see all exp's:
      -- macroEval env (trace ("exp = " ++ show expanded) expanded)
    Nothing -> return lisp

-- No macro to process, just return code as it is...
_macroEval _ lisp@(_) _ = return lisp

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
macroTransform :: 
     Env 
  -> Env 
  -> Env 
  -> Env 
  -> Env 
  -> Bool 
  -> LispVal 
  -> [LispVal] 
  -> LispVal 
  -> (LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal) -- ^Apply func
  -> String -- ^ Ellipsis symbol
  -> IOThrowsError LispVal
macroTransform defEnv env divertEnv renameEnv cleanupEnv dim identifiers (rule@(List _) : rs) input apply esym = do
  localEnv <- liftIO $ nullEnv -- Local environment used just for this invocation
                               -- to hold pattern variables
  result <- matchRule defEnv env divertEnv dim identifiers localEnv renameEnv cleanupEnv rule input esym
  case (result) of
    -- No match, check the next rule
    Nil _ -> macroTransform defEnv env divertEnv renameEnv cleanupEnv dim identifiers rs input apply esym
    _ -> do
        -- Walk the resulting code, performing the Clinger algorithm's 4 components
        walkExpanded defEnv env divertEnv renameEnv cleanupEnv dim True False (List []) (result) apply

-- Ran out of rules to match...
macroTransform _ _ _ _ _ _ _ _ input _ _ = throwError $ BadSpecialForm "Input does not match a macro pattern" input

-- Determine if the next element in a list matches 0-to-n times due to an ellipsis
macroElementMatchesMany :: LispVal -> String -> Bool
macroElementMatchesMany args@(List (_ : ps)) ellipsisSym = do
  if not (null ps)
     then (head ps) == (Atom ellipsisSym)
     else False
macroElementMatchesMany _ _ = False

{- Given input, determine if that input matches any rules
@return Transformed code, or Nil if no rules match -}
matchRule :: Env -> Env -> Env -> Bool -> LispVal -> Env -> Env -> Env -> LispVal -> LispVal -> String -> IOThrowsError LispVal
matchRule defEnv outerEnv divertEnv dim identifiers localEnv renameEnv cleanupEnv (List [pattern, template]) (List inputVar) esym = do
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
                transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym 0 [] (List []) template
      _ -> throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ String $ show p

 where
   -- A pair at the outmost level must be transformed to use the ellipsis, 
   -- or else its nary match will not work properly during pattern matching. 
   checkPattern ps@(DottedList ds d : _) is True = do
     case is of
       (DottedList _ _ : _) -> do 
         loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers 
                                  (List $ ds ++ [d, Atom esym])
                                  (List is)
                                   0 []
                                  (flagDottedLists [] (False, False) 0)
                                  esym
       (List _ : _) -> do 
         loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers 
                                  (List $ ds ++ [d, Atom esym])
                                  (List is)
                                   0 []
                                  (flagDottedLists [] (True, False) 0)
                                  esym
       _ -> loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers (List ps) (List is) 0 [] [] esym

   -- No pair, immediately begin matching
   checkPattern ps is _ = loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers (List ps) (List is) 0 [] [] esym

matchRule _ _ _ _ _ _ _ _ rule input _ = do
  throwError $ BadSpecialForm "Malformed rule in syntax-rules" $ List [Atom "rule: ", rule, Atom "input: ", input]

{- loadLocal - Determine if pattern matches input, loading input into pattern variables as we go,
in preparation for macro transformation. -}
loadLocal :: Env -> Env -> Env -> Env -> Env -> LispVal -> LispVal -> LispVal -> Int -> [Int] -> [(Bool, Bool)] -> String -> IOThrowsError LispVal
loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers pattern input ellipsisLevel ellipsisIndex listFlags esym = do
  case (pattern, input) of

       ((DottedList ps p), (DottedList isRaw iRaw)) -> do
         
         -- Split input into two sections: 
         --   is - required inputs that must be present
         --   i  - variable length inputs to each compare against p 
         let isSplit = splitAt (length ps) isRaw
         let is = fst isSplit
         let i = (snd isSplit) ++ [iRaw]

         result <- loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags esym
         case result of
            Bool True -> --  By matching on an elipsis we force the code 
                         --  to match pagainst all elements in i. 
                         loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers 
                                  (List $ [p, Atom esym]) 
                                  (List i)
                                   ellipsisLevel -- Incremented in the list/list match below
                                   ellipsisIndex
                                   (flagDottedLists listFlags (True, True) $ length ellipsisIndex)
                                   esym
            _ -> return $ Bool False

       (List (p : ps), List (i : is)) -> do -- check first input against first pattern, recurse...

         let nextHasEllipsis = macroElementMatchesMany pattern esym
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
         status <- checkLocal defEnv outerEnv divertEnv (localEnv) renameEnv identifiers level idx p i listFlags esym
         case (status) of
              -- No match
              Bool False -> if nextHasEllipsis
                                {- No match, must be finished with ...
                                Move past it, but keep the same input. -}
                                then do
                                        case ps of
                                          [Atom esym] -> return $ Bool True -- An otherwise empty list, so just let the caller know match is done
                                          _ -> loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers (List $ tail ps) (List (i : is)) ellipsisLevel ellipsisIndex listFlags esym
                                else return $ Bool False
              -- There was a match
              _ -> if nextHasEllipsis
                      then 
                           loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers pattern (List is)
                            ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                            idx -- Must keep index since it is incremented each time
                            listFlags
                            esym
                      else loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers (List ps) (List is) ellipsisLevel ellipsisIndex listFlags esym

       -- Base case - All data processed
       (List [], List []) -> return $ Bool True

       -- Ran out of input to process
       (List (_ : _), List []) -> do
         if (macroElementMatchesMany pattern esym)
            then do
              -- Ensure any patterns that are not present in the input still
              -- have their variables initialized so they are ready during transformation
              -- Note:
              -- Appending to eIndex to compensate for fact we are outside the list containing the nary match 
              let flags = getListFlags (ellipsisIndex ++ [0]) listFlags
              flagUnmatchedVars defEnv outerEnv localEnv identifiers pattern (fst flags) esym
            else return $ Bool False

       -- Pattern ran out, but there is still input. No match.
       (List [], _) -> return $ Bool False

       -- Check input against pattern (both should be single var)
       (_, _) -> checkLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex pattern input listFlags esym

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
flagUnmatchedVars :: Env -> Env -> Env -> LispVal -> LispVal -> Bool -> String -> IOThrowsError LispVal 

flagUnmatchedVars defEnv outerEnv localEnv identifiers (DottedList ps p) partOfImproperPattern esym = do
  flagUnmatchedVars defEnv outerEnv localEnv identifiers (List $ ps ++ [p]) partOfImproperPattern esym

flagUnmatchedVars defEnv outerEnv localEnv identifiers (Vector p) partOfImproperPattern esym = do
  flagUnmatchedVars defEnv outerEnv localEnv identifiers (List $ elems p) partOfImproperPattern esym

flagUnmatchedVars _ _ _ _ (List []) _ _ = return $ Bool True 

flagUnmatchedVars defEnv outerEnv localEnv identifiers (List (p : ps)) partOfImproperPattern esym = do
  _ <- flagUnmatchedVars defEnv outerEnv localEnv identifiers p partOfImproperPattern esym
  flagUnmatchedVars defEnv outerEnv localEnv identifiers (List ps) partOfImproperPattern esym

flagUnmatchedVars defEnv outerEnv localEnv identifiers (Atom p) partOfImproperPattern esym =
  if p == esym
     then return $ Bool True
     else flagUnmatchedAtom defEnv outerEnv localEnv identifiers p partOfImproperPattern

flagUnmatchedVars _ _ _ _ _ _ _ = return $ Bool True 

-- |Flag an atom that did not have any matching input
--
--  Note that an atom may not be flagged in certain cases, for example if
--  the var is lexically defined in the outer environment. This logic
--  matches that in the pattern matching code.
flagUnmatchedAtom :: Env -> Env -> Env -> LispVal -> String -> Bool -> IOThrowsError LispVal 
flagUnmatchedAtom defEnv outerEnv localEnv identifiers p improperListFlag = do
  isDefined <- liftIO $ isBound localEnv p
  isIdent <- findAtom (Atom p) identifiers
  if isDefined 
     -- Var already defined, skip it...
     then continueFlagging
     else case isIdent of
             Bool True -> do
                           matches <- identifierMatches defEnv outerEnv p
                           if not matches 
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
  defineNamespacedVar localEnv 
                      '_' -- "unmatched nary pattern variable" 
                      var $ Bool $ improperListFlag

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

-- ^ Check pattern against input to determine if there is a match
checkLocal :: Env            -- ^ Environment where the macro was defined
           -> Env            -- ^ Outer environment where this macro was called
           -> Env            -- ^ Outer env that the macro may divert values back to
           -> Env            -- ^ Local environment used to store temporary variables for macro processing
           -> Env            -- ^ Local environment used to store vars that have been renamed by the macro subsystem 
           -> LispVal        -- ^ List of identifiers specified in the syntax-rules
           -> Int            -- ^ Current nary (ellipsis) level
           -> [Int]          -- ^ Ellipsis Index, keeps track of the current nary (ellipsis) depth at each level 
           -> LispVal        -- ^ Pattern to match
           -> LispVal        -- ^ Input to be matched
           -> [(Bool, Bool)] -- ^ Flags to determine whether input pattern/variables are proper lists
           -> String         -- ^ Symbol used to specify ellipsis (IE, 0 or many match)
           -> IOThrowsError LispVal
checkLocal _ _ _ _ _ _ _ _ (Bool pattern) (Bool input) _ _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ _ _ (Number pattern) (Number input) _ _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ _ _ (Float pattern) (Float input) _ _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ _ _ (String pattern) (String input) _ _ = return $ Bool $ pattern == input
checkLocal _ _ _ _ _ _ _ _ (Char pattern) (Char input) _ _ = return $ Bool $ pattern == input
checkLocal defEnv outerEnv _ localEnv renameEnv identifiers ellipsisLevel ellipsisIndex (Atom pattern) input listFlags esym = do

  -- TODO: 
  --
  -- The code below uses this rename boolean as a factor to determine whether a named
  -- identifier has been redefined and thus should not match itself in the input. But the
  -- thing is, the actual code is supposed to compare the value at macro definition
  -- time with the value in the environment of use (outerEnv) to make this determination.
  -- So what is below is close but not truly correct.
  --
  isRenamed <- liftIO $ isRecBound renameEnv (pattern)
  doesIdentMatch <- identifierMatches defEnv outerEnv pattern

  if (ellipsisLevel) > 0
     {- FUTURE: may be able to simplify both cases below by using a
     lambda function to store the 'save' actions -}

             -- Var is part of a 0-to-many match, store up in a list...
     then do isDefined <- liftIO $ isBound localEnv pattern
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
                               then if (doesIdentMatch) && (not isRenamed)
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
         --isLexicallyDefinedPatternVar <- liftIO $ isBound outerEnv pattern -- Var defined in scope outside macro
         case (isIdent) of
            -- Fail the match if pattern is a literal identifier and input does not match
            Bool True -> do
                case input of
                    Atom inpt -> do
                        -- Pattern/Input are atoms; both must match
                        if (pattern == inpt && (doesIdentMatch)) && (not isRenamed) -- Regarding lex binding; see above, sec 4.3.2 from spec
--                        if (pattern == inpt && (not isLexicallyDefinedPatternVar)) && (not isRenamed) -- Regarding lex binding; see above, sec 4.3.2 from spec
                           then do _ <- defineVar localEnv pattern input
                                   return $ Bool True
                           else return $ (Bool False)
                    -- Pattern/Input cannot match because input is not an atom
                    _ -> return $ (Bool False)

            -- No literal identifier, just load up the var
            _ -> do _ <- defineVar localEnv pattern input
                    return $ Bool True
    where
      -- Store pattern variable in a nested list
      -- FUTURE: ellipsisLevel should probably be used here for validation.
      -- 
      -- some notes:
      --  (above): need to flag the ellipsisLevel of this variable.
      --  also, it is an error if, for an existing var, ellipsisLevel input does not match the var's stored level
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
        _ <- defineNamespacedVar localEnv 'p' {-"improper pattern"-} pat $ Bool $ fst flags
        defineNamespacedVar localEnv 'i' {-"improper input"-} pat $ Bool $ snd flags

checkLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex (Vector p) (Vector i) flags esym =
  -- For vectors, just use list match for now, since vector input matching just requires a
  -- subset of that behavior. Should be OK since parser would catch problems with trying
  -- to add pair syntax to a vector declaration. -}
  loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers (List $ elems p) (List $ elems i) ellipsisLevel ellipsisIndex flags esym

checkLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex pattern@(DottedList _ _) input@(DottedList _ _) flags esym =
  loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers pattern input ellipsisLevel ellipsisIndex flags esym

checkLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex (DottedList ps p) input@(List (_ : _)) flags esym = do
  loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers 
                                  (List $ ps ++ [p, Atom esym])
                                  input
                                   ellipsisLevel -- Incremented in the list/list match below
                                   ellipsisIndex
                                   (flagDottedLists flags (True, False) $ length ellipsisIndex) 
                                   esym
checkLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers ellipsisLevel ellipsisIndex pattern@(List _) input@(List _) flags esym =
  loadLocal defEnv outerEnv divertEnv localEnv renameEnv identifiers pattern input ellipsisLevel ellipsisIndex flags esym

checkLocal _ _ _ _ _ _ _ _ _ _ _ _ = return $ Bool False

-- |Determine if an identifier in a pattern matches an identifier of the same
--  name in the input.
--
-- Note that identifiers are lexically scoped: bindings that intervene
-- between the definition and use of a macro may cause match failure
--
-- TODO: what if var is a macro or a special form?
--
-- TODO: what about vars that are introduced during macro expansion, that are not
-- yet defined in an Env? This may be a future TBD
--
identifierMatches :: Env -> Env -> String -> IOThrowsError Bool
identifierMatches defEnv useEnv ident = do
  atDef <- liftIO $ isRecBound defEnv ident
  atUse <- liftIO $ isRecBound useEnv ident
  matchIdent atDef atUse

 where
  matchIdent False False = return True -- Never defined, so match
  matchIdent True True = do -- Defined in both places, check for equality
    d <- getVar defEnv ident
    u <- getVar useEnv ident
    return $ eqVal d u 
  matchIdent _ _ = return False -- Not defined in one place, reject it 

-- |This function walks the given block of code using the macro expansion algorithm,
--  recursively expanding macro calls as they are encountered.
expand :: 
     Env       -- ^Environment of the code being expanded
  -> Bool      -- ^True if the macro was defined within another macro
  -> LispVal   -- ^Code to expand
  -> (LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal) -- ^Apply func
  -> IOThrowsError LispVal -- ^Expanded code
expand env dim code apply = do
  renameEnv <- liftIO $ nullEnv
  cleanupEnv <- liftIO $ nullEnv

-- TODO: not sure if it is a problem to use env for both def and use, however I cannot think
-- of anything else to use below.
--
-- However, I believe this does highlight problems later on where defEnv is taken from the
-- function parameter instead of the Syntax object
--

  -- Keep track of diverted variables
  _ <- clearDivertedVars env
  walkExpanded env env env renameEnv cleanupEnv dim True False (List []) code apply

-- |Walk expanded code per Clinger's algorithm from Macros That Work
walkExpanded :: Env 
  -> Env 
  -> Env 
  -> Env 
  -> Env 
  -> Bool 
  -> Bool 
  -> Bool 
  -> LispVal 
  -> LispVal 
  -> (LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal) -- ^Apply func
  -> IOThrowsError LispVal
walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ isQuoted (List result) (List (List l : ls)) apply = do
  lst <- walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim True isQuoted (List []) (List l) apply
  walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List $ result ++ [lst]) (List ls) apply

walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ isQuoted (List result) (List ((Vector v) : vs)) apply = do
  List lst <- walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List []) (List $ elems v) apply
  walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List $ result ++ [asVector lst]) (List vs) apply

walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ isQuoted (List result) (List ((DottedList ds d) : ts)) apply = do
  List ls <- walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List []) (List ds) apply
  l <- walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List []) d apply
  walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List $ result ++ [DottedList ls l]) (List ts) apply

walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim startOfList inputIsQuoted (List result) (List (Atom aa : ts)) apply = do
  
 Atom a <- expandAtom renameEnv (Atom aa)

 -- If a macro is quoted, keep track of it and do not invoke rules below for
 -- procedure abstraction or macro calls 
 let isQuoted = inputIsQuoted || (a == "quote")

 isDefinedAsMacro <- liftIO $ isNamespacedRecBound useEnv macroNamespace a

 -- (currently) unused conditional variables for below test
 --isDiverted <- liftIO $ isRecBound divertEnv a
 --isMacroBound <- liftIO $ isRecBound renameEnv a
 --isLocalRename <- liftIO $ isNamespacedRecBound renameEnv 'r' {-"renamed"-} a

 -- Determine if we should recursively rename an atom
 -- This code is a bit of a hack/mess at the moment
 if isDefinedAsMacro 
--     || isDiverted
--     || (isMacroBound && not isLocalRename)
--     || not startOfList
     || a == aa -- Prevent an infinite loop
     -- Preserve keywords encountered in the macro 
     -- as each of these is really a special form, and renaming them
     -- would not work because there is nothing to divert back...
     || a == "if"
     || a == "let-syntax" 
     || a == "letrec-syntax" 
     || a == "define-syntax" 
     || a == "define"  
     || a == "set!"
     || a == "lambda"
     || a == "quote"
     || a == "expand"
     || a == "string-set!"
     || a == "set-car!"
     || a == "set-cdr!"
     || a == "vector-set!"
     || a == "hash-table-set!"
     || a == "hash-table-delete!"
    then walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv 
                          dim startOfList inputIsQuoted (List result) a ts isQuoted isDefinedAsMacro apply
    else walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv 
                      dim startOfList inputIsQuoted (List result) (List (Atom a : ts)) apply


-- Transform anything else as itself...
walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ isQuoted (List result) (List (t : ts)) apply = do
  walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False isQuoted (List $ result ++ [t]) (List ts) apply

-- Base case - empty transform
walkExpanded _ _ _ _ _ _ _ _ result@(List _) (List []) _ = return result

-- Single atom, rename (if necessary) and return
walkExpanded _ _ _ renameEnv _ _ _ _ _ (Atom a) _ = expandAtom renameEnv (Atom a)

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
walkExpanded _ _ _ _ _ _ _ _ _ transform _ = return transform

walkExpandedAtom :: Env 
  -> Env 
  -> Env 
  -> Env 
  -> Env 
  -> Bool 
  -> Bool 
  -> Bool 
  -> LispVal 
  -> String 
  -> [LispVal] 
  -> Bool -- is Quoted
  -> Bool -- is defined as macro
  -> (LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal) -- ^Apply func
  -> IOThrowsError LispVal

{- 
Some high-level design notes on how this could be made to work:

Note from http://www.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf

Also, internal deﬁne-syntax forms may appear wherever internal deﬁne forms are
permitted, in which case the deﬁnitions behave as if introduced by letrec-syntax

so we could transform a letrec-syntax form into another using define-syntax.
let-syntax could be handled in the same way, although we would need to walk
the macro to ensure that none of the introduced macros reference each other.


 if (startOfList) && a == "let-syntax" && not isQuoted -- TODO: letrec-syntax, and a better way to organize all this
  then case ts of
    List bindings : body -> do
        bodyEnv <- liftIO $ extendEnv -- TODO: not sure about this... how will this work?
        _ <- loadMacros env bodyEnv bindings
        -- TODO: expand the macro body
    -- TODO: error
  else 
-}

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True inputIsQuoted (List _)
    "let-syntax" 
    (List _bindings : _body)
    False _ apply = do
        bodyEnv <- liftIO $ extendEnv useEnv []
        bodyRenameEnv <- liftIO $ extendEnv renameEnv []
        _ <- loadMacros useEnv bodyEnv (Just bodyRenameEnv) True _bindings
        expanded <- walkExpanded defEnv bodyEnv divertEnv bodyRenameEnv cleanupEnv dim True inputIsQuoted (List [Atom "lambda", List []]) (List _body) apply
        return $ List [expanded]

walkExpandedAtom _ _ _ _ _ _ True _ _ "let-syntax" ts False _ _ = do
  throwError $ BadSpecialForm "Malformed let-syntax expression" $ List (Atom "let-syntax" : ts)

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True inputIsQuoted (List _)
    "letrec-syntax" 
    (List _bindings : _body)
    False _ apply = do
        bodyEnv <- liftIO $ extendEnv useEnv []
        bodyRenameEnv <- liftIO $ extendEnv renameEnv []
        _ <- loadMacros bodyEnv bodyEnv (Just bodyRenameEnv) True _bindings
        expanded <- walkExpanded defEnv bodyEnv divertEnv bodyRenameEnv cleanupEnv dim True inputIsQuoted (List [Atom "lambda", List []]) (List _body) apply
        return $ List [expanded]

walkExpandedAtom _ _ _ _ _ _ True _ _ "letrec-syntax" ts False _ _ = do
  throwError $ BadSpecialForm "Malformed letrec-syntax expression" $ List (Atom "letrec-syntax" : ts)

walkExpandedAtom _ useEnv _ renameEnv _ _ True _ (List _)
    "define-syntax" 
    ([Atom keyword, (List (Atom "syntax-rules" : Atom ellipsis : (List identifiers : rules)))])
    False _ _ = do
        -- Do we need to rename the keyword, or at least take that into account?
        renameEnvClosure <- liftIO $ copyEnv renameEnv
        _ <- defineNamespacedVar useEnv macroNamespace keyword $ Syntax (Just useEnv) (Just renameEnvClosure) True ellipsis identifiers rules
        return $ Nil "" -- Sentinal value
walkExpandedAtom _ useEnv _ renameEnv _ _ True _ (List _)
    "define-syntax" 
    ([Atom keyword, (List (Atom "syntax-rules" : (List identifiers : rules)))])
    False _ _ = do
        -- Do we need to rename the keyword, or at least take that into account?
        renameEnvClosure <- liftIO $ copyEnv renameEnv
        _ <- defineNamespacedVar useEnv macroNamespace keyword $ Syntax (Just useEnv) (Just renameEnvClosure) True "..." identifiers rules
        return $ Nil "" -- Sentinal value
walkExpandedAtom _ useEnv _ renameEnv _ _ True _ (List _)
    "define-syntax" 
    ([Atom keyword, 
       (List [Atom "er-macro-transformer",  
             (List (Atom "lambda" : List fparams : fbody))])])
    False _ _ = do
        f <- makeNormalFunc useEnv fparams fbody 
        _ <- defineNamespacedVar useEnv macroNamespace keyword $ SyntaxExplicitRenaming f
        return $ Nil "" -- Sentinal value
walkExpandedAtom _ _ _ _ _ _ True _ _ "define-syntax" ts False _ _ = do
  throwError $ BadSpecialForm "Malformed define-syntax expression" $ List (Atom "define-syntax" : ts)


{-
 - Notes regarding define and set
 -
TODO: need to call a new function to scan for define (and set! ??) forms. 
if found, need to add an entry to renameEnv (?) so as to get the transLiteral
code to work. otherwise there is no way for that code to know that a (define)
called within a macro is inserting a new binding.
do not actually need to do anything to the (define) form, just mark somehow
that it is inserting a binding for the var
-}

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List _)
    "define" 
    [Atom var, val]
    False _ apply = do
{- It seems like this should be necessary, but it causes problems so it is
   disabled for now...
      isAlreadyRenamed <- liftIO $ isRecBound renameEnv var
      case (isAlreadyRenamed) of
        _ -> do --False -> do -}
          _ <- defineVar renameEnv var $ Atom var
          walk
--        _ -> walk
 where walk = walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False False (List [Atom "define", Atom var]) (List [val]) apply
walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List result) a@"define" ts False _ apply = do
    -- define is malformed, just transform as normal atom...
    walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False False (List $ result ++ [Atom a]) (List ts) apply

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List _)
    "set!" 
    [Atom var, val]
    False _ apply = do
      isLexicalDef <- liftIO $ isRecBound useEnv var
      isAlreadyRenamed <- liftIO $ isRecBound renameEnv var
      case (isLexicalDef, isAlreadyRenamed) of
        -- Only create a new record for this variable if it has not yet been
        -- seen within this macro. Otherwise the existing algorithms will handle
        -- everything just fine...
        (True, False) -> do
           _ <- defineVar renameEnv var $ Atom var
           walk
        _ -> walk
  where
    walk = walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False False (List [Atom "set!"]) (List [Atom var, val]) apply

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List result) a@"set!" ts False _ apply = do
    -- define is malformed, just transform as normal atom...
    walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False False (List $ result ++ [Atom a]) (List ts) apply

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List _)
    "lambda" 
    (List vars : fbody)
    False _ apply = do
-- Placed here, the lambda primitive trumps a macro of the same name... (desired behavior?)
    -- Create a new Env for this, so args of the same name do not overwrite those in the current Env
--    env <- liftIO $ extendEnv (trace ("found procedure abstraction, vars = " ++ show vars ++ "body = " ++ show fbody) renameEnv) []
    env <- liftIO $ extendEnv renameEnv []
    renamedVars <- markBoundIdentifiers env cleanupEnv vars []
    walkExpanded defEnv useEnv divertEnv env cleanupEnv dim True False (List [Atom "lambda", (renamedVars)]) (List fbody) apply

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List result) a@"lambda" ts False _ apply = do
    -- lambda is malformed, just transform as normal atom...
    walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False False (List $ result ++ [Atom a]) (List ts) apply

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim True _ (List result)
    a
    ts 
    False True apply = do
    syn <- getNamespacedVar useEnv macroNamespace a
    case syn of
--
-- Note:
--
-- Why do we assume that defEnv is the same as the one defined for the macro? Should read
-- this out of the Syntax object, right?
--
-- A) I think this is because for a macro with a renameClosure, it may only be defined
--    within another macro. So defEnv is not modified by this macro definition, and
--    there is no need to insert it.
--
      Syntax _ (Just renameClosure) definedInMacro ellipsis identifiers rules -> do 
         -- Before expanding the macro, make a pass across the macro body to mark
         -- any instances of renamed variables. 
         -- 
         -- It seems this does not need to be done in the two cases below. 
         -- Presumably this is because in those cases there is no rename 
         -- environment inserted by the macro call, so no information is lost.
         --
         -- I am still concerned that this may highlight a flaw in the husk
         -- implementation, and that this solution may not be complete.
         --
         List lexpanded <- cleanExpanded defEnv useEnv divertEnv renameEnv renameEnv True False (List []) (List ts) apply
         macroTransform defEnv useEnv divertEnv renameClosure cleanupEnv definedInMacro (List identifiers) rules (List (Atom a : lexpanded)) apply ellipsis
      Syntax (Just _defEnv) _ definedInMacro ellipsis identifiers rules -> do 
        macroTransform _defEnv useEnv divertEnv renameEnv cleanupEnv definedInMacro (List identifiers) rules (List (Atom a : ts)) apply ellipsis
      Syntax Nothing _ definedInMacro ellipsis identifiers rules -> do 
        -- A child renameEnv is not created because for a macro call there is no way an
        -- renamed identifier inserted by the macro could override one in the outer env.
        --
        -- This is because the macro renames non-matched identifiers and stores mappings
        -- from the {rename ==> original}. Each new name is unique by definition, so
        -- no conflicts are possible.
        macroTransform defEnv useEnv divertEnv renameEnv cleanupEnv definedInMacro (List identifiers) rules (List (Atom a : ts)) apply ellipsis
      SyntaxExplicitRenaming transformer -> do
        erRenameEnv <- liftIO $ nullEnv -- Local environment used just for this
                                        -- Different than the syntax-rules rename env (??)
        expanded <- explicitRenamingTransform 
                      useEnv erRenameEnv (List (Atom a : ts)) transformer apply
        walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv 
          dim False False (List result) expanded apply

      _ -> throwError $ Default "Unexpected error processing a macro in walkExpandedAtom"

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim _ _ (List result)
    a
    ts
    True _ apply = do
    -- Cleanup all symbols in the quoted code
    List cleaned <- cleanExpanded 
                      defEnv useEnv divertEnv renameEnv cleanupEnv 
                      dim True
                      (List []) (List ts)
                      apply
    return $ List $ result ++ (Atom a : cleaned)

walkExpandedAtom defEnv useEnv divertEnv renameEnv cleanupEnv dim _ _ (List result)
    a ts isQuoted _ apply = do
    walkExpanded defEnv useEnv divertEnv renameEnv cleanupEnv 
                 dim False isQuoted 
                (List $ result ++ [Atom a]) (List ts)
                 apply

walkExpandedAtom _ _ _ _ _ _ _ _ _ _ _ _ _ _ = throwError $ Default "Unexpected error in walkExpandedAtom"

-- |Accept a list of bound identifiers from a lambda expression, and rename them
--  Returns a list of the renamed identifiers as well as marking those identifiers
--  in the given environment, so they can be renamed during expansion.
markBoundIdentifiers :: Env -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
markBoundIdentifiers env cleanupEnv (Atom v : vs) renamedVars = do
  Atom renamed <- _gensym v
  _ <- defineVar env v $ Atom renamed
  _ <- defineVar cleanupEnv renamed $ Atom v
  markBoundIdentifiers env cleanupEnv vs $ renamedVars ++ [Atom renamed]
markBoundIdentifiers env cleanupEnv (_: vs) renamedVars = markBoundIdentifiers env cleanupEnv vs renamedVars
markBoundIdentifiers _ _ [] renamedVars = return $ List renamedVars

-- |Expand an atom, optionally recursively
_expandAtom :: Bool -> Env -> LispVal -> IOThrowsError LispVal
_expandAtom isRec renameEnv (Atom a) = do
  isDefined <- getVar' renameEnv a
  case isDefined of
    Just expanded -> do
       case isRec of
         True  -> _expandAtom isRec renameEnv expanded
         False -> return expanded
    Nothing -> return $ Atom a 
_expandAtom _ _ a = return a

-- |Recursively expand an atom that may have been renamed multiple times
recExpandAtom :: Env -> LispVal -> IOThrowsError LispVal
recExpandAtom renameEnv a = _expandAtom True renameEnv a

-- |Expand an atom
expandAtom :: Env -> LispVal -> IOThrowsError LispVal
expandAtom renameEnv a = _expandAtom False renameEnv a

-- |Clean up any remaining renamed variables in the expanded code
--  Only needed in special circumstances to deal with quoting.
--
-- Notes:
--
--  Keep in mind this will never work when using the renameEnv from walk, because that env binds
--  (old name => new name) in order to clean up any new names prior to eval, there would
--  need to be another environment with the reverse mappings.
--
--  ALSO, due to parent Env logic going on, these bindings need to be in some sort of
--  'master' env that transcends those env's and maps all gensyms back to their original symbols
--
cleanExpanded :: 
     Env 
  -> Env 
  -> Env 
  -> Env 
  -> Env 
  -> Bool 
  -> Bool 
  -> LispVal 
  -> LispVal 
  -> (LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal) -- ^Apply func
  -> IOThrowsError LispVal

cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ (List result) (List (List l : ls)) apply = do
  lst <- cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim True (List []) (List l) apply
  cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False (List $ result ++ [lst]) (List ls) apply

cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ (List result) (List ((Vector v) : vs)) apply = do
  List lst <- cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim True (List []) (List $ elems v) apply
  cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False (List $ result ++ [asVector lst]) (List vs) apply

cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ (List result) (List ((DottedList ds d) : ts)) apply = do
  List ls <- cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim True (List []) (List ds) apply
  l <- cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim True (List []) d apply
  cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False (List $ result ++ [DottedList ls l]) (List ts) apply

cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim startOfList (List result) (List (Atom a : ts)) apply = do
  expanded <- recExpandAtom cleanupEnv $ Atom a
  cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False (List $ result ++ [expanded]) (List ts) apply

-- Transform anything else as itself...
cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim _ (List result) (List (t : ts)) apply = do
  cleanExpanded defEnv useEnv divertEnv renameEnv cleanupEnv dim False (List $ result ++ [t]) (List ts) apply

-- Base case - empty transform
cleanExpanded _ _ _ _ _ _ _ result@(List _) (List []) _ = do
  return result

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
cleanExpanded _ _ _ _ _ _ _ _ transform _ = return transform


{- |Transform input by walking the tranform structure and creating a new structure
    with the same form, replacing identifiers in the tranform with those bound in localEnv 

 This is essentially the rewrite step from MTW, and does all that is req'd, including:
 - renaming of free variables
 - collecting an env of variables that are renamed
 - diverting bindings back into the Env of use (outer env)
-}
transformRule :: Env        -- ^ Environment the macro was defined in
              -> Env        -- ^ Outer, enclosing environment
              -> Env        -- ^ Outer environment that the macro may divert values back to
              -> Env        -- ^ Environment local to the macro containing pattern variables
              -> Env        -- ^ Environment local to the macro containing renamed variables
              -> Env        -- ^ Environment local to the macro used to cleanup any left-over renamed vars 
              -> Bool
              -> LispVal    -- ^ Literal identifiers
              -> String     -- ^ ellipsisSymbol - Symbol used to identify an ellipsis
              -> Int        -- ^ ellipsisLevel - Nesting level of the zero-to-many match, or 0 if none
              -> [Int]      -- ^ ellipsisIndex - The index at each ellipsisLevel. This is used to read data stored in
                            --                   pattern variables.
              -> LispVal    -- ^ Resultant (transformed) value. 
                            -- ^ Must be a parameter as it mutates with each transform call
              -> LispVal    -- ^ The macro transformation, read out one atom at a time and rewritten to result
              -> IOThrowsError LispVal

-- Recursively transform a list
transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) transform@(List (List l : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform esym
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
     then do
             curT <- transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym level idx (List []) (List l)
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers 
                                          esym
                                          ellipsisLevel 
                                          (init ellipsisIndex) -- Issue #56 - done w/ellip so no need for last idx
                                          result $ tail ts
               List _ -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers 
                           esym
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [curT]) transform
               _ -> throwError $ Default "Unexpected error"
     else do
             lst <- transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List []) (List l)
             case lst of
                  List _ -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [lst]) (List ts)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "Macro transform error" $ List [lst, (List l), Number $ toInteger ellipsisLevel]

-- Recursively transform a vector by processing it as a list
-- FUTURE: can this code be consolidated with the list code?
transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) transform@(List ((Vector v) : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform esym
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
     then do
             -- Idea here is that we need to handle case where you have (vector ...) - EG: (#(var step) ...)
             curT <- transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym level idx (List []) (List $ elems v)
--             case (trace ("curT = " ++ show curT) curT) of
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel (init ellipsisIndex) result $ tail ts
               List t -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers 
                           esym
                           ellipsisLevel -- Do not increment level, just wait until the next go-round when it will be incremented above
                           idx -- Must keep index since it is incremented each time
                           (List $ result ++ [asVector t]) transform
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List []) (List $ elems v)
             case lst of
                  List l -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [asVector l]) (List ts)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [Vector v]), Number $ toInteger ellipsisLevel]

-- Recursively transform an improper list
transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) transform@(List (dl@(DottedList _ _) : ts)) = do
  let nextHasEllipsis = macroElementMatchesMany transform esym
  let level = calcEllipsisLevel nextHasEllipsis ellipsisLevel
  let idx = calcEllipsisIndex nextHasEllipsis level ellipsisIndex
  if nextHasEllipsis
--  if (trace ("trans Pair: " ++ show transform ++ " lvl = " ++ show ellipsisLevel ++ " idx = " ++ show ellipsisIndex) nextHasEllipsis)
     then do
             -- Idea here is that we need to handle case where you have (pair ...) - EG: ((var . step) ...)
             curT <- transformDottedList defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym level idx (List []) (List [dl])
             case curT of
               Nil _ -> -- No match ("zero" case). Use tail to move past the "..."
                        continueTransform defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel (init ellipsisIndex) result $ tail ts 
               List t -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers 
                          esym
                          ellipsisLevel -- Do not increment level, just wait until next iteration where incremented above
                          idx -- Keep incrementing each time
                         (List $ result ++ t) transform
               _ -> throwError $ Default "Unexpected error in transformRule"
     else do lst <- transformDottedList defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List []) (List [dl])
             case lst of
                  List l -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ l) (List ts)
                  Nil _ -> return lst
                  _ -> throwError $ BadSpecialForm "transformRule: Macro transform error" $ List [lst, (List [dl]), Number $ toInteger ellipsisLevel]

-- |Transform an atom
--
-- This is a complicated transformation because we need to take into account
-- literal identifiers, pattern variables, ellipses in the current list, and 
-- nested ellipses.
transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) transform@(List (Atom a : ts)) = do
  Bool isIdent <- findAtom (Atom a) identifiers -- Literal Identifier
  isDefined <- liftIO $ isBound localEnv a -- Pattern Variable

  if isIdent
     then literalHere
     else do
        if hasEllipsis
          then ellipsisHere isDefined
          else noEllipsis isDefined

  where
    literalHere = do
      expanded <- transformLiteralIdentifier defEnv outerEnv divertEnv renameEnv dim a
      if hasEllipsis 
         then do
              -- Skip over ellipsis if present
              -- 
              -- TODO:
              -- We should throw an error here, but the problem is that we need to differentiate
              -- between the case where an ellipsis is inserted as a shorthand for a pair (in which
              -- case this is allowed) or when an ellipsis is present in the actual macro (which
              -- should be an error).
              --
              transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [expanded]) (List $ tail ts)
         --   TODO: if error (per above logic) then -
         --   throwError $ Default "Unexpected ellipsis encountered after literal identifier in macro template" 
         else do
              continueTransformWith $ result ++ [expanded]

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
        val <- getNamespacedVar' localEnv namespc a
        case val of
            Just b -> return b
            Nothing -> return $ Bool False

    hasEllipsis = macroElementMatchesMany transform esym
    ellipsisHere isDefined = do
        if isDefined
             then do 
                    isImproperPattern <- loadNamespacedBool 'p' -- "improper pattern"
                    isImproperInput <- loadNamespacedBool 'i' -- "improper input"
                    -- Load variable and ensure it is a list
                    var <- getVar localEnv a
                    case var of
                      -- add all elements of the list into result
                      List _ -> do case (appendNil (Matches.getData var ellipsisIndex) isImproperPattern isImproperInput) of
                                     List aa -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ aa) (List $ tail ts)
                                     _ -> -- No matches for var
                                          continueTransform defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex result $ tail ts

                      Nil "" -> -- No matches, keep going
                                continueTransform defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex result $ tail ts
                      v@(_) -> transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [v]) (List $ tail ts)
             else -- Matched 0 times, skip it
                  transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) (List $ tail ts)

    noEllipsis isDefined = do
      isImproperPattern <- loadNamespacedBool 'p' -- "improper pattern"
      isImproperInput <- loadNamespacedBool 'i' -- "improper input"
      t <- if (isDefined)
              then do
                   var <- getVar localEnv a
                   case (var) of
                     Nil "" -> do 
                        -- Fix for issue #42: A 0 match case for var (input ran out in pattern), flag to calling code
                        --
                        -- What's happening here is that the pattern was flagged because it was not matched in
                        -- the pattern. We pick it up and in turn pass a special flag to the outer code (as t)
                        -- so that it can finally be processed correctly.
                        wasPair <- getNamespacedVar localEnv 
                                                    '_' --  "unmatched nary pattern variable" 
                                                    a
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
                                  else return var -- no ellipsis, just return elements directly, so all can be appended
                     _ -> if ellipsisLevel > 0
                             then return var -- Let this pass, in case var is not involved in o-to-n match
                             else return var
              else do
                  -- Rename each encountered symbol, but the trick is that we want to give
                  -- the same symbol the same new name if it is found more than once, so...
                  -- we need to keep track of the var in two environments to map both ways 
                  -- between the original name and the new name.

                  alreadyRenamed <- getNamespacedVar' localEnv 'r' {-"renamed"-} a
                  case alreadyRenamed of
                    Just renamed -> return renamed
                    Nothing -> do
                       Atom renamed <- _gensym a
                       _ <- defineNamespacedVar localEnv  'r' {-"renamed"-} a $ Atom renamed
                       _ <- defineNamespacedVar renameEnv 'r' {-"renamed"-} a $ Atom renamed
                       -- Keep track of vars that are renamed; maintain reverse mapping
                       _ <- defineVar cleanupEnv renamed $ Atom a -- Global record for final cleanup of macro
                       _ <- defineVar (renameEnv) renamed $ Atom a -- Keep for Clinger
--                       return $ Atom (trace ("macro call renamed " ++ a ++ " to " ++ renamed) renamed)
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
      transformRule defEnv outerEnv divertEnv 
                    localEnv
                    renameEnv cleanupEnv dim identifiers 
                    esym
                    ellipsisLevel 
                    ellipsisIndex 
                   (List $ results)
                   (List ts)

-- Transform anything else as itself...
transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) (List (t : ts)) = do
  transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [t]) (List ts) 

-- Base case - empty transform
transformRule _ _ _ _ _ _ _ _ _ _ _ result@(List _) (List []) = do
  return result

-- Transform a single var
--
-- The nice thing about this case is that the only way we can get here is if the
-- transform is an atom - if it is a list then there is no way this case can be reached.
-- So... we do not need to worry about pattern variables here. No need to port that code
-- from the above case.
transformRule defEnv outerEnv divertEnv localEnv renameEnv _ dim identifiers esym _ _ _ (Atom transform) = do
  Bool isIdent <- findAtom (Atom transform) identifiers
  isPattVar <- liftIO $ isRecBound localEnv transform
  if isPattVar && not isIdent
     then getVar localEnv transform
     else transformLiteralIdentifier defEnv outerEnv divertEnv renameEnv dim transform

-- If transforming into a scalar, just return the transform directly...
-- Not sure if this is strictly desirable, but does not break any tests so we'll go with it for now.
transformRule _ _ _ _ _ _ _ _ _ _ _ _ transform = return transform

-- |A helper function for transforming an atom that has been marked as as literal identifier
transformLiteralIdentifier :: Env -> Env -> Env -> Env -> Bool -> String -> IOThrowsError LispVal
transformLiteralIdentifier defEnv outerEnv divertEnv renameEnv definedInMacro transform = do
  isInDef <- liftIO $ isRecBound defEnv transform
  isRenamed <- liftIO $ isRecBound renameEnv transform
  if (isInDef && not definedInMacro) || (isInDef && definedInMacro && not isRenamed)
     then do
          {- Variable exists in the environment the macro was defined in,
             so divert that value back into the environment of use. The value
             is diverted back with a different name so as not to be shadowed by
             a variable of the same name in env of use.           -}
         value <- getVar defEnv transform
         Atom renamed <- _gensym transform
         _ <- defineVar divertEnv renamed value 

         -- Keep track of diverted values for use by the compiler
         List diverted <- getNamespacedVar outerEnv ' ' "diverted"
         _ <- setNamespacedVar outerEnv ' ' "diverted" $ 
             List (diverted ++ [List [Atom renamed, Atom transform]])

         return $ Atom renamed
     else do
         -- else if not defined in defEnv then just pass the var back as-is
         return $ Atom transform
         {-
           TODO:         
           above "else" is not entirely correct, a special form would not be defined but still
           has a meaning and could be shadowed in useEnv. need some way of being able to
           divert a special form back into useEnv...
         
           Or, consider the following example. csi throws an error because if is not defined.
           If we make the modifications to store intermediate vars somewhere that are introduced
           via lambda, set!, and define then we may be able to throw an error if the var is not
           defined, instead of trying to store the special form to a variable somehow.
           
           ;(define if 3)
           (define-syntax test-template
            (syntax-rules (if)
               ((_)
                   if)))
           (write (let ((if 1)) (test-template)) )
           (write (let ((if 2)) (test-template)) )
         -}

-- | A helper function for transforming an improper list
transformDottedList :: Env -> Env -> Env -> Env -> Env -> Env -> Bool -> LispVal -> String -> Int -> [Int] -> LispVal -> LispVal -> IOThrowsError LispVal
transformDottedList defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List result) (List (DottedList ds d : ts)) = do
          lsto <- transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List []) (List ds)
          case lsto of
            List lst -> do
              -- Similar logic to the parser is applied here, where
              -- results are transformed into either a list or pair depending upon whether
              -- they form a proper list
              --
              -- d is an n-ary match, per Issue #34
              r <- transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers 
                                 esym
                                 ellipsisLevel -- OK not to increment here, this is accounted for later on
                                 ellipsisIndex -- Same as above 
                                 (List []) 
                                 (List [d, Atom esym])
              case r of
                   -- Trailing symbol in the pattern may be neglected in the transform, so skip it...
                   List [] ->
                       transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts)
                   Nil _ ->  -- Same as above, no match for d, so skip it 
                       transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex (List $ result ++ [List lst]) (List ts)
                   List rst -> do
                       transformRule defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex 
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
--        [List l@(Atom "unquote" : _ )] -> List $ results ++ [DottedList ps $ List l] -- Special case from parser. 
        [List ls] -> List $ results ++ [List $ ps ++ ls] -- Again, convert to proper list because a proper list is at end
        [l] -> List $ results ++ [DottedList ps l]
        ls -> do
            -- Same concepts as above, but here we check the last entry of a list of elements
            -- FUTURE: should be able to use a common function to encapsulate logic above and below
            case last ls of
              List [] -> List $ results ++ [List $ ps ++ init ls]
              List lls -> List $ results ++ [List $ ps ++ (init ls) ++ lls]
              t -> List $ results ++ [DottedList (ps ++ init ls) t]


transformDottedList _ _ _ _ _ _ _ _ _ _ _ _ _ = throwError $ Default "Unexpected error in transformDottedList"

-- |Continue transforming after a preceding match has ended 
continueTransform :: Env -> Env -> Env -> Env -> Env -> Env -> Bool -> LispVal -> String -> Int -> [Int] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
continueTransform defEnv outerEnv divertEnv localEnv renameEnv cleanupEnv dim identifiers esym ellipsisLevel ellipsisIndex result remaining = do
    if not (null remaining)
       then transformRule defEnv outerEnv divertEnv 
                          localEnv 
                          renameEnv
                          cleanupEnv dim identifiers
                          esym
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

-- |Helper function to load macros from a let*-syntax expression
loadMacros :: Env       -- ^ Parent environment containing the let*-syntax expression
           -> Env       -- ^ Environment of the let*-syntax body
           -> Maybe Env -- ^ Environment of renamed variables, if applicable
           -> Bool      -- ^ True if the macro was defined inside another macro
           -> [LispVal] -- ^ List containing syntax-rule definitions
           -> IOThrowsError LispVal -- ^ A dummy value, unless an error is thrown

-- Standard processing for a syntax-rules transformer
loadMacros e be Nothing dim 
    (List 
        [Atom keyword, 
         (List (Atom "syntax-rules" : 
                Atom ellipsis :
                (List identifiers : rules)))] : 
        bs) = do
  _ <- defineNamespacedVar be macroNamespace keyword $ 
        Syntax (Just e) Nothing dim ellipsis identifiers rules
  loadMacros e be Nothing dim bs

-- Standard processing for a syntax-rules transformer
loadMacros e be Nothing dim 
    (List 
        [Atom keyword, 
         (List (Atom "syntax-rules" : 
                (List identifiers : rules)))] : 
        bs) = do
  _ <- defineNamespacedVar be macroNamespace keyword $ 
        Syntax (Just e) Nothing dim "..." identifiers rules
  loadMacros e be Nothing dim bs

-- Standard processing for an explicit renaming transformer
loadMacros e be Nothing dim 
    (List  
       [Atom keyword, (List [Atom "er-macro-transformer",  
             (List (Atom "lambda" : List fparams : fbody))])]
       : bs) = do
  f <- makeNormalFunc e fparams fbody 
  _ <- defineNamespacedVar be macroNamespace keyword $ SyntaxExplicitRenaming f
  loadMacros e be Nothing dim bs

-- This pattern is reached when there is a rename env, which
-- means that we were already expanding a syntax-rules macro
-- when loadMacros was called again.
loadMacros e be (Just re) dim 
    args@(List [Atom keyword, 
                (List (Atom syntaxrules : spec))] : 
               bs) = do
  Atom exKeyword <- expandAtom re (Atom keyword)
  exSynRules <- expandAtom re (Atom syntaxrules)

-- TODO: need to process identifiers and rules - are they just expanded, or cleaned up?

  case (exSynRules, spec) of
    (Atom "syntax-rules", 
     (Atom ellipsis :
      (List identifiers : rules))) -> do
        _ <- defineNamespacedVar be macroNamespace exKeyword $ 
             Syntax (Just e) (Just re) dim ellipsis identifiers rules
        loadMacros e be (Just re) dim bs
    (Atom "syntax-rules", 
      (List identifiers : rules)) -> do
--        -- Temporary hack to expand the rules
--        List exRules <- cleanExpanded e e e re re dim False False (List []) (List rules)

        -- TODO: error checking
        _ <- defineNamespacedVar be macroNamespace exKeyword $ 
--             Syntax (Just e) (Just re) dim identifiers (trace ("exRules = " ++ show exRules) exRules) --rules
--             Syntax (Just e) (Just re) dim identifiers exRules --rules
             Syntax (Just e) (Just re) dim "..." identifiers rules
        loadMacros e be (Just re) dim bs
    --
    -- TODO: should check for lambda instead of _
    --
    (Atom "er-macro-transformer",
      [List (Atom _ : List fparams : fbody)]) -> do

        -- TODO: this is not good enough, er macros will
        --       need access to the rename env
        f <- makeNormalFunc e fparams fbody 
        _ <- defineNamespacedVar be macroNamespace exKeyword $ SyntaxExplicitRenaming f
        loadMacros e be (Just re) dim bs
    _ -> throwError $ BadSpecialForm "Unable to evaluate form w/re" $ List args

loadMacros _ _ _ _ [] = return $ Nil ""
loadMacros _ _ _ _ form = throwError $ BadSpecialForm "Unable to evaluate form" $ List form 
