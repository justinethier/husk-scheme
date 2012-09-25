{-# Language ExistentialQuantification #-}

{- |
Module      : Language.Scheme.Primitives
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains primitive functions written in Haskell.
Most of these map directly to an equivalent Scheme function.

-}

module Language.Scheme.Primitives (
 -- * Pure functions
 -- ** List
   car
 , cdr 
 , cons
 , equal 
 -- ** Vector
 , buildVector 
 , vectorLength 
 , vectorRef 
 , vectorToList 
 , listToVector
 , makeVector
 -- ** Hash Table
 , hashTblExists 
 , hashTblRef
 , hashTblSize 
 , hashTbl2List
 , hashTblKeys
 , hashTblValues 
 , hashTblCopy
 , hashTblMake
 -- ** String
 , buildString
 , makeString
 , doMakeString
 , stringLength
 , stringRef
 , substring
 , stringCIEquals 
 , stringCIBoolBinop 
 , stringAppend 
 , stringToNumber
 , stringToList 
 , listToString
 , stringCopy 
 , symbol2String 
 , string2Symbol
 --data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)
 -- ** Character
 , charPredicate
 , charUpper
 , charLower
 , char2Int
 , int2Char

 -- ** Predicate
 , isHashTbl
 , isChar 
 , isString 
 , isBoolean 
 , isDottedList 
 , isProcedure 
 , isList 
 , isVector 
 , isNull 
 , isEOFObject 
 , isSymbol 
 -- ** Utility functions
 , unpackEquals 
 , boolBinop 
 , unaryOp 
 , strBoolBinop 
 , charBoolBinop 
 , boolBoolBinop
 , unpackStr 
 , unpackBool
 -- * Impure functions
 -- |All of these functions must be executed within the IO monad.
 
 -- ** Input / Output 
 , makePort 
 , closePort
 , currentOutputPort 
 , currentInputPort 
 , isOutputPort 
 , isInputPort
 , isCharReady
 , readProc 
 , readCharProc 
 , writeProc 
 , writeCharProc
 , readContents
 , load
 , readAll
 , fileExists
 , deleteFile
 -- ** Symbol generation
 , gensym
 , _gensym
 ) where
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Types
--import qualified Control.Exception
import Control.Monad.Error
import Data.Char hiding (isSymbol)
import Data.Array
import Data.Unique
import qualified Data.Map
import System.IO
import System.Directory (doesFileExist, removeFile)
import System.IO.Error

---------------------------------------------------
-- I/O Primitives
-- These primitives all execute within the IO monad
---------------------------------------------------

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [] = throwError $ NumArgs 1 []
makePort _ args@(_ : _) = throwError $ NumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

currentInputPort, currentOutputPort :: [LispVal] -> IOThrowsError LispVal
{- FUTURE: For now, these are just hardcoded to the standard i/o ports.
a future implementation that includes with-*put-from-file
would require a more involved implementation here as well as
other I/O functions hooking into these instead of std* -}
currentInputPort _ = return $ Port stdin
currentOutputPort _ = return $ Port stdout

isInputPort, isOutputPort :: [LispVal] -> IOThrowsError LispVal
isInputPort [Port port] = liftM Bool $ liftIO $ hIsReadable port
isInputPort _ = return $ Bool False

isOutputPort [Port port] = liftM Bool $ liftIO $ hIsWritable port
isOutputPort _ = return $ Bool False

isCharReady :: [LispVal] -> IOThrowsError LispVal
isCharReady [Port port] = do --liftM Bool $ liftIO $ hReady port
    result <- liftIO $ try (liftIO $ hReady port)
    case result of
        Left e -> if isEOFError e
                     then return $ Bool False
                     else throwError $ Default "I/O error reading from port" -- FUTURE: ioError e
        Right _ -> return $ Bool True
isCharReady _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = do
    input <- liftIO $ try (liftIO $ hGetLine port)
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port" -- FUTURE: ioError e
        Right inpStr -> do
            liftThrows $ readExpr inpStr
readProc args@(_ : _) = throwError $ BadSpecialForm "" $ List args

readCharProc :: (Handle -> IO Char) -> [LispVal] -> IOThrowsError LispVal
readCharProc func [] = readCharProc func [Port stdin]
readCharProc func [Port port] = do
    liftIO $ hSetBuffering port NoBuffering
    input <- liftIO $ try (liftIO $ func port)
    liftIO $ hSetBuffering port LineBuffering
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port"
        Right inpChr -> do
            return $ Char inpChr
readCharProc _ args@(_ : _) = throwError $ BadSpecialForm "" $ List args

{- writeProc :: --forall a (m :: * -> *).
             (MonadIO m, MonadError LispError m) =>
             (Handle -> LispVal -> IO a) -> [LispVal] -> m LispVal -}
writeProc func [obj] = writeProc func [obj, Port stdout]
writeProc func [obj, Port port] = do
    output <- liftIO $ try (liftIO $ func port obj)
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeProc _ other = if length other == 2
                     then throwError $ TypeMismatch "(value port)" $ List other
                     else throwError $ NumArgs 2 other

writeCharProc :: [LispVal] -> IOThrowsError LispVal
writeCharProc [obj] = writeCharProc [obj, Port stdout]
writeCharProc [obj@(Char _), Port port] = do
    output <- liftIO $ try (liftIO $ (hPutStr port $ show obj))
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeCharProc other = if length other == 2
                     then throwError $ TypeMismatch "(character port)" $ List other
                     else throwError $ NumArgs 2 other

fileExists, deleteFile :: [LispVal] -> IOThrowsError LispVal
fileExists [String filename] = do
    exists <- liftIO $ doesFileExist filename
    return $ Bool exists
fileExists [] = throwError $ NumArgs 1 []
fileExists args@(_ : _) = throwError $ NumArgs 1 args

deleteFile [String filename] = do
    output <- liftIO $ try(liftIO $ removeFile filename)
    case output of
        Left _ -> return $ Bool False
        Right _ -> return $ Bool True
deleteFile [] = throwError $ NumArgs 1 []
deleteFile args@(_ : _) = throwError $ NumArgs 1 args

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [] = throwError $ NumArgs 1 []
readContents args@(_ : _) = throwError $ NumArgs 1 args

load :: String -> IOThrowsError [LispVal]
load filename = do
  result <- liftIO $ doesFileExist filename
  if result
     then (liftIO $ readFile filename) >>= liftThrows . readExprList
     else throwError $ Default $ "File does not exist: " ++ filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [] = throwError $ NumArgs 1 []
readAll args@(_ : _) = throwError $ NumArgs 1 args

-- |Version of gensym that can be conveniently called from Haskell.
_gensym :: String -> IOThrowsError LispVal
_gensym prefix = do
    u <- liftIO $ newUnique
    return $ Atom $ prefix ++ (show $ Number $ toInteger $ hashUnique u)

-- |Generate a (reasonably) unique symbol, given an optional prefix.
--  This function is provided even though it is not part of R5RS.
gensym :: [LispVal] -> IOThrowsError LispVal
gensym [String prefix] = _gensym prefix
gensym [] = _gensym " g"
gensym args@(_ : _) = throwError $ NumArgs 1 args


---------------------------------------------------
-- "Pure" primitives
---------------------------------------------------

-- List primitives
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [(Vector arg1), (Vector arg2)] = eqvList equal [List $ (elems arg1), List $ (elems arg2)]
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- ------------ Vector Primitives --------------

makeVector, buildVector, vectorLength, vectorRef, vectorToList, listToVector :: [LispVal] -> ThrowsError LispVal
makeVector [(Number n)] = makeVector [Number n, List []]
makeVector [(Number n), a] = do
  let l = replicate (fromInteger n) a
  return $ Vector $ (listArray (0, length l - 1)) l
makeVector [badType] = throwError $ TypeMismatch "integer" badType
makeVector badArgList = throwError $ NumArgs 1 badArgList

buildVector (o : os) = do
  let lst = o : os
  return $ Vector $ (listArray (0, length lst - 1)) lst
buildVector badArgList = throwError $ NumArgs 1 badArgList

vectorLength [(Vector v)] = return $ Number $ toInteger $ length (elems v)
vectorLength [badType] = throwError $ TypeMismatch "vector" badType
vectorLength badArgList = throwError $ NumArgs 1 badArgList

vectorRef [(Vector v), (Number n)] = return $ v ! (fromInteger n)
vectorRef [badType] = throwError $ TypeMismatch "vector integer" badType
vectorRef badArgList = throwError $ NumArgs 2 badArgList

vectorToList [(Vector v)] = return $ List $ elems v
vectorToList [badType] = throwError $ TypeMismatch "vector" badType
vectorToList badArgList = throwError $ NumArgs 1 badArgList

listToVector [(List l)] = return $ Vector $ (listArray (0, length l - 1)) l
listToVector [badType] = throwError $ TypeMismatch "list" badType
listToVector badArgList = throwError $ NumArgs 1 badArgList

-- ------------ Hash Table Primitives --------------

-- Future: support (equal?), (hash) parameters
hashTblMake, isHashTbl, hashTblExists, hashTblRef, hashTblSize, hashTbl2List, hashTblKeys, hashTblValues, hashTblCopy :: [LispVal] -> ThrowsError LispVal
hashTblMake _ = return $ HashTable $ Data.Map.fromList []

isHashTbl [(HashTable _)] = return $ Bool True
isHashTbl _ = return $ Bool False

hashTblExists [(HashTable ht), key@(_)] = do
  case Data.Map.lookup key ht of
    Just _ -> return $ Bool True
    Nothing -> return $ Bool False
hashTblExists [] = throwError $ NumArgs 2 []
hashTblExists args@(_ : _) = throwError $ NumArgs 2 args

hashTblRef [(HashTable ht), key@(_)] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ BadSpecialForm "Hash table does not contain key" key
hashTblRef [(HashTable ht), key@(_), Func _ _ _ _] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ NotImplemented "thunk"
{- FUTURE: a thunk can optionally be specified, this drives definition of /default
Nothing -> apply thunk [] -}
hashTblRef [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblRef badArgList = throwError $ NumArgs 2 badArgList

hashTblSize [(HashTable ht)] = return $ Number $ toInteger $ Data.Map.size ht
hashTblSize [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblSize badArgList = throwError $ NumArgs 1 badArgList

hashTbl2List [(HashTable ht)] = do
  return $ List $ map (\ (k, v) -> List [k, v]) $ Data.Map.toList ht
hashTbl2List [badType] = throwError $ TypeMismatch "hash-table" badType
hashTbl2List badArgList = throwError $ NumArgs 1 badArgList

hashTblKeys [(HashTable ht)] = do
  return $ List $ map (\ (k, _) -> k) $ Data.Map.toList ht
hashTblKeys [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblKeys badArgList = throwError $ NumArgs 1 badArgList

hashTblValues [(HashTable ht)] = do
  return $ List $ map (\ (_, v) -> v) $ Data.Map.toList ht
hashTblValues [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblValues badArgList = throwError $ NumArgs 1 badArgList

hashTblCopy [(HashTable ht)] = do
  return $ HashTable $ Data.Map.fromList $ Data.Map.toList ht
hashTblCopy [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblCopy badArgList = throwError $ NumArgs 1 badArgList

-- ------------ String Primitives --------------

buildString :: [LispVal] -> ThrowsError LispVal
buildString [(Char c)] = return $ String [c]
buildString (Char c : rest) = do
  cs <- buildString rest
  case cs of
    String s -> return $ String $ [c] ++ s
    badType -> throwError $ TypeMismatch "character" badType
buildString [badType] = throwError $ TypeMismatch "character" badType
buildString badArgList = throwError $ NumArgs 1 badArgList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [(Number n)] = return $ doMakeString n ' ' ""
makeString [(Number n), (Char c)] = return $ doMakeString n c ""
makeString badArgList = throwError $ NumArgs 1 badArgList

doMakeString :: forall a . (Num a, Eq a) => a -> Char -> String -> LispVal
doMakeString n char s =
    if n == 0
       then String s
       else doMakeString (n - 1) char (s ++ [char])

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ foldr (const (+ 1)) 0 s -- Could probably do 'length s' instead...
stringLength [badType] = throwError $ TypeMismatch "string" badType
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)] = return $ Char $ s !! fromInteger k
stringRef [badType] = throwError $ TypeMismatch "string number" badType
stringRef badArgList = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring [(String s), (Number start), (Number end)] =
  do let slength = fromInteger $ end - start
     let begin = fromInteger start
     return $ String $ (take slength . drop begin) s
substring [badType] = throwError $ TypeMismatch "string number number" badType
substring badArgList = throwError $ NumArgs 3 badArgList

stringCIEquals :: [LispVal] -> ThrowsError LispVal
stringCIEquals [(String str1), (String str2)] = do
  if (length str1) /= (length str2)
     then return $ Bool False
     else return $ Bool $ ciCmp str1 str2 0
  where ciCmp s1 s2 idx = if idx == (length s1)
                             then True
                             else if (toLower $ s1 !! idx) == (toLower $ s2 !! idx)
                                     then ciCmp s1 s2 (idx + 1)
                                     else False
stringCIEquals [badType] = throwError $ TypeMismatch "string string" badType
stringCIEquals badArgList = throwError $ NumArgs 2 badArgList

stringCIBoolBinop :: ([Char] -> [Char] -> Bool) -> [LispVal] -> ThrowsError LispVal
stringCIBoolBinop op [(String s1), (String s2)] = boolBinop unpackStr op [(String $ strToLower s1), (String $ strToLower s2)]
  where strToLower str = map (toLower) str
stringCIBoolBinop _ [badType] = throwError $ TypeMismatch "string string" badType
stringCIBoolBinop _ badArgList = throwError $ NumArgs 2 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [(String s)] = return $ String s -- Needed for "last" string value
stringAppend (String st : sts) = do
  rest <- stringAppend sts
  case rest of
    String s -> return $ String $ st ++ s
    other -> throwError $ TypeMismatch "string" other
stringAppend [badType] = throwError $ TypeMismatch "string" badType
stringAppend badArgList = throwError $ NumArgs 1 badArgList

stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [(String s)] = do
  result <- (readExpr s)
  case result of
    n@(Number _) -> return n
    n@(Rational _) -> return n
    n@(Float _) -> return n
    n@(Complex _) -> return n
    _ -> return $ Bool False
stringToNumber [(String s), Number radix] = do
  case radix of
    2 -> stringToNumber [String $ "#b" ++ s]
    8 -> stringToNumber [String $ "#o" ++ s]
    10 -> stringToNumber [String s]
    16 -> stringToNumber [String $ "#x" ++ s]
    _ -> throwError $ Default $ "Invalid radix: " ++ show radix
stringToNumber [badType] = throwError $ TypeMismatch "string" badType
stringToNumber badArgList = throwError $ NumArgs 1 badArgList

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [(String s)] = return $ List $ map (Char) s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [(List [])] = return $ String ""
listToString [(List l)] = buildString l
listToString [badType] = throwError $ TypeMismatch "list" badType
listToString [] = throwError $ NumArgs 1 []
listToString args@(_ : _) = throwError $ NumArgs 1 args

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 2 badArgList

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList _ _]) = return $ Bool True
-- Must include lists as well since they are made up of 'chains' of pairs
isDottedList ([List []]) = return $ Bool False
isDottedList ([List _]) = return $ Bool True
isDottedList _ = return $ Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([Continuation _ _ _ _ _]) = return $ Bool True
isProcedure ([PrimitiveFunc _]) = return $ Bool True
isProcedure ([Func _ _ _ _]) = return $ Bool True
isProcedure ([IOFunc _]) = return $ Bool True
isProcedure ([EvalFunc _]) = return $ Bool True
isProcedure _ = return $ Bool False

isVector, isList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ Bool True
isVector _ = return $ Bool False
isList (List _) = return $ Bool True
isList _ = return $ Bool False

isNull :: [LispVal] -> ThrowsError LispVal
isNull ([List []]) = return $ Bool True
isNull _ = return $ Bool False

isEOFObject :: [LispVal] -> ThrowsError LispVal
isEOFObject ([EOF]) = return $ Bool True
isEOFObject _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ([Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = throwError $ NumArgs 1 []
symbol2String args@(_ : _) = throwError $ NumArgs 1 args

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol ([String s]) = return $ Atom s
string2Symbol [] = throwError $ NumArgs 1 []
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol args@(_ : _) = throwError $ NumArgs 1 args

charUpper :: [LispVal] -> ThrowsError LispVal
charUpper [Char c] = return $ Char $ toUpper c
charUpper [notChar] = throwError $ TypeMismatch "char" notChar

charLower :: [LispVal] -> ThrowsError LispVal
charLower [Char c] = return $ Char $ toLower c
charLower [notChar] = throwError $ TypeMismatch "char" notChar

char2Int :: [LispVal] -> ThrowsError LispVal
char2Int [Char c] = return $ Number $ toInteger $ ord c 
char2Int [notChar] = throwError $ TypeMismatch "char" notChar

int2Char :: [LispVal] -> ThrowsError LispVal
int2Char [Number n] = return $ Char $ chr $ fromInteger n 
int2Char [notInt] = throwError $ TypeMismatch "integer" notInt

-- |Determine if given character satisfies the given predicate
charPredicate :: (Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charPredicate pred ([Char c]) = return $ Bool $ pred c 
charPredicate _ _ = return $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([Char _]) = return $ Bool True
isChar _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ([String _]) = return $ Bool True
isString _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([Bool _]) = return $ Bool True
isBoolean _ = return $ Bool False


-- Utility functions
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp _ args@(_ : _) = throwError $ NumArgs 1 args

{- numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum -}
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr
charBoolBinop = boolBinop unpackChar
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "character" notChar

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
