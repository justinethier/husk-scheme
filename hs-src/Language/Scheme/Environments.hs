
{- |
Module      : Language.Scheme.Environments
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

-}

module Language.Scheme.Environments
    (
      primitives
    , ioPrimitives
    ) where
import Language.Scheme.Libraries
import Language.Scheme.Numerical
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Util
import Language.Scheme.Variables
import Control.Monad.Error
import qualified Data.Char
import System.IO

{- I/O primitives
Primitive functions that execute within the IO monad -}
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort openFile ReadMode),
                ("open-binary-input-file", makePort openBinaryFile ReadMode),
                ("open-output-file", makePort openFile WriteMode),
                ("open-binary-output-file", makePort openBinaryFile WriteMode),
                ("close-port", closePort),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("flush-output-port", flushOutputPort),
                ("textual-port?", isTextPort),
                ("binary-port?", isBinaryPort),
                ("input-port-open?", isInputPortOpen),
                ("output-port-open?", isOutputPortOpen),
                ("input-port?", isInputPort),
                ("output-port?", isOutputPort),
                ("char-ready?", isCharReady),

               -- The following optional procedures are NOT implemented:
               --
               {- with-input-from-file
               with-output-from-file
               transcript-on
               transcript-off -}
               --
               {- Consideration may be given in a future release, but keep in mind
               the impact to the other I/O functions. -}

                ("current-input-port", currentInputPort),
                ("current-output-port", currentOutputPort),
                ("read", readProc True),
                ("read-line", readProc False),
                ("read-char", readCharProc hGetChar),
                ("read-bytevector", readByteVector),
                ("peek-char", readCharProc hLookAhead),
                ("write", writeProc (\ port obj -> hPrint port obj)),
                ("write-char", writeCharProc),
                ("write-bytevector", writeByteVector),
                ("display", writeProc (\ port obj -> do
                  case obj of
                    String str -> hPutStr port str
                    _ -> hPutStr port $ show obj)),

              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci=?", stringCIEquals),
              ("string-ci<?", stringCIBoolBinop (<)),
              ("string-ci>?", stringCIBoolBinop (>)),
              ("string-ci<=?", stringCIBoolBinop (<=)),
              ("string-ci>=?", stringCIBoolBinop (>=)),
              ("string->symbol", string2Symbol),

              ("car", car),
              ("cdr", cdr),
              ("cons", cons),

              ("eq?",    eq),
              ("eqv?",   eq), -- TODO: not quite right, but maybe good enough for now
              ("equal?", recDerefToFnc equal),

              ("pair?", isDottedList),
              ("list?", unaryOp' isList),
              ("vector?", unaryOp' isVector),
              ("null?", isNull),
              ("string?", isString),

              ("list-copy", listCopy),

              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string->number", stringToNumber),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("string->vector", stringToVector),
              ("vector->string", vectorToString),
              ("string-copy", stringCopy),
              ("string->utf8", byteVectorStr2Utf),

              ("bytevector?", unaryOp' isByteVector),
              ("bytevector-length", byteVectorLength),
              ("bytevector-u8-ref", byteVectorRef),
              ("bytevector-append", byteVectorAppend),
              ("bytevector-copy", byteVectorCopy),
              ("utf8->string", byteVectorUtf2Str),

              ("vector-length",wrapLeadObj vectorLength),
              ("vector-ref",   wrapLeadObj vectorRef),
              ("vector-copy",  vectorCopy),
              ("vector->list", wrapLeadObj vectorToList),
              ("list->vector", wrapLeadObj listToVector),

              ("hash-table?",       wrapHashTbl isHashTbl),
              ("hash-table-exists?",wrapHashTbl hashTblExists),
              ("hash-table-ref",    wrapHashTbl hashTblRef),
              ("hash-table-size",   wrapHashTbl hashTblSize),
              ("hash-table->alist", wrapHashTbl hashTbl2List),
              ("hash-table-keys",   wrapHashTbl hashTblKeys),
              ("hash-table-values", wrapHashTbl hashTblValues),
              ("hash-table-copy",   wrapHashTbl hashTblCopy),

                -- From SRFI 96
                ("file-exists?", fileExists),
                ("delete-file", deleteFile),

                -- husk internal functions
                --("husk-path", getDataFileFullPath'),

                -- Other I/O functions
                ("print-env", printEnv'),
                ("env-exports", exportsFromEnv'),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("find-module-file", findModuleFile),
                ("system", system),
                ("gensym", gensym)]


printEnv' :: [LispVal] -> IOThrowsError LispVal
printEnv' [LispEnv env] = do
    result <- liftIO $ printEnv env
    return $ String result
printEnv' [] = throwError $ NumArgs (Just 1) []
printEnv' args = throwError $ TypeMismatch "env" $ List args

exportsFromEnv' :: [LispVal] -> IOThrowsError LispVal
exportsFromEnv' [LispEnv env] = do
    result <- liftIO $ exportsFromEnv env
    return $ List result
exportsFromEnv' err = return $ List []

{- "Pure" primitive functions -}
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numAdd),
              ("-", numSub),
              ("*", numMul),
              ("/", numDiv),
              ("modulo", numMod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("rationalize", numRationalize),

              ("round", numRound),
              ("floor", numFloor),
              ("ceiling", numCeiling),
              ("truncate", numTruncate),

              ("numerator", numNumerator),
              ("denominator", numDenominator),

              ("exp", numExp),
              ("log", numLog),
              ("sin", numSin),
              ("cos", numCos),
              ("tan", numTan),
              ("asin", numAsin),
              ("acos", numAcos),
              ("atan", numAtan),

              ("sqrt", numSqrt),
              ("expt", numExpt),

              ("make-rectangular", numMakeRectangular),
              ("make-polar", numMakePolar),
              ("real-part", numRealPart ),
              ("imag-part", numImagPart),
              ("magnitude", numMagnitude),
              ("angle", numAngle ),

              ("exact->inexact", numExact2Inexact),
              ("inexact->exact", numInexact2Exact),

              ("number->string", num2String),

              ("=", numBoolBinopEq),
              (">", numBoolBinopGt),
              (">=", numBoolBinopGte),
              ("<", numBoolBinopLt),
              ("<=", numBoolBinopLte),

              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),

              ("char=?",  charBoolBinop (==)),
              ("char<?",  charBoolBinop (<)),
              ("char>?",  charBoolBinop (>)),
              ("char<=?", charBoolBinop (<=)),
              ("char>=?", charBoolBinop (>=)),
              ("char-ci=?",  charCIBoolBinop (==)),
              ("char-ci<?",  charCIBoolBinop (<)),
              ("char-ci>?",  charCIBoolBinop (>)),
              ("char-ci<=?", charCIBoolBinop (<=)),
              ("char-ci>=?", charCIBoolBinop (>=)),
              ("char-alphabetic?", charPredicate Data.Char.isAlpha),
              ("char-numeric?", charPredicate Data.Char.isNumber),
              ("char-whitespace?", charPredicate Data.Char.isSpace),
              ("char-upper-case?", charPredicate Data.Char.isUpper),
              ("char-lower-case?", charPredicate Data.Char.isLower),
              ("char->integer", char2Int),
              ("integer->char", int2Char),
              ("char-upcase", charUpper),
              ("char-downcase", charLower),
              ("digit-value", charDigitValue),

              ("procedure?", isProcedure),
              ("nan?", isNumNaN),
              ("infinite?", isNumInfinite),
              ("finite?", isNumFinite),
              ("exact?", isNumExact),
              ("inexact?", isNumInexact),
              ("number?", isNumber),
              ("complex?", isComplex),
              ("real?", isReal),
              ("rational?", isRational),
              ("integer?", isInteger),
              ("eof-object?", isEOFObject),
              ("eof-object", eofObject),
              ("symbol?", isSymbol),
              ("symbol=?", isSymbolEq),
              ("symbol->string", symbol2String),
              ("char?", isChar),

              ("make-list", makeList),
              ("make-vector", makeVector),
              ("vector", buildVector),

              ("make-bytevector", makeByteVector),
              ("bytevector", byteVector),

              ("make-hash-table", hashTblMake),
              ("string", buildString),
              ("make-string", makeString),

              ("boolean?", isBoolean),
              ("boolean=?", isBooleanEq),

              ("husk-interpreter?", isInterpreter)]

-- |Custom function used internally in the test suite
isInterpreter :: [LispVal] -> ThrowsError LispVal
isInterpreter [] = return $ Bool True
isInterpreter _ = return $ Bool False

