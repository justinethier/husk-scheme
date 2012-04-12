;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for I/O-related forms 
;;
(unit-test-start "I/O (TODO)")

; TODO: tests for the following:
#|
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("input-port?", isInputPort),
                ("output-port?", isOutputPort),
                ("char-ready?", isCharReady),
                ("current-input-port", currentInputPort),
                ("current-output-port", currentOutputPort),
                ("read", readProc),
                ("read-char", readCharProc hGetChar),
                ("peek-char", readCharProc hLookAhead),
                ("write", writeProc (\ port obj -> hPrint port obj)),
                ("write-char", writeCharProc),
                ("display", writeProc (\ port obj -> case obj of
                ("read-contents", readContents),
                ("read-all", readAll),
                |#
(unit-test-handler-results)
