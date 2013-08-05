; TODO: list of shell commands to run
; ("cmd 1" "cmd 2" ...)
; map over them to execute script (or better, chain over in case one fails)
; 
; steps:
; - ask user about change log, blog post? (is there a better way to integrate these?)
; - run tests
; - prompt user to review tests
; - if OK, build docs
; - if OK, then prompt for version number and tag it
; - copy docs to gh-pages, commit, push
; - checkout master branch again (all releases off of master)
; - make sdist
; - prompt to upload to hackage, freecode, etc

(define-syntax script
  (syntax-rules ()
;    ((_) 'done)
    ((_ cmd) (system cmd))
    ((_ cmd cmds ...)
     (if (= (system cmd) 0)
         (script cmds ...)))))

(define *build-number* "3.12")
(script
;    "git checkout master"
;    "make test && make doc"
    "git checkout gh-pages"
    (if (file-exists? (string-append "API/" *build-number*))
        ""
        (string-append "mkdir API/" *build-number* " ; cp dist/doc/html/husk-scheme/* API/" *build-number*))
    "git checkout repl-dev" ; TESTING
    "echo \"TODO\"")
