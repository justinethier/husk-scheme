; TODO: list of shell commands to run
; ("cmd 1" "cmd 2" ...)
; map over them to execute script (or better, chain over in case one fails)
; 
; steps:

(define-syntax script
  (syntax-rules ()
;    ((_) 'done)
    ((_ cmd) (system cmd))
    ((_ cmd cmds ...)
     (if (= (system cmd) 0)
         (script cmds ...)))))

(define *build-number* "3.12")
(script
    "make clean"
    "make"
; - ask user about change log, blog post? (is there a better way to integrate these?)
; - run tests
    "make test"
    ;"make testc"
    "echo \"Check for test failures; press Enter to continue\" ; read temp"
; ?????  - if OK, then prompt for version number and tag it
; - prompt user to review tests
; - if OK, build docs
    "make doc"
    "git checkout gh-pages"
    ;(if (file-exists? (string-append "API/" *build-number* "/index.html"))
    ;    "echo \"docs up-to-date\", skipping"
    ;    (begin (write "updating docs")
    ;           (string-append "mkdir API/" *build-number* " ; cp dist/doc/html/husk-scheme/* API/" *build-number*)))
    (string-append "mkdir API/" *build-number* " ; cp dist/doc/html/husk-scheme/* API/" *build-number* " ; cd API/" *build-number* " ; git add * ; git commit * -m \"Build script added/updated API documentation for version " *build-number*)
    "git push origin gh-pages"
; - copy docs to gh-pages, commit, push
; - checkout master branch again (all releases off of master)
    "git checkout repl-dev" ; TESTING
    "make sdist"
    "echo \"TODO: upload to hackage, freecode, etc\"")
