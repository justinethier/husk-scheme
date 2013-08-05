(define *build-number* "3.12")

(define-syntax script
  (syntax-rules ()
;    ((_) 'done)
    ((_ cmd) (system cmd))
    ((_ cmd cmds ...)
     (if (= (system cmd) 0)
         (script cmds ...)))))

(script
    (string-append "Building " *build-number* " ...")
; TODO: - ask user about change log, blog post? (is there a better way to integrate these?)
    "make clean"
    "make"
    "make test"
; TODO:    "make testc"
    "echo \"Make sure no tests failed, and then press Enter to continue\" ; read temp"
    "make sdist"

; TODO: ?????  - if OK, then prompt for version number and tag it
; TODO: (string-append "git tag -a v" *build-number* " -m \"Version " *build-number* "\"")
; TODO: push tag to origin

    "make doc"
    "git checkout gh-pages"
    ;(if (file-exists? (string-append "API/" *build-number* "/index.html"))
    ;    "echo \"docs up-to-date\", skipping"
    ;    (begin (write "updating docs")
    ;           (string-append "mkdir API/" *build-number* " ; cp dist/doc/html/husk-scheme/* API/" *build-number*)))
    (string-append "mkdir API/" *build-number* " ; cp dist/doc/html/husk-scheme/* API/" *build-number*)
; TODO: _includes/sidebar*.html still need to be updated to reference a new build, need to address that somehow
    "echo \"API documentation updated; press Enter to commit and push to github\" ; read temp"
    (string-append "cd API/" *build-number* " && git add * && git commit * -m \"Build script added/updated API documentation for version " *build-number* "\"")
    "git push origin gh-pages"
; - copy docs to gh-pages, commit, push
; - checkout master branch again (all releases off of master)
    "git checkout repl-dev" ; TESTING, should be master
    "echo \"\""
    "echo \"TODO: upload to hackage, freecode, etc\"")
