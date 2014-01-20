#! /usr/bin/env huski
;
; husk automated build script
;
(define *build-number* "3.15.2")

(define-syntax script
  (syntax-rules ()
    ((_ cmd) (system cmd))
    ((_ cmd cmds ...)
     (if (= (system cmd) 0)
         (script cmds ...)))))

(script
    "echo \"husk scheme build system\""
    "echo \"Before continuing, make sure:\""
    "echo \"\""
    "echo \"(1) Change log is updated\""
    "echo \"(2) Build number has been updated in this script, cabal file, and Core.hs\""
    "echo \"(3) Release notes have been added to gh-pages\""
    "echo \"(4) This is the master branch. All releases must be off of master!!\""
    "echo \"\""
    "echo \"Press Enter to continue\" ; read temp"
    
    (string-append "echo \"Building " *build-number* " ...\"")
    "make clean"
    "make"
    "make sdist"
    "make test"
    "make testc"
    "echo \"Make sure no tests failed, and then press Enter to continue\" ; read temp"

    "make doc"
    "git checkout gh-pages"
        ;(if (file-exists? (string-append "API/" *build-number* "/index.html"))
        ;    "echo \"docs up-to-date\", skipping"
        ;    (begin (write "updating docs")
        ;           (string-append "mkdir API/" *build-number* " ; cp dist/doc/html/husk-scheme/* API/" *build-number*)))
    (string-append "mkdir API/" *build-number* 
                   " ; cp dist/doc/html/husk-scheme/* API/" *build-number*)
    ; Update build number to point to latest API docs, maybe other stuff
    (string-append "sed -i '17s/.*/husk_build_number:  " 
                   *build-number* "/' _config.yml")

    "echo \"API documentation updated; press Enter to commit and push to github\" ; read temp"
    (string-append "cd API/" *build-number* 
                   " && git add * && git commit * -m \"Build script added/updated API documentation for version " *build-number* "\"")
    "git commit _config.yml -m \"Build script updated build number\""
    "git push origin gh-pages"

    "git checkout master"
    "echo \"Docs done, press Enter to tag this release\" ; read temp"
    (string-append "git tag -a v" *build-number* " -m \"Version " *build-number* "\"")
    (string-append "git push origin v" *build-number*)

    "echo \"\""
    "echo \"Tag pushed to origin, build is ready for upload to hackage, freecode, etc\"")

