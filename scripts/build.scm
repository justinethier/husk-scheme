#! /usr/bin/env huski
;
; husk automated build script
;
(import (scripts lib version))

;; Force releases from master branch
(if (> (system "if [ `git rev-parse --abbrev-ref HEAD` = \"master\" ]; then true; else false; fi") 0)
    (begin
      (display "All releases must be from master, exiting...")
      (newline)
      (exit-fail)))

;; Assumes we are building using the target version of husk
(define *build-number* (get-husk-version))

(define-syntax script
  (syntax-rules ()
    ((_ cmd) (system cmd))
    ((_ cmd cmds ...)
     (if (= (system cmd) 0)
         (script cmds ...)))))

(display "  _   _           _      _   _           _ _     _                 _       _   ")(newline)
(display " | | | |         | |    | | | |         (_) |   | |               (_)     | |  ")(newline)
(display " | |_| |_   _ ___| | __ | | | |__  _   _ _| | __| |  ___  ___ _ __ _ _ __ | |_ ")(newline)
(display " |  _  | | | / __| |/ / | | | '_ \\| | | | | |/ _` | / __|/ __| '__| | '_ \\| __|")(newline)
(display " | | | | |_| \\__ \\   <  | | | |_) | |_| | | | (_| | \\__ \\ (__| |  | | |_) | |_ ")(newline)
(display " \\_| |_/\\__,_|___/_|\\_\\ | | |_.__/ \\__,_|_|_|\\__,_| |___/\\___|_|  |_| .__/ \\__|")(newline)
(display "                        | |                                         | |        ")(newline)
(display "                        |_|                                         |_|        ")(newline)
(newline)

(script
    "echo \"Before continuing, make sure:\""
    "echo \"\""
    (string-append "echo \"(1) Build number has been updated in cabal file, we are using " *build-number* " \"")    
    "echo \"(2) Change log is updated\""
    "echo \"(3) Release notes have been added to gh-pages\""
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
    "echo \"Make sure docs built OK, and then press Enter to push them to gh-pages\" ; read temp"
    
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

