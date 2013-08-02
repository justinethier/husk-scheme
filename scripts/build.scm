TODO: list of shell commands to run
("cmd 1" "cmd 2" ...)
map over them to execute script (or better, chain over in case one fails)

steps:
- ask user about change log, blog post? (is there a better way to integrate these?)
- run tests
- prompt user to review tests
- if OK, build docs
- if OK, then prompt for version number and tag it
- copy docs to gh-pages, commit, push
- checkout master branch again (all releases off of master)
- make sdist
- prompt to upload to hackage, freecode, etc
