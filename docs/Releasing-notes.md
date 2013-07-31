Checklist for a new release:

- Update version numbers, change log
- Sanity check: make test && make testc && make doc && make sdist
- Tag and push tag to github
- Upload release to hackage DB
- Add release to freecode, sourceforge (?)
- Add blog post to the website
- Update API docs on website

I want releases to 3rd party websites (hackage, freecode, etc) to stay manual. But it might be nice to try and automate some of the other work.
