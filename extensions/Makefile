#
# husk-scheme
# http://github.com/justinethier/husk-scheme
#
# Written by Justin Ethier
#
# Make file used to build the husk extensions and run test cases.
#

#UNIT_TEST_DIR = tests

all: config build
config:
	cabal configure --prefix=$(HOME) --user
build:
	cabal build
install:
	cabal install
sdist:
	cabal sdist
doc:
	# Create API documentation
	cabal haddock 

# TODO: tests
## Run all unit tests
#test: stdlib.scm
#	$(HUSKI) $(UNIT_TEST_DIR)/r5rs_pitfall.scm
#	@cd $(UNIT_TEST_DIR) ; $(HUSKI) run-tests.scm
