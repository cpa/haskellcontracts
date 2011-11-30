# The downloading of the examples and equinox is not done
# automatically.  You need to run
#
#   make egs equinox
#
# manually once.
.PHONY: all hcc paper

all: hcc paper

hcc:
	make -C src

paper: 
	make -C paper

# Get the examples.
egs: egs
	git clone git@github.com:cpa/haskellcontracts-examples.git egs

# Get equinox
#
# You need GHC6 to build it, but there is (well, last I checked) a
# precompiled up-to-date version in the tarball.
equinox: Folkung
	wget http://www.cse.chalmers.se/~koen/code/folkung.tar.gz	
	tar xf folkung.tar.gz
	@echo
	@echo
	@echo "ADD '`readlink -f Folkung/Haskell/equinox`' TO YOUR PATH"
