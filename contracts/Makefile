# Build the checker.
#
# The example looks for 'equinox' in your $PATH.  See below to get
# 'equinox'.
Check: *.hs Parser.hs
    # Save the intermediate .o and .hi files in a separate dir (./tmp)
    # so that location information in inferior ghci in emacs will work
    # (C-c M-.).
	ghc --make Check.hs -hidir tmp -odir tmp

Parser.hs: Parser.y
	happy Parser.y

# Make TAGS for emacs.
etags:
	find ./ -name '*.hs' -exec hasktags --etags {} +

# Get the examples.
egs:
	git clone git@github.com:cpa/haskellcontracts-examples.git egs

# Get Equinox.
#
# You need GHC6 to build it, but there is (well, last I checked) a
# precompiled up-to-date version in the tarball.
equinox:
	wget http://www.cse.chalmers.se/~koen/code/folkung.tar.gz	
	tar xf folkung.tar.gz
	@echo
	@echo
	@echo "ADD '`readlink -f Folkung/Haskell/equinox`' TO YOUR PATH"
