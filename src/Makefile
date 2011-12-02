# Build the checker.
#
# The example looks for 'equinox' in your $PATH.  See below to get
# 'equinox'.
hcc: *.hs Types/*.hs Parser.hs
    # Save the intermediate .o and .hi files in a separate dir (./tmp)
    # so that location information in inferior ghci in emacs will work
    # (C-c M-.).
	ghc --make Main.hs -o hcc -hidir tmp -odir tmp

Parser.hs: Parser.y
	happy Parser.y

# Make TAGS for emacs.
etags:
	find ./ -name '*.hs' -exec hasktags --etags {} +
