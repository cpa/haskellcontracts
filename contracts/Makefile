# Save the intermediate .o and .hi files in a separate dir (./tmp) so
# that location information in inferior ghci in emacs will work (C-c M-.).
Check: *.hs Parser.hs
	ghc --make Check.hs -hidir tmp -odir tmp

Parser.hs: Parser.y
	happy Parser.y

etags:
	find ./ -name '*.hs' -exec hasktags --etags {} +

# Keep a copy of the examples handy
egs:
	git clone git@github.com:cpa/haskellcontracts-examples.git egs
