# Save the intermediate .o and .hi files in a separate dir (./tmp) so
# that location information in inferior ghci in emacs will work (C-c M-.).
Check: *.hs
	ghc --make Check.hs -hidir tmp -odir tmp

etags:
	find ./ -name '*.hs' -exec hasktags --etags {} +
