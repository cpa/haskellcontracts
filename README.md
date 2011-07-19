# Install

 * Install equinox (https://github.com/nick8325/equinox/) and put it somewhere in your PATH
 * Get some packages: cabal install graphscc
 * Get https://github.com/cpa/haskellcontracts 
 * Compile the executable in the contracts directory: ghc --make Check.hs

# Use

./Check file [-t n] [-p]

 * -t n means that the program should stop if a contract takes more than n sec to prove (it doesn't work on windows, and not on any linux, I have yet to find a good way to do that)
 * -p means that the first-order TPTP theory is written in files for each contract proof (the name of the file is outputed on stdout)