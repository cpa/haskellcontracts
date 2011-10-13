# Install

 * Install equinox (https://github.com/nick8325/equinox/) and put it somewhere in your PATH

   Gives errors related to Random module in hidden package haskell98
   on ghc7.  Builds on ghc6 after

     cabal install --with-compiler ghc6 mtl syb quickcheck bnfc

   The GHC executable is specified in Haskell/Makefile.

 * Install the others engines (SPASS,vampire,E) if you want to compare them to Equinox and put them in your PATH too.
 * Get some packages: cabal install graphscc
 * Get the code: git clone https://github.com/cpa/haskellcontracts 
 * Compile the executable in the contracts directory: make

# Usage

See output of

  ./contracts/Check -h
