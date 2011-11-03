# Install

 1. Install equinox (https://github.com/nick8325/equinox/) and put it somewhere in your PATH

   Gives errors related to Random module in hidden package haskell98
   on ghc7.  Builds on ghc6 after

     cabal install --with-compiler ghc6 mtl syb quickcheck bnfc

   The GHC executable is specified in Haskell/Makefile.

 2. (Optional) Install the others engines (SPASS,vampire,E) if you want to compare them to Equinox and put them in your PATH too.

 3. Install dependencies:

      cabal install graphscc

 4. Get the code: 

      git clone https://github.com/cpa/haskellcontracts 
 
 5. Build the checker and pull in the examples: 

      cd ./contracts; make; make egs

 6. (Optional) Run some tests:

       ./egs/run-tests.sh all

# Usage

See output of

  ./contracts/Check -h
