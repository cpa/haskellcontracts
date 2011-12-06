# Links

Examples: https://github.com/cpa/haskellcontracts-examples

Mailing list: http://groups.google.com/group/haskellcontracts

Wiki: https://github.com/cpa/haskellcontracts/wiki

# Install

1. Install dependencies:

      cabal install graphscc cmdargs mtl

   The 'mtl' dependency is really anything providing
   'Control.Monad.State'.

1. Get the code:

      git clone https://github.com/cpa/haskellcontracts; cd haskellcontracts

1. Build the checker and pull in the examples:

      make; make egs

1. Get Equinox.

      make equinox

   and then add 'equinox' to your $PATH.

1. (Optional) Install the others engines (SPASS,vampire,E,z3) put them
in your $PATH.

1. (Optional) Run tests:

       ./egs/run-tests.sh all

# Usage

See output of

  ./src/hcc -h

# Optional: build Equinox.

This should be unneccessary, because Koen provides a precompiled
Equinox.

The Equinox source is available at
http://www.cse.chalmers.se/~koen/code/folkung.tar.gz.  You can use the
'make equinox' in ./contracts to download the source into
./contracts/Folkung.  There is another version available on github
(https://github.com/nick8325/equinox/), but I think that one is older.

I couldn't build it with ghc7 (errors related to Random module in
hidden package haskell98), but ghc6 worked, after

  cabal install --with-compiler ghc6 mtl syb quickcheck bnfc

The GHC executable is specified in Haskell/Makefile. So edit that, or
try

  make GHC=<path to ghc6>
