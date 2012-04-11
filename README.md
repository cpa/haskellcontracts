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
'make equinox' in ./ to download the source into
./Folkung.  There is another version available on github
(https://github.com/nick8325/equinox/), but I think that one is older.

UPDATE: the most recent Equinox uses dynymic libs not available on
`cam-02-unx` or older versions of Ubuntu.

Manually installing the .debs for the missing libs works for me in
Ubuntu 11.04.  I.e., download libffi6 and libgmp10 .debs for 11.10,
and then install manually with `dpkg -i`.

To run the precompiled Equinox on `cam-02-unx` you can:

    wget http://ubuntu.mirror.cambrium.nl/ubuntu//pool/main/libf/libffi/libffi6_3.0.11~rc1-2_i386.deb
    dpkg --extract libffi6_3.0.11\~rc1-2_i386.deb libffi6
    wget http://ubuntu.mirror.cambrium.nl/ubuntu//pool/main/g/gmp/libgmp10_5.0.1+dfsg-7ubuntu2_i386.deb
    dpkg --extract libgmp10_5.0.1+dfsg-7ubuntu2_i386.deb libgmp10
    LD_LIBRARY_PATH=/home/t-nathac/v/haskellcontracts.git/libgmp10/usr/lib:/home/t-nathac/v/haskellcontracts.git/libffi6/usr/lib/i386-linux-gnu/ equinox <equinox args>

I couldn't build Equinox with ghc7 (errors related to Random module in
hidden package haskell98), but ghc6 worked, after

    cabal install --with-compiler ghc6 mtl syb quickcheck bnfc

The GHC executable is specified in ./Folkung/Haskell/Makefile.  In ./Folkung you can do

    make -C Haskell GHC=ghc6 equinox

to build equinox with GHC 6.