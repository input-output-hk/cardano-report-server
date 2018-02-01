with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc822.ghc;
   name = "cardano-report-server";
   buildInputs = [ zlib git openssh gmp perl ];
   LANG = "en_US.UTF-8";
}

