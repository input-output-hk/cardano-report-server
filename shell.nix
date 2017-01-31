with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "cardano-report-server";
   buildInputs = [ zlib git openssh ];
   LANG = "en_US.UTF-8";
}

