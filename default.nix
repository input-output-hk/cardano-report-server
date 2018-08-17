with import <nixpkgs> { config = {}; overlays = []; };

let
  fixedhspkgs = haskellPackages.override {
    overrides = self: super: {
      o-clock = haskell.lib.doJailbreak super.o-clock;
      cardano-report-server = self.callCabal2nix "cardano-report-server" (lib.cleanSource ./.) {};
    };
  };
in fixedhspkgs.cardano-report-server
