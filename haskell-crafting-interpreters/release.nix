let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-nix-stack-intero =
            haskellPackagesNew.callCabal2nix "haskell-crafting-interpreters" ./haskell-crafting-interpreters.cabal { };

          intero =
            pkgs.haskell.lib.dontCheck(
            pkgs.haskell.lib.doJailbreak(
              haskellPackagesNew.callPackage ./nix/intero-0-1-34.nix { }
            ));
        };
      };
    };
  };

  customHaskellPkgs = import ./nix/nixpkgs.nix { inherit config; };

in
  { haskell-nix-stack-intero = customHaskellPkgs.haskellPackages.haskell-nix-stack-intero;
    haskellPackages = customHaskellPkgs.haskellPackages;
  }
