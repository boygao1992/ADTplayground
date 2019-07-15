let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-nix-stack-intero =
            haskellPackagesNew.callCabal2nix "haskell-nix-stack-intero" ./haskell-nix-stack-intero.cabal { };

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
