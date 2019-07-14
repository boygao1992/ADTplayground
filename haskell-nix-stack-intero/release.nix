let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-19-03.json; };
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-nix-stack-intero =
            haskellPackagesNew.callPackage ./default.nix { };

          intero =
            haskellPackagesNew.callPackage ./intero-0-1-34.nix { };
        };
      };
    };
  };

  haskellPackages = (import pinnedPkgs { inherit config; }).haskellPackages;

in
  { haskell-nix-stack-intero = haskellPackages.haskell-nix-stack-intero;
  }
