let
  release = import ./release.nix;
  haskellPackages = release.haskellPackages;
  projectDrv = release.haskell-nix-stack-intero;

  projectDrvEnv = projectDrv.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [
      haskellPackages.intero
    ];
  });

in
  projectDrvEnv
