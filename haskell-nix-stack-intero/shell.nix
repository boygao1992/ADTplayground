let
  projectDrv = (import ./release.nix).haskell-nix-stack-intero;

in
  projectDrv.env
