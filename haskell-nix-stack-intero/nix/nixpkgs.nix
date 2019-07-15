let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixos-19-03.json);
  src = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };
in
  import src
