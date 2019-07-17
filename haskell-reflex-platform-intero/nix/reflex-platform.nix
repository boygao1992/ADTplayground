let
  reflex-platform-json = builtins.fromJSON (builtins.readFile ./reflex-platform.json);
  reflex-platform = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    inherit (reflex-platform-json) rev sha256;
  };
in
  import reflex-platform
