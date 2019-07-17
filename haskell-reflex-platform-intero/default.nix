(import ./nix/reflex-platform.nix {}).project ({ pkgs, ... }: {
  name = "haskell-reflex-platform-intero";

  packages = {
    common = ./common;
    frontend = ./frontend;
  };

  shells = {
    ghc = [ "common" "frontend" ];
    ghcjs = ["common" "frontend"];
  };

  # overrides = self: super: {
    # lens = self.callHackage "lens" "4.15.4" {};
    # free = self.callCabal2nix "free" (pkgs.fetchFromGitHub {
    #   owner = "ekmett";
    #   repo = "free";
    #   rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9";
    #   sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s";
    # }) {};
    # temporary = self.callHackage "temporary" "1.3" {};
    # gi-cairo = pkgs.haskell.lib.overrideCabal super.gi-cairo (drv: {
    #   preCompileBuildDriver = ''
    #     PKG_CONFIG_PATH+=":${pkgs.cairo}/lib/pkgconfig"
    #     setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
    #     '';
    # });
  # };

  shellToolOverrides = ghc: super: {
    # inherit (ghc) hpack;
    # inherit (pkgs) chromium;
    # ghc-mod = null;
    # cabal-install = ghc.callHackage "cabal-install" "2.0.0.1" {};
    # ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid;
    stack = pkgs.haskell.lib.doJailbreak(ghc.callHackage "stack" "1.7.1" {});
    intero =
      pkgs.haskell.lib.dontCheck(
      pkgs.haskell.lib.doJailbreak(
        ghc.callPackage ./nix/intero-0-1-34.nix { }
      ));
  };

  withHoogle = true;

  useWarp = true;

  # android.frontend = {
  #   executableName = "frontend";
  #   applicationId = "org.example.frontend";
  #   displayName = "Example App";
  # };
})
