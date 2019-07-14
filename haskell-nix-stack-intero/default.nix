{ mkDerivation, base, stdenv, stack }:
mkDerivation {
  pname = "haskell-nix-stack-intero";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base stack ];
  homepage = "https://github.com/boygao1992/haskell-nix-stack-intero#readme";
  license = stdenv.lib.licenses.bsd3;
}
