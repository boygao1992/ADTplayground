{ mkDerivation, base, megaparsec, rio, stdenv }:
mkDerivation {
  pname = "haskell-crafting-interpreters";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base megaparsec rio ];
  homepage = "https://github.com/boygao1992/haskell-crafting-interpreters#readme";
  license = stdenv.lib.licenses.bsd3;
}
