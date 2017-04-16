{ mkDerivation, base, cmdargs, gloss, mtl, mwc-random, repa, stdenv
, vector, mesa_glu
}:
mkDerivation {
  pname = "gameoflife";
  version = "0.4.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cmdargs gloss mtl mwc-random repa vector mesa_glu
  ];
  executableHaskellDepends = [
    base cmdargs gloss mtl mwc-random repa vector mesa_glu
  ];
  homepage = "https://github.com/AndrewRademacher/game-of-life";
  description = "Conway's Game of Life";
  license = stdenv.lib.licenses.mit;
}
