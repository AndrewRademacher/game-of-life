{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cmdargs, gloss, mtl, mwc-random, repa
      , stdenv, vector
      }:
      mkDerivation {
        pname = "gameoflife";
        version = "0.4.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base cmdargs gloss mtl mwc-random repa vector
        ];
        executableHaskellDepends = [
          base cmdargs gloss mtl mwc-random repa vector
        ];
        homepage = "https://github.com/AndrewRademacher/game-of-life";
        description = "Conway's Game of Life";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
