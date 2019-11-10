{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, recursion-schemes, stdenv
      , text
      }:
      mkDerivation {
        pname = "recursion-scheme-naive";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base containers recursion-schemes text ];
        executableHaskellDepends = [
          base containers recursion-schemes text
        ];
        testHaskellDepends = [ base containers recursion-schemes text ];
        homepage = "https://github.com/zhujinxuan/recursion-scheme-naive#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
