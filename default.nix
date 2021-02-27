{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lens, lib, strict-tuple }:
      mkDerivation {
        pname = "strict-tuple-lens";
        version = "0.2";
        src = ./.;
        libraryHaskellDepends = [ base lens strict-tuple ];
        homepage = "https://github.com/emilypi/strict-tuple-lens";
        description = "Optics for the `strict-tuple` library";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
