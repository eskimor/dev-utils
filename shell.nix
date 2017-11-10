{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lens, megaparsec, stdenv, text, safe, doctest, hspec, prettyprinter }:
      mkDerivation {
        pname = "dev-utils";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [ base lens megaparsec text safe doctest hspec prettyprinter ];
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages' = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellPackages = haskellPackages'.override { overrides = self: super: {
    megaparsec = self.callPackage ./megaparsec.nix {};

  };
  };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
