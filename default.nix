{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, pandoc, pandoc-types, stdenv }:
      mkDerivation {
        pname = "pandoc-minted";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base pandoc pandoc-types ];
        homepage = "https://github.com/yurrriq/pandoc-minted#readme";
        description = "A pandoc filter to render LaTeX code blocks using minted";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
