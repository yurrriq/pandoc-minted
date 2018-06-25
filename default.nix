let
  _nixpkgs =
    with builtins;
    ({ owner ? "NixOS", repo ? "nixpkgs", rev, sha256 }:
     fetchTarball {
       url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
       inherit sha256;
     }) (fromJSON (readFile ./nixpkgs-src.json));
in

{ compiler ? "default", doBenchmark ? false, justStaticExecutables ? true }:

with import _nixpkgs rec {
  config = {
    packageOverrides = pkgs: {
      haskellPackages =
        (if compiler == "default"
           then pkgs.haskellPackages
           else pkgs.haskell.packages.${compiler}).override {
          overrides = hself: hsuper: {
            pandoc-minted =
              (if doBenchmark
                 then pkgs.haskell.lib.doBenchmark
                 else pkgs.lib.id)
                 ((if justStaticExecutables
                     then pkgs.haskell.lib.justStaticExecutables
                     else pkgs.lib.id)
                     (hself.callPackage ./src/pandoc-minted.nix {}));
          };
        };
    };
  };
};

let

  FONTCONFIG_FILE = makeFontsConf {
    fontDirectories = [
      iosevka
    ];
  };

  xelatex = texlive.combine {
    inherit (texlive)
      framed
      fvextra
      ifplatform
      latexmk
      lm-math
      minted
      scheme-small
      xetex
      xstring;
  };

in

stdenv.mkDerivation rec {
  name = "pandoc-minted-${version}";
  inherit (haskellPackages.pandoc-minted) version;
  src = ./src;

  outputs = [ "out" "docs" ];

  buildInputs = [
    cabal2nix
    pandoc
    which
    xelatex
  ] ++ (with haskellPackages; [
    hpack
    pandoc-minted
  ]) ++ (with python36Packages; [
    pygments
  ]);

  inherit FONTCONFIG_FILE;

  buildFlags = [
    "pandoc-minted=${haskellPackages.pandoc-minted}/bin/pandoc-minted"
    "Main.pdf"
  ];

  installPhase = ''
    install -Dt $out/bin -m755 ${haskellPackages.pandoc-minted}/bin/pandoc-minted
    install -Dm644 Main.pdf $docs/${name}.pdf
  '';

  # TODO: meta
}
