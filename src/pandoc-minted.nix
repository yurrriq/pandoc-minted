{ mkDerivation, base, pandoc, pandoc-types, stdenv }:
mkDerivation {
  pname = "pandoc-minted";
  version = "0.0.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base pandoc pandoc-types ];
  homepage = "https://github.com/yurrriq/pandoc-minted#readme";
  description = "A pandoc filter to render LaTeX code blocks using minted";
  license = stdenv.lib.licenses.mit;
}
