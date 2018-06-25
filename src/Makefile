.SUFFIXES: .cabal .lhs .nix .tex .pdf


.cabal.nix:
	@ cabal2nix . >$@


.lhs.tex:
	pandoc \
	--filter ${pandoc-minted} \
	--pdf-engine=xelatex \
	-f markdown+lhs \
	-s \
	-t latex \
	-o $@ \
	$<


.tex.pdf:
	@ latexmk -cd -pdf $<


pandoc-minted.cabal: package.yaml
	@ hpack
