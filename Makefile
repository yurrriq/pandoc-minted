prefix ?= ~/bin


.SUFFIXES: .lhs .tex .pdf # .html

# .lhs.html:
# 	pandoc \
# 	-f markdown+lhs+inline_code_attributes \
# 	-t html \
# 	$< -so $@


.lhs.tex:
	pandoc \
	-f markdown+lhs+inline_code_attributes \
	-t latex \
	--latex-engine=xelatex \
	--filter result/bin/pandoc-minted \
	$< -so $@


.tex.pdf:
	xelatex -shell-escape -pdf $<


.PHONY: all
all: build README.md docs/pandoc-minted.pdf # docs/index.html


.PHONY: build
build: default.nix
	@ nix-build


.PHONY: install
install: result/bin/pandoc-minted
	@ install -Dm755 -t ${prefix} $<


README.md: Main.lhs
	pandoc \
	-f markdown+lhs+inline_code_attributes \
	-t markdown_github \
	-o $@ $<
	sed -i 's|``` sourceCode|```haskell|' $@


default.nix: pandoc-minted.cabal
	@ nix-shell -p haskellPackages.cabal2nix --run 'cabal2nix --shell . >$@'


docs/pandoc-minted.pdf: Main.pdf
	@ mv $< $@


# docs/index.html: Main.html
# 	@ mv $< $@


pandoc-minted.cabal: package.yaml
	@ nix-shell -p haskellPackages.hpack --run hpack
