prefix ?= ~/bin

pandoc ?= pandoc

pandoc_flags := \
	-f markdown+lhs -t markdown_github


.PHONY: all
all: build README.md


.PHONY: build
build:
	@ nix-build


.PHONY: install
install: result/bin/pandoc-minted
	@ install -Dm755 -t ${prefix} $<


README.md: Main.lhs
	${pandoc} ${pandoc_flags} -o $@ $<
	sed -i 's|``` sourceCode|```haskell|' $@


default.nix: pandoc-minted.cabal
	@ nix-shell -p haskellPackages.cabal2nix --run 'cabal2nix --shell . >$@'


pandoc-minted.cabal: package.yaml
	@ nix-shell -p haskellPackages.hpack --run hpack
