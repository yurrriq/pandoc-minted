.PHONY: all
all: pandoc-minted.nix
	@ install -Dm644 -t docs $$(nix-build --no-out-link -A docs)/*.pdf


pandoc-minted.nix:
	@ nix-shell -p cabal2nix haskellPackages.hpack --pure --run 'make -C src $@'
