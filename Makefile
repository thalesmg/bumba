BUMBA_PORT?=5015

.PHONY: clean clean-all format
clean: clean-all
	nix-shell --run "hpack && cabal new-configure"

clean-all:
	-rm -rf dist dist-newstyle
	-rm cabal.project.local{,~}
	-rm bumba.cabal
	-rm .ghc.environment.x86_64-linux-8.6.5

watch-dev:
	nix-shell --run 'env BUMBA_URL=http://localhost:$(BUMBA_PORT) ghcid -c "cabal new-repl" -T "Main.main" -W'

release:
	nix-build release.nix

gitlab-ci: .gitlab-ci.yml

.gitlab-ci.yml: gitlab-ci.dhall
	dhall-to-yaml --omit-empty < $< > $@

format:
	nix-shell --run 'stylish-haskell -i **/*.hs'

build-serve-release: release
	$(MAKE) serve-release

serve-release:
	# FLASK_ENV=development is buggy! https://github.com/NixOS/nixpkgs/issues/42924
	nix-shell -p 'with (import <nixpkgs> {}); python38.withPackages (ps: with ps; [python38 flask flask-cors requests])' --run 'env FLASK_APP=./app/main.py FLASK_ENV=development python3 -m flask run -p $(BUMBA_PORT)'
