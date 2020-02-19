.PHONY: clean clean-all
clean: clean-all
	nix-shell --run "hpack && cabal new-configure"

clean-all:
	-rm -rf dist dist-newstyle
	-rm cabal.project.local{,~}
	-rm bumba.cabal
	-rm .ghc.environment.x86_64-linux-8.6.5

watch-dev:
	nix-shell --run 'env BUMBA_URL=http://localhost:5000 ghcid -c "cabal new-repl" -T "Main.main" -W'

release:
	nix-build release.nix

gitlab-ci: .gitlab-ci.yml

.gitlab-ci.yml: gitlab-ci.dhall
	dhall-to-yaml --omit-empty < $< > $@
