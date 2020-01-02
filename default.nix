{ reflex-platform ? import ./nix/reflex-platform.nix }:

let pkgs = reflex-platform.nixpkgs;
in reflex-platform.project (_: {
  withHoogle = false;
  useWarp = true;

  packages = { bumba = ./.; };

  overrides = self: super:
    let inherit (pkgs.lib.lists) fold;
    in fold (broken-test-pkg: acc:
      acc // {
        ${broken-test-pkg} =
          pkgs.haskell.lib.dontCheck super.${broken-test-pkg};
      }) { } [
        "Glob"
        "hourglass"
        "unliftio"
        "x509"
        "x509-validation"
        "tls"
        "mono-traversable"
        "conduit"
        "yaml"
        "reflex-dom-core"
        "reflex-dom"
        "SHA"
        "lens-aeson"
      ];

  shells = {
    ghc = [ "bumba" ];
    ghcjs = [ "bumba" ];
  };
})
