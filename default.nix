{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler ? "ghc"
}:

let
  pkgs = reflex-platform.nixpkgs;
  hp = pkgs.haskellPackages.override {
    all-cabal-hashes = builtins.fetchurl {
      url =
        "https://github.com/commercialhaskell/all-cabal-hashes/archive/999f036f945213ae78ca8e1741a52f9e60553df6.tar.gz";
      sha256 = "1926ak4ykc0br02vxi981hmqjfhlsz13g2vafsw30958lmvs8g7y";
    };
  };
in reflex-platform.project (_: {
  withHoogle = false;
  useWarp = true;

  packages = { bumba = ./.; };

  overrides = self: super:
    let
      inherit (pkgs.lib.lists) fold;
      callHackage = import ./nix/callHackageFix.nix pkgs self;
    in fold (broken-test-pkg: acc:
      acc // {
        ${broken-test-pkg} =
          pkgs.haskell.lib.dontCheck super.${broken-test-pkg};
      }) {
        inherit callHackage;
        regex-tdfa = self.callHackage "regex-tdfa" "1.3.1.0" {
          regex-base = self.callHackage "regex-base" "0.94.0.0" { };
        };
      } [
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
