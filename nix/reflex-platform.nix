# $ nix-prefetch-url --unpack https://github.com/reflex-frp/reflex-platform/archive/develop.zip
let
  baseNixpkgs = import ./nixpkgs.nix;
  source = {
    reflex-platform = baseNixpkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "156ebb7c391ec909d19e794e591f107462862543";
      sha256 = "0rvbaimcvcica80538i6vxckv8si1gbd1ynvgxv7zr5jd1gk6vi5";
    };
  };
  reflex-platform = import source.reflex-platform {
    system = builtins.currentSystem;
    config.android_sdk.accept_license = true;
  };
in
  reflex-platform
