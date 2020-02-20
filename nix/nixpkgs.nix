let
  nixpkgs-src = builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixpkgs-unstable-2020-02-20";
    url = "https://github.com/nixos/nixpkgs-channels";
    # Commit hash for nixos-unstable as of 2020-02-20
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "e2b4abe3c8f2e09adfc6a52007841d1b96c89371";
  };
in import nixpkgs-src { }
