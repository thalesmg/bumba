let
  pkgs = import <nixpkgs> {};
in
pkgs.python38.withPackages (ps:
  with ps;
  [
    requests
    flask
    flask-cors
  ]
)
