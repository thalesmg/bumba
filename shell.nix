{ compiler ? "ghc"
}:

let
  project = import ./default.nix { };
in
  project.shells.${compiler}
