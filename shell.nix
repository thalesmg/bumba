{ compiler ? "ghc"
}:

let
  project = import ./default.nix { inherit compiler; };
in
  project.shells.${compiler}
