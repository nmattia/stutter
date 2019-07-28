{}:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs
    {
      config = { allowUnfree = true; };
      overlays = import ./overlays.nix { inherit sources; };
    };
in
pkgs // { niv = pkgs.haskellPackages.niv; }
