let
  fetch = import ./fetch.nix;
in { nixpkgs ? fetch "nixpkgs" }: import nixpkgs {
  config = { allowUnfree = true; };
  overlays = import ./overlays.nix { inherit fetch; };
}
