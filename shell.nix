let
  pkgs = import ./nix {};
in pkgs.haskellPackages.shellFor
  {
    packages = p: [ p.stutter ];
    withHoogle = false;
    buildInputs = [ pkgs.cabal-install ];
  }
