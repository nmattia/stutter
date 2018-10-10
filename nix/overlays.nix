{ fetch ? import ./fetch.nix }:
[
  # Some extra sources
  (self: super:
    { haskellPackages = super.haskellPackages.extend
          (super.haskell.lib.packageSourceOverrides
            { stutter = self.lib.cleanSource ../.;
              snipcheck = fetch "snipcheck";
            }
          );
    }
  )
]
