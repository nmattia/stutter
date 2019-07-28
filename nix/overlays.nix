{ sources }:
[
  # Some extra sources
  (self: super:
    { haskellPackages = super.haskellPackages.extend
          (super.haskell.lib.packageSourceOverrides
            { stutter = self.lib.cleanSourceWith
                { filter = name: type:
                    self.lib.strings.hasPrefix
                      (builtins.toString ../src)
                      name
                      ||
                    self.lib.strings.hasPrefix
                      (builtins.toString ../test)
                      name
                      ||
                    self.lib.strings.hasPrefix
                      (builtins.toString ../exe)
                      name
                      ||
                    (name == builtins.toString ../stutter.cabal) ||
                    (name == builtins.toString ../README.md) ||
                    (name == builtins.toString ../LICENSE) ||
                    (name == builtins.toString ../Setup.hs)
                    ;

                  src = self.lib.cleanSource ../.;
                };
              inherit (sources) snipcheck;
              niv = (import sources.niv { pkgs = super; }).niv-source;
            }
          );
    }
  )
]
