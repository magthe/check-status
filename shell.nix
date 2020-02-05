with (import (builtins.fetchGit {
  name = "ghcide-for-nix";
  url = https://github.com/magthe/ghcide-for-nix;
  rev = "f48111e41c7a2b1b05bf46ddc4b46928d2a3e8d7";
}) );

let
  def = import ./default.nix;

  shell-pkgs = with haskellPackages;
    [cabal-install
     ghcid
     ghcide
     hlint
     ormolu
    ];

in
def.pkg.overrideAttrs (attrs: {
  src = null;
  buildInputs = shell-pkgs ++ attrs.buildInputs;
})
