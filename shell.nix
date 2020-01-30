with (import (builtins.fetchGit {
  name = "ghcide-for-nix";
  url = https://github.com/magthe/ghcide-for-nix;
  rev = "be2220f1269cfdb5d5eb877ca5424b6e9c8e5d5b";
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
