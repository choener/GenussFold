{
  description = ''
    Generic Haskell development flake
    This flake pins "nixpkgs" but not much more.
    Afterwards, the "real" nix shell with ghc 8.10.2 is called within a zsh shell.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    ghcicabal = { url = "github:choener/ghcicabal"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, flake-utils, ghcicabal }: let
    over = final: prev: {
      haskellPackages = (prev.haskellPackages.override{ overrides= hself: hsuper: let
          checked   = a: hself.callHackageDirect a {};
          unchecked = a: final.haskell.lib.dontCheck (checked a);
          unb       = a: final.haskell.lib.dontCheck (final.haskell.lib.unmarkBroken a);
        in {
          fused-effects = hself.fused-effects_1_1_0_0;
          lens          = hself.lens_4_19_2;
        };
      }).extend ( hself: hsuper: {
      });
    };
  in flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; overlays = [ ghcicabal.overlay self.overlay ]; };
      sharedBuildInputs = with pkgs; [  ];
    in  { devShell = pkgs.mkShell {
            #"NIX_PATH" = "nixpkgs=${pkgs}";
            shellHook = ''
              nix-shell --argstr pkgs=${pkgs} --run zsh
              exit
            '';
        };
       }) // {overlay = over;};
}

