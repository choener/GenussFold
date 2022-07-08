{
  description = ''
    GenussFold implements RNA folding with pseudoknots.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
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
          #fused-effects = hself.fused-effects_1_1_0_0;
          #lens          = hself.lens_4_19_2;
        };
      }).extend ( hself: hsuper: {
        GenussFold = hself.callPackage ./. {};
        #
        bimaps = hself.callPackage ./deps/bimaps {};
        ADPfusion = hself.callPackage ./deps/ADPfusion {};
        ADPfusionSubword = hself.callPackage ./deps/ADPfusionSubword {};
        DPutils = hself.callPackage ./deps/DPutils {};
        ForestStructures = hself.callPackage ./deps/ForestStructures {};
        FormalGrammars = hself.callPackage ./deps/FormalGrammars {};
        OrderedBits = hself.callPackage ./deps/OrderedBits {};
        PrimitiveArray = hself.callPackage ./deps/PrimitiveArray {};
      });
    };
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; overlays = [ ghcicabal.overlay self.overlay ]; };
      sharedBuildInputs = with pkgs; [ llvm ];
    in {
      # update dependencies via mr, develop the package, push changes, and update the flake
      # dependencies if major changes were made or before releasing
      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [
          p.GenussFold
          #
          p.ADPfusion
          p.ADPfusionSubword
          p.bimaps
          p.DPutils
          p.ForestStructures
          p.FormalGrammars
          p.OrderedBits
          p.PrimitiveArray
        ];
        withHoogle = true;
        buildInputs = with pkgs; [
          cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hls-tactics-plugin
          nodejs # required for lsp
          pkgs.ghcicabal # be explicit to get the final package
        ] ++ sharedBuildInputs;
      }; # devShell
      apps = {
        # Data source apps
        GenussFold = { type="app"; program="${pkgs.haskellPackages.GenussFold}/bin/GenussFold"; };
      };
      packages.WienRNA = pkgs.stdenv.mkDerivation {
        name = "GenussFold";
        unpackPhase = ".";
        buildPhase = ".";
        installPhase = ''
          mkdir -p $out/bin
          ln -s "${pkgs.haskellPackages.GenussFold}/bin/GenussFold $out/bin/GenussFold
        '';
      };
    }) // { overlay = over; };
}

