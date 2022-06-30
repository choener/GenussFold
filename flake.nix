{
  description = ''
    GenussFold implements RNA folding with pseudoknots.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    ghcicabal = { url = "github:choener/ghcicabal"; inputs.nixpkgs.follows = "nixpkgs"; };
    bimaps-src = {
      url = "github:choener/bimaps";
      flake = false;
    };
    ADPfusion-src = {
      url = "github:choener/ADPfusion";
      flake = false;
    };
    ADPfusionSubword-src = {
      url = "github:choener/ADPfusionSubword";
      flake = false;
    };
    DPutils-src = {
      url = "github:choener/DPutils";
      flake = false;
    };
    ForestStructures-src = {
      url = "github:choener/ForestStructures";
      flake = false;
    };
    FormalGrammars-src = {
      url = "github:choener/FormalGrammars";
      flake = false;
    };
    GrammarProducts-src = {
      url = "github:choener/GrammarProducts";
      flake = false;
    };
    OrderedBits-src = {
      url = "github:choener/OrderedBits";
      flake = false;
    };
    PrimitiveArray-src = {
      url = "github:choener/PrimitiveArray";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ghcicabal
            , bimaps-src, ADPfusion-src, ADPfusionSubword-src, DPutils-src, ForestStructures-src
            , FormalGrammars-src, GrammarProducts-src, OrderedBits-src, PrimitiveArray-src }: let
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
        bimaps = hself.callPackage bimaps-src {};
        ADPfusion = hself.callPackage ADPfusion-src {};
        ADPfusionSubword = hself.callPackage ADPfusionSubword-src {};
        DPutils = hself.callPackage DPutils-src {};
        ForestStructures = hself.callPackage ForestStructures-src {};
        FormalGrammars = hself.callPackage FormalGrammars-src {};
        GrammarProducts = hself.callPackage GrammarProducts-src {};
        OrderedBits = hself.callPackage OrderedBits-src {};
        PrimitiveArray = hself.callPackage PrimitiveArray-src {};
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
          p.DPutils
          p.FormalGrammars
          p.GrammarProducts
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

