{ pkgs ? <nixpkgs>, compiler ? null }:

# nixos 19-03

with import pkgs {};

let
  hsp = if compiler==null then haskellPackages else haskell.packages."${compiler}";
  hsPkgs0 = hsp.override {
    overrides = hself: hsuper:
      let compilerspecific = {
            "ghc8102" = { lens = hself.lens_4_19_2;
                          singletons = hself.singletons_2_7;
                        };
          };
      in
      {
        fused-effects = hself.fused-effects_1_1_0_0;
      } // compilerspecific.compiler or {};
  };
  #
  sourceOverrides = haskell.lib.packageSourceOverrides {
    GenussFold = ./.;
    #
    ADPfusion           = ./deps/ADPfusion;
    ADPfusionSubword    = ./deps/ADPfusionSubword;
    BiobaseTypes        = ./deps/BiobaseTypes;
    BiobaseXNA          = ./deps/BiobaseXNA;
    BiobaseTurner       = ./deps/BiobaseTurner;
    BiobaseENA          = ./deps/BiobaseENA;
    FormalGrammars      = ./deps/FormalGrammars;
    GrammarProducts     = ./deps/GrammarProducts;
    PrimitiveArray      = ./deps/PrimitiveArray;
    SciBaseTypes        = ./deps/SciBaseTypes;
    ViennaRNA-extras    = ./deps/ViennaRNA-extras;
    #
    bimaps           = ./deps/bimaps;
    DPutils          = ./deps/DPutils;
    ForestStructures = ./deps/ForestStructures;
    OrderedBits      = ./deps/OrderedBits;
    #
    ViennaRNA-bindings  = ./otherdeps/ViennaRNA-bindings;
  }; # haskellPackages override
  hsPkgs = (hsPkgs0.extend sourceOverrides).extend (hself: hsuper: {
    ViennaRNA-bindings = haskell.lib.dontCheck (hsuper.ViennaRNA-bindings);
  });
  # my own little tool
  cabalghcisrc =
    let local = ~/Documents/University/active/ghcicabal;
    in  if builtins.pathExists local
        then local
        else builtins.fetchGit {
          url = https://github.com/choener/ghcicabal;
          ref = "master";
        };
  cabalghci = hsPkgs.callPackage cabalghcisrc {};

in

hsPkgs.shellFor {
  packages = p: [
    p.GenussFold
    p.ADPfusion
    p.ADPfusionSubword
    p.bimaps
    p.BiobaseTypes
    p.BiobaseXNA
    p.BiobaseTurner
    p.BiobaseENA
    p.DPutils
    p.ForestStructures
    p.FormalGrammars
    p.GrammarProducts
    p.OrderedBits
    p.PrimitiveArray
    p.SciBaseTypes
#    p.ViennaRNA-bindings
    p.ViennaRNA-extras
  ];
  withHoogle = true;
  buildInputs = [
    cabal-install
    ({ "ghc8102" = llvm_9; }.${compiler} or llvm)
    cabalghci
  ];
} # shellFor

