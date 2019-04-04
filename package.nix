let
  lib =
    { src = ./lib;
      dependencies = [
        "attoparsec"
        "lens"
        "pipes"
        "pipes-bytestring"
        "QuickCheck"
        "smallcheck"
        "streaming"
        "streaming-bytestring"
        ];
      extensions = [
        "BangPatterns"
        "CPP"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "GADTs"
        "MultiParamTypeClasses"
        "RankNTypes"
        "TypeFamilies"
        "UnicodeSyntax"
        ];
      packages = [];
    };
in
  lib
