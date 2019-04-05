let
  lib =
    { src = ./lib;
      dependencies = [
        "aeson"
        "binary"
        "cereal"
        "cereal-vector"
        "containers"
        "deepseq"
        "hashable"
        "primitive"
        "storable-tuple"
        "unordered-containers"
        "vector"
        "vector-binary-instances"
        "vector-th-unbox"
        ];
      extensions = [
        "BangPatterns"
        "ConstraintKinds"
        "CPP"
        "DataKinds"
        "DefaultSignatures"
        "DeriveDataTypeable"
        "DeriveFoldable"
        "DeriveFunctor"
        "DeriveGeneric"
        "DeriveTraversable"
        "FlexibleContexts"
        "FlexibleInstances"
        "FunctionalDependencies"
        "GADTs"
        "GeneralizedNewtypeDeriving"
        "MultiParamTypeClasses"
        "PatternGuards"
        "PatternSynonyms"
        "PolyKinds"
        "RankNTypes"
        "RecordWildCards"
        "ScopedTypeVariables"
        "StandaloneDeriving"
        "TemplateHaskell"
        "TupleSections"
        "TypeApplications"
        "TypeFamilies"
        "TypeOperators"
        "UndecidableInstances"
        "UnicodeSyntax"
        ];
      # these are my (local) packages
      packages = [
        ];
    };
in
  lib

