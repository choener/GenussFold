let
  lib =
    { src = ./lib;
      dependencies = [ "bits" "primitive" "vector" "vector-algorithms" ];
      extensions = [
        "BangPatterns"
        "CPP"
        "FlexibleContexts"
        "PatternGuards"
        "ScopedTypeVariables"
        "UnicodeSyntax"
        "TemplateHaskell"
        ];
      packages = [];
    };
in
  lib
