{ mkDerivation, aeson, base, bifunctors, containers, criterion
, deepseq, fgl, lens, lib, QuickCheck, tasty, tasty-quickcheck
, tasty-th, unordered-containers, vector, vector-th-unbox
}:
mkDerivation {
  pname = "ForestStructures";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bifunctors containers deepseq fgl lens QuickCheck
    unordered-containers vector vector-th-unbox
  ];
  testHaskellDepends = [
    base containers QuickCheck tasty tasty-quickcheck tasty-th vector
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/choener/ForestStructures";
  description = "Tree- and forest structures";
  license = lib.licenses.bsd3;
}
