{ mkDerivation, aeson, base, binary, cereal, cereal-vector
, containers, criterion, deepseq, hashable, mwc-random, primitive
, QuickCheck, stdenv, storable-tuple, tasty, tasty-quickcheck
, tasty-th, unordered-containers, vector, vector-binary-instances
, vector-th-unbox
}:
mkDerivation {
  pname = "bimaps";
  version = "0.1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary cereal cereal-vector containers deepseq hashable
    primitive storable-tuple unordered-containers vector
    vector-binary-instances vector-th-unbox
  ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-quickcheck tasty-th
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq mwc-random unordered-containers
    vector
  ];
  homepage = "https://github.com/choener/bimaps";
  description = "bijections with multiple implementations";
  license = stdenv.lib.licenses.bsd3;
}
