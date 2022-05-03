{ mkDerivation, aeson, base, binary, cereal, cereal-vector
, containers, criterion, deepseq, hashable, lib, mwc-random
, primitive, QuickCheck, storable-tuple, tasty, tasty-quickcheck
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
    aeson base binary cereal cereal-vector containers deepseq hashable
    primitive QuickCheck storable-tuple tasty tasty-quickcheck tasty-th
    unordered-containers vector vector-binary-instances vector-th-unbox
  ];
  benchmarkHaskellDepends = [
    aeson base binary cereal cereal-vector containers criterion deepseq
    hashable mwc-random primitive storable-tuple unordered-containers
    vector vector-binary-instances vector-th-unbox
  ];
  homepage = "https://github.com/choener/bimaps";
  description = "bijections with multiple implementations";
  license = lib.licenses.bsd3;
}
