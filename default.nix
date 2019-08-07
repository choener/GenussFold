{ mkDerivation, attoparsec, base, bytestring, containers, criterion
, kan-extensions, lens, mtl, parallel, pipes, pipes-bytestring
, pipes-parse, primitive, QuickCheck, smallcheck, stdenv, streaming
, streaming-bytestring, stringsearch, tasty, tasty-quickcheck
, tasty-smallcheck, tasty-th, timeit, transformers, vector
}:
mkDerivation {
  pname = "DPutils";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers kan-extensions lens mtl
    parallel pipes pipes-bytestring pipes-parse primitive QuickCheck
    smallcheck streaming streaming-bytestring stringsearch tasty
    tasty-quickcheck tasty-smallcheck tasty-th transformers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers kan-extensions lens mtl
    parallel pipes pipes-bytestring pipes-parse primitive QuickCheck
    smallcheck streaming streaming-bytestring stringsearch tasty
    tasty-quickcheck tasty-smallcheck tasty-th transformers vector
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring containers criterion kan-extensions lens
    mtl parallel pipes pipes-bytestring pipes-parse primitive
    QuickCheck smallcheck streaming streaming-bytestring stringsearch
    tasty tasty-quickcheck tasty-smallcheck tasty-th timeit
    transformers vector
  ];
  homepage = "https://github.com/choener/DPutils";
  description = "utilities for DP";
  license = stdenv.lib.licenses.bsd3;
}
