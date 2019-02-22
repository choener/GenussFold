{ mkDerivation, attoparsec, base, bytestring, containers, criterion
, kan-extensions, lens, mtl, parallel, pipes, pipes-bytestring
, pipes-parse, QuickCheck, stdenv, streaming, streaming-bytestring
, stringsearch, tasty, tasty-quickcheck, tasty-th, timeit
, transformers, vector
}:
mkDerivation {
  pname = "DPutils";
  version = "0.0.2.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers kan-extensions parallel pipes
    QuickCheck streaming streaming-bytestring stringsearch transformers
    vector
  ];
  testHaskellDepends = [
    base bytestring containers lens mtl pipes pipes-bytestring
    pipes-parse QuickCheck streaming streaming-bytestring tasty
    tasty-quickcheck tasty-th vector
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion streaming streaming-bytestring timeit
    vector
  ];
  homepage = "https://github.com/choener/DPutils";
  description = "utilities for DP";
  license = stdenv.lib.licenses.bsd3;
}
