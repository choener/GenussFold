{ mkDerivation, ADPfusion, base, BenchmarkHistory, bits, containers
, DPutils, mmorph, mtl, OrderedBits, primitive, PrimitiveArray
, QuickCheck, stdenv, strict, tasty, tasty-quickcheck, tasty-th
, template-haskell, th-orphans, transformers, tuple, vector
}:
mkDerivation {
  pname = "ADPfusionSubword";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ADPfusion base bits containers DPutils mmorph mtl OrderedBits
    primitive PrimitiveArray QuickCheck strict template-haskell
    th-orphans transformers tuple vector
  ];
  testHaskellDepends = [
    ADPfusion base bits OrderedBits PrimitiveArray QuickCheck strict
    tasty tasty-quickcheck tasty-th vector
  ];
  benchmarkHaskellDepends = [
    base BenchmarkHistory PrimitiveArray vector
  ];
  homepage = "https://github.com/choener/ADPfusionSubword";
  description = "Efficient, high-level dynamic programming for CFGs on strings";
  license = stdenv.lib.licenses.bsd3;
}
