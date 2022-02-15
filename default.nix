{ mkDerivation, base, bits, criterion, lib, primitive, QuickCheck
, tasty, tasty-quickcheck, tasty-th, vector, vector-algorithms
}:
mkDerivation {
  pname = "OrderedBits";
  version = "0.0.2.0";
  src = ./.;
  libraryHaskellDepends = [
    base bits primitive vector vector-algorithms
  ];
  testHaskellDepends = [
    base bits primitive QuickCheck tasty tasty-quickcheck tasty-th
    vector vector-algorithms
  ];
  benchmarkHaskellDepends = [
    base bits criterion primitive vector vector-algorithms
  ];
  homepage = "https://github.com/choener/OrderedBits";
  description = "Efficient ordered (by popcount) enumeration of bits";
  license = lib.licenses.bsd3;
}
