{ mkDerivation, ADPfusion, base, bits, containers, DPutils, lib
, mmorph, mtl, OrderedBits, primitive, PrimitiveArray, QuickCheck
, strict, template-haskell, th-orphans, transformers, tuple, vector
}:
mkDerivation {
  pname = "ADPfusionSubword";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    ADPfusion base bits containers DPutils mmorph mtl OrderedBits
    primitive PrimitiveArray QuickCheck strict template-haskell
    th-orphans transformers tuple vector
  ];
  homepage = "https://github.com/choener/ADPfusionSubword";
  description = "Efficient, high-level dynamic programming for CFGs on strings";
  license = lib.licenses.bsd3;
}
