{ mkDerivation, ADPfusion, base, bits, containers, DPutils, mmorph
, mtl, OrderedBits, primitive, PrimitiveArray, QuickCheck, stdenv
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
  license = stdenv.lib.licenses.bsd3;
}
