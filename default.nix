{ mkDerivation, ADPfusion, ADPfusionSubword, ansi-wl-pprint, base
, bytestring, cmdargs, containers, data-default, FormalGrammars
, fused-effects, lens, lib, mtl, PrimitiveArray, QuickCheck
, semigroups, template-haskell, test-framework
, test-framework-quickcheck2, test-framework-th, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "GenussFold";
  version = "0.0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ADPfusion ADPfusionSubword ansi-wl-pprint base bytestring
    containers data-default FormalGrammars fused-effects lens mtl
    PrimitiveArray semigroups template-haskell text transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [ base cmdargs FormalGrammars ];
  testHaskellDepends = [
    base QuickCheck test-framework test-framework-quickcheck2
    test-framework-th
  ];
  homepage = "https://github.com/choener/GenussFold";
  description = "MCFGs for Genus-1 RNA Pseudoknots";
  license = lib.licenses.gpl3Plus;
}
