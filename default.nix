with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-ADPfusion
    ../Lib-DPutils
    ../Lib-OrderedBits
    ../Lib-PrimitiveArray
  ]) // {ADPfusionSubword = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.ADPfusionSubword ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      ADPfusion
      DPutils
      OrderedBits
      PrimitiveArray
    ];
  };
}
