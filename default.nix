with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    DPutils = ../Lib-DPutils;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.DPutils ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
