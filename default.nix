with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {DPutils = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.DPutils ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
