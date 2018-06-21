with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {bimaps = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.bimaps ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
