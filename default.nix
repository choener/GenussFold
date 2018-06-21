with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {ForestStructures = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.ForestStructures ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
