with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {OrderedBits = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.OrderedBits ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
