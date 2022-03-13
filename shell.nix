{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs =
    let ghcEnv = pkgs.haskellPackages.ghcWithHoogle
      (ps: with ps; [
        containers
        criterion
        deepseq
        hspec
        mtl
        mwc-probability
        mwc-random
        primitive
        vector
      ]);
    in
    [
      ghcEnv
      pkgs.haskellPackages.ormolu
      pkgs.haskell-language-server
    ];
}
