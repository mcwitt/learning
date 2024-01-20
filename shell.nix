{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs =
    let
      ghcEnv = pkgs.haskellPackages.ghcWithHoogle
        (ps: with ps; [
          containers
          criterion
          deepseq
          hashable
          hashtables
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
      pkgs.cabal-install
      pkgs.haskell-language-server
    ];
}
