{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc902
    ormolu
    haskell-language-server
  ];
}
