{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable"; # primary nixpkgs
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            # needs to be in nativeBuildInputs or clangd will complain about missing headers
            clang-tools
          ];
          buildInputs = with pkgs; [
            cabal-install
            haskell.compiler.ghc922
            ormolu
            haskell-language-server
            clang_13
            haskellPackages.alex
            haskellPackages.happy
            # for some reason this doesn't show up
            # need to do additional `nix-shell -p clang-tools`
            # clang-tools
          ];
        };
      }
    );
}
