{
  description = "fp labs 2023/24";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      hsPkgs =
        pkgs.haskell.packages."ghc946";

      buildInputs = [
        hsPkgs.ghc
        hsPkgs.haskell-language-server
        hsPkgs.hlint
        pkgs.cabal-install
      ];
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = buildInputs;

        # Make external Nix c libraries like zlib known to GHC, like
        # pkgs.haskell.lib.buildStackProject does
        # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
      };
    });
}
