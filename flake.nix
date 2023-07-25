{
  description = "machines-png";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          machines-png =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc962";
              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = "2.0.0.0";
                  ghcid = {};
                };
              };
              modules = [{
                enableLibraryProfiling = true;
              }];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.machines-png.flake {};
    in flake);
  nixConfig = {
    binaryCaches = [ "https://cache.iog.io" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
}
