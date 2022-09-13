{
  description = "Haskell Parsing Benchmark";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    let
      ghcVersion = "922";
      compiler = "ghc${ghcVersion}";
      # default systems compatible with pre-commit-hooks.nix
      # https://github.com/cachix/pre-commit-hooks.nix/pull/122
      defaultSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      overlay = final: prev:
        {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              "${compiler}" = prev.haskell.packages."${compiler}".override {
                overrides = hfinal: hprev:
                  with prev.haskell.lib.compose; {
                    haskell-parsing-benchmark = dontCheck (hprev.callCabal2nix "haskell-parsing-benchmark" ./. { });
                  };
              };
            };
          };
        };
      overlays = [ overlay ];
    in
    flake-utils.lib.eachSystem defaultSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      rec {
        devShell = pkgs.haskell.packages.${compiler}.shellFor {
          packages = p: [ p.haskell-parsing-benchmark ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell.packages."${compiler}".ghcid
            pkgs.haskell.packages."${compiler}".haskell-language-server
            pkgs.python3
            pkgs.llvm
          ];
        };

        packages = flake-utils.lib.flattenTree {
          graphql-parser = pkgs.haskell.packages.${compiler}.graphql-parser;
        };

        defaultPackage = packages.graphql-parser;
      });
}
