{

  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVersion = "924";
      compiler = "ghc${ghcVersion}";
      # default systems compatible with pre-commit-hooks.nix
      # https://github.com/cachix/pre-commit-hooks.nix/pull/122
      defaultSystems = [
        "aarch64-linux"
        # "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem defaultSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to do the evalPkgs trick so that IFD works with `nix flake check`
        # https://github.com/NixOS/nix/issues/4265
        evalPkgs = import nixpkgs { system = "x86_64-linux"; };

        # Our haskell packages override, needs to use evalPkgs because
        # cabal2nix uses IFD
        hsPkgs = evalPkgs.haskell.packages.${compiler}.override {
          overrides = hfinal: hprev: {
            cofree-bot = hfinal.callCabal2nix "cofree-bot" ./. { };
            openai-hs = hfinal.callCabal2nix "openai-hs" (evalPkgs.fetchFromGitHub {
              owner = "agrafix";
              repo = "openai-hs";
              rev = "1238dfaabd065085c81d93754cfab93cecb7b471";
              sha256 = "sha256-kkKTruinN4W4J4OutAPNYtjzPgU7pKdQhHL0JRY5ItE=";
            } + "/openai-hs") { };
            openai-hs-servant = hfinal.callCabal2nix "openai-hs" (evalPkgs.fetchFromGitHub {
              owner = "agrafix";
              repo = "openai-hs";
              rev = "1238dfaabd065085c81d93754cfab93cecb7b471";
              sha256 = "sha256-kkKTruinN4W4J4OutAPNYtjzPgU7pKdQhHL0JRY5ItE=";
            } + "/openai-hs-servant") { };
          };
        };

        scripts = import ./nix/scripts.nix {
          s = pkgs.writeShellScriptBin;
          ormolu = pkgs.ormolu;
        };
      in
      rec {

        # Note: cannot reference anything that depends on `evalPkgs` like `hsPkgs`
        # otherwise non-x86_64-linux users will not be able to build the dev env
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal2nix
            cabal-install
            ghcid
            haskell.compiler.${compiler}
            haskell.packages.${compiler}.haskell-language-server
            ormolu
            zlib
          ] ++ (builtins.attrValues scripts);
        };

        packages = flake-utils.lib.flattenTree {
          docker = import ./nix/docker.nix {
            inherit pkgs;
            cofree-bot = hsPkgs.cofree-bot;
          };
          cofree-bot = hsPkgs.cofree-bot;
        };

        defaultPackage = packages.cofree-bot;
      });
}
