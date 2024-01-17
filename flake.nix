{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;

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
            bifunctors = hfinal.bifunctors_5_6_1;
            chat-bots = hfinal.callCabal2nix "chat-bots" ./chat-bots/. { };
            chat-bots-contrib = hfinal.callCabal2nix "chat-bots" ./chat-bots-contrib/. { };
            cofree-bot = hfinal.callCabal2nix "cofree-bot" ./cofree-bot/. { };
            monoidal-functors = hfinal.callCabal2nix "monoidal-functors"
              (pkgs.fetchFromGitHub {
                owner = "solomon-b";
                repo = "monoidal-functors";
                rev = "0c444c9f357538b245509b2a9b1be093c37f2017";
                sha256 = "sha256-ENMQndrmpsrSxSegsQcZdSA7dVh+fIvET20PTEP9NbY=";
              })
              { };
            semigroupoids = hfinal.semigroupoids_6_0_0_1;
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
            nixpkgs-fmt
            ormolu
            shellcheck
            zlib
          ] ++ (builtins.attrValues scripts);
        };

        packages = flake-utils.lib.flattenTree {
          docker = import ./nix/docker.nix {
            inherit pkgs;
            cofree-bot = hsPkgs.cofree-bot;
          };
          chat-bots = hsPkgs.chat-bots;
          chat-bots-contrib = hsPkgs.chat-bots-contrib;
          cofree-bot = hsPkgs.cofree-bot;
        };

        defaultPackage = packages.cofree-bot;
      });
}
