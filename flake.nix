{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/24.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVersion = "963";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to do the evalPkgs trick so that IFD works with `nix flake check`
        # https://github.com/NixOS/nix/issues/4265
        evalPkgs = import nixpkgs { system = "x86_64-linux"; };

        # Our haskell packages override, needs to use evalPkgs because
        # cabal2nix uses IFD
        hsPkgs = evalPkgs.haskell.packages.${compiler}.override {
          overrides = hfinal: hprev: {
            chat-bots = hfinal.callCabal2nix "chat-bots" ./chat-bots/. { };
            chat-bots-contrib = hfinal.callCabal2nix "chat-bots" ./chat-bots-contrib/. { };
            cofree-bot = hfinal.callCabal2nix "cofree-bot" ./cofree-bot/. { };
            monoidal-functors = hfinal.callCabal2nix "monoidal-functors"
              (pkgs.fetchFromGitHub {
                owner = "solomon-b";
                repo = "monoidal-functors";
                rev = "eeb61da953592b7c01ab319b14f961e8f04c82c0";
                sha256 = "sha256-XnSffGuRTzr5LCrxu8x7AU3hmNk314Ip2ky2Z9xRJI0=";
              })
              { };
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
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal2nix
              cabal-install
              ghcid
              haskell.compiler.${compiler}
              haskell-language-server
              pkg-config
              ormolu
              shellcheck
              zlib
              zlib.dev
            ] ++ (builtins.attrValues scripts);
          };
        };

        packages = flake-utils.lib.flattenTree {
          docker = import ./nix/docker.nix {
            inherit pkgs;
            cofree-bot = hsPkgs.cofree-bot;
          };
          chat-bots = hsPkgs.chat-bots;
          chat-bots-contrib = hsPkgs.chat-bots-contrib;
          cofree-bot = hsPkgs.cofree-bot;
          default = hsPkgs.cofree-bot;
        };

        formatter = pkgs.nixfmt-rfc-style;
      });
}
