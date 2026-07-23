{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      ghcVersion = "9103";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to do the evalPkgs trick so that IFD works with `nix flake check`
        # https://github.com/NixOS/nix/issues/4265
        evalPkgs = import nixpkgs { system = "x86_64-linux"; };

        # Our haskell packages override, needs to use evalPkgs because
        # cabal2nix uses IFD
        mkHsPkgs =
          compiler:
          evalPkgs.haskell.packages.${compiler}.override {
            overrides = hfinal: hprev: {
              chat-bots = hfinal.callCabal2nix "chat-bots" ./chat-bots/. { };
              chat-bots-contrib = hfinal.callCabal2nix "chat-bots" ./chat-bots-contrib/. { };
              cofree-bot = hfinal.callCabal2nix "cofree-bot" ./cofree-bot/. { };
              list-t = hfinal.callCabal2nix "list-t" ./list-t/. { };
              machines-coalgebras = hfinal.callCabal2nix "list-t" ./machines-coalgebras/. { };
              monoidal-functors = hfinal.callCabal2nix "monoidal-functors" (pkgs.fetchFromGitHub {
                owner = "solomon-b";
                repo = "monoidal-functors";
                rev = "e951a2be496d57d7f4e5b582ad175e8e97a9ab7b";
                sha256 = "sha256-HfIMuU9yBp0JtN/ONOFku1wItbGLJl09fhaFzyiNVMg=";
              }) { };
              # Jailbreak relaxes matrix-client's stale `hashable <1.5` / `time <1.13`
              # upper bounds. nixpkgs 26.05 ships hashable 1.5 (GHC >= 9.6) and GHC
              # 9.12 ships time 1.14; the pinned rev's bounds predate both. This
              # strips the bounds without changing the pinned version.
              matrix-client = pkgs.haskell.lib.doJailbreak (hfinal.callCabal2nix "matrix-client" (pkgs.fetchFromGitHub {
                owner = "softwarefactory-project";
                repo = "matrix-client-haskell";
                rev = "57f13c2579fbcd9088fbb6b25046460e6cdbfdb0";
                sha256 = "sha256-GNCnlaxWhg5pDi8JYlXyrqVIljr7NcNy+tWYSb+YYAg=";
              }) { });
            };
          };

        mkShellFor =
          { compiler }:
          (mkHsPkgs compiler).shellFor {
            packages = p: [
              p.chat-bots
              p.chat-bots-contrib
              p.cofree-bot
              p.monoidal-functors
              p.list-t
            ];
            buildInputs =
              with pkgs;
              [
                cabal2nix
                cabal-install
                ghcid
                haskell.compiler.${compiler}
                haskell.packages.${compiler}.haskell-language-server
                just
                pkg-config
                ormolu
                shellcheck
                zlib
                zlib.dev
              ]
              ++ (builtins.attrValues scripts);
          };

        mkPkgsFor = compiler: {
          recurseForDerivations = true;
          chat-bots = (mkHsPkgs compiler).chat-bots;
          chat-bots-contrib = (mkHsPkgs compiler).chat-bots-contrib;
          cofree-bot = (mkHsPkgs compiler).cofree-bot;
        };

        hsPkgs = mkHsPkgs compiler;

        scripts = import ./nix/scripts.nix {
          s = pkgs.writeShellScriptBin;
          ormolu = pkgs.ormolu;
        };
      in
      rec {

        # Note: cannot reference anything that depends on `evalPkgs` like `hsPkgs`
        # otherwise non-x86_64-linux users will not be able to build the dev env
        devShells = {
          default = mkShellFor { inherit compiler; };
          # No ghc948 dev shell: haskell-language-server >= 2.12 (nixpkgs 26.05)
          # dropped GHC 9.4 support. 9.4.8 remains a build target below.
          ghc967 = mkShellFor { compiler = "ghc967"; };
          ghc984 = mkShellFor { compiler = "ghc984"; };
          ghc9103 = mkShellFor { compiler = "ghc9103"; };
          ghc9124 = mkShellFor { compiler = "ghc9124"; };
        };

        packages = flake-utils.lib.flattenTree rec {
          docker = import ./nix/docker.nix {
            inherit pkgs;
            cofree-bot = hsPkgs.cofree-bot;
          };
          ghc948 = mkPkgsFor "ghc948";
          ghc967 = mkPkgsFor "ghc967";
          ghc984 = mkPkgsFor "ghc984";
          ghc9103 = mkPkgsFor "ghc9103";
          ghc9124 = mkPkgsFor "ghc9124";
          chat-bots = hsPkgs.chat-bots;
          chat-bots-contrib = hsPkgs.chat-bots-contrib;
          cofree-bot = hsPkgs.cofree-bot;
          scripts = {
            recurseForDerivations = true;
            run-cofree-bot = pkgs.writeShellScriptBin "run-bot" ''
              echo $CR_PAT | ${pkgs.docker}/bin/docker login ghcr.io -u solomon-b --password-stdin
                ${pkgs.docker}/bin/docker compose -f docker-compose.yaml up --detach
            '';
            run-cofree-bot-cli = pkgs.writeShellScriptBin "run-bot" ''
              ${cofree-bot}/bin/cofree-bot -- cli
            '';
          };
          default = hsPkgs.cofree-bot;
        };

        apps = {
          run-cofree-bot = flake-utils.lib.mkApp { drv = self.packages.${system}."scripts/run-cofree-bot"; };
          run-cofree-bot-cli = flake-utils.lib.mkApp {
            drv = self.packages.${system}."scripts/run-cofree-bot-cli";
          };
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
