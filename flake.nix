{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      ghcVersion = "982";
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
        hsPkgs = evalPkgs.haskell.packages.${compiler}.override {
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
          default = hsPkgs.shellFor {
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
      }
    );
}
