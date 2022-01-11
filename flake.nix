{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    let
      compiler = "ghc8107";
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
        evalPkgs = import nixpkgs { system = "x86_64-linux"; };
        hsPkgs = pkgs.haskell.packages.${compiler}.override {
          overrides = hfinal: hprev: {
            cofree-bot = evalPkgs.haskell.packages.${compiler}.callCabal2nix "cofree-bot" ./. { };
            # command to reproduce:
            # cabal2nix https://github.com/softwarefactory-project/matrix-client-haskell --subpath matrix-client --revision f8610d8956bd146105292bb75821ca078d01b5ff > .nix/deps/matrix-client.nix
            matrix-client = hfinal.callPackage ./.nix/deps/matrix-client.nix { };
          };
        };
        brittany-config = pkgs.writeTextFile "brittany-config" (builtins.readFile ./brittany.yaml);
      in
      rec {

        devShell = hsPkgs.shellFor {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          packages = _: [ ];
          buildInputs = with pkgs; [
            hsPkgs.cabal-install
            hsPkgs.ghc
            hsPkgs.ghcid
            hsPkgs.brittany
            hsPkgs.haskell-language-server
            cabal2nix
            zlib
          ];
        };

        packages = flake-utils.lib.flattenTree {
          docker = import ./docker.nix {
            inherit pkgs;
            cofree-bot = hsPkgs.cofree-bot;
          };
          cofree-bot = hsPkgs.cofree-bot;
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              brittany = {
                name = "brittany";
                entry = "${hsPkgs.brittany}/bin/brittany --write-mode=inplace --config-file=${brittany-config}";
                files = "\\.l?hs$";
                files = "\\.(c|h)$";
                language = "system";
                pass_filenames = false;
              };
              cabal-fmt.enable = true;
            };
          };
        };

        defaultPackage = packages.cofree-bot;
      });
}
