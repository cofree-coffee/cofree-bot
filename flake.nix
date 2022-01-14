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
      ghcVersion = "8107";
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

            # command to reproduce:
            # cabal2nix https://github.com/softwarefactory-project/matrix-client-haskell --subpath matrix-client --revision f8610d8956bd146105292bb75821ca078d01b5ff > .nix/deps/matrix-client.nix
            matrix-client = hfinal.callPackage ./nix/deps/matrix-client.nix { };
          };
        };

        brittany-config = pkgs.writeTextFile {
          name = "brittany-config";
          text = builtins.readFile ./brittany.yaml;
        };

        scripts = import ./nix/scripts.nix {
          inherit brittany-config;
          s = pkgs.writeShellScriptBin;
          brittany = pkgs.haskellPackages.brittany;
        };
      in
      rec {

        # Note: cannot reference anything that depends on `evalPkgs` like `hsPkgs`
        # otherwise non-x86_64-linux users will not be able to build the dev env
        devShell = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          buildInputs = with pkgs; [
            haskellPackages.brittany
            (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
            cabal-install
            ghc
            ghcid
            cabal2nix
            zlib
          ] ++ (builtins.attrValues scripts);
          # ];
        };

        packages = flake-utils.lib.flattenTree {
          docker = import ./nix/docker.nix {
            inherit pkgs;
            cofree-bot = hsPkgs.cofree-bot;
          };
          repls = import ./nix/repl-containers.nix {
            inherit pkgs;
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
