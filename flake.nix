{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.05;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    easy-hls = {
      url = github:ssbothwell/easy-hls-nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, easy-hls, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          hls = pkgs.callPackage easy-hls {
            ghcVersions = [ "8.10.4" ];
          };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = _: [];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.zlib
            pkgs.zlib.dev
            pkgs.pkg-config
            hls
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.zlib}/lib
            export DYLD_LIBRARY_PATH=${pkgs.zlib}/lib
            export PATH=$PATH:$HOME/.local/bin
          '';
        };
        defaultPackage =
          pkgs.haskellPackages.callCabal2nix "cofree-bot" ./. {};
      });
}
