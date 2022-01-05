{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

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
    let overlay = import ./overlay.nix { compiler = "ghc8107"; };
        overlays = [ overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
        let pkgs = import nixpkgs { inherit system overlays; };
            hls = pkgs.callPackage easy-hls {
              ghcVersions = [ "8.10.7" ];
            };
        in rec {
          devShell = pkgs.haskellPackages.shellFor {
            packages = _: [];
            buildInputs = [
              pkgs.haskellPackages.cabal-install
              pkgs.haskellPackages.ghc
              pkgs.zlib
              hls
            ];
          };
          defaultPackage =
            pkgs.haskellPackages.cofree-bot;
          packages.docker = import ./docker.nix { inherit pkgs overlay; };
        } // { inherit overlay overlays; });
}
