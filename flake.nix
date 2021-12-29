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
    let overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
              (hfinal: hprev: {
                cofree-bot = hfinal.callCabal2nix "cofree-bot" ./. {};
                matrix-client = hfinal.callCabal2nixWithOptions "matrix-client" (final.fetchFromGitHub {
                  owner = "cofree-coffee";
                  repo = "matrix-client-haskell";
                  rev = "3aad90a0c836dfb1235e05ab6311d340f7a48dcf";
                  sha256 = "sha256-96AQqAHl8PLRdlhL/hoxcGpfXGMZxsB6Pmgzjl1uDt4=";
                }) "--subpath matrix-client" { };
              });
          });
        };
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
        } // { inherit overlay overlays; });
}
