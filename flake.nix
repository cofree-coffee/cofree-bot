{
  description = "Cofree.Coffee Matrix Bot";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils}:
    let supportedGHCVersion = "8107";
        compiler = "ghc${supportedGHCVersion}";
        overlay = import ./overlay.nix { inherit compiler;};
        overlays = [ overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
        let pkgs = import nixpkgs { inherit system overlays; };
            supportedGHCVersion = "8107";
            hls = pkgs.haskell-language-server.override { inherit supportedGhcVersions; };
        in rec {
          devShell = pkgs.haskellPackages.shellFor {
            packages = _: [];
            buildInputs = with pkgs; [
              haskellPackages.cabal-install
              haskellPackages.ghc
              haskellPackages.brittany
              zlib
              hls
            ];
          };
          packages.docker = import ./docker.nix { inherit pkgs; };
          checks = {
            docker = packages.docker;
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                brittany.enable = true;
                cabal-fmt.enable = true;
              };
            };
          };
          defaultPackage =
            pkgs.haskellPackages.cofree-bot;
        });
}
