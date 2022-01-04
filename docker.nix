let
  compiler = "ghc8107";
  overlay = import ./overlay.nix { inherit compiler; };
  pkgs = import <nixpkgs> { overlays = [overlay]; };
in
  { myAppImage = pkgs.dockerTools.buildLayeredImage {
      name = "cofree.coffee/cofree-bot";
      created = "now";
      contents = [ 
        pkgs.bash
        pkgs.coreutils
        pkgs.cacert
      ];
      config = {
        Cmd = [ 
          "${pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.cofree-bot}/bin/cofree-bot" "run" "--auth_token" "xyz" "--homeserver" "https://matrix.cofree.coffee"
        ];
      };
    };
  }
