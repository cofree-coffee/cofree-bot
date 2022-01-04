let
  compiler = "ghc8107";
  overlay = import ./overlay.nix { inherit compiler; };
  pkgs = import <nixpkgs> { overlays = [overlay]; };
in
  { myAppImage = pkgs.dockerTools.buildImage {
      name = "cofree.coffee/cofree-bot";
      contents = [ 
        pkgs.haskellPackages.cofree-bot
        pkgs.bash
        pkgs.coreutils
        pkgs.cacert
      ];
      config = {
        Cmd = [ 
          "${pkgs.haskellPackages.cofree-bot}/bin/cofree-bot" "run" "--auth_token" "xyz" "--homeserver" "https://matrix.cofree.coffee"
        ];
      };
    };
  }
