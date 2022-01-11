{ pkgs, overlay }:

pkgs.dockerTools.buildLayeredImage {
    name = "ghcr.io/cofree-coffee/cofree-bot";
    created = "now";
    tag = "latest";
    contents = [ 
      pkgs.bash
      pkgs.docker
      pkgs.cacert
    ];
    config = {
      Entrypoint = "${pkgs.bash}/bin/bash";
      Cmd = [ 
        "-c" "${pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.cofree-bot}/bin/cofree-bot run --auth_token $AUTH_TOKEN --homeserver https://matrix.cofree.coffee"
      ];
    };
  }
