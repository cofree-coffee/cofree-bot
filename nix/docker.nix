{ pkgs, cofree-bot }:

pkgs.dockerTools.buildLayeredImage {
  name = "ghcr.io/cofree-coffee/cofree-bot";
  created = "now";
  tag = "latest";
  contents = [
    pkgs.bash
    pkgs.cacert
    pkgs.docker-client
  ];
  config = {
    Entrypoint = "${pkgs.bash}/bin/bash";
    Cmd = [
      "-c"
      "${pkgs.haskell.lib.justStaticExecutables cofree-bot}/bin/cofree-bot run"
    ];
  };
}
