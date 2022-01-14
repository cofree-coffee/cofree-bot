{ pkgs }:
let
  mkImage = pkg: cmd:
    pkgs.dockerTools.buildLayeredImage {
      name = "ghcr.io/cofree-coffee/${pkg.name}-repl";
      created = "now";
      tag = "latest";
      contents = [
        pkg
      ];
      config = {
        Cmd = cmd;
      };
    };

  mkPath = xs:
    with pkgs.lib;
    with pkgs.lib.attrsets;
    let paths = mapAttrsToList (name: image: "ln -s ${image} $out/${name}") xs;
    in "mkdir -p $out" + (foldl' (a: b: a + ";" + b) "" paths);

  images =
    {
      python = mkImage pkgs.python3 [ "python" "-iq" ];
      node = mkImage pkgs.nodejs [ "node" "-i" ];
      ghci = mkImage pkgs.ghc [ "ghci" ];
    };
in
pkgs.runCommand "repls" { } (mkPath images) 
