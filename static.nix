let
  compiler = "ghc865";

  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/nh2/nixpkgs/archive/83fd89945d355892e0e95747c9a2c519491c1600.tar.gz";
    sha256 = "05jybj66gdzdmjgawa2a72b6pf669rfb6pljhlc3lpyq6dlnw87d";
  };

  static-haskell-nix = builtins.fetchTarball {
    url    = "https://github.com/nh2/static-haskell-nix/archive/c360f2a15f6947b411ecbd7ebaea925f6dbd68df.tar.gz";

    sha256 = "0y6ppiagh6dbvdhhnrq572xnw2yzn6d0gcmajrfdgdfwhsl21g95";
  };

  overlay = import ./overlay.nix { inherit compiler; };

  normalPkgs = import nixpkgs {
    system = "x86_64-linux";

    config = { };

    overlays = [ overlay ];
  };

  staticHaskell = import "${static-haskell-nix}/survey/default.nix" {
    inherit compiler normalPkgs;
  };

  staticHaskell_ = import "${static-haskell-nix}/survey/default.nix" {
    inherit compiler normalPkgs;
  };
in
  staticHaskell.haskellPackages.cofree-bot
