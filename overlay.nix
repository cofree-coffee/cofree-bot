{ compiler }:

final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${compiler}" = prev.haskell.packages."${compiler}".override (old: {
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
        }
      );
    };
  };
}
