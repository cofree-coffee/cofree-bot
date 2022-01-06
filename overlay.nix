{ compiler }:
final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${compiler}" = prev.haskell.packages."${compiler}".override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
          (hfinal: hprev: {
            cofree-bot = hfinal.callCabal2nix "cofree-bot" ./. {};
            matrix-client = hfinal.callCabal2nixWithOptions "matrix-client" (final.fetchFromGitHub {
              owner = "softwarefactory-project";
              repo = "matrix-client-haskell";
              rev = "f8610d8956bd146105292bb75821ca078d01b5ff";
              sha256 = "sha256-855W12DwGe/OisD7EGvmgStSaMlOc+usORARNtLtpqo=";
            }) "--subpath matrix-client" { };
          });
        }
      );
    };
  };
}
