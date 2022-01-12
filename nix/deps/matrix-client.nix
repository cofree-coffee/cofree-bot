{ mkDerivation
, aeson
, aeson-casing
, aeson-pretty
, base
, base64
, bytestring
, containers
, exceptions
, fetchgit
, hashable
, hspec
, http-client
, http-client-tls
, http-types
, lib
, profunctors
, retry
, SHA
, text
, time
, unordered-containers
}:
mkDerivation {
  pname = "matrix-client";
  version = "0.1.2.0";
  src = fetchgit {
    url = "https://github.com/softwarefactory-project/matrix-client-haskell";
    sha256 = "1am6xp93c48h76nfnwsfr5l54aw1wrmi1yy0ib7fy6ghc3bmd7pk";
    rev = "f8610d8956bd146105292bb75821ca078d01b5ff";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/matrix-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson
    aeson-casing
    base
    base64
    bytestring
    containers
    exceptions
    hashable
    http-client
    http-client-tls
    http-types
    profunctors
    retry
    SHA
    text
    time
    unordered-containers
  ];
  testHaskellDepends = [
    aeson
    aeson-casing
    aeson-pretty
    base
    base64
    bytestring
    containers
    exceptions
    hashable
    hspec
    http-client
    http-client-tls
    http-types
    profunctors
    retry
    SHA
    text
    time
    unordered-containers
  ];
  homepage = "https://github.com/softwarefactory-project/matrix-client-haskell#readme";
  description = "A matrix client library";
  license = lib.licenses.asl20;
}
