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
, network-uri
, profunctors
, retry
, SHA
, text
, time
, unordered-containers
}:
mkDerivation {
  pname = "matrix-client";
  version = "0.1.4.0";
  src = fetchgit {
    url = "https://github.com/softwarefactory-project/matrix-client-haskell";
    sha256 = "1hcgbbn4p4nljlfwa0zqym8wa8cyl7b77pvyi0ynb7952p5hnfka";
    rev = "90a173a082ed916e6f6017002531e2f345205747";
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
    network-uri
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
    network-uri
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
