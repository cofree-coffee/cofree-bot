#!/usr/bin/env bash
set -euxo pipefail

nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use cofree-coffee

nix build .#docker
image=$(docker load -i result | sed -n 's#^Loaded image: \([a-zA-Z0-9\.\/\-\:]*\)#\1#p')
docker push $image
