#!/usr/bin/env bash
set -euxo pipefail

nix build .#docker
image=$(docker load -i result | sed -n 's#^Loaded image: \([a-zA-Z0-9\.\/\-\:]*\)#\1#p')
docker push $image
