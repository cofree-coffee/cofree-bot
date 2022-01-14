#!/usr/bin/env bash
set -euxo pipefail

nix build .#deploy-bot
./result
