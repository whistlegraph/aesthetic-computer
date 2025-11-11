#!/usr/bin/env fish
# Run Octez tools via Docker with latest version
#
# This solves the protocol version mismatch issue by using the latest Octez Docker image
# instead of the outdated v20.3 binary in the devcontainer.
#
# Usage:
#   ./octez-docker.fish client --help
#   ./octez-docker.fish client --endpoint https://ghostnet.ecadinfra.com get balance for <address>
#   ./octez-docker.fish client originate contract ...

set OCTEZ_VERSION "master"  # Latest build with current protocol support
set OCTEZ_IMAGE "tezos/tezos:$OCTEZ_VERSION"

# Check if Docker is available
if not command -v docker &>/dev/null
    echo "❌ Error: Docker not available"
    exit 1
end

# Get the tool name (client, node, etc.)
set tool $argv[1]
set -e argv[1]

# Run Octez in Docker
docker run --rm -it \
    -v (pwd):/workspace \
    -v $HOME/.tezos-client:/root/.tezos-client \
    -w /workspace \
    $OCTEZ_IMAGE \
    octez-$tool $argv
