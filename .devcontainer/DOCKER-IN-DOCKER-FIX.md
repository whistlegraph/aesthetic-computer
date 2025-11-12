# Docker-in-Docker Volume Mount Fix

## Problem
When using Docker-in-Docker from inside a dev container, volume mounts don't work with paths like `$(pwd)` or `/workspaces/aesthetic-computer` because the Docker daemon runs on the HOST, not inside the dev container.

## Solution
Use the HOST path instead of the container path.

### Finding the Host Path
```bash
# Get your dev container name
set CONTAINER_NAME (cat /etc/hostname)

# Find the host source path for your workspace
docker inspect $CONTAINER_NAME | jq '.[0].Mounts[] | select(.Destination == "/workspaces/aesthetic-computer") | .Source'
# Output: "/home/me/aesthetic-computer"
```

### Using Docker with Correct Paths

**❌ WRONG (doesn't work in dev container):**
```bash
cd /workspaces/aesthetic-computer/tezos
docker run --rm -v (pwd):/workspace smartpy-amd64:0.23.1 python3 /workspace/script.py
# Error: Files not visible in container
```

**✅ CORRECT (works):**
```bash
cd /workspaces/aesthetic-computer/tezos  
docker run --rm -v /home/me/aesthetic-computer/tezos:/workspace smartpy-amd64:0.23.1 python3 /workspace/script.py
# Success: Files are accessible
```

## SmartPy Compilation Command

```bash
# Compile SmartPy contracts from dev container
cd /workspaces/aesthetic-computer/tezos
docker run --rm -v /home/me/aesthetic-computer/tezos:/workspace \
  smartpy-amd64:0.23.1 \
  python3 /workspace/keeps_fa2_final.py
```

## Quick Reference

For the `tezos/` directory specifically, use this command:

```fish
# From /workspaces/aesthetic-computer/tezos
docker run --rm -v /home/me/aesthetic-computer/tezos:/workspace smartpy-amd64:0.23.1 python3 /workspace/YOUR_SCRIPT.py
```

### Example: Compile the Keeps FA2 Contract
```fish
cd /workspaces/aesthetic-computer/tezos
docker run --rm -v /home/me/aesthetic-computer/tezos:/workspace smartpy-amd64:0.23.1 python3 /workspace/keeps_fa2_final.py
```

## Why This Happens

1. The dev container is itself a Docker container running on the host
2. When you run `docker` commands inside the dev container, you're using Docker-in-Docker
3. The Docker socket is bind-mounted from the host (`/var/run/docker.sock`)
4. Volume mounts are resolved by the HOST Docker daemon, not the container
5. `/workspaces/aesthetic-computer` exists in the container but not on the host
6. The host path is `/home/me/aesthetic-computer` (mapped to `/workspaces/aesthetic-computer` inside container)

## Automatic Detection

You can create a helper script:

```bash
#!/usr/bin/env fish
# ~/bin/docker-dev
set HOST_PATH (docker inspect (cat /etc/hostname) | jq -r '.[0].Mounts[] | select(.Destination == "/workspaces/aesthetic-computer") | .Source')
set CONTAINER_PATH /workspaces/aesthetic-computer
set REL_PATH (string replace $CONTAINER_PATH "" (pwd))
set MOUNT_PATH "$HOST_PATH$REL_PATH"

docker run --rm -v "$MOUNT_PATH:/workspace" $argv
```

Usage:
```bash
docker-dev smartpy-amd64:0.23.1 python3 /workspace/script.py
```
