#!/bin/bash

# Build the container with a simple name
echo "Building aesthetic-computer dev container..."
docker build -t aesthetic-computer:dev /Users/jas/Desktop/code/aesthetic-computer/.devcontainer

# Stop and remove any existing container
docker stop aesthetic 2>/dev/null || true
docker rm aesthetic 2>/dev/null || true

# Run the container with the same configuration as devcontainer.json
echo "Starting aesthetic-computer dev container..."
docker run -d \
  --name aesthetic \
  --hostname aesthetic \
  --init \
  --memory=8g \
  --memory-swap=8g \
  --shm-size=2g \
  --cgroupns=host \
  --tmpfs=/tmp:noexec,nosuid,size=2g \
  --cap-add=SYS_PTRACE \
  --security-opt=apparmor=unconfined \
  --ulimit nofile=131072:131072 \
  -p 0.0.0.0:8888:8888 \
  -p 0.0.0.0:8889:8889 \
  -v /Users/jas/Desktop/code/aesthetic-computer:/workspaces/aesthetic-computer:cached \
  -v /Users/jas/Desktop/code/aesthetic-computer/.devcontainer/.emacs.d:/home/me/.emacs.d:delegated \
  -v /Users/jas/Desktop/code/aesthetic-computer/.devcontainer/envs:/home/me/envs:delegated \
  -v /Users/jas/Desktop/code/aesthetic-computer/.devcontainer/fish_history:/home/me/.local/share/fish/fish_history:delegated \
  -v /Users/jas/Desktop/code/aesthetic-computer/.devcontainer/tezos-data:/home/me/.tezos-client:delegated \
  -v /Users/jas/Desktop/code/aesthetic-computer/.devcontainer/tezos-node:/home/me/.tezos-node:delegated \
  -v codespaces-linux-var-lib-docker:/var/lib/docker \
  -v /var/run/docker.sock:/var/run/docker-host.sock \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v vscode:/vscode \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  -e DISPLAY="${DISPLAY}" \
  -e HOST_IP="${HOST_IP}" \
  -e WSL_DISTRO_NAME="${WSL_DISTRO_NAME}" \
  -e DOCKER_BUILDKIT=1 \
  -e COMPOSE_DOCKER_CLI_BUILD=1 \
  aesthetic-computer:dev \
  /bin/sh -c "fish /entry.fish && sleep infinity"

echo "Container started. You can now connect VS Code to it."
echo "Use 'Dev Containers: Attach to Running Container' and select 'aesthetic'"
