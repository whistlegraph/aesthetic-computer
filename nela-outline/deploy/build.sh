#!/bin/sh
set -eu
cd "$(dirname "$0")"

# The pinned base image must already be tagged nela-outline-base:1.9.2.
# build-deps contains browserslist-to-esbuild 1.2.0,
# rollup-plugin-webpack-stats 3.1.3 and rollup-plugin-stats 2.1.3,
# matching the v1.9.2 lockfile. No application dependencies are upgraded.
docker run --rm --name nela-theme-build \
  --network=none --cpus=1 --memory=1g --memory-swap=1g --user 0 \
  -e NODE_ENV=production -e NODE_OPTIONS=--max-old-space-size=512 \
  -e RAYON_NUM_THREADS=1 -e CI=1 \
  -v "$PWD/source:/src" -v "$PWD/build-deps:/build-deps:ro" \
  -w /src nela-outline-base:1.9.2 sh -c '
    ln -sfn /opt/outline/node_modules /src/node_modules
    cp -R /build-deps/* /opt/outline/node_modules/
    node /opt/outline/node_modules/vite/bin/vite.js build
  '
docker build --build-arg BASE_IMAGE=nela-outline-base:1.9.2 \
  -t nela-outline:1.9.2-nela-20261003 .
