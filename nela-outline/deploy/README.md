# Outline 1.9.2 theme image

The build recipe used for the October 3, 2026 deployment. It overlays the
compiled frontend on the pinned Outline 1.9.2 image, preserving the backend and
old hashed assets for browser tabs opened before the update.

Before running `build.sh`, prepare these local inputs beside it:

- `source/`: Outline v1.9.2 with `../patches/v1.9.2/` applied.
- `build-deps/`: expanded npm packages `browserslist-to-esbuild@1.2.0`,
  `rollup-plugin-webpack-stats@3.1.3`, and `rollup-plugin-stats@2.1.3`, each in
  a directory matching its package name. These match the v1.9.2 lockfile.
- Docker image `nela-outline-base:1.9.2`: the base image and digest named in
  `Dockerfile`, pulled and tagged locally.

`build.sh` uses the base image's application dependencies and the three build
dependencies above, then builds `nela-outline:1.9.2-nela-20261003`. It runs the
frontend build without network access, with one CPU and a 1 GiB memory limit.

The recipe is preserved from the release; its dependency preparation is manual.
The original release passed seven tests, TypeScript checking, a production
build, and public asset checksum verification. On October 16, the deployed
patch and the published v1.9.2 patch were confirmed to produce identical trees.

Use `compose.nela.yml` as an override in the operator's existing deployment
configuration. A stock recipe redeploy restores the stock image. Transfer or
publish the custom image before using another Swarm node. Rebase and rebuild
for every Outline upgrade; do not mix this frontend with another backend version.

Keep the prior image digest and deployment configuration for rollback. Private
service specifications, environment values, and secrets are not part of this
source export.
