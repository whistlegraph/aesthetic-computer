ARG BASE_IMAGE=outlinewiki/outline:1.9.2@sha256:32d76719c378931dd65d93945930ca380d8376a0337d98a991fcc12b266f33cf
FROM ${BASE_IMAGE}
LABEL computer.nela.theme="2026-10-03" \
      computer.nela.outline-base="eec58aecd42ecf9a7fb0cc20e95bbf7a121ea891"
# Retain old hashed assets for browser tabs opened before the release.
COPY --chown=nodejs:nodejs source/build/app/ /opt/outline/build/app/
