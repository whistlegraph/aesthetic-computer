# Shared notebook renderer

Native Mac and iPhone Aesel bundle this renderer, its vendored libraries and
attributed artwork through `apple/aesel/bundle-session.sh`. This directory has
no Electron entry point, app packaging or update channel.

Rebuild the vendored rich-text libraries with `scripts/rich/build.mjs`.
