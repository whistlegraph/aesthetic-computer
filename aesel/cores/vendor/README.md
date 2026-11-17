# JSNES 2.1.0

Source: https://github.com/bfirsh/jsnes/tree/v2.1.0

`jsnes-2.1.0.min.cjs` is the same file as
`system/public/nes-player/vendor/jsnes-2.1.0.min.js`, the npm `jsnes@2.1.0`
distribution with only its source-map comment removed. It is renamed `.cjs`
because Aesel is an ES module package and this file is CommonJS.
License: Apache-2.0, reproduced in `LICENSE.jsnes`.

It lives here as well as beside the web player because the Aesel tarball packs
only `aesel/`, and a core has to run on a machine with no checkout.
