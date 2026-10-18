# Image paths

`src/image-path.mjs` is the browser-safe image request lifecycle used by Aesel's
hosted picture tools and No Paint's web studio. `picture` uses `/api/easel-image`;
`nopaint` uses `/api/nopaint-inference`. A surface supplies its authenticated
request function. The selected path preserves its endpoint, timeout, request ID,
model, quoted price and receipt. These endpoints have different allowances.

One path permits one pending image request. Repeated submissions of the current
request ID share its promise; changed input with the same ID is rejected. Errors
are not automatically retried. Cancel rejects the proposed artwork while the
request continues to settle its receipt. No Paint owns its Paint/No decisions,
masking, local history and WIP saves in the private product.

`src/workspace-theme.mjs` provides Slab's Aesel status palettes and MIME's Los
Angeles daylight clock. No Paint maps its document and image path to those
states; completion survives reload through the saved review step. MIME's web
feed uses the same daylight function. Native Slab and Whistlegraph continue to
follow the operating system's appearance. Neither browser reads the host's
private Slab state.

The two browser modules are served through tracked file links in
`system/public/easel/src/`; native/CLI installs include them in `src/`.
