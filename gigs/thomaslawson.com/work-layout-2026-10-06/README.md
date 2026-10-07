# Studio grids and Portrait of New York

Published October 6, 2026, from Fía's four iMessages and five screenshots.

The eleven older Studio periods use the recent-work grid: three columns on
desktop, two on tablet, one on phones. All 157 existing images, their order,
captions, image controls and artwork search destinations are retained.

Portrait of New York starts with its title and project details, followed by
the existing introduction, film and photographs. The film and title have
distinct IDs. Its nearly black Vimeo poster is replaced by an existing mural
photograph; the player and direct Vimeo link are retained. The stray project
details now sit below the title instead of near the footer.

`node build.mjs` embeds `layout.js` and `layout.css` into
`zzzzzz-tl-layout.php`. This isolated WordPress mu-plugin runs after the current
October modules. No database records or existing plugins are replaced.
Rollback: rename this one file out of the `.php` extension.

Validate with `node --check layout.js` and `php -l zzzzzz-tl-layout.php`.
`verify.mjs --evidence=<private directory>` checks saved production HTML with
the patch applied; `--live` checks the deployed pages. It covers every older
period and the mural page at 320, 390, 768 and 1440px, original artwork/caption
preservation, grid columns, overflow, image-viewer keyboard and focus behavior,
artwork search anchors, title/context order, visible poster loading and player
creation. `--smoke` limits checks to two representative pages at two widths.
Tests use Chrome desktop/mobile emulation, not physical devices. Vimeo blocks
this test connection, so player creation is checked separately from playback.

Private source snapshots, Fía's screenshots, deployment receipt, browser
results and rendered previews live in the client vault at
`site-refresh-2026-09/fia-feedback-2026-10-06/`.
