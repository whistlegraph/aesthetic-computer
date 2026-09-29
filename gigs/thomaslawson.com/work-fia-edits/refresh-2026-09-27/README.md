# TL design refresh — 2026-09-27

One design language over thomaslawson.com, shipped as a regular WP plugin
(`tl-design-refresh`) layered on top of the Fía polish mu-plugin. Deactivate
it to return to the polish-only site exactly.

- `refresh.css`, `refresh.js` — the source. Edit these.
- `node build-plugin.mjs <version>` → `plugin/tl-design-refresh.zip` (upload via wp-admin → Plugins → Add New → Upload).
- `./build-preview.sh` → `preview.css` + `inject.js` for headless preview.
- `tools/capture.mjs <name> [css] [js]` — screenshots every page in `pages.json`
  (desktop 1440 + mobile 390@2x) into `evidence/<name>/` with a type inventory
  per page. `TL_ONLY=slug,slug`, `TL_VP=desktop|mobile`, `TL_JOBS=n`. Needs
  `puppeteer-core` + `sharp` (install in a scratch dir, run from there).
- `tools/sheet.mjs`, `tools/compare.mjs` — contact sheets and before|after pairs.

Findings (before): 45 distinct desktop type styles across 6 families; Gotham
and Felix Titling were named but never loaded as web fonts; titles printed over
photographs; drop-shadow frames; all-caps captions; em-based Elementor paddings
that misaligned text from headings.

Live 2026-09-27: plugin v1.2.0 (v1.0.0 → v1.1.0 larger px scale → v1.2.0 eyebrow) installed + activated via wp-admin; served
CSS/JS verified byte-identical to source. Measured over all 49 pages
(`evidence/before` vs `evidence/after`, live, logged out):

| | before | after |
|---|---|---|
| font families | 6 (Poppins, Roboto, Gotham, Adobe Jenson, Georgia, Felix Titling) | 2 (Inter, Newsreader) |
| desktop type styles | 45 | 24 |
| visible text under 14px (desktop) | captions ~12px sitewide | none |
| horizontal overflow | 0 pages | 0 pages |

Side-by-side pairs for every page and both viewports: `evidence/compare/`.

Open for Fía/Tom: News banner is a 225×300 snapshot with baked-in text;
page title "Pat Douthewaite" → Douthwaite.

v1.5.0 (built 2026-09-28, not yet uploaded) — Fía's 2026-09-28 notes:
section headers become image-left / text-right (after the Burning Torch
About pages; the picture whole at its own proportions, stacked on phones),
and Beyond the Studio opens onto two new pages, `/curatorial-projects/`
and `/exhibitions/`, which the plugin serves from `build-cv.mjs` +
`fia-2026-09-27/tl-cv.csv` (her CV sheet, re-exported 2026-09-28 and
identical). A real WP page with either slug wins over the plugin's.
Preview evidence: `evidence/split-preview/`.
