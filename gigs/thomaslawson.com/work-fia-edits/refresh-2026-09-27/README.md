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

Live 2026-09-27: plugin v1.0.0 installed + activated via wp-admin; served
CSS/JS verified byte-identical to source. Measured over all 49 pages
(`evidence/before` vs `evidence/after`, live, logged out):

| | before | after |
|---|---|---|
| font families | 6 (Poppins, Roboto, Gotham, Adobe Jenson, Georgia, Felix Titling) | 2 (Inter, Newsreader) |
| desktop type styles | 45 | 26 |
| horizontal overflow | 0 pages | 0 pages |

Side-by-side pairs for every page and both viewports: `evidence/compare/`.

Open for Fía/Tom: News banner is a 225×300 snapshot with baked-in text;
page title "Pat Douthewaite" → Douthwaite.
