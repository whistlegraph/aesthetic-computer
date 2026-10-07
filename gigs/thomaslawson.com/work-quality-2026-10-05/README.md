# Thomas Lawson — mobile, accessibility and discovery

Published October 5, 2026, following Jeffrey's instruction to complete the
remaining mobile, accessibility and search/LLM discoverability work and test
on poorslice.

The homepage now opens with portrait work on phones and Candlelight on desktop.
It supports swipes, arrow keys and an optional seven-second slideshow, paused
initially. Focus, hover and backgrounding stop rotation; reduced motion disables
it. Captions remain available and images retain their full composition.

Search opens server-rendered artwork records. The new `/art-archive/` provides
ordinary links to all five public index categories, with pagination. The
existing legacy `/archive/` is preserved. Each of the 185 public artwork records
has visible text, a canonical URL and matching VisualArtwork structured data.
`/tl-artwork-sitemap.xml` is advertised alongside WordPress's sitemap in robots.txt.
Only the existing publication-filtered `tl_search_response()` supplies records;
this module does not query private inventory or modify content records.

The accessibility changes repair heading structure, empty and unnamed links,
image controls, visible keyboard focus, and modal focus containment/restoration.
They preserve the existing typography and artwork captions. Unknown artwork
URLs and out-of-range archive pages return 404.

## Build and deployment

`quality.php` is the template; `quality.js` and `quality.css` are embedded by
`node build.mjs` into `zzz-tl-quality.php`. Validate with `node --check quality.js`
and `php -l zzz-tl-quality.php`.

Upload the generated file to `wp-content/mu-plugins/zzz-tl-quality.staged`, then
rename to `zzz-tl-quality.php` and verify its readback hash. Activation requires
no database migration, cron job or rewrite flush. Roll back by renaming that
one file out of the `.php` extension. Keep the existing v1.8.0 design plugin and
October 4 closeout module; the older local v1.6.1 source must not replace them.

## Verification

Tests ran on poorslice in an isolated headless Chrome profile and Playwright
WebKit with iPhone 13 emulation. These are browser/emulation checks, not a
physical-phone or VoiceOver certification. Scripts here use the task's private
build directory on that machine and its existing Puppeteer/axe installation.

- `live-audit.mjs`: 56 pages, axe WCAG 2 A/AA and 2.1 AA plus best-practice
  rules at 390px; horizontal overflow at 320, 390, 768 and 1440px; loaded-image
  and JavaScript-error checks. The remaining empty heading on a legacy snapshot
  was repaired and rechecked in `final-checks.mjs`.
- `interaction.mjs`: 19 passing touch, keyboard, slideshow, reduced-motion,
  search/menu and current/legacy image-viewer checks. Also records the Vimeo
  connection restriction instead of treating embed creation as playback.
- `webkit.mjs`: six passing mobile interaction checks.
- `discovery.mjs`: 17 passing checks for all public artwork URLs in the sitemap,
  pagination across all categories, server-rendered content with JavaScript
  disabled, canonical/schema metadata, 404 behavior, mobile search navigation
  and the return link to the exact studio artwork.
- `final-checks.mjs`: three targeted accessibility rechecks and keyboard-started,
  muted Deirdre playback at 390px, with no overflow.

Screenshots, raw results and production backups belong in the private client
vault's `site-refresh-2026-09/quality-2026-10-05/`, not this repository. Inspect
foreground screenshots: background Chrome tabs can capture blank compositor
layers even when DOM and image loading checks pass.

Vimeo refuses playback from the testing connection. Deirdre has no supplied
caption track; media transcripts/descriptions and a normal-connection Vimeo
playback check remain content/verification follow-ups. Automated checks do not
establish complete accessibility conformance. No search ranking, indexing or
AI citation outcome is promised.

Discovery follows Google's [AI search guidance](https://developers.google.com/search/docs/appearance/ai-features):
crawlable textual content, internal links and structured data matching visible
content. Carousel behavior follows the [WAI carousel pattern](https://www.w3.org/WAI/ARIA/apg/patterns/carousel/).
