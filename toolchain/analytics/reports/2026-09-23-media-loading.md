# MIME media loading, 23 September 2026

Nine public ZIP tapes had stuck preview conversions. Restoring Lith’s verified oven credential and repairing them produced nine MP4 previews with posters. Originals total 379,582,044 bytes; previews total 49,264,685 bytes (87.0% smaller). Original tapes remain available for the audible scrub player. Repairs did not publish new federated posts.

| Tape | Original | Preview | First presented frame |
| --- | ---: | ---: | ---: |
| !ujs | 62,162,214 bytes | 1,596,612 bytes | 242 ms |
| !pjo | 179,755,593 bytes | 41,422,195 bytes | 311 ms |

These two previews have their MP4 metadata before media data (`ftyp, moov, free, mdat`). Two older direct-video tapes (!2x3 and !tgt) contain WebM bytes despite their MP4 labels; that compatibility issue remains.

The feed now skips the unused global MIME census. Three interleaved pairs on the same release returned the same 12 cards: full inventory 555/355/370 ms, feed-only 294/278/295 ms. Medians were 370 and 294 ms, respectively (21% lower).

The browser probe covered PNG decode, video first frame, JavaScript/KidLisp/text source fetch, and ZIP prefixes before conversion. After deployment the sampled PNG decoded in 190 ms; JavaScript and KidLisp source prefixes arrived in 76 and 55 ms. Source fetch is not program runtime readiness. These are small samples from Poorslice, with uncontrolled connection/CDN warmth, not global percentiles or an exhaustive crawl of every studio property. Existing videos varied between runs; no universal speedup is claimed.

Reproduce with `toolchain/analytics/media-speed.mjs --all-types --tapes ujs,pjo --out /tmp/media-speed.json` and `MIME_CDP_URL` pointing to a dedicated Chrome. The tape repair tool defaults to a read-only audit.

MIME uses natural media proportions, deferred loading and a single dominant autoplay preview. Interact opens the actual AC tape player on demand; leaving interaction, scrolling offscreen, hiding the page or navigating destroys it. The frame-tape player provides the same audible left/right scrub interaction as `video` after `cap`. Direct-video tapes retain the existing `video` player's more limited playback controls.

Validation: 13 focused backend/analytics tests passed, plus the Poorslice browser regression for center scrolling, mobile swipe, deferred loading, playback selection, player lifecycle and visit actions. A disposable production draft returned create 200, storage upload 200, unauthenticated read 403, and cleanup 200. This checks private draft transport, not a fresh public post or federated publication. Nine repaired originals exercised real preview conversion and authenticated completion.
