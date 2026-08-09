# oskiewar — Steam asset shot-list

Every image the store and library require, with exact dimensions from Valve's
graphical-assets doc (accessed 2026-08-09, post-2024 sizes — the old smaller
capsules are no longer accepted). The right column says where each one comes
from, because most of this renders out of the game itself:

- **`xbox/live/snapshot.mjs`** listens to the game's own draw commands and
  re-frames any object as a vector — fighters, heads, the whole stage — at any
  resolution, transparent background included (`--bg=none`). Capsule art
  scales losslessly because nothing is ever a screenshot.
- **The reel factory renderer** (`xbox/live/marketing/render.mjs`) runs the
  real game in headless Chrome at whatever viewport it is handed; the game
  lays itself out per-shape. Today it records 9:16 — a 1920×1080 lane is a
  viewport parameter, not new machinery. `render-social-preview.mjs` already
  burns landscape stills the same way.
- **The logotype** is the one asset that is neither: the title screen's
  lettering exists, but capsules need a designed, transparent, reads-at-174px
  logotype pass. That is a human (or `illy`) job, done once, reused in nine
  places.

Capsule content rule, verbatim intent from Valve: "just your game logo and
artwork" — no review quotes, no award badges, no body text. Screenshot rule:
actual gameplay only — no concept art, no cinematics, no marketing copy
burned in.

## Store page (required)

| Asset | Dimensions | Format | Source |
|---|---|---|---|
| Main capsule | 1232×706 | JPG/PNG | snapshot.mjs fighters (vector, any res) + logotype composition |
| Header capsule | 920×430 | JPG/PNG | same composition, recropped |
| Small capsule | 462×174 | JPG/PNG | logotype-dominant variant — must read at thumbnail size |
| Vertical capsule | 748×896 | JPG/PNG | portrait composition; the 9:16 reel covers already prove the fight reads tall |
| Screenshots ×5 minimum | ≥1920×1080, 16:9 | JPG/PNG | reel renderer at a 1920×1080 viewport; pick frames the way `dress.mjs` picks covers (mid-action, not the countdown) |
| Page background *(optional)* | 1438×810 | JPG/PNG | dimmed gameplay frame; Valve asks for "subtle, not too bright" |

## Library (required)

| Asset | Dimensions | Format | Source |
|---|---|---|---|
| Library capsule | 600×900 | JPG/PNG | vertical composition, recropped |
| Library header | 920×430 | JPG/PNG | reuse header capsule |
| Library hero | 3840×1240 | PNG | snapshot.mjs `frame` object re-framed wide — vectors make 4K free |
| Library logo | 1280 wide and/or 720 tall | PNG, transparent | the logotype alone; overlays the hero |

## Icons (required)

| Asset | Dimensions | Format | Source |
|---|---|---|---|
| Client/shortcut icon | 256×256 | PNG (ICO auto-generated) | `snapshot.mjs --object=dummy-head --bg=none` — already the favicon trick |
| Community/app icon | 184×184 | JPG | dummy-head on a flat game-palette ground |

## Trailer (required in practice — see STEAM.md caveats)

| Property | Spec |
|---|---|
| Resolution | up to 1920×1080; 16:9 preferred |
| Framerate | 30/29.97 or 60/59.94 fps |
| Codec | H.264 video + AAC audio, 5,000+ Kbps (Valve's reference preset: 20 Mbps / 192 Kbps AAC) |
| Container | .mp4, .mov, or .wmv |
| Audio | 44.1 or 48 kHz (anything else fails processing); transcoded down to stereo |
| Thumbnail | 1920×1080 JPG/PNG, **must be a frame from the video itself** |

Source: the reel factory end to end — same seeded match selection, same
uncut-round discipline, same synthesized audio tee — at a 1920×1080 viewport
and 60 fps. The trim-boundary lesson (`cardClearMs` / `tailHoldMs`) carries
over unchanged. One structural difference from a reel: a trailer can be
several rounds concatenated, and cuts *between* whole rounds keep the
"nothing inside is cut" rule intact. **Do not press release while the trailer
is still encoding** — Valve blocks it.

## If achievements ship

| Asset | Dimensions | Format | Notes |
|---|---|---|---|
| Achievement icons, ×2 per achievement | 64×64 displayed; author at 256×256 | JPG | achieved in color, unachieved gray — snapshot.mjs objects (head, ball, grenade, crater) tinted per state |

## Launch-day extra

| Asset | Dimensions | Format | Notes |
|---|---|---|---|
| Event cover (launch announcement) | 800×450 | JPG/PNG | artwork + message allowed here — this is the one canvas where marketing text is legal |

## The honest tally

Nine store/library images, two icons, five-plus screenshots, one trailer.
Of those, everything except the logotype and the capsule *compositions*
renders from tools that already exist and are already trusted. The real
asset lift is: one logotype, one composition session reused across nine
crops, one 16:9 render lane. A weekend, not a month.
