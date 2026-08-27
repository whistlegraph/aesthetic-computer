# laklok sisters — parity ledger

Laer Klokken has two sister interfaces that must stay in lockstep:

- **Raster** — `system/public/aesthetic.computer/disks/laklok.mjs`, an AC piece.
  Most chat behavior is inherited from `disks/chat.mjs` (via `chat.boot/paint/act/sim`),
  with laklok's own chrome (marquee, QR, settings pane) on top.
- **Vector** — `system/public/html/index.html`, plain DOM at laklok.com/html.
  Same wire protocol, same Auth0 session, no AC runtime.

## The update path (do this every time)

1. Edit the feature in **both** files (or deliberately log it below as one-sided).
2. `node toolchain/laklok-sisters/parity.mjs` — static check; fails on drift in
   the mirrored constants (media-link regex, themes, chips, circus colors,
   youtube regex, color keywords, QR URL, embed endpoints, char cap).
3. `node toolchain/laklok-sisters/sisters.mjs [--local]` — visual check;
   eyeball `out/report.html` and fail anything that doesn't read as kin.
4. Raster ships with the site deploy (`fish lith/deploy.fish` — live path!);
   vector is a static file on the same deploy. Chat protocol changes also need
   `npm run session:publish`.

When adding a new mirrored constant, add a check for it in `parity.mjs` in the
same change — an unmirrored constant is future drift.

## Feature matrix

| Feature | Raster (source) | Vector | Notes |
| --- | --- | --- | --- |
| Circus marquee "Laer Klokken" | laklok.mjs `paintLaerKlokkenSign` | `.marquee` + `CIRCUS` | colors checked |
| Themes ler/nat/skov/lakrids | `LAK_THEMES` | `:root[data-theme]` vars | roster checked |
| Settings pane (mode/tema/filter) | `paintSettings` | `#settings` | chips checked |
| Media-links filter | `chatView()` + `LAK_MEDIA_LINK` | `body.filter-links` + `MEDIA_LINK` | regex checked verbatim |
| Corner QR → laklok.com | `paintQR` (@akamfoad/qr dep) | `#qrlink` canvas (same dep) | URL checked |
| Token highlighting (@ url ' # $ *) | chat.mjs + lib/chat-highlighting.mjs | `TOKEN` + `renderTokens` | youtube regex checked |
| Sensitive-URL masking | lib/chat-highlighting.mjs | `SENSITIVE` list | |
| Inline `\color\` codes | chat.mjs `isChatColorCode` | `splitChatColors` | keywords checked |
| #painting embeds (64px, modal) | chat.mjs painting previews + Ken Burns | `.embed.painting` + lightbox | endpoints checked; Ken Burns is a raster-only flourish |
| YouTube embeds (thumb → jump out) | chat.mjs youtube previews | `.embed.yt` | thumb source checked |
| OG link cards | chat.mjs og previews | `.embed.og` (lazy, IntersectionObserver) | endpoints checked |
| Direct file embeds (img/vid/aud) | via OG/preview path | native `<img>/<video>/<audio>` | vector is richer here by medium |
| Post / edit (`chat:edit`) / delete (`chat:delete`) | chat.mjs (+ copy modal delete) | composer + ✎ ret / ✕ slet | same payloads |
| Hearts display | chat.mjs | `.heart` | neither side *sends* `chat:heart` yet |
| Presence + connection status | chat lib | header counts + dot | |
| 128-char cap | chat.mjs `chatMaxChars` | `maxlength` | checked |

## Known one-sided features (deliberate, revisit when they matter)

- **Raster only** (chat.mjs inheritance): per-user font picker (`CHAT_FONTS`),
  @handle autocomplete, attach (+) menu (photo→`#painting`, video→`!tape`
  uploads), message copy modal, link-confirm modal, per-character handle
  colors from `/api/handle-colors` (vector uses a deterministic hue),
  message sfx, inertial scroll physics (vector uses native scrolling),
  `!tape` code rendering.
- **Vector only**: native text selection, native media controls, lightbox for
  direct image URLs.

The biggest remaining gap is the attach/upload lane and `!tape` rendering on
the vector side.
