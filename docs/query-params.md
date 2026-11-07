# Query parameters

Generated from [`lib/params.mjs`](../system/public/aesthetic.computer/lib/params.mjs)
by `node system/scripts/params-doc.mjs`. Edit the ledger, not this file.

74 parameters. A flag is on when present; `=false`
does not turn it off — drop the name. Keep query parameters before any
`#fragment`.

**Scope** — `view`: presentation; bios re-applies *carry: bar* names on every
history write and the worker re-appends *carry: jump* names when a piece jumps.
`boot`: consumed on the main thread at start. `auth`: session hand-off,
stripped before the worker sees it. `piece`: reaches the piece as `$.query`.
`server`: read by the HTML shell.

**Kind** — `flag`: present → true. `number`: Number(value). `size`: bare,
`true`, or `WxH`. `string`: as written. `token`: opaque; never logged.

Unknown names reach the piece as strings and are warned once per worker.
Boot telemetry records which names were present — names only, never values.

| Parameter | Kind | Scope | Carry | Readers | Status | Effect |
| --- | --- | --- | --- | --- | --- | --- |
| `nogap` | flag | view | bar, jump | boot, bios, disk, server | live | Zero gap: the canvas fills its viewport, no rounded corners or checkerboard. |
| `nolabel` | flag | view | bar, jump | boot, bios, disk, kidlisp, piece:clock | live | Hide the corner label / HUD. |
| `nosource` | flag | view | bar, jump | boot, bios, disk | live | A KidLisp $code labels itself by name, not source. |
| `autoreload` | flag | view | bar | boot, bios, disk | live | Take piece updates automatically instead of showing the badge. |
| `shellhtml` | flag | view | bar | boot, bios, disk | live | An HTML shell (prompt.ac) owns the prompt and corner chrome in DOM. |
| `density` | number | view | bar | boot, bios | live | Rendering density; remembered in localStorage outside device mode. |
| `zoom` | number | view | bar | boot | live | Navigator zoom level; stored in localStorage and handed to bios, not applied. |
| `duration` | number | view | bar | boot, disk | live | Seconds for a timed piece's progress bar. |
| `daw` | string | view | bar | bios, piece:notepat, piece:metronome, piece:pedal | live | `daw=1`: hosted by Max/Ableton; pedal effects and Live sync come up. |
| `width` | number | view | bar | — | unused | Preserved on history writes; read nowhere. |
| `height` | number | view | bar | — | unused | Preserved on history writes; read nowhere. |
| `desktop` | flag | view | bar | boot, bios | live | Electron desktop pane: implies nogap, adds the `desktop` body class. |
| `device` | flag | view | bar, jump | boot, bios, disk | live | Display device (FF1): tv + nogap + noauth, density defaults to 8, no HUD overlays. |
| `tv` | flag | view | bar, jump | boot, bios, disk | live | TV display: touch and keyboard input off, fonts awaited before first paint. |
| `perf` | flag | view | jump | boot, disk | live | KidLisp performance / FPS HUD. |
| `auto-scale` (`autoScale`) | flag | view | jump | boot, disk | live | Scale density automatically to hold the target FPS. |
| `solo` | flag | view | bar, jump | boot, bios, disk | live | Lock the piece in place; navigation away is refused. |
| `highlight` | string | view | bar, jump | boot, bios, disk | live | HUD background highlight; bare for the default grey or a color (`#rrggbb`, `r,g,b`, name). |
| `maxfps` | number | view | bar | boot, bios | live | Cap the render frame rate, e.g. `maxfps=60`. |
| `spoofaudio` | flag | view | jump | boot, disk | live | Synthetic audio amplitude for headless captures. |
| `noauth` | flag | view | bar | boot, bios | live | Skip authentication entirely (embedded editors, previews). |
| `preview` | size | view | bar | boot, bios, disk | live | Render the piece's `preview()` thumbnail; bare, `true`, or `WxH`. `preview=walkieware` only hushes telemetry. |
| `icon` | size | view | bar | boot, bios, disk | live | Render the piece's `icon()`; bare, `true`, or `WxH`. |
| `mime` | string | view | bar | bios | live | `mime=1`: a mime card's player; starts muted, piece changes are not Back steps. |
| `reel` | string | view | bar | bios | live | `reel=1` (with mime=1 and noauth): the whole mime feed in one runtime. |
| `vscode` | string | boot | — | boot | live | `vscode=true`: running inside the VS Code extension; remembered in localStorage. |
| `signup` | flag | boot | — | boot | live | Open the sign-up dialog once auth is ready. |
| `supportSignUp` | string | boot | — | boot | live | Auth0 email-verified return (with success=true and code=success) → `notice=email-verified`. |
| `supportForgotPassword` | string | boot | — | boot | live | Auth0 password-reset return; removed from the bar. |
| `success` | string | boot | — | boot | live | Auth0 return flag; see supportSignUp. |
| `message` | string | boot | — | boot | live | Auth0 return message; removed from the bar. |
| `menuband` | string | boot | — | boot, server | live | `menuband=true`: the Menu Band webview; no service worker, no update cache. |
| `modulews` | string | boot | — | boot, server | live | WebSocket module-loader host for local hot reload. |
| `login` | string | boot | — | boot | live | `login=password`: open the password door instead of the social dialog. |
| `nocache` | flag | boot | — | boot, kidlisp | live | Sets window.acNOCACHE; KidLisp skips caching source (kidlisp.com editor). |
| `vst` | string | boot | — | bios | live | `vst=true`: running inside the VST plugin. |
| `camrot` | string | boot | — | bios | live | Camera rotation: `cw`, `ccw`, `90`, `-90`, `180`, `0`, `none`. |
| `camforce` | string | boot | — | bios | live | `camforce=1`: apply camrot even when the device says it is upright. |
| `workerbundle` | string | boot | — | bios | live | `workerbundle=0\|false`: run the unbundled disk.mjs worker. |
| `crossfade` | string | boot | — | — | unused | Listed by boot; read nowhere. |
| `ac_campaign` | string | boot | — | bios | live | Campaign tag on aesel.app / notepat.com visits (analytics). |
| `social-preview` | flag | boot | — | bios | live | Marks the visit as automated in analytics (social card render). |
| `offline-render` | flag | boot | — | bios | live | Marks the visit as automated in analytics (offline render). |
| `jev-vs-jev` | flag | boot | — | bios | live | Marks the visit as automated in analytics (jev demo). |
| `ac-automation` | flag | boot | — | bios | live | Marks the visit as automated in analytics (puppet / frame fleet). |
| `code` | token | auth | — | boot | live | Auth0 authorization code (with state), or `success` on the email-verified return. |
| `state` | token | auth | — | boot | live | Auth0 callback state. |
| `error` | token | auth | — | boot | live | Auth0 callback error. |
| `error_description` | token | auth | — | boot | live | Auth0 callback error text. |
| `session-aesthetic` | token | auth | — | boot | live | A session handed in by a host (desktop app, VS Code); stored, then removed. |
| `session-sotce` | token | auth | — | boot | live | A sotce session handed in by the VS Code extension; stored, then removed. |
| `notice` | string | piece | — | disk, piece:painting | live | A notice to flash on load: `success`, `cancel`, `email-verified`, or free text. |
| `dool` | string | piece | — | piece:prompt | live | `dool=true`: boot the prompt with the dool room already up. |
| `test` | string | piece | — | piece:chat, piece:laklok, piece:mail | live | Test hooks (BroadcastChannel) for browser e2e runs. |
| `ticket` | string | piece | — | piece:botce | live | A botce ticket key. |
| `rom` | string | piece | — | piece:gameboy | live | A ROM link to load. |
| `source` | string | piece | — | piece:give, piece:handle, piece:support | live | Where the visitor came from: `signup`, `homepage`. |
| `thanks` | string | piece | — | piece:give | live | `thanks=1`: show the thank-you state. |
| `amount` | string | piece | — | piece:support | live | Preset support amount (digits, optional two decimals). |
| `letter` | string | piece | — | piece:mail | live | A 24-hex letter id to open. |
| `renderer` | string | piece | — | piece:notepat | live | `renderer=cpu`: force the CPU painter. |
| `overlay` | string | piece | — | piece:notepat | live | `overlay=true\|1`: overlay mode. |
| `relayHandle` | string | piece | — | piece:notepat | live | Relay source handle. |
| `relayMachine` | string | piece | — | piece:notepat | live | Relay source machine id. |
| `relayAll` | string | piece | — | piece:notepat | live | `relayAll=1`: subscribe to every relay. |
| `relay` | string | piece | — | piece:notepat | live | `relay=all`: subscribe to every relay. |
| `amxd` | string | piece | — | piece:notepat | live | Hosted by the Max for Live device. |
| `effect` | string | piece | — | piece:pedal | live | `effect=1`: effect mode. |
| `novec` | flag | piece | — | piece:cancelok | live | Pixel-baked D-pad instead of the vector overlay. |
| `objectPreview` | string | piece | — | piece:object-viewer | live | `objectPreview=1`: preview an object. |
| `boot` | string | server | — | server | live | Boot screen theme for the HTML shell (default `empty`). |
| `noboot` | flag | server | — | server | live | No boot screen (theme `empty`). |
| `give` | string | server | — | server | live | `give=true\|false`: force the give sheet on or off. |
| `abtest` | string | server | — | server | live | `abtest=give`: the give A/B arm. |
