# KidLisp TV → live aesthetic.computer WebView — spec

Goal: make the MenuBand "KidLisp TV" render a **live aesthetic.computer
WebKit (WKWebView) page** instead of the native Swift KidLisp port, so the
panel shows the *real* runtime (every builtin, fonts, network pieces, future
language changes) with zero porting work.

Status: spec only. Part 1 documents the CURRENT (native) path so we know
exactly what we're swapping. Part 2 specs the WebView path.

---

## Part 1 — Current KidLisp path (native Swift port)

The TV today is fully native — it never touches a browser. It runs a
hand-ported Swift KidLisp interpreter into an offscreen framebuffer and
blits that into a skeuomorphic LCD bezel.

### Component map

| Concern | File | Notes |
|---|---|---|
| Floating LCD panel (bezel + power LED) | `Sources/MenuBand/KidLispTVPanel.swift` | `KidLispTVPanel: NSPanel` (borderless, `.popUpMenu` level, child of popover). `screenAspect = 192/120`, `bezelThickness = 14`. Holds one `KidLispTVView`. |
| The actual renderer | `Sources/MenuBand/KidLispTVView.swift` | `KidLispTVView: NSView`. 192×120 framebuffer, own ~30fps timer, nearest-neighbor upscale. `init(source:resWidth:resHeight:)`, `setSource(_:)`. Reads `amplitude` each `tick()`. |
| Swift KidLisp interpreter | `Sources/MenuBand/KidLisp/` | `KLLexer`, `KLExpr`, `KLEvaluator`, `KLFramebuffer`, `KLColor`, `KLRNG`. Conformance target: *KidLisp Decree '26: Core* — **phase-1 corpus only** (`KidLisp/README.md`). Not full parity with `kidlisp.mjs`. |
| Lifecycle + wiring | `Sources/MenuBand/AppDelegate.swift` | `kidlispTVPanel` ivar (`:24`). Created in `showPopover` (~`:2669`), torn down in `closePopover` (~`:2464`). |
| Audio → `amp` | `AppDelegate.swift` — `tv.ampProvider` closure (`:2700`) calls `currentSynthAmp()`; `kidlispTVAmplitude()` (~`:3206`) is the RMS+envelope helper | Combines `menuBand.synthSnapshotWaveform(...)` RMS with a decaying note-on envelope (`kidlispNoteOnTau = 0.25`), scaled to kidlisp.mjs's ~0–10 `amp` range. `KidLispTVView` polls `ampProvider` in `tick()` (`:198`). Note-on time stamped at `:464`. Also has a `busyProvider` (`:2708`) that skips ticks while notes are lit / bend gesture active so the eval never steals the audio thread. |
| Piece chooser | `AppDelegate.swift` (~`:3204`–`:3380`) | Menu built from `GET /api/store-kidlisp?recent=15&sortBy=hits` (`kidlispChooserURL :31`), 300s TTL cache. `loadKidLispPiece` → `tv.setSource(src)` + persists to `UserDefaults("notepat.kidlispTV.source")`. `resetKidLispPiece` restores `KidLispTVPanel.defaultSource`. |
| Default source | `KidLispTVPanel.defaultSource` (`:30`) | Amp-reactive twin-line waveform — first-run demo of the audio→visual loop. |

### Current data flow

```
synth audio ─┐
note-on ─────┤→ kidlispTVAmplitude() ──(0–10)──► KidLispTVView.amplitude
                                                        │ (30fps tick)
store-kidlisp API ─► chooser menu ─► setSource(src) ─► KLLexer/KLEvaluator
                                                        │
UserDefaults("notepat.kidlispTV.source") ◄── persist   ▼
                                                  KLFramebuffer 192×120
                                                        │ nearest-neighbor
                                                        ▼
                                           KidLispTVView.draw → bezel panel
```

### What the chooser stores

`loadKidLispPiece` calls `tv.setSource(src)` with the **raw KidLisp source
string** (fetched/expanded), not a `$code`. The native evaluator only knows
source text. The cache `$code` is used to *fetch* source, then discarded.

### Limits of the native path (the reason to switch)

- Phase-1 corpus only — anything outside the ported builtins silently no-ops
  or diverges from `kidlisp.mjs`.
- No fonts/text, no `paste`/network pieces, no `@handle/piece`, no future
  language features without re-porting each one.
- Two implementations to keep in lockstep (the Swift port vs `kidlisp.mjs`).

---

## Part 2 — Proposed: live aesthetic.computer WKWebView

Swap the **renderer** (`KidLispTVView`) for a `WKWebView` that loads the real
site. Keep the bezel/panel chrome, the chooser, the persistence, and the
amp/audio idea — only the thing inside the screen well changes.

### 2.1 New component

`Sources/MenuBand/KidLispTVWebView.swift` — a drop-in replacement that mirrors
`KidLispTVView`'s public surface so `KidLispTVPanel` / `AppDelegate` barely
change:

```swift
final class KidLispTVWebView: NSView {
    init(source: String)                 // matches the old initializer shape
    func setSource(_ src: String)        // reloads the webview at a new piece
    var amplitude: Float                  // (optional) forwarded into the page
}
```

Internally it owns a `WKWebView` with:
- `setValue(false, forKey: "drawsBackground")` — transparent so the bezel
  shows through (same trick as `SheetMusicView.swift:167`).
- `configuration.suppressesIncrementalRendering = true` and a minimal,
  non-persistent `WKWebsiteDataStore` if we don't want cookies.
- Lazy creation (build the webview on first popover open, like
  `SheetMusicView.ensureWebViews()`), so menu-bar idle cost stays ~0
  (see memory: *Slab/Menuband perf — off-main + lazy*).

### 2.2 URL contract (the heart of the swap)

The chooser already has the cache `$code` (it just throws it away today). For
the webview we keep the code and build a URL instead of fetching source.

**Cached piece:**
```
https://aesthetic.computer/$<code>?nogap=true&nolabel=true&density=1
```
Resolution: `parse.mjs` detects `$`-prefixed nanoid →
`kidlisp.mjs fetchCachedCode()` → `/api/store-kidlisp?code=…`.

**Raw inline source** (for `defaultSource` and any ad-hoc text) uses the
kidlisp URL encoding from `lib/kidlisp.mjs` (~`:15042`):

| char | encodes to |
|---|---|
| space | `_` |
| newline | `~` (tilde → newline on decode) |
| `%` | `¤` · `;` → `¨` · `#` → `%23` · `(` `)` → `%28` `%29` |

So `defaultSource` becomes:
```
https://aesthetic.computer/(wipe_black)~(ink_rainbow)~...?nogap=true&nolabel=true
```
A Swift `encodeKidLispForURL(_:)` helper should implement that table once.

**Clean-embed query params** (confirmed in `boot.mjs`):
- `nogap=true` — drop canvas padding so it fills the screen well.
- `nolabel=true` — hide the corner HUD label.
- `density=1` (or `2`) — pixel density; pick to match the bezel's pixel look.
- `tv=true` — TV display mode (no touch/keyboard) — **candidate**, verify it
  doesn't disable rendering we want.
- `zoom=N`, `perf=true` — available, probably unused here.

> Open question: there is no `nohud=true` — `nolabel=true` is the supported
> flag. Confirm `nogap`+`nolabel` give a fully chrome-free frame for an
> arbitrary `$code`; if a piece forces its own HUD, we may need a dedicated
> embed entry point on the AC side.

### 2.3 Sizing / pixel look

Native TV is locked to 192×120 nearest-neighbor. The webview renders at its
own CSS size; to keep the chunky LCD feel:
- Give the `WKWebView` the screen-well frame (same as `tv.frame` today).
- Drive resolution with `density=1` so AC's own pixel buffer stays coarse,
  **or** render small + scale the layer with
  `webView.layer?.magnificationFilter = .nearest` for hard pixels.
- Keep `KidLispTVPanel.screenAspect = 192/120`; AC honors the container
  aspect when `nogap` is set.

### 2.4 Audio / `amp` reactivity

Two options, in order of effort:

1. **Drop it (v1).** The site generates its own audio; pieces that use `amp`
   read the page's own mic/synth. Simplest — `kidlispTVAmplitude()` and the
   note-on envelope plumbing become dead code we can leave or delete.
2. **Forward MenuBand's amp into the page (v2).** Push the existing 0–10
   value over `webView.evaluateJavaScript("window.__menubandAmp = \(a)")`
   each tick, and (AC-side) let kidlisp's `amp` global fall back to
   `window.__menubandAmp` when present. Requires a small hook in
   `kidlisp.mjs`. Only do this if the synth→viz loop is a feature we care
   about keeping.

Recommendation: ship v1 (no forwarding); revisit v2 only if the reactive
demo is missed.

### 2.5 Lifecycle

- Create the webview lazily on first `showPopover` (don't build it at app
  launch — menubar idle cost).
- On `closePopover`, either keep it alive (cheap re-show, keeps the piece
  running) or `loadHTMLString("")`/`stopLoading()` to silence audio + stop
  the rAF loop while hidden. Prefer **pause on close** so a kidlisp piece
  isn't burning CPU/audio behind a closed popover.
- Network failure / offline: show the bezel with a dim "no signal" state
  (we can keep a tiny static fallback, or reuse the native renderer purely
  as an offline fallback — see 2.7).

### 2.6 Call-site changes (minimal)

- `KidLispTVPanel.swift`: change `let tv: KidLispTVView` →
  `KidLispTVWebView`, and the `init` that constructs it. Bezel untouched.
- `AppDelegate.swift`:
  - `loadKidLispPiece`: pass the **`$code`** (build URL) instead of fetching
    raw source. The chooser response already carries the code.
  - `resetKidLispPiece`: point at the encoded `defaultSource` URL.
  - `kidlispTVAmplitude()` + note-on stamping: keep only if doing v2.
  - `UserDefaults("notepat.kidlispTV.source")`: switch to storing the
    `$code` (or full URL). Migrate old stored raw-source values by treating
    a non-`$` value as inline source and encoding it.

### 2.7 Optional: native as offline fallback

Keep `KidLispTVView` + the Swift `KidLisp/` port compiled in, used only when
the webview can't reach the network (or for the `--kidlisp-render` headless
CLI in `main.swift`, which is independent and should stay). `KidLispTVPanel`
picks renderer at init based on reachability. Low priority.

---

## Decision summary

- **Replace the renderer, keep the chrome.** New `KidLispTVWebView` with the
  same `init(source:)` / `setSource(_:)` surface.
- **URL over source.** Chooser already has `$code`; load
  `aesthetic.computer/$code?nogap=true&nolabel=true&density=1`. Add a
  `encodeKidLispForURL` helper for raw source.
- **Drop amp forwarding in v1.** Site is self-contained.
- **Pause on popover close.** Don't run audio/rAF behind a closed popover.
- **Keep the native port** for the headless render CLI (+ optional offline
  fallback); it's no longer the live TV.

## To confirm before building

1. `nogap=true&nolabel=true` truly yields chrome-free output for an arbitrary
   `$code` (not just kidlisp pieces). If not, add an AC-side embed entry.
2. Transparent `WKWebView` (`drawsBackground=false`) composites cleanly over
   the bezel at `.popUpMenu` window level.
3. Audio autoplay: does AC need a user gesture to start sound inside a
   `WKWebView`? (Set `mediaTypesRequiringUserActionForPlayback = []`.)
4. App sandbox / MAS subset (see memory: *Menu Band → Mac App Store*): a
   network WKWebView needs the outgoing-network entitlement; fine for the
   prompt.ac build, check the sandboxed subset.
