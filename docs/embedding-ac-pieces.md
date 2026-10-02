# Embedding AC pieces: framing

The host owns the preview rectangle. AC fills that rectangle. A 4:3 preview
means a width-to-height ratio; it does not imply a palette, bezel, pixel density,
or filter.

## Set the viewport in the host

```html
<div class="piece-preview">
  <iframe
    title="Piece preview"
    src="https://aesthetic.computer/notepat?nogap=true&nolabel=true&autoreload=true"
    allow="autoplay"
  ></iframe>
</div>
```

```css
.piece-preview {
  position: relative;
  width: 100%;
  aspect-ratio: 4 / 3;
  overflow: hidden;
}
.piece-preview > iframe {
  position: absolute;
  inset: 0;
  display: block;
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
  border: 0;
}
```

The wrapper establishes a height before the iframe fills it. `display: block`
avoids the baseline space below inline iframes. Remove host padding and borders
when the preview should touch its container edges. In a native app, apply the
same ratio to the WebView's layout bounds.

An iframe is a separate viewport: style its element in the host, and configure
the AC document with its URL. Host CSS cannot reach into a cross-origin frame.
`object-fit` on an iframe does not resize the child canvas. Piece code should
compose against `screen.width` and `screen.height`, not the surrounding app.

## Configure the runtime

| Parameter | Effect | Use |
| --- | --- | --- |
| `nogap=true` | Sets runtime gap to zero; canvas wrapper fills its viewport and loses rounded corners/checkerboard background. | Edge-to-edge previews. |
| `nolabel=true` | Suppresses the runtime corner label/HUD. | Host supplies its own controls. |
| `autoreload=true` | Takes available piece updates automatically instead of showing the update badge. | Live authoring previews. |
| `noauth=true` | Skips the embedded runtime's authentication startup. | Draft previews whose host handles account access; omit when the embedded piece needs authentication. |
| `preview=true` | Selects the piece's `preview()` lifecycle behavior when present. | Intentional preview rendering, not an arbitrary preview identifier. |
| `icon=true` | Selects icon rendering behavior. | Intentional icon generation. |
| `density=…` | Changes the runtime's rendering density. | Deliberate resolution tuning, independently of aspect ratio. |

These flags are read by presence in several runtime paths. Remove a flag to
disable it; do not rely on `nogap=false` or `nolabel=false`.

The ordinary embedded authoring baseline is
`?nogap=true&nolabel=true&autoreload=true`. A blank draft runtime can start at
`/wipe` with `noauth=true` added. Keep query parameters before any `#fragment`.
`allow="autoplay"` permits autoplay within the iframe policy; it does not
guarantee audio without a browser-required user gesture.

Implementation: [boot parameters](../system/public/aesthetic.computer/boot.mjs),
[BIOS framing](../system/public/aesthetic.computer/bios.mjs),
[runtime CSS](../system/public/aesthetic.computer/style.css), and
[piece lifecycle/label handling](../system/public/aesthetic.computer/lib/disk.mjs).

## Keep framing through source changes

Changing source should not require navigating the iframe again. The native
Aesel bridge waits for runtime readiness, then injects a `dropped:piece` message
from inside the WebView:

```js
window.acSEND({
  type: "dropped:piece",
  content: {
    name: "aesel-preview",
    source,
    search: "nogap=true&nolabel=true&autoreload=true",
    isKidLisp: false,
  },
});
```

This is an internal runtime call, not a generic cross-origin iframe messaging
API. A browser host on another origin needs an explicit bridge; it cannot call
`frame.contentWindow.acSEND` directly.

The URL configures initial framing. The message's `search` preserves the piece
layer's display flags during replacement. The worker remembers label suppression
for its lifetime, and BIOS preserves view parameters when rewriting browser
history. Carry intentional `preview` or `icon` flags too, but do not add them as
tracking tags. A message being accepted is not proof of a successful paint;
authoring bridges should wait for rendering evidence and report runtime errors.

Existing integrations:

- [Aesel URL construction](../apple/aesel/Sources/Session.swift),
  [native injection and rendering evidence](../apple/aesel/Sources/AeselPieceView.swift).
- [Shared “Try” page](../system/public/aesthetic.computer/lib/try/shared-page.mjs):
  constructs embedded URLs with `nogap`, `nolabel`, and `noauth`.
- [Aesel phone sessions](../aesel/phone/session.mjs): published-piece preview URLs.
- [Whistlegraph host](../apple/whistlegraph/Resources/Web/engine.mjs),
  [native source bridge](../apple/whistlegraph/Sources/WhistlegraphAccount.swift).

## Diagnose an unwanted gap

Measure three rectangles separately: the host wrapper, iframe viewport, and
AC's `#aesthetic-computer` wrapper. In `nogap` mode, the latter should start at
`0,0` and fill the child viewport. Check screenshots after first paint, a source
replacement, a resize/rotation, and a density change.

| Symptom | Inspect |
| --- | --- |
| Space outside the iframe | Host margin, padding, border, inline baseline, or a fixed height competing with `aspect-ratio`. |
| Checkerboard or rounded corners inside it | `body.nogap` and the effective BIOS gap. |
| Label returns after an edit | Initial URL plus source message `search`; worker label persistence. |
| Correct frame, padded drawing | Piece composition and its own `screen`/reframe calls. |
| Tiny seam on one edge | Wrapper/canvas CSS dimensions and fractional scaling; zero-gap BIOS uses `100%` dimensions. |

BIOS must resolve an omitted `frame()` gap from `lastGap`/`startGap` **before**
toggling `body.nogap`. Otherwise a density change calling `frame()` can remove
the class while the effective gap remains zero. A useful regression sequence
is `?nogap=true`, followed by Cmd/Ctrl+Plus or an `ac-density-change` event:
the class and edge-to-edge bounds must survive both.

`window.acFORCE_NOGAP = true` is an internal host hook used by packed bundles
that have no normal query string. BIOS honors it on every reframe, including
explicit nonzero gap requests. It is not a URL parameter, and an ordinary web
embed should use `nogap=true` rather than depend on script injection.

## Iterate on Whistlegraph layout without restarting

For a running Whistlegraph app with the live-layout bridge, send a CSS file through
its existing owner-authenticated thread socket:

```sh
node slab/bin/ww.mjs layout wwRuboh /tmp/walkieware-layout.css
```

The stylesheet replaces the previous live override and persists locally across
launches. An empty file clears it. This updates the surrounding app shell, not
the cross-origin AC document. It does not reload the WebView, regenerate the
piece, or add a piece version. The device must be online and idle; the command
returns an acknowledgement after applying the override. CSS is limited to 100 KB.
Native Swift changes still require an app build/install; this is a CSS iteration
path, not Swift code hot reload.

Whistlegraph's native shell keeps its `Workspace` WebView at one stable SwiftUI
position. `WhistlegraphScreen.swift` owns the preview card, trailing caption,
version feed, and talk dock. `engine.mjs` remains the authority for versions
and generation; it sends typed, throttled display snapshots and accepts native
checkout/new-piece/stop commands. The engine document is the dedicated
`Resources/Web/shell.html`, not an Aesel prototype. Early provisional previews
are distinct from committed versions, so streaming remains visible before a
version is saved.

For live native layout tuning, the same CSS transport accepts bounded tokens:

```css
:root {
  --ww-spacing: 12;
  --ww-page-inset: 20;
  --ww-history-size: 22;
  --ww-talk-height: 98;
  --ww-title-size: 30;
}
```

The bridge reads these as numeric values and Swift clamps them to supported
ranges before updating observable layout state. This changes native layout
without recreating WebKit. Other CSS declarations affect only the web host;
they do not style SwiftUI controls. An empty override restores the defaults.
