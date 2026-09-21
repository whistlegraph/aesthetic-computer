# Aesthetic Eye for Aesel

Aesel passes when its controls work and its rendered states make sense. A build,
an accessibility tree, or a successful RPC call is evidence, not visual acceptance.
This workflow extends [the papers gate](../papers/aesthetic-eye.mjs) to an interactive app.

## Contract

1. Build the app. Keep testing in the background; use Desktop 2 when WebKit needs
   a visible window. Never activate a window merely to inspect it.
2. Read `aesel_map`. It reports stable control IDs, enabled states, the current
   surface and overlays. Disabled or unported features remain explicit.
3. Exercise the affected controls with `aesel_act`, then read `aesel_state` and
   `aesel_events`. An accepted action can still have asynchronous work pending.
   Pass the observed `session.id` as `expectedSessionID` on every action. A
   thread switch invalidates that observation; inspect again before continuing.
4. Capture notebook, settings, expanded preview and narrow notebook. Add loading,
   failure, account or other states whenever the change affects them.
5. Inspect each image at its intended size. Review type, contrast, spacing,
   redundant chrome, preview content and the observed interaction. An unreadable
   label, blank expected preview, misleading state or broken control fails.
6. Record the reviewer, timestamp, checks and concrete observations in
   `aesthetic-eye.json`. Run the gate against the same built app and evidence.

The capture command starts every scenario as **unreviewed**. It cannot certify
itself. The check fails missing required scenarios, missing review, failed checks,
modified evidence and a changed build. The running process reports its startup
bundle fingerprint, so pointing at a newer binary on disk cannot bless an old UI.

```sh
node slab/bin/aesel-eye.mjs capture reports/aesel-eye-run notebook '/path/Aesel Native.app'
node slab/bin/aesel-eye.mjs check reports/aesel-eye-run
```

Use a new directory after rebuilding. Capture native chrome, notebook and preview as separate surfaces: AppKit's
view rasterization omits composited WebKit content. Notebook and preview images
are mandatory wherever those surfaces are visible. These captures verify the
components; they do not prove whole-window occlusion. Use the optional `window`
capture for changes involving overlays or occlusion; it requires existing macOS
Screen Recording permission and fails without raising a permission prompt. Hidden/off-space WebKit
may be throttled; inspect readiness and pixels rather than waiting blindly.

## MCP

`slab/bin/aesel-mcp.mjs` shares the stack's HTTP/stdio transport. The daemon
installer registers `aesel` at `http://127.0.0.1:7781/mcp`. Its tools are:

| Tool | Purpose |
|---|---|
| `aesel_map` | Current surfaces, control IDs and availability |
| `aesel_state` | Revision, preview flags, session state and draft length |
| `aesel_act` | Named UI operation with validated arguments |
| `aesel_preview` | Actual WebKit URL, readiness and canvas sizes |
| `aesel_capture` | Native chrome, notebook, preview, or authorized whole-window PNG |
| `aesel_events` | Last 256 event kinds/timestamps, cursor by sequence |
| `aesel_eye_capture` | Save a build-bound, unreviewed scenario |
| `aesel_eye_check` | Enforce current evidence and explicit visual acceptance |

The native bridge uses a private same-user mailbox inside its macOS sandbox.
Requests are correlated to the current process instance and expire after 30
seconds. It opens no native listening port and provides no arbitrary evaluation.
The adapter does not launch or focus the app. Its HTTP endpoint binds loopback
and rejects browser-origin requests. For a remote Mac, run stdio through an
existing authenticated SSH connection; do not expose port 7781 publicly.

```sh
ssh blueberry node /path/to/aesthetic-computer/slab/bin/aesel-mcp.mjs
```

`AESEL_AUTOMATION_DIR` selects a mailbox for the adapter. Launch a native test
instance with `AESEL_AUTOMATION_NAMESPACE=eye` to use `automation-eye` separately
from the everyday app. This is especially useful with the existing debug
notebook fixture; fixture tests do not demonstrate account, inference or purchase
success. Physical-device transport and whole-window iOS capture are not provided.

State includes the selected provider, its availability, model catalog and pending
operation ID. `provider.select` takes `provider`, `model.select` takes `model`
(an empty string selects CLI default), and `turn.reconnect` checks the existing
operation without resending it. `ui.scale` takes `scale`; `preview.resize` takes
`width` and `height`. The native action validates each control's bounds.

For signed-out provider UI checks, the debug notebook fixture accepts
`AESEL_PREVIEW_PROVIDER=claude` or `codex`. This supplies test readiness only;
it does not connect a CLI or prove inference. Both fixtures use the stable
`notebook-preview` thread ID. Shared-session tests separately exercise source
updates through a fake provider and assert that no AC request is made.

Diagnostics record event names and timestamps, not prompts, tokens, source or
bridge payloads. State inspection includes visible piece/thread metadata.
Screenshots can contain whatever the user has displayed; capture only for an
authorized test. There is no PostHog or other analytics export.

Sending, publishing, account changes and purchases retain their ordinary effects.
Test those only with explicit task authorization. The default acceptance loop
uses draft editing and local screen transitions, without inference or purchases.

## Components

`AeselAutomation.swift` owns RPC, captures and the diagnostic ring. `ContentView`
maps controls to the same handlers used by the UI. `SessionHost` records session
event kinds, while the shared JavaScript session owns the persisted piece
revision. `Session.embeddedPreviewURL` applies Electron's `nogap`, `nolabel` and
`autoreload` contract while preserving other query parameters and fragments.

New UI controls must join the live map, define availability and effects, and be
covered by an appropriate state capture. Unsupported controls cannot silently
report success. Changes to the mailbox or gate also run:

```sh
node --test slab/test/aesel-mcp.test.mjs
```

Precedent consulted: `papers/SCORE.md`, `papers/AESTHETIC-EYE.md` and
`papers/aesthetic-eye.mjs`. This is a development-workflow extension of the
existing gate; no scholarly paper or PDF is implied.

## Initial validation — 21 September 2026

The macOS debug build and nine JavaScript checks passed, along with the Swift
preview/session checks. The live AC runtime rendered an injected orange frame
without its corner HUD in headless Chromium using the shipped native injection
script. Native MCP draft set/clear, settings open/close, preview expand/collapse,
and window resizing passed against the isolated debug fixture.

Native visual acceptance remains **failed**: the locked remote Mac produced
blank WebKit previews and a runtime timeout. Notebook and native chrome were
inspected, including the 420-point layout; this does not substitute for a working
native preview. Whole-window capture also lacked Screen Recording permission.
No account, inference, publication, purchase or physical-iOS result is claimed.
Rerun the gate on an unlocked test seat before declaring native UI acceptance.

## Follow-up acceptance — 21 September 2026

The same Mac build passed all four required scenarios on an unlocked fleet seat:
notebook, settings, expanded preview, and 420-point notebook. The injected orange
source renders in native WebKit with `nogap`, `nolabel`, and `autoreload`; the
provider/model controls and return button are legible. Transparent notebook
component captures were reviewed alongside their paper background in the app
capture. No test window was activated.

Build fingerprint: `ca30ec5997dda22af6c6c6e3ffe6b0225d9b0bfb279769842aff0c7c4a84702b`.
Mac and iOS compile checks, 21 JavaScript checks, and native provider-readiness
checks passed. The compiled fixture rejects stale thread actions and enables
Send for connected Codex while signed out of AC. This accepts the fixture UI;
it does not claim live provider inference, account, payment, publication, or
physical-iPhone acceptance. The earlier locked-seat results remain failed.
