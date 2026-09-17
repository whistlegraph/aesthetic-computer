# Captutor presentation guards

Reusable macOS guards for Captutor. The client renderer and branded assets live
in the private Iris repository; these modules contain no client artifacts.

- `macPalStage()` captures MacPal's state, unloads it for filming, verifies it
  stopped, and restores only what was previously running.
- `dismissAutomationBanner()` closes Chrome's exact automation infobar using
  Accessibility, then `assertPresentationClean()` verifies the filming surface.
  Save their results as `chrome-presentation.json` with the take.
- `createModalPolice()` inspects native Chrome dialogs, fingerprints their kind,
  title and buttons, and records recognition counts in
  `~/.local/share/captutor/modal-police/memo.json`. `events.jsonl` records
  `handled` and `blocked` transitions. An `onEvent` callback provides the same
  event to a mission controller without another inference call.

Call `check('preparing')` before recording. During recording, poll
`check('recording')` and race its rejection against the screenplay; stop the
recorder immediately on rejection and discard the take. Polls include a native
Accessibility scan and may take several seconds. English Chrome labels are
currently supported. Page content is excluded; unknown native sheets/dialogs
are flagged, never automatically dismissed. This is not a universal popup detector.

`connectWithModalPolice(connect)` watches only the supplied connection attempt.
Remote-debugging consent is allowed only during that attempt and with explicit
operator policy: `CAPTUTOR_ALLOW_REMOTE_DEBUGGING=1` or
`~/.config/captutor/modal-police.json` containing
`{"allowRemoteDebugging":true}`. The default is to flag it. Cached recognition
never grants permission; each action is checked against the current phase and
policy. A second native scan verifies the exact named action before clicking.
Do not run multiple watchers against the same Chrome session/cache concurrently.

Validate with `node --test slab/captutor/test/*.test.mjs`.
Keep these modules and tests identical to their `captutor/` counterparts in Iris.

## Nag-fighter

`node slab/captutor/bin/install-nag-fighter.mjs` installs a persistent macOS
launch agent. It clears the known Chrome automation banner between takes,
remembers matches, and flags other dialogs. It never grants remote-debugging
consent. Stage Mode owns the desk while `stage-mode.json` exists, so the idle
watcher yields to the renderer's guards instead of racing their clicks.
Status, events and service logs live in `~/.local/share/captutor/nag-fighter/`.
The loop waits 1.5 seconds between native scans; scans add several seconds.
Use `node slab/captutor/bin/nag-fighter.mjs --once` for a single inspection.

Stage Mode checks Chrome’s “Always Show Bookmarks Bar” menu state, hides it only when visible, and restores the previous setting on exit. The recording guard rejects a bar reopened during capture; `chrome-presentation.json` records `chromeBookmarksHidden`. English Chrome menu labels are required.

## Native-display capture

Use `node bin/stage.mjs --native render <screenplay>` to retain the current display mode and browser window. Stage cleanup still runs. Output uses measured display pixels and refuses resizing or cropping; native mode currently supports the docs format. Do not combine it with portrait rotation. Inspect the rendered result at normal viewing size.

A stock narrator must explicitly select a provider: `voice: "eleven:neutral:0"` requests ElevenLabs River with timestamps. Unknown voice names fail instead of silently selecting Jeffrey. Narration caches include the full provider request.
