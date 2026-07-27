# captutor

Screen-recorded, narrated, captioned software tutorials for fuser — assembled
from tooling AC already had, plus one new primitive.

```
node captutor.mjs narrate <screenplay>   # just the voice — check pacing (cheap)
node captutor.mjs render  <screenplay>   # narrate → record → drive → mp4 + vtt
node captutor.mjs render  <screenplay> --outbox ~/Desktop/outbox
node captutor.mjs deliver <screenplay> --format docs --outbox ~/Desktop/outbox
node captutor.mjs publish <screenplay>   # into apps/docs/public/ + the MDX line
node bin/from-docs.mjs apps/quickstart   # a docs page → a screenplay draft
node bin/stage.mjs render <screenplay>   # reversible HiDPI clean-stage filming mode
node bin/stage.mjs --vertical render <screenplay> --format vertical
node bin/brand-video.mjs --input take.mp4 --format docs --theme themes/fuser.mjs
```

`--outbox` (or `CAPTUTOR_OUTBOX`) publishes only the finished, burned-caption
MP4 and its VTT sidecar. A `captutor-outbox/v1` JSON manifest lands last, after
the media files have been copied atomically, so Iris's mission runner can treat
the manifest as the completion signal. Set `CAPTUTOR_TASK_GID` to tie that
delivery back to the Asana assignment that requested it.

`deliver --outbox` recuts and republishes an existing take without driving the
interface again. Use it for caption or encoding changes: the recorded negative
and measured cue timing stay intact, so the recut costs no credits.

## Reusable client branding and QA receipts

Captutor owns a client-neutral final brand pass in `lib/brand-chrome.mjs`.
Screenplays opt in with `brandChrome`; the theme supplies a client asset and
per-format geometry. The renderer preserves the lockup's aspect ratio, turns
the two copies inward, scales their safe margins from the delivery frame, and
gives them the slow `/marketing` + `/pop` drift. Its compact, high-opacity
shadow keeps a glowing mark separated from live UI without a broad gray halo.

The bundled Fuser example is `themes/fuser.mjs`. Fuser's repository also owns a
copy of its values at `tools/fuser-tutor/brand-theme.mjs`, pointing to Fuser's
own canonical asset. Future clients should add their own small theme object;
they do not fork Captutor's renderer.

Every render writes `captutor-storyboard/v1` trace data and generates a PDF
storyboard/QA receipt from frames extracted from the encoded MP4. A screenplay's
`acceptance` block declares the minimum duration, required cards, client chrome,
audio range, and named `check()` events that must be evidenced. Renders with an
acceptance contract fail closed when the PDF reports review required. The PDF
travels with the video, captions, storyboard JSON, and checksum manifest in the
outbox.

`--vertical` is Captutor's true portrait stage: Panda's display rotates 90° into
its 720×1280 logical / 1440×2560 physical HiDPI mode, the browser sits in a
630×1190-point window with uniform margins, and the exact previous display
profile is restored afterward. It requires `brew install displayplacer` on the
recording host.

If Fuser shows **Upgrade time!** after the reel starts, Captutor stops and keeps
the interrupted negative, appends a `captutor-failure/v1` row to
`out/failures.ndjson`, refreshes Fuser, reruns setup, and retries automatically.

## The one idea

**Narrate first, then perform to the click track.**

The normal way to make a tutorial is to film it and then fight the voiceover into
sync. captutor inverts that. `/api/say` (jeffrey PVC) with `withTimestamps`
returns ElevenLabs' per-character alignment, so every line's exact duration — and
every word's exact position inside it — is known *before* the camera rolls. The
UI then performs to that clock.

Two consequences worth knowing:

- **Captions are free and exact.** No whisper pass, no forced aligner, and none
  of the transcription-fixup every other pipeline in this repo needs
  (`recap/bin/subtitles.mjs` keeps a `transcriptFixes` table because whisper
  hears "notepat" as "Notepad"). We already know the words — we wrote them.
  Burned captions use those same timings to highlight the word currently being
  spoken, while regular white Arial on a translucent black box remains stable.
- **Overruns cannot desync.** `reel` reports `since`, the wall-clock instant the
  video's first frame exists. Each beat is stamped where it *actually* began
  against that origin, and narration is laid at those **measured** offsets, never
  the planned ones. A beat that takes 40s instead of the scripted 6 (an AI
  generation) delays only itself; every later beat stays glued to its frame.

The smoke test proves this on purpose: its fixture stalls 3.2s on "Fuse", and the
final beat lands at 17.0s instead of the planned ~15.1s — in sync.

## Pieces

| Piece | Where | What |
|---|---|---|
| `reel` | `slab/bin/reel.mjs` + `ScreenRecord.swift` | **new.** SCStream → `SCRecordingOutput` → hardware-encoded mp4. Lives in SlabMenubar because that app holds the Screen Recording grant and runs in the GUI session, where an ssh shell cannot go. |
| narration | `lib/narrate.mjs` | `/api/say`, content-hash cached (ElevenLabs bills per character). |
| driving | `lib/cdp.mjs` | Trusted CDP input against a real, signed-in Chrome. |
| cursor | `lib/cursor.mjs` | The pointer you actually see. |
| compose | `lib/compose.mjs` | ffmpeg mux + WebVTT. |
| login | `lib/login.mjs` + `lib/otp-mail.py` | Signs Iris in over email OTP, with nobody in the room. |
| credits | `lib/credits.mjs` | The floor under the robot: it spends a client's money. |

`reel` is deliberately **not** in this directory. It is general AC infra — the one
screen-video capability the whole repo was missing — so pop, marketing, recap and
AC Native get it too. Only the fuser-specific parts live here.

### Why not Playwright's recorder

Playwright records video and fuser already has an e2e harness, so it looks like
the obvious answer. But it films the browser's internal surface, not the machine:
no real window chrome, no compositor, and a 2560-wide hardware-encoded h264 is
simply a better picture than a VP8 page capture. `reel` also works for anything
else on the Mac — native apps, AC Native, slab — which Playwright never will.

We still drive with CDP, so we keep Playwright's determinism. **Capture and
driving are separable**; only the capture changed.

### Captutor Stage Mode

CDP clicks do not move the macOS cursor, and `reel` films the real window — so
without coordination the video would show a dead pointer parked in a corner while
buttons depressed by themselves. `bin/stage.mjs` is the production path: it
compiles a transparent, click-through Swift cursor overlay and moves its exact
arrow-tip hotspot to the same coordinate as each trusted CDP click. The native
overlay eases at 120 Hz, leaves a restrained particle trail, and emits a small
click burst. It is capture-visible but never participates in browser hit-testing;
`CAPTUTOR_REAL_CURSOR=1` remains available for explicitly human-driven takes.

Stage Mode is a reversible transaction around any Captutor command. It saves the
current desk, closes stale QuickTime previews, switches macOS to Light appearance
and the display to 2× HiDPI (1280×720 logical), centers the browser, raises
encoding quality, uses a branded light wallpaper, and temporarily hides desktop
icons, Dock, menu bar, Stats, Macpal's desktop badge, and Slab prompt sigils. The
recorder captures the complete physical desktop, preserving the real rounded
window, shadow, and equal margin. A Swift desktop-level renderer supplies the
selected client backdrop. It is click-through, runs behind every normal window,
and exits inside the same Stage transaction. Delivery changes only the tiny
ScreenCaptureKit status dot in the extreme top-right, using live pixels sampled
from the adjacent desktop; it never masks or crops the browser window.
Its `finally` handler restores the saved display mode, optional real-pointer size, wallpaper,
processes, and desktop preferences on success, failure, or interruption.

Stage backdrops are selected per brand. `fuser` is currently the default and
uses the twelve node positions from the production `fuser-mark.svg` as one
connected glossy metaball sculpture over a quiet black-and-white field. Obsidian,
pearl, and graphite variants stay inside Fuser's monochrome brand system. One
instanced Metal pass raymarches the live smooth-union field at the display's
native backing resolution; each logo tumbles independently through yaw, pitch,
and roll. Smaller background marks use a softer, lower-step depth-of-field
treatment to preserve the power budget. There is no sprite
stepping, opacity pulse, wordmark, or generated approximation. The former
monochrome rising-mark field remains available as `classic`.
The ambient drift reverses gently inside a per-logo safe inset, so even the
largest rotating metaball volume remains fully on-screen without edge fades.

Regenerate the three palette stills after changing the implicit surface:

```bash
swift captutor/bin/render-fuser-metaballs.swift captutor/assets
```

```bash
node bin/stage.mjs --brand fuser render <screenplay>
node bin/stage.mjs --brand classic render <screenplay>
```

## Staying signed in, and not spending the client's money

Two things have to be true before a take, and neither can wait for a human:
**Iris is logged in**, and **she can afford it.** `render` now checks both before
the camera rolls.

```bash
node captutor.mjs login     # sign in if needed (render calls this itself)
node captutor.mjs balance   # ✦ left, and what recent takes cost
```

**Login.** Fuser's session cookie is never written to disk — panda's Chrome
`Cookies` DB is months stale — so the session lives only in the running browser.
Quit Chrome and Iris is logged out. `ensureSignedIn()` drives Fuser's own email
OTP path (`emailOTP()` is enabled in `services/auth/auth.ts`): email → Continue →
a six-digit code is mailed → type it → in. The code is read straight out of
`iris@fuser.studio` over IMAP by `lib/otp-mail.py` — 120 lines of python stdlib,
because panda has `/usr/bin/python3` and no mail client, no npm tree, and nothing
we want to provision on a client's machine.

The subtle part is proving a code is *ours*. The newest Fuser mail is not good
enough: a code from ten minutes ago stays the newest one until the new mail lands,
and typing it burns the attempt. So we mark the mailbox's **UIDNEXT** before
pressing Continue and accept only a message at or above that mark — "arrived after
we asked" becomes an integer comparison, and no clock has to be trusted. Spam is
polled too; Fuser's own dialog tells you to check it, and an unattended run can't.

The app password lives on panda at `~/.config/captutor/iris.json` (chmod 600), or
in `CAPTUTOR_IRIS_APP_PASSWORD`. **Never in this directory** — it gets rsynced to
two office minis. The source of truth is `vault/fuser/iris-credentials.md`.

**Credits.** Iris's balance is real money on a client's production account, and
generating an app or an image debits it. So an unattended renderer is also a thing
that can quietly spend:

| Guard | Default | Override |
|---|---|---|
| warn | below 2,000✦ | `CAPTUTOR_CREDIT_WARN` |
| **refuse to record** | below 1,000✦ | `CAPTUTOR_CREDIT_FLOOR` |
| **take cap** | 5 takes / 60 min | `CAPTUTOR_MAX_TAKES`, `CAPTUTOR_TAKE_WINDOW_MIN` |

The cap is persisted (`out/takes.json`), not in-process, because the thing to be
afraid of is a shell loop calling captutor five hundred times — an in-process
counter would never see it. That ledger is also the answer to "what does a video
cost": every take records the balance before and after. The current screenplay
stops at *pointing* to the Generate button, so it costs **0** — the tutorials that
actually generate will not.

Balance comes from `account.getQuotas`, fuser's own tRPC route, called from inside
the page so the session cookie rides along. (Its REST twin at
`/api/v1/account/getQuotas` looks nicer and is CORS-blocked from `app.` — don't.)

## Running it

### Pathfinding preflight (invisible CDP frame)

Before authoring selectors, take one structured internal frame of the real
Fuser tab:

```sh
CDP_PORT=9333 node bin/cdp-frame.mjs --match fuser.studio
CDP_PORT=9333 node bin/cdp-frame.mjs --match fuser.studio \
  --screenshot /tmp/fuser-preflight.png
```

The JSON reports the viewport, focus, visible controls with locator candidates,
and React Flow nodes, handles, and edges. The optional screenshot comes from
`Page.captureScreenshot`: it is read directly from Chrome's compositor and
never draws Frame, OCR, target, cursor, or Puppet UI on the filming display.

Use this command for exploratory preflight instead of one-off `node -e` scripts
that call `attach()` and forget to close the socket. Programmatic probes should
use `withSession()` from `lib/cdp.mjs`, or close a directly owned session in a
`finally` block. The CLI has a 15-second hard ceiling and always closes CDP, so
pathfinding cannot silently turn into repeated 120-second tool timeouts.

Captutor also enables Chrome's renderer-crash event before setup and runs a
bounded page heartbeat immediately before recording and throughout every take.
Chrome can leave an "Aw, Snap!" target in `/json` with its old Fuser title and
URL, so target metadata alone is never treated as proof of health. A crash stops
the reel, preserves an `aborted-browser-*.mp4`, and records
`browser-renderer-crash` in `out/failures.ndjson` instead of leaving the mission
apparently in progress.

`reel` needs SlabMenubar running with the Screen Recording grant
(`node slab/bin/frame.mjs doctor`).

On a filming host such as Iris/Panda, `captutor/bin/install.sh` from the
Aesthetic Computer repository installs the source and bundled controller at
`~/.local/bin/reel.mjs`; Captutor discovers that path automatically. The native
SlabMenubar process remains the recorder because it owns the GUI session and
Screen Recording permission.

Driving needs a Chrome with remote debugging on a **profiled** user-data-dir —
Chrome ≥136 refuses `--remote-debugging-port` on the default profile. See
`vault/fuser/skills/drive-ui.md`; it also carries the fuser selector map and the
hard-won gotchas (synthetic Enter does *not* submit; Generate is the blue ✦ button
in the App node's bottom-center toolbar).

Captutor writes Fuser's `fuser-theme` preference before each take. The default
is `system`, so Panda's macOS appearance controls the result; a screenplay may
set `theme: "light"` or `theme: "dark"` only when the brief calls for it.

The self-test needs neither the fuser dev server nor a login:

```bash
open -na "Google Chrome" --args --remote-debugging-port=9333 \
  --user-data-dir=/tmp/chrome-cdp --no-first-run \
  "file://$PWD/fixtures/smoke.html"
CDP_PORT=9333 node captutor.mjs render smoke
```

## Screenplays

A beat is one spoken line plus what the UI does while it is spoken:

Screenplays also get a small decorative vocabulary. These marks live in an
isolated, pointer-transparent layer above the product and self-clear, so they can
guide attention without changing or intercepting the interface:

```js
{
  say: "Add a node from the left rail.",
  do: async ({ point, spotlight, burst }) => {
    await point("text=Add a Node");
    await spotlight("text=Add a Node", {
      label: "Add a Node", dim: 0.48, durationMs: 2200,
    });
    await burst("text=Add a Node", { glyph: "+" });
  },
}
```

- `spotlight(selector, options)` dims everything outside a padded target and
  adds an outer accent ring.
- `outline(selector, options)` draws the same ring without dimming the frame.
- `burst(selector, options)` emits a short deterministic glyph/particle bloom.
- `zoom(selector, options)` is a compatibility alias for the same feathered
  outline + dim treatment. Captutor never transforms the product DOM: doing so
  changes fixed/sticky layout and can crop the interface being taught.
- `resetCamera()` remains available for old screenplay cleanup, but current
  screenplays do not move the page camera.
- `clearEffects()` removes every active filming mark immediately.

The visual tools are also grouped under `effects` (`effects.spotlight`,
`effects.outline`, `effects.burst`, `effects.zoom`, `effects.resetCamera`,
`effects.clear`). Selectors accept the same
CSS, `text=`, and `js=` forms as `point` and `click`.

```js
{ say: "Press Fuse, and the flow runs.",
  do: async ({ click, cdp }) => {
    await click("[data-testid=fuse]");
    await cdp.waitFor("…shelf is open…");   // a real condition, never a sleep
  } }
```

Wait on conditions, not clocks: a sleep that is too short films a spinner, and one
that is too long films dead air.

`bin/from-docs.mjs` drafts a screenplay from a docs page — the page's prose becomes
the narration, and its `![alt](/shot.png)` screenshots become the blocking hints,
since those are exactly the moments the author thought needed a picture. It cannot
know which button to click; that is the real authoring work, and every `do:` comes
out as a stub.

Deriving the video from the page is what keeps the two from drifting apart.
`apps/` has 13 pages ready to draft. **Recipes has no docs page at all** — so that
one is authored by hand (`screenplays/smoke.mjs` is its skeleton), and the page and
the video get written together.

### Fuser onboarding contract

Tutorial capture must not depend on translated button copy, generated classes,
or the portal structure of a tooltip. Fuser exposes active tutorial state through
semantic DOM attributes:

- `data-onboarding-step` and `data-onboarding-active` mark the product target;
- `data-onboarding-overlay` and `data-onboarding-content-index` identify the card;
- `data-onboarding-requirement` reports a gated interaction;
- `data-onboarding-action` names Previous, Next, Finish, Skip, and Replay controls.

`lib/onboarding.mjs` reads this contract, waits for an exact step, and advances
the walkthrough. On localhost it can satisfy manual tutorial requirements through
Fuser's narrow `window.__fuserOnboardingAudit` bridge, allowing a non-billable
replay audit without reaching into Zustand or executing a model. The bridge is
absent in production.

## Known gaps

- **A Chrome relaunched over ssh films badly.** On panda, `bin/film-chrome.sh`
  started from an ssh shell comes up with the bookmarks bar showing (despite
  `prep-chrome` turning it off) and a *"Relaunch the browser to load your profile
  data and keep it encrypted"* infobar — Chrome cannot reach the keychain when it
  is not launched from the GUI session, and both end up in the frame. Signing in
  does **not** need a relaunch (`ensureSignedIn` works in the running browser), so
  this only bites after a reboot or a crash. Until it is fixed, start the filming
  browser from panda's own screen once, and leave it up.

- **`<VideoDocs>` is `muted` + `loop`.** All 45 existing docs videos are silent
  clips, so a narrated tutorial plays silent by default. It needs a `narrated`
  variant that drops `muted`/`loop`, keeps `controls`, and wires the
  already-present-but-empty `<track kind="captions" />` to our `.vtt`. That is a
  change in the *client* repo — not made yet.
- **Long waits are filmed in real time.** A 40s generation stays 40s. The offsets
  are all recorded, so a speed-ramp (ffmpeg `setpts` over a marked beat, then
  recompute) is straightforward — just not built.
- **Remote recording assumes one clock.** `since` and the beat stamps must share
  an epoch, which holds on one machine. Filming on `chicken` while driving from
  here would need clock-skew correction.
- Captions are a sidecar. `--burn` would need an ffmpeg with libass; the stock
  Homebrew build has neither libass nor drawtext (this is also why `recap`'s
  composer cannot run on this Mac).
