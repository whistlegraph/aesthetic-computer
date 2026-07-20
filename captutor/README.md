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
node bin/stage.mjs render <screenplay>   # reversible HiDPI full-desk filming mode
```

`--outbox` (or `CAPTUTOR_OUTBOX`) publishes only the finished, burned-caption
MP4 and its VTT sidecar. A `captutor-outbox/v1` JSON manifest lands last, after
the media files have been copied atomically, so Iris's mission runner can treat
the manifest as the completion signal. Set `CAPTUTOR_TASK_GID` to tie that
delivery back to the Asana assignment that requested it.

`deliver --outbox` recuts and republishes an existing take without driving the
interface again. Use it for caption or encoding changes: the recorded negative
and measured cue timing stay intact, so the recut costs no credits.

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

CDP clicks do not move the macOS cursor, and `reel` films the real screen — so
without coordination the video would show a dead pointer parked in a corner while
buttons depressed by themselves. `bin/stage.mjs` is the production path: it films
the real 1.5× macOS pointer and moves it smoothly to the exact same coordinate as
each trusted CDP click. The older shadow-DOM tutorial pointer remains available
outside Stage Mode.

Stage Mode is a reversible transaction around any Captutor command. It saves the
current desk, switches the display to 2× HiDPI (1280×720 logical / 2560×1440
capture), centers the browser with a desktop margin, raises encoding quality,
uses a neutral gray wallpaper, and temporarily hides desktop icons, Dock, menu
bar, Stats, Macpal's desktop badge, and Slab prompt sigils. Its `finally` handler
restores the saved display mode, pointer size, wallpaper, processes, and desktop
preferences on success, failure, or interruption.

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
