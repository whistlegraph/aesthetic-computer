# marketing/podcast — essays → jeffrey-voiced readings

A production lane that turns written essays (`papers/essay-*/*.tex`, `opinion/*.md`)
into spoken-word podcast episodes in @jeffrey's ElevenLabs voice, framed like a
liturgical reading.

**Mastering follows the /pop house law** (August '26, ported from
`pop/loner/c/cut-wax.sh`): substrate + a voice-tuned wax/FM material stage
(bass mono <120 Hz, slight side lift + slow drift, tanh + exciter, program
glue, 40 Hz–15 kHz ceiling; no wow on speech), then MEASURE → **one static
dB** → true-peak limiter (0.82 ≈ −1.7 dBTP) at **−14 LUFS**. Never a second
loudnorm.

## The dailies (bin/daily.mjs)

Alongside the longform readings, **Aesthetic Computer Radio** — an automated ~2-minute
episode: gather the last day of commit subjects (filtered, deduped, and
REDACTED — client/confidential lanes never reach the model), write a
~300-word script via `claude -p` in the storytelling register, produce with
`--bedstyle club --frame daily` (116 BPM four-on-the-floor bed), publish via
Buzzsprout. `daily-YYYY-MM-DD` slugs are pattern-cleared in `lib/hosted.mjs`;
the content guard is daily.mjs's redaction filter + prompt rules + word-count
and leak checks (it refuses to produce on any violation). One episode per day:
finished script + MP3 + sidecar are reused on reruns, independently of delivery.
Clockwork: **jasellite** cron `30 0 * * *` UTC
(~8:30 PM ET) via `~/.local/bin/podcast-daily` — sources
`~/.config/ac/buzzsprout.env`, sets `TZ=America/New_York`, ff-merges the
checkout, logs to `~/.podcast-daily.log`. Dailies wear one committed static
cover (`assets/the-daily-cover*.png`) so the appliance needs no xelatex; the
commit log is read from `FETCH_HEAD` after a best-effort fetch. `--stage`
publishes private for review; `--dry` writes the script only.

**Publishing queue.** `out/publishing-queue/<slug>/` snapshots the audio,
artwork, show notes, original publish date, destination and visibility before
upload. Each daily retries up to three jobs, oldest first. A Buzzsprout billing
rejection stops that retry pass and keeps the jobs; it never stops daily audio
production or the independent token stage. Without an episode receipt, token
metadata links to the show. Queued private episodes remain private.
`bin/buzzsprout.mjs queue` inspects the queue; `retry --limit=3` retries it;
`enqueue <slug> [--private]` saves an already-produced episode without uploading.
Exit 75 means queued, not published. Other errors remain visible after the
token stage. Network ambiguity and non-billing errors require inspection rather
than automatic reposting: reconcile a found Buzzsprout episode into its receipt,
or reset the job status to `queued` only after confirming no upload landed.
The `.upload.lock` serializes uploaders; after a crash, check its PID before
removing a stale lock. A saved receipt always wins over a leftover queue job.

**The daily token (bin/daily-token.mjs).** With `DAILY_MINT=1` the episode is
also minted as a hic et nunc OBJKT signed by aesthetic.tez, named just the
episode title. Its image is the update itself: the script set in AC's pixel
font by a KidLisp piece (stored as a new $code, with punctuation preserved)
over a date-colored starfield, rendered locally as a 512² GIF, with a frame as
the thumbnail. AC's own IPFS node pins it (`/api/ipfs-add`, admin-only, the
Kubo node Keeps uses) with TZIP-21 metadata (the script as description, links
to the episode and the live $code), `mint_OBJKT` mints `DAILY_EDITIONS` (1, a
daily unique) and an objkt ask lists it at `DAILY_PRICE_XTZ` (5). HEN
metadata is immutable, so a minted day can't be renamed or re-imaged.
Interactive dailies use `DAILY_ARTIFACT=zip` by default (`html` in an older env
also selects ZIP). The offline crawl is packaged as `index.html`, `cover.gif`
and `thumbnail.png` in `out/daily/<slug>.zip`; `/api/ipfs-add` pins those files
as a directory. The artifact URI is the directory root with MIME type
`application/x-directory`, as HEN/Teia require, while the GIF remains the
display image. Never mint a bare HTML or ZIP file CID as the interactive
artifact. `DAILY_ARTIFACT=gif` retains the GIF-only option. An unminted receipt
with previously pinned metadata in another format stops for inspection.
New interactive packages also carry `transcript.txt`, `content.json` and
`source.lisp`. The canonical transcript preserves Unicode and punctuation
even where the pixel font cannot draw them. Before signing, the crawl's text
must match the episode after font normalization; the pinned metadata and every
directory file are fetched through IPFS and compared with the intended bytes.
Any missing or mismatched content stops minting, including an old unminted
directory without transcripts. `contentQA` in the receipt records file SHA-256
hashes and the read-back time. GIF mode verifies the prose in pinned metadata.
These checks confirm content preservation, not visual legibility; the sandbox
render check remains separate.
Daily text now wraps at 20 columns, sizes from width in whole glyph pixels,
and scrolls without perspective shrinking. A new view starts at the title;
short tiled windows must not scale the font down from their height. Inspect
both a 512² viewer and a 990×270 tiled view with `daily-artifact-check.mjs`
(`--width 990 --height 270 --shots <directory>`) before accepting a change.
`DAILY_RICH_TEXT=1` selects a compact `(flow (heading ...) (paragraph ...))`
reader with named `(link "Aesel" "https://aesel.app")` buttons, manual scrolling
and keyboard link selection. Destinations live in `lib/daily-richtext.mjs`.
Keep this option off until the live AC runtime supports these forms; new
bundles carry the updated runtime, but live `$code` URLs use production.
Use `daily-artifact-check.mjs --reader` to verify static text and scrolling.
To verify a visible link, add `--link-x <x> --link-y <y> --expect-url <url>`;
the checker requires a separate tab and an unchanged reader URL. Interactive
hosts must allow popups for new tabs; blocked popups never replace the reader.
Stages resume from `out/daily/<slug>.token.json`; it refuses to sign with any
key but aesthetic.tez or below 0.15 XTZ (each day burns ~0.06). Needs
`AESTHETIC_KEY` in the appliance env (jasellite: `~/.config/ac/tezos-daily.env`,
sourced by `podcast-daily`), an @jeffrey `~/.ac-token` (signed in with
`tezos/ac-login.mjs` on a Mac and copied over; it refreshes itself) and
`npm install --prefix tezos` for Taquito. `--dry` renders without pinning or minting.
`bin/daily-reprice.mjs --date <day> --price <xtz>` moves a minted day to a new
price (retracts the objkt ask, relists what aesthetic.tez still holds).

## The shape of an episode

```
[intro jingle]  →  "A reading of the essay: <Title>, by @jeffrey.
                    Approximately <N> minutes."  →  [the essay, read
                    paragraph by paragraph with breath between]  →
                    "Here ends the reading."  →  [outro jingle]
```

The framing is deliberately churchy — a *reading of the essay*, an announced
length, a fixed closing ("here ends the reading") — so the catalog feels like a
lectionary of AC writing rather than a talk show.

## Voice

ElevenLabs **jeffrey-pvc** via the production `/api/say` proxy — the same voice as
the pop lane and the 24h recap (`provider="jeffrey"`, `voice="neutral:0"`).
Stability held ≥ 0.5 to keep identity intact. Synthesis costs real money but is
content-hash cached (`out/cache/`), so re-runs are free.

## Pipeline (all in `bin/`)

1. **`essay-to-script.mjs`** — strips LaTeX/Markdown to clean spoken prose.
   Drops footnotes, section headings, colophon, URLs; keeps the argument. Emits
   `{ title, author, date, paragraphs[], wordCount }`. Spoken-form replacements
   expand print abbreviations such as `Ave` to `Avenue` without changing the
   canonical essay.
2. **`jingle.mjs`** — synthesizes `intro.wav` / `outro.wav`: a short pentatonic
   bell motif (ascending in, resolving out). Deterministic, $0, no samples.
3. **`cover.mjs`** — one canonical PALS-mark cover identity for the series and
   every episode (`system/public/purple-pals.svg`). Episode titles live in
   metadata rather than being expressed as substitute/generated logo variants.
   The cover is embedded as ID3 album art and kept as `out/<slug>-cover.png`.
4. **`produce.mjs`** — the orchestrator. Narrates intro + each paragraph + outro
   via `/api/say`, trims each utterance's edges fricative-safely (low −45 dBFS
   gate, leading/trailing only — a consonant is dim but BRIGHT, so a hotter
   gate would eat an /s/), measures each utterance's VOICED onset and places
   *that* on the beat grid with the lead consonant ahead of the line (the
   vowel-on-the-beat rule from pop/loner v4), measures the real body duration
   with ffprobe to fill in the announced length, assembles jingle + VO +
   paragraph breaths with ffmpeg, embeds the cover, then runs the delivered
   body back through local Whisper. The round-trip report at `out/<slug>-speech-qa.json` compares
   heard words with the spoken script and identifies phrases for human review.
   It checks intelligibility, not prosody. The metadata sidecar records the QA
   status. Output: `out/<slug>.mp3`.
5. **`feed.mjs`** — aggregates the sidecars into `out/index.json` (catalog) +
   `out/feed.xml` (RSS 2.0 + iTunes), and renders the series cover `out/cover.png`.
   Filtered by the shared publish allowlist `lib/hosted.mjs`.
6. **`ship.mjs`** — the one-command publish: guardrail (allowlist) → Buzzsprout
   → CDN (mp3 + cover under the hosted name, which backs the papers podcast
   link) → feed regen → verify. `--papers` also runs papers deploy+index;
   `--private` stages on Buzzsprout. Stops before git (prints the finish block).
7. **`publish.mjs`** *(legacy)* — stages `publish/` + syncs the self-hosted CDN
   feed. Superseded by `ship.mjs` + Buzzsprout; kept for the retired RSS path.

**Allowlist:** `lib/hosted.mjs` maps each cleared `slug → siteName` and is the
single source of truth for what may publish. A slug absent from it never goes
public — `ship.mjs` refuses it and `feed.mjs` drops it.

## Feed / hosting

**Canonical podcast URL: `https://pod.prompt.ac`** — Buzzsprout custom domain
(CNAME `pod.prompt.ac → app.buzzsprout.com`, DNS-only, in the prompt.ac
Cloudflare zone). `https://podcast.aesthetic.computer` 301-redirects there
(proxied CNAME + redirect rule in the aesthetic.computer zone). The
subscribable RSS is Buzzsprout's: `https://feeds.buzzsprout.com/2628235.rss`.

Legacy self-hosted feed (pre-Buzzsprout): DO Spaces
(`assets-aesthetic-computer`) at `https://assets.aesthetic.computer/podcast/`,
feed at `/podcast/feed.xml`, pushed with `publish.mjs --push`. **Retired as a
subscription target** — it was never submitted to directories and shouldn't
be; Buzzsprout is the distribution path.

## Usage

```bash
cd marketing/podcast
node bin/produce.mjs ../../papers/essay-<slug>/<base>.tex --open   # → out/<slug>.mp3
node bin/ship.mjs <slug>          # publish: Buzzsprout + CDN + feed + verify
node bin/ship.mjs <slug> --papers # also deploy the papers PDF + index
# then commit the episode's files + `fish lith/deploy.fish` (ship prints the block)
```

Legacy self-hosted feed: `node bin/feed.mjs && node bin/publish.mjs --push`.

Flags: `--open` (slab-afplay the result), `--force` (bypass say cache),
`--forceqa` (bypass Whisper QA cache), `--noqa` (skip Whisper QA),
`--bedstyle sosoft|lofi`, `--bedgain 0.34`, `--nobed`,
`--stability 0.55 --similarity 0.8 --speed 0.98` (voice tuning),
`--base <url>` on `feed.mjs` (override the asset host).

For an addressed audio reply, use `--frame reply --nobed`. This retains the
voice, mastering, and speech QA, omits podcast framing and jingles, and marks
the audio metadata as a synthesized private reply. Keep its source in the
private paper directory and its slug outside the publish allowlist.

## Reused from /pop

- `/api/say` invocation pattern + content-hash caching — lifted from `pop/bin/say.mjs`.
- Jingle synthesis follows the pop DSP posture (phase-increment sines, exp-decay
  bells) but is self-contained here.

## Radio readalong releases

`DAILY_READALONG=1` selects the audio reader for **new** interactive token
receipts. Saved crawl receipts keep their renderer; already-listed tokens
return immediately. Jasellite keeps `DAILY_ARTIFACT=zip`, `DAILY_EDITIONS=1`,
and `DAILY_PRICE_XTZ=5`. The stored $code fetches the pinned JSON/audio;
its packed copy uses the local `episode.json` and `episode.mp3` in the same
IPFS directory. GNU Unifont and its license travel with the artifact.

The token stage runs Whisper small on the complete master, caches timings
by the audio hash, assembles every spoken section, renders a pastel Unifont
cover without Chrome, and packs the reader. A missing spoken timeline or
poor alignment stops that stage. Public data is pinned before storing the
$code; the receipt records its content hash so retries cannot silently
replace it. Every directory file, including the MP3, is read back before minting.

`--dry --out-dir <temporary directory>` checks copied episode inputs without
pinning, storing a $code, or minting. Verify the resulting ZIP with
`daily-artifact-check.mjs --readalong` at 512² and 990×270.

`(def episode (fetch "episode.json"))` followed by `(flow (listen episode))`
loads canonical prose, optional named link blocks, the actual mastered podcast,
and word timings. The Unifont reader starts a loop with just a thin seek line;
there is no pause control. Browsers that block autoplay need a first tap or
Space. Dragging, wheeling, or arrow keys scrub the recording through its text:
the audio position and yellow word move together, including within a line.
Up scratches backward, down runs forward, with speed changing pitch.
The gesture uses Stample's sample synth (`sampleSpeed`), including its existing
interpolation and seek declicking. After 140ms without movement, speed eases
back to 1; after 1.6s the view settles onto the newly selected spoken line.
Playback continues from there. Active letters wiggle individually;
pointer movement briefly nudges nearby characters. Leaving stops playback.
HTML audio starts while the finite recording decodes; the ready audio worklet
takes over at the current playhead. Normal playback continues if sample decode
is unavailable. Stopping the reader aborts its decode and kills its sample voice.
Pack readers at density 2 for smaller pixels; ordinary text flows retain
their existing font and scrolling behavior.

Each paragraph selects a stable palette from its subject: paper, music, play,
network, or garden. Backgrounds stay pastel, text stays dark, and the active
word and links have distinct contrasting inks. The colors ease between
passages and follow scrubbing. The choices are stored in `episode.json`,
so playing a release does not call a model or choose new random colors.

Production saves `<slug>.spoken.json` with every spoken opening, body sentence
and closing, exact arrangement bounds, and the master audio hash. Readalong
assembly includes all three sections while preserving canonical body prose.
The spoken opening supplies the title in the reader. `daily-*` filenames and
receipts remain stable; the public show name lives in `lib/radio.mjs`.

Run Whisper on the **entire master**, including its opening and closing.
Prefer `-dtw small -nfa -ml 1 -sow -ojf` with the small model, and include the
spoken timeline from production to keep highlights out of arranged pauses:

```sh
node bin/read-along.mjs script.md words.json 0 master.mp3 episode.json master.spoken.json
node bin/read-along-pack.mjs episode.json episode.mp3 unifont-16.0.03.hex.gz out/reader
node bin/read-along-preview.mjs out/reader 8898
```

The offset is zero for full-master transcription. The builder automatically
finds `<master>.spoken.json` if the last argument is omitted, and rejects a
mismatched audio hash. An explicit `.srt` argument retains legacy body-only
alignment for older recordings without a complete spoken timeline.
Keep episode.json and episode.mp3 beside the packed piece. The audio SHA-256
is recorded; the packer checks it and embeds only the required Unifont glyphs,
with the OFL and attribution alongside the page. Supply the upstream
`unifont-16.0.03.hex.gz` from unifoundry.com; packing needs no glyph service.
Canonical text is never replaced with ASR spelling. Edit alignment
records substitutions and omits missing words. DTW token onsets supersede
heuristic segment boundaries when present; the optional SRT bounds them by
the exact sentence cuts. They remain approximate ASR alignment, not phoneme
forced alignment. Playback interpolates between recent media-clock samples
and freezes during buffering. The readalong token stage performs these steps
automatically when `DAILY_READALONG=1`.
