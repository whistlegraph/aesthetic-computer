# LACMA Art + Technology Lab 2026 — Submission Session Log

**Submission deadline:** Wed 22 Apr 2026, 11:59 PM PST
**Submitted to:** https://lacma.submittable.com/submit/348727/2026-art-technology-lab-grants
**Applicant account:** Aesthetic Computer (@jeffrey · mail@aesthetic.computer)

This is the compressed log of the marathon session that finalized the
submission. It covers every non-trivial decision + every artifact + the
known-open TODOs so someone (or a future Claude) can continue the work
without re-reading 100k tokens of chat.

## Permalinks (stable — can be overwritten)

| artifact | URL | notes |
|---|---|---|
| Proposal PDF | `https://assets.aesthetic.computer/lacma-2026/lacma-2026.pdf` | 2.0 MB · 13 pages (incl. CV + call-for-proposals appendix) |
| Budget PDF | `https://assets.aesthetic.computer/lacma-2026/budget.pdf` | 22 KB · 2 pages (line items + milestones) |
| Grant video | `https://assets.aesthetic.computer/lacma-2026/grant-video.mp4` | ~15 MB · ~1:25 · Jeffrey PVC + Ken Burns slides |
| Landing page | `https://aesthetic.computer/lacma-2026/` | Submittable form mirror + below-fold extended reference |

Also mirrored on lith: `https://aesthetic.computer/lacma-2026/lacma-2026.pdf`
and `/budget.pdf`.

## Source of truth

| file | what it drives |
|---|---|
| `grants/lacma-2026/lacma-2026.tex` | proposal PDF (tectonic compile) |
| `grants/lacma-2026/budget.tex` | budget PDF |
| `grants/lacma-2026/LACMA-2026-APPLICATION-DRAFT.md` | Submittable copy-paste source |
| `system/public/lacma-2026/index.html` | landing page (lith-deployed) |
| `grants/lacma-2026/build-video-v6.py` | final video build pipeline |
| `grants/lacma-2026/generate-vo.mjs` | ElevenLabs TTS generation script |

## Naming decisions (all cascaded)

- **Product name:** just `Aesthetic.Computer` (branded dotted form).
  `AC Native` is retired from user-facing copy — it was a codename leaking
  into the pitch. The web runtime and the laptop-boot runtime are both
  "Aesthetic.Computer"; disambiguate by context ("in a browser" / "on a
  laptop") when needed.
- **Hardware product:** `AC Blank` (refurbished ThinkPad 11e Yoga Gen 6
  preloaded with Aesthetic.Computer at $128/seat). Stays.
- **Language:** referred to in prose as `KidLisp.com` (brand-as-URL pattern
  matching `Aesthetic.Computer`).
- **Default piece:** `notepat.com` — polyphonic instrument, also free in any
  browser.
- **Dropped jargon:** "bare-metal" everywhere. Replaced with plain English
  ("runs as the whole operating system" / "boots a laptop directly into art
  software" / "no desktop, no app store, no browser").
- **Three descriptive words on Submittable:** `instrument, library, network`
  (dropped `language` — library is the new primary frame).

## Structural reframe — the Library

Mid-session pivot from a "bring AC Native to LACMA" pitch to a **public
device library** pitch:

- A lending fleet of AC Blank laptops flashed with Aesthetic.Computer
  (12 units at $128/seat) circulating through **Flash Days, KidLisp
  Workshops, and Family Play afternoons** at LACMA.
- Public waitlist. Members borrow, take home, return.
- The library is OS-agnostic: Aesthetic.Computer is the flagship, but
  artists can flash their own custom creative operating systems onto
  library hardware.
- Positions the project as **civic infrastructure**, not an artist-tool.
- Pairs with LACMA's public-museum role more strongly than the prior
  framing did.

## Two-event arc

- **2027 Symposium — "We boot the cohort."** Flash Day on the museum
  floor; cohort + first library members flash AC Blanks together and
  take them home. Public KidLisp workshop. Talk / in-conversation
  alongside the 2023 cohort.
- **2028 Demo Day — "We play the room."** Multi-station installation
  premieres in the galleries; Family Play afternoons open the library
  fleet to visitors of any age; v1.0 of the open-source build pipeline
  ships so any institution can run its own room.

## Budget ($50,000 cap, all used)

- Artist fee (24 months) — $22,000
- **AC Device Library fleet** (12 AC Blank laptops × $128, cases +
  shipping + returns) — $2,000
- Mini Rig portable speakers for installation (5 × $120) — $600
- Studio hardware — $3,500
- USB drives, cables, peripherals — $500
- Installation fabrication — $2,500
- Workshop materials (guides, KidLisp reference cards) — $1,200
- **2027 Symposium "boot the cohort"** (cohort demo USB kit, on-site
  workshop station, travel) — $2,500
- **2028 Demo Day "play the room"** (multi-station install setup,
  public-program support, v1.0 release) — $3,000
- Paid event helpers (workshop TAs, install/take-down) — $3,000
- Server + compute infrastructure — $3,500
- Documentation production (video, photography, translation) — $2,000
- Contingency (10%) — $3,700
- **Total: $50,000**

## Submittable form state

All text fields populated. Save Draft clicked multiple times. The
final copy pushed matches the source of truth in
`LACMA-2026-APPLICATION-DRAFT.md`.

Field summary:
- Project Title · `Aesthetic Computer: Personal Computers Are Not Done Yet`
- First/Last/Email · Jeffrey/Scudder/mail@aesthetic.computer
- Three words · `instrument, library, network`
- One-sentence · "Aesthetic.Computer is a creative computing system and
  public device library..."
- Full description · 500 words (soft `491` with the trim, still safely
  under cap)
- Bio · "artist, educator, and technologist" prose version
- Three 100-word statements · merit / culture+tech / public-engagement
- Total requested · `50000`
- Implementation plan (5 rows) · Library Build-Out / Pre-Symposium /
  2027 Symposium / Library Scaling / 2028 Demo Day — totals $50k
- Other funding · GitHub Sponsors + Liberapay + studio funds

File uploads (user handles these in browser — Chrome MCP file-upload
was blocked by extension permissions):
- Detailed project budget file → upload `grants/lacma-2026/budget.pdf`
- Up to 5 JPEGs → upload from `grants/lacma-2026/jpegs/submit/`:
  `platform-screenshot`, `kidlisp-featured`, `card-gallery`,
  `card-berz`, `hardware-yoga`
- Video → paste `https://assets.aesthetic.computer/lacma-2026/grant-video.mp4`
  in the project description or attach as a 6th file.

## Voice cloning pipeline

- **ElevenLabs Creator tier** ($22/mo, 127k chars/mo, PVC included).
- **@jeffrey voice_id:** `dYNGZ848Oo6DtNBoeqgh` — Professional Voice
  Clone, trained on 5 samples (~2-3 hours of Jeffrey speech from the
  mediation lecture, India HCI, Korea HCI, 35c3 RDP, one more).
- **Fine-tuned models available:** eleven_multilingual_v2 (used),
  eleven_turbo_v2_5, eleven_v2_5_flash, eleven_flash_v2_5. `eleven_v3`
  NOT yet fine-tuned for this voice (if it becomes available, it
  unlocks audio-tag syntax like `[calm]`).
- **Voice settings for the final video:** `stability=0.65, similarity=0.9,
  style=0.15, use_speaker_boost=true` — higher stability + low style =
  calmer, less dramatic delivery.
- **Script:** 10 sentences of paragraph-flow prose with internal
  commas/colons/dashes so the TTS doesn't pause section-by-section.
- **Alignment:** ElevenLabs `/with-timestamps` endpoint returns char-
  level timing, parsed in `build-video-v6.py::tokenize()` with in-word
  period detection (so `Aesthetic.Computer` doesn't break sentences
  or captions).

PVC samples staged at `~/Desktop/jeffrey-pvc/` (6 files, ~3 hours
total; user uploaded these manually at elevenlabs.io/app/voice-lab →
Add Voice → Professional Voice Clone).

## Video build

- **Script:** `build-video-v6.py` — one-shot VO pipeline (single
  ElevenLabs call, single audio file, homogeneous voice).
- **Structure (~1:25):** 7 cards, no screen-capture demo, continuous
  VO.
  1. Hook — Jeffrey at his table with two laptops (IMG_2124 from
     give.aesthetic.computer slideshow)
  2. Three layers — AC demo poster still (slated for replacement)
  3. Community — platform screenshot (mobile + desktop)
  4. Feedstock — $berz KidLisp piece still
  5. Library — card-gallery (4 KidLisp sosoft cards)
  6. Events — Jeffrey + laptops (reuse with pan-rl Ken Burns)
  7. Close — $roz KidLisp piece still
- **Ken Burns motion:** applied to background images ONLY. Title text
  and subline render into a transparent PNG overlay that stays still
  over the moving background.
- **Typography:** YWFT Processing Bold for titles (up to 132pt auto-
  scale), YWFT Processing Regular for sublines (up to 40pt), white
  text with dark drop shadow offset (3,3).
- **Captions:** burned-in SRT, FontSize 16, white with 1px black
  outline + 2px shadow, MarginV 18 (low). Chunked 8 words / 52 chars
  max per cue. In-word periods (`Aesthetic.Computer`) don't split
  cues. Punctuation glued onto preceding word so cues end with
  periods.
- **QR code:** 110×110px top-right (24px margin), points at
  `https://aesthetic.computer/lacma-2026/`.
- **Palette:** cream `#f0e6cf`, sepia ink `#3d2e1f`, pink accent
  `#a54969`, dim `#8a7356` — extracted from the landing page's
  `:root` block.

## Assets gathered (`~/Desktop/ac-deck-assets/`)

- 28 image assets (live page screenshots, KidLisp pieces, shop
  photos, CV-staged JPEGs, cropped Yoga 11e illustrations from the
  Lenovo manual)
- 5 fonts (YWFT Processing regular/bold/light + Berkeley Mono
  regular/italic)
- QR PNG pointing at the landing page
- 6 Jeffrey photos downloaded from
  `assets.aesthetic.computer/jeffreys/jpg/` (only IMG_2124 verified
  to contain laptops)
- 2 playwright-recorded KidLisp animations (`kidlisp-berz-anim.mp4`,
  `kidlisp-roz-anim.mp4`) — captured via chromium headless at the
  piece URLs with `?nohud` query param

## Commit trail

Key commits on `main` (both `origin` and `tangled` knot):
- `217774d12` — reframe as AC Device Library
- `c7d026f1b` — rename AC Native → Aesthetic.Computer; drop bare-metal
- `62e033f38` — soften closing line (cut "not a product / argument")
- (further commits for .com naming, PDFs, video pipeline)

## Open TODOs (continue in next pass)

Asked for but not landed in-session due to deadline pressure; the
permalinks mean the video can be overwritten without resubmitting:

1. **Live KidLisp animation backgrounds.** The playwright captures
   are in the asset deck; wire them into `build-video-v6.py` segment
   encoding (replace `-loop 1 -i image.jpg` with `-stream_loop -1 -i
   animation.mp4 -t DUR` for the card backgrounds on slides 4 and 7).
   Also try the AC `/oven` endpoint for MP4 renders of KidLisp pieces.
2. **More Jeffrey photos with laptops.** Only IMG_2124 in the
   downloaded set has verified laptops. Need tagged data from the
   give.aesthetic.computer source — the per-image metadata has
   `focal`+`pois` but no content tags (no "laptop"/"computer"
   tag exists). Ask @jeffrey which IMG_xxxx have laptops and swap in.
3. **Per-character colorized slide titles** (magenta/pink alternation
   in YWFT font). Currently single-color titles; per-char colorize
   would require drawing each character separately in PIL with
   alternating palette.
4. **Numbered slides + top progress bar.** Like the `merry` feature
   in `disk.mjs` — show current slide number + progress through the
   pitch.
5. **"Three layers" slide background replacement** — current
   `ac-native-demo-poster.jpg` is too plain.
6. **Systray passphrase entry form** — plan at
   `plans/SYSTRAY-PASSPHRASE-ENTRY.md`. Post-LACMA infrastructure
   work for the Python daemon.
7. **Retry PVC for `eleven_v3`** if/when that model becomes fine-
   tuned for `dYNGZ848Oo6DtNBoeqgh` — unlocks audio-tag syntax
   (`[calm]`, `[excited]`, etc.) which multilingual_v2 doesn't parse.

## Commands cheat-sheet

```bash
# Regenerate + rebuild video (fast iteration loop)
cd grants/lacma-2026
python3 build-video-v6.py            # ~60s end-to-end
cp lacma-grant-video-v6.mp4 ~/Desktop/LACMA-GRANT-VIDEO.mp4

# Upload to CDN (overwrites the permalink)
AWS_SHARED_CREDENTIALS_FILE=/Users/jas/aesthetic-computer/aesthetic-computer-vault/home/.aws/credentials \
  aws s3 cp lacma-grant-video-v6.mp4 \
    s3://assets-aesthetic-computer/lacma-2026/grant-video.mp4 \
    --endpoint-url https://sfo3.digitaloceanspaces.com \
    --acl public-read --content-type video/mp4

# Recompile PDFs
tectonic -X compile lacma-2026.tex
tectonic -X compile budget.tex

# Deploy landing page
cd /Users/jas/aesthetic-computer && fish lith/deploy.fish

# Decrypt vault (needs passphrase)
fish /Users/jas/aesthetic-computer/aesthetic-computer-vault/devault.fish
# or manually:
gpg --batch --passphrase '...' -o \
  aesthetic-computer-vault/home/.ssh/id_rsa --decrypt \
  aesthetic-computer-vault/home/.ssh/id_rsa.gpg
```

## One-shot VO regen

```bash
# 5-min round trip for a full new take in Jeffrey's voice
cd grants/lacma-2026 && python3 build-video-v6.py
```

The script reads `ELEVENLABS_API_KEY` from
`aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`
automatically. Edit the `SCRIPT` variable at the top of
`build-video-v6.py` to change what Jeffrey says.

---

Session ran ~Apr 22 afternoon → Apr 23 early morning. Work happened
in the `funny-chatterjee-47c657` worktree on `main`; all pushed to
`origin` and `tangled` knot.
