# jungle

audio-only jungle tracks built bottom-up from AC instruments — chopped
breakbeats, deep dub sub, reggae skank, dub sirens. fast drums, half-time
feel: the kit sprints, the groove saunters, the MC rides loose over the
top.

opens on **sunbreak** as the first sub-format — sunlit / golden jungle,
daytime-block-party energy rather than dark-club menace. warmer pads,
melodic sub, playful skank. three variants ship together so the lane has
range to prove itself.

these are **beats for [fía](../../grants) to rap over** — the lane ships
the instrumental; fía is the MC. the `.txt` next to each `.np` is a
light spanish toast cue-scaffold (call-and-response stubs, section
hooks), not a finished lyric. her voice is performance on top of the
composition, recorded live, not synthesized.

## the hybrid break

jungle is, classically, *built on a sample* — the Amen break. `pop`'s
posture is bottom-up: compose from AC's own instruments, no top-down
sampling. so the break here is a **hybrid** (per @jeffrey, 2026-05-16):

- the groove is the AC percussion kit (`percussion.mjs` — the same
  kick/snare/hat/ride web + native notepat play) sequenced into
  chopped-Amen-style syncopation: displaced kicks, backbeat snare,
  ghost snares, busy shuffled hats, rolled phrase-end fills
- a synthesized **break-stab** — a short bitcrushed/flanged snare-roll
  burst — fires at phrase ends as the genre nod, the chopped-tape
  texture, without ever sampling the Amen

no Winstons. no loop. it won't sound *exactly* like a chopped Amen — it
sounds like AC playing jungle. that's the honest version of the form,
and it's the research question the lane exists to answer.

## format spec — sunbreak

- **length**: ~1:20–1:30
- **tempo**: 165 BPM (range 160–175), felt in **half-time** (~82
  perceived) so the toast sits loose over sprinting drums
- **key**: sunlit — minor pentatonic with a major-third warmth
  (A / D / E roots by default). golden, not menacing
- **structure**: intro (4) → roll-in (8) → drop 1 (16) → dub-break (8)
  → drop 2 (16) → out (4) bars
- **bed**:
  - **break** — hybrid AC-kit breakbeat, chopped + shuffled, half-time felt
  - **break-stab** — synthesized bitcrush/flange snare-roll burst at
    phrase ends (the genre nod)
  - **sub** — deep dub/reggae sub bass, long roots + offbeat push,
    ducked under the kick
  - **pad** — warm golden chord pad (sinepower), slow attack
  - **skank** — reggae offbeat chord stab (sinepower stab), the
    block-party bounce
  - **siren** — sparse dub-siren / airhorn nods, AC pitch-swept synth
- **vocal**: fía — live spanish ragga toast over the top. instrumental
  ships standalone; the toast is added by her, not by the renderer
- **output**: single mp3 per track in `out/<slug>.mp3`. no video

## variants

three takes on sunlit jungle, one renderer, selected per-track:

- **jungleton** — *jungletón*: jungle break with reggaeton / dembow
  tresillo woven into the kick + sub. spanish toast sits native. the
  live 2025 latin-club niche (Parzubanil)
- **raggasol** — classic ragga jungle, sunlit: chopped break, dub sub,
  reggae skank, sirens. the purest jungle DNA, warmed up
- **rodando** — liquid / rollers: rolling (not frantic-chopped) break,
  lush golden pads, musical chords. the most melodic — room to sing-rap

## arrangement notation

same convention as the dance + chillwave lanes — section markers as
comments gate which bed layers fire:

```
# intro 4 [break-thin, sub]
A2:_*4 ...

# drop 1 16 [break, break-stab, sub, skank, pad, siren]
A4:_*2 C5:_*2 ...

# dub-break 8 [sub, siren, no-break]
...
```

bracket flags pass through to `bin/render.mjs`. unflagged sections
inherit the style defaults.

## pipeline

`bin/render.mjs` — single-command renderer. mirrors the chillwave
renderer: parse the `.np` score + section flags, layer the hybrid
break / sub / pad / skank / siren, mix + peak-normalize → mp3.

```
node bin/render.mjs --slug raggasol
node bin/render.mjs --slug jungleton --style jungleton --bpm 168
node bin/render.mjs --slug rodando --layer break    # solo a layer
node bin/render.mjs all                              # render all three
```

**one-command deliverable** — `--cover` runs the whole chain: audio →
colored-pencil illustration (`gen-illy.mjs`, gpt-image-2) → typographic
cover composite (`../dance/bin/cover.mjs --illustration`) → cover art +
ID3 tags muxed into the mp3 in one pass. the illustration is cached, so
only the first run pays the gpt-image-2 cost; `--force-art` regenerates
it. if the illy can't be made (no API key / offline) it falls back to
the geometric cover.

```
node bin/render.mjs all --desktop --cover            # the full thing
node bin/render.mjs --slug raggasol --cover --force-art
```

cover illustration prompts live at `<slug>.illy.txt` (the rough-tooth /
no-outline / 15°-hatch colored-pencil style, one sunlit sound-system
emblem per track — no figures).

## tracks

- **jungleton** — jungletón, dembow-leaning, sunlit
- **raggasol** — classic ragga jungle, sunlit
- **rodando** — liquid rollers, sunlit, melodic

---

*maintained by @jeffrey*
