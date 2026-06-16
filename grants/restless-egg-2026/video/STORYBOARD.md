# Restless Egg — 60-sec Intro Video · STORYBOARD v1
### Aesthetic, Inc. · "instruments you play, not tools that make you efficient"

**Format:** 1920×1080, ~60s, H.264. **VO:** jeffrey-pvc (ElevenLabs voice_id
`dYNGZ848Oo6DtNBoeqgh`, with-timestamps for caption sync, stability ~0.35).
**Bed:** notepat playing its own melody (the product *is* the soundtrack), ducked
−12dB under VO. **Cut style:** hard cuts on the beat (pop rule: cuts > morphs);
exactly one signature morph (emblem→product). **Look:** /pop chrome tri-layer
backlight (glow + vignette + per-figure halo), YWFT title type, AC palette.

**Three visual lanes:**
- **CAP** — real screen capture (notepat, AC platform, KidLisp $codes) via
  `marketing/bin/capture-ac-native.mjs` + chrome screencast.
- **GEN** — Seedance (fal.ai) cinematic motion via the /pop harness (FAL_KEY in
  vault). **Objects/product/abstract only — NO faces** (Seedance moderation
  rejects them; that's fine, the VO carries the founder).
- **CARD** — designed title/number cards in the pop chrome aesthetic.

---

| # | Time | VO (jeffrey-pvc) | Visual | Lane | Motion / transition | Notes |
|---|------|------------------|--------|------|--------------------|-------|
| 1 | 0:00–0:06 | "I'm Jeffrey Scudder — a painter who spent the last decade building instruments instead of paintings." | The salvaged laptop on its stone pedestal (the **THE PUBLIC COMPUTER** emblem) comes alive; screen blooms with a KidLisp mark | **GEN** | Slow cinematic push-in; chrome glow blooms on the screen-light | Open on product, not bio. Lower-third title card "Aesthetic, Inc." fades in (YWFT) |
| 2 | 0:06–0:10 | *(beat — first notepat notes hit)* | **Morph**: the emblem laptop screen → live notepat UI | GEN→CAP | The one signature morph; melody bed starts here | This is the bed music's downbeat |
| 3 | 0:10–0:20 | "This is notepat — a software instrument. Keyboard, touch, MIDI, even pressure." | notepat playing: tiles lighting as notes trigger, waveform reacting | **CAP** | Hard cuts between wide UI and macro on the tiles, on the beat | Capture with `--hold c,j` so tiles render pressed; real audio = bed |
| 4 | 0:20–0:28 | "Under it, a 128-voice synthesizer I built from scratch — it models real instruments instead of playing back samples." | "128 PHYSICALLY-MODELED VOICES" chrome number card + abstract waveguide/string motion | CARD + **GEN** | Card slams in on beat; Seedance string-resonance b-roll behind | Number animates up 1→128; tri-layer backlight |
| 5 | 0:28–0:40 | "The wild part — notepat runs anywhere. Your browser, your phone, and as the first thing that boots on a salvaged fifty-dollar laptop. No OS underneath it." | Rapid 3-cut: browser → phone → bare-metal laptop booting from a USB stick, screen coming alive | CAP + CAP + **GEN** | Three hard cuts accelerating; the boot shot lands on the bar | "no OS underneath it" hits on the bare-metal frame |
| 6 | 0:40–0:50 | "It grows out of Aesthetic Computer — where eighteen thousand people have already made sixteen thousand little programs." | Fast montage of KidLisp `$code` pieces ($berz, $cow, $24m…) → traction card "18,000 MAKERS · 16,000 PROGRAMS" | **CAP** + CARD | Machine-gun cuts of generative pieces; numbers chrome-stamp in | Pull the $code stills/clips live from prod |
| 7 | 0:50–0:60 | "My company turns that into instruments people play and pay for. I want to make computing something you play again. That's what I'd do with Restless Egg." | Emblem returns, holds; chrome title "MAKE COMPUTING SOMETHING YOU PLAY AGAIN" → end card | **CARD** + GEN | Slow settle; bed resolves; vignette closes in | End card: `notepat.com` · `aesthetic.computer` · "Aesthetic Inc. × Restless Egg" |

**Runtime check:** ~60s. VO ≈ 150 words (≈60s at a relaxed pace). If long, trim
shot 4's clause first.

---

## Asset / build plan (the production order)

1. **VO** — synth shots 1–7 lines in jeffrey-pvc with-timestamps → `vo.wav` +
   `vo.json` (caption timing). Adapt `grants/lacma-2026/watch-pvc.mjs` /
   `generate-vo.mjs` (already wired to ElevenLabs + the vault key).
2. **CAP — notepat** — capture notepat playing (with audio) on prod/notepat.com;
   record a clean ~15s melody = the bed + the shot-3/shot-5 footage. Tool:
   `capture-ac-native.mjs` (extend to screencast video, or chrome screencast).
3. **CAP — platform** — grab KidLisp `$code` stills/clips ($berz, $cow, $24m,
   $duv, $kl1 — we already have card PNGs in `grants/lacma-2026/figures/`).
4. **GEN — Seedance** — 3 short b-roll clips via the /pop fal harness:
   (a) emblem laptop waking on the pedestal, (b) abstract string/waveguide
   resonance, (c) bare-metal boot beauty shot (USB → screen-alive). Objects only.
   Reuse `pop/lib` Seedance wrapper + FAL_KEY (vault).
5. **CARDS** — title / "128 voices" / traction / end card in the pop chrome
   look (YWFT + tri-layer backlight). Reuse `pop/lib` preview module + the
   `cover.png` emblem.
6. **ASSEMBLE** — adapt `grants/lacma-2026/build-video-v6.py`: lay VO + bed +
   the CAP/GEN/CARD clips per the table, hard-cut on beat, burn captions from
   `vo.json` (recap style), export `restless-egg-intro.mp4`.

## Open decisions / flags
- **Aspect:** 16:9 assumed (founder video). Confirm the form has no vertical pref.
- **Bare-metal boot footage:** "new capture" of a physical laptop boot needs the
  device on-hand; if not, the **GEN** Seedance beauty-shot covers it, or reuse
  the LACMA boot clip. Decide at step 4.
- **Captions:** burn-in (recap style) recommended — accelerator readers often
  watch muted first.
- **Length:** hard 60s cap (form limit). The script is timed to it.
