# heartshard — vocal map for "What's Inside Your Heart"

Source: `pop/samples/whats-inside-your-heart/Whistlegraph Acapella.wav`
(78.92s, **exactly 136.000 BPM**, first master beat ≈ 0.209s, key **E minor**).
Word timings via whisper-cli base.en (±50–100ms slop); notes via pyin median
per word. Bar:beat positions are grid-invariant — after the 136→150 stretch
(`acapella-150bpm.wav`, ratio 0.9066667) the same words sit on the same beats.

## Lyric (three phrases, round-style, three singers: Alex / Camille / Jeffrey)

- **A** — "What's inside your heart? Nothing but the hole you left behind"
- **B** — "Pumping blood and worms inside" (low register, D3–G3 — the bass voice)
- **C** — "Hey, what's up? Where's the love? Looks like you could use a hug" → **[scream]**

## Structure on the bar grid (4/4, ~44 bars sung + ring-out)

| bars | what happens | register |
|------|--------------|----------|
| 1–4 | A, first pass. Pickup "What's inside" flies high (F♯5→C5) then settles E4–G♯4 | mid |
| 4–6 | B — drops to D3/G3, ends on a subterranean E2 "inside" | low |
| 7–11 | C — conversational D3 rising to A3–G4 on "Looks like you could use a" | low→mid |
| 11–12 | "hug" + **scream #1** (E2 growl territory) | — |
| 12–15 | A, second pass (G4/F♯4 center, thicker) | mid |
| 16–18 | B again (D3, ends G♯2) | low |
| 19–22 | C again, "love" lifts to D4 | mid |
| 22–23 | "hug" + **scream #2** (jumps to D5) | high |
| 24–31 | A, **climax pass** — G♯5/D5/G5, "heart" melisma held bars 27–31 | HIGH |
| 31–32 | breath / gap — the one real hole in the vocal | — |
| 33–36 | "Nothing but the hole…" + B (C♯3/F♯3 — harmonic shift!) | low |
| 37–44 | C **twice** back-to-back ("Hey what's up" ×2) — outro chant | mid |

## Carve implications (what "understanding the vocals" means)

1. **The scream is the drop trigger.** Screams land end-of-bar-11 and
   end-of-bar-22. Drop 1 hits bar 12, drop 2 hits bar 23–24 right as the
   vocal goes G♯5. Goatshard's +5 second-drop convention (Em→Am) belongs at
   bar 24 under the climax pass.
2. **B-phrase = bass truce.** When the vocal sits at D3–E2 (bars 4–6, 16–18,
   33–36) the goat throat and bass must clear out or double it in unison —
   never fight that register.
3. **C-phrase is call-and-response.** "Hey, what's up?" begs shard-stab
   answers in the gaps (offbeats of bars 7–8, 19–20, 37–44). The outro
   double-C is the chant section: loop it, filter it, let the crowd have it.
4. **Bars 31–32 gap** is the only clean spot for a riser/silence before the
   final act.
5. **Melisma bars 27–31**: one held "heart" — sidechain everything to it,
   this is the lighter-in-the-air moment.
6. **SFX drawer exists**: per-singer one-shots (name drops, whistles, "hey!",
   groans, pops, "tttchew") in `samples/whats-inside-your-heart/sfx/` — use
   these as the frictus-tick/fill layer instead of generic percussion.
7. **Per-singer takes exist** (`takes-135/`): Alex/Camille/Jeffrey solo
   Best/Alt takes at 135 BPM — effectively per-voice stems. The round can be
   re-spatialized: one voice per pan position, drop voices in one at a time.

## Full word table (bar:beat @ pickup-relative grid, note)

```
 1:0.11 What's ·      | 12:3.83 What's G4    | 24:0.71 What's G♯5
 1:1.77 inside F♯5    | 13:1.59 inside F♯3   | 24:1.89 inside D5
 1:3.29 your C5       | 13:2.75 your G4      | 24:2.95 your D5
 2:0.35 heart F4      | 13:3.73 heart F♯4    | 24:3.70 heart D5
 2:2.03 Nothing E4    | 14:1.72 Nothing D4   | 25:1.24 Nothing G5
 3:0.05 but G4        | 14:3.49 but F♯4      | 25:3.14 but F♯5
 3:0.91 the G♯4       | 15:0.55 the G4       | 26:0.07 the G5
 3:1.77 hole F♯4      | 15:0.98 hole F4      | 26:0.78 hole F♯5
 3:2.93 you F♯4       | 15:1.98 you F♯4      | 26:1.86 you E5
 4:0.10 left E4       | 15:2.73 left F♯4     | 26:2.68 left E5
 4:0.94 behind E4     | 15:3.79 behind E4    | 26:3.77 behind E5
 4:3.64 Pumping D3    | 16:2.22 Pumping D3   | 27:2.31 What's E5
 5:2.09 blood D3      | 17:0.94 blood D3     | 28:3.68 inside ·
 5:3.83 and F3        | 17:2.57 and G3       | 30:1.05 your ·
 6:0.88 worms G3      | 17:3.68 worms E3     | 31:0.63 heart G4 (melisma end)
 6:2.62 inside E2     | 18:1.59 inside G♯2   |
 7:1.98 Hey ·         | 19:0.31 Hey D♯4      | 32:3.95 Nothing E4
 7:3.74 what's D3     | 19:1.55 what's D3    | 33:1.61 but G4 · 33:2.22 the A4
 8:1.42 up D3         | 19:3.14 up ·         | 33:2.92 hole E4 · 33:3.87 you G4
 8:3.05 Where's F♯2   | 20:0.39 Where's G♯3  | 34:0.58 left E4 · 34:1.53 behind E4
 9:1.30 the D3        | 20:2.15 the G3       | 34:3.77 Pumping C♯3
 9:2.25 love D3       | 20:2.49 love D4      | 35:1.95 blood C♯3 · 35:3.49 and F♯3
 9:3.59 Looks A3      | 21:0.01 Looks G♯3    | 36:0.42 worms F♯3 · 36:1.94 inside E3
10:1.51 like C♯4      | 21:1.78 like F♯4     | 37:0.84 Hey G♯2 · 37:2.59 what's G♯4
10:3.05 you G4        | 21:3.03 you F♯4      | 37:3.90 up D3
11:0.21 could F♯4     | 22:0.03 could F♯4    | 38:1.37 Where's G♯3 · 38:3.21 the D♯3
11:2.14 use E2        | 22:1.68 use E4       | 39:0.21 love D4
11:3.29 a E2          | 22:2.70 a E4         | 39:1.91 Looks G♯3 · 39:3.67 like G4
11:3.68 hug+SCREAM E2 | 22:3.02 hug+SCREAM D5| 40:0.74–40:3.98 you could use a hug (E4)
                      |                      | 41:1.84–42:0.13 Hey what's up (E4/F4/D3)
                      |                      | 42:1.24–43:0.07 Where's the love (A3)
                      |                      | 43:1.77–44:3.85 Looks like…hug (F♯4→E4)
```
