# AC Cast — the recurring characters of our productions

A reusable casting bible for every illustrated/felt AC production (pop covers,
marketing campaigns, grant films, papers). Drop a character's block into a
gpt-image / gen-promo / gen-felt prompt to keep figures consistent across the
whole body of work. Two media dominate our productions:

- **Needle-felt diorama** — hand-crafted wool puppets, visible fibre, bead
  eyes, stop-motion film-still vibe (*Isle of Dogs* / Aardman). NOT cartoon,
  NOT plush, NOT cute. Used by most /pop covers and the Restless Egg film.
- **Hatched colored-pencil + gouache on white paper** — print-comic restraint,
  diegetic light only. Used by the keymaps paper covers and the "computer help
  call" /pop covers.

> Source-of-truth prompt files are cited per entry — read them in full when you
> need the exact wording. Keep this file updated as new characters are cast.

---

## Leads

### Jeffrey (the recurring lead)
The constant across nearly every production. Recognizably the man in
`marketing/lib/jeffrey-refs.mjs` (SHOOT + SELFIE photos) — late-30s, medium
brown hair, short brown beard, **red plastic glasses**.
- **Signature props:** a **yellow Sailor Pro Gear fountain pen** clipped at the
  chest; an embroidered **yellow felt bear emblem** on the shirt; pale-blue
  pinstripe button-down; cobalt wide-leg trousers. (Vape pen as an alt prop in
  candid/internet-creator contexts.)
- **Felt form:** brown felt-yarn hair, short felt beard, hazel bead eyes,
  pale-flesh felt face. Source: `pop/dance/bin/cover-prompt.felt-portrait.template.txt` (lines 5-10).
- **Illustrated form:** see the keymaps cover, `papers/arxiv-keymaps/figures/cover-prompt.txt`.
- **Framing:** programmer + internet-native creative-computing artist (painting
  is the *root*, not the headline — see `grants/ssrc-just-tech-2026/`). Usually
  **absorbed in playing**, never an ad-hero pose.

---

## Ensembles

### The pixsies (the /pop "pixsies" album crew)
An **unnamed felt ensemble** (identity = a shared look, not a cast list) that
surrounds jeffrey across pop covers/visualizers.
- Felt puppets, **bead eyes** with glossy screen-catching pupils; the uncanny
  "tells": tiny **LED beads glowing cyan-green under the felt skin** (temple/
  ear/eye), hairline face seams, slightly off-human head proportions.
- **Rounded human ears only** — never pointed/elf/fae (despite "pixie").
- Wide diversity (all ages/races/genders), "grad-student energy" — thoughtful,
  a little tired, joyful. Eclectic clashed felt wardrobe (glasses, beanies,
  cardigans, tactical-vests; militaristic / girly-cute / cyberpunk-techwear).
- Each holds a **tiny AC laptop** — the one non-felt object, **shiny hard
  plastic**, lid bearing the **PALS logo** (two Keith-Haring-style linked
  outline-people). Free hand often raised palm-forward ("we are human").
- **Mood shifts per release:** eerily still + watching (trancepenta) → manic
  grinning party-demons with live-fire eyes (hellsine).
- Source: `pop/dance/bin/cover-prompt.felt-portrait.template.txt` (canonical
  look); fullest crew description in `pop/hellsine/hellsine.illy.txt` (~156-217).

### NELA Computer Club
A real Chinatown-LA meetup, rendered as a **packed cinema crowd** (archetypes,
not named characters).
- **The clubbers** — nerdier front-rows; plain gear (flannel/hoodie/tee/soft
  sweater), each with a small **open clamshell laptop** of varied make
  (ThinkPad, Framework, old MacBook, Pinebook, sticker-covered) showing real
  content (green terminal, sprite editor, music staff, paint, oscilloscope).
- **The cool-LA crowd** — fashion-aware locals 20s-40s (vintage tees, oversized
  button-downs, leather, gold, dyed hair); NO laptops — they hold **snacks +
  drinks** (striped popcorn, pretzels, tallboys, natural wine, haw flakes).
- **Room/palette:** deeply saturated **sage-green velvet** (seats/walls/carpet)
  under a riot of **multi-colored screen-light**; warm brass-sconce gold. Club
  brand = **black-on-white monospace ASCII/box-drawing** (`░░░`/`▒▒▒`).
- jeffrey sits *among* the clubbers, off-center, never the host.
- Source: `marketing/campaigns/nela-now-instant/cover-prompt.txt` + `brand-brief.md`.

---

## Recurring foils — the CEOs
Specific named tech CEOs drawn as jeffrey's **peer/double on the same eye-line**
(never a hero/villain pose), paired with him by a shared object — a matching
**Sailor Pro Gear fountain pen** (jeffrey yellow, the CEO another color) or the
**PALS laptop**. Felt versions wear **tattered/frayed felt clothing**.

### Bill Gates
Older, grey hair, soft rectangular glasses, **muted sage-green crewneck
sweater**; a **deep-red Sailor Pro Gear pen** clipped to his collar. Pensive,
faintly melancholy. Hatched colored-pencil ("computer help call" series).
- Source: `pop/marimba/marimbaba.illy.txt` (~34-43); `pop/RELEASES.md` (search "Gates").

### Mark Zuckerberg
- **Felt, young (~20-21):** pale freckled felt face, slight overbite, curly
  light-brown felt-yarn hair (cowlick + forehead curl), **navy felt hoodie with
  a faded crimson Harvard-style arch**, anxious/overwhelmed; operates the one
  PALS laptop. Source: `pop/dance/bin/cover-prompt.felt-portrait.template.txt` (line 12); trancepenta cover.
- **Illustrated, modern:** onboards jeffrey through a portal into a grey
  Horizon-Worlds metaverse (amazing grace / amaythingra story). Source: `pop/RELEASES.md` (~274-287).

---

## Cross-production motifs (the casting glue)
- **PALS laptop** — shiny plastic, lid = two linked Keith-Haring outline-people.
- **Sailor Pro Gear fountain pen** — jeffrey's is yellow; CEOs get a different color.
- **Yellow felt bear emblem** on jeffrey's shirt.
- **LED-under-felt glow** — the pixsies' uncanny tell.
- **Laptop geometry rule:** one screen, on the inner face only; a laptop seen
  from behind shows a plain blank lid back — never a screen/glow bleeding through.

## Source files (read in full when needed)
| Set | Canonical file |
|---|---|
| jeffrey (felt) | `pop/dance/bin/cover-prompt.felt-portrait.template.txt` |
| jeffrey (illustrated) | `papers/arxiv-keymaps/figures/cover-prompt.txt` |
| jeffrey identity photos | `marketing/lib/jeffrey-refs.mjs` |
| pixsies | `pop/dance/bin/cover-prompt.felt-portrait.template.txt`, `pop/hellsine/hellsine.illy.txt` |
| NELA Computer Club | `marketing/campaigns/nela-now-instant/{cover-prompt.txt,brand-brief.md}` |
| CEOs | `pop/marimba/marimbaba.illy.txt`, `pop/RELEASES.md` |
