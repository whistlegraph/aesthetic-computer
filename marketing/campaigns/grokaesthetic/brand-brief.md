# Grokaesthetic — campaign

For the CultureHub LA event page (event form drafted 2026-08-13; see
`grants/culturehub-la-2026/acceptance/FORM-EVENTS-2026-08-13.md`). Two public
events: Grokaesthetic Workshop (Sat Sep 19, BYOC hands-on) and the performance
(Thu Sep 24).

## The wordmark

One compound word, two voices: **"grok"** hand-lettered (AI, pink chisel
marker) flowing directly into **"aesthetic"** in YWFT Processing Bold. Flat
version at `refs/grokaesthetic-wordmark.png`, built deterministically from
`gens/grok-lettering.png` + a PIL render of the real OTF.

### Spelling gotcha — the reason for the green-screen route

gpt-image-2 **reliably mangles "grokaesthetic" when asked to place it onto a
scene** — three separate edit passes produced "aessthetic", "AASSTHETC", etc.
It spells the word correctly when the word IS the subject of a standalone
generate. So the working route is:

1. Generate the wordmark alone, in the target material, on a flat
   **pure chroma-key green** field (`gens/materials/type-chrome-key.png`).
2. Key + despill + trim it into a cutout (`refs/type-chrome-cutout.png`).
3. Composite it as a real type layer over the scene.

This is a type layer over artwork (normal poster design), not retouching a
failed detail inside an illustration — the no-compositing rule still holds for
fixing artwork.

## Material studies

`gens/materials/` — `CONTACT-SHEET.png` compares all four. Each keeps the
two-voice letterform contrast (loose brushy "grok" + squared pixel
"aesthetic"):

- **type-stone** — pale granite, deep relief on a rough-hewn slab, raking light
- **type-chrome** — extruded mirror chrome, magenta/cyan reflections *(chosen)*
- **type-inflatable** — glossy hot-pink vinyl balloon letters, seamed
- **type-glass** — cast glass with magenta/cyan caustics on a dark floor

## Scene renders

All 1536×1024 (3:2). Real AC interfaces on every screen — the purple prompt UI
(`refs/ac-prompt.png`) and notepat (`refs/ac-notepat-preview.png`, pulled from
`oven.aesthetic.computer/preview/1200x630/<piece>.png`). Real black ThinkPads
and MacBook Neos, never invented hardware. Jeffrey from the portraits platter.

- **`grokaesthetic-lasers.png`** — Saturday workshop: colored-pencil, laser
  lightworks, chrome GROKAESTHETIC wordmark in the headroom band
- **`performance-poster.png`** — Thursday performance: same scene carrying the
  two piece wordmarks instead — *Note(s)pat(ial) Native* over *The
  MacNeoPolitan Trio* with an ampersand, the broadside masthead lockup, both
  as **3D type** (`gens/materials/type-notespatial-3d.png`,
  `type-macneopolitan-3d.png`, green-screen route again — both spelled
  correctly first try, parentheses and internal caps intact):
  - Note(s)pat(ial) Native — extruded pixel-grid blocks, chrome-white with
    glowing violet parentheses
  - The MacNeoPolitan Trio — glossy enamel sans, neapolitan coloring (cream
    "The"/"Trio", blush "Mac", citrus "Neo", indigo "Politan")

  A first pass keyed the flat packet assets (`notespatial-grid-logo.png`,
  `macneopolitan-wordmark.png`) instead — don't. They are only ~630px wide, so
  upscaling plus a per-pixel neutral/colored recolor turns the antialiased
  edges into confetti. (A luminance key also silently deletes the light purple
  and pink glyphs; key on distance from the paper color if you ever must.)
- `scene-headroom.png` — same drawing, no type (title-safe upper quarter)
- `program-drawing-1536.png` — the full-bleed drawing, no headroom, no type
- `alt-photo.png` — photographic render of the same tableau
- `alt-vrworld.png` — social-VR / game-engine render of the same tableau

## Also here

- `grokaesthetic-poster.png` — the studio-photo chair poster (real
  `jeffery-av--02.jpg` + flat pink/YWFT wordmark). Alternate for the workshop.

Palette: packet colors — acpink #B44887, acdark #312B38, acpurple #7850B4.
No dates in any image; dates are still proposals pending CultureHub.
