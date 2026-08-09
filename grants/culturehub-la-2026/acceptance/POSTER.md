# Poster & event image — art direction

One illustration serves the event page, the poster, and the social cards. House
style is colored pencil on paper, matching the *Special Sign* cover (*Attic
Gremlin*) and the Menu Band campaign art in
`marketing/campaigns/menuband-looking-for-players/`.

## The picture

The two works in one frame. Three MacBook Neos sit in a row across the front —
indigo, citrus, blush — lids open, side by side, reading as a Neapolitan block.
Behind and around them, six mismatched salvaged laptops face inward in a ring.
A listener sits on the floor inside the ring. Speakers in the corners.

The Neapolitan row is the hero. The salvaged ring is the world it sits inside.

## Deliverables

| Crop | Size | Use |
| --- | --- | --- |
| 3:2 landscape | 1800×1200 | Event page, artist page, press |
| 1:1 square | 1600×1600 | Social cards |
| 4:5 portrait | 1400×1750 | Instagram, printed flyer |

Compose wide, with headroom at top and floor at bottom so the square and
portrait crops both survive. Keep the three colored laptops fully inside the
square crop.

## Generation prompt

> a horizontal 3:2 landscape COLORED-PENCIL DRAWING on paper — not a photograph.
> limited basic palette; build all tone from visible hatching and parallel
> striping, strokes showing direction and pressure; tapered open edges with paper
> tooth showing through; colors mix optically with gentle moiré where strokes
> cross. confident, observed, hand-drawn marks. fully legible and crisp — NO
> smudging, NO blur, NO soft wash, NO bokeh. compose wide.
>
> a dim studio performance room. across the front, three brand-new thin laptops
> stand open in a row, shoulder to shoulder, their lids in three distinct matte
> anodised colors — a dark slate navy indigo, a bright lemon-yellow citrus (yellow,
> NOT green and NOT chartreuse), and a soft dusty desaturated blush pink —
> reading together like a block of neapolitan ice cream. each of their
> screens is mostly empty desktop with a thin bright strip of tiny colorful piano
> keys running along the very top edge of the screen, like a menu bar. behind
> them, six mismatched older laptops (scuffed thinkpads, plastic netbooks,
> stickered lids, one with a cracked bezel) face inward in a wide ring, their
> screens showing a grid of colorful keyboard pads in reds, oranges, yellows,
> greens, blues, purples. one person sits cross-legged on the floor in the middle
> of the ring, eyes closed, listening. small dark speakers on stands in the
> corners, angled inward. warm overhead light plus the multicolored glow of nine
> screens on the floor and on the listener's face. a few small PALS stickers (two
> pink line-art figures holding hands) on the older lids. the room feels
> generous, quiet, and alive.
>
> avoid: blur, bokeh, soft wash, neon glow with no real light source, recursive
> screens (never a thumbnail of this scene shown on a screen), childish cartoon
> or flat vector look (this is a sincere hand-drawn colored-pencil drawing),
> corporate brand logos, any fruit logo or brand mark on a laptop lid, text or
> lettering anywhere in the image, costumes, mis-spellings.

Type is set separately in the packet's system — do not ask the generator for
lettering.

## Typography for the poster

From `packet/culturehub-packet.sty`:

- Title in `ywft-processing-bold` — **Whistlegraph presents**
- Work titles in `ywft-processing-light` italic — *Special Sign* · *MacNeoPolitan*
- Rule and accent in `acpink` `#B44887`
- Body in Latin Modern Sans, `acdark` `#312B38`
- PALS mark at the foot (`packet/assets/pals.pdf`)

## Alt text — write after the image is chosen

Describe, in order: the three colored laptops in a row, the menu-bar keyboards
along their screen tops, the ring of older laptops behind, the seated listener,
the corner speakers. Name the three colors. Do not use the word "Neapolitan" in
alt text — describe the block instead.

## Status

**MacNeoPolitan art is done** — three illustrations generated 2026-08-09 in
`marketing/campaigns/macneopolitan/gens/` (gpt-image-2, 1536×1024, 3:2), with
prompts, references, alt text, and provenance beside them:

- `block.png` — the Neapolitan block, three machines overhead, notes passing
- `trio.png` — Jeffrey seated in the middle of the ring of three
- `menubar.png` — the tiny piano beside the WiFi, battery, and clock

Those three cover *MacNeoPolitan* alone. The combined two-work image is
`gens/program.png` — three Neos open toward the viewer across the front with
their lit menu-bar strips visible, the six salvaged laptops ringed behind, a
listener between them. That is the one the event page runs.

Note the style split: the three MacNeoPolitan images are in the warm painterly
house style of the Menu Band campaign, while `program.png` is colored pencil,
matching the *Special Sign* cover and the existing public-page header. That is
deliberate — the program image has to sit across both works, so it belongs to
the CultureHub-facing lineage rather than to MacNeoPolitan's own.

- [x] Generate MacNeoPolitan art via the `illy` skill
- [x] Write alt text for all three
- [x] Generate the combined two-work image — `gens/program.png`, colored pencil,
  three Neos open toward the viewer across the front, the salvaged six ringed
  behind, a listener between them
- [x] Replace the interim image reference in `EVENT-PAGE.md`
- [x] Produce the square and portrait crops — `gens/program-{3x2,square,portrait}.png`
- [ ] Rebuild `packet/event-page-packet.pdf`
- [ ] Verify the blush machine against the real one once it is bought
