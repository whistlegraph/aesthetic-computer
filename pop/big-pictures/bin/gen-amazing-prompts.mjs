#!/usr/bin/env node
// gen-amazing-prompts.mjs — writes cover-prompt.txt for the
// amazing-grace **church / notepat / Neo laptop** scene.
//
// Concept (jas, 2026-05-20):
//   In a SMALL OLD CHURCH SANCTUARY at golden hour, jeffrey sits
//   among the congregation (NOT on a pulpit), CRADLING HIS CHARTREUSE
//   MACBOOK NEO in his lap, SINGING amazing grace while PLAYING the
//   notepat keyboard interface on the open laptop screen. The
//   pixsies + AC freaks sit in the pews around him, listening,
//   joining softly. Warm honey window light through the stained
//   glass. Peer-horizontal — jeffrey is one of the congregation,
//   never centered as preacher.
//
// Photoreal candid phone snapshot; portrait 1024×1536 for verses,
// square 1024×1024 for the cover.
//
// Usage: node pop/big-pictures/bin/gen-amazing-prompts.mjs [--dry]

import { writeFileSync, existsSync, copyFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

const DRY = process.argv.includes("--dry");
const ROOT = `${homedir()}/Documents/Shelf/gens/amazing-grace-sections`;

const MEDIUM =
`A CANDID PHOTOGRAPH — true-to-life color, real phone/camera snapshot taken inside a small old church sanctuary in late afternoon. Soft natural light from tall stained-glass windows; very fine grain; air a touch dust-mote-hazy. PHOTOREAL, not an illustration, not a painting, not colored-pencil, not gouache, not cinematic-glossy, not neon. Deadpan, tender, peer-horizontal: jeffrey shares the same eye-line as the congregation, nobody centered as a hero. NOBODY acknowledges the camera — all of them are inside the song. Tall vertical portrait orientation, composed so it also crops cleanly to a centered square. ABSOLUTELY NO app/phone/story UI chrome (no bars, avatars, handles, icons, send bars).`;

const SETTING =
`SETTING — the interior of a small old Protestant-style church sanctuary: warm wood pews in two short rows, faded red velvet seat cushions, a worn dark-wood center aisle, a low modest wooden pulpit at the very back (small, far, not the focus), an upright piano off to the side untouched, a tall arched window of stained glass throwing colored light, a small wooden cross on the back wall (modest, not large). Honey golden light slanting in. Hand-lettered hymn-board on the wall reading nothing readable. Plain wood-and-plaster walls, a couple of cheap metal chairs along the aisle, dust motes in the light beams. NO readable real-world brand wordmarks anywhere; signage generic and illegible. The hush of a Sunday-afternoon practice or a small open-door evening service.`;

const JEFFREY =
`JEFFREY (recognizable from the jeffrey reference photographs — medium-length brown hair, beard, his real face), REAL person, photoreal skin and hair: he sits in a PEW like everyone else — NOT at the pulpit, NOT centered as a preacher — third pew from the back on the aisle side, his green laptop OPEN ACROSS HIS LAP. He is SINGING — mouth gently open mid-word, head tilted slightly down toward the laptop screen, eyes half-closed in the music, a faint half-smile. His RIGHT HAND rests on the laptop keyboard, fingers gently pressing keys (he is PLAYING the on-screen notepat piano — keystrokes drive the notes, not a separate physical keyboard). His outfit from the references: a soft pale-blue faint-pinstripe long-sleeve button-down with a small embroidered yellow bear emblem low on the chest, a bright YELLOW PEN clipped at the chest pocket, soft RED plastic glasses on a thin cord around his neck, wide-leg medium-cobalt trousers, worn black canvas low-top sneakers. He looks tender, plain, fully inside the song. A peer among the congregation.`;

const NEO =
`JEFFREY'S LAPTOP — exactly ONE laptop in the frame: jeffrey's CITRUS-GREEN MacBook Neo, open across his lap. The lid faces back so the screen is partly visible to viewers behind him — but the SCREEN CONTENT is the simple notepat piano interface (a clean horizontal row of black + white piano keys, two octaves wide, on a dark slate background, with a single white-glowing key marking the note being played; NO app chrome, no menus, no logos). The closed-side of the lid (behind the hinge, facing the camera) carries a TORN WHITE-PAPER SCRAP taped where the Apple logo would be; on that scrap, hand-penned in thick black ink, is a single whistlegraph BUTTERFLY (use the attached whistlegraph-butterfly.png — exactly that shape, NOT the Apple logo, NOT the PALS logo). No second laptop anywhere; the congregation hold none.`;

const CONGREGATION =
`THE CONGREGATION — about 8-12 people in the pews around and behind jeffrey, a quiet mixed audience listening: AC FREAKS (the human peers) + PIXSIES (small synthetic-but-passing-as-human folk) of varied ages, ages mixed from kids to elders. Most sit in the pews two or three to a bench, hands folded in laps or holding a small printed paper hymn-sheet. A few of them sing softly along with jeffrey — mouths gently parted, eyes a little closed. An older pixsie at the back has both hands cupped quietly in their lap. One small child-sized pixsie peers over the pew-back, fascinated by jeffrey's laptop. Real outfits — button-downs, hoodies, printed plain tees, cardigans, sweaters, flannels, the occasional light jacket; never tank tops, never costumes. NONE of the congregation are turned toward the camera. NONE hold laptops or phones.`;

const PIXSIES =
`PIXSIES — the small adorable folk mixed throughout the pews look almost entirely HUMAN, child-sized to adult-sized, an adorable diverse mix (varied faces, skin tones, builds, ages, kids to elders), passable as human with only SUBTLE synthetic tells: a tiny LED faintly glowing under the skin at a temple, behind an ear, in an eye; maybe one hairline seam — easy to miss. NOT clunky robots, NOT mannequins. natural human ears — never pointed/elf/animal.`;

const PALETTE =
`PALETTE — warm late-afternoon honey light through stained glass: soft amber + ruby + lavender beams across the pews, dust motes catching in the slanting beams, the wood and worn red velvet glowing warm, the back of the sanctuary cool and dim in shadow. jeffrey's pale denim-blues + buttery yellow pen + small red glasses-cord all readable in the warm light. The chartreuse-green Neo lid glows a touch warmer here. Real soft photographic light, low and tender, never neon, never punchy.`;

const AVOID =
`AVOID — any illustration / drawing / colored-pencil / gouache / painterly look (this is a PHOTOGRAPH); cinematic-glossy or neon glow; lasers; chrome/UI overlays of any kind (no Story bars / send bars / app icons / handles); ANY visible second laptop or phone; large readable brand wordmarks anywhere (hymn-board, screen, hoodies, lid); a hero-pose centering on jeffrey or a preacher-at-pulpit framing; figures looking at or acknowledging the camera; pointed elf ears on pixsies; tank tops, costumes, choir robes, or stage outfits; an apple-logo on the green Neo lid; ornate cathedral grandeur (this is a SMALL plain church, not a basilica); any prosperity-megachurch staging; tangled cornucopias of stuff in the frame.`;

const NOTEPAT_NOTE =
`THE LAPTOP-SCREEN DETAIL — the visible part of the notepat interface looks like a clean simple row of black-and-white piano keys (a 2-octave horizontal keyboard) on a dark navy / slate background, with ONE key softly highlighted in white where jeffrey is pressing right now. No menus, no logos, no chrome, no second window. NEVER the surrounding scene reflected on the screen (no recursive screens — [[feedback_imagegen_no_recursive_screens]]).`;

const SECTIONS = {
  verse1:
`SECTION — VERSE 1 ("amazing grace, how sweet the sound"): the FIRST and only verse for this single-edit cover. jeffrey just settles in the pew, opens the Neo across his lap; the congregation has turned softly toward him in their seats; he begins to sing the first phrase, his right hand on the keyboard playing the first notepat keys; the warmest honey light is just landing across the pews. Two or three of the singers smile softly toward him. Mood: arrival, breath, the gentle first note of the hymn. Frame is intimate but wide enough to see ~6-8 of the congregation around him in pews.`,
  verse2:
`SECTION — VERSE 2 ("twas grace that taught my heart to fear"): the song has settled. The congregation now leans slightly in, heads tilted gently, eyes closing. jeffrey's eyes mostly closed, fully inside the chord. Light a touch warmer.`,
  verse3:
`SECTION — VERSE 3 ("through many dangers toils and snares"): a swell — singers join more audibly. An AC freak in the next pew rests a hand on the shoulder of the person beside them; one pixsie wipes the corner of an eye with a knuckle. Light still warm honey.`,
  verse4:
`SECTION — VERSE 4 ("the lord has promised good to me"): hand-clasping starts. Two people across the aisle reach across and squeeze hands once; a child-sized pixsie leans softly against an older pew-mate. jeffrey calm and steady on the notepat. Honey light deepening to amber.`,
  verse5:
`SECTION — VERSE 5 ("yea when this flesh and heart shall fail"): the most reverent verse. The sanctuary hushed. jeffrey leans a touch into the laptop, the chord held longer. Light raking long shadows across the pews. Mood: held breath; not sad, just deep.`,
  verse6:
`SECTION — VERSE 6 ("the earth shall soon dissolve like snow"): the stained-glass light is going cooler — amber turning to dusty rose. A few pew-overhead pendant lamps switching on, warming faces from above. jeffrey's eyes closed, the Neo screen casting a faint glow up on his face from the notepat keys. Mood: vastness, smallness.`,
  verse7:
`SECTION — VERSE 7 ("when weve been there ten thousand years"): final verse. Light has dropped — only the warm pendant lamps, a low last-color slip through the stained glass. The congregation faces lit warm from above. A few singers have stood quietly in their pews. jeffrey is still seated with the Neo across his lap, hands resting on the keys for the final hold. Mood: gentle release, the song finishing, no applause yet.`,
  cover:
`SECTION — COVER (square 1024×1024 hero crop): center the frame on jeffrey mid-pew with the chartreuse Neo open across his lap, his right hand on the keyboard, mouth gently open singing. Two or three of the congregation visible to his left and right in the pew and behind him, also gently singing or listening. The stained-glass window beam catches him from the side. The Neo's white-paper whistlegraph-butterfly scrap on the closed-side of the lid is clearly visible. The notepat 2-octave keyboard on the screen is partly visible from this angle, one key softly highlighted. Composition allows a small clear band at the top of the frame (church wall + light) suitable for a release wordmark later. Peer-horizontal, no hero-centering.`,
};

function build(section) {
  return [
    MEDIUM, SETTING, JEFFREY, NEO, NOTEPAT_NOTE, CONGREGATION, PIXSIES,
    SECTIONS[section], PALETTE, AVOID,
  ].join("\n\n");
}

let wrote = 0;
for (const section of Object.keys(SECTIONS)) {
  const dir = join(ROOT, section);
  const p = join(dir, "cover-prompt.txt");
  const body = build(section);
  if (DRY) {
    console.log(`── ${section} (${body.length} chars) ──`);
    console.log(body.slice(0, 200) + "…\n");
    continue;
  }
  if (existsSync(p)) {
    const stamp = new Date().toISOString().replace(/[:.]/g, "-");
    copyFileSync(p, p.replace(/\.txt$/, `.${stamp}.bak`));
  }
  writeFileSync(p, body + "\n");
  wrote++;
  console.log(`✓ wrote ${section}/cover-prompt.txt  (${body.length} chars)`);
}
if (!DRY) console.log(`\n${wrote} prompt(s) written under\n  ${ROOT}`);
