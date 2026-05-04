#!/usr/bin/env node
// instrument-cards.mjs — generate Pokemon-card-style 1080×1080 Instagram
// squares for menuband General-MIDI instruments.
//
// Pipeline:
//   1. FLUX (NVIDIA flux.1-schnell, free) renders a 1024×1024 personification
//      illustration for the given instrument number.
//   2. Python compositor lays in Processing YWFT typography, the program
//      number, the family badge, the QR code → aesthetic.computer/menuband,
//      and the printed URL.
//
// Usage:
//   node bin/instrument-cards.mjs                  # default: 5-card test set
//   node bin/instrument-cards.mjs --programs 0,24,40,56,73
//   node bin/instrument-cards.mjs --programs 0     # single
//   node bin/instrument-cards.mjs --out ~/Desktop/menuband-cards
//
// Output: 1080×1080 PNGs named `NNN-<slug>.png` (e.g. `000-acoustic-grand-piano.png`).

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(__dirname, "../../..");
const POP_PY = `${REPO}/pop/.venv/bin/python`;
const COMPOSER = `${__dirname}/instrument-card-compose.py`;
const FONT_REG = `${REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-regular.ttf`;
const FONT_BOLD = `${REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf`;

const NVIDIA_KEY = readFileSync(
  `${REPO}/aesthetic-computer-vault/.env`,
  "utf8",
).match(/^NVIDIA_API_KEY=(\S+)/m)?.[1];
if (!NVIDIA_KEY) {
  console.error("✗ NVIDIA_API_KEY not found in vault .env");
  process.exit(1);
}

// ── flags ───────────────────────────────────────────────────────────────
const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = process.argv[i + 1];
}
const DEFAULT_TEST = [0, 24, 40, 56, 73]; // piano, nylon guitar, violin, trumpet, flute
const programs = flags.programs
  ? flags.programs.split(",").map((s) => parseInt(s, 10)).filter((n) => n >= 0 && n < 128)
  : DEFAULT_TEST;
const OUT = flags.out
  ? resolve(process.cwd(), flags.out.replace(/^~/, homedir()))
  : `${REPO}/slab/menuband/assets/instrument-cards`;
const RAW = `${OUT}/.raw`;
mkdirSync(OUT, { recursive: true });
mkdirSync(RAW, { recursive: true });

// ── GM 128 (mirrors slab/menuband/Sources/MenuBand/GeneralMIDI.swift) ───
const GM = [
  "Acoustic Grand Piano", "Bright Acoustic Piano", "Electric Grand Piano", "Honky-tonk Piano",
  "Electric Piano 1", "Electric Piano 2", "Harpsichord", "Clavinet",
  "Celesta", "Glockenspiel", "Music Box", "Vibraphone",
  "Marimba", "Xylophone", "Tubular Bells", "Dulcimer",
  "Drawbar Organ", "Percussive Organ", "Rock Organ", "Church Organ",
  "Reed Organ", "Accordion", "Harmonica", "Tango Accordion",
  "Acoustic Guitar (nylon)", "Acoustic Guitar (steel)", "Electric Guitar (jazz)", "Electric Guitar (clean)",
  "Electric Guitar (muted)", "Overdriven Guitar", "Distortion Guitar", "Guitar Harmonics",
  "Acoustic Bass", "Electric Bass (finger)", "Electric Bass (pick)", "Fretless Bass",
  "Slap Bass 1", "Slap Bass 2", "Synth Bass 1", "Synth Bass 2",
  "Violin", "Viola", "Cello", "Contrabass",
  "Tremolo Strings", "Pizzicato Strings", "Orchestral Harp", "Timpani",
  "String Ensemble 1", "String Ensemble 2", "Synth Strings 1", "Synth Strings 2",
  "Choir Aahs", "Voice Oohs", "Synth Choir", "Orchestra Hit",
  "Trumpet", "Trombone", "Tuba", "Muted Trumpet",
  "French Horn", "Brass Section", "Synth Brass 1", "Synth Brass 2",
  "Soprano Sax", "Alto Sax", "Tenor Sax", "Baritone Sax",
  "Oboe", "English Horn", "Bassoon", "Clarinet",
  "Piccolo", "Flute", "Recorder", "Pan Flute",
  "Blown Bottle", "Shakuhachi", "Whistle", "Ocarina",
  "Lead 1 (square)", "Lead 2 (sawtooth)", "Lead 3 (calliope)", "Lead 4 (chiff)",
  "Lead 5 (charang)", "Lead 6 (voice)", "Lead 7 (fifths)", "Lead 8 (bass + lead)",
  "Pad 1 (new age)", "Pad 2 (warm)", "Pad 3 (polysynth)", "Pad 4 (choir)",
  "Pad 5 (bowed)", "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",
  "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)", "FX 4 (atmosphere)",
  "FX 5 (brightness)", "FX 6 (goblins)", "FX 7 (echoes)", "FX 8 (sci-fi)",
  "Sitar", "Banjo", "Shamisen", "Koto",
  "Kalimba", "Bagpipe", "Fiddle", "Shanai",
  "Tinkle Bell", "Agogo", "Steel Drums", "Woodblock",
  "Taiko Drum", "Melodic Tom", "Synth Drum", "Reverse Cymbal",
  "Guitar Fret Noise", "Breath Noise", "Seashore", "Bird Tweet",
  "Telephone Ring", "Helicopter", "Applause", "Gunshot",
];

const FAMILIES = [
  ["Piano",      0,   7,  [255, 90, 110]],
  ["Chromatic",  8,   15, [255, 200, 60]],
  ["Organ",      16,  23, [180, 90, 220]],
  ["Guitar",     24,  31, [240, 130, 50]],
  ["Bass",       32,  39, [120, 80, 200]],
  ["Strings",    40,  47, [220, 60, 70]],
  ["Ensemble",   48,  55, [200, 110, 200]],
  ["Brass",      56,  63, [240, 170, 40]],
  ["Reed",       64,  71, [70, 170, 110]],
  ["Pipe",       72,  79, [80, 180, 220]],
  ["Synth Lead", 80,  87, [255, 80, 200]],
  ["Synth Pad",  88,  95, [120, 200, 240]],
  ["Synth FX",   96,  103,[140, 220, 140]],
  ["Ethnic",     104, 111,[210, 130, 80]],
  ["Percussive", 112, 119,[230, 230, 80]],
  ["Sound FX",   120, 127,[160, 160, 170]],
];

function familyFor(p) {
  return FAMILIES.find(([, lo, hi]) => p >= lo && p <= hi);
}

function slugify(s) {
  return s.toLowerCase()
    .replace(/[()]/g, "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}

// ── illustration prompt ─────────────────────────────────────────────────
// The instrument is the protagonist, rendered IN ITS environment. The scene
// must convey three things:
//   sonic vibe   — the atmosphere implied by the instrument's sound
//   material soul — the physical materials, grain, patina, wear
//   legacy        — the cultural / historical setting the instrument lives in
// Hockney-style colored-pencil + watercolor wash, hatching for tone.
function promptFor(name, family) {
  // Tone clause: positive directives only — FLUX (and its CLIP/T5 encoders)
  // misread negations like "no people" and the moderator flags the result.
  //
  // Aesthetic target: a researched naturalist instrument plate, the kind
  // you'd find in a museum specimen catalog or a Diderot encyclopedia,
  // executed in layered colored pencil on toned paper. Each beat should
  // name a specific historically-correct detail — maker, era, material,
  // mechanism — so the illustration reads as accurate, not generic.
  const tone = [
    "colored pencil illustration on warm toned paper, naturalist museum-plate style",
    "scientific instrument-catalog precision, layered prismacolor strokes with visible pencil hatching",
    "soft tapered pencil edges, careful cross-hatching for shadow, paper-tooth texture",
    "dramatic close-up macro angle on the defining detail, dynamic perspective",
    "warm natural lighting, palpable material textures, historically-correct construction details",
  ].join(", ");

  // Per-instrument scene: SETTING + MATERIAL + SONIC MOOD + LEGACY.
  const beats = {
    "Acoustic Grand Piano":   "the action mechanism of a Steinway Model D concert piano in close-up: a row of compressed-wool felt hammers above polished tri-chord steel strings on a cast-iron plate, spruce soundboard visible behind, brass capstans and the escapement of a single key, walnut rim, late-19th-century construction",
    "Bright Acoustic Piano":  "an upright spinet in a sunlit parlour at golden hour, lace doily on top, sheet music open, mahogany cabinet glowing, lid scratched from years of Sunday afternoons, dust motes in the air",
    "Electric Grand Piano":   "a tinted-lid Yamaha CP-style stage piano on a 1980s soundstage, chrome stand, soft purple gels, anodised aluminium frame, a coiled cable, lush reverberant studio air",
    "Honky-tonk Piano":       "a slightly out-of-tune saloon upright in a wooden-floored Western dance hall at night, beer-ringed top, candlelight, missing ivory chips, the thumping joy of a Friday-night singalong",
    "Electric Piano 1":       "a Fender Rhodes Mark I on a wooden floor in a dim 1970s studio, tine bars visible through an open lid, suitcase legs, soft bell-like air, a Leslie cabinet glowing in the background",
    "Electric Piano 2":       "a Wurlitzer 200A on a school-music-room stand, cream tolex worn at the corners, red-sparkle keys, a school chalkboard behind, the bittersweet warmth of an old auditorium",
    "Harpsichord":            "an ornate French double-manual harpsichord in a baroque salon, gilt rocaille flourishes, painted soundboard with flowers and birds, jacks and quills visible through the raised lid, candle sconces and tapestries",
    "Clavinet":               "a Hohner D6 clavinet under stage lights of a 1970s funk gig, hammered-string mechanism through the smoked plexi top, a coiled cable, talcum-soft purple haze",
    "Celesta":                "a small upright celesta in a Victorian conservatory, felt-covered hammers striking metal bars, frosted-glass windows, the chime of a snowfall scene",
    "Glockenspiel":           "a portable orchestral glockenspiel on a rosewood stand backstage, polished steel bars in chromatic order, a pair of brass mallets resting, the bright steely shimmer of a fanfare",
    "Music Box":              "an antique cylinder music box open on a mahogany side table, brass pin-cylinder and steel comb visible, faded velvet lining, lace doily, the tiny crystalline lullaby of a wound spring",
    "Vibraphone":             "a Deagan vibraphone in a smoky jazz club, motor-driven discs spinning under the silver bars, brass damper bar, sustain pedal, mid-century chrome stand, the cool slow shimmer of bowed sustain",
    "Marimba":                "a 5-octave concert marimba in a recital hall, deep rosewood bars over fibreglass resonators, four mallets balanced on the bars, parquet stage, the warm woody resonance of a low roll",
    "Xylophone":              "a 1920s vaudeville xylophone on a tubular stand, hard rosewood bars, ribbon-wrapped mallets, vaudeville theatre curtains behind, the bright pop of a ragtime run",
    "Tubular Bells":          "a set of tubular bells in a cathedral nave, brass tubes hanging in chromatic order, a felt mallet on a stand, stained-glass light, the sustained sacred peal of a slow strike",
    "Dulcimer":               "an Appalachian mountain dulcimer on a quilt-draped chair on a porch, fretted soundboard, walnut and spruce, hand-whittled noter, distant blue-ridge mountains",
    "Drawbar Organ":          "a Hammond B-3 with Leslie 122 cabinet on stage, drawbars pulled in classic jazz registration, a worn pedalboard, the Leslie horn rotating in slow motion, the warm churchy whirl of a held chord",
    "Percussive Organ":       "a Hammond B-3 with percussion tab engaged in a Sunday-morning jazz combo, click of the second-harmonic decay, oak woodwork, gospel-blue stage light",
    "Rock Organ":             "a Hammond C-3 cranked through an overdriven Leslie at a 1970s arena, motors blurred, distortion in the air, sweat on the wood",
    "Church Organ":           "a vast pipe organ in a Romanesque cathedral, ranks of metal and wood pipes rising into vaulted stone, a console with stops and pedalboard, dust-shaft sunlight, the immense low pedal of a held tone",
    "Reed Organ":             "a 19th-century Estey parlor reed organ in a New England farmhouse, treadle pedals, knee-swell, hand-stenciled cabinet, lace curtains, the wheezy hymn of a Sunday morning",
    "Accordion":              "a piano accordion in a Parisian café at twilight, mother-of-pearl keys, mirrored bellows fully open, leather straps, a marble-topped table, the bittersweet musette of a chromatic run",
    "Harmonica":              "a chromatic harmonica on a worn wooden bartop in a delta blues juke joint, brass reed plates visible through a cracked side, neon beer sign, the crying bend of a held note",
    "Tango Accordion":        "a bandoneón open on a Buenos Aires café table at midnight, square bellows fully extended, button rows on each end, a glass of red wine, the heart-stopping melancholy of a tango",
    "Acoustic Guitar (nylon)":"close-up of the rosette of an Antonio de Torres Jurado 1860s Spanish classical guitar, mother-of-pearl mosaic in concentric rings around the soundhole, fan-braced spruce top, three nylon trebles and three silver-wound nylon basses crossing the soundhole, ebony fingerboard edge, slotted cedar headstock blurred behind",
    "Acoustic Guitar (steel)":"a dreadnought steel-string guitar on a porch swing at sunset, satin spruce top with bronze strings glinting, a leather strap and capo, fields beyond, the open-tuned ring of an Americana ballad",
    "Electric Guitar (jazz)": "a hollow-body archtop on a brass stand in a smoky 1950s jazz club, sunburst lacquer, f-holes, a single jazz humbucker, a martini on a piano, the warm round tone of a chord-melody",
    "Electric Guitar (clean)":"a Fender Stratocaster on a wooden floor of a sunny rehearsal room, three single-coil pickups, maple neck and fingerboard, a small tube amp, the chimey clean shimmer of an arpeggio",
    "Electric Guitar (muted)":"a Telecaster with palm-mute hand pose suggested, on a stage in a tight funk band, single-coil bridge pickup, ash body, the percussive chk-chk of a sixteenth-note chop",
    "Overdriven Guitar":      "a Gibson SG plugged into a cranked Marshall stack, valves glowing, smoke curling from the cone, the saturated growl of a held bend",
    "Distortion Guitar":      "a black Les Paul on a metal-band stage, scooped-mid distortion in the air, stack of 4×12 cabinets, sweat and stage fog, the chunky palm-muted thrash of a power chord",
    "Guitar Harmonics":       "a steel-string guitar on a quiet wooden floor, light pinching a 12th-fret harmonic, golden afternoon light, the bell-like ring of a natural overtone",
    "Acoustic Bass":          "an upright double bass leaning on a piano in a 1940s jazz club, deep cherry-amber varnish, gut strings, a worn bow on the chair, the walking pulse of a slow blues",
    "Electric Bass (finger)": "a Fender Precision bass against a tweed amp on a club stage, sunburst body, rosewood fingerboard, fingerstyle calluses on the strings, the rounded thump of a Motown groove",
    "Electric Bass (pick)":   "a Fender Jazz bass with a tortoiseshell pick on the strings, in a punk-rock garage, the bright clack of a downpicked riff",
    "Fretless Bass":          "a fretless Jaco-style Jazz bass on a music stand, smooth ebony fingerboard with no fret lines, the singing mwah of a glissando",
    "Slap Bass 1":            "a Music Man StingRay bass under a single warm stage spot, slap-thumb pose suggested above the strings, the percussive pop of a funk groove",
    "Slap Bass 2":            "a five-string boutique bass on a soulful R&B stage, double-thumbing pose suggested, slick stage fog",
    "Synth Bass 1":           "a Minimoog Model D on a 1970s synth-bass stage, hands-on-knob lighting, oscillator banks glowing, the fat saw growl of a low note",
    "Synth Bass 2":           "a Roland TB-303 on a smoky acid-house stage, silver-and-orange face, the resonant squelch of a sliding pattern",
    "Violin":                 "close-up of the f-hole and bridge of a 1715 Stradivari Cremonese violin, two-footed maple bridge cut by hand, four wound strings rising over it, amber spirit-varnish on flamed-maple ribs, the tip of a pernambuco bow with horsehair in soft focus, scroll just out of frame",
    "Viola":                  "a deeper-toned viola on a velvet-lined case, slightly larger than a violin, warmer red varnish, on a Vienna conservatory stand, the rich alto sigh of a slow melody",
    "Cello":                  "a cello standing alone on a wooden recital stage, endpin in the floor, cherry-amber body, a bow leaning against the chair, single warm spotlight, the deep singing voice of a held G string",
    "Contrabass":             "a giant double bass leaning against a stone wall of an old conservatory, dark spruce top, gut strings, German bow on a stool, the cavernous low rumble of a pizzicato",
    "Tremolo Strings":        "a string section in a film-scoring studio, bows blurred mid-tremolo, music stands lit from below, the shivering shimmer of suspense",
    "Pizzicato Strings":      "a chamber string quartet plucking — fingers visible above strings — in a wood-panelled hall, the dry hopping pluck of a baroque dance",
    "Orchestral Harp":        "a concert pedal harp on a marble floor of an ornate hall, gilded column, 47 strings catching golden light, seven pedals at the base, the cascading glissando of a fairy scene",
    "Timpani":                "four pedal timpani in a symphony stage half-circle, copper kettles polished to a mirror, calfskin heads, mallets resting, the rolling thunder of a Mahler climax",
    "String Ensemble 1":      "a full violin section bowing in unison on a film-scoring stage, music stands in formation, the warm lush wash of a Hollywood sustain",
    "String Ensemble 2":      "a string quintet in a wood-panelled chamber hall, instruments mid-bow, the silken sustained chord of a film cue",
    "Synth Strings 1":        "a Roland Juno-60 patched for strings on a stage, blue LCD glow, slow filter sweep suggested by light, the buttery analogue ensemble pad of a slow ballad",
    "Synth Strings 2":        "an ARP Solina String Ensemble on a wooden organ stand, BBD chips inside suggested by a soft chorus halo, the wide swirling polyphonic strings of a 1970s ballad",
    "Choir Aahs":             "an empty cathedral nave with stained-glass light, choir stalls lit, music stands set, the suspended 'aah' chord still hanging in the stone",
    "Voice Oohs":             "a vocal-booth diorama with a single condenser mic on a brass stand, pop filter, headphones on a stool, sound-treatment panels, the rounded 'ooh' of a ghost vocal",
    "Synth Choir":            "a vintage Mellotron M400 with the lid open, tape racks visible, the breathy choir cassettes loaded, the haunting Ligeti-tinged choir of a sci-fi cue",
    "Orchestra Hit":          "an empty orchestra stage frozen the instant after a unison stab, every chair facing forward, instruments resting mid-impact, the air still ringing",
    "Trumpet":                "close-up of the three Périnet piston valves of a Bb Vincent Bach Stradivarius 180S trumpet, mother-of-pearl finger buttons, lacquered yellow-brass casings with silver-plated valve caps, water-key spit valve at the leadpipe, bell flare and tuning slide blurred behind, design lineage from 1818",
    "Trombone":               "a tenor slide trombone in a brass-band rehearsal room, slide fully extended on a stand, F-attachment trigger and slide grease pot beside, the broad confident gliss of a big-band shout",
    "Tuba":                   "a sousaphone-style tuba on a New Orleans street parade at dusk, huge brass bell facing forward, oompah pulse implied, the warm fat low end of a second-line groove",
    "Muted Trumpet":          "a trumpet with a Harmon mute in a 1950s blue-lit jazz club, muted bell pointing at a single mic, the cool intimate whisper of a Miles solo",
    "French Horn":            "a double French horn on a velvet stool in a wood-panelled symphony hall, four rotary valves, coiled brass, hand resting in the bell suggested, the noble heroic call of a Wagnerian theme",
    "Brass Section":          "a five-piece brass section on a Motown studio stage, two trumpets, two trombones, sax, brass bells in formation, the punchy stab of a horn riff",
    "Synth Brass 1":          "a Yamaha DX7 in a 1980s pop studio, glowing red display, FM brass patch suggested by light, the bright synthetic brass stab of a power ballad",
    "Synth Brass 2":          "an Oberheim OB-Xa under stage lights, slabs of analogue brass implied, the fat unison synth-brass of a Van Halen riff",
    "Soprano Sax":            "a straight soprano saxophone on a brass stand in a smooth-jazz studio, slim brass body, pearl keys, the curling reedy clarity of a Coltrane phrase",
    "Alto Sax":               "an alto saxophone on a velvet-lined case in a smoky 1940s jazz club, brass body and pearl key cups, neck strap, a martini on the bar, the smoky lyrical voice of a bebop solo",
    "Tenor Sax":              "a tenor saxophone on a wooden floor of a Brooklyn jazz cellar, deep brass body, the broad smoky baritone of a Coltrane sheets-of-sound run",
    "Baritone Sax":           "a big bari sax on a stand in a big-band stage, low S-curve neck and large bell, the meaty baritone of a swing-era riff",
    "Oboe":                   "a slim grenadilla oboe on a music stand in a Vienna recital hall, double reed clearly visible, silver keys, the plaintive nasal voice of a tuning A",
    "English Horn":           "a cor anglais resting on a black case beside a music stand, bulbous pear-shaped bell, alto-oboe length, the haunting pastoral mood of a Dvořák largo",
    "Bassoon":                "a contrabassoon and bassoon side-by-side on stands in an orchestra pit, dark grenadilla wood with silver keys, double reeds, the warm reedy bass voice of an orchestra",
    "Clarinet":               "a Bb clarinet on an open velvet-lined case, glossy ebony body, silver keys gleaming, in a Mozart-era recital hall with painted ceilings, the liquid singing voice of a slow movement",
    "Piccolo":                "a tiny silver piccolo on a marching-band drum, half the length of a flute, head joint and three-piece body, the bright piercing trill of a Sousa march",
    "Flute":                  "close-up of the Boehm-system mechanism of a Powell silver concert flute, open-hole French keys with pinned-pad pivots and steel needle springs, precisely chamfered tone-hole edges, embouchure cut into the head joint blurred behind, lineage from Theobald Boehm 1832",
    "Recorder":               "a wooden alto recorder in a Renaissance music room, ivory-tipped, on a velvet cloth, hand-painted madrigal book open, the breathy pure tone of a consort",
    "Pan Flute":              "a set of bamboo pan pipes lashed with cord on a stone wall in an Andean village at dusk, snow-capped peaks distant, the airy plaintive whistle of a quechua melody",
    "Blown Bottle":           "a row of glass bottles filled to different levels on a wooden table, breath-blown across the tops, soft afternoon light, the hollow round whistle of a folk experiment",
    "Shakuhachi":             "a bamboo shakuhachi on a tatami mat in a Zen temple, root-end bell, four front holes, ink calligraphy scroll behind, the meditative breathy long tone of a honkyoku",
    "Whistle":                "a tin whistle on a pub bar in Galway, brass mouthpiece, six finger holes in a green-painted body, a half-finished pint, the high lilting reel of a session",
    "Ocarina":                "a clay sweet-potato ocarina on a moss-covered stone, finger holes glazed, earthy terracotta, sun dappled forest, the round earthen tone of a folk lullaby",
    "Lead 1 (square)":        "a Roland SH-101 on a synth desk, square-wave waveform glowing on an oscilloscope behind, the buzzy biting square-wave lead of an 80s arcade",
    "Lead 2 (sawtooth)":      "a Minimoog with sawtooth output to a screen, the bright cutting saw lead of a prog-rock solo",
    "Lead 3 (calliope)":      "a steam calliope on a vintage carousel under string lights, brass whistles in a row, steam curling, the carnival joy of a fairground waltz",
    "Lead 4 (chiff)":         "a Yamaha DX7 with a breathy 'chiff' patch suggested, the airy attack-transient lead of an 80s pop track",
    "Lead 5 (charang)":       "a 1980s synth keyboard in a Latin-pop studio, pearlescent display, the bright wiry charango-like lead of a Madonna track",
    "Lead 6 (voice)":         "a Roland V-Synth with a voice-formant lead patch suggested, vocal harmonics rendered as a glowing throat-shape on the screen",
    "Lead 7 (fifths)":        "a parallel-fifths synth lead patch, an 80s synth on a darkened stage, the heroic open lead of a stadium anthem",
    "Lead 8 (bass + lead)":   "a Roland JX-3P bass-plus-lead split patch, the punchy 80s synth-pop hook",
    "Pad 1 (new age)":        "a sound-bath circle with a Yamaha DX7 in the centre, soft pastel halos in the air, the slow shimmering new-age pad of a meditation tape",
    "Pad 2 (warm)":           "a Roland Jupiter-8 panel glowing in a dim studio, slow filter sweep, a soft warm halo of analogue pad in the air",
    "Pad 3 (polysynth)":      "an Oberheim Matrix-12 on a wooden stand, big polyphonic chord stacks visible as a soft chord-cloud above, the lush polysynth of an 80s ballad",
    "Pad 4 (choir)":          "a synth-choir patch on a Korg M1, ghostly choir silhouettes hovering, the haunting choir-pad of a Twin Peaks cue",
    "Pad 5 (bowed)":          "a synth bowed-pad patch, a violin bow drawing across a synth-string image, the slow swelling bowed pad of an ambient track",
    "Pad 6 (metallic)":       "a Yamaha SY77 with a metallic FM pad patch, slabs of polished metal hovering, the cold metallic pad of a sci-fi score",
    "Pad 7 (halo)":           "a halo-pad patch suggested by a glowing soft-blue halo over a dark synth, the angelic suspended halo-pad of a film cue",
    "Pad 8 (sweep)":          "a Roland D-50 with a slow filter sweep visualised as a gradient ribbon, the cinematic swelling sweep-pad of a movie trailer",
    "FX 1 (rain)":            "a synth-rain patch — a pane of glass with diagonal raindrops streaking, the gentle bell-like rain-fx of a Vangelis track",
    "FX 2 (soundtrack)":      "an empty cinema, a single screen lit, the orchestral soundtrack-fx pad lingering",
    "FX 3 (crystal)":         "a synth-crystal patch — refracting glass shards arranged on a black velvet, the bright shimmering crystal-fx of a video-game intro",
    "FX 4 (atmosphere)":      "an evaporating mist over a still pond at dawn, the soft synth-atmosphere drone of an ambient piece",
    "FX 5 (brightness)":      "a flash of bright synth light over a darkened stage, the searing brightness-fx swell",
    "FX 6 (goblins)":         "a goblin-bell patch on a Roland JX-8P suggested by a forest of tiny mischievous bell-shapes glinting in dim light",
    "FX 7 (echoes)":          "a synth-echoes patch — concentric ripples on a still water surface, the distant echoing fx of a dub track",
    "FX 8 (sci-fi)":           "a glowing-cabled modular synth at night, neon glow on patch leads, the alien sci-fi sweep of a 1970s soundtrack",
    "Sitar":                  "a sitar on a silk cushion in a North-Indian raga session, gourd resonator, twenty sympathetic strings, ornate carved neck, the buzzing drone and slow alap",
    "Banjo":                  "a 5-string open-back banjo on a porch swing in Appalachia, drumhead and resonator visible, hand-carved tuners, the bright rolling clawhammer of a mountain tune",
    "Shamisen":               "a tsugaru shamisen on a tatami mat, square skin-covered body, three silk strings, ivory bachi plectrum, sliding-door silhouette, the percussive snapped attack of a folk piece",
    "Koto":                   "a 13-string koto on a low wooden stand in a Japanese tea room, paulownia wood body with movable bridges, ivory finger picks, sliding shoji screens, the cascading plucked phrase of a sakura melody",
    "Kalimba":                "a thumb piano on a woven mat in a sunlit African courtyard, hand-tuned metal tines on a hardwood resonator board, the gentle chiming patter of a kalimba ostinato",
    "Bagpipe":                "a Highland bagpipe on a tartan cloth on a Scottish moor, three drone pipes upright, chanter laid across, sheepskin bag in plaid cover, mist rolling across heather, the unmistakable drone of a pibroch",
    "Fiddle":                 "a fiddle on a wooden chair on the porch of a Cape Breton kitchen ceilidh, the lively foot-tapping reel of a céilidh",
    "Shanai":                 "a north-Indian shehnai on a brass tray in a wedding pavilion, conical wooden body with metal bell, double reed, marigolds garlanding, the auspicious bright tone of a wedding processional",
    "Tinkle Bell":            "a brass altar bell on a marble altar in a quiet shrine, dome shape, wooden striker, incense smoke, the tiny crystalline tinkle of a held strike",
    "Agogo":                  "a pair of agogo bells in a Brazilian samba school courtyard, two cone-shaped iron bells fused, hammered surfaces, polyrhythmic carnival energy",
    "Steel Drums":            "a tenor steel pan on a Caribbean beach at sunset, hammered concave pan with chromatic note-fields, palm trees, the bright joyful melody of a calypso",
    "Woodblock":              "a hardwood Chinese temple woodblock on a wooden stand, hollowed body, mallet resting, the hollow knock-knock of a wuyue beat",
    "Taiko Drum":             "a giant ō-daiko taiko drum on a wooden cradle, rope-tensioned cowhide head, jin-haori robes folded nearby, festival lanterns, the deep thunder of a matsuri rhythm",
    "Melodic Tom":            "a row of tom-toms on a 1970s rock drum riser, concert toms tuned in a melodic line, polished maple shells, the descending tom fill of a ballad",
    "Synth Drum":             "a Simmons SDS-V hexagonal electronic drum kit under stage fog, glowing red logos, the descending pewww of an 80s drum hit",
    "Reverse Cymbal":         "a single Zildjian crash cymbal mid-frame on a stand, motion blur suggesting a reversed swell, the inhaled whoosh of a reverse-cymbal swell",
    "Guitar Fret Noise":      "a close-up of a hand sliding along a steel-string fretboard, fingertips on wound strings, the squeaky chirp of a position shift",
    "Breath Noise":           "a close-up of a flute embouchure with breath-spray rendered as soft white particles, the airy hush of a breath-attack",
    "Seashore":               "an empty beach at low tide, foam fringe on wet sand, gulls in the sky, the rolling shhhh of a wave",
    "Bird Tweet":             "a tropical aviary, parrots and finches mid-flight, the trilling chorus of dawn",
    "Telephone Ring":         "a black rotary phone on a 1950s office desk, bell ringing, brass cradle, the brassy double-ring of an old switchboard",
    "Helicopter":             "a Huey UH-1 in slow motion at sunset, rotors blurred, dust kicking up, the chopping wokka-wokka of the blades",
    "Applause":               "a packed concert hall standing ovation seen from the stage, hands blurred mid-clap, the wave of applause filling the air",
    "Gunshot":                "an old western movie poster image of a smoking revolver on a wooden bartop, no people, the cordite curl of a single shot",
  };
  const beat = beats[name] ||
    `the ${name.toLowerCase()} as the protagonist in its natural cultural setting, period-correct details, materials and historical context that evoke its sonic atmosphere`;

  return `${beat}. ${tone}. family: ${family.toLowerCase()} instrument`;
}

async function flux(prompt, seed) {
  const res = await fetch(
    "https://ai.api.nvidia.com/v1/genai/black-forest-labs/flux.1-schnell",
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${NVIDIA_KEY}`,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify({
        prompt,
        cfg_scale: 0,
        width: 1024,
        height: 1024,
        seed,
        steps: 4,
      }),
    },
  );
  if (!res.ok) throw new Error(`flux ${res.status}: ${await res.text()}`);
  const j = await res.json();
  const b64 =
    j.artifacts?.[0]?.base64 ||
    j.image?.replace(/^data:image\/\w+;base64,/, "") ||
    j.b64_json;
  if (!b64) throw new Error("flux: no image");
  return Buffer.from(b64, "base64");
}

// ── main ────────────────────────────────────────────────────────────────
console.log(
  `→ menuband instrument cards · ${programs.length} program(s) → ${OUT}`,
);

for (let idx = 0; idx < programs.length; idx++) {
  const p = programs[idx];
  const name = GM[p];
  const fam = familyFor(p);
  const slug = slugify(name);
  const num = String(p).padStart(3, "0");
  const rawPath = `${RAW}/${num}-${slug}.png`;
  const finalPath = `${OUT}/${num}-${slug}.png`;

  // Generate raw illustration via FLUX (cached on disk).
  // Tiny buffers (<8000B) are FLUX's safety-filter placeholder (solid black);
  // retry with a bumped seed up to MAX_TRIES.
  if (!existsSync(rawPath)) {
    console.log(`  [${idx + 1}/${programs.length}] FLUX #${num} ${name}`);
    const MAX_TRIES = 6;
    let buf = null;
    for (let t = 0; t < MAX_TRIES; t++) {
      try {
        const candidate = await flux(promptFor(name, fam[0]), 7000 + p + t * 9973);
        if (candidate.length >= 8000) { buf = candidate; break; }
        console.warn(`    safety-filter placeholder on try ${t + 1}, retrying…`);
      } catch (err) {
        console.warn(`    flux err try ${t + 1}: ${err.message}`);
      }
    }
    if (!buf) {
      console.error(`  ✗ FLUX failed for ${name} after ${MAX_TRIES} tries`);
      continue;
    }
    writeFileSync(rawPath, buf);
  } else {
    console.log(`  [${idx + 1}/${programs.length}] cached #${num} ${name}`);
  }

  // Composite final card.
  const args = [
    COMPOSER,
    "--raw", rawPath,
    "--out", finalPath,
    "--num", String(p),
    "--name", name,
    "--family", fam[0],
    "--family-rgb", fam[3].join(","),
    "--font-regular", FONT_REG,
    "--font-bold", FONT_BOLD,
    "--url", "aesthetic.computer/menuband",
    "--qr-target", "https://aesthetic.computer/menuband",
  ];
  const r = spawnSync(POP_PY, args, { stdio: "inherit" });
  if (r.status !== 0) {
    console.error(`  ✗ compositor failed for ${name}`);
    continue;
  }
  console.log(`    → ${finalPath}`);
}

console.log(`✓ done — ${OUT}`);
