#!/usr/bin/env node
// Add per-block descriptions to each block in the channel explaining why it's there.
// Matches by source URL (normalized: lowercased, trailing slash stripped).
//
// Usage: ARENA_TOKEN=... node set-descriptions.mjs

const TOKEN = process.env.ARENA_TOKEN;
const SLUG = "the-score-that-teaches-itself";
if (!TOKEN) { console.error("ARENA_TOKEN missing"); process.exit(1); }

const bySource = {
  // §1 — viral/social kin
  "https://en.wikipedia.org/wiki/renegade_(dance)":
    "Jalaiah Harmon's 14-year-old choreography diffused through TikTok without algorithmic push — learners watched it, learned it, performed it, posted it. The dance is the score.",
  "https://en.wikipedia.org/wiki/harlem_shake_(meme)":
    "A 30-second video format so reproducible it became its own genre. The format itself is the score; the content is just the performance.",
  "https://knowyourmeme.com/memes/how-to-draw-squidward":
    "Stepwise drawing memes — the tutorial is the artwork. Score as self-teaching comedy.",
  "https://en.wikipedia.org/wiki/pictogram":
    "Otl Aicher's Munich '72 pictograms — bodily instructions compressed to a single icon.",
  "https://en.wikipedia.org/wiki/isotype_(picture_language)":
    "Otto Neurath's picture language for statistics — a proposal for a universal graphic score.",

  // §2 — instructional / craft
  "https://en.wikipedia.org/wiki/knitting_abbreviations":
    "A closed symbol set encoding a three-dimensional wearable performance.",
  "https://en.wikipedia.org/wiki/crease_pattern":
    "Origami before folding — the score for a three-dimensional performance on flat paper.",
  "https://langorigami.com":
    "Robert Lang's crease-pattern work — paper-folding as programming.",
  "https://en.wikipedia.org/wiki/sewing_pattern":
    "Flat cut-outs that, executed correctly, produce a body. A garment is an assembled score.",
  "https://en.wikipedia.org/wiki/ikea":
    "Image-only assembly instructions that crossed language barriers by refusing language.",
  "https://en.wikipedia.org/wiki/lego":
    "Instruction booklets where each page is a diff from the previous state — score as rewrite.",
  "https://en.wikipedia.org/wiki/julia_child":
    "Recipes as performance scores: measurement, sequence, expected outcome, tasting as feedback.",
  "https://en.wikipedia.org/wiki/japanese_tea_ceremony":
    "Temae — every gesture choreographed and transmitted by watching, not by diagram.",

  // §3 — body / movement notation
  "https://en.wikipedia.org/wiki/labanotation":
    "Rudolf Laban's 1928 system — the West's most serious attempt at a staff-notation for the body.",
  "https://en.wikipedia.org/wiki/eshkol%e2%80%93wachman_movement_notation":
    "Noa Eshkol's system: the body as angles and arcs, notated on a grid.",
  "https://en.wikipedia.org/wiki/benesh_movement_notation":
    "The Royal Ballet's notation — entire company choreography on paper.",
  "https://en.wikipedia.org/wiki/kata":
    "Forms passed down by doing. No diagram, but the form itself is the score, memorized in bodies.",
  "https://en.wikipedia.org/wiki/american_football_plays":
    "X's and O's that collapse into eleven bodies moving in time.",

  // §4 — sport as score
  "https://en.wikipedia.org/wiki/skateboarding":
    "A 'line' is a spatial score — stringing tricks across architecture in a single read.",
  "https://en.wikipedia.org/wiki/dogtown_and_z-boys":
    "Empty pools as performance scores — swimming-pool topography repurposed as notation.",
  "https://en.wikipedia.org/wiki/surf_break":
    "Break diagrams: where to paddle, where to cut, where it closes out. A weather-dependent score.",
  "https://en.wikipedia.org/wiki/yardage_book":
    "A golfer's private notation — terrain, slope, club, intent. Personal score as tool.",
  "https://en.wikipedia.org/wiki/parkour":
    "A traceur's line is a reading of urban architecture through the body.",

  // §5 — vernacular / folk notation
  "https://en.wikipedia.org/wiki/shape_note":
    "Four shapes, one staff — a self-teaching notation for congregational singing. The score teaches you how to read it.",
  "https://en.wikipedia.org/wiki/sacred_harp":
    "The shape-note repertoire in practice: the tune is sung in solfège before the words — reading IS learning.",
  "https://en.wikipedia.org/wiki/tablature":
    "Finger position, not pitch. The diagram of where the hand goes; the sound is the consequence.",
  "https://en.wikipedia.org/wiki/neume":
    "The earliest Western notation — gestural marks for pitch shape, before pitch was quantized.",
  "https://en.wikipedia.org/wiki/jianpu":
    "Chinese numbered notation — reads like a score, works like a score, isn't staff notation.",
  "https://en.wikipedia.org/wiki/gongche_notation":
    "Traditional Chinese scales encoded in characters — an alphabet of pitch.",
  "https://en.wikipedia.org/wiki/sargam":
    "Seven syllables: Sa Re Ga Ma Pa Dha Ni. A score you sing as you read it.",
  "https://en.wikipedia.org/wiki/gahu":
    "West African drum notation and the oral-diagrammatic pedagogy behind it.",

  // §6 — Fluxus & event scores
  "https://en.wikipedia.org/wiki/grapefruit_(book)":
    "Yoko Ono's book of instructions — scores so small they fit on a card.",
  "https://en.wikipedia.org/wiki/water_yam":
    "George Brecht's event cards — single-instruction scores the size of a business card.",
  "https://en.wikipedia.org/wiki/dick_higgins":
    "Danger Music and the idea of scores that can't be performed safely.",
  "https://en.wikipedia.org/wiki/an_anthology_of_chance_operations":
    "La Monte Young's 1963 anthology — the origin point of Fluxus event scores.",
  "https://en.wikipedia.org/wiki/alison_knowles":
    "Make A Salad (1962): the salad is the score. The performance is eating.",

  // §7 — 20th-century graphic scores
  "https://en.wikipedia.org/wiki/treatise_(cardew)":
    "Cardew's 193-page graphic score — no instructions, no interpretation guide. The reader is the score.",
  "https://en.wikipedia.org/wiki/fontana_mix":
    "Cage's transparent overlay system — the first reconfigurable score.",
  "https://en.wikipedia.org/wiki/aria_(cage)":
    "Colored curves for vocal register, shapes for technique. A score you read like a weather map.",
  "https://en.wikipedia.org/wiki/concert_for_piano_and_orchestra_(cage)":
    "Eighty-four score 'solos' to be played in any order, for any duration, in any combination.",
  "https://en.wikipedia.org/wiki/variations_i":
    "Transparent sheets, dots and lines — the score is a configuration, not a sequence.",
  "https://en.wikipedia.org/wiki/december_1952":
    "Earle Brown's single page — rectangles in two dimensions, read in any direction.",
  "https://en.wikipedia.org/wiki/morton_feldman":
    "Feldman's graph pieces — intensity and register on a grid. Pitch becomes a choice, not an instruction.",
  "https://en.wikipedia.org/wiki/christian_wolff_(composer)":
    "Prose instructions as scores — Burdocks, For One, Two or Three People. Text as instrument.",
  "https://en.wikipedia.org/wiki/artikulation_(ligeti)":
    "Wehinger's listening score — drawn AFTER Ligeti's electronic piece, to teach you how to hear it.",
  "https://en.wikipedia.org/wiki/metastaseis":
    "Xenakis drafted string glissandi like architectural sections. The score is geometry.",
  "https://en.wikipedia.org/wiki/sonic_meditations":
    "Pauline Oliveros's text scores for group listening — attention itself as the performance.",
  "https://en.wikipedia.org/wiki/i_am_sitting_in_a_room":
    "Lucier's one paragraph of text — an entire composition.",
  "https://en.wikipedia.org/wiki/in_c":
    "Terry Riley: 53 phrases on one page, any ensemble, 45–90 minutes. A score infinitely rehearsable.",
  "https://en.wikipedia.org/wiki/composition_1960":
    "La Monte Young's text scores, including 'Draw a straight line and follow it.'",
  "https://www.moma.org/s/ge/curated_ge/styles/list_ge/artists/1191":
    "John Cage's 1969 anthology Notations — the graphic-score genre's first self-portrait.",
  "https://en.wikipedia.org/wiki/notations_21":
    "Theresa Sauer's 2009 follow-up — the tradition's next generation.",

  // §8 — computational / card-sized kin
  "https://aesthetic.computer/prompt":
    "The prompt is the instrument. Type a piece name, play it. A one-line score for the entire AC system.",
  "https://aesthetic.computer/notepat":
    "Keyboard as chromatic instrument. Pressure-sensitive keys as notational markup on QWERTY.",
  "https://en.wikipedia.org/wiki/bitsy":
    "Adam Le Doux's tile-based game tool — the syntax itself is the score.",
  "https://www.lexaloffle.com/pico-8.php":
    "A fantasy console where the 32K / 128×128 constraint produces the form.",
  "https://www.dwitter.net":
    "140 characters of JavaScript → an animation. Source code that fits on a card.",
  "https://en.wikipedia.org/wiki/demoscene":
    "Intros in kilobytes — source code as a compressed performance.",
  "https://en.wikipedia.org/wiki/inform_7":
    "Interactive fiction that reads like plain English. The source IS the play.",

  // §10 — whistlegraph
  "https://www.tiktok.com/@whistlegraph":
    "2.6 million followers. The distribution channel of a form that teaches itself.",
  "https://aesthetic.computer/whistlegraph":
    "The practice site: draw while singing, the result IS the score. Record, learn, perform.",
  "https://feralfile.com/artists/whistlegraph":
    "Forty-five whistlegraph editions on Feral File, 2023.",
  "https://rhizome.org":
    "Rhizome / New Museum 2022 commission: a 22-minute chalk performance in a public gallery.",
};

const norm = (u) => (u || "").toLowerCase().replace(/\/$/, "");

const url = `https://api.are.na/v2/channels/${SLUG}/contents?per=100`;
const res = await fetch(url, { headers: { Authorization: `Bearer ${TOKEN}` } });
const data = await res.json();
const blocks = data.contents;
console.log(`fetched ${blocks.length} blocks`);

let ok = 0, skip = 0, fail = 0;
const missing = [];
for (let i = 0; i < blocks.length; i++) {
  const b = blocks[i];
  if (b.class === "Text") { skip++; continue; }
  const key = norm(b.source && b.source.url);
  const desc = bySource[key];
  if (!desc) {
    missing.push({ id: b.id, source: b.source && b.source.url });
    continue;
  }
  const r = await fetch(`https://api.are.na/v2/blocks/${b.id}`, {
    method: "PUT",
    headers: { Authorization: `Bearer ${TOKEN}`, "Content-Type": "application/json" },
    body: JSON.stringify({ description: desc }),
  });
  if (r.ok || r.status === 204) {
    ok++;
    process.stdout.write(`  [${String(i + 1).padStart(2, "0")}/${blocks.length}] ok ${b.source.url.slice(0, 65)}\n`);
  } else {
    fail++;
    const t = await r.text();
    process.stdout.write(`  [${String(i + 1).padStart(2, "0")}/${blocks.length}] FAIL ${r.status} ${b.source.url}: ${t.slice(0, 120)}\n`);
  }
  await new Promise((r) => setTimeout(r, 300));
}
console.log(`\ndone: ${ok} updated, ${skip} text-blocks-skipped, ${fail} failed, ${missing.length} no-mapping`);
if (missing.length) {
  console.log("no-mapping (need to add to map):");
  for (const m of missing) console.log(" ", m);
}
