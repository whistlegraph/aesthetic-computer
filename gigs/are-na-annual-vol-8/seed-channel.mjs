#!/usr/bin/env node
// Seed blocks into the Are.na channel "self-teaching-scores".
// Blocks are added in reading-bottom → reading-top order so whistlegraph lands
// first on the channel page.
//
// Usage: ARENA_TOKEN=... node seed-channel.mjs

const TOKEN = process.env.ARENA_TOKEN;
const SLUG = "self-teaching-scores";
if (!TOKEN) { console.error("ARENA_TOKEN missing"); process.exit(1); }

const blocks = [
  // §1 — Viral / social kin (bottom of channel)
  { type: "link", v: "https://en.wikipedia.org/wiki/Renegade_(dance)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Harlem_Shake_(meme)" },
  { type: "link", v: "https://knowyourmeme.com/memes/how-to-draw-squidward" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Pictogram" },
  { type: "link", v: "https://en.wikipedia.org/wiki/ISOTYPE_(picture_language)" },

  // §2 — Instructional / craft
  { type: "link", v: "https://en.wikipedia.org/wiki/Knitting_abbreviations" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Crease_pattern" },
  { type: "link", v: "https://langorigami.com/" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Sewing_pattern" },
  { type: "link", v: "https://en.wikipedia.org/wiki/IKEA" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Lego" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Julia_Child" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Japanese_tea_ceremony" },

  // §3 — Body / movement notation
  { type: "link", v: "https://en.wikipedia.org/wiki/Labanotation" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Eshkol%E2%80%93Wachman_Movement_Notation" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Benesh_Movement_Notation" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Kata" },
  { type: "link", v: "https://en.wikipedia.org/wiki/American_football_plays" },

  // §4 — Sport-as-score / line
  { type: "link", v: "https://en.wikipedia.org/wiki/Skateboarding" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Dogtown_and_Z-Boys" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Surf_break" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Yardage_book" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Parkour" },

  // §5 — Vernacular / folk notation
  { type: "link", v: "https://en.wikipedia.org/wiki/Shape_note" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Sacred_Harp" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Tablature" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Neume" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Jianpu" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Gongche_notation" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Sargam" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Gahu" },

  // §6 — Fluxus & event scores
  { type: "link", v: "https://en.wikipedia.org/wiki/Grapefruit_(book)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Water_Yam" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Dick_Higgins" },
  { type: "link", v: "https://en.wikipedia.org/wiki/An_Anthology_of_Chance_Operations" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Alison_Knowles" },
  { type: "text", v: "“Draw a line. Follow it.” — Yoko Ono, *Line Piece* (1964)." },

  // §7 — 20th-century graphic scores (the canon)
  { type: "link", v: "https://en.wikipedia.org/wiki/Treatise_(Cardew)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Fontana_Mix" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Aria_(Cage)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Concert_for_Piano_and_Orchestra_(Cage)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Variations_I" },
  { type: "link", v: "https://en.wikipedia.org/wiki/December_1952" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Morton_Feldman" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Christian_Wolff_(composer)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Artikulation_(Ligeti)" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Metastaseis" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Sonic_Meditations" },
  { type: "link", v: "https://en.wikipedia.org/wiki/I_Am_Sitting_in_a_Room" },
  { type: "link", v: "https://en.wikipedia.org/wiki/In_C" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Composition_1960" },
  { type: "link", v: "https://www.moma.org/s/ge/curated_ge/styles/list_ge/artists/1191/" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Notations_21" },

  // §8 — Computational / card-sized kin
  { type: "link", v: "https://aesthetic.computer/prompt" },
  { type: "link", v: "https://aesthetic.computer/notepat" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Bitsy" },
  { type: "link", v: "https://www.lexaloffle.com/pico-8.php" },
  { type: "link", v: "https://www.dwitter.net/" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Demoscene" },
  { type: "link", v: "https://en.wikipedia.org/wiki/Inform_7" },

  // §9 — Framing text blocks (near top)
  { type: "text", v: "A drawing that constructs the performance it depicts." },
  { type: "text", v: "The score teaches you how to play it." },
  { type: "text", v: "Reproducibility, not algorithmic promotion, explains its viral spread." },

  // §10 — Whistlegraph (lands on top)
  { type: "link", v: "https://www.tiktok.com/@whistlegraph" },
  { type: "link", v: "https://aesthetic.computer/whistlegraph" },
  { type: "link", v: "https://feralfile.com/artists/whistlegraph" },
  { type: "link", v: "https://rhizome.org/" },
  { type: "text", v: "Every mark is a sung syllable. Watching, learning, and performing collapse into a single gesture." },
];

const url = `https://api.are.na/v2/channels/${SLUG}/blocks`;

let ok = 0, fail = 0;
const failures = [];

for (let i = 0; i < blocks.length; i++) {
  const b = blocks[i];
  const body = b.type === "text" ? { content: b.v } : { source: b.v };
  try {
    const res = await fetch(url, {
      method: "POST",
      headers: {
        "Authorization": `Bearer ${TOKEN}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify(body),
    });
    if (res.ok) {
      ok++;
      process.stdout.write(`  [${String(i + 1).padStart(2, "0")}/${blocks.length}] ${b.type} ok (${b.v.slice(0, 60)})\n`);
    } else {
      fail++;
      const txt = await res.text();
      failures.push({ i: i + 1, v: b.v, status: res.status, txt: txt.slice(0, 160) });
      process.stdout.write(`  [${String(i + 1).padStart(2, "0")}/${blocks.length}] FAIL ${res.status} (${b.v.slice(0, 60)})\n`);
    }
  } catch (e) {
    fail++;
    failures.push({ i: i + 1, v: b.v, err: String(e) });
    process.stdout.write(`  [${String(i + 1).padStart(2, "0")}/${blocks.length}] ERROR ${e.message} (${b.v.slice(0, 60)})\n`);
  }
  await new Promise((r) => setTimeout(r, 300));
}

console.log(`\ndone: ${ok} ok, ${fail} fail`);
if (failures.length) {
  console.log("failures:");
  for (const f of failures) console.log(" ", JSON.stringify(f));
}
