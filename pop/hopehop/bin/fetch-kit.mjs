#!/usr/bin/env node
// fetch-kit.mjs — pull a trap drum/perc kit from Freesound for hopehop.
//
// Grabs a few candidates per role (kick / snare / clap / closed-hat /
// open-hat / shaker / rim / perc) and writes kit.json mapping each role
// to a chosen local WAV (preview decoded to 44.1k mono by the lib). Re-run
// with --role <role> to refetch just one, or edit kit.json to pin a pick.
//
//   node pop/hopehop/bin/fetch-kit.mjs            # fetch all roles
//   node pop/hopehop/bin/fetch-kit.mjs --list     # show candidates per role
//   node pop/hopehop/bin/fetch-kit.mjs --pick kick=3   # pin role to candidate idx

import { searchSounds, downloadPreview } from "../../lib/freesound.mjs";
import { writeFileSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const KIT_PATH = resolve(HERE, "..", "kit.json");

// role → freesound query + duration filter. Trap-leaning, punchy one-shots.
// classic boom-bap / native-tongues flavor — dusty sampled-record one-shots,
// vintage drum-machine + live hand-perc, NOT modern trap. CC-only.
const ROLES = {
  kick:  { query: "boom bap kick drum",               filter: "duration:[0.1 TO 1.2]" },
  snare: { query: "boom bap snare",                   filter: "duration:[0.1 TO 1.5]" },
  clap:  { query: "909 clap",                         filter: "duration:[0.05 TO 1.5]" },
  hat:   { query: "lofi closed hihat",                filter: "duration:[0.02 TO 0.6]" },
  ohat:  { query: "open hihat analog",                filter: "duration:[0.05 TO 1.2]" },
  shaker:{ query: "shaker percussion one shot",       filter: "duration:[0.05 TO 1]" },
  tamb:  { query: "tambourine one shot",              filter: "duration:[0.05 TO 1.2]" },
  rim:   { query: "rimshot cross stick",              filter: "duration:[0.02 TO 0.8]" },
  perc:  { query: "conga hit live percussion",        filter: "duration:[0.05 TO 1.2]" },
  bongo: { query: "bongo hit",                        filter: "duration:[0.05 TO 1.2]" },
  crash: { query: "vintage crash cymbal",             filter: "duration:[0.5 TO 4]" },
  panther:{ query: "panther",                 filter: "duration:[2 TO 6]" },   // #536331 = clean tonal roar
  panther2:{ query: "jaguar growl roar",      filter: "duration:[1 TO 5]" },   // alt big-cat voice
  scratch:{ query: "turntable scratch vinyl", filter: "duration:[0.1 TO 1.5]" },
  scratch2:{ query: "dj scratch",             filter: "duration:[0.1 TO 1.5]" },
};

const argv = process.argv.slice(2);
const LIST = argv.includes("--list");
// --pick accepts one or more role=idx pairs, comma- or space-separated:
//   --pick clap=2  ·  --pick "kick=0,clap=2,hat=1"  ·  --pick kick=0 clap=2
const pins = {};
for (let i = 0; i < argv.length; i++) {
  if (!argv[i].startsWith("--pick")) continue;
  const specs = [];
  if (argv[i].includes("=")) specs.push(...argv[i].replace(/^--pick=?/, "").split(","));
  while (argv[i + 1] && !argv[i + 1].startsWith("--")) specs.push(...argv[++i].split(","));
  for (const s of specs) { const [r, n] = s.split("="); if (r && n !== undefined) pins[r.trim()] = parseInt(n, 10) || 0; }
}

const kit = existsSync(KIT_PATH) ? JSON.parse(readFileSync(KIT_PATH, "utf8")) : {};

for (const [role, spec] of Object.entries(ROLES)) {
  try {
    const data = await searchSounds({ query: spec.query, filter: spec.filter, pageSize: 5 });
    const results = data.results || [];
    if (LIST) {
      console.log(`\n${role}  (${spec.query})`);
      results.forEach((s, i) => console.log(`  [${i}] ${s.name} · ${s.duration.toFixed(2)}s · ${s.username} · ${s.license.split("/").slice(-3, -1).join("/")}`));
      continue;
    }
    const idx = pins[role] ?? 0;
    const chosen = results[idx] || results[0];
    if (!chosen) { console.warn(`! no result for ${role}`); continue; }
    const mp3 = await downloadPreview(chosen);
    const wav = mp3.replace(/\.mp3$/, ".wav");
    kit[role] = {
      path: existsSync(wav) ? wav : mp3,
      id: chosen.id, name: chosen.name, username: chosen.username,
      license: chosen.license, duration: chosen.duration,
    };
    console.log(`✓ ${role.padEnd(7)} ${chosen.name}  (#${chosen.id} · ${chosen.username})`);
  } catch (e) {
    console.error(`✗ ${role}: ${e.message}`);
  }
}

if (!LIST) {
  writeFileSync(KIT_PATH, JSON.stringify(kit, null, 2) + "\n");
  console.log(`\n✓ wrote ${KIT_PATH} (${Object.keys(kit).length} roles)`);
}
