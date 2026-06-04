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
const ROLES = {
  kick:  { query: "trap kick punchy 808",     filter: "duration:[0.1 TO 2]" },
  snare: { query: "trap snare",               filter: "duration:[0.1 TO 2]" },
  clap:  { query: "hand clap",                filter: "duration:[0.05 TO 1.5]" },
  hat:   { query: "closed hihat",             filter: "duration:[0.02 TO 0.6]" },
  ohat:  { query: "open hi hat",              filter: "duration:[0.05 TO 1.2]" },
  shaker:{ query: "shaker percussion one shot", filter: "duration:[0.05 TO 1]" },
  rim:   { query: "rim click percussion",     filter: "duration:[0.02 TO 0.8]" },
  perc:  { query: "percussion conga hit",     filter: "duration:[0.05 TO 1.2]" },
  crash: { query: "crash cymbal",             filter: "duration:[0.5 TO 4]" },
  panther:{ query: "panther",                 filter: "duration:[2 TO 6]" },   // #536331 = clean tonal roar
  panther2:{ query: "jaguar growl roar",      filter: "duration:[1 TO 5]" },   // alt big-cat voice
  scratch:{ query: "turntable scratch vinyl", filter: "duration:[0.1 TO 1.5]" },
  scratch2:{ query: "dj scratch",             filter: "duration:[0.1 TO 1.5]" },
};

const argv = process.argv.slice(2);
const LIST = argv.includes("--list");
const pickArg = argv.find((a) => a.startsWith("--pick"));
const pins = {};
if (pickArg) { const v = argv[argv.indexOf(pickArg) + 1] || ""; const [r, i] = v.split("="); if (r) pins[r] = parseInt(i, 10) || 0; }

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
