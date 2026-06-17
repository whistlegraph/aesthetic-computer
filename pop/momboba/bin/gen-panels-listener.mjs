#!/usr/bin/env node
// gen-panels-listener.mjs — generate the 9 LANDSCAPE felt-diorama stills
// for the momabobasheep YouTube listener video (one per movement) via
// gpt-image-2 with jeffrey identity refs. Wide 1536x1024 (16:9-ish), the
// landscape twin of gen-reel.mjs (which makes the 9:16 reel panels).
//
// Prompts: pop/momboba/pano/NN-*.txt — each is PREAMBLE.txt + the beat.
// Output:  pop/momboba/pano/out/NN-*.png (+ ~/Desktop mirror)
//
// Run:  node pop/momboba/bin/gen-panels-listener.mjs           # skips existing
//       node pop/momboba/bin/gen-panels-listener.mjs --force   # regen all
//       node pop/momboba/bin/gen-panels-listener.mjs --only 07 # one movement

import { readFileSync, writeFileSync, readdirSync, mkdirSync, existsSync, copyFileSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const PANO = join(LANE, "pano");
const OUT = join(PANO, "out");
mkdirSync(OUT, { recursive: true });

const FORCE = process.argv.includes("--force");
const _af = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const ONLY = _af("--only");
const SIZE = "1536x1024";                          // wide landscape

// identity refs — same set as gen-illy.mjs / gen-reel.mjs
const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
].filter(existsSync);

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n"))
      if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  }
  throw new Error("OPENAI_API_KEY not found");
}
const apiKey = loadOpenAIKey();
const preamble = readFileSync(join(PANO, "PREAMBLE.txt"), "utf8").trim();

const beats = readdirSync(PANO).filter((f) => /^\d\d-.*\.txt$/.test(f)).sort();
for (const f of beats) {
  if (ONLY && !f.startsWith(ONLY)) continue;
  const png = join(OUT, f.replace(".txt", ".png"));
  if (existsSync(png) && !FORCE) { console.log(`· cached ${basename(png)}`); continue; }
  const prompt = `${preamble}\n\n${readFileSync(join(PANO, f), "utf8").trim()}`;
  console.log(`▸ ${f} · ${SIZE} · ${REFS.length} refs`);
  const t0 = Date.now();
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    fd.append("image[]", new Blob([readFileSync(ref)], { type: "image/jpeg" }), ref.split("/").pop());
  }
  // network hiccups must not kill the batch — retry each beat up to 3×
  let b64 = null;
  for (let attempt = 1; attempt <= 3 && !b64; attempt++) {
    try {
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
      });
      if (!res.ok) { console.error(`  ✗ OpenAI ${res.status} (attempt ${attempt}): ${(await res.text()).slice(0, 200)}`); continue; }
      b64 = (await res.json()).data?.[0]?.b64_json ?? null;
    } catch (err) {
      console.error(`  ✗ ${err.code ?? err.message} (attempt ${attempt})`);
    }
  }
  if (!b64) { console.error(`✗ ${f} failed after 3 attempts — skipping`); continue; }
  writeFileSync(png, Buffer.from(b64, "base64"));
  copyFileSync(png, join(homedir(), "Desktop", `momabobasheep-pano-${basename(png)}`));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s → ${png.replace(REPO + "/", "")}`);
}
console.log("done");
