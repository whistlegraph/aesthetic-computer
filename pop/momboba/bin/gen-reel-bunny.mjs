#!/usr/bin/env node
// gen-reel-bunny.mjs — the BUNNY cut of the momabobasheep reel illys.
// Same 8 felt story beats as gen-reel.mjs but the sleeper is a
// needle-felted wool bunny (the AC felt bunny from the bunny-kidlisp
// campaign) instead of the jeffrey doll — so NO identity refs. Beat 01
// generates fresh; beats 02–08 pass the finished 01 panel as the
// character reference so the same bunny walks through every scene.
// Vertical 1024x1536 for the 9:16 reel; sequential (8 GB machine).
//
// Prompts: pop/momboba/bunny/NN-*.txt — each is PREAMBLE.txt + the beat.
// Output:  pop/momboba/bunny/out/NN-*.png (+ ~/Desktop mirror)
//
// Run:  node pop/momboba/bin/gen-reel-bunny.mjs            # skips existing
//       node pop/momboba/bin/gen-reel-bunny.mjs --force    # regen all
//       node pop/momboba/bin/gen-reel-bunny.mjs --only 04  # one beat

import { readFileSync, writeFileSync, readdirSync, mkdirSync, existsSync, copyFileSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const BUNNY = join(LANE, "bunny");
const OUT = join(BUNNY, "out");
mkdirSync(OUT, { recursive: true });

const FORCE = process.argv.includes("--force");
const _af = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const ONLY = _af("--only");
const SIZE = "1024x1536";

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
const preamble = readFileSync(join(BUNNY, "PREAMBLE.txt"), "utf8").trim();
const anchor = join(OUT, "01-arrival.png"); // character ref for beats 02–08

const beats = readdirSync(BUNNY).filter((f) => /^\d\d-.*\.txt$/.test(f)).sort();
for (const f of beats) {
  if (ONLY && !f.startsWith(ONLY)) continue;
  const png = join(OUT, f.replace(".txt", ".png"));
  if (existsSync(png) && !FORCE) { console.log(`· cached ${basename(png)}`); continue; }
  const useAnchor = png !== anchor && existsSync(anchor);
  const prompt = `${preamble}\n\n${useAnchor
    ? "the reference image shows THIS EXACT bunny in beat 1 — reuse him identically (wool colors, ears, red glasses), in a NEW scene:\n\n"
    : ""}${readFileSync(join(BUNNY, f), "utf8").trim()}`;
  console.log(`▸ ${f} · ${SIZE} · ${useAnchor ? "anchor ref" : "no refs"}`);
  const t0 = Date.now();
  // network hiccups must not kill the batch — retry each beat up to 3×
  let b64 = null;
  for (let attempt = 1; attempt <= 3 && !b64; attempt++) {
    try {
      let res;
      if (useAnchor) {
        const fd = new FormData();
        fd.append("model", "gpt-image-2");
        fd.append("prompt", prompt);
        fd.append("size", SIZE);
        fd.append("quality", "high");
        fd.append("n", "1");
        fd.append("image[]", new Blob([readFileSync(anchor)], { type: "image/png" }), "01-arrival.png");
        res = await fetch("https://api.openai.com/v1/images/edits", {
          method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
        });
      } else {
        res = await fetch("https://api.openai.com/v1/images/generations", {
          method: "POST",
          headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
          body: JSON.stringify({ model: "gpt-image-2", prompt, size: SIZE, quality: "high", n: 1 }),
        });
      }
      if (!res.ok) { console.error(`  ✗ OpenAI ${res.status} (attempt ${attempt}): ${(await res.text()).slice(0, 200)}`); continue; }
      b64 = (await res.json()).data?.[0]?.b64_json ?? null;
    } catch (err) {
      console.error(`  ✗ ${err.code ?? err.message} · ${err.cause?.code ?? err.cause?.message ?? ""} (attempt ${attempt})`);
      await new Promise((r) => setTimeout(r, 5000 * attempt));
    }
  }
  if (!b64) { console.error(`✗ ${f} failed after 3 attempts — skipping`); continue; }
  writeFileSync(png, Buffer.from(b64, "base64"));
  copyFileSync(png, join(homedir(), "Desktop", `momabobasheep-bunny-${basename(png)}`));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s → ${png.replace(REPO + "/", "")}`);
}
console.log("done");
