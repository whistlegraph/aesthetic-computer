#!/usr/bin/env node
// gen-deck.mjs — the 4 Restless Egg concept-film stills with JEFFREY's REAL,
// RECOGNIZABLE likeness (front-lit, face visible) via gpt-image-2
// /v1/images/edits, conditioned on the canonical jeffrey refs
// (marketing/lib/jeffrey-refs.mjs). No silhouette compromise — Seedance is
// gone, so his face comes back. Beat 2 also conditions on a real captured
// notepat UI frame (refs/ac-native-notepat.png) so the screen is the real
// instrument, not invented. See brand-brief.md.
//
// Usage: node gen-deck.mjs [--force]

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join, extname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { jeffreyRefs } from "../../../marketing/lib/jeffrey-refs.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const v = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(v, "utf8").split("\n"))
    if (line.startsWith("OPENAI_API_KEY="))
      return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("no OPENAI_API_KEY");
}

// one cohesive painterly look, jeffrey FRONT-LIT and CLEARLY HIM.
const STYLE =
  "Painterly digital illustration, cinematic 16:9, warm and moody, rich color, soft glow and gentle " +
  "film grain — a hand-painted indie-animated-film concept-art feel. The MAN in the scene is the specific " +
  "person in the reference photos: late thirties, warm brown eyes, brown stubble, medium brown hair — " +
  "match his face, hair, beard and build so it is unmistakably HIM, FACE VISIBLE and lit (front or " +
  "three-quarter view, NOT a silhouette, NOT from behind). Aesthetic Computer palette: warm cream, deep " +
  "violet, citrus-green terminal glow, coral accents. No readable text, no logos, no wordmarks. ";

// optional real notepat UI capture for beat 2's screen content.
const NOTEPAT_REF = join(HERE, "refs", "ac-native-notepat.png");

const BEATS = [
  {
    out: "still-1-painter.png",
    refs: [],
    prompt: STYLE +
      "BEAT ONE — THE PAINTER. A cluttered Los Angeles studio at night lit by a warm desk lamp and a faint " +
      "citrus-green terminal glow. He sits at the desk in a painter's posture, FACING three-quarters toward " +
      "us so his face catches the lamp light, a brush in one hand — but the 'canvas' he works is a glowing " +
      "laptop screen full of colored light. A quiet stack of old, dead, closed laptops beside him. Intimate, " +
      "nocturnal — the origin of an idea.",
  },
  {
    out: "still-2-awakening.png",
    refs: existsSync(NOTEPAT_REF) ? [NOTEPAT_REF] : [],
    prompt: STYLE +
      "BEAT TWO — THE AWAKENING. Close on him, his FACE lit warm and bright by a screen-bloom, an awed " +
      "half-smile, as his hands flip open a small salvaged ThinkPad and slot in a USB stick. The screen " +
      "blooms a vivid grid of glowing colored musical note-tiles and a bright waveform across the top " +
      "(match the supplied notepat UI reference for the on-screen content). Light and color flood out of the " +
      "machine onto his face and into the dark room. A fifty-dollar laptop becoming a glowing instrument.",
  },
  {
    out: "still-3-commons.png",
    refs: [],
    prompt: STYLE +
      "BEAT THREE — THE COMMONS. A vast pull-back into a dark constellation where thousands of tiny glowing " +
      "screens blink on, each a small distinct piece of colorful generative art, threads of light connecting " +
      "them. He stands in the lower foreground turned three-quarters so we still catch his lit face in " +
      "profile-to-front, looking up at the galaxy of a creative community. Awe and scale.",
  },
  {
    out: "still-4-invitation.png",
    refs: [],
    prompt: STYLE +
      "BEAT FOUR — THE INVITATION. A single refurbished laptop alone on a warm, softly-spotlit dark stage, " +
      "glowing like an instrument waiting to be played. He has just set it down and turns BACK TOWARD US, his " +
      "lit face catching the warm rim light, a hopeful half-smile — an offer, not a retreat. Cinematic, " +
      "reverent.",
  },
];

const apiKey = loadKey();
const force = process.argv.includes("--force");
const JREFS = jeffreyRefs();
console.log(`jeffrey refs: ${JREFS.length}`);

async function callWithRetry(fd, maxAttempts = 4) {
  let lastErr = null;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const ac = new AbortController();
    const timer = setTimeout(() => ac.abort(), 10 * 60 * 1000);
    try {
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST",
        headers: { Authorization: `Bearer ${apiKey}` },
        body: fd,
        signal: ac.signal,
      });
      clearTimeout(timer);
      return res;
    } catch (e) {
      clearTimeout(timer);
      lastErr = e;
      const cause = e?.cause?.code || e?.code || e?.name;
      const transient = ["ETIMEDOUT", "EPIPE", "ECONNRESET", "UND_ERR_SOCKET", "AbortError"].includes(cause);
      if (attempt < maxAttempts && transient) {
        const backoff = Math.min(60, 3 * Math.pow(2, attempt - 1));
        console.error(`  ⚠ attempt ${attempt} (${cause}); retry in ${backoff}s…`);
        await new Promise((r) => setTimeout(r, backoff * 1000));
        continue;
      }
      throw e;
    }
  }
  throw lastErr;
}

function appendImage(fd, path) {
  const buf = readFileSync(path);
  const ext = extname(path).toLowerCase();
  const mime = ext === ".png" ? "image/png" : ext === ".webp" ? "image/webp" : "image/jpeg";
  fd.append("image[]", new Blob([buf], { type: mime }), basename(path));
}

for (const beat of BEATS) {
  const out = join(HERE, beat.out);
  if (existsSync(out) && !force) { console.log(`· cached ${beat.out}`); continue; }
  const refs = [...JREFS, ...beat.refs];
  const t0 = Date.now();
  process.stdout.write(`▸ ${beat.out} (${refs.length} refs) … `);
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", beat.prompt);
  fd.append("size", "1536x1024");
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of refs) appendImage(fd, ref);
  const res = await callWithRetry(fd);
  if (!res.ok) { console.log(`FAIL ${res.status}: ${(await res.text()).slice(0, 200)}`); continue; }
  const j = await res.json();
  const b64 = j.data?.[0]?.b64_json;
  if (!b64) { console.log("no image"); continue; }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
console.log("done");
