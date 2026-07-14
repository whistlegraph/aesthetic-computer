#!/usr/bin/env node
// gen-capur.mjs — CaPUR (Calisthenics Puerto Rico) logo material studies with
// gpt-image-2, using the CaPUR wordmark as the image reference. Same technique
// as marketing/podcast/bin/gen-pals.mjs, tuned for a wordmark (legibility QC).
//
// Usage: node marketing/capur/gen-capur.mjs [--only chrome,neon] [--force] [--size 1024x1024]
//   → marketing/capur/out/<slug>.png (+ contact sheet)

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const OUT = resolve(HERE, "out");
mkdirSync(OUT, { recursive: true });
const REF = resolve(HERE, "ref", "CaPUR.png");

const argv = process.argv.slice(2);
const flag = (k) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : null; };
const FORCE = argv.includes("--force");
const SIZE = flag("size") || "1024x1024";
const ONLY = flag("only") ? new Set(flag("only").split(",")) : null;

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const envFile = resolve(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  const line = readFileSync(envFile, "utf8").split("\n").find((l) => l.startsWith("OPENAI_API_KEY="));
  return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
}
const KEY = loadKey();

const MARK = "the 'CaPUR' wordmark from the reference image — the bold stylized letters C, a, P, U, R together with the little calisthenics athlete figure doing a bar hold — reproduced faithfully and legibly as the single central emblem, its exact letterforms and the figure preserved and correctly spelled";
const RULES = "Square composition, the emblem centered with a generous calm margin so a round or square crop never clips it. One coherent studio object, no collage. Keep the CaPUR lettering crisp, sharp, and correctly spelled. No other text, no other logos, no watermark. No motion blur; crisp high detail throughout.";

const THEMES = [
  { slug: "chrome", prompt: `A mirror-polished liquid-chrome sculpture of ${MARK}, high-gloss reflective steel like a gym pull-up bar, smooth highlights, on a cool graphite-to-silver gradient background under studio light. Sleek, athletic, premium. ${RULES}` },
  { slug: "neon", prompt: `A glowing neon-sign version of ${MARK}, bright tropical-green and magenta neon tubes with a warm ambient glow and subtle reflection, mounted on a dark textured concrete gym wall at night. Energetic, nightlife. ${RULES}` },
  { slug: "concrete", prompt: `${MARK} cast as a crisp raised relief in smooth pale concrete, side-lit by warm tropical daylight so the letters and figure throw soft shadows, on a minimal sunlit stucco wall. Brutalist, sunny, Puerto Rican. ${RULES}` },
  { slug: "sunset", prompt: `A glossy enamel-badge version of ${MARK} in vivid palm-green and coral, embossed with a soft metallic edge, floating over a warm tropical sunset gradient (coral, gold, deep teal) with gentle bloom. Vibrant, beachy, uplifting. ${RULES}` },
];
const PICK = THEMES.filter((t) => !ONLY || ONLY.has(t.slug));

async function render(theme, out) {
  for (let a = 1; a <= 4; a++) {
    try {
      const fd = new FormData();
      fd.append("model", "gpt-image-2");
      fd.append("prompt", theme.prompt);
      fd.append("size", SIZE);
      fd.append("quality", "high");
      fd.append("n", "1");
      fd.append("image[]", new Blob([readFileSync(REF)], { type: "image/png" }), "CaPUR.png");
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${KEY}` }, body: fd,
        signal: AbortSignal.timeout(180000),
      });
      if (!res.ok) { const t = await res.text(); if (res.status >= 500 && a < 4) throw new Error(`HTTP ${res.status}`); throw new Error(`${res.status}: ${t.slice(0, 160)}`); }
      const j = await res.json();
      writeFileSync(out, Buffer.from(j.data[0].b64_json, "base64"));
      return true;
    } catch (e) {
      if (a < 4) { process.stdout.write(` retry${a}`); await new Promise((r) => setTimeout(r, 1500 * a)); }
      else { console.error(`\n    ${theme.slug} render: ${e.message}`); return false; }
    }
  }
}

const QC_MODEL = process.env.CAPUR_QC_MODEL || "gpt-4o";
async function qc(out) {
  if (process.env.CAPUR_NO_QC) return { ok: true };
  const b64 = readFileSync(out).toString("base64");
  const rubric = `You are grading a generated brand-logo image for "CaPUR" (a calisthenics brand). Answer ONLY strict JSON {"ok":boolean,"reason":"<=12 words"}:\n- The word must read exactly C-a-P-U-R, correctly spelled and legible. Reject if letters are garbled, misspelled, wrong count, or unreadable.\n- The emblem fills roughly the central 55-70% of the frame, centered, even margin. Reject if it bleeds edge-to-edge or is tiny.\n- Strong contrast, crisp, clearly readable. Reject if faint, blurry, or low-contrast.\n- Reject extra text, watermarks, other logos, or collage.\nBe strict about the spelling.`;
  try {
    const res = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST", headers: { Authorization: `Bearer ${KEY}`, "Content-Type": "application/json" },
      signal: AbortSignal.timeout(90000),
      body: JSON.stringify({ model: QC_MODEL, temperature: 0, max_tokens: 60, messages: [{ role: "user", content: [
        { type: "text", text: rubric },
        { type: "image_url", image_url: { url: `data:image/png;base64,${b64}`, detail: "low" } },
      ] }] }),
    });
    const j = await res.json();
    const m = (j.choices?.[0]?.message?.content ?? "").match(/\{[\s\S]*\}/);
    const v = JSON.parse(m ? m[0] : "{}");
    return { ok: !!v.ok, reason: String(v.reason || "").slice(0, 60) };
  } catch (e) { return { ok: true, reason: "qc unavailable" }; }
}

for (const theme of PICK) {
  const out = resolve(OUT, `${theme.slug}.png`);
  if (existsSync(out) && !FORCE) { console.log(`  · ${theme.slug} cached`); continue; }
  process.stdout.write(`  → ${theme.slug} …`);
  for (let g = 1; g <= 3; g++) {
    if (!(await render(theme, out))) { process.stdout.write(" ✗\n"); break; }
    const v = await qc(out);
    if (v.ok) { process.stdout.write(` ✓${g > 1 ? ` (try ${g})` : ""}\n`); break; }
    process.stdout.write(` ⤾ ${v.reason}${g < 3 ? " — regen" : " — kept last"}\n`);
    if (g < 3) process.stdout.write(`  → ${theme.slug} …`);
  }
}

// contact sheet
try {
  const pngs = PICK.map((t) => resolve(OUT, `${t.slug}.png`)).filter(existsSync);
  if (pngs.length) {
    const inputs = pngs.flatMap((p) => ["-i", p]);
    execFileSync("/opt/homebrew/bin/ffmpeg", ["-y", "-loglevel", "error", ...inputs,
      "-filter_complex", `${pngs.map((_, i) => `[${i}]scale=420:-1[s${i}]`).join(";")};${pngs.map((_, i) => `[s${i}]`).join("")}hstack=inputs=${pngs.length}`,
      resolve(OUT, "contact.png")]);
    console.log(`▸ contact sheet: marketing/capur/out/contact.png`);
  }
} catch (e) { console.error("contact sheet:", e.message); }
