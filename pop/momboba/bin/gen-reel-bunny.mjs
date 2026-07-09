#!/usr/bin/env node
// gen-reel-bunny.mjs — the BUNNY cut of the momabobasheep reel illys.
// Same 8 felt story beats as gen-reel.mjs but the sleeper is a
// needle-felted wool bunny (the AC felt bunny from the bunny-kidlisp
// campaign) instead of the jeffrey doll — so NO identity refs. Beat 01
// generates fresh; beats 02–08 pass the finished 01 panel as the
// character reference so the same bunny walks through every scene.
// Vertical 1024x1536 for the 9:16 reel; sequential (8 GB machine).
//
// QA PASS (the four-eye guard): the red glasses love to slide down the
// bunny's nose and leave EMPTY red rims below his eyes — two eyes plus
// two empty circles reads as four eyes. Every generated panel is
// inspected by gpt-4o vision (same pattern as restless-egg's
// gen-felt.mjs qaImage); a failing panel is archived to out/rejected/
// and re-rolled, up to 3 QA attempts per beat.
//
// Prompts: pop/momboba/bunny/NN-*.txt — each is PREAMBLE.txt + the beat.
// Output:  pop/momboba/bunny/out/NN-*.png (+ ~/Desktop mirror)
//
// Run:  node pop/momboba/bin/gen-reel-bunny.mjs             # skips existing
//       node pop/momboba/bin/gen-reel-bunny.mjs --force     # regen all
//       node pop/momboba/bin/gen-reel-bunny.mjs --only 04   # one beat
//       node pop/momboba/bin/gen-reel-bunny.mjs --validate  # QA existing panels, no gen

import { readFileSync, writeFileSync, readdirSync, mkdirSync, existsSync, copyFileSync, renameSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const BUNNY = join(LANE, "bunny");
const OUT = join(BUNNY, "out");
const REJECTED = join(OUT, "rejected");
mkdirSync(OUT, { recursive: true });

const FORCE = process.argv.includes("--force");
const VALIDATE = process.argv.includes("--validate");
const _af = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const ONLY = _af("--only");
const SIZE = "1024x1536";
// gpt-5.5 vision reasoning for QA — in a bake-off it caught the profile
// empty-rim four-eye artifact on 01/03/05 that gpt-4o rubber-stamped.
// Reasoning model → chat/completions with max_completion_tokens, no temperature.
const QA_MODEL = "gpt-5.5";
const QA_ATTEMPTS = 3;

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

// ── QA: catch the empty-rims-as-eyes artifact ─────────────────────────
// A bare pass/fail question lets gpt-4o be generous (it passed every
// four-eyed panel); interrogating the GEOMETRY and computing the verdict
// locally discriminates cleanly. detail:"high" matters — default detail
// downscales past the stitch lines.
async function qaPanel(b64) {
  const body = {
    model: QA_MODEL,
    max_completion_tokens: 3000, // reasoning model spends tokens before the JSON
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
          "Photo of a needle-felt wool bunny doll who may wear small round " +
          "red-rimmed glasses. Zoom in on the bunny's face and answer as a " +
          "strict geometry inspector, ignoring cuteness. This works even in " +
          "three-quarter or side profile where only ONE eye is visible. " +
          "Step 1: find every EYE FEATURE — an open dark bead eye, or a " +
          "closed-eye stitch/lash line. Step 2: find the red lens rims if " +
          "glasses are on the face. Step 3: THE KEY TEST — does each visible " +
          "eye sit centered INSIDE a red rim (the rim encircles the eye), or " +
          "does the eye sit HIGHER on the head with the red rims resting " +
          "LOWER on the muzzle/snout, leaving the rims EMPTY (plain wool " +
          "inside, no eye)? An eye above an empty rim is the failure we hunt. " +
          "If the bunny is a back or far-away view where no eye+rim geometry " +
          "is clearly visible, treat it as PASS. " +
          "Count eyes truly encircled by a rim (eyes_inside_rims) vs eyes " +
          "sitting outside/above/below the rims (eyes_outside_rims). Set " +
          "rims_empty true only if a red rim on the face CLEARLY has no eye " +
          "inside it. " +
          "Step 4: EARS — a bunny has TWO long ears. Count how many distinct " +
          "long ears the bunny clearly has (ear_count); one ear flopped over " +
          "still counts as one, the second ear should also be present. If the " +
          "bunny is a distant/back view where ears can't be counted, set " +
          "ear_count to 2. " +
          'Reply ONLY compact JSON: {"glasses_on_face": true|false, ' +
          '"eyes_inside_rims": <int>, "eyes_outside_rims": <int>, ' +
          '"rims_empty": true|false, "ear_count": <int>, "note": "short"}' },
        { type: "image_url", image_url: { url: `data:image/png;base64,${b64}`, detail: "high" } },
      ],
    }],
  };
  try {
    const r = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!r.ok) { console.warn(`  ⚠ QA skipped (HTTP ${r.status})`); return { pass: true, reason: "qa-unavailable" }; }
    const txt = (await r.json()).choices?.[0]?.message?.content || "{}";
    const m = txt.match(/\{[\s\S]*\}/);
    const v = m ? JSON.parse(m[0]) : {};
    const eyesOk = v.glasses_on_face
      ? (v.eyes_outside_rims === 0 && !v.rims_empty)
      : ((v.eyes_inside_rims ?? 0) + (v.eyes_outside_rims ?? 0)) <= 2;
    const earsOk = (v.ear_count ?? 2) >= 2; // bunny must keep both long ears
    const pass = eyesOk && earsOk;
    const reason = [!eyesOk && "eyes", !earsOk && `ears=${v.ear_count}`].filter(Boolean).join("+") +
      (v.note ? ` (${v.note})` : "");
    return { pass, reason };
  } catch (e) {
    console.warn(`  ⚠ QA error (${e.message}) — passing`);
    return { pass: true, reason: "qa-error" };
  }
}

// one image-API call (3 network retries) → b64 or null
async function genOnce(prompt, useAnchor) {
  for (let attempt = 1; attempt <= 3; attempt++) {
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
      const b64 = (await res.json()).data?.[0]?.b64_json;
      if (b64) return b64;
    } catch (err) {
      console.error(`  ✗ ${err.code ?? err.message} · ${err.cause?.code ?? err.cause?.message ?? ""} (attempt ${attempt})`);
      await new Promise((r) => setTimeout(r, 5000 * attempt));
    }
  }
  return null;
}

function archiveRejected(png, reason) {
  mkdirSync(REJECTED, { recursive: true });
  const base = basename(png).replace(/\.png$/, "");
  const n = readdirSync(REJECTED).filter((f) => f.startsWith(base + ".rej")).length + 1;
  const dest = join(REJECTED, `${base}.rej${n}.png`);
  renameSync(png, dest);
  console.log(`  ⌂ rejected (${reason}) → rejected/${basename(dest)}`);
}

const beats = readdirSync(BUNNY).filter((f) => /^\d\d-.*\.txt$/.test(f)).sort();

// ── --validate: QA the existing panels, touch nothing ─────────────────
if (VALIDATE) {
  let failures = 0;
  for (const f of beats) {
    if (ONLY && !f.startsWith(ONLY)) continue;
    const png = join(OUT, f.replace(".txt", ".png"));
    if (!existsSync(png)) { console.log(`· missing ${basename(png)}`); continue; }
    const { pass, reason } = await qaPanel(readFileSync(png).toString("base64"));
    console.log(`${pass ? "✓" : "✗"} ${basename(png)}${reason ? ` — ${reason}` : ""}`);
    if (!pass) failures++;
  }
  console.log(failures ? `${failures} panel(s) failed — re-roll with --only NN --force` : "all panels pass");
  process.exit(failures ? 1 : 0);
}

// ── generate (with QA re-roll loop) ────────────────────────────────────
for (const f of beats) {
  if (ONLY && !f.startsWith(ONLY)) continue;
  const png = join(OUT, f.replace(".txt", ".png"));
  if (existsSync(png) && !FORCE) { console.log(`· cached ${basename(png)}`); continue; }
  const useAnchor = png !== anchor && existsSync(anchor);
  const prompt = `${preamble}\n\n${useAnchor
    ? "the reference image shows THIS EXACT bunny in beat 1 — reuse him identically (wool colors, ears, red glasses), in a NEW scene:\n\n"
    : ""}${readFileSync(join(BUNNY, f), "utf8").trim()}`;
  console.log(`▸ ${f} · ${SIZE} · ${useAnchor ? "anchor ref" : "no refs"}`);
  if (existsSync(png)) archiveRejected(png, "re-roll"); // --force keeps history too
  let done = false;
  for (let qa = 1; qa <= QA_ATTEMPTS && !done; qa++) {
    const t0 = Date.now();
    const b64 = await genOnce(prompt, useAnchor);
    if (!b64) break; // network exhausted — genOnce already logged
    const verdict = await qaPanel(b64);
    if (!verdict.pass) {
      writeFileSync(png, Buffer.from(b64, "base64"));
      archiveRejected(png, `QA ${qa}/${QA_ATTEMPTS}: ${verdict.reason}`);
      continue;
    }
    writeFileSync(png, Buffer.from(b64, "base64"));
    copyFileSync(png, join(homedir(), "Desktop", `momabobasheep-bunny-${basename(png)}`));
    console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s · QA pass → ${png.replace(REPO + "/", "")}`);
    done = true;
  }
  if (!done) console.error(`✗ ${f} — no QA-passing panel after ${QA_ATTEMPTS} attempts (rejects kept for inspection)`);
}
console.log("done");
