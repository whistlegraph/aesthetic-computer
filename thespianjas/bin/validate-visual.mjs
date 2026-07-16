#!/usr/bin/env node
// Vision gate for generated Jeffrey assets and rendered performer contact sheets.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const argv = process.argv.slice(2);
const flag = (name, fallback = null) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[i + 1] : fallback;
};
const candidates = argv.filter((arg, i) => !arg.startsWith("--") && (i === 0 || !argv[i - 1].startsWith("--")));
if (!candidates.length) {
  console.error("usage: node thespianjas/bin/validate-visual.mjs frame.png [mouth-open.png eyes-left.png eyes-right.png] [--out report.json]");
  process.exit(2);
}

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = resolve(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
    }
  }
  throw new Error("OPENAI_API_KEY not found");
}

const identity = JSON.parse(readFileSync(resolve(ROOT, "identity.json"), "utf8"));
const refs = identity.references.map((path) => resolve(REPO, path));
const images = [...refs, ...candidates.map((path) => resolve(path))];
for (const path of images) if (!existsSync(path)) throw new Error(`missing image: ${path}`);
const dataUri = (path) => {
  const mime = extname(path).toLowerCase() === ".png" ? "image/png" : "image/jpeg";
  return `data:${mime};base64,${readFileSync(path).toString("base64")}`;
};

const prompt = `The first ${refs.length} images are canonical identity references for Jeffrey Alan Scudder.
The remaining ${candidates.length} images are a rendered avatar validation set, ideally neutral, mouth-open, eyes-left, and eyes-right.
Evaluate strict production readiness for a shoulder-up vertical talking-head video. Do not reward generic attractiveness.
Return JSON only with this exact shape:
{
  "pass": boolean,
  "scores": {
    "identity_likeness": 0-10,
    "face_detail_lod": 0-10,
    "mouth_articulation": 0-10,
    "independent_eye_readability": 0-10,
    "shoulder_up_framing": 0-10,
    "material_naturalness": 0-10,
    "lighting_readability": 0-10,
    "background_depth_texture": 0-10,
    "no_clipping_or_culling": 0-10
  },
  "blockers": ["short concrete issue"],
  "notes": "one concise sentence"
}
Pass only if identity_likeness >= 7, face_detail_lod >= 7, mouth_articulation >= 7, independent_eye_readability >= 7, shoulder_up_framing >= 7, material_naturalness >= 6, lighting_readability >= 6, background_depth_texture >= 6, no_clipping_or_culling >= 8. If the supplied images do not visibly demonstrate mouth opening and independent eye movement, fail those fields.`;

const content = [{ type: "text", text: prompt }];
images.forEach((path, i) => content.push(
  { type: "text", text: i < refs.length ? `canonical reference ${i + 1}` : `candidate frame ${i - refs.length + 1}` },
  { type: "image_url", image_url: { url: dataUri(path), detail: "high" } },
));
const response = await fetch("https://api.openai.com/v1/chat/completions", {
  method: "POST",
  headers: { Authorization: `Bearer ${loadKey()}`, "Content-Type": "application/json" },
  body: JSON.stringify({ model: flag("model", "gpt-4o"), temperature: 0, max_tokens: 900, response_format: { type: "json_object" }, messages: [{ role: "user", content }] }),
});
if (!response.ok) throw new Error(`vision ${response.status}: ${(await response.text()).slice(0, 500)}`);
const body = await response.json();
const report = JSON.parse(body.choices[0].message.content);
const out = flag("out");
if (out) { mkdirSync(dirname(resolve(out)), { recursive: true }); writeFileSync(resolve(out), JSON.stringify(report, null, 2) + "\n"); }
console.log(JSON.stringify(report, null, 2));
process.exit(report.pass ? 0 : 1);

