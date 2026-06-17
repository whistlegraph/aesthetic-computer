#!/usr/bin/env node
// momboba/video/build.mjs — ShotWizard driver for the momabobasheep LISTENER.
//
// ShotWizard calls this with:
//   node build.mjs --shot <id>    gen a shot's still (gpt-image-2) + clip (Seedance)
//   node build.mjs --assemble     cycle every DONE clip → plate → chrome → final
//
// A GEN shot needs a `stillPrompt` (the gpt-image-2 scene, felt-diorama style,
// PREAMBLE auto-prepended) and a `prompt` (the Seedance MOTION). The still
// renders to ../pano/out/<id>.png, the clip to ../out/motion/<id>.mp4, and the
// board.json is updated (source/clip/status) so the wizard reflects it.

import { readFileSync, writeFileSync, existsSync, mkdirSync, copyFileSync } from "node:fs";
import { resolve, dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";
import { generateShot } from "../../lib/fal-seedance.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));      // pop/momboba/video
const LANE = resolve(HERE, "..");                         // pop/momboba
const REPO = resolve(LANE, "..", "..");
const BIN = join(LANE, "bin");
const BOARD = join(HERE, "board.json");
const PANO = join(LANE, "pano");
const PANO_OUT = join(PANO, "out");
const MOTION = join(LANE, "out", "motion");
mkdirSync(PANO_OUT, { recursive: true });
mkdirSync(MOTION, { recursive: true });

const args = process.argv.slice(2);
const opt = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const relTo = (from, p) => relative(from, p);

// ── identity refs + OpenAI still gen (from gen-panels-listener.mjs) ────────
const SHOOT = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCH = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const REFS = [
  `${SHOOT}/jeffery-av--07.jpg`, `${SHOOT}/jeffery-av--01.jpg`, `${SHOOT}/jeffery-av--04.jpg`,
  `${ARCH}/2018-12-02_Bq4ckGFFNtW.jpg`, `${ARCH}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCH}/2021-07-10_CRI095Vl7AO_1.jpg`, `${ARCH}/2025-01-25_DFQ2lHPzN_W.jpg`,
].filter(existsSync);

function openAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault))
    for (const line of readFileSync(vault, "utf8").split("\n"))
      if (line.startsWith("OPENAI_API_KEY=")) return line.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("OPENAI_API_KEY not found");
}

async function genStill(scene, outPng, extraRefs = []) {
  const preamble = existsSync(join(PANO, "PREAMBLE.txt")) ? readFileSync(join(PANO, "PREAMBLE.txt"), "utf8").trim() : "";
  const prompt = preamble ? `${preamble}\n\n${scene}` : scene;
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", "1536x1024");
  fd.append("quality", "high");
  fd.append("n", "1");
  // identity refs + any per-shot extra refs (e.g. prior scenes fed forward).
  const allRefs = [...REFS, ...extraRefs.filter(existsSync)];
  const mime = (p) => (/\.png$/i.test(p) ? "image/png" : "image/jpeg");
  for (const ref of allRefs) fd.append("image[]", new Blob([readFileSync(ref)], { type: mime(ref) }), ref.split("/").pop());
  for (let attempt = 1; attempt <= 3; attempt++) {
    try {
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${openAIKey()}` }, body: fd,
      });
      if (!res.ok) { console.error(`  ✗ OpenAI ${res.status} (try ${attempt}): ${(await res.text()).slice(0, 160)}`); continue; }
      const b64 = (await res.json()).data?.[0]?.b64_json;
      if (b64) { writeFileSync(outPng, Buffer.from(b64, "base64")); return true; }
    } catch (e) { console.error(`  ✗ ${e.code ?? e.message} (try ${attempt})`); }
  }
  return false;
}

// ── board I/O ──────────────────────────────────────────────────────────────
const board = JSON.parse(readFileSync(BOARD, "utf8"));
const save = () => writeFileSync(BOARD, JSON.stringify(board, null, 2));

// ── --shot <id>: gen still (if needed) + clip ────────────────────────────────
const shotId = opt("--shot");
if (shotId) {
  const shot = board.shots.find((s) => s.id === shotId);
  if (!shot) { console.error(`✗ no shot "${shotId}"`); process.exit(1); }

  let stillPath = shot.source ? resolve(HERE, shot.source) : join(PANO_OUT, `${shotId}.png`);
  if (!existsSync(stillPath)) {
    if (!shot.stillPrompt) { console.error(`✗ ${shotId}: no still and no stillPrompt`); process.exit(1); }
    const extraRefs = (shot.refs || []).map((r) => resolve(HERE, r));
    console.log(`▸ still ${shotId} · gpt-image-2 · ${REFS.length}+${extraRefs.length} refs`);
    if (!await genStill(shot.stillPrompt, stillPath, extraRefs)) { console.error(`✗ still failed`); process.exit(1); }
    try { copyFileSync(stillPath, join(homedir(), "Desktop", `momabobasheep-${shotId}.png`)); } catch {}
    shot.source = relTo(HERE, stillPath); save();
    console.log(`✓ still → ${relTo(REPO, stillPath)}`);
  }

  const clipPath = join(MOTION, `${shotId}.mp4`);
  console.log(`▸ clip ${shotId} · Seedance 2.0 fast · 5s`);
  const r = await generateShot({
    image: stillPath, prompt: shot.prompt || "gentle, slow, looping felt motion; tiny drift",
    duration: "5", ratio: "16:9", resolution: "720p", tier: "fast",
    outPath: clipPath, label: shotId, log: (m) => console.log(m),
  });
  if (!r.ok) { console.error(`✗ ${shotId}: ${r.error}`); process.exit(1); }
  shot.clip = relTo(HERE, clipPath); shot.status = "done"; save();
  console.log(`✓ clip → ${relTo(REPO, clipPath)}`);
  process.exit(0);
}

// ── --assemble: cycle every done clip → plate → chrome → final ──────────────
if (args.includes("--assemble")) {
  const clips = board.shots
    .filter((s) => s.clip && existsSync(resolve(HERE, s.clip)))
    .map((s) => resolve(HERE, s.clip));
  if (!clips.length) { console.error("✗ no done clips to assemble"); process.exit(1); }
  console.log(`▸ assembling ${clips.length} clips (cycling crops + crossfade)`);
  const plate = join(LANE, "out", "listener-plate.mp4");
  let r = spawnSync("node", [join(BIN, "assemble-listener.mjs"), "--clips", clips.join(","), "--out", plate],
    { stdio: "inherit" });
  if (r.status !== 0) process.exit(r.status ?? 1);
  console.log(`▸ chroming → final + Desktop`);
  r = spawnSync("node", [join(BIN, "chrome-listener.mjs"), "--plate", plate, "--ss", "1", "--desktop"],
    { stdio: "inherit" });
  process.exit(r.status ?? 0);
}

console.error("usage: build.mjs --shot <id> | --assemble");
process.exit(1);
