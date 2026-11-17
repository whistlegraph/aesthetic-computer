#!/usr/bin/env node
// chillwave/bin/gen-motion-helpabeach.mjs — the helpabeach MOTION pass: each
// section animated from its storyboard panel (the piece's own illy), on the
// best image-to-video models fal.ai carries.
//
//   node pop/chillwave/bin/gen-motion-helpabeach.mjs --dry-run            # prompts + durations + price
//   node pop/chillwave/bin/gen-motion-helpabeach.mjs --only "drift 1"     # one section
//   node pop/chillwave/bin/gen-motion-helpabeach.mjs --model seedance25   # h3max (default) | seedance25 | seedance25-flash
//   node pop/chillwave/bin/gen-motion-helpabeach.mjs --seconds 6          # a short test instead of the whole section
//   node pop/chillwave/bin/gen-motion-helpabeach.mjs --force              # re-roll: a new take, the old one stays
//   node pop/chillwave/bin/gen-motion-helpabeach.mjs --assemble           # cut the newest take of each section to the track
//
// Models (Artificial Analysis image-to-video board, silent, 2026-11-17):
// Seedance 2.5 and MiniMax H3 Max sit level at the top of what has an API.
// Measured here from the balance: H3 Max about $0.06/s at 768P (the listed
// $0.0375/s is not what 768P bills) and ~20 s a shot, 15 s at most; Seedance
// 2.5 about $0.46/s at 720p and ~8 min a shot, 30 s at most.
// Takes are never deleted: out/motion/helpabeach-<i><a|b>-<model>.vN.mp4 (a
// section longer than the model's longest clip is two shots, the second
// picking up from the first one's last frame). The newest N wins at
// --assemble unless out/motion/takes.json names another file for the section.
// Panels are portrait (2:3), so the cut is a portrait reel.

import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateModelShot, uploadToFalStorage } from "../../lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "../..");
const OUT = resolve(HERE, "../out/motion");
const MANIFEST = resolve(POP, "../system/public/aesthetic.computer/disks/pop/helpabeach.json");
const CDN = "https://assets.aesthetic.computer";

const seedance = (s) => ({ prompt: s.prompt, image_url: s.image, duration: String(s.seconds), resolution: "720p", generate_audio: false });
const MODELS = {
  h3max: {
    endpoint: "minimax/h3-max/image-to-video", max: 15, perSec: 0.0625,
    input: (s) => ({ prompt: s.prompt, image_url: s.image, duration: s.seconds, resolution: "768P" }),
  },
  seedance25: { endpoint: "bytedance/seedance-2.5/us/image-to-video", max: 30, perSec: 0.46, input: seedance },
  "seedance25-flash": { endpoint: "bytedance/seedance-2.5/flash/image-to-video", max: 30, perSec: null, input: seedance },
};

const MEDIUM = `The image is a hand-drawn colored-pencil and gouache illustration on warm cream paper, and it stays one: the hatching, paper grain and soft edges hold in every frame, never turning photographic or smooth. Daylight from the tall windows only. Calm, unhurried, deadpan — slow chillwave pacing, small ordinary movements. The same two people, the same clothes, the same room throughout; nobody new enters. The lime-green Rhizome Health logo projected on the brick wall stays put and legible. No text, captions or interface appear.`;

// The look lives in the panels; these say only what moves. Two entries are
// the two halves of a section: one shot if the model can hold the whole
// length, otherwise a shot each.
const SHOTS = {
  "tide-in": [
    `The visiting artist walks in from the left across the pale wood floor, tote on his shoulder, looking around the bright loft clinic. Leaves of the plants stir as the door settles behind him. The camera drifts slowly forward with him.`,
    `He slows and stops near the exam bench. The clinician by her rolling cart looks up and gives a small friendly wave; he lifts a hand back and sets his tote down. The camera settles.`,
  ],
  "drift 1": [
    `Sign-in. He sits on the exam bench with the chartreuse laptop open on his lap and keeps typing, barely looking up. The clinician beside him turns a page on her clipboard and points to a line. A very slow push-in.`,
    `He signs the clipboard one-handed without closing the laptop, then goes straight back to typing. She nods, makes a note, and clips her pen back on the board.`,
  ],
  "swell 1": [
    `The extraction begins. He leans back on the bench, relaxed. The clinician gently adjusts the soft translucent conduit clipped to his forearm, and small glowing green glyphs and coloured pixels start to travel along it, one after another, into the clear glass vessel on the cart, which begins to glow. He watches with mild curiosity. The camera holds.`,
  ],
  "deep-current": [
    `The peak builds. The stream of green glowing bits thickens and surges along the conduit; the vessel's glow swells, throwing green light on both faces and the brick wall. He stiffens and grips the bench. The camera creeps closer.`,
    `The glow flickers in uneven pulses, the room's daylight seeming to dim with each one. The clinician steadies the conduit with both hands, concentrating; he squeezes his eyes shut, then opens them, breathing through it.`,
  ],
  "drift 2": [
    `It calms. The flicker settles into a slow even glow in the full vessel on the cart. He exhales, shoulders dropping. The clinician unclips the conduit from his forearm.`,
    `She coils the conduit and sets it on the cart beside the softly glowing vessel. He opens the chartreuse laptop on his knee and starts typing again. Quiet relief.`,
  ],
  undertow: [
    `The checkup. He sits on the bench typing while the clinician leans in and checks him over — a hand on his shoulder, a look at his forearm. A slow drift to the right.`,
    `She glances at the softly glowing vessel beside her, then writes on her chart. He nods without stopping typing; she nods back.`,
  ],
  "swell 2": [
    `Payment. The clinician counts out paper bills one by one into his open hand; he fans them, pleased, the chartreuse laptop still balanced on his knee. She smiles and closes the cash envelope. He tucks the money into his shirt pocket.`,
  ],
  "tide-out": [
    `The goodbye. Both standing, he tucks the chartreuse laptop under his arm and they reach out and shake hands warmly — two slow pumps.`,
    `They let go and exchange a nod and a small smile. Light from the tall windows shifts slightly as a cloud passes. He turns a little toward the door.`,
  ],
  ebb: [
    `He leaves. Laptop held against his chest, he walks toward the tall glass door on the left and pushes it open, giving a small wave back over his shoulder.`,
    `He steps through and is gone. The door swings slowly shut. In the room the clinician turns to her cart and the softly glowing vessel. The camera holds, the room emptying.`,
  ],
};

const args = process.argv.slice(2);
const flag = (name) => args.includes(`--${name}`);
const value = (name, fallback) => { const i = args.indexOf(`--${name}`); return i >= 0 ? args[i + 1] : fallback; };
const modelName = value("model", "h3max");
const model = MODELS[modelName];
if (!model) throw new Error(`unknown --model ${modelName} (${Object.keys(MODELS).join(", ")})`);

const manifest = JSON.parse(readFileSync(MANIFEST, "utf8"));
const sections = manifest.sections.map((s, i) => {
  const end = manifest.sections[i + 1]?.t ?? manifest.duration;
  return { i, name: s.name, start: s.t, length: end - s.t, image: `${CDN}${s.illy.replace(/^\/assets/, "")}` };
});

const version = (file) => Number(file.match(/\.v(\d+)\.mp4$/)[1]);
const takes = (id) => existsSync(OUT)
  ? readdirSync(OUT).filter((f) => f.startsWith(`helpabeach-${id}-`) && f.endsWith(".mp4")).sort((a, b) => version(a) - version(b))
  : [];

function run(cmd, cmdArgs) {
  const r = spawnSync(cmd, cmdArgs, { stdio: ["ignore", "pipe", "pipe"] });
  if (r.status !== 0) throw new Error(`${cmd} failed: ${r.stderr.toString().slice(-400)}`);
  return r.stdout.toString();
}

async function generate() {
  const only = value("only");
  const short = Number(value("seconds", 0));
  const wanted = sections.filter((s) => !only || s.name === only);
  if (!wanted.length) throw new Error(`no section named "${only}"`);
  mkdirSync(OUT, { recursive: true });
  let total = 0;
  const jobs = [];
  for (const s of wanted) {
    const halves = SHOTS[s.name];
    if (!halves) throw new Error(`no shot written for "${s.name}"`);
    const split = !short && s.length > model.max && halves.length > 1;
    const parts = split
      ? [{ tag: "a", prompt: halves[0], seconds: Math.ceil(s.length / 2) }, { tag: "b", prompt: halves[1], seconds: Math.ceil(s.length / 2) }]
      : [{ tag: "", prompt: halves.join(" Then: "), seconds: short || Math.min(model.max, Math.ceil(s.length)) }];
    let chain = Promise.resolve(s.image); // each part starts from the frame the last one ended on
    for (const part of parts) {
      const id = `${s.i}${part.tag}`;
      const have = takes(id).filter((f) => f.includes(`-${modelName}.`));
      const skip = have.length > 0 && !flag("force");
      const price = model.perSec ? `$${(model.perSec * part.seconds).toFixed(2)}` : "token-priced";
      if (model.perSec && !skip) total += model.perSec * part.seconds;
      console.log(`${id.padEnd(2)} ${s.name.padEnd(13)} ${s.length.toFixed(1)}s section → ${part.seconds}s on ${modelName} · ${price}${skip ? " · have a take, skipping" : ""}`);
      if (flag("dry-run")) { console.log(`    ${part.prompt}\n`); continue; }
      const n = 1 + Math.max(0, ...takes(id).map(version));
      const outPath = skip ? `${OUT}/${have.at(-1)}` : `${OUT}/helpabeach-${id}-${modelName}.v${n}.mp4`;
      const last = part === parts.at(-1);
      chain = chain.then(async (image) => {
        if (!image) return null; // the part before this one failed
        if (!skip) {
          const r = await generateModelShot({
            endpoint: model.endpoint, outPath, label: `${id} ${s.name}`,
            input: model.input({ prompt: `${part.prompt}\n\n${MEDIUM}`, image, seconds: part.seconds }),
          });
          console.log(r.ok ? `  ✓ ${id} ${s.name} → ${outPath}` : `  ✗ ${id} ${s.name}: ${r.error}`);
          if (!r.ok) return null;
        }
        if (last) return outPath;
        const frame = outPath.replace(/\.mp4$/, ".last.png");
        run("ffmpeg", ["-y", "-v", "error", "-sseof", "-0.1", "-i", outPath, "-frames:v", "1", "-update", "1", frame]);
        return uploadToFalStorage(frame, () => {});
      });
    }
    jobs.push(chain);
  }
  await Promise.all(jobs);
  if (model.perSec) console.log(`total ${flag("dry-run") ? "would be" : "submitted for"} about $${total.toFixed(2)} (a refused shot is not billed)`);
}

// Trim (or, for a take shorter than its slot, slow) each take to its exact
// length, join them with hard cuts, and lay the track under it.
function assemble() {
  const picks = existsSync(`${OUT}/takes.json`) ? JSON.parse(readFileSync(`${OUT}/takes.json`, "utf8")) : {};
  const parts = [];
  for (const s of sections) {
    // One take for the whole section, or an a and a b that share it.
    const newest = (id) => takes(id).filter((f) => f.includes(`-${modelName}.`)).at(-1); // the cut is one model's takes
    const whole = picks[s.name] || newest(s.i);
    const cuts = whole
      ? [{ file: whole, length: s.length, tag: "" }]
      : [{ file: newest(`${s.i}a`), length: s.length / 2, tag: "a" }, { file: newest(`${s.i}b`), length: s.length / 2, tag: "b" }];
    for (const cut of cuts) {
      if (!cut.file) throw new Error(`no take for "${s.name}"${cut.tag ? ` part ${cut.tag}` : ""} yet`);
      const src = `${OUT}/${cut.file}`;
      const have = Number(run("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", src]));
      const stretch = have < cut.length ? cut.length / have : 1; // slow a short take rather than freeze its last frame
      const part = `${OUT}/part-${s.i}${cut.tag}.mp4`;
      run("ffmpeg", ["-y", "-i", src, "-an", "-vf", `setpts=${stretch.toFixed(5)}*PTS,scale=720:1080:force_original_aspect_ratio=increase,crop=720:1080,fps=24`,
        "-t", cut.length.toFixed(3), "-c:v", "libx264", "-crf", "18", "-pix_fmt", "yuv420p", part]);
      parts.push(part);
      console.log(`${s.i}${cut.tag.padEnd(1)} ${s.name.padEnd(13)} ${cut.file}${stretch > 1 ? ` (slowed ×${stretch.toFixed(2)})` : ""}`);
    }
  }
  const list = `${OUT}/parts.txt`;
  writeFileSync(list, parts.map((p) => `file '${p}'`).join("\n"));
  const audio = `${OUT}/helpabeach.mp3`;
  if (!existsSync(audio)) run("curl", ["-sL", "-o", audio, `${CDN}/pop/helpabeach.mp3`]);
  const final = resolve(OUT, "../helpabeach-motion.mp4");
  run("ffmpeg", ["-y", "-f", "concat", "-safe", "0", "-i", list, "-i", audio, "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest", "-movflags", "+faststart", final]);
  console.log(`→ ${final}`);
}

if (flag("assemble")) assemble(); else await generate();
