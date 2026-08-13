#!/usr/bin/env node
// maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs — panels for
// SHAKEOUT, the femrag++ music video. The recurring AESTHETIC COMPUTER
// felt BUNNY throws himself a one-bunny dance: a wool sound-system party
// that starts as a lone shuffle on a rag-time parlor rug, tears open into
// drum and bass, and lands in a Jamaican dancehall of stacked felt
// speakers when the donks drop at 1:33.
//
// Character law is inherited VERBATIM from pop/momboba/bunny/PREAMBLE.txt
// (two long ears always, red glasses ride HIGH with an eye inside each
// lens, rims never empty). His EARS carry the rhythm here the way they
// carried sleepiness in the momboba cut — this lane's whole arc is in
// how hard the ears are flying.
//
// Medium: needle-felt wool (momboba's lane medium — the bunny is felt,
// and felt is FUZZY, which is the brief). 16:9 landscape for the video.
//
// Anchor pattern (from gen-sections-fluttabap360-bunny-reel.mjs): the
// first bunny beat `runway` generates with NO refs; every later beat
// passes the finished runway PNG as the character-consistency ref so the
// same bunny dances through all twelve sections. Face-visible beats are
// gated by a gpt-5.5 vision QA (two ears, eyes inside the red rims),
// re-rolled up to 3× on fail, rejects archived to out/rejected/.
//
// Output: pop/maytrax/out/femrag-plusplus-yt-sec-<i>-<name>.png
//   (the name the motion pipeline's pop-panel convention expects)
//
// Usage:
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs --list
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs --only runway
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs          # all, cached
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs --force

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync, renameSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { NEEDLE_FELT_WOOL, FRAMING_YT_LANDSCAPE } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const SLUG = "femrag-plusplus";
const OUT = `${LANE}/out`;
const REJECTED = `${OUT}/rejected`;
const SIZE = "1536x1024"; // 16:9-ish landscape
const QA_MODEL = "gpt-5.5";
const QA_ATTEMPTS = 3;

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const FORCE = flags.force === true;
const ONLY = flags.only ? String(flags.only).split(",") : null;
mkdirSync(OUT, { recursive: true });

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

// ── the character — inherited from pop/momboba/bunny/PREAMBLE.txt ────────
const BUNNY =
`THE BUNNY — a single needle-felted wool BUNNY, the recurring AESTHETIC COMPUTER character: a wonderfully woolly little rabbit with an off-white body, soft-grey ear tips and paws, TWO LONG felt ears, a little pink felt nose, tiny dark glossy bead eyes, and a round puff of a tail. he wears tiny round soft RED felt glasses — his signature. he stands and dances upright on his hind legs. the SAME bunny in every scene: identical wool colors, same two long ears, same red glasses.
HIS EARS ARE THE RHYTHM — in this story the ears carry the beat the way a dancer's hair does: hanging soft and heavy when the groove is quiet, swinging in wide arcs as it builds, flying and whipping out sideways at full extension when the drums drop, and finally settling. BOTH of his two long ears are ALWAYS present — even when one flies wide, the second stays visible. NEVER draw him with only one ear.
GLASSES GEOMETRY (CRITICAL): the bunny has exactly TWO eyes. a bunny's eyes sit HIGH and WIDE on the head — so the red glasses ride HIGH on his face, up over the eyes, NOT down low on the muzzle or snout. each eye — open bead eye or closed happy lash-line — is centered INSIDE its own round red lens, one eye per lens, the lens ringing the eye like real worn glasses. the red rims are NEVER empty. NEVER draw an eye ABOVE the rims with the empty lenses sitting lower on the snout (one real eye plus two empty red circles reads as extra eyes — the single worst failure).`;

// The world: one continuous felt room that transforms with the music.
const WORLD =
`THE ROOM — one continuous hand-felted room, roughly a 40 cm diorama, that TRANSFORMS with the music across the story: it begins as a hushed little wool parlor (a worn felt rag-rug, a stitched upright piano against the back wall, a dim wool lamp) and ends as a full JAMAICAN SOUND-SYSTEM DANCE — a leaning stack of enormous hand-felted speaker cabinets, wool cones bulging, tangled yarn cables across the floor. the SAME room throughout: the same rug, the same back wall, the same little lamp, progressively taken over by the speaker stack. everything is wool: the speakers, the cables, the light itself feels felted.`;

const LIGHT =
`LIGHT — diegetic and physically motivated, always. act one is warm dim lamp-light on wool. after the first drop, the room gains a real hand-felted lighting rig: a few small warm bulbs and one cool blue-green wool-diffused lamp, casting soft double shadows across the fibers. the dancehall section is lit warmer and lower — amber and deep green, the way a bass-heavy room feels. NO neon, NO glow filters, NO laser beams, NO light shafts drawn as objects.`;

const PALETTE =
`PALETTE — off-white and soft-grey wool bunny; round red glasses; oatmeal and dusty-rose rag-rug; warm brown felted wood; charcoal-grey speaker cabinets with oatmeal cones; brass-yellow bulb light; one cool teal accent lamp; amber and deep-green dancehall wash. warm, soft, matted, unsaturated — the colors of dyed wool, never of plastic.`;

const AVOID =
`AVOID — any 3d-render, clay, plastic, or smooth plush-toy look (this is real matted needle-felted wool); ANY human person anywhere (the bunny is the only character); a second bunny, a mirror, or any doubled figure; only one ear on the bunny; empty red glasses rims; any readable text, wordmark, signage, band name, or logo of any kind; any screen, phone, tablet, laptop, or DJ controller with a display; neon, glow filters, lens flares, laser beams, or light shafts drawn as solid objects; motion-blur streaks or drawn speed-lines; the character looking at or acknowledging the camera; any real-world brand.`;

const FRAME_NOTE =
`${FRAMING_YT_LANDSCAPE}
HONOR THE SHOT DIRECTIVE in this beat exactly: a WIDE shot lets the whole room breathe with the bunny comfortably within it; a CLOSE-UP crops in tight and FILLS the frame with the detail (do NOT pull back to a wide); a LOW-ANGLE puts the camera down on the rug looking up.`;

// ── the twelve beats — one per struct section ────────────────────────────
// Names + order MUST match pop/maytrax/out/femrag-plusplus.struct.json.
const BEATS = [
  {
    name: "runway", faceVisible: true,
    scene:
`WIDE, low and quiet. The felt bunny stands alone in the middle of the worn oatmeal rag-rug in the dim wool parlor, seen three-quarters from the front. He has just started to move — weight settled onto one hind paw, the other heel lifted a few millimeters off the rug, one front paw curled loosely at his chest. His two long ears hang heavy and straight down, only just beginning to sway. Behind him the stitched felt upright piano waits against the wall and the little wool lamp glows warm. Nothing else in the room has moved yet. The moment before a dance.`,
  },
  {
    name: "groove", faceVisible: true,
    scene:
`WIDE. The bunny has found the groove — a small contained two-step shuffle on the rug, hips turned slightly, one hind paw planted and the other sliding a few centimeters across the wool, both front paws up at rib height in a loose easy bounce. Both long ears now swing in a slow matched arc out to one side, lifted maybe thirty degrees off vertical. Bead eyes half-lidded, content. The rug has begun to rumple under his planted paw. Same parlor, same lamp — but a first small felted speaker cabinet has appeared beside the piano, its oatmeal wool cone facing him.`,
  },
  {
    name: "buildup1", faceVisible: true,
    scene:
`CLOSE-UP on the bunny from the chest up, filling the frame — individual wool fibers and needle pokes clearly resolved. He is winding up: chin tucked, shoulders drawn in tight, both front paws pulled to his chest, coiled. His two long ears are swept back behind his head and held there under tension, straining, at their tautest of the whole story — the frame just before release. Bead eyes wide inside the red lenses. Behind him the room is dark and out of focus except a single hot rim of bulb light along his fuzz. Every fiber on his silhouette lifts as a soft halo.`,
  },
  {
    name: "drop1a", faceVisible: true,
    scene:
`WIDE, LOW-ANGLE from down on the rug looking up. THE DROP — the bunny is airborne in a full shakeout: both hind paws off the rug by several centimeters, body twisted mid-turn, front paws flung wide and open. His two long ears fly straight out sideways at full extension, horizontal, one to each side. The rag-rug is visibly rucked and kicked up behind him where he launched. The room has torn open into the sound system: three hand-felted speaker cabinets now stacked crookedly against the back wall, wool cones bulging outward, yarn cables snaking across the floor around his feet. Warm bulbs and one teal wool lamp throw soft double shadows.`,
  },
  {
    name: "drop1b", faceVisible: true,
    scene:
`CLOSE-UP, filling the frame — the bunny's head and shoulders in full motion, single fibers and matted clumps resolving. He is deep in it: head thrown back, mouth-line open in delight, bead eyes squeezed into happy closed lash-lines centered inside their red lenses. Both long ears are mid-whip, caught in a wide S-curve as they swing past his head, wool wisps lifting off them. Sweat is not a thing here — but loose fiber is: a scatter of tiny wool wisps floats free in the bulb light around him. Behind, out of focus, the charcoal wall of speaker cabinets.`,
  },
  {
    name: "breakdown", faceVisible: true,
    scene:
`WIDE and still. The breakdown — the bunny has dropped into a slow heavy half-time sway, standing near the largest felted speaker cabinet with one front paw laid flat against its oatmeal wool cone, feeling the bass through his paw. Body low, knees bent, rocking. His two long ears hang low and heavy again, swinging in a slow lazy pendulum, out of phase with each other. The lighting has cooled: the teal wool lamp dominates, the warm bulbs are down to embers. The rug is thoroughly rumpled. Quiet, enormous, patient.`,
  },
  {
    name: "buildup2", faceVisible: true,
    scene:
`CLOSE-UP, tight on the bunny's face and upper body, filling the frame. The second wind-up, steeper than the first: he is crouched right down with his chin nearly at his own knees, both front paws pressed to the rug, every muscle of the little wool body gathered. His two long ears are pinned flat back along his spine, held under maximum tension. One bead eye visible inside its red lens, fixed and wide. A single hot bulb rim-lights his whole fuzzy outline against near-blackness. The most compressed frame in the film.`,
  },
  {
    name: "drop2a", faceVisible: true,
    scene:
`WIDE, LOW-ANGLE. THE SECOND DROP — bigger than the first. The bunny is at the very top of a leap, higher off the rug than before, body arched back, all four paws flung out in a star, tail puff visible. His two long ears whip up and back in matching arcs above his head. The rag-rug is kicked into a fold beneath him and a small cloud of loose wool wisps hangs in the air where he launched. The speaker stack has grown to a leaning wall of five cabinets, cones bulging, cables everywhere. Warm bulbs and teal lamp at full strength, hard double shadows across the fibers.`,
  },
  {
    name: "drop2b", faceVisible: true,
    scene:
`CLOSE-UP on the bunny's hind paws and the rug, filling the frame — this is the only beat where his face is not the subject. Wool fibers at maximum resolution: the matted pile of the rag-rug, individual dyed strands, the soft-grey felted pads of his hind paws mid-stamp, one paw planted deep enough to compress the rug's pile, the other lifting away with wisps trailing. A yarn cable curls past. Above and out of focus, the blurred oatmeal wool of his body still in motion.`,
  },
  {
    name: "ragga-a", faceVisible: true,
    scene:
`WIDE. THE DANCEHALL — the room has fully become a Jamaican sound-system dance. The stack is now enormous: seven or eight hand-felted speaker cabinets leaning in a crooked tower to the ceiling, huge oatmeal wool cones, thick yarn cables looped across the floor. The lighting has gone warm and low — deep amber and forest green. The bunny dances in the classic skanking posture: leaning forward, weight low, one shoulder dropped, both front paws swinging loosely across his body, one hind paw kicked back. His two long ears swing in a lazy, deliberately behind-the-beat arc — loose and cool, not frantic. He is having the time of his life.`,
  },
  {
    name: "ragga-b", faceVisible: true,
    scene:
`CLOSE-UP, low, filling the frame — the bunny and the mouth of the biggest felted speaker cone behind him. He has his back half-turned to the cabinet, leaning into it, one long ear flung forward across his own face by the swing and the other trailing behind, bead eyes happy inside their red lenses. The enormous oatmeal wool cone behind him is visibly pushed outward at its center — the wool physically bulging with a bass note — and loose fibers around its rim lift away from the surface. Deep amber light, forest-green shadows in the fuzz.`,
  },
  {
    name: "outro", faceVisible: true,
    scene:
`WIDE and warm. The end. The bunny has stopped dancing and stands still at the center of the thoroughly wrecked rug, breathing, one front paw resting on his own chest. His two long ears hang all the way down, completely spent, wool visibly fluffed and disheveled all over him — he has genuinely shaken his fuzz out. Loose wisps have settled across the rug like snow. The speaker stack looms quiet and dark behind him, cones at rest. Only the little original wool lamp is still lit, back to its warm act-one glow. The same parlor, after.`,
  },
];

if (flags.list) {
  BEATS.forEach((b, i) => console.log(`${i}\t${b.name}`));
  process.exit(0);
}

const panelPath = (i, name) => `${OUT}/${SLUG}-yt-sec-${i}-${name}.png`;
const ANCHOR = panelPath(0, "runway");

function build(beat) {
  return [
    `SHAKEOUT — one beat from a needle-felted music video.`,
    NEEDLE_FELT_WOOL,
    FRAME_NOTE,
    BUNNY,
    WORLD,
    LIGHT,
    PALETTE,
    `THIS BEAT — ${beat.scene}`,
    AVOID,
  ].join("\n\n");
}

const apiKey = loadOpenAIKey();

async function genOnce(promptText, refs, useEdit, label) {
  const MAX_TRIES = 4;
  for (let attempt = 1; attempt <= MAX_TRIES; attempt++) {
    const t0 = Date.now();
    try {
      let res;
      if (useEdit) {
        const fd = new FormData();
        fd.append("model", "gpt-image-2");
        fd.append("prompt", promptText);
        fd.append("size", SIZE);
        fd.append("quality", "high");
        fd.append("n", "1");
        for (const ref of refs) {
          const buf = readFileSync(ref);
          fd.append("image[]", new Blob([buf], { type: "image/png" }), basename(ref));
        }
        res = await fetch("https://api.openai.com/v1/images/edits", {
          method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
        });
      } else {
        res = await fetch("https://api.openai.com/v1/images/generations", {
          method: "POST",
          headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
          body: JSON.stringify({ model: "gpt-image-2", prompt: promptText, size: SIZE, quality: "high", n: 1 }),
        });
      }
      if (!res.ok) {
        const err = await res.text();
        const transient = res.status === 429 || res.status >= 500;
        if (transient && attempt < MAX_TRIES) {
          const wait = 4000 * attempt;
          console.warn(`  ⚠ OpenAI ${res.status} (${label}) — retry ${attempt} in ${wait / 1000}s`);
          await sleep(wait); continue;
        }
        console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 400)}`);
        return null;
      }
      const json = await res.json();
      const b64 = json.data?.[0]?.b64_json;
      if (!b64) { console.error(`✗ no image (${label})`); return null; }
      console.log(`  · gen ${((Date.now() - t0) / 1000).toFixed(1)}s`);
      return b64;
    } catch (e) {
      const cause = e?.cause?.code || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry in ${wait / 1000}s`);
        await sleep(wait); continue;
      }
      console.error(`✗ network fail (${label}): ${cause}`);
      return null;
    }
  }
  return null;
}

// Vision gate: two ears, an eye inside each red lens, exactly one bunny.
async function qaPanel(b64) {
  const ask =
`You are checking one frame of a needle-felt stop-motion music video for character errors. Answer STRICTLY as JSON: {"pass":true|false,"reason":"..."}.
FAIL if any of these are true:
1. The rabbit has only ONE visible long ear when the pose would show two (both ears must be present somewhere in frame).
2. Any red glasses lens is EMPTY — an eye sitting above/outside its rim while a red circle sits elsewhere on the face. Each visible eye must be centered INSIDE a red lens.
3. More than TWO eyes read on the face (an eye plus two empty rims counts as this failure).
4. There is MORE THAN ONE rabbit in the frame, or a human person appears.
5. There is readable text, a wordmark, or a logo anywhere.
Otherwise PASS. If the frame is a close-up of paws/rug where no face is shown, PASS (rules 1-3 do not apply).`;
  try {
    const r = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify({
        model: QA_MODEL,
        messages: [{
          role: "user",
          content: [
            { type: "text", text: ask },
            { type: "image_url", image_url: { url: `data:image/png;base64,${b64}` } },
          ],
        }],
      }),
    });
    if (!r.ok) { console.warn(`  ⚠ QA unavailable (${r.status}) — accepting`); return { pass: true, reason: "qa-skipped" }; }
    const j = await r.json();
    const text = j.choices?.[0]?.message?.content || "";
    const m = text.match(/\{[\s\S]*\}/);
    if (!m) return { pass: true, reason: "qa-unparsed" };
    return JSON.parse(m[0]);
  } catch {
    console.warn("  ⚠ QA network fail — accepting");
    return { pass: true, reason: "qa-skipped" };
  }
}

function archiveRejected(png, reason) {
  mkdirSync(REJECTED, { recursive: true });
  const base = basename(png).replace(/\.png$/, "");
  const n = readdirSync(REJECTED).filter((f) => f.startsWith(base + ".rej")).length + 1;
  const dest = join(REJECTED, `${base}.rej${n}.png`);
  renameSync(png, dest);
  console.log(`  ⌂ rejected (${reason}) → rejected/${basename(dest)}`);
}

async function generate(beat, i) {
  const outPath = panelPath(i, beat.name);
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return true; }
  // Every beat after the anchor refs the anchor so it is the same bunny.
  const useEdit = i > 0 && existsSync(ANCHOR);
  const refs = useEdit ? [ANCHOR] : [];
  const attempts = beat.faceVisible ? QA_ATTEMPTS : 1;
  console.log(`▸ ${i} ${beat.name} · ${SIZE} · ${useEdit ? "anchor ref" : "no refs"}${beat.faceVisible ? " · QA" : ""}`);
  for (let qa = 1; qa <= attempts; qa++) {
    const b64 = await genOnce(build(beat), refs, useEdit, beat.name);
    if (!b64) return false;
    if (beat.faceVisible) {
      const verdict = await qaPanel(b64);
      if (!verdict.pass) {
        writeFileSync(outPath, Buffer.from(b64, "base64"));
        archiveRejected(outPath, `QA ${qa}/${attempts}: ${verdict.reason}`);
        continue;
      }
      console.log(`  ✓ QA pass (${verdict.reason || "clean"})`);
    }
    writeFileSync(outPath, Buffer.from(b64, "base64"));
    console.log(`✓ ${rel}`);
    return true;
  }
  console.error(`✗ ${beat.name}: QA failed ${attempts}×`);
  return false;
}

// The anchor must exist (and go first) before any ref'd beat runs.
const todo = BEATS.map((b, i) => ({ b, i })).filter(({ b }) => !ONLY || ONLY.includes(b.name));
const needsAnchor = todo.some(({ i }) => i > 0) && !existsSync(ANCHOR);
if (needsAnchor && !todo.some(({ i }) => i === 0)) todo.unshift({ b: BEATS[0], i: 0 });

let failed = 0;
for (const { b, i } of todo) if (!(await generate(b, i))) failed++;
console.log(failed ? `✗ ${failed} panel(s) failed` : `✓ panels ready`);
process.exit(failed ? 1 : 0);
