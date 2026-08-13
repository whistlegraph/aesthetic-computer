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

// ── the character — pop/momboba/bunny/PREAMBLE.txt, wired for power ──────
const BUNNY =
`THE BUNNY — a single needle-felted wool BUNNY, the recurring AESTHETIC COMPUTER character: a wonderfully woolly little rabbit with an off-white body, soft-grey ear tips and paws, EXACTLY TWO LONG felt ears, a little pink felt nose, tiny dark glossy bead eyes, and a round puff of a tail. he wears tiny round soft RED felt glasses — his signature. he stands and dances upright on his hind legs. the SAME bunny in every scene: identical wool colors, same two long ears, same red glasses.
EAR COUNT (CRITICAL): he has EXACTLY TWO EARS. TWO. never three, never four, never a spare pair behind the first pair. when the ears fly or blur with motion they must still resolve as TWO separate ears attached at two points on the top of his head — a flying ear NEVER splits, doubles, echoes, or leaves a duplicate behind it.
HIS EARS ARE THE RHYTHM — the two ears carry the beat the way a dancer's hair does: hanging heavy when the groove is quiet, swinging in wide arcs as it builds, flying out sideways at full extension when the drums drop, and finally settling.
GLASSES GEOMETRY (CRITICAL): the bunny has exactly TWO eyes. a bunny's eyes sit HIGH and WIDE on the head — so the red glasses ride HIGH on his face, up over the eyes, NOT down low on the muzzle or snout. each eye — open bead eye or closed happy lash-line — is centered INSIDE its own round red lens, one eye per lens, the lens ringing the eye like real worn glasses. the red rims are NEVER empty. NEVER draw an eye ABOVE the rims with the empty lenses sitting lower on the snout (one real eye plus two empty red circles reads as extra eyes — the single worst failure).`;

// The ++ made visible: he is a felted creature with real electronics in him.
const ELECTRIC =
`ELECTRIC FELT — the bunny is WIRED. This is the whole look of the film and it must read in every frame:
· CIRCUITRY IN THE WOOL — fine enameled COPPER and colored magnet WIRE is needle-felted directly INTO his body, running across his chest, shoulders, back and down his limbs in deliberate looping CIRCUIT TRACES, stitched flush into the fibers like veins under skin. tiny felted solder-blobs and little brass eyelets sit where traces meet. the wire is a craft material here — hand-couched into wool, sometimes lifting a millimeter clear of the surface, never printed-on graphics.
· LEDS INSIDE HIM — small bright LEDs are embedded UNDERNEATH the felt, inside his body and inside each ear, and they light him FROM WITHIN. wool is translucent when backlit, so the light BLOOMS THROUGH the fibers: his chest and belly glow like a paper lantern, individual fibers over each LED catch fire with light and read as a soft bright halo, and the wool immediately around each LED goes warm and semi-transparent while the wool further away stays matted and opaque. you can see the discrete POINT of each LED as a hot core inside a soft glowing patch — never a flat even wash, never an overlay.
· HIS EARS LIGHT UP — each ear has a run of LEDs inside it, so when an ear swings past a dark background the whole ear glows through from the inside, the wool going translucent pink-white with the fiber structure visible against the light, like a hand held over a torch.
· THE GLASSES CATCH IT — the red felt rims pick up the internal glow from below and read hot red.
· THE TETHER — a slim felted battery pack sits at the small of his back, and a colored wire tail trails off him across the floor to the speaker stack: he is PLUGGED IN to the sound system, part of the circuit.`;

// The room: @jeffrey's old Ashland A-frame, felted.
const WORLD =
`THE ROOM — a needle-felted miniature of an A-FRAME HOUSE INTERIOR (modeled on the reference photo sheet): steep TRIANGULAR A-frame geometry is the defining shape of every wide shot — massive exposed timber rafters felted in dark walnut wool running up and meeting in a peak overhead, the sloped ceiling planes closing in on both sides, honey-and-amber WOOD PLANK walls and a wood plank FLOOR felted in warm ochre wool with visible board seams, and a big window set into the gable end. a round green wool rug sits on the floor. the SAME room throughout the film — same rafters, same plank floor, same gable window — progressively taken over by the sound system.
THE SOUND SYSTEM GROWS — hand-felted speaker cabinets accumulate along the walls and up under the rafters until they are a leaning tower; thick colored yarn CABLES snake everywhere across the plank floor and up into the beams, plugging into him.`;

const LIGHT =
`LIGHT — this is a LIGHT SHOW and it is all PHYSICALLY BUILT, never a filter. The A-frame rafters are strung with practical lighting the way a real party rigs a room:
· NEON — actual bent glass NEON TUBING and glowing EL-WIRE runs along the rafters and down the sloped ceiling planes, zip-tied to the beams with felted brackets, saturated magenta, cyan, acid-green and hot orange. the tubes are OBJECTS in the room with visible glass ends and wire leads — they light the wood, throw colored pools onto the plank floor, and put hard colored rims on the bunny's fuzz.
· BLASTING ON THE BEAT — the rig is mid-BLAST in the dynamic beats: several tubes at full blaze at once, colored light hitting the rafters hard, hot spill across the floor, deep saturated color everywhere and the wood glowing in it. in the quiet beats most tubes are dark or dimmed to a low ember and only his internal LEDs and one or two tubes are lit.
· HIS INTERNAL GLOW IS ALWAYS PART OF THE LIGHTING — the light coming out through his wool actually illuminates what is near him: the rug under his feet, his own paws, the wool he touches.
· contrast is high and the room is DARK between the lights — deep shadow in the rafter peak, hot saturated color below. every light source is a real fixture visible in frame or just outside it.`;

const PALETTE =
`PALETTE — off-white and soft-grey wool bunny lit from inside in warm white and amber; round red glasses; copper and enamel-green wire traces; honey, ochre and walnut felted wood; charcoal speaker cabinets with oatmeal cones; and the neon rig in saturated magenta, cyan, acid-green and hot orange. the WOOL stays matted and real — the saturation lives in the light, not in a filter over the image.`;

const AVOID =
`AVOID — any 3d-render, clay, plastic, or smooth plush-toy look (this is real matted needle-felted wool, and the light is real fixtures); ANY human person anywhere (the bunny is the only character); a second bunny, a mirror, or any doubled figure; MORE THAN TWO EARS on the bunny (no third ear, no doubled or echoed ear); empty red glasses rims; any readable text, wordmark, signage, band name, or logo of any kind; any post-processing look — no bloom filter over the whole frame, no lens flares, no laser beams as solid cones, no light shafts drawn as objects, no glowing outline traced around the character; motion-blur streaks or drawn speed-lines; the character looking at or acknowledging the camera; any real-world brand.`;

const POV_NOTE =
`POV INTEGRITY — this is the bunny's OWN first-person viewpoint: his eyes ARE the camera. The only parts of him in frame are his own front PAWS and forearms in the near foreground, seen from behind as his own hands would be. ABSOLUTELY NO second bunny, no mirror, no reflection, no face — his head is never visible because the camera is inside it.`;

const FRAME_NOTE =
`${FRAMING_YT_LANDSCAPE}
HONOR THE SHOT DIRECTIVE in this beat exactly: a WIDE shot lets the whole room breathe with the bunny comfortably within it; a CLOSE-UP crops in tight and FILLS the frame with the detail (do NOT pull back to a wide); a LOW-ANGLE puts the camera down on the rug looking up.`;

// ── the twelve beats — one per struct section ────────────────────────────
// Names + order MUST match pop/maytrax/out/femrag-plusplus.struct.json.
const BEATS = [
  {
    // Not a track section — the character/room ANCHOR every other panel is
    // generated against. Establishes the electric felt look in full.
    name: "runway", faceVisible: true,
    scene:
`WIDE, three-quarters from the front, the establishing frame of the film. The felt bunny stands on the round green wool rug on the plank floor of the dim A-frame room, the great walnut rafters rising and meeting in a peak above him and the gable window a dark blue rectangle behind. He is powered up and standing still: the copper circuit traces felted across his chest and limbs clearly visible, the LEDs inside his body glowing warm white-amber THROUGH his wool so his chest and belly read as a soft lantern and his two long ears glow translucent pink-white from the inside where they hang. His slim felted battery pack sits at the small of his back and its colored wire tail trails away across the plank floor. One neon tube along the near rafter is lit a low magenta; everything else in the room is dark. His red glasses catch the glow from below. Two ears, hanging heavy. Quiet, powered, waiting.`,
  },
      {
    // The film OPENS here — the track starts cold on the drop.
    name: "drop1a", faceVisible: false, pov: true,
    scene:
`FIRST-PERSON POV, and this is the FIRST SHOT OF THE FILM — the track opens cold on the drop, so this frame is already at full power. The camera is the bunny's own eyes, looking DOWN at the little felted LAPTOP open on the green wool rug in front of him. His own two off-white furry FOREPAWS reach into frame from the bottom, seen from behind as his own hands, both down on the laptop's felted keyboard mid-strike — he is HAMMERING the beat out. The laptop is entirely needle-felted: a soft charcoal-wool clamshell, a keyboard of tiny pale felted keycaps, and a SCREEN that is a flat panel of glowing felt showing only soft abstract blocks and bars of colored light (magenta, cyan, amber) in even rows — a music grid read purely as colored wool light, with NO readable text, numbers or letters anywhere. The screen blazes and throws hard colored light up onto his paws. The copper circuit traces felted into his forearms catch it, and the LEDs under the wool of his arms are at FULL BLAST, glowing hot from inside so his own forearms are lanterns. Beyond the laptop: the plank floor, the speaker tower, and the dark A-frame rafters overhead with the whole neon rig BLASTING ON — magenta, cyan and acid-green tubes at once, colored light raking the beams and pooling on the boards.`,
  },
  {
    name: "drop1b", faceVisible: true,
    scene:
`CLOSE-UP, filling the frame — the bunny's head and shoulders in full motion, single fibers and matted clumps resolving. Head thrown back, mouth-line open in delight, bead eyes squeezed into happy closed lash-lines centered inside their red lenses. His TWO long ears are mid-whip in a wide S-curve past his head — exactly two ears, each a separate glowing translucent shape with its internal LEDs visible as bright cores through the wool, fiber structure lit up against the light. The felted copper traces on his shoulders flare where cyan neon rakes across them. Loose wool wisps float free, catching magenta light. Behind, out of focus, the dark rafters and the blazing tubes.`,
  },
  {
    name: "breakdown", faceVisible: true,
    scene:
`WIDE and still, the room at its darkest. The bunny stands at the largest felted speaker cabinet with one front paw laid flat against its oatmeal wool cone, feeling the bass through his paw. Body low, knees bent, rocking. His TWO long ears hang low and heavy, swinging in a slow lazy pendulum. Almost every neon tube is DARK now — only one cyan tube along a far rafter still burns low, and the dominant light in the entire room is the glow coming OUT of the bunny himself: his chest lantern throws warm light across the speaker cone he is touching, up onto his own chin, and down onto the plank floor around his feet, his ears two soft glowing shapes in the dark. The A-frame rafters recede into blackness above. Enormous, quiet, patient.`,
  },
  {
    name: "buildup2", faceVisible: true,
    scene:
`CLOSE-UP, tight on the bunny's face and upper body, filling the frame. The second wind-up, steeper than the first: crouched all the way down with his chin nearly at his own knees, both front paws pressed to the plank floor, every muscle of the little wool body gathered. His TWO long ears are pinned flat back along his spine and held under maximum tension — two ears, both glowing hard from within. The LEDs inside him are pulsing brighter and faster than anywhere in the film, the wool over his chest nearly white-hot and translucent, individual fibers blazing, light spilling onto the boards under his paws. One bead eye visible inside its red lens, fixed and wide. The rest of the A-frame is black. The most compressed frame in the film.`,
  },
  {
    name: "drop2a", faceVisible: true,
    scene:
`WIDE, LOW-ANGLE into the A-frame peak. THE SECOND DROP — bigger than the first. The bunny is at the very top of a leap, higher than before, body arched back, all four paws flung out in a star, tail puff visible. His TWO long ears — exactly two — whip up and back above his head, both blazing translucent. He is at maximum power: his whole body glowing, throwing colored light onto the rug below. The neon rig is at FULL BLAST and there is more of it now — magenta, cyan, acid-green and hot orange tubes running the length of every rafter and down both sloped ceiling planes, the speaker tower stacked up under the beams behind him, hard saturated light everywhere, wool wisps hanging in the colored air where he launched.`,
  },
  {
    name: "drop2b", faceVisible: true,
    scene:
`EXTREME CLOSE-UP on the bunny's hind paws and the plank floor, filling the frame — the only beat where his face is not the subject. Wool fibers at maximum resolution: the soft-grey felted pads of his hind paws mid-stamp, one paw planted and compressing the wool of the green rug, the other lifting away with wisps trailing. Right through the frame run the felted-in COPPER CIRCUIT TRACES on his ankles and a thick colored yarn cable snaking across the boards, and an LED under the wool of his lower leg glows hot through the fibers, throwing light onto the plank floor. Magenta and cyan neon rake across the wood from above. Above and out of focus, the blurred glowing mass of his body still moving.`,
  },
  {
    name: "ragga-a", faceVisible: true,
    scene:
`WIDE. THE DANCEHALL — the A-frame is now a full sound-system dance. Felted speaker cabinets are stacked in a leaning tower up under the rafters, huge oatmeal wool cones, thick yarn cables looped across the plank floor and up into the beams. The neon has gone warm and heavy for this section: deep amber and forest-green tubes along the rafters, one hot orange run down the sloped ceiling, the wood glowing warm in it. The bunny skanks: leaning forward, weight low, one shoulder dropped, both front paws swinging loosely across his body, one hind paw kicked back. His TWO long ears swing in a lazy behind-the-beat arc, both glowing from within. His internal light has gone warm gold here, and his wire tail runs from his back across the floor into the base of the stack — plugged in.`,
  },
  {
    name: "ragga-b", faceVisible: true,
    scene:
`CLOSE-UP, low — the bunny and the mouth of the biggest felted speaker cone behind him. He has his back half-turned to the cabinet, leaning into it, ONE long ear flung forward across his own face by the swing and the OTHER trailing behind — exactly two ears, both lit from the inside and translucent against the dark cone. Bead eyes happy inside their red lenses. The enormous oatmeal wool cone behind him is visibly pushed outward at its center — the wool physically bulging with a bass note — and loose fibers around its rim lift away from the surface. Amber and forest-green neon rakes across the scene from the rafters above; his own gold internal glow lights the cone's wool where he leans on it.`,
  },
  {
    name: "ragga-breathe", faceVisible: true,
    scene:
`WIDE, the dancehall taking a breath. The neon rig has dropped almost all the way out — only one deep-amber tube along a far rafter still burns low — and the room falls back to the bunny's own light. He keeps skanking but small and easy, weight low, one shoulder dropped, both front paws swinging gently across his body. His TWO long ears swing in slow behind-the-beat arcs, glowing warm gold from the LEDs inside them, and the light coming out of his chest is now the brightest thing in the A-frame, throwing real warm light onto the green rug, the plank boards, and the base of the speaker tower beside him. The great rafters recede into darkness above. His wire tail runs across the floor into the stack. Held breath, all glow, no blast.`,
  },
  {
    name: "ragga-push", faceVisible: true,
    scene:
`WIDE, the room coming back. The neon is switching back ON across the rafters — amber, forest-green and hot orange tubes lighting one after another so the A-frame reads half-lit and building, hard color returning to the walnut beams and spilling down the sloped ceiling planes onto the plank floor. The bunny has dug in: deeper skank, weight dropped lower, one hind paw kicked further back, both front paws swinging in bigger diagonals across his body. His TWO long ears swing in wide committed arcs, both blazing translucent from within. His internal LEDs have pushed from warm gold up to near-white, the wool of his chest hot and semi-transparent. The speaker tower looms lit behind him, cables everywhere.`,
  },
  {
    name: "ragga-push-b", faceVisible: true,
    scene:
`WIDE, LOW-ANGLE from the plank floor looking up into the A-frame peak — the biggest dancehall frame in the film. Every neon tube in the rafters is at FULL BLAST at once: magenta, cyan, acid-green, amber and hot orange running the length of every beam and down both sloped ceiling planes, the whole triangular geometry of the room screaming with saturated color, hard light raking the walnut and pooling across the boards. The bunny is mid-skank at maximum power, body low and turned, one hind paw kicked back, both front paws thrown wide, his TWO long ears — exactly two — whipping out in big arcs, each one blazing translucent. His whole body is a lantern at full output, throwing colored light down onto the rug. The speaker tower rises the full height of the frame beside him, cones bulging, yarn cables everywhere, his wire tail plugged into its base. Loose wool wisps hang lit in the colored air.`,
  },
  {
    name: "outro", faceVisible: true,
    scene:
`WIDE and warm, the A-frame at rest. The bunny has stopped dancing and stands still at the center of the green rug on the plank floor, breathing, one front paw resting on his own chest. His TWO long ears hang all the way down, completely spent, and their internal light has faded to a last dim ember. His wool is visibly fluffed and disheveled all over — he has genuinely shaken his fuzz out — and loose wisps have settled across the rug and boards around him. Every neon tube in the rafters is DARK except one, a low magenta run along the near beam. The LEDs inside his chest have dimmed to a slow soft pulse, the only real light left, glowing gently through the wool. The speaker tower looms quiet behind him. The great rafters rise into the dark. After.`,
  },
];

if (flags.list) {
  BEATS.forEach((b, i) => console.log(`${i}\t${b.name}`));
  process.exit(0);
}

const panelPath = (i, name) => `${OUT}/${SLUG}-yt-sec-${i}-${name}.png`;
const ANCHOR = panelPath(0, "runway");
// @jeffrey's old Ashland A-frame — the room is modeled on this contact sheet
// (exposed rafters meeting in a peak, honey plank walls and floor, gable
// window, round green rug, string lights already strung along the beams).
const ASHLAND = resolve(REPO, "pop/nullabye/reel/special-sign/refs/ashland-house-platter-contact.jpg");

function build(beat) {
  return [
    `SHAKEOUT — one beat from a needle-felted music video.`,
    NEEDLE_FELT_WOOL,
    FRAME_NOTE,
    BUNNY,
    ELECTRIC,
    WORLD,
    LIGHT,
    PALETTE,
    beat.pov ? POV_NOTE : null,
    `THIS BEAT — ${beat.scene}`,
    AVOID,
  ].filter(Boolean).join("\n\n");
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
          const lower = ref.toLowerCase();
          const ext = lower.endsWith(".png") ? "png" : lower.endsWith(".webp") ? "webp" : "jpeg";
          fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), basename(ref));
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
1. EAR COUNT IS NOT TWO. Count every long rabbit ear in the frame, including any that is blurred, flying, or partly out of frame. The count must be EXACTLY TWO. Three or four ears — including a duplicated, echoed, or split ear trailing behind a moving one — is a FAIL. Only ONE ear visible when the pose would show both is also a FAIL.
2. Any red glasses lens is EMPTY — an eye sitting above/outside its rim while a red circle sits elsewhere on the face. Each visible eye must be centered INSIDE a red lens.
3. More than TWO eyes read on the face (an eye plus two empty rims counts as this failure).
4. There is MORE THAN ONE rabbit in the frame, or a human person appears.
5. There is readable text, a wordmark, letters, or numbers anywhere — including on any screen in frame.
Otherwise PASS. If the frame is a close-up of paws/floor, or a first-person POV showing only the rabbit's own forepaws with no head visible, then rules 1-3 do not apply — PASS unless rule 4 or 5 is broken.`;
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
  // The Ashland sheet carries the ROOM (A-frame geometry, plank wood, gable
  // window); the anchor carries the BUNNY. The anchor itself is generated
  // against Ashland alone, since it is what defines the character.
  const refs = [existsSync(ASHLAND) ? ASHLAND : null, i > 0 && existsSync(ANCHOR) ? ANCHOR : null].filter(Boolean);
  const useEdit = refs.length > 0;
  const needsQA = beat.faceVisible || beat.pov;   // POV still gets the no-text check
  const attempts = needsQA ? QA_ATTEMPTS : 1;
  console.log(`▸ ${i} ${beat.name} · ${SIZE} · ${useEdit ? refs.length + " refs" : "no refs"}${needsQA ? " · QA" : ""}${beat.pov ? " · POV" : ""}`);
  for (let qa = 1; qa <= attempts; qa++) {
    const b64 = await genOnce(build(beat), refs, useEdit, beat.name);
    if (!b64) return false;
    if (needsQA) {
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
