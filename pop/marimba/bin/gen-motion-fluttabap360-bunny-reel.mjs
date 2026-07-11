#!/usr/bin/env node
// marimba/bin/gen-motion-fluttabap360-bunny-reel.mjs — the BUNNY cut of
// the fluttabap360 METAMORPHOSIS REEL motion pass. Same 13-shot loop cut,
// shot names, durations and ONE morph (growdance grow→dance) as the
// jeffrey driver (gen-motion-fluttabap360-reel.mjs), but the metamorph is
// the BUNNYFLY — a colored-pencil + gouache bunny × monarch hybrid (two
// long ears, round red glasses, its OWN monarch wings, butterfly antennae)
// that hatches as a larval BUNNYPILLAR and later hardens into a metal
// bunnyfly drone. Motion prompts match the redesigned panels (dark-sharp
// MacBook keys, one-creature molt — no split). Each bunny beat-master panel
// (out/_fluttabap360-bunny-reel-beat-<name>.png) becomes the source frame
// for a Seedance 2.0 image-to-video shot; --assemble cuts the same 46.2s
// portrait phrase (mommywow → slinky → fly → ride, 34.7s→80.9s, loops)
// and hands off to chrome-reel-fluttabap360.mjs pointed at THIS lane's
// motion dir + slug.
//
// The bunny lane keeps its OWN motion dir (out/motion-bunny) so its takes
// never collide with the jeffrey reel's out/motion/takes.json.
//
// Story + cut list: pop/marimba/fluttabap360-reel.story.txt.
// Standalone driver (one shot per beat); same fal.mjs client
// (caching, queue-resume, retries). Mostly CUTS; ONE morph —
// growdance runs start-frame→end-frame from the grow panel to the
// dance panel (same-camera escalation; the stand-up rolls into the
// pirouette in one continuous shot). Morph clips ARRIVE on their end
// frame, so assembly trims them from the HEAD.
//
// Motion grammar (@jeffrey 2026-07-02): the camera is ALIVE — orbits,
// spins, dynamic tilt + shift — objects spin on their own axes, and
// light REFRACTS across every wet/glossy surface (goo, dew, metal,
// screen sheen) as painted gouache glints, never lens flare.
//
// Moderation: every figure beat is staged from behind / three-quarter-
// behind / head-down; a FAILED shot is logged and SKIPPED. CUT beats
// with no clip fall back to Ken Burns at assembly.
//
// Usage:
//   node pop/marimba/bin/gen-motion-fluttabap360-bunny-reel.mjs --dry-run     # prompts + cost
//   node pop/marimba/bin/gen-motion-fluttabap360-bunny-reel.mjs               # gen all missing
//   node pop/marimba/bin/gen-motion-fluttabap360-bunny-reel.mjs --only molt --force  # re-roll
//   node pop/marimba/bin/gen-motion-fluttabap360-bunny-reel.mjs --assemble    # 46.2s loop cut + chrome
//   pick/re-roll takes in ClipWizard (macOS) — writes out/motion/takes.json

import { readFileSync, writeFileSync, existsSync, mkdirSync, statSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateShot, RATE_PER_SEC } from "../../lib/fal.mjs";
import {
  COLORED_PENCIL_TOOTH_MOTION,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
} from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const MOTION = `${OUT}/motion-bunny`;
const SLUG = "fluttabap360-bunny-reel";

const AUDIO = `${OUT}/fluttabap360.mp3`;
// One full musical phrase: mommywow 34.7 → ride end 80.9 (46.2s).
// hush (7.7s) under cocoon/crack/hatch · slinky climb (15.4s) under
// hatchpov→laptopspov · FLY lift (15.4s) under molt/wing/drone ·
// ride outro (7.7s) under spacepov/space. Ends on the pass boundary —
// the reel loops back into the hush.
const AUDIO_START = 34.7;

const MEDIUM_MOTION = [
  `Diegetic light only — no glow filters, no neon, no motion-blur language, never cinematic, never a photograph. Hand-drawn colored-pencil + gouache on warm cream paper. Any figure never looks at the camera. Motion is graceful; the painted-paper monarchs drift on their own gentle arcs.`,
  `CAMERA + MOTION DYNAMICS — the camera is ALIVE: slow orbits, gentle spins, dynamic tilts and drifting shifts of frame; subjects and objects SPIN on their own axes as the camera counter-turns. Light REFRACTS across every wet or glossy surface — dew drops, goo strands, brushed-metal panels, screen sheen — painted glints that slide, split and re-gather as things rotate through the sun. The refraction is painted gouache highlight play, never a lens flare, never a glow filter.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  COLORED_PENCIL_TOOTH_MOTION,
].join("\n\n");

// One entry per shot. `dur` = Seedance generation length (clamped 4–15);
// `exact` = the trim in the 46.2s assembly (exacts sum to 46.2 and land
// the FLY lift on molt at +23.1s). `image`/`endImage` override the
// default same-name panel; endImage makes the shot a MORPH (head-trim).
const BEATS = {
  cocoon: {
    dur: 4, exact: 2.5,
    motion:
`Dawn stillness, then the crack: the human-sized jade-green paper-mache chrysalis SLOWLY ROTATES on its silk anchor, the crown of gold dots catching and refracting the first sun in little sliding glints as they turn. The bright seam down its side SPLITS a little wider — flakes of painted paper spiraling down spinning — the sliver of pale-yellow fabric inside shifting. The two waiting monarchs open and close their wings. The camera drifts slowly upward with a gentle tilt, dew on the lawn far below sparkling. The world holding its breath.`,
  },
  crack: {
    dur: 4, exact: 2.5,
    motion:
`Extreme close-up on the splitting seam: the hand-cut paper edge TEARS slowly wider, fiber by fiber, flakes of painted paper peeling away and spinning down out of frame. The fat dew-drop clinging beside the seam acts as a tiny LENS — the warm light from inside the chrysalis refracting through it, splitting into painted glints that slide as the drop trembles. The sliver of pale-yellow fabric shifts inside. The camera pushes in slowly with a slight roll, tilting into the widening split.`,
  },
  hatch: {
    dur: 4, exact: 2.7,
    motion:
`The BUNNYPILLAR — the caterpillar form of the bunnyfly: a plump softly-segmented off-white wool body with a row of tiny stubby legs, two long rabbit ears, round red glasses and short nubby proto-antennae, tiny crumpled wing-buds on his back — inches slowly OUT of the split chrysalis along the thick branch, gripping the bark with his little legs, gooey with birth: the glistening strands of pale-jade fluid stretching from his body back to the husk sag, wobble and SNAP one by one as he pulls forward, fat drops refracting the morning sun as they fall away toward the lawn. His wool stays matted and wet, the soaked ears drip, his segments flexing caterpillar-like as he crawls. The cracked husk swings and slowly rotates behind him, fluid stringing from its torn lip. The camera tracks alongside with a gentle tilt and shift, keeping his lowered head away from view. Wet shine stays painted gouache — never a glow.`,
  },
  hatchpov: {
    dur: 4, exact: 3.0,
    motion:
`First-person POV, brand-new eyes (no face ever visible, no second figure anywhere): the bunnypillar's own front stubby legs and the first ringed segment of his off-white wool caterpillar body grip the rough oak bark and pull forward, a strand of jade goo on one leg stretching and glinting in the sun. Beyond them the dawn park swims from blur into focus far below — dew refracting in tiny sparks across the lawn, the faint ground-grid resolving — and ONE monarch rises toward the camera-as-his-eyes, SPINNING slowly on its arc. The view sways and tilts with each pull forward, like the first time a body is used.`,
  },
  growdance: {
    dur: 6, exact: 4.4,
    image: "grow",
    endImage: "dance",
    motion:
`One continuous shot, start frame to end frame — the METAMORPHOSIS: seen from behind, the BUNNYPILLAR rears up out of his segmented caterpillar body and TRANSFORMS into the BUNNYFLY in one unfurling stretch — the ringed larval segments drawing together into a small velvety butterfly body, his OWN real monarch wings (orange + black, part of his body, never paper-mache) swelling and creaking OPEN to full span, drops of jade goo spinning off the wingtips, two long ears rising tall and two slender butterfly antennae extending — and the rise rolls WITHOUT A CUT into a full PIROUETTE: now the finished bunnyfly spins on the ball of one foot, wings fanning bands of refracted sunlight, and lands out of the turn exactly in the ballet lift of the final frame — relevé, low arabesque, paws curved overhead. The camera ORBITS slowly counter to his spin. Three or four monarchs corkscrew upward around him, sharing the rotation. Joy arriving all at once.`,
  },
  laptops: {
    dur: 4, exact: 2.5,
    motion:
`Wide from behind and over his shoulder, his head in three-quarter profile: the BUNNYFLY — bunny head, two long ears, round red glasses, his own monarch wings — kneels at his arc of five different-colored open laptops and PLAYS them like a marimba with his BARE PAWS — paw-tips bouncing off the pale MacBook keys in a quick alternating pattern, wrists loose and percussive, paws hopping machine to machine — while he SINGS like a pop star into the antenna-mic: one slender butterfly antenna curved down with its club-knob bobbing at his lips as his chin lifts and drops mid-note, mouth shaping the song, both long ears swaying to the rhythm. The camera ORBITS slowly around the arc — and as the angle changes, the glossy sheen on each notepat screen slides and refracts across the tile grids, candy-colored tiles winking pressed-and-released chords. The monarchs perched along the screen-tops flutter up and resettle with each strike. His wings tip in time. Gleeful, belting, musical.`,
  },
  mallets: {
    dur: 4, exact: 2.75,
    motion:
`Extreme close-up: the bunnyfly's bare paw-tips BOUNCE off the pale MacBook keys (a scatter of dark piano-sharp keys among them) like a pair of marimba mallets — strike, rebound, strike, wrist loose — each hit brightening the matching candy-colored tile on the notepat screen above, the grid winking through a chord. The screen's glossy sheen SPLITS and re-gathers around each bounce, painted glints sliding across the tilted glass. The camera tilts and shifts subtly with each strike, like a head keeping the beat. The monarch perched on the screen-top lip flutters and resettles in rhythm. The paw stays low in frame, face never shown.`,
  },
  laptopspov: {
    dur: 4, exact: 2.75,
    motion:
`First-person POV mid-performance (no face ever visible, no second figure anywhere): the bunnyfly's own bare forearms and paws play down across the arc of five different-colored laptops — left paw-tips striking one pale MacBook keyboard, right paw rebounding off another, alternating fast like a marimbist's mallets. The view BOBS and TILTS with the playing, and as it moves the sheen rolls in refracted bands across each screen, the notepat tile grids winking different chords under his paws. The monarchs along the screen-tops hop and flutter with the rhythm. Flow-state loose, the whole instrument his.`,
  },
  molt: {
    dur: 6, exact: 5.4,
    motion:
`The hardening: facing away from camera beside his neat stack of closed laptops, the BUNNYFLY's paws rise into one last port de bras — and his whole body CRYSTALLIZES into brushed metal from the wingtips inward, his own monarch wings hardening panel by panel into thin orange + black brushed-metal plates with rivet dots, the metal frost sweeping down across his woolly body and up his two long ears, closing around the red glasses. He stays ONE creature the entire shot — the bunnyfly BECOMING a metal bunnyfly in place, beginning to lift off. The camera circles slowly around him. CRITICAL: do NOT split him into a separate butterfly plus a leftover rabbit; NO second bunny, NO shed fur or empty body settling on the lawn, NO monarch flying away from him — one figure turning to metal. Solemn wonder, not chaos. The new metal wings give their first full beat.`,
  },
  wing: {
    dur: 5, exact: 4.5,
    motion:
`Extreme close-up on the bunnyfly's OWN single monarch wing, and the camera ORBITS it slowly as the wing itself rotates on its axis: the hardening SWEEPS across the surface like frost — soft painted orange + black crystallizing panel by panel into brushed metal, rivet dots surfacing one by one — and each new plate catches the sun as it turns, throwing sliding, splitting bands of refracted glint across the frame. The soft wing edge stiffens to a machined line. The small lens-eye at the wing root irises open and flashes once. The wing flexes, testing its new material.`,
  },
  drone: {
    dur: 6, exact: 5.5,
    motion:
`The METAL BUNNYFLY — the same hero hardened to a brushed-metal bunny × monarch, keeping its two long metal rabbit ears and its red glowing lens-ring glasses — CORKSCREWS upward over the shrinking park, spinning slowly on its own axis as it climbs, metal monarch wings beating, the sun sweeping turning bands of refracted light around the brushed-metal panels with every rotation, rivet dots glinting in sequence. Its six legs stay tucked neatly beneath the thorax, trailing back. NO separate rabbit anywhere below — the bunny IS this flying machine now, the lawn far below EMPTY of any figure. The camera SPIRALS up with it, counter-rotating gently, the oak and faint ground-grid wheeling below. The dotted signal-arc corkscrews behind it. The sky deepens toward indigo as they climb.`,
  },
  spacepov: {
    dur: 4, exact: 3.5,
    motion:
`First-person POV through the metal bunnyfly's red lens-eye (no full figure anywhere): the whole view BARREL-ROLLS slowly as it banks, its own brushed-metal monarch wingtips framing the left and right edges and its sleek metal rabbit ear-tips just visible at the top edges, starlight glinting and refracting off the panels in sliding sparks with each wingbeat. Between the wings Earth — the small blue-green hand-hatched marble — arcs gracefully across the rolling frame, the faint orbit-lines curving past. Stars stay fixed like bare paper under a rostrum camera. Vast, calm, leaving home.`,
  },
  space: {
    dur: 5, exact: 4.2,
    motion:
`Outer space, the held last frame: the METAL BUNNYFLY — brushed-metal bunny × monarch, two sleek metal ears, red lens-ring glasses, metal monarch wings — SPINS slowly on its own axis while drifting up among the bare-paper stars, wings beating in long slow strokes, each rotation sweeping a warm band of refracted sunlight around the metal panels, orange glints sliding wing to wing. Earth turns almost imperceptibly below. The camera drifts and tilts a hair, weightless. Everything slow, circular, final — composed to LOOP back into the dawn stillness of the first shot.`,
  },
};

function parseFlags(argv = process.argv) {
  const flags = {};
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
    else { flags[a.slice(2)] = next; i++; }
  }
  return flags;
}

const flags = parseFlags();
const TIER = flags.tier || "fast";
const RATE = RATE_PER_SEC[TIER];
const ONLY = flags.only ? String(flags.only).split(",").map((s) => s.trim()) : null;

mkdirSync(MOTION, { recursive: true });

// 13-shot loop cut. The hatch WIDE is back (gooey); grow + dance wides
// are fused into the growdance MORPH. exacts sum to 46.2.
const ORDER = [
  "cocoon", "crack", "hatch", "hatchpov", "growdance",
  "laptops", "mallets", "laptopspov", "molt",
  "wing", "drone", "spacepov", "space",
];

const panel = (name) => `${OUT}/_${SLUG}-beat-${name}.png`;
const shots = ORDER
  .map((name, i) => {
    const b = BEATS[name];
    return {
      name, i,
      dur: Math.min(15, Math.max(4, b.dur)),
      exact: b.exact,
      image: panel(b.image ?? name),
      endImage: b.endImage ? panel(b.endImage) : null,
      out: `${MOTION}/${SLUG}-${name}.mp4`,
      prompt: `${b.motion}\n\n${MEDIUM_MOTION}`.trim(),
    };
  })
  .filter((s) => (ONLY ? ONLY.includes(s.name) : true));

if (ONLY) {
  const missing = ONLY.filter((n) => !BEATS[n]);
  if (missing.length) { console.error(`✗ unknown beat(s): ${missing.join(", ")}`); process.exit(1); }
}

// ── generation pass ────────────────────────────────────────────────────
if (!flags.assemble) {
  const billed = shots.reduce((a, s) => a + s.dur, 0);
  console.log(`▸ ${SLUG} motion · ${shots.length} shot(s) · ${billed}s billed · ~$${(billed * RATE).toFixed(2)} @ ${TIER} 720p 9:16`);

  if (flags["dry-run"]) {
    for (const s of shots) console.log(`\n── ${s.name} · gen ${s.dur}s → cut ${s.exact}s${s.endImage ? " · MORPH" : ""}\n${s.prompt}`);
    process.exit(0);
  }

  for (const s of shots) {
    for (const p of [s.image, s.endImage].filter(Boolean)) {
      if (!existsSync(p)) { console.error(`✗ panel missing: ${p} — run gen-sections-fluttabap360-bunny-reel.mjs first`); process.exit(1); }
    }
  }

  let made = 0, skipped = 0, failed = 0;
  const rejects = [];
  for (const s of shots) {
    if (!flags.force && existsSync(s.out)) {
      console.log(`· ${s.name}: cached (${(statSync(s.out).size / 1e6).toFixed(1)} MB) — --force to re-roll`);
      skipped++;
      continue;
    }
    console.log(`\n▶ ${s.name} · ${s.dur}s${s.endImage ? " · MORPH" : ""} → ${s.out.replace(LANE + "/", "")}`);
    const r = await generateShot({
      image: s.image,
      endImage: s.endImage,
      prompt: s.prompt,
      duration: s.dur,
      ratio: "9:16",
      resolution: "720p",
      tier: TIER,
      outPath: s.out,
      label: s.name,
    });
    if (r.ok) {
      made++;
      console.log(`✓ ${s.name} · seed ${r.seed} · ${(r.bytes / 1e6).toFixed(1)} MB · ${r.seconds.toFixed(0)}s`);
    } else {
      failed++;
      const moderated = /moderation|content|policy|reject|nsfw|face/i.test(r.error || "");
      rejects.push({ name: s.name, moderated, error: r.error });
      console.error(`✗ ${s.name}: ${moderated ? "MODERATION" : "FAILED"} — ${r.error}`);
      console.error(`  (skipping ${s.name}, continuing batch)`);
    }
  }
  console.log(`\n── done · ${made} made · ${skipped} cached · ${failed} failed`);
  if (rejects.length) {
    console.log("  rejects:");
    for (const r of rejects) console.log(`    ${r.name}${r.moderated ? " [moderation]" : ""}: ${(r.error || "").slice(0, 120)}`);
  }
  process.exit(failed && !made && !skipped ? 1 : 0);
}

// ── assembly: picked takes → exact trims → concat → loop-phrase audio ──
// Emits a chrome-less BASE at 1080×1920@30 plus meta-fluttabap360-bunny-reel.json,
// then hands off to chrome-reel-fluttabap360.mjs (pointed at THIS lane's
// motion dir + slug). CUT shots open on their input panel — trim the
// TAIL. MORPH shots arrive on their end panel in their final frames —
// trim the HEAD so the arrival lands whole.
const W = 1080, H = 1920, FPS = 30;
const takesPath = `${MOTION}/takes.json`;
const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};
const BASE = `${MOTION}/base-${SLUG}.mp4`;

const totalSec = shots.reduce((a, s) => a + s.exact, 0);
console.log(`▸ assembling metamorphosis loop-cut base · ${shots.length} beats · ${totalSec.toFixed(1)}s · ${W}x${H}@${FPS}`);
const trimmed = [];
for (const s of shots) {
  const picked = takes[`${SLUG}-${s.name}`] ? resolve(MOTION, takes[`${SLUG}-${s.name}`]) : s.out;
  if (picked !== s.out) console.log(`  ☑ ${s.name}: ${takes[`${SLUG}-${s.name}`]}`);
  const t = `${MOTION}/trim-${SLUG}-${s.i}-${s.name}.mp4`;
  let res;
  if (existsSync(picked)) {
    const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
      "-of", "default=noprint_wrappers=1:nokey=1", picked], { encoding: "utf8" });
    const clipDur = Number(probe.stdout?.trim()) || 0;
    const head = s.endImage ? Math.max(0, clipDur - s.exact) : 0;
    if (head > 0.01) console.log(`  ↪ ${s.name}: morph — trimming ${head.toFixed(2)}s from head`);
    res = spawnSync("ffmpeg", [
      "-y", "-i", picked,
      ...(head > 0.01 ? ["-ss", head.toFixed(3)] : []),
      "-t", s.exact.toFixed(3),
      "-vf", `scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H},fps=${FPS}`, "-an",
      "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
      t,
    ], { stdio: ["ignore", "ignore", "pipe"] });
  } else {
    // No take → Ken Burns the panel so the cut still completes.
    const frames = Math.round(s.exact * FPS);
    console.log(`  ⚠ ${s.name}: no take — Ken Burns fallback (${frames}f)`);
    res = spawnSync("ffmpeg", [
      "-y", "-loop", "1", "-i", s.image,
      "-vf", `scale=-2:7680,zoompan=z='1+0.10*on/${frames}':x='iw/2-(iw/zoom)/2':y='ih/2-(ih/zoom)/2':d=${frames}:s=${W}x${H}:fps=${FPS}`,
      "-frames:v", String(frames), "-an",
      "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
      t,
    ], { stdio: ["ignore", "ignore", "pipe"] });
  }
  if (res.status !== 0) { console.error(`✗ trim failed ${s.name}: ${res.stderr.toString().slice(-300)}`); process.exit(1); }
  trimmed.push(t);
}

const listPath = `${MOTION}/concat-${SLUG}.txt`;
writeFileSync(listPath, trimmed.map((t) => `file '${t}'`).join("\n") + "\n");

console.log(`  concatenating + muxing audio (${AUDIO_START}s → ${(AUDIO_START + totalSec).toFixed(1)}s phrase, loop-clean) …`);
// Loop-friendly: the window ends exactly on the pass boundary, so only
// tiny anti-click fades — the reel is meant to run back into itself.
const mux = spawnSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", listPath,
  "-ss", AUDIO_START.toFixed(3), "-t", totalSec.toFixed(3), "-i", AUDIO,
  "-map", "0:v", "-map", "1:a",
  "-af", `afade=t=in:st=0:d=0.4,afade=t=out:st=${(totalSec - 0.8).toFixed(3)}:d=0.8`,
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest",
  "-movflags", "+faststart", BASE,
], { stdio: ["ignore", "ignore", "pipe"] });
if (mux.status !== 0) { console.error(`✗ mux failed: ${mux.stderr.toString().slice(-400)}`); process.exit(1); }
console.log(`✓ base ${BASE}`);

// beat timings for the chrome pass's per-beat tint
let acc = 0;
const slides = shots.map((s) => { const from = acc; acc += s.exact; return { name: s.name, from, to: acc }; });
writeFileSync(`${MOTION}/meta-${SLUG}.json`, JSON.stringify({ total: totalSec, slides }, null, 2));

const chrome = spawnSync("node", [`${HERE}/chrome-reel-fluttabap360.mjs`,
  "--motion-dir", MOTION, "--slug", SLUG], { stdio: ["ignore", "inherit", "inherit"] });
if (chrome.status !== 0) { console.error(`✗ chrome pass failed`); process.exit(1); }
