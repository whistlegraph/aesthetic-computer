#!/usr/bin/env node
// Animate the six Ashland Attic Gremlin panels with fal Seedance, then cut
// them against the locked Special Sign master. The same pass also makes a
// silent, seamless eight-second Spotify Canvas from the full-system shot.

import { existsSync, mkdirSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateShot, RATE_PER_SEC } from "../../../lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ILLYS = resolve(HERE, "illys-ashland");
const MOTION = resolve(HERE, "motion-ashland");
const OUT = resolve(HERE, "out/ashland");
const MASTER = resolve(HERE, "../../release/special-sign/special-sign-MASTER.wav");
const REEL = resolve(OUT, "special-sign-ashland-instagram-reel.mp4");
const CANVAS = resolve(OUT, "special-sign-ashland-spotify-canvas.mp4");

const LOOK = `Keep the source illustration's exact handcrafted colored-pencil and dry-pastel look on cool neutral-white fibrous paper. Preserve the actual Ashland A-frame room geometry, Jeffrey's identity and outfit, the acid-yellow-green MacBook Neo, and the separate circular sine-system. Motion must feel drawn frame by frame: visible pencil tooth and stable hatch marks, no photorealism, no glossy CGI, no lens flare, no captions or text. Keep exactly one person, one laptop, two arms, two hands and two legs; never invent limbs, fingers, furniture, screens, devices, rods or capsules. Camera motion is subtle and physical so the room remains recognizable. The laptop screen stays unreadable.`;

const BEATS = [
  {
    slug: "quiet-code", image: "01-quiet-code.png", dur: 7, exact: 6.315789,
    motion: `Almost total stillness. Jeffrey's fingers type quietly and naturally, with tiny wrist movements and a slow breath. The two red hanging bulbs barely sway. A monitor indicator and speaker cone make minute pulses. The dark globe remains dormant and perfectly spherical. Use a very slow, nearly imperceptible camera push through the real room.`,
  },
  {
    slug: "first-sine", image: "02-first-sine.png", dur: 5, exact: 4.736842,
    motion: `Jeffrey keeps typing with both hands, then turns his head a few degrees toward the globe. The single cyan filament climbs through the dark sphere and oscillates once like a pure sine; the lone coral point gently pulses. Everything else stays quiet. Make one restrained sideways camera drift, preserving the room and composition.`,
  },
  {
    slug: "listen-vibe", image: "03-listen-vibe.png", dur: 5, exact: 4.736842,
    motion: `Without leaving the keyboard, Jeffrey begins to feel the note: one heel taps, shoulders and head make a small rhythmic sway, fingers continue coding. The three coral rings rotate at distinct spatial angles, two glass bubbles drift, and the cyan vertical line breathes. The camera makes a gentle counter-orbit of only a few degrees.`,
  },
  {
    slug: "add-rings", image: "04-add-rings.png", dur: 7, exact: 6.315789,
    motion: `Jeffrey settles into the low one-knee crouch and types rapidly with both hands, his weight bouncing once on the beat. Each short coding gesture adds one new cyan ring, coral orbit or transparent acoustic-glass bubble to the sphere. The sine mark on the chalkboard responds and the speaker cone pulses. Camera tilts slightly with the crouch; room geometry stays locked.`,
  },
  {
    slug: "gremlin-build", image: "05-gremlin-build.png", dur: 7, exact: 6.315789,
    motion: `The compact coding-gremlin dance takes over: Jeffrey holds the laptop securely across his raised thigh, both hands typing, one sneaker bouncing, curved back and shoulders twitching rhythmically, hair lifting with the movement. The globe accelerates—cyan spirals tighten, hot-coral flybys sweep past, glass bubbles orbit—and patch cables and the chalk line tremble in sympathy. A small counter-orbit of the camera heightens the spin without smearing the pencil drawing.`,
  },
  {
    slug: "full-system", image: "06-full-system.png", dur: 10, exact: 9.473684,
    motion: `Maximum joyful but coherent Attic Gremlin performance, seen from the SAME LOCKED CAMERA for the entire shot. Jeffrey's body, face, clothing and laptop remain in exactly the source pose and orientation—never turn him around, never reveal his back, never add or replace a body. He securely balances Neo and continues typing with both hands; only one planted foot stamps lightly, hair trembles, shoulders pulse and his focused delighted expression shifts subtly. Each keystroke draws another clean cyan or coral trajectory from the laptop into the separate spherical system. Put nearly all animation in the system and room: rings rotate at different speeds, planets orbit, glass bubbles refract pencil highlights, the white core pulses, speakers thump, red bulbs sway, sticky notes flutter and the chalkboard sine rolls. No camera orbit, pan, zoom, tilt or angle change. The first and last compositions match closely for looping. Keep Jeffrey and Neo anatomically stable and crisply readable through all the energy.`,
  },
];

function flags(argv = process.argv) {
  const out = {};
  for (let i = 2; i < argv.length; i++) {
    const arg = argv[i];
    if (!arg.startsWith("--")) continue;
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) { out[arg.slice(2)] = next; i++; }
    else out[arg.slice(2)] = true;
  }
  return out;
}

function run(bin, args) {
  const result = spawnSync(bin, args, { stdio: "inherit" });
  if (result.status !== 0) process.exit(result.status ?? 1);
}

const opt = flags();
const tier = opt.tier || "fast";
const selected = opt.only ? new Set(String(opt.only).split(",").map((x) => x.trim())) : null;
const shots = BEATS.filter((b) => !selected || selected.has(b.slug));
mkdirSync(MOTION, { recursive: true });
mkdirSync(OUT, { recursive: true });

if (!opt.assemble) {
  const seconds = shots.reduce((sum, b) => sum + b.dur, 0);
  console.log(`Special Sign Ashland motion: ${shots.length} shots, ${seconds}s billed, ~$${(seconds * RATE_PER_SEC[tier]).toFixed(2)} at ${tier} tier.`);
  for (const beat of shots) {
    const image = resolve(ILLYS, beat.image);
    const outPath = resolve(MOTION, `${beat.slug}.mp4`);
    if (!existsSync(image)) throw new Error(`Missing illustration: ${image}`);
    if (opt["dry-run"]) {
      console.log(`\n${beat.slug}: ${beat.dur}s generated -> ${beat.exact.toFixed(6)}s cut\n${beat.motion}\n\n${LOOK}`);
      continue;
    }
    if (!opt.force && existsSync(outPath)) {
      console.log(`Cached ${beat.slug} (${(statSync(outPath).size / 1e6).toFixed(1)} MB)`);
      continue;
    }
    console.log(`\nGenerating ${beat.slug}: ${beat.dur}s -> ${outPath}`);
    const result = await generateShot({
      image,
      prompt: `${beat.motion}\n\n${LOOK}`,
      duration: beat.dur,
      ratio: "9:16",
      resolution: "720p",
      tier,
      audio: false,
      outPath,
      label: beat.slug,
    });
    if (!result.ok) throw new Error(`${beat.slug}: ${result.error}`);
    console.log(`Made ${beat.slug}: seed ${result.seed}, ${(result.bytes / 1e6).toFixed(1)} MB, ${result.seconds.toFixed(0)}s`);
  }
  process.exit(0);
}

for (const beat of BEATS) {
  const clip = resolve(MOTION, `${beat.slug}.mp4`);
  if (!existsSync(clip)) throw new Error(`Missing motion clip: ${clip}`);
}
if (!existsSync(MASTER)) throw new Error(`Missing master: ${MASTER}`);

const inputs = BEATS.flatMap((b) => ["-i", resolve(MOTION, `${b.slug}.mp4`)]);
const videoFilters = BEATS.map((b, i) =>
  `[${i}:v]trim=duration=${b.exact},setpts=PTS-STARTPTS,scale=1080:1920:flags=lanczos,fps=30,format=yuv420p[v${i}]`,
);
const concatInputs = BEATS.map((_, i) => `[v${i}]`).join("");
const total = BEATS.reduce((sum, b) => sum + b.exact, 0);
const filter = `${videoFilters.join(";")};${concatInputs}concat=n=${BEATS.length}:v=1:a=0[vout];[6:a]atrim=duration=${total},asetpts=PTS-STARTPTS[aout]`;

run("ffmpeg", [
  "-y", ...inputs, "-i", MASTER,
  "-filter_complex", filter,
  "-map", "[vout]", "-map", "[aout]",
  "-c:v", "libx264", "-preset", "slow", "-crf", "18", "-profile:v", "high", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "320k", "-ar", "48000", "-movflags", "+faststart", "-shortest", REEL,
]);

const full = resolve(MOTION, "full-system.mp4");
run("ffmpeg", [
  "-y", "-i", full,
  "-filter_complex",
  "[0:v]trim=duration=4,setpts=PTS-STARTPTS,scale=540:960:flags=lanczos,fps=30,format=yuv420p,split[fwd][copy];[copy]reverse[rev];[fwd][rev]concat=n=2:v=1:a=0[vout]",
  "-map", "[vout]", "-an", "-c:v", "libx264", "-preset", "slow", "-crf", "18", "-profile:v", "high", "-pix_fmt", "yuv420p", "-movflags", "+faststart", CANVAS,
]);

console.log(`Instagram Reel: ${REEL}`);
console.log(`Spotify Canvas: ${CANVAS}`);
