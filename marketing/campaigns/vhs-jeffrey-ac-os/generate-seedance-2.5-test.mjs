#!/usr/bin/env node

import { existsSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateSeedance25ReferenceShot } from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const repo = resolve(HERE, "../../..");
const motionSource = resolve(HERE, "jeffrey-ac-os-omnihuman-v1.5.mp4");
const audioSource = resolve(HERE, "jeffrey-pvc-v2.mp3");
const motion = resolve(HERE, "seedance-test-driver-4s.mp4");
const audio = resolve(HERE, "jeffrey-pvc-seedance-test-4s.mp3");
const out = resolve(HERE, "jeffrey-ac-os-seedance-2.5-test-4s.mp4");
const promptPath = resolve(HERE, "seedance-2.5-test-prompt.txt");

function ffmpeg(args) {
  const result = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...args], {
    stdio: "inherit",
  });
  if (result.status !== 0) throw new Error(`ffmpeg exited ${result.status}`);
}

if (!existsSync(motion)) {
  ffmpeg([
    "-i", motionSource, "-t", "4",
    "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "18",
    "-pix_fmt", "yuv420p", motion,
  ]);
}

if (!existsSync(audio)) {
  ffmpeg([
    "-i", audioSource, "-t", "4",
    "-c:a", "libmp3lame", "-q:a", "2", audio,
  ]);
}

const images = [
  resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/front.png"),
  resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/three-quarter.png"),
  resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/profile.png"),
  resolve(repo, "xbox/assets/jeffrey-meshy-v6/refs/full-body.png"),
  resolve(HERE, "source-v3-identity.png"),
  resolve(repo, "papers/iclc-2027-notespatial/figures/ac-native-laptop.png"),
  resolve(repo, "marketing/campaigns/keymaps-cover/refs/ac-native-notepat.png"),
];

const prompt = `[Identity]
[Image1], [Image2], [Image3], and [Image4] are four views of the same real person, Jeffrey Alan Scudder: front, three-quarter, profile, and full body. Together they exclusively define Jeffrey's facial geometry, eye spacing, long narrow nose, slim jaw and chin, ears, hairline, uneven medium-brown hair, skin texture, body proportions, and clothing. Preserve the same Jeffrey continuously in every frame and at every angle. Do not use the white photo backgrounds.

[Scene and prop]
[Image5] is the exact opening frame and defines the early-1990s community-access television studio, framing, lighting, Jeffrey's initial pose and clothing, the black ThinkPad in his hands, and the Aesthetic Computer screen. [Image6] defines the exact laptop hardware. [Image7] defines the exact screen interface. Keep one coherent laptop rigidly supported throughout; do not replace or redesign it.

[Motion]
[Video1] supplies only Jeffrey's body movement, head movement, hand gesture, weight shift, forward approach, camera movement, timing, and blocking. Do not use or inherit the person's face, identity, skin, hair, clothing, laptop, studio, image texture, or audio from [Video1]. Jeffrey's appearance comes only from [Image1] through [Image5].

[Audio]
[Audio1] is the exact Jeffrey voice recording and dialogue timing. Preserve this audio and synchronize Jeffrey's mouth naturally to it. Do not synthesize a different speaker, change the words, add music, or add narration.

[Output]
Create one continuous four-second vertical 9:16 photorealistic community-TV take beginning from [Image5]. Jeffrey speaks naturally while his head, shoulders, torso, weight, and free hand remain alive. Natural blinking and mouth motion; restrained facial expression; ordinary skin texture; no beauty filter. Preserve Jeffrey's identity, facial proportions, hair, clothing, laptop, screen, set geometry, equipment count, and lighting throughout. No cuts, duplicated objects, extra fingers, warped laptop, face morphing, enlarged eyes, widened jaw, generic presenter face, captions, logos, or watermarks.`;

writeFileSync(promptPath, `${prompt}\n`);

if (!existsSync(out)) {
  const result = await generateSeedance25ReferenceShot({
    images,
    videos: [motion],
    audios: [audio],
    prompt,
    duration: "4",
    ratio: "9:16",
    resolution: "480p",
    audio: true,
    outPath: out,
    label: "vhs-jeffrey-seedance-2.5-test",
  });
  if (!result.ok) throw new Error(result.error);
}

console.log(`✓ ${out}`);
