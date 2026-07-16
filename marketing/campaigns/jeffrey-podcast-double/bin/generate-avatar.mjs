import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { generateAvatar } from "../../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const CAMPAIGN = resolve(HERE, "..");
const REPO = resolve(CAMPAIGN, "../../..");
const image = resolve(CAMPAIGN, "gens/portrait-v1.png");
const audio = resolve(REPO, "marketing/talking-head/out/.work-yc/mono16k.wav");
const prompt = readFileSync(resolve(CAMPAIGN, "motion-prompt.txt"), "utf8").trim();
const outPath = resolve(CAMPAIGN, "gens/jeffrey-podcast-avatar-standard-v1.mp4");

mkdirSync(dirname(outPath), { recursive: true });
console.log("Illy plan:");
console.log("  provider: fal.ai");
console.log("  model: fal-ai/kling-video/ai-avatar/v2/standard");
console.log(`  identity: ${image}`);
console.log(`  audio: ${audio}`);
console.log(`  output: ${outPath}`);
console.log("  stages: identity-still → audio-driven-avatar → visual-validation");

const startedAt = new Date().toISOString();
const result = await generateAvatar({
  image,
  audio,
  prompt,
  tier: "standard",
  outPath,
  label: "jeffrey-podcast-avatar-standard-v1",
});

writeFileSync(`${outPath}.illy.json`, `${JSON.stringify({
  pipeline: "jeffrey-podcast-double",
  provider: "fal.ai",
  model: "fal-ai/kling-video/ai-avatar/v2/standard",
  image,
  audio,
  prompt,
  startedAt,
  completedAt: new Date().toISOString(),
  result,
}, null, 2)}\n`);

console.log(`✓ ${outPath}`);
