// Two identity-preserving loop masters, using the shared resumable FAL queue.
// Run from any directory: node marketing/pals/dripped/generate.mjs
import { existsSync, mkdirSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { generateShot } from '../../../pop/lib/fal.mjs';
const here = dirname(fileURLToPath(import.meta.url));
const out = resolve(here, '../../podcast/out/pals/turnarounds');
mkdirSync(out, { recursive: true });
const locked = 'Locked camera. Preserve the exact connected contours, scale, orientation and placement of both cyan glyphs for the whole clip. Do not rotate the logo or turn it into a creature. No cuts, zooms, text, new objects, or shape morphing. One smooth gentle cycle returning to the exact starting appearance. The silhouette must stay recognizable at every instant.';
const jobs = [
  { slug: 'psycho-dripped', prompt: `Animate only the liquid surface: glossy cyan, magenta and lime highlights travel slowly along the existing glyph contours. Hanging gummy droplets stretch very slightly, wobble, and settle back without detaching. A subtle light shimmer breathes once. The black background remains still. ${locked}` },
  { slug: 'psycho-dripped-pink', prompt: `The pink sunset clouds billow very slowly, with soft rose sunlight and atmospheric rays breathing gently behind the cyan glyphs. The liquid surface glints subtly and the tiny hanging drips sway almost imperceptibly, then settle. The emblem remains steady and clear against the photographic pink sky. ${locked}` },
];
const results = await Promise.all(jobs.map(async ({slug, prompt}) => {
  const image = resolve(here, 'stills', `${slug}.png`);
  const outPath = resolve(out, `${slug}.mp4`);
  if (existsSync(outPath)) return {slug, cached: true};
  const recipe = {provider:'fal.ai', model:'bytedance/seedance-2.0/image-to-video', prompt, source:`stills/${slug}.png`, endFrame:'same as source', duration:6, aspectRatio:'1:1', resolution:'1080p', audio:false};
  writeFileSync(resolve(here, `${slug}.motion.json`), JSON.stringify(recipe, null, 2)+'\n');
  const result = await generateShot({image, endImage:image, prompt, duration:'6', ratio:'1:1', resolution:'1080p', tier:'standard', audio:false, outPath, label:slug});
  writeFileSync(`${outPath}.result.json`, JSON.stringify({...recipe,...result,generatedAt:new Date().toISOString()}, null, 2)+'\n');
  if (!result.ok) throw Error(`${slug}: ${result.error}`);
  return {slug,...result};
}));
console.log(JSON.stringify(results, null, 2));
