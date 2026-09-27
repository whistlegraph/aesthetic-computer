#!/usr/bin/env node
// Default: non-billing plan. --submit explicitly starts or resumes ONE Meshy job.
import { createHash } from 'node:crypto';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const dir = dirname(fileURLToPath(import.meta.url));
const endpoint = 'meshy/v7.1/image-to-3d';
const input = {
  model_type: 'standard', topology: 'triangle', target_polycount: 30000,
  symmetry_mode: 'auto', should_remesh: true, should_texture: true,
  enable_pbr: true, enable_rigging: false, enable_animation: false,
  enable_safety_checker: true,
  texture_prompt: 'Aesel donkey: blue-gray colored-pencil fur, cream muzzle and belly, dark eyes visible above defining small bright-red reading glasses, dark mane, hooves and tail tuft. Preserve reference identity and visible pencil strokes. Matte nonmetallic fur, red glasses with clear lenses.',
};
const reference = resolve(dir, 'reference.png');
const plan = { provider: 'fal', endpoint, reference, input, estimatedModelUSD: 1.20,
  rigging: 'Separate local quadruped rig required; Meshy API rigging is humanoid-only.' };
if (!process.argv.includes('--submit')) {
  console.log(JSON.stringify(plan, null, 2));
  process.exit(0);
}
if (!existsSync(reference)) throw Error('Inspect and save the neutral-pose reference as reference.png first.');
const resultFile = resolve(dir, 'result.json');
const queueFile = resolve(dir, 'queue.json');
let result = existsSync(resultFile) ? JSON.parse(readFileSync(resultFile)).result : null;
let key = process.env.FAL_KEY;
if (!key) {
  const envPath = resolve(dir, '../../..', 'aesthetic-computer-vault/.devcontainer/envs/devcontainer.env');
  if (existsSync(envPath)) key = readFileSync(envPath, 'utf8').split('\n')
    .find(line => line.startsWith('FAL_KEY='))?.slice(8).trim().replace(/^['"]|['"]$/g, '');
}
if (!key) throw Error('FAL_KEY unavailable');
const headers = { Authorization: `Key ${key}`, 'Content-Type': 'application/json' };
const json = async (url, options = {}) => {
  const response = await fetch(url, { headers, signal: AbortSignal.timeout(120000), ...options });
  if (!response.ok) throw Error(`fal HTTP ${response.status}; receipt retained, no automatic resubmission`);
  return response.json();
};
let queue = existsSync(queueFile) ? JSON.parse(readFileSync(queueFile)) : null;
if (!result) {
  if (!queue) {
    const bytes = readFileSync(reference);
    const requestInput = { ...input, image_url: `data:image/png;base64,${bytes.toString('base64')}` };
    queue = await json(`https://queue.fal.run/${endpoint}`, { method: 'POST', body: JSON.stringify(requestInput) });
    writeFileSync(queueFile, JSON.stringify(queue, null, 2));
    console.log(`Submitted one model: ${queue.request_id}`);
  }
  for (let poll = 0; poll < 240; poll++) {
    const status = await json(queue.status_url);
    if (poll % 6 === 0) console.log(status.status);
    if (status.status === 'COMPLETED') {
      result = await json(queue.response_url);
      if (!result.model_glb?.url) throw Error('No model returned; receipt retained');
      writeFileSync(resultFile, JSON.stringify({ ...plan,
        reference: 'reference.png', referenceSha256: createHash('sha256').update(readFileSync(reference)).digest('hex'),
        characterBrief: '../donkey-character.md', requestId: queue.request_id,
        generatedAt: new Date().toISOString(), result }, null, 2));
      break;
    }
    if (status.status === 'FAILED' || status.error) throw Error('Generation failed; no automatic retry or fallback');
    await new Promise(done => setTimeout(done, 5000));
  }
  if (!result) throw Error('Job still pending; rerun --submit to resume the saved receipt');
}
for (const [name, file] of [['donkey.glb', result.model_glb], ['preview.png', result.thumbnail], ['donkey.fbx', result.model_urls?.fbx]]) {
  if (!file?.url || existsSync(resolve(dir, name))) continue;
  const response = await fetch(file.url, { signal: AbortSignal.timeout(120000) });
  if (!response.ok) throw Error(`Download ${name}: HTTP ${response.status}`);
  writeFileSync(resolve(dir, name), Buffer.from(await response.arrayBuffer()));
  console.log(`Saved ${name}`);
}
