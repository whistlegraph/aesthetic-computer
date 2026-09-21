// Read-only deployment check. Never prints screen contents or sends input.
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { resolve, join } from 'node:path';
import { homedir } from 'node:os';
import { pathToFileURL } from 'node:url';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
// A newer verifier can check an already-installed immutable release unchanged.
const root = process.argv[2] ? resolve(process.argv[2]) : resolve(import.meta.dirname, '../..');
const { captureFrame } = await import(pathToFileURL(join(root, 'slab/bin/frame.mjs')));
const { createComputerUseClient } = await import(pathToFileURL(join(root, 'slab/lib/computer-use-client.mjs')));
const { inspectMachineLeases } = await import(pathToFileURL(join(root, 'slab/lib/computer-use-lease.mjs')));
const manifest = JSON.parse(readFileSync(join(root, 'release.json')));
for (const [path, expected] of Object.entries(manifest.sha256)) {
  assert.equal(createHash('sha256').update(readFileSync(join(root, path))).digest('hex'), expected, path);
}
const installed = JSON.parse(readFileSync(join(homedir(), '.local/share/slab/computer-use/installed.json')));
assert.equal(installed.runtime, root);
assert.equal(installed.revision, manifest.revision);
const app = join(homedir(), 'Applications/SlabMenubar.app');
execFileSync('/usr/bin/codesign', ['--verify', '--deep', '--strict', app], { stdio: 'pipe' });
const uuid = execFileSync('/usr/bin/dwarfdump', ['--uuid', join(app, 'Contents/MacOS/slab-menubar')], { encoding: 'utf8' }).split(/\s+/)[1];
assert.equal(uuid, manifest.nativeUUID, 'Native build UUID');
const imported = await import(pathToFileURL(join(homedir(), '.local/bin/frame.mjs')));
assert.equal(imported.captureFrame, captureFrame, 'Captutor must import the installed Frame');
const client = createComputerUseClient({ allowedTools: ['frame', 'frame_click', 'frame_drag', 'puppet_snapshot', 'puppet_choose'], timeoutMs: 5000 });
const catalog = await client.discover();
assert.equal(catalog.tools.length, 5);
for (const name of ['frame_click', 'frame_drag']) {
  const schema = catalog.tools.find(t => t.name === name).inputSchema.properties;
  assert(schema.holdMs && schema.verify && schema.settleMs, name + ' speed controls');
}
const { chromium } = await import(pathToFileURL(join(root, 'node_modules/playwright-core/index.mjs')));
assert.equal(typeof chromium.connectOverCDP, 'function');
assert.deepEqual(inspectMachineLeases(), [], 'Do not interrupt active input');
const session = 'deploy_' + Date.now();
const options = { memory: true, screen: true, crop: [0, 0, 32, 32], noOCR: true, noVisual: true, quietOverlay: true };
const a = await captureFrame('local', { ...options, session: session + 'a', baseline: true });
const b = await captureFrame('local', { ...options, session: session + 'b', baseline: true });
const diff = await captureFrame('local', { ...options, session: session + 'a', diff: true });
for (const { env, jpg } of [a, b, diff]) {
  assert.equal(env.capture, 'ok', 'Native capture permission/availability');
  assert.equal(env.ax?.trusted, true, 'Native Accessibility permission required for input');
  assert(jpg?.length, 'Native pixels');
  for (const cap of ['target-guard-v1', 'guarded-click-v1', 'ax-verify-v1', 'click-hold-v1', 'guarded-drag-v1'])
    assert(env.nativeCapabilities.includes(cap), cap);
}
assert.notEqual(a.env.observation.id, b.env.observation.id);
assert.equal(diff.env.diff_baseline, 'matched');
assert.deepEqual(inspectMachineLeases(), []);
console.log(JSON.stringify({ ok: true, revision: manifest.revision, nativeUUID: uuid,
  hashes: Object.keys(manifest.sha256).length, mcp: 'Frame + Puppet', nativeCapture: true,
  accessibility: true, sessionIsolation: true, captutorFrameImport: true, captutor: installed.captutor }));
