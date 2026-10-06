import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import vm from 'node:vm';

const disk = await readFile(new URL('../public/aesthetic.computer/lib/disk.mjs', import.meta.url), 'utf8');
// Exercise the actual runtime recorder without booting the graphics worker.
const recorder = disk.slice(disk.indexOf('const previewEvidence ='), disk.indexOf('const alphabet ='));
function runtime(search) {
  const requests = [], evidence = [];
  const context = vm.createContext({ location: { search }, URLSearchParams, Date, JSON, Math,
    console: { log() {}, warn() {}, error() {}, info() {} }, performance: { now: () => 0 },
    createPreviewEvidence: () => ({ record: (...args) => evidence.push(args) }),
    send() {}, fetch: async (...args) => { requests.push(args); }, setTimeout: () => 1, clearTimeout() {},
  });
  vm.runInContext(recorder + '\nglobalThis.recorder = pieceRuns;', context);
  return { context, requests, evidence };
}
test('private Whistlegraph previews keep evidence locally and never upload console content', () => {
  const { context, requests, evidence } = runtime('?noauth=true&preview=walkieware');
  assert.equal(context.recorder.start({ slug: 'private-draft' }), null);
  vm.runInContext("console.log('private prompt'); recorder.error(Error('private source')); recorder.flush();", context);
  assert.equal(requests.length, 0);
  assert.equal(evidence[0][1], 'private prompt');
});
test('ordinary public pieces retain their existing run/error logging', () => {
  const { context, requests } = runtime('');
  assert.ok(context.recorder.start({ slug: 'notepat' }));
  vm.runInContext("console.log('public'); recorder.error(Error('test')); recorder.flush();", context);
  assert.deepEqual(requests.map(([, options]) => JSON.parse(options.body).phase), ['start', 'error', 'log']);
});
