import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

const source = readFileSync(new URL('../oven/grabber.mjs', import.meta.url), 'utf8');
const interception = source.slice(
  source.indexOf('async function interceptSelfRequests(page)'),
  source.indexOf('/**\n * Capture frames from a KidLisp piece'),
);
const backdrops = source.slice(
  source.indexOf('const BACKDROP_RENDER_VERSION'),
  source.indexOf('// Notepat.com OG Preview Image'),
).replaceAll('export async function', 'async function').replaceAll('export function', 'function');

test('capture requests bypass service workers and block update checks without blocking art', async () => {
  let onRequest;
  const setup = [];
  const page = {
    async setBypassServiceWorker(value) { setup.push(['bypass', value]); },
    async setRequestInterception(value) { setup.push(['intercept', value]); },
    on(event, callback) { assert.equal(event, 'request'); onRequest = callback; },
  };
  const context = vm.createContext({
    URL, console, BLOCKED_URL_PATTERNS: [], DROPPABLE_API_PATTERNS: [],
  });
  vm.runInContext(interception, context);
  await context.interceptSelfRequests(page);
  assert.deepEqual(setup, [['bypass', true], ['intercept', true]]);

  for (const path of ['/api/version', '/api/version?current=aaaaaaa', '/api/store-kidlisp?code=4bb', '/aesthetic.computer/lib/disk.mjs']) {
    const actions = [];
    await onRequest({
      url: () => `https://aesthetic.computer${path}`,
      abort: async reason => actions.push(['abort', reason]),
      continue: async () => actions.push(['continue']),
    });
    assert.deepEqual(actions, path.startsWith('/api/version')
      ? [['abort', 'blockedbyclient']] : [['continue']]);
  }
});

function harness() {
  const objects = new Map([
    ['backdrop/kidlisp/pool/4bb.webp', Buffer.alloc(50000)],
    ['backdrop/kidlisp/2026-10-15.webp', Buffer.alloc(50000)],
  ]);
  const grabs = [], reads = [];
  class Command { constructor(input) { this.input = input; } }
  class ListObjectsV2Command extends Command {}
  class HeadObjectCommand extends Command {}
  class PutObjectCommand extends Command {}
  const context = vm.createContext({
    process: { env: {} }, Buffer, console: { log() {}, warn() {}, error() {} },
    ListObjectsV2Command, HeadObjectCommand, PutObjectCommand,
    SPACES_BUCKET: 'test', SPACES_CDN_BASE: 'https://art.example',
    getTodayKey: () => '2026-10-15', getDayOfYear: () => 0,
    formatHits: String,
    fetchTopKidlispHits: async () => [{ code: '4bb', owner: { handle: '@jeffrey' }, hits: 10 }],
    grabPiece: async (piece, options) => {
      grabs.push({ piece, options });
      return { success: true, buffer: Buffer.alloc(50001) };
    },
    spacesClient: { async send(command) {
      const input = command.input;
      if (command instanceof PutObjectCommand) { objects.set(input.Key, input.Body); return {}; }
      reads.push(input);
      if (command instanceof HeadObjectCommand) {
        if (!objects.has(input.Key)) throw Error('not found');
        return {};
      }
      return { Contents: [...objects].filter(([key]) => key.startsWith(input.Prefix))
        .map(([Key, data]) => ({ Key, Size: data.length, LastModified: new Date() })) };
    } },
  });
  vm.runInContext(backdrops, context);
  return { context, objects, grabs, reads };
}

test('legacy pool and daily clips cannot be selected, including explicit piece requests', async () => {
  const { context, reads } = harness();
  assert.equal(await context.pickBackdropUrl({ code: '$4bb' }), null);
  assert.equal(await context.getLatestBackdropUrl(), null);
  assert.ok(reads.every(input => (input.Prefix || input.Key).startsWith('backdrop/kidlisp/v2/')));
});

test('pool warming and daily fallback use fresh grab keys and versioned CDN URLs', async () => {
  const { context, grabs } = harness();
  const daily = await context.generateKidlispBackdrop();
  assert.equal(daily.url, 'https://art.example/backdrop/kidlisp/v2/2026-10-15.webp');
  assert.equal((await context.pickBackdropUrl()).url, daily.url);
  await context.warmBackdropPool({ size: 1 });
  const pick = await context.pickBackdropUrl({ code: '$4bb' });
  assert.equal(pick.url, 'https://art.example/backdrop/kidlisp/v2/pool/4bb.webp');
  assert.equal(pick.pool, 1);
  assert.equal(grabs.length, 2);
  assert.ok(grabs.every(grab => grab.options.cacheKey === 'backdrop-v2'));
  await context.warmBackdropPool({ size: 1 });
  await context.generateKidlispBackdrop();
  assert.equal(grabs.length, 2, 'fresh captures remain reusable');
});
