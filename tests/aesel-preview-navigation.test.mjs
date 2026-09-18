import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import vm from 'node:vm';
import { noPaintHistoryTarget, NOPAINT_SESSION_SEED_KEY } from '../system/public/aesthetic.computer/lib/nopaint-navigation.mjs';

// Execute the actual BIOS history paths without starting its GPU/audio runtime.
const bios = readFileSync(new URL('../system/public/aesthetic.computer/bios.mjs', import.meta.url), 'utf8');
const between = (start, end) => bios.slice(bios.indexOf(start), bios.indexOf(end, bios.indexOf(start)));
const helpers = between('function preserveViewParams(', '\nfunction resolveBackgroundFillSpec');
const initialize = between('  preservedParams = {};', '\n  if (debug) {');
const cleanup = between('        if (!checkPackMode() && !frozenUrlPath) {', '\n      // if (currentPiece !== null)');
const navigate = between('      if ((content.pieceCount > 0 || content.alias === true)', '\n      UI.spinnerReset();');
const refresh = between('    if (type === "refresh") {', '\n    if (type === "web") {');

function view(query = '?nogap=true&nolabel=true&autoreload=true&zoom=2&code=auth-secret&state=auth-state') {
  const window = { location: new URL('https://aesthetic.computer/@jeffrey/rinara' + query), parent: { postMessage() {} } };
  const writes = [];
  const history = Object.fromEntries(['pushState', 'replaceState'].map(method => [method, (_, __, path) => {
    writes.push([method, path]);
    window.location = new URL(path, window.location);
  }]));
  window.history = history;
  const ctx = vm.createContext({ window, history, document: { title: '' }, URL, URLSearchParams,
    noPaintHistoryTarget, NOPAINT_SESSION_SEED_KEY, sessionStorage: {setItem() {}}, console: { log() {}, warn() {} }, location: window.location, preservedParams: {}, updateAutoReload: false,
    resolution: { gap: 0, nolabel: true, autoreload: true }, frozenUrlPath: null, checkPackMode: () => false,
    isKidlispSource: text => text?.startsWith('('), encodeKidlispForUrl: encodeURIComponent,
  });
  vm.runInContext(helpers + initialize, ctx);
  const load = (content) => { ctx.content = { pieceCount: 1, fromHistory: false, ...content }; vm.runInContext(cleanup + navigate, ctx); };
  return { ctx, window, writes, load };
}
function embedded(url) {
  for (const key of ['nogap', 'nolabel', 'autoreload']) assert.equal(url.searchParams.get(key), 'true', key);
  assert.equal(url.searchParams.get('zoom'), '2');
  assert.equal(url.searchParams.has('code'), false);
  assert.equal(url.searchParams.has('state'), false);
}

test('owned-piece load, source push, browser history and reload retain view flags', () => {
  const v = view();
  for (const fromHistory of [false, true, false]) {
    v.load({ text: '@jeffrey/rinara', path: 'media/@jeffrey/piece/rinara', fromHistory });
    embedded(v.window.location);
    assert.equal(v.window.location.pathname, '/@jeffrey/rinara');
    // A browser reload boots exactly this URL, with owned route for channel resubscription.
    embedded(new URL(v.window.location.href));
  }
});

test('Kidlisp, clock, prompt and painting navigation preserve view flags and target hash', () => {
  for (const content of [
    { text: '(wipe red)' },
    { path: 'aesthetic.computer/disks/prompt', params: ['(wipe red)'], text: 'prompt~(wipe red)' },
    { text: '*wibe', clockShortcode: '*wibe' },
    { text: '/prompt' },
    { text: 'painting', path: 'aesthetic.computer/disks/painting', hash: 'abc123' },
  ]) {
    const v = view(); v.load(content); embedded(v.window.location);
    if (content.hash) assert.equal(v.window.location.hash, '#abc123');
  }
});

test('explicit worker refresh restores all flags even after another caller stripped them', () => {
  const v = view();
  v.window.location = new URL('https://aesthetic.computer/@jeffrey/rinara#target');
  v.ctx.type = 'refresh';
  vm.runInContext('(function(){' + refresh + '})()', v.ctx);
  embedded(v.window.location);
  assert.equal(v.window.location.hash, '#target');
});

test('DAW dimensions and boolean empty parameters survive cleanup; pack views never write history', () => {
  const v = view('?daw&width=320&height=200&density=0&noauth&preview&icon');
  v.load({ text: '@jeffrey/rinara' });
  for (const key of ['daw', 'noauth', 'preview', 'icon']) assert.equal(v.window.location.searchParams.get(key), '');
  assert.equal(v.window.location.searchParams.get('density'), '0');
  assert.equal(v.window.location.searchParams.get('width'), '320');
  assert.equal(v.window.location.searchParams.get('height'), '200');
  v.ctx.checkPackMode = () => true;
  const count = v.writes.length;
  v.load({ text: '@jeffrey/other' });
  assert.equal(v.writes.length, count);
});
