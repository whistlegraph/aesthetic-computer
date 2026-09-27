import test from 'node:test';
import assert from 'node:assert/strict';
import { createNativeHost } from '../lib/oskiewar-host.mjs';

test('native theme scales destination coordinates and preserves atlas UVs and depth', () => {
  const calls = [];
  const api = { screen: { width: 683, height: 384 },
    system: { readFile: path => path === '/tmp/oskiewar-gpu' ? '1' : '' },
    wipe() {}, gpuBegin: () => true, themeReady: () => true, themeAssetReady: id => id >= 0 && id < 5,
    themeSprite: (...args) => { calls.push(args); return true; },
    themeQuad: (...args) => { calls.push(args); return true; } };
  const host = createNativeHost(api);
  assert.equal(host.themeReady(), true);
  assert.equal(host.themeAssetReady(3), true);
  assert.equal(host.themeAssetReady(4), true);
  assert.equal(host.themeAssetReady(5), false);
  assert.equal(host.themeSprite(1,1,2,3,4,10,20,30,40,.4,true,-.5), false);
  host.wipe(0,0,0);
  assert.equal(host.themeSprite(1,1,2,3,4,10,20,30,40,.4,true,-.5), true);
  assert.deepEqual(calls.pop(), [1,1,2,3,4,5,10,15,20,.4,true,-.5,true]);
  assert.equal(host.themeQuad(0,0,0,1672,941,0,0,.8,100,0,.8,100,100,.8,0,100,.8), true);
  assert.deepEqual(calls.pop(), [0,0,0,1672,941,0,0,.8,50,0,.8,50,50,.8,0,50,.8]);
  assert.equal(host.themeQuad(0,0,0,10,10,0,0,0), false);
  assert.equal(host.themeSprite(3,1,2,3,4,10,20,30,40,.4,true,-.5,false), true);
  assert.deepEqual(calls.pop(), [3,1,2,3,4,5,10,15,20,.4,true,-.5,false]);
});

test('older native clients decline a texture theme without affecting their renderer', () => {
  const host = createNativeHost({ screen: { width: 683, height: 384 },
    system: { readFile: () => '' } });
  assert.equal(host.themeReady(), false);
  assert.equal(host.themeAssetReady(2), false);
  assert.equal(host.themeSprite(0,0,0,1,1,0,0,1,1,0,false,0), false);
});

test('base-theme native clients do not claim support for new atlases', () => {
  const host = createNativeHost({ screen: { width: 683, height: 384 },
    system: { readFile: path => path === '/tmp/oskiewar-gpu' ? '1' : '' },
    themeReady: () => true });
  assert.equal(host.themeReady(), true);
  assert.equal(host.themeAssetReady(2), false);
  assert.equal(host.themeAssetReady(3), false);
});
