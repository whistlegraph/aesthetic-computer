import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
const source = await readFile(new URL('../oskiewar.js', import.meta.url), 'utf8');

test('a decal-only native host bakes pool marks once and draws one retained surface', () => {
  const calls = { stamps: 0, uploads: 0, draws: 0, clears: 0 };
  const api = new Function('decalClear', 'decalStamp', 'decalMeshUpload', 'decalMesh', `
    const noop=()=>{};
    let runtime=()=>({monotonicUs:0}),capabilities=()=>({platform:'macos-native'}),
      wipe=noop,box=noop,line=noop,triangle=noop,write=noop,systemWrite=noop;
    ${source}
    return { configureWorldMap, clearPoolDecals, addDecal, drawDecals,
      state:()=>({marks:poolDecalCount,geometry:decals.length}) };
  `)(() => { calls.clears++; return true; }, (...stamp) => {
    assert.equal(stamp.length, 12); assert.ok(stamp.every(Number.isFinite)); calls.stamps++; return true;
  }, (vertices, faces) => {
    assert.ok(vertices instanceof Float32Array && faces instanceof Float32Array);
    assert.ok(vertices.length > 0 && faces.length > 0); calls.uploads++; return 0;
  }, (handle, camera, ...bounds) => {
    assert.equal(handle, 0); assert.equal(camera.length, 27); assert.equal(bounds.length, 4); calls.draws++;
  });
  api.configureWorldMap('skatepark', 'pool');
  api.clearPoolDecals();
  for (let i = 0; i < 1800; i++) api.addDecal({ kind:'wheelmark', x:900+i%60*5,
    z:-150+Math.floor(i/60)*10, size:3, angle:0, strength:1 });
  const stamps = calls.stamps;
  assert.ok(stamps >= 1800);
  assert.deepEqual(api.state(), { marks:1800, geometry:0 });
  for (let i=0;i<60;i++) api.drawDecals([180,180,180]);
  assert.equal(calls.stamps, stamps, 'old marks are never resubmitted');
  assert.equal(calls.uploads, 1, 'the pool mesh crosses the bridge once');
  assert.equal(calls.draws, 60, 'one surface draw per frame regardless of mark count');
  api.clearPoolDecals();
  api.drawDecals([180,180,180]);
  assert.equal(calls.draws, 60, 'clearing the map removes its marks');
});
