import test from 'node:test';
import assert from 'node:assert/strict';
import {manifest,createThemeRenderer} from './theme.mjs';
const probe=()=>{const calls=[];const driver=Object.fromEntries(['clear','sprite','line','disc','rect'].map(k=>[k,(...a)=>calls.push([k,...a])]));return {calls,renderer:createThemeRenderer(driver)};};
const pose={seat:0,head:[120,120],radius:25,segments:[[[120,150],[120,180],8]],hand:[130,170],facing:1};
test('theme switching changes only presentation and leaves host pose intact',()=>{
 const {renderer,calls}=probe(),before=structuredClone(pose);renderer.fighter(pose);
 assert.ok(calls.some(c=>c[0]==='sprite'));calls.length=0;renderer.setTheme('flat');renderer.fighter(pose);
 assert.ok(calls.some(c=>c[0]==='disc'));assert.deepEqual(pose,before);
 assert.throws(()=>renderer.setTheme('missing'),RangeError);assert.equal(renderer.theme,'flat');
});
test('seat identity and facing stay independent of screen position',()=>{
 const {renderer,calls}=probe();renderer.fighter({...pose,seat:1,head:[20,120],facing:-1});
 assert.ok(calls.some(c=>c[0]==='sprite'&&c[2]===manifest.regions.xboxHead));
 const gun=calls.find(c=>c[0]==='sprite'&&c[2]===manifest.regions.gun);assert.equal(gun.at(-1),true);
});
test('atlas regions remain inside retained image',()=>{
 for(const [x,y,w,h]of Object.values(manifest.regions)){
  assert.ok(x>=0&&y>=0&&w>0&&h>0&&x+w<=manifest.atlasSize[0]&&y+h<=manifest.atlasSize[1]);
 }
});
test('missing native image capability is rejected explicitly',()=>{
 assert.throws(()=>createThemeRenderer({}),/Missing clear/);
 const {renderer}=probe();assert.throws(()=>renderer.fighter({...pose,seat:2}),/Unknown seat/);
});
