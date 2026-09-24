import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {compileLook} from './compile-look.mjs';
import {lookAt,paintLook,loadTimeline,LOOKS} from './score-look.mjs';
import {fixtureFrame,heldSlots} from './fixture-frame.mjs';
const score={dur:4,seats:6,ring:5,center:5,gain:.5,movements:[{t0:0,t1:4}],lanes:[
  {center:true,events:[{t:.5,dur:1,hz:261.63,g:.6},{t:2,dur:.6,hz:369.99,g:.6}]}]};
const timeline=compileLook(score);
test('held notes route only to center, change color and fall away',()=>{
  assert.equal(lookAt(timeline,1,0).energy,0);
  assert.ok(lookAt(timeline,1,5).energy>.5);
  assert.equal(lookAt(timeline,1,5).pitch,60);
  assert.equal(lookAt(timeline,2.3,5).pitch,66);
  assert.notDeepEqual(lookAt(timeline,1,5).rgb,lookAt(timeline,2.3,5).rgb);
  assert.ok(lookAt(timeline,3.8,5).energy<lookAt(timeline,2.3,5).energy);
});
test('clock seeks are deterministic; invalid, pre-roll and finished clocks black out',()=>{
  const a=lookAt(timeline,1.24,5);lookAt(timeline,3,5);
  assert.deepEqual(lookAt(timeline,1.24,5),a);
  for(const t of [-1,4,20,NaN,Infinity])assert.deepEqual(lookAt(timeline,t,5).rgb,[0,0,0]);
});
test('fixture mapping reserves d041-043 and uses actual room addresses',()=>{
  const frame=fixtureFrame(timeline,1);
  assert.deepEqual(frame.room.map(f=>f.address),[1,11,31,21]);
  const slots=heldSlots(timeline,1);
  assert.deepEqual(slots.slice(40,43),frame.held.rgb);
  assert.equal(slots.filter((v,i)=>v&&!(i>=40&&i<=42)).length,0);
});
test('full-byte loading works and detects malformed timelines',()=>{
  const bytes=new TextEncoder().encode(JSON.stringify(timeline));
  assert.deepEqual(loadTimeline({readFileBytes:()=>bytes}),timeline);
  assert.throws(()=>loadTimeline({readFileBytes:()=>new TextEncoder().encode('{}')}));
});
test('each raster grammar stays within 24 primitives, one wipe, one optional note',()=>{
  for(let section=0;section<LOOKS.length;section++) {
    const local={...timeline,movements:Array.from({length:section+1},()=>({t0:0,t1:4}))};
    let primitives=0,wipes=0,labels=0;
    const primitive=(...args)=>{primitives++;for(const arg of args)if(typeof arg==='number')assert.ok(Number.isFinite(arg));};
    paintLook({screen:{width:1280,height:720},wipe(){wipes++;},ink(){},line:primitive,box:primitive,write(){labels++;}},local,1,5,{noteLabels:true});
    assert.equal(wipes,1);assert.ok(primitives<=24);assert.equal(labels,1);
  }
  assert.equal(new Set(LOOKS.map(l=>l.mode)).size,11);
});
test('real score center spans palettes, has brighter peaks and bounded channel transitions',async()=>{
  const real=JSON.parse(await readFile(new URL('./notespatial-look.nstimeline',import.meta.url)));
  let max=0,delta=0;const colors=new Set();
  for(let f=1;f<real.frames;f++)for(let c=0;c<3;c++) {
    const i=f*30+25+c;
    max=Math.max(max,real.data[i]);delta=Math.max(delta,Math.abs(real.data[i]-real.data[i-30]));
  }
  for(const m of real.movements)colors.add(lookAt(real,(m.t0+m.t1)/2,5).rgb.join(','));
  assert.ok(max>190);assert.ok(max<=235);assert.ok(delta<=18);assert.ok(colors.size>=9);
});
