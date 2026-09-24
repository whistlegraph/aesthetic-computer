import assert from 'node:assert/strict';
import fs from 'node:fs';
const score=JSON.parse(fs.readFileSync(new URL('./score.json',import.meta.url)));
assert.equal(score.events.length,2947);
assert.equal(score.master,.25);
assert.equal(score.seats.length,6);
assert.equal(score.dmx.addresses[4],null);
for(const e of score.events){assert(e.t>=0&&e.t<score.duration);assert(e.seat==='sub'||Number.isInteger(e.seat)&&e.seat>=0&&e.seat<6);assert(score.samples[e.sample]);assert(Number.isFinite(e.gain)&&e.gain>=0);}
assert(score.events.every((e,i)=>!i||e.t>=score.events[i-1].t));
for(const [name,s] of Object.entries(score.samples)){const b=fs.readFileSync(new URL(s.url,import.meta.url));assert.equal(b.toString('ascii',0,4),'RIFF');assert.equal(b.toString('ascii',8,12),'WAVE');}
assert(score.events.some(e=>e.seat==='sub'));
assert.equal(new Set(score.events.filter(e=>e.seat!=='sub').map(e=>e.seat)).size,6);
console.log('Validated chronological 2,947-event score, six seats + sub, sample files and master.');
