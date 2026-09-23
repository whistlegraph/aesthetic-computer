import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {buildPlan,members,readinessProblems,notesOf} from './trio-fleet-plan.mjs';
const score=JSON.parse(readFileSync(new URL('../scores/trio-chorus-doowop.mbscore',import.meta.url)));
const profiles=Object.fromEntries(members.map(m=>[m,JSON.parse(readFileSync(new URL(`../members/${m}/voice.json`,import.meta.url)))]));
const fleet=[['a',80,'CENTER REAR'],['b',80,'RIGHT FRONT'],['c',80,'RIGHT REAR'],['d',80,'LEFT REAR'],['e',80,'LEFT FRONT'],['f',80,'HELD CENTER']];
const p=buildPlan(score,profiles,fleet),time=1000;
assert(Math.abs(p.duration-64*60/88)<.001);
assert.equal(p.requiredReceivers.length,11);
assert.equal(new Set(p.events.map(e=>e.id)).size,p.events.length);
const bass=notesOf(score.voices[1],score.bpm);
assert.deepEqual(p.events.filter(e=>e.layer==='sub').map(e=>[e.t,e.note]),bass.map(e=>[e.t,e.note-12]));
assert(p.events.filter(e=>e.layer==='dmx').every(e=>[1,11,21,31].includes(e.address)&&p.events.some(n=>n.id===e.sourceEvent&&n.t===e.t&&n.dur===e.dur)));
assert.throws(()=>buildPlan(score,profiles,fleet.slice(1)));
assert(p.events.filter(e=>e.layer==='dmx').every(e=>e.command.rgb.every(n=>Number.isInteger(n)&&n<=128)&&e.command.duration===e.dur));
const hashes=Array.from({length:51},(_,i)=>'asset-'+i);
const receipts=p.requiredReceivers.map(receiverId=>({displayMode:'concert',pointerHidden:true,fullscreen:true,centerMixHash:'mix',schema:'trio-fleet-ready-v1',receiverId,ready:true,phase:'ready',arrangementHash:p.arrangementHash,instance:'test-instance',observedAt:time,clockUncertaintyMs:2,capabilities:['timestamped-start','cancel-queued','actual-vocal-pcm'],assetHashes:hashes,duration:p.duration,activeAddresses:[1,11,21,31]}));
assert.deepEqual(readinessProblems(p,receipts,hashes,time,'mix'),[]);
assert(readinessProblems(p,receipts.filter(r=>r.receiverId!=='seat-1'),hashes,time,'mix').some(x=>x.includes('seat-1')));
assert(readinessProblems(p,receipts,[],time,'mix').some(x=>x.startsWith('voices:')));
assert(readinessProblems(p,receipts,hashes,time+16,'mix').some(x=>x.includes('stale')));
for(const [key,value,needle] of [['arrangementHash','wrong','wrong arrangement'],['duration',19.2,'Trio score'],['capabilities',[],'missing timestamped'],['assetHashes',[],'missing voice asset']]) {
 const broken=receipts.map(r=>({...r,[key]:value}));assert(readinessProblems(p,broken,hashes,time,'mix').some(x=>x.includes(needle)));
}
assert(p.payloads.every(x=>!('startEpoch' in x.info)&&x.info.singNoteGains&&x.info.singLineGains));
console.log('PASS: harmony-derived bass, linked note/DMX envelopes, all-six-seat requirement, exact-asset/freshness/clock/capability gates. No playback.');
