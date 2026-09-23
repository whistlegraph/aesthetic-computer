import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {splitVocalLines} from './chorus-arrange.mjs';

const score=JSON.parse(readFileSync(new URL('../scores/trio-iii-chorus.mbscore',import.meta.url)));
const parts=score.voices.map(v=>splitVocalLines(v).map((line,i)=>({...line,role:v.lineRoles[i],gain:v.lineGains[i]})));
const near=(a,b)=>Math.abs(a-b)<1e-4;
const end=line=>line.at+line.notes.reduce((n,[,d])=>n+d,0);
const scale=new Set([1,2,4,6,7,9,11]);
assert.equal(parts.length,3);
for(let i=0;i<parts.length;i++) {
  const v=score.voices[i];
  assert.equal(parts[i].length,v.lineRoles.length);
  assert.equal(parts[i].length,v.lineGains.length);
  assert.equal(v.double,false);
  assert.equal(v.previewInstrument,'sine');
  assert(/\((Enhanced|Premium)\)$/.test(v.singVoice),'No basic voices in this chorus');
  for(const [j,line] of parts[i].entries()) {
    if(j) assert(line.at>=end(parts[i][j-1])-1e-4,'One mouth cannot sing two parts at once');
    for(const [n] of line.notes) if(n!=='r') assert(scale.has(n%12),'D-major harmony');
    if(line.role!=='lead' && line.at<64) assert(line.gain<=.7,'Backing must leave room for the lead');
    if(i!==2) assert(end(line)<=40+1e-4 || line.at>=44-1e-4,'Frisbee entrance must be exposed');
  }
  assert(near(end(parts[i].at(-1)),95),'Every member shares the final cadence');
  assert.equal(parts[i].at(-1).role,'hum');
}
assert.equal(parts[0][0].at,0,'Immediate vocal opening');
const warm=parts[0].find(l=>l.lyrics==='I run warm and I am car-ried');
assert(near(warm.at,8.5));
assert.deepEqual(warm.notes,[[62,.5],[59,1],[61,1],[62,.5],[64,.5],[62,.5],[61,1],[57,2]]);
const warmHarmony=parts.slice(1).map(ls=>ls.find(l=>near(l.at,8.5)));
assert(warmHarmony.every(l=>l.role==='harmony'&&l.notes.length===warm.notes.length));
assert(parts[1].flatMap(l=>l.notes).filter(([n])=>n!=='r').every(([n])=>n<=47),'Blueberry stays in the bass register');
assert(parts.flat().reduce((n,l)=>n+(l.lyrics.match(/\bla\b/g)||[]).length,0)>=24,'Audible la-la vocabulary');
for(const role of ['bass','hum','scat']) assert(parts.flat().some(l=>l.role===role));
const family=parts.map(ls=>ls.find(l=>near(l.at,66)&&l.role==='lead'));
assert(family.every(Boolean));
assert(family.every(l=>l.lyrics===family[0].lyrics));
console.log('Chorus: melody preserved, immediate opening, monophonic roles, backing dynamics, exposed entrance, D-major notes, and shared cadence passed.');
