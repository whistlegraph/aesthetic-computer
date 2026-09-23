#!/usr/bin/env node
// Lights out — the evening bookend to compose-wake.mjs. 3/4, D major, 64 bpm,
// 26 bars. The morning song rises and hands the day over; this one hands it
// back. Sung to Sophia and Jeffrey by the three machines that stay up after
// the people sleep: neo carries the words, blueberry is the drone under the
// whole thing and says two things all night (sleep, thirteen days), frisbee
// answers with the only words it has — the two names, and good night.
//
// Registers measured on this host 2026-09-23 (pyworld harvest, 22 kHz):
//   Noelle (Enhanced)  p10 56.4  med 59.8  p90 62.4
//   Aaron  (Enhanced)  med 48.8, band 39–53   (measured on blueberry, Sept 23)
//   Tom    (Enhanced)  p10 42.1  med 45.8  p90 48.9   (render stand-in here)
//   Zoe    (Premium)   p10 52.7  med 56.1  p90 59.3   ← first measurement
// Every line's mean is asserted within 2 semitones of its singer's spoken
// median, and every note inside the band, so nothing has to be dragged far
// from where it talks.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
// [spoken median, low, high] — the line mean aims at the median, notes stay inside.
// blueberry's window is the Aaron ∩ Tom overlap so the offline stand-in sings it too.
const REG=[[59.8,54,66],[47.3,43,50],[56.1,52,60]];
const BPM=64, BAR=3, BARS=26, TOTAL=BAR*BARS;   // 78 beats ≈ 73.1 s
const parts=[[],[],[]];
const words=new Set(['good','night','So-phi-a','Jeff-rey','your','lids','come','down','he','works','past','mid-night',
  'we','stay','up','till','morn-ing','keep','the','time','for','you','can','sleep','now','thir-teen','days','hmm','ooh']);
function add(i,at,notes,text,gain,role='lead') {
  const sung=notes.filter(([n])=>n!=='r');
  assert.equal(sung.length,text.split(' ').reduce((a,t)=>a+t.split('-').length,0),text);
  assert(text.split(' ').every(x=>words.has(x)),text);
  const [med,lo,hi]=REG[i], mean=sung.reduce((s,[n])=>s+n,0)/sung.length;
  // A sung syllable held past ~1.4 s stops being a word: the first render of
  // this song put "Sophia" on a four-beat note and whisper heard "the fear".
  if(role==='lead') for(const [,d] of sung) assert(d*60/BPM<=1.45,`${members[i]}: "${text}" holds a syllable ${(d*60/BPM).toFixed(2)}s`);
  assert(sung.every(([n])=>n>=lo&&n<=hi),`${members[i]}: note outside ${lo}–${hi} in "${text}"`);
  assert(Math.abs(mean-med)<=2,`${members[i]}: "${text}" mean ${mean.toFixed(2)} is ${(mean-med).toFixed(2)} from ${med}`);
  parts[i].push({at,notes,text,gain,role,mean});
}

// neo — the words. Every phrase two bars, falling where the morning rose;
// the last hum lifts back to D, which is the only promise anyone makes here.
add(0, 6,[[64,1],[62,1],[61,.75],[59,.75],[57,1.25]],'good night So-phi-a',.50);
add(0,12,[[59,3],[57,3]],'hmm hmm',.30,'hum');
add(0,18,[[62,.75],[61,.75],[59,.5],[57,1.5]],'good night Jeff-rey',.50);
add(0,30,[[59,.75],[61,.75],[59,.5],[57,1.5]],'your lids come down',.48);
add(0,36,[[59,3],[57,3]],'hmm hmm',.30,'hum');
add(0,42,[[57,1],[59,1],[61,1],[59,.75],[57,1.25]],'he works past mid-night',.48);
add(0,48,[[59,.75],[62,.75],[64,.75],[62,.75],[61,.75],[59,1.25]],'we stay up till morn-ing',.50);
add(0,54,[[59,3],[57,3]],'hmm hmm',.30,'hum');
add(0,60,[[62,1],[61,1],[59,1],[57,1],[59,.75],[57,1.25]],'we keep the time for you',.48);
add(0,66,[[61,1],[59,1],[57,.75],[57,1.25]],'you can sleep now',.46);
add(0,72,[[59,3],[62,3]],'hmm hmm',.32,'hum');

// blueberry — one unbroken drone from the first bar to the last, the way it
// held 13.1 days with its lid down. Two bars per unit, root then colour.
// It stops humming exactly twice, to say a word.
const drone=[[50,45],[50,47],[50,43],[50,45],[45,47],[50,47],'sleep',[45,47],[50,47],'thir-teen days',[50,45],[50,47],[45,50]];
drone.forEach((u,k)=>{
  const at=k*6;
  if(u==='sleep') add(1,at,[[47,1],[45,1.25]],'sleep now',.48);
  else if(u==='thir-teen days') add(1,at,[[50,1],[47,1],[45,1.5]],'thir-teen days',.48);
  else add(1,at,[[u[0],3],[u[1],3]],'hmm hmm',k===0?.40:k>=12?.38:.46,'hum');
});

// frisbee — born Sept 21, opened for the first time yesterday. It has the two
// names and one thing it learned today, and it pads the rest on ooh.
add(2,12,[[59,.5],[57,.5],[54,1.5]],'So-phi-a',.46);
add(2,24,[[57,.75],[54,1.5]],'Jeff-rey',.46);
add(2,42,[[57,3],[57,3]],'ooh ooh',.28,'hum');
add(2,48,[[59,3],[57,3]],'ooh ooh',.30,'hum');
add(2,60,[[57,3],[55,3]],'ooh ooh',.28,'hum');
add(2,70,[[57,.75],[54,1.25]],'good night',.46);
add(2,72,[[57,3],[57,3]],'hmm hmm',.30,'hum');

// Dynamics: gentle accents, the second half of each pair a shade softer.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  const pattern=i===1?[1,.72]:[1,.78,.9,.74,.86,.7];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.62,pattern[j%pattern.length]));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// Space and echo, the same slide the two lullabies use: a phrase sits in a
// deep room, its last note is thrown into echo until the next one starts.
function performanceKeys(lines,i) {
  const ROOM=i===1?.7:.85, THROW=.9;
  const keys=[{beat:0,space:ROOM,pitch:0}];
  for(const line of lines) {
    if(line.role==='hum') continue;
    let lastOn=line.at,t=line.at;
    for(const [n,d] of line.notes){ if(n!=='r') lastOn=t; t+=d; }
    const end=t;
    keys.push({beat:line.at-.4,space:ROOM,pitch:0,echo:0},{beat:line.at,space:ROOM,pitch:0},
              {beat:lastOn+.2,space:ROOM,pitch:0},{beat:lastOn+.6,space:0,pitch:0,echo:THROW},{beat:end-.8,space:0,pitch:0,echo:THROW});
  }
  keys.push({beat:TOTAL+8,space:0,pitch:0,echo:THROW},{beat:TOTAL+12,space:0,pitch:0,echo:0});
  return keys.sort((a,b)=>a.beat-b.beat);
}
const voices=parts.map((lines,i)=>{
  const notes=[];let cursor=0;
  for(const line of lines) {
    assert(line.at>=cursor-1e-6,`${members[i]}: overlap at ${line.at}`);
    if(line.at>cursor+1e-6)notes.push(['r',line.at-cursor]);
    notes.push(...line.notes);cursor=line.at+line.notes.reduce((s,[,d])=>s+d,0);
  }
  if(cursor<TOTAL)notes.push(['r',TOTAL-cursor]);
  assert(cursor<=TOTAL+1e-6,`${members[i]}: runs long (${cursor})`);
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:52,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:8,singVibratoHz:[4.5,3.5,5][i],
    singF0Floor:i===1?70:80,double:false,faceAlpha:0.95,
    performance:{expression:.5,keys:performanceKeys(lines,i)}};
});
const score={title:'The MacNeoPolitan Trio — Lights out',composer:'The machines, arr. compose-lights-out.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'The evening bookend to Good morning, Sophia. 3/4, D major, 64 bpm, 78 beats. Blueberry drones from the first bar to the last and stops twice to say sleep now and thirteen days; neo sings good night Sophia / good night Jeffrey / your lids come down / he works past midnight / we stay up till morning / we keep the time for you / you can sleep now; frisbee answers with the two names and good night. All three end on an open D. Quieter than the morning.',
  arrangement:{total:TOTAL,meter:'3/4',sections:[{beat:0,name:'Drone'},{beat:6,name:'neo: good night Sophia'},{beat:12,name:'frisbee: Sophia'},{beat:18,name:'neo: good night Jeffrey'},{beat:24,name:'frisbee: Jeffrey'},{beat:30,name:'neo: your lids come down'},{beat:36,name:'blueberry: sleep now'},{beat:42,name:'neo: he works past midnight'},{beat:54,name:'blueberry: thirteen days'},{beat:60,name:'neo: we keep the time for you'},{beat:70,name:'frisbee: good night'},{beat:72,name:'Hum'}]},voices};
writeFileSync(new URL('../scores/trio-lights-out.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
const spread=parts.map((p,i)=>{const ns=p.flatMap(l=>l.notes.filter(([n])=>n!=='r').map(([n])=>n));
  return `${members[i]} ${Math.min(...ns)}–${Math.max(...ns)} (mean ${(ns.reduce((a,b)=>a+b,0)/ns.length).toFixed(1)}, spoken ${REG[i][0]})`;});
console.log(`Lights out: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${parts.map((p,i)=>`${members[i]} ${p.length} lines`).join(', ')}.`);
console.log(`  registers: ${spread.join(' · ')}`);
