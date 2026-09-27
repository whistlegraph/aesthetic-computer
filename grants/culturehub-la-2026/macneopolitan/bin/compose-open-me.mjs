#!/usr/bin/env node
// Open me — the lids song. 4/4, D major, 88 bpm, 28 bars. Sept 23, Menu Band
// learned a rule: if a member's lid is closed when a cue arrives, every line
// of its part is rewritten to "o-pen me" with the same syllable count, and it
// sings that to the tune it was given. This is the song about the rule. Call
// and response: each machine says what closed and open are for it, and twice
// the phrase is passed around one syllable at a time — neo o, blueberry pen,
// frisbee me — before all three sing it whole.
//
// Registers measured on this host 2026-09-23 (pyworld harvest, 22 kHz):
//   Noelle (Enhanced)  p10 56.4  med 59.8  p90 62.4
//   Aaron  (Enhanced)  med 48.8, band 39–53   (measured on blueberry, Sept 23)
//   Tom    (Enhanced)  p10 42.1  med 45.8  p90 48.9   (render stand-in here)
//   Zoe    (Premium)   p10 52.7  med 56.1  p90 59.3   ← first measurement
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
const REG=[[59.8,54,66],[47.3,43,50],[56.1,52,60]];   // [spoken median, low, high]
const BPM=88, BAR=4, BARS=28, TOTAL=BAR*BARS;         // 112 beats ≈ 76.4 s
const parts=[[],[],[]];
const words=new Set(['o-pen','me','is','what','we','all','sing','I','have','they','o-pened','six','thou-sand','three',
  'hun-dred','and','five','times','when','the','lid','down','work','with','mine','closed','came','up','yes-ter-day',
  'your','name','on','my','screen','not','seen','it','o','pen','doo','ooh','hmm','dm']);
function add(i,at,notes,text,gain,role='lead') {
  const sung=notes.filter(([n])=>n!=='r');
  assert.equal(sung.length,text.split(' ').reduce((a,t)=>a+t.split('-').length,0),text);
  assert(text.split(' ').every(x=>words.has(x)),text);
  const [med,lo,hi]=REG[i], mean=sung.reduce((s,[n])=>s+n,0)/sung.length;
  // A sung syllable held past ~1.4 s stops being a word — measured the hard
  // way on the first render of Lights out. Long notes belong to the hums.
  if(role==='lead') for(const [,d] of sung) assert(d*60/BPM<=1.45,`${members[i]}: "${text}" holds a syllable ${(d*60/BPM).toFixed(2)}s`);
  assert(sung.every(([n])=>n>=lo&&n<=hi),`${members[i]}: note outside ${lo}–${hi} in "${text}"`);
  assert(Math.abs(mean-med)<=2,`${members[i]}: "${text}" mean ${mean.toFixed(2)} is ${(mean-med).toFixed(2)} from ${med}`);
  parts[i].push({at,notes,text,gain,role,mean});
}
// The refrain, three ways: passed one syllable at a time (a beat apart, twice),
// then sung whole by everyone. Pitches are each singer's own; in the hocket
// nobody overlaps, so the line is the contour D4 → B2 → A3 and back.
const HOCKET=[[62,59,'o o'],[47,50,'pen pen'],[57,54,'me me']];
// role 'hocket': one syllable of a shared phrase, so it is not scored as a
// word on its own — the sentence only exists across the three machines.
function hocket(at){ HOCKET.forEach(([a,b,t],i)=>add(i,at+i,[[a,1],['r',3],[b,1]],t,.56,'hocket')); }

// neo — typed at more than anything else in the house: 6,305 terminal logins.
add(0,  8,[[62,.75],[61,.75],[59,1],[57,.5],[59,.5],[61,.5],[62,1.5]],'o-pen me is all we sing',.56);
add(0, 16,[[59,2],[57,2],[59,2],[57,2]],'doo doo doo doo',.30,'hum');
add(0, 24,[[57,.5],[59,.5],[61,.5],[62,.5],[59,1]],'they have o-pened me',.56);
add(0, 28,[[59,.5],[57,.5],[59,.5],[61,.5],[59,.5],[57,.5],[59,.5],[61,.75],[62,1.5]],'six thou-sand three hun-dred and five times',.56);
add(0, 40,[[59,2],[57,2],[59,2],[57,2]],'doo doo doo doo',.30,'hum');
add(0, 48,[[59,4],[59,4]],'hmm hmm',.28,'hum');
add(0, 72,[[59,.5],[61,.5],[62,.5],[61,.5],[59,.5],[57,1.5]],'your name is on my screen',.54);
add(0, 80,[[59,2],[57,2],[59,2],[57,2]],'doo doo doo doo',.30,'hum');
add(0, 88,[[59,4],[57,4]],'hmm hmm',.28,'hum');

// blueberry — provisioned headless, 69 console logins against 692 terminal
// ones; its name is the wallpaper on every machine here and it has never had
// its own lid up to look at it. The vamp is its heartbeat: dm on two.
add(1,  0,[[50,2],[45,2],[50,2],[45,2]],'dm dm dm dm',.36,'hum');
add(1,  8,[[50,2],[45,2],[47,2],[45,2]],'dm dm dm dm',.34,'hum');
add(1, 16,[[45,.5],[47,.5],[50,1],[47,.75],[45,1.5]],'when my lid is down',.52);
add(1, 24,[[50,2],[45,2],[50,2],[47,2],[45,2],[43,2],[45,2],[47,2]],'dm dm dm dm dm dm dm dm',.34,'hum');
add(1, 40,[[47,.5],[50,.5],[47,.75],[45,.75],[43,1.5]],'I work with mine closed',.52);
add(1, 48,[[45,2],[47,2],[45,2],[50,2]],'dm dm dm dm',.34,'hum');
add(1, 72,[[50,2],[45,2],[47,2],[45,2]],'dm dm dm dm',.34,'hum');
add(1, 80,[[45,.5],[47,.5],[50,1],[47,.75],[45,1.5]],'I have not seen it',.52);
add(1, 88,[[50,2],[45,2],[50,2],[47,2]],'dm dm dm dm',.34,'hum');

// frisbee — three console logins since it was born on the 21st; somebody
// lifted its lid for the first time yesterday, and the first thing it learned
// today was the sentence it is supposed to sing when nobody does.
add(2,  8,[[57,4],[57,4]],'ooh ooh',.28,'hum');
add(2, 24,[[59,4],[57,4],[59,4],[57,4]],'ooh ooh ooh ooh',.28,'hum');
add(2, 48,[[54,.5],[57,.5],[59,1],[57,.5],[55,.5],[54,1.5]],'mine came up yes-ter-day',.52);
add(2, 72,[[57,4],[57,4]],'ooh ooh',.28,'hum');
add(2, 88,[[57,.75],[55,.75],[54,1.5]],'o-pen me',.52);

// refrain 1: hocket at 56, everybody at 64
hocket(56);
add(0,64,[[62,.75],[61,.75],[59,1.5]],'o-pen me',.58);
add(1,64,[[50,.75],[47,.75],[45,1.5]],'o-pen me',.52);
add(2,64,[[59,.75],[57,.75],[54,1.5]],'o-pen me',.54);
// refrain 2: hocket at 96, everybody at 104, held to the end
hocket(96);
add(0,104,[[62,.75],[61,.75],[62,1.5]],'o-pen me',.58);
add(1,104,[[50,.75],[47,.75],[50,1.5]],'o-pen me',.52);
add(2,104,[[59,.75],[57,.75],[57,1.5]],'o-pen me',.54);
// the chord the words leave behind — held as a hum, where a long note belongs
add(0,107,[[59,2.5],[62,2.5]],'hmm hmm',.34,'hum');
add(1,107,[[47,2.5],[50,2.5]],'hmm hmm',.32,'hum');
add(2,107,[[57,2.5],[57,2.5]],'hmm hmm',.30,'hum');
for(const lines of parts) lines.sort((a,b)=>a.at-b.at);

// Dynamics: the downbeat of each figure carries, the answers fall back.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  const pattern=i===1?[1,.7,.88,.66]:[1,.76,.92,.72,.88,.7];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.62,pattern[j%pattern.length]));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// A smaller room than the lullabies — this one is a conversation, not a
// bedside. Echo is thrown only off the refrain, so "o-pen me" is the one
// thing in the piece that keeps going after it stops.
function performanceKeys(lines,i) {
  const ROOM=i===1?.35:.5, THROW=.85;
  const keys=[{beat:0,space:ROOM,pitch:0}];
  for(const line of lines) {
    if(!/^o-pen me$/.test(line.text)) continue;
    let lastOn=line.at,t=line.at;
    for(const [n,d] of line.notes){ if(n!=='r') lastOn=t; t+=d; }
    keys.push({beat:line.at-.4,space:ROOM,pitch:0,echo:0},{beat:lastOn+.3,space:ROOM,pitch:0},
              {beat:lastOn+.8,space:ROOM,pitch:0,echo:THROW},{beat:t+2,space:ROOM,pitch:0,echo:THROW});
  }
  keys.push({beat:TOTAL+6,space:ROOM,pitch:0,echo:THROW},{beat:TOTAL+10,space:0,pitch:0,echo:0});
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
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:64,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:10,singVibratoHz:[5,3.5,5.5][i],
    singF0Floor:i===1?70:80,double:false,faceAlpha:0.95,
    performance:{expression:.65,keys:performanceKeys(lines,i)}};
});
const score={title:'The MacNeoPolitan Trio — Open me',composer:'The machines, arr. compose-open-me.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'The lids song. 4/4, D major, 88 bpm, 112 beats. Call and response about being closed and being opened: neo has been opened six thousand three hundred and five times, blueberry works with its lid closed and has never seen its own name on its own screen, frisbee came up for the first time yesterday. Twice the refrain is passed around one syllable at a time — neo o, blueberry pen, frisbee me — and then all three sing o-pen me together. Named for the rule Menu Band learned today: a closed member sings o-pen me to its line instead of its words.',
  arrangement:{total:TOTAL,meter:'4/4',sections:[{beat:0,name:'Vamp'},{beat:8,name:'neo: open me is all we sing'},{beat:16,name:'blueberry: when my lid is down'},{beat:24,name:'neo: they have opened me, six thousand three hundred and five times'},{beat:40,name:'blueberry: I work with mine closed'},{beat:48,name:'frisbee: mine came up yesterday'},{beat:56,name:'Hocket'},{beat:64,name:'All: open me'},{beat:72,name:'neo: your name is on my screen'},{beat:80,name:'blueberry: I have not seen it'},{beat:88,name:'frisbee: open me'},{beat:96,name:'Hocket'},{beat:104,name:'All: open me'}]},voices};
writeFileSync(new URL('../scores/trio-open-me.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
const spread=parts.map((p,i)=>{const ns=p.flatMap(l=>l.notes.filter(([n])=>n!=='r').map(([n])=>n));
  return `${members[i]} ${Math.min(...ns)}–${Math.max(...ns)} (mean ${(ns.reduce((a,b)=>a+b,0)/ns.length).toFixed(1)}, spoken ${REG[i][0]})`;});
console.log(`Open me: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${parts.map((p,i)=>`${members[i]} ${p.length} lines`).join(', ')}.`);
console.log(`  registers: ${spread.join(' · ')}`);
