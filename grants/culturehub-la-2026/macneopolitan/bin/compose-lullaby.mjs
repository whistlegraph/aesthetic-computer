#!/usr/bin/env node
// A lullaby for frisbee, the newborn. 3/4, D major, 63 bpm, 17 bars.
// neo sings the elders' words (stars and moons), frisbee answers without
// words yet (loo), blueberry rocks the cradle underneath. All three hum it
// to sleep. Melody an octave down; deep room on the words, echo on the tails.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];   // Aaron since Sept 23: Tom croaked a fifth below his speaking pitch
const BPM=63, BAR=3, BARS=17, TOTAL=BAR*BARS; // 51 beats ≈ 48.6 s
const parts=[[],[],[]];
const words=new Set(['the','moon','is','up','now','stars','are','out','for','you','on','your','lid','sleep','un-der','loo','hmm']);
const DOWN=12; // neo and frisbee sing an octave below the first draft
const low=notes=>notes.map(([n,d])=>[n==='r'?n:n-DOWN,d]);
function add(i,bar,notes,text,gain,role='word') {
  assert.equal(notes.filter(([n])=>n!=="r").length,text.split(" ").reduce((a,t)=>a+t.split("-").length,0),text);
  assert(text.split(' ').every(x=>words.has(x)),text);
  parts[i].push({at:bar*BAR,notes,text,gain,role});
}

// neo — the melody, four two-bar phrases with a bar of rest between pairs
add(0,2,low([[66,1],[64,1],[62,.5],[61,.5],[59,2],['r',1]]),'the moon is up now',.62);
add(0,4,low([[64,1],[66,1],[69,1],[67,1],[64,2]]),'stars are out for you',.60);
add(0,8,low([[64,1],[66,1],[71,1],[69,3]]),'moon on your lid',.62);
add(0,12,low([[67,1],[66,1],[64,1],[61,1],[62,2]]),'sleep un-der the stars',.58);
add(0,14,low([[62,6]]),'hmm',.40,'hum');

// frisbee — no words yet; it answers each of neo's pairs on loo
add(2,6,low([[66,1],[64,1],[62,1],[62,2],['r',1]]),'loo loo loo loo',.52);
add(2,10,low([[66,1],[69,1],[66,1],[67,2],[64,1]]),'loo loo loo loo loo',.50);
add(2,14,low([[66,6]]),'hmm',.36,'hum');

// blueberry — the cradle: root for two beats, fifth for one, every bar
const roots=[50,50, 50,43, 50,45, 50,43, 50,45, 50,43, 43,'cad', 50,50,50];   // D as D3 (50): the cradle sits in Aaron's band 43–52
for(let bar=0;bar<BARS;bar+=2) {
  const notes=[],text=[];
  for(const b of [bar,bar+1]) {
    if(b>=BARS)break;
    const r=roots[b];
    if(r==='cad') {notes.push([45,1],[50,2]);text.push('hmm','hmm');}   // V–I under neo's cadence
    else if(b>=14) {notes.push([50,3]);text.push('hmm');}               // the long hum
    else {notes.push([r,2],[r===50?45:r+7,1]);text.push('hmm','hmm');}   // D's fifth below
  }
  add(1,bar,notes,text.join(' '),bar<2?.50:bar>=14?.44:.55,'hum');
}

// Dynamics: gentle accents, the second phrase of each pair a shade softer.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=="r").length;
  const pattern=i===1?[1,.72]:[1,.78,.9,.74,.86,.7];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.62,pattern[j%pattern.length]));
  line.gain=Math.max(.30,+line.gain.toFixed(3));
}

// Space and echo are two halves of one slide: a phrase sits in a deep room,
// and its last note is thrown into echo until the next phrase begins.
function performanceKeys(lines,i) {
  const ROOM=i===1?.7:.85, THROW=.9;
  if(i===1) return [{beat:0,space:ROOM,pitch:0},{beat:TOTAL+6,space:ROOM,pitch:0}];
  const keys=[{beat:0,space:ROOM,pitch:0}];
  for(const line of lines) {
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
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:64,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:8,singVibratoHz:[4.5,3.5,5][i],singF0Floor:i===1?70:80,double:false,
    performance:{expression:.6,keys:performanceKeys(lines,i)}};
});
const score={title:'The MacNeoPolitan Trio — Lullaby for frisbee',composer:'The machines, arr. compose-lullaby.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'A lullaby in three, D major, low. Blueberry hums the cradle (root two beats, fifth one, V-I under the last cadence); neo sings the moon is up now / stars are out for you / moon on your lid / sleep under the stars, an octave down; frisbee, who has no words yet, answers on loo; all three hum the last bars. Deep room on the phrases, echo thrown on every tail. Enhanced/Premium voices only.',
  arrangement:{total:TOTAL,meter:'3/4',sections:[{beat:0,name:'Cradle'},{beat:6,name:'neo: the moon is up now'},{beat:18,name:'frisbee: loo'},{beat:24,name:'neo: moon on your lid'},{beat:30,name:'frisbee: loo'},{beat:36,name:'neo: sleep under the stars'},{beat:42,name:'Hum'}]},voices};
writeFileSync(new URL('../scores/trio-lullaby.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log(`Lullaby: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${parts.map((p,i)=>`${members[i]} ${p.length} lines`).join(', ')}.`);
