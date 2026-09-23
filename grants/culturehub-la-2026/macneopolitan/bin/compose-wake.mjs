#!/usr/bin/env node
// A wake-up lullaby for Sophia. 3/4, D major, 69 bpm, 20 bars. The sleep
// lullaby (compose-lullaby.mjs) falls; this one rises. neo sings the words,
// frisbee says its first word — her name — and blueberry rocks the same cradle
// with the fifth on top. All three hum the last two bars. Soft on purpose.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];   // Aaron: speaks at 48.8, band 39–53 — the cradle sits inside it
const BPM=69, BAR=3, BARS=20, TOTAL=BAR*BARS; // 60 beats ≈ 52 s
const parts=[[],[],[]];
const words=new Set(['good','morn-ing','So-phi-a','the','sun','is','on','your','lid','cof-fee','take','time','day','out','for','you','hmm']);
function add(i,bar,notes,text,gain,role='word') {
  assert.equal(notes.filter(([n])=>n!=="r").length,text.split(" ").reduce((a,t)=>a+t.split("-").length,0),text);
  assert(text.split(' ').every(x=>words.has(x)),text);
  parts[i].push({at:bar*BAR,notes,text,gain,role});
}

// neo — the words, each phrase two bars, each one a little higher than the last
add(0,2, [[54,1],[57,1],[59,1],[62,1],[64,1.5],[62,.5]],'good morn-ing So-phi-a',.50);
add(0,4, [[57,1],[59,1],[61,1],[62,.5],[64,.5],[66,2]],'the sun is on your lid',.48);
add(0,8, [[59,1],[62,1],[64,1],[66,1],[67,2]],'the cof-fee is on',.50);
add(0,10,[[66,1],[64,1],[62,4]],'take your time',.46);
add(0,14,[[59,1],[62,1],[64,1],[66,.5],[64,.5],[62,2]],'the day is out for you',.48);
add(0,16,[[62,1],[64,1],[66,4]],'good morn-ing',.46);
add(0,18,[[62,6]],'hmm',.34,'hum');

// frisbee — its first word is her name; it answers neo twice
add(2,6, [[66,1],[64,1],[62,1],[64,.5],[62,.5],[59,2]],'So-phi-a So-phi-a',.44);
add(2,12,[[67,1],[66,1],[64,3],['r',1]],'So-phi-a',.42);
add(2,18,[[69,6]],'hmm',.30,'hum');

// blueberry — the cradle, fifth on top now: root two beats, fifth one, every bar.
// Written where Tom actually speaks (G2–D3 roots, fifths to A3): the first
// draft sat a fifth lower and the pitch shift croaked ("ribbity", Sept 23).
const roots=[50,50, 50,43, 50,45, 43,50, 50,43, 45,50, 43,45, 50,43, 50,50, 50,50];
for(let bar=0;bar<BARS;bar+=2) {
  const notes=[],text=[];
  for(const b of [bar,bar+1]) {
    const r=roots[b];
    if(b>=18) {notes.push([50,3]);text.push('hmm');}          // the long hum
    else {notes.push([r,2],[r===50?45:r+7,1]);text.push('hmm','hmm');}   // D's fifth below, so nothing leaves the band
  }
  add(1,bar,notes,text.join(' '),bar<2?.40:bar>=18?.36:.46,'hum');
}

// Dynamics: gentle accents, the second phrase of each pair a shade softer.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=="r").length;
  const pattern=i===1?[1,.72]:[1,.78,.9,.74,.86,.7];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.62,pattern[j%pattern.length]));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// Room on the phrases, echo thrown on every tail (same slide as the sleep lullaby).
function performanceKeys(lines,i) {
  const ROOM=i===1?.7:.8, THROW=.85;
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
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:52,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:8,singVibratoHz:[4.5,3.5,5][i],singF0Floor:i===1?70:80,double:false,faceAlpha:0.95,
    performance:{expression:.5,keys:performanceKeys(lines,i)}};
});
const score={title:'The MacNeoPolitan Trio — Good morning, Sophia',composer:'The machines, arr. compose-wake.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'A wake-up lullaby in three, D major, rising. Blueberry hums the cradle with the fifth on top; neo sings good morning Sophia / the sun is on your lid / the coffee is on / take your time / the day is out for you / good morning; frisbee says its first word, her name; all three hum the last two bars. Very quiet.',
  arrangement:{total:TOTAL,meter:'3/4',sections:[{beat:0,name:'Cradle'},{beat:6,name:'neo: good morning Sophia'},{beat:18,name:'frisbee: Sophia'},{beat:24,name:'neo: the coffee is on'},{beat:36,name:'frisbee: Sophia'},{beat:42,name:'neo: the day is out for you'},{beat:54,name:'Hum'}]},voices};
writeFileSync(new URL('../scores/trio-wake.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log(`Wake: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${parts.map((p,i)=>`${members[i]} ${p.length} lines`).join(', ')}.`);
