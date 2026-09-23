#!/usr/bin/env node
// Six études, one device each, wordless. The proof for STYLINGS.md: hocket,
// a drone under close seconds, a barbershop post and stepped swipe, bass with
// ticks, a three-voice round at one bar, and Reich-style pulsing vowels.
// 96 bpm, 4/4, D. Every note is inside the member's measured band; Zoe's band
// was measured today (median 55.4, p10 52.3, p90 59.3) and she is the middle
// voice, not a second soprano.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
const RANGE=[[54,66],[43,55],[50,62]];          // usable sung, per member
const BPM=96, BAR=4;
// tokens proven to render: DOOWOP.md / PHONEME-STUDY.md, plus ki, doom, ts, tk
// measured in today's floor probe.
const TOKENS=new Set(['doo','loo','hmm','bah','ooh','ah','dm','doom','ki','ts','tk']);
const parts=[[],[],[]];
function add(i,at,notes,text,gain,role='phoneme') {
  const toks=text.split(' ');
  assert.equal(notes.filter(([n])=>n!=='r').length,toks.length,`${members[i]}@${at}: ${text}`);
  assert(toks.every(t=>TOKENS.has(t)),`${members[i]}@${at}: unknown token in "${text}"`);
  for(const [n] of notes) if(n!=='r') assert(n>=RANGE[i][0]&&n<=RANGE[i][1],`${members[i]}: ${n} outside ${RANGE[i]}`);
  assert(toks.length<=40,`${members[i]}@${at}: ${toks.length} notes in one line`);
  parts[i].push({at,notes,text,gain,role});
}
const rep=(t,n)=>Array(n).fill(t).join(' ');

// ---- I. Hocket (beats 0–36) ------------------------------------------------
// One twelve-note D-mixolydian line cut between the three. It cannot stay in
// one octave — the bands are an octave and a half apart — so the melody is
// compound by construction. Half-beat notes: above the syllable floor.
const HOCKET=[[0,62],[2,57],[1,50],[0,64],[2,59],[1,52],[0,66],[2,60],[1,45],[0,62],[2,57],[1,50]];
{
  const seq=[[],[],[]];                          // per member: [beat, pitch]
  for(let cycle=0;cycle<6;cycle++)
    HOCKET.forEach(([who,pitch],k)=>seq[who].push([cycle*6+k*0.5,pitch]));
  for(const [i,hits] of seq.entries()) {
    const notes=[];let t=hits[0][0];
    for(const [beat,pitch] of hits) { if(beat>t+1e-9) notes.push(['r',beat-t]); notes.push([pitch,.5]); t=beat+.5; }
    add(i,hits[0][0],notes,rep('doo',hits.length),i===1?.52:.46);
  }
}

// ---- II. Drone and close seconds (40–88) -----------------------------------
// Aaron holds D. Noelle and Zoe rub a second above him and cross once, so the
// beating stops and starts again. Their bands overlap only over 57–59, which
// is exactly where this device wants to live.
add(1,40,Array.from({length:12},()=>[50,4]),rep('ooh',12),.40,'hum');
{
  const pairs=[[59,57],[61,59],[58,57],[62,60],[57,59],[59,57]];   // [Noelle, Zoe]
  for(const [i,who] of [[0,0],[2,1]]) {
    const notes=[],text=[];
    for(const p of pairs) for(let k=0;k<4;k++) { notes.push([p[who],2]); text.push('ah'); }
    add(i,40,notes,text.join(' '),.32,'hum');
  }
}

// ---- III. Barbershop tag (92–124) ------------------------------------------
// Noelle keeps the post. The two below her swipe — which this pipeline can
// only write as a staircase, once in whole beats and once in half-beats, so
// the seam can be heard and judged. Ends on the barbershop seventh, then D.
add(0,92,[[62,4],[62,4],[62,4],[62,4],[62,4],[64,4]],rep('ooh',6),.44,'hum');
add(1,92,[[50,4],[48,1],[47,1],[45,2],[45,4],[43,1],[45,1],[47,2]],rep('ooh',8),.44,'hum');
add(2,92,[[57,4],[56,1],[54,1],[53,2],[54,4],[52,1],[53,1],[54,2]],rep('ooh',8),.40,'hum');
add(1,108,[[45,.5],[46,.5],[47,.5],[48,.5],[49,.5],[50,.5],[49,.5],[48,.5],[45,4]],rep('ooh',9),.44,'hum');
add(2,108,[[53,.5],[54,.5],[55,.5],[56,.5],[57,.5],[56,.5],[55,.5],[54,.5],[54,4]],rep('ooh',9),.40,'hum');
add(0,116,[[60,4],[62,4]],'ooh ooh',.46,'hum');   // D7's seventh, then the root
add(1,116,[[50,4],[50,4]],'ooh ooh',.46,'hum');
add(2,116,[[54,4],[54,4]],'ooh ooh',.42,'hum');

// ---- IV. Bass and ticks (128–168) ------------------------------------------
// Aaron on doom/dm, Zoe on unpitched ticks — the one place the engine's
// unvoiced-onset pitch failure is the point — Noelle on a two-bar riff.
for(let half=0;half<2;half++) {
  const at=128+half*20,notes=[],text=[];
  for(let bar=0;bar<5;bar++) {
    notes.push([45,.5],['r',.5],[45,.5],['r',.5],[43,.5],['r',.5],[45,.5],['r',.5]);
    text.push('doom','dm','doom','dm');
  }
  add(1,at,notes,text.join(' '),.55,'bass');
  const zn=[],zt=[];
  for(let bar=0;bar<5;bar++) { zn.push(['r',1.5],[55,.25],['r',1.25],[57,.25],['r',.75]); zt.push('ts','tk'); }
  add(2,at,zn,zt.join(' '),.30,'percussion');
}
{
  const riff=[[[62,.5],[64,.5],[62,.5],[59,.5],[57,1],['r',1]],[[59,.5],[62,.5],[64,1],[62,1],['r',1]]];
  for(let half=0;half<2;half++) {
    const notes=[],text=[];
    for(let r=0;r<2;r++) for(const bar of riff) for(const n of bar) { notes.push(n); if(n[0]!=='r') text.push('bah'); }
    add(0,128+half*16,notes,text.join(' '),.42);
  }
}

// ---- V. Round at one bar (172–220) -----------------------------------------
// Every note is a D-chord tone, so the canon consonates in any rotation. Each
// member sings the same shape on the chord tones that sit in her own band:
// Noelle A3–F#4, Zoe F#3–D4, Aaron A2–F#3. Entries one bar apart.
const TUNE=[[0,1],[1,1],[2,1],[1,1],[0,2],[1,2],[2,1],[1,1],[0,1],[1,1],[0,4]];  // [degree, beats]
const CHORDTONES=[[57,62,66],[45,50,54],[54,57,62]];
for(const [i,entry] of [[0,172],[2,176],[1,180]]) {
  const notes=[],text=[];
  for(let pass=0;pass<2;pass++) for(const [deg,d] of TUNE) { notes.push([CHORDTONES[i][deg],d]); text.push('loo'); }
  add(i,entry,notes,text.join(' '),i===1?.44:.40);
  add(i,212,[[CHORDTONES[i][i===0?1:2],4],[CHORDTONES[i][i===0?1:2],4]],'loo loo',.38,'hum');
}

// ---- VI. Pulsing vowels (224–264) ------------------------------------------
// A chord breathed in and out. There is no breath, so the swell is written as
// eight half-beat notes under a rising and falling noteGains envelope. The two
// upper swells are offset by three beats, so the chord is never wholly there.
const SWELL=[.30,.52,.78,1,1,.78,.52,.30];
for(let k=0;k<4;k++) add(1,224+k*10,Array.from({length:10},()=>[45,1]),rep('dm',10),.30,'bass');
[[0,[62,64,66,64,62,62,64]],[2,[57,59,57,59,57,59]]].forEach(([i,pitches])=>{
  pitches.forEach((p,k)=>{
    const at=224+(i===0?0:3)+k*6;
    if(at+4>264) return;
    const line={at,notes:Array.from({length:8},()=>[p,.5]),text:rep('ooh',8),gain:.46,role:'hum',accents:SWELL.slice()};
    add(i,at,line.notes,line.text,line.gain,line.role);
    parts[i][parts[i].length-1].accents=SWELL.slice();
  });
});

const TOTAL=264;   // 264 beats at 96 bpm = 165 s

// Accents: a light pattern everywhere a swell has not already been written.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  if(!line.accents) {
    const pattern=line.role==='bass'?[1,.70,.88,.66]:line.role==='percussion'?[1,.82]:[1,.80,.92,.76,.88,.74];
    line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.62,pattern[j%pattern.length]));
  }
  assert.equal(line.accents.length,n,`${members[i]}@${line.at}: accent count`);
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// Room stays on; echo is thrown only on the two tails that want to ring —
// the barbershop chord and the round's final chord.
function performanceKeys(i) {
  const ROOM=i===1?.55:.7;
  return [{beat:0,space:ROOM,pitch:0},{beat:116,space:ROOM,pitch:0,echo:0},{beat:120,space:ROOM,pitch:0,echo:.8},
    {beat:126,space:ROOM,pitch:0,echo:0},{beat:212,space:ROOM,pitch:0,echo:0},{beat:216,space:ROOM,pitch:0,echo:.8},
    {beat:222,space:ROOM,pitch:0,echo:0},{beat:TOTAL+4,space:ROOM,pitch:0,echo:0}];
}
const voices=parts.map((lines,i)=>{
  lines.sort((a,b)=>a.at-b.at);
  const notes=[];let cursor=0;
  for(const line of lines) {
    assert(line.at>=cursor-1e-6,`${members[i]}: overlap at beat ${line.at} (cursor ${cursor})`);
    if(line.at>cursor+1e-6) notes.push(['r',line.at-cursor]);
    notes.push(...line.notes);
    cursor=line.at+line.notes.reduce((s,[,d])=>s+d,0);
  }
  assert(cursor<=TOTAL+1e-6,`${members[i]}: runs long (${cursor})`);
  if(cursor<TOTAL) notes.push(['r',TOTAL-cursor]);
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:52,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),
    lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:i===1?6:8,singVibratoHz:[4.5,3.5,5][i],
    singF0Floor:i===1?70:80,singGapMs:20,singLegatoMs:15,double:false,faceAlpha:0.95,
    performance:{expression:.45,keys:performanceKeys(i)}};
});
const sections=[{beat:0,name:'I. Hocket'},{beat:40,name:'II. Drone and close seconds'},
  {beat:92,name:'III. Barbershop tag'},{beat:128,name:'IV. Bass and ticks'},
  {beat:172,name:'V. Round at one bar'},{beat:224,name:'VI. Pulsing vowels'}];
const score={title:'The MacNeoPolitan Trio — Styling study',composer:'The machines, arr. compose-styling-study.mjs',
  bpm:BPM,machines:3,lead:0,phonemeOnly:true,
  description:'Six wordless études, one a cappella device each: hocket cut between the three; a held D under close seconds; a barbershop post with a stepped swipe; bass and unpitched ticks; a three-voice round entering a bar apart; and a chord pulsed in and out. 96 bpm, D, 165 s. Every note sits inside the member’s measured band, Zoe’s measured 23 September.',
  arrangement:{total:TOTAL,meter:'4/4',sections},voices};
writeFileSync(new URL('../scores/trio-styling-study.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
const secs=sections.map((s,k)=>`${s.name} ${(((sections[k+1]?.beat??TOTAL)-s.beat)*60/BPM).toFixed(0)}s`).join(', ');
console.log(`Styling study: ${TOTAL} beats at ${BPM} bpm = ${(TOTAL*60/BPM).toFixed(1)} s.`);
console.log(`  ${parts.map((p,i)=>`${members[i]} ${p.length} lines / ${p.reduce((s,l)=>s+l.notes.filter(([n])=>n!=='r').length,0)} notes`).join(', ')}`);
console.log(`  ${secs}`);
