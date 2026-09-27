#!/usr/bin/env node
// What starts with — a teaching piece in seven rounds. 4/4, D major, 96 bpm,
// 31 bars. Each round is four bars: neo names a letter, frisbee answers with
// a thing from the house that starts with it, and blueberry sings the letter
// and the thing together. B is the machine blueberry, F the machine frisbee,
// N the machine neo, S Sophia, J Jeffrey, M the moon, C the coffee — the
// whole cast of Good morning, Sophia in seven letters.
//
// Every line here was rendered and scored before it was written in (the probe
// tags in hear/: letters-*, probe5-*, probe7-*, probe8-*, probe13-*). What
// that cost, and what it taught:
//   · A letter alone on a line is read out as "capital B". neo's call is
//     therefore always a phrase with the letter in it.
//   · The letter name and the word "is" collide in the low male voice: "B is
//     for" comes back "He is for", "S is" as "Ass is", "N is" as "And as".
//     Each of those took its own wording — "and B is for blueberry" (the
//     free "and" gives the engine a run-up), "neo takes an N", "S stands for
//     Sophia" — measured at 0 % where the plain frame was 25–50 %.
//   · "S stands for Sophia" is the best of seven frames for S; its one
//     error is whisper spelling the name "Sofia". Every frame that spells
//     Sophia right loses the S instead ("Ass is for Sophia").
//   · One beat a syllable is the floor AND the ceiling here: stretched to 1.5
//     beats (0.94 s) blueberry's clean lines fell apart and Zoe's "Sophia"
//     became "so fear" again. Sophia keeps wake's own shape: three notes
//     falling, 1.4 beats each — 0.875 s, the same as at 69 bpm.
//
// Registers: neo (Noelle) 59.6, band 57–62 · blueberry (Aaron) 48.8, band
// 39–53, rendered here with Tom as a stand-in · frisbee (Zoe) 55.6, 52.5–59.4.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
import {checkBand} from './vocalisms.mjs';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
const REG=[[59.6,57,62],[48.8,43,53],[55.6,52.5,59.4]];
const BPM=96, BAR=4, CELL=16, BARS=31, TOTAL=BAR*BARS;   // 124 beats ≈ 77.5 s
const LETTERS=new Set('ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split(''));
const words=new Set(['the','let-ter','is','for','and','takes','an','stands',
  'blue-ber-ry','cof-fee','fris-bee','Jeff-rey','moon','ne-o','So-phi-a',
  'those','are','words','we','know','dm','ooh','doo','hmm']);
// The contour every measured line was measured on: a step up, a step up, and
// back down through the member's own speaking pitch. Lines take as much of it
// as they have syllables.
const SHAPE=[[59,61,62,61,59,57,59],[47,49,50,49,47,45,47],[55,57,59,57,55,54,55]];
const parts=[[],[],[]];
function add(i,at,notes,text,gain,role='lead') {
  const sung=notes.filter(([n])=>n!=='r');
  const toks=text.split(' ');
  assert.equal(sung.length,toks.reduce((a,t)=>a+t.split('-').length,0),text);
  assert(toks.every(t=>words.has(t)||LETTERS.has(t)),text);
  // A letter is always a whole token on one note — never split, never joined.
  assert(toks.filter(t=>LETTERS.has(t)).length<=1,`${members[i]}: two letters in "${text}"`);
  if(role==='lead') for(const [,d] of sung) {
    assert(d>=1-1e-9,`${members[i]}: "${text}" gives a syllable ${d} beats`);
    assert(d*60/BPM<=1.45,`${members[i]}: "${text}" holds a syllable ${(d*60/BPM).toFixed(2)}s`);
  }
  const [med,lo,hi]=REG[i], mean=sung.reduce((s,[n])=>s+n,0)/sung.length;
  assert(sung.every(([n])=>n>=lo&&n<=hi),`${members[i]}: note outside ${lo}–${hi} in "${text}"`);
  assert(Math.abs(mean-med)<=2,`${members[i]}: "${text}" mean ${mean.toFixed(2)} is ${(mean-med).toFixed(2)} from ${med}`);
  const b=checkBand(members[i],sung);
  assert(b.lo>=REG[i][1]&&b.hi<=REG[i][2],`${members[i]}: ${b.lo}–${b.hi} outside the measured band`);
  parts[i].push({at,notes,text,gain,role,mean});
}
/// A line on the member's own shape, one beat a syllable.
function say(i,at,text,gain,role='lead',dur=1) {
  const n=text.split(' ').reduce((a,t)=>a+t.split('-').length,0);
  add(i,at,Array.from({length:n},(_,k)=>[SHAPE[i][k%SHAPE[i].length],dur]),text,gain,role);
}

// The seven rounds. [letter, neo's call, frisbee's answer, blueberry's both,
// and — for the two words Zoe only keeps on a falling line — her notes].
const ROUNDS=[
  ['B','the let-ter B',     'the blue-ber-ry','and B is for blue-ber-ry',[[59,1],[57,1],[55,1],[54,1]]],
  ['C','C is for',          'the cof-fee',    'C is for cof-fee'],
  ['F','F is for',          'the fris-bee',   'F is for fris-bee'],
  ['J','the let-ter J',     'Jeff-rey',       'J is for Jeff-rey'],
  ['M','and the let-ter M', 'the moon',       'M is for the moon'],
  ['N','the let-ter N',     'ne-o',           'ne-o takes an N'],
  ['S','the let-ter S',     'So-phi-a',       'S stands for So-phi-a',[[59,1.4],[57,1.4],[55,1.4]]],
];
assert.equal(new Set(ROUNDS.map(r=>r[0])).size,ROUNDS.length,'each letter once');
ROUNDS.forEach(([L,call,answer,both,fall],c)=>{
  const s=c*CELL;
  say(0,s,call,.56);                                   // neo names it
  add(1,s,[[47,2],[49,2]],'dm dm',.34,'hum');          // blueberry keeps the floor
  // frisbee clears her throat before the first answer: the first line a voice
  // renders comes out tighter than the rest, and "the blueberry" was the one
  // line that lost it ("The Blubbery"). A hum ahead of it is enough.
  if(c===0) add(2,s,[[55,2],[57,2]],'ooh ooh',.28,'hum');
  // Zoe's Sophia is the one from wake: three notes falling, 1.4 beats each —
  // any longer and the last syllable is heard as "fear". "the blueberry"
  // wants the same descent; on the rising shape it came back "The Blubbery"
  // half the time, on this one never.
  if(fall) add(2,s+5,fall,answer,.54);
  else say(2,s+5,answer,.54);                          // frisbee answers
  say(1,s+9,both,.52);                                 // blueberry sings both
  // One of the two women holds a pad under him, never both — and never the
  // one who has just answered on a long note.
  if(c%2||L==='S') add(0,s+9,[[59,2],[61,2],[59,3]],'doo doo doo',.30,'hum');
  else             add(2,s+9,[[55,3.5],[57,3.5]],'ooh ooh',.28,'hum');
  assert(call.split(' ').includes(L)||both.split(' ').includes(L),`round ${L} lost its letter`);
});
// All three, together, on the only line in the piece that is not about a
// letter. Then the D they have been circling all along.
const CLOSE=ROUNDS.length*CELL;                        // beat 112
for(const i of [0,1,2]) say(i,CLOSE,'those are the words we know',[.58,.56,.56][i]);
add(0,CLOSE+6,[[59,3],[62,3]],'hmm hmm',.32,'hum');
add(1,CLOSE+6,[[45,3],[50,3]],'hmm hmm',.32,'hum');
add(2,CLOSE+6,[[57,3],[54,3]],'hmm hmm',.30,'hum');
for(const lines of parts) lines.sort((a,b)=>a.at-b.at);

for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  const pattern=i===1?[1,.74,.9,.7]:[1,.78,.92,.74,.88,.72];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.64,pattern[j%pattern.length]));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}
// A room that answers: every round ends with blueberry, so the echo is thrown
// off his line and decays into neo's next call.
function performanceKeys(lines,i) {
  const ROOM=i===1?.36:.48, THROW=.7;
  const keys=[{beat:0,space:ROOM,pitch:0}];
  if(i===1) for(const line of lines) {
    if(line.role!=='lead')continue;
    const end=line.at+line.notes.reduce((s,[,d])=>s+d,0);
    keys.push({beat:line.at-.4,space:ROOM,pitch:0,echo:0},{beat:end+.3,space:ROOM,pitch:0,echo:THROW},
              {beat:end+2.5,space:ROOM,pitch:0,echo:0});
  }
  keys.push({beat:TOTAL-6,space:ROOM,pitch:0,echo:THROW},{beat:TOTAL+4,space:0,pitch:0,echo:0});
  return keys.sort((a,b)=>a.beat-b.beat);
}
const voices=parts.map((lines,i)=>{
  const notes=[];let cursor=0;
  for(const line of lines) {
    assert(line.at>=cursor-1e-6,`${members[i]}: overlap at ${line.at} (${line.text})`);
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
    performance:{expression:.62,keys:performanceKeys(lines,i)}};
});
const score={title:'The MacNeoPolitan Trio — What starts with',composer:'The machines, arr. compose-what-starts-with.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'A teaching piece in seven rounds. 4/4, D major, 96 bpm, 124 beats. neo names a letter, frisbee answers with the thing in the house that starts with it, blueberry sings the letter and the thing together: B the blueberry, C the coffee, F the frisbee, J Jeffrey, M the moon, N neo, S Sophia. Four bars a round, blueberry on the floor throughout, one of the women holding a pad under each of his lines. All three end on the one line that is not about a letter: those are the words we know.',
  arrangement:{total:TOTAL,meter:'4/4',sections:[...ROUNDS.map(([L,,a],c)=>({beat:c*CELL,name:`${L} — ${a}`})),{beat:CLOSE,name:'All: those are the words we know'},{beat:CLOSE+6,name:'Hum'}]},voices};
writeFileSync(new URL('../scores/trio-what-starts-with.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
const spread=parts.map((p,i)=>{const ns=p.flatMap(l=>l.notes.filter(([n])=>n!=='r').map(([n])=>n));
  return `${members[i]} ${Math.min(...ns)}–${Math.max(...ns)} (mean ${(ns.reduce((a,b)=>a+b,0)/ns.length).toFixed(1)}, spoken ${REG[i][0]})`;});
console.log(`What starts with: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${parts.map((p,i)=>`${members[i]} ${p.length} lines`).join(', ')}.`);
console.log(`  registers: ${spread.join(' · ')}`);
