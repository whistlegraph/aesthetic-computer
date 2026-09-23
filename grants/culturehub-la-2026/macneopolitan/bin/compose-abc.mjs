#!/usr/bin/env node
// A B C — the alphabet, arranged for three machines and deliberately not sung
// to Twinkle. 4/4, D major, 100 bpm, 29 bars. The letters climb the trio: the
// bass says A–G, the mezzo H–N, the soprano O–S, and all three take T–Z
// together. Then the same ladder again with every phrase turned over, and one
// plain line at the end.
//
// Measured here 2026-09-23 before a note was written (hear/letters-*.json,
// hear/probe5-*.json):
//   · A letter ALONE on a line is not a letter. Apple's engine reads it out
//     as "capital B" — two words crammed on one note, 1.2–1.5 s where 0.6 s
//     was written. In a RUN of letters it says only the names. So every
//     letter line here is a run, and no run is ever split across a line.
//   · "W" needs no help: inside a run all three sing it and whisper writes W.
//     Spelled as "dou-ble-you" it renders as mush ("There we are", 300 %).
//   · A letter name wants exactly one beat — 0.6 s at this tempo. Stretched
//     to 1.5 beats the same run got worse, not better (29 % vs 0 % on neo).
//   · In blueberry's register E inside a run is heard as A (four contours,
//     all 14 %). Giving E the top note and two beats fixes it: 0 %.
//   · Five letters is the length whisper most often runs together; neo's
//     turned-over O P Q R S only held at 1.25 beats a letter (0 % twice,
//     100 % at 1). Seven-letter runs never needed it.
//   · Whisper sometimes writes a low male run with no spaces ("OPQRSTU"),
//     which the scorer counts as one wrong word though every letter is there
//     and in order. blueberry carries that cost; it is the judge, not him.
//
// Registers: neo (Noelle) 59.6, band 57–62 · blueberry (Aaron) 48.8, band
// 39–53, rendered here with Tom as a stand-in · frisbee (Zoe) 55.6, 52.5–59.4.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
import {checkBand} from './vocalisms.mjs';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
const REG=[[59.6,57,62],[48.8,43,53],[55.6,52.5,59.4]];   // [spoken median, low, high]
const BPM=100, BAR=4, BARS=29, TOTAL=BAR*BARS;            // 116 beats ≈ 69.6 s
const LETTERS=new Set('ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split(''));
const words=new Set(['twen-ty','six','let-ters','that','is','all','of','them',
  'and','now','we','know','sang','in','or-der','dm','ooh','doo','hmm']);
const parts=[[],[],[]];
function add(i,at,notes,text,gain,role='lead') {
  const sung=notes.filter(([n])=>n!=='r');
  const toks=text.split(' ');
  assert.equal(sung.length,toks.reduce((a,t)=>a+t.split('-').length,0),text);
  assert(toks.every(t=>words.has(t)||LETTERS.has(t)),text);
  // A run of letters is one unbroken line: no letter is ever split across
  // lines, and a line of letters holds nothing else.
  const letters=toks.filter(t=>LETTERS.has(t));
  assert(letters.length===0||letters.length===toks.length,`${members[i]}: letters mixed with words in "${text}"`);
  if(role==='lead') for(const [,d] of sung) {
    // one beat a syllable is the floor, 1.45 s is the ceiling — a word held
    // longer stops being a word, and long notes belong to the hums.
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

// neo — the top of the ladder. O P Q R S, then the last run with everyone.
add(0,  8,[[59,2],[57,2],[59,2],[61,2]],'doo doo doo doo',.30,'hum');
add(0, 16,[[59,2],[61,2],[62,2],[61,2]],'doo doo doo doo',.30,'hum');
add(0, 24,[[57,1],[59,1],[61,1],[62,1],[61,1]],'O P Q R S',.56);
add(0, 32,[[59,1],[61,1],[62,1],[61,1],[59,1],[57,1],[59,1]],'T U V W X Y Z',.58);
add(0, 40,[[59,1],[57,1],[59,1],[61,1],[59,2]],'twen-ty six let-ters',.54);
add(0, 48,[[59,4],[57,4]],'hmm hmm',.28,'hum');
add(0, 56,[[59,4],[61,4]],'hmm hmm',.28,'hum');
add(0, 64,[[62,2],[61,2],[59,2],[57,2]],'doo doo doo doo',.30,'hum');
add(0, 72,[[59,2],[61,2],[59,2],[57,2]],'doo doo doo doo',.30,'hum');
add(0, 80,[[62,1.25],[61,1.25],[59,1.25],[57,1.25],[59,1.25]],'O P Q R S',.56);
add(0, 88,[[62,1],[61,1],[59,1],[57,1],[59,1],[61,1],[62,1]],'T U V W X Y Z',.58);
add(0, 96,[[57,1],[59,1],[61,1],[59,1],[57,1],[59,2]],'and now we know them all',.58);
add(0,104,[[59,4],[62,4]],'hmm hmm',.32,'hum');

// blueberry — A B C D E F G is his, down where he lives, and he starts the
// song: the alphabet begins at the bottom of the trio and climbs out of it.
add(1,  0,[[45,2],[47,2],[50,2],[47,2]],'dm dm dm dm',.36,'hum');
add(1,  8,[[47,1],[49,1],[50,1],[49,1],[50,2],[49,1],[47,1]],'A B C D E F G',.54);
add(1, 16,[[47,2],[49,2],[47,2],[50,2]],'dm dm dm dm',.34,'hum');
add(1, 24,[[47,2],[50,2],[47,2],[49,2]],'dm dm dm dm',.34,'hum');
add(1, 32,[[47,1],[49,1],[50,1],[49,1],[47,1],[45,1],[47,1]],'T U V W X Y Z',.56);
add(1, 40,[[45,2],[47,2],[49,2],[47,2]],'dm dm dm dm',.34,'hum');
add(1, 48,[[47,2],[49,2],[50,2],[49,2]],'dm dm dm dm',.34,'hum');
add(1, 56,[[47,1],[49,1],[50,1],[49,1],[47,2]],'that is all of them',.52);
add(1, 64,[[50,1],[49,1],[47,1],[49,1],[50,2],[49,1],[47,1]],'A B C D E F G',.54);
add(1, 72,[[47,2],[45,2],[47,2],[49,2]],'dm dm dm dm',.34,'hum');
add(1, 80,[[49,2],[47,2],[50,2],[47,2]],'dm dm dm dm',.34,'hum');
add(1, 88,[[50,1],[49,1],[47,1],[45,1],[47,1],[49,1],[50,1]],'T U V W X Y Z',.56);
add(1, 96,[[47,1],[49,1],[50,1],[49,1],[47,1],[45,2]],'and now we know them all',.56);
add(1,104,[[45,4],[50,4]],'hmm hmm',.32,'hum');

// frisbee — H I J K L M N, the middle of the alphabet and the middle of the
// three voices. The cleanest letter singer of the trio: 0 % on every run.
add(2,  0,[[55,4],[55,4]],'ooh ooh',.28,'hum');
add(2,  8,[[55,4],[57,4]],'ooh ooh',.28,'hum');
add(2, 16,[[54,1],[55,1],[57,1],[59,1],[57,1],[55,1],[54,1]],'H I J K L M N',.54);
add(2, 24,[[55,4],[54,4]],'ooh ooh',.28,'hum');
add(2, 32,[[55,1],[57,1],[59,1],[57,1],[55,1],[54,1],[55,1]],'T U V W X Y Z',.56);
add(2, 40,[[57,4],[55,4]],'ooh ooh',.28,'hum');
add(2, 48,[[55,1],[57,1],[59,1],[57,1],[55,1],[54,2]],'we sang them in or-der',.52);
add(2, 56,[[54,4],[55,4]],'ooh ooh',.28,'hum');
add(2, 64,[[55,4],[57,4]],'ooh ooh',.28,'hum');
add(2, 72,[[59,1],[57,1],[55,1],[54,1],[55,1],[57,1],[59,1]],'H I J K L M N',.54);
add(2, 80,[[57,4],[55,4]],'ooh ooh',.28,'hum');
add(2, 88,[[59,1],[57,1],[55,1],[54,1],[55,1],[57,1],[59,1]],'T U V W X Y Z',.56);
add(2, 96,[[55,1],[57,1],[59,1],[57,1],[55,1],[54,2]],'and now we know them all',.56);
add(2,104,[[57,4],[54,4]],'hmm hmm',.30,'hum');
for(const lines of parts) lines.sort((a,b)=>a.at-b.at);

// Dynamics: the first letter of a run lands, the rest walk.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  const pattern=i===1?[1,.74,.9,.7]:[1,.78,.92,.74,.88,.72];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.64,pattern[j%pattern.length]));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// A classroom, not a bedside: a small room, and the only echo is thrown off
// the last line, so "and now we know them all" is what keeps going.
function performanceKeys(lines,i) {
  const ROOM=i===1?.34:.46, THROW=.8;
  const keys=[{beat:0,space:ROOM,pitch:0}];
  const last=lines.find(l=>l.text==='and now we know them all');
  if(last) {
    let lastOn=last.at,t=last.at;
    for(const [n,d] of last.notes){ if(n!=='r') lastOn=t; t+=d; }
    keys.push({beat:last.at-.4,space:ROOM,pitch:0,echo:0},{beat:lastOn+.3,space:ROOM,pitch:0},
              {beat:lastOn+.9,space:ROOM,pitch:0,echo:THROW});
  }
  keys.push({beat:TOTAL-4,space:ROOM,pitch:0,echo:THROW},{beat:TOTAL+4,space:0,pitch:0,echo:0});
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
    performance:{expression:.6,keys:performanceKeys(lines,i)}};
});
// Every letter once, in order, and no letter in two places.
const said=voices.flatMap((v,i)=>parts[i].flatMap(l=>l.text.split(' ').filter(t=>LETTERS.has(t))));
assert.equal(new Set(said).size,26,'all twenty six letters appear');
const score={title:'The MacNeoPolitan Trio — A B C',composer:'The machines, arr. compose-abc.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'The alphabet in the trio\'s own arrangement, not the Twinkle tune. 4/4, D major, 100 bpm, 116 beats. The letters climb the three voices: blueberry takes A B C D E F G at the bottom, frisbee H I J K L M N in the middle, neo O P Q R S on top, and all three sing T U V W X Y Z together. Between the two passes neo counts them (twenty six letters), frisbee says how (we sang them in order) and blueberry closes the count (that is all of them). The second pass turns every phrase over — the runs that rose now fall — and all three land on and now we know them all. Every letter line is an unbroken run, because a letter alone on a line is read out as "capital B".',
  arrangement:{total:TOTAL,meter:'4/4',sections:[{beat:0,name:'Vamp'},{beat:8,name:'blueberry: A B C D E F G'},{beat:16,name:'frisbee: H I J K L M N'},{beat:24,name:'neo: O P Q R S'},{beat:32,name:'All: T U V W X Y Z'},{beat:40,name:'neo: twenty six letters'},{beat:48,name:'frisbee: we sang them in order'},{beat:56,name:'blueberry: that is all of them'},{beat:64,name:'blueberry: A B C D E F G (turned over)'},{beat:72,name:'frisbee: H I J K L M N'},{beat:80,name:'neo: O P Q R S'},{beat:88,name:'All: T U V W X Y Z'},{beat:96,name:'All: and now we know them all'},{beat:104,name:'Hum'}]},voices};
writeFileSync(new URL('../scores/trio-abc.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
const spread=parts.map((p,i)=>{const ns=p.flatMap(l=>l.notes.filter(([n])=>n!=='r').map(([n])=>n));
  return `${members[i]} ${Math.min(...ns)}–${Math.max(...ns)} (mean ${(ns.reduce((a,b)=>a+b,0)/ns.length).toFixed(1)}, spoken ${REG[i][0]})`;});
console.log(`A B C: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${parts.map((p,i)=>`${members[i]} ${p.length} lines`).join(', ')}.`);
console.log(`  registers: ${spread.join(' · ')}`);
