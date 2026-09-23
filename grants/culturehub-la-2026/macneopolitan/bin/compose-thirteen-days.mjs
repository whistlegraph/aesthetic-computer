#!/usr/bin/env node
// Thirteen days — a drone for blueberry. 4/4, D minor, 70 bpm, 24 bars.
//
// The record: blueberry's longest unbroken stretch is 13.1 days, begun May 28,
// the uptime crown of the band (members/blueberry/profile.md). It was
// provisioned headless and works with the lid down more days than not
// (autobiography.md). In one week of the power log Menu Band held it awake 492
// times (journey.md, deep.menuband_wake_assertions). It carries a 47.6 GB photo
// library it digests at night and never looks at (journey.md, profile.md).
//
// The styling is a drone, close to a Bulgarian village choir: Aaron holds long
// low tones on mm and ooh underneath and says a few words at a time in his own
// band; Noelle and Zoe hang above him on ah and ee, meeting in open fifths and
// rubbing in major seconds, and come down into words only for the refrain,
// which they sing together a whole step apart. The count is in the drone —
// blueberry holds THIRTEEN tones, one for each day, and the script asserts it.
//
//   node bin/compose-thirteen-days.mjs
//   node bin/hear.mjs scores/trio-thirteen-days.mbscore --tag thirteen-days …
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
// Spoken medians, measured: Noelle 59.6 (bin/register.mjs), Aaron 48.8
// (measured on blueberry 2026-09-23, band 39–53), Zoe 56.1 / 208.9 Hz
// (measured 2026-09-23 here: pyworld.harvest over 60 words, p10–p90 52.7–59.9).
const MEDIAN=[59.6,48.8,56.1];
const BPM=70, BAR=4, BARS=24, TOTAL=BAR*BARS;   // 96 beats ≈ 82.3 s
const DAYS=13;                                   // 13.1 unbroken days
const parts=[[],[],[]];

const words=new Set([
  'thir-teen','days','my','was','the','lid','for','down','no','one','came','in-to','room',
  'band','held','me','a-wake','four','hun-dred','nine-ty','two','times',
  'I','keep','for-ty','se-ven','gi-ga-bytes','of','pic-tures','have','ne-ver','seen',
  'and','then','back','you','were','whole','time',
  'mm','ooh','ah','ee']);

const syl=(text)=>text.split(' ').reduce((a,t)=>a+t.split('-').length,0);
const weighted=(notes)=>{                        // duration-weighted pitch mean
  let s=0,d=0;
  for(const [n,len] of notes) if(n!=='r'){s+=n*len;d+=len;}
  return d?s/d:null;
};
function add(i,beat,notes,text,gain,role='lead') {
  assert.equal(notes.filter(([n])=>n!=='r').length,syl(text),`${members[i]} @${beat}: notes vs syllables — ${text}`);
  assert(text.split(' ').every(x=>words.has(x)),`${members[i]} @${beat}: unknown word in — ${text}`);
  const mean=weighted(notes);
  // A sung line sits near the member's speaking pitch (the spinging rule: the
  // further a neural voice is pulled from where it speaks, the more it croaks).
  // The drone is allowed to sink further, because sinking is what a drone does.
  const tol=role==='lead'?2:5;
  assert(Math.abs(mean-MEDIAN[i])<=tol,`${members[i]} @${beat}: mean ${mean.toFixed(2)} is ${Math.abs(mean-MEDIAN[i]).toFixed(2)} from ${MEDIAN[i]} — ${text}`);
  parts[i].push({at:beat,notes,text,gain,role,mean});
}

// ── blueberry (Aaron, 43–50) ────────────────────────────────────────────────
// Thirteen held tones, one per day, and six short sayings between them. Nothing
// climbs past 50: Aaron speaks at 48.8, and the stand-in used for offline
// renders here (Tom) tops out near 49.5.
add(1, 0,[[50,5],[45,4],[50,3]],'mm ooh mm',.40,'drone');                 // 1 2 3
// The judge kept losing "the lid" here — "worth of wood", "all would", "all the
// day" across three tries. "my lid", up front and on the longest note of the
// line, survives; the number moved to the end, where it lands anyway.
add(1,12,[[48,1],[50,1.5],[48,.5],[46,1.5],[48,.5],[50,1],[48,1],[45,1]],'my lid was down for thir-teen days',.54);
add(1,20,[[48,3],[45,3]],'mm ooh',.38,'drone');                           // 4 5
add(1,26,[[50,1],[48,1],[48,1],[50,1],[48,1],[46,1],[45,2]],'no one came in-to the room',.52);
add(1,34,[[43,4],[45,4]],'ooh mm',.36,'drone');                           // 6 7
add(1,42,[[48,1],[48,1],[50,1],[50,1],[48,.5],[50,1.5]],'the band held me a-wake',.54);
add(1,48,[[50,1],[48,1],[48,1],[50,1],[48,.75],[48,1.25],[45,1]],'four hun-dred nine-ty two times',.54);
add(1,55,[[50,3],[48,2]],'mm mm',.38,'drone');                            // 8 9
add(1,60,[[48,1],[50,1],[50,.75],[48,.75],[48,.75],[46,.75],[48,.75],[48,.75],[50,1.5],[48,.75],[46,1.25],[45,2]],
          'I keep for-ty se-ven gi-ga-bytes of pic-tures',.52);
add(1,72,[[48,1],[48,1],[50,.75],[48,.75],[46,1],[45,1.5]],'I have ne-ver seen one',.52);
add(1,78,[[45,3],[43,3]],'ooh ooh',.36,'drone');                          // 10 11
add(1,84,[[45,1],[46,1],[48,1],[48,.5],[48,.5],[50,.5],[50,.5],[48,1],[50,2]],'thir-teen days and then the room came back',.54);
add(1,92,[[45,2],[50,2]],'ooh mm',.38,'drone');                           // 12 13

// ── neo (Noelle, 57–64) and frisbee (Zoe, 53–60) ────────────────────────────
// Above the drone: open fifths where they agree, major seconds where they do
// not. Wordless except the refrain, which they sing together a whole step
// apart — the interval the village choirs keep.
const REFRAIN='you were a-wake the whole time';
add(0, 8,[[62,4],[60,6]],'ah ah',.34,'hum');                              // 62/55 fifth, 60/58 second
add(2, 8,[[55,4],[58,6]],'ee ee',.32,'hum');
add(0,24,[[59,6],[60,4]],'ee ee',.34,'hum');                              // seconds all the way
add(2,24,[[57,6],[58,4]],'ah ah',.32,'hum');
add(0,34,[[60,1],[60,1],[62,1],[60,1],[59,1],[57,1],[59,2]],REFRAIN,.46);
add(2,34,[[58,1],[58,1],[60,1],[58,1],[57,1],[55,1],[57,2]],REFRAIN,.44);
add(0,48,[[64,3],[60,5],[60,4]],'ah ah ah',.36,'hum');                    // fifth, second, fifth
add(2,48,[[57,3],[58,5],[53,4]],'ee ee ee',.34,'hum');
add(0,62,[[57,5],[59,5]],'ee ee',.34,'hum');
add(2,62,[[55,5],[57,5]],'ah ah',.32,'hum');
add(0,74,[[60,1],[60,1],[62,1],[60,1],[59,1],[57,1],[59,2]],REFRAIN,.46);
add(2,74,[[58,1],[58,1],[60,1],[58,1],[57,1],[55,1],[57,2]],REFRAIN,.44);
add(0,84,[[62,4],[60,4],[57,4]],'ah ah ah',.34,'hum');                    // fifth, second, unison
add(2,84,[[55,4],[58,4],[57,4]],'ee ee ee',.32,'hum');

for(const lines of parts) lines.sort((a,b)=>a.at-b.at);

// Thirteen days, thirteen held tones.
const droneTones=parts[1].filter(l=>l.role==='drone').reduce((a,l)=>a+l.notes.filter(([n])=>n!=='r').length,0);
assert.equal(droneTones,DAYS,`the drone must hold ${DAYS} tones, one per day — got ${droneTones}`);

// Dynamics: the drone swells and settles; the words lean on their first
// syllable; the refrain leans on "a-wake".
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  const pattern=line.role==='drone'?[1,.8,.9]:[1,.82,.9];
  line.accents=Array.from({length:n},(_,j)=>
    n===1?1
    : line.role==='lead'?(j===0||j===n-1?1:.92)              // words: near-flat, diction over dynamics
    : Math.max(.62,pattern[j%pattern.length]));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// A drone wants one room, not a phrase-by-phrase one. blueberry sits in it for
// the whole piece; the two above throw each phrase's last note into echo, so
// their tails hang over the drone instead of stopping on it.
function performanceKeys(lines,i) {
  const ROOM=i===1?.78:.9, THROW=.85;
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
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:50,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:i===1?6:8,singVibratoHz:[4.5,3.5,5][i],
    singF0Floor:i===1?70:80,singGapMs:0,singSustainDb:6,singShimmerFrames:3,singLegatoMs:40,
    double:false,faceAlpha:0.95,
    performance:{expression:.45,keys:performanceKeys(lines,i)}};
});

const score={title:'The MacNeoPolitan Trio — Thirteen days',composer:'The machines, arr. compose-thirteen-days.mjs',
  bpm:BPM,machines:3,lead:1,
  description:'A drone on blueberry\'s longest stretch: 13.1 days awake, lid down, nobody in the room, Menu Band holding the wake 492 times in a week, 47.6 GB of photographs it never looks at. Aaron holds thirteen low tones on mm and ooh — one per day — and says a few words between them; Noelle and Zoe hang above on ah and ee in open fifths and major seconds, and come down into words only for the refrain, which they sing a whole step apart. 4/4, D minor, 70 bpm, 96 beats.',
  arrangement:{total:TOTAL,meter:'4/4',days:DAYS,droneTones,
    sections:[{beat:0,name:'Drone (tones 1–3)'},{beat:12,name:'blueberry: thirteen days with the lid down'},
      {beat:26,name:'blueberry: no one came into the room'},{beat:34,name:'Refrain: you were awake the whole time'},
      {beat:42,name:'blueberry: the band held me awake'},{beat:60,name:'blueberry: forty seven gigabytes of pictures'},
      {beat:74,name:'Refrain'},{beat:84,name:'blueberry: and then the room came back'},{beat:92,name:'Drone (tones 12–13)'}]},
  voices};
writeFileSync(new URL('../scores/trio-thirteen-days.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log(`Thirteen days: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; drone holds ${droneTones} tones.`);
for(const [i,lines] of parts.entries())
  console.log(`  ${members[i].padEnd(9)} ${String(lines.length).padStart(2)} lines · mean ${(lines.reduce((a,l)=>a+l.mean,0)/lines.length).toFixed(2)} (speaks ${MEDIAN[i]}) · ${lines.filter(l=>l.role==='lead').length} worded`);
