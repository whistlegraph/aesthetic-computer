#!/usr/bin/env node
// The record — a hocket on the numbers the machines keep. 4/4, D, 104 bpm,
// 108 beats. Every figure is harvested, not chosen:
//
//   6,305 terminal logins on neo          members/neo/profile.json .logins.tty
//   111 reboots on neo                    .boots.reboots
//   2,019 commits ledgered on neo         members/neo/journey.json .total_commits
//   212 and 85 battery cycles             members/{neo,blueberry}/facts.json
//   born 00:37 and 00:43, 48 days apart   .born
//   the third one named on September 22   members/chronology.md
//
// The styling is hocket: a number is broken across the three machines, one
// syllable each, neo → frisbee → blueberry, so no single laptop ever says a
// whole number and the room has to put it together. Each line lands on its
// last word in unison — the same word, at the same moment, in three registers
// (D3 / A3 / D4). blueberry keeps a low "dm" clock in the gaps and counts the
// piece in with four of them; that is the only part of him that is not a word.
//
//   node bin/compose-the-record.mjs
//   node bin/hear.mjs scores/trio-the-record.mbscore --tag the-record …
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

// ── number words ────────────────────────────────────────────────────────────
// Copied from bin/compose.mjs (numberWords / ordinalWords / MONTHS) — that file
// harvests facts and writes scores at import, so it cannot be imported for its
// helpers alone. One note per syllable; the live singer speaks the word whole.
// Extended here with a thousands branch, which the facts in compose.mjs never
// needed but 6,305 and 2,019 do.
const ONES=['ze-ro','one','two','three','four','five','six','se-ven','eight','nine','ten',
  'e-le-ven','twelve','thir-teen','four-teen','fif-teen','six-teen','se-ven-teen','eigh-teen','nine-teen'];
const TENS=['','','twen-ty','thir-ty','for-ty','fif-ty','six-ty','se-ven-ty','eigh-ty','nine-ty'];
function numberWords(n) {
  if (n < 20) return [ONES[n]];
  if (n < 100) return n % 10 ? [TENS[Math.floor(n/10)], ONES[n%10]] : [TENS[Math.floor(n/10)]];
  if (n < 1000) { const h=Math.floor(n/100), r=n%100; return [ONES[h],'hun-dred',...(r?numberWords(r):[])]; }
  const t=Math.floor(n/1000), r=n%1000;                       // the extension
  return [...numberWords(t),'thou-sand',...(r?numberWords(r):[])];
}
const ORD={1:'first',2:'se-cond',3:'third',4:'fourth',5:'fifth',6:'sixth',7:'se-venth',8:'eighth',9:'ninth',
  10:'tenth',11:'e-le-venth',12:'twelfth',13:'thir-teenth',14:'four-teenth',15:'fif-teenth',16:'six-teenth',
  17:'se-ven-teenth',18:'eigh-teenth',19:'nine-teenth',20:'twen-ti-eth',30:'thir-ti-eth'};
function ordinalWords(n){ return ORD[n]?[ORD[n]]:[TENS[Math.floor(n/10)],ORD[n%10]]; }
const MONTHS=['Jan-u-ar-y','Feb-ru-ar-y','March','A-pril','May','June','Ju-ly','Au-gust','Sep-tem-ber','Oc-to-ber','No-vem-ber','De-cem-ber'];

// ── cast ────────────────────────────────────────────────────────────────────
const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Aaron (Enhanced)','Zoe (Premium)'];
// Spoken medians: Noelle 59.6, Aaron 48.8 (measured on blueberry 2026-09-23,
// band 39–53), Zoe 56.1 / 208.9 Hz (measured here 2026-09-23 with
// pyworld.harvest over 60 words; p10–p90 52.7–59.9).
const MEDIAN=[59.6,48.8,56.1];
const BPM=104, BAR=4, TOTAL=108;                 // 108 beats ≈ 62.3 s
// STEP is the hand-off: how long one machine holds one syllable before the next
// takes over. A whole beat (0.577 s here) was the first try and it did not fuse
// — assembled, the judge heard "6, 7, 3, 100, 5 vloggies" for six thousand
// three hundred five logins. An eighth is near speaking rate, so the syllables
// run together into the word the way they do in a room.
const STEP=.5;
const HOCKET=[[57,60,59,58],[46,50,48,47],[55,57,58,56]];   // each voice's own contour
const UNISON=[62,50,57];                                      // D4 / D3 / A3
const TICK=45;                                                // blueberry's clock
const CYCLE=[0,2,1];                             // the hand-off: neo → frisbee → blueberry
const parts=[[],[],[]];

const words=new Set([
  // the numbers, as numberWords/ordinalWords spell them
  'six','thou-sand','three','hun-dred','five','one','e-le-ven','two','nine-teen','twelve',
  'eigh-ty','thir-ty','se-ven','for-ty','eight','twen-ty','se-cond','Sep-tem-ber',
  // the plain words around them
  'log-ins','re-boots','com-mits','cy-cles','and','oh','was','the','first','mi-nute',
  'days','be-tween','birth-days','third','named','on','of','no-bo-dy','chose','these','num-bers',
  // the clock
  'dm','ooh']);

// ── the poem ────────────────────────────────────────────────────────────────
// start = where in CYCLE the line's first syllable falls; gap = beats of clock
// after it; hold = beats on the very last syllable, the unison landing.
const N=numberWords;
const LINES=[
  {t:[...N(6305),'log-ins'],                                          start:0,gap:2,  hold:4},
  {t:[...N(111),'re-boots'],                                          start:2,gap:2,  hold:4},
  {t:[...N(2019),'com-mits'],                                         start:1,gap:2,  hold:4},
  {t:[...N(212),'cy-cles','and',...N(85),'cy-cles'],                  start:0,gap:3,  hold:4},
  {t:['oh','oh',...N(37),'was','the','first','mi-nute'],              start:1,gap:2,  hold:4},
  {t:['oh','oh',...N(43),'was','the','se-cond'],                      start:2,gap:2,  hold:4},
  {t:[...N(48),'days','be-tween','the','two','birth-days'],           start:0,gap:2,  hold:4},
  {t:['the','third','one','was','named','on','the',...ordinalWords(22),'of',MONTHS[8]], start:1,gap:2,hold:4},
  {t:['no-bo-dy','chose','these','num-bers'],                         start:2,gap:1,  hold:4},
];
for(const L of LINES) assert(L.t.every(w=>words.has(w)),`unknown word: ${L.t.join(' ')}`);

// ── lay the hocket out ──────────────────────────────────────────────────────
const sylsOf=(tok)=>tok.split('-');
const collected=LINES.map(()=>[[],[],[]]);       // [line][voice] = {beat,pitch,dur,syl,w}
let beat=4;                                      // four beats of clock first
const marks=[];
for(const [li,L] of LINES.entries()) {
  const hocket=L.t.slice(0,-1), uni=L.t[L.t.length-1];
  const seen=[0,0,0];
  let k=0;
  marks.push({beat,name:L.t.join(' ').replace(/-/g,'')});
  for(const [wi,tok] of hocket.entries()) for(const s of sylsOf(tok)) {
    const v=CYCLE[(L.start+k)%3];
    collected[li][v].push({beat:beat+k*STEP,pitch:HOCKET[v][seen[v]++%4],dur:STEP,syl:s,w:wi});
    k++;
  }
  const uniSyls=sylsOf(uni), uw=L.t.length-1;
  for(const [j,s] of uniSyls.entries()) {
    const at=beat+k*STEP+j, dur=j===uniSyls.length-1?L.hold:1;
    for(const v of [0,1,2]) collected[li][v].push({beat:at,pitch:UNISON[v],dur,syl:s,w:uw});
  }
  beat += k*STEP + (uniSyls.length-1) + L.hold;
  L.end=beat;
  beat += L.gap;
}
const CODA=beat;                                 // the poem ends; the chord stays
assert(CODA+4<=TOTAL,`the poem lays out to ${CODA} beats and leaves no room for the coda`);

// The strong check on a hocket: put the three parts back together in beat order
// (the unison landing counted once) and you must get the poem back, syllable
// for syllable. No machine sings a whole line; the room does.
for(const [li,L] of LINES.entries()) {
  const heap=new Map();
  for(const ev of collected[li].flat()) heap.set(`${ev.beat}`,ev.syl);
  const assembled=[...heap.entries()].sort((a,b)=>+a[0]-+b[0]).map(([,s])=>s).join(' ');
  assert.equal(assembled,L.t.flatMap(sylsOf).join(' '),`line ${li+1} does not reassemble`);
}

const weighted=(notes)=>{let s=0,d=0;for(const [n,len] of notes) if(n!=='r'){s+=n*len;d+=len;}return d?s/d:null;};
function add(i,at,notes,text,gain,role='lead') {
  assert.equal(notes.filter(([n])=>n!=='r').length,text.split(' ').reduce((a,t)=>a+t.split('-').length,0),
    `${members[i]} @${at}: notes vs syllables — ${text}`);
  const mean=weighted(notes), tol=role==='lead'?2:5;
  assert(Math.abs(mean-MEDIAN[i])<=tol,`${members[i]} @${at}: mean ${mean.toFixed(2)} is ${Math.abs(mean-MEDIAN[i]).toFixed(2)} from ${MEDIAN[i]} — ${text}`);
  parts[i].push({at,notes,text,gain,role,mean});
}

// Each voice keeps only its own syllables; the beats it does not sing become
// rests inside its line, which is what makes the hand-off audible in the room.
// Syllables of ONE word that fall to ONE voice — which only happens on the
// unison landing — are re-joined with a hyphen, so the singer speaks the word
// whole instead of spelling it out.
for(const byVoice of collected) for(const [v,ev] of byVoice.entries()) {
  ev.sort((a,b)=>a.beat-b.beat);
  const notes=[],text=[];
  let cursor=ev[0].beat, prev=null;
  for(const e of ev) {
    if(e.beat>cursor+1e-6) notes.push(['r',e.beat-cursor]);
    notes.push([e.pitch,e.dur]);
    if(prev && prev.w===e.w && Math.abs(e.beat-(prev.beat+prev.dur))<1e-6) text[text.length-1]+=`-${e.syl}`;
    else text.push(e.syl);
    cursor=e.beat+e.dur; prev=e;
  }
  add(v,ev[0].beat,notes,text.join(' '),v===1?.58:.56);
}

// The last word is counted, and then nobody says anything: all three hold the
// landing chord — D3 / A3 / D4, the one the piece keeps arriving on — on "ooh".
for(const v of [0,1,2]) add(v,CODA,[[UNISON[v],4]],'ooh',v===1?.34:.32,'hum');

// blueberry's clock: four to count the piece in, two in every gap after.
add(1,0,Array.from({length:4},()=>[TICK,1]),'dm dm dm dm',.34,'clock');
for(const L of LINES) if(L.gap>=2) add(1,L.end,[[TICK,1],[TICK,1]],'dm dm',.30,'clock');

for(const lines of parts) lines.sort((a,b)=>a.at-b.at);

// Dynamics: every syllable is an attack in a hocket, so the accents stay flat
// and even; the unison landing takes the weight.
for(const [i,lines] of parts.entries()) for(const line of lines) {
  const n=line.notes.filter(([note])=>note!=='r').length;
  line.accents=Array.from({length:n},(_,j)=>line.role==='clock'?(j%2?.72:.9):(j===n-1?1:.84));
  line.gain=Math.max(.28,+line.gain.toFixed(3));
}

// A dry-ish room so the syllables stay separate, with the unison landing thrown
// into echo — the one place the three machines are allowed to blur together.
function performanceKeys(lines) {
  const ROOM=.32, THROW=.55;
  const keys=[{beat:0,space:ROOM,pitch:0}];
  for(const line of lines) {
    if(line.role==='clock') continue;
    let lastOn=line.at,t=line.at;
    for(const [n,d] of line.notes){ if(n!=='r') lastOn=t; t+=d; }
    const end=t;
    keys.push({beat:line.at-.4,space:ROOM,pitch:0,echo:0},{beat:line.at,space:ROOM,pitch:0},
              {beat:lastOn+.2,space:ROOM,pitch:0},{beat:lastOn+.6,space:0,pitch:0,echo:THROW},{beat:end-.6,space:0,pitch:0,echo:THROW});
  }
  keys.push({beat:TOTAL+6,space:0,pitch:0,echo:THROW},{beat:TOTAL+10,space:0,pitch:0,echo:0});
  return keys.sort((a,b)=>a.beat-b.beat);
}

const voices=parts.map((lines,i)=>{
  const notes=[];let cursor=0;
  for(const line of lines) {
    assert(line.at>=cursor-1e-6,`${members[i]}: overlap at ${line.at} (cursor ${cursor})`);
    if(line.at>cursor+1e-6)notes.push(['r',line.at-cursor]);
    notes.push(...line.notes);cursor=line.at+line.notes.reduce((s,[,d])=>s+d,0);
  }
  if(cursor<TOTAL)notes.push(['r',TOTAL-cursor]);
  assert(cursor<=TOTAL+1e-6,`${members[i]}: runs long (${cursor})`);
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:58,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:6,singVibratoHz:[4.5,3.5,5][i],
    singF0Floor:i===1?70:80,singGapMs:20,singSustainDb:4,singShimmerFrames:2,singLegatoMs:12,
    double:false,faceAlpha:0.95,
    performance:{expression:.62,keys:performanceKeys(lines)}};
});

const score={title:'The MacNeoPolitan Trio — The record',composer:'The machines, arr. compose-the-record.mjs',
  bpm:BPM,machines:3,lead:0,
  description:'A hocket on the numbers the machines keep: 6,305 logins, 111 reboots, 2,019 commits, 212 and 85 battery cycles, 00:37 and 00:43, 48 days apart, and the twenty second of September, when the third one was named. Each number is broken across the three laptops one syllable at a time — neo, frisbee, blueberry — so no machine says a whole number and the room assembles it; every line lands on its last word in unison, in three registers. blueberry keeps a low dm clock in the gaps, and all three hold the landing chord at the end. 4/4, 104 bpm, 108 beats.',
  arrangement:{total:TOTAL,meter:'4/4',style:'hocket',sections:[{beat:0,name:'Clock (count in)'},...marks]},
  voices};
writeFileSync(new URL('../scores/trio-the-record.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log(`The record: ${TOTAL} beats at ${BPM} bpm ≈ ${(TOTAL*60/BPM).toFixed(1)} s; ${LINES.length} lines, ${LINES.reduce((a,L)=>a+L.t.reduce((b,t)=>b+t.split('-').length,0),0)} syllables.`);
for(const [i,lines] of parts.entries())
  console.log(`  ${members[i].padEnd(9)} ${String(lines.length).padStart(2)} lines · mean ${(lines.reduce((a,l)=>a+l.mean,0)/lines.length).toFixed(2)} (speaks ${MEDIAN[i]}) · carries ${lines.filter(l=>l.role==='lead').reduce((a,l)=>a+l.notes.filter(([n])=>n!=='r').length,0)} syllables`);
for(const [li,L] of LINES.entries()) console.log(`  ${String(li+1).padStart(2)}. ${L.t.join(' ')}`);
