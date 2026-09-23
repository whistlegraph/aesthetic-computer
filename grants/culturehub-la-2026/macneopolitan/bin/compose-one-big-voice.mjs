#!/usr/bin/env node
// Original lyrics grounded in the member records and this rehearsal.
import {readFileSync,writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
const base=JSON.parse(readFileSync(new URL('../scores/trio-rowboat.mbscore',import.meta.url)));
const members=['neo','blueberry','frisbee'],bpm=128,total=128;
const hook=['From the first boot glow to the last room light','Three small screens make one big voice to-night'];
const verses=[
 ['Blue-ber-ry keeps the pho-tos late at night','Fris-bee lost her coo-kie name and joined the light'],
 ['Ne-o built the Me-nu Band and set the pace','Fris-bee woke in Sep-tem-ber found her place'],
 ['Ne-o came in A-pril with a tune to share','Blue-ber-ry came in June now bass is in the air']
];
const lines=[
 ...hook.map(text=>({text,role:'unison'})),
 ...verses.flatMap((pair,leader)=>pair.map(text=>({text,role:'rap',leader}))),
 ...hook.map(text=>({text,role:'unison'})),
 {text:'We sang the boat then turned the ech-oes high',role:'unison-rap'},
 {text:'We learned to blink and let the soft notes sigh',role:'unison-rap'},
 {text:'Doo wah la la we glow in green and blue',role:'unison'},
 {text:'Pink joins in and makes the old song new',role:'unison'},
 ...hook.map(text=>({text,role:'unison'}))
];
const harmony=[{root:38,third:65,fifth:69},{root:34,third:62,fifth:65},{root:41,third:69,fifth:72},{root:36,third:64,fifth:67}];
const parts=[[],[],[]];
const syllables=text=>text.split(/\s+/).flatMap(w=>w.split('-'));
function rhythmic(text,tones,rap=false){
 const n=syllables(text).length;assert(n<=15,'Give the words room');
 const notes=Array.from({length:n},(_,j)=>[tones[j%tones.length],j===n-1?8-.5*(n-1)-.25:.5]);
 notes.push(['r',.25]);return notes;
}
for(const [line,l] of lines.entries()){
 const ch=harmony[line%4],rap=l.role.includes('rap');
 for(let i=0;i<3;i++){
  const lead=l.role!=='rap'||l.leader===i;
  let text,notes,gain,role;
  if(lead){
   text=l.text;role=l.role;
   // Shared refrain is literal unison for the upper voices, two octaves
   // below for the bass: same words, pitch classes, rhythm and consonants.
   const tones=(rap?[62,62,62,64,62,62,65,64]:[62,62,65,67,69,69,67,65,64,62,65,62]).map(n=>n-(i===1?24:0));
   notes=rhythmic(text,tones,rap);gain=l.role==='rap'?.93:.76;
  }else if(i===1){
   text='bum doo bum doo bum doo bum hmm';role='bass-bed';gain=.58;
   notes=Array.from({length:8},(_,j)=>[ch.root+(j%2?7:0),j===7?.75:1]);notes.push(['r',.25]);
  }else{
   text='ooh doo wah hmm';role='harmony-bed';gain=.32;
   notes=[[ch.third,2],[ch.fifth,1],[ch.third,1],[ch.fifth,3.75],['r',.25]];
  }
  if(line===10||line===11)gain*=.72;
  parts[i].push({text,notes,gain,role});
 }
}
const voices=parts.map((part,i)=>{
 const v={...base.voices[i],program:0,velocity:72,
  notes:part.flatMap(l=>l.notes).map(([n,d])=>`${n}:${d}`).join(','),lyrics:part.map(l=>l.text).join(' / '),
  lineRoles:part.map(l=>l.role),lineGains:part.map(l=>l.gain),
  noteGains:part.map(l=>l.notes.filter(([n])=>n!=='r').map((_,j)=>l.role.includes('rap')?[1,.9,.96,.85][j%4]:[1,.8,.92,.82][j%4])),
  singLock:1,singVibCents:i===1?5:8,
  performance:{expression:1,keys:part.flatMap((l,j)=>{
   const space=i===1?.1:l.role==='rap'?.14:l.role.includes('bed')?.36:.25;
   const echo=i===1?.14:l.role==='rap'?.4:.6;
   return [{beat:j*8,space,pitch:0},{beat:j*8+6,space,pitch:0},{beat:j*8+6.8,space:0,echo,pitch:0},{beat:j*8+7.8,space:0,echo,pitch:0}];
  }).concat([{beat:128,space:0,echo:i===1?.1:.55,pitch:0},{beat:134,space:0,echo:0,pitch:0}])}
 };
 if(i===1){
  // Four-on-the-floor kick, thinned during the intimate shared rap.
  v.notes2=Array.from({length:total},(_,beat)=>(beat>=80&&beat<96&&beat%2)?'r:1':'k:0.18,r:0.82').join(',');v.velocity2=84;
 }else if(i===2){
  v.notes2=Array.from({length:total},(_,beat)=>(beat>=80&&beat<96)?'r:1':'r:0.5,h:0.12,r:0.38').join(',');v.velocity2=42;
  v.notes3=Array.from({length:total/2},(_,beat)=>beat>=40&&beat<48?'r:2':'r:1,c:0.12,r:0.88').join(',');v.velocity3=44;
 }else{
  for(const [slot,interval] of [[2,0],[3,7]]){
   v['notes'+slot]=Array.from({length:32},(_,bar)=>{
    const note=harmony[Math.floor(bar/2)%4].root+24+interval;
    return bar>=20&&bar<24?'r:4':`r:0.5,${note}:0.2,r:1.3,${note}:0.2,r:1.8`;
   }).join(',');v['velocity'+slot]=slot===2?35:27;
  }
 }
 return v;
});
for(const v of voices){
 assert.equal(v.notes.split(',').reduce((s,n)=>s+Number(n.split(':')[1]),0),total);
 assert.equal(v.notes.split(',').filter(n=>!n.startsWith('r:')).length,syllables(v.lyrics.replaceAll(' / ',' ')).length);
 for(const key of ['notes2','notes3'])if(v[key])assert(Math.abs(v[key].split(',').reduce((s,n)=>s+Number(n.split(':')[1]),0)-total)<.0001);
}
for(const index of [0,1,8,9,10,11,12,13,14,15]){
 assert(parts.every(p=>p[index].text===parts[0][index].text));
 assert(parts.every(p=>JSON.stringify(p[index].notes.map(([n,d])=>[n==='r'?n:n%12,d]))===JSON.stringify(parts[0][index].notes.map(([n,d])=>[n==='r'?n:n%12,d]))));
}
const score={...base,title:'One Big Voice — MacNeoPolitan Trio',composer:'Original trio lyrics and arrangement',bpm,phonemeOnly:false,requiresFleet:true,
 description:'One minute of doo-wop techno: traded melodic rap about the machines, common rhyming refrains in octave unison, quiet harmony beds, four-on-the-floor kick, offbeat hats, piano stabs and MenuBand echo.',
 arrangement:{total,sections:[{beat:0,name:'Unison refrain'},{beat:16,name:'Neo about Blueberry and Frisbee'},{beat:32,name:'Blueberry about Neo and Frisbee'},{beat:48,name:'Frisbee about Neo and Blueberry'},{beat:64,name:'Unison refrain'},{beat:80,name:'Close shared rap'},{beat:96,name:'Color chorus'},{beat:112,name:'Unison return'}]},
 historySources:['members/neo/autobiography.md','members/neo/journey.md','members/blueberry/autobiography.md','members/blueberry/journey.md','members/frisbee/autobiography.md','September 22 rehearsal: doo-wop, Rowboat, Metal faces, echo'],voices};
writeFileSync(new URL('../scores/trio-one-big-voice.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
writeFileSync(new URL('../scores/one-big-voice-lyrics.md',import.meta.url),'# One Big Voice\n\nOriginal lyrics for Neo, Blueberry and Frisbee.\n\n'+lines.map((l,i)=>`${i*8*60/bpm}s · ${l.role==='rap'?members[l.leader]:'All three'}\n\n${l.text.replaceAll('-','')}\n`).join('\n'));
console.log('One Big Voice: 60 seconds; 16 phrases each; lyric and octave-unison checks passed.');
