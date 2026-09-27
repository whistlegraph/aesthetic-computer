#!/usr/bin/env node
// Short wordless reinterpretation for the three existing Apple singers.
import {readFileSync,writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
const base=JSON.parse(readFileSync(new URL('../scores/trio-chorus-doowop.mbscore',import.meta.url)));
const bpm=144, bars=18, parts=[[],[],[]];
const chords=[{bass:36,third:63,fifth:67},{bass:39,third:67,fifth:70},{bass:44,third:60,fifth:63},{bass:43,third:59,fifth:62}];
// Chromatic C-minor hook, traded between voices; a newly voiced backing.
const hook=[[60,.5],[63,.5],[62,.5],[60,.5],[66,.5],[62,.5],[63,.25],[62,.25],[60,.5]];
const replies=[[67,.5],[67,.5],[70,.5],[68,.5],[67,1],[63,.5],[62,.5]];
function add(i,notes,text,gain){assert.equal(notes.filter(([n])=>n!=='r').length,text.split(' ').length);parts[i].push({notes,text,gain});}
for(let bar=0;bar<bars;bar++){
 const chord=bar<2||bar>=16?chords[0]:chords[(bar-2)%4];
 if(bar===17){
  add(0,[[63,1],[60,3]],'doo hmm',.64);
  add(1,[[36,1],[36,3]],'dum hmm',.8);
  add(2,[[67,1],[63,3]],'la ooh',.57);continue;
 }
 const lead=bar<8||bar>=14?0:2,back=lead===0?2:0;
 const quiet=bar===7||bar===13;
 let motif=bar<2||bar>=14||bar%4===0?hook:replies.map(([n,d])=>[n+(bar%4===3?-1:0),d]);
 add(lead,motif,motif.length===9?'dee la la dum bee la la la doo':'la la la la doo la wah',quiet?.47:.88);
 add(back,[[chord.third,1.5],[chord.fifth,.5],[chord.third,2]],'ooh la hmm',quiet?.32:.48);
 add(1,[[chord.bass,.75],['r',.25],[chord.bass,.5],[chord.bass+7,.5],[chord.bass,.75],['r',.25],[chord.bass+7,.5],[chord.bass,.5]],'dum ba boom dum bee bum',quiet?.52:.9);
}
const voices=parts.map((lines,i)=>({...base.voices[i],
 notes:lines.flatMap(l=>l.notes).map(([n,d])=>`${n}:${d}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
 lineRoles:lines.map(()=>i===1?'bass':'phoneme'),lineGains:lines.map(l=>l.gain),
 noteGains:lines.map(l=>l.notes.filter(([n])=>n!=='r').map((_,j)=>[1,.72,.86,.64][j%4])),
 singVibCents:i===1?6:9,
 performance:{expression:1,keys:(()=>{
 const keys=[];
 for(let bar=0;bar<18;bar++) {
  const lead=(bar<8||bar>=14)?0:2;
  const space=i===1?.14:i===lead?.28:.4;
  const echo=i===1?.18:i===lead?.5:.66;
  keys.push({beat:bar*4,space,pitch:0},
   {beat:bar*4+2.35,space,pitch:0},
   {beat:bar*4+3.1,space:0,echo,pitch:0},
   {beat:bar*4+3.75,space:0,echo,pitch:0});
 }
 keys.push({beat:72,space:0,echo:i===1?.16:.64,pitch:0},
  {beat:76,space:0,echo:0,pitch:0});
 return keys;
 })()}
}));
for(const v of voices)assert.equal(v.notes.split(',').reduce((s,n)=>s+Number(n.split(':')[1]),0),72);
const score={...base,title:'Toxic — MacNeoPolitan wordless sketch',composer:'After Toxic, performed by Britney Spears; short trio reinterpretation',bpm,requiresFleet:true,
 description:'30-second wordless chromatic C-minor sketch; traded hook, hummed thirds and fifths, syncopated bass. Newly voiced accompaniment, no source lyrics or recording.',
 arrangement:{total:72,sections:[{beat:0,name:'Chromatic hook'},{beat:8,name:'Neo lead'},{beat:32,name:'Frisbee lead'},{beat:56,name:'Hook return'},{beat:68,name:'Hum close'}]},voices};
writeFileSync(new URL('../scores/trio-toxic-sketch.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log('30 seconds at 144 BPM; three pinned Enhanced/Premium voices.');
