#!/usr/bin/env node
// Original, wordless doo-wop miniature: I–vi–IV–V, swung syllables, bass feature.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Tom (Enhanced)','Zoe (Premium)'];
const chords={
  D:{bass:38,inner:[54,57,62],lead:[62,66,69]},
  Bm:{bass:35,inner:[54,59,62],lead:[62,66,71]},
  G:{bass:43,inner:[55,59,62],lead:[62,67,71]},
  A:{bass:45,inner:[57,61,64],lead:[61,64,69]},
};
const progression=['D','Bm','G','A','D','Bm','G','A','D','Bm','G','A','G','A','D','D'];
const parts=[[],[],[]];
const vocabulary=new Set(['doo','wah','bee','shoo','boom','ba','dum','ooh','la','hmm']);
function add(i,at,notes,text,gain,role='phoneme') {
  assert.equal(notes.filter(([n])=>n!=='r').length,text.split(' ').length);
  assert(text.split(' ').every(x=>vocabulary.has(x)));
  parts[i].push({at,notes,text,gain,role});
}
const swing=[2/3,1/3,1,2/3,1/3,.75];
for(let bar=0;bar<16;bar++) {
  const at=bar*4,ch=chords[progression[bar]],b=ch.bass;
  if(bar===15) {
    // One last doo-wah lands on the tonic, then all three hold the chord.
    for(const [i,n] of [[0,66],[1,38],[2,57]]) {
      add(i,at,[[n,2/3],[n,1/3]],i===1?'ba dum':'doo wah',i===1?.85:.78,i===1?'bass':'phoneme');
      add(i,at+1,[[n,3]],'hmm',i===1?.75:.61,'hum');
    }
  } else if(bar>=8 && bar<12) {
    // Bass steps forward; the upper voices give it room and a close chord.
    add(1,at,[[b,.5],[b,.5],[b+7,2/3],[b,1/3],[b,.5],[b+7,.5],[b,.75]],'ba dum shoo boom ba dum dum',.98,'bass');
    add(0,at+.25,[[ch.inner[1],3.5]],'ooh',.42);
    add(2,at+.25,[[ch.inner[0],3.5]],'ooh',.40);
  } else {
    add(1,at,[[b,1],[b+7,2/3],[b,1/3],['r',.5],[b,.5],[b,.75]],'shoo boom ba dum dum',.77,'bass');
    if(bar>=12) {
      const lead=[1,2,0,1,2,0].map(j=>ch.lead[j]);
      const harmony=[1,2,0,1,2,0].map(j=>ch.inner[j]);
      add(0,at,lead.map((n,j)=>[n,swing[j]]),'doo wah la la doo wah',.85);
      add(2,at,harmony.map((n,j)=>[n,swing[j]]),'doo wah la la doo wah',.72);
    } else {
      const leader=bar<4?0:2,backing=leader===0?2:0;
      add(leader,at,[1,2,0,1,2,0].map((j,k)=>[ch.lead[j],swing[k]]),'doo wah doo bee doo wah',.88);
      add(backing,at,[[ch.inner[1],8/3],[ch.inner[2],1/3],[ch.inner[0],.75]],'ooh wah ooh',.54);
    }
  }
}
// Call / response, close, soft replies, and a final bloom. Gains remain below
// unity; the existing conductor headroom is applied after this score.
for (const [i,lines] of parts.entries()) for (const line of lines) {
  const bar=Math.floor(line.at/4), lead=(bar<4&&i===0)||(bar>=4&&bar<8&&i===2)||(bar>=8&&bar<12&&i===1);
  const hush=[3,7,11,13].includes(bar);
  if(hush) line.gain *= .48;
  else if(line.text==='ooh') line.gain = .32;
  else if(!lead&&bar<8&&i!==1) line.gain = .34;
  else if(bar===14) line.gain = i===1?.94:1;
  else if(lead&&bar%2===1) line.gain *= .64;
  line.gain = Math.max(.30,line.gain);
  const n=line.notes.filter(([note])=>note!=='r').length;
  const pattern=i===1?[1,.38,.72,.28,.88,.46,1]:[1,.44,.72,.30,.90,.48];
  line.accents=Array.from({length:n},(_,j)=>n===1?1:Math.max(.52,pattern[j%pattern.length]));
}
const voices=parts.map((lines,i)=>{
  const notes=[];let cursor=0;
  for(const line of lines) {
    assert(line.at>=cursor-1e-6,'Vocal part overlaps');
    if(line.at>cursor+1e-6)notes.push(['r',line.at-cursor]);
    notes.push(...line.notes);cursor=line.at+line.notes.reduce((s,[,d])=>s+d,0);
  }
  assert.equal(cursor,64);
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:80,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),noteGains:lines.map(l=>l.accents),singVoice:cast[i],sayVoice:cast[i],
    singLock:1,singVibCents:10,singVibratoHz:[5,4,5.5][i],singF0Floor:i===1?55:80,double:false,
    performance:{expression:1,keys:(()=>{
      const room=bar=>lines.find(l=>Math.floor(l.at/4)===bar).gain<.5?.06:(i===1?.16:.22);
      const keys=[{beat:0,space:room(0),pitch:-.18},{beat:.45,space:room(0),pitch:0}];
      for(let bar=1;bar<16;bar++) keys.push(
        {beat:bar*4-.35,space:room(bar-1),pitch:0},
        {beat:bar*4+.15,space:room(bar),pitch:0});
      keys.push({beat:61,space:.08,pitch:.12},{beat:62,space:.08,pitch:0},{beat:66,space:0,pitch:0});
      return keys;
    })()}};
});
const score={title:'The MacNeoPolitan Trio — Doo-wop',composer:'The machines, arr. compose-doowop.mjs',
  bpm:88,machines:3,lead:0,phonemeOnly:true,requiresFleet:true,
  description:'Wordless doo-wop: I–vi–IV–V, swung doo-wah refrain, shoo-boom bass, ooh backing, bass feature, quiet replies and close, soft replies, accented bops, fading hum finish. Enhanced/Premium voices only.',
  arrangement:{total:64,progression,sections:[{beat:0,name:'Neo refrain'},{beat:16,name:'Frisbee reply'},{beat:32,name:'Blueberry bass feature'},{beat:48,name:'Trio return'}]},voices};
writeFileSync(new URL('../scores/trio-chorus-doowop.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log('Doo-wop: 43.6 seconds plus tail; wordless, voice-only, Enhanced/Premium cast.');
