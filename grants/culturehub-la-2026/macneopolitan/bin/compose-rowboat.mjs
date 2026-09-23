#!/usr/bin/env node
import {readFileSync,writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';
const base=JSON.parse(readFileSync(new URL('../scores/trio-toxic-sketch.mbscore',import.meta.url)));
// Traditional public-domain melody. Two 24-beat verses at 96 BPM = 30 seconds.
const tune=[
 [[60,1.5],[60,1.5],[60,1],[62,.5],[64,1.5]],
 [[64,1],[62,.5],[64,1],[65,.5],[67,3]],
 [[72,.5],[72,.5],[72,.5],[67,.5],[67,.5],[67,.5],[64,.5],[64,.5],[64,.5],[60,.5],[60,.5],[60,.5]],
 [[67,1],[65,.5],[64,1],[62,.5],[60,3]]
];
const words=['Row row row your boat','Gent-ly down the stream','Mer-ri-ly mer-ri-ly mer-ri-ly mer-ri-ly','Life is but a dream'];
const parts=[[],[],[]];
for(let verse=0;verse<2;verse++)for(let phrase=0;phrase<4;phrase++){
 const leader=verse===0?0:2,backing=leader===0?2:0;
 parts[leader].push({notes:tune[phrase],text:words[phrase],gain:.88,role:'lead'});
 const harmony=phrase===1?[59,62]:phrase===3?[59,64]:[64,67];
 parts[backing].push({notes:[[harmony[0],3],[harmony[1],3]],text:phrase===3?'ooh hmm':'ooh doo',gain:.38,role:'harmony'});
 const bass=phrase===1?43:36;
 parts[1].push({notes:[[bass,1.5],[bass+7,1.5],[bass,1.5],[phrase===3?36:bass+7,1.5]],text:phrase===3?'dum doo dum hmm':'bum doo bum doo',gain:.7,role:'bass'});
}
const voices=parts.map((lines,i)=>({...base.voices[i],
 notes:lines.flatMap(l=>l.notes).map(([n,d])=>`${n}:${d}`).join(','),lyrics:lines.map(l=>l.text).join(' / '),
 lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),
 noteGains:lines.map(l=>l.notes.map((_,j)=>l.role==='lead'?[1,.86,.93,.82][j%4]:[1,.75][j%2])),
 performance:{expression:.9,keys:lines.flatMap((l,j)=>[
  {beat:j*6,space:i===1?.12:l.role==='lead'?.25:.4,pitch:0},
  {beat:j*6+4.1,space:i===1?.12:l.role==='lead'?.25:.4,pitch:0},
  {beat:j*6+5,space:0,echo:i===1?.15:l.role==='lead'?.5:.62,pitch:0},
  {beat:j*6+5.8,space:0,echo:i===1?.15:l.role==='lead'?.5:.62,pitch:0}
 ]).concat([{beat:48,space:0,echo:i===1?.1:.5,pitch:0},{beat:51,space:0,echo:0,pitch:0}])}
}));
for(const v of voices){
 assert.equal(v.notes.split(',').reduce((s,n)=>s+Number(n.split(':')[1]),0),48);
 assert.equal(v.notes.split(',').length,v.lyrics.split(' / ').flatMap(l=>l.split(/\s+/).flatMap(w=>w.split('-'))).length);
}
const score={...base,title:'Row, Row, Row Your Boat — MacNeoPolitan Trio',composer:'Traditional; trio arrangement',bpm:96,
 phonemeOnly:false,requiresFleet:true,description:'Two short verses: Neo then Frisbee sings the words, with soft humming and doo backing over Blueberry bass. MenuBand phrase-ending echo and room.',
 arrangement:{total:48,sections:[{beat:0,name:'Neo lead'},{beat:24,name:'Frisbee lead'}]},voices};
writeFileSync(new URL('../scores/trio-rowboat.mbscore',import.meta.url),JSON.stringify(score,null,2)+'\n');
console.log('Rowboat: 30 seconds; lyric lead plus wordless harmony and bass.');
