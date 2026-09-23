#!/usr/bin/env node
// An original wordless miniature for the three machines. No lyric substitution.
import {writeFileSync} from 'node:fs';
import assert from 'node:assert/strict';

const members=['neo','blueberry','frisbee'];
const cast=['Noelle (Enhanced)','Tom (Enhanced)','Zoe (Premium)'];
const parts=[[],[],[]];
const chords=[
  [38,54,57],[38,54,57],[35,50,54],[43,59,62],
  [38,54,57],[45,61,64],[40,55,59],[45,61,64],
  [35,50,54],[43,59,62],[38,54,57],[45,61,64],
  [43,59,62],[45,61,64],[38,54,57],[38,54,57],
];
const vocabulary=new Set(['la','loo','doo','dah','bah','dum','bum','hmm','ooh','ah','ee']);
function add(who,at,notes,lyrics,gain,role='phoneme') {
  assert.equal(notes.filter(([n])=>n!=='r').length,lyrics.split(' ').length);
  assert(lyrics.split(' ').every(s=>vocabulary.has(s)),'Only phonetic syllables');
  parts[who].push({at,notes,lyrics,gain,role});
}
for(let bar=0;bar<16;bar++) {
  const at=bar*4,[bass,third,fifth]=chords[bar];
  if(bar===15) {
    add(0,at,[[57,4]],'hmm',.58,'hum');
    add(1,at,[[38,4]],'hmm',.75,'hum');
    add(2,at,[[54,4]],'hmm',.58,'hum');
  } else if(bar<4) {
    add(1,at,[[bass,1.5],['r',.5],[bass+7,.75],[bass,.75]],'dum bum dum',.75,'bass');
    add(0,at,[[third,.75],[fifth,.25],[third,1],[bass+24,1],[fifth,.75]],'la loo la la loo',.82);
    add(2,at+.25,[[fifth,3.5]],bar%2?'ooh':'hmm',.46,bar%2?'phoneme':'hum');
  } else if(bar<8) {
    // Hocket: upper voices leave space for each other; bass answers below.
    add(1,at+.5,[[bass,.75],['r',1],[bass+7,.75],[bass,.75]],'dum bum dum',.72,'bass');
    add(0,at,[[third,.5],[fifth,.5],[third,.5]],'doo la loo',.82);
    add(2,at+1.75,[[fifth,.5],[third,.5],[fifth,1]],'la la ah',.84);
  } else if(bar<12) {
    // Open vowels bloom into sustained three-part chords.
    add(1,at,[[bass,3.75]],'ooh',.70,'bass');
    add(0,at,[[third,2.5],[fifth,1.25]],bar%2?'ah loo':'ooh ah',.66);
    add(2,at,[[fifth,2.5],[third,1.25]],bar%2?'ooh ah':'ah ooh',.64);
  } else {
    // The la figures meet rhythmically, then resolve into the final hum.
    add(1,at,[[bass,1],['r',.5],[bass+7,.5],[bass,1.75]],'dum bum dum',.80,'bass');
    add(0,at,[[third,2/3],[fifth,1/3],[third,2/3],[fifth,1/3],[third,1],[fifth,.75]],'la la loo la la ah',.84);
    add(2,at,[[fifth,2/3],[third,1/3],[fifth,2/3],[third,1/3],[fifth,1],[third,.75]],'la la la loo la ah',.80);
  }
}
const voices=parts.map((lines,i)=>{
  let cursor=0;const notes=[];
  for(const line of lines) {
    assert(line.at>=cursor-1e-6,'One mouth, one part');
    if(line.at>cursor)notes.push(['r',line.at-cursor]);
    notes.push(...line.notes);cursor=line.at+line.notes.reduce((n,[,d])=>n+d,0);
  }
  assert.equal(cursor,64);
  return {name:`${members[i]} sings (${cast[i]})`,program:78,velocity:80,
    notes:notes.map(([n,d])=>`${n}:${+d.toFixed(6)}`).join(','),lyrics:lines.map(l=>l.lyrics).join(' / '),
    lineRoles:lines.map(l=>l.role),lineGains:lines.map(l=>l.gain),
    singVoice:cast[i],sayVoice:cast[i],singLock:1,singVibCents:8,singVibratoHz:[5,4,5.5][i],
    singF0Floor:i===1?55:80,double:false};
});
const score={title:'The MacNeoPolitan Trio — Phoneme study',composer:'The machines, arr. compose-phonemes.mjs',
  bpm:96,machines:3,lead:0,phonemeOnly:true,
  description:'Wordless vocal trio: la/loo motifs, bass dum/bum, hocket, open-vowel chords, shared hum. No instruments or spoken material.',
  arrangement:{total:64,sections:[{beat:0,name:'la motifs'},{beat:16,name:'hocket'},{beat:32,name:'open vowels'},{beat:48,name:'convergence'}]},voices};
const path=new URL('../scores/trio-chorus-phonemes.mbscore',import.meta.url);
writeFileSync(path,JSON.stringify(score,null,2)+'\n');
console.log('Phoneme study: 40 seconds; 48 vocal phrases; three Enhanced/Premium voices; no words or instruments.');
