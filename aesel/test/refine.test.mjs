import test from 'node:test';import assert from 'node:assert/strict';
import {mkdtempSync,mkdirSync,writeFileSync,readFileSync,existsSync,rmSync} from 'node:fs';
import {tmpdir} from 'node:os';import {join} from 'node:path';
import {refineTask,parseChoice,parseExperience,materialise,changed,validate} from '../bin/refine.mjs';

const edit=(stage,file,from,to)=>{const path=join(stage,'context/harness/aesel',file);const text=readFileSync(path,'utf8');assert.ok(text.includes(from),`${from} in ${file}`);writeFileSync(path,text.replace(from,to));};

test('replies parse into a choice and an evidence-backed lesson, or none',()=>{
 assert.deepEqual(parseChoice('<why>B runs.</why>\n```choice\nB\n```'),{choice:'B',why:'B runs.'});
 assert.equal(parseChoice('I like B').choice,'');
 const full=parseExperience('<edits>\nsection: x\nverdict: helped\n</edits>\n<experience>\ncue: games with keys\nlesson: in piece-visual, ask for touch too\nevidence: row x\nconfidence: low\n</experience>');
 assert.equal(full.lesson.cue,'games with keys');assert.match(full.edits,/verdict: helped/);
 assert.equal(parseExperience('<edits>\nsection: x\n</edits>\nNO_ENTRY').lesson,null);
});

test('a starting harness lays its sections over the source, and an edit must keep every section and placeholder',()=>{
 const dir=mkdtempSync(join(tmpdir(),'from-'));
 try{
  mkdirSync(join(dir,'from/aesel'),{recursive:true});writeFileSync(join(dir,'from/aesel/systemprompt.md'),'## piece-visual\n\nBREATHE.\n');
  const full=materialise(join(dir,'from'),join(dir,'full'));
  assert.match(readFileSync(join(full,'aesel/systemprompt.md'),'utf8'),/## piece-visual\n\nBREATHE\.\n/);
  assert.match(readFileSync(join(full,'aesel/guardrails.md'),'utf8'),/## dev-servers/);
  assert.deepEqual(Object.keys(changed(materialise('',join(dir,'source')),full)),['aesel/piece-visual']);
  validate(full);
  const path=join(full,'aesel/systemprompt.md');
  writeFileSync(path,readFileSync(path,'utf8').replace('{{model}}','the model').replace(/## corner-label\n\n[^\n]+\n/,'')+'\n## brand-new\n\nx\n');
  assert.throws(()=>validate(full),e=>/interface-model must keep exactly the placeholders \{model\}/.test(e.message)&&/corner-label was removed/.test(e.message)&&/brand-new/.test(e.message));
 }finally{rmSync(dir,{recursive:true,force:true});}
});

test('a round proposes, retries a rejected edit, composes, selects blind and leaves a reviewable harness and a lesson',async()=>{
 const runs=mkdtempSync(join(tmpdir(),'refine-runs-')),library=mkdtempSync(join(tmpdir(),'refine-lib-'));
 const calls=[];
 const claude=async(stage,mission)=>{
  const brief=readFileSync(join(stage,'MISSION.md'),'utf8');calls.push(stage.split('/').pop()+(mission.includes('REJECTED')?':retry':''));
  if(brief.includes('HOW THE MODEL WORKS')){assert.ok(existsSync(join(stage,'context/rollouts/base/frames/f.png')),'the trace travels with its frames');
   edit(stage,'tool_guidance.md','Avoid a planning preamble','Never plan aloud; avoid a planning preamble');return {text:'Cut the preamble.',usd:.01};}
  if(brief.includes('WHAT A PIECE-MAKER KNOWS')){
   if(!mission.includes('REJECTED')){edit(stage,'systemprompt.md','identifier for this request is {{model}}','identifier for this request is the model');return {text:'oops',usd:.01};}
   edit(stage,'systemprompt.md','identifier for this request is the model','identifier for this request is {{model}}');
   edit(stage,'guardrails.md','Dev servers: do not stop','Dev servers: never stop');return {text:'Fixed.',usd:.01};
  }
  if(brief.includes('COMPOSE ONE HARNESS')){edit(stage,'tool_guidance.md','Avoid a planning preamble','Never plan aloud; avoid a planning preamble');return {text:'Kept inv.',usd:.01};}
  if(brief.includes('CHOOSE THE BEST PIECE')){assert.ok(!existsSync(join(stage,'context/pool')),'the selector never sees harnesses');assert.ok(existsSync(join(stage,'context/outputs/A/piece.mjs')));return {text:'<why>It runs.</why>\n```choice\nA\n```',usd:.01};}
  if(brief.includes('WHAT THIS ROUND TAUGHT'))return {text:'<edits>\nsection: no-preamble\nverdict: helped\n</edits>\n<experience>\ncue: a short typographic request\nlesson: in no-preamble, forbid planning aloud\nevidence: no-preamble helped\nconfidence: low\n</experience>',usd:.01};
  throw Error('unexpected stage '+stage);
 };
 const rollout=async(task,harness,out)=>{mkdirSync(join(out,'source'),{recursive:true});mkdirSync(join(out,'frames'));
  writeFileSync(join(out,'source/turn-1.mjs'),`// ${harness.split('/').pop()}\nexport function paint({wipe}){wipe('navy')}\n`);
  writeFileSync(join(out,'frames/f.png'),'png');writeFileSync(join(out,'trace.md'),'## Turn 1\n\n![4000 ms](frames/f.png)\n');
  return {delivered:true,turns:[{turn:1,delivered:true,acceptance:'checked',notes:[]}]};};
 try{
  const summary=await refineTask({id:'piece-typography-counter',kind:'piece',domain:'typography',requests:['a counter']},{runs,library,claude,rollout,log:()=>{}});
  assert.ok(calls.includes('propose-dom:retry'),'a harness that drops a placeholder gets one more try');
  assert.deepEqual(Object.keys(summary.delivered).sort(),['base','composed','dom','inv']);
  const round=join(runs,'piece-typography-counter');
  if(summary.changed.length){validate(materialise(join(round,'adopted'),join(round,'check')));}
  assert.match(readFileSync(join(round,'changes.md'),'utf8'),/chose/);
  assert.match(readFileSync(join(library,'typography/INDEX.md'),'utf8'),/cue: a short typographic request/);
  assert.match(readFileSync(join(library,'typography/entries/piece-typography-counter/EXPERIENCE.md'),'utf8'),/verdict: helped/);
  assert.equal(summary.claudeUsd,0.06);
 }finally{rmSync(runs,{recursive:true,force:true});rmSync(library,{recursive:true,force:true});}
});

test('a base run that never reached the model stops the round before any improver is paid',async()=>{
 const runs=mkdtempSync(join(tmpdir(),'refine-runs-')),library=mkdtempSync(join(tmpdir(),'refine-lib-'));
 let asked=0;
 const rollout=async(task,harness,out)=>{mkdirSync(out,{recursive:true});return {delivered:false,turns:[{turn:1,delivered:false,error:'Out of braincells — buy more from the braincell meter in Aesel.'}]};};
 try{
  await assert.rejects(refineTask({id:'t',domain:'d',requests:['x']},{runs,library,rollout,claude:async()=>{asked++;return {text:'',usd:1};},log:()=>{}}),e=>e.infrastructure&&/braincells/.test(e.message));
  assert.equal(asked,0);
 }finally{rmSync(runs,{recursive:true,force:true});rmSync(library,{recursive:true,force:true});}
});
