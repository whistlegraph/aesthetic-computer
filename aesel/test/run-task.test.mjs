import test from 'node:test';import assert from 'node:assert/strict';
import {mkdtempSync,readFileSync,existsSync,lstatSync,readdirSync,writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';import {join} from 'node:path';
import {execFileSync} from 'node:child_process';
import {recorder,renderTrace,usageByTurn,copySurface,SURFACES} from '../src/run-task.mjs';
const png='iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==';
const events=[
 {turn:1,method:'whistlegraph/turn',params:{request:'a pink ball'}},
 {turn:1,method:'turn/started',params:{}},
 {turn:1,method:'item/agentMessage/delta',params:{delta:"I'm rolling out a pink ball."}},
 {turn:1,method:'item/modelCode/delta',params:{tool:'write_piece',delta:'{"source":"export function paint(){}"}'}},
 {turn:1,method:'turn/usage',params:{usage:{input_tokens:100,output_tokens:40}}},
 {turn:1,method:'whistlegraph/paint',params:{renderID:1,rendered:true,logs:[{level:'error',text:'TypeError: x is undefined'}],frames:[{atMs:0,width:1,height:1,png}]}},
 {turn:1,method:'whistlegraph/step',params:{text:'repairing the picture'}},
 {turn:1,method:'turn/started',params:{}},
 {turn:1,method:'turn/usage',params:{usage:{input_tokens:50,output_tokens:10}}},
 {turn:1,method:'whistlegraph/review',params:{passed:false,observations:'The ball is blue.',findings:['Make the ball pink.']}},
];
test('a run records frames as files and keeps their names in events.jsonl',()=>{
 const out=mkdtempSync(join(tmpdir(),'run-'));execFileSync('mkdir',[join(out,'frames')]);
 const record=recorder(out);for(const e of events)record.write(e);
 const lines=readFileSync(join(out,'events.jsonl'),'utf8').trim().split('\n').map(JSON.parse);
 assert.equal(lines.length,events.length);
 const paint=lines.find(l=>l.method==='whistlegraph/paint');
 assert.equal(paint.params.frames[0].file,'frames/turn-1-render-1-1.png');assert.equal(paint.params.frames[0].png,undefined);
 assert.ok(existsSync(join(out,paint.params.frames[0].file)));
});
test('the trace reads as request, rounds, actions and observations',()=>{
 const out=mkdtempSync(join(tmpdir(),'run-'));execFileSync('mkdir',[join(out,'frames')]);
 const record=recorder(out);for(const e of events)record.write(e);
 const usage=usageByTurn(record.events);assert.deepEqual(usage,{1:{rounds:2,input:150,output:50}});
 const trace=renderTrace({id:'pink-ball',kind:'whistlegraph'},record.events,{delivered:true,turns:[{turn:1,delivered:true,acceptance:'reviewed-with-notes',repairs:1,notes:['Make the ball pink.'],usage:usage[1]}]});
 for(const want of ['## Turn 1','> a pink ball','### Round 1',"**model:** I'm rolling out a pink ball.",'**action:** `write_piece`','**observation:** painted · 1 runtime error','TypeError','![0 ms](frames/turn-1-render-1-1.png)','_repairing the picture_','### Round 2','**review:** findings — The ball is blue.','- Make the ball pink.','turn 1: reviewed-with-notes, repaired · 2 rounds, 50 tokens out'])
  assert.ok(trace.includes(want),want);
});
test('a copied surface holds files, never links back to its source',()=>{
 const to=join(mkdtempSync(join(tmpdir(),'surface-')),'aesel');copySurface(SURFACES.aesel.source,to);
 assert.ok(readdirSync(to).length>=3);assert.ok(readdirSync(to).every(f=>lstatSync(join(to,f)).isFile()));
});
test('an edited harness reaches the prompts, and its report names the edit',()=>{
 const dir=mkdtempSync(join(tmpdir(),'harness-'));copySurface(SURFACES.aesel.source,join(dir,'aesel'));
 const file=join(dir,'aesel/systemprompt.md');
 writeFileSync(file,readFileSync(file,'utf8').replace(/## no-name-on-screen\n\n[^\n]+/,'## no-name-on-screen\n\nEDITED.')+'\n## brand-new\n\nUnplaced.\n');
 const script=`const {applyHarness}=await import(${JSON.stringify(new URL('../src/run-task.mjs',import.meta.url).href)});
const report=await applyHarness(${JSON.stringify(dir)});const {harnessText}=await import(${JSON.stringify(new URL('../src/harness-text.mjs',import.meta.url).href)});
console.log(JSON.stringify({report:report.aesel,text:harnessText('no-name-on-screen')}));`;
 const {report,text}=JSON.parse(execFileSync(process.execPath,['--input-type=module','-e',script],{encoding:'utf8'}));
 assert.equal(text,'EDITED.');assert.deepEqual(report.changed,['no-name-on-screen']);assert.deepEqual(report.added,['brand-new']);assert.deepEqual(report.inherited,[]);
});
test('the funding preflight asks lith, and a silent lith is not a verdict',async()=>{
 const {openRouterFunding}=await import('../src/run-task.mjs');
 const seen=[];const fetch=async(url,o)=>{seen.push([url,o.headers.Authorization]);return {ok:true,json:async()=>({funded:false,floor_usd:5,balance_usd:2})};};
 assert.deepEqual(await openRouterFunding('https://site','t',fetch),{funded:false,floor_usd:5,balance_usd:2});
 assert.deepEqual(seen,[['https://site/api/openrouter-status','Bearer t']]);
 assert.equal(await openRouterFunding('https://site','t',async()=>({ok:false,status:404})),null);
 assert.equal(await openRouterFunding('https://site','t',async()=>{throw Error('offline');}),null);
});
