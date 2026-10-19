import test from 'node:test';import assert from 'node:assert/strict';
import {mkdtempSync,mkdirSync,existsSync,readFileSync} from 'node:fs';
import {tmpdir} from 'node:os';import {join} from 'node:path';
import {runPieceTask} from '../src/piece-run.mjs';
import {recorder,renderTrace} from '../src/run-task.mjs';

const png='iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==';
const stream=events=>({ok:true,body:{getReader(){const lines=events.map(e=>`data: ${JSON.stringify(e)}\n`);let i=0;return {read:async()=>i<lines.length?{done:false,value:new TextEncoder().encode(lines[i++])}:{done:true}};}}});
const writes=source=>[{type:'content_block_start',index:0,content_block:{type:'tool_use',id:'t1',name:'write_piece'}},{type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source,note:'draft'})}},{type:'content_block_stop',index:0},{type:'message_delta',delta:{stop_reason:'tool_use'}}];
const say=text=>[{type:'content_block_delta',index:0,delta:{type:'text_delta',text}},{type:'message_delta',delta:{stop_reason:'end_turn'}}];
const serving=turns=>{let call=0;return async(_url,options)=>{serving.sent.push(JSON.parse(options.body));return stream(turns[Math.min(call++,turns.length-1)]);};};
serving.sent=[];
// `ac check` stand-in: a piece that names `throw` fails, anything else runs.
const play=async file=>({problems:/throw/.test(readFileSync(file,'utf8'))?['error: boom']:[],frame:png});

test('each request is a turn; every save is checked and the check reaches ac_preview',async()=>{
 const out=mkdtempSync(join(tmpdir(),'piece-run-'));mkdirSync(join(out,'frames'));
 const record=recorder(out);
 const fetch=serving([writes('function paint({ wipe }) { wipe("navy"); }\nexport { paint };\n'),say('Navy.'),
  writes('function paint({ wipe }) { throw Error("x"); }\nexport { paint };\n'),say('Done.')]);
 serving.sent.length=0;
 const outcome=await runPieceTask({task:{id:'t',kind:'piece',requests:['navy','break it']},auth:{token:async()=>'tok'},onEvent:e=>record.write(e),fetch,play});
 assert.equal(outcome.turns.length,2);
 assert.deepEqual(outcome.turns.map(t=>t.delivered),[true,false]);
 assert.equal(outcome.turns[1].error,'the final check found errors');assert.deepEqual(outcome.turns[1].notes,['error: boom']);
 assert.equal(outcome.delivered,false);
 // The save was checked before its tool result went back, so the next round carries what the check saw.
 assert.match(JSON.stringify(serving.sent[1].messages),/ac check: ran with a tap and a space; no errors/);
 assert.match(JSON.stringify(serving.sent[3].messages),/error: boom/);
 const checks=record.events.filter(e=>e.method==='piece/check');
 assert.equal(checks.length,4,'two saves and two final checks');
 assert.ok(existsSync(join(out,checks[0].params.frames[0].file)),'frames are written as files');
 const trace=renderTrace({id:'t',kind:'piece'},record.events,{delivered:false,turns:outcome.turns.map(({source,...t})=>t)});
 for(const want of ['## Turn 1','> navy','## Turn 2','**observation:** ac check — no errors','**observation:** ac check — 1 problem','error: boom','turn 2: not delivered — the final check found errors'])
  assert.ok(trace.includes(want),want);
});

test('a turn that writes nothing is not delivered and ends the task',async()=>{
 const outcome=await runPieceTask({task:{id:'t',requests:['hello','again']},auth:{token:async()=>'tok'},fetch:serving([say('Hi.')]),play});
 assert.equal(outcome.turns.length,1);assert.equal(outcome.turns[0].error,'the piece was not written');assert.equal(outcome.delivered,false);
});

test('a run says which harness sections its prompt carried, matching around placeholders',async()=>{
 const {sentSections}=await import('../src/run-task.mjs');
 const sections={'piece-visual':'Let the piece stand on its own, without captions.','interface-model':'Your interface is Aesel. The model identifier is {{model}}. Report that identifier exactly.','pro-reply':'Write plainly, the way Tao Lin writes it.','only-slot':'{{source}}'};
 const system='…Let the piece stand on its own, without captions.\nYour interface is Aesel. The model identifier is x/y. Report that identifier exactly.…';
 assert.deepEqual(sentSections(system,sections),['piece-visual','interface-model']);
 const events=[];serving.sent.length=0;
 await runPieceTask({task:{id:'t',requests:['navy']},auth:{token:async()=>'tok'},onEvent:e=>events.push(e),fetch:serving([writes('export function paint({wipe}){wipe("navy")}\n'),say('Navy.')]),play});
 const prompts=events.filter(e=>e.method==='piece/prompt');
 assert.equal(prompts.length,1,'recorded once');assert.equal(prompts[0].params.system,serving.sent[0].system.map(b=>b.text).join('\n\n'));
});
