import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,writeFile,readFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {AcServer} from '../src/ac-server.mjs';
const end=stop=>({type:'message_delta',delta:{stop_reason:stop}});
const write=(id,source)=>[
 {type:'content_block_start',index:0,content_block:{type:'tool_use',id,name:'write_piece'}},
 {type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source})}},
 {type:'content_block_stop',index:0}
];
function stream(events){return new Response(events.map(e=>'data: '+JSON.stringify(e)+'\n\n').join(''),{headers:{'Content-Type':'text/event-stream'}});}
test('output continuation retains complete tools, discards unfinished JSON, and pairs results',async t=>{
 const dir=await mkdtemp(join(tmpdir(),'ww-resume-'));t.after(()=>rm(dir,{recursive:true,force:true}));
 const file=join(dir,'piece.mjs');await writeFile(file,'// base');
 const requests=[],statuses=[];
 const engine=new AcServer({piece:{file},jev:null,token:()=> 'fixture',rounds:1,outputContinuations:2,fetch:async(_,init)=>{
  requests.push(JSON.parse(init.body));
  if(requests.length===1)return stream([...write('complete','// checkpoint'),{type:'content_block_start',index:1,content_block:{type:'tool_use',id:'unfinished',name:'write_piece'}},{type:'content_block_delta',index:1,delta:{type:'input_json_delta',partial_json:'{"source":"BROKEN'}},end('max_tokens')]);
  assert.equal((await readFile(file,'utf8')).trim(),'// checkpoint');return stream([end('end_turn')]);
 }});
 engine.on('notification',n=>{if(n.method==='turn/completed')statuses.push(n.params.turn);});
 await engine.startTurn('edit');
 assert.equal(requests.length,2);assert.equal(statuses.at(-1).status,'completed');
 const sent=JSON.stringify(requests[1].messages);assert.match(sent,/tool_result/);assert.match(sent,/complete/);assert.doesNotMatch(sent,/BROKEN|"id":"unfinished"/);
});
test('repeated caps pause rather than loop indefinitely',async()=>{
 let calls=0;const statuses=[];
 const engine=new AcServer({jev:null,token:()=> 'fixture',outputContinuations:2,fetch:async()=>{calls++;return stream([end('max_tokens')]);}});
 engine.on('notification',n=>{if(n.method==='turn/completed')statuses.push(n.params.turn);});
 await engine.startTurn('edit');assert.equal(calls,3);assert.match(statuses.at(-1).error.message,/checkpoint/);
});
