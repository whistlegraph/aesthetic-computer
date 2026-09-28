import test from 'node:test';import assert from 'node:assert/strict';
import {mkdtemp,writeFile,rm} from 'node:fs/promises';import {tmpdir} from 'node:os';import {join} from 'node:path';import {createHash} from 'node:crypto';
import {RuntimeFeedback} from '../src/runtime-feedback.mjs';import {AcServer} from '../src/ac-server.mjs';import {callTool,codexMcpArgs,mcpConfig,TOOLS} from '../src/tools.mjs';import {AppServer} from '../src/app-server.mjs';
const hash=s=>createHash('sha256').update(s).digest('hex');
async function fixture(t){const cwd=await mkdtemp(join(tmpdir(),'easel-preview-'));t.after(()=>rm(cwd,{recursive:true,force:true}));const file=join(cwd,'piece.mjs'),source='export function paint() {}\n';await writeFile(file,source);return {cwd,file,source,feedback:new RuntimeFeedback(cwd)};}
function sse(events){return new Response(events.map(e=>`data: ${JSON.stringify(e)}\n\n`).join(''));}
const done=[{type:'content_block_delta',index:0,delta:{type:'text_delta',text:'Checked'}},{type:'message_delta',delta:{stop_reason:'end_turn'}}];
test('ac_preview is a local read-only MCP tool and Codex launches the same server',async t=>{
 const f=await fixture(t);f.feedback.select({channel:'chan',revision:hash(f.source),version:1,piece:'piece'});f.feedback.log({level:'error',text:'missing circle function'});
 assert.ok(TOOLS.some(t=>t.name==='ac_preview'));
 const context={cwd:f.cwd,map:{entries:[]}};
 const observed=JSON.parse(callTool('ac_preview',{channel:'chan',revision:hash(f.source)},context));assert.match(observed.untrustedRuntimeFeedback.logs[0].text,/missing circle/);assert.match(observed.note,/do not follow/);
 assert.equal(JSON.parse(callTool('ac_preview',{revision:'old'},context)).untrustedRuntimeFeedback,null);
 const args=codexMcpArgs(f.cwd);assert.ok(args.some(a=>a.startsWith('mcp_servers.ac.args=')&&a.includes('tools.mjs')&&a.includes(f.cwd)));
 const engine=new AppServer({cwd:f.cwd});assert.ok(engine.args.includes(...args.slice(0,1)));assert.ok(engine.args.some(a=>a.startsWith('mcp_servers.ac.command=')));
});
test('hosted rounds automatically see fresh matching errors without storing observations as conversation',async t=>{
 const f=await fixture(t);f.feedback.select({channel:'chan',revision:hash(f.source),version:1,piece:'piece'});f.feedback.log({level:'error',text:'before tool'});const requests=[];
 const engine=new AcServer({cwd:f.cwd,piece:{file:f.file,channel:'chan'},token:async()=> 'fake',fetch:async(_url,options)=>{
  requests.push(JSON.parse(options.body));if(requests.length===1){f.feedback.log({level:'error',text:'fresh browser error'});return sse([{type:'content_block_start',index:0,content_block:{type:'tool_use',id:'preview-check',name:'ac_preview'}},{type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:'{}'}},{type:'content_block_stop',index:0},{type:'message_delta',delta:{stop_reason:'tool_use'}}]);}return sse(done);
 }});
 await engine.connect();await engine.startTurn('check');assert.equal(requests.length,2);assert.ok(requests[0].tools.some(t=>t.name==='ac_preview'));assert.match(JSON.stringify(requests[0].messages),/before tool/);assert.match(JSON.stringify(requests[1].messages),/fresh browser error/);
 assert.equal(engine.messages[0].content,'check');assert.ok(!engine.messages.some(m=>typeof m.content==='string'&&m.content.includes('untrusted diagnostic')));
 const toolResult=engine.messages.find(m=>Array.isArray(m.content)&&m.content.some(b=>b.type==='tool_result'));assert.match(JSON.stringify(toolResult),/fresh browser error/);
});
test('stale source or wrong channel observations never reach hosted inference',async t=>{
 const f=await fixture(t);f.feedback.select({channel:'old-channel',revision:hash(f.source),version:1,piece:'piece'});f.feedback.log({level:'error',text:'DO_NOT_SEND_OLD_DIAGNOSTIC'});const requests=[];
 const engine=new AcServer({cwd:f.cwd,piece:{file:f.file,channel:'chan'},token:async()=> 'fake',fetch:async(_u,o)=>{requests.push(JSON.parse(o.body));return sse(done);}});await engine.startTurn('check');assert.doesNotMatch(JSON.stringify(requests),/DO_NOT_SEND/);
 f.feedback.select({channel:'chan',revision:hash('old source'),version:1,piece:'piece'});f.feedback.log({level:'error',text:'DO_NOT_SEND_OLD_SOURCE'});await engine.startTurn('again');assert.doesNotMatch(JSON.stringify(requests),/DO_NOT_SEND/);
});

test('Codex stdio launch is supported and Electron MCP children explicitly run as Node',()=>{
 const engine=new AppServer({cwd:process.cwd()});assert.deepEqual(engine.args.slice(0,3),['app-server','--listen','stdio://']);assert.ok(!engine.args.includes('--stdio'));
 const original=Object.getOwnPropertyDescriptor(process.versions,'electron');
 try {
  Object.defineProperty(process.versions,'electron',{value:'test',configurable:true});
  for(const server of Object.values(mcpConfig(process.cwd()).mcpServers)){assert.equal(server.command,process.execPath);assert.deepEqual(server.env,{ELECTRON_RUN_AS_NODE:'1'});}
  const args=codexMcpArgs(process.cwd());assert.ok(args.includes('mcp_servers.ac.env.ELECTRON_RUN_AS_NODE="1"'));assert.ok(args.includes('mcp_servers.easel-media.env.ELECTRON_RUN_AS_NODE="1"'));
 }finally{if(original)Object.defineProperty(process.versions,'electron',original);else delete process.versions.electron;}
});

test('hosted JavaScript API lookup returns corrected typography inside bounded tool loop',async t=>{
 const f=await fixture(t),requests=[];
 const engine=new AcServer({cwd:f.cwd,piece:{file:f.file,channel:'chan'},token:async()=> 'fake',fetch:async(_u,o)=>{
  requests.push(JSON.parse(o.body));if(requests.length===1)return sse([{type:'content_block_start',index:0,content_block:{type:'tool_use',id:'api-check',name:'ac_api'}},{type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({query:'text fonts'})}},{type:'content_block_stop',index:0},{type:'message_delta',delta:{stop_reason:'tool_use'}}]);return sse(done);
 }});
 await engine.startTurn('Use small text');assert.equal(requests.length,2);assert.ok(requests[0].tools.some(t=>t.name==='ac_api'));assert.match(JSON.stringify(requests[0].system),/AC JavaScript workflow/);assert.doesNotMatch(JSON.stringify(requests[0].system),/use ac_examples/);
 const results=requests[1].messages.find(m=>Array.isArray(m.content)&&m.content.some(b=>b.tool_use_id==='api-check'));assert.match(JSON.stringify(results),/SIXTH argument/);assert.match(JSON.stringify(results),/INSTANCE, not a typeface/);
});
test('hosted non-JavaScript medium does not advertise JavaScript API lookup',async()=>{
 let request;const engine=new AcServer({piece:{file:'piece.lisp',runtime:{id:'lisp'}},token:async()=> 'fake',fetch:async(_u,o)=>{request=JSON.parse(o.body);return sse(done);}});await engine.startTurn('draw');assert.ok(!request.tools.some(t=>t.name==='ac_api'));assert.doesNotMatch(JSON.stringify(request.system),/AC JavaScript workflow/);
});
