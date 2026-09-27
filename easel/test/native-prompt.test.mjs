import test from 'node:test';
import assert from 'node:assert/strict';
import {nativeInstructions} from '../native/prompt.mjs';
import {nativePreview,nativeInputPixels} from '../native/preview.mjs';
import {PIECE_INSTRUCTIONS} from '../src/harness-contract.mjs';
import {PIECE_VISUAL,PIECE_RESPONSIVE,PIECE_CLOCK,PIECE_SOUND,PIECE_REPLY} from '../src/piece-prompt.mjs';
import {mcpConfig,codexMcpArgs} from '../src/tools.mjs';

test('native startup preserves shared Electron behavior, full guides and native limitations',()=>{
 const prompt=nativeInstructions('User: make a drum');
 for(const rule of [PIECE_INSTRUCTIONS,PIECE_VISUAL,PIECE_RESPONSIVE,PIECE_CLOCK,PIECE_SOUND,PIECE_REPLY])assert(prompt.includes(rule));
 for(const content of ['# pieces.md','# screen.md','# hand.md','sound.howl','clock.resync()','120 BPM','Unix epoch zero','screen.width','upper-right','ac_frame','ac_preview','rendered source revision','User: make a drum'])assert(prompt.includes(content),content);
 for(const server of Object.values(mcpConfig('/work',{AESEL_NATIVE_SESSION:'my-thread'}).mcpServers))assert.equal(server.env.AESEL_NATIVE_SESSION,'my-thread');
 assert(codexMcpArgs('/work',{AESEL_NATIVE_SESSION:'my-thread'}).some(value=>value.includes('AESEL_NATIVE_SESSION')));
 assert(!JSON.stringify(mcpConfig('/work')).includes('AESEL_NATIVE_SESSION'));
});
const png='iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScLbtAAAAABJRU5ErkJggg==';
function fixture({change=false,sessionID='thread',ready=true}={}) {
 let reads=0;const calls=[];
 return {calls,rpc:async(method,params)=>{
  calls.push([method,params]);
  if(method==='state')return {instance:'instance',session:{id:sessionID},piece:{version:change?reads++:0,sourceBytes:10},preview:{visible:true}};
  if(method==='preview')return {inspection:JSON.stringify({ready,canvases:[{width:89,height:59}]})};
  if(method==='capture')return {mimeType:'image/png',data:png};
 }};
}
test('native input attaches a fresh thread-bound image and distinguishes drawable from snapshot pixels',async()=>{
 const f=fixture();const input=await nativeInputPixels('thread',f);
 assert.equal(input.images[0].data,png);assert.match(input.context,/"width":89/);assert.match(input.context,/"snapshot":\{"width":1,"height":1\}/);
 assert.match(input.context,/"renderedRevisionVerified":false/);
 assert.deepEqual(f.calls.map(c=>c[0]),['state','preview','capture','state']);
 const inspection=await nativePreview('thread',{...fixture(),image:false});assert.deepEqual(inspection.images,[]);
});
test('missing, unready, cross-thread and changed previews never attach another thread or stale pixels',async()=>{
 const wrong=fixture({sessionID:'other'});const input=await nativeInputPixels('thread',wrong);
 assert.deepEqual(input.images,[]);assert.match(input.context,/different thread/);assert.equal(wrong.calls.length,1);
 for(const f of [fixture({change:true}),fixture({ready:false})]){
  const result=await nativeInputPixels('thread',f);assert.deepEqual(result.images,[]);assert.match(result.context,/unavailable/);
 }
 const failed=await nativeInputPixels('thread',{rpc:async()=>{throw Error('offline')}});
 assert.deepEqual(failed.images,[]);assert.match(failed.context,/offline/);
});

test('native tool discovery describes actual capture limits and rejects unsupported revision checks',async()=>{
 const {handle}=await import('../src/tools.mjs');const previous=process.env.AESEL_NATIVE_SESSION;
 try {
  process.env.AESEL_NATIVE_SESSION='thread';
  const result=handle({id:1,method:'tools/list'},{cwd:'/work'});
  const frame=result.result.tools.find(t=>t.name==='ac_frame');
  assert.match(frame.description,/statistics and OCR are not provided/);
  assert.deepEqual(Object.keys(frame.inputSchema.properties),['image']);
  const refused=await handle({id:2,method:'tools/call',params:{name:'ac_preview',arguments:{revision:'pretend'}}},{cwd:'/work'});
  assert.equal(refused.result.isError,true);assert.match(refused.result.content[0].text,/unavailable/);
 }finally{if(previous===undefined)delete process.env.AESEL_NATIVE_SESSION;else process.env.AESEL_NATIVE_SESSION=previous;}
});
