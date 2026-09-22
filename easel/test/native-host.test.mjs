import test from 'node:test';
import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import {mkdtempSync,rmSync,writeFileSync,readFileSync,mkdirSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {NativeHost,serveHost} from '../native/host.mjs';
import {NativeProvider} from '../phone/native-provider.mjs';

const source='export function paint({wipe}) { wipe("red"); }';
const tick=()=>new Promise(resolve=>setTimeout(resolve,25));
async function until(predicate) {for(let i=0;i<100;i++){if(predicate())return;await tick();}throw Error('Timed out');}
class Engine extends EventEmitter {
  constructor(){super();this.turns=[];this.answers=[];this.threadId='thread';}
  async connect(){if(this.wait)await this.wait;}
  async startTurn(text,options){this.turns.push(text);this.inputOptions=options;}
  respond(id,result){this.answers.push({id,result});}
  async interrupt(){this.emit('notification',{method:'turn/completed',params:{turn:{status:'interrupted'}}});}
  complete(){this.emit('notification',{method:'turn/completed',params:{turn:{status:'completed'}}});}
  close(){this.emit('exit');}
}
function fixture(t){
 const root=mkdtempSync(join(tmpdir(),'aesel-host-')),engine=new Engine();
 const host=new NativeHost({root,engineFactory:(provider,options)=>{engine.options=options;return engine;},discover:id=>id==='claude'?'/fake/claude':null,capturePreview:async()=>({context:'',images:[]})});
 t.after(()=>{host.close();rmSync(root,{recursive:true,force:true});});
 return {root,engine,host,configure:()=>host.rpc('configure',{sessionID:'test',provider:'claude',model:'sonnet',source})};
}
test('one durable operation starts once and carries source plus scoped approvals',async t=>{
 const {root,engine,host,configure}=fixture(t);await configure();
 const p={sessionID:'test',operationID:'once',text:'draw'};
 await host.rpc('turn',p);await host.rpc('turn',p);await until(()=>engine.turns.length===1);
 engine.emit('request',{id:7,method:'item/commandExecution/requestApproval',params:{command:'example'}});
 await assert.rejects(host.rpc('approval',{sessionID:'test',operationID:'wrong',id:'7',decision:'accept'}),/expired/);
 await host.rpc('approval',{sessionID:'test',operationID:'once',id:'7',decision:'accept'});
 assert.deepEqual(engine.answers,[{id:7,result:{decision:'accept'}}]);
 await assert.rejects(configure(),/Wait/);
 writeFileSync(join(root,'workspaces/test/piece.mjs'),'export const paint = () => {};');engine.complete();
 await until(()=>host.session('test').active===null);
 const events=await host.rpc('events',{sessionID:'test',operationID:'once'});
 assert.equal(events.operation.status,'completed');assert.ok(events.events.some(x=>x.type==='source'));
 await host.rpc('turn',p);assert.equal(engine.turns.length,1);
});
test('stop during connection cannot launch the pending prompt',async t=>{
 const {host,engine,configure}=fixture(t);await configure();
 let release;engine.wait=new Promise(resolve=>release=resolve);
 await host.rpc('turn',{sessionID:'test',operationID:'stop',text:'draw'});
 await host.rpc('interrupt',{sessionID:'test',operationID:'stop'});release();
 await until(()=>host.session('test').active===null);assert.equal(engine.turns.length,0);
 assert.equal(host.session('test').operations.stop.status,'interrupted');
});
test('restart marks an interrupted operation without replay; corrupt ledger fails closed',async t=>{
 const {host,root,configure}=fixture(t);await configure();
 await host.rpc('turn',{sessionID:'test',operationID:'restart',text:'draw'});host.close();
 const next=new NativeHost({root});
 assert.equal((await next.rpc('events',{sessionID:'test',operationID:'restart'})).operation.status,'interrupted');
 mkdirSync(join(root,'workspaces/corrupt'));writeFileSync(join(root,'workspaces/corrupt/host.json'),'{broken');
 assert.throws(()=>next.session('corrupt'),/checkpoint/);next.close();
});
test('bad model and source rejected before changing saved source',async t=>{
 const {host,root,configure}=fixture(t);await configure();
 await assert.rejects(host.rpc('configure',{sessionID:'test',provider:'claude',model:'injected',source}),/Choose a model/);
 await assert.rejects(host.rpc('configure',{sessionID:'test',provider:'claude',model:'sonnet',source:'export function {'}));
 assert.equal(readFileSync(join(root,'workspaces/test/piece.mjs'),'utf8'),source);
});
test('loopback requires bearer and rejects browser-origin requests',async t=>{
 const {host}=fixture(t),token='a'.repeat(64),server=await serveHost({host,token});t.after(()=>server.close());
 const url=`http://127.0.0.1:${server.address().port}/rpc`,body=JSON.stringify({method:'capabilities'});
 assert.equal((await fetch(url,{method:'POST',body})).status,401);
 assert.equal((await fetch(url,{method:'POST',body,headers:{Authorization:`Bearer ${token}`,Origin:'https://example.com'}})).status,401);
 const response=await fetch(url,{method:'POST',body,headers:{Authorization:`Bearer ${token}`}});
 assert.equal(response.status,200);assert.equal((await response.json()).result.providers[0].available,true);
});
test('lost turn response retains ID; reconnect observes completion without another submission',async()=>{
 let submits=0,persisted;const operation={id:'existing',after:3};
 const provider=new NativeProvider({sessionID:'test',provider:'claude',model:'sonnet',source:()=>source,onSource:()=>{},onOperation:value=>persisted=value,
 rpc:async method=>{if(method==='configure')return {sequence:0};if(method==='turn'){submits++;throw Error('lost');}if(method==='events')return {oldest:1,events:[],operation:{status:'completed'}};}});
 await assert.rejects(provider.startTurn('draw'),/Reconnect/);assert.ok(persisted?.id);await provider.follow();assert.equal(submits,1);assert.equal(persisted,null);
 const restored=new NativeProvider({operation});restored.operation.after=8;assert.equal(operation.after,3);
});

test('native turns deliver preview images and cancellation during capture never sends a prompt',async t=>{
 const {host,engine,configure}=fixture(t);await configure();
 const image={type:'image',mimeType:'image/png',data:'fixture'};
 host.capturePreview=async id=>{assert.equal(id,'test');return {context:' current preview',images:[image]};};
 await host.rpc('turn',{sessionID:'test',operationID:'pixels',text:'draw'});
 await until(()=>engine.turns.length===1);
 assert.match(engine.options.developerInstructions,/Responsive composition/);assert.match(engine.options.developerInstructions,/clock.resync/);assert.equal(engine.options.environment.AESEL_NATIVE_SESSION,'test');
 assert.equal(engine.turns[0],'draw current preview');assert.deepEqual(engine.inputOptions.images,[image]);
 engine.complete();await until(()=>host.session('test').active===null);
 let release;host.capturePreview=()=>new Promise(resolve=>release=resolve);
 await host.rpc('turn',{sessionID:'test',operationID:'cancel-capture',text:'never send'});
 await until(()=>Boolean(release));
 await host.rpc('interrupt',{sessionID:'test',operationID:'cancel-capture'});
 release({context:'',images:[]});await until(()=>host.session('test').active===null);
 assert.equal(engine.turns.length,1);
});
