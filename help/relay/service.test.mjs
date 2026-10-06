import test from 'node:test';
import assert from 'node:assert/strict';
import {EventEmitter, once} from 'node:events';
import {mkdtempSync,readFileSync,rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {randomUUID} from 'node:crypto';
import {createRelay,ownerAuth} from './service.mjs';
import {RemoteServer} from '../../aesel/src/remote-server.mjs';
class Engine extends EventEmitter {
  async connect(){this.threadId=randomUUID();return {thread:{id:this.threadId}};}
  async startTurn(text){this.starts=(this.starts||0)+1;this.text=text;this.emit('notification',{method:'turn/started',params:{turn:{id:'turn-1'}}});this.emit('request',{id:'approval-1',method:'item/commandExecution/requestApproval',params:{command:'edit piece'}});}
  respond(id,result){this.decision=result.decision;this.emit('notification',{method:'item/agentMessage/delta',params:{delta:'Saved.'}});this.emit('notification',{method:'turn/completed',params:{turn:{id:'turn-1',status:'completed'}}});}
  close(){this.closed=true;}
}
async function setup(t){
 const root=mkdtempSync(join(tmpdir(),'aesel-relay-'));let engine;
 const authorize=async header=>{if(header!=='Bearer owner')throw Object.assign(Error('Private'),{status:403});};
 const start=async()=>{const server=createRelay({root,authorize,factory:()=>engine=new Engine()});server.listen(0,'127.0.0.1');await once(server,'listening');return server;};
 let server=await start();
 const url=()=>`http://127.0.0.1:${server.address().port}`;
 const call=async(path,data,token='owner')=>{const r=await fetch(url()+path,{method:data?'POST':'GET',headers:{authorization:`Bearer ${token}`,'content-type':'application/json'},...(data?{body:JSON.stringify(data)}:{})});return {status:r.status,...await r.json()};};
 t.after(async()=>{server.closeAllConnections();await new Promise(r=>server.close(r));rmSync(root,{recursive:true,force:true});});
 return {root,url,call,engine:()=>engine,restart:async()=>{server.closeAllConnections();await new Promise(r=>server.close(r));server=await start();}};
}
const base='/api/aesel/sessions';
test('only the verified owner can enter, with no token cached in plaintext',async()=>{
 let calls=0;const auth=ownerAuth({adminSub:'owner',fetch:async()=>{calls++;return {ok:true,json:async()=>({sub:'owner',email_verified:true})};}});
 await assert.rejects(auth(),{status:401});await auth('Bearer ok');await auth('Bearer ok');assert.equal(calls,1);
 const denied=ownerAuth({adminSub:'owner',fetch:async()=>({ok:true,json:async()=>({sub:'someone-else',email_verified:true})})});await assert.rejects(denied('Bearer bad'),{status:403});
});
test('saved drawing, idempotent submission, approvals and restart recovery',async t=>{
 const f=await setup(t);assert.equal((await f.call(base,{provider:'claude'},'other')).status,403);
 const {thread}=await f.call(base,{provider:'claude'});const requestId=randomUUID();
 const input={requestId,text:'Draw this',images:[{mimeType:'image/png',data:'aGVsbG8='}]};
 assert.equal((await f.call(`${base}/${thread.id}/turn`,input)).status,'running');
 await new Promise(r=>setTimeout(r,10));
 await f.call(`${base}/${thread.id}/turn`,input);assert.equal(f.engine().starts,1);
 assert.deepEqual(JSON.parse(readFileSync(join(f.root,thread.id,requestId+'.json'))),input);
 const events=await f.call(`${base}/${thread.id}`);assert.equal(events.pending.length,1);
 await f.call(`${base}/${thread.id}/respond`,{id:'approval-1',result:{decision:'accept'}});
 assert.equal(f.engine().decision,'accept');assert.equal((await f.call(`${base}/${thread.id}`)).busy,false);
 await f.restart();const restored=await f.call(`${base}/${thread.id}`);assert.equal(restored.requests[requestId].status,'completed');assert.ok(restored.events.some(e=>e.value.method==='turn/completed'));
});
test('an interrupted service retains input and never repeats the provider turn',async t=>{
 const f=await setup(t);const {thread}=await f.call(base,{provider:'claude'});const input={requestId:randomUUID(),text:'Keep me'};
 await f.call(`${base}/${thread.id}/turn`,input);await new Promise(r=>setTimeout(r,10));await f.restart();
 const restored=await f.call(`${base}/${thread.id}`);assert.equal(restored.busy,false);assert.equal(restored.requests[input.requestId].status,'interrupted');
 assert.equal((await f.call(`${base}/${thread.id}/turn`,input)).status,'interrupted');
});
test('remote Aesel adapter carries streaming and approvals across HTTP',async t=>{
 const f=await setup(t);const remote=new RemoteServer({url:f.url(),token:async()=> 'owner',pollMs:5});t.after(()=>remote.close());
 const result=await remote.connect();assert.ok(result.thread.id);
 remote.on('request',request=>remote.respond(request.id,{decision:'accept'}));
 const done=new Promise(resolve=>remote.on('notification',v=>{if(v.method==='turn/completed')resolve(v.params.turn.status);}));
 await remote.startTurn('Make something');assert.equal(await done,'completed');assert.equal(f.engine().decision,'accept');
});
test('lost acknowledgement retries the same request without starting twice',async t=>{
 const f=await setup(t);let lost=false;
 const remote=new RemoteServer({url:f.url(),token:async()=> 'owner',pollMs:5,fetch:async(url,options)=>{
   const response=await fetch(url,options);
   if(url.endsWith('/turn')&&!lost){lost=true;await response.text();throw Error('Connection lost after acceptance');}
   return response;
 }});t.after(()=>remote.close());await remote.connect();
 const result=await remote.startTurn('Exactly once');assert.equal(result.turn.status,'inProgress');assert.equal(f.engine().starts,1);
 const state=await f.call(`${base}/${remote.threadId}`);assert.equal(state.events[0].value.params.turn.id,result.turn.id);
});
