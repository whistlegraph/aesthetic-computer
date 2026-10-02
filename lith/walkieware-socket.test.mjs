import test from 'node:test';
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {once} from 'node:events';
import {WebSocket} from 'ws';
import {attachWalkiewareSocket} from './walkieware-socket.mjs';
import {ReceiptJournal,AttemptReceipt,hashSource} from '../aesel/src/attempt-receipt.mjs';
import {attachMusicalSocket} from './musical-socket.mjs';
import {mongoWalkiewareStore,validateLedger,sourceHash} from '../system/backend/walkieware.mjs';
import {WalkiewareThread,threadIdentity,verifyThreadRevision} from '../aesel/src/walkieware-thread.mjs';
const id='11111111-1111-4111-8111-111111111111';
const ledger={format:1,head:0,versions:[{id:0,parent:null,source:'export function paint({wipe}){wipe(0);}',request:null,createdAt:'today',layers:0}]};
test('remote edits reject stale versions, mismatched source and a local ask starting during hashing',async()=>{
 const source=ledger.versions[0].source,state={busy:false,head:0,source};
 const command={baseVersion:0,baseHash:sourceHash(source)};
 await verifyThreadRevision(command,()=>state);
 await assert.rejects(verifyThreadRevision({...command,baseVersion:1},()=>state),/Version changed/);
 await assert.rejects(verifyThreadRevision({...command,baseHash:'wrong'},()=>state),/Version changed/);
 let n=0;await assert.rejects(verifyThreadRevision(command,()=>({...state,busy:++n>1})),/Version changed/);
});
function memoryCollection(){
 const docs=new Map();
 const find=query=>[...docs.values()].find(row=>Object.entries(query).every(([k,v])=>k==='receipts.id'?!row.receipts?.some(r=>r.id===v.$ne):row[k]===v));
 return {createIndex:async()=>{},findOne:async q=>structuredClone(find(q)||null),insertOne:async row=>{if(docs.has(row._id)||find({codeKey:row.codeKey}))throw Object.assign(Error('duplicate'),{code:11000});docs.set(row._id,structuredClone(row));},updateOne:async(q,u)=>{const row=find(q);if(!row)return {modifiedCount:0};Object.assign(row,structuredClone(u.$set));for(const [k,v]of Object.entries(u.$inc||{}))row[k]+=v;for(const [k,v]of Object.entries(u.$push||{}))row[k]=[...(row[k]||[]),...structuredClone(v.$each)].slice(v.$slice);for(const k of Object.keys(u.$unset||{}))delete row[k];return {modifiedCount:1};}};
}
function inbox(ws){const queue=[],waiters=[];ws.on('message',raw=>{const m=JSON.parse(raw);const i=waiters.findIndex(w=>w.type===m.type);if(i<0)queue.push(m);else waiters.splice(i,1)[0].resolve(m);});return type=>{const i=queue.findIndex(m=>m.type===type);return i>=0?Promise.resolve(queue.splice(i,1)[0]):new Promise(resolve=>waiters.push({type,resolve}));};}
async function client(url,auth){const ws=new WebSocket(url),next=inbox(ws);await once(ws,'open');ws.send(JSON.stringify({type:'authenticate',token:'owner',...auth}));return {ws,next,send:m=>ws.send(JSON.stringify(m))};}
async function fixture(t){const server=createServer();const store=mongoWalkiewareStore(memoryCollection(),{name:()=> 'wwRuboh'});const auth=async h=>h.authorization==='Bearer owner'?'owner':h.authorization==='Bearer stranger'?'stranger':null;
 const musical=attachMusicalSocket(server,{authenticate:auth,decide:async()=>{}});
 const binding=attachWalkiewareSocket(server,{authenticate:auth,store:async()=>store});server.listen(0,'127.0.0.1');await once(server,'listening');t.after(()=>{binding.close();musical.close();server.close();});return {url:`ws://127.0.0.1:${server.address().port}/api/walkieware-stream`,store};}
test('names are reserved, versions immutable, undo retains history, stale writes rejected',async()=>{
 let n=0;const store=mongoWalkiewareStore(memoryCollection(),{name:()=>++n<3?'wwRuboh':'wwLemop'});
 assert.equal((await store.open('owner',id)).code,'wwRuboh');
 assert.equal((await store.open('owner','22222222-2222-4222-8222-222222222222')).code,'wwLemop');
 await assert.rejects(store.open('other',id));assert.equal(await store.read('other','wwRuboh'),null);
 assert.ok(await store.save('owner',id,0,ledger));assert.equal(await store.save('owner',id,0,ledger),null);
 const changed=structuredClone(ledger);changed.versions[0].source='tampered';await assert.rejects(store.save('owner',id,1,changed),/immutable/);
 const next=structuredClone(ledger);next.versions.push({id:1,parent:0,source:'new',request:'edit',createdAt:'today',layers:1});next.head=1;
 assert.ok(await store.save('owner',id,1,next));next.head=0;assert.equal((await store.save('owner',id,2,next)).ledger.versions.length,2);
 assert.throws(()=>validateLedger({...ledger,head:99}));
});
test('real sockets isolate accounts, persist versions and relay checked commands to the running device',{timeout:5000},async t=>{
 const f=await fixture(t),device=await client(f.url,{role:'device',id});assert.equal((await device.next('ready')).thread.code,'wwRuboh');
 device.send({type:'sync',revision:0,ledger});await device.next('saved');
 const intruder=await client(f.url,{role:'agent',code:'wwRuboh',token:'stranger'});assert.match((await intruder.next('error')).error,/unavailable/);
 const agent=await client(f.url,{role:'agent',code:'wwRuboh'});assert.equal((await agent.next('ready')).thread.ledger.head,0);
 agent.send({type:'command',id:'edit-1',action:'ask',text:'Make it 3D',baseVersion:0,baseHash:sourceHash(ledger.versions[0].source)});
 const command=await device.next('command');assert.equal(command.text,'Make it 3D');await agent.next('accepted');
 agent.send({type:'command',id:'edit-2',action:'undo',baseVersion:0,baseHash:'stale'});assert.equal((await agent.next('result')).error,'Device busy');
 device.send({type:'result',id:'edit-1',ok:true,head:1});assert.equal((await agent.next('result')).ok,true);
 agent.send({type:'command',id:'layout-1',action:'layout',css:'#live-work {padding: 12px}',baseVersion:0,baseHash:sourceHash(ledger.versions[0].source)});
 const layout=await device.next('command');assert.equal(layout.css,'#live-work {padding: 12px}');await agent.next('accepted');
 device.send({type:'result',id:'layout-1',ok:true,head:0});assert.equal((await agent.next('result')).head,0);
 assert.equal((await f.store.read('owner','wwRuboh')).ledger.versions.length,1,'layout does not create piece history');
 agent.ws.close();await once(agent.ws,'close');
 const second=await client(f.url,{role:'agent',code:'wwRuboh'});assert.equal((await second.next('ready')).online,true);
 device.send({type:'ping'});assert.equal((await device.next('pong')).attached,true);
 second.ws.close();
 device.ws.close();await once(device.ws,'close');
 const offline=await client(f.url,{role:'agent',code:'wwRuboh'});assert.equal((await offline.next('ready')).online,false);
 offline.send({type:'command',id:'edit-3',action:'undo',baseVersion:0,baseHash:'stale'});assert.match((await offline.next('result')).error,/offline/);
});
test('phone client persists identity, coalesces ledger sync without echo loop and survives reconnect',{timeout:5000},async t=>{
 const f=await fixture(t),values=new Map(),storage={getItem:k=>values.get(k)||null,setItem:(k,v)=>values.set(k,v)};
 const identity=threadIdentity(storage,'piece',()=>id);assert.equal(threadIdentity(storage,'piece',()=> 'wrong').id,identity.id);
 let connectedResolve;const connected=new Promise(r=>connectedResolve=r);
 const phone=new WalkiewareThread({storage,key:'piece',token:()=> 'owner',ledger:()=>ledger,state:()=>({busy:false,head:0,source:ledger.versions[0].source}),onStatus:(code,status)=>{if(status==='Connected')connectedResolve(code);},onCommand:async()=>({ok:true}),WebSocketImpl:WebSocket,url:f.url});t.after(()=>phone.suspend());
 await phone.resume();assert.equal(await connected,'wwRuboh');
 for(let i=0;i<50&&phone.sending;i++)await new Promise(r=>setTimeout(r,10));
 await new Promise(r=>setTimeout(r,50));assert.equal((await f.store.read('owner','wwRuboh')).revision,1);
 phone.sync();await new Promise(r=>setTimeout(r,30));assert.equal((await f.store.read('owner','wwRuboh')).revision,1);
 phone.suspend();await new Promise(r=>setTimeout(r,30));await phone.resume();await new Promise(r=>setTimeout(r,80));assert.equal(phone.ready,true);assert.equal((await f.store.read('owner','wwRuboh')).revision,1);
});
test('silent socket stalls reconnect even when close never emits; concurrent resume opens one socket',{timeout:2000},async()=>{
 const values=new Map(),storage={getItem:k=>values.get(k)||null,setItem:(k,v)=>values.set(k,v)};
 let count=0,offline=0;
 class SilentSocket {
  constructor(){count++;this.readyState=1;queueMicrotask(()=>this.onopen?.());}
  send(text){const m=JSON.parse(text);if(m.type==='authenticate')queueMicrotask(()=>this.onmessage?.({data:JSON.stringify({type:'ready',thread:{code:'wwRuboh',revision:1,ledger}})}));}
  close(){this.readyState=3;}
 }
 const phone=new WalkiewareThread({storage,key:'stall',token:()=> 'owner',ledger:()=>ledger,state:()=>({}),onStatus:(_,status)=>{if(status==='Offline')offline++;},onCommand:async()=>({ok:true}),WebSocketImpl:SilentSocket,heartbeatMs:5,maxIdleMs:15,reconnectMs:1});
 try{await Promise.all([phone.resume(),phone.resume()]);assert.equal(count,1);await new Promise(r=>setTimeout(r,55));assert.ok(count>=2);assert.ok(offline>=1);}finally{phone.suspend();}
});

async function completedReceipt(){
 const values=new Map(),storage={getItem:k=>values.get(k),setItem:(k,v)=>values.set(k,v)};
 const recorder=new AttemptReceipt({journal:new ReceiptJournal(storage,'test'),requestID:id,parent:0,parentHash:await hashSource('before'),path:'current',model:'tested/model'});
 recorder.request();recorder.finish('failed',await hashSource('before'));return recorder.value;
}
test('receipt store is owner-scoped, immutable, retry-idempotent and bounded',async()=>{
 const store=mongoWalkiewareStore(memoryCollection(),{name:()=> 'wwRuboh'});await store.open('owner',id);
 const receipt=await completedReceipt();await store.receipt('owner',id,receipt);await store.receipt('owner',id,receipt);
 assert.equal((await store.read('owner','wwRuboh')).receipts.length,1);
 await assert.rejects(store.receipt('owner',id,{...receipt,status:'completed'}),/immutable/);
 await assert.rejects(store.receipt('stranger',id,receipt),/unavailable/);
 for(let i=0;i<101;i++)await store.receipt('owner',id,{...receipt,id:crypto.randomUUID()});
 assert.equal((await store.read('owner','wwRuboh')).receipts.length,100);
 assert.equal(await store.clearReceipts('stranger','wwRuboh'),false);
 assert.equal(await store.clearReceipts('owner','wwRuboh'),true);assert.equal((await store.read('owner','wwRuboh')).receipts,undefined);
});
test('socket acknowledges persisted receipts and rejects agent uploads',{timeout:5000},async t=>{
 const f=await fixture(t),device=await client(f.url,{role:'device',id});
 assert.ok((await device.next('ready')).capabilities.includes('attempt-receipts-v1'));
 const receipt=await completedReceipt();device.send({type:'receipt',receipt});assert.equal((await device.next('receiptSaved')).id,receipt.id);
 assert.equal((await f.store.read('owner','wwRuboh')).receipts[0].status,'failed');
 const agent=await client(f.url,{role:'agent',code:'wwRuboh'});await agent.next('ready');
 agent.send({type:'receipt',receipt});assert.match((await agent.next('error')).error,/Unsupported/);
});

test('receipt client defers old servers, retries after lost acknowledgement, and drains on acknowledgement',async()=>{
 const values=new Map(),storage={getItem:k=>values.get(k)||null,setItem:(k,v)=>values.set(k,v)};
 const receipts=new ReceiptJournal(storage,'delivery'),receipt=await completedReceipt();receipts.save(receipt);
 const sockets=[];
 class Socket {
  constructor(){this.readyState=1;this.sent=[];sockets.push(this);}
  send(text){this.sent.push(JSON.parse(text));}
  close(){this.readyState=3;}
  async ready(capabilities){await this.onmessage({data:JSON.stringify({type:'ready',capabilities,thread:{code:'wwRuboh',revision:0,ledger}})});}
 }
 const phone=new WalkiewareThread({storage,key:'delivery',receipts,token:()=> 'owner',ledger:()=>ledger,state:()=>({}),onStatus:()=>{},onCommand:async()=>({}),WebSocketImpl:Socket});
 try {
  await phone.resume();await sockets[0].ready(undefined);
  assert.equal(sockets[0].sent.some(m=>m.type==='receipt'),false);assert.equal(receipts.pending().id,receipt.id);
  phone.suspend();await phone.resume();await sockets[1].ready(['attempt-receipts-v1']);
  assert.equal(sockets[1].sent.filter(m=>m.type==='receipt').length,1);
  phone.flushReceipts();assert.equal(sockets[1].sent.filter(m=>m.type==='receipt').length,1);
  phone.suspend();await phone.resume();await sockets[2].ready(['attempt-receipts-v1']);
  assert.equal(sockets[2].sent.find(m=>m.type==='receipt').receipt.id,receipt.id);
  await sockets[2].onmessage({data:JSON.stringify({type:'receiptSaved',id:receipt.id})});
  assert.equal(receipts.pending(),null);assert.equal(phone.ready,true);
 }finally{phone.suspend();}
});
