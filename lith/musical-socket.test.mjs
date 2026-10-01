import test from 'node:test';
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {once} from 'node:events';
import {WebSocket} from 'ws';
import {attachMusicalSocket} from './musical-socket.mjs';
import {createMusicalHandler} from '../system/backend/easel-musical-jev.mjs';
import {MusicalInputSocket} from '../aesel/src/musical-input-socket.mjs';
import {MusicalInputAdvisor} from '../aesel/src/musical-input-advisor.mjs';
const input={transcript:'Private words',words:[],sound:{frames:[1,2,3].map(i=>({atMs:i*100,pitchHz:440,rms:.2})),onsetsMs:[]}};
const features={hasSpeech:true,hasTonalSound:true,soundAfterSpeech:false,contour:'steady',attacks:0,rhythm:'unknown',energy:'steady'};
const body=(sequence=1)=>({schema:'walkieware-input/v1',sessionId:'12345678-1234-1234-1234-123456789012',sequence,features});
const answer={answers:{mapping:{choice:'follow_speech',probabilities:{follow_speech:.95}}}};
async function fixture(t,overrides={}){
 let auths=0,calls=0,quotas=0;
 const authenticate=async h=>{auths++;return h.authorization==='Bearer valid'?'account':null;};
 const decide=createMusicalHandler({authenticate,budget:{consume:async()=>{quotas++;return true;}},evaluate:async()=>{calls++;return answer;}});
 const server=createServer();const binding=attachMusicalSocket(server,{authenticate,decide,...overrides});server.listen(0,'127.0.0.1');await once(server,'listening');
 t.after(()=>{binding.close();server.close();});
 return {url:`ws://127.0.0.1:${server.address().port}/api/easel-musical-stream`,counts:()=>({auths,calls,quotas})};
}
function inbox(ws){const messages=[],waiters=[];ws.on('message',d=>{const m=JSON.parse(d);const i=waiters.findIndex(w=>w.type===m.type);if(i>=0)waiters.splice(i,1)[0].resolve(m);else messages.push(m);});return type=>{const i=messages.findIndex(m=>m.type===type);if(i>=0)return Promise.resolve(messages.splice(i,1)[0]);return new Promise(resolve=>waiters.push({type,resolve}));};}
async function client(url){const ws=new WebSocket(url),next=inbox(ws);await once(ws,'open');return {ws,next,send:m=>ws.send(JSON.stringify(m))};}
test('real socket authenticates once, keeps per-decision quotas and reports timing', {timeout:3000},async t=>{
 const f=await fixture(t),c=await client(f.url);c.send({type:'authenticate',token:'valid'});await c.next('ready');
 for(let sequence=1;sequence<=2;sequence++){c.send({type:'observation',body:body(sequence)});assert.equal((await c.next('received')).sequence,sequence);const r=await c.next('decision');assert.equal(r.choice,'follow_speech');assert.equal(r.sequence,sequence);assert.ok(r.serverMs>=r.elapsedMs);}
 assert.deepEqual(f.counts(),{auths:1,calls:2,quotas:2});
 c.send({type:'observation',body:{...body(3),prompt:'injected'}});assert.equal((await c.next('error')).status,400);assert.equal(f.counts().calls,2);
});
test('unauthenticated, malformed and expired connections are closed', {timeout:3000},async t=>{
 const f=await fixture(t,{authMs:30,lifetimeMs:150});
 const a=await client(f.url);a.send({type:'observation',body:body()});assert.equal((await once(a.ws,'close'))[0],1008);
 const b=await client(f.url);b.send({type:'authenticate',token:'wrong'});assert.equal((await once(b.ws,'close'))[0],1008);
 const c=await client(f.url);assert.equal((await once(c.ws,'close'))[0],1008);
 const d=await client(f.url);d.send({type:'authenticate',token:'valid'});await d.next('ready');d.send({type:'observation'});assert.equal((await once(d.ws,'close'))[0],1008);
 const e=await client(f.url);e.send({type:'authenticate',token:'valid'});await e.next('ready');assert.equal((await once(e.ws,'close'))[0],1000);
 assert.equal(f.counts().calls,0);
});
test('slow inference keeps newest pending observation; cancel aborts active work', {timeout:3000},async t=>{
 const started=[],releases=[];
 const f=await fixture(t,{decide:(event,{signal})=>new Promise(resolve=>{const b=JSON.parse(event.body);started.push(b.sequence);const release=()=>resolve({statusCode:200,body:JSON.stringify({...b,schema:'walkieware-decision/v1',choice:'sustain',confidence:.95})});releases.push(release);signal.addEventListener('abort',release);})});
 const c=await client(f.url);c.send({type:'authenticate',token:'valid'});await c.next('ready');
 c.send({type:'observation',body:body(1)});await c.next('received');
 c.send({type:'observation',body:body(2)});await c.next('received');
 c.send({type:'observation',body:body(3)});await c.next('received');assert.equal((await c.next('error')).sequence,2);
 releases[0]();assert.equal((await c.next('decision')).sequence,1);assert.deepEqual(started,[1,3]);
 c.send({type:'cancel',sessionId:body().sessionId});await new Promise(r=>setTimeout(r,10));
 c.send({type:'observation',body:body(4)});await c.next('received');assert.deepEqual(started,[1,3,4]);releases[2]();assert.equal((await c.next('decision')).sequence,4);
});
test('phone client uses warm socket, matching cache, HTTP on cold connection, reconnect after close', {timeout:4000},async t=>{
 const f=await fixture(t);const events=[];let ready;const waitReady=()=>new Promise(r=>ready=r);let readyPromise=waitReady();
 const transport=new MusicalInputSocket({url:f.url,WebSocketImpl:WebSocket,token:()=> 'valid',onEvent:(event,fields)=>{events.push({event,fields});if(event==='inputSocketReady')ready();},fetchImpl:async()=>{throw Error('Unexpected HTTP');}});t.after(()=>transport.suspend());
 transport.connect();await readyPromise;
 const a=new MusicalInputAdvisor({token:()=> 'valid',fetchImpl:transport.fetch,onEvent:(event,fields)=>events.push({event,fields})});
 assert.equal((await a.finish(input)).choice,'follow_speech');assert.equal((await a.finish(input)).choice,'follow_speech');
 assert.equal(f.counts().calls,1);assert.equal(events.find(e=>e.event==='jevDecision').fields.transport,'socket');assert.ok(events.some(e=>e.event==='inputSocketAck'));
 readyPromise=waitReady();transport.ws.close();await readyPromise;assert.equal(f.counts().auths,2);
 let http=0;const cold=new MusicalInputSocket({token:()=> 'valid',WebSocketImpl:null,fetchImpl:async()=>{http++;return {ok:true};}});assert.equal((await cold.fetch('url',{body:'{}'})).ok,true);assert.equal(http,1);cold.suspend();
});
