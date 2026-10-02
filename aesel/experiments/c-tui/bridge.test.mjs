import test from 'node:test';
import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import {NativeBridge,frame,decodeInput} from './bridge.mjs';

function fixture(){
  const sent=[],engines=[];
  class Fake extends EventEmitter{
    constructor(options){super();this.options=options;this.threadId=null;this.closed=false;this.prompts=[];engines.push(this);}
    async connect(){this.threadId=this.options.resumeThreadId||'same-thread';}
    async startTurn(text){this.prompts.push(text);this.emit('notification',{method:'turn/started',params:{turn:{id:'t'}}});}
    close(){this.closed=true;}
    reject(){}
  }
  return {sent,engines,bridge:new NativeBridge({send:(...message)=>sent.push(message),create:options=>new Fake(options),retryTimeout:20})};
}
test('framing accepts split UTF-8 packets and rejects oversized or unknown input',()=>{
  const calls=[],decode=decodeInput((...args)=>calls.push(args));
  for(const byte of frame('P','hello 🟣'))decode(Buffer.from([byte]));
  assert.deepEqual(calls,[['P','hello 🟣']]);
  assert.throws(()=>decodeInput(()=>{})(frame('P','x'.repeat(8192))),/exceeds/);
  assert.throws(()=>decodeInput(()=>{})(frame('Z','')),/Unknown/);
});
test('a failed accepted turn resumes the same thread with a continuation, not a second original prompt',async()=>{
  const {bridge,engines,sent}=fixture();
  await bridge.submit('make something');
  engines[0].emit('fatal',new Error('connection reset'));
  assert.equal(bridge.active,false);
  assert.equal(engines.length,1,'recovery requires explicit /retry');
  await bridge.submit('',{retry:true});
  assert.equal(engines[1].options.resumeThreadId,'same-thread');
  assert.match(engines[1].prompts[0],/do not repeat completed actions/);
  assert.notEqual(engines[1].prompts[0],'make something');
  engines[0].emit('notification',{method:'item/agentMessage/delta',params:{delta:'stale'}});
  assert.ok(!sent.some(([type,text])=>type==='D'&&text==='stale'));
  bridge.close();
});
test('cancel during startup prevents submission and leaves the next prompt usable',async()=>{
  const {bridge,engines}=fixture();let release;
  bridge.create=options=>{
    const engine=new EventEmitter();Object.assign(engine,{options,closed:false,threadId:null,prompts:[],close(){this.closed=true;},
      connect:()=>new Promise(resolve=>{release=()=>{engine.threadId='same-thread';resolve();};}),startTurn:async text=>engine.prompts.push(text)});
    engines.push(engine);return engine;
  };
  const turn=bridge.submit('cancel this');bridge.cancel();release();await turn;
  assert.deepEqual(engines[0].prompts,[]);assert.equal(bridge.pending,null);
  const next=bridge.submit('next');release();await next;assert.deepEqual(engines[1].prompts,['next']);bridge.close();
});
test('provider retry stays singular, times out, and retains the interrupted request',async()=>{
  const {bridge,engines}=fixture();await bridge.submit('continue me');
  engines[0].emit('notification',{method:'error',params:{willRetry:true}});
  engines[0].emit('notification',{method:'error',params:{willRetry:true}});
  assert.equal(engines.length,1);assert.equal(bridge.active,true);
  await new Promise(resolve=>setTimeout(resolve,35));
  assert.equal(bridge.active,false);assert.equal(bridge.pending.text,'continue me');assert.equal(engines[0].closed,true);bridge.close();
});
