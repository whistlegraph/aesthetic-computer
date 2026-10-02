import assert from 'node:assert/strict';
import test from 'node:test';
import {EventEmitter} from 'node:events';
import {deferredEngine} from '../src/deferred-engine.mjs';

test('provider loading preserves restored conversation, events and private method receivers',async()=>{
  let loads=0;
  class Real extends EventEmitter{
    #value='connected';
    constructor(options){super();this.model=options.model;this.messages=[];this.threadId='new';}
    async connect(){this.emit('notification',{ready:true});return {model:this.model};}
    respond(){return this.#value;}
    close(){this.closed=true;}
  }
  const Engine=deferredEngine(async()=>{loads++;return Real;});
  const engine=new Engine({model:'chosen'}),events=[];
  engine.threadId='restored';engine.messages=[{role:'user',content:'keep this'}];engine.turns=3;
  engine.on('notification',event=>events.push(event));
  assert.equal(loads,0,'constructing the editor must not load provider code');
  const first=engine.connect();assert.equal(engine.connect(),first);
  assert.deepEqual(await first,{model:'chosen'});
  assert.equal(loads,1);assert.equal(engine.threadId,'restored');assert.equal(engine.turns,3);
  assert.equal(engine.messages[0].content,'keep this');
  assert.deepEqual(events,[{ready:true}]);assert.equal(engine.respond(),'connected');
  engine.threadId='next';assert.equal(engine.threadId,'next');
  engine.close();assert.equal(engine.closed,true);
});

test('closing while a provider imports cannot start an orphan engine',async()=>{
  let resolveImport,constructed=0;
  const Engine=deferredEngine(()=>new Promise(resolve=>{resolveImport=resolve;}));
  const engine=new Engine({});
  const opening=engine.connect();await Promise.resolve();engine.close();
  resolveImport(class{constructor(){constructed++;}});
  await assert.rejects(opening,/closed while opening/);
  assert.equal(constructed,0);
});
