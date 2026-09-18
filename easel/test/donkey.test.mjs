import test from 'node:test';import assert from 'node:assert/strict';import donkey from '../desktop/donkey.js';
const {phaseFor,animationFrame,manifestAnimations,ANIMATIONS}=donkey;
test('donkey reflects generation, launch, approval and quiet states',()=>{
 const select=status=>phaseFor({status,now:100000,lastActivity:0,awakeUntil:0});
 for(const s of ['working','generating','streaming','writing'])assert.equal(select(s),'working');
 for(const s of ['starting','connecting','queued'])assert.equal(select(s),'running');
 for(const s of ['awaiting','approval','error'])assert.equal(select(s),'awake');
 assert.equal(select('ready'),'sleeping');assert.equal(phaseFor({status:'ready',now:1000,lastActivity:0,awakeUntil:2000}),'awake');assert.equal(phaseFor({status:'complete',now:3000,lastActivity:0,awakeUntil:2000}),'idle');
});
test('frame timing uses explicit holds, exact boundaries and reduced-motion stills',()=>{
 assert.deepEqual(animationFrame('idle',1399),{frame:0,delay:1});assert.deepEqual(animationFrame('idle',1400),{frame:1,delay:180});assert.deepEqual(animationFrame('idle',1580),{frame:0,delay:1400});
 assert.deepEqual(animationFrame('working',360),{frame:10,delay:180});assert.deepEqual(animationFrame('running',480),{frame:12,delay:120});assert.deepEqual(animationFrame('sleeping',2000,true),{frame:4,delay:null});
});
test('malformed asset manifest cannot create an unbounded animation loop',()=>{
 assert.equal(manifestAnimations({idle:{frames:[100],durations:[0]}}),ANIMATIONS);assert.deepEqual(manifestAnimations({animations:ANIMATIONS}),ANIMATIONS);
});

test('hidden windows cancel animation timers and reduced-motion working stays still',async()=>{
 const {readFileSync}=await import('node:fs'),{runInNewContext}=await import('node:vm');
 const listeners=new Map(),timers=new Map();let serial=0,now=0;
 const doc={createElement:()=>({getContext:()=>({})}),hidden:false,addEventListener:(n,f)=>listeners.set(n,f),removeEventListener:n=>listeners.delete(n)};
 const motion={matches:false,addEventListener:(n,f)=>listeners.set('motion',f),removeEventListener:()=>listeners.delete('motion')};
 const window={document:doc,matchMedia:()=>motion,Image:class{},fetch:async()=>({ok:false}),setTimeout:(f,d)=>{timers.set(++serial,{f,d});return serial;},clearTimeout:id=>timers.delete(id)};
 runInNewContext(readFileSync(new URL('../desktop/companion-scene.js',import.meta.url),'utf8'),{window});
 runInNewContext(readFileSync(new URL('../desktop/donkey.js',import.meta.url),'utf8'),{window});
 const canvas={dataset:{},getContext:()=>({}),setAttribute(){}};
 const companion=window.AeselDonkey.createDonkey({canvas,clock:()=>now});companion.update({status:'working'});assert.equal(timers.size,1);assert.equal([...timers.values()][0].d,180);
 doc.hidden=true;listeners.get('visibilitychange')();assert.equal(timers.size,0);
 motion.matches=true;doc.hidden=false;listeners.get('visibilitychange')();assert.equal(timers.size,0);
 companion.update({status:'ready'});assert.equal([...timers.values()][0].d,2500);
 now=70000;listeners.get('motion')();assert.equal(canvas.dataset.state,'sleeping');assert.equal(timers.size,0);
 listeners.get('keydown')();assert.equal(canvas.dataset.state,'awake');assert.equal(timers.size,1);
 companion.destroy();assert.equal(timers.size,0);assert.equal(listeners.size,0);
});
