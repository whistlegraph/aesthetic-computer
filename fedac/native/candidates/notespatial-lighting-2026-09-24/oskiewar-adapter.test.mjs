import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import vm from 'node:vm';
import {LOOKS} from './score-look.mjs';
test('Oskiewar adapter draws every movement with finite silent primitives',async()=>{
  let calls=0;
  const draw=(...args)=>{calls++;for(const a of args)if(typeof a==='number')assert.ok(Number.isFinite(a));};
  const context=vm.createContext({viewWidth:()=>1920,viewHeight:1080,screenRect:draw,filledCapsule:draw,
    wipe:draw,typeWrite:draw,triangleDepth:0,clamp:(x,a,b)=>Math.max(a,Math.min(b,x))});
  vm.runInContext(await readFile(new URL('./oskiewar-adapter.js',import.meta.url),'utf8'),context);
  for(const [section,look] of LOOKS.entries()) {
    calls=0;
    context.drawNotepatScore({look:{...look,section,active:true,rgb:[100,140,180],energy:.6,pitch:60},movement:{t0:0,t1:60}},30);
    assert.ok(calls>3&&calls<=65);
  }
});
