import test from "node:test";
import assert from "node:assert/strict";
import {readFile} from "node:fs/promises";
import {createRozPlan,ROZ_SOURCE} from "../kidlisp/graph/roz-plan.mjs";
import {validateRozNodes} from "../kidlisp/graph/roz-nodes.mjs";
import {prepareRozAssets,renderRozCPU} from "../kidlisp/graph/roz-assets.mjs";
import {createRozClock} from "../kidlisp/graph/roz-clock.mjs";

test("pinned $roz uses reproducible reference controls across timer boundaries",async()=>{
  const corpus=JSON.parse(await readFile(new URL("../kidlisp/conformance/corpus.json",import.meta.url)));
  assert.equal(ROZ_SOURCE,corpus.pieces.find(p=>p.code==="roz").source);
  const a=createRozPlan(),b=createRozPlan(),other=createRozPlan({seed:2});
  const alpha=new Set(),spin=new Set(),ops=new Set();let changed=false;
  for(let frame=0;frame<600;frame++) {
    const command=a.next();assert.deepEqual(command,b.next());validateRozNodes(command.nodes);
    changed ||= JSON.stringify(command)!==JSON.stringify(other.next());
    assert.ok(command.steps>0&&command.steps<10000);
    for(const n of command.nodes){ops.add(n.op);if(n.op==="line")alpha.add(n.color[3]);if(n.op==="spin")spin.add(n.value);}
  }
  assert.ok(changed);assert.deepEqual([...alpha].sort(),[24,64]);assert.deepEqual([...spin].sort((a,b)=>a-b),[-2,-1,1,2]);
  assert.deepEqual([...ops].sort(),["circle","contrast","line","scroll","spin","zoom"]);
  assert.throws(()=>createRozPlan({source:"(wipe red)"}),/pinned/);
  for(const options of [{width:513},{height:0},{seed:-1},{stepMs:0}])assert.throws(()=>createRozPlan(options));
});

test("effect gather maps preserve actual CPU sampling, including partial rows",()=>{
  for(const [width,height]of [[32,32],[33,35],[96,70]]) {
    const a=prepareRozAssets(width,height);
    for(const [key,map]of Object.entries(a.maps)) {
      const words=Uint32Array.from({length:width*height},(_,i)=>(0xff000000|i)>>>0);
      const b={width,height,pixels:new Uint8ClampedArray(words.buffer)};
      const node=key==="zoom"?{op:"zoom",value:1.1}:{op:"spin",value:Number(key.split(":")[1])};
      renderRozCPU(b,[node]);assert.deepEqual(Uint32Array.from(words,w=>w&0xffffff),map);
      assert.ok(map.every(i=>i<width*height));
    }
  }
});

test("shader command bounds reject unsupported work",()=>{
  for(const nodes of [null,Array(7).fill({op:"zoom",value:1.1}),[{op:"zoom",value:2}],[{op:"circle",radius:4,color:[255,0,0,9]}],[{op:"circle",radius:4,color:[7,0,0,8]}],[{op:"spin",value:NaN}],[{op:"scroll",x:2,y:0}],[{op:"line",color:[0,0,0,256]}],[{op:"blur"}]])assert.throws(()=>validateRozNodes(nodes));
  assert.throws(()=>prepareRozAssets(1024,1024));
});

test("presentation cadence cannot halve the simulation clock",()=>{
  for(const fps of [30,60,120]) {
    const clock=createRozClock();let steps=0;
    for(let i=0;i<=fps*4;i++)steps+=clock.tick(i*1000/fps);
    assert.equal(steps,240,`${fps} fps must execute 240 updates in four seconds`);
  }
  const clock=createRozClock();clock.tick(0);
  assert.equal(clock.tick(1000),4,"suspension catch-up is bounded");
  assert.equal(clock.tick(2000,{paused:true}),0);
  assert.equal(clock.tick(2000+1000/60),1);
  assert.equal(clock.tick(2050,{blocked:true}),0);
  assert.equal(clock.tick(2050),2,"queued GPU work delays rather than discards effects");
});
