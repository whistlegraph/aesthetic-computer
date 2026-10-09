import test from "node:test";
import assert from "node:assert/strict";
import {KidLisp} from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import {KidLispExecution} from "../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import * as graph from "../system/public/aesthetic.computer/lib/graph.mjs";
import {inclusions,auditInclusions} from "../kidlisp/conformance/inclusions.mjs";

function replay(source, sources, frames=4) {
  const execution=new KidLispExecution({seed:1,maxSteps:20000});
  const lisp=new KidLisp({execution}),piece=lisp.module(source,true);
  lisp.cacheInitiated=true;lisp.autoDensityOverride=true;
  for(const [code,source]of Object.entries(sources))lisp.embeddedSourceCache.set(code,source);
  const display={width:64,height:64,pixels:new Uint8ClampedArray(64*64*4)};
  const api={screen:{...display},params:[],colon:[],clock:execution.clock,system:{fps:60},sound:{},
    fps(){},needsPaint(){},toggleHUD(){},send(){},
    page(buffer){assert.ok(buffer);Object.assign(api.screen,buffer);graph.setBuffer(buffer);},
    ink(...args){graph.color(...graph.findColor(...args));return api;},inkrn:()=>graph.color(),
    wipe(...args){const old=graph.color();graph.color(...graph.findColor(...args));graph.clear();graph.color(...old);return api;},
    unmask:graph.unmask,paste:graph.paste,blend:graph.blendMode,line:graph.line,box:graph.box,circle:graph.circle,
    scroll:graph.scroll,resetScrollState:graph.resetScrollState,spin:graph.spin,smoothSpin:graph.smoothSpin,
  };
  api.backgroundFill=api.wipe;api.page(display);lisp.setAPI(api);
  const warnings=[],errors=[],oldWarn=console.warn,oldError=console.error;
  console.warn=(...a)=>warnings.push(a.join(" "));console.error=(...a)=>errors.push(a.join(" "));
  const output=[];
  try {
    execution.beginFrame(0);piece.boot(execution.bindApi(api));
    for(let frame=0;frame<frames;frame++) {
      execution.beginFrame(frame);api.page(display);api.frame=frame+1;
      piece.sim(execution.bindApi(api));piece.paint(execution.bindApi(api));
      assert.equal(api.screen.pixels,display.pixels,"nested evaluation must restore the display");
      assert.equal(execution.state.depth,0);assert.equal(execution.state.error,null);
      output.push(display.pixels.slice());
    }
    return {frames:output,warnings,errors,lisp};
  } finally {piece.leave?.();console.warn=oldWarn;console.error=oldError;}
}
const pixel=(rgba,x,y)=>[...rgba.slice((x+y*64)*4,(x+y*64)*4+4)];

test("nested inclusions composite every level into the parent on their first frame",()=>{
  const r=replay("black\n($mid 0 0 w h)",{mid:"($deep 0 0 w h)",deep:"($leaf 0 0 w/2 h)",leaf:"red"});
  assert.deepEqual(r.errors,[]);assert.deepEqual(r.warnings,[]);
  for(const rgba of r.frames){assert.deepEqual(pixel(rgba,16,32),[255,0,0,255]);assert.deepEqual(pixel(rgba,48,32),[0,0,0,255]);}
});

test("two parents sharing source retain independent child buffers and frame counters",()=>{
  const r=replay("black\n($left 0 0 w/2 h)\n($right w/2 0 w/2 h)",{
    left:"($leaf 0 0 w h)",right:"($leaf 0 0 w h)",leaf:"(wipe frame 0 0)",
  });
  assert.deepEqual(r.errors,[]);assert.deepEqual(r.warnings,[]);
  r.frames.forEach((rgba,i)=>{assert.deepEqual(pixel(rgba,16,32),[i+1,0,0,255]);assert.deepEqual(pixel(rgba,48,32),[i+1,0,0,255]);});
});

test("nested alpha composites and later parent drawing retain source order",()=>{
  const r=replay("black\n($mid 0 0 w h)",{mid:"blue\n($leaf 0 0 w h 128)\n(ink lime)\n(box 0 0 8 8)",leaf:"red"});
  assert.deepEqual(r.errors,[]);
  // Retain the existing integer compositor's >>8 byte rounding.
  assert.deepEqual(pixel(r.frames[0],32,32),[128,0,126,255]);
  assert.deepEqual(pixel(r.frames[0],2,2),[0,255,0,255]);
});

test("self and mutual cycles stop with one actionable warning per ancestry",()=>{
  for(const sources of [{aaa:"($aaa)"},{aaa:"($bbb)",bbb:"($aaa)"}]) {
    const r=replay("black\n($aaa)",sources);
    assert.deepEqual(r.errors,[]);assert.equal(r.warnings.length,1);assert.match(r.warnings[0],/inclusion cycle:.*\$aaa/);
    assert.deepEqual(pixel(r.frames.at(-1),32,32),[0,0,0,255]);
  }
});

test("dependency audit distinguishes repeated inclusions from cycles and prose",()=>{
  assert.deepEqual(inclusions('(write "$fake") ; $nope\n($real) ($real)'),["real","real"]);
  const report=auditInclusions({root:{source:"($aaa) ($aaa)"},aaa:{source:"($bbb)"},bbb:{source:"red"}},["root"]);
  assert.equal(report.maxDepth,2);assert.deepEqual(report.cycles,[]);assert.equal(report.duplicates[0].count,2);
  assert.deepEqual(auditInclusions({root:{source:"($bad)"}}).missing,[{parent:"root",child:"bad"}]);
  assert.ok(auditInclusions({aaa:{source:"($bbb)"},bbb:{source:"($aaa)"}}).cycles.length);
});

test("acyclic nesting has a finite depth budget",()=>{
  const sources=Object.fromEntries(Array.from({length:40},(_,i)=>[`n${String(i).padStart(2,"0")}`,i===39?"red":`($n${String(i+1).padStart(2,"0")})`]));
  const r=replay("black\n($n00)",sources,1);
  assert.deepEqual(r.errors,[]);assert.equal(r.warnings.length,1);assert.match(r.warnings[0],/inclusion depth limit/);
});
