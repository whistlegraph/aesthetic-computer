import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFileSync } from "node:fs";
import { bootTelemetryUpdate, writeBootTelemetry } from "../backend/boot-telemetry.mjs";
import { createBootDiagnostics } from "../public/aesthetic.computer/lib/boot-diagnostics.mjs";

// Evaluate the small Mongo expression subset against a document to exercise
// out-of-order delivery; no database, live telemetry, or browser is involved.
function apply(doc, pipeline) {
  const value = (v) => {
    if (typeof v === "string" && v.startsWith("$")) return v.slice(1).split('.').reduce((o,k)=>o?.[k], doc);
    if (Array.isArray(v)) return v.map(value);
    if (!v || typeof v !== "object") return v;
    if ("$literal" in v) return structuredClone(v.$literal);
    if ("$ifNull" in v) { const [a,b]=v.$ifNull; return value(a) ?? value(b); }
    if ("$eq" in v) { const [a,b]=value(v.$eq); return a===b; }
    if ("$cond" in v) { const [c,a,b]=v.$cond; return value(value(c)?a:b); }
    if ("$concatArrays" in v) return value(v.$concatArrays).flat();
    if ("$slice" in v) { const [a,n]=value(v.$slice); return a.slice(n); }
    throw Error("Unknown expression");
  };
  for (const stage of pipeline) {
    const writes=Object.entries(stage.$set).map(([key,v])=>[key,value(v)]);
    for(const [key,v] of writes){const keys=key.split('.');const last=keys.pop();let dest=doc;for(const k of keys)dest=dest[k]??={};dest[last]=v;}
  }
  return doc;
}
const base={bootId:"synthetic",meta:{host:"synthetic.invalid",user:null},server:{country:null},now:new Date("2026-09-21T07:00:00Z")};
const phase=(doc, phase, data={}, extra={})=>apply(doc,bootTelemetryUpdate({...base,phase,data,...extra}));

test("any first boot phase preserves identity despite a delayed start",()=>{
  for(const first of ["log","error","complete"]){
    const doc=phase({},first,{events:[{message:"synthetic"}]});
    phase(doc,"start");
    assert.equal(doc.bootId,base.bootId);
    assert.equal(doc.meta.host,base.meta.host);
    assert.deepEqual(doc.createdAt,base.now);
    assert.equal(doc.status,first==="complete"?"success":first==="error"?"error":"started");
  }
});
test("completion survives late errors and retains the error evidence and late user",()=>{
  const doc=phase({},"complete",{elapsedTotal:80});
  phase(doc,"error",{error:{message:"$private-looking-literal"}});
  phase(doc,"log",{events:[]},{meta:{user:{handle:"@synthetic"}}});
  assert.equal(doc.status,"success");
  assert.equal(doc.error.error.message,"$private-looking-literal");
  assert.equal(doc.summary.elapsedTotal,80);
  assert.equal(doc.meta.host,base.meta.host);
  assert.equal(doc.meta.user.handle,"@synthetic");
});
test("boot event retention stays bounded across successive batches",()=>{
  const doc=phase({},"log",{events:Array.from({length:600},(_,n)=>({n}))});
  phase(doc,"log",{events:[{n:600}]});
  assert.equal(doc.events.length,500);
  assert.equal(doc.events[0].n,101);
  assert.equal(doc.events.at(-1).n,600);
});
function timingFixture(){
  let now=0,tick,cleared=false;
  const document=new EventTarget();document.visibilityState="visible";
  const window=new EventTarget(),events=[];
  const perf={now:()=>now,getEntriesByType:()=>[]};
  const tracker=createBootDiagnostics({document,window,performance:perf,origin:"https://synthetic.invalid",setInterval:fn=>{tick=fn;return 1;},clearInterval:()=>{cleared=true;},onLifecycle:e=>events.push(e)});
  return {tracker,document,window,events,perf,time:n=>{now=n;},tick:()=>tick(),cleared:()=>cleared};
}
test("hidden intervals and timer suspension are distinct, and finish removes listeners",()=>{
  const f=timingFixture();f.time(1000);f.tick();
  f.document.visibilityState="hidden";f.document.dispatchEvent(new Event("visibilitychange"));
  f.time(21000);f.tick();
  f.document.visibilityState="visible";f.document.dispatchEvent(new Event("visibilitychange"));
  f.time(22000);const s=f.tracker.finish();
  assert.equal(s.hiddenMs,20000);assert.equal(s.visibleMs,2000);assert.equal(s.maxTimerGapMs,19000);
  assert.equal(f.cleared(),true);f.window.dispatchEvent(new Event("pagehide"));assert.equal(f.events.length,2);
  f.time(99999);assert.deepEqual(f.tracker.snapshot(),s);
});
test("completion detects a delayed timer even before its queued callback runs",()=>{
  const f=timingFixture();f.time(20000);
  assert.equal(f.tracker.finish().maxTimerGapMs,19000);
});
test("resource timing exports only fixed public core labels, bounded, without URLs",()=>{
  const f=timingFixture();
  f.perf.getEntriesByType=()=>[
    {name:"https://synthetic.invalid/api/mail?token=secret",duration:999},
    {name:"https://other.invalid/aesthetic.computer/bios.mjs",duration:999},
    ...Array.from({length:20},(_,i)=>({name:`https://synthetic.invalid/aesthetic.computer/bios.mjs?secret=${i}`,duration:i,requestStart:1,responseStart:2,responseEnd:3})),
  ];
  const rows=f.tracker.resources();assert.equal(rows.length,12);
  assert.equal(rows[0].durationMs,19);assert.equal(rows[0].module,"bios");
  assert.doesNotMatch(JSON.stringify(rows),/secret|https|mail|other/);
});
test("a hanging batched log request does not prevent the completion POST",async()=>{
  const source=readFileSync(new URL("../public/aesthetic.computer/boot.mjs",import.meta.url),"utf8");
  const section=source.slice(source.indexOf("const bootTelemetry = (() => {"),source.indexOf("bootTelemetry.start();"));
  const phases=[],timeouts=[];
  const context=vm.createContext({crypto:{randomUUID:()=>"synthetic"},Date,Math,Intl,AbortController,
    navigator:{userAgent:"synthetic"},location:{hostname:"synthetic.invalid"},document:{},
    window:{},performance:{now:()=>1},bootStartTime:0,setTimeout:(fn,ms)=>{if(ms===10000)timeouts.push(fn);return timeouts.length;},clearTimeout(){},
    bootDiagnostics:{snapshot:()=>({}),finish:()=>({}),resources:()=>[]},
    fetch:async(_url,opts)=>{const {phase}=JSON.parse(opts.body);phases.push(phase);if(phase==="log")await new Promise((_,reject)=>opts.signal.addEventListener("abort",()=>reject(new Error("aborted"))));},
  });
  vm.runInContext(section+'bootTelemetry.enqueue("synthetic"); globalThis.completion=bootTelemetry.complete({elapsedTotal:1});',context);
  assert.deepEqual(phases,["log","complete"]);
  for(const timeout of timeouts)timeout();
  await context.completion;
});

test("new records have deterministic IDs; legacy identity is preserved",()=>{
  assert.equal(phase({},"start")._id,base.bootId);
  assert.equal(phase({_id:"legacy-object-id"},"complete")._id,"legacy-object-id");
});
test("only a duplicate insert for this boot retries as an update",async()=>{
  const calls=[];
  const duplicate=Object.assign(new Error("duplicate"),{code:11000,keyValue:{_id:base.bootId}});
  const boots={async updateOne(...args){calls.push(args);if(calls.length===1)throw duplicate;return {matchedCount:1};}};
  await writeBootTelemetry(boots,base.bootId,[]);
  assert.equal(calls[0][2].upsert,true);
  assert.deepEqual(calls[1][0],{_id:base.bootId,bootId:base.bootId});
  assert.equal(calls[1][2],undefined);
  await assert.rejects(writeBootTelemetry({updateOne:async()=>{throw Object.assign(new Error("other unique index"),{code:11000,keyValue:{other:"x"}});}},base.bootId,[]),/other unique index/);
});

test("malformed boot metadata is rejected before opening a database connection",async()=>{
  const source=readFileSync(new URL("../netlify/functions/boot-log.mjs",import.meta.url),"utf8")
    .replace(/^import .*;$/gm,"").replace("export async function handler","async function handler");
  const context=vm.createContext({respond:(statusCode,body)=>({statusCode,body}),connect:()=>{throw Error("must not connect");}});
  vm.runInContext(source,context);
  for(const fields of [{bootId:""},{bootId:"a".repeat(129)},{bootId:1},{phase:"bogus"},{meta:null},{data:null},{data:[]}]){
    const result=await context.handler({httpMethod:"POST",body:JSON.stringify({bootId:"synthetic",phase:"start",...fields})});
    assert.equal(result.statusCode,400);
  }
});
