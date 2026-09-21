import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { assertFrameTarget, nativeInputRequest, assertNativeInputReceipt } from "../lib/frame-target.mjs";
import { withMachineLease, machineLeaseKey } from "../lib/computer-use-lease.mjs";
import { withFrameSession, frameSessionId, frameStateKey, nativeFrameSession } from "../lib/frame-session.mjs";

test("frame session IDs survive asynchronous overlap and never share state keys", async () => {
  const read = id => withFrameSession({sessionId:id},{headers:{}},async()=>{
    const before=frameStateKey("local");await new Promise(r=>setTimeout(r,id==="one"?15:5));
    assert.equal(frameSessionId(),id);assert.equal(frameStateKey("local"),before);
    return {key:before,native:nativeFrameSession()};
  });
  const [a,b]=await Promise.all([read("one"),read("two")]);
  assert.notEqual(a.key,b.key);assert.notEqual(a.native,b.native);
  assert.throws(()=>withFrameSession({}, {headers:{},tool:"frame_reframe"},()=>{}),/sessionId is required/);
  assert.throws(()=>withFrameSession({sessionId:"bad token"},{},()=>{}),/Invalid/);
});

test("native lease excludes other processes, is reentrant, and releases on errors", async t => {
  const root=await mkdtemp(join(tmpdir(),"input-leases-test-"));
  t.after(()=>rm(root,{recursive:true,force:true}));
  const module=new URL("../lib/computer-use-lease.mjs",import.meta.url).href;
  const child=spawn(process.execPath,["--input-type=module","-e",`import {withMachineLease} from ${JSON.stringify(module)};await withMachineLease({local:true},async()=>{console.log('held');await new Promise(r=>process.stdin.once('data',r));},{root:${JSON.stringify(root)}});`],{stdio:["pipe","pipe","pipe"]});
  t.after(()=>child.kill());
  await once(child.stdout,"data");
  let entered=false;
  await assert.rejects(withMachineLease({local:true},()=>{entered=true;},{root,timeoutMs:50}),/busy/);
  assert.equal(entered,false);
  const closed=once(child,"close");child.stdin.end("release");await closed;
  await assert.rejects(withMachineLease({local:true},async()=>{
    await withMachineLease({local:true},()=>{}, {root});throw new Error("fixture failure");
  },{root}),/fixture failure/);
  await withMachineLease({local:true},()=>{entered=true;},{root});assert.equal(entered,true);
  assert.equal(machineLeaseKey({ssh:"jas@neo -i key"}),machineLeaseKey({sshHost:"neo"}));
});

test("native target guard rejects switched windows, moved geometry, and old observations", () => {
  const before={capture:"ok",capture_scope:"window",observation:{id:"one",windowId:4},meta:{frontmost:{pid:123}},crop:{x:10,y:20,w:400,h:300}};
  assertFrameTarget(before,structuredClone(before),"one");
  assert.throws(()=>assertFrameTarget(before,before,"old"),/superseded/);
  const moved=structuredClone(before);moved.crop.x++;
  assert.throws(()=>assertFrameTarget(before,moved,"one"),/moved/);
  const switched=structuredClone(before);switched.observation.windowId=5;
  assert.throws(()=>assertFrameTarget(before,switched,"one"),/changed/);
  assert.throws(()=>assertFrameTarget({...before,capture_scope:"crop"},before),/window-scoped/);
});

test('native input binds its receipt to the authorizing observation and refuses malformed input', () => {
  const before={capture:'ok',capture_scope:'window',observation:{id:'one',windowId:4},meta:{frontmost:{pid:123}},crop:{x:10,y:20,w:400,h:300}};
  const request=nativeInputRequest(before,'one',{x:30,y:40,count:1,settleMs:0});
  assert.equal(request.observationId,'one');
  assert.throws(()=>nativeInputRequest(before,'old'),/superseded/);
  assert.throws(()=>nativeInputRequest(before,'one',{x:NaN,y:40,count:1,settleMs:0}),/Invalid native click/);
  assert.throws(()=>nativeInputRequest(before,'one',{x:30,y:40,count:1.5,settleMs:0}),/Invalid native click/);
  assertNativeInputReceipt({nativeInput:{observationId:'one',status:'dispatched'}},request,'dispatched');
  assert.throws(()=>assertNativeInputReceipt({capture:'ok'},request,'dispatched'),/unknown.*not retried/);
  assert.throws(()=>assertNativeInputReceipt({nativeInput:{observationId:'two',status:'dispatched'}},request,'dispatched'),/mismatched/);
  assert.throws(()=>assertNativeInputReceipt({nativeInput:{observationId:'one',status:'guarded'}},request,'dispatched'),/mismatched/);
});

test('compact observations require successful AX evidence and bounded verification', () => {
  const before={capture:'verified',capture_scope:'window',observation:{id:'compact',windowId:4,kind:'ax-verification'},
    meta:{frontmost:{pid:123}},crop:{x:10,y:20,w:400,h:300},nativeCapabilities:['ax-verify-v1'],nativeInput:{verification:{ok:true}}};
  const click={x:30,y:40,count:1,settleMs:0,verify:{x:50,y:60,role:'AXStaticText',attribute:'AXValue',equals:'Count: 3',timeoutMs:250}};
  assert.equal(nativeInputRequest(before,'compact',click).verify.equals,'Count: 3');
  assert.throws(()=>nativeInputRequest({...before,nativeInput:{verification:{ok:false}}},'compact',click),/changed/);
  assert.throws(()=>nativeInputRequest({...before,observation:{...before.observation,kind:'pixels'}},'compact',click),/changed/);
  assert.throws(()=>nativeInputRequest(before,'old',click),/superseded/);
  for (const bad of [{attribute:'AXChildren'},{timeoutMs:Infinity},{equals:''},{equals:'x'.repeat(513)},{role:'button'}]) {
    assert.throws(()=>nativeInputRequest(before,'compact',{...click,verify:{...click.verify,...bad}}),/Invalid or unsupported/);
  }
});

test('custom native hold accepts fractions and rejects unsupported or invalid values', () => {
  const before={capture:'ok',capture_scope:'window',observation:{id:'one',windowId:4},meta:{frontmost:{pid:123}},crop:{x:10,y:20,w:400,h:300},nativeCapabilities:['click-hold-v1']};
  const click={x:30,y:40,count:1,settleMs:0,holdMs:2.5};
  const request=nativeInputRequest(before,'one',click);
  assert.equal(request.holdMs,2.5);
  assertNativeInputReceipt({nativeInput:{observationId:'one',status:'dispatched',holdMs:2.5}},request,'dispatched');
  assert.throws(()=>assertNativeInputReceipt({nativeInput:{observationId:'one',status:'dispatched',holdMs:40}},request,'dispatched'),/unknown.*not retried/);
  for (const holdMs of [-1,NaN,Infinity,1001,'2.5']) assert.throws(()=>nativeInputRequest(before,'one',{...click,holdMs}),/holdMs/);
  assert.throws(()=>nativeInputRequest({...before,nativeCapabilities:[]},'one',click),/unsupported/);
});


test('native drag validates its path and requires a release receipt', () => {
  const before={capture:'ok',capture_scope:'window',observation:{id:'one',windowId:4},meta:{frontmost:{pid:123}},crop:{x:10,y:20,w:400,h:300},nativeCapabilities:['click-hold-v1','guarded-drag-v1']};
  const click={x:30,y:40,count:1,settleMs:0,holdMs:0,drag:{x:60,y:70,durationMs:0}};
  const request=nativeInputRequest(before,'one',click);
  const receipt={observationId:'one',status:'dispatched',kind:'drag',holdMs:0,durationMs:0,releasePosted:true};
  assertNativeInputReceipt({nativeInput:receipt},request,'dispatched');
  assert.throws(()=>assertNativeInputReceipt({nativeInput:{...receipt,releasePosted:false}},request,'dispatched'),/unknown/);
  assert.throws(()=>nativeInputRequest(before,'one',{...click,count:2}),/drag/);
  assert.throws(()=>nativeInputRequest(before,'one',{...click,drag:{...click.drag,x:NaN}}),/drag/);
  assert.throws(()=>nativeInputRequest(before,'one',{...click,drag:{...click.drag,durationMs:-1}}),/drag/);
  assert.throws(()=>nativeInputRequest(before,'one',{...click,drag:{...click.drag,releaseMs:NaN}}),/drag/);
  const dwell=nativeInputRequest(before,'one',{...click,drag:{...click.drag,releaseMs:8}});
  assert.throws(()=>assertNativeInputReceipt({nativeInput:{...receipt,releaseMs:0}},dwell,'dispatched'),/unknown/);
  assertNativeInputReceipt({nativeInput:{...receipt,releaseMs:8}},dwell,'dispatched');
  assert.throws(()=>nativeInputRequest({...before,nativeCapabilities:['click-hold-v1']},'one',click),/unsupported native drag/);
});
