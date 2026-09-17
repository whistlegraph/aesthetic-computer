import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { assertFrameTarget } from "../lib/frame-target.mjs";
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
