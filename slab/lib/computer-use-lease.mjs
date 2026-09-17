// Cross-process coordination for controllers running on this host. Never steal
// an abandoned lease: a timed-out/terminated owner may have delivered input.
import { mkdirSync, writeFileSync, rmSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";
import { createHash, randomUUID } from "node:crypto";
import { AsyncLocalStorage } from "node:async_hooks";
const held = new AsyncLocalStorage();
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

export function machineLeaseKey(spec) {
  if (spec?.local) return "local";
  const host = spec?.sshHost || spec?.ssh;
  if (!host) throw new Error("Native coordination needs an explicit machine transport");
  return `remote:${host.split(/\s+/)[0].replace(/^[^@]+@/, "").toLowerCase()}`;
}
function tryAcquire(key, root) {
  mkdirSync(root, { recursive: true, mode: 0o700 });
  const path = join(root, createHash("sha256").update(key).digest("hex").slice(0,32));
  try { mkdirSync(path, { mode: 0o700 }); }
  catch (error) { if (error.code === "EEXIST") return null; throw error; }
  try { writeFileSync(join(path,"owner.json"), JSON.stringify({pid:process.pid,token:randomUUID(),since:new Date().toISOString()}), {mode:0o600}); }
  catch(error) { rmSync(path,{recursive:true,force:true});throw error; }
  return () => rmSync(path,{recursive:true,force:true});
}
function options(opts) {
  return {timeoutMs:1000,root:process.env.SLAB_INPUT_LEASE_DIR || join(homedir(),".local/share/slab/input-leases"),...opts};
}
export async function withMachineLease(spec, fn, opts = {}) {
  const key=machineLeaseKey(spec);
  if (held.getStore()?.has(key)) return fn();
  const {timeoutMs,root}=options(opts), deadline=Date.now()+timeoutMs;
  let release;
  while (!(release=tryAcquire(key,root))) {
    if (Date.now()>=deadline) throw new Error(`Computer-use machine busy; no action sent. Inspect input leases in ${root} before clearing an abandoned owner.`);
    await delay(25);
  }
  const active=new Set(held.getStore() || []);active.add(key);
  try { return await held.run(active,fn); } finally { release(); }
}
export function withMachineLeaseSync(spec, fn, opts = {}) {
  const key=machineLeaseKey(spec);
  if (held.getStore()?.has(key)) return fn();
  const {timeoutMs,root}=options(opts),deadline=Date.now()+timeoutMs;
  let release;
  while (!(release=tryAcquire(key,root))) {
    if (Date.now()>=deadline) throw new Error(`Computer-use machine busy; no action sent. Inspect input leases in ${root}.`);
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)),0,0,25);
  }
  const active=new Set(held.getStore() || []);active.add(key);
  try { return held.run(active,fn); } finally { release(); }
}

export function inspectMachineLeases(opts = {}) {
  const {root}=options(opts);
  let names;
  try {names=readdirSync(root);} catch(error) {if(error.code==="ENOENT") return [];throw error;}
  return names.filter(name=>/^[a-f0-9]{32}$/.test(name)).map(id=>{
    try {
      const owner=JSON.parse(readFileSync(join(root,id,"owner.json"),"utf8"));
      let alive=true;
      try {process.kill(owner.pid,0);} catch(error) {alive=error.code!=="ESRCH";}
      return {id,pid:owner.pid,since:owner.since,ownerAlive:alive};
    } catch {return {id,ownerAlive:null};}
  });
}
