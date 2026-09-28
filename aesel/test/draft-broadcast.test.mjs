import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, writeFile, readFile, readdir, stat, rm, symlink } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import { DraftBroadcast } from '../src/draft-broadcast.mjs';
const artifactId = '11111111-1111-4111-8111-111111111111';
const identity = {artifactId, kind:'paper'};
const session = {handle:'tester',read:()=>({user:{sub:'auth0|tester'}}),token:async()=>'private-token'};
const digest = b => createHash('sha256').update(b).digest('hex');
async function fixture(t) {
 const cwd = await mkdtemp(join(tmpdir(),'draft-broadcast-'));
 t.after(()=>rm(cwd,{recursive:true,force:true}));
 async function preview(version, content=`draft ${version}`, mime='text/plain') {
  const dir=join(cwd,'.easel-media/artifacts',artifactId,`v${version}`); await mkdir(dir,{recursive:true});
  const data=Buffer.from(content), path=join(dir,'draft.txt'); await writeFile(path,data);
  await writeFile(join(dir,'revision.json'),JSON.stringify({version,files:['draft.txt'],hashes:{'draft.txt':digest(data)},preview:{path:'draft.txt',mime}}));
  return {path,mime,version,artifactId};
 }
 const calls=[], records=new Map();
 const fetch=async(url, options)=>{
  const body=JSON.parse(options.body); calls.push({body,method:options.method,at:Date.now(),headers:options.headers});
  const old=records.get(body.id);
  if(old && old.sequence>=body.sequence) return Response.json({error:'Stale sequence',sequence:old.sequence},{status:409});
  const meta={...body,status:options.method==='DELETE'?'stopped':'live',route:`https://aesthetic.computer/watch/?id=${body.id}`,expiresAt:new Date(Date.now()+3600000).toISOString()};
  delete meta.data; records.set(body.id,meta); return Response.json(meta);
 };
 const client=new DraftBroadcast({cwd,session,fetch}); t.after(()=>client.close());
 return {cwd,preview,calls,records,fetch,client};
}
test('explicit start, minimal payload, private stable record; signed-out reads stay local',async t=>{
 const f=await fixture(t), p=await f.preview(1);
 assert.equal((await f.client.current(identity)).active,false);
 await assert.rejects(readdir(join(f.cwd,'.easel')),/ENOENT/);
 const state=await f.client.start(p,identity); assert.equal(state.active,true); assert.equal(state.artifactId,artifactId);
 assert.match(state.route,/\/watch\/\?id=[a-f0-9]{32}$/);
 assert.deepEqual(Object.keys(f.calls[0].body).sort(),['data','id','kind','mime','sequence','status','version']);
 const files=await readdir(join(f.cwd,'.easel/broadcasts')), file=join(f.cwd,'.easel/broadcasts',files[0]);
 const record=await readFile(file,'utf8'); assert.equal(record.includes('private-token'),false); assert.equal(record.includes('draft 1'),false); assert.equal((await stat(file)).mode&0o777,0o600);
 const loggedOut=new DraftBroadcast({cwd:f.cwd,session:{read:()=>null},fetch:()=>assert.fail('network')});
 assert.equal((await loggedOut.current(identity)).active,false); loggedOut.close();
});
test('dedupes bytes and coalesces 30 incremental previews to latest serial frame',async t=>{
 const f=await fixture(t); await f.client.start(await f.preview(1),identity);
 await f.client.update(await f.preview(2,'draft 1'),identity); assert.equal(f.calls.length,1);
 const previews=await Promise.all(Array.from({length:30},(_,i)=>f.preview(i+3)));
 await Promise.all(previews.map(p=>f.client.update(p,identity)));
 assert.equal(f.calls.at(-1).body.version,32); assert.equal(f.calls.at(-1).body.data,Buffer.from('draft 32').toString('base64'));
 assert.ok(f.calls.length<=3); assert.ok(f.calls[1].at-f.calls[0].at>=480);
});
test('restart does not authorize updates; saved ID and sequence revoke and resume',async t=>{
 const f=await fixture(t),p=await f.preview(1),first=await f.client.start(p,identity); f.client.close();
 const next=new DraftBroadcast({cwd:f.cwd,session,fetch:f.fetch}); t.after(()=>next.close());
 const persisted=await next.current(identity); assert.equal(persisted.enabled,true); assert.equal(persisted.active,false); assert.equal(persisted.route,null);
 await assert.rejects(next.update(p,identity),/not connected/);
 const stopped=await next.stop(identity); assert.equal(stopped.id,first.id); assert.equal(stopped.enabled,false); assert.ok(stopped.sequence>first.sequence); assert.equal(f.calls.at(-1).method,'DELETE');
 const resumed=await next.start(p,identity); assert.equal(resumed.id,first.id); assert.equal(resumed.active,true); assert.ok(resumed.sequence>stopped.sequence);
});
test('409 advances durable sequence before retry',async t=>{
 const f=await fixture(t),p=await f.preview(1); let first=true;
 f.client.fetch=async(url,opt)=>{ const b=JSON.parse(opt.body); const [name]=await readdir(join(f.cwd,'.easel/broadcasts')); const stored=JSON.parse(await readFile(join(f.cwd,'.easel/broadcasts',name))); assert.equal(stored.sequence,b.sequence); if(first){first=false; return Response.json({sequence:90},{status:409});} return f.fetch(url,opt); };
 const result=await f.client.start(p,identity); assert.equal(result.sequence,91);
});
test('hash mismatch, escaping paths, unsupported MIME, source-ahead and oversized files never upload',async t=>{
 const f=await fixture(t),p=await f.preview(1); await writeFile(p.path,'changed'); await assert.rejects(f.client.start(p,identity),/hash/);
 await assert.rejects(f.client.start({...p,path:'/etc/hosts'},identity),/leaves/);
 await assert.rejects(f.client.start({...p,mime:'text/html'},identity),/supported/);
 const big=await f.preview(2,Buffer.alloc(8*1024*1024+1)); await assert.rejects(f.client.start(big,identity),/regular file/);
 const p3=await f.preview(3), revision=join(p3.path,'../revision.json'); const r=JSON.parse(await readFile(revision)); r.sourceAhead=true; await writeFile(revision,JSON.stringify(r)); await assert.rejects(f.client.start(p3,identity),/Build/);
 assert.equal(f.calls.length,0);
});
test('transport failure clears active QR; explicit retry succeeds',async t=>{
 const f=await fixture(t), states=[]; f.client.onState=s=>states.push(s);
 await f.client.start(await f.preview(1),identity); const p=await f.preview(2);
 f.client.fetch=async()=>Response.json({error:'failed'},{status:503});
 await assert.rejects(f.client.update(p,identity),/503/); assert.equal(states.at(-1).active,false); assert.equal(states.at(-1).route,null); assert.match(states.at(-1).error,/503/);
 f.client.fetch=f.fetch; assert.equal((await f.client.start(p,identity)).active,true);
});
test('timeouts abort transport; close cancels long byte-budget waits',async t=>{
 const f=await fixture(t); f.client.timeoutMs=20;
 f.client.fetch=async(url,{signal})=>new Promise((resolve,reject)=>signal.addEventListener('abort',()=>reject(signal.reason),{once:true}));
 await assert.rejects(f.client.start(await f.preview(1),identity),/timed out/);
 f.client.fetch=f.fetch; f.client.nextUpload=0;
 await f.client.start(await f.preview(2,Buffer.alloc(1024*1024,65)),identity);
 const pending=f.client.update(await f.preview(3),identity); const rejected=assert.rejects(pending,/closed/);
 await new Promise(r=>setTimeout(r,20)); f.client.close(); await rejected; assert.equal(f.client.timers.size,0);
});
test('account changes discard late success and suspension requires fresh consent',async t=>{
 const f=await fixture(t),p=await f.preview(1); let owner='auth0|tester', entered, release;
 const started=new Promise(r=>entered=r), held=new Promise(r=>release=r);
 f.client.session={...session,read:()=>({user:{sub:owner}})};
 f.client.fetch=async(url,opt)=>{entered(); await held; return f.fetch(url,opt);};
 const pending=f.client.start(p,identity), rejected=assert.rejects(pending,/account changed/);
 await started; owner='auth0|other'; release(); await rejected;
 assert.equal((await f.client.current(identity)).active,false);
 owner='auth0|tester'; f.client.suspend(); await assert.rejects(f.client.update(p,identity),/not connected/);
});
test('stop aborts in-flight upload and revokes with a newer sequence',async t=>{
 const f=await fixture(t),p=await f.preview(1); let entered;
 const started=new Promise(r=>entered=r);
 f.client.fetch=async(url,opt)=>{
  if(opt.method==='DELETE')return f.fetch(url,opt);
  entered(); return new Promise((resolve,reject)=>opt.signal.addEventListener('abort',()=>reject(opt.signal.reason),{once:true}));
 };
 const pending=f.client.start(p,identity), rejected=assert.rejects(pending,/stopped/);
 await started; const stopped=await f.client.stop(identity); await rejected;
 assert.equal(stopped.active,false); assert.equal(stopped.enabled,false); assert.equal(stopped.sequence,2); assert.equal(f.calls[0].method,'DELETE');
});
test('stop during initial preview validation prevents a late start from uploading',async t=>{
 const f=await fixture(t),p=await f.preview(1); let entered,release;
 const started=new Promise(r=>entered=r), held=new Promise(r=>release=r), frame=f.client.frame.bind(f.client);
 f.client.frame=async(...args)=>{entered();await held;return frame(...args);};
 const pending=f.client.start(p,identity), rejected=assert.rejects(pending,/stopped/);
 await started; await f.client.stop(identity); release(); await rejected; assert.equal(f.calls.length,0);
});
test('Game Boy source-ahead broadcasts only verified main.c instead of stale ROM',async t=>{
 const f=await fixture(t), gameboy={artifactId,kind:'gameboy'};
 async function sourcePreview(version,source,declared=true,hashed=true){
  const p=await f.preview(version,'old ROM must not be uploaded','application/x-gameboy-rom');
  const dir=join(p.path,'..'), file=join(dir,'revision.json'), revision=JSON.parse(await readFile(file));
  await writeFile(join(dir,'main.c'),source);
  if(declared)revision.files.push('main.c');
  if(hashed)revision.hashes['main.c']=digest(Buffer.from(source));
  revision.sourceAhead=true; await writeFile(file,JSON.stringify(revision)); return p;
 }
 const first=await sourcePreview(1,'void main(void) { /* draft */ }');
 await f.client.start(first,gameboy);
 assert.equal(f.calls[0].body.mime,'text/plain');
 assert.equal(Buffer.from(f.calls[0].body.data,'base64').toString(),'void main(void) { /* draft */ }');
 await f.client.update(await sourcePreview(2,'void main(void) { /* draft */ }'),gameboy);
 assert.equal(f.calls.length,1);
 await f.client.update(await sourcePreview(3,'void main(void) { /* updated */ }'),gameboy);
 assert.equal(f.calls.length,2); assert.match(Buffer.from(f.calls[1].body.data,'base64').toString(),/updated/);
 await assert.rejects(f.client.update(await sourcePreview(4,'secret',false),gameboy),/declared/);
 await assert.rejects(f.client.update(await sourcePreview(5,'secret',true,false),gameboy),/hashed/);
 const tampered=await sourcePreview(6,'original'); await writeFile(join(tampered.path,'../main.c'),'tampered');
 await assert.rejects(f.client.update(tampered,gameboy),/source hash mismatch/);
 assert.equal(f.calls.length,2);
});

test('automatic artifacts reserve stable QR before upload, reconnect after restart and renew idle frames',async t=>{
 const f=await fixture(t),p=await f.preview(1);
 const reserved=await f.client.reserve(identity);assert.match(reserved.scanUrl,/watch\/\?id=/);assert.equal(f.calls.length,0);
 const first=await f.client.automatic(p,{kind:identity.kind});assert.equal(first.id,reserved.id);assert.equal(first.active,true);
 const channel=await f.client.channel(identity);channel.metadata.expiresAt=new Date(Date.now()+1000).toISOString();
 await f.client.automatic(p,{kind:identity.kind});assert.equal(f.calls.length,2);
 await f.client.stop(identity);f.client.close();
 const next=new DraftBroadcast({cwd:f.cwd,session,fetch:f.fetch});t.after(()=>next.close());
 assert.equal((await next.reserve(identity)).id,first.id);
 const resumed=await next.automatic(p,identity);assert.equal(resumed.active,true);assert.equal(resumed.id,first.id);
});
