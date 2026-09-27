import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, writeFile, readFile, readdir, rm, chmod } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import { request, handleMessage } from '../bin/aesel-mcp.mjs';
import { check, buildFingerprint, CHECKS } from '../bin/aesel-eye.mjs';

test('MCP discovery and browser-origin rejection do not require a running app', async () => {
  const reply=await handleMessage({id:1,method:'tools/list'});
  assert(reply.result.tools.some(tool=>tool.name==='aesel_capture'));
  assert.equal(await handleMessage({method:'notifications/initialized'}),null);
  assert((await handleMessage({id:2,method:'tools/list'},{headers:{origin:'https://untrusted.example'}})).error);
});

test('mailbox correlates responses, expires unanswered requests, and requires a private directory', async () => {
  const dir=await mkdtemp(join(tmpdir(),'aesel-rpc-'));
  await chmod(dir,0o700);await mkdir(join(dir,'requests'));await mkdir(join(dir,'responses'));
  await writeFile(join(dir,'instance.json'),JSON.stringify({schema:1,pid:process.pid,instance:'fixture'}));
  try {
    const pending=request('state',{}, {directory:dir,timeoutMs:1000});
    let file;
    for(let i=0;i<20&&!file;i++){file=(await readdir(join(dir,'requests'))).find(name=>name.endsWith('.json'));if(!file)await new Promise(r=>setTimeout(r,10));}
    const input=JSON.parse(await readFile(join(dir,'requests',file),'utf8'));
    assert.equal(input.instance,'fixture');assert.equal(input.method,'state');
    await writeFile(join(dir,'responses',file),JSON.stringify({id:input.id,result:{footer:'v0'}}));
    assert.deepEqual(await pending,{footer:'v0'});
    assert.deepEqual(await readdir(join(dir,'requests')),[]);
    await assert.rejects(request('state',{}, {directory:dir,timeoutMs:10}),/timed out/);
    assert.deepEqual(await readdir(join(dir,'requests')),[]);
    await chmod(dir,0o755);
    await assert.rejects(request('state',{}, {directory:dir}),/private/);
  } finally {await rm(dir,{recursive:true,force:true});}
});

test('Aesthetic Eye fails absent scenarios, unreviewed images, changed evidence, and stale builds', async () => {
  const dir=await mkdtemp(join(tmpdir(),'aesel-eye-'));
  const bundle=join(dir,'Fixture.app');await mkdir(bundle);await writeFile(join(bundle,'code'),'build1');
  const bytes=Buffer.from('fixture');
  const fingerprint=await buildFingerprint(bundle);
  const scenarios=[];
  for(const name of ['notebook','settings','preview-expanded','notebook-narrow']){
    const files=[];
    for(const suffix of ['app.png','state.json']){const path=`${name}-${suffix}`;const content=suffix==='state.json'?Buffer.from(JSON.stringify({buildSha256:fingerprint,preview:{visible:false}})):bytes;await writeFile(join(dir,path),content);files.push({path,sha256:createHash('sha256').update(content).digest('hex')});}
    scenarios.push({name,files,design:'pass',checks:Object.fromEntries(CHECKS.map(key=>[key,'pass'])),notes:'Test fixture; never product acceptance'});
  }
  const manifest={schema:1,kind:'aesel-ui',bundle,buildSha256:await buildFingerprint(bundle),visualInference:true,reviewer:{name:'test fixture',kind:'visual-inference'},reviewedAt:new Date().toISOString(),scenarios};
  const save=()=>writeFile(join(dir,'aesthetic-eye.json'),JSON.stringify(manifest));
  try{
    await save();assert.equal((await check(dir)).pass,true);
    const stateFile=scenarios[0].files.find(f=>f.path.endsWith('-state.json'));
    const original=await readFile(join(dir,stateFile.path));
    const changed=Buffer.from(JSON.stringify({buildSha256:fingerprint,preview:{visible:true}}));
    await writeFile(join(dir,stateFile.path),changed);stateFile.sha256=createHash('sha256').update(changed).digest('hex');await save();
    assert((await check(dir)).errors.some(e=>e.includes('visible preview evidence missing')));
    await writeFile(join(dir,stateFile.path),original);stateFile.sha256=createHash('sha256').update(original).digest('hex');await save();
    manifest.visualInference=false;await save();assert.equal((await check(dir)).pass,false);
    manifest.visualInference=true;const last=manifest.scenarios.pop();await save();assert((await check(dir)).errors.some(e=>e.includes('Required scenario')));
    manifest.scenarios.push(last);await save();await writeFile(join(dir,'notebook-app.png'),'changed');assert((await check(dir)).errors.some(e=>e.includes('evidence changed')));
    await writeFile(join(bundle,'code'),'build2');assert((await check(dir)).errors.some(e=>e.includes('Stale build')));
  }finally{await rm(dir,{recursive:true,force:true});}
});
