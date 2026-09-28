import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,writeFile,readFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {ACSession} from '../src/ac-session.mjs';
import {requireAccountEntry} from '../src/account-entry.mjs';

async function fixture(t, fetch) {
  const root=await mkdtemp(join(tmpdir(),'aesel-account-'));
  t.after(()=>rm(root,{recursive:true,force:true}));
  const file=join(root,'token');
  await writeFile(file,JSON.stringify({access_token:'token',user:{sub:'old',handle:'cached'}}));
  return new ACSession({file,fetch});
}
test('TUI access verifies the token and current server handle, not a cached name',async t=>{
  const session=await fixture(t,async url=>Response.json(url.includes('/userinfo')?{sub:'account'}:{handle:'current'}));
  assert.deepEqual(await session.requireAccount(),{sub:'account',handle:'current'});
  assert.equal(session.handle,'current');
});
test('TUI access rejects missing handles, expired login and offline verification',async t=>{
  for(const mode of ['no-handle','expired','offline']){
    const session=await fixture(t,async url=>{
      if(mode==='offline')throw Error('offline');
      if(mode==='expired')return new Response('',{status:401});
      return url.includes('/userinfo')?Response.json({sub:'account'}):new Response('',{status:404});
    });
    await assert.rejects(session.requireAccount(),mode==='no-handle'?/handle/:mode==='expired'?/401/:/offline/);
    if(mode==='no-handle')assert.equal(session.handle,'');
  }
});
test('onboarding does not continue after sign-in without a handle',async()=>{
  let signedIn=false,handle='',checks=0; const prompts=[];
  const session={
    async requireAccount(){checks++;if(!signedIn)throw Error('Sign in');if(!handle)throw Error('Claim a handle');return {handle};},
    async login(){signedIn=true;},async claimHandle(name){handle=name;}
  };
  const answers=['/login','/handle test'];
  assert.equal(await requireAccountEntry(session,{output:{write(){}},question:async p=>{prompts.push(p);return answers.shift();}}),true);
  assert.equal(checks,3);assert.equal(handle,'test');assert.equal(prompts.length,2);
});
test('account changes during verification cannot unlock a different session',async t=>{
  let release;
  const session=await fixture(t,async url=>url.includes('/userinfo')?new Promise(r=>release=r):Response.json({handle:'test'}));
  const checking=session.requireAccount();
  await new Promise(r=>setImmediate(r));
  await writeFile(session.file,JSON.stringify({access_token:'different',user:{handle:'different'}}));
  release(Response.json({sub:'old-account'}));
  await assert.rejects(checking,/changed/);
  assert.equal(JSON.parse(await readFile(session.file,'utf8')).access_token,'different');
});
