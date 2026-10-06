import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtempSync,mkdirSync,copyFileSync,readFileSync,writeFileSync,rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {resolve,dirname} from 'node:path';
import {runtimeManifest,assetFile,stageRuntime,verifyRuntime} from '../oskiewar-manifest.mjs';
import {defaultCharacterEntry,readRelease} from '../../live/release-client.mjs';
import {newRelease} from '../oskiewar-release.mjs';
test('shell dependencies are packaged and a module-only change invalidates the release',()=>{
 const original=runtimeManifest(),root=mkdtempSync(resolve(tmpdir(),'oskiewar-release-'));
 try {
  for(const url of Object.keys(original.files)){const path=assetFile(url,root);mkdirSync(dirname(path),{recursive:true});copyFileSync(assetFile(url),path);}
  assert.equal(runtimeManifest(root).release,original.release);
  const file=assetFile('/oskiewar-wizard.mjs',root);writeFileSync(file,readFileSync(file,'utf8')+'\n// module-only update\n');
  const changed=runtimeManifest(root);assert.equal(changed.game,original.game);assert.notEqual(changed.release,original.release);
  const receipt=newRelease(changed.game,'next','live',{desired:{runtimeHash:original.release},channels:{web:{hash:original.game}}},changed.release);
  assert.equal(receipt.channels.web.status,'pending');
  const out=resolve(root,'stage');mkdirSync(out);stageRuntime(out,root);
  for(const [url,entry] of Object.entries(changed.files))assert.equal(readFileSync(resolve(out,'.'+url)).length,entry.bytes);
  verifyRuntime(out,changed);
  writeFileSync(resolve(out,'account.mjs'),'stale');
  assert.throws(()=>verifyRuntime(out,changed),/Bundle asset differs/);
  for(const url of ['/account.mjs','/oskiewar-wizard.mjs','/oskiewar-fighter.mjs','/release-client.mjs','/render-quality.mjs','/photo-theme.mjs','/aesthetic.computer/lib/auth0-otp.mjs'])assert.ok(changed.files[url],url);
 } finally {rmSync(root,{recursive:true,force:true});}
});
test('touch and presentation options keep the same default; explicit game routes do not',()=>{
 for(const path of ['/','/?touch','/?touch&renderer=canvas','/3d','/?touch&app=ios'])assert.equal(defaultCharacterEntry(new URL('https://oskiewar.com'+path)),true,path);
 for(const path of ['/?practice','/?park=desert1','/?replay=abc','/classic','/room123','/workshop','/#bookmark'])assert.equal(defaultCharacterEntry(new URL('https://oskiewar.com'+path)),false,path);
});
test('release checks tolerate offline hosts and reject malformed manifests',async()=>{
 const manifest=runtimeManifest();assert.deepEqual(await readRelease(async()=>Response.json(manifest)),manifest);
 assert.equal(await readRelease(async()=>Response.json({release:'fake'})),null);
 assert.equal(await readRelease(async()=>{throw Error('offline');}),null);
});
