import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, rm, rename, copyFile, writeFile, symlink, realpath } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createQuest, scanQuest } from './quest.mjs';
import { dragPointCore } from '../bin/macos.mjs';
import { sortQuest } from './sort.mjs';

async function fixture(t) {
  const parent=await realpath(await mkdtemp(join(tmpdir(),'finder-quest-test-')));
  t.after(()=>rm(parent,{recursive:true,force:true}));
  const downloads=join(parent,'Downloads');await mkdir(downloads);
  const layout=['Loose','More stuff','','Loose','More stuff','','Loose','More stuff',''];
  const quest=await createQuest(parent,{layout});
  for(const f of quest.files.filter(f=>f.download))await writeFile(join(downloads,f.name),f.content);
  return {parent,downloads,quest,layout};
}

test('replay layout and Terminal batch preserve all twelve assets and leave downloads empty',async t=>{
  const {downloads,quest,layout}=await fixture(t);
  assert.deepEqual((await scanQuest(quest,downloads)).layout,layout);
  assert.equal((await sortQuest(await scanQuest(quest,downloads),downloads)).length,12);
  const state=await scanQuest(quest,downloads);
  assert.equal(state.sorted,12);assert.equal(state.returned,0,'shell sorting does not simulate browser returns');
  assert.equal(state.files.some(f=>f.inDownloads),false);
  assert.deepEqual(await sortQuest(state,downloads),[],'verified sorting is idempotent');
  await assert.rejects(createQuest(quest.root,{layout:['../escape']}),/Replay layout/);
});

test('Terminal preflight refuses duplicates, changed bytes, destinations, and symlinks before moving',async t=>{
  const {downloads,quest}=await fixture(t);
  const file=quest.files[0],source=join(quest.root,file.initial),dest=join(quest.root,file.category,file.name);
  const state=await scanQuest(quest,downloads);
  await copyFile(source,dest);
  await assert.rejects(sortQuest(state,downloads),/Destination already exists/);
  await assert.rejects(sortQuest(await scanQuest(quest,downloads),downloads),/duplicate/);
  await rm(dest);await writeFile(source,'changed');
  await assert.rejects(sortQuest(state,downloads),/contents/);
  await rm(source);await writeFile(dest,file.content);await symlink(dest,source);
  await assert.rejects(sortQuest(state,downloads),/Symlink source/);
  await rm(source);await rm(dest);await writeFile(source,file.content);
  const download=quest.files.find(f=>f.download);
  await writeFile(join(downloads,download.name),'impostor');
  await assert.rejects(sortQuest(await scanQuest(quest,downloads),downloads),/contents/);
  assert.equal((await scanQuest(quest,downloads)).sorted,0,'all preflight checks happen before any move');
});

test('score verifies exact locations, duplicate copies, contents, downloads, and returns', async t => {
  const parent=await mkdtemp(join(tmpdir(),'finder-quest-test-'));
  t.after(()=>rm(parent,{recursive:true,force:true}));
  const downloads=join(parent,'Downloads');await mkdir(downloads);
  const quest=await createQuest(parent);
  assert.equal((await scanQuest(quest,downloads)).sorted,0);
  for(const f of quest.files.filter(f=>!f.download)) await rename(join(quest.root,f.initial),join(quest.root,f.category,f.name));
  assert.equal((await scanQuest(quest,downloads)).sorted,9);
  const sample=quest.files[0], path=join(quest.root,sample.category,sample.name);
  await copyFile(path,join(quest.root,sample.name));
  assert.equal((await scanQuest(quest,downloads)).sorted,8,'duplicate is not a successful move');
  await rm(join(quest.root,sample.name));
  await writeFile(path,'not the original');
  assert.equal((await scanQuest(quest,downloads)).sorted,8,'renamed impostor must not pass');
  await writeFile(path,sample.content);
  for(const f of quest.files.filter(f=>f.download)) {
    const source=join(downloads,f.name),dest=join(quest.root,f.category,f.name);
    await writeFile(source,f.content); await copyFile(source,dest);
    assert.equal((await scanQuest(quest,downloads)).files.find(x=>x.id===f.id).sorted,false);
    await rm(source); quest.uploaded.add(f.id);
  }
  quest.startedAt=Date.now();
  assert.equal((await scanQuest(quest,downloads)).complete,true);
  const extra=quest.files.find(f=>f.download), dot=extra.name.lastIndexOf('.');
  const duplicate=join(downloads,extra.name.slice(0,dot)+' (1)'+extra.name.slice(dot));
  await writeFile(duplicate,extra.content);
  assert.equal((await scanQuest(quest,downloads)).complete,false,'browser-suffixed copies in Downloads count too');
  await rm(duplicate);
  await rename(path,join(quest.root,sample.name));
  assert.equal((await scanQuest(quest,downloads)).complete,false,'completion reflects current state');
  await symlink(parent,join(quest.root,'outside-link'));
  assert.equal((await scanQuest(quest,downloads)).sorted,11,'symlink must not escape or recurse');
});

test('native drag validates endpoints and releases the mouse in a finally block',()=>{
  let script;
  dragPointCore({local:true},[10,20],[300,400],{run:(_spec,_cmd,{stdin})=>{script=stdin;}});
  assert.match(script,/kCGEventLeftMouseDragged/);assert.match(script,/finally\s*\{[\s\S]*kCGEventLeftMouseUp/);
  assert.throws(()=>dragPointCore({local:true},[NaN,2],[3,4]),/finite/);
  assert.throws(()=>dragPointCore({local:true},[1,2],[3,4],{durationMs:1}),/duration/);
});
