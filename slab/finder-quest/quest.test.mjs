import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, rm, rename, copyFile, writeFile, symlink } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createQuest, scanQuest } from './quest.mjs';
import { dragPointCore } from '../bin/macos.mjs';

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
