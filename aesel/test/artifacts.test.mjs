import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile, writeFile, mkdir, rm, symlink, rename } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { randomUUID } from 'node:crypto';
import { Artifacts } from '../src/artifacts.mjs';
async function fixture(t) {
  const cwd = await mkdtemp(join(tmpdir(), 'easel-artifacts-'));
  t.after(() => rm(cwd, { recursive: true, force: true }));
  return { cwd, store: new Artifacts(cwd) };
}
async function contents(selected) {
  return Object.fromEntries(await Promise.all(selected.revision.files.map(async name => [name, await readFile(join(selected.root, name))])));
}
test('project creates each medium and preserves artifacts across selection and restart', async t => {
  const {cwd,store}=await fixture(t);
  assert.equal(await store.selected(), null);
  const sound=await store.create('sound','My phrase');
  const picture=await store.create('picture','My picture');
  const paper=await store.create('paper','My paper');
  assert.equal((await store.selected()).kind,'paper');
  assert.equal((await store.read()).artifacts.length,3);
  await store.select(sound.artifactId);
  assert.equal((await new Artifacts(cwd).selected()).name,'My phrase');
  await store.select(picture.artifactId);
  assert.equal((await store.selected()).revision.preview.mime,'image/png');
  await store.select(paper.artifactId);
  assert.equal((await store.selected()).kind,'paper');
  await store.select('piece');
  assert.equal(await store.selected(),null);
  assert.equal((await store.read()).artifacts.length,3);
});
test('sound rollback appends a revision restoring every saved byte without regeneration',async t=>{
  const{store}=await fixture(t);await store.create('sound');
  const first=await store.selected(), saved=await contents(first);
  await store.run('rhythm',{hits:3,pulses:8,midi:48});
  assert.notDeepEqual((await contents(await store.selected()))['sound.wav'],saved['sound.wav']);
  const restored=await store.rollback(1);
  assert.equal(restored.version,3);
  const current=await store.selected();
  assert.equal(current.revision.restoredFrom,1);
  assert.deepEqual(await contents(current),saved);
  assert.deepEqual(await contents(first),saved);
  assert.deepEqual((await store.versions()).map(v=>v.version),[1,2,3]);
});
test('picture proposals, acceptance and rollback retain separate preview and accepted image',async t=>{
  const{store}=await fixture(t);await store.create('picture');
  const blank=await contents(await store.selected());
  await store.run('propose',{seed:'integration',color:[255,0,0,255],points:[{x:10,y:10},{x:300,y:300}],thickness:12});
  const proposal=await contents(await store.selected());
  assert.deepEqual(proposal['composite.png'],blank['composite.png']);
  assert.notDeepEqual(proposal['preview.png'],blank['preview.png']);
  await store.run('accept');
  assert.deepEqual((await contents(await store.selected()))['composite.png'],proposal['preview.png']);
  await store.rollback(2);
  assert.deepEqual(await contents(await store.selected()),proposal);
});
test('failed actions and unauthorized paid generation preserve selected revision',async t=>{
  const{store}=await fixture(t);await store.create('sound');
  const before=await store.read(),saved=await contents(await store.selected());
  await assert.rejects(store.run('set_score',{score:{beats:64,bpm:30,notes:[]}}));
  await assert.rejects(store.run('made_up'));
  await assert.rejects(store.run('generate',{authorized:true}));
  await assert.rejects(store.select('../unknown'));
  assert.deepEqual(await store.read(),before);
  assert.deepEqual(await contents(await store.selected()),saved);
  assert.equal((await store.versions()).length,1);
});
test('rollback detects corruption without moving selected version',async t=>{
  const{store}=await fixture(t);await store.create('sound');
  const old=await store.selected();await store.run('rhythm',{hits:2,pulses:4});
  await writeFile(join(old.root,'sound.wav'),'corrupted');
  await assert.rejects(store.rollback(1),/hash mismatch/);
  assert.equal((await store.selected()).version,2);
});
test('project lock serializes distinct store instances and releases after errors',async t=>{
  const{cwd,store}=await fixture(t);const second=new Artifacts(cwd);
  let release,entered;const gate=new Promise(r=>{release=r;});const ready=new Promise(r=>{entered=r;});
  const running=store.locked(async()=>{entered();await gate;});
  await ready;
  try{await assert.rejects(second.create('sound'),/already running/);}finally{release();await running;}
  await assert.rejects(store.locked(async()=>{throw new Error('expected');}),/expected/);
  await second.create('sound');assert.equal((await store.selected()).version,1);
});
async function unsafeCommit(store,setup,files,preview=null) {
  return store.locked(async()=>{
    const stage=join(store.root,'jobs',randomUUID());await mkdir(stage,{recursive:true});
    await setup(stage);
    const artifact={id:randomUUID(),kind:'sound',version:0,name:'unsafe'};
    const project={format:1,selected:artifact.id,artifacts:[artifact]};
    return store.commit(project,artifact,stage,{files,preview});
  });
}
test('commit rejects path traversal, symlink output and undeclared preview',async t=>{
  const{cwd,store}=await fixture(t);const outside=join(cwd,'outside');await writeFile(outside,'private');
  await assert.rejects(unsafeCommit(store,async()=>{},['../../outside']),/Path leaves/);
  await assert.rejects(unsafeCommit(store,stage=>symlink(outside,join(stage,'audio')),['audio']),/symlink/i);
  await assert.rejects(unsafeCommit(store,stage=>writeFile(join(stage,'audio'),'x'),['audio'],{path:'missing',mime:'audio/wav'}),/Preview/);
  assert.equal((await store.read()).artifacts.length,0);
  assert.equal(await readFile(outside,'utf8'),'private');
});
test('commit reserves revision metadata through normalized filename aliases',async t=>{
  const{store}=await fixture(t);
  await assert.rejects(unsafeCommit(store,stage=>writeFile(join(stage,'revision.json'),'pretend'),['./revision.json']));
  assert.equal((await store.read()).artifacts.length,0);
});
test('unlisted staged symlinks cannot enter a saved revision',async t=>{
  const{cwd,store}=await fixture(t);const outside=join(cwd,'outside');await writeFile(outside,'private');
  await assert.rejects(unsafeCommit(store,async stage=>{await writeFile(join(stage,'audio'),'x');await symlink(outside,join(stage,'hidden'));},['audio']));
  assert.equal((await store.read()).artifacts.length,0);
});
test('jobs and artifact storage directories cannot redirect operations outside the project',async t=>{
  for(const directory of ['jobs','artifacts']){
    const{cwd,store}=await fixture(t);const outside=join(cwd,'external');await mkdir(outside);await mkdir(store.root);await symlink(outside,join(store.root,directory));
    await assert.rejects(store.create('sound'),/symlink|unsafe/i);
    assert.equal((await store.read()).artifacts.length,0);
  }
});
test('selected revisions cannot be read through a substituted directory symlink',async t=>{
  const{cwd,store}=await fixture(t);await store.create('sound');const selected=await store.selected();
  const outside=join(cwd,'external');await rename(selected.root,outside);await symlink(outside,selected.root);
  await assert.rejects(store.selected(),/symlink|unsafe/i);
});
test('tools expose AC drawing and remote images only in Picture',async t=>{
  const{store}=await fixture(t);assert.deepEqual(await store.tools(),[]);
  await store.create('sound');assert.ok((await store.tools()).some(t=>t.name==='artifact_set_score'));assert.match(await store.context(),/score.json/);
  await store.create('picture');const tools=await store.tools();assert.ok(tools.some(t=>t.name==='artifact_propose'));assert.ok(tools.some(t=>t.name==='artifact_generate'));assert.ok(tools.some(t=>t.name==='artifact_draw'));assert.ok(!tools.some(t=>t.name==='artifact_set_score'));
});
test('model-facing Paper tools cannot grant visual review or paid authorization',async t=>{
  const{store}=await fixture(t);await store.create('paper');
  const before=await store.read();const tools=await store.tools();
  assert.ok(tools.some(t=>t.name==='artifact_build'));
  assert.ok(!tools.some(t=>t.name==='artifact_qa'));
  await assert.rejects(store.run('qa',{reviewed:true,aestheticEye:{status:'pass'},figureTable:{status:'pass'}}),/explicit user-reviewed/);
  assert.deepEqual(await store.read(),before);
  // Explicit caller review reaches adapter validation, rather than granting QA itself.
  await assert.rejects(store.run('qa',{}, {reviewed:true}), error=> !/explicit user-reviewed/.test(error.message));
  assert.deepEqual(await store.read(),before);
});
test('root project symlinks and oversized unlisted staged files are rejected',async t=>{
  const{cwd,store}=await fixture(t);const outside=join(cwd,'outside');await mkdir(outside);await symlink(outside,store.root);
  await assert.rejects(store.create('sound'),/symlink/i);
  await assert.rejects(store.read(),/symlink/i);
  const{store:second}=await fixture(t);
  await assert.rejects(unsafeCommit(second,async stage=>{
    await writeFile(join(stage,'audio'),'x');
    const{open}=await import('node:fs/promises');const file=await open(join(stage,'unlisted'),'w');try{await file.truncate(64*1024*1024+1);}finally{await file.close();}
  },['audio']),/64 MiB/);
});

test('export saves accepted picture bytes, never an unaccepted proposal, and refuses overwrite', async t => {
  const {cwd,store}=await fixture(t); await store.create('picture');
  const blank=await store.selected(); const accepted=await readFile(join(blank.root,'composite.png'));
  await store.run('propose',{seed:'export',color:[255,0,0,255],points:[{x:10,y:10},{x:300,y:300}],thickness:12});
  const selected=await store.selected();
  assert.notDeepEqual(await readFile(join(selected.root,'preview.png')),accepted);
  const output=join(cwd,'exported.png'); await store.export(output);
  assert.deepEqual(await readFile(output),accepted);
  await assert.rejects(store.export(output),{code:'EEXIST'});
  await assert.rejects(store.export(join(selected.root,'overwrite.png')),/outside/);
  assert.equal((await store.selected()).version,selected.version,'export does not create a version');
});
test('preview/export verify immutable bytes and reject unbuilt paper exports', async t => {
  const {cwd,store}=await fixture(t); await store.create('sound');
  const preview=await store.preview(); assert.equal(preview.mime,'audio/wav');
  const output=join(cwd,'sound.wav'); await store.export(output);
  assert.deepEqual(await readFile(output),await readFile(preview.path));
  await writeFile(preview.path,'corrupted');
  await assert.rejects(store.preview(),/hash mismatch/);
  await assert.rejects(store.export(join(cwd,'other.wav')),/hash mismatch/);
  await store.create('paper');
  await assert.rejects(store.export(join(cwd,'draft.pdf')),/Build or render/);
});
test('tools follow selected medium and cannot call a previous medium action', async t => {
  const {store}=await fixture(t); const image=await store.create('picture');
  assert.ok((await store.tools()).some(t=>t.name==='artifact_propose'));
  await store.create('sound'); assert.ok(!(await store.tools()).some(t=>t.name==='artifact_propose'));
  await assert.rejects(store.run('propose',{seed:'stale'}),/Unknown sound action/);
  await store.select(image.artifactId); assert.ok((await store.tools()).some(t=>t.name==='artifact_propose'));
  await store.select('piece'); assert.deepEqual(await store.tools(),[]); await assert.rejects(store.run('propose',{}),/Select a/);
});

test('GameBoy selection supplies source tools and preserves last ROM without exporting stale builds', async t => {
  const {cwd,store}=await fixture(t);
  let made;
  try { made=await store.create('gameboy','Pocket'); } catch(error) { if(/GBDK|lcc|compiler/.test(error.message)) return t.skip('GBDK compiler not installed');throw error; }
  const current=await store.selected(); assert.equal(current.kind,'gameboy');
  assert.ok((await store.context()).includes('main.c'));
  assert.ok((await store.tools()).some(t=>t.name==='artifact_write_source'));
  if(!current.revision.files.includes('game.gb')) return t.skip('Source-only GameBoy scaffold without GBDK');
  const output=join(cwd,'pocket.gb'); await store.export(output);
  assert.equal((await readFile(output)).length,32768);
  const source=await readFile(join(current.root,'main.c'),'utf8');
  await store.run('write_source',{source:source+'\n// new edit\n'});
  const changed=await store.selected();assert.equal(changed.revision.metadata.sourceAhead,true);
  assert.equal((await store.preview()).mime,'application/x-gameboy-rom');
  await assert.rejects(store.export(join(cwd,'stale.gb')),/Build the current/);
  await store.rollback(1);await store.export(join(cwd,'restored.gb'));
  assert.deepEqual(await readFile(join(cwd,'restored.gb')),await readFile(output));
  assert.equal(made.artifactId,changed.id);
});
