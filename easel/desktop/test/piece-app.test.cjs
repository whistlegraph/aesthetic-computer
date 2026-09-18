const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {createHash} = require('node:crypto');
const {projectIdentity,validatePieceApp,sessionPath,findSessionInstance,registerProject,registerRunning,listProjects} = require('../piece-app.cjs');

function fixture(t) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'piece-app-test-'));
  t.after(() => fs.rmSync(root, {recursive:true,force:true}));
  const p = projectIdentity({workspace:root,name:'goti',instance:'window-6c491a98-7517-4d00-9c8b-6694965014dd',stateRoot:root,appPath:path.join(root,'goti.app'),baseApp:path.join(root,'Aesel.app'),baseExecutable:path.join(root,'Aesel.app/Contents/MacOS/aesel'),piece:path.join(root,'goti.mjs')});
  fs.mkdirSync(p.appPath); return p;
}
test('renaming a project preserves the app identity, pinned app path and existing session', t => {
  const p = fixture(t), next = projectIdentity({...p,previous:p,name:'bird',appPath:'/ignored/bird.app'});
  assert.equal(next.id,p.id); assert.equal(next.bundleId,p.bundleId); assert.equal(next.appPath,p.appPath);
  const expected = path.join(p.stateRoot,'sessions',createHash('sha256').update(`${p.workspace}\0${p.instance}`).digest('hex')+'.json');
  assert.equal(sessionPath(p),expected); assert.equal(sessionPath(next),expected);
  assert.notEqual(projectIdentity({...p,previous:undefined}).bundleId,p.bundleId);
});
test('registry distinguishes installed, running and stale apps, and cleanup is owner-scoped', t => {
  const p=fixture(t); registerProject(p);
  assert.equal(listProjects(p.stateRoot)[0].pid,null);
  const release=registerRunning(p); assert.equal(listProjects(p.stateRoot)[0].pid,process.pid);
  assert.throws(()=>registerRunning(p,{pid:process.pid+1,alive(){}}),/already running/);
  release(); assert.equal(listProjects(p.stateRoot)[0].pid,null);
  const stale=registerRunning(p,{pid:9999999});
  assert.equal(listProjects(p.stateRoot,{alive(){throw Object.assign(Error('gone'),{code:'ESRCH'});}})[0].pid,null);
  const current=registerRunning(p,{alive(){throw Object.assign(Error('gone'),{code:'ESRCH'});}});
  stale(); assert.equal(listProjects(p.stateRoot)[0].pid,process.pid); current();
  fs.rmSync(p.appPath,{recursive:true}); assert.equal(listProjects(p.stateRoot)[0].installed,false);
});
test('invalid names, paths and conflicting bundle identities are rejected', t => {
  const p=fixture(t);
  for(const values of [{name:'../bad'},{name:'a:b'},{name:''},{workspace:'relative'},{bundleId:'computer.aesthetic.easel'},{instance:'../default'}])assert.throws(()=>validatePieceApp({...p,...values}),/Invalid/);
});
test('finds a project reopened as the default window without restoring an older piece', t => {
  const p=fixture(t); fs.mkdirSync(path.join(p.stateRoot,'sessions'));
  fs.writeFileSync(sessionPath(p),JSON.stringify({cwd:p.workspace,live:{file:'/old-piece.mjs'},savedAt:'2026-09-17T00:00:00Z'}));
  fs.writeFileSync(sessionPath({...p,instance:'default'}),JSON.stringify({cwd:p.workspace,live:{file:p.piece},savedAt:'2026-09-18T00:00:00Z'}));
  assert.equal(findSessionInstance({...p,fallback:p.instance}),'default');
});
