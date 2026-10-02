import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtempSync,writeFileSync,readFileSync,rmSync,utimesSync,statSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {createHash} from 'node:crypto';
import {launchTarget} from '../src/launch-target.mjs';
const digest=value=>createHash('sha256').update(value).digest('hex');
function fixture(t){
 const root=mkdtempSync(join(tmpdir(),'aesel-launch-'));t.after(()=>rmSync(root,{recursive:true,force:true}));
 writeFileSync(join(root,'tui.mjs'),'let x=1;');writeFileSync(join(root,'.tui-built.mjs'),'bundle');
 const base=JSON.stringify({schema:1,sources:{'tui.mjs':digest('let x=1;')},sha256:digest('bundle')});
 writeFileSync(join(root,'.tui-built.json'),base);writeFileSync(join(root,'.tui-bun.cjs'),'bun code');writeFileSync(join(root,'.tui-bun.cjs.jsc'),'bytecode');
 writeFileSync(join(root,'.tui-bun.json'),JSON.stringify({schema:1,bun:'test-version',sourceManifest:digest(base),code:digest('bun code'),bytecode:digest('bytecode')}));
 return root;
}
test('Bun selects verified bytecode for its exact runtime; Node retains the JavaScript bundle',t=>{
 const root=fixture(t);
 assert.equal(launchTarget(root,{bunVersion:'test-version'}),'.tui-bun.cjs');
 assert.equal(launchTarget(root,{bunVersion:''}),'.tui-built.mjs');
 assert.equal(launchTarget(root,{bunVersion:'new-version'}),'.tui-built.mjs');
});
test('same-size edits with restored timestamps cannot run old bundled code',t=>{
 const root=fixture(t),file=join(root,'tui.mjs'),before=statSync(file);
 writeFileSync(file,'let x=2;');utimesSync(file,before.atime,before.mtime);
 assert.equal(launchTarget(root,{bunVersion:'test-version'}),'tui.mjs');
});
test('missing or damaged Bun artifacts fall back without executing stale bytecode',t=>{
 const root=fixture(t),file=join(root,'.tui-bun.cjs.jsc');
 writeFileSync(file,'damaged');assert.equal(launchTarget(root,{bunVersion:'test-version'}),'.tui-built.mjs');
 rmSync(file);assert.equal(launchTarget(root,{bunVersion:'test-version'}),'.tui-built.mjs');
 writeFileSync(file,'bytecode');writeFileSync(join(root,'.tui-bun.cjs'),'changed');
 assert.equal(launchTarget(root,{bunVersion:'test-version'}),'.tui-built.mjs');
});
test('Bun bytecode must match the current source manifest as well as its code',t=>{
 const root=fixture(t),file=join(root,'.tui-built.json');
 writeFileSync(file,readFileSync(file,'utf8')+'\n');
 assert.equal(launchTarget(root,{bunVersion:'test-version'}),'.tui-built.mjs');
});
