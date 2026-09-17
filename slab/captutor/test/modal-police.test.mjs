import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtempSync,rmSync,readFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {createModalPolice,connectWithModalPolice} from '../lib/modal-police.mjs';
const hit=kind=>({kind,title:kind,buttons:kind==='remote-debugging'?['Cancel','Allow']:['Close']});
function setup(t,kind,options={}){
 const directory=mkdtempSync(join(tmpdir(),'modal-police-'));t.after(()=>rmSync(directory,{recursive:true,force:true}));
 const actions=[],events=[];
 const police=createModalPolice({directory,scan:async()=>[hit(kind)],act:async k=>actions.push(k),onEvent:e=>events.push(e),...options});
 return {police,actions,events,directory};
}
test('remembers recognition and recalls only a permitted preparation action',async t=>{
 const f=setup(t,'automation-banner');await f.police.check('preparing');await f.police.check('preparing');
 assert.equal(f.events[1].remembered,true);assert.equal(f.actions.length,2);
 const memo=JSON.parse(readFileSync(join(f.directory,'memo.json')));assert.equal(Object.values(memo)[0].count,2);
 await assert.rejects(f.police.check('recording'),/blocked/);assert.equal(f.actions.length,2);
});
test('consent requires explicit policy and a pending connection',async t=>{
 const denied=setup(t,'remote-debugging');await assert.rejects(denied.police.check('connecting'));assert.deepEqual(denied.actions,[]);
 const allowed=setup(t,'remote-debugging',{allowRemoteDebugging:true});await allowed.police.check('connecting');
 await assert.rejects(allowed.police.check('preparing'));await assert.rejects(allowed.police.check('recording'));assert.equal(allowed.actions.length,1);
});
test('unknown dialog is flagged once while present, never clicked',async t=>{
 const f=setup(t,'unknown');await assert.rejects(f.police.check('preparing'));await assert.rejects(f.police.check('preparing'));
 assert.equal(f.events.length,1);assert.deepEqual(f.actions,[]);
});
test('unavailable inspection blocks the watcher',async t=>{
 const f=setup(t,'unknown',{scan:async()=>{throw Error('AX unavailable');}});await assert.rejects(f.police.check());assert.equal(f.events[0].kind,'inspection-failed');
});
test('connection watcher stops when its connection resolves',async()=>{
 let checks=0;const result=await connectWithModalPolice(async()=>{await new Promise(r=>setTimeout(r,15));return 'connected';},{intervalMs:1,police:{check:async()=>{checks++;}}});
 assert.equal(result,'connected');const before=checks;await new Promise(r=>setTimeout(r,5));assert.equal(checks,before);assert.ok(checks>0);
});
