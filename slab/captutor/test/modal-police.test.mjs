import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtempSync,rmSync,readFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {runInNewContext} from 'node:vm';
import {CHROME_MODAL_SCRIPT,createModalPolice,connectWithModalPolice} from '../lib/modal-police.mjs';
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

test('native scan deduplicates Chrome sheets and excludes web content',()=>{
 const element=(role,name,children=[],subrole='')=>({role:()=>role,subrole:()=>subrole,name:()=>name,description:()=>name,value:()=>'',uiElements:()=>children,position:()=>[806,442]});
 const dialog=element('AXSheet','Allow remote debugging?',[element('AXButton','Cancel'),element('AXButton','Allow')]);
 const fakePage=element('AXWebArea','',[element('AXSheet','fake page dialog',[element('AXButton','Allow')])]);
 const window=element('AXWindow','',[dialog,fakePage]);
 const result=runInNewContext(CHROME_MODAL_SCRIPT+';JSON.stringify(uniqueHits.map(({kind,title,buttons})=>({kind,title,buttons})))',{
  Application:()=>({processes:{byName:()=>({windows:()=>[window,dialog]})}}),
 });
 const hits=JSON.parse(result);assert.equal(hits.length,1);assert.equal(hits[0].kind,'remote-debugging');assert.deepEqual(hits[0].buttons,['Cancel','Allow']);
});

test('native banner recognition includes toolbar infobars without choosing tab close',()=>{
 const e=(role,name,children=[])=>({role:()=>role,subrole:()=>'',name:()=>name,description:()=>name,value:()=>'',uiElements:()=>children,position:()=>[20,30]});
 const close=e('AXButton','Close');close.description=()=> 'Close button';
 const infobar=e('AXToolbar','',[e('AXStaticText','Chrome is being controlled by automated test software'),e('AXButton','Turn off in settings'),close]);
 const window=e('AXWindow','',[infobar,e('AXButton','Close')]);
 const result=runInNewContext(CHROME_MODAL_SCRIPT+';JSON.stringify(uniqueHits.map(({kind,buttons})=>({kind,buttons})))',{Application:()=>({processes:{byName:()=>({windows:()=>[window]})}})});
 assert.deepEqual(JSON.parse(result),[{kind:'automation-banner',buttons:['Turn off in settings','Close']}]);
});

test('connection watcher rescans a moved consent dialog instead of abandoning it',async()=>{
 let checks=0;const result=await connectWithModalPolice(async()=>{await new Promise(r=>setTimeout(r,20));return 'connected';},{intervalMs:1,police:{check:async()=>{if(++checks===1)throw Error('Modal changed or action ambiguous');}}});
 assert.equal(result,'connected');assert.ok(checks>1);
});
