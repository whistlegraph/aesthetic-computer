import test from 'node:test';import assert from 'node:assert/strict';import {assertSingleRecordingTab} from '../lib/single-tab.mjs';
const mock=windows=>({send:async(method,p)=>{if(method==='Target.getTargets')return {targetInfos:windows.map((w,i)=>({type:'page',targetId:String(i)}))};if(method==='Browser.getWindowForTarget')return {windowId:p?.targetId===undefined?1:windows[Number(p.targetId)]};throw Error('Unexpected mutation '+method);}});
test('one recording tab accepts other windows without mutation',async()=>assert.equal((await assertSingleRecordingTab(mock([1,2,2]))).tabCount,1));
test('extra recording tab fails without closing it',async()=>assert.rejects(assertSingleRecordingTab(mock([1,1])),/2 tabs/));
test('missing recording target fails closed',async()=>assert.rejects(assertSingleRecordingTab(mock([2])),/0 tabs/));
