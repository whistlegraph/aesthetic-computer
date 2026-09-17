import test from 'node:test';import assert from 'node:assert/strict';import {assertPresentationClean,dismissAutomationBanner} from '../lib/presentation-check.mjs';
const executor=({macpal=false,text=[],failed=false}={})=>(file,args)=>file.endsWith('osascript')?{status:failed?1:0,stdout:JSON.stringify(text)}:{status:macpal?0:1};
test('clean chrome and hidden MacPal pass',()=>assert.equal(assertPresentationClean({exec:executor()}).chromeDebuggingUIAbsent,true));
test('dialog, banner, MacPal and unavailable native inspection refuse capture',()=>{for(const state of [{text:['Allow remote debugging?']},{text:['Chrome is being controlled by automated test software']},{macpal:true},{failed:true}])assert.throws(()=>assertPresentationClean({exec:executor(state)}));});

test('banner dismissal emits a durable check result, including already absent',()=>{for(const dismissed of [true,false]){const r=dismissAutomationBanner({exec:()=>({status:0,stdout:JSON.stringify({dismissed,bannerFound:dismissed})})});assert.equal(r.dismissed,dismissed);assert.equal(r.schema,'captutor-chrome-presentation/v1');assert.ok(r.checkedAt);}});
test('unidentifiable native close control blocks filming',()=>assert.throws(()=>dismissAutomationBanner({exec:()=>({status:1,stderr:'no unique close control'})}),/safely dismiss/));
