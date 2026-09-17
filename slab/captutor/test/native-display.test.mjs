import test from 'node:test';import assert from 'node:assert/strict';
import {nativeDisplayFormat,assertNativePixels} from '../lib/native-display.mjs';
import {assertHiDPIStage} from '../lib/stage-contract.mjs';
import {parseStageFlags} from '../lib/stage-mode.mjs';
const g={width:2560,height:1440,pixelWidth:2560,pixelHeight:1440};
test('native capture uses actual display pixels at 1x and 2x',()=>{assert.deepEqual(nativeDisplayFormat(g).out,{w:2560,h:1440});assert.deepEqual(nativeDisplayFormat({...g,pixelWidth:5120,pixelHeight:2880}).out,{w:5120,h:2880});assert.throws(()=>nativeDisplayFormat({...g,pixelWidth:2559}));});
test('native mode retains Stage requirement and detects display changes',()=>{assert.throws(()=>assertHiDPIStage({required:true,nativeDisplay:g}));assert.equal(assertHiDPIStage({required:true,stageMode:true,nativeDisplay:g,screen:{width:2560,height:1440,dpr:1}}).mode,'native');assert.throws(()=>assertHiDPIStage({required:true,stageMode:true,nativeDisplay:g,screen:{width:1280,height:720,dpr:2}}));});
test('native delivery refuses cropping and upscaling',()=>{assertNativePixels({w:2560,h:1440},{w:2560,h:1440});assert.throws(()=>assertNativePixels({w:1920,h:1080},{w:2560,h:1440}));assert.throws(()=>assertNativePixels({w:2560,h:1600},{w:2560,h:1440}));});
test('native flag is consumed and refuses rotation',()=>{const p=parseStageFlags(['--native','--brand','fuser','render','demo.mjs']);assert.equal(p.native,true);assert.deepEqual(p.args,['render','demo.mjs']);assert.throws(()=>parseStageFlags(['--native','--vertical','render']));});
