import test from 'node:test';import assert from 'node:assert/strict';import {nativeCanvasRegion,validateNativeRegion} from '../lib/native-region.mjs';
const display={width:2560,height:1440,pixelWidth:2560,pixelHeight:1440};
const browser={screenX:448,screenY:158,outerWidth:1676,outerHeight:1102,innerWidth:1676,innerHeight:1000,canvas:{x:0,y:0,width:1676,height:1000}};
test('native crop fits canvas at exact 16:9 without desktop',()=>{const r=nativeCanvasRegion(display,browser);assert.equal(r.w*9,r.h*16);assert.ok(r.x>448&&r.y>260);assert.ok(r.x+r.w<2124&&r.y+r.h<1260);assert.equal(validateNativeRegion(r,{w:2560,h:1440}),`crop=${r.w}:${r.h}:${r.x}:${r.y}`);});
test('retina pixels retain native scale',()=>{const r=nativeCanvasRegion({...display,pixelWidth:5120,pixelHeight:2880},browser);assert.ok(r.w>3000);assert.equal(r.w*9,r.h*16);});
test('reject off-display, changed source and tiny regions',()=>{assert.throws(()=>nativeCanvasRegion(display,{...browser,screenX:2200}));assert.throws(()=>validateNativeRegion({x:100,y:100,w:1280,h:720},{w:1280,h:720}));assert.throws(()=>nativeCanvasRegion(display,{...browser,canvas:{x:0,y:0,width:100,height:100}}));});
