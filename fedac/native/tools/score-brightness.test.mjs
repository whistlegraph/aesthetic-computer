import test from 'node:test';
import assert from 'node:assert/strict';
import {brightnessAt,createScoreBrightness} from '../lib/score-brightness.mjs';

test('seat cues respect time and ignore invalid percentages',()=>{
 const cues=[{t:2,percent:40},{t:3,percent:70,seat:5},{t:4,percent:999}];
 assert.equal(brightnessAt(cues,1,0,0),0);
 assert.equal(brightnessAt(cues,5,0,0),40);
 assert.equal(brightnessAt(cues,5,5,0),70);
});

test('audio hits reach maximum, release to zero, and idle ignores speech',()=>{
 let status,adjustment;
 const system={brightness:0,readFile:()=>JSON.stringify({brightnessPercent:null,brightnessMode:'audio'}),brightnessAdjust:v=>adjustment=v,writeFile:(p,s)=>status=JSON.parse(s)};
 const control=createScoreBrightness();control.boot(system);
 control.update(system,0,-1,1);assert.equal(status.requested,0);
 control.update(system,1,1,.1);assert.equal(status.requested,100);assert.equal(adjustment,10000);
 system.brightness=100;control.update(system,1.3,1.3,0);assert(status.requested>0&&status.requested<100);
 control.update(system,3,3,0);assert.equal(status.requested,0);
});

test('zero override survives reload and wins over an audio hit',()=>{
 let status;const adjustments=[];
 const system={brightness:0,readFile:()=>'{"brightnessPercent":0}',brightnessAdjust:v=>adjustments.push(v),writeFile:(p,s)=>status=JSON.parse(s)};
 const control=createScoreBrightness();control.boot(system);control.update(system,1,1,.5);
 assert.equal(status.requested,0);assert.equal(status.mode,'override');
 assert(!adjustments.some(v=>v>0));
});
