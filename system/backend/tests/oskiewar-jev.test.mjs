import test from 'node:test';
import assert from 'node:assert/strict';
import { createHandler } from '../oskiewar-jev.mjs';
import { cleanScene, buttons } from '../../../xbox/live/jev-vs-jev/model.mjs';
const ticket='12345678-1234-1234-1234-123456789abc';
const event=body=>({httpMethod:'POST',headers:{},body:JSON.stringify(body)});
const scene={self:{x:100,y:200},private:'SECRET',opponent:{x:200,y:200,dx:100}};
test('demo validates observations and reports actual separate usage',async()=>{
  let calls=0;
  const handler=createHandler({store:{start:async()=>ticket,consume:async()=>true},evaluate:async request=>{
    calls++;assert.doesNotMatch(JSON.stringify(request),/SECRET/);
    return {model:'jev',answers:{motion:{choice:'right'},action:{choice:'kick'}},usage:{input_tokens:100,output_tokens:20,cost:.00001}};
  }});
  assert.equal((await handler(event({op:'start'}))).statusCode,200);
  const r=JSON.parse((await handler(event({ticket,seat:0,scene}))).body);
  assert.deepEqual(r.down,['ArrowRight','A']);assert.equal(r.usage.costUsd,.00001);
  assert.equal((await handler(event({ticket,seat:2,scene}))).statusCode,400);
  assert.equal(calls,1);
});
test('exhausted allowance and provider failure never substitute a bot move',async()=>{
  const denied=createHandler({store:{consume:async()=>false},evaluate:()=>assert.fail()});
  assert.equal((await denied(event({ticket,seat:1,scene}))).statusCode,429);
  const broken=createHandler({store:{consume:async()=>true},evaluate:async()=>{throw Error('SECRET');}});
  const r=await broken(event({ticket,seat:0,scene}));assert.equal(r.statusCode,503);assert.doesNotMatch(r.body,/SECRET|down/);
});
test('model input and outputs are bounded',()=>{
  assert.throws(()=>cleanScene({self:{x:'secret',y:1}}));
  assert.throws(()=>buttons('toward','kick'));
  assert.deepEqual(buttons('still','wait'),[]);
});
