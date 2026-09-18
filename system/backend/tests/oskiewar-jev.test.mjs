import test from 'node:test';
import assert from 'node:assert/strict';
import { createHandler } from '../oskiewar-jev.mjs';
import { cleanScene, buttons, controlPlan, controlsAt, decisionRequest } from '../../../xbox/live/jev-vs-jev/model.mjs';
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
test('Jev can face and strike without a full walk carrying it through the target',()=>{
  const turn=controlPlan('face_left','punch');
  assert.deepEqual(turn.down,['ArrowLeft','B']);
  assert.equal(turn.motionMs,20);
  assert.deepEqual(controlsAt(turn,0),['ArrowLeft','B']);
  assert.deepEqual(controlsAt(turn,25),['B']);
  assert.deepEqual(controlsAt(turn,220),[]);
  assert.deepEqual(controlsAt(null,0),[]);
  assert.ok(turn.actionMs>turn.motionMs);
  assert.ok(turn.motionMs*880/1000<20,'turn travels less than a fighter width');
  assert.equal(controlPlan('step_right','kick').motionMs,55);
  assert.equal(controlPlan('still','jump').actionMs,450);
  for(const [move,action] of [['still','punch'],['left','jump'],['face_right','kick']]){
    const p=controlPlan(move,action);
    assert.ok(p.releaseMs-Math.max(p.motionMs,p.actionMs)>=60,'next decision follows a real button release');
  }
});
test('a chosen platform drop supplies two down presses inside the combo window',()=>{
  const p=controlPlan('still','drop');
  assert.deepEqual(controlsAt(p,30),['ArrowDown']);
  assert.deepEqual(controlsAt(p,90),[]);
  assert.deepEqual(controlsAt(p,160),['ArrowDown']);
  assert.deepEqual(controlsAt(p,230),[]);
});
test('observation distinguishes airborne opponents and missing attack limbs',()=>{
  const value=cleanScene({self:{x:10,y:100,combat:{punch:false,kickLeft:true,kickRight:false,secret:'PRIVATE'}},
    opponent:{x:80,y:50,dx:70,dy:50,grounded:false,vx:20,vy:-300}});
  assert.equal(value.self.combat.punch,false);
  assert.equal(value.self.combat.kickLeft,true);
  assert.equal(value.opponent.grounded,false);
  assert.equal(value.opponent.vy,-300);
  assert.doesNotMatch(JSON.stringify(value),/PRIVATE|secret/);
});
test('Jev chooses from usable attacks after dismemberment',()=>{
  const s={self:{x:10,y:100,facing:1,combat:{punch:true,kickLeft:true,kickRight:false}},
    opponent:{x:80,y:100,dx:70,dy:0}};
  let choices=decisionRequest(s).questions.action.criteria;
  assert.ok(choices.punch);assert.equal(choices.kick,undefined);assert.equal(choices.item,undefined);
  s.opponent.dx=-70;
  choices=decisionRequest(s).questions.action.criteria;
  assert.ok(choices.kick);
  s.self.combat={headOnly:true};
  choices=decisionRequest(s).questions.action.criteria;
  assert.match(choices.punch,/Spit/);assert.match(choices.kick,/Spit/);
  assert.equal(choices.block,undefined);
});
