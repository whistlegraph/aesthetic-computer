import test from 'node:test';
import assert from 'node:assert/strict';
import { chooseObservedTarget } from '../lib/jev-computer-use.mjs';
const input=()=>({goal:'Start match',observation:{id:'frame1',target:'page1',capturedAt:new Date().toISOString()},
  candidates:[{id:'start',label:'Start match',role:'button',visible:true,locator:'#start'}]});
test('selects an observed target without executing or sending coordinates',async()=>{
  const result=await chooseObservedTarget(input(),{evaluate:async request=>{
    assert.doesNotMatch(JSON.stringify(request),/#start|page1|frame1/);
    return {answers:{next:{choice:'target_0',probabilities:{target_0:.99}}}};
  }});
  assert.equal(result.candidate.locator,'#start');assert.equal(result.performed,false);assert.equal(result.target,'page1');
});
test('unknown outcomes and stale frames never call the model',async()=>{
  const evaluate=()=>assert.fail('must not call');
  assert.equal((await chooseObservedTarget({...input(),previousOutcome:'unknown'},{evaluate})).action,'observe');
  const stale=input();stale.observation.capturedAt='2000-01-01';
  assert.equal((await chooseObservedTarget(stale,{evaluate})).reason,'stale_observation');
});
test('low confidence and invented choices cannot become targets',async()=>{
  for(const choice of ['target_500','target_0']) {
    const result=await chooseObservedTarget(input(),{evaluate:async()=>({answers:{next:{choice,probabilities:{[choice]:.4}}}})});
    assert.equal(result.action,'observe');assert.equal(result.candidate,undefined);
  }
});
