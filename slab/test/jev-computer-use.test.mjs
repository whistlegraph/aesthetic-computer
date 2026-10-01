import test from 'node:test';
import assert from 'node:assert/strict';
import { candidatesFromFrame, chooseObservedTarget } from '../lib/jev-computer-use.mjs';
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

test('selection state and bounded history preserve false and strip private fields', async () => {
  const request = input();
  request.candidates[0].selected = false;
  request.recentActions = [{ action: 'click', label: 'Tab 1', role: 'tab', outcome: 'verified',
    value: 'private value', locator: '#private', target: 'private-page', node: 111 }];
  const result = await chooseObservedTarget(request, { evaluate: async payload => {
    assert.deepEqual(payload.state.targets, [{ id: 'target_0', label: 'Start match', role: 'button', selected: false }]);
    assert.deepEqual(payload.state.recentActions, [{ action: 'click', label: 'Tab 1', role: 'tab', outcome: 'verified' }]);
    assert.doesNotMatch(JSON.stringify(payload), /private|111|#start|page1|frame1/);
    return { answers: { next: { choice: 'target_0', probabilities: { target_0: .99 } } } };
  } });
  assert.equal(result.action, 'target');
  assert.equal(request.recentActions[0].value, 'private value', 'caller history is not mutated');
});

test('unknown history prevents another decision; oversized and malformed history is rejected', async () => {
  const action = { action: 'click', label: 'Tab 1', role: 'tab', outcome: 'verified' };
  const options = { evaluate: () => assert.fail('must not call') };
  const result = await chooseObservedTarget({ ...input(), recentActions: [{ ...action, outcome: 'unknown' }] }, options);
  assert.equal(result.reason, 'verify_previous_action');
  for (const recentActions of [Array(4).fill(action), null, [{ ...action, label: 'a'.repeat(161) }],
    [{ ...action, role: 'a'.repeat(31) }], [{ ...action, action: 'eval' }], [{ ...action, outcome: 'assumed' }]]) {
    await assert.rejects(chooseObservedTarget({ ...input(), recentActions }, options), /recent actions|Recent actions/);
  }
});

test('Frame carries only explicitly observed boolean selection state', () => {
  const controls = [true, false, undefined, 'false'].map(selected => ({ text: 'Tab', role: 'tab', selected,
    rect: { width: 20, height: 20 } }));
  const candidates = candidatesFromFrame({ controls });
  assert.deepEqual(candidates.map(c => c.selected), [true, false, undefined, undefined]);
  assert.equal('selected' in candidates[2], false);
  assert.equal('selected' in candidates[3], false);
});
