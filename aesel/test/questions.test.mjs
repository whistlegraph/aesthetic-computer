import assert from 'node:assert/strict';
import test from 'node:test';
import {questionFor, questionText, answerFor, answerQuestion, declineQuestion} from '../src/questions.mjs';

const claude = {id:'ask-1', method:'item/tool/askUserQuestion', params:{questions:[
  {question:'Which color?', header:'Color', multiSelect:false, options:[{label:'Red',description:'warm'},{label:'Blue'}]},
  {question:'Which sizes?', header:'Size', multiSelect:true, options:[{label:'S'},{label:'M'},{label:'L'}]},
]}};

test('Claude questions are answered by number, several numbers, or in words', () => {
  const q = questionFor(claude);
  assert.equal(q.engine, 'claude');
  assert.match(questionText(q.questions[0], 1, 2), /1\/2 · Color — Which color\?\n  1\. Red — warm\n  2\. Blue/);
  assert.deepEqual(answerFor(q.questions[0], '2'), ['Blue']);
  assert.deepEqual(answerFor(q.questions[0], '1,2'), ['1,2'], 'single select takes several numbers as words');
  assert.deepEqual(answerFor(q.questions[0], '9'), ['9']);
  assert.deepEqual(answerFor(q.questions[1], '1, 3'), ['S', 'L']);
  assert.equal(answerQuestion(q, 'green please'), null);
  assert.deepEqual(answerQuestion(q, '2 3'), {decision:'answer', answers:{'Which color?':'green please', 'Which sizes?':'M, L'}});
  assert.deepEqual(declineQuestion(q), {decision:'decline'});
});

test('Codex questions take the general path; approval-shaped and secret ones do not', () => {
  const codex = {id:7, method:'item/tool/requestUserInput', params:{questions:[{id:'q1', question:'Name it?', options:[]}]}};
  const q = questionFor(codex);
  assert.equal(q.engine, 'codex');
  assert.deepEqual(answerQuestion(q, 'Moss'), {answers:{q1:{answers:['Moss']}}});
  assert.deepEqual(declineQuestion(q), {answers:{}});
  const approval = {id:8, method:'item/tool/requestUserInput', params:{questions:[{id:'a', question:'Run it?', options:[{label:'Allow'},{label:'Deny'}]}]}};
  assert.equal(questionFor(approval), null);
  assert.equal(questionFor({id:9, method:'item/tool/requestUserInput', params:{questions:[{id:'s', question:'Token?', isSecret:true}]}}), null);
  assert.equal(questionFor({id:10, method:'item/commandExecution/requestApproval', params:{command:'ls'}}), null);
});
