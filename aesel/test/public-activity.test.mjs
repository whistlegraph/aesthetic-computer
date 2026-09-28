import test from 'node:test';
import assert from 'node:assert/strict';
import { publicActivity, observeToolActivity, toolActivity } from '../src/public-activity.mjs';

test('bubble uses first-person observed phases, with no claim of success', () => {
  for (const status of ['preparing','connecting','waiting','generating','composing','writing','working','approval','interrupting']) {
    assert.match(publicActivity({busy:true,status}), /^I'm /);
  }
  assert.equal(publicActivity({busy:false,activityText:'stale',activityStage:'stale'}), '');
  assert.equal(publicActivity({busy:true,status:'approval',activityText:'stale'}), "I'm waiting for your approval");
  assert.equal(publicActivity({busy:true,status:'interrupting',activityText:'stale'}), "I'm stopping");
});

test('actual public deltas retain the whole reply, not a sliding tail', () => {
  const state = {busy:true,status:'generating',activityText:''};
  const words = "I've made the circle smaller. ".repeat(12);
  for (const char of words) {
    state.activityText += char;
    assert.equal(publicActivity(state), state.activityText.trim());
  }
});

test('parallel tools remain accurate as calls finish out of order', () => {
  const state = {busy:true,status:'tool',activityText:'old reply',activityMessageId:'old'};
  const read = {id:'read',type:'mcpToolCall',tool:'mcp__ac__ac_symbol'};
  const frame = {id:'frame',type:'dynamicToolCall',tool:'ac_frame · private detail'};
  observeToolActivity(state,'item/started',read);
  assert.equal(publicActivity(state), "I'm reading the source");
  assert.equal(state.activityMessageId,null);
  observeToolActivity(state,'item/started',frame);
  assert.equal(publicActivity(state), "I'm looking at the preview");
  observeToolActivity(state,'item/completed',read);
  assert.equal(publicActivity(state), "I'm looking at the preview");
  observeToolActivity(state,'item/completed',{id:'advice',type:'dynamicToolCall',tool:'jev'});
  assert.equal(publicActivity(state), "I'm looking at the preview");
  observeToolActivity(state,'item/completed',frame);
  assert.equal(publicActivity(state), 'old reply');
  assert.equal(state.status,'working');
});

test('only known operations receive specific captions; arguments never appear', () => {
  assert.equal(toolActivity({type:'fileChange'}), "I'm editing the piece");
  assert.equal(toolActivity({type:'dynamicToolCall',tool:'Write · /private/file'}), "I'm editing the piece");
  assert.equal(toolActivity({type:'dynamicToolCall',tool:'Bash · secret command'}), "I'm running a command");
  assert.equal(toolActivity({type:'mcpToolCall',tool:'unknown',arguments:{secret:'private'}}), "I'm using a tool");
});


test('contextual public intent survives tool preparation, but cannot override inspection or a stop', () => {
  const state = {busy:true,status:'composing',activityIntent:"I'm untangling that roof overlap"};
  assert.equal(publicActivity(state), state.activityIntent);
  const frame = {id:'frame',type:'dynamicToolCall',tool:'ac_frame'};
  observeToolActivity(state,'item/started',frame);
  assert.equal(publicActivity(state), "I'm looking at the preview");
  observeToolActivity(state,'item/completed',frame);
  assert.equal(publicActivity(state), state.activityIntent);
  state.status='interrupting';
  assert.equal(publicActivity(state), "I'm stopping");
  state.busy=false;
  assert.equal(publicActivity(state), '');
});
