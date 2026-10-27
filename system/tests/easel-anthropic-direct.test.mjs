// Claude on AC's Anthropic credits: which models go direct, and what a
// usage block with no reported cost bills as.
import test from 'node:test';
import assert from 'node:assert/strict';
import {anthropicCost,braincellsFromCost,usageBraincells} from '../backend/easel-paid-credits.mjs';
import {ANTHROPIC_DIRECT,anthropicThinking,EASEL_MODELS} from '../backend/easel-policy.mjs';

test('every direct model is allowlisted and priced', () => {
  for (const model of Object.keys(ANTHROPIC_DIRECT)) {
    assert.ok(Object.hasOwn(EASEL_MODELS, model), model);
    assert.equal(typeof anthropicCost(model, {}), 'number', model);
  }
});

test('cost prices input, output and both cache writes', () => {
  const usage = {input_tokens: 1e6, output_tokens: 1e6, cache_read_input_tokens: 1e6,
    cache_creation_input_tokens: 2e6, cache_creation: {ephemeral_1h_input_tokens: 1e6}};
  // Sonnet 5.5 at $2 / $10: 2 + 10 + 0.2 + 2.5 + 4.
  assert.equal(anthropicCost('anthropic/claude-sonnet-5.5', usage).toFixed(6), '18.700000');
  assert.equal(anthropicCost('openai/gpt-5.4', usage), null);
});

test('a direct call bills at the same 2× markup as a reported cost', () => {
  const usage = {input_tokens: 1200, output_tokens: 300};
  const cost = anthropicCost('anthropic/claude-opus-5.5', usage);
  assert.equal(usageBraincells({model: 'anthropic/claude-opus-5.5', tokens: 1500, cost}), braincellsFromCost(cost));
});

test('thinking controls translate to the shapes each model accepts', () => {
  assert.deepEqual(anthropicThinking('anthropic/claude-opus-5.5', {thinking: {type: 'enabled', budget_tokens: 2048}}), {thinking: {type: 'adaptive'}});
  assert.deepEqual(anthropicThinking('anthropic/claude-sonnet-5.5', {thinking: {type: 'disabled'}}), {thinking: {type: 'between_tools'}});
  assert.deepEqual(anthropicThinking('anthropic/claude-opus-5.5', {thinking: {type: 'disabled'}}), {output_config: {effort: 'low'}});
  assert.deepEqual(anthropicThinking('anthropic/claude-sonnet-5.5', {reasoning: {effort: 'none'}}), {output_config: {effort: 'low'}});
  assert.deepEqual(anthropicThinking('anthropic/claude-sonnet-5.5', {}), {});
});
