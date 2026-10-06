import test from 'node:test';
import assert from 'node:assert/strict';
import {checkedPrompt} from '../Resources/Web/prompt-limit.mjs';
test('remote requests use the same 96 grapheme limit as iPhone typing', () => {
  assert.equal(checkedPrompt('a'.repeat(96)).length, 96);
  assert.throws(() => checkedPrompt('a'.repeat(97)), /96 characters/);
  const family = '👨‍👩‍👧‍👦';
  assert.equal(checkedPrompt(family.repeat(96)), family.repeat(96));
  assert.throws(() => checkedPrompt(family.repeat(97)), /96 characters/);
  assert.equal(checkedPrompt(' fix\nthe leaf '), 'fix the leaf');
  assert.throws(() => checkedPrompt('  '), /required/);
});
