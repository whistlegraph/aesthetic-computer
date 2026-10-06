import test from 'node:test';
import assert from 'node:assert/strict';
import {inferenceError} from '../Resources/Web/inference-error.mjs';

test('exhausted inference directs the user to this app purchase screen', () => {
  const message = inferenceError({billing: true, message: 'Out of braincells — buy more from the braincell meter in Aesel, or switch provider with /provider. Free braincells reset at midnight UTC.'});
  assert.match(message, /Brain settings/);
  assert.match(message, /midnight UTC/);
  assert.doesNotMatch(message, /Aesel|\/provider/);
});

test('daily limits and unrelated errors retain their actual reason', () => {
  for (const message of ["This request would exceed today's limit; it resets in 2h.", 'Your account is unavailable.', 'The preview failed.']) {
    assert.equal(inferenceError({billing: true, message}), message);
  }
  assert.equal(inferenceError(undefined), '');
});
