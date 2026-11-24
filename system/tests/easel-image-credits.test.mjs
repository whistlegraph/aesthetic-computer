import test from 'node:test';
import assert from 'node:assert/strict';
import { imageOutputUsd, imageHoldBraincells, imageBraincells } from '../backend/easel-image-credits.mjs';
import { imageRequest, DEFAULT_IMAGE_MODEL } from '../backend/easel-images.mjs';

const ask = (over = {}) => imageRequest({ jobId: 'job-0123456789abcdef', prompt: 'a purple pug reading', quality: 'low', ...over });

test('the default hosted image is the newest Flare, and old ids still work', () => {
  assert.equal(ask().model, DEFAULT_IMAGE_MODEL);
  assert.equal(DEFAULT_IMAGE_MODEL, 'gpt-image-2.6-flare');
  assert.equal(ask({ model: 'gpt-image-2' }).model, 'gpt-image-2');
  assert.throws(() => ask({ model: 'gpt-image-1' }), /Unsupported/);
});

test('table prices: 2.5 is a quarter of gpt-image-2 at medium, and 2.6 is held at the higher table', () => {
  assert.equal(imageOutputUsd({ model: 'gpt-image-2', quality: 'medium', size: '1024x1024' }), 0.053);
  assert.equal(imageOutputUsd({ model: 'gpt-image-2.5-flare', quality: 'medium', size: '1024x1024' }), 0.01317);
  assert.equal(imageOutputUsd({ model: 'gpt-image-2.6-flare', quality: 'high', size: '1024x1536' }), 0.165);
  assert.equal(imageOutputUsd({ model: 'openai/gpt-image-2.5/flare/text-to-image', quality: 'low', size: '1024x1024' }), 0.00588);
});

test('a low square print holds a few thousand braincells, and a reference holds more', () => {
  const plain = imageHoldBraincells(ask());
  assert.ok(plain >= 3600 && plain < 3800, `hold ${plain}`);
  assert.ok(imageHoldBraincells(ask({ reference: 'iVBORw0KGgo=' })) > plain + 19_000);
});

test('the bill is the tokens the provider reported, at twice cost, never above the hold', () => {
  const input = ask();
  // 50 text tokens in, 196 image tokens out: (50×$5 + 196×$30)/1M = $0.00613 → ×2 at 200k/$.
  const used = imageBraincells(input, { input_tokens: 50, input_tokens_details: { text_tokens: 50, image_tokens: 0 }, output_tokens: 196 });
  assert.equal(used, Math.ceil((50 * 5 + 196 * 30) / 1e6 * 2 * 200_000));
  const huge = imageBraincells(input, { input_tokens: 0, output_tokens: 10_000_000 });
  assert.equal(huge, imageHoldBraincells(input));
});

test('an image that reports no usage is billed at the table price', () => {
  const input = ask({ model: 'gpt-image-2.5-flare', quality: 'medium' });
  const billed = imageBraincells(input, null);
  assert.ok(billed >= Math.ceil(0.01317 * 2 * 200_000) && billed <= imageHoldBraincells(input));
});
