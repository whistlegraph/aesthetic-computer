// What a hosted image costs in braincells. Images draw on the same free
// allowance and wallet as text inference (easel-inference.mjs), at the same
// markup: twice what the provider charges AC, at pack value.
//
// OpenAI bills GPT Image by the token: $5 per million text input tokens, $8
// per million image input tokens and $30 per million image output tokens, the
// same for gpt-image-2, 2.5 and 2.6 (developers.openai.com/api/docs/pricing,
// read 2026-11-24). Each response carries its own `usage`, so the bill is
// what that image actually used. The table below only bounds the hold and
// prices a response that came back without usage.
import { braincellsFromCost } from './easel-paid-credits.mjs';

const TEXT_INPUT = 5 / 1e6, IMAGE_INPUT = 8 / 1e6, IMAGE_OUTPUT = 30 / 1e6;

// USD per image, output only, by quality: [low, medium, high]. gpt-image-2 is
// OpenAI's published per-image table; 2.5 is fal's table at the same token
// rates. 2.6 has no published table yet, so it is held at gpt-image-2's,
// which uses about four times 2.5's tokens at medium and high.
const SQUARE = { 'gpt-image-2': [0.006, 0.053, 0.211], 'gpt-image-2.5': [0.00588, 0.01317, 0.05268] };
const TALL = { 'gpt-image-2': [0.005, 0.041, 0.165], 'gpt-image-2.5': [0.00474, 0.01029, 0.04116] };
const QUALITY = { low: 0, medium: 1, high: 2 };

// The family a model id prices as. fal's Flare is OpenAI's 2.5 Flare.
function family(model) {
  if (/gpt-image-2\.5/.test(model)) return 'gpt-image-2.5';
  return 'gpt-image-2'; // gpt-image-2 itself, and 2.6 held at the higher table
}

export function imageOutputUsd({ model, quality = 'medium', size = '1024x1024' }) {
  const table = size === '1024x1024' ? SQUARE : TALL;
  const row = table[family(model)], at = QUALITY[quality];
  if (!row || at === undefined) throw new Error('This image setting has no braincell price.');
  return row[at];
}

// A reference image is sent as a PNG of at most 16 megapixels; OpenAI
// processes it at high fidelity. This bounds its input tokens generously.
const REFERENCE_BOUND_USD = 0.05;
const promptTokensBound = prompt => Math.ceil(Buffer.byteLength(String(prompt || ''), 'utf8') * 1.25);

// The most one image request can cost, in braincells. The table is a typical
// image, not a ceiling, so the hold carries half again; what is not used goes
// straight back to the wallet when the image settles.
const HOLD_HEADROOM = 1.5;
export function imageHoldBraincells(input) {
  const usd = imageOutputUsd(input) * HOLD_HEADROOM + promptTokensBound(input.prompt) * TEXT_INPUT + (input.reference ? REFERENCE_BOUND_USD : 0);
  return braincellsFromCost(usd);
}

// What a finished image costs, in braincells, never more than its hold. From
// the provider's usage when it reported tokens; otherwise the table price,
// the prompt bound and a reference at a fifth of its bound.
export function imageBraincells(input, usage) {
  const hold = imageHoldBraincells(input);
  const tokens = usage && Number.isFinite(Number(usage.output_tokens));
  let usd;
  if (tokens) {
    const details = usage.input_tokens_details || {};
    const textIn = Number(details.text_tokens), imageIn = Number(details.image_tokens);
    const split = Number.isFinite(textIn) || Number.isFinite(imageIn);
    usd = (split ? (textIn || 0) * TEXT_INPUT + (imageIn || 0) * IMAGE_INPUT
      : (Number(usage.input_tokens) || 0) * (input.reference ? IMAGE_INPUT : TEXT_INPUT))
      + Number(usage.output_tokens) * IMAGE_OUTPUT;
  } else {
    usd = imageOutputUsd(input) + promptTokensBound(input.prompt) * TEXT_INPUT
      + (input.reference ? REFERENCE_BOUND_USD / 5 : 0);
  }
  return Math.min(hold, braincellsFromCost(usd));
}
