import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { parseEnv } from 'node:util';
import { evaluateChoices } from './jev-decisions.mjs';

const cues = {
  inspect_api: 'Check relevant ac_api documentation before another source repair.',
  inspect_preview: 'Inspect fresh ac_preview evidence, and ac_frame if needed, before changing source again.',
  repair: 'Use the concrete error evidence for one focused repair, then verify the current revision.',
  reconsider: 'Previous repairs have not cleared the error. Reconsider its cause using API and runtime evidence before editing again.',
  continue: 'Continue the existing flow without an extra intervention.',
};
const knownTools = new Set(['write_piece', 'ac_api', 'ac_preview', 'ac_frame']);

export function configuredJev({ env = process.env, home = homedir() } = {}) {
  let config = {};
  try { config = JSON.parse(readFileSync(join(home, '.config/easel/jev.json'), 'utf8')); } catch {}
  const enabled = env.EASEL_JEV === undefined ? config.enabled === true : env.EASEL_JEV === '1';
  if (!enabled) return null;
  let apiKey = env.OPENROUTER_API_KEY;
  if (!apiKey) try {
    apiKey = parseEnv(readFileSync(join(home, '.config/aesthetic-computer/jev.env'), 'utf8')).OPENROUTER_API_KEY;
  } catch {}
  if (!apiKey) return null;
  return new JevAdvisor({ evaluate: (request, options) => evaluateChoices(request, { ...options, apiKey }) });
}

export class JevAdvisor {
  constructor({ evaluate = evaluateChoices, timeoutMs = 1200 } = {}) {
    this.evaluate = evaluate; this.timeoutMs = timeoutMs; this.beginTurn();
  }
  beginTurn() { this.calls = 0; this.seen = new Set(); this.writes = 0; this.apiLookups = 0; this.failures = 0; }
  async advise({ feedback, blocks, results, signal }) {
    this.writes += blocks.filter(b => b.name === 'write_piece').length;
    this.apiLookups += blocks.filter(b => b.name === 'ac_api').length;
    const failedTools = blocks.filter((b, i) => results[i]?.is_error && knownTools.has(b.name)).map(b => b.name);
    const errors = (feedback?.logs || []).filter(l => l.level === 'error');
    if (!failedTools.length && !errors.length) return null;
    this.failures++;
    const kinds = new Set();
    for (const error of errors) {
      const text = String(error.text || '');
      kinds.add(/SyntaxError/.test(text) ? 'syntax' : /ReferenceError/.test(text) ? 'reference'
        : /TypeError/.test(text) ? 'type' : 'other');
      if (/is not a function|is not defined/.test(text)) kinds.add('unknown_api');
    }
    // Only categories and counts leave the machine. No source, log prose,
    // prompts, file paths, handle, or revision is sent to Jev.
    const state = { errors: [...kinds].sort(), failedTools, currentPreview: !!feedback,
      frameObserved: !!feedback?.frame, priorWrites: Math.min(this.writes, 12),
      apiLookups: Math.min(this.apiLookups, 12), repeatedFailure: this.failures > 1 };
    if (kinds.has('syntax') || failedTools.includes('write_piece'))
      return { choice: 'repair', cue: cues.repair, local: true };
    const fingerprint = JSON.stringify({ ...state, priorWrites: undefined });
    if (this.calls >= 2 || this.seen.has(fingerprint)) return null;
    this.calls++; this.seen.add(fingerprint);
    const controller = new AbortController();
    const combined = signal ? AbortSignal.any([signal, controller.signal]) : controller.signal;
    let timer, onAbort;
    const started = performance.now();
    try {
      combined.throwIfAborted();
      const deadline = new Promise((_, reject) => {
        onAbort = () => reject(combined.reason);
        combined.addEventListener('abort', onAbort, { once: true });
        timer = setTimeout(() => controller.abort(new DOMException('Jev deadline', 'TimeoutError')), this.timeoutMs);
      });
      const result = await Promise.race([deadline, this.evaluate({ state, questions: { next: {
        type: 'choice', criteria: cues,
        instructions: 'Select the next diagnostic step for an Aesel JavaScript piece from these aggregate error categories. ' +
          'Unknown APIs favor documentation, missing observations favor preview inspection, concrete defects favor repair. ' +
          'Repeated errors after API lookup favor reconsidering the diagnosis. Otherwise continue. ' +
          'Do not declare success, change models, or authorize publication.',
      } } }, { signal: combined })]);
      const answer = result.answers?.next;
      if (!Object.hasOwn(cues, answer?.choice)) return null;
      const probability = answer.probabilities?.[answer.choice];
      return { choice: answer.choice,
        cue: Number.isFinite(probability) && probability >= .8 && answer.choice !== 'continue' ? cues[answer.choice] : '',
        elapsedMs: Math.round(performance.now() - started), model: result.model, usage: result.usage };
    } catch {
      if (signal?.aborted) signal.throwIfAborted();
      return null;
    } finally {
      clearTimeout(timer);
      if (onAbort) combined.removeEventListener('abort', onAbort);
    }
  }
}
