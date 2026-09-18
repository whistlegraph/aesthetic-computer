#!/usr/bin/env node
import { writeFileSync } from 'node:fs';
import { parseArgs } from 'node:util';
import { evaluate } from './evaluate.mjs';
import { evaluateChoices } from './openrouter.mjs';
import { fixtures } from './benchmark-fixtures.mjs';

const { values } = parseArgs({ options: {
  repeats: { type: 'string', default: '3' },
  provider: { type: 'string', default: 'both' },
  output: { type: 'string' },
} });
const repeats = Number(values.repeats);
if (!Number.isInteger(repeats) || repeats < 1 || repeats > 10)
  throw new Error('--repeats must be 1–10.');
if (!['both', 'vercel', 'openrouter'].includes(values.provider))
  throw new Error('--provider must be both, vercel, or openrouter.');
const providers = [
  { name: 'vercel', run: evaluate, key: process.env.AI_GATEWAY_API_KEY },
  { name: 'openrouter', run: evaluateChoices, key: process.env.OPENROUTER_API_KEY },
].filter(p => values.provider === 'both' || values.provider === p.name);
for (const p of providers) if (!p.key) throw new Error(`Missing credentials for ${p.name}.`);

const report = { at: new Date().toISOString(), node: process.version,
  methodology: 'Sequential calls, alternating provider order each repetition. First request per provider reported separately. Warm means reused process/network pool, not guaranteed provider cache state. Synthetic fixtures; agreement is not general accuracy. Elapsed includes network, gateway, inference, JSON parsing and validation.',
  repeats, fixtures, samples: [], summaries: {} };
for (let repeat = 0; repeat < repeats; repeat++) {
  for (const fixture of fixtures) {
    for (const provider of repeat % 2 ? [...providers].reverse() : providers) {
      if (provider.stopped) continue;
      const started = performance.now();
      const sample = { provider: provider.name, repeat, fixture: fixture.id, expected: fixture.expected };
      try {
        const result = await provider.run(fixture.request);
        sample.elapsedMs = Math.round(performance.now() - started);
        const answer = Object.values(result.answers)[0];
        Object.assign(sample, { choice: answer.choice, matches: answer.choice === fixture.expected,
          probabilities: answer.probabilities,
          confidence: answer.confidence ?? Object.values(result.providerMetadata?.typesafe?.confidence || {})[0],
          model: result.model || result.providerMetadata?.gateway?.routing?.canonicalSlug,
          costUsd: Number(result.usage?.cost ?? result.providerMetadata?.gateway?.cost ?? 0),
          usage: result.usage });
      } catch (error) {
        sample.elapsedMs = Math.round(performance.now() - started);
        sample.error = error.message;
        // Do not hammer a provider after a credential, credit, or rate-limit error.
        if (/HTTP (401|402|403|429)\b/.test(error.message)) provider.stopped = error.message;
      }
      report.samples.push(sample);
      console.error(`${provider.name} ${fixture.id}: ${sample.choice || sample.error} · ${sample.elapsedMs}ms`);
    }
  }
}
const percentile = (sorted, p) => sorted[Math.max(0, Math.ceil(sorted.length * p) - 1)] ?? null;
for (const provider of providers) {
  const samples = report.samples.filter(s => s.provider === provider.name);
  const ok = samples.filter(s => !s.error);
  const times = ok.map(s => s.elapsedMs).sort((a, b) => a - b);
  const warm = samples.slice(1).filter(s => !s.error).map(s => s.elapsedMs).sort((a, b) => a - b);
  report.summaries[provider.name] = { requests: samples.length, successful: ok.length,
    ...(provider.stopped ? { stopped: provider.stopped } : {}),
    expectedMatches: ok.filter(s => s.matches).length, firstMs: samples[0]?.elapsedMs,
    medianMs: percentile(times, .5), p95Ms: percentile(times, .95),
    warmMedianMs: percentile(warm, .5), minMs: times[0], maxMs: times.at(-1),
    costUsd: ok.reduce((sum, s) => sum + s.costUsd, 0) };
}
if (values.output) writeFileSync(values.output, JSON.stringify(report, null, 2) + '\n', { mode: 0o600 });
console.log(JSON.stringify(report.summaries, null, 2));
if (report.samples.some(s => s.error)) process.exitCode = 1;
