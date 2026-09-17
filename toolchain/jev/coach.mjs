#!/usr/bin/env node
// Existing Oskiewar coach/workshop MCP plus a Jev practice decision.
import { createInterface } from 'node:readline';
import { pathToFileURL } from 'node:url';
import { handleMessage as coach } from '../../xbox/live/coach.mjs';
import { evaluate } from './evaluate.mjs';

const practice = {
  observe: 'Gather more fight evidence before choosing a drill.',
  defense: 'Practice blocking; too many incoming contacts land unblocked.',
  accuracy: 'Practice attack timing and range; too many attacks miss.',
  recovery: 'Practice returning to defense after attacks; attacks are being punished.',
  spacing: 'Practice distance control; the opponent repeatedly wins at the observed spacing.',
};
const cues = {
  observe: 'Play another exchange so the coach has more evidence.',
  defense: 'Practice blocking the next incoming attack.',
  accuracy: 'Get into range before throwing your next attack.',
  recovery: 'Return to defense after your attack.',
  spacing: 'Change your distance before the next exchange.',
};

export function decisionRequest(report, seat = 0) {
  if (![0, 1].includes(seat)) throw new Error('seat must be 0 or 1.');
  const fighter = report.fighters?.find(f => f.seat === seat);
  if (!fighter || !report.frames) throw new Error('No fighter observations yet; use coach_in and coach_watch first.');
  // Send only aggregate gameplay evidence, excluding names, room IDs and chat.
  const fields = ['seat', 'score', 'roundWins', 'deaths', 'kills', 'hitsTaken',
    'hitsTakenBy', 'hitsTakenWhile', 'blocks', 'blocksBy', 'blockRate',
    'attacks', 'airPct', 'crouchPct', 'blockingPct', 'averageDistance'];
  return {
    state: { seat, frames: report.frames, seconds: report.seconds,
      rounds: report.rounds,
      fighters: report.fighters.map(f => Object.fromEntries(fields
        .filter(key => Object.hasOwn(f, key)).map(key => [key, f[key]]))) },
    questions: { practice: {
      type: 'choice', criteria: practice,
      instructions: 'Choose one next practice focus for the requested Oskiewar seat, using only these observed statistics. ' +
        'These are session aggregates, not current positions or causal proof. ' +
        'Prefer observe when the evidence is sparse or inconclusive. ' +
        'Do not infer controls, map geometry, available equipment, or unseen mechanics.',
    } },
  };
}

const tool = {
  name: 'coach_jev',
  description: 'Ask Jev for one practice focus from the existing coach ledger. ' +
    'Sends aggregate gameplay statistics to Vercel AI Gateway/TypeSafe; needs AI_GATEWAY_API_KEY. ' +
    'Returns a fixed coaching cue and model probabilities. Does not edit the workshop. ' +
    'Use coach_workshop inspect separately to design a drill from this recommendation.',
  inputSchema: { type: 'object', properties: {
    seat: { type: 'integer', enum: [0, 1], default: 0 },
  }, additionalProperties: false },
};

export function createHandler({ coachHandler = coach, evaluateImpl = evaluate } = {}) {
  let busy = false;
  return async function handle(message) {
    if (message.method === 'tools/list') {
      const response = await coachHandler(message);
      return { ...response, result: { ...response.result,
        tools: [...response.result.tools, tool] } };
    }
    if (message.method !== 'tools/call' || message.params?.name !== 'coach_jev')
      return coachHandler(message);
    const reply = (value, isError = false) => ({ jsonrpc: '2.0', id: message.id,
      result: { ...(isError ? { isError: true } : {}),
        content: [{ type: 'text', text: JSON.stringify(value) }] } });
    if (busy) return reply('A Jev decision is already running.', true);
    busy = true;
    try {
      const analysis = await coachHandler({ jsonrpc: '2.0', id: message.id,
        method: 'tools/call', params: { name: 'coach_analyze', arguments: {} } });
      if (analysis.result?.isError || analysis.error) return analysis;
      const report = JSON.parse(analysis.result.content[0].text);
      const request = decisionRequest(report, message.params.arguments?.seat ?? 0);
      const started = performance.now();
      const result = await evaluateImpl(request);
      const answer = result.answers.practice;
      if (!Object.hasOwn(cues, answer.choice)) throw new Error('Invalid practice choice.');
      return reply({ model: 'typesafe-ai/jev', seat: request.state.seat,
        practice: answer.choice, cue: cues[answer.choice],
        probabilities: answer.probabilities,
        confidence: result.providerMetadata?.typesafe?.confidence,
        elapsedMs: Math.round(performance.now() - started), usage: result.usage,
        evidenceFrames: report.frames });
    } catch (error) {
      return reply(error.message, true);
    } finally { busy = false; }
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const handle = createHandler();
  const lines = createInterface({ input: process.stdin, terminal: false });
  lines.on('line', async line => {
    if (!line.trim()) return;
    let message;
    try { message = JSON.parse(line); }
    catch {
      console.log(JSON.stringify({ jsonrpc: '2.0', id: null,
        error: { code: -32700, message: 'Invalid JSON' } }));
      return;
    }
    const response = await handle(message);
    if (response) console.log(JSON.stringify(response));
  });
  lines.on('close', () => coach({ id: 0, method: 'tools/call', params: { name: 'coach_out' } }));
  console.error('Oskiewar coach + workshop + Jev ready.');
}
