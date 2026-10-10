#!/usr/bin/env node
// A Whistlegraph turn worker (apple/whistlegraph/TURNS.md, slice 2).
//
// Claims turns from the queue, runs the model loop with the engine's own
// modules, checks the source, commits the version to the thread ledger, and
// moves on. Nothing here knows about the phone. No pixels yet: every version
// this worker saves is marked unreviewed; slice 3 adds the paint check and
// the picture review in a worker-thread isolate.
//
// Env: MONGODB_CONNECTION_STRING, MONGODB_NAME, WHISTLEGRAPH_WORKER_SECRET,
//      AC_SITE (default https://aesthetic.computer), WORKER_NAME, TURN_CONCURRENCY (default 4)
import {MongoClient} from 'mongodb';
import {mkdtempSync, readFileSync, writeFileSync, rmSync} from 'node:fs';
import {join} from 'node:path';
import {tmpdir, hostname} from 'node:os';
import {mongoTurnQueue} from '../system/backend/whistlegraph-turns.mjs';
import {mongoWhistlegraphStore, sourceHash} from '../system/backend/whistlegraph.mjs';
import {AcServer} from '../aesel/src/ac-server.mjs';
import {sourceChecks, compileEditContract, runEditExperiment, validateCandidate} from '../aesel/src/edit-contract.mjs';
import {selectedBranch} from '../apple/whistlegraph/Resources/Web/branch-context.mjs';
import {GENERATION_INSTRUCTIONS, generationProfile} from '../apple/whistlegraph/Resources/Web/generation-policy.mjs';
import {WARE_INSTRUCTIONS} from '../apple/whistlegraph/Resources/Web/wares.mjs';
import {inferenceRequest} from '../apple/whistlegraph/Resources/Web/inference-input.mjs';
import {reviewVisualResult} from '../apple/whistlegraph/Resources/Web/visual-review.mjs';
import {RenderPool} from './whistlegraph-render.mjs';

const SITE = process.env.AC_SITE || 'https://aesthetic.computer';
const WORKER = process.env.WORKER_NAME || `${hostname()}:${process.pid}`;
const CONCURRENCY = Math.max(1, Number(process.env.TURN_CONCURRENCY) || 4);
const RENDERERS = Math.max(1, Number(process.env.RENDER_CONCURRENCY) || 2);
const HEARTBEAT_MS = 30_000;
const BASE_PIECE = 'export function paint({wipe}) { wipe("black"); }';
const log = (...args) => console.log(new Date().toISOString(), `[${WORKER}]`, ...args);

// The engine's fetch, minus the phone: the worker secret names the owner, and
// max_tokens follows the profile like the app's makeServer does.
function workerFetch(owner, settings) {
  return async (url, options) => {
    const body = JSON.parse(options.body); body.max_tokens = settings.maxTokens;
    const headers = {...(options.headers || {}), 'x-ac-worker': process.env.WHISTLEGRAPH_WORKER_SECRET, 'x-ac-owner': owner};
    delete headers.Authorization; delete headers.authorization;
    return fetch(url, {...options, headers, body: JSON.stringify(body)});
  };
}

// One turn: the same contract the phone compiles, the same model loop, the
// same single repair, the same picture review — on a render pool instead of
// the phone's preview. Returns {source, findings, repairs, notes, acceptance, checks}.
const REPAIR_NOTE = (findings) => '\n\nREPAIR THIS CANDIDATE ONCE. Preserve the edit contract; fix only these checks, inspect the resulting preview, and do not claim visual acceptance:\n' + JSON.stringify(findings);
const runtimeLevel = l => l.level === 'warn' && /^(?:Type|Reference|Range|Syntax)?Error\b|\bat paint \(/.test(l.text) ? 'error' : l.level;
export async function runTurn({job, thread, pool, onCheckpoint = () => {}, signal, log = () => {}}) {
  const head = thread.ledger?.versions.find(v => v.id === thread.ledger.head);
  if (!head || head.id !== job.baseVersion || sourceHash(head.source) !== job.baseHash) throw Object.assign(Error('The piece moved on before this turn ran'), {code: 'moved'});
  const cwd = mkdtempSync(join(tmpdir(), 'wg-turn-'));
  const file = join(cwd, 'whistlegraph.mjs');
  try {
    let source = head.source || BASE_PIECE;
    writeFileSync(file, source.endsWith('\n') ? source : source + '\n');
    const chalk = job.request.drawing && pool ? await pool.chalkImage(job.request.drawing).catch(error => { log('chalk image failed', error.message); return null; }) : null;
    const prompt = compileEditContract({request: job.request.text, ...selectedBranch(thread.ledger), source}) +
      '\nCurrent preview: 390 × 520 CSS points. Compose for this shape using screen.width and screen.height; keep subjects within the canvas and remain responsive when it resizes.';
    const settingsFor = repair => generationProfile('', {repair, model: job.model || '', image: !!job.request.drawing});
    let completed = false, error = '', server = null, checkpointAt = 0, renders = 0, lastPainted = null, repairs = 0;
    const checks = [], notes = [];
    const generate = async (task, repair) => {
      const settings = settingsFor(repair);
      server = new AcServer({cwd, piece: {file, checkpoint: async () => { source = readFileSync(file, 'utf8'); const now = Date.now(); if (now - checkpointAt > 5000) { checkpointAt = now; await onCheckpoint(source); } }},
        token: async () => 'worker', fetch: workerFetch(job.owner, settings), site: SITE, preview: false, frameCapture: false, model: settings.model,
        rounds: settings.rounds, outputContinuations: settings.outputContinuations, reasoning: settings.reasoning, thinking: settings.thinking,
        developerInstructions: GENERATION_INSTRUCTIONS + '\n' + WARE_INSTRUCTIONS});
      completed = false; error = '';
      server.on('notification', ({method, params}) => {
        if (method === 'turn/completed') { completed = !params.turn.error && params.turn.status === 'completed'; error = params.turn.error ? String(params.turn.error.message || params.turn.error) : ''; }
      });
      signal?.addEventListener('abort', () => server?.interrupt(), {once: true});
      await server.startTurn(chalk ? [{type: 'text', text: task}, chalk] : task);
      source = readFileSync(file, 'utf8');
      return completed;
    };
    // The paint check: the candidate's own painted event for its exact hash,
    // plus the runtime's console, judged by the same validator the phone uses.
    const inspect = async () => {
      const hash = sourceHash(source);
      if (!pool) { const findings = sourceChecks(source); return {passed: findings.length === 0, findings, sourceHash: hash, proof: null}; }
      const proof = await pool.render({source, renderID: ++renders});
      proof.logs = proof.logs.map(l => ({...l, level: runtimeLevel(l)}));
      const validation = validateCandidate(source, {rendered: proof.rendered, sourceHash: proof.sourceHash, logs: proof.logs}, hash);
      if (proof.rendered && validation.passed) lastPainted = {source, proof};
      log('paint check', job.code, proof.rendered ? 'painted' : 'not painted', validation.findings.map(f => f.code).join(',') || 'clean');
      return {...validation, proof};
    };
    const result = await runEditExperiment({prompt, generate, inspect, cancelled: () => signal?.aborted === true, onRepair: () => { repairs = 1; log('repairing', job.code); }});
    if (result.cancelled) throw Object.assign(Error('Stopped'), {code: 'cancelled'});
    if (!result.completed) throw Object.assign(Error(error || 'The model did not finish'), {draft: source !== head.source ? source : null});
    if (source.trim() === (head.source || '').trim()) throw Error('No change to the piece');
    let candidate = source, proof = result.validation?.proof || null, findings = result.validation?.findings || [];
    if (!result.validation?.passed) {
      // A repair that broke the picture does not cost the work: the last candidate that painted is kept, with a note.
      if (lastPainted && lastPainted.source !== candidate) { candidate = lastPainted.source; proof = lastPainted.proof; findings = []; notes.push('The last repair did not paint; kept the last picture that did.'); }
      else throw Object.assign(Error('Nothing painted: ' + findings.map(f => f.code).join(', ')), {draft: source});
    }
    if (!pool) return {source: candidate, findings, repairs, notes, acceptance: 'unreviewed', checks: ['source-only']};
    // The picture review, as on the phone: one repair if none was spent, the
    // reviewer being unavailable is not a verdict, and findings left after the
    // repair are notes on the saved version rather than a veto.
    const review = (src, prf) => reviewVisualResult({evidence: {sourceHash: prf.sourceHash, renderID: prf.renderID, frames: prf.frames}, sourceHash: prf.sourceHash, renderID: prf.renderID, source: src,
      request: inferenceRequest(job.request.text), history: selectedBranch(thread.ledger), drawing: chalk, model: settingsFor(false).model, token: 'worker', fetch: workerFetch(job.owner, settingsFor(false)), signal});
    let acceptance = 'unreviewed';
    try {
      let verdict = await review(candidate, proof); checks.push(verdict.passed ? 'visual-pass' : 'visual-fail');
      log('picture review', job.code, verdict.passed ? 'passed' : 'findings: ' + verdict.findings.join('; ').slice(0, 200));
      if (!verdict.passed && repairs === 0 && candidate === source) {
        repairs = 1; log('repairing picture', job.code);
        const before = {source: candidate, proof};
        const ok = await generate(prompt + REPAIR_NOTE(verdict.findings), true);
        const again = ok ? await inspect() : null;
        if (again?.passed) { candidate = source; proof = again.proof; verdict = await review(candidate, proof); checks.push(verdict.passed ? 'visual-pass' : 'visual-fail'); }
        else { candidate = before.source; proof = before.proof; notes.push('The repair did not paint; kept the picture before it.'); }
      }
      acceptance = verdict.passed ? 'reviewed' : 'reviewed-with-notes';
      if (!verdict.passed) notes.push(...verdict.findings.slice(0, 4));
    } catch (reviewError) {
      if (signal?.aborted) throw reviewError;
      checks.push('visual-unreviewed'); notes.push('Review unavailable: ' + String(reviewError.message || reviewError).slice(0, 160));
      log('picture review unavailable', job.code, reviewError.message);
    }
    return {source: candidate, findings, repairs, notes, acceptance, checks};
  } finally { rmSync(cwd, {recursive: true, force: true}); }
}

// Append the version to the thread, as the phone's commit does. store.save
// refuses if the revision moved, so a race with the phone cannot fork.
export async function commitVersion(store, job, thread, source, notes, {acceptance = 'unreviewed', checks = []} = {}) {
  const ledger = thread.ledger;
  const id = Math.max(...ledger.versions.map(v => v.id)) + 1;
  const version = {id, parent: ledger.head, source, request: job.request.text, createdAt: new Date().toISOString(), layers: 0};
  const saved = await store.save(job.owner, thread._id, thread.revision, {...ledger, head: id, versions: [...ledger.versions, version]});
  if (!saved) throw Object.assign(Error('The piece moved on while the turn ran'), {code: 'moved'});
  return {versionID: id, revision: saved.revision, sourceHash: sourceHash(source), notes, acceptance, checks};
}

async function work(queue, store, job, pool) {
  let beat = setInterval(() => queue.heartbeat(job._id, WORKER).catch(() => {}), HEARTBEAT_MS);
  const started = Date.now();
  try {
    const thread = await store.read(job.owner, job.code);
    if (!thread) throw Object.assign(Error('Thread unavailable'), {code: 'moved'});
    const turn = await runTurn({job, thread, pool, onCheckpoint: source => queue.heartbeat(job._id, WORKER, source), log});
    const fresh = await store.read(job.owner, job.code);
    const result = await commitVersion(store, job, fresh, turn.source, [...turn.findings.map(f => f.code), ...turn.notes], {acceptance: turn.acceptance, checks: turn.checks});
    await queue.complete(job._id, WORKER, {...result, repairs: turn.repairs, elapsedMs: Date.now() - started});
    log('done', job.code, 'v' + result.versionID, turn.acceptance, `${Date.now() - started} ms`, turn.repairs ? 'after a repair' : '');
  } catch (error) {
    const draft = error.draft || job.checkpoint || null;
    await queue.fail(job._id, WORKER, error.message, {draft});
    log('failed', job.code, error.message);
  } finally { clearInterval(beat); }
}

export async function main() {
  for (const name of ['MONGODB_CONNECTION_STRING', 'MONGODB_NAME', 'WHISTLEGRAPH_WORKER_SECRET']) if (!process.env[name]) { console.error(`${name} is not set`); process.exit(2); }
  const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING, {serverSelectionTimeoutMS: 15000});
  await client.connect();
  const db = client.db(process.env.MONGODB_NAME);
  const queue = mongoTurnQueue(db.collection('walkieware-turns'));
  const store = mongoWhistlegraphStore(db.collection('walkieware-threads'));
  const pool = new RenderPool({site: SITE, size: RENDERERS, log});
  await pool.start();
  let running = 0, stopping = false;
  const stop = () => { stopping = true; log('stopping after current turns'); };
  process.on('SIGTERM', stop); process.on('SIGINT', stop);
  log('ready', `turns ${CONCURRENCY}, renderers ${RENDERERS}`, SITE);
  while (!stopping) {
    if (running >= CONCURRENCY) { await new Promise(r => setTimeout(r, 500)); continue; }
    let job = null;
    try { job = await queue.claim(WORKER); } catch (error) { log('claim failed', error.message); }
    if (!job) { await new Promise(r => setTimeout(r, 2000)); continue; }
    running++; log('claimed', job.code, job._id, `attempt ${job.attempts}`);
    work(queue, store, job, pool).finally(() => running--);
  }
  while (running > 0) await new Promise(r => setTimeout(r, 500));
  await pool.close();
  await client.close();
}
if (process.argv[1] && import.meta.url === new URL(`file://${process.argv[1]}`).href) main().catch(error => { console.error(error); process.exit(1); });
