// Strict allowlist: no prompts, generated source, logs, audio or raw responses.
const id = v => typeof v === 'string' && /^[a-f0-9-]{36}$/i.test(v);
const hash = v => typeof v === 'string' && /^[a-f0-9]{64}$/.test(v) ? v : null;
const label = v => typeof v === 'string' && /^[a-zA-Z0-9_.:/-]{1,160}$/.test(v) ? v : null;
const count = v => Number.isSafeInteger(v) && v >= 0 && v <= 1e9 ? v : null;
const amount = v => Number.isFinite(v) && v >= 0 && v <= 1e9 ? v : null;
const date = v => typeof v === 'string' && v.length <= 40 && Number.isFinite(Date.parse(v)) ? new Date(v).toISOString() : null;
export function validateReceipt(r) {
  if (!r || JSON.stringify(r).length > 24000 || r.format !== 1 || !id(r.id) || !id(r.requestID) ||
      !['current', 'compiled', 'local'].includes(r.path) || !['completed', 'failed', 'interrupted', 'unchanged'].includes(r.status) ||
      !hash(r.parentHash) || count(r.parent) === null || !date(r.startedAt) || !date(r.finishedAt) ||
      !Array.isArray(r.rounds) || r.rounds.length > 32 || !Array.isArray(r.checks) || r.checks.length > 20) throw Error('Invalid attempt receipt');
  return {format: 1, id: r.id, requestID: r.requestID, provenance: 'client-observed', path: r.path, status: r.status,
    parent: r.parent, parentHash: r.parentHash, resultHash: hash(r.resultHash), model: label(r.model),
    startedAt: date(r.startedAt), finishedAt: date(r.finishedAt), elapsedMs: amount(r.elapsedMs),
    firstOutputMs: amount(r.firstOutputMs), firstMatchingPaintMs: amount(r.firstMatchingPaintMs),
    checkpoints: count(r.checkpoints), repairs: count(r.repairs), acceptance: 'unreviewed',
    observations: (Array.isArray(r.observations)?r.observations:[]).slice(-64).filter(o=>hash(o.sourceHash)&&count(o.renderID)!==null&&['painted','invalidated','console'].includes(o.kind)).map(o=>({sourceHash:o.sourceHash,renderID:o.renderID,kind:o.kind,level:['error','warn'].includes(o.level)?o.level:null,atMs:amount(o.atMs)})),
    checks: r.checks.map(c => ({code: label(c.code), sourceHash: hash(c.sourceHash)})).filter(c => c.code && c.sourceHash),
    rounds: r.rounds.map((v, index) => ({index, startedMs: amount(v.startedMs), headersMs: amount(v.headersMs), httpStatus: count(v.httpStatus),
      reportedModel: label(v.reportedModel), providerRequestID: label(v.providerRequestID), reasoning: v.reasoning === 'none' ? 'none' : null,
      thinking: v.thinking === 'disabled' ? 'disabled' : null, usage: v.usage ? {
        inputTokens: count(v.usage.inputTokens), outputTokens: count(v.usage.outputTokens), costUSD: amount(v.usage.costUSD),
        cacheReadTokens: count(v.usage.cacheReadTokens), cacheWriteTokens: count(v.usage.cacheWriteTokens), thinkingTokens: count(v.usage.thinkingTokens)
      } : null}))};
}
