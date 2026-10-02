// Content-free, client-observed receipts. They are not authoritative billing.
export const RECEIPT_LIMIT = 100;
const number = n => Number.isFinite(n) && n >= 0 ? n : null;
const label = s => typeof s === 'string' && /^[a-zA-Z0-9_.:/-]{1,160}$/.test(s) ? s : null;
export async function hashSource(source) {
  const hash = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(source));
  return [...new Uint8Array(hash)].map(b => b.toString(16).padStart(2, '0')).join('');
}
export class ReceiptJournal {
  constructor(storage, key) {
    this.storage = storage; this.key = key + '-receipts'; this.rows = [];
    try { const rows = JSON.parse(storage.getItem(this.key) || '[]'); if (Array.isArray(rows)) this.rows = rows.filter(r=>r?.receipt?.format===1 && Array.isArray(r.receipt.rounds)).slice(-RECEIPT_LIMIT); } catch {}
    for (const row of this.rows) if (row.receipt.status === 'running') {
      Object.assign(row.receipt, {status: 'interrupted', finishedAt: new Date().toISOString(), elapsedMs: null}); row.pending = true;
    }
    this.persist();
  }
  persist() { try { this.storage.setItem(this.key, JSON.stringify(this.rows)); } catch { /* Generation must survive a full storage quota. */ } }
  save(receipt) {
    const row = {receipt: structuredClone(receipt), pending: receipt.status !== 'running'};
    const index = this.rows.findIndex(r => r.receipt.id === receipt.id);
    if (index < 0) this.rows.push(row); else this.rows[index] = row;
    this.rows = this.rows.slice(-RECEIPT_LIMIT); this.persist();
  }
  pending() { return this.rows.find(r => r.pending)?.receipt ?? null; }
  acknowledge(id) { const row = this.rows.find(r => r.receipt.id === id); if (row) { row.pending = false; this.persist(); } }
}
export class AttemptReceipt {
  constructor({requestID, parent, parentHash, path, model, journal, now = () => performance.now()}) {
    this.now = now; this.start = now(); this.journal = journal;
    this.value = {format: 1, id: crypto.randomUUID(), requestID, provenance: 'client-observed', parent, parentHash, resultHash: null,
      path, model, startedAt: new Date().toISOString(), finishedAt: null, elapsedMs: null, status: 'running',
      firstOutputMs: null, firstMatchingPaintMs: null, checkpoints: 0, repairs: 0, acceptance: 'unreviewed', rounds: [], checks: [], observations: []};
    this.save();
  }
  elapsed() { return Math.round(this.now() - this.start); }
  save() { this.journal.save(this.value); }
  request() {
    const round = {index: this.value.rounds.length, startedMs: this.elapsed(), headersMs: null, httpStatus: null, reportedModel: null, providerRequestID: null, usage: null,
      reasoning: 'none', thinking: 'disabled'};
    this.value.rounds.push(round); this.save(); return round;
  }
  headers(round, response) { Object.assign(round, {headersMs: this.elapsed(), httpStatus: response.status, providerRequestID: label(response.headers?.get?.('x-request-id'))}); this.save(); }
  notify(method, params) {
    const round = this.value.rounds.at(-1);
    if (!['item/modelCode/delta', 'item/agentMessage/delta', 'model/reported', 'turn/usage'].includes(method)) return;
    if (['item/modelCode/delta', 'item/agentMessage/delta'].includes(method)) {
      if(this.value.firstOutputMs!==null)return;
      this.value.firstOutputMs=this.elapsed();
    }
    if (round && method === 'model/reported') { round.reportedModel = label(params.reported); round.providerRequestID = label(params.providerRequestID) || round.providerRequestID; }
    if (round && method === 'turn/usage') {
      const u = params.usage || {};
      // Replace the round receipt; duplicate delivery never doubles totals.
      round.usage = {inputTokens: number(u.input_tokens), outputTokens: number(u.output_tokens), costUSD: number(u.cost),
        cacheReadTokens: number(u.cache_read_input_tokens), cacheWriteTokens: number(u.cache_creation_input_tokens),
        thinkingTokens: number(u.output_tokens_details?.thinking_tokens)};
    }
    this.save();
  }
  painted() { this.value.firstMatchingPaintMs ??= this.elapsed(); this.save(); }
  observe(event) {
    if(!['painted','invalidated','console'].includes(event.kind))return;
    if(event.kind==='console'&&!['error','warn'].includes(event.event?.level))return;
    this.value.observations.push({sourceHash:event.sourceHash,renderID:event.requestID,kind:event.kind,level:event.event?.level||null,atMs:this.elapsed()});
    this.value.observations=this.value.observations.slice(-64);this.save();
  }
  finish(status, resultHash, checks = []) {
    Object.assign(this.value, {status, resultHash, checks, finishedAt: new Date().toISOString(), elapsedMs: this.elapsed()}); this.save();
  }
}
