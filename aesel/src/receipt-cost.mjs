// Provider-reported USD, not the wallet tariff. Keep lifetime totals when the
// detailed receipt journal rolls over; each retained attempt replaces itself.
export class ReceiptCost {
  constructor(storage, key, rows, limit) {
    this.storage = storage; this.key = key + '-receipt-cost';
    this.value = {usd: 0, missing: 0, partial: rows.length >= limit, recent: {}};
    try {
      const saved = JSON.parse(storage.getItem(this.key));
      if (saved && Number.isFinite(saved.usd) && saved.usd >= 0 && Number.isInteger(saved.missing) && saved.missing >= 0 && saved.recent && typeof saved.recent === 'object') this.value = saved;
    } catch {}
    for (const row of rows) this.record(row.receipt, rows);
  }
  record(receipt, rows) {
    const next = {usd: 0, missing: 0};
    for (const round of receipt.rounds) {
      const cost = round.usage?.costUSD;
      if (Number.isFinite(cost) && cost >= 0) next.usd += cost;
      else if (round.httpStatus === null || round.httpStatus === undefined || round.httpStatus < 400) next.missing++;
    }
    const previous = this.value.recent[receipt.id] || {usd: 0, missing: 0};
    this.value.usd = Math.max(0, this.value.usd + next.usd - previous.usd);
    this.value.missing = Math.max(0, this.value.missing + next.missing - previous.missing);
    this.value.recent[receipt.id] = next;
    const retained = new Set(rows.map(row => row.receipt.id));
    for (const id of Object.keys(this.value.recent)) if (!retained.has(id)) delete this.value.recent[id];
    try { this.storage.setItem(this.key, JSON.stringify(this.value)); } catch {}
  }
  snapshot() { return {usd: this.value.usd, partial: !!this.value.partial || this.value.missing > 0}; }
}
