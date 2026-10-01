// Durable request journal. A crash may happen on either side of ledger commit.
export function readAttempt(storage, key) {
  try { return JSON.parse(storage.getItem(key + '-inflight') || 'null'); } catch { return null; }
}
export function saveAttempt(storage, key, attempt) {
  storage.setItem(key + '-inflight', JSON.stringify(attempt));
  return attempt;
}
export function claimAttempt(storage, key, ledger, manual = false) {
  const attempt = readAttempt(storage, key);
  if (!attempt) return null;
  if (ledger.versions.some(v => v.requestID === attempt.id)) {
    storage.removeItem(key + '-inflight');
    return null;
  }
  const head = ledger.versions.find(v => v.id === ledger.head);
  if (!head || attempt.parent !== head.id || attempt.baseSource !== head.source) return null;
  if (!manual && (attempt.status !== 'working' || attempt.retries >= 1)) return null;
  return saveAttempt(storage, key, {...attempt, retries: (attempt.retries || 0) + 1, status: 'working'});
}
