// A remix has one new v0 and a durable citation, never the source's private history.
export function remixRequest(ref) {
  if (!ref || typeof ref.code !== 'string' || !/^[\w~#$!@/.-]{1,160}$/.test(ref.code) ||
      !Number.isSafeInteger(ref.version) || ref.version < 0 || ref.version > 100000 ||
      typeof ref.handle !== 'string' || ref.handle.length > 65 ||
      !/^https:\/\/mime\.ac\/(?:p\/)?[A-Za-z0-9_%~.-]+$/.test(ref.url) ||
      !/^[a-f0-9]{64}$/.test(ref.hash)) throw Error('The original work could not be identified.');
  const original = (ref.handle ? ref.handle.replace(/\/$/, '') + '/' : '') + ref.code;
  return 'Remix\nINPUT DATA:\n' + JSON.stringify({transcript: `Stole from ${original} · v${ref.version}`, remix: ref});
}
export function remixOf(request) {
  try { return JSON.parse(request.split('\nINPUT DATA:\n')[1]).remix || null; } catch { return null; }
}
export function remixCaption(request) {
  try { return JSON.parse(request.split('\nINPUT DATA:\n')[1]).transcript || request; } catch { return request || ''; }
}
export function remixLedger(source, ref) {
  if (typeof source !== 'string' || !source.trim()) throw Error('The original work is empty.');
  return {format: 1, head: 0, versions: [{id: 0, parent: null, source, request: remixRequest(ref), createdAt: new Date().toISOString(), layers: 0}]};
}

// Rollback removes partially written values before restoring old values, so a
// full store cannot leave the active work half-swapped. The archive is retained.
export function replacePieceForRemix(storage, key, identity, ledger, source, next) {
  const suffixes = ['', '-versions', '-thread', '-cloud-revision', '-cloud-ledger', '-attempt', '-inflight', '-receipts', '-receipt-cost', '-draft'];
  const before = new Map(suffixes.map(suffix => [key + suffix, storage.getItem(key + suffix)]));
  const extras = Object.fromEntries(suffixes.filter(s => !['', '-versions', '-thread'].includes(s)).flatMap(s => before.get(key + s) === null ? [] : [[s, before.get(key + s)]]));
  const archive = 'whistlegraph-archive-' + identity.id;
  const previousArchive = storage.getItem(archive);
  storage.setItem(archive, JSON.stringify({identity, ledger, source, extras, archivedAt: new Date().toISOString()}));
  try {
    for (const name of before.keys()) storage.removeItem(name);
    storage.setItem(key, next.versions[0].source);
    storage.setItem(key + '-versions', JSON.stringify(next));
    // A new thread identity is reserved by the engine after reload.
  } catch (error) {
    for (const name of before.keys()) storage.removeItem(name);
    if (previousArchive === null) storage.removeItem(archive); else storage.setItem(archive, previousArchive);
    for (const [name, value] of before) if (value !== null) storage.setItem(name, value);
    throw error;
  }
}
