// Expose current names without duplicating a phone's entire archive. A
// partially completed old migration may contain both; the current name wins.
// A completed migration's old recovery copies must not resurrect deleted work.
export function migrateLegacyStorage(storage) {
  const legacy = storage.getItem('whistlegraph-storage-migrated') !== '1';
  // A partial old migration can hold a record under both names. The current
  // name wins, so the old copy is dead weight against WebKit's 5 MB quota.
  if (legacy) for (const key of Array.from({length: storage.length}, (_, i) => storage.key(i)))
    if (key?.startsWith('walkieware-') && storage.getItem('whistlegraph-' + key.slice('walkieware-'.length)) !== null) storage.removeItem(key);
  const oldKey = key => legacy && key.startsWith('whistlegraph-') ? 'walkieware-' + key.slice('whistlegraph-'.length) : null;
  const physicalKey = key => {
    const old = oldKey(key);
    return storage.getItem(key) === null && old && storage.getItem(old) !== null ? old : key;
  };
  const keys = () => [...new Set(Array.from({length: storage.length}, (_, i) => storage.key(i))
    .filter(key => key && (legacy || !key.startsWith('walkieware-')))
    .map(key => legacy && key.startsWith('walkieware-') ? 'whistlegraph-' + key.slice('walkieware-'.length) : key))];
  return {
    get length() { return keys().length; },
    key(index) { return keys()[index] ?? null; },
    getItem(key) { return storage.getItem(physicalKey(String(key))); },
    setItem(key, value) { storage.setItem(physicalKey(String(key)), String(value)); },
    removeItem(key) {
      key = String(key);
      storage.removeItem(key);
      const old = oldKey(key);
      if (old) storage.removeItem(old);
    },
    clear() { storage.clear(); },
  };
}

// The last cloud ledger used to be kept whole beside the ledger itself, in
// the open piece and inside every parked piece's extras. Each copy becomes
// a mark. On a phone with 5 MB this is the difference between parking a
// piece and "no room" (2026-10-10: 5.3 MB used, half of it copies).
export function compactLedgerCopies(storage, mark) {
  let freed = 0;
  for (const key of Array.from({length: storage.length}, (_, i) => storage.key(i))) {
    if (!key) continue;
    try {
      if (key.endsWith('-cloud-ledger')) {
        const value = storage.getItem(key);
        if (value && !value.startsWith('mark:')) { const next = mark(JSON.parse(value)); storage.setItem(key, next); freed += value.length - next.length; }
      } else if (key.startsWith('whistlegraph-archive-')) {
        const value = storage.getItem(key); const saved = JSON.parse(value);
        const copy = saved?.extras?.['-cloud-ledger'];
        if (typeof copy === 'string' && !copy.startsWith('mark:')) { saved.extras['-cloud-ledger'] = mark(JSON.parse(copy)); const next = JSON.stringify(saved); storage.setItem(key, next); freed += value.length - next.length; }
      }
    } catch {}
  }
  return freed;
}
