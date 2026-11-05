// Fast path is limited to musical keys. Pending shortcuts keep their original
// ordering, and the normal frame path remains the cold-audio fallback.
export function fastNotepatKey(event, { ready, queued = 0, mode } = {}) {
  return !!ready && queued === 0 && mode !== "batched" && !event.ctrl && !event.alt && !event.meta &&
    /^keyboard:(down|up):([a-z0-9;\]' ]|shift)$/.test(event.name);
}

export function summarizeLatency(rows) {
  const stats = values => {
    const a = values.filter(Number.isFinite).sort((a, b) => a - b);
    const q = p => a.length ? a[Math.ceil(a.length * p) - 1] : null;
    return { count: a.length, p50: q(.5), p95: q(.95), p99: q(.99), max: q(1) };
  };
  return {
    inputToPieceMs: stats(rows.map(r => r.pieceAt - r.inputAt)),
    inputToBridgeMs: stats(rows.map(r => r.bridgeAt - r.inputAt)),
    inputToEstimatedOutputMs: stats(rows.map(r => Number.isFinite(r.estimatedOutputAt) ? r.estimatedOutputAt - r.inputAt : null)),
  };
}

// Explicit local-only probe. No keys, notes, audio, device identity or uploads.
export function createNotepatLatency({ mode = "fast", limit = 512, now = () => performance.timeOrigin + performance.now() } = {}) {
  let serial = 0;
  const pending = new Set(), rows = [];
  return {
    mode, rows,
    begin() {
      if (pending.size + rows.length >= limit) return null;
      const id = ++serial; pending.add(id);
      return { id, mode: this.mode, inputAt: now() };
    },
    record(row) {
      if (!pending.delete(row.id)) return; // Composite voices count once.
      rows.push(row);
    },
    report() { return { schema: 1, app: "notepat", mode: this.mode, attempted: serial,
      missing: pending.size, samples: rows.slice(), summary: summarizeLatency(rows) }; },
  };
}

// Run on an already activated, visible Notepat tab with the sine sound selected.
// Timing starts inside the page; browser automation/RPC time is excluded.
export async function benchmarkNotepat({ count = 40, mode = "fast", holdMs = 60, stallMs = 0 } = {}) {
  if (document.visibilityState !== "visible" || !window.audioWorkletReady) throw Error("Activate audio in a visible Notepat tab first");
  if (!Number.isInteger(count) || count < 1 || count > 200 || !["fast", "batched"].includes(mode) || holdMs < 20 || holdMs > 500 || stallMs < 0 || stallMs > 50) throw Error("Invalid benchmark options");
  const previous = window.acNotepatLatency;
  const probe = window.acNotepatLatency = createNotepatLatency({ mode });
  const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
  const key = type => window.dispatchEvent(new KeyboardEvent(type, { key: "c", code: "KeyC", bubbles: true }));
  try {
    for (let i = 0; i < count; i++) {
      key("keydown");
      if (stallMs) { const until = performance.now() + stallMs; while (performance.now() < until) {} }
      await sleep(holdMs); key("keyup"); await sleep(37 + (i * 7) % 23);
    }
    await sleep(250);
    return { ...probe.report(), synthetic: true, holdMs, stallMs,
      revision: await fetch('/.commit-ref', { cache: 'no-store' }).then(r => r.ok ? r.text() : null).then(s => /^[a-f0-9]{40}$/.test(s?.trim()) ? s.trim() : null).catch(() => null),
      viewport: { width: innerWidth, height: innerHeight }, userAgent: navigator.userAgent };
  } finally { key("keyup"); window.acNotepatLatency = previous; }
}
