// No per-frame traffic. Summaries cover visible play, including other players'
// builds. No prompt text, input events, owner identity or browser fingerprint.
export class ClientDiagnostics {
  constructor(send, now = () => performance.now()) {
    this.send = send;
    this.now = now;
    this.enabled = false;
    this.reset();
  }
  reset() {
    this.at = this.now();
    this.lastFrame = this.lastSnapshot = null;
    this.ids = new Set();
    this.values = { frames: 0, frameGapMaxMs: 0, frameWorkMaxMs: 0, renderMaxMs: 0,
      snapshotGapMaxMs: 0, longTasks: 0, longTaskMaxMs: 0, frameErrors: 0, messageMaxMs: 0,
      reconcileMaxMs: 0, pendingCommandsMax: 0, snapshotsCoalesced: 0 };
  }
  max(key, value) {
    if (this.enabled && Number.isFinite(value)) this.values[key] = Math.max(this.values[key] || 0, value);
  }
  jobs(ids) { if (this.enabled) for (const id of ids) if (this.ids.size < 12) this.ids.add(id); }
  snapshot() {
    if (!this.enabled) return;
    const now = this.now();
    if (this.lastSnapshot !== null) this.max('snapshotGapMaxMs', now - this.lastSnapshot);
    this.lastSnapshot = now;
  }
  longTask(ms) {
    if (!this.enabled) return;
    this.values.longTasks++;
    this.max('longTaskMaxMs', ms);
  }
  frame({ visible, workMs, renderMs, backend, objects, triangles, rttMs, jitterMs, queuedBytes, buildIds = [] }) {
    if (!this.enabled) return;
    if (!visible) { this.reset(); return; }
    const now = this.now();
    this.jobs(buildIds);
    if (this.lastFrame !== null) this.max('frameGapMaxMs', now - this.lastFrame);
    this.lastFrame = now;
    this.values.frames++;
    this.max('frameWorkMaxMs', workMs);
    this.max('renderMaxMs', renderMs);
    if (now - this.at < 5000) return;
    this.send({ type: 'diagnostic', ...this.values, durationMs: now - this.at,
      buildIds: [...this.ids], backend, objects, triangles, rttMs, jitterMs, queuedBytes });
    this.reset();
    this.lastFrame = now;
  }
}
