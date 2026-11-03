// Monotonic local-clock echo: server time and clock synchronization aren't needed.
export class Connection {
  constructor() {
    this.reset();
  }
  reset() {
    this.next = 0;
    this.pending = new Map();
    this.rtt = null;
    this.jitter = 0;
    this.lastReply = null;
    this.samples = 0;
    this.lastSnapshot = null;
    this.arrivalJitter = 0;
    this.lastTick = null;
  }
  probe(now, canSend = true) {
    for (const [id, at] of this.pending)
      if (now - at > 5000) this.pending.delete(id);
    if (!canSend || now < this.next) return null;
    this.next = now + 1000;
    this.pending.set(now, now);
    return { type: 'ping', at: now };
  }
  reply(at, now) {
    if (!this.pending.has(at)) return false;
    this.pending.delete(at);
    const trip = now - at;
    if (!Number.isFinite(trip) || trip < 0 || trip > 5000) return false;
    if (this.rtt === null) this.rtt = trip;
    else {
      this.jitter += (Math.abs(trip - this.rtt) - this.jitter) * 0.15;
      this.rtt += (trip - this.rtt) * 0.15;
    }
    this.samples++;
    this.lastReply = now;
    return true;
  }
  snapshot(tick, now, hz = 60) {
    if (
      this.lastTick !== null &&
      tick > this.lastTick &&
      now - this.lastSnapshot < 2000
    ) {
      const delta = Math.abs(
        now - this.lastSnapshot - ((tick - this.lastTick) * 1000) / hz,
      );
      this.arrivalJitter += (delta - this.arrivalJitter) * 0.1;
    }
    this.lastSnapshot = now;
    this.lastTick = tick;
  }
  state(now, open, queued = 0) {
    const stale = this.lastSnapshot !== null && now - this.lastSnapshot > 1000;
    const noEcho = this.lastReply !== null && now - this.lastReply > 3500;
    const stalled = stale || noEcho || queued >= 8192;
    const bars =
      !open || stalled || this.rtt === null
        ? 0
        : this.rtt < 50
          ? 4
          : this.rtt < 100
            ? 3
            : this.rtt < 170
              ? 2
              : 1;
    return {
      bars,
      label: !open
        ? 'OFFLINE'
        : stalled
          ? 'STALLED'
          : this.rtt === null
            ? '...'
            : `${Math.round(this.rtt)} MS`,
      rtt: this.rtt,
      jitter: this.jitter,
      arrivalJitter: this.arrivalJitter,
      samples: this.samples,
      queued,
      stalled,
    };
  }
}
