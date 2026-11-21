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
  // 📶 How shoddy the line is, 0 to 1: the room's word late, arriving
  // unevenly, or the echo slow or wobbling. 0 until the room has spoken.
  shoddy(now) {
    if (this.lastSnapshot === null) return 0;
    const clamp = (n) => (n < 0 ? 0 : n > 1 ? 1 : n);
    const late = clamp((now - this.lastSnapshot - 120) / 1500);
    const uneven = clamp((this.arrivalJitter - 12) / 60);
    const slow = this.rtt === null ? 0 : clamp((this.rtt - 180) / 400);
    const wobble = clamp((this.jitter - 30) / 150);
    return Math.max(late, 0.6 * uneven, 0.4 * slow, 0.5 * wobble);
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
