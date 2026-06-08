// Melody Transport — a shared musical clock (BPM + origin) that both the melody
// scheduler and the KidLisp timer engine read, so visual timers can lock to and
// syncopate against the groove.
//
// Created lazily by the first `(melody …)` in a piece (that melody becomes the
// "master"). The `b` timing unit divides `beatMs`; the `beat`/`beatphase`
// KidLisp variables expose the running position. `s`/`f` timers keep wall-clock
// meaning but phase-align to `startMs` via the timer engine's offset hook.
//
// Per-context (a KidLisp instance owns `this.transport`; clock.mjs could own a
// module-level one) so embedded/independent pieces don't share transport state.

export const DEFAULT_BPM = 120;

const _now = () =>
  typeof performance !== "undefined" ? performance.now() : Date.now();

export function createTransport({
  bpm = DEFAULT_BPM,
  startMs = _now(),
  timeSignature = "4/4",
} = {}) {
  const beatsPerMeasure = parseInt(String(timeSignature).split("/")[0], 10) || 4;
  return {
    startMs,
    bpm,
    beatMs: 60000 / bpm,
    timeSignature,
    beatsPerMeasure,

    // Change tempo while keeping the current beat phase continuous.
    setBpm(nextBpm, now = _now()) {
      if (!Number.isFinite(nextBpm) || nextBpm <= 0) return;
      const beat = this.getBeat(now);
      this.bpm = nextBpm;
      this.beatMs = 60000 / nextBpm;
      this.startMs = now - beat * this.beatMs; // re-anchor so phase doesn't jump
    },

    // Running float beat count since origin (monotonic). e.g. (spin (* 2 beat)).
    getBeat(now = _now()) {
      return (now - this.startMs) / this.beatMs;
    },

    // 0..1 sawtooth within the current beat. e.g. (scroll 0 (* 10 beatphase)).
    getPhase(now = _now()) {
      const b = this.getBeat(now);
      return b - Math.floor(b);
    },

    // 0..1 within the current measure.
    getMeasurePhase(now = _now()) {
      const b = this.getBeat(now);
      const m = b % this.beatsPerMeasure;
      return m / this.beatsPerMeasure;
    },
  };
}
