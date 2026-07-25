const MAX_RESIDENT_WINDOWS = 64;
const MAX_SAMPLES = 60;
const MIN_REPORTS = 12;
const MIN_SPAN_MS = 8_000;

function clamp(value, low = 0, high = 1) {
  return Math.max(low, Math.min(high, value));
}

function finite(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function normalizeSample(value, at) {
  const traits = value?.traits || value || {};
  return Object.freeze({
    at: Math.max(0, Math.floor(finite(at))), hp: clamp(finite(value?.hp), 0, 100),
    life: Math.max(0, Math.min(3, Math.floor(finite(value?.life)))),
    actual: clamp(finite(traits.actual)), variance: clamp(finite(traits.variance)),
    spatial: clamp(finite(traits.spatial)), noise: clamp(finite(traits.noise)),
    coherence: clamp(finite(traits.coherence)), muddiness: clamp(finite(traits.muddiness)),
    resolution: [32, 64, 128, 256].includes(Number(value?.resolution)) ? Number(value.resolution) : 64,
    sonicVoices: Math.max(0, Math.min(5, Math.floor(finite(value?.sonicVoices)))),
    role: String(value?.role || "resident").slice(0, 24),
  });
}

function average(values, key) {
  return values.reduce((sum, value) => sum + value[key], 0) / Math.max(1, values.length);
}

export class PieceVmPhenotypeOracle {
  constructor(stored = null) {
    this.windows = new Map();
    for (const [id, samples] of stored?.windows || []) {
      if (!/^[a-f0-9]{12}$/.test(String(id)) || !Array.isArray(samples)) continue;
      const normalized = samples.slice(-MAX_SAMPLES).map((sample) => normalizeSample(sample, sample?.at));
      if (normalized.length) this.windows.set(id, normalized);
    }
    while (this.windows.size > MAX_RESIDENT_WINDOWS) this.windows.delete(this.windows.keys().next().value);
  }

  static fromJSON(value) {
    return new PieceVmPhenotypeOracle(value);
  }

  ingest(rows, at = Date.now()) {
    const touched = new Set();
    for (const value of rows || []) {
      const id = String(value?.id || "");
      if (!/^[a-f0-9]{12}$/.test(id)) continue;
      const samples = this.windows.get(id) || [];
      samples.push(normalizeSample(value, at));
      if (samples.length > MAX_SAMPLES) samples.splice(0, samples.length - MAX_SAMPLES);
      this.windows.delete(id);
      this.windows.set(id, samples);
      touched.add(id);
    }
    while (this.windows.size > MAX_RESIDENT_WINDOWS) this.windows.delete(this.windows.keys().next().value);
    return [...touched].map((id) => this.summary(id));
  }

  summary(id) {
    const samples = this.windows.get(String(id)) || [];
    if (!samples.length) return null;
    const reports = new Set(samples.map((value) => value.at)).size;
    const firstAt = samples[0].at, lastAt = samples.at(-1).at;
    const hpMean = average(samples, "hp");
    const viability = hpMean / 100;
    const continuity = samples.filter((value) => value.life === 0).length / samples.length;
    const actual = average(samples, "actual"), variance = average(samples, "variance");
    const spatial = average(samples, "spatial"), coherence = average(samples, "coherence");
    const noise = average(samples, "noise"), muddiness = average(samples, "muddiness");
    const score = clamp(viability * .40 + continuity * .15 + clamp(actual * 12) * .15 +
      clamp(spatial * 4) * .10 + coherence * .10 + clamp(variance * 4) * .10 -
      noise * .08 - muddiness * .05);
    const ready = reports >= MIN_REPORTS && lastAt - firstAt >= MIN_SPAN_MS;
    const resolutions = [...new Set(samples.map((value) => value.resolution))].sort((a, b) => a - b);
    return Object.freeze({
      id: String(id), samples: samples.length, reports, firstAt, lastAt,
      spanMs: lastAt - firstAt, ready, score,
      selectionBias: ready ? clamp((score - .5) * .12, -.06, .06) : 0,
      hpMean, continuity, actual, variance, spatial, coherence, noise, muddiness,
      resolutions, sonicVoices: samples.at(-1).sonicVoices,
      sonicVoiceMean: average(samples, "sonicVoices"), role: samples.at(-1).role,
    });
  }

  selectionBias(id) {
    return this.summary(id)?.selectionBias || 0;
  }

  snapshot() {
    return [...this.windows.keys()].map((id) => this.summary(id))
      .filter(Boolean).sort((left, right) => right.score - left.score || left.id.localeCompare(right.id));
  }

  toJSON() {
    return { schema: 1, windows: [...this.windows.entries()].map(([id, samples]) =>
      [id, samples.map((value) => ({ ...value }))]) };
  }
}

export const PIECE_VM_PHENOTYPE_LIMITS = Object.freeze({
  maxResidents: MAX_RESIDENT_WINDOWS, maxSamples: MAX_SAMPLES,
  minReports: MIN_REPORTS, minSpanMs: MIN_SPAN_MS,
});
