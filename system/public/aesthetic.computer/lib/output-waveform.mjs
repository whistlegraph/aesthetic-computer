// A lazy, silent tap of the piece's speaker outputs. No audio is rerouted.
export function createOutputWaveform(getSources) {
  const taps = new Map();
  const read = (sampleCount = 128) => {
    const count = Number.isFinite(sampleCount) ? Math.max(16, Math.min(512, Math.round(sampleCount))) : 128;
    const sources = new Set(
      getSources().filter((source) => source?.context?.state === "running"),
    );
    for (const [source, tap] of taps) {
      if (sources.has(source)) continue;
      if (tap.owned) source.disconnect(tap.analyser);
      taps.delete(source);
    }
    if (!sources.size) return [];
    const mixed = new Array(count).fill(0);
    for (const source of sources) {
      let tap = taps.get(source);
      if (!tap) {
        const owned = typeof source.getFloatTimeDomainData !== "function";
        const analyser = owned ? source.context.createAnalyser() : source;
        if (owned) {
          analyser.fftSize = 2048;
          source.connect(analyser);
        }
        tap = { analyser, owned, samples: new Float32Array(analyser.fftSize) };
        taps.set(source, tap);
      }
      tap.analyser.getFloatTimeDomainData(tap.samples);
      for (let i = 0; i < mixed.length; i++)
        mixed[i] +=
          tap.samples[Math.floor((i * tap.samples.length) / mixed.length)];
    }
    return mixed.map((value) =>
      Number.isFinite(value) ? Math.max(-1, Math.min(1, value)) : 0,
    );
  };
  read.dispose = () => {
    for (const [source, tap] of taps)
      if (tap.owned) source.disconnect(tap.analyser);
    taps.clear();
  };
  return read;
}
