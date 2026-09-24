// Optional score.brightness = [{ t: seconds, percent: 0..100, seat?: 0..5 }].
// Hardware adjusts in roughly 5% steps; this is a backlight cue, not DMX.
export function brightnessAt(events, time, seat, fallback = 100) {
  if (!Array.isArray(events) || !Number.isFinite(time)) return fallback;
  const valid = events.filter(e => Number.isFinite(e?.t) && e.t >= 0 &&
    Number.isFinite(e.percent) && e.percent >= 0 && e.percent <= 100 &&
    (e.seat === undefined || e.seat === seat)).sort((a, b) => a.t - b.t);
  let value = fallback;
  for (const event of valid) {
    if (event.t > time) break;
    value = event.percent;
  }
  return value;
}

export function createScoreBrightness() {
  let last = -Infinity, lastRead = -Infinity, lastReport = -Infinity;
  let events = [], seat, override = 0, mode = 'audio', envelope = 0, accumulatedPeak = 0;
  let threshold = 0.004, release = 0.3;
  function controls(system) {
    try {
      const control = JSON.parse(system.readFile('/pieces/performance-controls.json'));
      if (control.brightnessPercent === null) override = null;
      else if (Number.isFinite(control.brightnessPercent) && control.brightnessPercent >= 0 && control.brightnessPercent <= 100)
        override = control.brightnessPercent;
      if (['audio','score'].includes(control.brightnessMode)) mode = control.brightnessMode;
      if (Array.isArray(control.brightness)) events = control.brightness;
      if (Number.isFinite(control.audioThreshold) && control.audioThreshold > 0) threshold = control.audioThreshold;
      if (Number.isFinite(control.brightnessRelease) && control.brightnessRelease >= 0.1 && control.brightnessRelease <= 3) release = control.brightnessRelease;
    } catch {}
  }
  return {
    boot(system) {
      try { seat = JSON.parse(system.readFile('/pieces/spatial-rehearsal-config.json')).seat; } catch {}
      controls(system);
      // Respect the existing override without a flash to maximum on reload.
      if (override === 0) system.brightnessAdjust?.(-10000);
      else if (override === 100) system.brightnessAdjust?.(10000);
    },
    update(system, now, scoreTime, amplitude = 0) {
      if (!Number.isFinite(now)) return;
      if (Number.isFinite(amplitude)) accumulatedPeak = Math.max(accumulatedPeak, amplitude);
      if (now - lastRead >= 0.25 || now < lastRead) { controls(system); lastRead = now; }
      if (now >= last && now - last < 0.05) return;
      const dt = Number.isFinite(last) && now >= last ? now - last : 0;
      last = now;
      const playing = Number.isFinite(scoreTime) && scoreTime >= 0;
      envelope = !playing ? 0 : accumulatedPeak >= threshold ? 1 : envelope * Math.exp(-dt / release);
      accumulatedPeak = 0;
      const musical = mode === 'audio' ? (envelope < 0.025 ? 0 : Math.round(envelope * 20) * 5) : brightnessAt(events, scoreTime, seat, 0);
      const requested = override ?? musical;
      const current = system.brightness;
      const supported = Number.isFinite(current) && current >= 0 && typeof system.brightnessAdjust === 'function';
      if (supported && Math.abs(requested - current) >= 3) {
        system.brightnessAdjust(requested === 100 ? 10000 : requested === 0 ? -10000 : Math.round((requested - current) / 5));
      }
      if (now - lastReport >= 0.25 || now < lastReport) {
        lastReport = now;
        system.writeFile('/pieces/brightness-status.json', JSON.stringify({
          requested, actual: current, supported, mode: override === null ? mode : 'override', at: now,
        }));
      }
    },
  };
}
