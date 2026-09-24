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
  let last = -Infinity, events = [], seat, override = 100;
  return {
    boot(system) {
      try { events = JSON.parse(system.readFile('/pieces/spatial-rehearsal.nsscore')).brightness || []; } catch {}
      try { seat = JSON.parse(system.readFile('/pieces/spatial-rehearsal-config.json')).seat; } catch {}
      system.brightnessAdjust?.(10000);
    },
    update(system, now, scoreTime) {
      if (!Number.isFinite(now) || now - last < 0.25) return;
      last = now;
      try {
        const control = JSON.parse(system.readFile('/pieces/performance-controls.json'));
        if (control.brightnessPercent === null) override = null;
        else if (Number.isFinite(control.brightnessPercent) && control.brightnessPercent >= 0 && control.brightnessPercent <= 100)
          override = control.brightnessPercent;
      } catch {}
      const requested = override ?? brightnessAt(events, scoreTime, seat);
      const current = system.brightness;
      const supported = Number.isFinite(current) && current >= 0 && typeof system.brightnessAdjust === 'function';
      if (supported && Math.abs(requested - current) >= 3) {
        system.brightnessAdjust(requested === 100 ? 10000 : Math.round((requested - current) / 5));
      }
      // Readback is the runtime's sampled value; adjustment appears next update.
      system.writeFile('/pieces/brightness-status.json', JSON.stringify({
        requested, actual: current, supported, mode: override === null ? 'score' : 'override', at: now,
      }));
    },
  };
}
