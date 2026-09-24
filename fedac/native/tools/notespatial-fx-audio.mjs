// Mono seat preview of audio.c's room -> glitch -> compressor -> wobble -> drive.
// Uses its delay taps, feedback, smoothing and saturation equations at the
// preview sample rate. Hardware gain, DAC latency and room acoustics are absent.
import { ribbon } from '../lib/spatial-rehearsal.mjs';
export const FX_KEYS = ['fxRoom', 'fxGlitch', 'fxWobble', 'fxDrive'];

export function renderSeatEffects(feed, score, seat, { sampleRate = 44100, from = 0 } = {}) {
  const fx = { ...score, ...(score.seatFx?.[seat] || {}) };
  if (!FX_KEYS.some(k => fx[k]?.length)) return feed;
  const room = new Float32Array(Math.ceil(sampleRate * .5)), delay = Math.floor(sampleRate * .12);
  const wobble = new Float32Array(1024), mixes = [0, 0, 0, 0], targets = [0, 0, 0, 0];
  let rp = 0, wp = 0, phase = 0, held = 0, holdCount = 0, comp = 0;
  const attack = 1 - Math.exp(-1 / (.0002 * sampleRate)), release = 1 - Math.exp(-1 / (.04 * sampleRate));
  for (let i = 0; i < feed.length; i++) {
    if (i % 128 === 0) FX_KEYS.forEach((k, j) => { targets[j] = Math.max(0, Math.min(1, ribbon(fx, k, from + i / sampleRate))); });
    for (let j = 0; j < 4; j++) mixes[j] += (targets[j] - mixes[j]) * .00005;
    const [rm, gm, wm, dm] = mixes;
    let v = feed[i];
    if (rm > .001) {
      const wet = room[(rp - delay + room.length) % room.length] * .5
        + room[(rp - 2 * delay + room.length) % room.length] * .3
        + room[(rp - 3 * delay + room.length) % room.length] * .2;
      room[rp] = Math.tanh((v + wet * .3) * .995 * .65) / .65;
      v = v * (1 - rm) + wet * rm;
    } else room[rp] = 0;
    rp = (rp + 1) % room.length;
    if (gm > .001) {
      const interval = 1 + Math.round((Math.floor(sampleRate / 1600) - 1) * gm * gm);
      if (++holdCount >= interval) {
        holdCount = 0;
        const levels = 2 ** Math.max(4, Math.min(12, 12 - Math.round(gm * 8)));
        held = Math.round(v * levels) / levels;
      }
      v = v * (1 - gm) + held * gm;
    }
    const peak = Math.abs(v);
    comp += (peak > comp ? attack : release) * (peak - comp);
    if (comp > .4) v *= (.4 + (comp - .4) * .125) / comp;
    const d = 96 + (Math.sin(phase) * .5 + .5) * 384;
    phase = (phase + Math.PI * 2 * .4 / 48000) % (Math.PI * 2);
    const pos = (wp - d + 1024) % 1024, p = Math.floor(pos), f = pos - p;
    const delayed = wobble[p] * (1 - f) + wobble[(p + 1) & 1023] * f;
    wobble[wp] = v + delayed * .45; wp = (wp + 1) & 1023;
    if (wm > .001) v = v * (1 - wm) + delayed * wm;
    if (dm > .001) v = v * (1 - dm) + Math.tanh(v * (1 + dm * 5)) * .8 * dm;
    if (!Number.isFinite(v)) throw Error(`Nonfinite FX output: seat ${seat}, sample ${i}`);
    feed[i] = v;
  }
  return feed;
}
