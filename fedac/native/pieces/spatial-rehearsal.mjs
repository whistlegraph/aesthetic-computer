// Spatial rehearsal, 26.09.18
// Local synthesis on every seat, coordinated over Wi-Fi. Microphones stay closed.
import { voicePosition, sourceGain, hasFocus, ringSeats, noteColor, ribbon } from '../lib/spatial-rehearsal.mjs';

let config, score, error = '', phase = 'ready', origin = null;
let lastStatus = -1, lastRead = -1, seenCommand = '';
let cursors = [], voices = [], mode = 'score', lastBeep = -1;
let identifyAt = null, glow = 0, flyCursors = [];
let beepCount = 0, outputPeak = 0, maxFrameGap = 0, previousTime = null;
let networkHalfRttMs = null, runId = null;
let presence = null, presenceRaw = '', presenceSeen = -Infinity, presencePoll = -Infinity;

function seatConnection(i, now) {
  if (!presence || now - presenceSeen > 5) return 'unknown';
  return presence.seats.find(s => s.seat === i)?.state || 'unknown';
}

function readJSON(system, path) {
  // readFile returns only the tail of large files; scores need the full bytes.
  if (path.endsWith('.nsscore') && system.readFileBytes) {
    const buffer = system.readFileBytes(path);
    if (!buffer) throw Error('score file missing');
    const bytes = new Uint8Array(buffer), chunks = [];
    for (let i = 0; i < bytes.length; i += 4096)
      chunks.push(String.fromCharCode.apply(null, bytes.subarray(i, i + 4096)));
    return JSON.parse(chunks.join(''));
  }
  return JSON.parse(system.readFile(path));
}
function runDuration() { return mode === 'beeps' ? 20 : (config?.maxSeconds || score?.dur || 20); }
function stopVoices() { for (const v of voices) v.voice?.kill?.(0.03); voices = []; }

// Effects a score may carry as dry/wet ribbons over its duration (fxRoom,
// fxDrive, fxWobble, fxGlitch), with per-seat overrides in score.seatFx[seat].
// Applied ten times a second to the engine's global mixes; every mix returns
// to zero when the piece stops, so a rehearsal never leaves a laptop wet.
const FX_UNITS = { fxRoom: 'room', fxDrive: 'drive', fxWobble: 'wobble', fxGlitch: 'glitch' };
let fxLast = {}, fxAt = -Infinity;
function applyFx(sound, t) {
  if (t - fxAt < 0.1) return;
  fxAt = t;
  const mine = score.seatFx?.[config.seat] || {};
  for (const [key, unit] of Object.entries(FX_UNITS)) {
    const arr = mine[key] || score[key];
    if (!arr?.length) continue;
    const v = Math.max(0, Math.min(1, ribbon({ dur: score.dur, [key]: arr }, key, t)));
    if (Math.abs((fxLast[key] ?? -1) - v) < 0.005) continue;
    fxLast[key] = v;
    sound[unit]?.setMix?.(v);
  }
}
function clearFx(sound) {
  for (const [key, unit] of Object.entries(FX_UNITS)) if (fxLast[key] > 0) sound[unit]?.setMix?.(0);
  fxLast = {}; fxAt = -Infinity;
}
const hasFx = () => !!(score && (score.seatFx || Object.keys(FX_UNITS).some(k => score[k]?.length)));

let standalone = false; // seat given on the command line: play the baked score on its own clock
export function boot({ system, sound, colon, params }) {
  sound.microphone.close();
  try {
    // `spatial-rehearsal:3` or `spatial-rehearsal:3:6` seats this laptop without the
    // controller (seat 3 of 6; the sixth seat is the held center) and starts the
    // baked score a few seconds after boot, for rehearsing one part alone.
    const arg = colon?.[0] ?? params?.[0];
    if (arg !== undefined && /^\d+$/.test(String(arg))) {
      const seats = +(colon?.[1] ?? params?.[1] ?? 6);
      config = { seat: +arg - 1, seats, machineName: 'ac-device', maxSeconds: 0 };
      standalone = true;
    } else {
      try { config = readJSON(system, '/pieces/spatial-rehearsal-config.json'); }
      catch (_) { throw Error('Assign a seat with the spatial controller, or jump to spatial-rehearsal:<seat>'); }
    }
    if (!Number.isInteger(config.seats) || config.seats < 2 || config.seats > 16) throw Error('seats must be 2–16');
    if (!Number.isInteger(config.seat) || config.seat < 0 || config.seat >= config.seats) throw Error('seat outside ensemble');
    score = readJSON(system, '/pieces/spatial-rehearsal.nsscore');
    if (!(score.dur > 0) || !score.lanes?.length) throw Error('invalid score');
    if (!config.maxSeconds) config.maxSeconds = score.dur;
    cursors = score.lanes.map(() => 0);
    system.startSSH?.();
  } catch (e) { error = e.message; phase = 'error'; }
}

export function sim({ sound, system, screen, wifi }) {
  const now = sound.time;
  if (standalone && origin === null && !error && now > 0) { origin = now + 3; phase = 'countdown'; mode = 'score'; runId = 'standalone'; }
  if (now - presencePoll > .25) {
    presencePoll = now;
    try {
      const raw = system.readFile('/pieces/spatial-rehearsal-presence.json');
      if (raw && raw !== presenceRaw) {
        const parsed = JSON.parse(raw);
        if (!Array.isArray(parsed.seats)) throw Error('invalid presence');
        presence = parsed; presenceRaw = raw; presenceSeen = now;
      }
    } catch (_) { /* Partial or missing presence expires to unknown. */ }
  }
  if (previousTime !== null) maxFrameGap = Math.max(maxFrameGap, now - previousTime);
  previousTime = now;
  outputPeak = Math.max(outputPeak, sound.speaker?.amplitudes?.left || 0, sound.speaker?.amplitudes?.right || 0);
  if (now - lastRead >= 0.01) {
    lastRead = now;
    let cmd;
    try { cmd = readJSON(system, '/pieces/spatial-rehearsal-command.json'); } catch (_) { /* retry incomplete PUT */ }
    if (cmd?.id && cmd.id !== seenCommand) {
      seenCommand = cmd.id;
      if (cmd.action === 'clock') {
        system.writeFile('/pieces/spatial-rehearsal-clock.json', JSON.stringify({ id: cmd.id, audioTime: now }));
      }
      if (cmd.action === 'identify') identifyAt = now + 1 + config.seat * 0.7;
      if (cmd.action === 'stop' || cmd.action === 'arm') {
        stopVoices(); clearFx(sound); origin = null; identifyAt = null;
        phase = error ? 'error' : 'ready';
      }
      if (cmd.action === 'prepare' && !error && Number.isFinite(cmd.startAt) && cmd.startAt > now + 1) {
        stopVoices(); clearFx(sound); origin = cmd.startAt; phase = 'prepared'; runId = cmd.id;
        mode = cmd.mode === 'beeps' ? 'beeps' : 'score';
        networkHalfRttMs = cmd.networkHalfRttMs;
        cursors = score.lanes.map(() => 0); flyCursors = []; lastBeep = -1; beepCount = 0; outputPeak = 0; maxFrameGap = 0;
      }
      if (cmd.action === 'play' && phase === 'prepared' && origin > now + 0.5) phase = 'countdown';
    }
  }
  if (identifyAt !== null && now >= identifyAt) {
    const voice = sound.synth({ type: 'sine', tone: 440 * Math.pow(2, config.seat / 5), volume: 0.4, duration: 0.35, attack: 0.01, decay: 0.06 });
    voices.push({ voice, end: -Infinity }); identifyAt = null;
  }
  if (!error && origin !== null && ['countdown', 'playing'].includes(phase)) {
    const t = now - origin;
    if (t >= 0) phase = 'playing';
    if (t >= runDuration()) {
      stopVoices(); clearFx(sound); phase = 'finished';
    } else if (t >= 0 && mode === 'beeps') {
      const beat = Math.floor(t);
      if (beat !== lastBeep) {
        lastBeep = beat;
        if (t - beat < 0.15) {
          const voice = sound.synth({ type: 'sine', tone: 660, volume: 0.6, duration: 0.20, attack: 0.008, decay: 0.04 });
          voices = [{ voice }]; beepCount++;
        }
      }
    } else if (t >= 0) {
      if (hasFx()) applyFx(sound, t);
      voices = voices.filter(v => {
        if (t >= v.end) return false;
        v.voice?.update?.({ volume: v.g * sourceGain(score, voicePosition(score, v.lane, t), config.seat, config.seats) });
        return true;
      });
      for (let i = 0; i < score.lanes.length; i++) {
        const events = score.lanes[i].events;
        while (cursors[i] < events.length && events[cursors[i]].t <= t) {
          const e = events[cursors[i]++];
          if (e.t + e.dur <= t || t - e.t > 0.1) continue;
          const g = Math.min(0.65, Math.max(0, e.g * (score.gain ?? 0.35)));
          const gain = sourceGain(score, voicePosition(score, i, t), config.seat, config.seats);
          const voice = sound.synth({ type: e.wave, tone: e.hz || 220,
            duration: e.t + e.dur - t, volume: g * gain, attack: e.attack ?? 0.01, decay: e.decay ?? 0.04,
            ...(Number.isInteger(e.gm) ? { gmProgram: e.gm } : {}) }); // a GM program when the score names one; `type` is its fallback
          voices.push({ voice, lane: i, end: e.t + e.dur, g });
        }
      }
    }
  }
  if (now - lastStatus >= 0.1) {
    lastStatus = now;
    system.writeFile('/pieces/spatial-rehearsal-status.json', JSON.stringify({
      machineName: config?.machineName || 'ac-device', ip: wifi?.ip || config?.ip || '', seat: config?.seat, seats: config?.seats, phase, mode, runId, error, command: seenCommand, audioTime: now,
      scoreTime: origin === null ? null : Math.min(runDuration(), now - origin), origin,
      microphone: { hot: sound.microphone.hot, recording: sound.microphone.recording },
      connectivity: { stale: now - presenceSeen > 5, seats: Array.from({length: config?.seats || 0}, (_, i) => ({ seat: i, state: seatConnection(i, now) })) },
      beepCount, outputPeak, maxFrameGap, networkHalfRttMs, glow, duration: runDuration(), scoreDuration: score?.dur, scoreName: score?.name, geometry: score?.geometry || 'ring', seatOrder: score?.seatOrder || Array.from({length:config?.seats || 0}, (_,i)=>i),
      sources: score ? score.lanes.map((lane, i) => {
        const t = origin === null ? 0 : Math.max(0, Math.min(runDuration(), now - origin));
        const position = voicePosition(score, i, t);
        return { lane: i, name: lane.name, color: lane.color, position,
          seatGain: sourceGain(score, position, config.seat, config.seats),
          active: mode === 'score' && phase === 'playing' && voices.some(v => v.lane === i && v.end > t),
        };
      }) : [],
      timing: 'Wi-Fi estimated audio clock; output latency and drift uncalibrated',
      battery: system.battery, screen: { width: screen.width, height: screen.height }, output: sound.speaker?.amplitudes,
    }));
  }
}

export function paint({ wipe, ink, box, line, circle, write, screen, sound, system, wifi }) {
  const amp = Math.max(0, sound.speaker?.amplitudes?.left || 0, sound.speaker?.amplitudes?.right || 0);
  const focusTime = origin === null ? 0 : Math.max(0, sound.time - origin);
  const focused = score && hasFocus(score, config.seat, config.seats, focusTime);
  glow = focused && amp > .002 ? Math.min(1, Math.sqrt(amp) * 2.8) : 0;
  const own = score?.seatColors?.[config?.seat] || [255, 226, 120];
  wipe(Math.round(12 + (own[0] * .3 - 12) * glow), Math.round(15 + (own[1] * .3 - 15) * glow), Math.round(23 + (own[2] * .3 - 23) * glow));
  const w = screen.width, h = screen.height;
  const battery = system?.battery;
  let batteryLabel = 'BAT --';
  if (battery?.percent >= 0) {
    batteryLabel = battery.percent + '%' + (battery.charging ? ' +' : '');
    if (!battery.charging && battery.minutesLeft > 0)
      batteryLabel += ' ' + Math.floor(battery.minutesLeft / 60) + 'h' + String(battery.minutesLeft % 60).padStart(2, '0');
  }
  ink(...(battery?.percent >= 0 && battery.percent <= 15 ? [255, 165, 120] : [240, 245, 250]));
  write(batteryLabel, { x: w - 8 - batteryLabel.length * 6, y: 8, font: '6x10', size: 1 });
  ink(210, 225, 240);
  write(config?.machineName || 'ac-device', { x: 75, y: 9, font: '6x10' });
  write(wifi?.ip || config?.ip || 'LAN unavailable', { x: 75, y: 26, font: '6x10' });
  if (error) { ink(255, 140, 130); write(error, { x: 8, y: 55 }); return; }
  if (!score) return;
  const t = origin === null ? -1 : Math.min(runDuration(), sound.time - origin);
  const isCenter = config.seat === score.center;

  // The view into the space: this laptop's notes come from far away as
  // frames in the screen's own aspect, growing as they approach, filling
  // the screen exactly when they sound, then fading with the note.
  const LOOK = 3, cx = w * .5, cy = h * .5;
  const frames = [];
  for (let i = 0; i < score.lanes.length; i++) {
    const lane = score.lanes[i], evs = lane.events;
    let j = flyCursors[i] || 0;
    while (j < evs.length && evs[j].t + evs[j].dur + .6 < t) j++;
    flyCursors[i] = j;
    for (let k = j; k < evs.length && evs[k].t <= t + LOOK; k++) {
      const e = evs[k];
      const gain = sourceGain(score, voicePosition(score, i, e.t), config.seat, config.seats);
      if (gain * gain < .5) continue;
      frames.push({ e, until: e.t - t });
    }
  }
  frames.sort((p, q) => q.until - p.until); // far first, so near frames draw on top
  for (const { e, until } of frames) {
    const base = noteColor(e.note) || own, sharp = e.note && e.note.includes('#');
    const sounding = until <= 0, held = Math.max(.3, e.dur), left = sounding ? Math.max(0, 1 - (-until) / held) : 1;
    // approaching: grows on a square law; hit: one short blink; then it reverses and recedes over the note
    const near = sounding ? left : 1 - Math.max(0, Math.min(1, until / LOOK));
    const sc = sounding && -until < .08 ? 1 : .05 + .95 * Math.pow(near, sounding ? 1.6 : 2.2);
    const fw = Math.round(w * sc), fh = Math.round(h * sc), x0 = Math.round(cx - fw / 2), y0 = Math.round(cy - fh / 2);
    const bright = sounding ? .35 + .65 * left : .3 + .7 * near;
    const col = base.map(v => Math.round(v * bright)), dark = base.map(v => Math.round(v * bright * .45));
    if (sounding && -until < .08) { // the blink: the whole screen is the note for a moment
      ink(...col); box(x0, y0, fw, fh, 'fill');
      ink(...dark); for (let y = y0 + 4; y < y0 + fh; y += 8) line(x0, y, x0 + fw, y);
    } else {
      if (sc > .12) { ink(...dark); const gap = sounding ? 6 : 4; for (let y = y0 + 3; y < y0 + fh - 1; y += gap) line(x0 + 2, y, x0 + fw - 3, y); }
      ink(...(sharp ? [225, 225, 235] : col)); box(x0, y0, fw, fh, 'outline');
      if (sc > .25) { ink(...dark); box(x0 + 2, y0 + 2, fw - 4, fh - 4, 'outline'); }
    }
    if (e.note && sc > .18) {
      ink(...(sharp ? [225, 225, 235] : col));
      write(e.note, { x: x0 + 4, y: y0 + 3, font: '6x10', size: sc > .6 ? 3 : sc > .35 ? 2 : 1 });
    }
  }
  ink(...own);
  write('LAPTOP', { x: 10, y: 10, font: '6x10' });
  write(isCenter ? 'C' : String(config.seat + 1), { x: 10, y: 28, font: '6x10', size: 5 });
  ink(255, 220, 100);
  const ringN = ringSeats(score, config.seats), ringIdx = config.seat > (score.center ?? 99) ? config.seat - 1 : config.seat;
  const placement = isCenter ? ['HELD', 'CENTER'] : score.geometry === 'line' ? ['LINE', String(config.seat + 1)] : ringIdx === 0 ? ['FRONT'] : ['AT ' + Math.round(ringIdx / ringN * 360) + ' DEG'];
  placement.forEach((word, i) => write(word, { x: w - 8 - word.length * 12, y: 28 + i * 23, font: '6x10', size: 2 }));
  const mv = (score.movements || []).find(m => t >= m.t0 && t < m.t1);
  ink(180, 199, 223);
  if (mv) write(mv.name.replace(/·/g, '-'), { x: 10, y: h - 44, font: '6x10' });
  const connected = Array.from({ length: config.seats }, (_, i) => seatConnection(i, sound.time) === 'online').filter(Boolean).length;
  ink(...(connected === config.seats ? [160, 215, 180] : [255, 180, 110]));
  write(sound.time - presenceSeen > 5 ? 'NETWORK DATA STALE' : connected + '/' + config.seats + ' connected', { x: 10, y: h - 29, font: '6x10' });
  ink(180, 199, 223);
  write((mode === 'beeps' ? 'Pulse' : score.name) + ' / ' + phase + (t >= 0 ? '  ' + Math.floor(t / 60) + ':' + String(Math.floor(t % 60)).padStart(2, '0') : ''), { x: 10, y: h - 14, font: '6x10' });
}

export function act({ event, system }) { if (event.is('keyboard:down:escape')) system.jump('prompt'); }
export function leave() { stopVoices(); }
