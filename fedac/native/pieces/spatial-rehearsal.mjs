// Spatial rehearsal, 26.09.18
// Local synthesis on every seat, coordinated over Wi-Fi. Microphones stay closed.
import { voicePosition, sourceGain, hasFocus } from '../lib/spatial-rehearsal.mjs';

let config, score, error = '', phase = 'ready', origin = null;
let lastStatus = -1, lastRead = -1, seenCommand = '';
let cursors = [], voices = [], mode = 'score', lastBeep = -1;
let identifyAt = null, glow = 0;
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

export function boot({ system, sound }) {
  sound.microphone.close();
  try {
    try { config = readJSON(system, '/pieces/spatial-rehearsal-config.json'); }
    catch (_) { throw Error('Assign a seat with the spatial controller'); }
    if (!Number.isInteger(config.seats) || config.seats < 2 || config.seats > 16) throw Error('seats must be 2–16');
    if (!Number.isInteger(config.seat) || config.seat < 0 || config.seat >= config.seats) throw Error('seat outside ensemble');
    score = readJSON(system, '/pieces/spatial-rehearsal.nsscore');
    if (!(score.dur > 0) || !score.lanes?.length) throw Error('invalid score');
    cursors = score.lanes.map(() => 0);
    system.startSSH?.();
  } catch (e) { error = e.message; phase = 'error'; }
}

export function sim({ sound, system, screen, wifi }) {
  const now = sound.time;
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
        stopVoices(); origin = null; identifyAt = null;
        phase = error ? 'error' : 'ready';
      }
      if (cmd.action === 'prepare' && !error && Number.isFinite(cmd.startAt) && cmd.startAt > now + 1) {
        stopVoices(); origin = cmd.startAt; phase = 'prepared'; runId = cmd.id;
        mode = cmd.mode === 'beeps' ? 'beeps' : 'score';
        networkHalfRttMs = cmd.networkHalfRttMs;
        cursors = score.lanes.map(() => 0); lastBeep = -1; beepCount = 0; outputPeak = 0; maxFrameGap = 0;
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
      stopVoices(); phase = 'finished';
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
            duration: e.t + e.dur - t, volume: g * gain, attack: e.attack ?? 0.01, decay: e.decay ?? 0.04 });
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
  wipe(Math.round(12 + 42 * glow), Math.round(15 + 48 * glow), Math.round(23 + 54 * glow));
  const battery = system?.battery;
  let batteryLabel = 'BAT --';
  if (battery?.percent >= 0) {
    batteryLabel = battery.percent + '%' + (battery.charging ? ' +' : '');
    if (!battery.charging && battery.minutesLeft > 0)
      batteryLabel += ' ' + Math.floor(battery.minutesLeft / 60) + 'h' + String(battery.minutesLeft % 60).padStart(2, '0');
  }
  ink(...(battery?.percent >= 0 && battery.percent <= 15 ? [255, 165, 120] : [240, 245, 250]));
  write(batteryLabel, { x: screen.width - 8 - batteryLabel.length * 6, y: 8, font: '6x10', size: 1 });
  ink(210, 225, 240);
  write(config?.machineName || 'ac-device', { x: 75, y: 9, font: '6x10' });
  write(wifi?.ip || config?.ip || 'LAN unavailable', { x: 75, y: 26, font: '6x10' });
  if (error) { ink(255, 140, 130); write(error, { x: 8, y: 55 }); return; }
  if (!score) return;
  const t = origin === null ? 0 : Math.max(0, Math.min(runDuration(), sound.time - origin));
  const w = screen.width, h = screen.height;
  const cx = w * .65, cy = h * .51, radius = Math.min(h * .30, w * .19);
  // A shared floor plan: front at the top, increasing seat numbers clockwise.
  // This is the instructed layout, not a sensor estimate of laptop locations.
  const floor = (angle, r = radius) => [cx + Math.sin(angle) * r, cy - Math.cos(angle) * r];
  const isLine = score.geometry === 'line', order = isLine ? score.seatOrder : Array.from({length: config.seats}, (_,i)=>i);
  const lineX = u => w * .36 + u * w * .59;
  ink(125, 145, 170);
  if (!isLine) for (let j = 0; j < 64; j++) line(...floor(j / 64 * Math.PI * 2), ...floor((j + 1) / 64 * Math.PI * 2));
  ink(230, 235, 245);
  if (isLine) {
    line(lineX(0),cy,lineX(1),cy);
    write(score.motion === 'bounce' ? 'BACK AND FORTH' : 'LEFT TO RIGHT', { x: lineX(0), y: cy - 55, font: '6x10' });
    line(lineX(0),cy+48,lineX(1),cy+48);
    line(lineX(1),cy+48,lineX(1)-7,cy+43); line(lineX(1),cy+48,lineX(1)-7,cy+53);
    if (score.motion === 'bounce') {
      line(lineX(0),cy+48,lineX(0)+7,cy+43); line(lineX(0),cy+48,lineX(0)+7,cy+53);
    }
  } else write('YOU', { x: cx - 9, y: cy - 4, font: '6x10' });
  if (!isLine) { line(cx, cy - 14, cx, cy - 30);
  line(cx, cy - 30, cx - 4, cy - 24); line(cx, cy - 30, cx + 4, cy - 24); }
  for (let i = 0; i < score.lanes.length; i++) {
    const p = voicePosition(score, i, t), xy = isLine ? [lineX(p.line), cy - 30] : floor(p.angle, radius * .74);
    ink(...score.lanes[i].color.map(c => Math.round(c + (255 - c) * .35)));
    circle(xy[0], xy[1], 4, true);
  }
  for (const i of order) {
    const p = isLine ? [lineX(order.indexOf(i)/(order.length-1)),cy] : floor(i / config.seats * Math.PI * 2), own = i === config.seat;
    const state = seatConnection(i, sound.time), online = state === 'online';
    const lost = state === 'offline' || state === 'error';
    ink(...(lost ? [255, 105, 105] : !online ? [255, 190, 80] : own ? [255, 219, 90] : [195, 208, 225]));
    box(p[0] - 17, p[1] - 16, 34, 30, own && online ? 'fill' : 'outline');
    line(p[0] - 20, p[1] + 17, p[0] + 20, p[1] + 17, 2);
    if (!online) {
      const label = state === 'offline' ? 'OFFLINE' : state === 'error' ? 'ERROR' : state === 'unstable' ? 'LINK?' : 'UNKNOWN';
      write(label, { x: p[0] - label.length * 3, y: p[1] + 21, font: '6x10' });
      if (lost) { line(p[0] - 19, p[1] - 16, p[0] + 19, p[1] + 14); line(p[0] + 19, p[1] - 16, p[0] - 19, p[1] + 14); }
    }
    ink(...(own && online ? [20, 25, 35] : [240, 245, 255]));
    write(String(i + 1), { x: p[0] - 6, y: p[1] - 11, font: '6x10', size: 2 });
  }
  const placements = [['FRONT'], ['FRONT', 'RIGHT'], ['REAR', 'RIGHT'], ['REAR', 'LEFT'], ['FRONT', 'LEFT']];
  ink(245, 245, 250);
  write('LAPTOP', { x: 10, y: 10, font: '6x10' });
  write(String(config.seat + 1), { x: 10, y: 28, font: '6x10', size: 5 });
  ink(255, 220, 100);
  const placement = isLine ? (order.indexOf(config.seat) === 0 ? ['LEFT','END'] : order.indexOf(config.seat) === order.length - 1 ? ['RIGHT','END'] : ['POSITION', String(order.indexOf(config.seat) + 1)]) : config.seats === 5 ? placements[config.seat] : ['AT ' + Math.round(config.seat / config.seats * 360) + ' DEG'];
  placement.forEach((word, i) => write(word, { x: 10, y: 89 + i * 23, font: '6x10', size: 2 }));
  ink(215, 225, 240);
  write(isLine ? 'Facing the laptops:' : 'At center, face 1.', { x: 10, y: 157, font: '6x10' });
  write(isLine ? order.map(i => i + 1).join(' - ') : 'Speakers inward.', { x: 10, y: 174, font: '6x10' });
  write('Space evenly.', { x: 10, y: 191, font: '6x10' });
  const connected = Array.from({ length: config.seats }, (_, i) => seatConnection(i, sound.time) === 'online').filter(Boolean).length;
  ink(...(connected === config.seats ? [160, 215, 180] : [255, 180, 110]));
  write(sound.time - presenceSeen > 5 ? 'NETWORK DATA STALE' : connected + '/' + config.seats + ' connected' + (isLine && seatConnection(0,sound.time) === 'offline' ? ' / 1 OFFLINE' : ''), { x: 10, y: h - 29, font: '6x10' });
  ink(180, 199, 223);
  write((mode === 'beeps' ? 'Pulse' : score.name) + ' / ' + phase, { x: 10, y: h - 14, font: '6x10' });

}

export function act({ event, system }) { if (event.is('keyboard:down:escape')) system.jump('prompt'); }
export function leave() { stopVoices(); }
