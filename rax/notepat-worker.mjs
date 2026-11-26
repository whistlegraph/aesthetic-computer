// One running disk owns the courtyard keyboard, picture and sounding voices.
import { parentPort, workerData } from 'node:worker_threads';
import { deflateSync } from 'node:zlib';
import { acPiece } from './ac-piece.mjs';
import { createPanel } from './panel.mjs';
import { kioskPlan } from './kiosks-plan.mjs';

const board = kioskPlan('lobby').find(p => p.id === 'yard-notepat');
const voices = new Map(), releases = new Map();
let serial = 0, program = null, soundChanged = false;
const changed = () => { soundChanged = true; };
const sound = {
  synth({ type, tone, volume = 1 }) {
    const id = ++serial;
    const voice = { id, type, tone, volume };
    voices.set(id, voice); changed();
    return {
      update({ tone }) { if (tone !== undefined && tone !== voice.tone) { voice.tone = tone; changed(); } },
      kill(fade) { if (voices.delete(id)) { releases.set(id, fade); changed(); } },
    };
  },
  sampleContext: () => null,
  createGm({ status }) {
    const samples = new Map();
    function stop(id, fade) {
      const serial = samples.get(id);
      if (voices.delete(serial)) { releases.set(serial, fade); changed(); }
      samples.delete(id);
    }
    return {
      program(value) { program = value; changed(); status({ ready: true, program }); },
      note({ id, program, note, velocity, pan, volume }) {
        stop(id);
        const voice = { id: ++serial, program, note, velocity, pan, volume };
        samples.set(id, voice.id); voices.set(voice.id, voice); changed();
      },
      update({ id, semitones }) {
        const v = voices.get(samples.get(id));
        if (v && v.semitones !== semitones) { v.semitones = semitones; changed(); }
      },
      stop, panic() { for (const id of samples.keys()) stop(id); },
    };
  },
};
const device = await acPiece(board.piece, {
  base: new URL('../system/public/aesthetic.computer/', import.meta.url).href,
  seed: board.id, kept: { 'notepat:layout': 'piano' }, sound,
});
const panel = createPanel({ ...board, piece: device.piece, host: device.host });
await device.ready();
panel.step();
if (workerData.settings) device.instrumentSettings(workerData.settings);
else {
  // Old servers kept pointer gestures. Migrate their control changes, then
  // release any hands that were still down when the last process stopped.
  const down = new Set();
  for (const [name, x, y, seat, finger = 1] of workerData.events || []) {
    const pointer = 16 * (seat + 1) + finger;
    if (name === 'touch') down.add(pointer); else down.delete(pointer);
    panel.act(name, x, y, 'touch', pointer);
  }
  for (const pointer of down) panel.act('lift', -1, -1, 'touch', pointer);
}
let saved = '', lastFrame = -1;
function publish() {
  if (soundChanged) {
    parentPort.postMessage({ type: 'notepat-sound', at: Date.now() + 65, voices: [...voices.values()], program, releases: Object.fromEntries(releases) });
    releases.clear();
    soundChanged = false;
  }
  const settings = device.instrumentSettings();
  const text = JSON.stringify(settings);
  if (text !== saved) { saved = text; parentPort.postMessage({ type: 'settings', settings }); }
}
function frame() {
  panel.step();
  publish();
  if (panel.frame === lastFrame) return;
  lastFrame = panel.frame;
  const pixels = deflateSync(panel.pixels, { level: 1 }).toString('base64');
  parentPort.postMessage({ type: 'notepat-frame', pixels });
}
frame();
let timer;
parentPort.on('message', msg => {
  if (msg.type === 'watch') {
    clearInterval(timer);
    if (msg.active) { frame(); timer = setInterval(frame, 50); }
  } else if (msg.type === 'key') panel.key(msg.way, msg);
  else if (msg.type === 'pointer') panel.act(msg.name, msg.x, msg.y, 'touch', msg.pointer);
  else if (msg.type === 'flush') { publish(); parentPort.postMessage({ type: 'flushed' }); return; }
  publish();
});
