// surtitle.mjs — one sung word at a time, under a MacNeoPolitan face.
// An AC OS laptop sits beneath each Neo; the conductor (pop/eightgigabytes/live/conduct.py)
// PUTs /pieces/surtitle-command.json: `clock` probes this laptop's audio clock, `prepare`
// hands over the member, its color, the piece's duration and the word timeline with a
// startAt on that clock, `stop` clears. The same protocol as spatial-rehearsal; nothing
// here makes a sound. Esc returns to the prompt.
let seenCommand = null, cue = null, origin = null, lastRead = 0, lastStatus = 0, phase = 'ready';
const LEAD = 0.12, LINGER = 0.45;   // the word shows a hair early and stays a beat after

export function boot({ system, sound }) {
  sound.microphone?.close?.();
  system.startSSH?.();
}

export function sim({ system, sound, screen, wifi }) {
  const now = sound.time;
  if (now - lastRead >= 0.01) {
    lastRead = now;
    let cmd;
    try { cmd = JSON.parse(system.readFile('/pieces/surtitle-command.json')); } catch (_) { /* retry incomplete PUT */ }
    if (cmd?.id && cmd.id !== seenCommand) {
      seenCommand = cmd.id;
      if (cmd.action === 'clock') system.writeFile('/pieces/surtitle-clock.json', JSON.stringify({ id: cmd.id, audioTime: now }));
      if (cmd.action === 'stop') { cue = null; origin = null; phase = 'ready'; }
      if (cmd.action === 'prepare' && Number.isFinite(cmd.startAt) && Array.isArray(cmd.words)) {
        cue = cmd; origin = cmd.startAt; phase = 'prepared';
      }
    }
  }
  if (origin !== null && cue) {
    const t = now - origin;
    if (t >= 0) phase = 'playing';
    if (t >= (cue.duration || 0) + 1) phase = 'finished';
  }
  if (now - lastStatus >= 0.5) {
    lastStatus = now;
    system.writeFile('/pieces/surtitle-status.json', JSON.stringify({
      phase, member: cue?.member, command: seenCommand, audioTime: now,
      ip: wifi?.ip || '', screen: { width: screen.width, height: screen.height },
    }));
  }
}

export function paint({ wipe, ink, write, screen, sound }) {
  wipe(0, 0, 0);
  if (!cue) { ink(90, 90, 90); write('surtitle', { center: 'xy', size: 2 }); return; }
  const [r, g, b] = cue.color || [255, 255, 255];
  const t = sound.time - origin;
  if (t < 0) { ink(r, g, b); write(cue.member, { center: 'xy', size: 2 }); return; }
  // the sung word: the latest word that has started (a little early), until it has lingered
  let word = null;
  for (let i = cue.words.length - 1; i >= 0; i--) {
    const w = cue.words[i];
    if (w[1] - LEAD <= t) { if (t < w[2] + LINGER) word = w; break; }
  }
  if (!word) return;
  const text = word[0].replace(/-/g, '');
  const size = Math.max(1, Math.min(14, Math.floor((screen.width * 0.9) / (text.length * 6))));
  ink(r, g, b);
  write(text, { center: 'xy', size });
}

export function act({ event, system }) {
  if (event.is('keyboard:down:escape')) system.jump('prompt');
}
