// 🦁 Zoo 2026.09.19
// A grid of animal calls built from the organic voices:
// sound.howl, sound.growl, sound.breath, sound.chirp.

const calls = {
  bray: [
    ["howl", { pitch: 300, slide: 520, vowel: "i", scale: 0.9, vibrato: 0.6, rasp: 0.15, duration: 0.5 }, 0],
    ["howl", { pitch: 260, slide: 140, vowel: "a", scale: 0.8, vibrato: 0.3, rasp: 0.5, attack: 0.03, release: 0.4, duration: 0.7 }, 500],
  ],
  roar: [
    ["growl", { pitch: 70, rasp: 0.7, size: 1.3, tremor: 0.6, duration: 1.6 }, 0],
    ["howl", { pitch: 90, slide: 70, vowel: "a", rasp: 0.5, scale: 0.8, vibrato: 0, attack: 0.12, release: 0.5, duration: 1.6 }, 50],
  ],
  wolf: [["howl", { pitch: 220, slide: 330, vowel: "o", vibrato: 0.4, rasp: 0.1, attack: 0.25, release: 0.6, duration: 2.4 }, 0]],
  bird: [["chirp", { pitch: 2500, slide: 4200, count: 4, rate: 9, duration: 0.6 }, 0]],
  "breath in": [["breath", { direction: "in", duration: 0.9 }, 0]],
  "breath out": [["breath", { direction: "out", duration: 0.9 }, 0]],
  growl: [["growl", { duration: 1.2 }, 0]],
};

const pads = []; // { name, btn, flashUntil }
const pending = []; // { at, kind, params } — the second half of a bray, etc.
const voices = []; // Everything still ringing, for `leave`.

function layout({ screen, geo }) {
  const cols = screen.width >= 240 ? 3 : 2;
  const rows = Math.ceil(pads.length / cols);
  const top = 24, gap = 4, edge = 6; // top clears the corner label
  const w = (screen.width - edge * 2 - gap * (cols - 1)) / cols;
  const h = (screen.height - top - edge - gap * (rows - 1)) / rows;
  pads.forEach((pad, i) => {
    const x = edge + (i % cols) * (w + gap), y = top + Math.floor(i / cols) * (h + gap);
    pad.btn.box = new geo.Box(x, y, w, h);
  });
}

function play(sound, kind, params) {
  const v = sound[kind](params);
  voices.push(v);
  if (voices.length > 24) voices.shift();
}

function boot($) {
  const { ui } = $;
  for (const name in calls) pads.push({ name, btn: new ui.Button(0, 0, 1, 1), flashUntil: 0 });
  layout($);
}

function paint({ wipe, ink, screen }) {
  wipe(20, 24, 32);
  const now = Date.now();
  for (const { name, btn, flashUntil } of pads) {
    const hot = now < flashUntil;
    const { x, y, w, h } = btn.box;
    ink(hot ? [255, 200, 60] : btn.down ? [90, 110, 160] : [50, 60, 80]).box(x, y, w, h);
    ink(hot ? [255, 240, 180] : [120, 140, 180]).box(x, y, w, h, "outline");
    ink(hot ? [30, 20, 0] : [220, 230, 240]).write(name, { x: x + w / 2, y: y + h / 2 - 4, center: "x" });
  }
  ink(90, 100, 120).write("tap an animal", { y: screen.height - 14, right: 6 });
}

function act({ event: e, sound, pens, screen, geo }) {
  if (e.is("reframed")) layout({ screen, geo });
  const now = Date.now();
  for (const pad of pads) {
    pad.btn.act(e, {
      down: () => {
        let longest = 0;
        for (const [kind, params, delay] of calls[pad.name]) {
          if (delay) pending.push({ at: now + delay, kind, params });
          else play(sound, kind, params);
          longest = Math.max(longest, delay + params.duration * 1000);
        }
        pad.flashUntil = now + longest;
      },
    }, pens?.());
  }
}

function sim({ sound }) {
  const now = Date.now();
  for (let i = pending.length - 1; i >= 0; i--) {
    if (now < pending[i].at) continue;
    const { kind, params } = pending.splice(i, 1)[0];
    play(sound, kind, params);
  }
}

function leave() {
  pending.length = 0;
  for (const v of voices) v.kill(0.1);
  voices.length = 0;
}

export { boot, paint, act, sim, leave };
