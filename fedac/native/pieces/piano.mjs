// Piano, 26.10.20
// The notepat redux, patterned on Menu Band: one instrument, the qwerty
// piano, chords on the modifier keys, a shift linger, two effect sends and
// a dark scope. Nothing else on screen.

// Menu Band's notepat table: letter → semitones from middle C.
const SEMI = { z:-2, x:-1, c:0, v:1, d:2, s:3, e:4, f:5, w:6, g:7, r:8, a:9, q:10, b:11,
               h:12, t:13, i:14, y:15, j:16, k:17, u:18, l:19, o:20, m:21, p:22, n:23, ";":24, "'":25 };
const ROWS = ["qwertyuiop", "asdfghjkl;'", "zxcvbnm,./"]; // for pan + the key strip
const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
const GM = ["Acoustic Grand Piano","Bright Acoustic Piano","Electric Grand Piano","Honky-tonk Piano","Electric Piano 1","Electric Piano 2","Harpsichord","Clavi","Celesta","Glockenspiel","Music Box","Vibraphone","Marimba","Xylophone","Tubular Bells","Dulcimer","Drawbar Organ","Percussive Organ","Rock Organ","Church Organ","Reed Organ","Accordion","Harmonica","Tango Accordion","Acoustic Guitar (nylon)","Acoustic Guitar (steel)","Electric Guitar (jazz)","Electric Guitar (clean)","Electric Guitar (muted)","Overdriven Guitar","Distortion Guitar","Guitar harmonics","Acoustic Bass","Electric Bass (finger)","Electric Bass (pick)","Fretless Bass","Slap Bass 1","Slap Bass 2","Synth Bass 1","Synth Bass 2","Violin","Viola","Cello","Contrabass","Tremolo Strings","Pizzicato Strings","Orchestral Harp","Timpani","String Ensemble 1","String Ensemble 2","SynthStrings 1","SynthStrings 2","Choir Aahs","Voice Oohs","Synth Voice","Orchestra Hit","Trumpet","Trombone","Tuba","Muted Trumpet","French Horn","Brass Section","SynthBrass 1","SynthBrass 2","Soprano Sax","Alto Sax","Tenor Sax","Baritone Sax","Oboe","English Horn","Bassoon","Clarinet","Piccolo","Flute","Recorder","Pan Flute","Blown Bottle","Shakuhachi","Whistle","Ocarina","Lead 1 (square)","Lead 2 (sawtooth)","Lead 3 (calliope)","Lead 4 (chiff)","Lead 5 (charang)","Lead 6 (voice)","Lead 7 (fifths)","Lead 8 (bass + lead)","Pad 1 (new age)","Pad 2 (warm)","Pad 3 (polysynth)","Pad 4 (choir)","Pad 5 (bowed)","Pad 6 (metallic)","Pad 7 (halo)","Pad 8 (sweep)","FX 1 (rain)","FX 2 (soundtrack)","FX 3 (crystal)","FX 4 (atmosphere)","FX 5 (brightness)","FX 6 (goblins)","FX 7 (echoes)","FX 8 (sci-fi)","Sitar","Banjo","Shamisen","Koto","Kalimba","Bag pipe","Fiddle","Shanai","Tinkle Bell","Agogo","Steel Drums","Woodblock","Taiko Drum","Melodic Tom","Synth Drum","Reverse Cymbal","Guitar Fret Noise","Breath Noise","Seashore","Bird Tweet","Telephone Ring","Helicopter","Applause","Gunshot"];

const LINGER = 4.0, FADE = 0.1, VOL = 0.7;
const LIT = [150, 255, 210], DIM = [28, 46, 44], GLASS = [9, 11, 14], INK = [120, 150, 140];

let held = {};          // letter → { voices, midi, linger }
let octave = 4, program = 0; // program null = the AC piano wave
let mods = { shift: false, caps: false, meta: false, alt: false, control: false };
let metaChorded = false;
let digits = "", digitAt = 0;
let space = 0, echo = 0, notice = null;
let sfx = null;

const freq = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const pan = (k) => { for (const [r, row] of ROWS.entries()) { const i = row.indexOf(k); if (i >= 0) return ((i + r * 0.5) / 10.5 * 2 - 1) * 0.9; } return 0; };
const chord = () => !(mods.meta || mods.alt || mods.control) ? [0]
  : mods.meta && mods.alt ? (mods.control ? [0, 5, 7] : [0, 2, 7])
  : mods.control ? (mods.alt ? [0, 3, 6] : [0, 4, 8])
  : mods.alt ? [0, 3, 7] : [0, 4, 7];

function flash(text) { notice = { text, until: Date.now() + 1800 }; }

function press(k, sound) {
  if (held[k] || SEMI[k] === undefined) return;
  const midi = 60 + SEMI[k] + (octave - 4) * 12, ivls = chord(), p = pan(k), voices = [];
  for (const iv of ivls) {
    const v = sound.synth({ type: "piano", tone: freq(midi + iv), duration: Infinity, volume: VOL,
                            attack: 0.02, decay: FADE, pan: Math.max(-0.85, Math.min(0.85, p + (iv ? (iv - 4) * 0.04 : 0))),
                            gmProgram: program === null ? undefined : program });
    if (v) voices.push(v);
  }
  if (mods.meta) metaChorded = true;
  held[k] = { voices, midi, ivls, linger: mods.shift || mods.caps };
}

function release(k, sound, fade) {
  const h = held[k]; if (!h) return;
  for (const v of h.voices) sound.kill(v, fade ?? (h.linger ? LINGER : FADE));
  delete held[k];
}

function digit(d, sound) {
  const now = Date.now();
  if (now - digitAt > 800 || digits.length >= 3) digits = "";
  digitAt = now; digits += d;
  const n = parseInt(digits, 10);
  program = n === 0 ? null : Math.min(127, n - 1);
  flash(program === null ? "AC piano" : `${program + 1}. ${GM[program]}`);
  sound.synth({ type: "piano", tone: 523.25, duration: 0.12, volume: 0.1, attack: 0.002, decay: 0.1,
                gmProgram: program === null ? undefined : program });
}

function send(sound) { sound.room?.setMix?.(space); sound.echo?.setMix?.(echo); }

export function boot({ sound }) { sfx = sound; send(sound); flash(`1. ${GM[0]}`); }

export function act({ event: e, sound, system }) {
  const key = e.key?.toLowerCase();
  if (e.is("keyboard:down") && key === "capslock" && !e.repeat) { mods.caps = !mods.caps; return; }
  if (key in mods) {
    if (e.is("keyboard:down")) { if (key === "meta" && !mods.meta) metaChorded = false; mods[key] = true; }
    if (e.is("keyboard:up")) mods[key] = false;
    return;
  }
  if (e.is("keyboard:up")) { release(key, sound); return; }
  if (!e.is("keyboard:down") || e.repeat) return;
  if (key === "escape") { system.jump("prompt"); return; }
  if (key === "backspace") { for (const k of Object.keys(held)) release(k, sound, 0.08); return; }
  if (key === ",") { octave = Math.max(1, octave - 1); flash(`octave ${octave}`); return; }
  if (key === ".") { octave = Math.min(7, octave + 1); flash(`octave ${octave}`); return; }
  if (key === "arrowup")    { space = Math.min(1, space + 0.1); send(sound); return; }
  if (key === "arrowdown")  { space = Math.max(0, space - 0.1); send(sound); return; }
  if (key === "arrowright") { echo  = Math.min(1, echo  + 0.1); send(sound); return; }
  if (key === "arrowleft")  { echo  = Math.max(0, echo  - 0.1); send(sound); return; }
  if (key >= "0" && key <= "9" && key.length === 1) { digit(key, sound); return; }
  press(key, sound);
}

function meter(ink, box, x, y, v, label, write) {
  for (let i = 0; i < 10; i++) { ink(...(i < Math.round(v * 10) ? LIT : DIM)); box(x + i * 7, y, 5, 6); }
  ink(...INK); write(label, { x: x + 74, y: y - 1, font: "matrix" });
}

export function paint({ wipe, ink, box, line, write, screen, sound }) {
  const w = screen.width, h = screen.height, m = Math.round(w * 0.04);
  wipe(...GLASS);

  // The scope: a dark raster across the middle, drawn in C.
  const sy = Math.round(h * 0.26), sh = Math.round(h * 0.34);
  ink(...DIM); box(m - 1, sy - 1, w - m * 2 + 2, sh + 2);
  sound.speaker?.drawStrip?.(m, sy, w - m * 2, sh, 2.0, 0.5, 0);

  // The piano: one cell per key, two octaves and two more, lit while held.
  const keys = Object.keys(SEMI), cw = Math.floor((w - m * 2) / keys.length), kh = Math.round(h * 0.11);
  const ky = sy + sh + Math.round(h * 0.06), kx = Math.round((w - cw * keys.length) / 2);
  keys.forEach((k, i) => {
    const sharp = NAMES[((SEMI[k] % 12) + 12) % 12].includes("#"), on = !!held[k];
    ink(...(on ? LIT : sharp ? [16, 22, 24] : DIM));
    box(kx + i * cw + 1, ky + (sharp ? 0 : 4), cw - 2, kh - (sharp ? 10 : 4));
    ink(...(on ? GLASS : INK)); write(k, { x: kx + i * cw + Math.floor(cw / 2) - 3, y: ky + kh + 4, font: "matrix" });
  });

  // Readouts along the top right, clear of the corner label.
  const name = program === null ? "AC piano" : `${String(program + 1).padStart(3, "0")} ${GM[program]}`;
  ink(...LIT); write(name, { x: w - m - name.length * 6, y: 8 });
  ink(...INK); write(`octave ${octave}${mods.caps ? "  sustain" : ""}${mods.shift ? "  linger" : ""}`, { x: w - m - 6 * 22, y: 22 });
  meter(ink, box, w - m - 110, 40, space, "space", write);
  meter(ink, box, w - m - 110, 52, echo, "echo", write);

  // What's sounding, bottom left; the notice over it when something changed.
  const sounding = Object.values(held).map((v) => NAMES[((v.midi % 12) + 12) % 12] + Math.floor(v.midi / 12 - 1)).join(" ");
  ink(...LIT); write(sounding, { x: m, y: h - 36 });
  if (notice && Date.now() < notice.until) { ink(...LIT); write(notice.text, { x: m, y: h - 52 }); }
  ink(...INK); write("shift linger  caps sustain  win/alt/ctrl chords  , . octave  arrows space/echo  0-9 instrument  backspace stop", { x: m, y: h - 18, font: "matrix" });
}

export function leave({ sound }) { for (const k of Object.keys(held)) release(k, sound, 0.08); }
