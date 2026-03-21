// voice.mjs — System voice chooser for AC Native OS
// Select between female/male TTS voice, preview it, persist across reboots.

const VOICES = [
  { id: "female", label: "female", desc: "default voice" },
  { id: "male", label: "male", desc: "deeper voice" },
  { id: "off", label: "off", desc: "disable keystroke voicing" },
];

let selected = 0;
let frame = 0;
let T;
let previewTimer = 0;

function boot({ system, sound }) {
  T = __theme.update();
  // Read current voice from config
  let currentId = "female";
  try {
    const raw = system?.readFile?.("/mnt/config.json");
    if (raw) {
      const cfg = JSON.parse(raw);
      if (cfg.voice) currentId = cfg.voice;
    }
  } catch (_) {}
  selected = VOICES.findIndex((v) => v.id === currentId);
  if (selected < 0) selected = 0;
}

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down:arrowup")) {
    selected = (selected - 1 + VOICES.length) % VOICES.length;
    // Preview the voice
    const male = VOICES[selected].id === "male" ? 1 : 0;
    sound?.speakVoice?.(VOICES[selected].label, male);
  }
  if (e.is("keyboard:down:arrowdown")) {
    selected = (selected + 1) % VOICES.length;
    const male = VOICES[selected].id === "male" ? 1 : 0;
    sound?.speakVoice?.(VOICES[selected].label, male);
  }
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    const id = VOICES[selected].id;
    system?.saveConfig?.("voice", id);
    const male = id === "male" ? 1 : 0;
    sound?.speakVoice?.("voice saved", male);
    system?.jump?.("prompt");
  }
  if (e.is("keyboard:down:escape")) {
    system?.jump?.("prompt");
  }
}

function paint({ wipe, ink, box, write, screen }) {
  frame++;
  T = __theme.update();
  const W = screen.width;
  const H = screen.height;

  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const font = "6x10";
  const total = VOICES.length;

  // Card layout
  const cardH = 28;
  const cardGap = 4;
  const totalH = total * cardH + (total - 1) * cardGap;
  const startY = Math.floor((H - totalH) / 2);
  const cardX = 8;
  const cardW = W - 16;

  for (let i = 0; i < total; i++) {
    const v = VOICES[i];
    const y = startY + i * (cardH + cardGap);
    const isSel = i === selected;

    // Card background
    ink(T.bgAlt[0], T.bgAlt[1], T.bgAlt[2]);
    box(cardX, y, cardW, cardH, true);

    // Selection indicator
    if (isSel) {
      ink(T.cursor[0], T.cursor[1], T.cursor[2]);
      box(cardX - 2, y - 1, cardW + 4, cardH + 2, false);
      if (frame % 40 < 28) {
        write(">", { x: cardX + 3, y: y + 5, size: 1, font });
      }
    }

    // Voice label
    ink(T.fg, T.fg, T.fg);
    write(v.label, { x: cardX + 14, y: y + 3, size: 1, font });

    // Description (dimmed)
    ink(T.fgDim, T.fgDim, T.fgDim);
    write(v.desc, { x: cardX + 14, y: y + 15, size: 1, font });
  }

  // Hint at bottom
  ink(T.fgMute, T.fgMute, T.fgMute);
  write("up/down + enter  esc to cancel", { x: 4, y: H - 14, size: 1, font });
}

function sim() {}
function leave() {}

export { boot, paint, act, sim, leave };
