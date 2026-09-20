// ruke, 26.09.13

/* #region 📝 notes
  A single notepat-style pad: neon flash while held, ghost trail on release.
#endregion */

const pad = { note: "4G", label: "G", tint: [110, 200, 255] };

let btn, voice, trail = 0;

// Centered pad, sized off the smaller dimension, kept clear of the label.
function layout({ screen }) {
  const w = Math.min(120, Math.round(Math.min(screen.width, screen.height) * 0.5));
  const h = Math.round(w * 0.75);
  return [
    Math.round((screen.width - w) / 2),
    Math.max(24, Math.round((screen.height - h) / 2)),
    w,
    h,
  ];
}

function boot({ ui, screen }) {
  btn = new ui.Button(...layout({ screen }));
}

function paint({ wipe, ink, screen, paintCount }) {
  wipe(16, 14, 22);

  btn?.paint((b) => {
    let c = pad.tint;

    if (b.down) {
      // Dayglow cycle — the held pad should be unmistakable across the room.
      const phase = (paintCount * 0.15) % 3;
      const f =
        phase < 1 ? [255, 80, 120] : phase < 2 ? [80, 255, 80] : [120, 80, 255];
      c = c.map((v, i) => Math.min(255, Math.round(v * 0.4 + f[i] * 0.6)));
    }

    ink(c, 196).box(b.box);

    if (b.over && !b.down) {
      ink(...pad.tint, 40).box(b.box);
      ink(...pad.tint, 140).box(b.box, "outline");
    }

    if (trail > 0) {
      ink("maroon", Math.max(1, trail * 180)).box(
        b.box.x + b.box.w / 2,
        b.box.y + b.box.h / 2,
        trail * b.box.w,
        trail * b.box.h,
        "center",
      );
    }

    ink(b.down ? "white" : [20, 18, 26]).write(
      pad.label,
      { x: b.box.x + 3, y: b.box.y + 2 },
      undefined,
      undefined,
      false,
      "MatrixChunky8",
    );
  });

  ink(120, 130, 150).write(pad.note, { x: 4, y: screen.height - 12 });
}

function sim() {
  if (trail > 0) trail = Math.max(0, trail - 0.06);
}

function act({ event: e, sound, pens, screen, geo }) {
  if (e.is("reframed")) btn.box = new geo.Box(...layout({ screen }));

  const hit = () => {
    voice?.kill(0.1);
    voice = sound.synth({
      type: "sine",
      tone: pad.note,
      attack: 0.005,
      decay: 0.9,
      duration: "🔁",
      volume: 0.8,
    });
  };

  const release = (fade = 0.15) => {
    voice?.kill(fade);
    voice = undefined;
    trail = 1;
  };

  btn?.act(
    e,
    {
      down: hit, // also fires when a gesture slides onto the pad
      out: () => release(),
      push: () => release(),
      cancel: () => release(0.02),
    },
    pens?.(),
  );
}

function leave() {
  voice?.kill(0.1);
  voice = undefined;
}

export { boot, paint, sim, act, leave };
