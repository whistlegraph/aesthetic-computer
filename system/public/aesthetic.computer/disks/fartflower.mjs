// Fartflower, 2026.4.15
// One button. Press it. Flower blooms. Fart plays.

let bloomProgress = 0;
let activeFart = null;
let fartButton;

// 🥾 Boot
function boot({ ui: { Button }, screen, hud }) {
  hud.label();
  const size = 60;
  fartButton = new Button(
    (screen.width - size) / 2,
    (screen.height - size) / 2,
    size,
    size,
  );
}

// 🧮 Sim
function sim({ num, needsPaint }) {
  const target = activeFart ? 1 : 0;
  const speed = activeFart ? 0.08 : 0.06;
  const next = num.clamp(bloomProgress + (target - bloomProgress) * speed, 0, 1);
  if (next !== bloomProgress) {
    bloomProgress = next;
    needsPaint();
  }
}

// 🎨 Paint
function paint({ wipe, ink, screen, num }) {
  wipe(240);

  const cx = screen.width / 2;
  const cy = screen.height / 2;
  const petals = 6;
  const petalR = num.lerp(10, 80, bloomProgress);
  const alpha = (bloomProgress * 200) | 0;

  for (let i = 0; i < petals; i++) {
    const a = (i / petals) * Math.PI * 2;
    const px = cx + Math.cos(a) * (50 + petalR * 0.5);
    const py = cy + Math.sin(a) * (50 + petalR * 0.5);
    ink(255, 100, 150, alpha).circle(px, py, petalR, true);
  }

  ink(100, 150, 80).line(cx, cy, cx, cy + 100);
  ink(255, 200, 0).circle(cx, cy, num.lerp(5, 20, bloomProgress), true);

  const { x, y, w, h } = fartButton.box;
  ink(fartButton.down ? "lime" : "white").box(x, y, w, h);
  ink("black").box(x, y, w, h, "outline");
  ink("black").write("fart", { x: x + 8, y: y + h / 2 - 4 });
}

// 🎪 Act
function act({ event: e, sound, num }) {
  const fart = sound?.fart;
  if (e.is("touch") || e.is("lift")) {
    console.log("🌸 fartflower event:", e.name, "fart fn?", typeof fart);
  }
  fartButton.act(e, {
    down: () => {
      console.log("🌸 DOWN — calling fart()");
      if (!fart) { console.warn("🌸 sound.fart unavailable"); return; }
      try { activeFart?.kill(0.1); } catch (err) { console.warn("🌸 kill err", err); }
      try {
        activeFart = fart({
          pressure: 0.95,
          pitch: 70 + num.rand() * 40,
          rasp: 0.25,
          volume: 1,
          pan: 0,
        });
        // Skid: sweep pitch down over the fart so it "deflates"
        setTimeout(() => {
          activeFart?.update?.({
            pitch: 45 + num.rand() * 15,
            rasp: 0.15,
            duration: 0.4,
          });
        }, 80);
        console.log("🌸 fart() returned id:", activeFart?.id);
      } catch (err) {
        console.error("🌸 fart() threw", err?.message || err);
      }
    },
    push: () => {
      console.log("🌸 PUSH (release)");
      activeFart = null;
    },
  });
}

function meta() {
  return { title: "Fartflower", desc: "Press the button. Flower blooms." };
}

export { boot, sim, paint, act, meta };
