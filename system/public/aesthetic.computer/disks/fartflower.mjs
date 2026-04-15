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
function act({ event: e, sound: { fart }, num }) {
  fartButton.act(e, {
    push: () => {
      activeFart?.kill(0.1);
      activeFart = fart({
        pressure: 0.8,
        pitch: 60 + num.rand() * 30,
        rasp: 0.6,
        volume: 0.7,
        pan: 0,
      });
      activeFart.enableSustain();
      setTimeout(() => {
        activeFart?.disableSustain();
        activeFart = null;
      }, 500);
    },
  });
}

function meta() {
  return { title: "Fartflower", desc: "Press the button. Flower blooms." };
}

export { boot, sim, paint, act, meta };
