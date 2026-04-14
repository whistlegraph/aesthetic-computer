// 🌸 Fartflower 2025.04.14
// One button. Click it. Flower blooms. Fart sound plays.

let bloomProgress = 0;
let activeFart = null;
let fartButton;

// 🥾 Boot
function boot({ ui: { Button }, screen }) {
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const buttonSize = 60;

  // Single centered button
  fartButton = new Button("💨", {
    box: [
      centerX - buttonSize / 2,
      centerY - buttonSize / 2,
      buttonSize,
      buttonSize,
    ],
    fontSize: 32,
  });
}

// 🧮 Sim
function sim({ num }) {
  // Animate the bloom
  if (activeFart && bloomProgress < 1) {
    bloomProgress = num.clamp(bloomProgress + 0.08, 0, 1);
  } else if (!activeFart && bloomProgress > 0) {
    bloomProgress = num.clamp(bloomProgress - 0.06, 0, 1);
  }
}

// 🎨 Paint
function paint({ wipe, ink, circle, line, screen, num }) {
  wipe(240);

  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const petalCount = 6;
  const maxPetalRadius = 80;
  const petalRadius = num.lerp(10, maxPetalRadius, bloomProgress);

  // Draw petals in a circle
  for (let i = 0; i < petalCount; i++) {
    const angle = (i / petalCount) * Math.PI * 2;
    const x = centerX + Math.cos(angle) * (50 + petalRadius * 0.5);
    const y = centerY + Math.sin(angle) * (50 + petalRadius * 0.5);

    // Petals fade in/out with bloom
    const petalAlpha = Math.floor(bloomProgress * 200);
    ink(255, 100, 150, petalAlpha).circle(x, y, petalRadius);
  }

  // Center stem
  ink(100, 150, 80).line(centerX, centerY, centerX, centerY + 100);

  // Center of flower (grows with bloom)
  const centerRadius = num.lerp(5, 20, bloomProgress);
  ink(255, 200, 0).circle(centerX, centerY, centerRadius);

  // Draw button
  fartButton.paint({ ink });
}

// ✒ Act
function act({ event: e, sound: { fart } }) {
  if (fartButton.trigger(e)) {
    // Kill previous fart if still active
    if (activeFart) {
      activeFart.kill(0.1);
    }

    // Trigger new fart sound with physical modeling parameters
    activeFart = fart({
      pressure: 0.8,
      pitch: 60 + Math.random() * 30,
      rasp: 0.6,
      volume: 0.7,
      pan: 0,
    });

    // Enable sustain for the bloom animation duration
    activeFart.enableSustain();

    // Disable sustain after bloom completes
    setTimeout(() => {
      if (activeFart) {
        activeFart.disableSustain();
        activeFart = null;
      }
    }, 500);
  }
}

export { boot, sim, paint, act };
