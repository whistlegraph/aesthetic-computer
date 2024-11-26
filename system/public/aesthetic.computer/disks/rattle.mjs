// Rattle, 2023.8.14.21.03.16
// An instrument for shaking.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
 - [🍊] Add accelerometer information.
#endregion */

const { abs } = Math;

// 🥾 Boot
function boot({ wipe, motion }) {
  wipe();
  motion.start();
}

// 🎨 Paint
function paint({ wipe, ink, motion, screen, sound: { synth } }) {
  wipe(0, 130, 80);

  // TODO: How can I pretty print json by inserting \n appropriately here?

  const mo = motion.current;
  let values = {};

  if (mo) {
    values = {
      accel: {
        x: mo?.accel.x.toFixed(2),
        y: mo?.accel.y.toFixed(2),
        z: mo?.accel.z.toFixed(2),
      },
      accelWithGravity: {
        x: mo?.accelWithGravity.x.toFixed(2),
        y: mo?.accelWithGravity.y.toFixed(2),
        z: mo?.accelWithGravity.z.toFixed(2),
      },
      rotation: {
        alpha: mo?.rotation.alpha.toFixed(2),
        beta: mo?.rotation.beta.toFixed(2),
        gamma: mo?.rotation.gamma.toFixed(2),
      },
    };
  }

  const lowvol = 0.95;
  const hivol = 1;
  const makePerc = (hz) => {
    synth({
      type: "triangle",
      tone: hz / 2,
      duration: 0.01,
      attack: 0,
      volume: hivol / 2,
    });

    synth({ type: "sawtooth", tone: hz, duration: 0.0025, volume: hivol });

    synth({
      type: "square",
      tone: hz / 4,
      duration: 0.005,
      volume: lowvol,
      decay: 0.999,
    });
  };

  if (abs(values.rotation.alpha > 100)) {
    makePerc(1000);
  }

  if (abs(values.rotation.beta > 100)) {
    makePerc(2000);
  }

  if (abs(values.rotation.gamma > 100)) {
    makePerc(3000);
  }

  ink(255).write(
    JSON.stringify(values, null, 1),
    { x: 12, y: 20 },
    "blue",
    screen.width,
  );

  if (!motion.on) {
    ink(255).write("Press to enable motion.", { center: "xy" });
  }
}

// 🎪 Act
function act({ event, motion }) {}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
function leave({ motion }) {
  motion.stop();
}

// 📰 Meta
function meta() {
  return {
    title: "Rattle",
    desc: "An instrument for shaking.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

// 📚 Library
//   (Useful functions used throughout the piece)
