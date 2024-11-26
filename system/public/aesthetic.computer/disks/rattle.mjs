// Rattle, 2023.8.14.21.03.16
// An instrument for shaking.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
 - [🍊] Add accelerometer information.
#endregion */

const { abs } = Math;

const release = 0;
let catch1, catch2, catch3;
let values = {};

catch1 = release;
catch2 = release;
catch3 = release;

// 🥾 Boot
function boot({ wipe, motion }) {
  wipe();
  motion.start();
}

// 🎨 Paint
function paint({ wipe, ink, motion, screen, sound: { synth } }) {
  wipe(0, 130, 80);

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
function sim({ motion, pen, sound: { synth } }) {
  const mo = motion.current;
  if (mo) {
    values = {
      accel: {
        x: mo?.accel?.x.toFixed(2),
        y: mo?.accel?.y.toFixed(2),
        z: mo?.accel?.z.toFixed(2),
      },
      accelWithGravity: {
        x: mo?.accelWithGravity?.x.toFixed(2),
        y: mo?.accelWithGravity?.y.toFixed(2),
        z: mo?.accelWithGravity?.z.toFixed(2),
      },
      rotation: {
        alpha: mo?.rotation?.alpha.toFixed(2) || (pen?.delta.x * 5),
        beta: mo?.rotation?.beta.toFixed(2) || (pen?.delta.y * 5),
        gamma: mo?.rotation?.gamma.toFixed(2),
      },
    };

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

    if (catch1 < release) catch1 += 1;
    if (catch2 < release) catch2 += 1;
    if (catch3 < release) catch3 += 1;

    if (abs(values.rotation.alpha) > 20) {
      if (catch1 === release) {
        makePerc(50 * values.rotation.alpha);
        catch1 = 0;
      }
    }

    if (abs(values.rotation.beta) > 20) {
      if (catch2 === release) {
        makePerc(100 * values.rotation.beta);
        catch2 = 0;
      }
    }

    if (abs(values.rotation.gamma) > 20) {
      if (catch3 === release) {
        makePerc(25 * values.rotation.gamma);
        catch3 = 0;
      }
    }
  }
}

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
