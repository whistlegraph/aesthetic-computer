// Rattle, 2023.8.14.21.03.16
// An instrument for shaking.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
 - [🍊] Add accelerometer information.
#endregion */

const { abs, min, max } = Math;

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

let t1, t2, t3;
const bass1 = 840;
const bass2 = 850;
const bass3 = 860;
let t1t = 0,
  t2t = 0,
  t3t = 0;
const lo = 10;
const hi = 900;

// 🧮 Sim
function sim({ num, motion, pen, sound: { synth } }) {
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
        alpha: mo?.rotation?.alpha.toFixed(2) || pen?.delta.x * 5,
        beta: mo?.rotation?.beta.toFixed(2) || pen?.delta.y * 5,
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

    if (!t1) {
      t1 = synth({
        type: "noise-white",
        tone: t1t, // 50 * abs(values.rotation.alpha),
        volume: 0,
        duration: "🔁",
      });
    }

    if (!t2) {
      //t2 = synth({
        //type: "noise-white",
        //tone: t2t, // 100 * abs(values.rotation.beta),
        //volume: 0,
        //duration: "🔁",
      //});
    }

    if (!t3) {
      /*
      t3 = synth({
        type: "noise-white",
        tone: t3t, // 25 * abs(values.rotation.gamma),
        volume: 0,
        duration: "🔁",
      });
      */
    }

    const div = 10;

    function calvol(val) {
      return max(0.0, min(1, abs(val * val) * 50));
    }

    {
      const val = parseFloat(values.accel.y);
      t1t += val;
      t1t = num.lerp(t1t, 0, 0.0353);
      values.t1t = bass1 + abs(t1t);
      t1?.update({
        tone: values.t1t,
        volume: calvol(t1t / (bass1 * 2)),
        duration: 0.005,
      });
    }

    {
      const val = parseFloat(values.rotation.beta);
      t2t += val / div;
      t2t = num.lerp(t2t, 0, 0.0353);
      values.t2t = bass2 + abs(t2t);
      t2?.update({
        tone: values.t2t,
        volume: calvol(t2t / (bass2 * 2)),
        duration: 0.005,
      });
    }

    {
      const val = parseFloat(values.rotation.gamma);
      t3t += val / div;
      t3t = num.lerp(t3t, 0, 0.0353);
      values.t3t = bass3 + abs(t3t);
      t3?.update({
        tone: values.t3t,
        volume: calvol(t3t / (bass3 * 2)),
        duration: 0.005,
      });
    }

    /*
    if (abs(values.rotation.alpha) > 20) {
      if (catch1 === release) {
        const tone = 50 * abs(values.rotation.alpha);
        t1?.update({ tone: tone, duration: 0.001 });
        catch1 = 0;
      }
    }
    */

    /*
    if (abs(values.rotation.beta) > 20) {
      if (catch2 === release) {
        const tone = 100 * abs(values.rotation.beta);
        makePerc(tone);
        catch2 = 0;
      }
    }
    */

    /*
    if (abs(values.rotation.gamma) > 20) {
      if (catch3 === release) {
        const tone = 25 * abs(values.rotation.gamma);
        makePerc(tone);
        catch3 = 0;
      }
    }
    */
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
