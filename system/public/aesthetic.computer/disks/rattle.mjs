// Rattle, 2023.8.14.21.03.16
// An instrument for shaking.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
 - [] Figure out how to disable iOS Shake to Undo
      after entering rattle from `prompt`.
   - [] This may be the only way: https://stackoverflow.com/a/50965905.
   - [] But refreshing / reloading the page also could work.
 - [] Tapping top left corner word to go back
      still requests permission for accelerometer.
#endregion */

const { abs, min, max } = Math;

const release = 0;
let catch1, catch2, catch3;
let values = {};

catch1 = release;
catch2 = release;
catch3 = release;

let type;

// Stample support
const wavetypes = ["noise-white", "stample"];
let stampleSampleId = null;
let stampleSampleData = null;
let stampleSampleRate = null;
let fallbackSfx = null;
let stampleBtn = null;

// 🥾 Boot
function boot({ wipe, motion, colon, net, store, sound, ui, screen }) {
  wipe();
  motion.start();
  type = colon[0] || "noise-white";
  
  // Create stample mode toggle button (top right corner)
  stampleBtn = new ui.TextButton("stample", {
    top: 8,
    right: 8,
    screen,
  });
  
  // Preload fallback sound for stample mode
  net
    .preload("startup")
    .then((sfx) => {
      fallbackSfx = sfx;
      console.log("🎵 Rattle: Loaded fallback sfx:", sfx);
    })
    .catch((err) => console.warn("🎵 Rattle: Failed to load fallback sfx:", err));

  // Load stample sample from store
  stampleSampleId = null;
  stampleSampleData = null;
  stampleSampleRate = null;

  if (store?.retrieve) {
    (async () => {
      try {
        const storedSample =
          store["stample:sample"] ||
          (await store.retrieve("stample:sample", "local:db"));
        if (storedSample?.data?.length) {
          const storedId = storedSample.id || "stample";
          stampleSampleId = storedId;
          stampleSampleData = storedSample.data;
          stampleSampleRate = storedSample.sampleRate;
          sound?.registerSample?.(storedId, storedSample.data, storedSample.sampleRate);
          console.log("🎵 Rattle loaded stample sample:", storedId, storedSample.data.length, "samples");
        } else {
          console.log("🎵 Rattle: No stample sample found in store (record one in `stample` piece first)");
        }
      } catch (err) {
        console.warn("🎵 Rattle: Failed to load stample sample:", err);
      }
    })();
  }
}

// 🎨 Paint
function paint({
  api,
  wipe,
  ink,
  motion,
  screen,
  sound: { synth },
  goto,
  face,
  crawl,
  down,
  up,
  ui,
}) {
  wipe(0, 130, 80);

  ink(255).write(
     JSON.stringify(values, null, 1),
     { x: 12, y: 20 },
     "blue",
     screen.width,
   );
   
  // Draw stample mode button
  if (stampleBtn) {
    // Update button text to reflect current mode
    stampleBtn.txt = type === "stample" ? "stample" : "noise";
    stampleBtn.reposition({ top: 8, right: 8, screen }, stampleBtn.txt);
    
    const isStampleMode = type === "stample";
    // scheme = [background, border, text, text-shadow]
    const scheme = isStampleMode 
      ? ["orange", "yellow", "black", "orange"]
      : ["gray", "white", "white", "gray"];
    const hoverScheme = isStampleMode
      ? ["yellow", "orange", "black", "yellow"]
      : ["white", "gray", "black", "white"];
    
    stampleBtn.paint({ ink }, scheme, hoverScheme);
  }

  if (!motion.on) {
    ink(255).write("Press to enable motion.", { center: "xy" });
  }

  // 🪧 Add notice for disabling Shake to Undo? 24.11.26.18.32
  if (motion.on) {
    // TODO: Use turtle graphics to draw lines for the rotation?

    // ink("yellow").line
    /*
    ink("white");
    goto(screen.width / 2, screen.height / 2);
    face(values.rotation.alpha - 90);
    down();
    crawl(32);
    up();

    ink("yellow");
    goto(screen.width / 2, screen.height / 2);
    face(values.rotation.beta - 90);
    down();
    crawl(32);
    up();

    ink("blue");
    goto(screen.width / 2, screen.height / 2);
    face(values.rotation.gamma - 90);
    down();
    crawl(32);
    up();
    */

    ink("orange");
    goto(screen.width / 2, screen.height / 2);
    face(-90);
    down();
    crawl(values.accel.y * 32);
    up();
    
    ink("brown");
    goto(screen.width / 2, screen.height / 2);
    face(0);
    down();
    crawl(values.accel.x * 32);
    up();
    
    ink("white", 127);
    goto(screen.width / 2, screen.height / 2);
    face(45);
    down();
    crawl(values.accel.z * 32);
    up();
  }
}

// 🎪 Act
function act({ event: e, motion, sound, pens }) {
  // Handle stample button
  if (stampleBtn) {
    stampleBtn.act(e, {
      down: () => {
        // Cycle through wavetypes
        const currentIndex = wavetypes.indexOf(type);
        const nextIndex = (currentIndex + 1) % wavetypes.length;
        type = wavetypes[nextIndex];
        console.log(`🎵 Rattle: Changed sound to ${type}`);
        
        // Kill the current t4 sound so it gets recreated with the new type
        t4?.kill?.(0.1);
        t4 = null;
        
        // Play a short blip to confirm the change
        sound.synth({
          type: "triangle",
          tone: 880,
          duration: 0.05,
          attack: 0.001,
          volume: 0.3,
        });
      },
    }, pens?.());
  }
}

let t1, t2, t3, t4;
const bass1 = 840;
const bass2 = 850;
const bass3 = 860;
const bass4 = 440;
let t1t = 0,
  t2t = 0,
  t3t = 0,
  t4t = 0;
const lo = 10;
const hi = 900;

// 🧮 Sim
function sim({ num, motion, pen, sound, sound: { synth } }) {
  const mo = motion.current;
  if (mo.accel?.x !== undefined) {
    values = {
      accel: {
        x: mo?.accel?.x.toFixed(6),
        y: mo?.accel?.y.toFixed(6),
        z: mo?.accel?.z.toFixed(6),
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
      t2 = synth({
        type: "noise-white",
        tone: t2t, // 100 * abs(values.rotation.beta),
        volume: 0,
        duration: "🔁",
      });
    }

    if (!t3) {
      t3 = synth({
        type: "noise-white",
        tone: t3t, // 25 * abs(values.rotation.gamma),
        volume: 0,
        duration: "🔁",
      });
    }

    if (!t4) {
      const sampleId = stampleSampleId || fallbackSfx;
      if (type === "stample" && sampleId) {
        t4 = sound.play(sampleId, {
          volume: 0,
          pitch: 440,
          loop: true,
        });
      } else {
        t4 = synth({
          type: type === "stample" ? "noise-white" : type,
          tone: t4t,
          volume: 0,
          duration: "🔁",
        });
      }
    }

    const div = 10;

    function calvol(val) {
      return max(0.0, min(1, abs(val * val) * 50));
    }

    {
      const val = parseFloat(values.rotation.alpha);
      t1t += val / div;
      t1t = num.lerp(t1t, 0, 0.0353);
      values.t1t = bass1 + abs(t1t);
      t1?.update({
        tone: values.t1t,
        volume: 0,//calvol(t1t / (bass1 * 2)),
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
        volume: 0,//calvol(t2t / (bass2 * 2)),
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
        volume: 0,//calvol(t3t / (bass3 * 2)),
        duration: 0.005,
      });
    }

    {
      // const val = (abs(parseFloat(values.accel.x)) + abs(parseFloat(values.accel.y)) + abs(parseFloat(values.accel.z)))/3 ;
      const val = abs(parseFloat(values.accel.y));
      t4t += val;
      t4t = num.lerp(t4t, 0, 0.045);
      values.t4t = -25 + t4t;
      const vol = calvol(t4t / 60);
      values.vol = t4t / 100;
      const tone = 800 + values.t4t * 10;
      values.tone = tone;
      
      // Update with pitch for stample, tone for synth
      if (type === "stample" && stampleSampleId) {
        t4?.update({
          pitch: tone,
          volume: values.vol,
        });
      } else {
        t4?.update({
          tone,
          volume: values.vol,
          duration: 0.005,
        });
      }
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
