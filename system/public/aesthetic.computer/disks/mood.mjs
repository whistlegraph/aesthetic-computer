// Mood, 23.01.13.14.18
// Choose a mood and build an image using tools from that mood.
// Designed w/ Molly Soda

/* #region 🏁 todo
+ Now
  - [] Make a face model similar to the sketches: https://www.figma.com/file/QnWOcgXFWdl4Zu9sJpNX6W/Make-A-Portrait?node-id=0%3A1&t=By8B8YzFWCHpxCJt-1 
    - [] Make lines for -_- face.
    - [] Draw filled colored circles.
    - [] Make smiley bendable. 
    - [] Transition from -_- -> :) -> :(
+ Later
+ Done
- [x] Make palette. 
- [x] Draw background and colored outline circle.
#endregion */

const palette = {
  neutral: [0, 255, 0], // green
  happy: [255, 255, 0], // yellow
  sad: [65, 95, 255], // blue
  angry: [245, 0, 0], // red
};

const bg = [245, 130, 195];

let faceSize = 64;

// 🥾 Boot (Runs once before first paint and sim)
function boot() {}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, circle, screen, pen, num, line }) {
  // Computation
  // const eyeWidth = faceSize / 12;
  const eyeWidth = faceSize / 6;

  const eye = {
    a: { x: -eyeWidth, y: 0 },
    b: { x: eyeWidth, y: 0 },
  };

  // Creating positions for left and right eyes to be drawn from `eye`.

  const eyeY = faceSize / 6;

  const left = {
    x: screen.center.x - faceSize / 2,
    y: screen.center.y - eyeY,
  };

  const right = {
    x: screen.center.x + faceSize / 2,
    y: screen.center.y - eyeY,
  };

  const leftEyeA = num.p2.add(eye.a, left);
  const leftEyeB = num.p2.add(eye.b, left);

  const rightEyeA = num.p2.add(eye.a, right);
  const rightEyeB = num.p2.add(eye.b, right);

  // Rendering
  wipe(bg);
  ink(palette.neutral);
  circle(...screen.center, faceSize); // Face
  line(leftEyeA.x, leftEyeA.y, leftEyeB.x, leftEyeB.y); // Left Eye
  line(rightEyeA.x, rightEyeA.y, rightEyeB.x, rightEyeB.y); // Right Eye

  // Make mouth line...

  const mouthWidth = eyeWidth * 2;

  const mouth = {
    a: { x: -mouthWidth, y: 0 },
    b: { x: mouthWidth, y: 0 },
  };

  const mouthPos = {
    x: screen.center.x,
    y: screen.center.y + faceSize / 2.5,
  };

  const mouthA = num.p2.add(mouth.a, mouthPos);
  const mouthB = num.p2.add(mouth.b, mouthPos);

  line(mouthA.x, mouthA.y, mouthB.x, mouthB.y);
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, jump }) {
  // if (e.is("touch")) faceSize += 2;
  if (e.is("touch")) jump("camera");
}

/*
// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, act, paint };
