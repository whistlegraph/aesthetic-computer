// 🔁 Loop

// These numbers define the budgeted frame-time (max) for CPU and Rendering.
// The updates will repeat multiple times per frame, but rendering will only
// ever happen once per display refresh.

const updateFps = 120; // This is constant and should be used for interpolation.
let renderFps = 165; // This is a maximum and will vary across environments.
const updateRate = 1000 / updateFps;
let renderRate = 1000 / renderFps;
let updateTime = 0;
let renderTime = 0;
let lastNow;
let input;
let updateAndRender;

// Input runs once per loop.
// Update runs multiple times.
// Render runs once if enough time has passed.
function loop(now, XR = false) {
  input(now);

  const delta = now - lastNow;

  updateTime += delta;
  renderTime += delta;
  lastNow = now;

  let updateTimes = 0;

  while (updateTime >= updateRate) {
    updateTimes += 1;
    updateTime -= updateRate;
  }

  let needsRender = false;

  if (renderTime >= renderRate) {
    needsRender = true;
    renderTime -= renderRate;
  }

  updateAndRender(needsRender, updateTimes, now);

  // We don't use requestAnimationframe when
  // running the main loop in an VR/AR/XR environment.
  if (!XR) window.requestAnimationFrame(loop);
}

// Reset simulation updates when the document resumes focus.
// (Prevent simulations from stacking up and runnning at once when suspended)
document.addEventListener("visibilitychange", function () {
  if (!document.hidden) {
    updateTime = 0;
    lastNow = performance.now();
  }
});

// Start the loop.
function start(inputFun, updateAndRenderFun) {
  input = inputFun;
  updateAndRender = updateAndRenderFun;
  lastNow = performance.now();
  window.requestAnimationFrame(loop);
}

// Update the frame rate.
function frameRate(n = 165) {
  renderFps = n;
  if (renderFPS !== 165) console.log("🎞️ FPS:", renderFps);
  renderRate = 1000 / renderFps;
  renderTime = 0;
}

export const mainLoop = loop;
export { start, frameRate };
