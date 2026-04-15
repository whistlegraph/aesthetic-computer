// Test animation frame progression and timing
import { KidLispMini, parse } from './kidlisp-mini/eval.mjs';
import { TimingEngine } from './kidlisp-mini/timing.mjs';

const source = `fade:red-blue-black-blue-red
(ink (? rainbow white 0) (1s... 24 64))
(line w/2 0 w/2 h)
(spin (2s... -1.125 1.125))
(zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
(ink (? cyan yellow magenta) 8)
(circle w/2 h/2 (? 2 4 8))`;

const ast = parse(source);
const kidlisp = new KidLispMini();
const timing = new TimingEngine(42); // seed

// Track API calls
const apiCalls = [];
const api = {
  screen: { width: 1280, height: 800 },
  wipe: (r, g, b, a) => apiCalls.push({ fn: 'wipe', args: [r, g, b, a] }),
  ink: (r, g, b, a) => apiCalls.push({ fn: 'ink', args: [r, g, b, a] }),
  line: (x0, y0, x1, y1) => apiCalls.push({ fn: 'line', args: [x0, y0, x1, y1] }),
  circle: (cx, cy, r, mode) => apiCalls.push({ fn: 'circle', args: [cx, cy, r, mode] }),
  fadeBackground: (colors, frame) => apiCalls.push({ fn: 'fadeBackground', colorCount: colors.length, frame }),
  scroll: (dx, dy) => apiCalls.push({ fn: 'scroll', args: [dx, dy] }),
  spin: (angle) => apiCalls.push({ fn: 'spin', args: [angle] }),
  zoom: (factor) => apiCalls.push({ fn: 'zoom', args: [factor] }),
  contrast: (factor) => apiCalls.push({ fn: 'contrast', args: [factor] }),
};

kidlisp.setApi(api);

console.log('=== DEBUG: API and Environment ===');
console.log('API set:', !!kidlisp.api);
console.log('API.ink:', !!kidlisp.api?.ink);
console.log('globalEnv.ink:', !!kidlisp.globalEnv.ink);
console.log();

console.log('=== FRAME-BY-FRAME ANIMATION TEST ===\n');

// Test frames 0, 30 (0.5s at 60fps), 60 (1s), 120 (2s)
const testFrames = [0, 30, 60, 120];

testFrames.forEach(frameNum => {
  apiCalls.length = 0; // reset
  kidlisp.frameCount = frameNum;

  console.log(`Frame ${frameNum} (${(frameNum/60).toFixed(2)}s):`);

  // Evaluate only key expressions to see what's happening
  // expr 1: ink with timing (1s... 24 64)
  const inkExpr = ast[1];
  const inkResult = kidlisp.evalExpr(inkExpr, kidlisp.globalEnv, api, frameNum);
  console.log(`  ink evalExpr result: ${inkResult}`);
  const inkCall = apiCalls.find(c => c.fn === 'ink');
  console.log(`  ink API called: ${!!inkCall}`);
  console.log(`  ink alpha arg: ${inkCall?.args[3]} (interpolating 24→64 over 1s)`);

  // expr 3: spin with timing (2s... -1.125 1.125)
  apiCalls.length = 0;
  const spinExpr = ast[3];
  const spinResult = kidlisp.evalExpr(spinExpr, kidlisp.globalEnv, api, frameNum);
  const spinCall = apiCalls.find(c => c.fn === 'spin');
  const expectedSpin = -1.125 + ((1.125 - (-1.125)) * ((frameNum * 16) % 2000) / 2000);
  const spinAngle = spinCall?.args[0];
  console.log(`  spin API called: ${!!spinCall}`);
  console.log(`  spin angle: ${typeof spinAngle === 'number' ? spinAngle.toFixed(4) : spinAngle} (expected: ${expectedSpin.toFixed(4)})`);

  // expr 5: 0.5s (contrast 1.05)
  apiCalls.length = 0;
  const contrastExpr = ast[5];
  const contrastResult = kidlisp.evalExpr(contrastExpr, kidlisp.globalEnv, api, frameNum);
  console.log(`  0.5s contrast result: ${contrastResult?.toFixed(4)} (should be 0→1 over 0.5s)`);

  console.log();
});

console.log('=== TIMING FORM EVALUATION ===');
console.log('Checking timing form handling...');

// Check if timing form [timing, startVal, endVal] is being evaluated
const testTimingExpr = [{ type: 'timing-repeating', ms: 1000 }, 24, 64];
console.log('Test expr: [timing-repeating(1000ms), 24, 64]');
for (let f = 0; f <= 60; f += 15) {
  const result = kidlisp.evalExpr(testTimingExpr, kidlisp.globalEnv, api, f);
  const progress = (f * 16) % 1000 / 1000;
  const expected = 24 + (64 - 24) * progress;
  console.log(`  Frame ${f}: result=${result?.toFixed(1)}, expected=${expected.toFixed(1)}`);
}
