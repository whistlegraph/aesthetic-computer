// Test spin with timing form
import { KidLispMini, parse } from './kidlisp-mini/eval.mjs';

const source = `(spin (2s... -1.125 1.125))`;

const ast = parse(source);
console.log('Parsed AST:', JSON.stringify(ast, null, 2));

const kidlisp = new KidLispMini();
const calls = [];
const api = {
  screen: { width: 1280, height: 800 },
  spin: (angle) => {
    calls.push({ fn: 'spin', angle });
    console.log(`  spin(${angle.toFixed(4)})`);
  },
};

kidlisp.setApi(api);

console.log('\n=== TEST: spin with timing form ===\n');

// Test frames at different points in the 2-second cycle
const testFrames = [0, 30, 60, 120];

testFrames.forEach(frame => {
  calls.length = 0;
  const ms = 2000;
  const progress = (frame * 16) % ms / ms;
  const expectedAngle = -1.125 + (1.125 - (-1.125)) * progress;

  console.log(`Frame ${frame} (${(frame/60).toFixed(2)}s, progress=${progress.toFixed(3)}):`);

  kidlisp.frameCount = frame;
  const expr = ast[0];
  console.log(`  Expr: ${JSON.stringify(expr)}`);
  kidlisp.evalExpr(expr, kidlisp.globalEnv, api, frame);

  const call = calls[0];
  if (call) {
    console.log(`  Result: ${call.angle.toFixed(4)} (expected: ${expectedAngle.toFixed(4)})`);
    if (Math.abs(call.angle - expectedAngle) > 0.01) {
      console.log(`  ⚠️  OFF BY ${(call.angle - expectedAngle).toFixed(4)}`);
    }
  }
  console.log();
});
