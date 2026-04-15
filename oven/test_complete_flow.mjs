// Test complete parsing and evaluation flow
import { KidLispMini, parse } from './kidlisp-mini/eval.mjs';
import { makeRenderer } from './kidlisp-mini/render.mjs';

// Test source (as it should be fixed)
const source = `fade:red-blue-black-blue-red
(ink (? rainbow white 0) (1s... 24 64))
(line w/2 0 w/2 h)
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
(ink (? cyan yellow magenta) 8)
(circle w/2 h/2 (? 2 4 8))`;

console.log('=== FIXED SOURCE PARSING ===');
const ast = parse(source);
console.log('✓ AST parsed successfully');
console.log('  AST length:', ast.length, '(expected: 9)');

console.log('\n=== TOP-LEVEL EXPRESSIONS ===');
ast.forEach((expr, i) => {
  if (Array.isArray(expr)) {
    const fn = expr[0];
    const argCount = expr.length - 1;
    console.log(`  [${i}] (${fn} ...${argCount > 0 ? ` ${argCount} args` : ''})`);
  } else if (typeof expr === 'object' && expr.type === 'fade') {
    console.log(`  [${i}] fade:${expr.colors.join('-')}`);
  } else {
    console.log(`  [${i}] ${expr}`);
  }
});

console.log('\n=== MOCK EVALUATION ===');
// Create a mock renderer to track calls
const W = 1280, H = 800;
const renderer = makeRenderer(W, H);
const api = {
  screen: { width: W, height: H },
  wipe: (r, g, b, a = 255) => console.log(`    api.wipe(${r},${g},${b},${a})`),
  ink: (r, g, b, a = 255) => console.log(`    api.ink(${r},${g},${b},${a})`),
  line: (x0, y0, x1, y1) => console.log(`    api.line(${x0},${y0},${x1},${y1})`),
  circle: (cx, cy, r, mode) => console.log(`    api.circle(${cx},${cy},${r},${mode})`),
  fadeBackground: (colors, frame) => console.log(`    api.fadeBackground([...${colors.length} colors],${frame})`),
  scroll: (dx, dy) => console.log(`    api.scroll(${dx},${dy})`),
  spin: (angle) => console.log(`    api.spin(${angle})`),
  zoom: (factor) => console.log(`    api.zoom(${factor})`),
  contrast: (factor) => console.log(`    api.contrast(${factor})`),
};

const kidlisp = new KidLispMini();
kidlisp.setApi(api);

console.log('Evaluating frame 0...');
for (let i = 0; i < Math.min(3, ast.length); i++) {
  const expr = ast[i];
  console.log(`  expr[${i}]:`);
  try {
    const result = kidlisp.evalExpr(expr, kidlisp.globalEnv, api, 0);
    console.log(`    → result: ${typeof result === 'object' ? JSON.stringify(result).substring(0, 50) : result}`);
  } catch (e) {
    console.log(`    → ERROR: ${e.message}`);
  }
}

console.log('\n✅ Flow complete!');
