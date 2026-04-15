// Test ink function with new color+alpha pattern
import { KidLispMini, parse } from './kidlisp-mini/eval.mjs';

const source = `(ink (? white black) (1s... 32 224))
(circle w/2 h/2 100)`;

const ast = parse(source);
const kidlisp = new KidLispMini();

// Mock API to track calls
const calls = [];
const api = {
  screen: { width: 1280, height: 800 },
  ink: (...args) => {
    calls.push({ fn: 'ink', args });
    console.log(`  ink(${args.map(a => Array.isArray(a) ? `[${a}]` : a).join(', ')})`);
  },
  circle: (cx, cy, r) => {
    calls.push({ fn: 'circle', args: [cx, cy, r] });
    console.log(`  circle(${cx}, ${cy}, ${r})`);
  },
};

kidlisp.setApi(api);

console.log('=== TEST: ink with color + alpha override ===\n');

// Test frames
for (let frame of [0, 30, 60]) {
  calls.length = 0;
  console.log(`Frame ${frame} (${(frame/60).toFixed(2)}s):`);

  kidlisp.frameCount = frame;
  for (const expr of ast) {
    kidlisp.evalExpr(expr, kidlisp.globalEnv, api, frame);
  }

  const inkCall = calls.find(c => c.fn === 'ink');
  if (inkCall) {
    const [color, alpha] = inkCall.args;
    console.log(`    → Color: ${Array.isArray(color) ? `[${color.join(',')}]` : color}, Alpha: ${alpha}`);
  }
  console.log();
}

console.log('✅ ink pattern test complete');
