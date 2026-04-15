// Verify the bundled source parses correctly
import { parse } from './kidlisp-mini/eval.mjs';

const bundledSource = `fade:red-blue-black-blue-red
(ink (? rainbow white 0) (1s... 24 64))
(line w/2 0 w/2 h)
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
(ink (? cyan yellow magenta) 8)
(circle w/2 h/2 (? 2 4 8))`;

const ast = parse(bundledSource);
console.log('=== PARSED AST ===');
console.log('AST length:', ast.length);
console.log('Top-level expressions:');
ast.forEach((expr, i) => {
  if (Array.isArray(expr)) {
    console.log(`  [${i}]: (${expr[0]} ...)`);
  } else if (typeof expr === 'object' && expr.type === 'fade') {
    console.log(`  [${i}]: fade:${expr.colors.join('-')}`);
  } else {
    console.log(`  [${i}]: ${expr}`);
  }
});

console.log('\n=== FUNCTION CALLS ===');
ast.forEach((expr, i) => {
  if (Array.isArray(expr) && typeof expr[0] === 'string') {
    const fn = expr[0];
    const args = expr.slice(1).length;
    console.log(`  ${fn}: ${args} args`);
  }
});
