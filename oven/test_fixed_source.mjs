// Test fixed source with explicit parentheses
import { KidLispMini, parse } from './kidlisp-mini/eval.mjs';

const originalSource = `fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)`;

const fixedSource = `fade:red-blue-black-blue-red
(ink (? rainbow white 0) (1s... 24 64))
(line w/2 0 w/2 h)
(spin (2s... -1.125 1.125))
(zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
(ink (? cyan yellow magenta) 8)
(circle w/2 h/2 (? 2 4 8))`;

console.log('=== ORIGINAL SOURCE ===');
try {
  const ast1 = parse(originalSource);
  console.log('AST length:', ast1.length);
  console.log('Top-level items (first 5):');
  ast1.slice(0, 5).forEach((item, i) => {
    if (Array.isArray(item)) {
      console.log(`  [${i}]: [${item[0]} ...]`);
    } else {
      console.log(`  [${i}]: ${typeof item === 'object' ? JSON.stringify(item) : item}`);
    }
  });
} catch (e) {
  console.error('Parse error:', e.message);
}

console.log('\n=== FIXED SOURCE ===');
try {
  const ast2 = parse(fixedSource);
  console.log('AST length:', ast2.length);
  console.log('Top-level items (first 5):');
  ast2.slice(0, 5).forEach((item, i) => {
    if (Array.isArray(item)) {
      console.log(`  [${i}]: [${item[0]} ...]`);
    } else {
      console.log(`  [${i}]: ${typeof item === 'object' ? JSON.stringify(item) : item}`);
    }
  });
} catch (e) {
  console.error('Parse error:', e.message);
}
