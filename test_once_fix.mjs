// Test script to verify the 'once' command reset behavior
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const kidlispPath = join(__dirname, 'system/public/aesthetic.computer/lib/kidlisp.mjs');

// Dynamically import kidlisp
const kidlispModule = await import('./system/public/aesthetic.computer/lib/kidlisp.mjs');
const { module: createKidlispModule } = kidlispModule;

console.log("🧪 Testing 'once' command reset behavior...\n");

// Test 1: Same source should preserve 'once' state
console.log("📝 Test 1: Same source should preserve 'once' state");
const source1 = `(once (def x 42))
(+ x 1)`;

let moduleA = createKidlispModule(source1);
let result1 = moduleA.boot({});
console.log("  First run:", result1); // Should execute 'once' and set x=42

let moduleA2 = createKidlispModule(source1); // Same source
let result2 = moduleA2.boot({});
console.log("  Second run (same source):", result2); // Should NOT re-execute 'once', x should still be 42

// Test 2: Different source should reset 'once' state
console.log("\n📝 Test 2: Different source should reset 'once' state");
const source2 = `(once (def x 100))
(+ x 1)`;

let moduleB = createKidlispModule(source2); // Different source
let result3 = moduleB.boot({});
console.log("  First run (different source):", result3); // Should execute new 'once' and set x=100

// Test 3: Back to original source should reset again
console.log("\n📝 Test 3: Back to original source should reset again");
let moduleC = createKidlispModule(source1); // Back to original source
let result4 = moduleC.boot({});
console.log("  Back to original source:", result4); // Should execute 'once' again and set x=42

console.log("\n✅ Test completed!");
