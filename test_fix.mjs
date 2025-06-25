import { isKidlispSource, isPromptInKidlispMode } from "./system/public/aesthetic.computer/lib/kidlisp.mjs";

console.log("🧪 Testing kidlisp detection fix...\n");

const testCases = [
  // Cases that should NOT be detected as kidlisp
  { input: "prutti~3", expected: false, desc: "piece slug with parameter" },
  { input: "prutti~red", expected: false, desc: "piece slug with color parameter" },
  { input: "line~red", expected: false, desc: "simple piece with parameter" },
  { input: "hello~world", expected: false, desc: "simple text with tilde" },
  { input: "prutti_3", expected: false, desc: "piece slug with underscore parameter" },
  
  // Cases that SHOULD be detected as kidlisp
  { input: "(wipe blue)", expected: true, desc: "parentheses kidlisp" },
  { input: "; comment", expected: true, desc: "comment" },
  { input: "wipe blue\nink red", expected: true, desc: "multiline with function and arguments" },
  { input: "line 10 20\nbox 5 5", expected: true, desc: "multiline functions with arguments" },
  { input: "wipe_blue~ink_red", expected: true, desc: "encoded multiline kidlisp" },
];

testCases.forEach(({ input, expected, desc }) => {
  const result = isKidlispSource(input);
  const status = result === expected ? "✅" : "❌";
  console.log(`${status} ${desc}: "${input}" -> ${result} (expected: ${expected})`);
  
  if (result !== expected) {
    console.log(`   FAILED: Expected ${expected}, got ${result}`);
  }
});

console.log("\n🧪 Testing isPromptInKidlispMode...\n");

testCases.forEach(({ input, expected, desc }) => {
  const result = isPromptInKidlispMode(input);
  const status = result === expected ? "✅" : "❌";
  console.log(`${status} ${desc}: "${input}" -> ${result} (expected: ${expected})`);
  
  if (result !== expected) {
    console.log(`   FAILED: Expected ${expected}, got ${result}`);
  }
});
