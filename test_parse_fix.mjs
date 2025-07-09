// Test script to verify the parse fix for share piece routing
import { parse } from './system/public/aesthetic.computer/lib/parse.mjs';

console.log("Testing parse fix for share piece routing...\n");

// Mock location object
const mockLocation = {
  hostname: 'localhost',
  port: '8888'
};

// Test cases
const testCases = [
  {
    input: "share~ink§line§line§line§scroll_1§(once_(wipe_blue))",
    description: "Share piece with multi-line kidlisp parameters",
    expectedPiece: "share"
  },
  {
    input: "(wipe_blue)",
    description: "Standalone kidlisp in parentheses",
    expectedPiece: "(...)"
  },
  {
    input: "wipe_blue§line_red",
    description: "Standalone kidlisp with encoded newlines",
    expectedPiece: "(...)"
  },
  {
    input: "prompt~(wipe_blue)",
    description: "Prompt piece with kidlisp parameter",
    expectedPiece: "prompt"
  },
  {
    input: "line~red",
    description: "Regular piece with parameter",
    expectedPiece: "line"
  }
];

testCases.forEach((testCase, index) => {
  console.log(`Test ${index + 1}: ${testCase.description}`);
  console.log(`Input: ${testCase.input}`);
  
  try {
    const result = parse(testCase.input, mockLocation);
    console.log(`Parsed piece: ${result.piece}`);
    console.log(`Expected piece: ${testCase.expectedPiece}`);
    console.log(`✅ ${result.piece === testCase.expectedPiece ? 'PASS' : 'FAIL'}`);
    
    if (result.piece !== testCase.expectedPiece) {
      console.log(`❌ Expected piece '${testCase.expectedPiece}' but got '${result.piece}'`);
      console.log("Full result:", result);
    }
  } catch (error) {
    console.log(`❌ Error parsing: ${error.message}`);
  }
  
  console.log("---");
});

console.log("Parse fix test completed.");
