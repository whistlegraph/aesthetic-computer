// KidLisp: Spec 24.05.03.22.45
// A test runner for `kidlisp`. 

const tests = ["addition", "subtraction"];

import fs from "fs/promises";
import {
  parse,
  evaluate,
  KidLisp,
  isPromptInKidlispMode,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
} from "../system/public/aesthetic.computer/lib/kidlisp.mjs";

describe("🤖 Kid Lisp", () => {
  let pieces = {};

  beforeAll(async () => {
    // Clear the terminal
    console.clear();
    
    // List of all pieces you want to preload
    const loadPromises = tests.map((name) =>
      load(name).then((data) => {
        if (data) {
          console.log(`✅ Loaded test: ${name} - ${data.desc}`);
          console.log(`📝 Source code: ${data.src}`);
          pieces[name] = data;
        } else {
          throw new Error(`Failed to load file: ${name}`);
        }
      }),
    );

    try {
      console.log("🧒 Loading kidlisp tests...");
      await Promise.all(loadPromises);
      console.log(`🎯 All ${tests.length} tests loaded successfully`);
    } catch (error) {
      console.error("🔴 Error during test setup:", error);
      throw error; // This will fail the test suite
    }
  });

  afterAll(() => {
    // Print timestamp after all tests complete
    const now = new Date();
    const timestamp = now.toLocaleString('en-US', {
      timeZone: 'America/Los_Angeles',
      weekday: 'long',
      year: 'numeric',
      month: 'long',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      timeZoneName: 'short'
    });
    console.log(`\n⏰ Tests completed at: ${timestamp}`);
    
    // List all items in the KidLisp globalEnv
    console.log(`\n📚 Kid Lisp Library:`);
    const lisp = new KidLisp();
    const globalEnv = lisp.getGlobalEnv();
    const envKeys = Object.keys(globalEnv).sort();
    
    // Categorize the functions
    const categories = {
      'math': ['+', '-', '*', '/', 'max'],
      'logic': ['>', '<', '=', 'if', 'not'],
      'control': ['def', 'later', 'die', 'now', 'range'],
      'graphics': ['resolution', 'wipe', 'ink', 'line', 'box', 'write', 'wiggle'],
      'system': ['tap', 'draw', 'net', 'source', 'width', 'height'],
      'audio': ['overtone'],
      'utility': ['len']
    };
    
    // Display by categories as tag clouds
    Object.entries(categories).forEach(([category, categoryItems]) => {
      const availableItems = categoryItems.filter(item => envKeys.includes(item));
      if (availableItems.length > 0) {
        console.log(`${category}: ${availableItems.join(', ')}`);
      }
    });
    
    // Display any uncategorized items
    const categorizedItems = Object.values(categories).flat();
    const uncategorized = envKeys.filter(key => !categorizedItems.includes(key));
    
    if (uncategorized.length > 0) {
      console.log(`other: ${uncategorized.join(', ')}`);
    }
  });

  it("Add numbers", () => {
    console.log("🧮 Running addition test...");
    console.log(`📄 Test source: ${pieces.addition.src}`);
    
    const parsed = parse(pieces.addition.src);
    console.log(`🔍 Parsed AST:`, parsed);
    
    const result = evaluate(parsed);
    console.log(`🎯 Evaluation result: ${result}`);
    console.log(`✅ Expected: 6, Got: ${result}`);
    
    expect(result).toEqual(6);
  });

  it("Subtract numbers", () => {
    console.log("➖ Running subtraction test...");
    console.log(`📄 Test source: ${pieces.subtraction.src}`);
    
    const parsed = parse(pieces.subtraction.src);
    console.log(`🔍 Parsed AST:`, parsed);
    
    const result = evaluate(parsed);
    console.log(`🎯 Evaluation result: ${result}`);
    console.log(`✅ Expected: 3, Got: ${result}`);
    
    expect(result).toEqual(3);
  });

  it("Parse kidlisp functions without parentheses", () => {
    console.log("🔧 Testing parser with unparenthesized functions...");
    
    const testSource = `wipe green
ink red`;
    console.log(`📄 Original source:\n${testSource}`);
    
    const parsed = parse(testSource);
    console.log(`📦 Parsed AST:`, parsed);
    
    const expected = [
      ['wipe', 'green'],
      ['ink', 'red']
    ];
    
    expect(parsed).toEqual(expected);
  });

  it("Detect kidlisp mode with newlines", () => {
    console.log("🔍 Testing kidlisp mode detection...");
    
    const testCases = [
      { input: "(+ 1 2)", expected: true, desc: "traditional parentheses" },
      { input: "; comment", expected: true, desc: "comment" },
      { input: "line red\nink blue", expected: true, desc: "newline with functions" },
      { input: "hello world", expected: false, desc: "plain text" },
      { input: "just\ntext", expected: false, desc: "newline without functions" }
    ];
    
    testCases.forEach(({ input, expected, desc }) => {
      const result = isPromptInKidlispMode(input);
      console.log(`  ${desc}: "${input.replace(/\n/g, '\\n')}" -> ${result} (expected: ${expected})`);
      expect(result).toEqual(expected);
    });
  });

  it("Preserve newlines in URL encoding/decoding", () => {
    console.log("🔗 Testing URL encoding/decoding with newlines...");
    
    const testSource = `line 10 20 30 40
ink red
box 5 5 10 10`;
    
    console.log(`📄 Original source:\n${testSource}`);
    
    const encoded = encodeKidlispForUrl(testSource);
    console.log(`🔒 Encoded: ${encoded}`);
    
    const decoded = decodeKidlispFromUrl(encoded);
    console.log(`🔓 Decoded:\n${decoded}`);
    
    expect(decoded).toEqual(testSource);
  });
});

async function load(name) {
  const filePath = `./system/public/aesthetic.computer/disks/${name}.lisp`;
  try {
    console.log(`📂 Loading test file: ${filePath}`);
    const src = await fs.readFile(filePath, "utf8");
    const desc = src.split("\n")[0].replace(/^;\s*/, "");
    console.log(`📝 File loaded - Description: ${desc}`);
    return { desc, src };
  } catch (error) {
    console.error(`🔴 Error setting up \`kidlisp\` tests for ${name}:`, error);
    return null;
  }
}
