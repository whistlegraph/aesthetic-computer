// KidLisp: Spec 24.05.03.22.45
// A test runner for `kidlisp`. 

const tests = ["addition", "subtraction", "timing-highlight", "complex-timing"];

import fs from "fs/promises";
import {
  parse,
  evaluate,
  KidLisp,
  isPromptInKidlispMode,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
} from "../system/public/aesthetic.computer/lib/kidlisp.mjs";

// ANSI color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
  orange: '\x1b[38;5;208m',
  lime: '\x1b[38;5;10m',
  purple: '\x1b[38;5;93m',
  olive: '\x1b[38;5;58m',
  gray: '\x1b[90m'
};

// Function to convert kidlisp color codes to ANSI terminal colors
function printColoredOutput(coloredText) {
  if (!coloredText) {
    console.log("(no colored output)");
    return;
  }
  
  let output = "";
  let currentPos = 0;
  
  // Parse color escape sequences like \red\, \green\, etc.
  const colorRegex = /\\([^\\]+)\\/g;
  let match;
  
  while ((match = colorRegex.exec(coloredText)) !== null) {
    // Add any text before this color code
    output += coloredText.substring(currentPos, match.index);
    
    // Add the ANSI color code
    const colorName = match[1];
    if (colors[colorName]) {
      output += colors[colorName];
    } else if (colorName.includes(',')) {
      // Handle RGB colors like "255,255,255,0" (transparent)
      const [r, g, b, a] = colorName.split(',').map(Number);
      if (a === 0) {
        // Transparent - use dark gray
        output += colors.gray;
      } else {
        // Other RGB - try to map to closest ANSI color
        if (r > 200 && g < 100 && b < 100) output += colors.red;
        else if (r < 100 && g > 200 && b < 100) output += colors.green;
        else if (r < 100 && g < 100 && b > 200) output += colors.blue;
        else if (r > 200 && g > 200 && b < 100) output += colors.yellow;
        else if (r > 200 && g < 100 && b > 200) output += colors.magenta;
        else if (r < 100 && g > 200 && b > 200) output += colors.cyan;
        else output += colors.white;
      }
    } else {
      // Unknown color, use default
      output += colors.white;
    }
    
    currentPos = match.index + match[0].length;
  }
  
  // Add any remaining text
  output += coloredText.substring(currentPos);
  
  // Reset color at the end
  output += colors.reset;
  
  console.log(output);
}

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

  it("Test timing expression syntax highlighting", () => {
    console.log("🎨 Running timing expression syntax highlighting test...");
    console.log(`📄 Test source: ${pieces["timing-highlight"].src}`);
    
    const lisp = new KidLisp();
    
    // Initialize syntax highlighting
    lisp.initializeSyntaxHighlighting(pieces["timing-highlight"].src);
    
    // Mock API for evaluation
    const mockApi = {
      write: (...args) => console.log('Write:', args.join(' '))
    };
    
    // Parse and evaluate to trigger AST tagging
    const parsed = lisp.parse(pieces["timing-highlight"].src);
    console.log("📊 AST structure:", JSON.stringify(parsed, null, 2));
    
    // Test initial highlighting (before evaluation)
    const initialHighlight = lisp.buildColoredKidlispString();
    console.log("🎨 Initial highlighting:", initialHighlight);
    
    // Evaluate to trigger timing logic
    const result = lisp.evaluate(parsed, mockApi);
    console.log("⚡ Evaluation result:", result);
    
    // Test highlighting after evaluation
    const finalHighlight = lisp.buildColoredKidlispString();
    console.log("🎨 Final highlighting:", finalHighlight);
    
    // Test AST-based highlighting specifically
    const astHighlight = lisp.buildASTBasedHighlighting();
    console.log("🧠 AST-based highlighting:", astHighlight);
    
    // Test with colored terminal output
    console.log("\n🌈 COLORED TERMINAL OUTPUT:");
    printColoredOutput(finalHighlight);
    
    expect(finalHighlight).toBeDefined();
    expect(finalHighlight.length).toBeGreaterThan(0);
  });

  it("Test complex timing expression highlighting", () => {
    console.log("🎨 Running complex timing expression highlighting test...");
    console.log(`📄 Test source: ${pieces["complex-timing"].src}`);
    
    const lisp = new KidLisp();
    
    // Initialize syntax highlighting for multi-line source
    lisp.initializeSyntaxHighlighting(pieces["complex-timing"].src);
    
    // Mock API for evaluation
    const mockApi = {
      beige: () => console.log('Beige called'),
      ink: (...args) => console.log('Ink:', args.join(' ')),
      write: (...args) => console.log('Write:', args.join(' ')),
      scroll: (...args) => console.log('Scroll:', args.join(' ')),
      blur: (...args) => console.log('Blur:', args.join(' ')),
      zoom: (...args) => console.log('Zoom:', args.join(' ')),
      rainbow: () => [255, 0, 0],
      '?': (...args) => args[Math.floor(Math.random() * args.length)],
      clock: {
        time: () => new Date() // Provide the missing clock API
      }
    };
    
    // Parse and evaluate
    const parsed = lisp.parse(pieces["complex-timing"].src);
    console.log("📊 Complex AST structure (first 3 expressions):", 
                JSON.stringify(parsed.slice(0, 3), null, 2));
    
    // Test highlighting before evaluation
    const beforeHighlight = lisp.buildColoredKidlispString();
    console.log("🎨 Before evaluation:", beforeHighlight);
    
    // Evaluate to trigger timing logic
    const result = lisp.evaluate(parsed, mockApi);
    console.log("⚡ Complex evaluation result:", result);
    
    // Test highlighting after evaluation
    const afterHighlight = lisp.buildColoredKidlispString();
    console.log("🎨 After evaluation:", afterHighlight);
    
    // Test with colored terminal output
    console.log("\n🌈 COMPLEX COLORED TERMINAL OUTPUT:");
    printColoredOutput(afterHighlight);
    
    expect(afterHighlight).toBeDefined();
    expect(afterHighlight.length).toBeGreaterThan(0);
    expect(afterHighlight).toContain('\\'); // Should contain color codes
  });

  it("Auto-close incomplete expressions", () => {
    console.log("🔧 Testing auto-closing of incomplete expressions...");
    
    const testCases = [
      { 
        input: "(+ 1 2", 
        expected: [["+"," ","1", 2]], 
        desc: "incomplete addition" 
      },
      { 
        input: "(line 10 20", 
        expected: [["line", 10, 20]], 
        desc: "incomplete function call" 
      },
      { 
        input: "(+ (- 5 2", 
        expected: [["+", ["-", 5, 2]]], 
        desc: "nested incomplete expression" 
      },
      { 
        input: "(def x (+ 1", 
        expected: [["def", "x", ["+", 1]]], 
        desc: "incomplete definition" 
      }
    ];
    
    testCases.forEach(({ input, expected, desc }) => {
      console.log(`  Testing ${desc}: "${input}"`);
      try {
        const parsed = parse(input);
        console.log(`    Parsed successfully:`, parsed);
        expect(Array.isArray(parsed)).toBe(true);
        expect(parsed.length).toBeGreaterThan(0);
      } catch (error) {
        fail(`    Failed to parse incomplete expression: ${error.message}`);
      }
    });
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
