// KidLisp: Spec 24.05.03.22.45
// A test runner for `kidlisp`. 

const tests = ["addition", "subtraction", "timing-highlight", "complex-timing"];

import fs from "fs/promises";
import path from "path";
import { fileURLToPath } from "url";
import {
  parse,
  evaluate,
  KidLisp,
  isPromptInKidlispMode,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
} from "../system/public/aesthetic.computer/lib/kidlisp.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ANSI color codes for terminal output
const colors = {
  reset: "\x1b[0m",
  bright: "\x1b[1m",
  red: "\x1b[31m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  magenta: "\x1b[35m",
  cyan: "\x1b[36m",
  white: "\x1b[37m",
  orange: "\x1b[38;5;208m",
  lime: "\x1b[38;5;10m",
  purple: "\x1b[38;5;93m",
  olive: "\x1b[38;5;58m",
  gray: "\x1b[90m",
};

function isRgbEscape(text) {
  return /^\d{1,3},\d{1,3},\d{1,3}(,\d{1,3})?$/.test(text);
}

function rgbEscapeToAnsi(text) {
  const parts = text.split(",").map((p) => Number.parseInt(p.trim(), 10));
  if (parts.length < 3) return "";
  const [r, g, b] = parts;
  if ([r, g, b].some((n) => Number.isNaN(n))) return "";
  return `\x1b[38;2;${Math.max(0, Math.min(255, r))};${Math.max(0, Math.min(255, g))};${Math.max(0, Math.min(255, b))}m`;
}

// Convert kidlisp color escapes (e.g. \yellow\ or \255,0,0,255\) to ANSI.
function kidlispColoredToAnsi(coloredText) {
  if (!coloredText) return "";

  let output = "";
  let currentPos = 0;

  const colorRegex = /\\([^\\]+)\\/g;
  let match;

  while ((match = colorRegex.exec(coloredText)) !== null) {
    output += coloredText.substring(currentPos, match.index);

    const colorName = match[1];
    if (colors[colorName]) {
      output += colors[colorName];
    } else if (isRgbEscape(colorName)) {
      output += rgbEscapeToAnsi(colorName);
    }

    currentPos = match.index + match[0].length;
  }

  output += coloredText.substring(currentPos);
  output += colors.reset;
  return output;
}

function printColoredOutput(coloredText) {
  if (!coloredText) {
    console.log("(no colored output)");
    return;
  }
  console.log(kidlispColoredToAnsi(coloredText));
}

function printSyntaxHighlightedKidlisp(source) {
  const lisp = new KidLisp();
  lisp.initializeSyntaxHighlighting(source);
  const colored = lisp.buildColoredKidlispString();
  printColoredOutput(colored);
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
          console.log("📝 Source code (syntax highlighted):");
          printSyntaxHighlightedKidlisp(data.src);
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
    
    // List all items in the KidLisp globalEnv (colorized)
    console.log("\n");
    printColoredOutput("\\bright\\\\cyan\\📚 Kid Lisp Library:\\reset\\");
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

    const categoryColors = {
      math: "yellow",
      logic: "lime",
      control: "orange",
      graphics: "cyan",
      system: "purple",
      audio: "magenta",
      utility: "olive",
      other: "gray",
    };

    const isRgbArray = (v) =>
      Array.isArray(v) &&
      (v.length === 3 || v.length === 4) &&
      v.slice(0, 3).every((n) => Number.isFinite(n));

    const clamp255 = (n) => Math.max(0, Math.min(255, Math.round(n)));

    const formatLibraryItem = (item, fallbackColor) => {
      const value = globalEnv[item];

      // If this identifier is a color constant in the env, show it in that exact RGB.
      if (isRgbArray(value)) {
        const r = clamp255(value[0]);
        const g = clamp255(value[1]);
        const b = clamp255(value[2]);
        return `\\${r},${g},${b},255\\${item}\\reset\\`;
      }

      // Otherwise colorize by category.
      const color = fallbackColor || "white";
      return `\\${color}\\${item}\\reset\\`;
    };

    const joinStyled = (items, fallbackColor) => {
      const sep = "\\gray\\, \\reset\\";
      return items.map((i) => formatLibraryItem(i, fallbackColor)).join(sep);
    };
    
    // Display by categories as tag clouds (colorized)
    Object.entries(categories).forEach(([category, categoryItems]) => {
      const availableItems = categoryItems.filter((item) => envKeys.includes(item));
      if (availableItems.length > 0) {
        const color = categoryColors[category] || "white";
        printColoredOutput(
          `\\${color}\\${category}\\reset\\: ${joinStyled(availableItems, color)}`,
        );
      }
    });
    
    // Display any uncategorized items
    const categorizedItems = Object.values(categories).flat();
    const uncategorized = envKeys.filter(key => !categorizedItems.includes(key));
    
    if (uncategorized.length > 0) {
      printColoredOutput(
        `\\${categoryColors.other}\\other\\reset\\: ${joinStyled(uncategorized, categoryColors.other)}`,
      );
    }

    // --- Syntax-highlighted preview (uses kidlisp.mjs highlighter) ---
    // This makes the library dump reflect exactly what the client highlighter does.
    const snippetForItem = (item, category) => {
      // Operators and math-ish tokens
      if (["+", "-", "*", "/", "%", "max", "min", "mod"].includes(item)) {
        return `(${item} 1 2)`;
      }

      // Control forms
      if (item === "def") return "(def x 1)";
      if (item === "later") return "(later 1 (write \"HI\"))";
      if (item === "die") return "(die)";
      if (item === "now") return "(now)";
      if (item === "range") return "(range 5)";

      // Logic
      if ([">", "<", "="] .includes(item)) return `(${item} 2 1)`;
      if (item === "if") return "(if yes (write \"Y\") (write \"N\"))";
      if (item === "not") return "(not yes)";

      // Drawing/system-ish
      if (item === "resolution") return "(resolution 64 64)";
      if (item === "wipe") return "(wipe black)";
      if (item === "ink") return "(ink white)";
      if (item === "line") return "(line 0 0 width height)";
      if (item === "box") return "(box 1 1 10 10)";
      if (item === "write") return "(write \"HELLO\")";
      if (item === "wiggle") return "(wiggle 0.1)";
      if (item === "tap") return "(tap (write \"TAP\"))";
      if (item === "draw") return "(draw (line 0 0 width height))";
      if (item === "net") return "(net)";
      if (item === "source") return "(source)";
      if (item === "width") return "width";
      if (item === "height") return "height";

      // Audio
      if (item === "overtone") return "(overtone 220)";

      // Utility
      if (item === "len") return "(len \"abc\")";

      // Colors/constants
      const value = globalEnv[item];
      if (isRgbArray(value)) {
        return `(ink ${item})`;
      }

      // Fallback: present it as a call so it gets tokenized
      // (works for identifiers like resetSpin, smoothspin, ready?, etc.)
      if (typeof item === "string" && item.length > 0) {
        return `(${item})`;
      }

      // Shouldn't happen, but keep it safe.
      return "; (unknown)";
    };

    const buildLibraryPreviewSource = () => {
      const lines = [];
      lines.push("; kidlisp library — syntax highlight preview");

      Object.entries(categories).forEach(([category, categoryItems]) => {
        const availableItems = categoryItems.filter((item) => envKeys.includes(item));
        if (availableItems.length === 0) return;

        lines.push("");
        lines.push(`; ${category}`);
        availableItems.forEach((item) => {
          lines.push(snippetForItem(item, category));
        });
      });

      if (uncategorized.length > 0) {
        lines.push("");
        lines.push(`; other (${uncategorized.length})`);
        uncategorized.forEach((item) => {
          lines.push(snippetForItem(item, "other"));
        });
      }

      return lines.join("\n");
    };

    printColoredOutput("\\bright\\\\cyan\\\\n📚 Kid Lisp Library (syntax preview)\\reset\\");
    printSyntaxHighlightedKidlisp(buildLibraryPreviewSource());
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
    console.log("📄 Test source (syntax highlighted):");
    printSyntaxHighlightedKidlisp(pieces["timing-highlight"].src);
    
    const lisp = new KidLisp();
    
    // Initialize syntax highlighting
    lisp.initializeSyntaxHighlighting(pieces["timing-highlight"].src);
    
    // Mock API for evaluation
    const mockApi = {
      write: (...args) => console.log('Write:', args.join(' ')),
      clock: {
        time: () => new Date(),
      },
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
    
    // Test with colored terminal output
    console.log("\n🌈 COLORED TERMINAL OUTPUT:");
    printColoredOutput(finalHighlight);
    
    expect(finalHighlight).toBeDefined();
    expect(finalHighlight.length).toBeGreaterThan(0);
  });

  it("Test complex timing expression highlighting", () => {
    console.log("🎨 Running complex timing expression highlighting test...");
    console.log("📄 Test source (syntax highlighted):");
    printSyntaxHighlightedKidlisp(pieces["complex-timing"].src);
    
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

  it("Tiny timing tokens use fast pulsed blink windows", () => {
    const lisp = new KidLisp();

    // Sub-second timing should pulse quickly instead of staying continuously "on".
    const tinySecondStart = lisp.getTimingEditBlinkState("0.01s", 0);
    const tinySecondLater = lisp.getTimingEditBlinkState("0.01s", 40);
    expect(tinySecondStart.isBlinking).toBeTrue();
    expect(tinySecondLater.isBlinking).toBeFalse();

    // Sub-frame timing gets the same fast pulse treatment.
    const tinyFrameStart = lisp.getTimingEditBlinkState("1f", 0);
    const tinyFrameLater = lisp.getTimingEditBlinkState("1f", 40);
    expect(tinyFrameStart.isBlinking).toBeTrue();
    expect(tinyFrameLater.isBlinking).toBeFalse();

    // Longer timers keep a broader blink window.
    const normalStart = lisp.getTimingEditBlinkState("1.5s", 0);
    const normalLater = lisp.getTimingEditBlinkState("1.5s", 300);
    expect(normalStart.isBlinking).toBeTrue();
    expect(normalLater.isBlinking).toBeFalse();
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
  const filePath = path.resolve(
    __dirname,
    "..",
    "system",
    "public",
    "aesthetic.computer",
    "disks",
    `${name}.lisp`,
  );
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
