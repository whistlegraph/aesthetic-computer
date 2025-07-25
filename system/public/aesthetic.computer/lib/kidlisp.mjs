// Kidlisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

// ❤️‍🔥 TODO: Add UTC Support to s... timers. etc. to be compatible with 'clock.mjs'.
//        - Selectabe values via game controller.


import { parseMelody, noteToTone } from "./melody-parser.mjs";
import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

// Global cache registry for storing cached codes by source hash
const cacheRegistry = new Map();

// Helper function to generate a hash for source code
function getSourceHash(source) {
  // Simple hash function for source code
  let hash = 0;
  for (let i = 0; i < source.length; i++) {
    const char = source.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return Math.abs(hash).toString(36);
}

// Function to get cached code for a source
function getCachedCode(source) {
  const hash = getSourceHash(source);
  return cacheRegistry.get(hash);
}

// Function to store cached code for a source
function setCachedCode(source, code) {
  const hash = getSourceHash(source);
  cacheRegistry.set(hash, code);
}

/* #region 📚 Examples / Notebook 
 Working programs:

  (later cross x y (line (- 10 x) (- 10 y) (+ 10 x) (+ 10 y)))
  (cross (/ width 2) (/ height 2))

  // TODO: Could I get this to evaluate?
  (cross width/2*(10+x) height/2)
  //     ^ Where this just leans on a Javascript expression evaluator
  //       that can access the local context.


 Conceptual program:

 (later cross x y (line x-10 y-10 x+10 y+10))
 (cross width/2 height/2)

 Even more conceptual program:

 to cross x y -> line x-10 y-10 x+10 y+10
 cross width/2 height/2


  (ink "red")
  (cross 16 16)
  (ink "yellow")
  (cross 32 32)
  (ink "blue")
  (cross (+ 1 2 3) (+ 4 5 6))

  (+ 4 5 5)
  (* (+ 2 3) (- 4 1))

  (def x 10)
  (def y (+ x 10))
  (+ x y)

  ; Paste me into the AC prompt!
  ; (resolution 128)
  (wipe "blue")
  (ink "lime")
  (line 90 (+ 10 20 30) 30 80)
  (ink 255 0 0)
  (line 30 20 100 190)
  (ink "orange" (wiggle 64))
  (box 8 32 (- width 20) (wiggle 32))
  (+ 1 2 3)

  ((line 20 80 80 80))
  (later cross x y
    (line x-10 y-10 x+10 y+10)
    (line x-10 y+10 x+10 y-10)
  )
  (ink yellow)
  (cross 16 32)
#endregion */

/* #region 🏁 TODO 
 - [🟢] Write the graphics and interaction module for the step debugger.
 - [] Resaving the kidlisp file should not reload the whole frontend stack.
 - [] Evaluate `hoot` lang / give it a try? https://spritely.institute/hoot
  *** kidlisp debugger ***
    - [] light up each line as it executes
    - [] be able to see the values visually connect
    - [] the debugger would eventually become the editor...
    - [] and it needs to be built into the running page
    - [] this starts by making a drawing / graph of the ast
    - [] and by lighting up each list / atom as it gets evaluated / lighting
 - [] `def` and `later` should be the same keyword.
 - [] Should code reloads reset the state or could I dump
      the state on each reload?
  - [] Add a "loading..." display until the network request completes.
  - [] Add a button to each handle, similar to the `list` command.
  - [] How should assignment actually work?
  - [] And shorthand for incrementing a value.
  - [] Implement: ; (once (wipe black))
  - [] Is there a way to get live updates
      to be even faster while using the
      lisp?
    - [] Like being able to soft-reset the environment
        parse the code but nothing else.
  - [] Runtime Modes
    - [] Everything should be "alive" in a continuous loop
        and for optimization or one-time effects, elements
        can be striken from the AST like global definitions.
  - [] Could all `defs` be removed from the parsed AST once they run once?
  - [] Add a repeat like goto statement?
  - [] Or something to enable looping.
  - [] Add the ability to make sound.
  + Done
  - [x] Closing and opening the VS Code extension should remember the last piece
      loaded.
   - [x] This should work by broadcasting the piece slug to the extension
        on each switch then using that for the page open url, but not storing
        it across vscode opened and closed
        sessions.
  - [x] Refactored for multiple environment support - 2025.6.17
  - [x] Have tests run automatically in some window.
  - [x] Set up this module with some actual JavaScript testing framework
       with expectations so I can make sure it passes.
  - [x] Add named function definitions.
  + [x] Add variable setting and retrieval.
  - [x] Get publishing working for lisp code / pasted code.
  - [x] Add global variables / more complex programming.
  - [x] Add support for comments.
  - [x] Get a named command running through, like "line" and "ink". 
#endregion */

const VERBOSE = false;
const PERF_LOG = false; // Enable performance logging
const { floor, max } = Math;
import { cssColors } from "./num.mjs";

// Performance tracking utilities
const perfTimers = {};
const perfLogs = []; // Store logs for display in piece
const perfOrder = [
  "parse",
  "precompile",
  "frame-evaluation",
  "repeat-setup",
  "repeat-with-iterator",
  "fast-draw-loop",
]; // Fixed order for consistent display

function perfStart(label) {
  if (PERF_LOG) perfTimers[label] = performance.now();
}
function perfEnd(label) {
  if (PERF_LOG && perfTimers[label]) {
    const duration = performance.now() - perfTimers[label];
    if (duration > 0.1) {
      // Only log operations > 0.1ms
      const logEntry = `${label}: ${duration.toFixed(2)}ms`;

      // Remove existing entry for this label to maintain order
      const existingIndex = perfLogs.findIndex((log) =>
        log.startsWith(label + ":"),
      );
      if (existingIndex !== -1) {
        perfLogs.splice(existingIndex, 1);
      }

      // Insert in correct position based on perfOrder
      const orderIndex = perfOrder.indexOf(label);
      if (orderIndex !== -1) {
        // Find where to insert based on order
        let insertIndex = 0;
        for (let i = 0; i < perfLogs.length; i++) {
          const logLabel = perfLogs[i].split(":")[0];
          const logOrderIndex = perfOrder.indexOf(logLabel);
          if (logOrderIndex !== -1 && logOrderIndex < orderIndex) {
            insertIndex = i + 1;
          }
        }
        perfLogs.splice(insertIndex, 0, logEntry);
      } else {
        // If not in predefined order, add at end
        perfLogs.push(logEntry);
      }

      // Keep only last 8 logs to avoid memory issues
      if (perfLogs.length > 8) perfLogs.splice(8);
    }
    delete perfTimers[label];
  }
}

// 🎰 Parser & Evaluation utility functions (stateless)
const identifierRegex = /[a-zA-Z_]\w*/g;
const validIdentifierRegex = /^[a-zA-Z_]\w*$/;

function tokenize(input) {
  if (VERBOSE) console.log("🪙 Tokenizing:", input);

  const regex = /\s*(;.*|[()]|"(?:[^"\\]|\\.)*"|[^\s()";]+)/g;
  const tokens = [];
  let match;
  while ((match = regex.exec(input)) !== null) {
    const token = match[1];
    if (!token.startsWith(";")) {
      // Filter out comments
      tokens.push(token);
    }
  }

  // Auto-close incomplete expressions by counting parentheses balance
  let parenBalance = 0;
  for (const token of tokens) {
    if (token === "(") {
      parenBalance++;
    } else if (token === ")") {
      parenBalance--;
    }
  }

  // Add closing parentheses for any unclosed expressions
  while (parenBalance > 0) {
    tokens.push(")");
    parenBalance--;
  }

  if (VERBOSE) console.log("🪙 Tokens:", tokens);
  return tokens;
}

function readFromTokens(tokens) {
  const result = [];
  while (tokens.length > 0) {
    if (tokens[0] === ")") {
      throw new Error("Unexpected ')'");
    }
    result.push(readExpression(tokens));
  }
  return result;
}

function readExpression(tokens) {
  if (tokens.length === 0) {
    throw new Error("Unexpected end of input");
  }
  let token = tokens.shift();
  if (token === "(") {
    const list = [];
    while (tokens.length > 0 && tokens[0] !== ")") {
      list.push(readExpression(tokens));
    }
    if (tokens.length === 0) {
      // Auto-closing should have handled this, but just in case
      console.warn("🔧 Auto-closing: Missing ')' was handled by tokenizer");
      return list;
    }
    tokens.shift(); // Remove the ')'
    return list;
  } else {
    return atom(token);
  }
}

function atom(token) {
  if (token[0] === '"' && token[token.length - 1] === '"') {
    return token; // Return string with quotes intact for later processing
  } else if (/^\d*\.?\d+s\.\.\.?$/.test(token)) {
    // Preserve tokens like "1s...", "2s...", "1.5s..." as strings for timed iteration
    return token;
  } else if (/^\d*\.?\d+s$/.test(token)) {
    // Preserve tokens like "1s", "2s", "10s", "1.5s", "0.3s" as strings for timing
    return token;
  } else {
    const num = parseFloat(token);
    const result = isNaN(num) ? token : num;
    return result;
  }
}

function processArgStringTypes(args) {
  if (!Array.isArray(args)) {
    // Don't convert undefined to string - preserve it as undefined
    if (args === undefined) {
      return undefined;
    }
    return args?.toString();
  }
  return args.map((arg) => {
    if (typeof arg === "string" && arg.startsWith('"') && arg.endsWith('"')) {
      return arg.slice(1, -1); // Remove quotes
    }
    return arg;
  });
}

function unquoteString(str) {
  if (str.startsWith('"') && str.endsWith('"')) {
    return str.slice(1, -1);
  } else {
    return str;
  }
}

function existing(item) {
  return item !== undefined && item !== null;
}

function getNestedValue(obj, path) {
  return path?.split(".").reduce((acc, part) => acc && acc[part], obj);
}

// KidLisp Environment Class
class KidLisp {
  constructor() {
    this.ast = null; // Abstract syntax tree.
    this.networkCache = { sources: {} };
    this.globalDef = {};
    this.localEnvStore = [{}];
    this.localEnv = this.localEnvStore[0];
    this.localEnvLevel = 0;
    this.tapper = null;
    this.drawer = null;
    this.frameCount = 0; // Frame counter for timing functions
    this.lastSecondExecutions = {}; // Track last execution times for second-based timing
    this.instantTriggersExecuted = {}; // Track which instant triggers have already fired

    // Cache state (URLs stored per instance)
    this.shortUrl = null; // Store cached short URL
    this.cachedCode = null; // Store cached code
    this.cachingInProgress = false; // Flag to prevent multiple cache requests

    // Microphone state tracking
    this.microphoneConnected = false;
    this.microphoneApi = null;

    // Performance optimizations
    this.functionCache = new Map(); // Cache function lookups
    this.globalEnvCache = null; // Cache global environment
    this.fastPathFunctions = new Set([
      "line",
      "ink",
      "wipe",
      "backdrop",
      "box",
      "repeat",
      "+",
      "-",
      "*",
      "/",
      "=",
      ">",
      "<",
      "mic",
      "paste",
      "stamp",
    ]); // Common functions for fast path
    // Reduce map allocations - reuse simple objects
    this.reusableTimingKey = ""; // Reuse string for timing keys  
    this.mathCache = new Map(); // Cache for math expressions
    this.sequenceCounters = new Map(); // Track sequence positions for ... function

    // Syntax highlighting state
    this.syntaxHighlightSource = null; // Source code for syntax highlighting
    this.expressionPositions = []; // Positions of expressions in source
    this.currentlyHighlighted = new Set(); // Currently highlighted expressions
    this.executionHistory = []; // History of executed expressions for flash effects
    this.currentExecutingExpression = null; // Currently executing expression
    this.lastExecutionTime = 0; // Last execution timestamp
    this.flashDuration = 500; // Duration for flash effects in milliseconds

    // Handle attribution cache
    this.cachedOwnerHandle = null; // Cache the resolved handle
    this.cachedOwnerSub = null; // Track which sub we cached the handle for

    // Melody state tracking
    this.melodies = new Map(); // Track active melodies
    this.melodyTimers = new Map(); // Track timing for each melody

    // Resolution state tracking
    this.halfResolutionApplied = false;
    this.thirdResolutionApplied = false;
    this.fourthResolutionApplied = false;

    // Once special form state tracking
    this.onceExecuted = new Set(); // Track which once blocks have been executed
    
    // Timing blink state tracking
    this.timingBlinks = new Map(); // Track timing expressions that should blink
    this.blinkDuration = 200; // Duration for timing blinks in milliseconds
    
    // Active timing expressions tracking for syntax highlighting
    this.activeTimingExpressions = new Map(); // Track which timing expressions are currently active
    
    // Syntax highlighting signals from execution
    this.syntaxSignals = new Map(); // Direct signals from evaluation: expressionId -> color
    this.expressionRegistry = new Map(); // Map AST expressions to unique IDs
    this.nextExpressionId = 0; // Counter for expression IDs
    
    // Ink state management
    this.inkState = undefined; // Track KidLisp ink color (starts undefined)
    this.inkStateSet = false; // Track if ink has been explicitly set
  }

  // Reset all state for a fresh KidLisp instance
  reset() {
    // Reset core state
    this.ast = null;
    this.globalDef = {};
    this.localEnvStore = [{}];
    this.localEnv = this.localEnvStore[0];
    this.localEnvLevel = 0;
    this.tapper = null;
    this.drawer = null;
    this.frameCount = 0;
    
    // Keep first-line color state during reset (preserve for resize/reframe)
    // this.firstLineColor = null; // Don't reset - keep for background persistence
    
    // Reset timing state
    this.lastSecondExecutions = {};
    this.instantTriggersExecuted = {};
    
    // Reset cache state
    this.cachedCode = null;
    this.cachingInProgress = false;
    this.shortUrl = null;
    
    // Reset microphone state
    this.microphoneConnected = false;
    this.microphoneApi = null;
    
    // Reset performance caches
    this.functionCache.clear();
    this.globalEnvCache = null;
    this.mathCache.clear();
    this.sequenceCounters.clear();
    
    // Reset syntax highlighting state
    this.syntaxHighlightSource = null;
    this.expressionPositions = [];
    this.currentlyHighlighted.clear();
    this.executionHistory = [];
    this.currentExecutingExpression = null;
    this.lastExecutionTime = 0;
    
    // Reset handle attribution cache
    this.cachedOwnerHandle = null;
    this.cachedOwnerSub = null;
    
    // Reset melody state
    this.melodies.clear();
    this.melodyTimers.clear();
    
    // Reset resolution state
    this.halfResolutionApplied = false;
    this.thirdResolutionApplied = false;
    this.fourthResolutionApplied = false;
    
    // NOTE: onceExecuted is NOT cleared to preserve 'once' state across module loads
    // This allows once blocks to work consistently between inline and .lisp file execution
    // this.onceExecuted.clear(); // <-- REMOVED
    
    // Reset timing blink tracking
    this.timingBlinks.clear();
    
    // Reset active timing expressions tracking
    this.activeTimingExpressions.clear();
    
    // Don't reset ink state during reset - preserve across frame transitions
    // this.inkState = undefined;
    // this.inkStateSet = false;
  }

  // Register an expression and get its unique ID
  registerExpression(expr) {
    // Create a stable key for the expression
    const key = JSON.stringify(expr);
    if (!this.expressionRegistry.has(key)) {
      this.expressionRegistry.set(key, this.nextExpressionId++);
    }
    return this.expressionRegistry.get(key);
  }

  // Signal syntax highlighting for an expression
  signalSyntaxHighlight(expr, color) {
    const id = this.registerExpression(expr);
    this.syntaxSignals.set(id, color);
    
    // Debug logging
    if (this.frameCount % 60 === 0) {
      // console.log(`📡 SYNTAX SIGNAL: expr ${id} -> ${color}`, expr);
    }
  }

  // Clear all syntax signals (called each frame)
  clearSyntaxSignals() {
    this.syntaxSignals.clear();
  }

  // Mark a timing expression as triggered for blinking
  markTimingTriggered(timingToken) {
    const now = performance.now();
    this.timingBlinks.set(timingToken, {
      triggerTime: now,
      duration: this.blinkDuration
    });
  }

  // Check if a timing token should be blinking
  isTimingBlinking(timingToken) {
    if (!this.timingBlinks.has(timingToken)) {
      return false;
    }
    
    const blinkInfo = this.timingBlinks.get(timingToken);
    const now = performance.now();
    const elapsed = now - blinkInfo.triggerTime;
    
    if (elapsed > blinkInfo.duration) {
      // Blink has expired, remove it
      this.timingBlinks.delete(timingToken);
      return false;
    }
    
    // Return true if we're in the first half of the blink duration (creates flash effect)
    return elapsed < (blinkInfo.duration / 2);
  }

  // Format an expression for HUD display (convert to readable text)
  formatExpressionForHUD(expr) {
    if (typeof expr === "string") {
      return expr;
    }
    if (typeof expr === "number") {
      return expr.toString();
    }
    if (Array.isArray(expr)) {
      // Convert array expressions to readable format like "(ink white)" or "(line)"
      if (expr.length === 0) return "()";
      if (expr.length === 1) return `(${expr[0]})`;
      
      // Format common patterns nicely
      const head = expr[0];
      const args = expr.slice(1);
      
      if (head === "ink" && args.length === 1) {
        return `(ink ${args[0]})`;
      }
      if (head === "line" && args.length === 0) {
        return "(line)";
      }
      if (head === "line" && args.length > 0) {
        return `(line ${args.join(" ")})`;
      }
      
      // Generic formatting: show first few items to keep it concise
      const displayArgs = args.slice(0, 3);
      const argsText = displayArgs.join(" ");
      const ellipsis = args.length > 3 ? "..." : "";
      return `(${head} ${argsText}${ellipsis})`;
    }
    
    // Fallback for other types
    return String(expr);
  }

  // Detect first-line color from AST without executing code
  detectFirstLineColor() {
    if (!this.ast) return;
    
    // Get the first item from the AST
    const firstItem = Array.isArray(this.ast) && this.ast.length > 0 ? this.ast[0] : this.ast;
    let colorName = null;
    
    // Check if it's a bare string color name
    if (typeof firstItem === "string") {
      colorName = firstItem;
    }
    // Check if it's a single-argument function call like ["red"]
    else if (Array.isArray(firstItem) && firstItem.length === 1 && typeof firstItem[0] === "string") {
      colorName = firstItem[0];
    }
    
    if (colorName) {
      const globalEnv = this.getGlobalEnv();
      
      // Check if it's a color name in the global environment
      if (globalEnv[colorName] && typeof globalEnv[colorName] === "function") {
        try {
          // Test if this is a color function by calling it
          globalEnv[colorName]();
          
          // If we get here without error, it's a color function
          this.firstLineColor = colorName;
        } catch (e) {
          // Not a color function, ignore
        }
      }
    }
  }

  // Get the background fill color for reframe operations
  getBackgroundFillColor() {
    return this.firstLineColor;
  }

  // Clear KidLisp ink state (reset to undefined)
  clearInkState() {
    this.inkState = undefined;
    this.inkStateSet = false;
  }

  // Get current KidLisp ink state
  getInkState() {
    return this.inkStateSet ? this.inkState : undefined;
  }

  // Check if the AST contains microphone-related functions
  containsMicrophoneFunctions(ast) {
    if (!ast) return false;

    if (typeof ast === "string") {
      // Only 'mic' function requires microphone access
      // 'amplitude' is for speaker amplitude, not microphone
      return ast === "mic";
    }

    if (Array.isArray(ast)) {
      return ast.some((item) => this.containsMicrophoneFunctions(item));
    }

    return false;
  }

  // Optimize common patterns - this could be expanded for other patterns
  compileOptimizedRepeat(ast) {
    // Actually, let's disable optimization for rainbow ink to preserve correct behavior
    // The rainbow function needs to be called through the interpreter to work properly
    return null;

    /* Disabled optimization - keeping for reference
    // Look for pattern: (repeat height i (ink rainbow) (line 0 i width i))
    // More flexible pattern matching
    if (Array.isArray(ast) && ast.length >= 3 && ast[0] === 'repeat' && 
        ast[1] === 'height' && ast[2] === 'i') {
      
      // Check if there's a line drawing pattern in the body
      const hasLinePattern = ast.slice(3).some(item => 
        Array.isArray(item) && item[0] === 'line'
      );
      
      const hasRainbowInk = ast.slice(3).some(item =>
        Array.isArray(item) && item[0] === 'ink' && item.includes('rainbow')
      );
      
      if (hasLinePattern && hasRainbowInk) {
        // Return optimized JavaScript function that respects the rainbow ink
        return (api) => {
          perfStart('optimized-repeat');
          const height = api.screen?.height || 256;
          
          for (let i = 0; i < height; i++) {
            // Call rainbow for each line to get proper color variation
            api.ink?.(api.help?.rainbow?.() || [255, 0, 0]);
            api.line?.(0, i, api.screen?.width || 256, i);
          }
          perfEnd('optimized-repeat');
          return height; // Return number of lines drawn
        };
      }
    }
    return null;
    */
  }

  // Method to pre-compile AST for optimizations
  precompileAST(ast) {
    if (!Array.isArray(ast)) return ast;

    const optimized = [];
    for (const item of ast) {
      if (Array.isArray(item)) {
        const compiledOptimization = this.compileOptimizedRepeat(item);
        if (compiledOptimization) {
          optimized.push({ optimized: true, func: compiledOptimization });
        } else {
          // Apply macro expansion to convert fastmath expressions
          const expanded = this.expandFastMathMacros(item);
          optimized.push(this.precompileAST(expanded));
        }
      } else {
        // Apply macro expansion to atoms as well
        optimized.push(this.expandFastMathMacros(item));
      }
    }
    return optimized;
  }

  // Macro expansion step: convert fastmath expressions like "i*2" to ["*", "i", 2]
  expandFastMathMacros(expr) {
    if (typeof expr === "string") {
      // Check for parenthesized expressions like "(min width height)/250"
      const parenMatch = expr.match(
        /^(\([^)]+\))\s*([+\-*/%])\s*(\w+|\d+(?:\.\d+)?)$/,
      );
      if (parenMatch) {
        const [, parenExpr, op, right] = parenMatch;

        // Parse the parenthesized expression (strip outer parens and tokenize)
        const innerExpr = parenExpr.slice(1, -1).trim(); // Remove ( and )

        // Simple tokenization - split by spaces and convert to appropriate types
        const tokens = innerExpr.split(/\s+/).map((token) => {
          // Convert numeric tokens to numbers
          if (/^\d+(?:\.\d+)?$/.test(token)) {
            return parseFloat(token);
          }
          return token;
        });

        // Convert right operand to number if it's numeric
        const rightValue = /^\d+(?:\.\d+)?$/.test(right)
          ? parseFloat(right)
          : right;

        // Return the expanded prefix expression
        return [op, tokens, rightValue];
      }

      // Check for chained math expressions like "width/5.67666*0.828"
      const chainedMatch = expr.match(
        /^(\w+)\s*([+\-*/%])\s*(\d+(?:\.\d+)?)\s*([+\-*/%])\s*(\d+(?:\.\d+)?)$/,
      );
      if (chainedMatch) {
        const [, variable, op1, num1, op2, num2] = chainedMatch;

        // Convert to nested prefix expressions: (* (/ width 5.67666) 0.828)
        const innerExpr = [op1, variable, parseFloat(num1)];
        const outerExpr = [op2, innerExpr, parseFloat(num2)];
        return outerExpr;
      }

      // Check for simple infix math expressions
      const infixMatch = expr.match(
        /^(\w+)\s*([+\-*/%])\s*(\w+|\d+(?:\.\d+)?)$/,
      );
      if (infixMatch) {
        const [, left, op, right] = infixMatch;

        // Convert right operand to number if it's numeric
        const rightValue = /^\d+(?:\.\d+)?$/.test(right)
          ? parseFloat(right)
          : right;

        // Return the expanded prefix expression - left should stay as unquoted identifier
        const result = [op, left, rightValue];
        return result;
      }
    } else if (Array.isArray(expr)) {
      // Handle adjacent array elements that might form fast math expressions
      // Look for patterns like [expression, "/10"] or [expression, "*2"]
      const expanded = [];
      for (let i = 0; i < expr.length; i++) {
        const current = expr[i];
        const next = expr[i + 1];

        // Check if current is an array/expression and next is a math operation string
        if (Array.isArray(current) && typeof next === "string") {
          const mathMatch = next.match(/^([+\-*/%])(\d+(?:\.\d+)?)$/);
          if (mathMatch) {
            const [, op, num] = mathMatch;
            // Combine them into a proper math expression
            expanded.push([
              op,
              this.expandFastMathMacros(current),
              parseFloat(num),
            ]);
            i++; // Skip the next element since we consumed it
            continue;
          }
        }

        // Otherwise, recursively expand the current element
        expanded.push(this.expandFastMathMacros(current));
      }
      return expanded;
    }

    // Return unchanged if no expansion needed
    return expr;
  }

  // Update browser URL to show the short $prefixed code
  updateBrowserUrl(shortCode, api) {
    try {
      // Only update if we're in a browser environment and not in an iframe
      if (typeof window !== 'undefined' && window.history && window.location && !api.net?.iframe) {
        const currentPath = window.location.pathname;
        const newPath = `/$${shortCode}`; // Add $ prefix for cached codes
        
        // Only update if the path is different to avoid unnecessary history entries
        if (currentPath !== newPath) {
          // Use replaceState to avoid creating a new history entry
          window.history.replaceState(null, '', newPath);
          console.log(`🔗 Updated browser URL to: ${window.location.origin}${newPath}`);
        }
      }
    } catch (error) {
      console.warn('Failed to update browser URL:', error);
    }
  }

  // Cache kidlisp source code for QR generation
  async cacheKidlispSource(source, api) {
    // Skip caching for .lisp files since they're already stored as files
    if (this.isLispFile) {
      return;
    }
    
    // Skip if already cached or caching is in progress
    if (this.cachedCode || this.cachingInProgress) {
      return;
    }
    
    // Check if caching is enabled from store (default: true)
    const cacheEnabled = api.store?.["kidlisp:cache-enabled"] !== false;
    
    // Skip caching if disabled or no source
    if (!cacheEnabled || !source || source.trim().length === 0) {
      return;
    }
    
    // Set flag to prevent multiple requests
    this.cachingInProgress = true;
    
    try {
      // Use the new request function that handles both auth and anonymous users
      const response = await api.net.request('POST', '/api/store-kidlisp', { source });
      
      if (response && (response.status === 200 || response.status === 201)) {
        this.shortUrl = `aesthetic.computer/$${response.code}`;
        
        // Store the short URL for QR generation
        this.cachedCode = response.code;
        
        // Store in global registry for access from disk.mjs
        setCachedCode(source, response.code);
        
        // Log the generated $code to console with styled formatting
        console.log(
          `%c$${response.code}`,
          'background: yellow; color: black; font-weight: bold; font-family: monospace; padding: 2px 4px; font-size: 12px;'
        );
        
        // Update browser URL to show the short code
        this.updateBrowserUrl(response.code, api);
      } else {
        // Silently handle caching failures - auth issues are common and not critical
        // console.warn('Failed to cache kidlisp:', response?.status, response?.message || 'Unknown error');
      }
    } catch (error) {
      // Silently handle caching failures - auth issues are common and not critical  
      // console.warn('Failed to cache kidlisp:', error.message || error);
    } finally {
      // Always clear the flag when done (success or failure)
      this.cachingInProgress = false;
    }
  }

  // Parse and evaluate a lisp source module
  // into a running aesthetic computer piece.
  module(source, isLispFile = false) {
    // Console log the interpreted KidLisp code with styled formatting and dogs
    console.log(
      `%c🐕 ${source} 🐶`,
      'display: inline-block; background: #000; color: #FFEB3B; font-family: monospace; font-weight: bold; font-size: 12px; line-height: 1.4; white-space: pre-wrap; padding: 2px 4px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); text-shadow: 0 0 2px rgba(255, 235, 59, 0.3);'
    );
    
    // Reset all state for fresh instance when loading a new module
    this.reset();
    
    // Clear first-line color when loading new code
    this.firstLineColor = null;
    
    // Store flag to skip caching for .lisp files
    this.isLispFile = isLispFile;
    
    perfStart("parse");
    const parsed = this.parse(source);
    perfEnd("parse");

    perfStart("ast-copy");
    this.ast = JSON.parse(JSON.stringify(parsed)); // Deep copy of original source. 🙃
    perfEnd("ast-copy");

    // Precompile optimizations
    perfStart("precompile");
    this.ast = this.precompileAST(this.ast);
    perfEnd("precompile");

    // Initialize syntax highlighting
    this.initializeSyntaxHighlighting(source);

    // Clear handle attribution cache for new module
    this.cachedOwnerHandle = null;
    this.cachedOwnerSub = null;

    /*if (VERBOSE)*/ // console.log("🐍 Snake:", parsed);

    // 🧩 Piece API
    return {
      boot: ({ wipe, params, clock, screen, sound, delay, pieceCount, net, backgroundFill }) => {
        // Resync clock for accurate timing (like clock.mjs does)
        clock?.resync?.();

        // Preload MatrixChunky8 font for QR code generation in KidLisp pieces
        net?.preloadTypeface?.("MatrixChunky8");

        // ...existing boot code...

        this.globalDef.paramA = params[0];
        this.globalDef.paramB = params[1];
        this.globalDef.paramC = params[2];

        // Initialize microphone reference but don't auto-connect
        if (sound?.microphone) {
          this.microphoneApi = sound.microphone;

          // Only auto-connect if the piece actually uses microphone functions
          const usesMicrophone = this.containsMicrophoneFunctions(this.ast);

          if (
            usesMicrophone &&
            this.microphoneApi.permission === "granted" &&
            sound.enabled?.()
          ) {
            console.log(
              "🎤 Boot: Auto-connecting microphone (piece uses mic functions)",
            );
            delay(() => {
              this.microphoneApi.connect();
            }, 15);
          } else if (usesMicrophone) {
            console.log(
              "🎤 Boot: Piece uses microphone but permission not granted or sound disabled",
            );
          }
        } else {
          // console.log("🎤 Boot: No microphone API available");
        }

        // Just set up initial state, don't execute program here
        // console.log(pieceCount);
        
        // Detect first-line color from AST if not already set (e.g., during resize)
        if (!this.firstLineColor && this.ast) {
          this.detectFirstLineColor();
        }
        
        // Set background fill color for reframe operations
        if (this.firstLineColor && backgroundFill) {
          backgroundFill(this.firstLineColor);
        }
        
        // Use first-line color as default background if available, otherwise erase
        if (this.firstLineColor) {
          wipe(this.firstLineColor);
        } else {
          wipe("erase");
        }
        
        // Set initial ink color to undefined for KidLisp pieces
        // This ensures a clean slate for each piece instead of inheriting system colors
        if (!this.inkStateSet) {
          // Don't call ink() as that would set the state - just mark as unset
          this.inkState = undefined;
          this.inkStateSet = false;
        }
      },
      paint: ($) => {
        // console.log("🖌️ Kid Lisp is Painting...", $.paintCount);
        this.frameCount++; // Increment frame counter for timing functions

        // Cache kidlisp source for QR code generation instantly
        // This prevents caching work-in-progress code and saves server space
        const cacheDelayFrames = 0; // Instant caching
        
        if (this.frameCount >= cacheDelayFrames && !this.cachedCode) {
          this.cacheKidlispSource(source, $);
        }

        // Clear syntax signals from previous frame
        this.clearSyntaxSignals();

        // Update HUD with syntax highlighting
        this.updateHUDWithSyntaxHighlighting($);

        // Restore KidLisp ink state at the beginning of each paint frame
        // This ensures ink color persists despite modifications by HUD, QR codes, etc.
        if (this.inkStateSet && this.inkState !== undefined) {
          $.ink?.(this.inkState);
        } else {
          // If no ink has been explicitly set in KidLisp, set ink to undefined
          // This prevents inheriting colors from HUD, QR codes, etc. and allows
          // the underlying system to handle the undefined ink state properly
          $.ink?.(undefined);
        }

        perfStart("frame-evaluation");
        try {
          // Then execute the full program
          this.localEnvLevel = 0; // Reset state per program evaluation.
          this.localEnv = this.localEnvStore[this.localEnvLevel];
          /*const evaluated = */ this.evaluate(this.ast, $);
        } catch (err) {
          console.error("⛔ Evaluation failure:", err);
        }
        perfEnd("frame-evaluation");

        // Display performance logs in the piece
        if (PERF_LOG && perfLogs.length > 0) {
          $.ink?.("yellow");
          perfLogs.forEach((log, index) => {
            $.write?.(log, { x: 2, y: 24 + index * 12 }, "black");
          });
        }

        // TODO: Re-enable the below:
        // Print the program output value in the center of the screen if
        // necessary.
        // ink("white").write(evaluated || "nada", { center: "xy" });

        // QR code is now handled as an overlay in disk.mjs

        // TODO: Add haltability to paint with a shortcut that triggers the below.
        // return false;
      },
      sim: ({ sound }) => {
        // Poll speaker for audio amplitude data (like clock.mjs does)
        sound.speaker?.poll();

        // Make sure we have microphone reference (could come from sound parameter)
        if (!this.microphoneApi && sound?.microphone) {
          this.microphoneApi = sound.microphone;
        }

        // Always poll microphone if available (like stample does)
        if (this.microphoneApi) {
          this.microphoneApi.poll?.();

          // Update connection status
          if (this.microphoneApi.connected && !this.microphoneConnected) {
            this.microphoneConnected = true;
            console.log("🎤 Microphone connected in kidlisp");
          }
        }

        // Debug: Log mic data occasionally
        if (this.frameCount % 60 === 0 && this.microphoneApi?.connected) {
          console.log("🎤 Mic amplitude:", this.microphoneApi.amplitude);
        }

        // Update melody playback
        this.updateMelodies({ sound });
      },
      act: ({ event: e, api }) => {
        if (e.is("touch")) {
          api.needsPaint();
          this.tap(api);
        }
        if (e.is("draw")) {
          api.needsPaint();
          this.draw(api, { dy: e.delta.y, dx: e.delta.x });
        }

        // Handle microphone connection events
        if (e.is("microphone-connect:success")) {
          console.log("🎤 Microphone connected successfully!");
          this.microphoneConnected = true;
        }
        if (e.is("microphone-connect:failure")) {
          console.warn("🎤 Failed to connect microphone:", e.reason);
        }
      },
    };
  }

  // Main parsing method - handles both single expressions and multi-line input
  parse(input) {
    // Handle multi-line kidlisp input by auto-wrapping function calls
    if (input.includes("\n")) {
      const lines = input
        .split("\n")
        .map((line) => {
          // Strip inline comments (everything after semicolon)
          const commentIndex = line.indexOf(";");
          if (commentIndex !== -1) {
            line = line.substring(0, commentIndex);
          }
          return line.trim();
        })
        .filter((line) => line.length > 0);

      const wrappedLines = lines.map((line, index) => {
        // Check if this line is likely a continuation of a multi-line expression
        const isContinuation =
          index > 0 &&
          // Line starts with an identifier but not at the beginning of the input
          /^[a-zA-Z_]\w*/.test(line) &&
          !line.startsWith("(") &&
          // Previous line ends with an incomplete expression (has opening paren or is incomplete)
          lines[index - 1].includes("(") &&
          lines[index - 1].split("(").length >
            lines[index - 1].split(")").length;

        // If line doesn't start with ( and looks like a function call, wrap it
        // BUT don't wrap if it's likely a continuation line
        if (
          !line.startsWith("(") &&
          /^[a-zA-Z_]\w*/.test(line) &&
          !isContinuation
        ) {
          return `(${line})`;
        }
        return line;
      });

      // Join lines and parse as single input
      input = wrappedLines.join(" ");
    }

    const tokens = tokenize(input);
    const parsed = readFromTokens(tokens);
    return parsed;
  }

  // 🫵 Tap
  tap(api) {
    if (this.tapper) {
      this.evaluate(this.tapper, api);
    }
  }

  // ✏️ Draw
  draw(api, env) {
    if (this.drawer) {
      this.evaluate(this.drawer, api, env);
    }
  }

  // 🎵 Melody playback methods
  parseMelodyString(melodyString) {
    // Delegate to the shared melody parser
    return parseMelody(melodyString);
  }

  playMelodyNote(api, melodyId) {
    const melodyState = this.melodies.get(melodyId);
    if (!melodyState) return;

    const noteData = melodyState.notes[melodyState.index];
    if (!noteData) return;

    const { note, octave, duration } = noteData;
    let noteDuration = duration * melodyState.baseTempo;

    // Apply swing/feel adjustments
    if (melodyState.feel === "waltz" && melodyState.timeSignature === "3/4") {
      // In waltz feel, slightly rush beats 2 and 3, emphasize beat 1
      const beatInMeasure =
        melodyState.measurePosition % melodyState.beatsPerMeasure;
      if (beatInMeasure === 0) {
        // Beat 1: slightly longer (emphasized)
        noteDuration *= 1.05;
      } else {
        // Beats 2 and 3: slightly shorter (lighter)
        noteDuration *= 0.98;
      }
    } else if (melodyState.feel === "swing") {
      // Classic jazz swing: off-beats are delayed and shortened
      const beatInMeasure =
        melodyState.measurePosition % melodyState.beatsPerMeasure;
      if (beatInMeasure % 1 !== 0) {
        // Off-beat
        noteDuration *= 0.9; // Shorter off-beats
      }
    }

    if (note !== "rest" && api.sound?.synth) {
      // Convert note to tone format expected by the synth
      const tone = this.noteToTone(note, octave);

      api.sound.synth({
        type: "sine",
        tone: tone,
        duration: (noteDuration / 1000) * 0.98, // 98% of the duration for smoother flow
        attack: 0.01,
        decay: 0.99,
        volume: 0.8, // Increased volume to make it more audible
      });
    }

    // Update measure position for swing calculations
    melodyState.measurePosition += duration;

    // Schedule next note based on current note's duration
    melodyState.nextNoteTime = performance.now() + noteDuration;
    melodyState.lastPlayTime = performance.now();

    // Advance to next note
    melodyState.index = (melodyState.index + 1) % melodyState.notes.length;
  }

  // Update melody playback in simulation loop
  updateMelodies(api) {
    const currentTime = performance.now();

    for (const [melodyId, melodyState] of this.melodies) {
      if (currentTime >= melodyState.nextNoteTime) {
        this.playMelodyNote(api, melodyId);
      }
    }
  }

  // Convert note letter to frequency
  noteToTone(note, octave = null) {
    // Delegate to the shared melody parser
    return noteToTone(note, octave);
  }

  // CORE OPTIMIZATION HELPER: Check if expression contains a variable
  containsVariable(expr, varName) {
    if (typeof expr === "string") {
      return expr === varName;
    }
    if (typeof expr === "number") {
      return false;
    }
    if (Array.isArray(expr)) {
      return expr.some((subExpr) => this.containsVariable(subExpr, varName));
    }
    return false;
  }

  // Create global environment (cached for performance)
  getGlobalEnv() {
    if (this.globalEnvCache) {
      return this.globalEnvCache;
    }

    this.globalEnvCache = {
      // once: (api, args) => {
      //   console.log("Oncing...", args);
      //   if (this.drawer) this.evaluate(this.drawer, api);
      // },
      now: (api, args) => {
        if (args.length === 2) {
          const name = unquoteString(args[0]);
          if (this.globalDef.hasOwnProperty(name)) {
            this.globalDef[name] = args[1];
          } else {
            console.warn("🚫🧠 Not defined:", name);
          }
          return args[1];
        }
        console.error("❗ Invalid `now`. Wrong number of arguments.");
      },
      // Program Architecture
      def: (api, args, env) => {
        if (args.length === 2) {
          const name = unquoteString(args[0]);
          // Validate the identifier.
          if (!validIdentifierRegex.test(name)) {
            console.error("️❗ Invalid identifier name:", name);
            return;
          }

          // If we're in a local environment (like inside a repeat loop),
          // define the variable in the current local environment
          if (this.localEnvLevel > 0) {
            this.localEnv[name] = args[1];
          } else {
            // Otherwise, define globally as before
            if (!Object.prototype.hasOwnProperty.call(this.globalDef, name)) {
              this.globalDef[name] = args[1];
            } else {
              // Variable already defined, skip redefinition
            }
          }
          return args[1];
        }
        console.error("❗ Invalid `def`. Wrong number of arguments.");
      },
      die: (api, args) => {
        const name = unquoteString(args[0]);
        const def = this.globalDef[name];
        if (def) {
          delete this.globalDef[name];
          def.kill?.(); // Kill a sound or the object has a destructor.
        }
      },
      later: (api, args) => {
        if (args.length >= 2) {
          const name = args[0]; // The first argument as the name.
          let params = [];
          let body = null;

          // Iterate over the arguments starting from the second item.
          args.slice(1).forEach((arg, index) => {
            if (Array.isArray(arg) && body === null) {
              // body = [arg];
              body = args.slice(index + 1);
            } else if (body === null) {
              params.push(arg);
            }
          });

          if (body === null) {
            console.error("No body found in arguments for 'later' function.");
            return;
          }

          this.globalDef[name] = { body, params }; // Assign the array to the global definitions under the name.

          if (VERBOSE) {
            console.log(
              `Latered '${name}' as:`,
              this.globalDef[name],
              "with parameters:",
              params,
            );
          }

          return body;
        }
      },
      net: {
        handles: (api) => {
          const iter = { iterable: true, data: [] };
          if (!this.networkCache.handles) {
            this.networkCache.handles = "loading";
            fetch("/api/handles")
              .then((response) => response.json())
              .then((data) => {
                // console.log("👱 Handles:", data);
                this.networkCache.handles = data.handles;
                iter.data = data.handles;
                api.needsPaint(); // 🖌️ Always require a paint after any network fetch.
              })
              .catch((error) => console.warn(error));
          } else if (Array.isArray(this.networkCache.handles)) {
            iter.data = this.networkCache.handles;
          }
          return iter;
        },
      },
      tap: (api, args) => {
        this.tapper = args;
      },
      draw: (api, args) => {
        this.drawer = args;
      },
      if: (api, args, env) => {
        if (!args || args.length < 1) {
          console.error("❗ Invalid `if`. Wrong number of arguments.");
          return false;
        }
        const evaled = this.evaluate(args[0], api, env);
        if (evaled) this.evaluate(args.slice(1), api, env);
      },
      once: (api, args, env) => {
        if (!args || args.length < 1) {
          console.error("❗ Invalid `once`. Wrong number of arguments.");
          return;
        }

        // Create a unique key for this once block based on its entire content
        const onceKey = JSON.stringify(args);

        // Only execute if this exact once block hasn't been executed before
        if (!this.onceExecuted.has(onceKey)) {
          this.onceExecuted.add(onceKey);
          // Evaluate all the arguments (expressions) in the once block
          let result;
          for (const arg of args) {
            result = this.evaluate(arg, api, env);
          }
          return result;
        }

        // Return undefined if already executed
        return undefined;
      },
      not: (api, args, env) => {
        if (!args || args.length < 1) {
          console.error("❗ Invalid `not`. Wrong number of arguments.");
          return false;
        }
        const evaled = this.evaluate(args[0], api, env);
        if (!evaled) this.evaluate(args.slice(1), api, env);
      },
      range: (api, args) => {
        // console.log("Range detected:", args);
        if (args.length === 3) {
          const array = args[0].data; // Assume `{iterable: true, data: Array}`.
          const startIndex = max(0, floor(args[1]));
          const endIndex = max(0, floor(args[2]));
          // console.log("Range:", array.length, startIndex, endIndex);

          if (
            Array.isArray(array) &&
            typeof startIndex === "number" &&
            typeof endIndex === "number"
          ) {
            return { iterable: true, data: array.slice(startIndex, endIndex) };
          } else {
            console.error(
              "❗ Invalid arguments for `range`. Expected an array and two numbers.",
            );
          }
        } else {
          console.error("❗ Invalid `range`. Wrong number of arguments.");
        }
      },
      // 🧠 Logical Operators
      ">": (api, args, env) => {
        if (!args || args.length < 2) {
          console.error("❗ Invalid `>`. Wrong number of arguments.");
          return false;
        }
        const left = this.evaluate(args[0], api, env),
          right = this.evaluate(args[1], api, env);
        if (left > right) {
          // console.log("✅", left, "is > than", right, args.slice(2));
          return this.evaluate(args.slice(2), api, env);
        } else {
          return false;
        }
      },
      "<": (api, args, env) => {
        if (!args || args.length < 2) {
          console.error("❗ Invalid `<`. Wrong number of arguments.");
          return false;
        }
        const left = this.evaluate(args[0], api, env),
          right = this.evaluate(args[1], api, env);
        if (left < right) {
          // console.log("✅", left, "is < than", right, args.slice(2));
          return this.evaluate(args.slice(2), api, env);
        } else {
          return false;
        }
      },
      "=": (api, args, env) => {
        if (!args || args.length < 2) {
          console.error("❗ Invalid `=`. Wrong number of arguments.");
          return false;
        }
        const left = this.evaluate(args[0], api, env),
          right = this.evaluate(args[1], api, env);
        if (left === right) {
          // If there are additional arguments, evaluate them, otherwise return true
          return args.length > 2
            ? this.evaluate(args.slice(2), api, env)
            : true;
        } else {
          return false;
        }
      },
      // ➗ Mathematical Operators
      max: (api, args, env) => {
        // Simply evaluate each argument in the current environment
        const nums = args
          .map((arg) => this.evaluate(arg, api, this.localEnv))
          .filter((value) => typeof value === "number" && !isNaN(value));
        return nums.length > 0 ? Math.max(...nums) : 0;
      },
      min: (api, args, env) => {
        // Simply evaluate each argument in the current environment
        const nums = args
          .map((arg) => this.evaluate(arg, api, this.localEnv))
          .filter((value) => typeof value === "number" && !isNaN(value));
        return nums.length > 0 ? Math.min(...nums) : 0;
      },
      sin: (api, args = []) => {
        // If no argument provided, use frame count * 0.01 as time-based oscillator
        if (args.length === 0) {
          return Math.sin(this.frameCount * 0.01);
        }
        // Otherwise, evaluate the argument and return its sine
        const value = this.evaluate(args[0], api, this.localEnv);
        return typeof value === "number" ? Math.sin(value) : 0;
      },
      cos: (api, args = []) => {
        // If no argument provided, use frame count * 0.01 as time-based oscillator
        if (args.length === 0) {
          return Math.cos(this.frameCount * 0.01);
        }
        // Otherwise, evaluate the argument and return its cosine
        const value = this.evaluate(args[0], api, this.localEnv);
        return typeof value === "number" ? Math.cos(value) : 0;
      },
      "+": (api, args, env) => {
        // Simply evaluate each argument in the current environment and add
        const result = args.reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          return acc + (typeof value === "number" ? value : 0);
        }, 0);
        return result;
      },
      "-": (api, args, env) => {
        // Simply evaluate each argument in the current environment and subtract
        if (args.length === 0) return 0;
        if (args.length === 1) {
          const value = this.evaluate(args[0], api, this.localEnv);
          return -(typeof value === "number" ? value : 0);
        }
        const first = this.evaluate(args[0], api, this.localEnv);
        const result = args.slice(1).reduce(
          (acc, arg) => {
            const value = this.evaluate(arg, api, this.localEnv);
            return acc - (typeof value === "number" ? value : 0);
          },
          typeof first === "number" ? first : 0,
        );
        return result;
      },
      "*": (api, args, env) => {
        // Simply evaluate each argument in the current environment and multiply
        const result = args.reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          return acc * (typeof value === "number" ? value : 0);
        }, 1);
        return result;
      },
      mul: (api, args, env) => {
        // Alias for * (multiplication operator)
        const result = args.reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          return acc * (typeof value === "number" ? value : 0);
        }, 1);
        return result;
      },
      "/": (api, args, env) => {
        // Simply evaluate each argument in the current environment and divide
        if (args.length === 0) return 0;
        const first = this.evaluate(args[0], api, this.localEnv);
        const result = args.slice(1).reduce(
          (acc, arg) => {
            const value = this.evaluate(arg, api, this.localEnv);
            const divisor = typeof value === "number" ? value : 1;
            return divisor !== 0 ? acc / divisor : acc;
          },
          typeof first === "number" ? first : 0,
        );
        return result;
      },
      "%": (api, args, env) => {
        // Simply evaluate each argument in the current environment and apply modulo
        if (args.length < 2) return 0;
        const first = this.evaluate(args[0], api, this.localEnv);
        const second = this.evaluate(args[1], api, this.localEnv);
        const a = typeof first === "number" ? first : 0;
        const b = typeof second === "number" ? second : 1;
        return b !== 0 ? a % b : 0;
      },
      mod: (api, args, env) => {
        // Alias for % (modulo operator)
        if (args.length < 2) return 0;
        const first = this.evaluate(args[0], api, this.localEnv);
        const second = this.evaluate(args[1], api, this.localEnv);
        const a = typeof first === "number" ? first : 0;
        const b = typeof second === "number" ? second : 1;
        return b !== 0 ? a % b : 0;
      },
      // Paint API
      resolution: (api, args) => {
        // Handle special fraction keywords
        if (args.length === 1 && typeof args[0] === "string") {
          const fraction = args[0];
          let divisor;
          let trackingFlag;

          switch (fraction) {
            case "half":
              divisor = 2;
              trackingFlag = "halfResolutionApplied";
              break;
            case "third":
              divisor = 3;
              trackingFlag = "thirdResolutionApplied";
              break;
            case "fourth":
              divisor = 4;
              trackingFlag = "fourthResolutionApplied";
              break;
            default:
              // Not a recognized fraction keyword, treat as regular resolution call
              api.resolution?.(...args);
              return;
          }

          // Initialize tracking if not exists
          if (!this[trackingFlag]) {
            this[trackingFlag] = false;
          }

          // Only apply fractional resolution once to prevent repeated scaling
          if (!this[trackingFlag]) {
            const currentWidth = api.screen?.width || 256;
            const currentHeight = api.screen?.height || 256;

            // Set resolution to the fraction of current dimensions
            const newWidth = Math.floor(currentWidth / divisor);
            const newHeight = Math.floor(currentHeight / divisor);

            api.resolution?.(newWidth, newHeight);
            this[trackingFlag] = true;

            console.log(
              `🔄 ${fraction} resolution applied: ${currentWidth}x${currentHeight} → ${newWidth}x${newHeight}`,
            );
          } else {
            console.log(
              `🔄 ${fraction} resolution already applied, skipping to prevent squashing`,
            );
          }
        } else {
          // Regular resolution call with explicit dimensions
          api.resolution?.(...args);
          // Reset all fraction resolution flags when explicit dimensions are set
          this.halfResolutionApplied = false;
          this.thirdResolutionApplied = false;
          this.fourthResolutionApplied = false;
        }
      },
      wipe: (api, args) => {
        api.wipe?.(processArgStringTypes(args));
      },
      ink: (api, args) => {
        // Handle different ink invocation patterns
        if (args.length === 0) {
          // Called with no arguments - return current ink state
          return this.inkState;
        } else if (args.length === 1 && (args[0] === null || args[0] === undefined)) {
          // Called with null/undefined - clear the ink state  
          this.clearInkState();
          return undefined;
        } else {
          // Called with color arguments - store and apply the new ink state
          this.inkState = processArgStringTypes(args);
          this.inkStateSet = true;
          api.ink?.(processArgStringTypes(args));
        }
      },
      line: (api, args = []) => {
        api.line(...args);
      },
      // Batch line drawing for performance
      lines: (api, args = []) => {
        // Expects arrays of line coordinates: (lines [[x1 y1 x2 y2] [x1 y1 x2 y2] ...])
        if (args.length > 0 && Array.isArray(args[0])) {
          args[0].forEach((lineArgs) => {
            if (Array.isArray(lineArgs) && lineArgs.length >= 4) {
              api.line(...lineArgs);
            }
          });
        }
      },
      wiggle: (api, args = []) => {
        api.wiggle(...args);
      },
      box: (api, args = []) => {
        // console.log(args);
        api.box(...args);
      },
      shape: (api, args = []) => {
        // Handle shape arguments - can be flat array of coordinates or array of pairs
        if (args.length === 0) return;

        // Check if last argument is a string indicating filled/outline
        let filled = true;
        let thickness = 1;
        let points = args;

        const lastArg = args[args.length - 1];
        if (typeof lastArg === "string") {
          const fillMode = unquoteString(lastArg);
          if (
            fillMode === "outline" ||
            fillMode === "unfilled" ||
            fillMode === "false"
          ) {
            filled = false;
            points = args.slice(0, -1); // Remove the fill mode from points
          }
          // Check for thickness specification like "outline:3"
          if (fillMode.startsWith("outline:")) {
            filled = false;
            thickness = parseInt(fillMode.split(":")[1]) || 1;
            points = args.slice(0, -1);
          }
        }

        // Pass the arguments to the API shape function
        api.shape({ points, filled, thickness });
      },
      scroll: (api, args = []) => {
        api.scroll(...args);
      },
      spin: (api, args = []) => {
        api.spin(...args);
      },
      resetSpin: (api, args = []) => {
        api.resetSpin();
      },
      smoothspin: (api, args = []) => {
        api.smoothSpin(...args);
      },
      sort: (api, args = []) => {
        api.sort(...args);
      },
      zoom: (api, args = []) => {
        api.zoom(...args);
      },
      blur: (api, args = []) => {
        api.blur(...args);
      },
      pan: (api, args = []) => {
        api.pan(...args);
      },
      unpan: (api, args = []) => {
        api.unpan();
      },
      mask: (api, args = []) => {
        // Convert individual args to a box object that mask() expects
        if (args.length >= 4) {
          const box = {
            x: args[0],
            y: args[1],
            width: args[2],
            height: args[3],
          };
          api.mask(box);
        }
      },
      unmask: (api, args = []) => {
        api.unmask();
      },
      steal: (api, args = []) => {
        api.steal(...args);
      },
      putback: (api, args = []) => {
        api.putback(...args);
      },
      // 🖼️ Image pasting and stamping
      paste: (api, args = []) => {
        // Process string arguments to remove quotes (e.g., "@handle/timestamp")
        const processedArgs = args.map((arg) => {
          if (
            typeof arg === "string" &&
            arg.startsWith('"') &&
            arg.endsWith('"')
          ) {
            return arg.slice(1, -1);
          }
          // Handle special 'painting' keyword to reference system.painting
          if (typeof arg === "string" && arg === "painting") {
            return api.system?.painting;
          }
          return arg;
        });
        api.paste(...processedArgs);
      },
      stamp: (api, args = []) => {
        // Process string arguments to remove quotes (e.g., "@handle/timestamp")
        const processedArgs = args.map((arg) => {
          if (
            typeof arg === "string" &&
            arg.startsWith('"') &&
            arg.endsWith('"')
          ) {
            return arg.slice(1, -1);
          }
          // Handle special 'painting' keyword to reference system.painting
          if (typeof arg === "string" && arg === "painting") {
            return api.system?.painting;
          }
          return arg;
        });
        api.stamp(...processedArgs);
      },
      // Convert args to string and remove surrounding quotes for text commands
      write: (api, args = []) => {
        const content = unquoteString(args[0]?.toString() || "");
        let x = args[1];
        let y = args[2];

        // Process x argument to handle quoted strings
        if (typeof x === "string" && x.startsWith('"') && x.endsWith('"')) {
          x = unquoteString(x);
        }

        // Process y argument to handle quoted strings
        if (typeof y === "string" && y.startsWith('"') && y.endsWith('"')) {
          y = unquoteString(y);
        }

        // Only process background if it's not undefined - no should pass undefined for transparent bg
        const bg =
          args[3] !== undefined ? processArgStringTypes(args[3]) : undefined;

        const size = args[4]; // 5th parameter for scale/size
        const bounds = args[5]; // 6th parameter for text bounds/wrapping

        // Build options object if we have additional parameters
        const options = {};
        if (bg !== undefined) {
          options.bg = bg;
        }
        if (bounds !== undefined) {
          options.bounds = bounds;
        }

        // Check for centering keywords
        const centerX = x === "center";
        const centerY = y === "center";

        // Build position object
        const pos = {};

        // Handle centering
        if (centerX && centerY) {
          pos.center = "xy"; // Center both x and y
        } else if (centerX) {
          pos.center = "x"; // Center only x
          pos.y = y;
        } else if (centerY) {
          pos.center = "y"; // Center only y
          pos.x = x;
        } else {
          // No centering, use explicit coordinates
          if (x !== undefined) pos.x = x;
          if (y !== undefined) pos.y = y;
        }

        if (size !== undefined) {
          pos.size = size;
        }

        // Use options-based call if we have options, otherwise simple position call
        if (Object.keys(options).length > 0) {
          api.write(content, pos, options);
        } else {
          api.write(content, pos);
        }
      },
      // 🏷️ HUD label
      label: (api, args = []) => {
        const text = args[0] ? unquoteString(args[0].toString()) : undefined;
        const color = args[1];
        const offset = args[2];
        api.hud?.label?.(text, color, offset);
        return text;
      },
      len: (api, args = []) => {
        return args[0]?.toString().length;
      },
      // (Getters / globals).
      source: (api, args = [], env, colon) => {
        // console.log("✍️ Source:", this.ast, args, env, colon);
        let sourcedAST = [];
        if (colon) {
          if (!this.networkCache.sources[colon]) {
            this.networkCache.sources[colon] = "loading";
            fetch(`/aesthetic.computer/disks/${colon}.lisp`)
              .then((response) => response.text())
              .then((code) => {
                this.networkCache.sources[colon] = this.parse(code);
                api.needsPaint(); // 🖌️ Always require a paint after any network fetch.
              })
              .catch((error) => console.warn(error));
          } else {
            sourcedAST = this.networkCache.sources[colon];
          }
        } else {
          sourcedAST = this.ast;
        }
        return { iterable: true, data: sourcedAST };
      },
      width: (api) => {
        return api.screen.width;
      },
      w: (api) => { // Abbreviation for width
        return api.screen.width;
      },
      height: (api) => {
        return api.screen.height;
      },
      h: (api) => { // Abbreviation for height
        return api.screen.height;
      },
      frame: (api) => {
        return this.frameCount || 0;
      },
      f: (api) => { // Abbreviation for frame
        return this.frameCount || 0;
      },
      clock: (api) => {
        return Date.now(); // Returns UTC milliseconds since epoch
      },
      // 🔄 Repeat function (highly optimized)
      repeat: (api, args, env) => {
        perfStart("repeat-setup");
        if (args.length < 2) {
          console.error(
            "❗ repeat requires at least 2 arguments: count and expression(s)",
          );
          return undefined;
        }

        // Evaluate the count argument in case it's a variable or expression
        const countValue = this.evaluate(args[0], api, env);
        const count = Number(countValue);
        if (isNaN(count) || count < 0) {
          console.error(
            "❗ repeat count must be a non-negative number, got:",
            countValue,
          );
          return undefined;
        }
        perfEnd("repeat-setup");

        let result;

        // Check if we have an iterator variable (3+ args with 2nd arg being a string)
        if (args.length >= 3 && typeof args[1] === "string") {
          perfStart("repeat-with-iterator");
          const iteratorVar = args[1];
          const expressions = args.slice(2);

          // Fast path optimization: Check if this is a simple drawing pattern
          // like: (repeat count i (ink ...) (box ...))
          if (
            expressions.length === 2 &&
            Array.isArray(expressions[0]) &&
            expressions[0][0] === "ink" &&
            Array.isArray(expressions[1]) &&
            expressions[1][0] === "box"
          ) {
            perfStart("fast-draw-loop");
            // Optimized fast path for ink+box patterns
            const inkExpr = expressions[0];
            const boxExpr = expressions[1];

            // OPTIMIZATION 1: Pre-resolve color palette to avoid repeated parsing
            let paletteColors = null;
            let sequenceKey = null;
            let resolvedColors = null;

            if (
              inkExpr.length === 2 &&
              Array.isArray(inkExpr[1]) &&
              inkExpr[1][0] === "..."
            ) {
              paletteColors = inkExpr[1].slice(1); // Extract palette colors
              sequenceKey = JSON.stringify(paletteColors);

              // Pre-resolve colors to RGB values
              resolvedColors = paletteColors.map((colorName) => {
                // Use the API's color resolution if available
                if (api.help?.color) {
                  return api.help.color(colorName);
                }
                return colorName; // Fallback to string
              });

              // Initialize sequence counter if needed
              if (!this.sequenceCounters) {
                this.sequenceCounters = new Map();
              }
              if (!this.sequenceCounters.has(sequenceKey)) {
                this.sequenceCounters.set(sequenceKey, 0);
              }
            }

            // OPTIMIZATION 2: Pre-analyze box arguments for constant vs variable parts
            const boxArgs = boxExpr.slice(1);
            const analyzedBoxArgs = boxArgs.map((arg) => {
              if (
                Array.isArray(arg) &&
                arg.length === 3 &&
                arg[0] === "*" &&
                arg[1] === iteratorVar
              ) {
                // This is i*multiplier pattern
                return { type: "iterator_multiply", multiplier: arg[2] };
              } else if (this.containsVariable(arg, iteratorVar)) {
                return { type: "variable", expr: arg };
              } else {
                // Pre-evaluate constant expressions
                return {
                  type: "constant",
                  value: this.evaluate(arg, api, { ...this.localEnv, ...env }),
                };
              }
            });

            const prevLocalEnv = this.localEnv;

            // Set up minimal environment
            this.localEnvLevel += 1;
            if (!this.localEnvStore[this.localEnvLevel]) {
              this.localEnvStore[this.localEnvLevel] = Object.create(null);
            }
            const loopEnv = this.localEnvStore[this.localEnvLevel];

            // Copy environment efficiently
            for (const key in this.localEnv) {
              loopEnv[key] = this.localEnv[key];
            }
            for (const key in env) {
              loopEnv[key] = env[key];
            }
            this.localEnv = loopEnv;

            // OPTIMIZATION 3: Ultra-fast hot loop with pre-computed values
            for (let i = 0; i < count; i++) {
              loopEnv[iteratorVar] = i;

              // OPTIMIZATION 4: Direct color application without evaluation overhead
              if (resolvedColors) {
                const currentIndex = this.sequenceCounters.get(sequenceKey);
                const nextIndex = (currentIndex + 1) % resolvedColors.length;
                this.sequenceCounters.set(sequenceKey, nextIndex);
                const color = resolvedColors[currentIndex];

                // Direct color application
                if (typeof color === "object" && color.r !== undefined) {
                  // Pre-resolved RGB
                  api.ink(color.r, color.g, color.b, color.a || 255);
                } else {
                  api.ink(color);
                }
              } else {
                // Fallback to regular evaluation
                this.evaluate(inkExpr, api, this.localEnv);
              }

              // OPTIMIZATION 5: Fast box evaluation with pre-computed args
              const evaluatedBoxArgs = analyzedBoxArgs.map((arg) => {
                switch (arg.type) {
                  case "iterator_multiply":
                    return i * arg.multiplier; // Direct multiplication, no evaluation
                  case "constant":
                    return arg.value; // Pre-computed constant
                  case "variable":
                    return this.evaluate(arg.expr, api, this.localEnv);
                  default:
                    return 0;
                }
              });

              // Direct API call with minimal overhead
              api.box(...evaluatedBoxArgs);
            }

            this.localEnvLevel -= 1;
            this.localEnv = prevLocalEnv;
            perfEnd("fast-draw-loop");
            perfEnd("repeat-with-iterator");
            return result;
          }

          // OPTIMIZED: Standard loop with pre-evaluation and environment reuse
          const baseEnv = { ...this.localEnv, ...env };
          const prevLocalEnv = this.localEnv;

          // CORE OPTIMIZATION: Pre-analyze expressions for iterator dependency
          const expressionAnalysis = expressions.map((expr) => {
            const containsIterator = this.containsVariable
              ? this.containsVariable(expr, iteratorVar)
              : true;
            return { expr, containsIterator };
          });

          // CORE OPTIMIZATION: Pre-evaluate iterator-independent expressions
          const invariantResults = new Map();
          expressionAnalysis.forEach((analysis, index) => {
            if (!analysis.containsIterator) {
              invariantResults.set(
                index,
                this.evaluate(analysis.expr, api, baseEnv),
              );
            }
          });

          // Create reusable environment object
          this.localEnvLevel += 1;
          if (!this.localEnvStore[this.localEnvLevel]) {
            this.localEnvStore[this.localEnvLevel] = Object.create(null); // Faster than {}
          }

          // CORE OPTIMIZATION: Reuse environment object, direct property assignment
          const loopEnv = this.localEnvStore[this.localEnvLevel];
          // Clear previous properties more efficiently
          for (const key in loopEnv) {
            delete loopEnv[key];
          }
          // Copy base environment properties directly
          for (const key in baseEnv) {
            loopEnv[key] = baseEnv[key];
          }
          this.localEnv = loopEnv;

          for (let i = 0; i < count; i++) {
            // CORE OPTIMIZATION: Direct assignment instead of environment spread
            loopEnv[iteratorVar] = i;

            // Execute expressions with optimized evaluation
            expressionAnalysis.forEach((analysis, index) => {
              if (invariantResults.has(index)) {
                result = invariantResults.get(index);
              } else {
                result = this.evaluate(analysis.expr, api, this.localEnv);
              }
            });
          }

          // Restore previous environment
          this.localEnvLevel -= 1;
          this.localEnv = prevLocalEnv;
          perfEnd("repeat-with-iterator");
        } else {
          // Original behavior - no iterator variable
          const expressions = args.slice(1);
          for (let i = 0; i < count; i++) {
            for (const expr of expressions) {
              result = this.evaluate(expr, api, env);
            }
          }
        }

        return result;
      },
      // Abbreviation for repeat
      rep: (api, args, env) => {
        return this.getGlobalEnv().repeat(api, args, env);
      },
      // 🎲 Random selection
      choose: (api, args = []) => {
        if (args.length === 0) return undefined;
        // Use the help.choose function from the common API if available
        if (api.help?.choose) {
          return api.help.choose(...args);
        }
        // Fallback to simple random selection
        const randomIndex = Math.floor(Math.random() * args.length);
        return args[randomIndex];
      },
      // 🎲 Random selection (alias)
      "?": (api, args = []) => {
        if (args.length === 0) return undefined;
        // Use the help.choose function from the common API if available
        if (api.help?.choose) {
          return api.help.choose(...args);
        }
        // Fallback to simple random selection
        const randomIndex = Math.floor(Math.random() * args.length);
        return args[randomIndex];
      },
      // 🔄 Sequential selection (cycles through arguments in order)
      "...": (api, args = [], env) => {
        if (args.length === 0) return undefined;

        // Create a stable sequence key based on argument values
        const sequenceKey = JSON.stringify(args);

        // Initialize sequence counters if needed
        if (!this.sequenceCounters) {
          this.sequenceCounters = new Map();
        }

        // Initialize counter for this specific argument combination
        if (!this.sequenceCounters.has(sequenceKey)) {
          this.sequenceCounters.set(sequenceKey, 0);
        }

        // Get current index and increment for next call
        const currentIndex = this.sequenceCounters.get(sequenceKey);
        const nextIndex = (currentIndex + 1) % args.length;
        this.sequenceCounters.set(sequenceKey, nextIndex);

        return args[currentIndex];
      },
      // 🔄 Sequential selection (alias with two dots)
      "..": (api, args = [], env) => {
        // Delegate to the three-dot version
        return this.getGlobalEnv()["..."](api, args, env);
      },
      // 🔈 Sound
      overtone: (api, args = []) => {
        // console.log("Synth at:", args);
        let tone;
        if (args[0] === undefined) {
          tone = 440;
        } else {
          tone = (args[0] * args[1]) / args[2];
        }
        return api.sound.synth({
          type: "square",
          tone,
          // beats: 0.1,
          duration: Infinity,
          attack: 0.01,
          decay: 0.5,
          volume: 0.15,
        });
      },
      rainbow: (api) => {
        return api.num?.rainbow() || [255, 0, 0]; // Fallback to red if not available
      },
      // 🎨 Noise generation
      noise: (api, args = []) => {
        const variant = args.length > 0 ? unquoteString(args[0]) : null;
        if (variant === "digitpain") {
          api.noise16DIGITPAIN?.();
        } else if (variant === "aesthetic") {
          api.noise16Aesthetic?.();
        } else if (variant === "sotce") {
          api.noise16Sotce?.();
        } else {
          // Default to basic noise16
          api.noise16?.();
        }
      },
      // 🔧 Debug function
      debug: (api, args = []) => {
        console.log("🔧 DEBUG:", args);
        return "debug called";
      },
      log: (api, args = []) => {
        // console.clear(); // Commented out to preserve debug logs
        console.log("📝 LOG:", ...args);
        return args[0];
      },
      // 💾 Cache function - loads cached KidLisp code using nanoid
      // Usage: (cache abc123XY) or (cache $abc123XY) loads cached code
      cache: (api, args = []) => {
        if (args.length === 0) {
          console.warn("❗ cache function requires a nanoid argument");
          return undefined;
        }
        
        let cacheId = unquoteString(args[0].toString());
        
        // Strip $ prefix if present (allow both $OrqM and OrqM formats)
        if (cacheId.startsWith("$")) {
          cacheId = cacheId.slice(1);
        }
        
        // Simple validation - just check it's not empty and alphanumeric
        if (!cacheId || !/^[0-9A-Za-z]+$/.test(cacheId)) {
          console.warn("❗ Invalid cache code:", cacheId);
          return cacheId; // Return as-is if not valid
        }
        
        // Return a promise that fetches and evaluates the cached code
        return fetchCachedCode(cacheId).then(source => {
          if (source) {
            console.log("🎯 Loading cached KidLisp code:", cacheId);
            // Parse and evaluate the cached source
            const parsed = this.parse(source);
            return this.evaluate(parsed, api, this.localEnv);
          } else {
            console.warn("❌ No cached code found for:", cacheId);
            return undefined;
          }
        }).catch(error => {
          console.error("❌ Error loading cached code:", cacheId, error);
          return undefined;
        });
      },
      // 🚫 Disable wrapper - ignores wrapped expressions
      no: (api, args = []) => {
        // Always return JavaScript undefined, regardless of arguments
        // This effectively disables/comments out the expression and passes undefined to callers
        return undefined;
      },
      // ✅ Enable wrapper - passthrough that evaluates wrapped expressions
      yes: (api, args = []) => {
        // Evaluate and return the first argument (passthrough behavior)
        // This allows easy toggling between no/yes during development
        if (args.length === 0) return undefined;
        return this.evaluate(args[0], api, this.localEnv);
      },
      // 🎤 Microphone
      mic: (api, args = []) => {
        // Lazy connection: try to connect microphone if not already connected
        if (this.microphoneApi && !this.microphoneApi.connected) {
          if (
            this.microphoneApi.permission === "granted" &&
            api.sound?.enabled?.()
          ) {
            console.log("🎤 Lazy connecting microphone (mic function called)");
            this.microphoneApi.connect();
          } else if (this.microphoneApi.permission !== "granted") {
            // Only log this occasionally to avoid spam
            if (this.frameCount % 120 === 0) {
              console.log(
                "🎤 Microphone permission not granted, cannot connect",
              );
            }
          }
        }

        // Debug: Log the current state (less frequently)
        if (this.frameCount % 120 === 0 && this.microphoneApi) {
          console.log(
            "🎤 Mic function called - Connected:",
            this.microphoneApi.connected,
            "Amplitude:",
            this.microphoneApi.amplitude,
          );
        }

        // Return current amplitude if microphone is available and connected
        if (this.microphoneApi && this.microphoneApi.connected) {
          return this.microphoneApi.amplitude || 0;
        }

        // If not connected yet, return 0
        return 0;
      },
      // 🔊 Real-time audio amplitude from speakers/system audio
      amplitude: (api, args = []) => {
        let amplitudeValue = 0;

        // Access speaker amplitude from the sound API like clock.mjs does
        if (api.sound?.speaker?.amplitudes?.left !== undefined) {
          amplitudeValue = api.sound.speaker.amplitudes.left;
        }
        // Fallback: check for right channel if left is not available
        else if (api.sound?.speaker?.amplitudes?.right !== undefined) {
          amplitudeValue = api.sound.speaker.amplitudes.right;
        }

        return amplitudeValue;
      },
      // 🎵 Melody - plays a sequence of notes in a loop
      melody: (api, args = []) => {
        if (args.length === 0) return;

        const melodyString = unquoteString(args[0]);
        const bpm = args.length > 1 ? parseFloat(args[1]) : 120; // Default 120 BPM
        const timeSignature = args.length > 2 ? unquoteString(args[2]) : "4/4"; // Default 4/4 time
        const feel = args.length > 3 ? unquoteString(args[3]) : "straight"; // Default straight feel

        // Parse time signature
        const [numerator, denominator] = timeSignature.split("/").map(Number);
        const beatsPerMeasure = numerator;
        const beatUnit = denominator; // 4 = quarter note, 8 = eighth note, etc.

        // Convert BPM to milliseconds per quarter note
        // Adjust base tempo based on beat unit
        let baseTempo = (60 / bpm) * 1000; // 60 seconds / BPM * 1000ms
        if (beatUnit === 8) {
          baseTempo = baseTempo / 2; // Eighth note gets the beat
        } else if (beatUnit === 2) {
          baseTempo = baseTempo * 2; // Half note gets the beat
        }

        const melodyId = melodyString + timeSignature + feel; // Include time signature and feel in ID

        // Check if this melody is already defined and hasn't changed
        if (this.melodies.has(melodyId)) {
          // Melody already exists, don't redefine (like def behavior)
          return;
        }

        // Parse the melody string into notes with durations
        const notes = this.parseMelodyString(melodyString);

        if (notes.length === 0) return;

        // Store the melody state
        const melodyState = {
          notes: notes,
          index: 0,
          lastPlayTime: 0,
          nextNoteTime: 0,
          baseTempo: baseTempo,
          timeSignature: timeSignature,
          beatsPerMeasure: beatsPerMeasure,
          beatUnit: beatUnit,
          feel: feel,
          measurePosition: 0, // Track position within the measure for swing
          isPlaying: false,
        };

        this.melodies.set(melodyId, melodyState);

        // Start playing immediately
        this.playMelodyNote(api, melodyId);

        return melodyString;
      },
      // 🔊 Speaker - returns whether sound is enabled
      speaker: (api) => {
        return api.sound?.enabled?.() || false;
      },
      
      // 🎨 Backdrop - shorthand for (once (wipe color)) to set background once
      backdrop: (api, args = []) => {
        if (args.length === 0) {
          console.error("❗ Invalid `backdrop`. Requires at least one color argument.");
          return;
        }

        // Create a unique key for this backdrop call based on its arguments
        const backdropKey = "backdrop_" + JSON.stringify(args);

        // Only execute if this exact backdrop hasn't been executed before
        if (!this.onceExecuted.has(backdropKey)) {
          this.onceExecuted.add(backdropKey);
          
          // Call wipe with the provided arguments
          if (api.wipe) {
            if (args.length === 1) {
              return api.wipe(this.evaluate(args[0], api, this.localEnv));
            } else if (args.length === 3) {
              // RGB values
              return api.wipe(
                this.evaluate(args[0], api, this.localEnv),
                this.evaluate(args[1], api, this.localEnv),
                this.evaluate(args[2], api, this.localEnv)
              );
            } else if (args.length === 4) {
              // RGBA values
              return api.wipe(
                this.evaluate(args[0], api, this.localEnv),
                this.evaluate(args[1], api, this.localEnv),
                this.evaluate(args[2], api, this.localEnv),
                this.evaluate(args[3], api, this.localEnv)
              );
            }
          }
        }

        return undefined;
      },
      
      // Programmatically add all CSS color constants to the global environment.
      ...Object.keys(cssColors).reduce((acc, colorName) => {
        acc[colorName] = () => cssColors[colorName];
        return acc;
      }, {}),
    };

    return this.globalEnvCache;
  }

  // Fast evaluation for common expressions to avoid full recursive evaluation
  fastEval(expr, api, env) {
    if (typeof expr === "number") return expr;
    if (typeof expr === "string") {
      // Fast variable lookup - don't cache iterator variables that change frequently
      let value;
      if (Object.prototype.hasOwnProperty.call(this.localEnv, expr)) {
        value = this.localEnv[expr];
      } else if (env && Object.prototype.hasOwnProperty.call(env, expr)) {
        value = env[expr];
      } else if (Object.prototype.hasOwnProperty.call(this.globalDef, expr)) {
        value = this.globalDef[expr];
      }

      if (value !== undefined) {
        return value;
      }

      // Check global environment
      const globalEnv = this.getGlobalEnv();
      value = globalEnv[expr];
      if (typeof value === "function") {
        value = value(api, []); // Pass empty args array for functions called as variables
        return value; // Return the function result directly, even if it's undefined
      }

      const result = value !== undefined ? value : expr;
      return result;
    }

    // Fast math expression evaluation
    if (Array.isArray(expr) && expr.length === 3) {
      const [op, left, right] = expr;
      if (op === "+" || op === "-" || op === "*" || op === "/" || op === "%") {
        const leftVal = this.fastEval(left, api, env);
        const rightVal = this.fastEval(right, api, env);

        // Only proceed if both are numbers
        if (typeof leftVal === "number" && typeof rightVal === "number") {
          switch (op) {
            case "+":
              return leftVal + rightVal;
            case "-":
              return leftVal - rightVal;
            case "*":
              return leftVal * rightVal;
            case "/":
              return leftVal / rightVal;
            case "%":
              return leftVal % rightVal;
          }
        }
      }
    }

    // Fall back to full evaluation
    return this.evaluate(expr, api, env);
  }

  // Optimized function resolution
  resolveFunction(head, api, env) {
    // Check cache first
    const cacheKey = `${head}_${this.localEnvLevel}`;
    if (this.functionCache.has(cacheKey)) {
      return this.functionCache.get(cacheKey);
    }

    let result = null;

    // Fast lookup order: local -> env -> global -> globalDef -> api
    if (existing(this.localEnv[head])) {
      result = { type: "local", value: this.localEnv[head] };
    } else if (existing(env?.[head])) {
      result = { type: "env", value: env[head] };
    } else if (existing(this.getGlobalEnv()[head])) {
      result = { type: "global", value: this.getGlobalEnv()[head] };
    } else if (existing(this.globalDef[head])) {
      result = { type: "globalDef", value: this.globalDef[head] };
    } else if (existing(api[head]) && typeof api[head] === "function") {
      result = { type: "api", value: api[head] };
    }

    // Cache the result (but not for local env since it changes)
    if (result && result.type !== "local") {
      this.functionCache.set(cacheKey, result);
    }

    return result;
  }

  evaluate(parsed, api = {}, env, inArgs) {
    perfStart("evaluate-total");
    if (VERBOSE) console.log("➗ Evaluating:", parsed);

    let body;

    // Get global environment for this instance
    perfStart("get-global-env");
    const globalEnv = this.getGlobalEnv();
    perfEnd("get-global-env");

    // Add screen reference directly to global environment
    if (api.screen) {
      globalEnv.screen = api.screen;
    }

    // Create local environment for a function that temporarily keeps the params.
    if (parsed.body) {
      const newLocalEnv = {};

      body = parsed.body;

      parsed.params.forEach((param, i) => {
        console.log(
          "😉 Binding param:",
          param,
          "to value:",
          inArgs?.[i],
          "at index:",
          i,
        );
        // inArgs are already evaluated arguments, so just bind them directly
        if (i < inArgs.length) {
          newLocalEnv[param] = inArgs[i];
        } else {
          console.warn(
            `Parameter ${param} at index ${i} has no corresponding argument`,
          );
          newLocalEnv[param] = undefined;
        }
      });

      // Set up the new local environment
      this.localEnvLevel += 1;
      if (!this.localEnvStore[this.localEnvLevel]) {
        this.localEnvStore[this.localEnvLevel] = {};
      }
      // Copy existing environment and add new bindings
      this.localEnvStore[this.localEnvLevel] = {
        ...this.localEnv,
        ...newLocalEnv,
      };
      this.localEnv = this.localEnvStore[this.localEnvLevel];

      console.log(
        "🟠 Local env level:",
        this.localEnvLevel,
        "Environment:",
        this.localEnv,
      );

      if (VERBOSE)
        console.log("Running:", body, "with environment:", this.localEnv);
    } else {
      // Or just evaluate with the global environment, ensuring it's an array.
      // If parsed is an array that looks like a function call, wrap it
      if (
        Array.isArray(parsed) &&
        parsed.length > 0 &&
        typeof parsed[0] === "string"
      ) {
        body = [parsed]; // Wrap the function call
      } else {
        body = Array.isArray(parsed) ? parsed : [parsed];
      }
    }

    if (VERBOSE) console.log("🏃 Body:", body);

    // 🎨 First-line color shorthand: If the first item is just a color name, 
    // treat it as (once (wipe <color>)) for easy backdrop setting
    if (body.length > 0 && !parsed.body) {
      const firstItem = body[0];
      let colorName = null;
      
      // Check if it's a bare string color name
      if (typeof firstItem === "string") {
        colorName = firstItem;
      }
      // Check if it's a single-argument function call like ["red"]
      else if (Array.isArray(firstItem) && firstItem.length === 1 && typeof firstItem[0] === "string") {
        colorName = firstItem[0];
      }
      
      if (colorName) {
        const globalEnv = this.getGlobalEnv();
        
        // Check if it's a color name in the global environment
        if (globalEnv[colorName] && typeof globalEnv[colorName] === "function") {
          try {
            // Test if this is a color function by calling it
            globalEnv[colorName]();
            
            // If we get here without error, it's a color function
            // Store the color name as the default background for this KidLisp piece
            if (!this.firstLineColor) {
              this.firstLineColor = colorName;
              
              // Set the background fill color for reframe operations
              if (api.backgroundFill) {
                api.backgroundFill(colorName);
              }
              
              // Apply wipe once using the once mechanism
              const backdropKey = "first_line_backdrop_" + colorName;
              if (!this.onceExecuted.has(backdropKey)) {
                this.onceExecuted.add(backdropKey);
                if (api.wipe) {
                  api.wipe(colorName);
                }
                
                // Show visual feedback only once
                console.log("🎨 First-line color backdrop:", colorName);
                
                // Add visual indicator to HUD showing backdrop was applied
                if (api.hud?.label) {
                  api.hud.label(`🎨 ${colorName} backdrop`, undefined, undefined);
                  // Set a timer to clear the label after a few seconds
                  setTimeout(() => {
                    if (api.hud?.label) {
                      api.hud.label(undefined);
                    }
                  }, 3000);
                }
              }
            }
            // Remove the first item so it doesn't get evaluated again
            body = body.slice(1);
          } catch (e) {
            // Not a color function, proceed normally
          }
        }
      }
    }

    let result;

    for (const item of body) {
      /*if (VERBOSE)*/ // console.log("🥡 Item:", item /*, "body:", body*/);

      // Handle optimized functions first
      if (item && typeof item === "object" && item.optimized) {
        perfStart("optimized-execution");
        result = item.func(api);
        perfEnd("optimized-execution");
        continue;
      }

      if (Array.isArray(item)) {
        // The first element indicates the function to call
        let [head, ...args] = item;

        // 🎵 Handle integer timing: (0 ...), (1 ...), (2 ...), etc.
        // Also handle second timing: (1s ...), (2s ...), etc.
        if (typeof head === "number" && Number.isInteger(head)) {
          const frameDivisor = head + 1; // 0 = every frame, 1 = every 2nd frame, etc.
          if (this.frameCount % frameDivisor === 0) {
            this.markTimingTriggered(head.toString()); // Trigger blink for integer timing
            // Execute the timing arguments with proper context
            let timingResult;
            for (const arg of args) {
              timingResult = this.evaluate([arg], api, env);
            }
            result = timingResult;
          }
          continue; // Skip normal function processing
        } else if (typeof head === "string" && /^\d*\.?\d+s!?$/.test(head)) {
          // Handle second-based timing like "0s", "1s", "2s", "5s", "1.5s", "0.3s"
          // Also handle instant trigger modifier like "0.25s!", "1s!", "2s!"
          const hasInstantTrigger = head.endsWith("!");
          const timeString = hasInstantTrigger ? head.slice(0, -1) : head;
          const seconds = parseFloat(timeString.slice(0, -1)); // Remove 's' and parse as float

          if (seconds === 0) {
            // 0s = every frame (no timing restriction)
            this.markTimingTriggered(head); // Trigger blink
            let timingResult;
            for (const arg of args) {
              timingResult = this.evaluate([arg], api, env);
            }
            result = timingResult;
          } else if (seconds < 0.016) {
            // For very small intervals (less than ~60fps), limit to 60fps max to prevent excessive triggering
            const minInterval = 0.016; // ~16ms, roughly 60fps
            const clockResult = api.clock.time();
            if (!clockResult) continue;

            const currentTimeMs = clockResult.getTime ? clockResult.getTime() : Date.now();
            const currentTime = currentTimeMs / 1000;
            const timingKey = head + "_" + args.length;

            if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
              this.lastSecondExecutions[timingKey] = currentTime;
            } else {
              const lastExecution = this.lastSecondExecutions[timingKey];
              const diff = currentTime - lastExecution;

              if (diff >= minInterval) {
                this.lastSecondExecutions[timingKey] = currentTime;
                this.markTimingTriggered(head);
                let timingResult;
                for (const arg of args) {
                  timingResult = this.evaluate([arg], api, env);
                }
                result = timingResult;
              }
            }
          } else {
            const clockResult = api.clock.time(); // Get time (Date object)
            if (!clockResult) continue;

            // Convert Date object to milliseconds, then to seconds
            const currentTimeMs = clockResult.getTime
              ? clockResult.getTime()
              : Date.now();
            const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

            // Create a unique key for this timing expression - use simpler key generation
            const timingKey = head + "_" + args.length;

            // Initialize lastExecution to current time if not set
            if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
              // If has instant trigger (!) and hasn't been executed yet, execute immediately
              if (
                hasInstantTrigger &&
                !this.instantTriggersExecuted[timingKey]
              ) {
                this.instantTriggersExecuted[timingKey] = true;
                // Set baseline time for future intervals
                this.lastSecondExecutions[timingKey] = currentTime;

                this.markTimingTriggered(head); // Trigger blink
                let timingResult;
                for (const arg of args) {
                  timingResult = this.evaluate([arg], api, env);
                }
                result = timingResult;
              } else {
                // Normal first-time setup - establish baseline without executing
                this.lastSecondExecutions[timingKey] = currentTime;
              }
            } else {
              // Normal timing logic for subsequent calls
              const lastExecution = this.lastSecondExecutions[timingKey];
              const diff = currentTime - lastExecution;

              // Add small tolerance for floating-point precision issues
              // Use a 5ms tolerance to prevent rapid fire due to precision errors
              const tolerance = 0.005; // 5 milliseconds in seconds
              const adjustedSeconds = Math.max(seconds, tolerance);

              // Check if enough time has passed since last execution
              if (diff >= adjustedSeconds) {
                this.lastSecondExecutions[timingKey] = currentTime;

                this.markTimingTriggered(head); // Trigger blink
                // Execute the timing arguments with proper context
                let timingResult;
                for (const arg of args) {
                  timingResult = this.evaluate([arg], api, env);
                }
                result = timingResult;
              }
            }
          }
          continue; // Skip normal function processing
        } else if (
          typeof head === "string" &&
          /^\d*\.?\d+s\.\.\.?$/.test(head)
        ) {
          // Handle timed iteration like "1s...", "2s...", "1.5s..."
          const seconds = parseFloat(head.slice(0, head.indexOf("s"))); // Extract seconds part

          // Get current time directly using Date.now() instead of evaluating clock function
          const currentTimeMs = Date.now();
          const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

          // Create a unique key for this timed iteration expression - use simpler key generation
          const timingKey = head + "_" + args.length;

          // Initialize timing tracking if not set
          if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
            this.lastSecondExecutions[timingKey] = currentTime;
            // Initialize sequence counter for this timed iteration
            if (!this.sequenceCounters) {
              this.sequenceCounters = new Map();
            }
            this.sequenceCounters.set(timingKey, 0);
          }

          const lastExecution = this.lastSecondExecutions[timingKey];
          const diff = currentTime - lastExecution;

          // Check if enough time has passed to advance to next item
          if (diff >= seconds) {
            this.lastSecondExecutions[timingKey] = currentTime;
            
            // Mark timing as triggered for red blink effect
            this.markTimingTriggered(head);

            // Advance the sequence counter
            const currentIndex = this.sequenceCounters.get(timingKey) || 0;
            const nextIndex = (currentIndex + 1) % args.length;
            this.sequenceCounters.set(timingKey, nextIndex);
            
            // Debug timing switches for "2s..." expressions
            if (head === "2s...") {
              console.log(`⏰ TIMING SWITCH: ${timingKey} - ${currentIndex} → ${nextIndex}, args.length: ${args.length}, diff: ${diff.toFixed(2)}s`);
            }
          }

          // Always return the current item (not just when advancing)
          if (args.length > 0) {
            const currentIndex = this.sequenceCounters.get(timingKey) || 0;
            
            // Debug timing state for "2s..." expressions (commented out - too frequent)
            // if (head === "2s...") {
            //   console.log(`⏰ TIMING DEBUG: ${timingKey} - currentIndex: ${currentIndex}, args.length: ${args.length}, diff: ${diff.toFixed(2)}s, threshold: ${seconds}s`);
            // }
            
            // Track the active timing expression for syntax highlighting
            this.activeTimingExpressions.set(timingKey, {
              currentIndex,
              totalArgs: args.length,
              timingToken: head,
              args: args
            });
            
            // Signal syntax highlighting for timing expressions
            for (let i = 0; i < args.length; i++) {
              const color = i === currentIndex ? "olive" : "255,255,255,0"; // Active: olive, Inactive: transparent
              this.signalSyntaxHighlight(args[i], color);
            }
            
            // Evaluate the selected argument instead of just returning it
            result = this.evaluate(args[currentIndex], api, env);
          }
          continue; // Skip normal function processing
        }

        // const colon = head.split(":")[1]; // TODO: Take into account colon param / work it in.

        // Make sure head exists and re-evaluate or iterate if not a string.
        if (!existing(head)) {
          return this.evalNotFound(head);
        }

        if (Array.isArray(head)) {
          const evaledHead = this.evaluate([head], api, env);
          const newEval = [evaledHead, ...args];
          result = this.evaluate([newEval], api, env);
          continue;
        }

        if (typeof head !== "string" && head?.iterable) {
          result = this.iterate(head, api, args, env);
          continue;
        }

        // Parse the head string splitting on dots and colons.
        if (typeof head !== "string") {
          // If head is not a string, it might be a number or other value
          // Convert to string or handle appropriately
          head = String(head);
        }

        let splitHead = [];
        let colon = null;

        // Special handling for ... function name to avoid dot splitting
        if (head === "..." || head === "..") {
          // Don't split these special function names

          colon = head.includes(":") ? head.split(":")[1] : null;
          head = head.split(":")[0];
        } else {
          splitHead = head.split(".");
          head = splitHead[0];

          const colonSplit = head.split(":");
          head = colonSplit[0];
          colon = colonSplit[1]; // Will be incorporated in globalEnv api.
        }

        // if (head === "box") {
        //  console.log("🧕 Head:", head);
        // }

        // Check if the function requires recursive evaluation
        if (head === "now" || head === "def" || head === "die")
          args[0] = `"${args[0]}"`; // Pre-wrap the first arg as a string.

        // Use optimized function resolution
        const resolved = this.resolveFunction(head, api, env);

        if (resolved) {
          const { type, value } = resolved;

          switch (type) {
            case "local":
              perfStart(`local-${head}`);
              if (VERBOSE)
                console.log("😫 Local definition found!", head, value);
              if (value.iterable) {
                result = this.iterate(value, api, args, env);
              } else {
                result = value;
              }
              perfEnd(`local-${head}`);
              break;

            case "env":
              perfStart(`env-${head}`);
              if (value.iterable) {
                result = this.iterate(value, api, args, env);
              } else {
                result = value;
              }
              perfEnd(`env-${head}`);
              break;

            case "global":
              // Handle global environment functions
              if (typeof value === "function" || typeof value === "object") {
                let processedArgs;
                // Functions that don't need argument evaluation
                if (
                  head === "later" ||
                  head === "tap" ||
                  head === "draw" ||
                  head === "if" ||
                  head === "not" ||
                  head === ">" ||
                  head === "<" ||
                  head === "=" ||
                  head === "net" ||
                  head === "source" ||
                  head === "choose" ||
                  head === "?" ||
                  head === "repeat" ||
                  head === "once"
                ) {
                  processedArgs = args;
                } else {
                  // Use fast evaluation for arguments with current local environment
                  processedArgs = args.map((arg, index) => {
                    if (
                      Array.isArray(arg) ||
                      (typeof arg === "string" && !/^".*"$/.test(arg))
                    ) {
                      const result = this.fastEval(arg, api, this.localEnv);
                      return result;
                    } else {
                      return arg;
                    }
                  });
                }

                if (splitHead[1]) {
                  result = getNestedValue(value, item[0])(api, processedArgs);
                } else {
                  result = value(api, processedArgs, env, colon);
                  if (result?.iterable) {
                    result = this.iterate(result, api, args, env);
                    continue;
                  }
                }
              } else {
                result = value;
              }
              break;

            case "globalDef":
              // User-defined functions - use fast evaluation for arguments
              const evaluatedArgs = args.map((arg) =>
                Array.isArray(arg) ||
                (typeof arg === "string" && !/^".*"$/.test(arg))
                  ? this.fastEval(arg, api, this.localEnv)
                  : arg,
              );

              result =
                Array.isArray(value) || value.body
                  ? this.evaluate(value, api, this.localEnv, evaluatedArgs)
                  : value;
              break;

            case "api":
              // API functions - use fast evaluation for arguments
              const apiArgs = args.map((arg) =>
                Array.isArray(arg) ||
                (typeof arg === "string" && !/^".*"$/.test(arg))
                  ? this.fastEval(arg, api, this.localEnv)
                  : arg,
              );
              result = value(...apiArgs);
              break;
          }
        } else {
          // console.log(
          //   "⛔ No match found for:",
          //   head,
          //   "localEnv:",
          //   this.localEnv,
          // );
          if (Array.isArray(head)) {
            if (VERBOSE) console.log("Environment:", this.localEnv);
            result = this.evaluate(head, api, this.localEnv);
          } else {
            result = this.evalNotFound(head, api, this.localEnv);
          }
        }
      } else {
        // Handle simple variable lookups and expressions
        let root, tail;
        if (typeof item === "string") {
          [root, tail] = item.split(".");
        }

        if (!Array.isArray(env) && existing(env?.[root])) {
          result = getNestedValue(env, item);
        } else {
          // Use fast evaluation for simple cases
          result = this.fastEval(item, api, env);

          // If fast eval returned the original item, it wasn't found
          if (result === item) {
            if (Array.isArray(item)) {
              result = this.evaluate(item, api, env);
            } else {
              result = this.evalNotFound(item, api, env);
            }
          }
        }
      }
    }

    // Restore previous local environment if we were in a function
    if (parsed.body) {
      this.localEnvLevel -= 1;
      this.localEnv = this.localEnvStore[this.localEnvLevel] || {};
      console.log(
        "🔙 Restored env level:",
        this.localEnvLevel,
        "Environment:",
        this.localEnv,
      );
    }

    perfEnd("evaluate-total");
    return result;
  }

  evalNotFound(expression, api, env) {
    if (typeof expression !== "string") {
      // console.log("🤖 Expression:", expression);
      return expression; // Return numbers.
    } else {
      // console.log("🤖 Attempting JavaScript expression evaluation:", expression);
    }

    // Check if this is a simple function identifier that can be auto-called
    if (validIdentifierRegex.test(expression)) {
      const globalEnv = this.getGlobalEnv();

      // Check if the identifier exists as a function in the global environment
      if (
        globalEnv[expression] &&
        typeof globalEnv[expression] === "function"
      ) {
        // Auto-call the function with no arguments
        return globalEnv[expression](api, [], env);
      }

      // Check if it exists in the API
      if (api[expression] && typeof api[expression] === "function") {
        return api[expression]();
      }
    }

    // 📖 Identifiers can only start with a letter a-z or A-Z and cannot
    //    include mathematical operators but can include underscores or
    //    digits after the first character

    // Parse the expression to extract identifiers.
    const identifiers = expression.match(identifierRegex) || [];

    // Get global environment for this instance
    const globalEnv = this.getGlobalEnv();

    // Evaluate identifiers by running this.evaluate([id], api, env);
    identifiers.forEach((id) => {
      // Use fast evaluation for identifier lookup
      let value = this.fastEval(id, api, env);

      if (value === id) {
        // Variable not found, try to get it from global environment
        console.warn("❗ Identifier not found:", id);
        value = 0;
      }

      // Replace any identifiers and cancel out prefixed double negatives.
      expression = expression.replace(new RegExp(`\\b${id}\\b`, "g"), value);
    });

    const compute = new Function(`return ${expression};`);
    const result = compute();
    // console.log("Evaluated result:", result);
    return result;
  }

  // Loop over an iterable.
  iterate(iterable, api, args, outerEnv) {
    // console.log("Iterable:", iterable);
    iterable.data.forEach((item, index) => {
      const env = { ...outerEnv };
      // Make the list item available in the local environment.
      env.index = index;
      env.item = item;
      if (Array.isArray(item)) item = { iterable: true, data: item.slice() };
      env.data = item;
      // console.log("🔢 Index:", index, "Item:", item, "Env:", env);
      // Execute any provided function argument on each item.
      args.forEach((arg) => this.evaluate([arg], api, env));
    });
    return iterable;
  }

  // Syntax highlighting methods for HUD integration

  // Initialize syntax highlighting for a kidlisp source
  initializeSyntaxHighlighting(source) {
    this.syntaxHighlightSource = source;
    this.expressionPositions = this.mapExpressionsToPositions(source);
    this.executionHistory = [];
    this.currentlyHighlighted.clear();
  }

  // Map parsed expressions to their positions in the source code
  mapExpressionsToPositions(source) {
    const positions = [];
    let charIndex = 0;

    // Process the entire source as one string to handle multi-line expressions
    let i = 0;
    while (i < source.length) {
      if (source[i] === "(") {
        const exprStart = charIndex + i;
        let depth = 1;
        let j = i + 1;

        // Find matching closing paren, handling multi-line expressions
        while (j < source.length && depth > 0) {
          if (source[j] === "(") depth++;
          else if (source[j] === ")") depth--;
          j++;
        }

        if (depth === 0) {
          const exprEnd = charIndex + j;
          const exprText = source.substring(i, j);

          // Only add top-level expressions to avoid overlaps
          const isNested = positions.some(pos => 
            exprStart > pos.start && exprStart < pos.end
          );

          if (!isNested) {
            positions.push({
              start: exprStart,
              end: exprEnd,
              text: exprText,
              isExpression: true,
            });
          }
          
          i = j; // Skip past this expression
        } else {
          i++; // Unmatched opening paren, move forward
        }
      } else {
        i++;
      }
    }

    return positions;
  }

  // Mark an expression as currently executing
  markExpressionExecuting(expr) {
    const now = performance.now();
    this.currentExecutingExpression = expr;
    this.lastExecutionTime = now;

    // Add to execution history for flash effect
    this.executionHistory.push({
      expression: expr,
      timestamp: now,
      type: "execution",
    });

    // Keep history limited to recent items
    if (this.executionHistory.length > 50) {
      this.executionHistory = this.executionHistory.slice(-25);
    }
  }

  // Generate colored syntax highlighting string for HUD using proper tokenization
  buildColoredKidlispString() {
    if (!this.syntaxHighlightSource) return "";

    try {
      // Tokenize the source code for proper syntax highlighting
      const tokens = tokenize(this.syntaxHighlightSource);
      
      if (tokens.length === 0) {
        return `\\white\\${this.syntaxHighlightSource}`;
      }

      // Build colored string by finding each token in the original source and preserving whitespace
      let result = "";
      let sourceIndex = 0;
      let lastColor = null;
      
      for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];
        const color = this.getTokenColor(token, tokens, i);
        
        // Find the token in the original source starting from our current position
        const tokenIndex = this.syntaxHighlightSource.indexOf(token, sourceIndex);
        
        if (tokenIndex !== -1) {
          // Add any whitespace/formatting between the last token and this one
          const whitespace = this.syntaxHighlightSource.substring(sourceIndex, tokenIndex);
          result += whitespace;
          
          // Add color escape sequence when color changes
          if (color !== lastColor) {
            result += `\\${color}\\`;
            lastColor = color;
          }
          
          // Add the token itself
          result += token;
          
          // Update our position in the source
          sourceIndex = tokenIndex + token.length;
        }
      }
      
      // Add any remaining whitespace at the end
      if (sourceIndex < this.syntaxHighlightSource.length) {
        result += this.syntaxHighlightSource.substring(sourceIndex);
      }
      
      return result;
    } catch (error) {
      console.warn("Error building colored kidlisp string:", error);
      // Fallback to plain white text
      return `\\white\\${this.syntaxHighlightSource}`;
    }
  }

  // Check if a token is part of a timing expression and get its state
  getTimingTokenState(token, tokens, index) {
    // First, check if the current token itself is a timing token
    if (/^\d*\.?\d+s\.\.\.?$/.test(token)) {
      // Try to find this timing key in our active tracking
      // We need to check all possible timing keys since we don't know the exact arg count here
      for (const [timingKey, data] of this.activeTimingExpressions) {
        if (timingKey.startsWith(token + "_")) {
          return {
            timingKey,
            currentIndex: data.currentIndex,
            totalArgs: data.totalArgs,
            isBlinking: this.isTimingBlinking(token),
            isInActiveArg: true // The timing token itself is always "active"
          };
        }
      }
      
      // Fallback to sequence counters if not in active expressions
      for (const [timingKey, currentIndex] of this.sequenceCounters) {
        if (timingKey.startsWith(token + "_")) {
          const argCount = parseInt(timingKey.split("_")[1]) || 0;
          return {
            timingKey,
            currentIndex,
            totalArgs: argCount,
            isBlinking: this.isTimingBlinking(token),
            isInActiveArg: true
          };
        }
      }
    }

    // Check if this token is inside a timing expression's arguments
    for (let i = 0; i < index; i++) {
      const prevToken = tokens[i];
      if (/^\d*\.?\d+s\.\.\.?$/.test(prevToken)) {
        if (token === "line" || token === "box") {
          console.log(`🔍 Found timing token "${prevToken}" at index ${i} while processing "${token}" at index ${index}`);
        }
        // Try to find this timing key in our active tracking
        for (const [timingKey, data] of this.activeTimingExpressions) {
          if (timingKey.startsWith(prevToken + "_")) {
            if (token === "line" || token === "box") {
              console.log(`🔍 Checking timing key "${timingKey}" for token "${token}"`);
            }
            
            // Check if current token is within one of the timing arguments
            const argInfo = this.getTokenArgPosition(tokens, i, index);
            if (token === "line" || token === "box") {
              console.log(`🔍 getTokenArgPosition(${i}, ${index}) returned:`, argInfo);
            }
            
            if (argInfo && argInfo.argIndex < data.totalArgs) {
              const isActive = argInfo.argIndex === data.currentIndex;
              // Debug: Log which argument this token belongs to
              if (token === "line" || token === "box") {
                console.log(`🔍 Token "${token}" -> arg ${argInfo.argIndex}, current active: ${data.currentIndex}, is active: ${isActive}`);
              }
              return {
                timingKey,
                currentIndex: data.currentIndex,
                totalArgs: data.totalArgs,
                isBlinking: this.isTimingBlinking(prevToken),
                isInActiveArg: isActive
              };
            } else {
              console.log(`🔍 Token "${token}" not in valid argument range or argInfo is null`);
            }
          }
        }
        
        // Fallback to sequence counters
        for (const [timingKey, currentIndex] of this.sequenceCounters) {
          if (timingKey.startsWith(prevToken + "_")) {
            const argCount = parseInt(timingKey.split("_")[1]) || 0;
            const argInfo = this.getTokenArgPosition(tokens, i, index);
            if (argInfo && argInfo.argIndex < argCount) {
              return {
                timingKey,
                currentIndex,
                totalArgs: argCount,
                isBlinking: this.isTimingBlinking(prevToken),
                isInActiveArg: argInfo.argIndex === currentIndex
              };
            }
          }
        }
      }
    }

    return null; // Not part of any timing expression
  }

  // Get which argument position a token is in relative to a timing token
  getTokenArgPosition(tokens, timingIndex, tokenIndex) {
    let argIndex = -1; // Start at -1, will increment to 0 for first argument
    let parenDepth = 0;
    let currentTokenPos = timingIndex + 1;
    
    // Only debug for tokens we care about
    const isDebugToken = tokens[tokenIndex] === "line" || tokens[tokenIndex] === "box";
    
    if (isDebugToken) {
      console.log(`🔍 getTokenArgPosition: Analyzing "${tokens[tokenIndex]}" (index ${tokenIndex}) relative to timing at ${timingIndex}`);
      console.log(`🔍 Token range: ${timingIndex + 1} to ${tokenIndex}`);
    }
    
    while (currentTokenPos < tokens.length && currentTokenPos <= tokenIndex) {
      const token = tokens[currentTokenPos];
      
      if (token === "(") {
        parenDepth++;
        if (parenDepth === 1) {
          // Start of a new top-level argument
          argIndex++;
          if (isDebugToken) {
            console.log(`🔍 Starting arg ${argIndex} at token "${token}" (pos ${currentTokenPos})`);
          }
        }
      } else if (token === ")") {
        parenDepth--;
        if (isDebugToken) {
          console.log(`🔍 Closing paren at pos ${currentTokenPos}, parenDepth now ${parenDepth}`);
        }
        if (parenDepth < 0) {
          // We've exited the timing expression
          if (isDebugToken) {
            console.log(`🔍 Exited timing expression at pos ${currentTokenPos}`);
          }
          break;
        }
      }
      
      if (currentTokenPos === tokenIndex) {
        if (isDebugToken) {
          console.log(`🔍 Found "${tokens[tokenIndex]}" inside arg ${argIndex} at depth ${parenDepth}`);
        }
        return { argIndex, tokenDepth: parenDepth };
      }
      
      currentTokenPos++;
    }
    
    if (isDebugToken) {
      console.log(`🔍 Token "${tokens[tokenIndex]}" not found in timing arguments`);
    }
    return null; // Token not found in timing arguments
  }

  // Get the arguments count for a timing expression (for key generation)
  getTimingArgsCount(tokens, timingIndex) {
    let argCount = 0;
    let parenDepth = 0;
    
    for (let i = timingIndex + 1; i < tokens.length; i++) {
      const token = tokens[i];
      
      if (token === "(") {
        parenDepth++;
        if (parenDepth === 1) {
          // This is the start of a new top-level argument
          argCount++;
        }
      } else if (token === ")") {
        parenDepth--;
        if (parenDepth < 0) {
          // We've exited the timing expression
          break;
        }
      }
    }
    
    return argCount;
  }

  // Get the arguments for a timing expression
  getTimingArgs(tokens, timingIndex) {
    const args = [];
    let parenDepth = 0;
    let currentArg = [];
    let foundArgs = false;
    
    for (let i = timingIndex + 1; i < tokens.length; i++) {
      const token = tokens[i];
      
      if (token === "(") {
        parenDepth++;
        currentArg.push(token);
        if (!foundArgs) {
          foundArgs = true;
        }
      } else if (token === ")") {
        parenDepth--;
        currentArg.push(token);
        
        if (parenDepth === 0) {
          if (foundArgs && currentArg.length > 0) {
            args.push(currentArg);
            currentArg = [];
          }
          if (foundArgs) break; // End of timing arguments
        }
      } else if (parenDepth === 0 && foundArgs) {
        // We're at the same level as the timing token, end of arguments
        break;
      } else if (parenDepth > 0) {
        currentArg.push(token);
      }
    }
    
    return args;
  }

  // Check if a token is inside an active timing expression argument
  getTimingExpressionState(token, tokens, index) {
    // Debug: log active timing expressions periodically
    // if ((token === "box" || token === "line") && this.frameCount % 60 === 0) {
    //   console.log(`🕐 DEBUG: Active timing expressions:`, Array.from(this.activeTimingExpressions.entries()));
    // }
    
    // Check if we're inside any active timing expression
    for (const [timingKey, timingData] of this.activeTimingExpressions) {
      // For each active timing expression, check if this token is part of it
      const tokenPosition = this.findTokenInTimingArgs(token, index, tokens, timingData);
      
      if (tokenPosition.found) {
        const isInActiveArg = tokenPosition.argIndex === timingData.currentIndex;
        
        // Debug: Log timing state for tokens we care about (only when frame count is divisible by 30 to reduce spam)
        if ((token === "line" || token === "box" || token === "(" || token === ")") && this.frameCount % 30 === 0) {
          // console.log(`🕐 Token "${token}" timing state: argIndex=${tokenPosition.argIndex}, currentIndex=${timingData.currentIndex}, isInActiveArg=${isInActiveArg}`);
        }
        
        return {
          isInActiveArg: isInActiveArg,
          currentArgIndex: tokenPosition.argIndex,
          currentIndex: timingData.currentIndex,
          timingKey,
          timingData
        };
      }
    }
    
    return null; // Not in any timing expression
  }

  findTokenInTimingArgs(token, tokenIndex, tokens, timingData) {
    // Instead of manually parsing tokens, let's use the fact that we already have
    // the parsed AST structure. Find the timing expression in the AST and check
    // which argument contains our token.
    
    // Debug: log what we're looking for (commented out - too frequent)
    // if (token === "box") {
    //   console.log(`🔍🔍 findTokenInTimingArgs for "${token}" at index ${tokenIndex}`);
    //   console.log(`🔍🔍 Looking for timingData.timingToken:`, timingData.timingToken);
    //   console.log(`🔍🔍 Available tokens:`, tokens);
    // }
    
    // Find the timing expression AST node that corresponds to this timing token
    const timingExpressions = this.findTimingExpressionsInAST(this.ast, timingData.timingToken);
    
    // if (token === "box") {
    //   console.log(`🔍🔍 Found timing expressions in AST:`, timingExpressions);
    // }
    
    // For each timing expression, check if our token is part of its arguments
    for (const timingExpr of timingExpressions) {
      // First check if the token is directly inside an argument
      const argIndex = this.findTokenInTimingExpressionArgs(token, tokenIndex, tokens, timingExpr);
      if (argIndex !== -1) {
        // if (token === "box") {
        //   console.log(`🔍🔍 TOKEN FOUND in arg ${argIndex}!`);
        // }
        return { found: true, argIndex };
      }
      
      // For parentheses, we need to check if they bound a timing argument s-expression
      if (token === "(" || token === ")") {
        const parenArgIndex = this.findParenthesesInTimingArgs(tokenIndex, tokens, timingExpr);
        if (parenArgIndex !== -1) {
          return { found: true, argIndex: parenArgIndex };
        }
      }
    }
    
    return { found: false };
  }

  // Find timing expressions in the AST that match the given timing token
  findTimingExpressionsInAST(ast, timingToken) {
    const results = [];
    
    if (Array.isArray(ast)) {
      // Check if this array is a timing expression
      if (ast.length > 1 && ast[0] === timingToken) {
        results.push(ast);
      }
      
      // Recursively search sub-expressions
      for (const item of ast) {
        results.push(...this.findTimingExpressionsInAST(item, timingToken));
      }
    }
    
    return results;
  }

  // Find which timing argument a parenthesis belongs to by analyzing token positions
  findParenthesesInTimingArgs(parenIndex, tokens, timingExpr) {
    // Find the position range of the timing expression in the token stream
    const timingToken = timingExpr[0]; // e.g., "2s..."
    const timingStartIndex = tokens.indexOf(timingToken);
    
    if (timingStartIndex === -1) return -1;
    
    // Find the matching closing parenthesis for the timing expression
    let parenCount = 0;
    let timingEndIndex = -1;
    for (let i = timingStartIndex - 1; i < tokens.length; i++) { // Start before timing token to catch opening paren
      if (tokens[i] === "(") {
        parenCount++;
        if (parenCount === 1 && i < timingStartIndex) {
          // This is the opening paren of the timing expression
          continue;
        }
      } else if (tokens[i] === ")") {
        parenCount--;
        if (parenCount === 0) {
          timingEndIndex = i;
          break;
        }
      }
    }
    
    if (timingEndIndex === -1) return -1;
    
    // Check if our parenthesis is within the timing expression range
    if (parenIndex <= timingStartIndex || parenIndex >= timingEndIndex) {
      return -1; // Parenthesis is outside the timing expression
    }
    
    // Now we need to figure out which argument this parenthesis belongs to
    // by analyzing the structure between timingStartIndex and timingEndIndex
    const args = timingExpr.slice(1); // Skip the timing token
    let currentArgIndex = 0;
    let currentTokenPos = timingStartIndex + 1; // Start after timing token
    
    for (let argIndex = 0; argIndex < args.length; argIndex++) {
      const arg = args[argIndex];
      
      // Calculate how many tokens this argument should consume
      const argTokenCount = this.countTokensInExpression(arg);
      const argEndPos = currentTokenPos + argTokenCount - 1;
      
      // Check if our parenthesis falls within this argument's range
      if (parenIndex >= currentTokenPos && parenIndex <= argEndPos) {
        return argIndex;
      }
      
      currentTokenPos = argEndPos + 1;
    }
    
    return -1; // Parenthesis not found in any argument
  }

  // Count how many tokens an AST expression would consume in the token stream
  countTokensInExpression(expr) {
    if (typeof expr === "string" || typeof expr === "number") {
      return 1;
    }
    if (Array.isArray(expr)) {
      // Array expressions have opening paren + contents + closing paren
      let count = 2; // ( and )
      for (const item of expr) {
        count += this.countTokensInExpression(item);
      }
      return count;
    }
    return 1;
  }

  // Find which argument of a timing expression contains a specific token
  findTokenInTimingExpressionArgs(token, tokenIndex, tokens, timingExpr) {
    // timingExpr is like ["2s...", [["line"], ["line"]], ["box", 0, 0, 25, 25]]
    // We need to find which argument (index 1, 2, etc.) contains our token
    
    // Debug: log the structure for tokens we care about
    // if ((token === "line" || token === "box") && this.frameCount % 60 === 0) {
      // console.log(`🔍 findTokenInTimingExpressionArgs for "${token}":`, JSON.stringify(timingExpr, null, 2));
    // }
    
    const args = timingExpr.slice(1); // Skip the timing token itself
    
    for (let argIndex = 0; argIndex < args.length; argIndex++) {
      if (this.doesExpressionContainToken(args[argIndex], token)) {
        if ((token === "line" || token === "box") && this.frameCount % 60 === 0) {
          // console.log(`🔍 Token "${token}" found in arg ${argIndex}:`, JSON.stringify(args[argIndex], null, 2));
        }
        return argIndex;
      }
    }
    
    return -1; // Token not found in any argument
  }

  // Check if an AST expression contains a specific token
  doesExpressionContainToken(expr, token) {
    if (typeof expr === "string") {
      return expr === token;
    }
    if (typeof expr === "number") {
      // Handle numeric tokens - convert both to string for comparison
      return expr.toString() === token;
    }
    if (Array.isArray(expr)) {
      return expr.some(item => this.doesExpressionContainToken(item, token));
    }
    return false;
  }

  // Determine the color for a specific token based on its type and context
  getTokenColor(token, tokens, index) {
    // First check if this token is affected by timing expressions
    const timingExprState = this.getTimingExpressionState(token, tokens, index);
    
    // Check for timing patterns that should blink when triggered
    if (/^\d*\.?\d+s\.\.\.?$/.test(token)) {
      const timingState = this.getTimingTokenState(token, tokens, index);
      if (timingState && timingState.isBlinking) {
        return "red"; // Bright red flash when timing triggers
      }
      return "yellow"; // Yellow for timing tokens like "1s...", "2s..."
    }
    
    // If this token is inside a timing expression, color it based on active state
    if (timingExprState) {
      if (timingExprState.isInActiveArg) {
        // Check if the timing token for this expression is currently blinking
        const isTimingBlinking = this.isTimingBlinking(timingExprState.timingData.timingToken);
        if (isTimingBlinking) {
          // Make the entire active timing expression blink red when timing triggers
          return "red";
        }
        // This token is in the currently active timing argument - use olive color
        return "olive";
      } else {
        // This token is in an inactive timing argument - use transparent
        return "255,255,255,0"; // White with 0 alpha (transparent)
      }
    }
    
    // For all other tokens, use normal coloring
    return this.getNormalTokenColor(token, tokens, index);
  }

  // Get the normal color for a token (the original getTokenColor logic)
  getNormalTokenColor(token, tokens, index) {
    // Check for comments first
    if (token.startsWith(";")) {
      return "gray";
    }

    // Check for strings (quoted text)
    if (token.startsWith('"') && token.endsWith('"')) {
      return "yellow";
    }

    // Check for all numbers (integers, floats, positive, negative) BEFORE timing patterns
    // This ensures consistent coloring for all literal numeric values
    if (/^-?\d+(\.\d+)?$/.test(token)) {
      return "green";
    }

    // Check for timing patterns (but exclude pure numbers which were handled above)
    if (/^\d*\.?\d+s!?$/.test(token)) {
      // Check if this timing token is currently blinking
      if (this.isTimingBlinking(token)) {
        return "red"; // Bright red flash when timing triggers
      }
      return "yellow"; // Yellow for second-based timing like "1s", "0.5s", "0.1s"
    }

    // Check for parentheses - use rainbow nesting colors
    if (token === "(" || token === ")") {
      return this.getParenthesesColor(tokens, index);
    }

    // Check if this token is a function name (first token after opening paren)
    if (index > 0 && tokens[index - 1] === "(") {
      return this.getFunctionColor(token);
    }

    // Check if this token is a valid CSS color name
    if (cssColors && cssColors[token]) {
      // Return the actual color value for CSS colors like "red", "blue", etc.
      const colorValue = cssColors[token];
      if (Array.isArray(colorValue) && colorValue.length >= 3) {
        const rgbColor = `${colorValue[0]},${colorValue[1]},${colorValue[2]}`;
        // Return as RGB format for the HUD system
        return rgbColor;
      }
    }

    // Check if this is a bare function call (likely at start of line or after newline)
    // This handles KidLisp's auto-wrapping of non-parenthetical expressions
    if (this.isBareFunction(token, tokens, index)) {
      return this.getFunctionColor(token);
    }

    // Check if this is a known function name (even if not directly after parentheses)
    const knownFunctions = [
      "wipe", "ink", "line", "box", "circle", "write", "paste", "stamp", "point", "poly",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "?", "...", "..", "overtone", "rainbow", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll", 
      "spin", "resetSpin", "smoothspin", "sort", "zoom", "blur", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes"
    ];
    
    // Math operators get special green color
    if (["+", "-", "*", "/", "%", "mod", "=", ">", "<", ">=", "<=", "abs", "sqrt", "min", "max"].includes(token)) {
      return "lime";
    }
    
    if (knownFunctions.includes(token)) {
      return "cyan"; // Changed from blue to cyan (teal)
    }

    // Default for variables and other identifiers
    return "orange";
  }

  // Helper method to determine if a token is a bare function call
  isBareFunction(token, tokens, index) {
    // Don't treat tokens that are clearly arguments as bare functions
    if (index > 0) {
      const prevToken = tokens[index - 1];
      // If previous token is an opening paren, this is already handled as a function
      if (prevToken === "(") {
        return false;
      }
      // If previous token is not a closing paren or start of expression, this is likely an argument
      if (prevToken !== ")") {
        return false;
      }
    }

    // Check if this looks like a function name
    if (!/^[a-zA-Z_]\w*$/.test(token)) {
      return false;
    }

    // Check against known functions and CSS colors
    const knownFunctions = [
      "wipe", "ink", "line", "box", "circle", "write", "paste", "stamp", "point", "poly",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "?", "...", "..", "overtone", "rainbow", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll", 
      "spin", "resetSpin", "smoothspin", "sort", "zoom", "blur", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes"
    ];

    return knownFunctions.includes(token) || (cssColors && cssColors[token]);
  }

  // Helper method to get the appropriate color for a function
  getFunctionColor(token) {
    // Math operators get special green color
    if (["+", "-", "*", "/", "%", "mod", "=", ">", "<", ">=", "<=", "abs", "sqrt", "min", "max"].includes(token)) {
      return "lime";
    }
    
    // Special forms and control flow
    if (["def", "if", "cond", "later", "once", "lambda", "let", "do"].includes(token)) {
      return "purple";
    }
    
    // Repeat gets its own lighter color for better readability
    if (token === "repeat") {
      return "magenta"; // Lighter than purple, more readable
    }
     
    // All other functions should be teal instead of blue
    return "cyan";
  }

  // Get color for parentheses based on nesting depth (rainbow pattern)
  getParenthesesColor(tokens, index) {
    // Calculate nesting depth at this position
    let depth = 0;
    for (let i = 0; i < index; i++) {
      if (tokens[i] === "(") {
        depth++;
      } else if (tokens[i] === ")") {
        depth--;
      }
    }
    
    // Adjust depth for closing parentheses
    if (tokens[index] === ")") {
      depth--;
    }
    
    // Rainbow colors for different nesting depths
    const parenColors = [
      "192,192,192", // Light gray (depth 0)
      "255,215,0",   // Gold (depth 1) 
      "255,165,0",   // Orange (depth 2)
      "255,105,180", // Hot pink (depth 3)
      "138,43,226",  // Blue violet (depth 4)
      "0,191,255",   // Deep sky blue (depth 5)
      "50,205,50"    // Lime green (depth 6)
    ];
    
    // Cycle through colors for deeper nesting
    const colorIndex = depth % parenColors.length;
    return parenColors[colorIndex];
  }

  // Check if an expression matches the currently executing one
  isExpressionMatch(posText, executingExpr) {
    if (typeof executingExpr === "string") {
      return posText.includes(executingExpr);
    } else if (Array.isArray(executingExpr)) {
      const exprStr = JSON.stringify(executingExpr);
      return posText.includes(executingExpr[0]); // Match function name
    }
    return false;
  }

  // Determine the type of expression for syntax coloring
  getExpressionType(exprText) {
    const trimmed = exprText.trim();

    if (trimmed.startsWith("(")) {
      // Extract function name
      const match = trimmed.match(/^\(\s*([^\s)]+)/);
      if (match) {
        const funcName = match[1];

        // Special forms and control flow
        if (
          ["def", "if", "cond", "repeat", "later", "once", "lambda", "let", "do"].includes(funcName)
        ) {
          return "special";
        }

        // Drawing and graphics functions
        if (
          ["ink", "wipe", "line", "box", "circle", "write", "paste", "stamp", "point", "poly"].includes(
            funcName,
          )
        ) {
          return "function";
        }

        // Math and comparison functions
        if (["+", "-", "*", "/", "=", ">", "<", ">=", "<=", "mod", "abs", "sqrt"].includes(funcName)) {
          return "function";
        }

        // Other built-in functions
        if (["print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round"].includes(funcName)) {
          return "function";
        }

        return "function";
      }
    } else if (/^-?\d+(\.\d+)?$/.test(trimmed)) {
      return "number";
    } else if (trimmed.startsWith('"') && trimmed.endsWith('"')) {
      return "string";
    } else if (trimmed.startsWith(";")) {
      return "comment";
    } else {
      return "variable";
    }

    return "unknown";
  }

  // Update HUD with syntax highlighted kidlisp
  async updateHUDWithSyntaxHighlighting(api) {
    // console.log("🔥 updateHUDWithSyntaxHighlighting called!", {
    //   hasHud: !!api.hud,
    //   hasLabel: !!api.hud?.label,
    //   hasSyntaxHighlightSource: !!this.syntaxHighlightSource
    // });
    if (!api.hud || !api.hud.label || !this.syntaxHighlightSource) return;

    const coloredString = this.buildColoredKidlispString();
    if (coloredString) {
      // Check if we have cached KidLisp owner info for attribution
      let attributionText = "";
      if (api.cachedKidlispOwner) {
        // Use cached handle if we already fetched it for this sub
        if (this.cachedOwnerSub === api.cachedKidlispOwner && this.cachedOwnerHandle) {
          attributionText = ` \\gray\\by @${this.cachedOwnerHandle}`;
        } else {
          // Fetch handle for new sub (async, will show on next frame)
          this.getHandleFromSub(api.cachedKidlispOwner);
        }
      }

      // Only enable syntax highlighting for inline kidlisp pieces from source,
      // not for .lisp files which should show plain piece names
      if (!this.isLispFile) {
        // Set HUD to support inline colors AND disable piece name color override
        api.hud.supportsInlineColor = true;
        api.hud.disablePieceNameColoring = true; // Disable automatic piece name coloring
        
        // Always force cache invalidation for syntax highlighting by updating frame hash
        // This ensures that color changes in KidLisp syntax highlighting are detected
        api.hud.frameHash = performance.now();
        
        // Call HUD label with colored string plus attribution if available
        api.hud.label(coloredString + attributionText, undefined, 0);
      } else {
        // For .lisp files, use default HUD behavior without syntax highlighting
        // This will show the plain piece name with normal coloring
        api.hud.supportsInlineColor = false;
        api.hud.disablePieceNameColoring = false;
        
        // Don't call hud.label() for .lisp files - let the normal HUD system handle it
      }
    }
  }

  // Fetch handle from user sub using the /handle API endpoint (with caching)
  async getHandleFromSub(userSub) {
    // Don't re-fetch if we already have this handle cached
    if (this.cachedOwnerSub === userSub && this.cachedOwnerHandle) {
      return this.cachedOwnerHandle;
    }

    try {
      const response = await fetch(`/handle?for=${userSub}`);
      if (response.status === 200) {
        const data = await response.json();
        this.cachedOwnerHandle = data.handle;
        this.cachedOwnerSub = userSub;
        return data.handle;
      }
    } catch (error) {
      console.warn("Error fetching handle:", error);
    }
    
    // Clear cache if fetch failed
    this.cachedOwnerHandle = null;
    this.cachedOwnerSub = null;
    return null;
  }

  // Helper method to strip color codes (copied from clock.mjs pattern)
  stripColorCodes(str) {
    return str.replace(/\\[a-zA-Z0-9]+\\/g, "");
  }
}

// Singleton KidLisp instance to preserve state across inline and .lisp file executions
let globalKidLispInstance = null;

// Module function that reuses a singleton KidLisp instance to preserve `once` state
function module(source, isLispFile = false) {
  if (!globalKidLispInstance) {
    globalKidLispInstance = new KidLisp();
  }
  return globalKidLispInstance.module(source, isLispFile);
}

// Standalone parse function (for compatibility)
function parse(program) {
  const lisp = new KidLisp();
  return lisp.parse(program);
}

// Standalone evaluate function (for compatibility with tests, and an empty api)
function evaluate(parsed, api = {}) {
  const lisp = new KidLisp();
  return lisp.evaluate(parsed, api);
}

// URL encoding/decoding utilities for kidlisp pieces
function isKidlispSource(text) {
  if (!text) return false;

  // Only consider input as KidLisp if:
  // 1. It starts with '(' (parenthesis), OR
  // 2. It contains a newline
  // This prevents false positives on commands like "clock (ceg) (dfa)"
  // where parentheses appear later in the string but it's not KidLisp code

  // Check if it starts with opening parenthesis (clear KidLisp indicator)
  if (text.startsWith("(")) {
    return true;
  }

  // Check if it contains newlines (multi-line input is likely KidLisp)
  if (text.includes("\n")) {
    return true;
  }

  // Check for encoded kidlisp that might have newlines encoded as §
  if (text.includes("§")) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n");
    // If decoded version starts with ( or contains newlines, it's KidLisp
    if (decoded.startsWith("(") || decoded.includes("\n")) {
      return true;
    }
  }

  // Check for encoded kidlisp that might have newlines encoded as _
  // Only check this if it looks like URL-encoded content (multiple underscores in sequence
  // or typical KidLisp function patterns), not just any underscore usage
  if (
    text.includes("_") &&
    (text.includes("__") || // Multiple consecutive underscores (likely encoded spaces)
      text.match(/\b(wipe|ink|line|box|def|later)_/)) // Known KidLisp functions with underscore
  ) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n");
    // If decoded version starts with ( or contains newlines, it's KidLisp
    if (decoded.startsWith("(") || decoded.includes("\n")) {
      return true;
    }
  }

  // NOTE: We do NOT treat ~ alone as a kidlisp indicator because ~ is used
  // as a parameter separator in regular piece URLs like "line~red"
  // The ~ to newline conversion only happens during decoding if there are
  // other strong indicators that the text is actually kidlisp

  // For all other cases (single line input that doesn't start with '('),
  // don't consider it KidLisp to avoid false positives
  return false;
}

function encodeKidlispForUrl(source) {
  const isKidlisp = isKidlispSource(source);

  if (!isKidlisp) {
    return source;
  }

  // For sharing, we want to preserve the structure so it can be parsed correctly
  // Use Unicode characters that display nicely in browser URL bars
  // Modern browsers handle these characters fine without percent-encoding
  const encoded = source
    .replace(/ /g, "_") // Space to underscore (already safe)
    .replace(/\n/g, "§") // Use § for newlines - displays fine in browsers
    .replace(/%/g, "¤") // Use ¤ for percent - displays fine in browsers
    .replace(/;/g, "¨"); // Use ¨ for semicolon - displays fine in browsers

  return encoded;
}

function decodeKidlispFromUrl(encoded) {
  // Standard decoding for content
  let decoded = encoded
    .replace(/_/g, " ")
    .replace(/%C2%A7/g, "\n") // UTF-8 encoded § to newline
    .replace(/%C2%A4/g, "%") // UTF-8 encoded ¤ to percent
    .replace(/%C2%A8/g, ";") // UTF-8 encoded ¨ to semicolon
    .replace(/§/g, "\n") // Direct § to newline (fallback)
    .replace(/¤/g, "%") // Direct ¤ to percent (fallback)
    .replace(/¨/g, ";") // Direct ¨ to semicolon (fallback)
    // Standard URL decoding
    .replace(/%28/g, "(")
    .replace(/%29/g, ")")
    .replace(/%2E/g, ".")
    .replace(/%22/g, '"')
    .replace(/%3B/g, ";");

  // Only convert ~ to newlines for KidLisp code, not for commands like "prompt~"
  if (!encoded.startsWith("prompt~") && isKidlispSource(decoded)) {
    decoded = decoded.replace(/~/g, "\n");
  }

  return decoded;
}

// Check if the prompt is currently in kidlisp mode based on the input text
function isPromptInKidlispMode(promptText) {
  if (!promptText || typeof promptText !== "string") return false;
  // Use the original text (not trimmed) for proper newline detection
  return isKidlispSource(promptText);
}

// Add result logging to see if detection is working
function logKidlispDetection(source) {
  const result =
    source.includes("\n") || source.includes("wipe") || source.includes("line");
  console.log(
    "🔍 MANUAL DETECTION for:",
    JSON.stringify(source),
    "result:",
    result,
  );
  return result;
}

// Utility function to check if a string matches the nanoid pattern used by store-kidlisp
// Uses customAlphabet('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', 4-12)
// Starts at 4 characters, grows incrementally to 12 as needed
// Must contain at least one digit AND one letter to distinguish from disk names
function isNanoidPattern(str) {
  if (!str || typeof str !== "string") return false;
  // Check for nanoid lengths from 4 to 12 characters (incremental growth)
  if (str.length < 4 || str.length > 12) return false;
  // Must contain only alphanumeric characters
  if (!/^[0-9A-Za-z]+$/.test(str)) return false;
  // Must contain at least one digit AND one letter to distinguish from disk names like "prompt"
  const hasDigit = /[0-9]/.test(str);
  const hasLetter = /[A-Za-z]/.test(str);
  return hasDigit && hasLetter;
}

// Function to fetch cached KidLisp code from nanoid
async function fetchCachedCode(nanoidCode) {
  try {
    const response = await fetch(`/api/store-kidlisp?code=${nanoidCode}`);
    if (response.ok) {
      const data = await response.json();
      return data.source;
    } else {
      console.warn(`❌ Failed to load cached code: ${nanoidCode}`);
      return null;
    }
  } catch (error) {
    console.error(`❌ Error loading cached code: ${nanoidCode}`, error);
    return null;
  }
}

// Export function to get syntax highlighting colors for progress bars
function getSyntaxHighlightingColors(source) {
  if (!source) {
    return [{ type: "default", r: 220, g: 60, b: 60, weight: 1.0 }];
  }
  
  // Use existing tokenizer
  const tokens = tokenize(source);
  if (tokens.length === 0) {
    return [{ type: "default", r: 220, g: 60, b: 60, weight: 1.0 }];
  }
  
  // Create a temporary KidLisp instance to access color methods
  const tempInstance = new KidLisp();
  
  // Map tokens to colors using existing syntax highlighting system
  const colors = tokens.map(token => {
    const weight = Math.max(0.01, token.length / source.length);
    const colorStr = tempInstance.getTokenColor(token, tokens, tokens.indexOf(token));
    
    // Convert color string to RGB values
    let r, g, b;
    
    // Handle RGB format colors (like "192,192,192")
    if (colorStr.includes(',')) {
      const parts = colorStr.split(',').map(p => parseInt(p.trim()));
      r = parts[0] || 220;
      g = parts[1] || 60;
      b = parts[2] || 60;
    } else {
      // Handle named colors
      switch (colorStr) {
        case "cyan":
        case "teal":
          r = 64; g = 224; b = 208;
          break;
        case "lime":
          r = 50; g = 205; b = 50;
          break;
        case "green":
          r = 34; g = 139; b = 34;
          break;
        case "yellow":
          r = 255; g = 255; b = 0;
          break;
        case "orange":
          r = 255; g = 165; b = 0;
          break;
        case "purple":
          r = 128; g = 0; b = 128;
          break;
        case "magenta":
          r = 255; g = 0; b = 255;
          break;
        case "red":
          r = 255; g = 0; b = 0;
          break;
        case "gray":
        case "grey":
          r = 128; g = 128; b = 128;
          break;
        case "white":
          r = 255; g = 255; b = 255;
          break;
        default:
          r = 220; g = 80; b = 80; // Fallback red
      }
    }
    
    // Classify token type for semantic information
    let type = "default";
    if (token === "(" || token === ")") {
      type = "parenthesis";
    } else if (/^-?\d+(\.\d+)?$/.test(token)) {
      type = "number";
    } else if (token.startsWith('"') && token.endsWith('"')) {
      type = "string";
    } else if (token.startsWith(";")) {
      type = "comment";
    } else if (["+", "-", "*", "/", "%", "mod", "=", ">", "<", ">=", "<=", "abs", "sqrt", "min", "max"].includes(token)) {
      type = "operator";
    } else if (["def", "if", "cond", "later", "once", "lambda", "let", "do", "repeat"].includes(token)) {
      type = "special";
    } else {
      type = "function";
    }
    
    return { type, r, g, b, weight, token };
  });
  
  return colors;
}

export {
  module,
  parse,
  evaluate,
  KidLisp,
  isKidlispSource,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
  isPromptInKidlispMode,
  fetchCachedCode,
  getCachedCode,
  setCachedCode,
  getSyntaxHighlightingColors,
};
