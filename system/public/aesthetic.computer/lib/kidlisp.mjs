// Kidlisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.

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
 - [] Closing and opening the VS Code extension should remember the last piece
      loaded.
   - [] This should work by broadcasting the piece slug to the extension
        on each switch then using that for the page open url, but not storing
        it across vscode opened and closed
        sessions.
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

    // Microphone state tracking
    this.microphoneConnected = false;
    this.microphoneApi = null;

    // Performance optimizations
    this.functionCache = new Map(); // Cache function lookups
    this.globalEnvCache = null; // Cache global environment
    this.fastPathFunctions = new Set(['line', 'ink', 'wipe', 'box', 'repeat', '+', '-', '*', '/', '=', '>', '<', 'mic', 'paste', 'stamp']); // Common functions for fast path
    this.expressionCache = new Map(); // Cache for simple expressions
    this.variableCache = new Map(); // Cache for variable lookups
    this.mathCache = new Map(); // Cache for math expressions
    this.sequenceCounters = new Map(); // Track sequence positions for ... function
    
    // Melody state tracking
    this.melodies = new Map(); // Track active melodies
    this.melodyTimers = new Map(); // Track timing for each melody
    
    // Resolution state tracking
    this.halfResolutionApplied = false;
    this.thirdResolutionApplied = false;
    this.fourthResolutionApplied = false;
    
    // Resolution state tracking
    this.halfResolutionApplied = false; // Track if half resolution has been applied
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
        const tokens = innerExpr.split(/\s+/).map(token => {
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
            expanded.push([op, this.expandFastMathMacros(current), parseFloat(num)]);
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

  // Parse and evaluate a lisp source module
  // into a running aesthetic computer piece.
  module(source) {
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

    /*if (VERBOSE)*/ // console.log("🐍 Snake:", parsed);

    // 🧩 Piece API
    return {
      boot: ({ wipe, params, clock, screen, sound, delay, pieceCount }) => {
        // Resync clock for accurate timing (like clock.mjs does)
        clock?.resync?.();

        this.globalDef.paramA = params[0];
        this.globalDef.paramB = params[1];
        this.globalDef.paramC = params[2];

        // Initialize microphone like stample does
        if (sound?.microphone) {
          this.microphoneApi = sound.microphone;
          // console.log(
          //   "🎤 Boot: Microphone available, permission:",
          //   this.microphoneApi.permission,
          //   "enabled:",
          //   sound.enabled?.(),
          // );

          // Check permission and auto-connect if granted (like stample)
          if (
            this.microphoneApi.permission === "granted" &&
            sound.enabled?.()
          ) {
            // console.log("🎤 Boot: Attempting to connect microphone...");
            delay(() => {
              this.microphoneApi.connect();
            }, 15);
          }
        } else {
          // console.log("🎤 Boot: No microphone API available");
        }

        // Just set up initial state, don't execute program here
        // console.log(pieceCount);
        wipe("erase");
      },
      paint: ($) => {
        // console.log("🖌️ Kid Lisp is Painting...", $.paintCount);
        this.frameCount++; // Increment frame counter for timing functions

        // Clear caches that might have stale data between frames
        this.variableCache.clear();
        this.expressionCache.clear();

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
        const isContinuation = index > 0 && (
          // Line starts with an identifier but not at the beginning of the input
          (/^[a-zA-Z_]\w*/.test(line) && !line.startsWith("(")) &&
          (
            // Previous line ends with an incomplete expression (has opening paren or is incomplete)
            lines[index - 1].includes("(") && 
            (lines[index - 1].split("(").length > lines[index - 1].split(")").length)
          )
        );
        
        // If line doesn't start with ( and looks like a function call, wrap it
        // BUT don't wrap if it's likely a continuation line
        if (!line.startsWith("(") && /^[a-zA-Z_]\w*/.test(line) && !isContinuation) {
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
    const notes = [];
    let i = 0;
    
    while (i < melodyString.length) {
      let char = melodyString[i];
      
      // Handle octave-first notation (4c, 5g#, etc)
      if (/[0-9]/.test(char)) {
        const octave = parseInt(char);
        i++;
        
        if (i < melodyString.length) {
          const noteChar = melodyString[i].toLowerCase();
          if (/[a-g]/.test(noteChar)) {
            let note = noteChar;
            i++;
            
            // Check for sharp or flat modifiers
            if (i < melodyString.length) {
              const nextChar = melodyString[i];
              if (nextChar === 's' || nextChar === '#') {
                note += '#'; // Convert both 's' and '#' to # internally
                i++;
              } else if (nextChar === 'b') {
                note += 'b';
                i++;
              }
            }
            
            // Default duration is 1 (quarter note)
            let duration = 1;
            
            // Check for duration modifiers using dots or dashes (optional)
            if (i < melodyString.length) {
              const nextChar = melodyString[i];
              // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = thirty-second, etc.
              if (nextChar === '.') {
                let dots = 0;
                while (i < melodyString.length && melodyString[i] === '.') {
                  dots++;
                  i++;
                }
                // no dots = quarter (1.0), . = eighth (0.5), .. = sixteenth (0.25), ... = thirty-second (0.125), etc.
                duration = 1.0 / Math.pow(2, dots);
              }
              // Use dashes for longer notes: - = half, -- = whole, --- = double whole, etc.
              else if (nextChar === '-') {
                let dashes = 0;
                while (i < melodyString.length && melodyString[i] === '-') {
                  dashes++;
                  i++;
                }
                // - = half (2.0), -- = whole (4.0), --- = double whole (8.0), etc.
                duration = Math.pow(2, dashes);
              }
            }
            
            notes.push({ note, octave, duration });
          }
        }
      }
      // Handle note-first notation (c4, d#5, etc) 
      else if (/[a-g]/.test(char.toLowerCase())) {
        let note = char.toLowerCase();
        i++;
        
        // Check for sharp or flat modifiers
        if (i < melodyString.length) {
          const nextChar = melodyString[i];
          if (nextChar === 's' || nextChar === '#') {
            note += '#'; // Convert both 's' and '#' to # internally
            i++;
          } else if (nextChar === 'b') {
            note += 'b';
            i++;
          }
        }
        
        // Check for octave number
        let octave = 4; // Default octave
        if (i < melodyString.length) {
          const nextChar = melodyString[i];
          if (/[0-9]/.test(nextChar)) {
            octave = parseInt(nextChar);
            i++;
          }
        }
        
        // Default duration is 1 (quarter note)
        let duration = 1;
        
        // Check for duration modifiers using dots or dashes (optional)
        if (i < melodyString.length) {
          const nextChar = melodyString[i];
          // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = thirty-second, etc.
          if (nextChar === '.') {
            let dots = 0;
            while (i < melodyString.length && melodyString[i] === '.') {
              dots++;
              i++;
            }
            // no dots = quarter (1.0), . = eighth (0.5), .. = sixteenth (0.25), ... = thirty-second (0.125), etc.
            duration = 1.0 / Math.pow(2, dots);
          }
          // Use dashes for longer notes: - = half, -- = whole, --- = double whole, etc.
          else if (nextChar === '-') {
            let dashes = 0;
            while (i < melodyString.length && melodyString[i] === '-') {
              dashes++;
              i++;
            }
            // - = half (2.0), -- = whole (4.0), --- = double whole (8.0), etc.
            duration = Math.pow(2, dashes);
          }
        }
        
        notes.push({ note, octave, duration });
      }
      // Handle rests
      else if (char === '_' || char === ' ' || char === '-') {
        let duration = 1;
        i++;
        
        // Check for duration modifiers using dots or dashes on rests
        if (i < melodyString.length) {
          const nextChar = melodyString[i];
          // Use dots for divisions: no dots = quarter, . = eighth, .. = sixteenth, ... = thirty-second, etc.
          if (nextChar === '.') {
            let dots = 0;
            while (i < melodyString.length && melodyString[i] === '.') {
              dots++;
              i++;
            }
            // no dots = quarter (1.0), . = eighth (0.5), .. = sixteenth (0.25), ... = thirty-second (0.125), etc.
            duration = 1.0 / Math.pow(2, dots);
          }
          // Use dashes for longer rests: - = half, -- = whole, --- = double whole, etc.
          else if (nextChar === '-') {
            let dashes = 0;
            while (i < melodyString.length && melodyString[i] === '-') {
              dashes++;
              i++;
            }
            // - = half (2.0), -- = whole (4.0), --- = double whole (8.0), etc.
            duration = Math.pow(2, dashes);
          }
        }
        
        notes.push({ note: 'rest', octave: null, duration });
      }
      // Handle measure separators (optional, for readability)
      else if (char === '|') {
        // Measure bars are just visual separators, skip them
        i++;
      } else {
        i++;
      }
    }
    
    return notes;
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
      const beatInMeasure = melodyState.measurePosition % melodyState.beatsPerMeasure;
      if (beatInMeasure === 0) {
        // Beat 1: slightly longer (emphasized)
        noteDuration *= 1.05;
      } else {
        // Beats 2 and 3: slightly shorter (lighter)
        noteDuration *= 0.98;
      }
    } else if (melodyState.feel === "swing") {
      // Classic jazz swing: off-beats are delayed and shortened
      const beatInMeasure = melodyState.measurePosition % melodyState.beatsPerMeasure;
      if (beatInMeasure % 1 !== 0) { // Off-beat
        noteDuration *= 0.9; // Shorter off-beats
      }
    }
    
    if (note !== 'rest' && api.sound?.synth) {
      // Convert note to tone format expected by the synth
      const tone = this.noteToTone(note, octave);
      
      api.sound.synth({
        type: "sine",
        tone: tone,
        duration: (noteDuration / 1000) * 0.98, // 98% of the duration for smoother flow
        attack: 0.01,
        decay: 0.99,
        volume: 0.8  // Increased volume to make it more audible
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
    // Handle both simple notes and notes with octave/modifiers
    const noteMap = {
      'c': 'C',
      'c#': 'C#',
      'db': 'C#',
      'd': 'D',
      'd#': 'D#',
      'eb': 'D#',
      'e': 'E',
      'f': 'F',
      'f#': 'F#',
      'gb': 'F#',
      'g': 'G',
      'g#': 'G#',
      'ab': 'G#',
      'a': 'A',
      'a#': 'A#',
      'bb': 'A#',
      'b': 'B'
    };
    
    const normalizedNote = note.toLowerCase();
    const baseNote = noteMap[normalizedNote] || 'C';
    
    // Use specified octave, or default to 4
    const finalOctave = octave !== null ? octave : 4;
    
    return `${finalOctave}${baseNote}`;
  }

  // CORE OPTIMIZATION HELPER: Check if expression contains a variable
  containsVariable(expr, varName) {
    if (typeof expr === 'string') {
      return expr === varName;
    }
    if (typeof expr === 'number') {
      return false;
    }
    if (Array.isArray(expr)) {
      return expr.some(subExpr => this.containsVariable(subExpr, varName));
    }
    return false;
  }

  // Create global environment (cached for performance)
  getGlobalEnv() {
    // Force cache refresh for debugging - remove this line later
    this.globalEnvCache = null;
    
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
            
            console.log(`🔄 ${fraction} resolution applied: ${currentWidth}x${currentHeight} → ${newWidth}x${newHeight}`);
          } else {
            console.log(`🔄 ${fraction} resolution already applied, skipping to prevent squashing`);
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
        api.ink?.(processArgStringTypes(args));
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
          if (fillMode === "outline" || fillMode === "unfilled" || fillMode === "false") {
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
        const processedArgs = args.map(arg => 
          typeof arg === 'string' && arg.startsWith('"') && arg.endsWith('"') 
            ? arg.slice(1, -1) 
            : arg
        );
        api.paste(...processedArgs);
      },
      stamp: (api, args = []) => {
        // Process string arguments to remove quotes (e.g., "@handle/timestamp")  
        const processedArgs = args.map(arg => 
          typeof arg === 'string' && arg.startsWith('"') && arg.endsWith('"') 
            ? arg.slice(1, -1) 
            : arg
        );
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
        const bg = args[3] !== undefined ? processArgStringTypes(args[3]) : undefined;
        
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
          pos.center = "xy";  // Center both x and y
        } else if (centerX) {
          pos.center = "x";   // Center only x
          pos.y = y;
        } else if (centerY) {
          pos.center = "y";   // Center only y
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
      height: (api) => {
        return api.screen.height;
      },
      frame: (api) => {
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
          if (expressions.length === 2 && 
              Array.isArray(expressions[0]) && expressions[0][0] === "ink" &&
              Array.isArray(expressions[1]) && expressions[1][0] === "box") {
            
            perfStart("fast-draw-loop");
            // Optimized fast path for ink+box patterns
            const inkExpr = expressions[0];
            const boxExpr = expressions[1];
            
            // OPTIMIZATION 1: Pre-resolve color palette to avoid repeated parsing
            let paletteColors = null;
            let sequenceKey = null;
            let resolvedColors = null;
            
            if (inkExpr.length === 2 && Array.isArray(inkExpr[1]) && inkExpr[1][0] === "...") {
              paletteColors = inkExpr[1].slice(1); // Extract palette colors
              sequenceKey = JSON.stringify(paletteColors);
              
              // Pre-resolve colors to RGB values
              resolvedColors = paletteColors.map(colorName => {
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
            const analyzedBoxArgs = boxArgs.map(arg => {
              if (Array.isArray(arg) && arg.length === 3 && arg[0] === "*" && arg[1] === iteratorVar) {
                // This is i*multiplier pattern
                return { type: 'iterator_multiply', multiplier: arg[2] };
              } else if (this.containsVariable(arg, iteratorVar)) {
                return { type: 'variable', expr: arg };
              } else {
                // Pre-evaluate constant expressions
                return { type: 'constant', value: this.evaluate(arg, api, { ...this.localEnv, ...env }) };
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
                if (typeof color === 'object' && color.r !== undefined) {
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
              const evaluatedBoxArgs = analyzedBoxArgs.map(arg => {
                switch (arg.type) {
                  case 'iterator_multiply':
                    return i * arg.multiplier; // Direct multiplication, no evaluation
                  case 'constant':
                    return arg.value; // Pre-computed constant
                  case 'variable':
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
          const expressionAnalysis = expressions.map(expr => {
            const containsIterator = this.containsVariable ? this.containsVariable(expr, iteratorVar) : true;
            return { expr, containsIterator };
          });

          // CORE OPTIMIZATION: Pre-evaluate iterator-independent expressions
          const invariantResults = new Map();
          expressionAnalysis.forEach((analysis, index) => {
            if (!analysis.containsIterator) {
              invariantResults.set(index, this.evaluate(analysis.expr, api, baseEnv));
            }
          });

          // Create reusable environment object
          this.localEnvLevel += 1;
          if (!this.localEnvStore[this.localEnvLevel]) {
            this.localEnvStore[this.localEnvLevel] = Object.create(null); // Faster than {}
          }

          // CORE OPTIMIZATION: Reuse environment object, direct property assignment
          const loopEnv = this.localEnvStore[this.localEnvLevel];
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
        // Debug: Log the current state
        if (this.frameCount % 60 === 0) {
          console.log(
            "🎤 Mic function called - API:",
            !!this.microphoneApi,
            "Connected:",
            this.microphoneApi?.connected,
            "Amplitude:",
            this.microphoneApi?.amplitude,
          );
        }

        // Return current amplitude if microphone is available and connected
        if (this.microphoneApi && this.microphoneApi.connected) {
          return this.microphoneApi.amplitude || 0;
        }

        // If not connected yet, return 0 (connection is handled in boot)
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
        const [numerator, denominator] = timeSignature.split('/').map(Number);
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
          isPlaying: false
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
          const hasInstantTrigger = head.endsWith('!');
          const timeString = hasInstantTrigger ? head.slice(0, -1) : head;
          const seconds = parseFloat(timeString.slice(0, -1)); // Remove 's' and parse as float

          if (seconds === 0) {
            // 0s = every frame (no timing restriction)
            let timingResult;
            for (const arg of args) {
              timingResult = this.evaluate([arg], api, env);
            }
            result = timingResult;
          } else {
            const clockResult = api.clock.time(); // Get time (Date object)
            if (!clockResult) continue;

            // Convert Date object to milliseconds, then to seconds
            const currentTimeMs = clockResult.getTime
              ? clockResult.getTime()
              : Date.now();
            const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

            // Create a unique key for this timing expression
            const timingKey = `${head}_${JSON.stringify(args)}`;

            // Initialize lastExecution to current time if not set
            if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
              // If has instant trigger (!) and hasn't been executed yet, execute immediately
              if (hasInstantTrigger && !this.instantTriggersExecuted[timingKey]) {
                this.instantTriggersExecuted[timingKey] = true;
                // Set baseline time for future intervals
                this.lastSecondExecutions[timingKey] = currentTime;
                
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

              // Check if enough time has passed since last execution
              if (diff >= seconds) {
                this.lastSecondExecutions[timingKey] = currentTime;

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
        } else if (typeof head === "string" && /^\d*\.?\d+s\.\.\.?$/.test(head)) {
          // Handle timed iteration like "1s...", "2s...", "1.5s..."
          const seconds = parseFloat(head.slice(0, head.indexOf('s'))); // Extract seconds part

          const clockResult = this.evaluate("clock", api, env);
          if (clockResult) {
            const currentTimeMs = clockResult.getTime
              ? clockResult.getTime()
              : Date.now();
            const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

            // Create a unique key for this timed iteration expression
            const timingKey = `${head}_${JSON.stringify(args)}`;

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
              
              // Advance the sequence counter
              const currentIndex = this.sequenceCounters.get(timingKey) || 0;
              const nextIndex = (currentIndex + 1) % args.length;
              this.sequenceCounters.set(timingKey, nextIndex);
            }

            // Always return the current item (not just when advancing)
            if (args.length > 0) {
              const currentIndex = this.sequenceCounters.get(timingKey) || 0;
              result = args[currentIndex];
            }
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
                  head === "repeat"
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
      if (globalEnv[expression] && typeof globalEnv[expression] === 'function') {
        // Auto-call the function with no arguments
        return globalEnv[expression](api, [], env);
      }
      
      // Check if it exists in the API
      if (api[expression] && typeof api[expression] === 'function') {
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
}

// Module function that creates and returns a new KidLisp instance
function module(source) {
  const lisp = new KidLisp();
  return lisp.module(source);
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

  // Traditional kidlisp indicators - must start with ( or ;
  if (text.startsWith("(") || text.startsWith(";")) {
    return true;
  }

  // Check for encoded kidlisp (contains § suggesting newlines were encoded)
  if (text.includes("§")) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n");
    // Check decoded version - must start with ( or ; or contain newlines
    if (decoded.startsWith("(") || decoded.startsWith(";")) {
      return true;
    }
    if (decoded.includes("\n")) {
      const lines = decoded.split("\n");
      const hasKidlispLines = lines.some((line) => {
        const trimmed = line.trim();
        return (
          trimmed &&
          (trimmed.startsWith("(") || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed))
        );
      });
      return hasKidlispLines;
    }
  }

  if (text.includes("_") && text.match(/[a-zA-Z_]\w*_[a-zA-Z]/)) {
    const decoded = text
      .replace(/_/g, " ")
      .replace(/§/g, "\n")
      .replace(/~/g, "\n");
    // Check decoded version without recursion
    if (decoded.startsWith("(") || decoded.startsWith(";")) {
      return true;
    }
    if (decoded.includes("\n")) {
      const lines = decoded.split("\n");
      const hasKidlispLines = lines.some((line) => {
        const trimmed = line.trim();
        return (
          trimmed &&
          (trimmed.startsWith("(") || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed))
        );
      });
      return hasKidlispLines;
    }
    // Check if decoded looks like kidlisp function calls
    const trimmed = decoded.trim();
    if (/^[a-zA-Z_]\w*(\s|$)/.test(trimmed)) {
      return true;
    }
  }

  // Check if it contains newlines and looks like kidlisp (has function calls)
  if (text.includes("\n")) {
    const lines = text.split("\n");
    // If any line looks like a function call, treat as kidlisp
    const hasKidlispLines = lines.some((line) => {
      const trimmed = line.trim();
      return (
        trimmed &&
        (trimmed.startsWith("(") || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed))
      );
    });
    return hasKidlispLines;
  }

  // For simple text without § or newlines, only consider it kidlisp if it starts with ( or ;
  // This prevents simple slugs like "line~red" (which might become "line_red") from being detected as kidlisp
  return false;
}

function encodeKidlispForUrl(source) {
  const isKidlisp = isKidlispSource(source);

  if (!isKidlisp) {
    return source;
  }

  // For sharing, we want to preserve the structure so it can be parsed correctly
  // Spaces become underscores, newlines become ~ symbols (avoiding UTF-8 encoding issues with §)
  // But we keep parentheses and other structural elements intact
  // Handle problematic characters specially since they can cause URI malformation
  const encoded = source
    .replace(/ /g, "_")
    .replace(/\n/g, "~")
    .replace(/;/g, "%3B"); // Encode semicolons to prevent URI malformation
  return encoded;
}

function decodeKidlispFromUrl(encoded) {
  // Only decode if the result would be valid kidlisp
  const decoded = encoded
    .replace(/_/g, " ")
    .replace(/§/g, "\n") // Support legacy § separator
    .replace(/~/g, "\n") // Support new ~ separator (avoids UTF-8 encoding issues)
    .replace(/%28/g, "(")
    .replace(/%29/g, ")")
    .replace(/%2E/g, ".")
    .replace(/%22/g, '"')
    .replace(/%3B/g, ";") // Decode semicolons
    .replace(/S/g, "#"); // Decode sharp symbols from 'S' back to '#'
  return isKidlispSource(decoded) ? decoded : encoded;
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

export {
  module,
  parse,
  evaluate,
  KidLisp,
  isKidlispSource,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
  isPromptInKidlispMode,
};
