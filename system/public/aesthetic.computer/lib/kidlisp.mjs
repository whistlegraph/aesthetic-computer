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
const perfOrder = ['parse', 'precompile', 'frame-evaluation', 'repeat-setup', 'repeat-with-iterator', 'fast-draw-loop']; // Fixed order for consistent display

function perfStart(label) {
  if (PERF_LOG) perfTimers[label] = performance.now();
}
function perfEnd(label) {
  if (PERF_LOG && perfTimers[label]) {
    const duration = performance.now() - perfTimers[label];
    if (duration > 0.1) { // Only log operations > 0.1ms
      const logEntry = `${label}: ${duration.toFixed(2)}ms`;
      
      // Remove existing entry for this label to maintain order
      const existingIndex = perfLogs.findIndex(log => log.startsWith(label + ':'));
      if (existingIndex !== -1) {
        perfLogs.splice(existingIndex, 1);
      }
      
      // Insert in correct position based on perfOrder
      const orderIndex = perfOrder.indexOf(label);
      if (orderIndex !== -1) {
        // Find where to insert based on order
        let insertIndex = 0;
        for (let i = 0; i < perfLogs.length; i++) {
          const logLabel = perfLogs[i].split(':')[0];
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
    
    // Performance optimizations
    this.functionCache = new Map(); // Cache function lookups
    this.globalEnvCache = null; // Cache global environment
    this.fastPathFunctions = new Set(['line', 'ink', 'wipe', 'box', 'repeat', '+', '-', '*', '/', '=', '>', '<']); // Common functions for fast path
    this.expressionCache = new Map(); // Cache for simple expressions
    this.variableCache = new Map(); // Cache for variable lookups
    this.mathCache = new Map(); // Cache for math expressions
    this.sequenceCounters = new Map(); // Track sequence positions for ... function
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
    if (typeof expr === 'string') {
      // Check for chained math expressions like "width/5.67666*0.828"
      const chainedMatch = expr.match(/^(\w+)\s*([+\-*/%])\s*(\d+(?:\.\d+)?)\s*([+\-*/%])\s*(\d+(?:\.\d+)?)$/);
      if (chainedMatch) {
        const [, variable, op1, num1, op2, num2] = chainedMatch;
        
        // Convert to nested prefix expressions: (* (/ width 5.67666) 0.828)
        const innerExpr = [op1, variable, parseFloat(num1)];
        const outerExpr = [op2, innerExpr, parseFloat(num2)];
        return outerExpr;
      }
      
      // Check for simple infix math expressions
      const infixMatch = expr.match(/^(\w+)\s*([+\-*/%])\s*(\w+|\d+(?:\.\d+)?)$/);
      if (infixMatch) {
        const [, left, op, right] = infixMatch;
        
        // Convert right operand to number if it's numeric
        const rightValue = /^\d+(?:\.\d+)?$/.test(right) ? parseFloat(right) : right;
        
        // Return the expanded prefix expression - left should stay as unquoted identifier
        const result = [op, left, rightValue];
        return result;
      }
    } else if (Array.isArray(expr)) {
      // Recursively expand all elements in arrays
      return expr.map(item => this.expandFastMathMacros(item));
    }
    
    // Return unchanged if no expansion needed
    return expr;
  }

  // Parse and evaluate a lisp source module
  // into a running aesthetic computer piece.
  module(source) {
    perfStart('parse');
    const parsed = this.parse(source);
    perfEnd('parse');
    
    perfStart('ast-copy');
    this.ast = JSON.parse(JSON.stringify(parsed)); // Deep copy of original source. 🙃
    perfEnd('ast-copy');
    
    // Precompile optimizations
    perfStart('precompile');
    this.ast = this.precompileAST(this.ast);
    perfEnd('precompile');
    
    /*if (VERBOSE)*/ // console.log("🐍 Snake:", parsed);

    // 🧩 Piece API
    return {
      boot: ({ wipe, params, clock, screen, pieceCount }) => {
        // Resync clock for accurate timing (like clock.mjs does)
        clock?.resync?.();

        this.globalDef.paramA = params[0];
        this.globalDef.paramB = params[1];
        this.globalDef.paramC = params[2];
        
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
        
        perfStart('frame-evaluation');
        try {
          // Then execute the full program
          this.localEnvLevel = 0; // Reset state per program evaluation.
          this.localEnv = this.localEnvStore[this.localEnvLevel];
          /*const evaluated = */ this.evaluate(this.ast, $);
        } catch (err) {
          console.error("⛔ Evaluation failure:", err);
        }
        perfEnd('frame-evaluation');

        // Display performance logs in the piece
        if (PERF_LOG && perfLogs.length > 0) {
          $.ink?.("yellow");
          perfLogs.forEach((log, index) => {
            $.write?.(log, { x: 2, y: 24 + (index * 12) }, "black");
          });
        }

        // TODO: Re-enable the below:
        // Print the program output value in the center of the screen if
        // necessary.
        // ink("white").write(evaluated || "nada", { center: "xy" });

        // TODO: Add haltability to paint with a shortcut that triggers the below.
        // return false;
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

      const wrappedLines = lines.map((line) => {
        // If line doesn't start with ( and looks like a function call, wrap it
        if (!line.startsWith("(") && /^[a-zA-Z_]\w*/.test(line)) {
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
            if (!this.globalDef.hasOwnProperty(name)) {
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
          return args.length > 2 ? this.evaluate(args.slice(2), api, env) : true;
        } else {
          return false;
        }
      },
      // ➗ Mathematical Operators
      max: (api, args) => {
        const nums = args
          .map((arg) => parseFloat(unquoteString(arg)))
          .filter((n) => !isNaN(n));
        return nums.length > 0 ? Math.max(...nums) : 0;
      },
      "+": (api, args, env) => {
        // Simply evaluate each argument in the current environment and add
        const result = args.reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          return acc + (typeof value === 'number' ? value : 0);
        }, 0);
        return result;
      },
      "-": (api, args, env) => {
        // Simply evaluate each argument in the current environment and subtract
        if (args.length === 0) return 0;
        if (args.length === 1) {
          const value = this.evaluate(args[0], api, this.localEnv);
          return -(typeof value === 'number' ? value : 0);
        }
        const first = this.evaluate(args[0], api, this.localEnv);
        const result = args.slice(1).reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          return acc - (typeof value === 'number' ? value : 0);
        }, typeof first === 'number' ? first : 0);
        return result;
      },
      "*": (api, args, env) => {
        // Simply evaluate each argument in the current environment and multiply
        const result = args.reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          return acc * (typeof value === 'number' ? value : 0);
        }, 1);
        return result;
      },
      "/": (api, args, env) => {
        // Simply evaluate each argument in the current environment and divide
        if (args.length === 0) return 0;
        const first = this.evaluate(args[0], api, this.localEnv);
        const result = args.slice(1).reduce((acc, arg) => {
          const value = this.evaluate(arg, api, this.localEnv);
          const divisor = typeof value === 'number' ? value : 1;
          return divisor !== 0 ? acc / divisor : acc;
        }, typeof first === 'number' ? first : 0);
        return result;
      },
      "%": (api, args, env) => {
        // Simply evaluate each argument in the current environment and apply modulo
        if (args.length < 2) return 0;
        const first = this.evaluate(args[0], api, this.localEnv);
        const second = this.evaluate(args[1], api, this.localEnv);
        const a = typeof first === 'number' ? first : 0;
        const b = typeof second === 'number' ? second : 1;
        return b !== 0 ? a % b : 0;
      },
      // Paint API
      resolution: (api, args) => {
        api.resolution?.(...args);
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
          args[0].forEach(lineArgs => {
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
      scroll: (api, args = []) => {
        api.scroll(...args);
      },
      spin: (api, args = []) => {
        api.spin(...args);
      },
      resetSpin: (api, args = []) => {
        api.resetSpin();
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
          const box = { x: args[0], y: args[1], width: args[2], height: args[3] };
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
      label: (api, args = []) => {
        api.hud?.label(...processArgStringTypes(args));
      },
      copy: (api, args = []) => {
        //
      },
      // Convert args to string and remove surrounding quotes for text commands
      write: (api, args = []) => {
        const content = processArgStringTypes(args[0]);
        const x = args[1];
        const y = args[2];
        const bg = args[3] ? processArgStringTypes(args[3]) : undefined;
        
        // Build options object if we have additional parameters
        const options = {};
        if (bg !== undefined) {
          options.bg = bg;
        }
        
        // Call write with proper parameters
        if (x !== undefined && y !== undefined) {
          api.write(content, x, y, options);
        } else {
          api.write(content, { x, y }, bg);
        }
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
        perfStart('repeat-setup');
        if (args.length < 2) {
          console.error("❗ repeat requires at least 2 arguments: count and expression(s)");
          return undefined;
        }
        
        // Evaluate the count argument in case it's a variable or expression
        const countValue = this.evaluate(args[0], api, env);
        const count = Number(countValue);
        if (isNaN(count) || count < 0) {
          console.error("❗ repeat count must be a non-negative number, got:", countValue);
          return undefined;
        }
        perfEnd('repeat-setup');
        
        let result;
        
        // Check if we have an iterator variable (3+ args with 2nd arg being a string)
        if (args.length >= 3 && typeof args[1] === 'string') {
          perfStart('repeat-with-iterator');
          const iteratorVar = args[1];
          const expressions = args.slice(2);
          
          // Standard loop - set up proper environment with iterator variable
          const baseEnv = { ...this.localEnv, ...env };
          const prevLocalEnv = this.localEnv;
          
          // Create reusable environment object
          this.localEnvLevel += 1;
          if (!this.localEnvStore[this.localEnvLevel]) {
            this.localEnvStore[this.localEnvLevel] = { ...baseEnv };
          }
          
          // Reuse the same environment object, just update the iterator
          const loopEnv = this.localEnvStore[this.localEnvLevel];
          Object.assign(loopEnv, baseEnv); // Copy base environment once
          this.localEnv = loopEnv;
          
          for (let i = 0; i < count; i++) {
            // Update the iterator variable in the environment
            loopEnv[iteratorVar] = i;
            
            // Execute expressions with iterator variable in scope
            for (const expr of expressions) {
              result = this.evaluate(expr, api, this.localEnv);
            }
          }
          
          // Restore previous environment
          this.localEnvLevel -= 1;
          this.localEnv = prevLocalEnv;
          perfEnd('repeat-with-iterator');
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
    if (typeof expr === 'number') return expr;
    if (typeof expr === 'string') {
      // Fast variable lookup - don't cache iterator variables that change frequently
      let value;
      if (this.localEnv.hasOwnProperty(expr)) {
        value = this.localEnv[expr];
      } else if (env && env.hasOwnProperty(expr)) {
        value = env[expr];
      } else if (this.globalDef.hasOwnProperty(expr)) {
        value = this.globalDef[expr];
      }
      
      if (value !== undefined) {
        return value;
      }
      
      // Check global environment
      const globalEnv = this.getGlobalEnv();
      value = globalEnv[expr];
      if (typeof value === 'function') {
        value = value(api);
      }
      const result = value !== undefined ? value : expr;
      return result;
    }
    
    // Fast math expression evaluation
    if (Array.isArray(expr) && expr.length === 3) {
      const [op, left, right] = expr;
      if (op === '+' || op === '-' || op === '*' || op === '/' || op === '%') {
        const leftVal = this.fastEval(left, api, env);
        const rightVal = this.fastEval(right, api, env);
        
        // Only proceed if both are numbers
        if (typeof leftVal === 'number' && typeof rightVal === 'number') {
          switch (op) {
            case '+': return leftVal + rightVal;
            case '-': return leftVal - rightVal;
            case '*': return leftVal * rightVal;
            case '/': return leftVal / rightVal;
            case '%': return leftVal % rightVal;
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
      result = { type: 'local', value: this.localEnv[head] };
    } else if (existing(env?.[head])) {
      result = { type: 'env', value: env[head] };
    } else if (existing(this.getGlobalEnv()[head])) {
      result = { type: 'global', value: this.getGlobalEnv()[head] };
    } else if (existing(this.globalDef[head])) {
      result = { type: 'globalDef', value: this.globalDef[head] };
    } else if (existing(api[head]) && typeof api[head] === "function") {
      result = { type: 'api', value: api[head] };
    }
    
    // Cache the result (but not for local env since it changes)
    if (result && result.type !== 'local') {
      this.functionCache.set(cacheKey, result);
    }
    
    return result;
  }

  evaluate(parsed, api = {}, env, inArgs) {
    perfStart('evaluate-total');
    if (VERBOSE) console.log("➗ Evaluating:", parsed);
    
    let body;

    // Get global environment for this instance
    perfStart('get-global-env');
    const globalEnv = this.getGlobalEnv();
    perfEnd('get-global-env');
    
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
      if (Array.isArray(parsed) && parsed.length > 0 && typeof parsed[0] === 'string') {
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
      if (item && typeof item === 'object' && item.optimized) {
        perfStart('optimized-execution');
        result = item.func(api);
        perfEnd('optimized-execution');
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
        } else if (typeof head === "string" && /^\d*\.?\d+s$/.test(head)) {
          // Handle second-based timing like "0s", "1s", "2s", "5s", "1.5s", "0.3s"
          const seconds = parseFloat(head.slice(0, -1)); // Remove 's' and parse as float

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
            const currentTimeMs = clockResult.getTime ? clockResult.getTime() : Date.now();
            const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

            // Create a unique key for this timing expression
            const timingKey = `${head}_${JSON.stringify(args)}`;

            // Initialize lastExecution to current time if not set
            if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
              this.lastSecondExecutions[timingKey] = currentTime;
              continue; // Skip first execution to establish baseline
            }

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
            case 'local':
              perfStart(`local-${head}`);
              if (VERBOSE) console.log("😫 Local definition found!", head, value);
              if (value.iterable) {
                result = this.iterate(value, api, args, env);
              } else {
                result = value;
              }
              perfEnd(`local-${head}`);
              break;
              
            case 'env':
              perfStart(`env-${head}`);
              if (value.iterable) {
                result = this.iterate(value, api, args, env);
              } else {
                result = value;
              }
              perfEnd(`env-${head}`);
              break;
              
            case 'global':
              // Handle global environment functions
              if (typeof value === "function" || typeof value === "object") {
                let processedArgs;
                // Functions that don't need argument evaluation
                if (head === "later" || head === "tap" || head === "draw" || head === "if" || 
                    head === "not" || head === ">" || head === "<" || head === "=" || 
                    head === "net" || head === "source" || head === "choose" || head === "?" || 
                    head === "repeat") {
                  processedArgs = args;
                } else {
                  // Use fast evaluation for arguments with current local environment
                  processedArgs = args.map((arg, index) => {
                    if (Array.isArray(arg) || (typeof arg === "string" && !/^".*"$/.test(arg))) {
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
              
            case 'globalDef':
              // User-defined functions - use fast evaluation for arguments
              const evaluatedArgs = args.map((arg) =>
                Array.isArray(arg) || (typeof arg === "string" && !/^".*"$/.test(arg))
                  ? this.fastEval(arg, api, this.localEnv)
                  : arg
              );
              
              result = Array.isArray(value) || value.body
                ? this.evaluate(value, api, this.localEnv, evaluatedArgs)
                : value;
              break;
              
            case 'api':
              // API functions - use fast evaluation for arguments
              const apiArgs = args.map((arg) =>
                Array.isArray(arg) || (typeof arg === "string" && !/^".*"$/.test(arg))
                  ? this.fastEval(arg, api, this.localEnv)
                  : arg
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

    perfEnd('evaluate-total');
    return result;
  }

  evalNotFound(expression, api, env) {
    if (typeof expression !== "string") {
      // console.log("🤖 Expression:", expression);
      return expression; // Return numbers.
    } else {
      // console.log("🤖 Attempting JavaScript expression evaluation:", expression);
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

  // Traditional kidlisp indicators
  if (text.startsWith("(") || text.startsWith(";")) {
    return true;
  }

  // Check for encoded kidlisp (contains § or ~ or _ suggesting it was URL encoded)
  if (text.includes("§") || text.includes("~")) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n").replace(/~/g, "\n");
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
  }

  if (text.includes("_") && text.match(/[a-zA-Z_]\w*_[a-zA-Z]/)) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n").replace(/~/g, "\n");
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
  const encoded = source.replace(/ /g, "_").replace(/\n/g, "~");
  return encoded;
}

function decodeKidlispFromUrl(encoded) {
  // Only decode if the result would be valid kidlisp
  const decoded = encoded
    .replace(/_/g, " ")
    .replace(/§/g, "\n")     // Support legacy § separator
    .replace(/~/g, "\n")     // Support new ~ separator (avoids UTF-8 encoding issues)
    .replace(/%28/g, "(")
    .replace(/%29/g, ")")
    .replace(/%2E/g, ".")
    .replace(/%22/g, '"');
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
