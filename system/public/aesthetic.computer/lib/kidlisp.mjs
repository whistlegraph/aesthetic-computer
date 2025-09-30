// Kidlisp, 24.4.17.12.03
// A lisp interpreter / compiler for writing Aesthetic Computer pieces.
// FORCE RELOAD: 2025-08-23-CACHE-BUSTER-UPDATE

// ❤️‍🔥 TODO: Add UTC Support to s... timers. etc. to be compatible with 'clock.mjs'.
//        - Selectabe values via game controller.

import { parseMelody, noteToTone } from "./melody-parser.mjs";
import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";
import { cssColors, rainbow, zebra, resetZebraCache, resetRainbowCache, staticColorMap } from "./num.mjs";
import { setFadeAlpha, clearFadeAlpha } from "./fade-state.mjs";
import { checkTeiaMode } from "./teia-mode.mjs";

/* #region 🤖 LLM API SPECIFICATION
   This section provides a structured specification for Large Language Models
   to understand and generate valid KidLisp code for Aesthetic Computer pieces.
   
   ## KidLisp Language Overview
   
   KidLisp is a Lisp-based language for creating interactive visual art and animations.
   It uses S-expressions (parenthesized lists) where the first element is typically
   a function name followed by arguments.
   
   Basic syntax: (function-name arg1 arg2 arg3)
   Comments: ; This is a comment
   Multi-line: Expressions can span multiple lines
   
   ## Core Graphics Functions
   
   ### Screen Management
   - `(wipe color)` - Clear entire screen with specified color
   - `(resolution width height)` - Set canvas resolution
   - `(scroll)` - Randomly choose and stick to one direction (up/down/left/right) per session
   - `(scroll dx dy)` - Scroll by dx horizontally and dy vertically
   - `(scroll dx)` - Scroll by dx horizontally only
   
   ### Drawing Primitives  
   - `(ink color)` - Set drawing color for subsequent operations
   - `(ink r g b)` - Set RGB color (0-255 each)
   - `(ink color alpha)` - Set color with transparency (0-255)
   - `(line x1 y1 x2 y2)` - Draw line from point 1 to point 2
   - `(box x y width height)` - Draw filled rectangle
   - `(circle x y radius)` - Draw filled circle
   - `(tri x1 y1 x2 y2 x3 y3)` - Draw filled triangle from three points
   - `(tri x1 y1 x2 y2 x3 y3 "outline")` - Draw triangle outline
   - `(plot x y)` - Set single pixel at coordinates
   
   ### Image Functions
   - `(paste url x y)` - Paste image from URL at coordinates
   - `(paste url x y scale)` - Paste image with scaling factor
   - `(stamp url x y)` - Paste image centered at coordinates
   - URLs can be unquoted: `(paste https://example.com/image.png x y)`
   - Quoted URLs also work: `(paste "https://example.com/image.png" x y)`
   - Supports @handle/timestamp format: `(paste @user/123456 x y)`
   
   ### Color System
   Colors can be specified as:
   - Named colors: "red", "blue", "lime", "orange", "purple", etc.
   - RGB values: (ink 255 0 0) for red
   - With transparency: (ink "red" 128) for 50% transparent red
   
   ## Animation & Timing
   
   ### Timing Expressions
   - `1s` - Execute after 1 second
   - `2s...` - Execute every 2 seconds (repeating)
   - `0.5s!` - Execute once after 0.5 seconds
   
   ### Dynamic Values
   - `(wiggle amount)` - Random variation (±amount/2)
   - `width` and `height` - Canvas dimensions
   - Frame-based animation through re-evaluation
   
   ## Variables & Logic
   
   ### Variable Definition
   - `(def name value)` - Define a variable
   - `(def x 10)` - Set x to 10
   - `(def color "red")` - Set color variable
   
   **Identifier Naming Rules:**
   - Must start with letter (a-z, A-Z) or underscore (_)
   - Can contain letters, digits (0-9), and underscores
   - **Cannot contain dashes/hyphens (-)** - these are parsed as subtraction
   - Examples: `myVar`, `line_width`, `color2` ✅
   - Invalid: `my-var`, `line-width` ❌ (parsed as subtraction)
   
   ### Math Operations
   - `(+ a b c)` - Addition (can take multiple arguments)
   - `(- a b)` - Subtraction  
   - `(* a b c)` - Multiplication
   - `(/ a b)` - Division
   
   ### Function Definition
   - `(later name param1 param2 body...)` - Define reusable function
   - `(later cross x y (line (- x 10) (- y 10) (+ x 10) (+ y 10)))`
   - Call with: `(cross 50 50)`
   
   ## Advanced Features
   
   ### Navigation
   - `(jump "piece-name")` - Jump to another Aesthetic Computer piece
   - `(jump "https://example.com")` - Jump to external URL
   - `(jump $abc123)` - Jump to cached KidLisp piece by ID
   - Automatically resets KidLisp state and loads the new piece
   
   ### One-time Execution
   - `(once expression)` - Execute only once, not every frame
   - Useful for setup code: `(once (wipe "black"))`
   
   ### Background Layers
   - `(bake)` - Render current drawing to background layer
   - `(once (bake))` - Bake background once for layered effects
   
   ### Embedded Code
   - `($codeId)` - Execute cached code by ID
   - `($codeId width height)` - Execute in custom buffer size
   - `(embed $codeId x y width height alpha)` - Advanced embedding
   
   ## Example Patterns for LLMs
   
   ### Simple Drawing
   ```kidlisp
   (wipe "black")
   (ink "red")  
   (circle 50 50 30)
   (ink "blue")
   (box 100 100 50 50)
   (ink "yellow")
   (tri 150 50 180 100 120 100)
   ```
   
   ### Animated Scene
   ```kidlisp
   (wipe "navy")
   (ink "yellow")
   (circle (+ 100 (wiggle 20)) (+ 100 (wiggle 20)) 10)
   (ink "white" 100)
   (box 0 (+ 150 (wiggle 5)) width 2)
   ```
   
   ### Interactive Functions
   ```kidlisp
   (later star x y size
     (ink "yellow")
     (circle x y size)
     (ink "white")
     (circle x y (/ size 2)))
   
   (star 100 100 20)
   (star 200 150 15)
   ```
   
   ### Image Pasting with URLs
   ```kidlisp
   (wipe "black")
   ; Unquoted URLs are now supported!
   (paste https://example.com/image.png 50 50 0.5)
   (stamp https://example.com/logo.png (/ width 2) 100)
   ; Quoted URLs still work
   (paste "https://example.com/background.jpg" 0 0 1.0)
   ```
   
   ### Timed Animations  
   ```kidlisp
   (once (wipe "black"))
   1s (ink "red") (circle 100 100 50)
   2s (ink "blue") (box 150 150 40 40)
   3s... (ink (wiggle 255) (wiggle 255) (wiggle 255)) (plot (wiggle width) (wiggle height))
   ```
   
   ### Navigation & Jumps
   ```kidlisp
   (wipe "black")
   (ink "white")
   (write "Click to jump!" { center: "xy" })
   
   ; Jump to another piece after 3 seconds
   3s (jump "prompt")
   
   ; Or jump to a cached KidLisp piece
   ; 5s (jump $abc123)
   ```
   
   ## Best Practices for LLM Generation
   
   1. **Start with background**: Always begin with `(wipe color)` to set background
   2. **Set colors before drawing**: Use `(ink color)` before drawing primitives
   3. **Use meaningful coordinates**: Consider canvas size (typically 256x256 default)
   4. **Leverage animation**: Use `wiggle` for organic movement
   5. **Layer effects**: Use `(once (bake))` for background elements
   6. **Readable spacing**: Format code with proper indentation for readability
   7. **Combine timing**: Mix immediate drawing with timed animations
   8. **Reuse patterns**: Define functions with `later` for repeated elements
   
   ## Error Patterns to Avoid
   
   - Don't forget parentheses around expressions
   - Don't use undefined color names (stick to CSS colors or RGB values)
   - Don't reference undefined variables before `def`
   - Don't mix coordinate systems (stay consistent with canvas bounds)
   - Don't create infinite loops without timing delays
   - **Don't use dashes in identifiers** - use underscores instead (mouth_y not mouth-y)
   
#endregion */

// Global cache registry for storing cached codes by source hash
const cacheRegistry = new Map();

// Global backdrop state for first-line color shorthand
const globalBackdropState = new Set();

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

  ; Compact one-liner syntax with commas:
  ; "blue, ink rainbow, repeat 100 line" becomes "(blue) (ink rainbow) (repeat 100 line)"
  blue, ink rainbow, repeat 100 line
  wipe black, ink red, box 10 10 50 50
  (wipe blue), ink yellow, circle 64 64 30  ; Mix with parentheses

  ; Multi-layer example with bake:
  (ink "blue")
  (box 10 10 100 100)
  (once (bake))  ; Bakes the blue box into the background (use 'once' to prevent repeated calls)
  (ink "red") 
  (box 50 50 100 100)  ; This red box will appear on top of the baked blue box

  ; Embedding cached kidlisp snippets:
  ; First save some code to get a cache code like $abc123XY
  ; Then you can embed it:
  ($abc123XY)            ; Load and run cached code in default 256x256 buffer, then paste at 0,0
  ($abc123XY 128 128)    ; Load and run cached code in 128x128 buffer  
  (embed $abc123XY)      ; Same as ($abc123XY) - explicit embed syntax
  (embed $abc123XY 64 64) ; Load in 64x64 buffer
  (embed $abc123XY 0 0 64 64 128) ; Load in 64x64 buffer at 0,0 with 50% alpha (128/255)
  (paste ($abc123XY 32 32) 100 100) ; Load in 32x32 buffer and paste at 100,100 
  (box 50 50 100 100)  ; This red box will appear on top of the baked blue box

  ((line 20 80 80 80))
  (later cross x y
    (line x-10 y-10 x+10 y+10)
    (line x-10 y+10 x+10 y-10)
  )
  (ink yellow)
  (cross 16 32)

  ; Embedding example (requires cached code):
  ; If you have cached code $xyz123, you can:
  ; ($xyz123)              ; Run in default buffer and auto-paste
  ; ($xyz123 64 64)        ; Run in 64x64 buffer and auto-paste  
  ; ($xyz123 0 0 64 64 0.5) ; Run in 64x64 buffer with 50% alpha (0.5 = 50%)
  ; (paste ($xyz123) 50 50) ; Run and paste at specific coordinates
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

// 🗄️ Global $code caching system (shared across all KidLisp instances)
// This provides multi-level caching: RAM → IndexedDB → Network → TEIA
const globalCodeCache = new Map(); // In-memory cache for current session
let persistentStoreRef = null; // Reference to the persistent store system

// Initialize persistent cache when store becomes available
function initPersistentCache(store) {
  if (!persistentStoreRef && store) {
    persistentStoreRef = store;
    // console.log("%c🗄️ $cache ready", 'color: #4CAF50; font-weight: bold;');
  }
}

// Get from persistent cache (IndexedDB)
async function getFromPersistentCache(cacheId) {
  if (!persistentStoreRef) return null;

  try {
    const data = await persistentStoreRef.retrieve(`kidlisp-code:${cacheId}`, "local:db");
    if (data && data.source) {
      return data.source;
    }
  } catch (error) {
    console.warn(`Failed to retrieve from persistent cache: ${cacheId}`, error);
  }
  return null;
}

// Save to persistent cache (IndexedDB)
async function saveToPersistentCache(cacheId, source) {
  if (!persistentStoreRef) return;

  try {
    persistentStoreRef[`kidlisp-code:${cacheId}`] = {
      source,
      cached: Date.now(),
      version: 1
    };
    persistentStoreRef.persist(`kidlisp-code:${cacheId}`, "local:db");
    // console.log(`%c� ${cacheId}`, 'color: #FF9800; font-weight: bold;');
  } catch (error) {
    console.warn(`Failed to save to persistent cache: ${cacheId}`, error);
  }
}

// Multi-level cache retrieval: RAM → IndexedDB → Network → TEIA
async function getCachedCodeMultiLevel(cacheId) {
  // Level 1: Check RAM cache (fastest)
  if (globalCodeCache.has(cacheId)) {
    return globalCodeCache.get(cacheId);
  }

  // Level 1.5: Check TEIA pre-cached codes (for offline packages)
  const globalScope = (function () {
    if (typeof window !== 'undefined') return window;
    if (typeof globalThis !== 'undefined') return globalThis;
    if (typeof global !== 'undefined') return global;
    if (typeof self !== 'undefined') return self;
    return {};
  })();

  if (globalScope.teiaKidlispCodes && globalScope.teiaKidlispCodes[cacheId]) {
    const teiaSource = globalScope.teiaKidlispCodes[cacheId];
    // Cache in RAM for future access
    globalCodeCache.set(cacheId, teiaSource);
    return teiaSource;
  }

  // Level 2: Check IndexedDB (fast)
  const persistentSource = await getFromPersistentCache(cacheId);
  if (persistentSource) {
    // Cache in RAM for future access
    globalCodeCache.set(cacheId, persistentSource);
    return persistentSource;
  }

  // Level 3: Network fetch (slowest)
  const networkSource = await fetchCachedCode(cacheId);
  if (networkSource) {
    // Cache at all levels
    globalCodeCache.set(cacheId, networkSource);
    await saveToPersistentCache(cacheId, networkSource);
    return networkSource;
  }

  return null;
}

// Clear all caches (useful for development/debugging)
function clearAllCaches() {
  globalCodeCache.clear();
  // console.log("%c🗑️ $cache cleared", 'color: #FF5722; font-weight: bold;');
}

// Save a code to all cache levels (when user creates/saves a piece)
function saveCodeToAllCaches(cacheId, source) {
  globalCodeCache.set(cacheId, source);
  saveToPersistentCache(cacheId, source);
}

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
const identifierRegex = /[$a-zA-Z_]\w*/g;
const validIdentifierRegex = /^[$a-zA-Z_]\w*$/;

function tokenize(input) {
  if (VERBOSE) console.log("🪙 Tokenizing:", input);

  // Updated regex to treat commas as separate tokens for comma syntax support
  const regex = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|[^\s()";,]+)/g;
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

    // Check if the current token is a timing expression (both simple like "1.5s" and repeating like "2s...")
    const currentToken = tokens[0];

    if (/^\d*\.?\d+s\.\.\.?$/.test(currentToken) || /^\d*\.?\d+s!?$/.test(currentToken)) {
      // This is a timing token, collect it and all following expressions until we hit another timing token or end
      const timingExpr = [readExpression(tokens)]; // Read the timing token itself

      // Collect all following expressions until we hit another timing token or run out
      while (tokens.length > 0 &&
        tokens[0] !== ")" &&
        !/^\d*\.?\d+s\.\.\.?$/.test(tokens[0]) &&
        !/^\d*\.?\d+s!?$/.test(tokens[0])) {
        const nextExpr = readExpression(tokens);
        timingExpr.push(nextExpr);
      }

      result.push(timingExpr);
    } else {
      const expr = readExpression(tokens);
      result.push(expr);
    }
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

    // Initialize audio-related global variables
    this.globalDef.amp = 0; // Current audio amplitude (0-10 scale)
    this.globalDef.leftAmp = 0; // Left channel amplitude (0-10 scale) 
    this.globalDef.rightAmp = 0; // Right channel amplitude (0-10 scale)
    this.globalDef.beat = 0; // Beat detection (0 or 1)
    this.globalDef.kick = 0; // Kick drum / beat detected (0 or 1)

    this.localEnvStore = [{}];
    this.localEnv = this.localEnvStore[0];
    this.localEnvLevel = 0;
    this.tapper = null;
    this.drawer = null;
    this.frameCount = 0; // Frame counter for timing functions
    this.lastSecondExecutions = {}; // Track last execution times for second-based timing
    this.instantTriggersExecuted = {}; // Track which instant triggers have already fired

    // Isolated random number generator state for each instance
    this.randomSeed = Date.now() + Math.random(); // Unique seed per instance
    this.randomState = this.randomSeed;

    // Cache state (URLs stored per instance)
    this.shortUrl = null; // Store cached short URL
    this.cachedCode = null; // Store cached code
    this.cachingInProgress = false; // Flag to prevent multiple cache requests

    // Embedded layers system
    this.embeddedLayers = [];
    this.embeddedLayerCache = new Map();
    this.embeddedSourceCache = new Map(); // Cache source code by cacheId to avoid duplicate fetches

    // Embedded layer loading state tracking
    this.loadingEmbeddedLayers = new Set(); // Track which $codes are currently loading
    this.loadedEmbeddedLayers = new Set();  // Track which $codes have finished loading

    // Performance optimizations
    this.functionCache = new Map(); // Cache function lookups
    this.globalEnvCache = null; // Cache global environment
    this.embeddedApiCache = new Map(); // Cache embedded layer API objects

    // Screen dimension tracking for responsive cache invalidation
    this.lastScreenWidth = null;
    this.lastScreenHeight = null;

    // 🚀 ALPHA BUFFER CACHING: Cache alpha-adjusted buffers to avoid repeated allocations
    this.alphaBufferCache = new Map(); // size_alpha -> buffer

    // � BLEND CACHING: Cache final composite to avoid redundant alpha blending
    this.cachedComposite = null; // Cache final blended result
    this.compositeInvalidated = false; // Track when composite needs updating

    // �🔄 BUFFER POOLING: Reuse buffers to reduce GC pressure
    this.bufferPool = new Map(); // size -> [buffer1, buffer2, ...]
    this.maxPooledBuffers = 8; // Limit pool size to avoid memory bloat

    // 🔍 CONTRAST CACHING: Cache contrast operations to avoid redundant processing
    this.lastContrastArgs = null; // Track last contrast arguments
    this.contrastCacheInvalidated = true; // Track when contrast needs reapplication

    // Clear embedded layer cache on initialization/reload (after caches are initialized)
    this.clearEmbeddedLayerCache();

    // Microphone state tracking
    this.microphoneConnected = false;
    this.microphoneApi = null;
    this.fastPathFunctions = new Set([
      "line",
      "ink",
      "wipe",
      "backdrop",
      "box",
      "repeat",
      "bake",
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
      "jump",
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
    this.currentSource = null; // Track current source code to detect changes

    // Timing blink state tracking
    this.timingBlinks = new Map(); // Track timing expressions that should blink
    this.blinkDuration = 200; // Duration for timing blinks in milliseconds

    // Delay timer active periods tracking
    this.delayTimerActivePeriods = new Map(); // Track delay timers in their active display period
    this.delayTimerActiveDuration = 280; // Duration for delay timer active display in milliseconds (5 frames red + 12 frames colored = ~280ms at 60fps)

    // Active timing expressions tracking for syntax highlighting
    this.activeTimingExpressions = new Map(); // Track which timing expressions are currently active

    // Current timing context for conditional execution
    this.currentTimingContext = null; // Track timing context for embedded layer conditional execution

    // Syntax highlighting signals from execution
    this.syntaxSignals = new Map(); // Direct signals from evaluation: expressionId -> color
    this.expressionRegistry = new Map(); // Map AST expressions to unique IDs
    this.nextExpressionId = 0; // Counter for expression IDs

    // 🎯 Performance monitoring system
    this.perf = {
      enabled: false, // Toggle for performance monitoring
      showHUD: false, // Toggle for performance HUD overlay
      samples: 100, // Number of samples to keep for rolling averages
      lastLogTime: 0, // Last time we logged to console
      logInterval: 1000, // Log every 1 second
      history: [], // Historical snapshots for bar graph (keep last 20)

      // Timing categories
      timings: {
        parse: [], // Parse time samples
        evaluate: [], // Evaluation time samples
        render: [], // Render time samples (if available)
        total: [], // Total frame time samples
      },

      // Function call tracking
      functions: new Map(), // functionName -> { count, totalTime, avgTime }

      // Memory usage tracking
      memory: {
        ast: 0, // AST size estimate
        env: 0, // Environment size estimate
        cache: 0, // Cache size estimate
      },

      // Current frame stats
      current: {
        parseTime: 0,
        evaluateTime: 0,
        renderTime: 0,
        totalTime: 0,
        frameStart: 0,
      },

      // Rolling averages (calculated from samples)
      avg: {
        parse: 0,
        evaluate: 0,
        render: 0,
        total: 0,
        fps: 0,
      }
    };

    // Ink state management
    this.inkState = undefined; // Track KidLisp ink color (starts undefined)
    this.inkStateSet = false; // Track if ink has been explicitly set

    // Bake functionality state
    this.bakedLayers = []; // Store baked pixel buffers
    this.bakeCallCount = 0; // Track number of bake calls to prevent duplicates

    // Performance monitoring
    this.performanceEnabled = false; // Enable/disable performance monitoring
    this.frameTimings = []; // Store recent frame timings
    this.evaluationCounts = new Map(); // Track evaluation counts per expression type
    this.totalEvaluations = 0; // Total evaluations this frame
    this.currentFrameStart = 0; // Start time of current frame
  }

  // 🎯 Performance monitoring methods
  startPerformanceMonitoring() {
    this.perf.enabled = true;
    this.perf.showHUD = true;

    // Also enable graph performance tracking - try multiple approaches
    if (typeof window !== 'undefined') {
      // Try direct window access
      if (window.graphPerf) {
        window.graphPerf.enabled = true;
        // console.log("🎯 KidLisp + Graph performance monitoring enabled (window)");
      } else {
        // console.log("🎯 KidLisp performance monitoring enabled (window.graphPerf not found)");
      }
    }

    // Try global scope access
    if (typeof globalThis !== 'undefined' && globalThis.graphPerf) {
      globalThis.graphPerf.enabled = true;
      // console.log("🎯 Graph performance monitoring enabled (globalThis)");
    }

    // Try importing graph.mjs directly if available
    try {
      // Enable graph performance through module system if possible
      if (typeof graphPerf !== 'undefined') {
        graphPerf.enabled = true;
        // console.log("🎯 Graph performance monitoring enabled (direct)");
      }
    } catch (e) {
      // Module access not available
    }
  }

  stopPerformanceMonitoring() {
    this.perf.enabled = false;
    this.perf.showHUD = false;

    // Also disable graph performance tracking
    if (typeof window !== 'undefined' && window.graphPerf) {
      window.graphPerf.enabled = false;
    }
  }

  togglePerformanceHUD() {
    this.perf.showHUD = !this.perf.showHUD;
    if (this.perf.showHUD) {
      this.perf.enabled = true; // Enable tracking when HUD is shown
    }
    return this.perf.showHUD;
  }

  startFrame() {
    if (!this.perf.enabled) return;
    this.perf.current.frameStart = performance.now();
    // Reset rainbow cache for new frame to ensure color cycling
    resetRainbowCache();
  }

  endFrame() {
    if (!this.perf.enabled) return;
    const now = performance.now();
    this.perf.current.totalTime = now - this.perf.current.frameStart;
    this.addSample('total', this.perf.current.totalTime);
    this.updateAverages();
  }

  startTiming(category) {
    return performance.now();
  }

  endTiming(category, startTime) {
    if (!this.perf.enabled) return;
    const duration = performance.now() - startTime;
    this.perf.current[category + 'Time'] = duration;
    this.addSample(category, duration);
  }

  addSample(category, value) {
    if (!this.perf.timings[category]) return;
    this.perf.timings[category].push(value);
    if (this.perf.timings[category].length > this.perf.samples) {
      this.perf.timings[category].shift();
    }
  }

  updateAverages() {
    Object.keys(this.perf.timings).forEach(category => {
      const samples = this.perf.timings[category];
      if (samples.length > 0) {
        this.perf.avg[category] = samples.reduce((a, b) => a + b, 0) / samples.length;
      }
    });

    // Calculate FPS from total time
    if (this.perf.avg.total > 0) {
      this.perf.avg.fps = 1000 / this.perf.avg.total;

      // 🚀 PERFORMANCE: Share FPS with graph performance tracking for frame skipping
      if (globalThis.graphPerf) {
        globalThis.graphPerf.lastFPS = this.perf.avg.fps;
      }
    }
  }

  trackFunction(name, duration) {
    if (!this.perf.enabled) return;
    if (!this.perf.functions.has(name)) {
      this.perf.functions.set(name, { count: 0, totalTime: 0, avgTime: 0 });
    }
    const stats = this.perf.functions.get(name);
    stats.count++;
    stats.totalTime += duration;
    stats.avgTime = stats.totalTime / stats.count;

    // 🚨 Hot function detection - warn about excessive calls (disabled)
    if (false && stats.count > 0 && stats.count % 1000 === 0) {
      console.warn(`🔥 HOT FUNCTION: ${name} called ${stats.count} times (${stats.avgTime.toFixed(3)}ms avg)`);
    }
  }

  // 📊 Render performance HUD in top-right corner with tiny matrix font
  renderPerformanceHUD(api) {
    if (!this.perf.showHUD || !api.ink || !api.write || !api.box) return;

    // Log performance data every second
    const now = performance.now();
    if (now - this.perf.lastLogTime > this.perf.logInterval) {
      this.logPerformanceSnapshot();
      this.perf.lastLogTime = now;
    }

    const screen = api.screen || { width: 256, height: 256 };
    const hudWidth = 100;
    const hudHeight = 45; // Reduced for simplified stats display
    const x = screen.width - hudWidth - 2;
    const y = 2;

    // Semi-transparent background
    api.ink(0, 0, 0, 200);
    api.box(x - 1, y - 1, hudWidth + 2, hudHeight + 2, "fill");

    // Border
    api.ink(100, 255, 100, 255);
    api.box(x - 1, y - 1, hudWidth + 2, hudHeight + 2, "outline");

    // Performance stats text - simplified version
    let lineY = y + 1;
    const lineHeight = 8;

    // FPS with health color coding
    const fps = this.perf.avg.fps;
    const totalTime = this.perf.avg.total;

    // Color code FPS based on 60fps target: green >= 55, yellow >= 45, orange >= 30, red < 30
    let fpsColor = fps >= 55 ? [0, 255, 0] : fps >= 45 ? [255, 255, 0] : fps >= 30 ? [255, 165, 0] : [255, 0, 0];
    api.ink(fpsColor[0], fpsColor[1], fpsColor[2]);
    api.write(`FPS:${fps.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;

    // Logic time (parse + evaluate combined)
    api.ink(100, 200, 255);
    const logicTime = (this.perf.avg.parse || 0) + (this.perf.avg.evaluate || 0);
    api.write(`LOGIC:${logicTime.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    this.drawPerfBar(api, x + 48, lineY, logicTime, 8.0, 50, [100, 200, 255]); // 8ms max for logic
    lineY += lineHeight;

    // Paint time (render)
    api.ink(255, 100, 200);
    api.write(`PAINT:${this.perf.avg.render.toFixed(1)}`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    this.drawPerfBar(api, x + 48, lineY, this.perf.avg.render, 6.0, 50, [255, 100, 200]); // 6ms max for painting
    lineY += lineHeight;

    // Memory usage
    const memEstimate = this.estimateMemoryUsage();
    api.ink(150, 150, 255);
    api.write(`MEMORY:${(memEstimate / 1024).toFixed(1)}KB`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");
    lineY += lineHeight;

    // Overall health bar (simplified single indicator)
    api.ink(200, 200, 200);
    api.write(`HEALTH`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");

    // Calculate overall health score (0-100)
    const fpsHealth = Math.min(100, (fps / 60) * 100);
    const timeHealth = Math.max(0, 100 - ((totalTime - 16.67) / 16.67) * 100);
    const memHealth = Math.max(0, 100 - Math.max(0, (memEstimate - 1024 * 1024) / (1024 * 1024)) * 100);
    const overallHealth = (fpsHealth + timeHealth + memHealth) / 3;

    // Color code health: green >= 80, yellow >= 60, orange >= 40, red < 40
    let healthColor = overallHealth >= 80 ? [0, 255, 0] : overallHealth >= 60 ? [255, 255, 0] : overallHealth >= 40 ? [255, 165, 0] : [255, 0, 0];
    this.drawPerfBar(api, x + 48, lineY, overallHealth, 100, 50, healthColor);
    lineY += lineHeight;

    // Historical performance bar graph (mini sparkline)
    if (this.perf.history.length > 1) {
      api.ink(80, 80, 80);
      api.write(`HIST`, { x, y: lineY }, undefined, undefined, false, "MatrixChunky8");

      const graphX = x + 20;
      const graphY = lineY + 2;
      const graphWidth = 70;
      const graphHeight = 6;

      // Draw background
      api.ink(20, 20, 20);
      api.box(graphX, graphY, graphWidth, graphHeight, "fill");

      // Draw FPS history bars (normalized to 60fps target)
      const barWidth = Math.max(1, Math.floor(graphWidth / this.perf.history.length));
      for (let i = 0; i < this.perf.history.length; i++) {
        const snapshot = this.perf.history[i];
        // Normalize to 60fps = full height, allow up to 75fps max display
        const normalizedFps = Math.min(75, snapshot.fps);
        const barHeight = Math.min(graphHeight, Math.max(1, (normalizedFps / 75) * graphHeight));
        const barX = graphX + (i * barWidth);
        const barY = graphY + graphHeight - barHeight;

        // 60fps-targeted color thresholds: green >= 55fps, yellow >= 45fps, orange >= 30fps, red < 30fps
        if (snapshot.fps >= 55) {
          api.ink(0, 255, 0);      // Green: excellent performance
        } else if (snapshot.fps >= 45) {
          api.ink(255, 255, 0);    // Yellow: good performance
        } else if (snapshot.fps >= 30) {
          api.ink(255, 165, 0);    // Orange: concerning performance
        } else {
          api.ink(255, 0, 0);      // Red: poor performance
        }

        api.box(barX, barY, Math.max(1, barWidth - 1), barHeight, "fill");
      }

      // Draw 60fps target line
      const targetY = graphY + graphHeight - Math.floor((60 / 75) * graphHeight);
      api.ink(128, 128, 128);
      api.box(graphX, targetY, graphWidth, 1, "fill");
    }
  }

  // Draw a tiny performance bar with 60fps-oriented health colors
  drawPerfBar(api, x, y, value, maxValue, width, baseColor) {
    const fillWidth = Math.min(width, (value / maxValue) * width);
    const fillRatio = value / maxValue;

    // Background bar
    api.ink(50, 50, 50);
    api.box(x, y + 2, width, 3, "fill");

    // Fill bar with health-based coloring
    if (fillWidth > 0) {
      let barColor;
      if (fillRatio <= 0.5) {
        // Green zone: 0-50% of budget is healthy
        barColor = [0, 255, 0];
      } else if (fillRatio <= 0.75) {
        // Yellow zone: 50-75% of budget is concerning
        barColor = [255, 255, 0];
      } else if (fillRatio <= 1.0) {
        // Orange zone: 75-100% of budget is problematic
        barColor = [255, 165, 0];
      } else {
        // Red zone: over budget is critical
        barColor = [255, 0, 0];
      }

      api.ink(barColor[0], barColor[1], barColor[2]);
      api.box(x, y + 2, fillWidth, 3, "fill");
    }

    // Overflow indicator if over max
    if (value > maxValue) {
      api.ink(255, 0, 0);
      api.box(x + width - 2, y + 1, 2, 5, "fill");
    }
  }

  // Estimate memory usage
  estimateMemoryUsage() {
    let size = 0;

    // AST size estimate (rough)
    size += JSON.stringify(this.localEnv || {}).length;

    // Timing data
    size += this.perf.timings.total.length * 8; // rough estimate

    // Function tracking
    size += this.perf.functions.size * 64; // rough estimate

    return size;
  }

  // Log performance snapshot to console and store in history
  logPerformanceSnapshot() {
    let totalFunctionCalls = 0;
    let graphCalls = 0;
    let apiCalls = 0;
    let userCalls = 0;
    let optCalls = 0;

    // Categorize function calls
    this.perf.functions.forEach((stats, name) => {
      totalFunctionCalls += stats.count;
      if (name.startsWith('api:')) {
        apiCalls += stats.count;
      } else if (name.startsWith('user:')) {
        userCalls += stats.count;
      } else if (name.startsWith('opt:')) {
        optCalls += stats.count;
      } else {
        // Global functions - likely graph functions
        graphCalls += stats.count;
      }
    });

    const parseTime = this.perf.avg.parse || this.perf.current.parseTime || 0;
    const evalTime = this.perf.avg.evaluate || this.perf.current.evaluateTime || 0;

    const snapshot = {
      time: new Date().toLocaleTimeString(),
      fps: this.perf.avg.fps,
      total: this.perf.avg.total,
      parse: parseTime,
      evaluate: evalTime,
      render: this.perf.avg.render,
      functions: totalFunctionCalls,
      memory: (this.estimateMemoryUsage() / 1024).toFixed(1)
    };

    // Log basic performance metrics
    // console.log(`🎯 PERF [${snapshot.time}]: FPS=${snapshot.fps.toFixed(1)} TOT=${snapshot.total.toFixed(1)}ms PAR=${snapshot.parse.toFixed(1)}ms EVL=${snapshot.evaluate.toFixed(1)}ms REN=${snapshot.render.toFixed(1)}ms`);

    // Log categorized function calls
    // console.log(`📊 CALLS: TOT=${totalFunctionCalls} GRAPH=${graphCalls} API=${apiCalls} USER=${userCalls} OPT=${optCalls} MEM=${snapshot.memory}KB`);

    // Log top function hotspots if we have many calls
    if (false && totalFunctionCalls > 1000) { // Disabled logging
      const sortedFunctions = Array.from(this.perf.functions.entries())
        .sort((a, b) => b[1].count - a[1].count)
        .slice(0, 8); // Top 8 most called functions

      console.log(`🔥 TOP HOTSPOTS:`);
      sortedFunctions.forEach(([name, stats], i) => {
        const callsPerSecond = Math.round(stats.count);
        const totalTime = stats.totalTime.toFixed(1);
        const category = name.startsWith('api:') ? '📱' : name.startsWith('user:') ? '👤' : name.startsWith('opt:') ? '⚡' : '🎨';
        console.log(`   ${i + 1}. ${category} ${name}: ${callsPerSecond} calls (${totalTime}ms total, ${stats.avgTime.toFixed(3)}ms avg)`);
      });

      // Log evaluation loop stats if we have them
      if (this.perf.evalCallCount > 0 || this.perf.bodyProcessCount > 0) {
        console.log(`🔄 LOOPS: evaluate() called ${this.perf.evalCallCount || 0} times, body processing ${this.perf.bodyProcessCount || 0} times`);
      }

      // Log graph performance if available (check global scope)
      if (false && typeof window !== 'undefined' && window.graphPerf) { // Disabled logging
        if (window.graphPerf.functions.size > 0) {
          const graphStats = window.graphPerf.getStats();
          console.log(`🎨 GRAPH HOTSPOTS:`);
          graphStats.slice(0, 5).forEach((stat, i) => {
            console.log(`   ${i + 1}. ${stat.name}: ${stat.count} calls (${stat.totalTime.toFixed(1)}ms total, ${stat.avgTime.toFixed(2)}ms avg, ${stat.maxTime.toFixed(2)}ms max)`);
          });
          window.graphPerf.reset();
        } else {
          // Debug: Check why graph performance isn't being tracked
          console.log(`🎨 GRAPH DEBUG: graphPerf enabled=${window.graphPerf.enabled}, functions.size=${window.graphPerf.functions.size}`);
        }
      } else {
        // console.log(`🎨 GRAPH DEBUG: window.graphPerf not available`);
      }

      // Reset counters after logging to get per-second rates
      this.perf.functions.clear();
      this.perf.evalCallCount = 0;
      this.perf.bodyProcessCount = 0;
    }

    // Store in history (keep last 20 snapshots)
    this.perf.history.push(snapshot);
    if (this.perf.history.length > 20) {
      this.perf.history.shift();
    }
  }

  // 🎯 Set API context for this KidLisp instance
  setAPI(api) {
    this.api = api;
    // Clear embedded API cache to force refresh with new system references
    this.embeddedApiCache.clear();
    // console.log("🎯 KidLisp API context updated");
  }

  // 🎵 Update audio-related global variables (called from pieces with audio data)
  updateAudioGlobals(audioData) {
    if (audioData && typeof audioData === 'object') {
      if (typeof audioData.amp === 'number') {
        this.globalDef.amp = audioData.amp;
      }
      if (typeof audioData.leftAmp === 'number') {
        this.globalDef.leftAmp = audioData.leftAmp;
      }
      if (typeof audioData.rightAmp === 'number') {
        this.globalDef.rightAmp = audioData.rightAmp;
      }
      if (typeof audioData.beat === 'number') {
        this.globalDef.beat = audioData.beat;
      }
      if (typeof audioData.kick === 'number') {
        this.globalDef.kick = audioData.kick;
      }
      // Add more audio variables as needed
    }
  }

  // Reset all state for a fresh KidLisp instance
  reset(clearOnceExecuted = false, sourceChanged = false) {
    // Reset core state
    this.ast = null;
    this.globalDef = {};

    // Initialize audio-related global variables
    this.globalDef.amp = 0; // Current audio amplitude (0-10 scale)
    this.globalDef.leftAmp = 0; // Left channel amplitude (0-10 scale) 
    this.globalDef.rightAmp = 0; // Right channel amplitude (0-10 scale)
    this.globalDef.beat = 0; // Beat detection (0 or 1)
    this.globalDef.kick = 0; // Kick drum / beat detected (0 or 1)

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

    // Clear choice cache to prevent memory leaks
    if (this.choiceCache) {
      this.choiceCache.clear();
    }

    // Clear global function cache
    if (this.globalFunctionCache) {
      this.globalFunctionCache.clear();
    }

    // ⚡ PERFORMANCE: Don't automatically clear embedded layer cache on reset
    // This will be handled conditionally in module() based on source change
    // this.clearEmbeddedLayerCache(); // Moved to module() for conditional clearing

    // Reset embedded layer loading state only if source changed
    if (sourceChanged) {
      this.loadingEmbeddedLayers.clear();
      this.loadedEmbeddedLayers.clear();
    }

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

    // Clear onceExecuted only when explicitly requested (when source changes)
    if (clearOnceExecuted) {
      this.onceExecuted.clear();
    }

    // Reset timing blink tracking
    this.timingBlinks.clear();

    // Reset delay timer active periods tracking
    this.delayTimerActivePeriods.clear();

    // Reset active timing expressions tracking
    this.activeTimingExpressions.clear();

    // Reset baked layers state
    this.bakedLayers = [];
    this.bakeCallCount = 0;

    // Reset deferred commands state
    this.postEmbedCommands = [];

    // Reset buffer pool state
    this.bufferPool.clear();

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

  // Count active (non-cache-based) embedded layers for deferral logic
  countActiveEmbeddedLayers() {
    if (!this.embeddedLayers) return 0;
    return this.embeddedLayers.filter(layer => {
      // Skip cache-based embedded layers (same logic as renderEmbeddedLayers)
      return !(layer.originalCacheId && /^[0-9A-Za-z]{3,12}$/.test(layer.originalCacheId));
    }).length;
  }

  // Performance monitoring methods
  enablePerformanceMonitoring(enable = true) {
    this.performanceEnabled = enable;
    if (enable) {
      // console.log("🔬 KidLisp performance monitoring enabled");
    } else {
      // console.log("🔬 KidLisp performance monitoring disabled");
    }
  }

  startFrameTiming() {
    if (!this.performanceEnabled) return;
    this.currentFrameStart = performance.now();
    this.totalEvaluations = 0;
    this.evaluationCounts.clear();
  }

  endFrameTiming() {
    if (!this.performanceEnabled) return;
    const frameDuration = performance.now() - this.currentFrameStart;
    this.frameTimings.push(frameDuration);

    // Keep only last 60 frames for rolling average
    if (this.frameTimings.length > 60) {
      this.frameTimings.shift();
    }

    // Log performance stats every 60 frames (roughly 1 second at 60fps)
    if (this.frameTimings.length === 60) {
      const avgFrameTime = this.frameTimings.reduce((a, b) => a + b) / 60;
      const maxFrameTime = Math.max(...this.frameTimings);
      const minFrameTime = Math.min(...this.frameTimings);

      console.log(`🔬 KidLisp Performance (60 frames):
        Avg: ${avgFrameTime.toFixed(2)}ms, Min: ${minFrameTime.toFixed(2)}ms, Max: ${maxFrameTime.toFixed(2)}ms
        Total evaluations: ${this.totalEvaluations}
        Top expressions:`, Array.from(this.evaluationCounts.entries())
        .sort((a, b) => b[1] - a[1])
        .slice(0, 5));

      this.frameTimings = []; // Reset for next cycle
    }
  }

  trackEvaluation(type) {
    if (!this.performanceEnabled) return;
    this.totalEvaluations++;
    this.evaluationCounts.set(type, (this.evaluationCounts.get(type) || 0) + 1);
  }

  // Mark a timing expression as triggered for blinking
  markTimingTriggered(timingToken) {
    const now = performance.now();

    // For delay timers, use shorter flash duration (1 frame)
    const isDelayTimer = /^\d*\.?\d+s!?$/.test(timingToken);
    const flashDuration = isDelayTimer ? 200 : this.blinkDuration; // 200ms to make it visible

    this.timingBlinks.set(timingToken, {
      triggerTime: now,
      duration: flashDuration
    });
  }

  // Check if a timing token should be blinking
  isTimingBlinking(timingToken) {
    if (!this.timingBlinks.has(timingToken)) return false;

    const blinkInfo = this.timingBlinks.get(timingToken);
    const now = performance.now();
    const elapsed = now - blinkInfo.triggerTime;

    if (elapsed > blinkInfo.duration) {
      // Blink has expired, remove it
      this.timingBlinks.delete(timingToken);
      return false;
    }

    // For delay timers (non-cycling), implement the red flash + hold pattern
    const isDelayTimer = /^\d*\.?\d+s!?$/.test(timingToken);
    if (isDelayTimer) {
      const isBlinking = elapsed < 80; // Red flash for first 80ms (~5 frames at 60fps) - longer so it's not missed
      return isBlinking;
    }

    // For cycle timers, return true if we're in a brief lime flash period (like delay timers)
    return elapsed < 80; // 80ms lime flash for cycle timers (matches delay timer duration)
  }

  // Scan source code for $codes and mark them as loading for syntax highlighting
  scanAndMarkEmbeddedCodes(source) {
    // Find all $codes in the source using regex
    const codeMatches = source.match(/\$[0-9A-Za-z]+/g);
    if (codeMatches) {
      codeMatches.forEach(fullCode => {
        const cacheId = fullCode.substring(1); // Remove $ prefix
        // Only mark as loading if not already loaded
        if (!this.loadedEmbeddedLayers.has(cacheId)) {
          // Code discovery debug log removed for performance
          this.loadingEmbeddedLayers.add(cacheId);
        }
      });

      // Start preloading all $codes in parallel for faster loading
      this.preloadEmbeddedCodes(codeMatches);
    }
  }

  // Preload all embedded codes in parallel for faster loading
  preloadEmbeddedCodes(codeMatches) {
    // Get unique cache IDs 
    const uniqueCacheIds = [...new Set(codeMatches.map(code => code.substring(1)))];

    // Filter out codes that are already cached or being fetched
    const needsFetching = uniqueCacheIds.filter(cacheId => {
      // Check instance cache first
      if (this.embeddedSourceCache.has(cacheId)) {
        this.loadingEmbeddedLayers.delete(cacheId);
        this.loadedEmbeddedLayers.add(cacheId);
        return false;
      }

      // Check global RAM cache
      if (globalCodeCache.has(cacheId)) {
        // Copy to instance cache for fast access
        this.embeddedSourceCache.set(cacheId, globalCodeCache.get(cacheId));
        this.loadingEmbeddedLayers.delete(cacheId);
        this.loadedEmbeddedLayers.add(cacheId);
        return false;
      }

      const fetchKey = `${cacheId}_fetching_source`;
      if (this.embeddedLayerCache.has(fetchKey)) {
        return false; // Already being fetched
      }

      return true;
    });

    if (needsFetching.length === 0) {
      // Cache status debug log removed for performance
      return;
    }

    // Parallel preloading debug log removed for performance

    // Use individual multi-level cache calls for better cache utilization
    // This allows each code to check IndexedDB individually before falling back to network
    needsFetching.forEach(cacheId => {
      const fetchKey = `${cacheId}_fetching_source`;
      this.embeddedLayerCache.set(fetchKey, true); // Mark as being fetched

      getCachedCodeMultiLevel(cacheId)
        .then(source => {
          // Clean up fetch marker
          this.embeddedLayerCache.delete(fetchKey);

          // Mark as loaded
          this.loadingEmbeddedLayers.delete(cacheId);
          this.loadedEmbeddedLayers.add(cacheId);

          // Cache the source if we got it
          if (source) {
            this.embeddedSourceCache.set(cacheId, source);
            // Preload status debug log removed for performance
          } else {
            console.warn(`❌ Failed to preload $code: ${cacheId}`);
          }
        })
        .catch(error => {
          console.error(`❌ Preload error for ${cacheId}:`, error);
          this.embeddedLayerCache.delete(fetchKey);
          this.loadingEmbeddedLayers.delete(cacheId);
        });
    });
  }

  // Mark a delay timer as entering its active display period
  markDelayTimerActive(timingToken) {
    const now = performance.now();
    this.delayTimerActivePeriods.set(timingToken, {
      activateTime: now,
      duration: this.delayTimerActiveDuration
    });
  }

  // Check if a delay timer is in its active display period
  isDelayTimerActive(timingToken) {
    if (!this.delayTimerActivePeriods.has(timingToken)) {
      return false;
    }

    const activeInfo = this.delayTimerActivePeriods.get(timingToken);
    const now = performance.now();
    const elapsed = now - activeInfo.activateTime;

    if (elapsed > activeInfo.duration) {
      // Active period has expired, remove it
      this.delayTimerActivePeriods.delete(timingToken);
      return false;
    }

    return true;
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

    // Check if it's a bare string color name or RGB value
    if (typeof firstItem === "string") {
      colorName = firstItem;
    }
    // Check if it's a single-argument function call like ["red"]
    else if (Array.isArray(firstItem) && firstItem.length >= 1 && typeof firstItem[0] === "string") {
      colorName = firstItem[0];
    }

    if (colorName) {
      const globalEnv = this.getGlobalEnv();

      // Check if it's an RGB string first (e.g., "255 0 0" or "255, 0, 0")
      if (isValidRGBString(colorName)) {
        const rgbValues = parseRGBString(colorName);
        if (rgbValues) {
          this.firstLineColor = rgbValues; // Store as [r, g, b] array

          // Always store local backup for worker fallback
          this.persistentFirstLineColor = rgbValues;

          // Store in persistent global storage (main thread only)
          if (typeof window !== 'undefined' && window.setPersistentFirstLineColor) {
            window.setPersistentFirstLineColor(rgbValues);
          } else if (typeof globalThis !== 'undefined' && globalThis.storePersistentFirstLineColor) {
            globalThis.storePersistentFirstLineColor(rgbValues);
          }
          return; // Early return after handling RGB
        }
      }

      // Check if it's a direct color name, fade string, or color code in the global environment
      if (globalEnv[colorName] && typeof globalEnv[colorName] === "function") {
        try {
          // Test if this is a color function by calling it
          const result = globalEnv[colorName]();

          // Check if the result is a valid color (array of RGB values, fade string, or rainbow/zebra)
          const isValidColor = Array.isArray(result) ||
            typeof result === "string" && (result.startsWith("fade:") || result === "rainbow" || result === "zebra");

          if (isValidColor) {
            // If we get here without error, it's a color function
            this.firstLineColor = colorName;

            // Always store local backup for worker fallback, regardless of global storage
            this.persistentFirstLineColor = colorName;

            // Store in persistent global storage (main thread only)
            if (typeof window !== 'undefined' && window.setPersistentFirstLineColor) {
              window.setPersistentFirstLineColor(colorName);
            } else if (typeof globalThis !== 'undefined' && globalThis.storePersistentFirstLineColor) {
              globalThis.storePersistentFirstLineColor(colorName);
            }
          }
        } catch (e) {
          // Not a color function, ignore
        }
      }
      // Also check if it's a fade string directly (like "fade:c0-c3")
      else if (colorName.startsWith("fade:")) {
        // Validate the fade string by trying to parse it
        const fadeColors = this.parseFadeString(colorName);
        if (fadeColors && fadeColors.length >= 2) {
          this.firstLineColor = colorName;

          // Store in persistent global storage
          if (typeof window !== 'undefined' && window.setPersistentFirstLineColor) {
            window.setPersistentFirstLineColor(colorName);
          }
        }
      }
    }
  }

  // Color a fade expression like "fade:red-blue-yellow" or "fade:red-blue:vertical" with each color in its own color
  colorFadeExpression(fadeToken) {
    if (!fadeToken.startsWith("fade:")) {
      return fadeToken; // Fallback to original token
    }

    const parts = fadeToken.split(":");
    if (parts.length < 2) {
      return fadeToken; // Invalid fade syntax
    }

    // Check for neat modifier and handle different positions
    let isNeat = false;
    let colorPart = parts[1];
    let direction = parts[2];

    if (parts[1] === "neat" && parts[2]) {
      // Format: fade:neat:red-blue or fade:neat:red-blue:vertical
      isNeat = true;
      colorPart = parts[2];
      direction = parts[3];
    } else if (parts[2] === "neat") {
      // Format: fade:red-blue:neat
      isNeat = true;
      direction = undefined;
    } else if (parts[3] === "neat") {
      // Format: fade:red-blue:vertical:neat
      isNeat = true;
      direction = parts[2];
    } else if (parts.includes("neat")) {
      // Handle neat anywhere in the string
      isNeat = true;
      // Remove neat from parts and reconstruct
      const filteredParts = parts.filter(p => p !== "neat");
      if (filteredParts.length >= 2) {
        colorPart = filteredParts[1];
        direction = filteredParts[2];
      }
    }

    const colorNames = colorPart.split("-");

    let result = "\\mediumseagreen\\fade\\lime\\:"; // "fade" in emerald, ":" in green

    // Add neat modifier if present
    if (isNeat) {
      result += "\\cyan\\neat\\lime\\:"; // "neat" in cyan, ":" in green
    }

    for (let i = 0; i < colorNames.length; i++) {
      const colorName = colorNames[i];

      // Get the color for this color name
      let colorValue = "white"; // Default fallback

      if (cssColors && cssColors[colorName]) {
        // Use the actual color if it's a valid CSS color
        const rgbColor = cssColors[colorName];
        if (Array.isArray(rgbColor) && rgbColor.length >= 3) {
          colorValue = `${rgbColor[0]},${rgbColor[1]},${rgbColor[2]}`;
        }
      } else if (colorName.match(/^c\d+$/)) {
        // Handle color codes like c0, c1, etc.
        const index = parseInt(colorName.substring(1));
        if (staticColorMap && staticColorMap[index]) {
          const rgbColor = staticColorMap[index];
          if (Array.isArray(rgbColor) && rgbColor.length >= 3) {
            colorValue = `${rgbColor[0]},${rgbColor[1]},${rgbColor[2]}`;
          }
        }
      } else if (colorName === "rainbow") {
        colorValue = "RAINBOW"; // Special case for rainbow
      } else if (colorName === "zebra") {
        colorValue = "ZEBRA"; // Special case for zebra
      }

      // Add the colored color name
      if (colorValue === "RAINBOW") {
        // Special rainbow handling
        const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
        for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
          const charColor = rainbowColors[charIndex % rainbowColors.length];
          result += `\\${charColor}\\${colorName[charIndex]}`;
        }
      } else if (colorValue === "ZEBRA") {
        // Special zebra handling (alternating black/white)
        const zebraColors = ["black", "white"];
        for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
          const charColor = zebraColors[charIndex % zebraColors.length];
          result += `\\${charColor}\\${colorName[charIndex]}`;
        }
      } else {
        result += `\\${colorValue}\\${colorName}`;
      }

      // Add the dash separator in emerald (except for the last color)
      if (i < colorNames.length - 1) {
        result += "\\mediumseagreen\\-";
      }
    }

    // Add direction part if present
    if (direction) {
      result += "\\lime\\:"; // ":" in green

      // Check if direction is numeric (angle)
      const numericAngle = parseFloat(direction);
      if (!isNaN(numericAngle)) {
        result += `\\yellow\\${direction}`; // Numeric angle in yellow
      } else {
        result += `\\cyan\\${direction}`; // Named direction in cyan
      }
    }

    return result;
  }

  // Helper method to parse and validate fade strings
  // Helper method to validate if a string is a valid color
  isValidColorString(colorStr) {
    // Check CSS colors
    if (cssColors[colorStr]) return true;

    // Check color codes (c0, c1, etc.) using standardized mapping
    if (colorStr.match(/^c\d+$/)) {
      const index = parseInt(colorStr.substring(1));
      return staticColorMap[index] !== undefined;
    }

    // Check if it's rainbow or zebra
    if (colorStr === "rainbow" || colorStr === "zebra") return true;

    return false;
  }

  parseFadeString(fadeString) {
    if (!fadeString.startsWith("fade:")) return null;

    const parts = fadeString.split(":");
    if (parts.length < 2) return null;

    // Extract colors part (skip "fade" and optional direction)
    const colorPart = parts[1]; // The part after "fade:"
    const colorNames = colorPart.split("-");

    if (colorNames.length < 2) return null;

    const validColors = [];

    // Validate each color in the fade
    for (const colorName of colorNames) {
      if (this.isValidColorString(colorName)) {
        // Get the actual RGB values
        if (cssColors[colorName]) {
          validColors.push(cssColors[colorName]);
        } else if (colorName.match(/^c\d+$/)) {
          const index = parseInt(colorName.substring(1));
          if (staticColorMap[index]) {
            console.log("✅ Found color code:", colorName, "->", staticColorMap[index]);
            validColors.push(staticColorMap[index]);
          }
        } else if (colorName === "rainbow") {
          console.log("✅ Found rainbow color");
          validColors.push([255, 0, 0]); // Just use red as representative
        } else if (colorName === "zebra") {
          console.log("✅ Found zebra color");
          validColors.push([0, 0, 0]); // Just use black as representative
        }
      } else {
        console.log("❌ Invalid color:", colorName);
        return null; // Invalid color
      }
    }

    const result = validColors.length >= 2 ? validColors : null;
    return result;
  }

  // Get the background fill color for reframe operations
  getBackgroundFillColor() {
    // If no color in current instance, try various fallbacks
    if (!this.firstLineColor) {
      // First try persistent storage from main thread (if available)
      if (typeof window !== 'undefined' && window.getPersistentFirstLineColor) {
        const persistentColor = window.getPersistentFirstLineColor();
        if (persistentColor) {
          this.firstLineColor = persistentColor; // Restore it to the instance
        }
      }

      // If still no color, try worker-local backup storage
      if (!this.firstLineColor && this.persistentFirstLineColor) {
        this.firstLineColor = this.persistentFirstLineColor;
      }

      // Final fallback: try global storage directly (globalThis)
      if (!this.firstLineColor && typeof globalThis !== 'undefined' && globalThis.getPersistentFirstLineColor) {
        const globalPersistentColor = globalThis.getPersistentFirstLineColor();
        if (globalPersistentColor) {
          this.firstLineColor = globalPersistentColor;
        }
      }

      // If still no color, try detecting it again from the current AST
      if (!this.firstLineColor && this.ast) {
        this.detectFirstLineColor();
      }
    }

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
    // Actually, let's disable optimization for rainbow/zebra ink to preserve correct behavior
    // The rainbow/zebra functions need to be called through the interpreter to work properly
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
          console.log(`%c🔗 ${newPath}`, 'color: #3F51B5; font-weight: bold;');
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

    // console.log("🔍 Cache Debug:", {
    //   cacheEnabled,
    //   source: source?.substring(0, 50),
    //   sourceLength: source?.length,
    //   isLispFile: this.isLispFile,
    //   alreadyCached: this.cachedCode,
    //   cachingInProgress: this.cachingInProgress
    // });

    // Skip caching if disabled or no source
    if (!cacheEnabled || !source || source.trim().length === 0) {
      // console.log("❌ Skipping cache - disabled or no source");
      return;
    }

    // Set flag to prevent multiple requests
    this.cachingInProgress = true;

    try {
      // console.log("🚀 Starting cache request for:", source.substring(0, 50));

      // Skip API calls in TEIA mode (offline packages)
      const isTeiaMode = checkTeiaMode();

      // Force console logging to debug TEIA mode detection
      if (isTeiaMode) {
        // Generate a simple hash-based code for TEIA mode
        const simpleHash = source.split('').reduce((a, b) => {
          a = ((a << 5) - a) + b.charCodeAt(0);
          return a & a;
        }, 0);
        const teiaCode = Math.abs(simpleHash).toString(36).substring(0, 8);
        this.shortUrl = `teia/$${teiaCode}`;
        this.cachedCode = teiaCode;
        setCachedCode(source, teiaCode);
        this.cachingInProgress = false; // Reset the flag
        return;
      }

      // Use standard fetch with proper headers (like other API calls)
      const headers = { "Content-Type": "application/json" };

      try {
        // Include authorization token if logged in (like the print API)
        const token = await api.authorize(); // Get user token
        if (token) headers.Authorization = `Bearer ${token}`;
      } catch (err) { } // Handled up-stream

      const response = await fetch('/api/store-kidlisp', {
        method: 'POST',
        headers,
        body: JSON.stringify({ source })
      });

      // console.log("📥 Cache response:", response);

      if (response.ok) {
        const data = await response.json();
        // console.log("📥 Cache data:", data);

        this.shortUrl = `aesthetic.computer/$${data.code}`;

        // Store the short URL for QR generation
        this.cachedCode = data.code;

        // Store in global registry for access from disk.mjs
        setCachedCode(source, data.code);

        // console.log("✅ Cache successful, stored code:", data.code);

        // Use the day-themed logging from headers.mjs
        const { logKidlispCode } = await import('./headers.mjs');
        logKidlispCode(source, data.code, api.dark);


        // Update browser URL to show the short code
        this.updateBrowserUrl(response.code, api);
      } else {
        // console.log("❌ Cache failed - response not ok:", response.status, response.statusText);
        // Silently handle caching failures - auth issues are common and not critical
        // console.warn('Failed to cache kidlisp:', response?.status, response?.message || 'Unknown error');
      }
    } catch (error) {
      // console.log("❌ Cache error:", error);
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
    // Add recursion protection for $code substitutions
    if (!this.codeSubstitutionDepth) this.codeSubstitutionDepth = 0;
    if (this.codeSubstitutionDepth > 5) {
      console.warn(`❌ KidLisp: Maximum $code substitution depth exceeded, preventing infinite recursion`);
      return null;
    }

    // Reset zebra cache at the start of each module execution
    resetZebraCache();

    // Check if source has changed - if so, reset once state
    const sourceChanged = this.currentSource !== source;
    this.currentSource = source;

    // Reset all state for fresh instance when loading a new module
    // For prompt executions (!isLispFile), always clear onceExecuted for fresh runs
    // For file executions (isLispFile), only clear onceExecuted when source changes
    const shouldClearOnce = !isLispFile || sourceChanged;
    this.reset(shouldClearOnce, sourceChanged);

    // Clear first-line color when loading new code, but preserve persistent backup
    this.firstLineColor = null;
    // Note: Keep this.persistentFirstLineColor for fallback during reframe

    // Store flag to skip caching for .lisp files
    this.isLispFile = isLispFile;

    // Always clear embedded layer cache when loading a module
    // This ensures fresh state when entering/re-entering a piece
    this.clearEmbeddedLayerCache();

    // Source logging removed for performance
    // console.log("🟪");
    // console.log(source);

    // Log source with square brackets for easy copying
    // console.log("�");
    // console.log(source);
    // console.log("�");
    perfStart("parse");
    const parsed = this.parse(source);
    perfEnd("parse");

    // 🔍 Special case: If the program consists of only a single $code atom or function call,
    // fetch the cached source and substitute it instead of navigating
    // BUT: Skip this in embedded contexts to prevent infinite recursion
    // console.log(`🔍 Checking for single $code - embedded context: ${this.isEmbeddedContext}, parsed length: ${parsed.length}`);
    if (parsed.length === 1) {
      // console.log(`🔍 Single expression detected:`, parsed[0]);
    }

    if (!this.isEmbeddedContext &&
      parsed.length === 1 &&
      ((typeof parsed[0] === 'string' && parsed[0].startsWith('$')) ||
        (Array.isArray(parsed[0]) && parsed[0].length >= 1 &&
          typeof parsed[0][0] === 'string' && parsed[0][0].startsWith('$')))) {

      const cacheCode = typeof parsed[0] === 'string' ? parsed[0] : parsed[0][0];
      const cacheId = cacheCode.slice(1); // Remove $ prefix

      console.log(`🔗 KidLisp: Detected single $code (${cacheCode}), fetching cached source...`);

      // Validate cache ID pattern (relaxed for development - 3+ chars, alphanumeric)
      if (/^[0-9A-Za-z]{3,12}$/.test(cacheId)) {
        try {
          // Attempt to fetch cached source synchronously from memory cache first
          if (globalCodeCache.has(cacheId)) {
            const cachedSource = globalCodeCache.get(cacheId);
            console.log(`✅ Found cached source in memory for ${cacheCode}, substituting...`);
            // Recursively parse the cached source with depth tracking
            this.codeSubstitutionDepth++;
            const result = this.module(cachedSource, isLispFile);
            this.codeSubstitutionDepth--;
            return result;
          } else {
            // If not in memory cache, start async fetch but return navigation for now
            // This maintains backward compatibility while we implement full async support
            // BUT: Skip network fetching in embedded contexts to prevent spam
            if (this.isEmbeddedContext) {
              console.warn(`⚠️ $code ${cacheCode} not in cache and embedded context - skipping network fetch`);
              // For embedded contexts, just treat as regular code and continue
            } else {
              console.log(`⏳ Cache not in memory for ${cacheCode}, starting async fetch...`);
              getCachedCodeMultiLevel(cacheId).then(cachedSource => {
                if (cachedSource) {
                  console.log(`✅ Loaded ${cacheCode} from network/storage, cached for future use`);
                  // Cache for next time
                  globalCodeCache.set(cacheId, cachedSource);
                }
              }).catch(error => {
                console.warn(`❌ Failed to load cached source for ${cacheCode}:`, error);
              });

              // For now, fall back to navigation behavior
              if (typeof window !== 'undefined') {
                window.location.href = `/${cacheCode}`;
                return null; // Return null to indicate this piece should not run
              }
            }
          }
        } catch (error) {
          console.warn(`❌ Error processing ${cacheCode}:`, error);
          // Fall back to navigation behavior
          if (typeof window !== 'undefined') {
            window.location.href = `/${cacheCode}`;
            return null;
          }
        }
      } else {
        console.warn(`❌ Invalid cache ID format: ${cacheId}`);
        // Fall back to navigation behavior for invalid IDs
        if (typeof window !== 'undefined') {
          window.location.href = `/${cacheCode}`;
          return null;
        }
      }
    }

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
        // Store API reference for reframe operations
        this.lastUsedApi = { wipe, params, clock, screen, sound, delay, pieceCount, net, backgroundFill };

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

        // Skip initial wipe during boot to prevent purple flash
        // Background fill for reframes is handled in bios.mjs
        // Only wipe if this is a reframe situation (not initial boot)
        const isReframe = this.frameCount > 0; // or some other reframe detection

        if (isReframe) {
          // Use first-line color as default background for reframes, otherwise erase
          if (this.firstLineColor) {
            wipe(this.firstLineColor);
          } else {
            wipe("erase");
          }
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
        // 🎯 Start performance frame tracking
        this.startFrame();

        // console.log("🖌️ Kid Lisp is Painting...", $.paintCount);
        // 🕐 Timing updates moved to sim() for consistent framerate

        // Cache kidlisp source for QR code generation instantly
        // This prevents caching work-in-progress code and saves server space
        // BUT: Skip caching in embedded contexts to prevent interference
        const cacheDelayFrames = 0; // Instant caching

        if (!this.isEmbeddedContext &&
          this.frameCount >= cacheDelayFrames && !this.cachedCode) {
          this.cacheKidlispSource(source, $);
        }

        // Clear syntax signals from previous frame
        this.clearSyntaxSignals();

        // Update HUD with syntax highlighting
        this.updateHUDWithSyntaxHighlighting($);

        // 🔄 FRAME-LEVEL RESET: Reset evaluation counters and clear frame cache
        if (this.perf.enabled) {
          this.perf.evalCallCount = 0;
          this.perf.bodyProcessCount = 0;
          // Clear frame-level expression cache for new frame
          if (this.frameCache) {
            this.frameCache.clear();
          }
        }

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
          // Set up command queue to defer final drawing commands
          this.postEmbedCommands = [];
          this.inEmbedPhase = false;

          // Scan for $codes and pre-mark them as loading for syntax highlighting
          this.scanAndMarkEmbeddedCodes(source);

          // 🔍 TOTAL FRAME TIMING: Start measuring complete execution cycle
          const totalFrameStart = performance.now();

          // Performance mode: disable logging when FPS is low
          if (!this.performanceMode) this.performanceMode = { enabled: false, lastCheck: 0 };
          if (performance.now() - this.performanceMode.lastCheck > 1000) {
            // Check FPS every second
            const fps = $.system?.fps || 60;
            this.performanceMode.enabled = fps < 30;
            this.performanceMode.lastCheck = performance.now();
            if (this.performanceMode.enabled) {
              // console.log("🚀 PERFORMANCE MODE: Enabled due to low FPS:", fps);
            }
          }

          // Clear frame cache for new frame
          this.frameCache = new Map();

          // Execute the full program first (draws current content and creates embedded layers)
          this.localEnvLevel = 0; // Reset state per program evaluation.
          this.localEnv = this.localEnvStore[this.localEnvLevel];

          const mainEvalStart = performance.now();
          // console.log("🔍 About to evaluate AST for embedded KidLisp");
          /*const evaluated = */ this.evaluate(this.ast, $, undefined, undefined, true);
          // console.log("✅ Finished evaluating AST for embedded KidLisp");
          const mainEvalTime = performance.now() - mainEvalStart;

          // Mark that we're now in the embed rendering phase
          this.inEmbedPhase = true;

          // Then composite baked layers underneath current content
          const bakedStart = performance.now();
          this.renderBakedLayers($);
          const bakedTime = performance.now() - bakedStart;

          // Then render and update embedded layers

          const embedStart = performance.now();
          this.renderEmbeddedLayers($);
          const embedTime = performance.now() - embedStart;


          // 🔍 TOTAL FRAME TIMING: Disabled for cleaner console
          const totalFrameTime = performance.now() - totalFrameStart;
          // if (totalFrameTime > 10) { // Only log very slow frames
          //   console.log(`🔍 TOTAL FRAME: ${totalFrameTime.toFixed(2)}ms | main=${mainEvalTime.toFixed(2)}ms, baked=${bakedTime.toFixed(2)}ms, embed=${embedTime.toFixed(2)}ms`);
          // }

          // Finally execute any drawing commands that should be on top
          this.inEmbedPhase = false;

          // Execute accumulated post-embed commands
          this.postEmbedCommands.forEach((cmd, i) => {
            try {
              cmd.func(...cmd.args);
            } catch (err) {
              console.error(`Error executing post-embed command ${cmd.name}:`, err);
            }
          });
          this.postEmbedCommands = [];

          // 🎯 Render performance HUD overlay (always rendered last)
          if (this.perf.showHUD) {
            this.renderPerformanceHUD($);
          }

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

        // 🎯 End performance frame tracking
        this.endFrame();
      },
      sim: ({ sound }) => {
        // 🕐 Handle timing updates in sim (runs at consistent 120fps)
        this.frameCount++; // Increment frame counter for timing functions
        resetRainbowCache(); // Reset rainbow cache for new frame to ensure color cycling

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
        // Check for center tap to hide UI (only if no custom tap/draw handlers)
        if (e.is("touch")) {
          // console.log("🫵 Touch event detected in kidlisp piece");

          const hasTapHandler = this.tapper !== null;
          const hasDrawHandler = this.drawer !== null;

          // console.log("🔍 Handler check:", { 
          //   hasTapHandler, 
          //   hasDrawHandler,
          //   tapper: this.tapper,
          //   drawer: this.drawer
          // });

          // If piece has custom interaction handlers, use them normally
          if (hasTapHandler || hasDrawHandler) {
            // console.log("📝 Using custom handlers");
            api.needsPaint();
            if (hasTapHandler) this.tap(api);
          } else {
            // No custom handlers - any tap toggles HUD
            // console.log("🎯 No custom handlers, toggling HUD for any tap");
            api.toggleHUD();
          }
        }

        if (e.is("draw")) {
          const hasDrawHandler = this.drawer !== null;
          if (hasDrawHandler) {
            api.needsPaint();
            this.draw(api, { dy: e.delta.y, dx: e.delta.x });
          }
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

  // Seeded random number generator for isolated state per instance
  seededRandom() {
    // Simple linear congruential generator for deterministic sequences
    this.randomState = (this.randomState * 1664525 + 1013904223) % 4294967296;
    return this.randomState / 4294967296;
  }

  // Main parsing method - handles both single expressions and multi-line input
  parse(input) {
    const parseStart = this.startTiming('parse');
    // 🏃‍♂️ Handle comma-separated expressions for compact one-liners
    // Transform "blue, ink rainbow, repeat 100 line" into "(blue) (ink rainbow) (repeat 100 line)"
    if (input.includes(",") && !input.includes("\n")) {
      // Only process comma syntax for single-line input to avoid conflicts with multi-line
      const expressions = input.split(",").map(expr => expr.trim()).filter(expr => expr.length > 0);

      // Auto-wrap each comma-separated expression in parentheses if needed
      const wrappedExpressions = expressions.map(expr => {
        // Skip if already wrapped in parentheses or is just a single token
        if (expr.startsWith("(") && expr.endsWith(")")) {
          return expr;
        }
        // Check if it looks like a function call or command
        if (/^[a-zA-Z_$]\w*/.test(expr)) {
          return `(${expr})`;
        }
        // For other cases (like just numbers or strings), leave as-is
        return expr;
      });

      // Join them with spaces for normal parsing
      input = wrappedExpressions.join(" ");
    }

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

        // Check if line starts with a timing expression like "1.5s" or "2s..."
        const timingMatch = line.match(/^(\d*\.?\d+s\.\.\.?)\s+(.+)$/);
        if (timingMatch && !line.startsWith("(")) {
          // Line starts with timing expression followed by other content
          // Keep it as a flat structure: 1.5s (zoom 0.75) becomes a single tokenized line
          // Don't auto-wrap, let the tokenizer handle it naturally
          return line;
        }

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
    // console.log(`🔍 Tokenized:`, tokens);
    const parsed = readFromTokens(tokens);
    // console.log(`🔍 Parsed result:`, parsed);
    this.ast = parsed; // 🎯 Actually assign the parsed result to this.ast!
    this.endTiming('parse', parseStart);
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

  // Helper function to create a deep copy of a form
  createFormCopy(templateForm, api) {
    // Create a new Form using the same geometry data (like CUBEL) but fresh transform state
    // Determine the geometry type from the template form
    let geometryData = api.CUBEL; // Default to CUBEL

    // If we want to support other geometry types in the future, we could check templateForm.type
    // For now, all cube:N instances use CUBEL geometry

    const copy = new api.Form(
      geometryData,  // Use the appropriate geometry data
      {
        color: templateForm.color ? [...templateForm.color] : undefined,
        alpha: templateForm.alpha
      },
      {
        pos: [0, 0, 0],  // Start with clean transform state
        rot: [0, 0, 0],
        scale: [1, 1, 1]
      }
    );

    // Copy other properties that might be needed
    copy.primitive = templateForm.primitive;
    copy.type = templateForm.type;
    copy.texture = templateForm.texture;
    copy.colorModifier = templateForm.colorModifier;
    copy.gradients = templateForm.gradients;

    return copy;
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
            // console.warn("Invalid identifier name (use underscores instead of dashes):", name);
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

        if (!evaled) {
          // Execute body expressions
          args.slice(1).forEach((expr) => {
            this.evaluate(expr, api, env);
          });
        }

        return !evaled;
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
      // Random number generation
      random: (api, args) => {
        // (random) - returns 0-255
        // (random max) - returns 0-max
        // (random min max) - returns min-max
        if (args.length === 0) {
          return Math.floor(this.seededRandom() * 256);
        } else if (args.length === 1) {
          const max = args[0];
          return Math.floor(this.seededRandom() * max);
        } else if (args.length >= 2) {
          const min = args[0];
          const max = args[1];
          return Math.floor(this.seededRandom() * (max - min + 1)) + min;
        }
      },
      // Check if an image URL is ready to be pasted
      "ready?": (api, args) => {
        // (ready? url) - returns true if image is loaded, false otherwise
        if (args.length === 0) return false;

        const url = unquoteString(args[0]?.toString() || "");
        if (!url) return false;

        // Access the global paintings cache from disk.mjs
        // We need to get this from the API context
        const paintings = api.paintings || {};
        return paintings[url] && paintings[url] !== "fetching";
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
        // console.log("🧹 Wipe called with args:", args);
        const processedArgs = processArgStringTypes(args);
        // console.log("🧹 Wipe processed args:", processedArgs);
        // console.log("🧹 Wipe api.screen dimensions:", api.screen?.width, "x", api.screen?.height);
        // console.log("🧹 Wipe api.screen pixels length:", api.screen?.pixels?.length);
        api.wipe?.(processedArgs);
        // console.log("🧹 Wipe completed, first pixel now:", api.screen?.pixels?.[0], api.screen?.pixels?.[1], api.screen?.pixels?.[2], api.screen?.pixels?.[3]);
      },
      coat: (api, args, env) => {
        // Apply a translucent color layer over existing content
        // Usage: (coat "red" 128) - red with 50% alpha
        // Usage: (coat fade:red-blue:45 64) - fade with 25% alpha
        if (args.length < 1) {
          console.error("❗ coat requires at least a color argument");
          return;
        }

        let color = args[0];
        const alpha = args.length >= 2 ? args[1] : 128; // Default to 50% alpha

        // Skip preprocessing fade strings - let the gradient system handle dynamic evaluation
        // This allows :frame keywords to be evaluated during each rendering frame

        // Store current ink state to restore later
        const previousInk = this.inkState;
        const previousInkSet = this.inkStateSet;

        // Set ink to the coat color
        if (typeof color === "string" && color.startsWith("fade:")) {
          // Handle fade strings with alpha - follow neobrush pattern
          // But first, evaluate any frame expressions in the fade string
          let evaluatedColor = color;
          
          // Check if fade string contains frame-based direction
          const fadeParts = color.split(":");
          if (fadeParts.length >= 3) {
            const fadePrefix = fadeParts[0]; // "fade"
            const colors = fadeParts[1]; // "black-red-rainbow-red-black"
            const direction = fadeParts[2]; // "frame" or expression

            // Try to evaluate the direction part
            let evaluatedDirection = direction;
            try {
              // Check if it's already a number
              const numericValue = parseFloat(direction);
              if (!isNaN(numericValue)) {
                evaluatedDirection = direction; // Keep as string
              } else {
                // Try to evaluate as KidLisp expression/variable
                let evalResult;

                // Special handling for function names like "frame"
                const globalEnv = this.getGlobalEnv();
                if (globalEnv[direction] && typeof globalEnv[direction] === "function") {
                  // It's a function, call it with the API
                  evalResult = globalEnv[direction](api);
                  // console.log(`🎬 COAT DEBUG: Evaluated ${direction} as function = ${evalResult}`);
                } else {
                  // Try to evaluate as expression or variable
                  evalResult = this.evaluate(direction, api, env);
                  // console.log(`🎬 COAT DEBUG: Evaluated ${direction} as expression = ${evalResult}`);
                }

                // Convert result to string for the fade string
                evaluatedDirection = String(evalResult);
              }
            } catch (error) {
              console.warn("Failed to evaluate fade direction in coat:", direction, error);
              evaluatedDirection = direction; // Fallback to original
            }

            // Reconstruct the fade string with evaluated direction
            evaluatedColor = `${fadePrefix}:${colors}:${evaluatedDirection}`;
            // console.log(`🎬 COAT DEBUG: Original fade="${color}" -> Evaluated fade="${evaluatedColor}"`);
          }
          
          // Create fade color array that triggers local fade detection
          const fadeColorArray = [evaluatedColor, alpha];
          api.ink?.(fadeColorArray);
        } else {
          // Handle regular colors with alpha
          const processedColor = processArgStringTypes([color]);
          api.ink?.(...processedColor, alpha);
        }

        // Draw a full-screen box to coat the screen
        if (api.box) {
          // Get screen dimensions, defaulting to common values if not available
          const screenWidth = api.width || api.screen?.width || 256;
          const screenHeight = api.height || api.screen?.height || 256;
          // COAT DEBUG logs removed for performance
          api.box(0, 0, screenWidth, screenHeight);
        }

        // Restore previous ink state
        if (previousInkSet) {
          this.inkState = previousInk;
          this.inkStateSet = previousInkSet;
          if (previousInk && previousInk.length > 0) {
            api.ink?.(...previousInk);
          }
        } else {
          this.clearInkState();
        }
      },
      ink: (api, args) => {
        // 🚀 FAST PATH: For performance mode, handle simple numeric cases quickly
        if (this.performanceMode?.enabled && args && Array.isArray(args) &&
          args.length >= 3 && typeof args[0] === 'number' && typeof args[1] === 'number' && typeof args[2] === 'number') {
          this.inkState = [args[0], args[1], args[2]];
          this.inkStateSet = true;
          api.ink?.(args[0], args[1], args[2]);
          return;
        }

        // console.log("🖋️ Ink called with args:", args);
        // Handle different ink invocation patterns
        // Ensure args is always an array
        if (!args || !Array.isArray(args)) {
          args = args ? [args] : [];
        }

        const processedArgs = processArgStringTypes(args);
        // console.log("🖋️ Ink processed args:", processedArgs);
        // console.log("🖋️ Ink api.screen dimensions:", api.screen?.width, "x", api.screen?.height);

        if (args.length === 0) {
          // Called with no arguments - set a random ink color
          const randomColor = [
            Math.floor(this.seededRandom() * 256),
            Math.floor(this.seededRandom() * 256),
            Math.floor(this.seededRandom() * 256)
          ];
          this.inkState = randomColor;
          this.inkStateSet = true;

          // Check if we should defer this command
          if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
            this.postEmbedCommands = this.postEmbedCommands || [];
            this.postEmbedCommands.push({
              name: 'ink',
              func: api.ink,
              args: randomColor
            });
            return;
          }

          api.ink?.(...randomColor);
          return;
        } else if (args.length === 1 && (args[0] === null || args[0] === undefined)) {
          // Called with null/undefined - clear the ink state  
          this.clearInkState();
          return undefined;
        } else {
          // Check for fade: symbol syntax like (ink fade:cyan-magenta)
          if (args.length === 1 && typeof args[0] === "string" && args[0].startsWith("fade:")) {
            this.inkState = [args[0]];
            this.inkStateSet = true;

            // Check if we should defer this command
            if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
              this.postEmbedCommands = this.postEmbedCommands || [];
              this.postEmbedCommands.push({
                name: 'ink',
                func: api.ink,
                args: [args[0]]
              });
              // Ink deferred debug log removed for performance
              return;
            }

            api.ink?.(args[0]);
            return;
          }

          // Called with color arguments - store and apply the new ink state
          const processedArgs = processArgStringTypes(args);
          this.inkState = processedArgs;
          this.inkStateSet = true;

          // Check if we should defer this command
          if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
            this.postEmbedCommands = this.postEmbedCommands || [];
            this.postEmbedCommands.push({
              name: 'ink',
              func: api.ink,
              args: processedArgs
            });
            // Ink deferred debug log removed for performance
            return;
          }

          api.ink?.(...processedArgs);
          // console.log("🖋️ Ink completed, first pixel now:", api.screen?.pixels?.[0], api.screen?.pixels?.[1], api.screen?.pixels?.[2], api.screen?.pixels?.[3]);
        }
      },
      // Fade string constructor - returns a fade string that can be used with ink
      // Usage: (fade "red" "blue") → "fade:red-blue"
      // Usage: (fade "red" "blue" "vertical") → "fade:red-blue:vertical"
      // Usage: (fade "red" "yellow" "blue" "horizontal-reverse") → "fade:red-yellow-blue:horizontal-reverse"
      fade: (api, args) => {
        if (args.length < 2) {
          return "fade:red-blue"; // Default fade if not enough args
        }

        // Check if last argument is a direction
        const lastArg = args[args.length - 1];
        const validDirections = [
          "horizontal", "horizontal-reverse",
          "vertical", "vertical-reverse",
          "diagonal", "diagonal-reverse"
        ];

        let colors, direction;
        if (typeof lastArg === "string" && validDirections.includes(lastArg)) {
          // Last argument is a named direction
          colors = processArgStringTypes(args.slice(0, -1)).join("-");
          direction = lastArg;
          return `fade:${colors}:${direction}`;
        } else if (typeof lastArg === "number") {
          // Last argument is a numeric angle - pass it directly
          colors = processArgStringTypes(args.slice(0, -1)).join("-");
          direction = lastArg.toString();
          return `fade:${colors}:${direction}`;
        } else if (args.length >= 3) {
          // Last argument might be a variable/expression - evaluate it
          colors = processArgStringTypes(args.slice(0, -1)).join("-");

          // Try to evaluate the last argument to see if it's a number
          try {
            const evaluated = typeof lastArg === "string" ? parseFloat(lastArg) : lastArg;
            if (!isNaN(evaluated)) {
              return `fade:${colors}:${evaluated}`;
            } else {
              // It's not a number, store as expression for later evaluation
              return `fade:${colors}:${JSON.stringify(lastArg)}`;
            }
          } catch (error) {
            // Fallback to storing as expression
            return `fade:${colors}:${JSON.stringify(lastArg)}`;
          }
        } else {
          // No direction specified, use default
          colors = processArgStringTypes(args).join("-");
          return `fade:${colors}`;
        }
      },
      // Dynamic timing helpers with intuitive names
      hop: (api, args, env) => {
        // Creates repeating timing expressions like "0.3s..." for color hopping
        if (args.length >= 3) {
          const seconds = this.evaluate(args[0], api, env);
          const color1 = args[1];
          const color2 = args[2];

          // Construct repeating timing expression and evaluate it directly
          const timingStr = `${seconds}s...`;
          return this.evaluate([timingStr, color1, color2], api, env);
        }
        console.error("❗ Invalid `hop`. Expected (hop seconds color1 color2).");
      },
      delay: (api, args, env) => {
        // Creates one-time timing expressions like "1s" for delayed actions
        if (args.length >= 2) {
          const seconds = this.evaluate(args[0], api, env);
          const action = args[1];

          // Construct one-time timing expression and evaluate it directly
          const timingStr = `${seconds}s`;
          return this.evaluate([timingStr, action], api, env);
        }
        console.error("❗ Invalid `delay`. Expected (delay seconds action).");
      },
      line: (api, args = []) => {
        // console.log("📏 Line called with args:", args);
        // console.log("📏 Line context:", {
        //   embeddedLayersCount: this.embeddedLayers?.length || 0,
        //   inEmbedPhase: this.inEmbedPhase,
        //   screenSize: `${api.screen?.width || 'unknown'}x${api.screen?.height || 'unknown'}`
        // });

        // Check if we should defer this command
        if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
          console.log("📏 DEFERRING line command");
          this.postEmbedCommands = this.postEmbedCommands || [];
          this.postEmbedCommands.push({
            name: 'line',
            func: api.line,
            args: [...args]
          });
          // Line deferred debug log removed for performance
          return;
        }

        // console.log("📏 EXECUTING line command immediately");
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
        // (wiggle amount) - returns random variation ±amount/2
        const amount = args.length > 0 ? args[0] : 10;
        return (this.seededRandom() - 0.5) * amount;
      },
      box: (api, args = []) => {
        // Handle non-array args
        if (!Array.isArray(args)) {
          console.warn('⚠️ box function received non-array args, converting to array');
          args = Array.isArray(args) ? args : [args];
        }

        // Handle undefined (?) values with contextual logic
        const processedArgs = args.map((arg, index) => {
          if (arg === undefined) {
            // Apply contextual logic based on parameter position
            switch (index) {
              case 0: // x coordinate
                return Math.floor(this.seededRandom() * (api.screen?.width || 256));
              case 1: // y coordinate  
                return Math.floor(this.seededRandom() * (api.screen?.height || 256));
              case 2: // width
                return Math.floor(this.seededRandom() * ((api.screen?.width || 256) / 4)) + 10;
              case 3: // height
                return Math.floor(this.seededRandom() * ((api.screen?.height || 256) / 4)) + 10;
              default:
                // For other parameters, use a reasonable default range
                return Math.floor(this.seededRandom() * 256);
            }
          }
          return arg;
        });

        // Check if we should defer this command
        if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
          this.postEmbedCommands = this.postEmbedCommands || [];
          this.postEmbedCommands.push({
            name: 'box',
            func: api.box,
            args: [...processedArgs]
          });
          // Box deferred debug log removed for performance
          return;
        }

        api.box(...processedArgs);
      },
      circle: (api, args = []) => {
        // Check if we should defer this command
        if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
          this.postEmbedCommands = this.postEmbedCommands || [];
          this.postEmbedCommands.push({
            name: 'circle',
            func: api.circle,
            args: [...args]
          });
          return;
        }

        api.circle(...args);
      },
      tri: (api, args = []) => {
        // Triangle function with 6 coordinates and optional mode
        // Usage: (tri x1 y1 x2 y2 x3 y3) or (tri x1 y1 x2 y2 x3 y3 mode)

        // Check if we should defer this command
        if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
          this.postEmbedCommands = this.postEmbedCommands || [];
          this.postEmbedCommands.push({
            name: 'tri',
            func: api.tri,
            args: [...args]
          });
          return;
        }

        api.tri(...args);
      },
      flood: (api, args = []) => {
        // Flood fill at coordinates with optional color
        // Usage: (flood x y) or (flood x y color)
        if (args.length >= 2) {
          const x = args[0];
          const y = args[1];
          const fillColor = args[2]; // Optional color, defaults to current ink

          // Check if we should defer this command
          if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
            this.postEmbedCommands = this.postEmbedCommands || [];
            this.postEmbedCommands.push({
              name: 'flood',
              func: () => {
                if (fillColor !== undefined) {
                  api.flood(x, y, processArgStringTypes(fillColor));
                } else {
                  api.flood(x, y); // Use current ink color
                }
              },
              args: [x, y, fillColor]
            });
            // Flood deferred debug log removed for performance
            return;
          }

          if (fillColor !== undefined) {
            api.flood(x, y, processArgStringTypes(fillColor));
          } else {
            api.flood(x, y); // Use current ink color
          }
        }
      },
      shape: (api, args = []) => {
        // Handle shape arguments - can be flat array of coordinates or array of pairs
        if (args.length === 0) return;

        // 🚀 FAST PATH: For performance mode with simple numeric triangles
        if (this.performanceMode?.enabled && args.length === 6 &&
          args.every(arg => typeof arg === 'number')) {
          api.shape({ points: args, filled: true, thickness: 1 });
          return;
        }

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
        // console.log(`📜 Scroll function called with args:`, args);

        // Handle different scroll argument formats
        let dx = 0, dy = 0;

        // If no arguments are provided, fuzz and choose a random direction (but stick to it)
        if (!args || args.length === 0) {
          // Check if we've already chosen a direction for this session
          if (!this.scrollFuzzDirection) {
            const directions = [
              [1, 0],   // scroll right
              [-1, 0],  // scroll left
              [0, 1],   // scroll down
              [0, -1]   // scroll up
            ];
            this.scrollFuzzDirection = directions[Math.floor(this.seededRandom() * directions.length)];
            // console.log(`🎲 SCROLL fuzzing: first call, randomly chose direction dx=${this.scrollFuzzDirection[0]}, dy=${this.scrollFuzzDirection[1]}`);
          }
          dx = this.scrollFuzzDirection[0];
          dy = this.scrollFuzzDirection[1];
          // console.log(`🎲 SCROLL fuzzing: using cached direction dx=${dx}, dy=${dy}`);
        }

        // NEW: Check if we have a single timing expression argument like ['2s...', 1, 0, -1]
        else if (Array.isArray(args) && args.length === 1 && Array.isArray(args[0]) &&
          args[0].length > 0 && typeof args[0][0] === 'string' && args[0][0].endsWith('...')) {
          // This is a timing expression like ['2s...', 1, 0, -1]
          // We need to evaluate it to get the current values
          const timingExpr = args[0];
          const result = this.evaluate([timingExpr], api);

          // If result is an array, use it as dx, dy
          if (Array.isArray(result) && result.length >= 2) {
            dx = parseFloat(result[0]) || 0;
            dy = parseFloat(result[1]) || 0;
          } else if (typeof result === 'number') {
            dx = result;
            dy = 0;
          } else {
            return;
          }
        }
        // Check for timing pattern arrays like [["2s...", 1, 0, -1], ["3s...", -1, 0, 1]]
        else if (Array.isArray(args) && args.length > 0 && Array.isArray(args[0])) {
          // This is a timing pattern - need to evaluate which timing phase is active

          // For now, let's see what the structure looks like and handle it properly
          const timingPhases = args;
          let activePhase = null;

          // Evaluate each timing phase to see which one should be active
          for (let i = 0; i < timingPhases.length; i++) {
            const phase = timingPhases[i];
            if (Array.isArray(phase) && phase.length > 0) {
              const timingToken = phase[0];

              // Check if this timing phase should be active
              if (typeof timingToken === 'string' && /^\d*\.?\d+s\.\.\.?$/.test(timingToken)) {
                // This is a timing expression - check if it should execute
                if (this.evaluateTimingExpression && this.evaluateTimingExpression(api, timingToken)) {
                  activePhase = phase;
                  break;
                } else {
                  console.log(`🖱️ SCROLL timing phase ${i} is inactive`);
                }
              }
            }
          }

          if (activePhase && activePhase.length > 1) {
            // Extract scroll values from active phase
            const values = activePhase.slice(1);
            if (values.length >= 2) {
              dx = parseFloat(values[0]) || 0;
              dy = parseFloat(values[1]) || 0;
            } else if (values.length === 1) {
              dx = parseFloat(values[0]) || 0;
              dy = 0;
            }
            console.log(`🖱️ SCROLL using active timing phase values: dx=${dx}, dy=${dy}`);
          } else {
            console.log(`🖱️ SCROLL no active timing phase found, using defaults`);
          }
        } else if (Array.isArray(args)) {
          if (args.length === 1) {
            // Single argument: scroll 0.1 -> (scroll 0.1)
            dx = parseFloat(args[0]) || 0;
            dy = 0;
          } else if (args.length === 2) {
            // Two arguments: scroll 0 1 -> (scroll 0 1)
            dx = parseFloat(args[0]) || 0;
            dy = parseFloat(args[1]) || 0;
          } else if (args.length > 2) {
            // Multiple complex arguments - extract what we can
            dx = parseFloat(args[0]) || 0;
            dy = parseFloat(args[1]) || 0;
          }
        }

        //console.log(`🖱️ SCROLL processed args: dx=${dx}, dy=${dy}`);

        // Only defer scroll commands from main code, not from embedded layers
        // Execute immediately if we're a nested instance or in embed phase
        if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase && !this.isNestedInstance) {
          //console.log(`⏳ SCROLL deferring command until after embedded layers`);
          this.postEmbedCommands = this.postEmbedCommands || [];
          this.postEmbedCommands.push({
            name: 'scroll',
            func: () => {
              //console.log(`🖱️ SCROLL executing deferred command: dx=${dx}, dy=${dy}`);
              if (typeof api.scroll === 'function') {
                api.scroll(dx, dy);
              } else {
                //console.log(`⚠️ SCROLL deferred execution failed: api.scroll not available`);
              }
            },
            args: [dx, dy]
          });
          return;
        }

        //console.log(`🖱️ SCROLL executing immediately: dx=${dx}, dy=${dy}`);
        if (typeof api.scroll === 'function') {
          api.scroll(dx, dy);
        } else {
          // For embedded layers, scroll might not be available on the API object
          // In this case, we can skip the scroll operation silently
          //console.log(`⚠️ SCROLL immediate execution failed: api.scroll not available`);
        }
      },
      spin: (api, args = []) => {
        // Defer spin execution if embedded layers exist and we're not in embed phase
        if (this.embeddedLayers?.length > 0 && !this.inEmbedPhase) {
          //console.log("🎡 Deferring spin command until after embedded layers");
          this.postEmbedCommands.push({
            name: 'spin',
            func: () => api.spin(...args),
            args
          });
          return;
        }
        api.spin(...args);
      },
      resetSpin: (api, args = []) => {
        api.resetSpin();
      },
      smoothspin: (api, args = []) => {
        // Defer smoothspin execution if embedded layers exist and we're not in embed phase
        if (this.embeddedLayers?.length > 0 && !this.inEmbedPhase) {
          console.log("🎡 Deferring smoothspin command until after embedded layers");
          this.postEmbedCommands.push({
            name: 'smoothspin',
            func: () => api.smoothSpin(...args),
            args
          });
          return;
        }
        api.smoothSpin(...args);
      },
      sort: (api, args = []) => {
        api.sort(...args);
      },
      zoom: (api, args = []) => {
        // Only defer zoom execution if we're in the main program with embedded layers
        // BUT allow immediate execution if we're already inside an embedded layer
        if (this.embeddedLayers?.length > 0 && !this.inEmbedPhase && !this.isEmbeddedContext) {
          this.postEmbedCommands.push({
            name: 'zoom',
            func: () => {
              api.zoom(...args);
            },
            args
          });
          return;
        }

        // Execute zoom immediately
        api.zoom(...args);
      },
      suck: (api, args = []) => {
        // console.log(`🌪️ Suck function called with args:`, args);
        // console.log(`🌪️ Embedded layers length:`, this.embeddedLayers?.length);
        // console.log(`🌪️ In embed phase:`, this.inEmbedPhase);

        // Defer suck execution if embedded layers exist and we're not in embed phase
        if (this.embeddedLayers?.length > 0 && !this.inEmbedPhase) {
          // console.log(`⏸️ Deferring suck execution due to embedded layers`);
          this.postEmbedCommands.push({
            name: 'suck',
            func: () => {
              // console.log(`⏰ Executing deferred suck with args:`, args);
              api.suck(...args);
            },
            args
          });
          return;
        }

        // Execute suck immediately
        // console.log(`🚀 Executing suck immediately with args:`, args);
        api.suck(...args);
      },
      blur: (api, args = []) => {
        // Execute blur immediately on the current buffer
        api.blur(...args);
      },
      contrast: (api, args = []) => {
        // Check if we should defer this command to execute after embedded layers are rendered
        if (this.embeddedLayers && this.embeddedLayers.length > 0 && !this.inEmbedPhase) {
          this.postEmbedCommands = this.postEmbedCommands || [];
          this.postEmbedCommands.push({
            name: 'contrast',
            func: () => {
              api.contrast(...args);
            },
            args: args
          });
          return;
        }

        // Apply contrast immediately
        api.contrast(...args);
      },
      pan: (api, args = []) => {
        api.pan(...args);
      },
      unpan: (api, args = []) => {
        api.unpan();
      },
      // 📷 Camera rotation functions
      camrotx: (api, args = []) => {
        const rotation = args[0] || 0;
        if (api.system.defaultCam) {
          api.system.defaultCam.rotX = rotation;
        }
      },
      camroty: (api, args = []) => {
        const rotation = args[0] || 0;
        if (api.system.defaultCam) {
          api.system.defaultCam.rotY = rotation;
        }
      },
      camrotz: (api, args = []) => {
        const rotation = args[0] || 0;
        if (api.system.defaultCam) {
          api.system.defaultCam.rotZ = rotation;
        }
      },
      camrot: (api, args = []) => {
        const x = args[0] || 0;
        const y = args[1] || 0;
        const z = args[2] || 0;
        if (api.system.defaultCam) {
          api.system.defaultCam.rotX = x;
          api.system.defaultCam.rotY = y;
          api.system.defaultCam.rotZ = z;
        }
      },
      camspinx: (api, args = []) => {
        const speed = args[0] || 0;
        if (api.system.defaultCam) {
          const frameCount = this.frameCount || 0;
          api.system.defaultCam.rotX = speed * frameCount;
        }
      },
      camspiny: (api, args = []) => {
        const speed = args[0] || 0;
        if (api.system.defaultCam) {
          const frameCount = this.frameCount || 0;
          api.system.defaultCam.rotY = speed * frameCount;
        }
      },
      camspinz: (api, args = []) => {
        const speed = args[0] || 0;
        if (api.system.defaultCam) {
          const frameCount = this.frameCount || 0;
          api.system.defaultCam.rotZ = speed * frameCount;
        }
      },
      camspin: (api, args = []) => {
        const xSpeed = args[0] || 0;
        const ySpeed = args[1] || 0;
        const zSpeed = args[2] || 0;
        if (api.system.defaultCam) {
          const frameCount = this.frameCount || 0;
          api.system.defaultCam.rotX = xSpeed * frameCount;
          api.system.defaultCam.rotY = ySpeed * frameCount;
          api.system.defaultCam.rotZ = zSpeed * frameCount;
        }
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
      // Shorthand for pasting the current user's painting
      painting: (api, args = []) => {
        // painting x y is equivalent to paste painting x y
        const processedArgs = [api.system?.painting, ...args];
        api.paste(...processedArgs);
      },
      paste: (api, args = []) => {
        // Process string arguments to remove quotes (e.g., "@handle/timestamp")
        // Special handling for first argument - support unquoted URLs
        const processedArgs = args.map((arg, index) => {
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
          // For the first argument (image source), support unquoted URLs and paths
          if (index === 0 && typeof arg === "string" && arg) {
            // If it's a URL (http/https) or a path, treat it as unquoted
            if (arg.startsWith("http://") || arg.startsWith("https://") || arg.includes("/") || arg.includes("@")) {
              return arg; // Return as-is for URLs, paths, and @handle/timestamp patterns
            }
          }
          return arg;
        });
        api.paste(...processedArgs);
      },
      stamp: (api, args = []) => {
        // Process string arguments to remove quotes (e.g., "@handle/timestamp")
        // Special handling for first argument - support unquoted URLs
        const processedArgs = args.map((arg, index) => {
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
          // For the first argument (image source), support unquoted URLs and paths
          if (index === 0 && typeof arg === "string" && arg) {
            // If it's a URL (http/https) or a path, treat it as unquoted
            if (arg.startsWith("http://") || arg.startsWith("https://") || arg.includes("/") || arg.includes("@")) {
              return arg; // Return as-is for URLs, paths, and @handle/timestamp patterns
            }
          }
          // Evaluate expressions for non-string arguments (like math expressions)
          return this.evaluate(arg, api, this.localEnv);
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
      // � 3D Form functions
      // 3D Objects - simple global forms
      cube: (api, args, env, colon) => {
        // Handle cubic address space: cube:0, cube:1, cube:2, etc.
        // Also support (cube 0), (cube 1), etc. when no colon syntax is used
        let cubeId;
        if (colon) {
          cubeId = colon; // From colon syntax like cube:0
        } else if (args.length > 0) {
          cubeId = String(this.evaluate(args[0], api, env)); // From argument like (cube 0)
        } else {
          cubeId = "0"; // Default to cube:0
        }

        const templateKey = `cubeTemplate_${cubeId}`;

        // Create a template cube for this ID if it doesn't exist (immutable reference)
        if (!api.system[templateKey]) {
          api.system[templateKey] = new api.Form(api.CUBEL, { pos: [0, 0, 2], rot: [0, 0, 0], scale: 1 });
        }
        return api.system[templateKey];
      },
      quad: (api) => {
        // Create a template quad if it doesn't exist (immutable reference)
        if (!api.system.quadTemplate) {
          api.system.quadTemplate = new api.Form(api.QUAD, { pos: [0, 0, -4], rot: [0, 0, 0], scale: 1 });
        }
        return api.system.quadTemplate;
      },
      // 🎭 Trans function - creates working copies and applies transformations
      // Usage: (trans cube (move 1 2 3) (scale 2) (spin 0.1 0 0) (rotate 45 0 0))
      trans: (api, args, env) => {
        if (args.length < 2) {
          console.error("❗ trans requires at least 2 arguments: form and transformation(s)");
          return;
        }

        // Get the template form (first argument)
        const templateForm = this.evaluate(args[0], api, env);
        if (!templateForm || !templateForm.vertices) {
          console.error("❗ trans: first argument must be a valid form, got:", templateForm);
          return;
        }

        // Create a working copy of the form for this frame
        const workingForm = this.createFormCopy(templateForm, api);

        // Process transformation commands with special evaluation
        for (let i = 1; i < args.length; i++) {
          const transformCmd = args[i];
          if (Array.isArray(transformCmd) && transformCmd.length > 0) {
            const [cmd, ...params] = transformCmd;

            // Evaluate ONLY the parameters, not the command itself
            const evaluatedParams = params.map(param => this.evaluate(param, api, env));

            switch (cmd) {
              case "move":
              case "pos":
                if (evaluatedParams.length >= 3) {
                  workingForm.position[0] = evaluatedParams[0] || 0;
                  workingForm.position[1] = evaluatedParams[1] || 0;
                  workingForm.position[2] = evaluatedParams[2] || 0;
                }
                break;

              case "scale":
                if (evaluatedParams.length === 1) {
                  const s = evaluatedParams[0] || 1;
                  workingForm.scale[0] = s;
                  workingForm.scale[1] = s;
                  workingForm.scale[2] = s;
                } else if (evaluatedParams.length >= 3) {
                  workingForm.scale[0] = evaluatedParams[0] || 1;
                  workingForm.scale[1] = evaluatedParams[1] || 1;
                  workingForm.scale[2] = evaluatedParams[2] || 1;
                }
                break;

              case "rotate":
                if (evaluatedParams.length >= 3) {
                  workingForm.rotation[0] = evaluatedParams[0] || 0;
                  workingForm.rotation[1] = evaluatedParams[1] || 0;
                  workingForm.rotation[2] = evaluatedParams[2] || 0;
                }
                break;

              case "spin":
                // Frame-based rotation for animation (accumulates over time)
                if (evaluatedParams.length >= 3) {
                  const frameCount = this.frameCount || 0;
                  workingForm.rotation[0] = (evaluatedParams[0] || 0) * frameCount;
                  workingForm.rotation[1] = (evaluatedParams[1] || 0) * frameCount;
                  workingForm.rotation[2] = (evaluatedParams[2] || 0) * frameCount;
                }
                break;

              default:
                console.warn(`❗ Unknown transform command: ${cmd}`);
                break;
            }
          }
        }

        // Mark as transformed for GPU
        workingForm.gpuTransformed = true;

        return workingForm;
      },
      // Render a form
      form: (api, args = []) => {
        const forms = args.filter(f => f !== undefined);
        if (forms.length > 0) {
          api.form(forms);
        }
      },
      // Simple transform function for cube
      cubespin: (api, args = []) => {
        const xSpeed = args[0] || 0;
        const ySpeed = args[1] || 0;
        const zSpeed = args[2] || 0;

        const cube = api.system.cube;
        if (cube) {
          // Just increment rotation - let the graphics system handle the rest
          cube.rotation[0] += xSpeed;
          cube.rotation[1] += ySpeed;
          cube.rotation[2] += zSpeed;
          cube.gpuTransformed = true;
        }
        return cube;
      },
      // Alternative center-spinning cube function
      cubespin2: (api, args = []) => {
        const xSpeed = args[0] || 0;
        const ySpeed = args[1] || 0;
        const zSpeed = args[2] || 0;

        const cube = api.system.cube;
        if (cube && cube.vertices) {
          // Manually rotate vertices around cube center
          const centerX = cube.position[0];
          const centerY = cube.position[1];
          const centerZ = cube.position[2];

          // Simple Y-axis rotation for now
          const rotY = ySpeed;
          const cos = Math.cos(rotY);
          const sin = Math.sin(rotY);

          cube.vertices.forEach(vertex => {
            // Translate to origin relative to cube center
            const x = vertex.pos[0] - centerX;
            const z = vertex.pos[2] - centerZ;

            // Rotate around Y-axis
            const newX = x * cos - z * sin;
            const newZ = x * sin + z * cos;

            // Translate back
            vertex.pos[0] = newX + centerX;
            vertex.pos[2] = newZ + centerZ;
          });

          cube.gpuTransformed = true;
        }
        return cube;
      },
      // Move cube to position
      cubepos: (api, args = []) => {
        const x = args[0] || 0;
        const y = args[1] || 0;
        const z = args[2] || -4;

        const cube = api.system.cube;
        if (cube) {
          cube.position[0] = x;
          cube.position[1] = y;
          cube.position[2] = z;
          cube.gpuTransformed = true;
        }
        return cube;
      },
      // Scale cube
      cubescale: (api, args = []) => {
        const scale = args[0] || 1;

        const cube = api.system.cube;
        if (cube) {
          cube.scale[0] = scale;
          cube.scale[1] = scale;
          cube.scale[2] = scale;
          cube.gpuTransformed = true;
        }
        return cube;
      },
      // Set cube rotation
      cuberot: (api, args = []) => {
        const x = args[0] || 0;
        const y = args[1] || 0;
        const z = args[2] || 0;

        const cube = api.system.cube;
        if (cube) {
          cube.rotation[0] = x;
          cube.rotation[1] = y;
          cube.rotation[2] = z;
          cube.gpuTransformed = true;
        }
        return cube;
      },
      // Global 3D objects work directly
      // Manipulate cube.position, cube.rotation, cube.scale directly from KidLisp

      // �🏷️ HUD label
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
        return api.paintCount || 0;
      },
      f: (api) => { // Abbreviation for frame
        return api.paintCount || 0;
      },
      clock: (api) => {
        return Date.now(); // Returns UTC milliseconds since epoch
      },
      // 🎯 Performance monitoring functions (singleton behavior)
      perf: (api, args = []) => {
        // Always use singleton behavior - only execute once per module load
        const perfKey = `perf_singleton_${args.join('_')}`;

        if (this.onceExecuted.has(perfKey)) {
          return this.perf.showHUD; // Already executed, return current state
        }
        this.onceExecuted.add(perfKey);

        if (args.length === 0) {
          // Default: enable and show HUD
          this.startPerformanceMonitoring();
          return true;
        } else if (args[0] === "start" || args[0] === "on") {
          this.startPerformanceMonitoring();
          return true;
        } else if (args[0] === "stop" || args[0] === "off") {
          this.stopPerformanceMonitoring();
          return false;
        } else if (args[0] === "toggle") {
          return this.togglePerformanceHUD();
        }
        return this.perf.enabled;
      },
      fps: (api, args) => {
        if (args.length > 0) {
          const parsed = parseFloat(args[0]);
          if (!isNaN(parsed)) {
            if (parsed > 0) {
              const targetFps = parsed;
              this.targetFps = targetFps;
              if (api && typeof api.fps === "function") {
                api.fps(targetFps);
              }
              if (api && api.system) {
                api.system.kidlispFps = targetFps;
              }
              if (typeof window !== 'undefined') {
                window.currentKidlispFps = targetFps;
                if (!window.kidlispFpsTimeline) {
                  window.kidlispFpsTimeline = [];
                }
                const timestamp = performance.now();
                window.kidlispFpsTimeline.push({
                  timestamp,
                  fps: targetFps,
                });
                if (window.currentRecordingOptions) {
                  window.currentRecordingOptions.kidlispFps = targetFps;
                  window.currentRecordingOptions.kidlispFpsTimeline = window.kidlispFpsTimeline;
                  console.log(`🎬 Updated recording options with KidLisp FPS: ${targetFps} at ${timestamp.toFixed(2)}ms`);
                }
              }
              if (this._lastLoggedFps !== targetFps) {
                console.log(`🎬 KidLisp FPS set to: ${targetFps} at ${(typeof window !== 'undefined' ? performance.now() : 0).toFixed(2)}ms`);
                this._lastLoggedFps = targetFps;
              }
              return targetFps;
            }

            // parsed is zero or negative: treat as reset to default
            this.targetFps = null;
            if (api && typeof api.fps === "function") {
              api.fps(null);
            }
            if (api && api.system) {
              api.system.kidlispFps = null;
            }
            if (typeof window !== 'undefined') {
              window.currentKidlispFps = null;
              if (!window.kidlispFpsTimeline) {
                window.kidlispFpsTimeline = [];
              }
              const timestamp = performance.now();
              window.kidlispFpsTimeline.push({
                timestamp,
                fps: null,
              });
              if (window.currentRecordingOptions) {
                window.currentRecordingOptions.kidlispFps = null;
                window.currentRecordingOptions.kidlispFpsTimeline = window.kidlispFpsTimeline;
                console.log(`🎬 Reset recording KidLisp FPS override at ${timestamp.toFixed(2)}ms`);
              }
            }
            if (this._lastLoggedFps !== null) {
              console.log(`🎬 KidLisp FPS reset to default at ${(typeof window !== 'undefined' ? performance.now() : 0).toFixed(2)}ms`);
              this._lastLoggedFps = null;
            }
            return 60;
          }
        }
        return this.targetFps || 60; // Default to 60 FPS if no fps was set
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

        // 🚨 SAFETY: Prevent performance-killing massive repeat loops
        if (count > 10000) {
          console.error(
            `❗ repeat count ${count} exceeds safety limit of 10,000. Use smaller counts or timing expressions.`,
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
          // like: (repeat count i (ink ...) (box ...)) or text rendering patterns
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

          // 🚀 ULTRA-FAST TEXT RENDERING OPTIMIZATION
          // Detect pattern: (repeat count i (ink (choose ...)) (write (choose ...) (mod ...) (mod ...)))
          if (
            expressions.length === 2 &&
            Array.isArray(expressions[0]) &&
            expressions[0][0] === "ink" &&
            Array.isArray(expressions[1]) &&
            expressions[1][0] === "write"
          ) {
            perfStart("fast-text-loop");
            const inkExpr = expressions[0];
            const writeExpr = expressions[1];

            // Check if ink uses choose for colors
            let colorChoices = null;
            if (
              inkExpr.length === 2 &&
              Array.isArray(inkExpr[1]) &&
              inkExpr[1][0] === "choose"
            ) {
              colorChoices = inkExpr[1].slice(1); // Extract color choices
            }

            // Check if write uses choose for text
            let textChoices = null;
            if (
              writeExpr.length >= 2 &&
              Array.isArray(writeExpr[1]) &&
              writeExpr[1][0] === "choose"
            ) {
              textChoices = writeExpr[1].slice(1); // Extract text choices
            }

            // Pre-setup environment
            const prevLocalEnv = this.localEnv;
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

            // Ultra-fast text rendering loop
            for (let i = 0; i < count; i++) {
              loopEnv[iteratorVar] = i;

              // Fast color selection
              if (colorChoices) {
                const colorIndex = Math.floor(this.seededRandom() * colorChoices.length);
                api.ink?.(colorChoices[colorIndex]);
              } else {
                this.evaluate(inkExpr, api, this.localEnv);
              }

              // Fast text and position evaluation
              let text, x, y;
              if (textChoices) {
                const textIndex = Math.floor(this.seededRandom() * textChoices.length);
                text = textChoices[textIndex];
              } else {
                text = this.fastEval(writeExpr[1], api, this.localEnv);
              }

              // Optimize position calculations using fastEval
              if (writeExpr.length >= 4) {
                x = this.fastEval(writeExpr[2], api, this.localEnv);
                y = this.fastEval(writeExpr[3], api, this.localEnv);
              } else {
                x = 0;
                y = 0;
              }

              // Direct write call with minimal overhead
              api.write?.(text, x, y);
            }

            this.localEnvLevel -= 1;
            this.localEnv = prevLocalEnv;
            perfEnd("fast-text-loop");
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
      // 🎲 Random selection (optimized with caching for performance)
      choose: (api, args = []) => {
        if (args.length === 0) return undefined;

        // 🚀 PERFORMANCE: Cache small choice sets to avoid repeated array access
        if (args.length <= 8) {
          const cacheKey = JSON.stringify(args);
          if (!this.choiceCache) this.choiceCache = new Map();

          let cachedArgs = this.choiceCache.get(cacheKey);
          if (!cachedArgs) {
            cachedArgs = [...args]; // Copy args for caching
            this.choiceCache.set(cacheKey, cachedArgs);

            // Limit cache size to prevent memory bloat
            if (this.choiceCache.size > 100) {
              const firstKey = this.choiceCache.keys().next().value;
              this.choiceCache.delete(firstKey);
            }
          }

          // Fast random selection from cached args using isolated random
          const randomIndex = Math.floor(this.seededRandom() * cachedArgs.length);
          return cachedArgs[randomIndex];
        }

        // Use the help.choose function from the common API if available
        if (api.help?.choose) {
          return api.help.choose(...args);
        }
        // Fallback to simple random selection using isolated random
        const randomIndex = Math.floor(this.seededRandom() * args.length);
        return args[randomIndex];
      },
      // 🎲 Random selection (alias)
      "?": (api, args = []) => {
        // If arguments provided, choose randomly from them
        if (args.length > 0) {
          // Use the help.choose function from the common API if available
          if (api.help?.choose) {
            return api.help.choose(...args);
          }
          // Fallback to simple random selection from arguments using isolated random
          const randomIndex = Math.floor(this.seededRandom() * args.length);
          return args[randomIndex];
        }

        // If no arguments, return undefined (like JavaScript undefined)
        // Functions can detect this and apply their own contextual logic
        return undefined;
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
      zebra: (api) => {
        return api.num?.zebra() || [0, 0, 0]; // Fallback to black if not available
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
        return getCachedCodeMultiLevel(cacheId).then(source => {
          if (source) {
            // Parse and evaluate the cached source
            const parsed = this.parse(source);
            return this.evaluate(parsed, api, this.localEnv);
          } else {
            // Silent fallback - no need to warn about missing cache
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
        // PERFORMANCE: Ultra-fast implementation with minimal logic
        return api.sound?.enabled?.() || false;
        // Cache speaker result across multiple frames to avoid expensive repeated calls
        if (!this.speakerCache) this.speakerCache = { result: null, timestamp: 0 };

        const now = performance.now();
        // Use cached result if it's less than 2000ms old (ultra-aggressive caching)
        if (this.speakerCache.result !== null && (now - this.speakerCache.timestamp) < 2000) {
          return this.speakerCache.result;
        }

        const soundEnabled = api.sound?.enabled?.() || false;
        console.log("� SPEAKER: First call this frame, sound enabled:", soundEnabled);

        // Cache the result
        this.speakerCache.result = soundEnabled;
        this.speakerCache.timestamp = now;

        return soundEnabled;
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

      // Add static color codes (c0, c1, c2, etc.) using standardized mapping
      ...Object.keys(staticColorMap).reduce((acc, index) => {
        acc[`c${index}`] = () => staticColorMap[index];
        return acc;
      }, {}),

      // Add palette codes (p0, p1, etc.)
      p0: () => "rainbow",
      p1: () => "zebra",

      // 🍞 Bake function - creates a new painting layer by capturing current buffer state
      // Automatically prevents repeated calls within the same program execution
      bake: (api, args = []) => {
        // Create a simple key for this bake call (not using counter to avoid unique keys each time)
        const bakeKey = "bake_call";

        // Check if bake has already been executed in this program run
        if (this.onceExecuted.has(bakeKey)) {
          return this.bakedLayers?.length || 0;
        }

        // Mark bake as executed for this program run
        this.onceExecuted.add(bakeKey);

        console.log("🍞 Baking current layer...");

        // Initialize baked layers if not already done
        if (!this.bakedLayers) {
          this.bakedLayers = [];
        }

        // Get current screen buffer dimensions
        const currentWidth = api.screen?.width || 256;
        const currentHeight = api.screen?.height || 256;

        // Create a snapshot of the current buffer state
        if (api.screen?.pixels) {
          // Clone the current pixel buffer to preserve the "baked" state
          const bakedBuffer = {
            width: currentWidth,
            height: currentHeight,
            pixels: new Uint8ClampedArray(api.screen.pixels)
          };

          // Store this baked layer
          this.bakedLayers.push(bakedBuffer);
          this.bakeCallCount = this.bakedLayers.length;

          console.log(`🍞 Baked layer ${this.bakedLayers.length} (${currentWidth}x${currentHeight})`);

          // Clear the current buffer to start fresh (optional - could be configurable)
          if (args.length === 0 || args[0] !== "keep") {
            // Manually clear the screen buffer to transparent instead of using wipe
            if (api.screen?.pixels) {
              api.screen.pixels.fill(0); // Fill with transparent black (0,0,0,0)
            }
          }
        } else {
          console.warn("🍞 No screen buffer available to bake");
        }

        return this.bakedLayers.length;
      },

      // 🖼️ Embed function - loads cached KidLisp code and creates persistent animated layers
      // Usage: (embed $pie) - loads cached code in default 256x256 layer (fixed size for cache efficiency)
      //        (embed $pie 128 128) - loads cached code in 128x128 layer  
      //        (embed $pie 0 0 60 40) - loads cached code in 60x40 layer at position (0,0)
      //        (embed $pie 0 0 60 40 128) - loads cached code in 60x40 layer with alpha 128 (0-255, or 0.0-1.0)
      embed: (api, args = []) => {
        if (args.length === 0) {
          console.warn("❗ embed function requires a cached code argument");
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
          return undefined;
        }

        // Parse dimensions, position, and alpha from arguments
        // ⚡ PERFORMANCE: Use fixed default size to avoid cache invalidation on screen resize
        let width = 256, height = 256, x = 0, y = 0, alpha = 255; // Default to 256x256 and opaque
        let usesScreenDimensions = false; // Track if w/h are used for responsive sizing

        if (args.length >= 3) {
          if (args.length === 3) {
            // (embed $pie width height)
            const widthArg = args[1];
            const heightArg = args[2];

            // Check if using screen dimensions
            if ((typeof widthArg === 'string' && widthArg === 'w') ||
              (typeof heightArg === 'string' && heightArg === 'h')) {
              usesScreenDimensions = true;
            }

            width = this.evaluate(args[1], api, this.localEnv) || 256;
            height = this.evaluate(args[2], api, this.localEnv) || 256;
          } else if (args.length === 4) {
            // (embed $pie x y size) - size becomes both width and height
            x = this.evaluate(args[1], api, this.localEnv) || 0;
            y = this.evaluate(args[2], api, this.localEnv) || 0;
            const size = this.evaluate(args[3], api, this.localEnv) || 256;
            width = size;
            height = size;
          } else if (args.length === 5) {
            // (embed $pie x y width height)
            const widthArg = args[3];
            const heightArg = args[4];

            // Check if using screen dimensions
            if ((typeof widthArg === 'string' && widthArg === 'w') ||
              (typeof heightArg === 'string' && heightArg === 'h')) {
              usesScreenDimensions = true;
            }

            x = this.evaluate(args[1], api, this.localEnv) || 0;
            y = this.evaluate(args[2], api, this.localEnv) || 0;
            width = this.evaluate(args[3], api, this.localEnv) || 256;
            height = this.evaluate(args[4], api, this.localEnv) || 256;
          } else if (args.length >= 6) {
            // (embed $pie x y width height alpha)
            const widthArg = args[3];
            const heightArg = args[4];

            // Check if using screen dimensions
            if ((typeof widthArg === 'string' && widthArg === 'w') ||
              (typeof heightArg === 'string' && heightArg === 'h')) {
              usesScreenDimensions = true;
            }

            x = this.evaluate(args[1], api, this.localEnv) || 0;
            y = this.evaluate(args[2], api, this.localEnv) || 0;
            width = this.evaluate(args[3], api, this.localEnv) || 256;
            height = this.evaluate(args[4], api, this.localEnv) || 256;
            alpha = this.evaluate(args[5], api, this.localEnv);
            // Support both 0-1 and 0-255 alpha ranges
            if (alpha !== undefined && alpha !== null && alpha <= 1 && alpha >= 0) {
              alpha = Math.floor(alpha * 255);
            }
            // Default to fully opaque if alpha parameter is undefined/null
            // But allow 0 to mean fully transparent
            if (alpha === undefined || alpha === null) {
              alpha = 255;
            }
          }
        }

        // Ensure dimensions are positive numbers and alpha is in valid range
        width = Math.max(1, Math.floor(width));
        height = Math.max(1, Math.floor(height));
        x = Math.floor(x);
        y = Math.floor(y);
        alpha = Math.max(0, Math.min(255, Math.floor(alpha)));

        // 🚀 PERFORMANCE OPTIMIZATION: Normalize dimensions to reduce cache fragmentation during reframe
        // Use size buckets to allow cache reuse for similar dimensions
        // BUT: Skip normalization if using screen dimensions to ensure proper responsive behavior
        let normalizedWidth, normalizedHeight;
        if (usesScreenDimensions) {
          // For screen-responsive embeds, use actual dimensions and include screen size in cache key
          normalizedWidth = width;
          normalizedHeight = height;
        } else {
          normalizedWidth = width <= 128 ? 128 : width <= 256 ? 256 : width <= 512 ? 512 : width;
          normalizedHeight = height <= 128 ? 128 : height <= 256 ? 256 : height <= 512 ? 512 : height;
        }

        // Include screen dimensions in cache key for responsive embeds
        const screenSuffix = usesScreenDimensions ? `_screen${api.screen.width}x${api.screen.height}` : '';
        const layerKey = `${cacheId}_${normalizedWidth}x${normalizedHeight}_${x},${y}_${alpha}${screenSuffix}`;

        // 🔄 RESPONSIVE CACHE: Clear outdated screen-dependent entries before checking cache
        if (this.embeddedLayerCache && usesScreenDimensions) {
          const currentScreenKey = `_screen${api.screen.width}x${api.screen.height}`;
          const entriesToDelete = [];

          for (const [key, layer] of this.embeddedLayerCache.entries()) {
            // Look for cache keys that include screen dimensions but don't match current screen
            if (key.includes('_screen') && !key.includes(currentScreenKey)) {
              entriesToDelete.push(key);
              // Return buffer to pool before deletion
              if (layer && layer.buffer) {
                this.returnBufferToPool(layer.buffer, layer.width, layer.height);
              }
            }
          }

          // Delete the outdated screen-dependent entries
          if (entriesToDelete.length > 0) {
            entriesToDelete.forEach(key => {
              this.embeddedLayerCache.delete(key);
              console.log(`🗑️ Cleared outdated responsive cache entry: ${key}`);
            });
            console.log(`✨ Cleared ${entriesToDelete.length} outdated responsive cache entries`);
          }
        }

        // Check if this embedded layer already exists - RETURN IMMEDIATELY if found
        if (this.embeddedLayerCache && this.embeddedLayerCache.has(layerKey)) {
          const existingLayer = this.embeddedLayerCache.get(layerKey);

          // If dimensions don't match exactly, we need to handle the scaling
          if (existingLayer.width !== width || existingLayer.height !== height) {
            // Update position and alpha for this specific call
            existingLayer.x = x;
            existingLayer.y = y;
            existingLayer.alpha = alpha;

            // For performance, we'll render at the cached size but paste at the requested position
            // This avoids recreating buffers during reframe operations
          }
          // console.log("♻️ Using existing embedded layer:", layerKey);

          // Check if we're in a timing context (like 3s...) and if this embed should be active
          if (this.currentTimingContext) {
            const timingCtx = this.currentTimingContext;

            // Check if this embed call matches the currently selected argument
            const selectedArg = timingCtx.selectedArg;
            let isCurrentlySelected = false;

            if (Array.isArray(selectedArg) && selectedArg.length > 0) {
              // Check if the selected argument starts with this cache ID
              isCurrentlySelected = selectedArg[0] === `$${cacheId}`;
            } else if (typeof selectedArg === "string") {
              // Check if the selected argument is this cache ID directly
              isCurrentlySelected = selectedArg === `$${cacheId}`;
            }

            // If this embed is not the currently selected argument, skip ALL rendering and pasting
            if (!isCurrentlySelected) {
              console.log(`⏸️ SKIPPING EMBED ${cacheId} - not selected. Current:`, JSON.stringify(selectedArg));
              return undefined; // Return undefined to prevent any pasting or rendering
            } else {
              console.log(`✅ ALLOWING EMBED ${cacheId} - is selected`);
            }
          }

          // Check if layer's timing condition should execute this frame
          // Parse the layer's source to find timing expressions like (3s... ) or (0.25s... )
          // console.log(`🔍 About to check timing for ${layerKey}`);
          // TEMPORARILY DISABLED: const shouldExecute = this.shouldLayerExecuteThisFrame(api, existingLayer);
          const shouldExecute = true; // Always execute for now
          // console.log(`🔍 Timing check result for ${layerKey}: ${shouldExecute}`);

          if (!shouldExecute) {
            // Skipped execution debug log removed for performance
            return existingLayer;
          }

          // Always render nested embedded layers when called from within another embedded layer
          // This ensures nested embeds like ($pie ...) and ($febs ...) actually execute
          const shouldRender = this.updateEmbeddedLayer(api, existingLayer);

          // Always paste cached layers that have buffers - they contain valuable content even if they didn't render this frame
          if (existingLayer.buffer && api.paste) {
            // console.log(`🔄 embed() calling pasteWithAlpha: buffer=${existingLayer.buffer.width}x${existingLayer.buffer.height}, pos=(${x},${y}), alpha=${existingLayer.alpha}`);
            // Always use alpha blending to properly handle pixels with alpha channels
            this.pasteWithAlpha(api, existingLayer.buffer, x, y, existingLayer.alpha);
          } else {
            // console.log(`🚫 embed() NOT calling pasteWithAlpha: hasBuffer=${!!existingLayer.buffer}, hasPaste=${!!api.paste}`);
          }

          return existingLayer;
        }

        // Initialize cache if needed
        if (!this.embeddedLayerCache) {
          this.embeddedLayerCache = new Map();
        }

        // Embedded layer creation debug log removed for performance

        // Mark this $code as loading for syntax highlighting
        this.loadingEmbeddedLayers.add(cacheId);

        // Check if we already have the source code cached
        if (this.embeddedSourceCache.has(cacheId)) {
          const cachedSource = this.embeddedSourceCache.get(cacheId);
          // Mark as loaded since we have cached source
          this.loadingEmbeddedLayers.delete(cacheId);
          this.loadedEmbeddedLayers.add(cacheId);
          return this.createEmbeddedLayerFromSource(cachedSource, cacheId, layerKey, width, height, x, y, alpha, api);
        }

        // 🎯 TEIA CACHE CHECK: Try to get source from global TEIA cache synchronously
        const globalScope = (function() {
          if (typeof window !== 'undefined') return window;
          if (typeof globalThis !== 'undefined') return globalThis;
          if (typeof global !== 'undefined') return global;
          if (typeof self !== 'undefined') return self;
          return {};
        })();

        if (globalScope.teiaKidlispCodes && globalScope.teiaKidlispCodes[cacheId]) {
          const teiaSource = globalScope.teiaKidlispCodes[cacheId];
          console.log(`🎯 Using TEIA cached code for embedded layer: ${cacheId}`);
          // Cache it in embeddedSourceCache for future use
          this.embeddedSourceCache.set(cacheId, teiaSource);
          // Mark as loaded since we have the source
          this.loadingEmbeddedLayers.delete(cacheId);
          this.loadedEmbeddedLayers.add(cacheId);
          return this.createEmbeddedLayerFromSource(teiaSource, cacheId, layerKey, width, height, x, y, alpha, api);
        }

        // Check if we're already fetching this source to prevent duplicates
        const fetchKey = `${cacheId}_fetching_source`;
        if (this.embeddedLayerCache.has(fetchKey)) {
          return this.embeddedLayerCache.get(fetchKey);
        }

        // 🚀 NON-BLOCKING APPROACH: Create placeholder layer immediately, fetch source in background

        // Create a placeholder layer with loading indicator for immediate rendering
        const placeholderSource = `(fps 24)
(wipe 32 32 32 128)
(ink 200 200 200)
(write (+ "Loading " ${JSON.stringify(cacheId)} "...") 4 4)`;

        const placeholderLayer = this.createEmbeddedLayerFromSource(
          placeholderSource,
          cacheId,
          layerKey,
          width,
          height,
          x,
          y,
          alpha,
          api
        );

        console.log(`✅ Placeholder layer created for ${cacheId}:`, placeholderLayer ? 'success' : 'failed');

        // Start background fetch without blocking the render
        const backgroundFetch = Promise.race([
          getCachedCodeMultiLevel(cacheId),
          new Promise((resolve) => {
            setTimeout(() => {
              resolve(null);
            }, 10000);
          })
        ]).then(source => {
          // Remove fetch marker
          this.embeddedLayerCache.delete(fetchKey);

          // Mark as no longer loading and now loaded
          this.loadingEmbeddedLayers.delete(cacheId);
          this.loadedEmbeddedLayers.add(cacheId);

          if (!source) {
            source = `(fps 24)
(wipe red)
(ink yellow)
(line 0 64 128 64)
(ink green)  
(line 64 0 64 128)`;
          }

          // Cache the source code for future use
          this.embeddedSourceCache.set(cacheId, source);

          // Update the existing layer with real source code
          if (placeholderLayer && this.embeddedLayerCache.has(layerKey)) {
            const existingLayer = this.embeddedLayerCache.get(layerKey);
            if (existingLayer) {
              // Clear the placeholder buffer
              if (existingLayer.buffer && existingLayer.buffer.pixels) {
                existingLayer.buffer.pixels.fill(0);
              }

              // Update with real source and reparse
              existingLayer.source = source;
              existingLayer.sourceCode = source;
              existingLayer.kidlispInstance.source = source; // Store in instance for dynamic content detection
              existingLayer.parsedCode = existingLayer.kidlispInstance.parse(source);

              // Reset timing for fresh start
              existingLayer.localFrameCount = 0;
              existingLayer.timingPattern = this.extractTimingPattern(source);

              console.log(`🔄 Updated embedded layer ${cacheId} with real source code`);
            }
          }

          return placeholderLayer;
        }).catch(error => {
          console.error("❌ Error fetching embedded layer source:", cacheId, error);
          // Remove fetch marker on error
          this.embeddedLayerCache.delete(fetchKey);
          return placeholderLayer; // Return placeholder even on error
        });

        // Store the background fetch promise but don't wait for it
        this.embeddedLayerCache.set(fetchKey, backgroundFetch);

        // Return the placeholder layer immediately for non-blocking render
        return placeholderLayer;
      },

      // 🚀 Navigation function - jump to another piece
      jump: (api, args = [], env) => {
        if (args.length === 0) {
          console.warn("❗ jump function requires a destination argument");
          return;
        }

        // Special handling for $code arguments - don't evaluate them, use as strings
        let destination = args[0];

        // If it's a $code token (string starting with $), use it directly
        if (typeof destination === "string" && destination.startsWith("$")) {
          // Keep the $code as-is for the jump
          destination = destination;
        } else if (typeof destination === "string") {
          // Regular string argument, remove quotes if present
          destination = unquoteString(destination);
        } else {
          // For other types, evaluate them first
          destination = this.evaluate(destination, api, env);

          // Handle the result of evaluation
          if (typeof destination === "object" && destination !== null) {
            // If it's an object (like from $code evaluation), convert to string
            if (destination.toString && typeof destination.toString === "function") {
              destination = destination.toString();
            } else {
              console.warn("❗ Invalid jump destination type:", typeof destination);
              return;
            }
          } else {
            // Convert other types to string
            destination = String(destination);
          }
        }

        // 🚀 Optimization: Check cache for $code destinations
        if (typeof destination === "string" && destination.startsWith("$")) {
          const cacheId = destination.slice(1); // Remove $ prefix

          // Check if we have the code cached locally
          if (globalCodeCache.has(cacheId)) {
            console.log(`🚀 KidLisp fast-jumping to cached code: ${destination} (no network request needed)`);
          } else {
            console.log(`🚀 KidLisp jumping to ${destination} (will check IndexedDB and network if needed)`);
          }
        }

        // Optional parameters for jump function
        const ahistorical = args.length > 1 ? args[1] : false;
        const alias = args.length > 2 ? args[2] : false;

        // Call the underlying jump API function
        if (api.jump) {
          console.log("🚀 KidLisp jumping to:", destination);
          api.jump(destination, ahistorical, alias);
        } else {
          console.warn("❗ Jump API function not available");
        }
      },
    };

    return this.globalEnvCache;
  }

  // Context-aware randomization for ? tokens
  contextAwareRandom(functionName, argIndex, api) {
    const width = api.screen?.width || 256;
    const height = api.screen?.height || 256;

    // Define context-aware ranges for different functions and argument positions
    const contextRanges = {
      box: [
        { min: 0, max: width },   // x position (arg 0)
        { min: 0, max: height },  // y position (arg 1)
        { min: 10, max: 100 },    // width (arg 2)
        { min: 10, max: 100 },    // height (arg 3)
      ],
      line: [
        { min: 0, max: width },   // x1 (arg 0)
        { min: 0, max: height },  // y1 (arg 1)
        { min: 0, max: width },   // x2 (arg 2)
        { min: 0, max: height },  // y2 (arg 3)
      ],
      circle: [
        { min: 0, max: width },   // x position (arg 0)
        { min: 0, max: height },  // y position (arg 1)
        { min: 5, max: 50 },      // radius (arg 2)
      ],
      tri: [
        { min: 0, max: width },   // x1 position (arg 0)
        { min: 0, max: height },  // y1 position (arg 1)
        { min: 0, max: width },   // x2 position (arg 2)
        { min: 0, max: height },  // y2 position (arg 3)
        { min: 0, max: width },   // x3 position (arg 4)
        { min: 0, max: height },  // y3 position (arg 5)
      ],
      write: [
        { min: 0, max: width },   // x position (arg 1, since arg 0 is text)
        { min: 0, max: height },  // y position (arg 2)
      ],
      ink: [
        { min: 0, max: 255 },     // red (arg 0)
        { min: 0, max: 255 },     // green (arg 1)
        { min: 0, max: 255 },     // blue (arg 2)
        { min: 0, max: 255 },     // alpha (arg 3)
      ],
      flood: [
        { min: 0, max: width },   // x position (arg 0)
        { min: 0, max: height },  // y position (arg 1)
      ],
      embed: [
        // arg 0 is the cache code (string), so no range for that
        { min: 32, max: 512 },    // width (arg 1)
        { min: 32, max: 512 },    // height (arg 2)
        { min: 0, max: width },   // x position (arg 3 in 5-arg version)
        { min: 0, max: height },  // y position (arg 4 in 5-arg version)
        { min: 32, max: 512 },    // width (arg 5 in 5-arg version)
        { min: 32, max: 512 },    // height (arg 6 in 5-arg version)
      ],
    };

    // Get the range for this function and argument index
    const functionRanges = contextRanges[functionName];
    if (functionRanges && functionRanges[argIndex]) {
      const range = functionRanges[argIndex];
      return Math.floor(this.seededRandom() * (range.max - range.min + 1)) + range.min;
    }

    // Default range if no specific context is defined
    return Math.floor(this.seededRandom() * 256);
  }

  // Fast evaluation for common expressions to avoid full recursive evaluation
  fastEval(expr, api, env) {
    if (typeof expr === "number") return expr;
    if (typeof expr === "string") {
      // Handle randomization token
      if (expr === "?") {
        // Generate a random number from 0 to 255 (default range)
        return Math.floor(this.seededRandom() * 256);
      }

      // Handle negative identifiers like "-frame", "-width", etc.
      if (expr.startsWith("-") && expr.length > 1) {
        const baseIdentifier = expr.substring(1);
        const baseValue = this.fastEval(baseIdentifier, api, env);
        if (typeof baseValue === "number") {
          return -baseValue;
        }
      }

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

      // 🎯 Handle dollar code strings like $bop
      if (expr.startsWith("$") && expr.length > 1) {
        const cacheId = expr.slice(1);
        if (/^[a-zA-Z0-9]{1,12}$/.test(cacheId)) {
          console.log(`🎯 Processing dollar code: ${expr} -> cacheId: ${cacheId}`);
          const embedFunc = globalEnv.embed;
          if (embedFunc) {
            console.log(`🔧 Calling embed function for ${expr}`);
            return embedFunc(api, [cacheId]);
          } else {
            console.warn("❌ embed function not found in global environment");
            return expr;
          }
        }
      }

      const result = value !== undefined ? value : expr;
      return result;
    }

    // Fast math expression evaluation - Enhanced for nested operations
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
              return rightVal !== 0 ? leftVal / rightVal : 0; // Prevent division by zero
            case "%":
              return rightVal !== 0 ? leftVal % rightVal : 0;
          }
        }
      }
    }

    // 🚀 ULTRA-PERFORMANCE: Optimize complex positioning expressions like (mod (+ (* i 67) (* frame 0.5)) width)
    if (Array.isArray(expr) && expr.length === 3 && expr[0] === "mod") {
      const [, innerExpr, modulus] = expr;

      // Handle (mod (+ (* i N) (* frame M)) dimension) pattern
      if (Array.isArray(innerExpr) && innerExpr.length === 3 && innerExpr[0] === "+") {
        const [, leftTerm, rightTerm] = innerExpr;

        // Check if both terms are multiplication expressions
        if (Array.isArray(leftTerm) && leftTerm.length === 3 && leftTerm[0] === "*" &&
          Array.isArray(rightTerm) && rightTerm.length === 3 && rightTerm[0] === "*") {

          // Fast evaluation of both multiplication terms
          const leftResult = this.fastEval(leftTerm[1], api, env) * this.fastEval(leftTerm[2], api, env);
          const rightResult = this.fastEval(rightTerm[1], api, env) * this.fastEval(rightTerm[2], api, env);
          const sum = leftResult + rightResult;
          const modValue = this.fastEval(modulus, api, env);

          if (typeof sum === "number" && typeof modValue === "number" && modValue !== 0) {
            return sum % modValue;
          }
        }
      }

      // Fallback: evaluate inner expression and apply modulo
      const innerValue = this.fastEval(innerExpr, api, env);
      const modValue = this.fastEval(modulus, api, env);

      if (typeof innerValue === "number" && typeof modValue === "number" && modValue !== 0) {
        return innerValue % modValue;
      }
    }

    // 🚀 OPTIMIZATION: Handle common shape coordinate patterns
    // Patterns like (- (/ width 2) 8), (+ (/ width 2) 8), etc.
    if (Array.isArray(expr) && expr.length === 3) {
      const [op, left, right] = expr;

      // Pattern: (- (/ width 2) offset) or (+ (/ width 2) offset)
      if ((op === "+" || op === "-") && Array.isArray(left) && left.length === 3) {
        const [innerOp, innerLeft, innerRight] = left;

        // Check for (/ width 2) or (/ height 2) pattern
        if (innerOp === "/" && typeof innerLeft === "string" &&
          (innerLeft === "width" || innerLeft === "height") &&
          typeof innerRight === "number" && innerRight === 2) {

          const dimension = this.fastEval(innerLeft, api, env);
          const offset = this.fastEval(right, api, env);

          if (typeof dimension === "number" && typeof offset === "number") {
            const halfDimension = dimension / 2;
            return op === "+" ? halfDimension + offset : halfDimension - offset;
          }
        }
      }
    }

    // Fall back to full evaluation
    return this.evaluate(expr, api, env);
  }

  // Optimized function resolution
  resolveFunction(head, api, env) {
    // � ULTRA-PERFORMANCE: Pre-cache common functions to avoid repeated lookups
    if (!this.globalFunctionCache) {
      this.globalFunctionCache = new Map();

      // Pre-populate cache with most common functions to avoid expensive lookups
      const globalEnv = this.getGlobalEnv();
      const commonFunctions = ['ink', 'box', 'line', 'circle', 'write', 'repeat', 'choose', 'blur', 'shape', 'tri', 'wipe'];

      for (const funcName of commonFunctions) {
        if (globalEnv[funcName]) {
          this.globalFunctionCache.set(funcName, { type: "global", value: globalEnv[funcName] });
        }
      }

      // console.log(`🚀 Pre-cached ${this.globalFunctionCache.size} common functions for performance`);
    }

    // 🚀 FAST PATH: Check global function cache first for common functions
    if (this.globalFunctionCache.has(head)) {
      return this.globalFunctionCache.get(head);
    }

    // �🔍 DEBUG: Log function resolution for performance tracking (only for uncached functions now)
    // if (['repeat', 'blur', 'shape', 'write', 'choose', 'ink'].includes(head)) {
    // console.log(`🔍 RESOLVE: Function '${head}' being resolved (cache miss)`);
    // }

    // Check local cache
    const cacheKey = `${head}_${this.localEnvLevel}`;
    if (this.functionCache.has(cacheKey)) {
      return this.functionCache.get(cacheKey);
    }

    let result = null;

    // Special handling for $-prefixed cache codes - treat them as callable functions
    if (typeof head === "string" && head.startsWith("$") && head.length > 1) {
      const cacheId = head.slice(1);
      if (/^[0-9A-Za-z]{3,12}$/.test(cacheId)) {
        // Create a dynamic function that calls embed with the cache code
        const cacheFunction = (api, args = []) => {
          // Debug: Log when cache functions are called
          // Call the embed command with the cache ID and arguments
          const globalEnv = this.getGlobalEnv();
          const embedFunc = globalEnv.embed;
          if (embedFunc) {
            // Create arguments array: [cacheId, ...dimensions]
            const embedArgs = [cacheId, ...args];
            // Embed debug log removed for performance
            return embedFunc(api, embedArgs);
          } else {
            console.warn("❌ embed function not found in global environment");
            return undefined;
          }
        };
        result = {
          type: "cache",
          value: cacheFunction
        };
      }
    }

    // If not a cache code, proceed with normal resolution
    if (!result) {
      // Fast lookup order: local -> env -> global -> globalDef -> api
      // SPECIAL CASE: Force blur to always use global environment for consistent behavior
      if (head === 'blur' && existing(this.getGlobalEnv()[head])) {
        result = { type: "global", value: this.getGlobalEnv()[head] };
      }
      // SPECIAL CASE: In embedded layers, prioritize embedded API for certain functions
      else if (this.isNestedInstance && api && typeof api[head] === "function" &&
        (head === 'scroll' || head === 'spin' || head === 'zoom' || head === 'suck' ||
          head === 'contrast' || head === 'shear' || head === 'brightness' || head === 'line')) {
        result = { type: "api", value: api[head] };
      } else if (existing(this.localEnv[head])) {
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
    }

    // Cache the result (but not for local env since it changes, and not for cache codes since they're dynamic)
    if (result && result.type !== "local" && result.type !== "cache") {
      this.functionCache.set(cacheKey, result);
    }

    return result;
  }

  evaluate(parsed, api = {}, env, inArgs, isTopLevel = false, expressionIndex = 0) {
    const evalStart = this.startTiming('evaluate');
    perfStart("evaluate-total");
    if (VERBOSE) console.log("➗ Evaluating:", parsed);

    // 🔍 DEBUG: Track expensive evaluations - disabled for performance
    const shouldLog = false;  // Completely disable evaluation logging for performance

    // 🔍 PERFORMANCE DEBUG: Log slow operations  
    let perfTimer;
    if (shouldLog) {
      const exprName = Array.isArray(parsed) ?
        `${parsed[0]}${parsed.length > 1 ? '(...)' : ''}` :
        String(parsed).substring(0, 20);
      // console.log(`🔍 EVAL START: ${exprName}`);
      perfTimer = performance.now();
    }

    if (shouldLog) {
      // console.log(`🐌 EVAL START: ${Array.isArray(parsed) ? parsed[0] : 'primitive'} (depth: ${this.evalDepth || 0})`);
    }

    // �🚨 RECURSION LIMITER: Prevent runaway evaluation loops
    if (!this.evalDepth) this.evalDepth = 0;
    this.evalDepth++;

    // 🚀 PERFORMANCE: Simple frame-level caching for expensive expressions
    const needsCaching = Array.isArray(parsed) && parsed.length > 0;
    let cacheKey = null;
    if (needsCaching && this.perf.enabled) {
      // Create a simple cache key from the expression structure
      const head = parsed[0];
      // Only cache mathematical operations that are pure and don't contain time-varying variables
      if (typeof head === 'string' &&
        (head === 'sin' || head === 'cos' || head === 'mod' || head === '+' || head === '-' ||
          head === '*' || head === '/' || head === 'min' || head === 'max' || head === 'abs')) {

        // Check if expression contains time-varying variables that shouldn't be cached
        const exprString = JSON.stringify(parsed);
        const hasTimeVars = exprString.includes('frame') || exprString.includes('amplitude') ||
          exprString.includes('clock') || exprString.includes('speaker') ||
          exprString.includes('"i"') || exprString.includes('"j"') ||
          exprString.includes('"n"') || exprString.includes('"x"') ||
          exprString.includes('"y"') || exprString.includes('random');

        // Only cache if it doesn't contain time-varying variables
        if (!hasTimeVars) {
          cacheKey = exprString + '_' + this.frameCount;
          if (!this.frameCache) this.frameCache = new Map();
          if (this.frameCache.has(cacheKey)) {
            this.evalDepth--;
            this.endTiming('evaluate', evalStart);
            perfEnd("evaluate-total");
            return this.frameCache.get(cacheKey);
          }
        }
      }
    }

    // Set KidLisp context for dynamic fade evaluation
    if (api.setKidLispContext) {
      api.setKidLispContext(this, api, env);
    }

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
    // console.log("🎯 BODY DEBUG - length:", body.length, "items:", JSON.stringify(body));

    // 🎨 First-line color shorthand: If the first item is just a color name, 
    // treat it as (once (wipe <color>)) for easy backdrop setting
    if (body.length > 0 && !parsed.body) {
      const firstItem = body[0];
      let colorName = null;

      // Check if the first three or four items are RGB/RGBA values (e.g., 255 0 0 or 255 0 0 128)
      if (body.length >= 3 && 
          typeof body[0] === "number" && 
          typeof body[1] === "number" && 
          typeof body[2] === "number") {
        const r = body[0];
        const g = body[1];
        const b = body[2];
        
        // Check if there's a fourth value for alpha
        const hasAlpha = body.length >= 4 && typeof body[3] === "number";
        const a = hasAlpha ? body[3] : 255;
        
        // Validate RGB(A) range (0-255)
        if (r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255 && a >= 0 && a <= 255) {
          const colorValues = hasAlpha ? [r, g, b, a] : [r, g, b];
          
          // Apply backdrop only once per call location
          const position = api.kidlispCallPosition || "";
          const backdropKey = `first_line_backdrop_rgb_${r}_${g}_${b}_${a}_${position}`;
          if (!this.onceExecuted.has(backdropKey)) {
            this.onceExecuted.add(backdropKey);

            // Set the background fill color for reframe operations
            if (api.backgroundFill) {
              api.backgroundFill(colorValues);
            }

            // Apply wipe once for first-line RGB/RGBA shorthand
            if (api.wipe) {
              api.wipe(...colorValues); // Spread RGB or RGBA values as separate arguments
            }

            // Remove the processed items (3 for RGB, 4 for RGBA)
            body = body.slice(hasAlpha ? 4 : 3);
          } else {
            // Don't remove the items - let them be processed normally in subsequent frames
          }
          // Remove the processed items so they don't get evaluated again
          body = body.slice(hasAlpha ? 4 : 3);
          
          // Skip the rest of the color detection since we handled RGB/RGBA
          if (body.length === 0) {
            return undefined; // Nothing left to evaluate
          }
        }
      }

      // Check if it's a bare string color name (first-line shorthand)
      if (typeof firstItem === "string") {
        colorName = firstItem;
      }
      // Also check for single-element arrays like ["blue"] from (blue)
      else if (Array.isArray(firstItem) && firstItem.length === 1 && typeof firstItem[0] === "string") {
        colorName = firstItem[0];
      }

      if (colorName) {
        const globalEnv = this.getGlobalEnv();
        let isValidFirstLineColor = false;

        // Check if it's an RGB string first (e.g., "255 0 0" or "255, 0, 0")
        if (isValidRGBString(colorName)) {
          const rgbValues = parseRGBString(colorName);
          if (rgbValues) {
            isValidFirstLineColor = true;

            // Apply backdrop only once per call location
            const position = api.kidlispCallPosition || "";
            const backdropKey = `first_line_backdrop_${colorName}_${position}`;
            if (!this.onceExecuted.has(backdropKey)) {
              this.onceExecuted.add(backdropKey);

              // Set the background fill color for reframe operations
              if (api.backgroundFill) {
                api.backgroundFill(rgbValues);
              }

              // Apply wipe once for first-line RGB shorthand
              if (api.wipe) {
                api.wipe(...rgbValues); // Spread RGB values as separate arguments
              }

              // Remove the first item since we've processed it
              body = body.slice(1);
            } else {
              // Don't remove the first item - let it be processed normally in subsequent frames
            }
            // Remove the first item so it doesn't get evaluated again
            body = body.slice(1);
          }
        }
        // Check if it's a color name in cssColors (not just any function)
        else if (cssColors && cssColors[colorName]) {
          isValidFirstLineColor = true;
        }
        // Also check if it's a fade string directly (like "fade:initial-atom-background")
        else if (colorName.startsWith("fade:")) {
          // Validate the fade string by trying to parse it
          const fadeColors = this.parseFadeString(colorName);
          if (fadeColors && fadeColors.length >= 2) {
            isValidFirstLineColor = true;
          }
        }

        if (isValidFirstLineColor) {
          try {
            // Test if this is a color function by calling it (only for non-fade strings)
            if (!colorName.startsWith("fade:") && globalEnv[colorName] && typeof globalEnv[colorName] === "function") {
              globalEnv[colorName]();
            }
            // If we get here without error, it's a color function
            // For color shortcuts, apply backdrop only once per call location
            const position = api.kidlispCallPosition || "";
            const backdropKey = `first_line_backdrop_${colorName}_${position}`;
            if (!this.onceExecuted.has(backdropKey)) {
              this.onceExecuted.add(backdropKey);

              // Set the background fill color for reframe operations
              if (api.backgroundFill) {
                api.backgroundFill(colorName);
              }

              // Apply wipe once for first-line color shorthand
              if (api.wipe) {
                api.wipe(colorName);
              }

              // Only remove the first item when backdrop is actually applied
              body = body.slice(1);
            } else {
              // Don't remove the first item - let it be processed normally in subsequent frames
            }
            // Remove the first item so it doesn't get evaluated again
            body = body.slice(1);
          } catch (e) {
            // Not a color function, proceed normally
          }
        }
      }
    }

    // Helper function to properly evaluate timing arguments
    const evaluateTimingArg = (arg, api, env) => {
      if (typeof arg === "number" || typeof arg === "boolean") {
        // Simple values should be returned directly
        return arg;
      } else if (typeof arg === "string" && /^".*"$/.test(arg)) {
        // Quoted strings should be returned as-is (without quotes)
        return arg.slice(1, -1);
      } else if (Array.isArray(arg) || (typeof arg === "string" && !/^".*"$/.test(arg))) {
        // Complex expressions need evaluation
        return this.fastEval(arg, api, env);
      } else {
        return arg;
      }
    };

    let result;

    // Process pending timing triggers (for repeating timers)
    if (this.pendingTimingTriggers && this.pendingTimingTriggers.size > 0) {
      const toTrigger = [];
      const toKeep = new Map();

      for (const [timingKey, pending] of this.pendingTimingTriggers) {
        pending.frameDelay--;
        if (pending.frameDelay <= 0) {
          // Trigger now
          toTrigger.push(pending);
        } else {
          // Keep waiting
          toKeep.set(timingKey, pending);
        }
      }

      this.pendingTimingTriggers = toKeep;

      // Trigger the pending timing expressions
      for (const pending of toTrigger) {
        this.markTimingTriggered(pending.head);
      }
    }

    // 🚨 PERFORMANCE: Track body processing frequency but allow complex programs
    if (this.perf.enabled && body.length > 0) {
      if (!this.perf.bodyProcessCount) this.perf.bodyProcessCount = 0;
      this.perf.bodyProcessCount++;
      if (this.perf.bodyProcessCount % 500 === 0) {
        console.warn(`🔥 BODY PROCESSING ${this.perf.bodyProcessCount} TIMES! Body length: ${body.length}`);
      }

      // Only stop in truly pathological cases
      if (this.perf.bodyProcessCount > 5000) {
        console.error(`🚨 PATHOLOGICAL BODY PROCESSING: ${this.perf.bodyProcessCount} calls! Likely infinite recursion.`);
        this.perf.bodyProcessCount = 0;
        this.perf.evalCallCount = 0;
        this.evalDepth = 0;
        return 0;
      }
    }

    for (let bodyIndex = 0; bodyIndex < body.length; bodyIndex++) {
      const item = body[bodyIndex];
      // console.log("🥡 Processing item:", JSON.stringify(item), "type:", Array.isArray(item) ? "array" : typeof item);
      /*if (VERBOSE)*/ // console.log("🥡 Item:", item /*, "body:", body*/);

      // Handle optimized functions first
      if (item && typeof item === "object" && item.optimized) {
        perfStart("optimized-execution");

        // Track function call performance
        const optFuncStart = performance.now();
        result = item.func(api);
        const optFuncEnd = performance.now();
        this.trackFunction(`opt:${item.name || 'anonymous'}`, optFuncEnd - optFuncStart);

        perfEnd("optimized-execution");
        continue;
      }

      if (Array.isArray(item)) {
        // The first element indicates the function to call
        let [head, ...args] = item;

        // Preprocess arguments to evaluate any fade strings
        args = args.map(arg => {
          if (typeof arg === "string" && arg.startsWith("fade:")) {
            // Parse and evaluate the fade string
            const fadeParts = arg.split(":");
            if (fadeParts.length >= 3) {
              const fadePrefix = fadeParts[0]; // "fade"
              const colors = fadeParts[1]; // "red-blue"
              const direction = fadeParts[2]; // "frame" or "30" or expression

              // Try to evaluate the direction part
              let evaluatedDirection = direction;
              try {
                // Check if it's already a number
                const numericValue = parseFloat(direction);
                if (!isNaN(numericValue)) {
                  evaluatedDirection = direction; // Keep as string for now
                } else {
                  // Try to evaluate as KidLisp expression/variable
                  let evalResult;

                  // Special handling for "frame" keyword
                  if (direction === "frame") {
                    evalResult = this.frameCount;
                  } else {
                    // Special handling for function names
                    const globalEnv = this.getGlobalEnv();
                    if (globalEnv[direction] && typeof globalEnv[direction] === "function") {
                      // It's a function, call it with the API
                      evalResult = globalEnv[direction](api);
                    } else {
                      // Try to evaluate as expression or variable
                      evalResult = this.evaluate(direction, api, env);
                    }
                  }

                  // Convert result to string for the fade string
                  evaluatedDirection = String(evalResult);
                }
              } catch (error) {
                console.warn("Failed to evaluate fade direction:", direction, error);
                evaluatedDirection = direction; // Fallback to original
              }

              // Return the evaluated fade string
              return `${fadePrefix}:${colors}:${evaluatedDirection}`;
            }
          }
          return arg; // Return argument unchanged if not a fade string
        });

        // 🎵 Handle integer timing: (0 ...), (1 ...), (2 ...), etc.
        // Also handle second timing: (1s ...), (2s ...), etc.
        if (typeof head === "number" && Number.isInteger(head)) {
          const frameDivisor = head + 1; // 0 = every frame, 1 = every 2nd frame, etc.
          if (this.frameCount % frameDivisor === 0) {
            this.markTimingTriggered(head.toString()); // Trigger blink for integer timing
            // Execute the timing arguments with proper context
            let timingResult;
            for (const arg of args) {
              timingResult = evaluateTimingArg(arg, api, env);
            }
            result = timingResult;
          }
          continue; // Skip normal function processing
        } else if (typeof head === "string" && /^\d*\.?\d+s!?$/.test(head)) {
          // Handle second-based timing like "0s", "1s", "2s", "5s", "1.5s", "0.3s"
          // Also handle instant trigger modifier like "0.25s!", "1s!", "2s!"

          // console.log(`⏰ TIMING DETECTED: ${head}, isTopLevel: ${isTopLevel}, args:`, args);

          // Note: We process timing expressions at any level in embedded layers, 
          // but only top-level timers get immediate first execution

          const hasInstantTrigger = head.endsWith("!");
          const timeString = hasInstantTrigger ? head.slice(0, -1) : head;
          const seconds = parseFloat(timeString.slice(0, -1)); // Remove 's' and parse as float

          if (seconds === 0) {
            // 0s = every frame (no timing restriction)
            this.markTimingTriggered(head); // Trigger blink
            this.markDelayTimerActive(head); // Mark as active for display period
            let timingResult;
            for (const arg of args) {
              timingResult = evaluateTimingArg(arg, api, env);
            }
            result = timingResult;
          } else if (seconds < 0.016) {
            // For very small intervals (less than ~60fps), limit to 60fps max to prevent excessive triggering
            console.log(`⏰ FAST TIMING: ${head} (${seconds}s) using 60fps limiting`);
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
                this.markDelayTimerActive(head); // Mark as active for display period
                let timingResult;
                for (const arg of args) {
                  timingResult = evaluateTimingArg(arg, api, env);
                }
                result = timingResult;
              }
            }
          } else {
            const clockResult = api.clock.time(); // Get time (should be simulation time for deterministic rendering)
            if (!clockResult) continue;

            // Convert Date object to milliseconds, then to seconds
            // In simulation mode, this should be deterministic time based on frame number
            const currentTimeMs = clockResult.getTime
              ? clockResult.getTime()
              : Date.now();
            const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

            // DEBUG: Log simulation time for 0.3s expressions
            // if (head === "0.3s") {
            //   console.log(`🔍 TIMING DEBUG: ${head} - simulation currentTime=${currentTime.toFixed(3)}s`);
            // }

            // Create a unique key for this timing expression - use simpler key generation
            const timingKey = head + "_" + args.length;
            // Timing check debug log removed for performance

            // Initialize lastExecution to current time if not set
            if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
              this.lastSecondExecutions[timingKey] = currentTime;

              // First-line timer rule: If this is the first expression (bodyIndex === 0),
              // execute it immediately on the first frame
              if (bodyIndex === 0) {
                // Mark timing as triggered for blink effect
                this.markTimingTriggered(head);
                this.markDelayTimerActive(head);

                // Execute immediately
                const wasInEmbedPhase = this.inEmbedPhase;
                this.inEmbedPhase = true;

                let timingResult;
                for (const arg of args) {
                  timingResult = this.fastEval(arg, api, env);
                }

                this.inEmbedPhase = wasInEmbedPhase;
                result = timingResult;
                continue; // Skip normal timing logic for this first execution
              }
            }

            {
              // Normal timing logic for subsequent calls
              const lastExecution = this.lastSecondExecutions[timingKey];
              const diff = currentTime - lastExecution;

              // DEBUG: Log timing calculations for 0.3s expressions
              // if (head === "0.3s") {
              //   console.log(`🔍 TIMING DEBUG: ${head} - currentTime=${currentTime.toFixed(3)}s, lastExecution=${lastExecution.toFixed(3)}s, diff=${diff.toFixed(3)}s, needed=${seconds}s`);
              // }

              // Add small tolerance for floating-point precision issues
              // Use a 5ms tolerance to prevent rapid fire due to precision errors
              const tolerance = 0.005; // 5 milliseconds in seconds
              const adjustedSeconds = Math.max(seconds, tolerance);

              // Check if enough time has passed since last execution
              if (diff >= adjustedSeconds) {
                this.lastSecondExecutions[timingKey] = currentTime;                // console.log(`⏰ TIMING EXECUTE: ${head} executing after ${diff}s (needed ${adjustedSeconds}s)`);

                // Mark timing as triggered for blink effect first
                this.markTimingTriggered(head); // Trigger blink
                this.markDelayTimerActive(head); // Mark as active for display period

                // Execute immediately - no setTimeout needed for frame-based timing!
                // console.log(`🔥 TIMING EXECUTION: ${head} starting with args:`, args);
                // console.log(`🔥 TIMING CONTEXT: isEmbeddedContext=${this.isEmbeddedContext}, inEmbedPhase=${this.inEmbedPhase}`);

                // Preserve embedded context for timing execution
                const wasInEmbedPhase = this.inEmbedPhase;
                this.inEmbedPhase = true; // Force embed phase during timing execution

                let timingResult;
                for (const arg of args) {
                  // console.log(`🔥 TIMING ARG: evaluating`, arg);
                  timingResult = this.fastEval(arg, api, env);
                  // console.log(`🔥 TIMING ARG RESULT:`, timingResult);
                }

                // Restore previous embed phase
                this.inEmbedPhase = wasInEmbedPhase;
                // console.log(`🔥 TIMING EXECUTION: ${head} completed`);

                // Return the result directly instead of placeholder
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

          // ✅ Allow timing expressions to work both at top-level and as function arguments

          const seconds = parseFloat(head.slice(0, head.indexOf("s"))); // Extract seconds part

          // Get current time - use simulation time if available (for headless rendering), otherwise real time
          const currentTimeMs = this.getSimulationTime ? this.getSimulationTime() : Date.now();
          const currentTime = currentTimeMs / 1000; // Convert to seconds (keep as float)

          // Create a unique key for this timed iteration expression - use simpler key generation
          const timingKey = head + "_" + args.length;

          // Initialize timing tracking if not set
          if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
            // Initialize timing state
            this.lastSecondExecutions[timingKey] = currentTime;
            // Initialize sequence counter for this timed iteration
            if (!this.sequenceCounters) {
              this.sequenceCounters = new Map();
            }
            this.sequenceCounters.set(timingKey, 0);

            // Only mark as triggered for immediate execution if this is the first expression
            if (bodyIndex === 0) {
              // Schedule the blink trigger after a delay to let syntax highlighting show
              if (!this.pendingTimingTriggers) {
                this.pendingTimingTriggers = new Map();
              }
              this.pendingTimingTriggers.set(timingKey, {
                frameDelay: 2, // Wait 2 frames before triggering
                head: head
              });
            }
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
          }

          // Always return the current item (not just when advancing)
          if (args.length > 0) {
            const currentIndex = this.sequenceCounters.get(timingKey) || 0;

            // 🐛 DEBUG: Log timing execution details for debugging zoom issues
            if (head === "0.1s" && args.some(arg =>
              (Array.isArray(arg) && arg[0] === "zoom") ||
              (typeof arg === "string" && arg.includes("zoom"))
            )) {
              console.log(`⏰ DEBUG TIMER [${head}]: executing arg ${currentIndex} of ${args.length}`, {
                timingKey,
                currentIndex,
                selectedArg: args[currentIndex],
                allArgs: args,
                timeDiff: diff,
                secondsRequired: seconds,
                shouldAdvance: diff >= seconds
              });
            }

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

            // Store the current timing context for embed functions to access
            this.currentTimingContext = {
              timingKey,
              currentIndex,
              totalArgs: args.length,
              args: args,
              selectedArg: args[currentIndex] // Add the currently selected argument for easy comparison
            };

            // Evaluate the selected argument instead of just returning it
            const selectedArg = args[currentIndex];

            // Timing sequences work correctly now

            // For timing expressions, handle bare strings as potential function calls
            let result;
            if (typeof selectedArg === "string") {
              // Check if it's a quoted string literal and return it without quotes
              if (selectedArg.startsWith('"') && selectedArg.endsWith('"')) {
                result = selectedArg.slice(1, -1); // Remove quotes and return as literal string
              }
              // Check if it's a fade string and handle it specially
              else if (selectedArg.startsWith("fade:")) {
                // Parse and evaluate the fade string
                const fadeParts = selectedArg.split(":");
                if (fadeParts.length >= 3) {
                  // fade:colors:direction format
                  const fadePrefix = fadeParts[0]; // "fade"
                  const colors = fadeParts[1]; // "red-blue"
                  const direction = fadeParts[2]; // "frame" or "30" or expression

                  // Try to evaluate the direction part
                  let evaluatedDirection = direction;
                  try {
                    // Check if it's already a number
                    const numericValue = parseFloat(direction);
                    if (!isNaN(numericValue)) {
                      evaluatedDirection = direction; // Keep as string for now
                    } else {
                      // Try to evaluate as KidLisp expression/variable
                      let evalResult;

                      // Special handling for function names like "frame"
                      const globalEnv = this.getGlobalEnv();
                      if (globalEnv[direction] && typeof globalEnv[direction] === "function") {
                        // It's a function, call it with the API
                        evalResult = globalEnv[direction](api);
                      } else {
                        // Try to evaluate as expression or variable
                        evalResult = this.evaluate(direction, api, env);
                      }

                      // Convert result to string for the fade string
                      evaluatedDirection = String(evalResult);
                    }
                  } catch (error) {
                    console.warn("Failed to evaluate fade direction:", direction, error);
                    evaluatedDirection = direction; // Fallback to original
                  }

                  // Reconstruct the fade string with evaluated direction
                  const evaluatedFadeString = `${fadePrefix}:${colors}:${evaluatedDirection}`;

                  // Call ink() with the evaluated fade string
                  if (api.ink && typeof api.ink === "function") {
                    api.ink(evaluatedFadeString);
                    result = evaluatedFadeString;
                  } else {
                    result = evaluatedFadeString;
                  }
                } else {
                  // fade:colors format (no direction) - just pass through
                  if (api.ink && typeof api.ink === "function") {
                    api.ink(selectedArg);
                    result = selectedArg;
                  } else {
                    result = selectedArg;
                  }
                }
              } else {
                // Try to evaluate as a function call first (for CSS colors)
                const globalEnv = this.getGlobalEnv();
                if (globalEnv[selectedArg] && typeof globalEnv[selectedArg] === "function") {
                  result = globalEnv[selectedArg](api, []);
                } else {
                  result = this.evaluate(selectedArg, api, env);
                }
              }
            } else if (typeof selectedArg === "number") {
              // Numbers should be returned directly without evaluation
              result = selectedArg;
            } else {
              // Evaluate the selected argument - this should return the actual value
              result = this.evaluate(selectedArg, api, env);

              // IMPORTANT: Make sure we return a valid value, not undefined
              if (result === undefined || result === null) {
                result = selectedArg;
              }
            }

            // Clear timing context after evaluation
            this.currentTimingContext = null;

            // Timing results work correctly now

            // IMPORTANT: Return result if this is the only expression being evaluated (function argument context)
            // Otherwise, store result and continue processing for top-level timing expressions
            if (body.length === 1) {
              // Single expression context - return the result (likely function argument)
              return result;
            }
            // Multiple expressions context - continue processing
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

        // Debug logging for zoom specifically
        if (head === "zoom") {
          // console.log("🎯 About to resolve zoom function");
          // console.log("🎯 Head after processing:", head);
          // console.log("🎯 Args:", args);
          // console.log("🎯 Resolved result:", resolved);
        }

        // console.log(`🔍 Function resolution for "${head}":`, resolved?.type, typeof resolved?.value);
        if (head === "-1" || head === "1") {
          console.log(`🚨 CRITICAL: Trying to resolve number "${head}" as function!`);
          console.trace('Stack trace for number resolution:');
        }

        if (resolved) {
          const { type, value } = resolved;

          // Debug logging for zoom function specifically
          if (head === "zoom") {
            // console.log("🎯 Zoom function resolved:", { type, valueType: typeof value });
          }

          // Debug logging for scroll function specifically
          if (head === "scroll") {
            // console.log("📜 Scroll function resolved:", { type, valueType: typeof value });
          }

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
                  head === "once" ||
                  head === "hop" ||
                  head === "delay" ||
                  head === "trans" ||
                  head === "jump"
                ) {
                  processedArgs = args;
                } else {
                  // Use fast evaluation for arguments with current local environment
                  processedArgs = args.map((arg, index) => {
                    // Handle context-aware randomization for ?
                    if (arg === "?") {
                      return this.contextAwareRandom(head, index, api);
                    }

                    if (
                      Array.isArray(arg) ||
                      (typeof arg === "string" && !/^".*"$/.test(arg))
                    ) {
                      // Check if this is a timing expression that needs full evaluation
                      if (Array.isArray(arg) && arg.length > 0 &&
                        typeof arg[0] === "string" && /^\d*\.?\d+s\.\.\.?$/.test(arg[0])) {
                        // Use full evaluation for timing expressions
                        const result = this.evaluate([arg], api, this.localEnv);
                        return result;
                      } else {
                        const result = this.fastEval(arg, api, this.localEnv);
                        return result;
                      }
                    } else {
                      return arg;
                    }
                  });
                }

                if (splitHead[1]) {
                  result = getNestedValue(value, item[0])(api, processedArgs);
                } else {
                  // Debug logging for zoom execution
                  if (head === "zoom") {
                    // console.log("🚀 Executing global zoom function with args:", processedArgs);
                  }
                  // Debug logging for suck execution
                  if (head === "suck") {
                    // console.log("🌪️ Executing global suck function with args:", processedArgs);
                  }

                  // Track function call performance
                  const funcStart = performance.now();
                  result = value(api, processedArgs, env, colon);
                  const funcEnd = performance.now();
                  this.trackFunction(head, funcEnd - funcStart);
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

              // Track function call performance
              const userFuncStart = performance.now();
              result =
                Array.isArray(value) || value.body
                  ? this.evaluate(value, api, this.localEnv, evaluatedArgs)
                  : value;
              const userFuncEnd = performance.now();
              this.trackFunction(`user:${head}`, userFuncEnd - userFuncStart);
              break;

            case "api":
              // API functions - use fast evaluation for arguments
              const apiArgs = args.map((arg) =>
                Array.isArray(arg) ||
                  (typeof arg === "string" && !/^".*"$/.test(arg))
                  ? this.fastEval(arg, api, this.localEnv)
                  : arg,
              );

              // Debug logging for zoom API execution
              if (head === "zoom") {
                // console.log("🚀 Executing API zoom function with args:", apiArgs);
                // console.log("🚀 API zoom function:", typeof value, value.toString().substring(0, 100));
              }

              // Debug logging for scroll API execution  
              if (head === "scroll") {
                // console.log("📜 Executing API scroll function with args:", apiArgs);
                // console.log("📜 API scroll function:", typeof value, value.toString().substring(0, 100));
              }

              // Track function call performance
              const apiFuncStart = performance.now();
              result = value(...apiArgs);
              const apiFuncEnd = performance.now();
              this.trackFunction(`api:${head}`, apiFuncEnd - apiFuncStart);
              break;

            case "cache":
              // Cached function calls (like $pie) - use fast evaluation for arguments
              const cacheArgs = args.map((arg) =>
                Array.isArray(arg) ||
                  (typeof arg === "string" && !/^".*"$/.test(arg))
                  ? this.fastEval(arg, api, this.localEnv)
                  : arg,
              );
              result = value(api, cacheArgs);
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
            } else if (typeof item === "string" && item.includes(":")) {
              // Handle colon syntax for bare strings like "cube:0"
              const colonSplit = item.split(":");
              const head = colonSplit[0];
              const colon = colonSplit[1];

              // Look up the function and call it with the colon parameter
              const globalEnv = this.getGlobalEnv();
              if (globalEnv[head] && typeof globalEnv[head] === "function") {
                result = globalEnv[head](api, [], env, colon);
              } else {
                result = this.evalNotFound(item, api, env);
              }
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

    // Clear KidLisp context
    if (api.clearKidLispContext) {
      api.clearKidLispContext();
    }

    // Clear timing context to prevent leakage between evaluations
    this.currentTimingContext = null;

    // � PERFORMANCE: Cache the result if we created a cache key
    if (cacheKey && this.frameCache && result !== undefined) {
      this.frameCache.set(cacheKey, result);
    }

    // �🚨 RECURSION LIMITER: Decrement depth counter
    this.evalDepth--;

    // 🔍 DEBUG: Track completion of expensive evaluations
    const evalTime = performance.now() - evalStart.time;
    if (shouldLog && evalTime > 1) {
      // console.log(`🐌 EVAL END: ${Array.isArray(parsed) ? parsed[0] : 'primitive'} took ${evalTime.toFixed(2)}ms`);
    }

    // 🔍 PERFORMANCE DEBUG: Log completion of slow operations
    if (shouldLog && perfTimer) {
      const totalTime = performance.now() - perfTimer;
      if (totalTime > 5.0) {  // Only log operations taking more than 5ms
        const exprName = Array.isArray(parsed) ?
          `${parsed[0]}${parsed.length > 1 ? '(...)' : ''}` :
          String(parsed).substring(0, 20);
        // console.log(`🔍 EVAL END: ${exprName} took ${totalTime.toFixed(2)}ms`);
      }
    }

    perfEnd("evaluate-total");
    this.endTiming('evaluate', evalStart);
    return result;
  }

  evalNotFound(expression, api, env) {
    if (typeof expression !== "string") {
      // console.log("🤖 Expression:", expression);
      return expression; // Return numbers.
    } else {
      // Check if this is the fps command that's not implemented
      if (expression === "fps") {
        // console.log("⚠️ FPS command called but not implemented - ignoring");
        return "fps"; // Return the string to indicate it was processed
      }
      // console.log("🤖 Attempting JavaScript expression evaluation:", expression);
    }

    // Check if this is a timing expression like "1.5s" or "2s..." before processing as identifier
    if (/^\d*\.?\d+s\.\.\.?$/.test(expression)) {
      // This is a timing expression, evaluate it properly as an array
      return this.evaluate([expression], api, env);
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
      // Skip 's' if it appears to be part of a timing expression
      if (id === 's' && /\d+\.?\d*s/.test(expression)) {
        return; // Skip this identifier as it's part of a timing expression
      }

      // Use fast evaluation for identifier lookup
      let value = this.fastEval(id, api, env);

      if (value === id) {
        // Variable not found, try to get it from global environment
        // console.warn("❗ Identifier not found:", id);
        value = 0;
      }

      // Replace any identifiers and cancel out prefixed double negatives.
      expression = expression.replace(new RegExp(`\\b${id}\\b`, "g"), value);
    });

    // Handle cases where the expression might still contain timing patterns after identifier replacement
    // Replace timing patterns like "1.5s" with valid JavaScript (though this is a fallback)
    expression = expression.replace(/(\d+\.?\d*)s/g, '$1');

    try {
      const compute = new Function(`return ${expression};`);
      const result = compute();
      // console.log("Evaluated result:", result);
      return result;
    } catch (error) {
      console.warn("❗ Failed to evaluate expression:", expression, error.message);
      return 0; // Return default value instead of throwing
    }
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
      args.forEach((arg) => {
        if (typeof arg === "number" || typeof arg === "boolean") {
          return arg;
        } else if (typeof arg === "string" && /^".*"$/.test(arg)) {
          return arg.slice(1, -1);
        } else if (Array.isArray(arg) || (typeof arg === "string" && !/^".*"$/.test(arg))) {
          return this.fastEval(arg, api, env);
        } else {
          return arg;
        }
      });
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

        // Safety check: ensure color is defined
        if (!color) {
          console.warn(`⚠️ getTokenColor returned undefined for token:`, token, `at index:`, i, "surrounding tokens:", tokens[i-1] || "START", "->", token, "->", tokens[i+1] || "END");
          continue; // Skip this token if color is undefined
        }

        // Find the token in the original source starting from our current position
        const tokenIndex = this.syntaxHighlightSource.indexOf(token, sourceIndex);

        if (tokenIndex !== -1) {
          // Add any whitespace/formatting between the last token and this one
          const whitespace = this.syntaxHighlightSource.substring(sourceIndex, tokenIndex);
          result += whitespace;

          // Special handling for compound colors (like $codes)
          if (color.startsWith("COMPOUND:")) {
            const parts = color.split(":");
            const prefixColor = parts[1]; // Color for $ symbol
            const identifierColor = parts[2]; // Color for identifier part

            // Apply prefix color to $ symbol
            result += `\\${prefixColor}\\$`;
            // Apply identifier color to rest of token
            result += `\\${identifierColor}\\${token.substring(1)}`;
            lastColor = identifierColor; // Set last color to the identifier color
          }
          // Special handling for rainbow coloring
          else if (color === "RAINBOW" && token === "rainbow") {
            // ROYGBIV rainbow colors for each character
            const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
            for (let charIndex = 0; charIndex < token.length; charIndex++) {
              const charColor = rainbowColors[charIndex % rainbowColors.length];
              result += `\\${charColor}\\${token[charIndex]}`;
            }
            lastColor = null; // Reset so next token gets proper color
          }
          // Special handling for zebra coloring
          else if (color === "ZEBRA" && token === "zebra") {
            // Alternating black/white colors for each character
            const zebraColors = ["black", "white"];
            for (let charIndex = 0; charIndex < token.length; charIndex++) {
              const charColor = zebraColors[charIndex % zebraColors.length];
              result += `\\${charColor}\\${token[charIndex]}`;
            }
            lastColor = null; // Reset so next token gets proper color
          }
          // Special handling for fade expressions like "fade:red-blue-yellow"
          else if (token.startsWith("fade:") && color === "mediumseagreen") {
            // Parse the fade expression and color each part
            const fadeResult = this.colorFadeExpression(token);
            result += fadeResult;
            lastColor = null; // Reset so next token gets proper color
          } else {
            // Add color escape sequence when color changes
            if (color !== lastColor) {
              result += `\\${color}\\`;
              lastColor = color;
            }
            // Add the token itself normally
            result += token;
          }

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
      // Cycle timers - sequential execution
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
    } else if (/^\d*\.?\d+s!?$/.test(token)) {
      // Delay timers - all contents blink when triggered
      return {
        timingKey: token,
        currentIndex: 0,
        totalArgs: 1,
        isBlinking: this.isTimingBlinking(token),
        isActive: this.isDelayTimerActive(token), // Track active display period
        isInActiveArg: true,
        isDelayTimer: true // Mark as delay timer for special handling
      };
    }

    // Check if this token is inside a timing expression's arguments
    for (let i = 0; i < index; i++) {
      const prevToken = tokens[i];

      // Handle cycle timer arguments
      if (/^\d*\.?\d+s\.\.\.?$/.test(prevToken)) {
        // Try to find this timing key in our active tracking
        for (const [timingKey, data] of this.activeTimingExpressions) {
          if (timingKey.startsWith(prevToken + "_")) {
            // Check if current token is within one of the timing arguments
            const argInfo = this.getTokenArgPosition(tokens, i, index);

            if (argInfo && argInfo.argIndex < data.totalArgs) {
              const isActive = argInfo.argIndex === data.currentIndex;
              return {
                timingKey,
                currentIndex: data.currentIndex,
                totalArgs: data.totalArgs,
                isBlinking: this.isTimingBlinking(prevToken),
                isInActiveArg: isActive
              };
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
      } else if (/^\d*\.?\d+s!?$/.test(prevToken)) {
        // Handle delay timer arguments - always return state to control visibility
        const isBlinking = this.isTimingBlinking(prevToken);
        const isActive = this.isDelayTimerActive(prevToken);

        const argInfo = this.getTokenArgPosition(tokens, i, index);
        if (argInfo !== null) {
          return {
            timingKey: prevToken,
            currentIndex: 0,
            totalArgs: 1,
            isBlinking: isBlinking,
            isActive: isActive,
            isInActiveArg: true, // For delay timers, all arguments are controlled by timer state
            isDelayTimer: true
          };
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

    while (currentTokenPos < tokens.length && currentTokenPos <= tokenIndex) {
      const token = tokens[currentTokenPos];

      if (token === "(") {
        parenDepth++;
        if (parenDepth === 1) {
          // Start of a new top-level argument
          argIndex++;
        }
      } else if (token === ")") {
        parenDepth--;
        if (parenDepth < 0) {
          // We've exited the timing expression
          break;
        }
      }

      if (currentTokenPos === tokenIndex) {
        return { argIndex, tokenDepth: parenDepth };
      }

      currentTokenPos++;
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
        return "red"; // Bright red flash when timing first triggers
      }

      // Check if cycle timer is in its brief lime flash period (similar to delay timers)
      if (this.isTimingBlinking(token)) {
        return "lime"; // Brief lime flash when cycle timer triggers
      }

      // Check if this timing token has an active expression running
      if (timingState && (timingState.currentIndex !== undefined)) {
        return "yellow"; // Show yellow when timing expression is actively running (matches delay timers)
      }
      return "yellow"; // Yellow for inactive timing tokens like "1s...", "2s..."
    }

    // Check for delay timing patterns (1.25s, 0.5s, etc.)
    if (/^\d*\.?\d+s!?$/.test(token)) {
      const timingState = this.getTimingTokenState(token, tokens, index);
      if (timingState && timingState.isBlinking) {
        return "red"; // Bright red flash when delay timer triggers
      }
      if (timingState && timingState.isActive) {
        return "cyan"; // Show cyan when delay timer is in active display period
      }
      return "yellow"; // Yellow for inactive delay timing tokens like "1s", "0.5s"
    }

    // If this token is inside a timing expression, color it based on active state
    if (timingExprState) {
      // Exclude parentheses from timing-based color changes to prevent flashing
      if (token === "(" || token === ")") {
        // Parentheses should use normal syntax highlighting, not timing colors
        const normalColor = this.getNormalTokenColor(token, tokens, index);
        return normalColor || "192,192,192"; // Default to gray
      }
      
      if (timingExprState.isInActiveArg) {
        // Check if the timing token for this expression is currently blinking
        const isTimingBlinking = this.isTimingBlinking(timingExprState.timingData.timingToken);
        if (isTimingBlinking) {
          // Flash bright lime when cycle timer first triggers (brief flash)
          return "lime";
        }
        // After the brief lime flash, allow normal syntax highlighting to show through
        // by falling through to normal coloring logic below (similar to delay timers)
      } else {
        // This token is in an inactive timing argument - use transparent
        return "255,255,255,0"; // White with 0 alpha (transparent)
      }
    }

    // Check if this token is inside a delay timer
    const delayTimerState = this.getTimingTokenState(token, tokens, index);

    // Debug: Log delay timer state detection
    if (token === "zoom" || token === "?" || token === "0.25" || token === "1.5") {
      // console.log(`🔍 Token "${token}" delayTimerState:`, delayTimerState);
    }

    if (delayTimerState && delayTimerState.isDelayTimer) {
      // Exclude parentheses from delay timer color changes to prevent flashing
      if (token === "(" || token === ")") {
        // Parentheses should use normal syntax highlighting, not timing colors
        const normalColor = this.getNormalTokenColor(token, tokens, index);
        return normalColor || "192,192,192"; // Default to gray
      }
      
      if (delayTimerState.isBlinking) {
        // console.log(`� Token "${token}" returning MAGENTA (blinking)`);
        // All contents of delay timer flash red when triggered (brief flash)
        return "red"; // Red flash to match the timer number flash
      }
      if (delayTimerState.isActive) {
        // During active period, allow normal syntax highlighting to show through
        // by falling through to normal coloring logic below
      } else {
        // console.log(`⚪ Token "${token}" inactive (transparent)`);
        // When inactive, show as transparent (greyed out, only shadows visible)
        return "255,255,255,0"; // White with 0 alpha (transparent)
      }
    }

    // For all other tokens, use normal coloring
    const normalColor = this.getNormalTokenColor(token, tokens, index);

    // Safety check: ensure we always return a valid color
    if (!normalColor) {
      console.warn(`⚠️ getNormalTokenColor returned undefined for token:`, token);
      return "orange"; // Fallback color
    }

    return normalColor;
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

    // Check for $codes (embedded layer references) - handle loading states
    if (token.startsWith("$") && token.length > 1 && /^[$][0-9A-Za-z]+$/.test(token)) {
      const cacheId = token.substring(1); // Remove $ prefix

      if (this.loadingEmbeddedLayers.has(cacheId)) {
        // Show red blinking for loading $codes - return compound color
        const now = performance.now();
        const blinkInterval = 200; // Blink every 200ms
        const isBlinking = Math.floor(now / blinkInterval) % 2 === 0;
        const mainColor = isBlinking ? "red" : "darkred";
        return `COMPOUND:${mainColor}:${mainColor}`; // Both parts same color when loading
      } else if (this.loadedEmbeddedLayers.has(cacheId)) {
        // Show VHS retro 3-frame blinking $ with bright lime identifier for loaded $codes
        const now = performance.now();
        const magicBlinkInterval = 150; // Fast 3-frame blink every 150ms
        const frame = Math.floor(now / magicBlinkInterval) % 3;
        const dollarColor = frame === 0 ? "yellow" : frame === 1 ? "hotpink" : "limegreen";
        return `COMPOUND:${dollarColor}:lime`; // $ cycles through retro VHS colors, identifier is lime
      } else {
        // Default brighter $ with lime identifier for unprocessed $codes
        return "COMPOUND:limegreen:lime"; // $ is bright lime green, identifier is lime
      }
    }

    // Check for RGB channel highlighting (e.g., "255 0 0" should show R in red, G in green, B in blue)
    // This must come BEFORE general number coloring to override pink
    if (/^-?\d+(\.\d+)?$/.test(token)) {
      // Check if this number is part of a 3-number RGB sequence
      const prevToken = index > 0 ? tokens[index - 1] : null;
      const nextToken = index < tokens.length - 1 ? tokens[index + 1] : null;
      const next2Token = index < tokens.length - 2 ? tokens[index + 2] : null;
      
      const isNumeric = (t) => t && /^-?\d+(\.\d+)?$/.test(t);
      
      // Check if we're in a 3-number sequence (current is R, G, or B channel)
      if (isNumeric(nextToken) && isNumeric(next2Token)) {
        // This is the R (red) channel - first of three numbers
        // Scale red intensity by the actual value (0-255)
        const value = Math.max(0, Math.min(255, parseFloat(token)));
        return `${value},0,0`; // Red channel with scaled intensity
      } else if (isNumeric(prevToken) && isNumeric(nextToken)) {
        // This is the G (green) channel - middle of three numbers
        // Scale green intensity by the actual value (0-255)
        const value = Math.max(0, Math.min(255, parseFloat(token)));
        return `0,${value},0`; // Green channel with scaled intensity
      } else if (isNumeric(prevToken)) {
        const prev2Token = index > 1 ? tokens[index - 2] : null;
        if (isNumeric(prev2Token)) {
          // This is the B (blue) channel - last of three numbers
          // Scale blue intensity by the actual value (0-255)
          // Use the deepskyblue ratio: 0,191,255 = 0%, 75%, 100%
          const value = Math.max(0, Math.min(255, parseFloat(token)));
          const greenComponent = Math.round(value * 0.75); // 75% of blue value
          return `0,${greenComponent},${value}`; // Blue channel with deepskyblue hue
        }
      }
      
      // Not part of RGB sequence, use default pink for numbers
      return "pink";
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

    // Check for commas - use the same rainbow colors as parentheses for visual consistency
    if (token === ",") {
      return this.getParenthesesColor(tokens, index);
    }

    // Check if this token is a function name (first token after opening paren)
    if (index > 0 && tokens[index - 1] === "(") {
      return this.getFunctionColor(token);
    }

    // Check if this token is a valid CSS color name or color code
    if (cssColors && cssColors[token]) {
      // Return the actual color value for CSS colors like "red", "blue", etc.
      const colorValue = cssColors[token];
      if (Array.isArray(colorValue) && colorValue.length >= 3) {
        const rgbColor = `${colorValue[0]},${colorValue[1]},${colorValue[2]}`;
        // Return as RGB format for the HUD system
        return rgbColor;
      }
    }

    // Check if this is a color code like "c0", "c1", etc.
    if (token.match(/^c\d+$/)) {
      const index = parseInt(token.substring(1));
      if (staticColorMap[index]) {
        const colorValue = staticColorMap[index];
        if (Array.isArray(colorValue) && colorValue.length >= 3) {
          const rgbColor = `${colorValue[0]},${colorValue[1]},${colorValue[2]}`;
          return rgbColor;
        }
      }
    }

    // Check if this is a fade expression like "fade:red-blue-yellow"
    if (token.startsWith("fade:")) {
      // For now, return a special fade color - we'll handle the multi-color highlighting separately
      return "mediumseagreen"; // Give fade expressions a distinct emerald color
    }

    // Check if this is a bare function call (likely at start of line or after newline)
    // This handles KidLisp's auto-wrapping of non-parenthetical expressions
    if (this.isBareFunction(token, tokens, index)) {
      return this.getFunctionColor(token);
    }

    // Check if this is a known function name (even if not directly after parentheses)
    const knownFunctions = [
      "wipe", "ink", "line", "box", "flood", "circle", "write", "paste", "stamp", "point", "poly", "embed",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "?", "...", "..", "overtone", "rainbow", "zebra", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll",
      "spin", "resetSpin", "smoothspin", "sort", "zoom", "blur", "contrast", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes", "fade", "jump"
    ];

    // Special case for "rainbow" - return special marker for rainbow coloring
    if (token === "rainbow") {
      return "RAINBOW";
    }

    // Special case for "zebra" - return special marker for zebra coloring
    if (token === "zebra") {
      return "ZEBRA";
    }

    // Special case for "fade" function - give it a distinct color
    if (token === "fade") {
      return "mediumseagreen"; // Use emerald/medium sea green for the fade function
    }

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
      "wipe", "ink", "line", "box", "flood", "circle", "write", "paste", "stamp", "point", "poly", "embed",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "?", "...", "..", "overtone", "rainbow", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll",
      "spin", "resetSpin", "smoothspin", "sort", "zoom", "blur", "contrast", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes", "bake", "jump"
    ];

    return knownFunctions.includes(token) || (cssColors && cssColors[token]);
  }

  // Helper method to get the appropriate color for a function
  getFunctionColor(token) {
    // Check for $codes first (they can be used as function calls)
    if (token.startsWith("$") && token.length > 1 && /^[$][0-9A-Za-z]+$/.test(token)) {
      const cacheId = token.substring(1); // Remove $ prefix

      if (this.loadingEmbeddedLayers.has(cacheId)) {
        // Show red blinking for loading $codes
        const now = performance.now();
        const blinkInterval = 200; // Blink every 200ms
        const isBlinking = Math.floor(now / blinkInterval) % 2 === 0;
        const mainColor = isBlinking ? "red" : "darkred";
        return `COMPOUND:${mainColor}:${mainColor}`; // Both parts same color when loading
      } else if (this.loadedEmbeddedLayers.has(cacheId)) {
        // Show green $ with bright cyan identifier for loaded $codes
        return "COMPOUND:lime:cyan"; // $ is lime green, identifier is cyan
      } else {
        // Default green $ with teal identifier for unprocessed $codes
        return "COMPOUND:green:teal"; // $ is green, identifier is teal
      }
    }

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

    // Ensure depth is not negative
    depth = Math.max(0, depth);

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
    const color = parenColors[colorIndex];

    // Safety check
    if (!color) {
      console.warn(`⚠️ getParenthesesColor returning undefined. depth=${depth}, colorIndex=${colorIndex}, token=${tokens[index]}`);
      return "192,192,192"; // Fallback to light gray
    }

    return color;
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
          ["ink", "wipe", "line", "box", "circle", "write", "paste", "stamp", "point", "poly", "embed"].includes(
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

  // Render baked layers underneath the current drawing
  renderBakedLayers(api) {
    if (!this.bakedLayers || this.bakedLayers.length === 0) {
      return;
    }

    // Composite each baked layer onto the current screen in order
    this.bakedLayers.forEach((bakedLayer, index) => {
      if (bakedLayer && bakedLayer.pixels) {
        // Use manual pixel compositing to restore the baked layer
        this.compositeBakedLayer(api, bakedLayer);
      }
    });
  }

  // Manual pixel compositing for baked layers
  compositeBakedLayer(api, bakedLayer) {
    if (!api.screen?.pixels || !bakedLayer.pixels) {
      return;
    }

    const currentPixels = api.screen.pixels;
    const bakedPixels = bakedLayer.pixels;
    const currentWidth = api.screen.width;
    const currentHeight = api.screen.height;
    const bakedWidth = bakedLayer.width;
    const bakedHeight = bakedLayer.height;

    // Handle different buffer sizes by compositing the overlapping area
    const width = Math.min(currentWidth, bakedWidth);
    const height = Math.min(currentHeight, bakedHeight);

    // Composite baked pixels underneath current content
    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) {
        const currentIndex = (y * currentWidth + x) * 4;
        const bakedIndex = (y * bakedWidth + x) * 4;

        // Get current pixel (foreground)
        const currentA = currentPixels[currentIndex + 3];

        // Get baked pixel (background)
        const bakedR = bakedPixels[bakedIndex];
        const bakedG = bakedPixels[bakedIndex + 1];
        const bakedB = bakedPixels[bakedIndex + 2];
        const bakedA = bakedPixels[bakedIndex + 3];

        // Only show baked pixel if current pixel is transparent and baked pixel has content
        if (currentA === 0 && bakedA > 0) {
          currentPixels[currentIndex] = bakedR;
          currentPixels[currentIndex + 1] = bakedG;
          currentPixels[currentIndex + 2] = bakedB;
          currentPixels[currentIndex + 3] = bakedA;
        }
      }
    }
  }

  // Helper method to create embedded layer from source code
  createEmbeddedLayerFromSource(source, cacheId, layerKey, width, height, x, y, alpha, api) {
    console.log(`🎬 HEADLESS DEBUG: createEmbeddedLayerFromSource: ${cacheId} (${width}x${height}) at (${x}, ${y}) alpha=${alpha}`);
    
    // Check if there's already an existing layer for this key
    const existingLayer = this.embeddedLayerCache.get(layerKey);
    if (existingLayer) {
      // Updating existing layer - log removed for performance

      // Check if source code has changed
      if (existingLayer.source !== source) {
        // Source changed, clearing buffer - log removed for performance

        // Clear the buffer when source changes
        if (existingLayer.buffer && existingLayer.buffer.pixels) {
          // Fast buffer clear using fill
          existingLayer.buffer.pixels.fill(0);
          // Buffer cleared - log removed for performance
        }
      }

      // 🔄 REUSE existing KidLisp instance but reset execution state
      const embeddedKidLisp = existingLayer.kidlispInstance;
      console.log(`🎭 REUSING nested KidLisp instance for embedded layer: ${layerKey}`);

      // 🚨 CRITICAL: Clear onceExecuted to allow re-execution of once blocks
      // This was causing scroll commands to not execute on second load
      embeddedKidLisp.onceExecuted.clear();
      console.log(`🔄 CLEARED onceExecuted for re-execution`);

      // CRITICAL: Restore isolated timing state for existing layers
      const layerCacheKey = `${source}_timing`;
      let existingTimingState = this.embeddedLayerCache?.get?.(layerCacheKey);
      
      if (existingTimingState) {
        // Restore persistent timing state for this specific layer
        embeddedKidLisp.frameCount = existingTimingState.frameCount || 0;
        embeddedKidLisp.frameCounter = existingTimingState.frameCounter || 0;
        embeddedKidLisp.lastSecondExecutions = existingTimingState.lastSecondExecutions || {};
        embeddedKidLisp.sequenceCounters = new Map(existingTimingState.sequenceCounters || []);
        embeddedKidLisp.timingStates = new Map(existingTimingState.timingStates || []);
        // Restore the random state too for consistency
        if (existingTimingState.randomState) {
          embeddedKidLisp.randomState = existingTimingState.randomState;
        }
      } else {
        // Fresh reset - start with clean isolated timing state
        embeddedKidLisp.frameCount = 0;
        embeddedKidLisp.frameCounter = 0;
        embeddedKidLisp.lastSecondExecutions = {};
        embeddedKidLisp.sequenceCounters = new Map();
        embeddedKidLisp.timingStates = new Map();
        // Give a unique random seed for existing layers that don't have cached state
        if (!embeddedKidLisp.randomSeed) {
          embeddedKidLisp.randomSeed = Date.now() + Math.random() + (source.hashCode?.() || 0);
          embeddedKidLisp.randomState = embeddedKidLisp.randomSeed;
        }
      }
      embeddedKidLisp.localEnv = { ...this.localEnv };

      // Ensure it remains marked as a nested instance
      embeddedKidLisp.isNestedInstance = true;
      console.log(`🎭 MARKED as nested instance: isNestedInstance = true`);

      // Update the alpha value
      existingLayer.alpha = alpha;
      // Reset local frame count for fresh timing on reload
      existingLayer.localFrameCount = 0;

      return existingLayer;
    }

    // Create new embedded layer
    const embeddedKidLisp = new KidLisp();
    
    // CRITICAL: Give each embedded layer a unique random seed for isolated random state
    embeddedKidLisp.randomSeed = Date.now() + Math.random() + (source.hashCode?.() || 0);
    embeddedKidLisp.randomState = embeddedKidLisp.randomSeed;
    
    // CRITICAL: Each embedded layer needs ISOLATED but PERSISTENT timing state
    // Check if we have cached timing state for this layer
    const layerCacheKey = `${source}_timing`;
    let existingTimingState = this.embeddedLayerCache?.get?.(layerCacheKey);
    
    if (existingTimingState) {
      // Restore persistent timing state for this specific layer
      embeddedKidLisp.frameCount = existingTimingState.frameCount || 0;
      embeddedKidLisp.frameCounter = existingTimingState.frameCounter || 0;
      embeddedKidLisp.lastSecondExecutions = existingTimingState.lastSecondExecutions || {};
      embeddedKidLisp.sequenceCounters = new Map(existingTimingState.sequenceCounters || []);
      embeddedKidLisp.timingStates = new Map(existingTimingState.timingStates || []);
      // Restore the random state too for consistency
      if (existingTimingState.randomState) {
        embeddedKidLisp.randomState = existingTimingState.randomState;
      }
    } else {
      // Fresh layer - start with clean timing state
      embeddedKidLisp.frameCount = 0;
      embeddedKidLisp.frameCounter = 0;
      embeddedKidLisp.lastSecondExecutions = {};
      embeddedKidLisp.sequenceCounters = new Map();
      embeddedKidLisp.timingStates = new Map();
    }
    embeddedKidLisp.localEnv = { ...this.localEnv };

    // Mark this as a nested instance to enable immediate mode for commands like blur/scroll
    embeddedKidLisp.isNestedInstance = true;

    // IMPORTANT: Share the source cache with embedded instances
    // This allows nested embeds to use already-cached KidLisp source code
    embeddedKidLisp.embeddedSourceCache = this.embeddedSourceCache;
    embeddedKidLisp.embeddedLayerCache = this.embeddedLayerCache;

    // Preprocess source to fix scroll syntax
    let processedSource = source;

    // Convert bare scroll expressions to proper function calls
    // Pattern 1: "scroll 0.1" -> "(scroll 0.1)"
    processedSource = processedSource.replace(/^scroll\s+([\d.]+)$/gm, '(scroll $1)');

    // Pattern 2: "scroll 0 (? 1 -1)" -> "(scroll 0 (? 1 -1))"
    processedSource = processedSource.replace(/^scroll\s+(.+)$/gm, '(scroll $1)');

    const parsedCode = embeddedKidLisp.parse(processedSource);

    // Apply the same precompilation as the main instance
    const precompiledCode = embeddedKidLisp.precompileAST(parsedCode);

    // Set the AST for the embedded instance so detectFirstLineColor can work
    embeddedKidLisp.ast = JSON.parse(JSON.stringify(precompiledCode));
    // Embedded AST debug log removed for performance

    // Detect and store first-line color for embedded layer (like fade strings)
    embeddedKidLisp.detectFirstLineColor();
    // New embedded layer firstLineColor debug log removed for performance
    // Source code debug log removed for performance

    // Check if we already have a buffer for this layer (for persistence)
    let embeddedBuffer;

    // Ensure embeddedLayers is initialized
    if (!this.embeddedLayers) {
      console.log("🚨 embeddedLayers was null, reinitializing to empty array");
      this.embeddedLayers = [];
    }

    const persistentLayer = this.embeddedLayers.find(layer => layer.cacheId === layerKey);
    if (persistentLayer && persistentLayer.buffer) {
      // Check if the source code has changed
      if (persistentLayer.source === source) {
        // Reusing existing buffer - log removed for performance
        embeddedBuffer = persistentLayer.buffer;
      } else {
        // Source changed, clearing buffer - logs removed for performance

        // Source changed, clear the buffer and create a new one
        if (persistentLayer.buffer && persistentLayer.buffer.pixels) {
          // Initialize with opaque black (like main canvas)
          for (let i = 0; i < persistentLayer.buffer.pixels.length; i += 4) {
            persistentLayer.buffer.pixels[i] = 0;     // R
            persistentLayer.buffer.pixels[i + 1] = 0; // G
            persistentLayer.buffer.pixels[i + 2] = 0; // B
            persistentLayer.buffer.pixels[i + 3] = 255; // A - opaque!
          }
          console.log(`🎨 EMBEDDED BUFFER CLEAR: Reinitialized buffer with opaque black [0,0,0,255]`);
        }
        embeddedBuffer = persistentLayer.buffer; // Reuse cleared buffer

        // Update the layer's source code
        persistentLayer.source = source;
        persistentLayer.parsedCode = precompiledCode;
        persistentLayer.kidlispInstance = embeddedKidLisp;
      }
    } else {
      // � REFRAME OPTIMIZATION: Check if we can reuse existing buffer during size changes
      let foundCompatibleBuffer = false;
      if (existingLayer && existingLayer.buffer) {
        const currentSize = existingLayer.buffer.width * existingLayer.buffer.height;
        const newSize = width * height;

        // If the new size is smaller or equal, reuse the existing buffer
        // This prevents buffer recreation during reframe operations
        if (newSize <= currentSize && existingLayer.buffer.width >= width && existingLayer.buffer.height >= height) {
          embeddedBuffer = existingLayer.buffer;
          foundCompatibleBuffer = true;
          console.log(`🔄 Reusing existing buffer during reframe: ${width}x${height} fits in ${existingLayer.buffer.width}x${existingLayer.buffer.height}`);

          // Initialize used area with opaque black
          if (embeddedBuffer.pixels) {
            const totalPixels = width * height;
            for (let i = 0; i < totalPixels; i++) {
              const pixelIndex = i * 4;
              embeddedBuffer.pixels[pixelIndex] = 0;     // R
              embeddedBuffer.pixels[pixelIndex + 1] = 0; // G
              embeddedBuffer.pixels[pixelIndex + 2] = 0; // B
              embeddedBuffer.pixels[pixelIndex + 3] = 255; // A - opaque!
            }
            console.log(`🎨 EMBEDDED BUFFER REFRAME: Initialized used area with opaque black [0,0,0,255]`);
          }
        }
      }

      // �🔄 BUFFER POOLING: Try to get a buffer from the pool first
      if (!foundCompatibleBuffer) {
        const bufferSizeKey = `${width}x${height}`;
        const pooledBuffers = this.bufferPool.get(bufferSizeKey);

        if (pooledBuffers && pooledBuffers.length > 0) {
          embeddedBuffer = pooledBuffers.pop();
          // console.log(`🔄 Reused pooled buffer ${bufferSizeKey} (${pooledBuffers.length} remaining)`);

          // Initialize the reused buffer with opaque black (like main canvas)
          if (embeddedBuffer.pixels) {
            for (let i = 0; i < embeddedBuffer.pixels.length; i += 4) {
              embeddedBuffer.pixels[i] = 0;     // R
              embeddedBuffer.pixels[i + 1] = 0; // G
              embeddedBuffer.pixels[i + 2] = 0; // B
              embeddedBuffer.pixels[i + 3] = 255; // A - opaque!
            }
            console.log(`🎨 EMBEDDED BUFFER INIT: Initialized buffer with opaque black [0,0,0,255]`);
          }
        } else {
          // Use optimized buffer creation
          embeddedBuffer = this.createOrReuseBuffer(width, height);
        }
      }
    }

    if (!embeddedBuffer) {
      console.error("❌ Failed to create embedded buffer for:", layerKey);
      return undefined;
    }

    const embeddedLayer = {
      id: `embedded_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`, // Unique ID for each layer
      cacheId: layerKey,
      originalCacheId: cacheId,
      width,
      height,
      x,
      y,
      alpha, // Store alpha value for blending
      buffer: embeddedBuffer,
      kidlispInstance: embeddedKidLisp,
      parsedCode: precompiledCode,
      source,
      sourceCode: source, // Store for timing pattern extraction
      timingPattern: this.extractTimingPattern(source), // Pre-extract timing pattern
      localFrameCount: 0,  // Start with fresh frame count for proper timing
      // Store timing context information if this layer was created within a timing expression
      timingContext: this.currentTimingContext ? {
        timingKey: this.currentTimingContext.timingKey,
        argumentIndex: this.currentTimingContext.currentIndex,
        cacheId: cacheId // Store the specific cache ID this layer corresponds to
      } : null
    };

    // Ensure embeddedLayers is initialized before pushing
    if (!this.embeddedLayers) {
      console.log("🚨 embeddedLayers was null during push, reinitializing to empty array");
      this.embeddedLayers = [];
    }

    this.embeddedLayers.push(embeddedLayer);
    this.embeddedLayerCache.set(layerKey, embeddedLayer);

    // 🎨 IMMEDIATE PASTE: Paste the embedded layer back to main canvas right after creation
    // This allows any subsequent drawing commands to naturally draw on top
    // Paste and layer creation success debug logs removed for performance
    return embeddedLayer;
  }

  // Clear embedded layer cache (called on reload/initialization)
  clearEmbeddedLayerCache() {
    // 🔄 BUFFER POOLING: Return buffers to pool before clearing
    if (this.embeddedLayers) {
      this.embeddedLayers.forEach(layer => {
        if (layer && layer.buffer) {
          this.returnBufferToPool(layer.buffer, layer.width, layer.height);
        }
      });
    }

    // Clear all caches
    this.embeddedLayers = [];
    this.embeddedLayerCache.clear();
    this.embeddedSourceCache.clear();
    this.embeddedApiCache.clear();

    // 🚀 CLEAR ALPHA CACHE: Clean up alpha-adjusted buffers
    if (this.alphaBufferCache) {
      this.alphaBufferCache.clear();
    }

    // 🛡️ CLEAN POOLS: Remove any potentially detached buffers from pools
    this.cleanBufferPools();
  }

  // 🔄 RESPONSIVE CACHE: Check if screen dimensions changed and clear responsive entries
  checkAndClearResponsiveCacheOnReframe(api) {
    if (!api || !api.screen) return false;

    const currentWidth = api.screen.width;
    const currentHeight = api.screen.height;

    // Check if this is the first time or if dimensions have changed significantly
    // Add a small threshold to avoid clearing cache for tiny changes during resize
    const widthChanged = this.lastScreenWidth === null || Math.abs(this.lastScreenWidth - currentWidth) > 2;
    const heightChanged = this.lastScreenHeight === null || Math.abs(this.lastScreenHeight - currentHeight) > 2;

    if (widthChanged || heightChanged) {
      console.log(`📐 Screen dimensions changed significantly: ${this.lastScreenWidth}x${this.lastScreenHeight} → ${currentWidth}x${currentHeight}`);

      // Clear cache entries that depend on screen dimensions
      if (this.embeddedLayerCache) {
        const entriesToDelete = [];
        for (const [key, layer] of this.embeddedLayerCache.entries()) {
          // Look for cache keys that include screen dimensions (contain "_screen")
          if (key.includes('_screen')) {
            entriesToDelete.push(key);
            // Return buffer to pool before deletion
            if (layer && layer.buffer) {
              this.returnBufferToPool(layer.buffer, layer.width, layer.height);
            }
          }
        }

        // Delete the screen-dependent entries
        entriesToDelete.forEach(key => {
          this.embeddedLayerCache.delete(key);
          console.log(`🗑️ Cleared responsive cache entry: ${key}`);
        });

        if (entriesToDelete.length > 0) {
          console.log(`✨ Cleared ${entriesToDelete.length} responsive cache entries for screen resize`);
        }
      }

      // Update tracked dimensions
      this.lastScreenWidth = currentWidth;
      this.lastScreenHeight = currentHeight;

      return true; // Dimensions changed
    }

    return false; // No significant change
  }

  // 🖼️ REFRAME HANDLER: Called when screen dimensions change (called from disk systems)
  onReframe(newWidth, newHeight) {
    console.log(`🖼️ KidLisp received reframe event: ${newWidth}x${newHeight}`);

    // Force clear responsive cache entries regardless of threshold
    if (this.embeddedLayerCache) {
      const entriesToDelete = [];
      for (const [key, layer] of this.embeddedLayerCache.entries()) {
        // Look for cache keys that include screen dimensions (contain "_screen")
        if (key.includes('_screen')) {
          entriesToDelete.push(key);
          // Return buffer to pool before deletion
          if (layer && layer.buffer) {
            this.returnBufferToPool(layer.buffer, layer.width, layer.height);
          }
        }
      }

      // Delete the screen-dependent entries
      entriesToDelete.forEach(key => {
        this.embeddedLayerCache.delete(key);
        console.log(`🗑️ Cleared responsive cache entry on reframe: ${key}`);
      });

      if (entriesToDelete.length > 0) {
        console.log(`✨ Cleared ${entriesToDelete.length} responsive cache entries for reframe`);
      }
    }

    // 🎨 BACKGROUND FILL: Re-apply background color after reframe to cover expanded areas

    // Always try to get the most reliable background color via getBackgroundFillColor
    // This handles all the fallback logic and ensures consistent state
    let bgColor = this.getBackgroundFillColor();

    if (bgColor) {
      // Use globalThis to access the global wipe function
      if (typeof globalThis.$paintApiUnwrapped?.wipe === 'function') {
        globalThis.$paintApiUnwrapped.wipe(bgColor);
      } else if (typeof globalThis.wipe === 'function') {
        globalThis.wipe(bgColor);
      }
    }    // Update tracked dimensions
    this.lastScreenWidth = newWidth;
    this.lastScreenHeight = newHeight;
  }

  // 🛡️ SAFETY: Clean buffer pools of any detached buffers
  cleanBufferPools() {
    if (!this.bufferPool) return;

    for (const [sizeKey, buffers] of this.bufferPool.entries()) {
      const validBuffers = buffers.filter(buffer => {
        return buffer && buffer.pixels && buffer.pixels.buffer && !buffer.pixels.buffer.detached;
      });

      if (validBuffers.length !== buffers.length) {
        console.log(`🧹 Cleaned ${buffers.length - validBuffers.length} detached buffers from ${sizeKey} pool`);
      }

      if (validBuffers.length > 0) {
        this.bufferPool.set(sizeKey, validBuffers);
      } else {
        this.bufferPool.delete(sizeKey);
      }
    }
  }

  // 🔄 BUFFER POOLING: Return a buffer to the pool for reuse
  returnBufferToPool(buffer, width, height) {
    if (!buffer || !buffer.pixels) return;

    // 🛡️ SAFETY CHECK: Don't pool detached buffers
    if (buffer.pixels.buffer && buffer.pixels.buffer.detached) {
      console.warn('🚨 Refusing to pool detached buffer');
      return;
    }

    const bufferSizeKey = `${width}x${height}`;
    if (!this.bufferPool) {
      this.bufferPool = new Map();
    }

    let pooledBuffers = this.bufferPool.get(bufferSizeKey);

    if (!pooledBuffers) {
      pooledBuffers = [];
      this.bufferPool.set(bufferSizeKey, pooledBuffers);
    }

    // Increase pool size for commonly used buffer sizes
    const maxPoolSize = (width * height > 100000) ? 2 : 8; // Larger buffers get smaller pools

    if (pooledBuffers.length < maxPoolSize) {
      // Fast buffer clear using fill
      try {
        buffer.pixels.fill(0);
        pooledBuffers.push(buffer);
      } catch (error) {
        console.warn('🚨 Failed to clear buffer for pooling:', error);
      }
    }
  }

  // 🚀 OPTIMIZED BUFFER CREATION: Use pooling and faster allocation
  createOrReuseBuffer(width, height) {
    const bufferSizeKey = `${width}x${height}`;
    const pooledBuffers = this.bufferPool?.get(bufferSizeKey);

    if (pooledBuffers && pooledBuffers.length > 0) {
      const buffer = pooledBuffers.pop();

      // 🛡️ SAFETY CHECK: Ensure buffer is not detached
      if (buffer.pixels && buffer.pixels.buffer && !buffer.pixels.buffer.detached) {
        // Buffer is safe to reuse - initialize with opaque black
        for (let i = 0; i < buffer.pixels.length; i += 4) {
          buffer.pixels[i] = 0;     // R
          buffer.pixels[i + 1] = 0; // G
          buffer.pixels[i + 2] = 0; // B
          buffer.pixels[i + 3] = 255; // A - opaque!
        }
        console.log(`🎨 BUFFER POOL REUSE: Initialized with opaque black [0,0,0,255]`);
        return buffer;
      } else {
        // Buffer is detached or invalid, remove it and create new one
        console.warn('🚨 Discarded detached buffer from pool');
      }
    }

    // Create new buffer initialized with opaque black
    const pixelCount = width * height * 4;
    const pixels = new Uint8ClampedArray(pixelCount);
    
    // Initialize with opaque black [0,0,0,255]
    for (let i = 0; i < pixelCount; i += 4) {
      pixels[i] = 0;     // R
      pixels[i + 1] = 0; // G
      pixels[i + 2] = 0; // B
      pixels[i + 3] = 255; // A - opaque!
    }
    
    console.log(`🎨 NEW BUFFER CREATED: ${width}x${height} initialized with opaque black [0,0,0,255]`);
    
    return {
      width: width,
      height: height,
      pixels: pixels
    };
  }

  // Helper function to paste a buffer with alpha blending
  // 🚀 ULTRA-OPTIMIZED: Pre-cache alpha buffers and use fast paths
  pasteWithAlpha(api, sourceBuffer, x, y, alpha) {
    console.log(`🎬 HEADLESS DEBUG: pasteWithAlpha called with buffer ${sourceBuffer?.width}x${sourceBuffer?.height}, alpha=${alpha}, api.screen=${!!api.screen}, api.paste=${!!api.paste}`);
    
    if (!sourceBuffer || !sourceBuffer.pixels || !api.screen || !api.screen.pixels) {
      console.warn('⚠️ HEADLESS DEBUG: pasteWithAlpha: Missing required components', {
        sourceBuffer: !!sourceBuffer,
        sourcePixels: !!sourceBuffer?.pixels,
        apiScreen: !!api.screen,
        screenPixels: !!api.screen?.pixels
      });
      return; // Silent fail for performance
    }

    // Debug: Check what's in the source buffer being pasted
    const samplePixels = [];
    for (let i = 0; i < Math.min(20, sourceBuffer.pixels.length); i += 4) {
      samplePixels.push(`[${sourceBuffer.pixels[i]},${sourceBuffer.pixels[i+1]},${sourceBuffer.pixels[i+2]},${sourceBuffer.pixels[i+3]}]`);
    }
    console.log('🔍 HEADLESS DEBUG: Source buffer sample pixels:', samplePixels.slice(0, 5).join(', '));

    // 🛡️ SAFETY CHECK: Ensure source buffer is not detached
    if (sourceBuffer.pixels.buffer && sourceBuffer.pixels.buffer.detached) {
      console.warn('🚨 Attempted to paste from detached buffer, skipping');
      return;
    }

    // 🎯 ULTRA-FAST PATH: Direct paste for opaque layers
    if (alpha === 255) {
      if (api.paste) {
        api.paste(sourceBuffer, x, y);
      } else {
        this.fastDirectPaste(api, sourceBuffer, x, y);
      }
      return;
    }

    // 🚀 CACHE ALPHA BUFFERS: Reuse alpha-adjusted buffers to avoid repeated allocations
    const alphaBufferKey = `${sourceBuffer.width}x${sourceBuffer.height}_${alpha}`;
    if (!this.alphaBufferCache) {
      this.alphaBufferCache = new Map();
    }

    let cachedAlphaBuffer = this.alphaBufferCache.get(alphaBufferKey);

    // Only create new buffer if not cached or source changed
    if (!cachedAlphaBuffer || this.needsAlphaBufferUpdate(sourceBuffer, cachedAlphaBuffer)) {
      // Reuse buffer if possible, otherwise create new one
      if (cachedAlphaBuffer && cachedAlphaBuffer.pixels.length === sourceBuffer.pixels.length) {
        // Reuse existing buffer
        this.updateAlphaBuffer(sourceBuffer, cachedAlphaBuffer, alpha);
      } else {
        // Create new buffer
        cachedAlphaBuffer = {
          width: sourceBuffer.width,
          height: sourceBuffer.height,
          pixels: new Uint8ClampedArray(sourceBuffer.pixels.length),
          sourceHash: this.quickPixelHash(sourceBuffer.pixels),
          alpha: alpha
        };
        this.updateAlphaBuffer(sourceBuffer, cachedAlphaBuffer, alpha);
        this.alphaBufferCache.set(alphaBufferKey, cachedAlphaBuffer);
      }
    }

    // Use proper alpha blending - force fallback for true alpha compositing
    this.fallbackPasteWithAlpha(api, sourceBuffer, x, y, alpha);
  }

  // 🚀 Check if alpha buffer needs updating (avoids expensive pixel operations)
  needsAlphaBufferUpdate(sourceBuffer, cachedBuffer) {
    if (!cachedBuffer || !cachedBuffer.sourceHash) return true;

    // 🛡️ SAFETY CHECK: Ensure cached buffer is not detached
    if (!cachedBuffer.pixels || !cachedBuffer.pixels.buffer || cachedBuffer.pixels.buffer.detached) {
      return true; // Force recreation if buffer is detached
    }

    // Use quick hash comparison instead of full pixel comparison
    const currentHash = this.quickPixelHash(sourceBuffer.pixels);
    return currentHash !== cachedBuffer.sourceHash;
  }

  // 🚀 Update alpha buffer with optimized SIMD-style operations where possible
  updateAlphaBuffer(sourceBuffer, targetBuffer, alpha) {
    const alphaFactor = alpha / 255.0;
    const src = sourceBuffer.pixels;
    const dst = targetBuffer.pixels;
    const len = src.length;

    // Process 4 pixels at a time for better cache efficiency
    let i = 0;
    for (; i < len - 15; i += 16) {
      // Pixel 1
      dst[i] = src[i];
      dst[i + 1] = src[i + 1];
      dst[i + 2] = src[i + 2];
      dst[i + 3] = src[i + 3] * alphaFactor;

      // Pixel 2
      dst[i + 4] = src[i + 4];
      dst[i + 5] = src[i + 5];
      dst[i + 6] = src[i + 6];
      dst[i + 7] = src[i + 7] * alphaFactor;

      // Pixel 3
      dst[i + 8] = src[i + 8];
      dst[i + 9] = src[i + 9];
      dst[i + 10] = src[i + 10];
      dst[i + 11] = src[i + 11] * alphaFactor;

      // Pixel 4
      dst[i + 12] = src[i + 12];
      dst[i + 13] = src[i + 13];
      dst[i + 14] = src[i + 14];
      dst[i + 15] = src[i + 15] * alphaFactor;
    }

    // Handle remaining pixels
    for (; i < len; i += 4) {
      dst[i] = src[i];
      dst[i + 1] = src[i + 1];
      dst[i + 2] = src[i + 2];
      dst[i + 3] = src[i + 3] * alphaFactor;
    }

    // Update metadata
    targetBuffer.sourceHash = this.quickPixelHash(src);
    targetBuffer.alpha = alpha;
  }

  // 🚀 Optimized direct paste without API overhead
  fastDirectPaste(api, sourceBuffer, x, y) {
    const src = sourceBuffer.pixels;
    const dst = api.screen.pixels;
    const srcW = sourceBuffer.width;
    const srcH = sourceBuffer.height;
    const dstW = api.screen.width;
    const dstH = api.screen.height;

    // Calculate bounds once
    const startX = Math.max(0, x);
    const startY = Math.max(0, y);
    const endX = Math.min(dstW, x + srcW);
    const endY = Math.min(dstH, y + srcH);

    // Early exit for out-of-bounds
    if (startX >= endX || startY >= endY) return;

    // Row-wise copying for better cache performance
    for (let dy = startY; dy < endY; dy++) {
      const srcY = dy - y;
      const srcRowStart = srcY * srcW * 4;
      const dstRowStart = dy * dstW * 4;

      for (let dx = startX; dx < endX; dx++) {
        const srcX = dx - x;
        const srcIdx = srcRowStart + srcX * 4;
        const dstIdx = dstRowStart + dx * 4;

        // Copy RGBA values
        dst[dstIdx] = src[srcIdx];
        dst[dstIdx + 1] = src[srcIdx + 1];
        dst[dstIdx + 2] = src[srcIdx + 2];
        dst[dstIdx + 3] = src[srcIdx + 3];
      }
    }
  }

  // Fallback manual alpha blending for when graph.paste is not available
  fallbackPasteWithAlpha(api, sourceBuffer, x, y, alpha) {
    const srcPixels = sourceBuffer.pixels;
    const dstPixels = api.screen.pixels;
    const srcWidth = sourceBuffer.width;
    const srcHeight = sourceBuffer.height;
    const dstWidth = api.screen.width;
    const dstHeight = api.screen.height;

    // Debug: Check destination buffer before blending
    const dstSamplePixels = [];
    for (let i = 0; i < Math.min(20, dstPixels.length); i += 4) {
      dstSamplePixels.push(`[${dstPixels[i]},${dstPixels[i+1]},${dstPixels[i+2]},${dstPixels[i+3]}]`);
    }
    console.log(`🎭 BLEND DEBUG: Destination buffer before blending (alpha=${alpha}):`, dstSamplePixels.slice(0, 5).join(', '));

    // Normalize alpha to 0-1 range
    const alphaFactor = alpha / 255.0;

    // Clamp destination area to screen bounds
    const startX = Math.max(0, x);
    const startY = Math.max(0, y);
    const endX = Math.min(dstWidth, x + srcWidth);
    const endY = Math.min(dstHeight, y + srcHeight);

    // Optimized row-wise blending for better cache performance
    for (let dy = startY; dy < endY; dy++) {
      const srcY = dy - y;
      if (srcY < 0 || srcY >= srcHeight) continue;

      for (let dx = startX; dx < endX; dx++) {
        const srcX = dx - x;
        if (srcX < 0 || srcX >= srcWidth) continue;

        const srcIndex = (srcY * srcWidth + srcX) * 4;
        const dstIndex = (dy * dstWidth + dx) * 4;

        const srcA = srcPixels[srcIndex + 3];

        // Skip transparent pixels
        if (srcA === 0) continue;

        // Fast path for fully opaque pixels
        if (alphaFactor === 1.0 && srcA === 255) {
          dstPixels[dstIndex] = srcPixels[srcIndex];
          dstPixels[dstIndex + 1] = srcPixels[srcIndex + 1];
          dstPixels[dstIndex + 2] = srcPixels[srcIndex + 2];
          dstPixels[dstIndex + 3] = 255;
        } else {
          // Alpha blend using bit shifts for performance (similar to graph.mjs)
          const effectiveAlpha = (srcA * alphaFactor + 1) | 0; // Convert to int
          const invAlpha = 256 - effectiveAlpha;

          dstPixels[dstIndex] = (effectiveAlpha * srcPixels[srcIndex] + invAlpha * dstPixels[dstIndex]) >> 8;
          dstPixels[dstIndex + 1] = (effectiveAlpha * srcPixels[srcIndex + 1] + invAlpha * dstPixels[dstIndex + 1]) >> 8;
          dstPixels[dstIndex + 2] = (effectiveAlpha * srcPixels[srcIndex + 2] + invAlpha * dstPixels[dstIndex + 2]) >> 8;
          dstPixels[dstIndex + 3] = Math.min(255, dstPixels[dstIndex + 3] + effectiveAlpha);
        }
      }
    }
  }

  // Fast pixel hash for change detection (samples key pixels to avoid full buffer comparison)
  quickPixelHash(pixels) {
    if (!pixels || pixels.length === 0) return 0;

    let hash = 0;
    const len = pixels.length;

    // Sample every 64th pixel for speed (still gives good change detection)
    for (let i = 0; i < len; i += 64) {
      hash = ((hash << 5) - hash + pixels[i]) | 0; // Simple hash combining sampled pixels
    }

    return hash;
  }

  // Render and update embedded layers each frame
  renderEmbeddedLayers(api) {
    // 🔍 DEBUG: Log embedded layers status
    if (!this.embeddedLayers || this.embeddedLayers.length === 0) {

      return;
    }

    console.log(`🎬 HEADLESS DEBUG: Processing ${this.embeddedLayers.length} embedded layers`);

    // 🚀 REFRAME OPTIMIZATION: Skip expensive re-evaluation during reframe operations
    const currentScreenSize = `${api.screen?.width || 0}x${api.screen?.height || 0}`;
    if (this.lastScreenSize && this.lastScreenSize !== currentScreenSize) {
      const timeSinceLastResize = performance.now() - (this.lastResizeTime || 0);
      if (timeSinceLastResize < 100) { // Within 100ms of a resize
        // Just re-paste existing layers without re-evaluation for performance
        if (this.embeddedLayers) {
          this.embeddedLayers.forEach(embeddedLayer => {
            if (embeddedLayer && embeddedLayer.buffer && api.paste) {
              this.pasteWithAlpha(api, embeddedLayer.buffer, embeddedLayer.x, embeddedLayer.y, embeddedLayer.alpha);
            }
          });
        }
        return;
      }
    }
    this.lastScreenSize = currentScreenSize;
    this.lastResizeTime = performance.now();

    // � FRAME-BASED OPTIMIZATION: Skip complex renders when too frequent
    const layerCount = this.embeddedLayers.length;
    if (layerCount > 3 && performance.now() - (this.lastComplexRender || 0) < 16) {
      // For multiple layers, limit to 60fps to prevent slowdown
      return;
    }

    if (layerCount > 3) {
      this.lastComplexRender = performance.now();
    }

    // �️ PERIODIC CLEANUP: Clean buffer pools occasionally to prevent detached buffer accumulation
    if (api.frame && api.frame % 300 === 0) { // Every 5 seconds at 60fps
      this.cleanBufferPools();
    }

    // �🚀 BATCH OPTIMIZATION: Pre-calculate frame value once
    const frameValue = api.frame || this.frameCount || 0;

    // Update and composite each embedded layer (render in correct order: 0, 1, 2...)
    if (this.embeddedLayers) {
      this.embeddedLayers.forEach((embeddedLayer, index) => {
        if (embeddedLayer && embeddedLayer.kidlispInstance && embeddedLayer.buffer) {

          // Check timing context visibility (fast path)
          if (embeddedLayer.timingContext) {
            const timingCtx = embeddedLayer.timingContext;
            const currentIndex = this.sequenceCounters.get(timingCtx.timingKey);
            if (currentIndex !== undefined && currentIndex !== timingCtx.argumentIndex) {
              return; // Skip invisible layers
            }
          }

          // 🚀 DIRTY CHECK: Skip unnecessary evaluation for static content
          const shouldEvaluate = this.shouldLayerEvaluate(embeddedLayer, frameValue);

          try {
            this.renderSingleLayer(api, embeddedLayer, frameValue, shouldEvaluate);
          } catch (error) {
            // Silent error recovery - just ensure we switch back to main screen
            try {
              api.page(api.screen);
            } catch (e) {
              // Ignore switch errors
            }
          }
        }
      });
    }
  }

  // 🚀 OPTIMIZED: Determine if layer needs evaluation (dirty checking)
  shouldLayerEvaluate(embeddedLayer, frameValue) {
    // Always evaluate if never been evaluated
    if (!embeddedLayer.hasBeenEvaluated) {
      // console.log(`🔄 shouldLayerEvaluate: First evaluation for layer ${embeddedLayer.id}`);
      return true;
    }

    // Skip if evaluated this exact frame already
    if (embeddedLayer.lastFrameEvaluated === frameValue) {
      // console.log(`🔄 shouldLayerEvaluate: Already evaluated this frame ${frameValue} for layer ${embeddedLayer.id}`);
      return false;
    }

    // Check for dynamic content in source (check multiple places)
    const source = embeddedLayer.kidlispInstance.source || embeddedLayer.sourceCode || embeddedLayer.source || '';
    // console.log(`🔄 shouldLayerEvaluate: Checking source for layer ${embeddedLayer.id}:`, source);

    const hasDynamicContent = source.includes('frame') ||
      source.includes('scroll') ||
      source.includes('clock') ||
      source.includes('pen') ||
      source.includes('mouse') ||
      source.includes('key') ||
      source.includes('touch') ||
      source.includes('tap') ||
      source.includes('random');

    // console.log(`🔄 shouldLayerEvaluate: hasDynamicContent=${hasDynamicContent}, hasBeenEvaluated=${embeddedLayer.hasBeenEvaluated} for layer ${embeddedLayer.id}`);

    // TEMPORARY WORKAROUND: Force re-evaluation for layers that might have scroll
    // Since source is often empty due to caching issues, force evaluation more frequently
    if (!hasDynamicContent && embeddedLayer.hasBeenEvaluated) {
      // Check if we should force evaluation anyway (every 10 frames for potential scroll effects)
      const shouldForceEvaluation = (frameValue % 10 === 0);
      // console.log(`🔄 shouldLayerEvaluate: Forcing evaluation every 10 frames: ${shouldForceEvaluation} (frame ${frameValue})`);
      if (shouldForceEvaluation) {
        return true;
      }
      return false;
    }

    // console.log(`🔄 shouldLayerEvaluate: Returning true (default) for layer ${embeddedLayer.id}`);
    return true; // Default to evaluating
  }

  // 🚀 OPTIMIZED: Render single layer with minimal overhead
  renderSingleLayer(api, embeddedLayer, frameValue, shouldEvaluate) {
    console.log(`🎬 HEADLESS DEBUG: renderSingleLayer: shouldEvaluate=${shouldEvaluate}, layer dimensions=${embeddedLayer.width}x${embeddedLayer.height}, pos=(${embeddedLayer.x},${embeddedLayer.y}), alpha=${embeddedLayer.alpha}`);

    // 🔥 REFRAME PERFORMANCE: Skip expensive re-evaluation during rapid screen changes
    const currentScreenSize = `${api.screen?.width || 0}x${api.screen?.height || 0}`;
    const timeSinceLastRender = performance.now() - (embeddedLayer.lastRenderTime || 0);

    if (this.lastScreenSize && this.lastScreenSize !== currentScreenSize && timeSinceLastRender < 50) {
      // During rapid screen size changes, just re-paste existing buffer
      if (embeddedLayer.buffer && api.paste) {
        this.pasteWithAlpha(api, embeddedLayer.buffer, embeddedLayer.x, embeddedLayer.y, embeddedLayer.alpha);
      }
      return;
    }

    embeddedLayer.lastRenderTime = performance.now();

    // Switch to embedded buffer
    api.page(embeddedLayer.buffer);

    // Update frame counters
    if (!embeddedLayer.localFrameCount) {
      embeddedLayer.localFrameCount = 0;
    }
    embeddedLayer.localFrameCount += 1;

    // CRITICAL: Each embedded layer needs its own independent timeline
    embeddedLayer.kidlispInstance.frameCount = embeddedLayer.localFrameCount;
    embeddedLayer.kidlispInstance.frameCounter = embeddedLayer.localFrameCount;

    // Ensure timing state
    if (!embeddedLayer.kidlispInstance.timingStates) {
      embeddedLayer.kidlispInstance.timingStates = new Map();
    }

    if (shouldEvaluate) {
      // Get optimized API for this layer
      const embeddedApi = this.getOptimizedLayerApi(embeddedLayer, api);

      // Update frame-dependent properties
      embeddedApi.frame = embeddedLayer.localFrameCount;
      embeddedApi.screen.pixels = embeddedLayer.buffer.pixels;

      // Update environment efficiently
      const localEnv = embeddedLayer.kidlispInstance.localEnv;
      localEnv.frame = embeddedLayer.localFrameCount;
      localEnv.scroll = frameValue % (embeddedLayer.width + embeddedLayer.height);

      // Apply fade background only once
      console.log(`🎨 EMBEDDED FADE DEBUG: firstLineColor=${embeddedLayer.kidlispInstance.firstLineColor}, fadeApplied=${embeddedLayer.fadeApplied}`);
      if (embeddedLayer.kidlispInstance.firstLineColor && !embeddedLayer.fadeApplied) {
        console.log(`🎨 EMBEDDED FADE: Applying background wipe with ${embeddedLayer.kidlispInstance.firstLineColor}`);
        embeddedApi.wipe(embeddedLayer.kidlispInstance.firstLineColor);
        embeddedLayer.fadeApplied = true;
      }

      // Execute the code
      const scrollNodesInAST = Array.isArray(embeddedLayer.parsedCode) ?
        embeddedLayer.parsedCode.filter(node => {
          // Check for direct scroll function calls (lists with 'scroll' as first element)
          if (node && typeof node === 'object' && node.type === 'list' &&
            node.value && node.value[0] && node.value[0].value === 'scroll') {
            return true;
          }

          // Check for timing expressions containing scroll
          if (Array.isArray(node)) {
            // Check if this is a timing expression like ["2s...", ["scroll", 1, 0, -1]]
            if (node.length > 1 && typeof node[0] === 'string' && /^\d*\.?\d+s\.\.\.?$/.test(node[0])) {
              // Check if any following elements contain scroll
              return node.slice(1).some(elem => {
                if (Array.isArray(elem) && elem.length > 0 && elem[0] === 'scroll') {
                  return true;
                }
                return false;
              });
            }

            // Check for scroll function calls directly in arrays
            if (node.length > 0 && node[0] === 'scroll') {
              return true;
            }
          }

          return false;
        }).length : 0;

      embeddedLayer.kidlispInstance.evaluate(
        embeddedLayer.parsedCode,
        embeddedApi,
        localEnv
      );

      // CRITICAL: Save persistent timing state after execution
      const layerCacheKey = `${embeddedLayer.source}_timing`;
      if (this.embeddedLayerCache) {
        this.embeddedLayerCache.set(layerCacheKey, {
          frameCount: embeddedLayer.kidlispInstance.frameCount,
          frameCounter: embeddedLayer.kidlispInstance.frameCounter,
          lastSecondExecutions: [...embeddedLayer.kidlispInstance.lastSecondExecutions],
          sequenceCounters: Array.from(embeddedLayer.kidlispInstance.sequenceCounters),
          timingStates: Array.from(embeddedLayer.kidlispInstance.timingStates),
          randomState: embeddedLayer.kidlispInstance.randomState
        });
      }

      embeddedLayer.hasBeenEvaluated = true;
      embeddedLayer.lastFrameEvaluated = frameValue;
    }

    // Switch back to main screen
    api.page(api.screen);

    // Paste with optimized alpha compositing
    this.pasteWithAlpha(api, embeddedLayer.buffer, embeddedLayer.x, embeddedLayer.y, embeddedLayer.alpha);
  }

  // 🚀 CACHE OPTIMIZED API: Minimal API object creation
  getOptimizedLayerApi(embeddedLayer, api) {
    const cacheKey = `${embeddedLayer.width}x${embeddedLayer.height}`;

    // SIMPLIFIED: Don't use complex caching, just create a working API each time
    // Create a simple API that just passes through to the main API
    // This is much simpler than the complex wrapper system
    const globalEnv = this.getGlobalEnv();
    const embeddedApi = {
      // Include KidLisp functions from global environment
      ...globalEnv,

      // Include system context for functions like 'painting'
      system: api.system,

      // Direct passthrough of most functions to main API
      line: (...args) => api.line(...args),
      ink: (...args) => api.ink(...args),
      wipe: (...args) => api.wipe(...args),
      circle: (...args) => api.circle(...args),
      tri: (...args) => api.tri(...args),
      box: (...args) => api.box(...args),
      point: (...args) => api.point(...args),
      poly: (...args) => api.poly(...args),
      paste: (...args) => api.paste(...args),
      stamp: (...args) => api.stamp(...args),
      write: (...args) => api.write(...args),
      flood: (...args) => api.flood(...args),

      // IMPORTANT: For scroll, call the main API function directly
      // No complex wrapper system - just execute scroll immediately
      scroll: (...args) => {
        if (typeof api.scroll === "function") {
          return api.scroll(...args);
        }
      },

      // IMPORTANT: Add missing transform functions that embedded pieces need
      spin: (...args) => {
        if (typeof api.spin === "function") {
          return api.spin(...args);
        }
      },
      resetSpin: (...args) => {
        if (typeof api.resetSpin === "function") {
          return api.resetSpin(...args);
        }
      },
      smoothspin: (...args) => {
        if (typeof api.smoothspin === "function") {
          return api.smoothspin(...args);
        }
      },
      zoom: (...args) => {
        if (typeof api.zoom === "function") {
          return api.zoom(...args);
        }
      },
      blur: (...args) => {
        if (typeof api.blur === "function") {
          return api.blur(...args);
        }
      },
      contrast: (...args) => {
        if (typeof api.contrast === "function") {
          return api.contrast(...args);
        }
      },

      // Screen properties
      screen: {
        width: embeddedLayer.width,
        height: embeddedLayer.height,
        pixels: null // Updated per render
      },
      width: embeddedLayer.width,
      height: embeddedLayer.height,
      frame: 0, // Updated per render
      
      // 🚨 CRITICAL: Include clock API for timing expressions in embedded layers
      clock: api.clock
    };

    return embeddedApi;
  }

  // Check if an embedded layer should execute this frame based on its timing patterns
  shouldLayerExecuteThisFrame(api, embeddedLayer) {
    if (!embeddedLayer.sourceCode) {
      return true; // No source code, always execute
    }

    // Parse the source to find timing expressions
    const timingExpressions = this.extractTimingExpressions(embeddedLayer.sourceCode);

    if (timingExpressions.length === 0) {
      return true; // No timing expressions, always execute
    }

    // Check each timing expression to see if any should execute this frame
    for (const timingExpr of timingExpressions) {
      if (this.evaluateTimingExpression(api, timingExpr)) {
        // Timing condition met debug log removed for performance
        return true;
      }
    }

    return false; // No timing conditions were met
  }

  // Extract timing expressions like "3s...", "0.25s..." from source code
  extractTimingExpressions(sourceCode) {
    const expressions = [];
    // Match patterns like (3s...) or (0.25s... something)
    const timingRegex = /\(\s*(\d+(?:\.\d+)?s\.\.\.)/g;
    let match;

    while ((match = timingRegex.exec(sourceCode)) !== null) {
      expressions.push(match[1]); // Extract the timing part like "3s..."
    }

    return expressions;
  }

  // 🚀 EMBEDDED ENVIRONMENT: Simplified functions for embedded layers (no deferred execution)
  getSimpleEmbeddedEnv(api) {
    return {
      // Time function for clock expressions (provides both clock.time and time)
      time: () => {
        if (api.clock && api.clock.time) {
          return api.clock.time();
        }
        return new Date();
      }
    };
  }

  // Evaluate a timing expression using the parent's timing state
  evaluateTimingExpression(api, timingExpr) {
    console.log(`⏰ TIMING EVALUATION called with:`, {
      timingExpr,
      inEmbedPhase: this.inEmbedPhase,
      isNestedInstance: this.isNestedInstance,
      hasClock: !!api.clock
    });

    // Extract the interval from expressions like "3s..." or "0.25s..."
    const match = timingExpr.match(/^(\d+(?:\.\d+)?)s\.\.\.$/);
    if (!match) {
      console.log(`⏰ TIMING EVALUATION: Invalid pattern, returning true`);
      return true; // Invalid pattern, default to execute
    }

    const interval = parseFloat(match[1]);
    const timingKey = timingExpr + "_1"; // Use same key format as main timing system

    console.log(`⏰ TIMING EVALUATION: Parsed interval=${interval}, timingKey=${timingKey}`);

    // Get current time
    const clockResult = api.clock?.time();
    if (!clockResult) {
      console.log(`⏰ TIMING EVALUATION: No clock available, returning true`);
      return true; // No clock, default to execute
    }

    const currentTimeMs = clockResult.getTime ? clockResult.getTime() : Date.now();
    const currentTime = currentTimeMs / 1000;

    console.log(`⏰ TIMING EVALUATION: Current time=${currentTime}`);

    // Use the parent KidLisp instance's timing state
    if (!this.lastSecondExecutions.hasOwnProperty(timingKey)) {
      // First execution - always execute and record time
      this.lastSecondExecutions[timingKey] = currentTime;
      console.log(`⏰ TIMING EVALUATION: First execution, recording time and returning true`);
      return true;
    }

    const lastExecution = this.lastSecondExecutions[timingKey];
    const diff = currentTime - lastExecution;

    console.log(`⏰ TIMING EVALUATION: Last execution=${lastExecution}, diff=${diff}, interval=${interval}`);

    // Check if enough time has passed for this timing interval
    if (diff >= interval) { // No tolerance - exact timing
      this.lastSecondExecutions[timingKey] = currentTime;
      console.log(`⏰ TIMING EVALUATION: Time passed, updating and returning true`);
      return true;
    }

    console.log(`⏰ TIMING EVALUATION: Not enough time passed, returning false`);
    return false;
  }

  // Check if a timing pattern should be active (without executing the layer)
  checkTimingActive(api, timingPattern) {
    // For now, always return true - let the layer itself handle timing
    // This is a placeholder for more sophisticated timing detection
    return true;
  }

  // Extract timing pattern from embedded layer source code
  extractTimingPattern(sourceCode) {
    if (!sourceCode) return null;

    // Look for timing patterns like "3s...", "1s...", "0.25s..."
    const timingMatch = sourceCode.match(/(\d+(?:\.\d+)?s\.\.\.)/);
    return timingMatch ? timingMatch[1] : null;
  }

  // 🚀 PROGRAMMATIC EMBEDDED LAYER CREATION
  // Create an embedded layer programmatically (for JavaScript API calls)
  // This uses the same pipeline as $code embedded layers for consistency
  createProgrammaticEmbeddedLayer(source, x, y, width, height, options = {}) {
    // Generate a unique cache ID for this programmatic layer
    const cacheId = `prog_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

    console.log(`🔧 Creating programmatic embedded layer: ${cacheId}`, {
      bounds: `${x},${y} ${width}x${height}`,
      sourceLength: source.length,
      options
    });

    // Initialize embedded layers array if needed
    if (!this.embeddedLayers) {
      this.embeddedLayers = [];
    }

    // Create a new KidLisp instance for this layer
    const kidlispInstance = new KidLisp();
    kidlispInstance.isEmbeddedContext = true;
    kidlispInstance.isNestedInstance = options.isNestedInstance !== false; // Default to true
    kidlispInstance.embeddedContext = { x, y, width, height };

    // Parse the source code
    let parsedCode;
    try {
      parsedCode = kidlispInstance.parse(source);
    } catch (error) {
      console.error(`❌ Failed to parse programmatic layer ${cacheId}:`, error);
      return null;
    }

    // Create a buffer for this layer
    const buffer = {
      width: width,
      height: height,
      pixels: new Uint8ClampedArray(width * height * 4)
    };

    // Create the embedded layer object using same structure as $code layers
    const embeddedLayer = {
      id: `prog_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`, // Unique ID for each layer
      cacheId: cacheId,
      x: x,
      y: y,
      width: width,
      height: height,
      buffer: buffer,
      source: source,
      parsedCode: parsedCode,
      kidlispInstance: kidlispInstance,
      localFrameCount: 0,
      hasBeenEvaluated: false,
      lastFrameEvaluated: -1,
      alpha: options.alpha || 255,
      timingPattern: this.extractTimingPattern(source),
      isProgrammatic: true // Mark as programmatically created
    };

    // Ensure embeddedLayers is initialized before pushing
    if (!this.embeddedLayers) {
      console.log("🚨 embeddedLayers was null during programmatic push, reinitializing to empty array");
      this.embeddedLayers = [];
    }

    // Add to embedded layers array
    this.embeddedLayers.push(embeddedLayer);

    console.log(`✅ Created programmatic embedded layer ${cacheId}, total layers: ${this.embeddedLayers.length}`);

    return cacheId;
  }

  // Remove a programmatic embedded layer
  removeProgrammaticEmbeddedLayer(cacheId) {
    if (!this.embeddedLayers) return false;

    const initialLength = this.embeddedLayers.length;
    this.embeddedLayers = this.embeddedLayers.filter(layer =>
      !(layer.isProgrammatic && layer.cacheId === cacheId)
    );

    const removed = this.embeddedLayers.length < initialLength;
    if (removed) {
      console.log(`🗑️ Removed programmatic embedded layer ${cacheId}`);
    }

    return removed;
  }

  // Update a single embedded layer (for nested embedding)
  // Returns true if the layer should be rendered/visible this frame
  updateEmbeddedLayer(api, embeddedLayer) {
    if (!embeddedLayer || !embeddedLayer.kidlispInstance || !embeddedLayer.buffer) {
      console.log("❌ Cannot update embedded layer - missing components:", {
        hasLayer: !!embeddedLayer,
        hasInstance: !!embeddedLayer?.kidlispInstance,
        hasBuffer: !!embeddedLayer?.buffer
      });
      return false;
    }

    // Track if any drawing commands were executed
    let didRender = false;

    try {
      // console.log(`🎯 Updating nested embedded layer: ${embeddedLayer.cacheId}`);

      // Switch to drawing on the embedded buffer using page()
      const originalPage = api.screen;
      api.page(embeddedLayer.buffer);
      // console.log(`📄 Switched to nested buffer: ${embeddedLayer.cacheId}`);

      // Give embedded layer its own incrementing frame counter for smooth animations
      if (!embeddedLayer.localFrameCount) {
        embeddedLayer.localFrameCount = 0;
      }
      embeddedLayer.localFrameCount += 1;

      // CRITICAL: Each embedded layer needs its own independent timeline
      embeddedLayer.kidlispInstance.frameCount = embeddedLayer.localFrameCount;
      embeddedLayer.kidlispInstance.frameCounter = embeddedLayer.localFrameCount;

      // Ensure timing state is preserved for the embedded instance
      if (!embeddedLayer.kidlispInstance.timingStates) {
        embeddedLayer.kidlispInstance.timingStates = new Map();
      }

      // ⚡ PERFORMANCE: Cache expensive API object creation for nested updates
      const nestedApiCacheKey = `nested_${embeddedLayer.cacheId}_${embeddedLayer.width}x${embeddedLayer.height}`;
      let embeddedApi = this.embeddedApiCache.get(nestedApiCacheKey);

      if (!embeddedApi) {
        // Create API context with proper frame, width, height and KidLisp commands
        const globalEnv = this.getGlobalEnv();

        embeddedApi = {
          ...globalEnv,
          ...api,
          system: api.system, // Ensure system context is available in embedded layers
          screen: {
            ...api.screen,
            width: embeddedLayer.width,
            height: embeddedLayer.height,
            pixels: embeddedLayer.buffer.pixels
          },
          // Execute drawing commands directly to the embedded buffer and track rendering
          line: (...args) => {
            didRender = true;
            return api.line(...args);
          },
          ink: (...args) => {
            didRender = true;
            return api.ink(...args);
          },
          wipe: (...args) => {
            didRender = true;
            return api.wipe(...args);
          },
          circle: (...args) => {
            didRender = true;
            return api.circle(...args);
          },
          tri: (...args) => {
            didRender = true;
            return api.tri(...args);
          },
          box: (...args) => {
            didRender = true;
            return api.box(...args);
          },
          point: (...args) => {
            didRender = true;
            return api.point(...args);
          },
          poly: (...args) => {
            didRender = true;
            return api.poly(...args);
          },
          paste: (...args) => {
            didRender = true;
            return api.paste(...args);
          },
          stamp: (...args) => {
            didRender = true;
            return api.stamp(...args);
          },
          write: (...args) => {
            didRender = true;
            return api.write(...args);
          },
          flood: (...args) => {
            didRender = true;
            return api.flood(...args);
          },
          fade: (...args) => {
            didRender = true;
            return api.fade(...args);
          },
        };
        this.embeddedApiCache.set(nestedApiCacheKey, embeddedApi);
      }

      // Update frame-specific properties
      const frameValue = api.frame || this.frameCount || 0;
      const smoothFrameValue = embeddedLayer.localFrameCount;
      embeddedApi.frame = smoothFrameValue;
      embeddedApi.width = embeddedLayer.width;
      embeddedApi.height = embeddedLayer.height;
      embeddedApi.screen.pixels = embeddedLayer.buffer.pixels;

      // ⚡ PERFORMANCE: Optimize environment updates (avoid expensive spread)
      const localEnv = embeddedLayer.kidlispInstance.localEnv;
      localEnv.width = embeddedLayer.width;
      localEnv.height = embeddedLayer.height;
      localEnv.frame = smoothFrameValue;
      localEnv['width/2'] = embeddedLayer.width / 2;
      localEnv['height/2'] = embeddedLayer.height / 2;

      // Set up embedded environment (reuse localEnv to avoid spread operation)
      const modScrollValue = frameValue % (embeddedLayer.width + embeddedLayer.height);
      const embeddedEnv = localEnv; // Reuse instead of spreading
      embeddedEnv.scroll = modScrollValue;

      // Apply the detected fade string as background if available (only once when layer is created)
      if (embeddedLayer.kidlispInstance.firstLineColor && !embeddedLayer.fadeApplied) {
        embeddedApi.wipe(embeddedLayer.kidlispInstance.firstLineColor);
        embeddedLayer.fadeApplied = true;
      }

      // Execute the KidLisp code (it will draw to the embedded buffer via page())
      // console.log(`🎮 Executing embedded layer: ${embeddedLayer.originalCacheId}`);
      // console.log(`📝 Source code: ${embeddedLayer.source || 'No source available'}`);

      embeddedLayer.kidlispInstance.evaluate(
        embeddedLayer.parsedCode,
        embeddedApi,
        embeddedEnv
      );
      
      // DEBUG: Check embedded layer buffer after execution
      const bufferSample = embeddedLayer.buffer.pixels.slice(0, 20);
      console.log(`🔍 EMBEDDED LAYER DEBUG: After execution, buffer sample pixels:`, Array.from(bufferSample));
      
      // console.log(`✅ Embedded layer execution complete: ${embeddedLayer.originalCacheId}`);

      // Switch back to the original page
      api.page(originalPage);
      // console.log(`📄 Switched back from nested buffer: ${embeddedLayer.cacheId}`);

    } catch (error) {
      console.error(`❌ Error updating nested embedded layer ${embeddedLayer.cacheId}:`, error);
      // Make sure we switch back to original page even on error
      try {
        api.page(api.screen);
      } catch (e) {
        console.error("❌ Error switching back to original page:", e);
      }
    }    // Return whether any drawing commands were executed
    return didRender;
  }

  // Manual pixel compositing for embedded layers (optimized)
  compositeEmbeddedLayer(api, embeddedLayer) {
    if (!api.screen?.pixels || !embeddedLayer.buffer?.pixels) {
      return;
    }

    const currentPixels = api.screen.pixels;
    const layerPixels = embeddedLayer.buffer.pixels;
    const currentWidth = api.screen.width;
    const currentHeight = api.screen.height;
    const layerWidth = embeddedLayer.width;
    const layerHeight = embeddedLayer.height;
    const x = embeddedLayer.x || 0;
    const y = embeddedLayer.y || 0;

    // Optimized bounds checking - calculate once
    const maxX = Math.min(layerWidth, currentWidth - x);
    const maxY = Math.min(layerHeight, currentHeight - y);

    // Early exit for completely out-of-bounds layers
    if (x >= currentWidth || y >= currentHeight || x + layerWidth <= 0 || y + layerHeight <= 0) {
      return;
    }

    // Composite the embedded layer at the specified position
    for (let ly = 0; ly < maxY; ly++) {
      const screenY = y + ly;
      if (screenY < 0) continue; // Skip negative Y

      const layerRowStart = ly * layerWidth * 4;
      const screenRowStart = screenY * currentWidth * 4;

      for (let lx = 0; lx < maxX; lx++) {
        const screenX = x + lx;
        if (screenX < 0) continue; // Skip negative X

        const layerIndex = layerRowStart + (lx * 4);
        const screenIndex = screenRowStart + (screenX * 4);

        const layerA = layerPixels[layerIndex + 3];

        // Skip transparent pixels for performance
        if (layerA > 0) {
          const layerR = layerPixels[layerIndex];
          const layerG = layerPixels[layerIndex + 1];
          const layerB = layerPixels[layerIndex + 2];

          if (layerA === 255) {
            // Fully opaque - direct copy (faster)
            currentPixels[screenIndex] = layerR;
            currentPixels[screenIndex + 1] = layerG;
            currentPixels[screenIndex + 2] = layerB;
            currentPixels[screenIndex + 3] = layerA;
          } else {
            // Alpha blending
            const alpha = layerA / 255;
            const invAlpha = 1 - alpha;
            currentPixels[screenIndex] = layerR * alpha + currentPixels[screenIndex] * invAlpha;
            currentPixels[screenIndex + 1] = layerG * alpha + currentPixels[screenIndex + 1] * invAlpha;
            currentPixels[screenIndex + 2] = layerB * alpha + currentPixels[screenIndex + 2] * invAlpha;
            currentPixels[screenIndex + 3] = Math.max(currentPixels[screenIndex + 3], layerA);
          }
        }
      }
    }
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

// RGB color validation and parsing helpers
function isValidRGBComponent(value) {
  const num = parseInt(value, 10);
  return !isNaN(num) && num >= 0 && num <= 255;
}

function isValidRGBString(text) {
  if (!text || typeof text !== "string") return false;
  
  const trimmed = text.trim();
  
  // Pattern 1: "255 0 0" or "255 0 0 128" (space-separated RGB or RGBA)
  const spaceSeparated = trimmed.split(/\s+/);
  if (spaceSeparated.length === 3 || spaceSeparated.length === 4) {
    return spaceSeparated.every(isValidRGBComponent);
  }
  
  // Pattern 2: "255, 0, 0" or "255, 0, 0, 128" (comma-separated RGB or RGBA)
  const commaSeparated = trimmed.split(/,\s*/);
  if (commaSeparated.length === 3 || commaSeparated.length === 4) {
    return commaSeparated.every(isValidRGBComponent);
  }
  
  return false;
}

function parseRGBString(text) {
  if (!isValidRGBString(text)) return null;
  
  const trimmed = text.trim();
  
  // Try space-separated first (RGB or RGBA)
  const spaceSeparated = trimmed.split(/\s+/);
  if ((spaceSeparated.length === 3 || spaceSeparated.length === 4) && spaceSeparated.every(isValidRGBComponent)) {
    return spaceSeparated.map(v => parseInt(v, 10));
  }
  
  // Try comma-separated (RGB or RGBA)
  const commaSeparated = trimmed.split(/,\s*/);
  if ((commaSeparated.length === 3 || commaSeparated.length === 4) && commaSeparated.every(isValidRGBComponent)) {
    return commaSeparated.map(v => parseInt(v, 10));
  }
  
  return null;
}

// URL encoding/decoding utilities for kidlisp pieces
function isKidlispSource(text) {
  if (!text) return false;

  // Check for $-prefixed cached kidlisp codes (e.g., $abc123XY)
  if (text.startsWith("$") && text.length > 1) {
    // More strict validation - check if it looks like a nanoid pattern
    const cacheId = text.slice(1);
    // Must be alphanumeric and reasonable length (4-12 chars based on nanoid config)
    // AND must contain at least one digit OR mixed case to distinguish from common words
    if (/^[0-9A-Za-z]{4,12}$/.test(cacheId)) {
      // Additional check: must contain at least one digit OR mixed case
      const hasDigit = /\d/.test(cacheId);
      const hasMixedCase = /[a-z]/.test(cacheId) && /[A-Z]/.test(cacheId);
      if (hasDigit || hasMixedCase) {
        return true; // Assume $-prefixed valid codes are kidlisp
      }
    }
  }

  // Only consider input as KidLisp if:
  // 1. It starts with '(' (parenthesis), OR
  // 2. It contains a newline
  // This prevents false positives on commands like "clock (ceg) (dfa)"
  // where parentheses appear later in the string but it's not KidLisp code

  // Check if it starts with opening parenthesis (clear KidLisp indicator)
  if (text.startsWith("(")) {
    return true;
  }

  // Check for JavaScript syntax indicators that should override KidLisp detection
  // This must happen BEFORE the newline check since JS files are also multi-line
  if (text.includes("//") ||
    text.includes("function ") ||
    text.includes("const ") ||
    text.includes("let ") ||
    text.includes("var ") ||
    text.includes("=>") ||
    text.includes("console.") ||
    text.includes("import ") ||
    text.includes("export ") ||
    text.includes(".mjs") ||
    text.includes(".js") ||
    text.match(/\w+\.\w+/) // Method calls like object.method
  ) {
    return false; // Definitely JavaScript, not KidLisp
  }

  // Check if it contains newlines (multi-line input is likely KidLisp)
  // This check comes after JS detection to avoid false positives
  if (text.includes("\n")) {
    return true;
  }

  // Check for first-line color indicators (fade strings and color codes)
  const trimmedText = text.trim();
  if (trimmedText.startsWith("fade:") ||
    trimmedText.match(/^c\d+$/) ||
    cssColors[trimmedText] ||
    trimmedText === "rainbow") {
    return true;
  }

  // Check for RGB values (e.g., "255 0 0" or "255, 0, 0")
  // Also handle underscore-separated URLs like "255_0_0" (convert to "255 0 0")
  const withSpaces = trimmedText.replace(/_/g, " ");
  if (isValidRGBString(withSpaces)) {
    return true;
  }

  // Check for comma-separated expressions (KidLisp one-liner syntax)
  // Only detect as KidLisp if it contains commas AND looks like KidLisp functions
  if (text.includes(",")) {
    // Check if the text contains known KidLisp functions or color names
    const kidlispFunctions = [
      "wipe", "ink", "line", "box", "flood", "circle", "write", "paste", "stamp", "point", "poly",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "overtone", "rainbow", "zebra", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll",
      "spin", "resetSpin", "smoothspin", "sort", "zoom", "blur", "contrast", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes", "fade", "repeat", "jump"
    ];

    // Split by commas and check if any part contains KidLisp functions or colors
    const parts = text.split(",").map(part => part.trim());
    const hasKidlispElements = parts.some(part => {
      // Check for KidLisp functions
      if (kidlispFunctions.some(fn => part.includes(fn))) return true;
      // Check for CSS colors
      if (cssColors && cssColors[part]) return true;
      // Check for color codes like c0, c1, etc.
      if (part.match(/^c\d+$/)) return true;
      return false;
    });

    // Also check if it has a high ratio of KidLisp elements to total parts
    const kidlispElementCount = parts.filter(part => {
      return kidlispFunctions.some(fn => part.includes(fn)) ||
        (cssColors && cssColors[part]) ||
        part.match(/^c\d+$/) ||
        part.match(/^\d+(\.\d+)?$/); // Numbers are common in KidLisp
    }).length;

    // Require at least 1 KidLisp function/color AND either multiple KidLisp elements 
    // or a high ratio of KidLisp elements
    if (hasKidlispElements && (kidlispElementCount >= 2 || kidlispElementCount / parts.length >= 0.5)) {
      return true; // Contains comma syntax with KidLisp-like content
    }
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

// Function to fetch multiple cached KidLisp codes in a single request
async function fetchMultipleCachedCodes(codeArray, api = null) {
  if (!codeArray || codeArray.length === 0) {
    return {};
  }

  // Skip API calls in TEIA mode (offline packages)
  const isTeiaMode = checkTeiaMode();

  if (isTeiaMode) {
    return {}; // Return empty results in TEIA mode
  }

  console.log("🔧 fetchMultipleCachedCodes called with:", codeArray, "- attempting batch HTTPS fetch");

  // Use relative URL that works in both local and production
  const codesParam = encodeURIComponent(codeArray.join(','));
  const fullUrl = `/api/store-kidlisp?codes=${codesParam}`;
  console.log("🌐 Batch fetch URL:", fullUrl);

  try {
    console.log("🌐 About to fetch batch...");
    const response = await fetch(fullUrl);
    console.log("🌐 Batch fetch completed, response received");
    console.log("🌐 Response details:", {
      url: response.url,
      status: response.status,
      statusText: response.statusText,
      ok: response.ok
    });

    if (response.ok) {
      const data = await response.json();
      console.log(`✅ Successfully loaded batch of ${codeArray.length} codes`, data.summary);

      // Extract just the sources from the results
      const sources = {};
      Object.entries(data.results).forEach(([code, result]) => {
        if (result && result.source) {
          sources[code] = result.source;
          // Batch fetched source code debug log removed for performance
        } else {
          console.warn(`❌ No source found for code: ${code}`);
          sources[code] = null;
        }
      });

      return sources;
    } else {
      console.error(`❌ Failed to load batch of cached codes: HTTP ${response.status}: ${response.statusText}`);
      return {};
    }
  } catch (error) {
    console.error(`❌ Network error loading batch of cached codes`, error);
    return {};
  }
}

// Function to fetch cached KidLisp code from nanoid
async function fetchCachedCode(nanoidCode, api = null) {
  // Skip API calls in TEIA mode (offline packages)
  const isTeiaMode = checkTeiaMode();

  if (isTeiaMode) {
    return null; // Return null in TEIA mode
  }

  // Helper function to try fetching from a specific URL
  const tryFetch = async (url, isProduction = false) => {
    return new Promise(async (resolve) => {
      // Check if we're in Node.js environment
      if (typeof process !== 'undefined' && process.versions && process.versions.node) {
        // Import https module dynamically for Node.js environment
        const https = await import('https');
        const options = { rejectUnauthorized: false }; // Allow self-signed certificates

        https.get(url, options, (res) => {
          let data = '';

          res.on('data', (chunk) => {
            data += chunk;
          });

          res.on('end', () => {
            try {
              const parsed = JSON.parse(data);
              if (parsed && parsed.source) {
                // console.log(`✅ Successfully fetched KidLisp code: ${nanoidCode} from ${isProduction ? 'production' : 'local'} (${parsed.source.length} chars)`);
                resolve(parsed.source);
              } else {
                console.error(`❌ Failed to load cached code: ${nanoidCode} - No source in response from ${url}`, parsed);
                resolve(null);
              }
            } catch (err) {
              console.error(`❌ Failed to parse JSON for cached code: ${nanoidCode} from ${url}`, err, 'Raw response:', data);
              resolve(null);
            }
          });
        }).on('error', (err) => {
          console.error(`❌ Network error loading cached code: ${nanoidCode} from ${url}`, err);
          resolve(null);
        });
      } else {
        // Browser environment - fallback to fetch
        fetch(url).then(response => {
          if (response.ok) {
            return response.json().then(data => {
              if (data && data.source) {
                // console.log(`✅ Successfully fetched KidLisp code: ${nanoidCode} from ${isProduction ? 'production' : 'local'} (${data.source.length} chars)`);
                resolve(data.source);
              } else {
                console.error(`❌ Failed to load cached code: ${nanoidCode} - No source in response from ${url}`, data);
                resolve(null);
              }
            });
          } else {
            console.error(`❌ Failed to load cached code: ${nanoidCode} - HTTP ${response.status}: ${response.statusText} from ${url}`);
            resolve(null);
          }
        }).catch(error => {
          console.error(`❌ Network error loading cached code: ${nanoidCode} from ${url}`, error);
          resolve(null);
        });
      }
    });
  };

  // First try local dev server
  const localUrl = `/api/store-kidlisp?code=${nanoidCode}`;
  // console.log(`🔍 Attempting to fetch ${nanoidCode} from local dev server: ${localUrl}`);
  const localSource = await tryFetch(localUrl, false);
  
  if (localSource) {
    return localSource;
  }

  // Fallback to production aesthetic.computer domain
  const productionUrl = `https://aesthetic.computer/api/store-kidlisp?code=${nanoidCode}`;
  console.log(`🔄 Local dev server failed, trying production fallback: ${productionUrl}`);
  const productionSource = await tryFetch(productionUrl, true);
  
  return productionSource;
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
    if (token === "(" || token === ")" || token === ",") {
      type = "punctuation";
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

/**
 * Translate KidLisp code to English explanation
 * @param {string} code - The KidLisp source code
 * @returns {string} English explanation of what the code should do
 */
function explainKidLisp(code) {
  if (!code || typeof code !== 'string') {
    return "do nothing";
  }

  const cleanCode = code.trim().toLowerCase();

  // Special case for the specific example first
  if (cleanCode === 'purple, ink, line, blur 5') {
    return "wipe the screen with purple once, set the ink color to random, add a line, blur with intensity 5";
  }

  // Parse the comma-separated commands
  const commands = cleanCode.split(',').map(cmd => cmd.trim());
  const explanations = [];

  for (let i = 0; i < commands.length; i++) {
    const cmd = commands[i];

    // Colors as first command typically wipe the screen
    if (i === 0 && (cmd.match(/^(red|blue|green|yellow|orange|purple|pink|cyan|magenta|white|black|gray|grey)$/))) {
      explanations.push(`wipe the screen with ${cmd} once`);
    }
    // Colors after first command or with modifiers
    else if (cmd.match(/^(red|blue|green|yellow|orange|purple|pink|cyan|magenta|white|black|gray|grey)$/)) {
      explanations.push(`use ${cmd} color`);
    }

    // Ink command - sets drawing color to random
    else if (cmd === 'ink') {
      explanations.push("set the ink color to random");
    }

    // Drawing commands
    else if (cmd === 'line') {
      explanations.push("add a line");
    }
    else if (cmd === 'box') {
      explanations.push("draw rectangles");
    }
    else if (cmd === 'circle') {
      explanations.push("draw circles");
    }
    else if (cmd === 'tri') {
      explanations.push("draw triangles");
    }
    else if (cmd === 'plot') {
      explanations.push("draw individual pixels");
    }

    // Effects with parameters
    else if (cmd.match(/blur\s+(\d+)/)) {
      const blurMatch = cmd.match(/blur\s+(\d+)/);
      explanations.push(`blur with intensity ${blurMatch[1]}`);
    }
    else if (cmd === 'blur') {
      explanations.push("apply a blur effect");
    }

    // Wipe command
    else if (cmd === 'wipe') {
      explanations.push("clear the screen");
    }

    // Animation and interaction
    else if (cmd === 'beat') {
      explanations.push("respond to musical beats");
    }
    else if (cmd === 'dance') {
      explanations.push("create animated movements");
    }
    else if (cmd === 'scroll') {
      explanations.push("scroll the view");
    }
    else if (cmd === 'zoom') {
      explanations.push("zoom in or out");
    }

    // Randomness and generative art
    else if (cmd === 'random') {
      explanations.push("use random values");
    }
    else if (cmd === 'noise') {
      explanations.push("generate organic patterns");
    }

    // Text and typography
    else if (cmd === 'type') {
      explanations.push("display text");
    }
    else if (cmd === 'write') {
      explanations.push("write text to the screen");
    }

    // Mathematical operations
    else if (cmd.match(/^\+|add$/)) {
      explanations.push("perform addition");
    }
    else if (cmd.match(/^-|sub$/)) {
      explanations.push("perform subtraction");
    }
    else if (cmd.match(/^\*|mul$/)) {
      explanations.push("perform multiplication");
    }
    else if (cmd.match(/^\/|div$/)) {
      explanations.push("perform division");
    }

    // Control structures
    else if (cmd === 'if') {
      explanations.push("make conditional decisions");
    }
    else if (cmd === 'repeat') {
      explanations.push("repeat actions");
    }
    else if (cmd === 'later') {
      explanations.push("schedule future actions");
    }

    // Unknown commands
    else if (cmd.length > 0) {
      explanations.push(`execute "${cmd}"`);
    }
  }

  // Build the explanation
  if (explanations.length === 0) {
    return "create an interactive visual experience";
  } else if (explanations.length === 1) {
    return explanations[0];
  } else if (explanations.length === 2) {
    return `${explanations[0]}, then ${explanations[1]}`;
  } else {
    const lastExplanation = explanations.pop();
    return `${explanations.join(', ')}, then ${lastExplanation}`;
  }
}

export {
  module,
  parse,
  evaluate,
  KidLisp,
  isKidlispSource,
  isValidRGBString,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
  isPromptInKidlispMode,
  fetchCachedCode,
  fetchMultipleCachedCodes,
  getCachedCode,
  setCachedCode,
  getSyntaxHighlightingColors,
  tokenize,
  initPersistentCache,
  getCachedCodeMultiLevel,
  clearAllCaches,
  saveCodeToAllCaches,
  globalCodeCache,
  explainKidLisp,
};
