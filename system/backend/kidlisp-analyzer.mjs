// kidlisp-analyzer.mjs - Formal static analysis of KidLisp source code
// Extracts structural, behavioral, and attribution traits for NFT metadata

// ═══════════════════════════════════════════════════════════════════════════
// FORMAL LISP ANALYSIS - S-Expression Structure
// ═══════════════════════════════════════════════════════════════════════════

// Special forms in KidLisp (not regular function calls)
const SPECIAL_FORMS = ["def", "if", "later", "repeat", "bunch", "range", "tap", "draw", "once"];

// Primitive forms (built-in functions)
const PRIMITIVES = {
  // Drawing
  ink: "gfx", line: "gfx", box: "gfx", circle: "gfx", tri: "gfx", 
  poly: "gfx", plot: "gfx", point: "gfx", shape: "gfx", lines: "gfx",
  // Screen
  wipe: "screen", resolution: "screen", scroll: "screen", bake: "screen", coat: "screen",
  // Text
  write: "text", print: "text", label: "text", debug: "io", log: "io",
  // Media
  paste: "media", stamp: "media", embed: "media",
  // Color
  rainbow: "color", zebra: "color", fade: "color", flood: "color",
  // Transform
  wiggle: "xform", spin: "xform", smoothspin: "xform", resetSpin: "xform",
  zoom: "xform", pan: "xform", unpan: "xform",
  // Math
  sin: "math", cos: "math", tan: "math", floor: "math", ceil: "math",
  round: "math", random: "rand", noise: "rand", min: "math", max: "math",
  mod: "math",
  // Logic
  not: "logic", choose: "logic", sort: "logic",
  // Time
  delay: "time", hop: "time", now: "time", die: "time",
  // Audio
  speaker: "audio", melody: "audio", overtone: "audio", mic: "audio", amplitude: "audio",
  // Effects
  blur: "fx", contrast: "fx", mask: "fx", unmask: "fx", steal: "fx", putback: "fx",
  // Navigation
  jump: "nav",
  // Network
  net: "net",
  // Utility
  len: "util", source: "util", trans: "util", qr: "util",
};

// ═══════════════════════════════════════════════════════════════════════════
// MAIN ANALYSIS FUNCTION
// ═══════════════════════════════════════════════════════════════════════════

export function analyzeKidLisp(source, options = {}) {
  if (!source || typeof source !== "string") {
    return { error: "Invalid source", traits: [] };
  }

  // Basic metrics
  const lines = source.split("\n");
  const codeLines = lines.filter(l => l.trim() && !l.trim().startsWith(";"));
  const commentLines = lines.filter(l => l.trim().startsWith(";"));
  
  // S-expression analysis
  const sexp = analyzeSExpressions(source);
  
  // Form analysis
  const forms = analyzeForms(source);
  
  // Timing patterns
  const timing = analyzeTimingPatterns(source);
  
  // Definitions
  const defs = analyzeDefinitions(source);
  
  // Dependencies (embeds)
  const deps = analyzeDependencies(source);
  
  // Color usage
  const colors = analyzeColors(source);
  
  // Behavioral traits
  const behavior = analyzeBehavior(source, forms, timing);
  
  // Build formal traits for NFT
  const traits = buildFormalTraits({
    source,
    lines: codeLines.length,
    comments: commentLines.length,
    chars: source.length,
    sexp,
    forms,
    timing,
    defs,
    deps,
    colors,
    behavior,
  });

  return {
    // Structural metrics
    lines: codeLines.length,
    comments: commentLines.length,
    chars: source.length,
    
    // S-expression structure
    sexp,
    
    // Form breakdown
    forms,
    
    // Timing
    timing,
    
    // Definitions
    defs,
    
    // Dependencies
    deps,
    
    // Colors
    colors,
    
    // Behavior flags
    behavior,
    
    // NFT Traits
    traits,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// S-EXPRESSION ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeSExpressions(source) {
  let depth = 0;
  let maxDepth = 0;
  let sexpCount = 0;
  let atomCount = 0;
  
  // Count parens and track depth
  for (let i = 0; i < source.length; i++) {
    const c = source[i];
    if (c === "(") {
      depth++;
      sexpCount++;
      maxDepth = Math.max(maxDepth, depth);
    } else if (c === ")") {
      depth = Math.max(0, depth - 1);
    }
  }
  
  // Count atoms (identifiers, numbers, strings outside parens)
  const atoms = source.match(/[a-zA-Z_][a-zA-Z0-9_-]*|\d+\.?\d*|"[^"]*"/g) || [];
  atomCount = atoms.length;
  
  // Density = expressions per line
  const lines = source.split("\n").filter(l => l.trim()).length || 1;
  const density = Math.round((sexpCount / lines) * 10) / 10;
  
  return {
    expressions: sexpCount,
    atoms: atomCount,
    maxDepth,
    density,
    balanced: depth === 0,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// FORM ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeForms(source) {
  const calls = {};
  const categories = {};
  
  // Match function calls: (name ...)
  const pattern = /\(\s*([a-zA-Z_][a-zA-Z0-9_-]*)/g;
  let match;
  
  while ((match = pattern.exec(source)) !== null) {
    const name = match[1];
    calls[name] = (calls[name] || 0) + 1;
    
    // Categorize
    if (SPECIAL_FORMS.includes(name)) {
      categories.special = (categories.special || 0) + 1;
    } else if (PRIMITIVES[name]) {
      const cat = PRIMITIVES[name];
      categories[cat] = (categories[cat] || 0) + 1;
    }
  }
  
  // Also check bare forms (comma syntax)
  const barePattern = /^([a-zA-Z_][a-zA-Z0-9_-]*)\s/gm;
  while ((match = barePattern.exec(source)) !== null) {
    const name = match[1];
    if (PRIMITIVES[name] || SPECIAL_FORMS.includes(name)) {
      calls[name] = (calls[name] || 0) + 1;
    }
  }
  
  const unique = Object.keys(calls);
  const total = Object.values(calls).reduce((a, b) => a + b, 0);
  
  return {
    unique: unique.length,
    total,
    calls,
    categories,
    top: unique.sort((a, b) => calls[b] - calls[a]).slice(0, 5),
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// TIMING PATTERN ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeTimingPatterns(source) {
  const timings = [];
  const pattern = /(\d*\.?\d+)s(\.\.\.?|!)?/g;
  let match;
  
  while ((match = pattern.exec(source)) !== null) {
    const seconds = parseFloat(match[1]) || 1;
    const modifier = match[2] || "";
    timings.push({
      seconds,
      loop: modifier === "..." || modifier === "..",
      once: modifier === "!",
    });
  }
  
  const hasLoops = timings.some(t => t.loop);
  const hasOnce = timings.some(t => t.once);
  const intervals = timings.map(t => t.seconds);
  const fastest = intervals.length ? Math.min(...intervals) : null;
  const slowest = intervals.length ? Math.max(...intervals) : null;
  
  return {
    count: timings.length,
    hasLoops,
    hasOnce,
    fastest,
    slowest,
    timings,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// DEFINITION ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeDefinitions(source) {
  const vars = [];
  const funcs = [];
  
  // (def name value)
  const defPattern = /\(\s*def\s+([a-zA-Z_][a-zA-Z0-9_-]*)/g;
  let match;
  while ((match = defPattern.exec(source)) !== null) {
    vars.push(match[1]);
  }
  
  // (later name ...)
  const laterPattern = /\(\s*later\s+([a-zA-Z_][a-zA-Z0-9_-]*)/g;
  while ((match = laterPattern.exec(source)) !== null) {
    funcs.push(match[1]);
  }
  
  return {
    variables: [...new Set(vars)],
    functions: [...new Set(funcs)],
    varCount: [...new Set(vars)].length,
    funcCount: [...new Set(funcs)].length,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// DEPENDENCY ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeDependencies(source) {
  const deps = [];
  const pattern = /\$([a-zA-Z][a-zA-Z0-9]*)/g;
  let match;
  
  while ((match = pattern.exec(source)) !== null) {
    if (!deps.includes(match[1])) {
      deps.push(match[1]);
    }
  }
  
  return {
    count: deps.length,
    pieces: deps,
    isComposite: deps.length > 0,
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// COLOR ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeColors(source) {
  const named = new Set();
  const patterns = [];
  
  // Named colors
  const colorNames = ["red", "blue", "green", "yellow", "orange", "purple", "pink", 
    "cyan", "magenta", "white", "black", "gray", "grey", "lime", "navy", "teal"];
  const namedPattern = new RegExp(`"(${colorNames.join("|")})"`, "gi");
  let match;
  while ((match = namedPattern.exec(source)) !== null) {
    named.add(match[1].toLowerCase());
  }
  
  // Special patterns
  if (/rainbow/.test(source)) patterns.push("rainbow");
  if (/zebra/.test(source)) patterns.push("zebra");
  if (/fade:/.test(source)) patterns.push("gradient");
  
  return {
    named: [...named],
    patterns,
    count: named.size + patterns.length,
    hasRainbow: patterns.includes("rainbow"),
    hasGradient: patterns.includes("gradient"),
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// BEHAVIOR ANALYSIS
// ═══════════════════════════════════════════════════════════════════════════

function analyzeBehavior(source, forms, timing) {
  const cats = forms.categories;
  
  return {
    // Interactivity
    interactive: !!(cats.special && (source.includes("(tap") || source.includes("(draw"))),
    drawable: source.includes("(draw"),
    tappable: source.includes("(tap"),
    
    // Animation
    animated: !!(cats.xform || timing.count > 0),
    looping: timing.hasLoops,
    timed: timing.count > 0,
    
    // Media
    hasAudio: !!cats.audio,
    hasNetwork: !!cats.net,
    hasEffects: !!cats.fx,
    
    // Complexity indicators
    hasConditionals: /\(\s*(if|>|<|=|\?)\s/.test(source),
    hasIteration: /\(\s*(repeat|bunch|range)\s/.test(source),
    hasRecursion: detectRecursion(source),
    hasRandomness: !!cats.rand,
    
    // Purity
    isPure: !cats.net && !cats.audio && !cats.io,
  };
}

function detectRecursion(source) {
  // Check if any defined function calls itself
  const laterPattern = /\(\s*later\s+([a-zA-Z_][a-zA-Z0-9_-]*)[^)]*\)/g;
  let match;
  while ((match = laterPattern.exec(source)) !== null) {
    const name = match[1];
    const body = match[0];
    if (body.includes(`(${name}`) || body.includes(` ${name} `)) {
      return true;
    }
  }
  return false;
}

// ═══════════════════════════════════════════════════════════════════════════
// FORMAL TRAIT BUILDER (for NFT attributes)
// ═══════════════════════════════════════════════════════════════════════════

function buildFormalTraits(analysis) {
  const { sexp, forms, timing, defs, deps, colors, behavior, lines, chars } = analysis;
  const traits = [];
  
  // ─── CORE IDENTITY ───
  traits.push({ name: "Language", value: "KidLisp" });
  
  // ─── STRUCTURAL ───
  traits.push({ name: "Lines", value: String(lines) });
  traits.push({ name: "Expressions", value: String(sexp.expressions) });
  traits.push({ name: "Depth", value: String(sexp.maxDepth) });
  traits.push({ name: "Density", value: String(sexp.density) });
  traits.push({ name: "Vocabulary", value: String(forms.unique) });
  
  // ─── FORM CLASSIFICATION ───
  // Size category
  let size;
  if (lines <= 1) size = "Atom";
  else if (lines <= 3) size = "Molecule";
  else if (lines <= 8) size = "Cell";
  else if (lines <= 20) size = "Organism";
  else if (lines <= 50) size = "Colony";
  else size = "Ecosystem";
  traits.push({ name: "Size", value: size });
  
  // Depth category
  let depthCat;
  if (sexp.maxDepth <= 2) depthCat = "Flat";
  else if (sexp.maxDepth <= 4) depthCat = "Nested";
  else if (sexp.maxDepth <= 6) depthCat = "Deep";
  else depthCat = "Recursive";
  traits.push({ name: "Structure", value: depthCat });
  
  // ─── BEHAVIORAL ───
  if (behavior.interactive) traits.push({ name: "Interactive", value: "Yes" });
  if (behavior.drawable) traits.push({ name: "Drawable", value: "Yes" });
  if (behavior.animated) traits.push({ name: "Animated", value: "Yes" });
  if (behavior.looping) traits.push({ name: "Looping", value: "Yes" });
  if (behavior.hasAudio) traits.push({ name: "Audio", value: "Yes" });
  if (behavior.hasRandomness) traits.push({ name: "Generative", value: "Yes" });
  if (behavior.hasConditionals) traits.push({ name: "Conditional", value: "Yes" });
  if (behavior.hasIteration) traits.push({ name: "Iterative", value: "Yes" });
  if (behavior.hasRecursion) traits.push({ name: "Recursive", value: "Yes" });
  if (behavior.isPure) traits.push({ name: "Pure", value: "Yes" });
  if (behavior.hasEffects) traits.push({ name: "Effects", value: "Yes" });
  if (behavior.hasNetwork) traits.push({ name: "Networked", value: "Yes" });
  
  // ─── TIMING ───
  if (timing.count > 0) {
    traits.push({ name: "Timed", value: "Yes" });
    if (timing.fastest) traits.push({ name: "Tempo", value: `${timing.fastest}s` });
  }
  
  // ─── COMPOSITION ───
  if (deps.isComposite) {
    traits.push({ name: "Composite", value: "Yes" });
    traits.push({ name: "Dependencies", value: String(deps.count) });
  }
  
  // ─── DEFINITIONS ───
  if (defs.varCount > 0) traits.push({ name: "Variables", value: String(defs.varCount) });
  if (defs.funcCount > 0) traits.push({ name: "Functions", value: String(defs.funcCount) });
  
  // ─── COLOR ───
  if (colors.hasRainbow) traits.push({ name: "Rainbow", value: "Yes" });
  if (colors.hasGradient) traits.push({ name: "Gradient", value: "Yes" });
  if (colors.count > 0) traits.push({ name: "Colors", value: String(colors.count) });
  
  // ─── CATEGORIES USED ───
  const catNames = Object.keys(forms.categories);
  if (catNames.includes("gfx")) traits.push({ name: "Graphics", value: "Yes" });
  if (catNames.includes("xform")) traits.push({ name: "Transforms", value: "Yes" });
  if (catNames.includes("text")) traits.push({ name: "Text", value: "Yes" });
  if (catNames.includes("math")) traits.push({ name: "Math", value: "Yes" });
  
  return traits;
}

// Export version for tracking
export const ANALYZER_VERSION = "2.0.0";
