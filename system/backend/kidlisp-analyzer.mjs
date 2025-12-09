// kidlisp-analyzer.mjs - Static analysis of KidLisp source code
// Extracts traits, functions used, complexity metrics for NFT metadata

// KidLisp API Categories
// Each category represents a major capability area
export const KIDLISP_API = {
  // Core Drawing Primitives
  drawing: {
    name: "Drawing",
    functions: ["ink", "line", "box", "circle", "tri", "poly", "plot", "point", "shape", "lines"],
    description: "Basic drawing primitives",
  },
  
  // Screen Management
  screen: {
    name: "Screen",
    functions: ["wipe", "resolution", "scroll", "bake", "coat"],
    description: "Screen and canvas management",
  },
  
  // Text
  text: {
    name: "Text",
    functions: ["write", "print", "label", "debug", "log"],
    description: "Text rendering and output",
  },
  
  // Images & Media
  media: {
    name: "Media",
    functions: ["paste", "stamp", "embed"],
    description: "Image and media embedding",
  },
  
  // Color System
  color: {
    name: "Color",
    functions: ["rainbow", "zebra", "fade", "flood"],
    description: "Advanced color and gradients",
  },
  
  // Animation & Motion
  animation: {
    name: "Animation",
    functions: ["wiggle", "spin", "smoothspin", "resetSpin", "zoom", "pan", "unpan"],
    description: "Motion and animation effects",
  },
  
  // Math & Logic
  math: {
    name: "Math",
    functions: ["+", "-", "*", "/", "%", "mod", "sin", "cos", "tan", "floor", "ceil", "round", "random", "noise", "min", "max"],
    description: "Mathematical operations",
  },
  
  // Control Flow
  control: {
    name: "Control",
    functions: ["if", "not", "repeat", "bunch", "range", "once", "later", "def", "now", "die", "choose", "sort"],
    description: "Program flow and logic",
  },
  
  // Comparison
  comparison: {
    name: "Comparison",
    functions: [">", "<", "=", "?"],
    description: "Comparison operators",
  },
  
  // Timing
  timing: {
    name: "Timing",
    functions: ["delay", "hop"],
    description: "Timing and scheduling",
  },
  
  // Interaction
  interaction: {
    name: "Interaction",
    functions: ["tap", "draw"],
    description: "User interaction handlers",
  },
  
  // Audio
  audio: {
    name: "Audio",
    functions: ["speaker", "melody", "overtone", "mic", "amplitude"],
    description: "Sound and music",
  },
  
  // Effects
  effects: {
    name: "Effects",
    functions: ["blur", "contrast", "mask", "unmask", "steal", "putback"],
    description: "Visual effects and filters",
  },
  
  // Navigation
  navigation: {
    name: "Navigation",
    functions: ["jump"],
    description: "Piece navigation",
  },
  
  // Network
  network: {
    name: "Network",
    functions: ["net"],
    description: "Network and API access",
  },
  
  // Utility
  utility: {
    name: "Utility",
    functions: ["len", "source", "trans", "qr"],
    description: "Utility functions",
  },
};

// All known functions flattened
export const ALL_FUNCTIONS = Object.values(KIDLISP_API).flatMap(cat => cat.functions);

// Timing pattern regex
const TIMING_PATTERN = /(\d*\.?\d+)s(\.\.\.?|!)?/g;

// Analyze KidLisp source code and extract rich metadata
export function analyzeKidLisp(source, options = {}) {
  if (!source || typeof source !== "string") {
    return { error: "Invalid source" };
  }
  
  const lines = source.split("\n");
  const nonEmptyLines = lines.filter(l => l.trim().length > 0);
  const commentLines = lines.filter(l => l.trim().startsWith(";"));
  const codeLines = nonEmptyLines.filter(l => !l.trim().startsWith(";"));
  
  // Extract all function calls
  const functionCalls = extractFunctionCalls(source);
  
  // Count unique functions
  const uniqueFunctions = [...new Set(functionCalls)];
  
  // Categorize functions
  const categories = categorize(uniqueFunctions);
  
  // Extract timing expressions
  const timings = extractTimings(source);
  
  // Count definitions
  const definitions = countDefinitions(source);
  
  // Detect complexity indicators
  const complexity = analyzeComplexity(source, functionCalls, definitions);
  
  // Detect embedded references
  const embeds = extractEmbeds(source);
  
  // Detect colors used
  const colors = extractColors(source);
  
  // Build traits list for NFT attributes
  const categoryNames = Object.keys(categories);
  const traits = buildTraits({
    categories,
    categoryNames,
    timings,
    definitions,
    complexity,
    embeds,
    colors,
    lineCount: codeLines.length,
    commentCount: commentLines.length,
    uniqueFunctionCount: uniqueFunctions.length,
    uniqueFunctions,
    embedCount: embeds.length,
  });
  
  return {
    // Basic metrics
    lineCount: lines.length,
    codeLineCount: codeLines.length,
    commentLineCount: commentLines.length,
    charCount: source.length,
    
    // Function analysis
    functionCalls: functionCalls.length,
    uniqueFunctionCount: uniqueFunctions.length,
    uniqueFunctions,
    
    // Categories in use
    categories,
    categoryNames: Object.keys(categories),
    
    // Timing
    hasTimings: timings.length > 0,
    timings,
    
    // Definitions
    variableCount: definitions.variables.length,
    functionDefCount: definitions.functions.length,
    definitions,
    
    // Complexity
    complexity,
    
    // Embeds
    embedCount: embeds.length,
    embeds,
    
    // Colors
    colorCount: colors.length,
    colors,
    
    // NFT Traits (ready for attributes array)
    traits,
  };
}

// Extract all function calls from source
function extractFunctionCalls(source) {
  const calls = [];
  
  // Match function calls: (functionName ...)
  // Also match bare function names at start of comma-separated expressions
  const patterns = [
    /\(\s*([a-zA-Z_+\-*/%<>=?][a-zA-Z0-9_+\-*/%<>=?.:]*)/g, // (function ...)
    /^\s*([a-zA-Z_][a-zA-Z0-9_]*)\s/gm, // bare function at line start (comma syntax)
  ];
  
  for (const pattern of patterns) {
    let match;
    while ((match = pattern.exec(source)) !== null) {
      const func = match[1].split(":")[0].split(".")[0]; // Strip colon params and dots
      if (ALL_FUNCTIONS.includes(func) || isOperator(func)) {
        calls.push(func);
      }
    }
  }
  
  return calls;
}

function isOperator(func) {
  return ["+", "-", "*", "/", "%", ">", "<", "=", "?"].includes(func);
}

// Categorize functions by API area
function categorize(functions) {
  const result = {};
  
  for (const [key, category] of Object.entries(KIDLISP_API)) {
    const used = functions.filter(f => category.functions.includes(f));
    if (used.length > 0) {
      result[key] = {
        name: category.name,
        functions: used,
        count: used.length,
      };
    }
  }
  
  return result;
}

// Extract timing expressions
function extractTimings(source) {
  const timings = [];
  let match;
  
  while ((match = TIMING_PATTERN.exec(source)) !== null) {
    const value = parseFloat(match[1]) || 1;
    const suffix = match[2] || "";
    
    timings.push({
      raw: match[0],
      seconds: value,
      repeating: suffix === "..." || suffix === "..",
      once: suffix === "!",
    });
  }
  
  return timings;
}

// Count variable and function definitions
function countDefinitions(source) {
  const variables = [];
  const functions = [];
  
  // (def name value)
  const defPattern = /\(\s*def\s+([a-zA-Z_][a-zA-Z0-9_]*)/g;
  let match;
  while ((match = defPattern.exec(source)) !== null) {
    variables.push(match[1]);
  }
  
  // (later name params... body)
  const laterPattern = /\(\s*later\s+([a-zA-Z_][a-zA-Z0-9_]*)/g;
  while ((match = laterPattern.exec(source)) !== null) {
    functions.push(match[1]);
  }
  
  return {
    variables: [...new Set(variables)],
    functions: [...new Set(functions)],
  };
}

// Analyze code complexity
function analyzeComplexity(source, functionCalls, definitions) {
  const nestingDepth = calculateNestingDepth(source);
  const hasRecursion = detectRecursion(source, definitions.functions);
  const hasConditionals = /\(\s*(if|not|>|<|=|choose|\?)\s/.test(source);
  const hasLoops = /\(\s*(repeat|bunch|range)\s/.test(source);
  const hasInteraction = /\(\s*(tap|draw)\s/.test(source);
  const hasAnimation = /\(\s*(wiggle|spin|smoothspin|zoom|pan)\s/.test(source);
  const hasAudio = /\(\s*(speaker|melody|overtone|mic|amplitude)\s/.test(source);
  const hasNetwork = /\(\s*net\s/.test(source);
  const hasEmbeds = /\$[a-zA-Z0-9]+/.test(source);
  
  // Calculate complexity score
  let score = 0;
  score += Math.min(functionCalls.length * 0.5, 20);
  score += definitions.variables.length * 2;
  score += definitions.functions.length * 5;
  score += nestingDepth * 3;
  if (hasRecursion) score += 10;
  if (hasConditionals) score += 3;
  if (hasLoops) score += 3;
  if (hasInteraction) score += 5;
  if (hasAnimation) score += 2;
  if (hasAudio) score += 5;
  if (hasNetwork) score += 5;
  if (hasEmbeds) score += 3;
  
  // Determine tier
  let tier;
  if (score < 10) tier = "Simple";
  else if (score < 25) tier = "Moderate";
  else if (score < 50) tier = "Complex";
  else tier = "Advanced";
  
  return {
    score: Math.round(score),
    tier,
    nestingDepth,
    hasRecursion,
    hasConditionals,
    hasLoops,
    hasInteraction,
    hasAnimation,
    hasAudio,
    hasNetwork,
    hasEmbeds,
  };
}

// Calculate maximum nesting depth
function calculateNestingDepth(source) {
  let maxDepth = 0;
  let currentDepth = 0;
  
  for (const char of source) {
    if (char === "(") {
      currentDepth++;
      maxDepth = Math.max(maxDepth, currentDepth);
    } else if (char === ")") {
      currentDepth = Math.max(0, currentDepth - 1);
    }
  }
  
  return maxDepth;
}

// Detect recursive function calls
function detectRecursion(source, functionNames) {
  for (const name of functionNames) {
    // Look for function calling itself within its body
    const laterPattern = new RegExp(`\\(\\s*later\\s+${name}[^)]*\\([^)]*${name}`, "s");
    if (laterPattern.test(source)) {
      return true;
    }
  }
  return false;
}

// Extract embedded code references
function extractEmbeds(source) {
  const embeds = [];
  const pattern = /\$([a-zA-Z0-9]+)/g;
  let match;
  
  while ((match = pattern.exec(source)) !== null) {
    if (!embeds.includes(match[1])) {
      embeds.push(match[1]);
    }
  }
  
  return embeds;
}

// Extract color references
function extractColors(source) {
  const colors = new Set();
  
  // Named colors in quotes
  const namedPattern = /"(red|blue|green|yellow|orange|purple|pink|cyan|magenta|white|black|gray|grey|lime|navy|teal|maroon|olive|silver|aqua|fuchsia|coral|salmon|gold|khaki|plum|violet|indigo|tan|sienna|peru|chocolate|crimson|tomato|turquoise)"/gi;
  let match;
  while ((match = namedPattern.exec(source)) !== null) {
    colors.add(match[1].toLowerCase());
  }
  
  // rainbow and zebra
  if (/rainbow/.test(source)) colors.add("rainbow");
  if (/zebra/.test(source)) colors.add("zebra");
  
  // fade patterns
  if (/fade:/.test(source)) colors.add("fade");
  
  return [...colors];
}

// Build traits array for NFT attributes
function buildTraits(analysis) {
  const traits = [];
  
  // Language
  traits.push({ name: "Language", value: "KidLisp" });
  
  // Complexity Tier
  traits.push({ name: "Complexity", value: analysis.complexity.tier });
  
  // Complexity Score (numeric for rarity)
  traits.push({ name: "Complexity Score", value: String(analysis.complexity.score) });
  
  // Line Count Range
  const lineRange = getLineRange(analysis.lineCount);
  traits.push({ name: "Size", value: lineRange });
  
  // Lines of Code (exact)
  traits.push({ name: "Lines of Code", value: String(analysis.lineCount) });
  
  // API Categories Used
  for (const catKey of Object.keys(analysis.categories)) {
    const cat = analysis.categories[catKey];
    traits.push({ name: `Uses ${cat.name}`, value: "Yes" });
  }
  
  // Category Count
  traits.push({ name: "API Breadth", value: String(analysis.categoryNames.length) });
  
  // Unique Functions
  traits.push({ name: "Unique Functions", value: String(analysis.uniqueFunctionCount) });
  
  // Interactive?
  if (analysis.complexity.hasInteraction) {
    traits.push({ name: "Interactive", value: "Yes" });
  }
  
  // Animated?
  if (analysis.complexity.hasAnimation) {
    traits.push({ name: "Animated", value: "Yes" });
  }
  
  // Audio?
  if (analysis.complexity.hasAudio) {
    traits.push({ name: "Audio", value: "Yes" });
  }
  
  // Timed?
  if (analysis.timings.length > 0) {
    traits.push({ name: "Timed", value: "Yes" });
    if (analysis.timings.some(t => t.repeating)) {
      traits.push({ name: "Looping", value: "Yes" });
    }
  }
  
  // Networked?
  if (analysis.complexity.hasNetwork) {
    traits.push({ name: "Networked", value: "Yes" });
  }
  
  // Has Embeds?
  if (analysis.embedCount > 0) {
    traits.push({ name: "Composite", value: "Yes" });
    traits.push({ name: "Embed Count", value: String(analysis.embedCount) });
  }
  
  // Nesting Depth (for algorithmic complexity)
  if (analysis.complexity.nestingDepth >= 5) {
    traits.push({ name: "Deep Nesting", value: String(analysis.complexity.nestingDepth) });
  }
  
  // Variable Count
  if (analysis.definitions.variables.length > 0) {
    traits.push({ name: "Variables", value: String(analysis.definitions.variables.length) });
  }
  
  // Custom Function Count
  if (analysis.definitions.functions.length > 0) {
    traits.push({ name: "Custom Functions", value: String(analysis.definitions.functions.length) });
  }
  
  // Has Recursion?
  if (analysis.complexity.hasRecursion) {
    traits.push({ name: "Recursive", value: "Yes" });
  }
  
  // Has Conditionals?
  if (analysis.complexity.hasConditionals) {
    traits.push({ name: "Conditional", value: "Yes" });
  }
  
  // Has Loops?
  if (analysis.complexity.hasLoops) {
    traits.push({ name: "Iterative", value: "Yes" });
  }
  
  // Color Palette
  if (analysis.colors.length > 0) {
    if (analysis.colors.includes("rainbow")) {
      traits.push({ name: "Rainbow", value: "Yes" });
    }
    if (analysis.colors.includes("fade")) {
      traits.push({ name: "Gradients", value: "Yes" });
    }
    traits.push({ name: "Color Count", value: String(analysis.colorCount) });
  }
  
  // Comments
  if (analysis.commentLineCount > 0) {
    traits.push({ name: "Documented", value: "Yes" });
    traits.push({ name: "Comment Lines", value: String(analysis.commentLineCount) });
  }
  
  return traits;
}

// Get line count range bucket
function getLineRange(lineCount) {
  if (lineCount <= 5) return "Micro (1-5)";
  if (lineCount <= 15) return "Small (6-15)";
  if (lineCount <= 30) return "Medium (16-30)";
  if (lineCount <= 60) return "Large (31-60)";
  if (lineCount <= 100) return "Mega (61-100)";
  return "Epic (100+)";
}

// Export version for tracking
export const ANALYZER_VERSION = "1.0.0";
