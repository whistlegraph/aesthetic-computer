// Tool: preview_kidlisp
// Validate KidLisp code syntax without publishing

interface ValidationResult {
  valid: boolean;
  errors: string[];
  warnings: string[];
  stats: {
    expressions: number;
    functions_used: string[];
    has_animation: boolean;
    has_interaction: boolean;
    has_wipe: boolean;
  };
}

function validateKidLisp(source: string): ValidationResult {
  const errors: string[] = [];
  const warnings: string[] = [];
  const functionsUsed = new Set<string>();
  let expressionCount = 0;
  let hasAnimation = false;
  let hasInteraction = false;
  let hasWipe = false;

  if (!source.trim()) {
    errors.push("Source code is empty");
    return {
      valid: false,
      errors,
      warnings,
      stats: {
        expressions: 0,
        functions_used: [],
        has_animation: false,
        has_interaction: false,
        has_wipe: false,
      },
    };
  }

  // Check balanced parentheses
  let depth = 0;
  let inString = false;
  let stringChar = "";
  let inComment = false;

  for (let i = 0; i < source.length; i++) {
    const ch = source[i];

    if (inComment) {
      if (ch === "\n") inComment = false;
      continue;
    }

    if (inString) {
      if (ch === stringChar && source[i - 1] !== "\\") inString = false;
      continue;
    }

    if (ch === ";") {
      inComment = true;
      continue;
    }

    if (ch === '"' || ch === "'") {
      inString = true;
      stringChar = ch;
      continue;
    }

    if (ch === "(") {
      depth++;
      expressionCount++;
    } else if (ch === ")") {
      depth--;
      if (depth < 0) {
        errors.push(`Unmatched closing parenthesis at position ${i}`);
      }
    }
  }

  if (depth > 0) {
    errors.push(
      `${depth} unclosed parenthes${depth === 1 ? "is" : "es"} — missing closing )`,
    );
  }
  if (inString) {
    errors.push("Unterminated string literal");
  }

  // Extract function calls (first token after open paren)
  const funcPattern = /\(\s*([a-zA-Z_+\-*/%?][a-zA-Z0-9_]*)/g;
  let match;
  while ((match = funcPattern.exec(source)) !== null) {
    const fname = match[1];
    functionsUsed.add(fname);

    if (fname === "wipe") hasWipe = true;
    if (fname === "tap" || fname === "draw") hasInteraction = true;
  }

  // Check for bare word background colors
  const firstLine = source.trim().split("\n")[0].trim();
  const bareColors = [
    "black",
    "white",
    "red",
    "blue",
    "green",
    "purple",
    "navy",
    "brown",
    "salmon",
    "beige",
    "cyan",
  ];
  if (bareColors.some((c) => firstLine.startsWith(c))) {
    hasWipe = true;
  }

  // Check for timing expressions (animation)
  if (/\d+\.?\d*s[.!]?/.test(source)) hasAnimation = true;
  if (
    functionsUsed.has("wiggle") ||
    functionsUsed.has("spin") ||
    functionsUsed.has("scroll") ||
    functionsUsed.has("zoom")
  ) {
    hasAnimation = true;
  }

  // Warnings
  if (!hasWipe) {
    warnings.push(
      "No background color found — consider starting with (wipe color) or a bare color name",
    );
  }

  // Check for common mistakes: dashes in identifiers
  const dashIdentifier = /\(def\s+[a-zA-Z][a-zA-Z0-9]*-[a-zA-Z]/;
  if (dashIdentifier.test(source)) {
    errors.push(
      "Identifier contains a dash (-) which is parsed as subtraction. Use underscores: my_var not my-var",
    );
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    stats: {
      expressions: expressionCount,
      functions_used: [...functionsUsed].sort(),
      has_animation: hasAnimation,
      has_interaction: hasInteraction,
      has_wipe: hasWipe,
    },
  };
}

export const previewKidLispTool = {
  name: "preview_kidlisp",
  description:
    "Validate KidLisp source code without publishing. Checks syntax (balanced parens, strings), identifies functions used, and flags common mistakes. Use this before publish_kidlisp to catch errors.",
  inputSchema: {
    type: "object" as const,
    properties: {
      source: {
        type: "string",
        description: "KidLisp source code to validate",
      },
    },
    required: ["source"],
  },
};

export async function previewKidLisp(args: { source: string }) {
  return validateKidLisp(args.source);
}
