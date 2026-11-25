// KidLisp URL, kidlisp-url.mjs
// How KidLisp source travels: telling it apart from a piece name, carrying it
// in a URL, tokenizing it for colour, and the $code registry. Small and pure so
// the main thread (parse.mjs, headers.mjs, bios.mjs) can use it without
// loading the whole evaluator; kidlisp.mjs re-exports every name here.

import { cssColors } from "./num.mjs";

const VERBOSE = false;

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

function tokenize(input) {
  if (VERBOSE) console.log("🪙 Tokenizing:", input);

  // Updated regex to support both double quotes and single quotes for strings
  const regex = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s()";',]+)/g;
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

// URL encoding/decoding utilities for kidlisp pieces
function isKidlispSource(text) {
  if (!text) return false;
  
  // console.log("🔍 [KidLisp Detection START] text:", text, "has comma:", text.includes(","), "starts with (:", text.startsWith("("));

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
    text.match(/[a-zA-Z_$]\w*\.\w+/) // Method calls like object.method (but not decimal numbers like 0.2)
  ) {
    return false; // Definitely JavaScript, not KidLisp
  }

  const trimmedText = text.trim();
  const loweredTrimmedText = trimmedText.toLowerCase();

  // Treat KidLisp comments as KidLisp source (used by prompt-mode detection).
  // KidLisp uses ';' for line comments.
  if (loweredTrimmedText.startsWith(";")) {
    return true;
  }

  // Never treat share-prefixed snippets as KidLisp; these are share metadata, not source
  if (loweredTrimmedText.startsWith("share")) {
    return false;
  }

  // Check if it contains newlines. Multi-line input is only considered KidLisp
  // if it contains KidLisp-like content (e.g., known functions), not just any text.
  // This check comes after JS detection (and share guard) to avoid false positives.
  if (text.includes("\n")) {
    const lines = text.split(/\r?\n/).map((l) => l.trim()).filter(Boolean);
    if (lines.length === 0) return false;

    // Comment-only multi-line blocks count as KidLisp.
    if (lines.every((l) => l.startsWith(";"))) {
      return true;
    }

    const kidlispLineFunctions = new Set([
      "wipe", "ink", "line", "box", "flood", "circle", "write", "paste", "stamp", "point", "poly",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "overtone", "rainbow", "zebra", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll",
      "flip", "spin", "resetspin", "smoothspin", "sort", "zoom", "blur", "contrast", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes", "fade", "repeat", "jump",
      "def", "later", "frame",
    ]);

    const looksLikeKidlispLine = (line) => {
      if (!line || line.startsWith(";")) return false;
      if (line.startsWith("(")) return true;

      // Single-line KidLisp indicators that can appear inside multi-line prompts.
      const lowered = line.toLowerCase();
      if (
        lowered.startsWith("fade:") ||
        /^c\d+$/.test(lowered) ||
        /^p\d+$/.test(lowered) ||
        lowered === "rainbow" ||
        lowered === "zebra" ||
        (cssColors && cssColors[lowered])
      ) {
        return true;
      }

      // RGB values like "255 0 0" or "255, 0, 0".
      const withSpaces = lowered.replace(/_/g, " ");
      if (isValidRGBString(withSpaces)) {
        return true;
      }

      const firstToken = lowered.split(/\s+/)[0] || "";
      const cleanedToken = firstToken.replace(/^[^a-z0-9]+/i, "").replace(/[^a-z0-9]+$/i, "");
      return kidlispLineFunctions.has(cleanedToken);
    };

    if (lines.some(looksLikeKidlispLine)) {
      return true;
    }

    return false;
  }

  // Check for first-line color indicators (fade strings and color codes)

  // Never treat share-prefixed snippets as KidLisp; these are share metadata, not source
  if (loweredTrimmedText.startsWith("share")) {
    return false;
  }
  if (trimmedText.startsWith("fade:") ||
    trimmedText.match(/^c\d+$/) ||
    trimmedText.match(/^p\d+$/) || // Pattern codes like p0, p1, etc.
    cssColors[trimmedText] ||
    trimmedText === "rainbow" ||
    trimmedText === "zebra") {
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
    if (VERBOSE) console.log("🔍 [KidLisp Detection] Comma-separated text:", text);
    // Check if the text contains known KidLisp functions or color names
    const kidlispFunctions = [
      "wipe", "ink", "line", "box", "flood", "circle", "write", "paste", "stamp", "point", "poly",
      "print", "debug", "random", "sin", "cos", "tan", "floor", "ceil", "round",
      "noise", "choose", "overtone", "rainbow", "zebra", "mic", "amplitude",
      "melody", "speaker", "resolution", "lines", "wiggle", "shape", "scroll",
      "flip", "spin", "resetSpin", "smoothspin", "sort", "zoom", "blur", "contrast", "pan", "unpan",
      "mask", "unmask", "steal", "putback", "label", "len", "now", "die",
      "tap", "draw", "not", "range", "mul", "log", "no", "yes", "fade", "repeat", "jump"
    ];

    // Split by commas and check if any part contains KidLisp functions or colors
    const parts = text.split(",").map(part => part.trim()).filter(Boolean);
    if (VERBOSE) console.log("🔍 [KidLisp Detection] Parts:", parts);

    const analyzePart = (part) => {
      // Remove wrapping braces/parentheses commonly used in share metadata
      const normalized = part.replace(/^[{\[(]+/, "").replace(/[}\])]+$/, "").trim();
      if (!normalized) return { isFunction: false, isColor: false, isNumber: false, isRGB: false };

      const firstTokenMatch = normalized.match(/^[^\s]+/);
      const firstToken = firstTokenMatch ? firstTokenMatch[0].toLowerCase() : "";
      const firstTokenStripped = firstToken.replace(/^[^a-z0-9]+/i, "");
      const cleanedToken = firstTokenStripped.replace(/[^a-z0-9:._-]+$/i, ""); // Preserve decimal points

      if (VERBOSE) console.log(`🔍 [KidLisp Detection] Analyzing part: "${part}" -> normalized: "${normalized}" -> cleanedToken: "${cleanedToken}"`);

      const isFunction = kidlispFunctions.includes(cleanedToken) ||
        (cleanedToken.startsWith("(") && kidlispFunctions.includes(cleanedToken.slice(1)));
      const isColor = !!(cssColors && cssColors[cleanedToken]) ||
        /^c\d+$/.test(cleanedToken) ||
        cleanedToken.startsWith("fade:");
      const isNumber = /^-?\d+(\.\d+)?$/.test(cleanedToken);
      const isRGB = isValidRGBString(cleanedToken.replace(/_/g, " "));
      
      // Check for #painting codes (not hex colors)
      const isPaintingCode = normalized.startsWith("#") && /^#[0-9A-Za-z]{1,8}$/.test(normalized) && (() => {
        const codepart = normalized.substring(1);
        // Check if it's NOT a pure hex color (3, 4, 6, or 8 hex digits)
        const isHexColor = /^[0-9A-Fa-f]{3}$|^[0-9A-Fa-f]{4}$|^[0-9A-Fa-f]{6}$|^[0-9A-Fa-f]{8}$/.test(codepart);
        return !isHexColor;
      })();

      if (VERBOSE) console.log(`🔍 [KidLisp Detection] isFunction: ${isFunction}, isColor: ${isColor}, isNumber: ${isNumber}, isRGB: ${isRGB}, isPaintingCode: ${isPaintingCode}`);

      return { isFunction, isColor, isNumber, isRGB, isPaintingCode };
    };

    const analyses = parts.map(analyzePart);
    const kidParts = analyses.filter(({ isFunction, isColor, isNumber, isRGB, isPaintingCode }) => isFunction || isColor || isNumber || isRGB || isPaintingCode);
    const functionParts = analyses.filter(({ isFunction }) => isFunction);
    
    if (VERBOSE) console.log(`🔍 [KidLisp Detection] kidParts: ${kidParts.length}, functionParts: ${functionParts.length}, total parts: ${parts.length}`);

    // Require at least one recognized KidLisp function or multiple KidLisp-like tokens
    const result = kidParts.length > 0 && (functionParts.length > 0 || kidParts.length >= 2 || (kidParts.length / parts.length) >= 0.75);
    if (VERBOSE) console.log(`🔍 [KidLisp Detection] Final result: ${result}`);
    
    if (result) {
      return true;
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
    .replace(/;/g, "¨") // Use ¨ for semicolon - displays fine in browsers
    .replace(/#/g, "%23"); // Use %23 for hash - prevents URL fragment interpretation

  return encoded;
}

function decodeKidlispFromUrl(encoded) {
  // Standard decoding for content
  let decoded = encoded
    .replace(/_/g, " ")
    .replace(/%C2%A7/g, "\n") // UTF-8 encoded § to newline
    .replace(/%C2%A4/g, "%") // UTF-8 encoded ¤ to percent
    .replace(/%C2%A8/g, ";") // UTF-8 encoded ¨ to semicolon
    .replace(/%23/g, "#") // URL-encoded hash to #
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

export {
  getSourceHash,
  getCachedCode,
  setCachedCode,
  tokenize,
  isValidRGBComponent,
  isValidRGBString,
  isKidlispSource,
  encodeKidlispForUrl,
  decodeKidlispFromUrl,
};
