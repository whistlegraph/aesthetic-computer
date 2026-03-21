// kidlisp-analyzer.mjs - KidLisp source analysis for NFT metadata
// Keeps it simple: character count is the only trait.

export function analyzeKidLisp(source, options = {}) {
  if (!source || typeof source !== "string") {
    return { error: "Invalid source", chars: 0, lines: 0, traits: [] };
  }

  const chars = source.length;
  const lines = source.split("\n").filter(l => l.trim() && !l.trim().startsWith(";")).length;

  return {
    chars,
    lines,
    traits: [
      { name: "Length", value: String(chars) },
    ],
  };
}

// Export version for tracking
export const ANALYZER_VERSION = "3.0.0";
