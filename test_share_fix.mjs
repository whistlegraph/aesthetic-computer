#!/usr/bin/env node

// Test the share fix for multi-line kidlisp

// Simulate the functions we need
function isKidlispSource(text) {
  if (!text) return false;
  
  if (text.startsWith("(")) {
    return true;
  }
  
  if (text.includes("\n")) {
    return true;
  }
  
  if (text.includes("§")) {
    const decoded = text.replace(/_/g, " ").replace(/§/g, "\n");
    if (decoded.startsWith("(") || decoded.includes("\n")) {
      return true;
    }
  }
  
  return false;
}

function encodeKidlispForUrl(source) {
  const isKidlisp = isKidlispSource(source);
  
  if (!isKidlisp) {
    return source;
  }
  
  const encoded = source
    .replace(/ /g, "_")
    .replace(/\n/g, "§");
  
  return encoded;
}

// Test cases
console.log("Testing share fix...");

// Case 1: Raw multi-line kidlisp (what disk.mjs would encode)
const rawKidlisp = "line\nwipe blue";
console.log("Raw kidlisp:", rawKidlisp);
console.log("Is kidlisp source?", isKidlispSource(rawKidlisp));
const encoded = encodeKidlispForUrl(rawKidlisp);
console.log("Encoded:", encoded);

// Case 2: Already encoded kidlisp (what share.mjs would receive)
const alreadyEncoded = "line§wipe_blue";
console.log("\nAlready encoded:", alreadyEncoded);
console.log("Is kidlisp source?", isKidlispSource(alreadyEncoded));

// Case 3: Test the share piece logic
function testShareLogic(params) {
  let slug;
  
  // New share piece logic
  if (params[0] && (params[0].includes("§") || params[0].includes("_"))) {
    // Already encoded kidlisp content - use as-is
    slug = params.join("~");
    console.log("  -> Detected as already encoded");
  } else if (isKidlispSource(params[0])) {
    // For raw kidlisp, rejoin with spaces and then use centralized URL encoding
    const kidlispSource = params.join(" ");
    slug = encodeKidlispForUrl(kidlispSource);
    console.log("  -> Detected as raw kidlisp, encoded to:", slug);
  } else {
    // For regular pieces, use the normal tilde joining
    slug = params.join("~");
    console.log("  -> Detected as regular piece");
  }
  
  return slug;
}

console.log("\nTesting share piece logic:");
console.log("Raw multi-line kidlisp (from prompt):", ["line\nwipe blue"]);
const slug1 = testShareLogic(["line\nwipe blue"]);
console.log("Result slug:", slug1);

console.log("\nAlready encoded params:", ["line§wipe_blue"]);
const slug2 = testShareLogic(["line§wipe_blue"]);
console.log("Result slug:", slug2);

console.log("\nRegular piece params:", ["prompt", "red"]);
const slug3 = testShareLogic(["prompt", "red"]);
console.log("Result slug:", slug3);
