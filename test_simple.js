// Simple test
const text = "prutti~3";

// Simulate the isKidlispSource logic
function testIsKidlispSource(text) {
  if (!text) return false;

  // Traditional kidlisp indicators - must start with ( or ;
  if (text.startsWith("(") || text.startsWith(";")) {
    return true;
  }

  // Check for encoded kidlisp (contains § suggesting newlines were encoded)
  if (text.includes("§")) {
    return true; // simplified for test
  }

  if (text.includes("_") && text.match(/[a-zA-Z_]\w*_[a-zA-Z]/)) {
    return true; // simplified for test
  }

  // Check if it contains newlines and looks like kidlisp (has function calls)
  if (text.includes("\n")) {
    return true; // simplified for test
  }

  // For simple text without § or newlines, only consider it kidlisp if it starts with ( or ;
  return false;
}

console.log("Testing:", JSON.stringify(text));
console.log("isKidlispSource result:", testIsKidlispSource(text));
console.log("text.includes('~'):", text.includes("~"));
console.log("Overall condition:", testIsKidlispSource(text) && text.includes("~"));
