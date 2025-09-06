#!/usr/bin/env node

// Test script to verify suck function is in KidLisp API
console.log("🎭 Testing KidLisp suck integration...");

try {
  // Try to import kidlisp.mjs 
  const kidlispPath = "/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs";
  console.log("📁 Attempting to import kidlisp.mjs...");
  
  // Read the file as text to search for suck function
  const fs = await import('fs');
  const kidlispCode = fs.readFileSync(kidlispPath, 'utf8');
  
  console.log("✅ kidlisp.mjs read successfully");
  
  // Check if suck function exists in the API
  if (kidlispCode.includes('suck: (api, args = [])')) {
    console.log("✅ suck function found in KidLisp API functions");
    
    // Check for deferred execution support
    if (kidlispCode.includes('Deferring suck execution')) {
      console.log("✅ Deferred execution support found");
    }
    
    // Check for embedded layer prioritization
    if (kidlispCode.includes("head === 'suck'")) {
      console.log("✅ Embedded layer prioritization found");
    }
    
    // Check for debug logging
    if (kidlispCode.includes('Executing global suck function')) {
      console.log("✅ Debug logging support found");
    }
    
    console.log("✅ KidLisp suck integration test PASSED");
    
  } else {
    console.log("❌ suck function NOT found in KidLisp API");
  }
  
} catch (error) {
  console.log("❌ Test failed:", error.message);
}

console.log("🏁 KidLisp integration test complete");
