// Test to verify stample.mjs can be loaded without syntax errors
// This test specifically validates that there are no duplicate const declarations

import { strict as assert } from "assert";

async function testStampleLoads() {
  try {
    // Attempt to dynamically import the module
    // This will throw a SyntaxError if there are duplicate const declarations
    const stamplePath = new URL(
      "../system/public/aesthetic.computer/disks/stample.mjs",
      import.meta.url
    ).pathname;
    
    await import(stamplePath);
    
    console.log("✓ stample.mjs loads successfully without syntax errors");
    return true;
  } catch (error) {
    console.error("✗ Failed to load stample.mjs:", error.message);
    throw error;
  }
}

// Run the test
testStampleLoads()
  .then(() => {
    console.log("\nAll tests passed!");
    process.exit(0);
  })
  .catch((error) => {
    console.error("\nTest failed!");
    process.exit(1);
  });
