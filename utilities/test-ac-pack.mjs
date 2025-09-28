#!/usr/bin/env node

// Test script for ac-pack utility
// Tests packing a simple piece and verifying the output

import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { AcPacker } from "./ac-pack.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function runTests() {
  console.log("🧪 Running ac-pack tests...\n");

  // Test 1: Pack a simple JavaScript piece
  try {
    console.log("📋 Test 1: Packing 'brush' piece...");
    const packer = new AcPacker("brush", {
      title: "Test Brush",
      description: "Test packing of brush piece"
    });
    
    await packer.pack();
    
    // Verify the output
    const outputDir = path.join(__dirname, "..", "tokens", "brush");
    const zipPath = `${outputDir}.zip`;
    
    // Check if files exist
    const indexExists = await fs.access(path.join(outputDir, "index.html")).then(() => true).catch(() => false);
    const zipExists = await fs.access(zipPath).then(() => true).catch(() => false);
    const bootExists = await fs.access(path.join(outputDir, "aesthetic.computer", "boot.mjs")).then(() => true).catch(() => false);
    
    console.log(`  ✅ index.html: ${indexExists ? 'exists' : 'missing'}`);
    console.log(`  ✅ ZIP file: ${zipExists ? 'exists' : 'missing'}`);
    console.log(`  ✅ boot.mjs: ${bootExists ? 'exists' : 'missing'}`);
    
    if (indexExists && zipExists && bootExists) {
      console.log("  🎉 Test 1 PASSED\n");
    } else {
      console.log("  ❌ Test 1 FAILED\n");
      return false;
    }
    
  } catch (error) {
    console.log(`  ❌ Test 1 FAILED: ${error.message}\n`);
    return false;
  }

  // Test 2: Pack a Lisp piece
  try {
    console.log("📋 Test 2: Packing 'brush' Lisp piece...");
    const packer = new AcPacker("brush", {
      title: "Test Lisp Brush",
      description: "Test packing of Lisp brush piece"
    });
    
    await packer.pack();
    
    console.log("  🎉 Test 2 PASSED\n");
    
  } catch (error) {
    console.log(`  ❌ Test 2 FAILED: ${error.message}\n`);
    return false;
  }

  // Test 3: Verify index.html content
  try {
    console.log("📋 Test 3: Verifying HTML content...");
    const outputDir = path.join(__dirname, "..", "tokens", "brush");
    const indexPath = path.join(outputDir, "index.html");
    const indexContent = await fs.readFile(indexPath, "utf8");
    
    const hasTitle = indexContent.includes("<title>");
    const hasOgImage = indexContent.includes('property="og:image"');
    const hasBootScript = indexContent.includes("boot.mjs");
    const hasTeiaMode = indexContent.includes("acTEIA_MODE");
    
    console.log(`  ✅ Has title tag: ${hasTitle}`);
    console.log(`  ✅ Has og:image meta: ${hasOgImage}`);
    console.log(`  ✅ Has boot script: ${hasBootScript}`);
    console.log(`  ✅ Has Teia mode: ${hasTeiaMode}`);
    
    if (hasTitle && hasOgImage && hasBootScript && hasTeiaMode) {
      console.log("  🎉 Test 3 PASSED\n");
    } else {
      console.log("  ❌ Test 3 FAILED\n");
      return false;
    }
    
  } catch (error) {
    console.log(`  ❌ Test 3 FAILED: ${error.message}\n`);
    return false;
  }

  console.log("🎉 All tests passed! ac-pack is working correctly.\n");
  
  console.log("📋 Next steps:");
  console.log("1. Install archiver: cd utilities && npm install");
  console.log("2. Test with a real piece: node utilities/ac-pack.mjs paint");
  console.log("3. Upload the generated zip to teia.art/mint");
  
  return true;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  runTests().catch(console.error);
}
