#!/usr/bin/env node

// Research different compression techniques for the bundle
// Compare: gzip, brotli, different compression levels, base64 alternatives

import { promises as fs } from "fs";
import { gzipSync, brotliCompressSync, constants } from "zlib";

const bundlePath = process.argv[2] || "/workspaces/aesthetic-computer/tezos/keep-bundles/wwi-ultra-minimal-nft.html";

async function main() {
  console.log("📊 Compression Technique Comparison\n");
  
  const content = await fs.readFile(bundlePath, "utf8");
  const originalSize = content.length;
  
  console.log(`Original size: ${(originalSize / 1024).toFixed(2)} KB\n`);
  
  // Test gzip at different levels
  console.log("🗜️  GZIP Compression:");
  for (let level = 6; level <= 9; level++) {
    const compressed = gzipSync(content, { level });
    const base64 = compressed.toString("base64");
    const ratio = ((1 - compressed.length / originalSize) * 100).toFixed(1);
    const base64Ratio = ((1 - base64.length / originalSize) * 100).toFixed(1);
    console.log(`  Level ${level}: ${(compressed.length / 1024).toFixed(2)} KB (${ratio}% savings)`);
    console.log(`            + base64: ${(base64.length / 1024).toFixed(2)} KB (${base64Ratio}% savings)`);
  }
  
  // Test brotli at different levels
  console.log("\n💎 Brotli Compression:");
  for (let level = 9; level <= 11; level++) {
    const compressed = brotliCompressSync(content, {
      params: {
        [constants.BROTLI_PARAM_QUALITY]: level,
        [constants.BROTLI_PARAM_SIZE_HINT]: originalSize
      }
    });
    const base64 = compressed.toString("base64");
    const ratio = ((1 - compressed.length / originalSize) * 100).toFixed(1);
    const base64Ratio = ((1 - base64.length / originalSize) * 100).toFixed(1);
    console.log(`  Level ${level}: ${(compressed.length / 1024).toFixed(2)} KB (${ratio}% savings)`);
    console.log(`            + base64: ${(base64.length / 1024).toFixed(2)} KB (${base64Ratio}% savings)`);
  }
  
  // Test without base64 (using array buffer approach)
  console.log("\n🔬 Alternative Delivery Methods:");
  const gzipBest = gzipSync(content, { level: 9 });
  const brotliBest = brotliCompressSync(content, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: 11,
      [constants.BROTLI_PARAM_SIZE_HINT]: originalSize
    }
  });
  
  console.log(`  Gzip level 9 raw bytes: ${(gzipBest.length / 1024).toFixed(2)} KB`);
  console.log(`  Brotli level 11 raw bytes: ${(brotliBest.length / 1024).toFixed(2)} KB`);
  console.log(`  Base64 overhead: ${((gzipBest.toString("base64").length / gzipBest.length - 1) * 100).toFixed(1)}%`);
  
  // Calculate what we could achieve with Brotli
  console.log("\n🎯 Target Analysis:");
  const targetKB = 256;
  const gzipWithBase64 = gzipBest.toString("base64").length / 1024;
  const brotliWithBase64 = brotliBest.toString("base64").length / 1024;
  
  console.log(`  Current (gzip+base64): ${gzipWithBase64.toFixed(2)} KB`);
  console.log(`  With Brotli+base64: ${brotliWithBase64.toFixed(2)} KB`);
  console.log(`  Savings from Brotli: ${(gzipWithBase64 - brotliWithBase64).toFixed(2)} KB`);
  console.log(`  Target: ${targetKB} KB`);
  
  if (brotliWithBase64 <= targetKB) {
    console.log(`\n✅ ✅ ✅ BROTLI FITS IN ${targetKB} KB LIMIT! ✅ ✅ ✅`);
    console.log(`  Headroom: ${(targetKB - brotliWithBase64).toFixed(2)} KB`);
  } else {
    console.log(`\n⚠️  Still need to save ${(brotliWithBase64 - targetKB).toFixed(2)} KB more`);
  }
  
  // Test JSON minification opportunities
  console.log("\n📦 Content Optimization Opportunities:");
  const vfsMatch = content.match(/window\.VFS\s*=\s*({[\s\S]+?});/);
  if (vfsMatch) {
    const vfsSize = vfsMatch[0].length;
    console.log(`  VFS object size: ${(vfsSize / 1024).toFixed(2)} KB (${((vfsSize / originalSize) * 100).toFixed(1)}% of total)`);
    
    // Try re-serializing VFS with minimal JSON
    try {
      const vfsObj = eval('(' + vfsMatch[1] + ')');
      const minimalJSON = JSON.stringify(vfsObj);
      const savings = vfsMatch[0].length - (`window.VFS=${minimalJSON};`.length);
      console.log(`  Potential JSON minification: ${(savings / 1024).toFixed(2)} KB`);
    } catch (e) {
      console.log(`  Could not parse VFS for analysis`);
    }
  }
  
  // Estimate how much smaller content needs to be for brotli to fit
  const neededContentSize = targetKB * 1024 / 0.29; // Brotli achieves ~71% compression
  const contentReduction = originalSize - neededContentSize;
  console.log(`\n📐 Content Reduction Needed:`);
  console.log(`  Current uncompressed: ${(originalSize / 1024).toFixed(2)} KB`);
  console.log(`  Need uncompressed: ${(neededContentSize / 1024).toFixed(2)} KB`);
  console.log(`  Must remove: ${(contentReduction / 1024).toFixed(2)} KB of source`);
}

main().catch(console.error);
