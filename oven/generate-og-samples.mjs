#!/usr/bin/env node
/**
 * KidLisp.com OG Image Generator
 * 
 * Generates Open Graph preview images for KidLisp.com social sharing.
 * Uses top trending pieces from the TV API, deduplicates by source similarity,
 * and creates various layout options.
 * 
 * Usage:
 *   node generate-og-samples.mjs              # Generate all layouts to /scratch
 *   node generate-og-samples.mjs mosaic       # Generate specific layout
 *   node generate-og-samples.mjs --production # Upload to CDN (skip local save)
 * 
 * Layouts:
 *   - featured:   Single top piece with branding overlay
 *   - mosaic:     4x4 grid of top 16 unique pieces with KidLisp.com text
 *   - filmstrip:  5 frames from the same piece (animation sequence)
 *   - code-split: Code preview + visual split screen
 * 
 * Source Deduplication:
 *   Pieces with >90% similar source code (by trigram Jaccard) are filtered out
 *   to ensure visual variety in the mosaic.
 * 
 * Automation:
 *   This script is designed to run daily via cron on the oven backend.
 *   Images are uploaded to DigitalOcean Spaces CDN with 24hr cache.
 */

import 'dotenv/config';
import { promises as fs } from 'fs';
import { join } from 'path';

// Import the OG generation functions
import { generateKidlispOGImage, closeAll } from './grabber.mjs';

const SCRATCH_DIR = '/workspaces/aesthetic-computer/scratch';
const LAYOUTS = ['featured', 'mosaic', 'filmstrip', 'code-split'];

async function main() {
  const args = process.argv.slice(2);
  const productionMode = args.includes('--production');
  const specificLayout = args.find(a => LAYOUTS.includes(a));
  const layoutsToGenerate = specificLayout ? [specificLayout] : LAYOUTS;
  
  // Parse --handle=@jeffrey or --handle jeffrey
  let handle = null;
  const handleArg = args.find(a => a.startsWith('--handle'));
  if (handleArg) {
    if (handleArg.includes('=')) {
      handle = handleArg.split('=')[1];
    } else {
      const idx = args.indexOf(handleArg);
      if (idx < args.length - 1) {
        handle = args[idx + 1];
      }
    }
  }
  
  console.log('🖼️  KidLisp OG Image Generator');
  console.log('================================');
  console.log(`📋 Layouts: ${layoutsToGenerate.join(', ')}`);
  console.log(`📦 Mode: ${productionMode ? 'Production (CDN upload)' : 'Development (local save)'}`);
  if (handle) console.log(`👤 Handle filter: ${handle}`);
  console.log('');
  
  // Ensure scratch directory exists (for dev mode)
  if (!productionMode) {
    await fs.mkdir(SCRATCH_DIR, { recursive: true });
  }
  
  const results = [];
  
  for (const layout of layoutsToGenerate) {
    console.log(`\n📸 Generating ${layout} layout...`);
    
    try {
      const result = await generateKidlispOGImage(layout, true, { handle }); // force=true to skip cache
      
      if (result.buffer) {
        // Save locally in dev mode
        if (!productionMode) {
          const filename = `kidlisp-og-${layout}.png`;
          const filepath = join(SCRATCH_DIR, filename);
          await fs.writeFile(filepath, result.buffer);
          console.log(`   💾 Saved: ${filepath}`);
        }
        
        console.log(`   📊 Size: ${(result.buffer.length / 1024).toFixed(1)} KB`);
        console.log(`   🌐 CDN: ${result.url}`);
        
        if (result.featuredPiece) {
          console.log(`   🎯 Featured: $${result.featuredPiece.code} (${result.featuredPiece.hits} hits)`);
        }
        
        results.push({ layout, success: true, url: result.url, size: result.buffer.length });
      }
      
    } catch (error) {
      console.error(`   ❌ Failed: ${error.message}`);
      results.push({ layout, success: false, error: error.message });
    }
  }
  
  // Cleanup all connections
  console.log('\n🧹 Cleaning up...');
  await closeAll();
  
  // Summary
  console.log('\n✨ Generation Complete!');
  console.log('========================');
  
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);
  
  if (successful.length > 0) {
    console.log(`\n✅ Successful (${successful.length}):`);
    for (const r of successful) {
      console.log(`   ${r.layout}: ${(r.size / 1024).toFixed(1)} KB → ${r.url}`);
    }
  }
  
  if (failed.length > 0) {
    console.log(`\n❌ Failed (${failed.length}):`);
    for (const r of failed) {
      console.log(`   ${r.layout}: ${r.error}`);
    }
  }
  
  // List local files in dev mode
  if (!productionMode) {
    console.log('\n📂 Local files in /scratch:');
    try {
      const files = await fs.readdir(SCRATCH_DIR);
      const ogFiles = files.filter(f => f.startsWith('kidlisp-og-'));
      for (const f of ogFiles) {
        const stat = await fs.stat(join(SCRATCH_DIR, f));
        console.log(`   ${f} (${(stat.size / 1024).toFixed(1)} KB)`);
      }
    } catch (e) {
      // Ignore readdir errors
    }
  }
  
  process.exit(failed.length > 0 ? 1 : 0);
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
