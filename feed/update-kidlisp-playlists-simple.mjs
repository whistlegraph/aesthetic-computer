#!/usr/bin/env node

/**
 * Simple script to regenerate KidLisp playlists with provenance blocks
 * Just runs the three playlist generation scripts in sequence
 * 
 * Usage: FEED_API_SECRET=your_secret node update-kidlisp-playlists-simple.mjs
 */

import { spawn } from 'child_process';

const API_SECRET = process.env.FEED_API_SECRET;

if (!API_SECRET || API_SECRET === 'YOUR_FEED_API_SECRET_HERE') {
  console.error('❌ Error: FEED_API_SECRET environment variable must be set');
  console.error('Usage: FEED_API_SECRET=your_secret node update-kidlisp-playlists-simple.mjs');
  process.exit(1);
}

const scripts = [
  { name: 'Top 100 KidLisp', path: './feed/create-top-kidlisp-playlist.mjs' },
  { name: 'Colors', path: './feed/create-kidlisp-colors-playlist.mjs' },
  { name: 'Chords', path: './feed/create-kidlisp-chords-playlist.mjs' }
];

async function runScript(scriptInfo) {
  return new Promise((resolve, reject) => {
    console.log(`\n🎨 Running ${scriptInfo.name}...`);
    console.log(`   Script: ${scriptInfo.path}`);
    
    const child = spawn('node', [scriptInfo.path], {
      env: { ...process.env, FEED_API_SECRET: API_SECRET },
      stdio: 'inherit',
      cwd: '/workspaces/aesthetic-computer'
    });

    child.on('close', (code) => {
      if (code === 0) {
        console.log(`✅ ${scriptInfo.name} completed successfully\n`);
        resolve({ success: true, name: scriptInfo.name });
      } else {
        console.error(`❌ ${scriptInfo.name} failed with code ${code}\n`);
        resolve({ success: false, name: scriptInfo.name, code });
      }
    });

    child.on('error', (err) => {
      console.error(`❌ Failed to run ${scriptInfo.name}:`, err.message);
      reject(err);
    });
  });
}

async function main() {
  console.log('🔄 Regenerating KidLisp Playlists with Provenance Blocks\n');
  console.log('This will create/update all KidLisp playlists with proper provenance blocks.\n');

  const results = [];
  
  for (const script of scripts) {
    const result = await runScript(script);
    results.push(result);
  }

  console.log('\n📊 Summary:');
  const succeeded = results.filter(r => r.success).length;
  const failed = results.filter(r => !r.success).length;
  
  console.log(`✅ Succeeded: ${succeeded}`);
  console.log(`❌ Failed: ${failed}`);
  
  if (failed > 0) {
    console.log('\nFailed scripts:');
    results.filter(r => !r.success).forEach(r => {
      console.log(`  - ${r.name}`);
    });
    process.exit(1);
  }
  
  console.log('\n🎉 All playlists regenerated successfully!');
  console.log('All playlists now include provenance blocks according to DP-1 spec.');
}

main().catch(error => {
  console.error('❌ Fatal error:', error);
  process.exit(1);
});
