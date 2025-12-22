#!/usr/bin/env node
// Batch mint keeps from keep-queue.txt
// Reads the AC auth from vault and mints each code sequentially

import fs from 'fs';
import path from 'path';
import os from 'os';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Load auth from ~/.ac-token (same as artery-tui.mjs)
const authPath = path.join(os.homedir(), '.ac-token');

if (!fs.existsSync(authPath)) {
  console.error('❌ Auth not found at:', authPath);
  console.error('   Run: node artery/artery-tui.mjs and login first (press "a" then "l")');
  process.exit(1);
}

const auth = JSON.parse(fs.readFileSync(authPath, 'utf8'));
console.log(`🔑 Loaded auth for: ${auth.email || auth.user?.email || 'unknown'}`);

// Load queue
const queuePath = path.join(__dirname, 'keep-queue.txt');
const codes = fs.readFileSync(queuePath, 'utf8')
  .split('\n')
  .map(c => c.trim())
  .filter(c => c.length > 0);

console.log(`📋 ${codes.length} codes to mint\n`);

// Disable SSL verification for localhost
process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';

const API_URL = 'https://localhost:8888/api/keep-mint';

async function mintCode(code, index) {
  console.log(`\n[${index + 1}/${codes.length}] 🎨 Minting $${code}...`);
  
  try {
    const response = await fetch(API_URL, {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${auth.access_token}`,
        'Content-Type': 'application/json',
        'Accept': 'text/event-stream',
      },
      body: JSON.stringify({ piece: code, mode: 'mint' }),
    });

    if (!response.ok) {
      const text = await response.text();
      console.log(`   ❌ Error (${response.status}): ${text.slice(0, 200)}`);
      return { code, success: false, error: text };
    }

    // Parse SSE stream
    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let buffer = '';
    let tokenId = null;
    let currentEvent = 'progress';

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      
      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split('\n');
      buffer = lines.pop() || '';
      
      for (const line of lines) {
        if (line.startsWith('event: ')) {
          currentEvent = line.slice(7).trim();
          continue;
        }
        
        if (line.startsWith('data: ')) {
          try {
            const data = JSON.parse(line.slice(6));
            
            if (currentEvent === 'error' || data.error) {
              console.log(`   ⚠️  ${data.error}${data.minter ? ` by ${data.minter}` : ''}`);
              if (data.tokenId) {
                console.log(`   Already kept as token #${data.tokenId}`);
              }
              return { code, success: false, error: data.error, tokenId: data.tokenId };
            }
            
            if (currentEvent === 'progress') {
              process.stdout.write(`   ${data.step || data.message || '...'}\r`);
            }
            
            if (currentEvent === 'complete' && data.success) {
              tokenId = data.tokenId;
              console.log(`   ✅ Kept as token #${tokenId}`);
              console.log(`   ${data.objktUrl || ''}`);
              return { code, success: true, tokenId };
            }
          } catch (e) {
            // Ignore JSON parse errors for partial data
          }
        }
      }
    }
    
    return { code, success: !!tokenId, tokenId };
  } catch (e) {
    console.log(`   ❌ Fetch error: ${e.message}`);
    return { code, success: false, error: e.message };
  }
}

// Main
async function main() {
  const results = { success: 0, failed: 0, skipped: 0 };
  const startTime = Date.now();
  
  for (let i = 0; i < codes.length; i++) {
    const result = await mintCode(codes[i], i);
    if (result.success) {
      results.success++;
    } else if (result.tokenId) {
      results.skipped++; // Already minted
    } else {
      results.failed++;
    }
    
    // Small delay between mints to avoid overwhelming
    if (i < codes.length - 1) {
      await new Promise(r => setTimeout(r, 500));
    }
  }
  
  const elapsed = ((Date.now() - startTime) / 1000 / 60).toFixed(1);
  console.log(`\n${'='.repeat(50)}`);
  console.log(`✅ Complete in ${elapsed} minutes`);
  console.log(`   Success: ${results.success}`);
  console.log(`   Skipped (already minted): ${results.skipped}`);
  console.log(`   Failed: ${results.failed}`);
}

main().catch(console.error);
