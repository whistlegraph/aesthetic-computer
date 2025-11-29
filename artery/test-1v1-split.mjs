#!/usr/bin/env node
/**
 * 🎮 1v1 Split Test
 * 
 * Test the split view for local 1v1 dueling scenarios.
 * Opens split with two pieces side by side for testing multiplayer interactions.
 * 
 * Usage: node test-1v1-split.mjs [piece] [piece2]
 * 
 * Examples:
 *   node test-1v1-split.mjs prompt          - Two prompts side by side
 *   node test-1v1-split.mjs notepat         - Two notepat instances  
 *   node test-1v1-split.mjs prompt notepat  - Prompt on top, notepat on bottom
 */

import Artery from './artery.mjs';

// ═══════════════════════════════════════════════════════════════════════════
// TERMINAL COLORS
// ═══════════════════════════════════════════════════════════════════════════
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';
const RED = '\x1b[91m';
const DIM = '\x1b[2m';
const BOLD = '\x1b[1m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}🎮${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const infoLog = (msg) => console.log(`${CYAN}ℹ️  ${msg}${RESET}`);
const warnLog = (msg) => console.log(`${YELLOW}⚠️  ${msg}${RESET}`);

// ═══════════════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════════════

export async function main() {
  const args = process.argv.slice(2);
  
  // Parse pieces from args
  let piece1 = 'prompt';
  let piece2 = null;
  
  for (const arg of args) {
    if (arg === 'help' || arg === '--help' || arg === '-h') {
      showHelp();
      return;
    }
    if (!piece1 || piece1 === 'prompt') {
      piece1 = arg;
    } else if (!piece2) {
      piece2 = arg;
    }
  }
  
  // If only one piece specified, use it for both
  if (!piece2) {
    piece2 = piece1;
  }
  
  console.log(`\n${BOLD}${MAGENTA}🎮 1v1 Split Test${RESET}`);
  console.log(`${DIM}Testing split view for local multiplayer${RESET}\n`);
  
  infoLog(`Top panel: ${BOLD}${piece1}${RESET}`);
  infoLog(`Bottom panel: ${BOLD}${piece2}${RESET}\n`);
  
  testLog('Connecting to AC...\n');
  
  const client = new Artery();
  
  try {
    await client.connect();
    testLog('Connected to AC');
    
    // Build the split URL
    const splitPath = piece1 === piece2 
      ? `split~${piece1}` 
      : `split~${piece1}~${piece2}`;
    
    testLog(`Navigating to: ${splitPath}`);
    
    // Navigate to split view
    await client.send('piece:load', { path: splitPath });
    
    // Wait for navigation
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Reconnect after navigation
    await client.connect();
    testLog('Reconnected after navigation\n');
    
    successLog('Split view loaded!\n');
    
    console.log(`${DIM}═══════════════════════════════════════════════════${RESET}`);
    console.log(`${BOLD}Split View Active${RESET}`);
    console.log(`${DIM}───────────────────────────────────────────────────${RESET}`);
    console.log(`  Top:    ${CYAN}${piece1}${RESET}`);
    console.log(`  Bottom: ${CYAN}${piece2}${RESET}`);
    console.log(`${DIM}───────────────────────────────────────────────────${RESET}`);
    console.log(`\n${DIM}Press Ctrl+C to exit${RESET}\n`);
    
    // Keep connection alive for monitoring
    await new Promise((resolve) => {
      process.on('SIGINT', () => {
        console.log(`\n${YELLOW}Exiting...${RESET}`);
        resolve();
      });
    });
    
  } catch (error) {
    console.error(`${RED}Error: ${error.message}${RESET}`);
    process.exit(1);
  } finally {
    client.disconnect();
    testLog('Disconnected');
  }
}

function showHelp() {
  console.log(`
${BOLD}${MAGENTA}🎮 1v1 Split Test${RESET}
${DIM}Test split view for local 1v1 dueling${RESET}

${BOLD}Usage:${RESET}
  node test-1v1-split.mjs [piece1] [piece2]

${BOLD}Arguments:${RESET}
  piece1    First piece (top panel), default: prompt
  piece2    Second piece (bottom panel), default: same as piece1

${BOLD}Examples:${RESET}
  ${DIM}# Two prompts side by side${RESET}
  node test-1v1-split.mjs prompt

  ${DIM}# Two notepat instances for music dueling${RESET}
  node test-1v1-split.mjs notepat

  ${DIM}# Different pieces - prompt on top, notepat on bottom${RESET}
  node test-1v1-split.mjs prompt notepat

  ${DIM}# Two drawing canvases${RESET}
  node test-1v1-split.mjs line

${BOLD}Notes:${RESET}
  - The split view creates two iframes, each running a piece
  - Each panel operates independently
  - Good for testing multiplayer/dueling scenarios locally
  - Use 'split~piece1~piece2' URL format directly in browser
`);
}

// Run if called directly
main().catch(console.error);
