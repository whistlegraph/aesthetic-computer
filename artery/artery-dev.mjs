#!/usr/bin/env node
/**
 * 🩸 Artery Dev Runner - Hot-reloading wrapper for artery-tui.mjs
 * Watches for changes and automatically restarts the TUI.
 */

import { spawn } from 'child_process';
import { watch } from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const ARTERY_TUI_PATH = path.join(__dirname, 'artery-tui.mjs');
const ARTERY_PATH = path.join(__dirname, 'artery.mjs');

let child = null;
let restarting = false;
let debounceTimer = null;

// ANSI codes
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const FG_MAGENTA = '\x1b[35m';
const FG_YELLOW = '\x1b[33m';
const FG_GREEN = '\x1b[32m';
const FG_DIM = '\x1b[2m';

function log(msg) {
  console.log(`${FG_MAGENTA}${BOLD}🩸 Artery Dev:${RESET} ${msg}`);
}

function startArtery() {
  if (child) {
    return;
  }

  child = spawn('node', [ARTERY_TUI_PATH], {
    stdio: 'inherit',
    env: { ...process.env, ARTERY_DEV_MODE: 'true' }
  });

  child.on('exit', (code, signal) => {
    child = null;
    if (restarting) {
      restarting = false;
      // Small delay before restart to let file system settle
      setTimeout(startArtery, 100);
    } else if (signal !== 'SIGTERM' && signal !== 'SIGINT') {
      // Unexpected exit - restart after delay
      log(`${FG_YELLOW}Process exited (${code}), restarting in 1s...${RESET}`);
      setTimeout(startArtery, 1000);
    }
  });

  child.on('error', (err) => {
    log(`${FG_YELLOW}Error: ${err.message}${RESET}`);
    child = null;
  });
}

function restartArtery() {
  if (child) {
    restarting = true;
    log(`${FG_GREEN}File changed, restarting...${RESET}`);
    child.kill('SIGTERM');
  } else {
    startArtery();
  }
}

function debounceRestart() {
  if (debounceTimer) {
    clearTimeout(debounceTimer);
  }
  debounceTimer = setTimeout(restartArtery, 200);
}

// Watch for file changes
function setupWatchers() {
  const filesToWatch = [ARTERY_TUI_PATH, ARTERY_PATH];
  
  log(`${FG_DIM}Watching for changes...${RESET}`);
  log(`${FG_DIM}  - artery-tui.mjs${RESET}`);
  log(`${FG_DIM}  - artery.mjs${RESET}`);
  console.log(''); // Blank line before TUI starts
  
  for (const file of filesToWatch) {
    try {
      watch(file, { persistent: true }, (eventType, filename) => {
        if (eventType === 'change') {
          debounceRestart();
        }
      });
    } catch (err) {
      log(`${FG_YELLOW}Warning: Could not watch ${path.basename(file)}: ${err.message}${RESET}`);
    }
  }
}

// Handle graceful shutdown
process.on('SIGINT', () => {
  if (child) {
    child.kill('SIGTERM');
  }
  process.exit(0);
});

process.on('SIGTERM', () => {
  if (child) {
    child.kill('SIGTERM');
  }
  process.exit(0);
});

// Main
log(`${FG_GREEN}Starting in dev mode with hot reload${RESET}`);
setupWatchers();
startArtery();
