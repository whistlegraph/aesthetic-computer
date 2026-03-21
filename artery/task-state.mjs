#!/usr/bin/env node
/**
 * 🎯 Task State Manager - Bridge between Emacs MCP and VS Code status bar
 * 
 * Usage:
 *   node task-state.mjs set working "Processing files..."
 *   node task-state.mjs set done "Completed!"
 *   node task-state.mjs set error "Something went wrong"
 *   node task-state.mjs set idle
 *   node task-state.mjs get
 *   node task-state.mjs clear
 * 
 * The task state file is at /tmp/aesthetic-task-state.json
 * VS Code extension watches this file and updates the status bar accordingly.
 * 
 * Status colors:
 *   - idle: default (gray)
 *   - working: yellow (warning background)
 *   - done: green (prominent background) + flash effect
 *   - error: red (error background)
 */

import { readFileSync, writeFileSync, unlinkSync, existsSync } from 'fs';

const TASK_STATE_FILE = '/tmp/aesthetic-task-state.json';

const validStatuses = ['idle', 'working', 'done', 'error'];

function readState() {
  try {
    if (existsSync(TASK_STATE_FILE)) {
      return JSON.parse(readFileSync(TASK_STATE_FILE, 'utf-8'));
    }
  } catch {}
  return { status: 'idle', label: '', progress: 0 };
}

function writeState(state) {
  state.timestamp = Date.now();
  writeFileSync(TASK_STATE_FILE, JSON.stringify(state, null, 2));
  console.log('✅ Task state updated:', JSON.stringify(state));
}

function printUsage() {
  console.log(`
🎯 Task State Manager

Usage:
  node task-state.mjs set <status> [label] [progress]
  node task-state.mjs get
  node task-state.mjs clear

Status values:
  idle    - Default state (gray)
  working - Task in progress (yellow, spinning icon)
  done    - Task completed (green, flashing)
  error   - Task failed (red)

Examples:
  node task-state.mjs set working "Building project..."
  node task-state.mjs set working "Processing" 50
  node task-state.mjs set done "Build complete!"
  node task-state.mjs set error "Build failed"
  node task-state.mjs set idle
  node task-state.mjs clear
`);
}

const [,, command, ...args] = process.argv;

switch (command) {
  case 'set': {
    const [status, label, progress] = args;
    if (!status || !validStatuses.includes(status)) {
      console.error(`❌ Invalid status. Must be one of: ${validStatuses.join(', ')}`);
      process.exit(1);
    }
    writeState({
      status,
      label: label || status,
      progress: progress ? parseInt(progress, 10) : (status === 'done' ? 100 : 0),
    });
    break;
  }
  
  case 'get': {
    const state = readState();
    console.log(JSON.stringify(state, null, 2));
    break;
  }
  
  case 'clear': {
    try {
      if (existsSync(TASK_STATE_FILE)) {
        unlinkSync(TASK_STATE_FILE);
        console.log('✅ Task state cleared');
      } else {
        console.log('ℹ️  No task state file to clear');
      }
    } catch (err) {
      console.error('❌ Failed to clear task state:', err.message);
    }
    break;
  }
  
  default:
    printUsage();
    break;
}
