#!/usr/bin/env node
/**
 * 🤖 ac-tezbot - Tezos Operations Daemon
 * 
 * A simple background daemon that handles Tezos RPC calls
 * without blocking the main TUI. Uses file-based IPC.
 * 
 * Communication:
 *   Command:  /tmp/tezbot-cmd.json   (write command here)
 *   Response: /tmp/tezbot-out.json   (read result here)
 *   Status:   /tmp/tezbot.pid        (daemon PID)
 * 
 * Usage:
 *   node ac-tezbot.mjs              - Start daemon
 *   node ac-tezbot.mjs stop         - Stop daemon
 *   node ac-tezbot.mjs status       - Check if running
 *   node ac-tezbot.mjs cmd balance  - Send command directly
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { spawn, execSync } from 'child_process';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// File paths for IPC
const PATHS = {
  pid: '/tmp/tezbot.pid',
  cmd: '/tmp/tezbot-cmd.json',
  out: '/tmp/tezbot-out.json',
  log: '/tmp/tezbot.log',
  keepsScript: path.join(__dirname, 'keeps.mjs')
};

// ============================================================================
// Daemon Mode
// ============================================================================

async function runDaemon() {
  console.log('🤖 ac-tezbot starting...');
  
  // Write PID file
  fs.writeFileSync(PATHS.pid, process.pid.toString());
  
  // Clean up old files
  if (fs.existsSync(PATHS.cmd)) fs.unlinkSync(PATHS.cmd);
  if (fs.existsSync(PATHS.out)) fs.unlinkSync(PATHS.out);
  
  // Write initial status
  writeOutput({ status: 'ready', message: 'Tezbot ready', timestamp: Date.now() });
  
  console.log(`📁 Watching: ${PATHS.cmd}`);
  console.log(`📤 Output:   ${PATHS.out}`);
  console.log(`🆔 PID:      ${process.pid}`);
  
  // Watch for command file changes
  let processing = false;
  let lastMtime = 0;
  
  // Poll for commands (more reliable than fs.watch in containers)
  setInterval(async () => {
    if (processing) return;
    
    try {
      if (!fs.existsSync(PATHS.cmd)) return;
      
      const stat = fs.statSync(PATHS.cmd);
      if (stat.mtimeMs <= lastMtime) return;
      
      lastMtime = stat.mtimeMs;
      processing = true;
      
      const cmdData = JSON.parse(fs.readFileSync(PATHS.cmd, 'utf8'));
      console.log(`\n📥 Command: ${cmdData.action}`);
      
      await processCommand(cmdData);
      
      // Remove command file after processing
      fs.unlinkSync(PATHS.cmd);
      processing = false;
      
    } catch (err) {
      console.error('❌ Error:', err.message);
      writeOutput({ status: 'error', error: err.message, timestamp: Date.now() });
      processing = false;
    }
  }, 200); // Poll every 200ms
  
  // Handle shutdown
  process.on('SIGTERM', cleanup);
  process.on('SIGINT', cleanup);
  
  console.log('✅ Tezbot running. Ctrl+C to stop.\n');
}

function cleanup() {
  console.log('\n🛑 Tezbot stopping...');
  if (fs.existsSync(PATHS.pid)) fs.unlinkSync(PATHS.pid);
  process.exit(0);
}

// ============================================================================
// Command Processing
// ============================================================================

async function processCommand(cmd) {
  const { action, args = [], network = 'ghostnet', timeout = 30000 } = cmd;
  
  writeOutput({ 
    status: 'running', 
    action, 
    message: `Executing: ${action}...`,
    timestamp: Date.now() 
  });
  
  return new Promise((resolve) => {
    const fullArgs = [PATHS.keepsScript, action, ...args];
    let output = '';
    let resolved = false;
    
    const child = spawn('node', fullArgs, {
      cwd: path.dirname(PATHS.keepsScript),
      env: { ...process.env, TEZBOT_NETWORK: network },
      stdio: ['ignore', 'pipe', 'pipe']
    });
    
    child.stdout.on('data', (data) => {
      output += data.toString();
    });
    
    child.stderr.on('data', (data) => {
      output += data.toString();
    });
    
    child.on('close', (code) => {
      if (resolved) return;
      resolved = true;
      
      const result = {
        status: code === 0 ? 'success' : 'error',
        action,
        code,
        output: output.trim(),
        timestamp: Date.now()
      };
      
      // Parse structured data from output if possible
      if (action === 'balance' && code === 0) {
        const match = output.match(/Balance:\s*([\d.]+)\s*XTZ/);
        if (match) result.balance = parseFloat(match[1]);
      }
      
      writeOutput(result);
      console.log(`✅ ${action} completed (code: ${code})`);
      resolve(result);
    });
    
    child.on('error', (err) => {
      if (resolved) return;
      resolved = true;
      
      const result = {
        status: 'error',
        action,
        error: err.message,
        timestamp: Date.now()
      };
      
      writeOutput(result);
      console.log(`❌ ${action} failed: ${err.message}`);
      resolve(result);
    });
    
    // Timeout
    setTimeout(() => {
      if (!resolved) {
        resolved = true;
        try { child.kill('SIGKILL'); } catch (e) {}
        
        const result = {
          status: 'timeout',
          action,
          error: `Operation timed out after ${timeout/1000}s`,
          output: output.trim(),
          timestamp: Date.now()
        };
        
        writeOutput(result);
        console.log(`⏰ ${action} timed out`);
        resolve(result);
      }
    }, timeout);
  });
}

function writeOutput(data) {
  fs.writeFileSync(PATHS.out, JSON.stringify(data, null, 2));
}

// ============================================================================
// Client Commands
// ============================================================================

function sendCommand(action, args = []) {
  const cmd = {
    action,
    args,
    timestamp: Date.now()
  };
  
  fs.writeFileSync(PATHS.cmd, JSON.stringify(cmd, null, 2));
  console.log(`📤 Sent: ${action}`);
  
  // Wait for response
  const startTime = Date.now();
  const timeout = 35000;
  
  process.stdout.write('⏳ Waiting');
  
  const poll = setInterval(() => {
    process.stdout.write('.');
    
    if (Date.now() - startTime > timeout) {
      clearInterval(poll);
      console.log('\n❌ Timeout waiting for response');
      process.exit(1);
    }
    
    if (!fs.existsSync(PATHS.out)) return;
    
    try {
      const result = JSON.parse(fs.readFileSync(PATHS.out, 'utf8'));
      // Wait for final status (not 'running' or 'ready')
      const finalStatuses = ['success', 'error', 'timeout'];
      if (result.timestamp > cmd.timestamp && result.action === action && finalStatuses.includes(result.status)) {
        clearInterval(poll);
        console.log('\n');
        
        if (result.output) {
          console.log(result.output);
        } else {
          console.log(JSON.stringify(result, null, 2));
        }
        
        process.exit(result.status === 'success' ? 0 : 1);
      }
    } catch (e) {}
  }, 300);
}

function checkStatus() {
  if (!fs.existsSync(PATHS.pid)) {
    console.log('🔴 Tezbot not running');
    return false;
  }
  
  const pid = fs.readFileSync(PATHS.pid, 'utf8').trim();
  
  try {
    process.kill(parseInt(pid), 0); // Check if process exists
    console.log(`🟢 Tezbot running (PID: ${pid})`);
    
    if (fs.existsSync(PATHS.out)) {
      const lastOutput = JSON.parse(fs.readFileSync(PATHS.out, 'utf8'));
      const age = Math.round((Date.now() - lastOutput.timestamp) / 1000);
      console.log(`   Last activity: ${age}s ago (${lastOutput.status})`);
    }
    
    return true;
  } catch (e) {
    console.log('🔴 Tezbot not running (stale PID file)');
    fs.unlinkSync(PATHS.pid);
    return false;
  }
}

function stopDaemon() {
  if (!fs.existsSync(PATHS.pid)) {
    console.log('🔴 Tezbot not running');
    return;
  }
  
  const pid = fs.readFileSync(PATHS.pid, 'utf8').trim();
  
  try {
    process.kill(parseInt(pid), 'SIGTERM');
    console.log(`🛑 Sent SIGTERM to PID ${pid}`);
    
    // Wait a moment and clean up
    setTimeout(() => {
      if (fs.existsSync(PATHS.pid)) fs.unlinkSync(PATHS.pid);
      console.log('✅ Tezbot stopped');
    }, 500);
  } catch (e) {
    console.log('🔴 Could not stop tezbot:', e.message);
    if (fs.existsSync(PATHS.pid)) fs.unlinkSync(PATHS.pid);
  }
}

// ============================================================================
// Main
// ============================================================================

const args = process.argv.slice(2);

switch (args[0]) {
  case 'stop':
    stopDaemon();
    break;
    
  case 'status':
    checkStatus();
    break;
    
  case 'cmd':
    if (!args[1]) {
      console.log('Usage: node ac-tezbot.mjs cmd <action> [args...]');
      console.log('Actions: balance, status, tokens, mint, upload, lock');
      process.exit(1);
    }
    if (!checkStatus()) {
      console.log('⚠️  Start tezbot first: node ac-tezbot.mjs');
      process.exit(1);
    }
    sendCommand(args[1], args.slice(2));
    break;
    
  case undefined:
  case 'start':
    if (checkStatus()) {
      console.log('⚠️  Already running. Use "stop" first to restart.');
      process.exit(1);
    }
    runDaemon();
    break;
    
  default:
    console.log(`
🤖 ac-tezbot - Tezos Operations Daemon

Usage:
  node ac-tezbot.mjs              Start daemon
  node ac-tezbot.mjs stop         Stop daemon  
  node ac-tezbot.mjs status       Check status
  node ac-tezbot.mjs cmd <action> Send command

Actions:
  balance   Check wallet balance
  status    Contract status
  tokens    List tokens
  mint      Mint token (needs piece name)
  upload    Upload to IPFS
  lock      Lock token metadata
`);
}
