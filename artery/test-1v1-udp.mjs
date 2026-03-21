#!/usr/bin/env node
/**
 * 🎮 1v1 UDP Diagnostic Test
 * 
 * Tests UDP/WebRTC connectivity for the 1v1 piece.
 * Checks TURN server, geckos.io channels, and message flow.
 * 
 * Usage: node artery/test-1v1-udp.mjs [--verbose]
 */

import Artery from './artery.mjs';
import http from 'http';
import https from 'https';
import { spawn, spawnSync, execSync } from 'child_process';

// ═══════════════════════════════════════════════════════════════════════════
// TERMINAL COLORS
// ═══════════════════════════════════════════════════════════════════════════
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const MAGENTA = '\x1b[95m';
const RED = '\x1b[91m';
const WHITE = '\x1b[97m';
const PURPLE_BG = '\x1b[45m';
const GREEN_BG = '\x1b[42m';
const RED_BG = '\x1b[41m';
const YELLOW_BG = '\x1b[43m';

const log = (msg) => console.log(`${PURPLE_BG}${WHITE}🎮${RESET} ${msg}`);
const pass = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const fail = (msg) => console.log(`${RED}❌ ${msg}${RESET}`);
const warn = (msg) => console.log(`${YELLOW}⚠️  ${msg}${RESET}`);
const info = (msg) => console.log(`${CYAN}ℹ️  ${msg}${RESET}`);
const debug = (msg) => console.log(`${DIM}   ${msg}${RESET}`);

const section = (title) => {
  console.log(`\n${MAGENTA}${BOLD}═══ ${title} ═══${RESET}\n`);
};

// ═══════════════════════════════════════════════════════════════════════════
// DIAGNOSTIC FUNCTIONS  
// ═══════════════════════════════════════════════════════════════════════════

// Check if a port is listening
function checkPort(port, type = 'tcp') {
  try {
    const protocol = type === 'udp' ? '-u' : '';
    const result = spawnSync('ss', ['-ln', protocol], { encoding: 'utf-8' });
    return result.stdout.includes(`:${port} `);
  } catch {
    return false;
  }
}

// Check if TURN server is running
async function checkTurnServer() {
  log('Checking TURN server...');
  
  // Check if turnserver process is running
  try {
    const result = spawnSync('pgrep', ['-f', 'turnserver'], { encoding: 'utf-8' });
    if (result.status !== 0) {
      fail('TURN server (coturn) is NOT running');
      info('Fix: Start TURN server with:');
      debug('turnserver -c /workspaces/aesthetic-computer/.devcontainer/turnserver.conf');
      return false;
    }
  } catch {
    fail('Could not check TURN server process');
    return false;
  }
  
  // Check if TURN server is listening on port 3478
  const turnListening = checkPort(3478, 'udp');
  if (!turnListening) {
    fail('TURN server not listening on UDP port 3478');
    return false;
  }
  
  pass('TURN server running and listening on UDP:3478');
  return true;
}

// Check if session server is running with UDP ports
async function checkSessionServer() {
  log('Checking session server (geckos.io)...');
  
  // Check for session server process
  try {
    const result = spawnSync('pgrep', ['-f', 'session.mjs'], { encoding: 'utf-8' });
    if (result.status !== 0) {
      fail('Session server is NOT running');
      info('Fix: Session server should be started by artery-tui');
      return { running: false, udpPorts: [] };
    }
  } catch {}
  
  // Check UDP ports 10000-10007 (geckos.io range)
  const udpPorts = [];
  for (let p = 10000; p <= 10007; p++) {
    if (checkPort(p, 'udp')) {
      udpPorts.push(p);
    }
  }
  
  if (udpPorts.length === 0) {
    warn('Session server running but no UDP ports 10000-10007 listening');
    info('Geckos.io may be waiting for first client connection to bind ports');
  } else {
    pass(`Session server UDP ports listening: ${udpPorts.join(', ')}`);
  }
  
  // Check HTTPS port
  const httpsListening = checkPort(8889, 'tcp');
  if (httpsListening) {
    pass('Session server HTTPS listening on port 8889');
  } else {
    fail('Session server HTTPS NOT listening on port 8889');
    return { running: false, udpPorts };
  }
  
  return { running: true, udpPorts };
}

// Check session server logs for UDP activity
async function checkSessionLogs() {
  log('Checking session server logs for UDP activity...');
  
  // Get emacs buffer content for session
  try {
    const result = spawnSync('emacsclient', [
      '-e', '(with-current-buffer "📋-session" (buffer-substring-no-properties (max (- (point-max) 5000) (point-min)) (point-max)))'
    ], { encoding: 'utf-8', timeout: 3000 });
    
    if (result.status === 0) {
      const content = result.stdout;
      
      // Parse for UDP-related messages
      const udpConnections = (content.match(/🩰 UDP .* connected/g) || []).length;
      const udpMoves = (content.match(/🩰 UDP 1v1:move/g) || []).length;
      const udpIdentity = (content.match(/🩰 UDP .* sent identity/g) || []).length;
      
      debug(`UDP connections: ${udpConnections}`);
      debug(`UDP identity messages: ${udpIdentity}`);
      debug(`UDP 1v1:move broadcasts: ${udpMoves}`);
      
      if (udpConnections === 0) {
        warn('No UDP connections found in recent logs');
        info('This could mean clients are not establishing WebRTC connections');
        return { udpActive: false, content };
      }
      
      if (udpMoves > 0) {
        pass(`UDP is active! ${udpMoves} 1v1:move broadcasts found`);
        return { udpActive: true, content };
      }
      
      warn('UDP connections exist but no 1v1:move traffic');
      return { udpActive: false, content };
    }
  } catch (e) {
    debug(`Could not read session logs: ${e.message}`);
  }
  
  return { udpActive: false, content: '' };
}

// Connect to AC and get UDP status from page
async function checkBrowserUDP(client) {
  log('Checking browser UDP status via CDP...');
  
  try {
    // Get UDP variables from 1v1.mjs
    const result = await client.send('Runtime.evaluate', {
      expression: `
        (function() {
          // Check if we have UDP channel reference
          const hasUdpChannel = typeof udpChannel !== 'undefined' && udpChannel !== null;
          const udpConnected = hasUdpChannel ? (udpChannel.connected || false) : false;
          
          // Check window-level UDP state
          const windowUdp = window.UDP || {};
          
          return JSON.stringify({
            hasUdpChannel,
            udpConnected,
            url: window.location.href,
            // Check console for UDP logs
          });
        })()
      `
    });
    
    if (result.result?.value) {
      const data = JSON.parse(result.result.value);
      debug(`hasUdpChannel: ${data.hasUdpChannel}`);
      debug(`udpConnected: ${data.udpConnected}`);
      debug(`URL: ${data.url}`);
      return data;
    }
  } catch (e) {
    debug(`Could not check browser UDP: ${e.message}`);
  }
  
  return null;
}

// Get console logs filtered for UDP
async function getUDPConsoleLogs(client) {
  log('Collecting browser console logs for UDP...');
  
  const logs = [];
  
  // Enable console logging
  await client.send('Runtime.enable');
  
  // Get existing logs
  try {
    const result = await client.send('Runtime.evaluate', {
      expression: `
        // Search for UDP-related console messages
        "UDP status check - see browser console for details"
      `
    });
    
    // Listen for console events for a moment
    const consoleHandler = (msg) => {
      if (msg.type === 'consoleAPICalled') {
        const text = msg.args?.map(a => a.value || a.description || '').join(' ') || '';
        if (text.includes('🩰') || text.toLowerCase().includes('udp')) {
          logs.push(text);
        }
      }
    };
    
    // Give time for any pending console logs
    await new Promise(resolve => setTimeout(resolve, 500));
    
    return logs;
  } catch (e) {
    debug(`Could not get console logs: ${e.message}`);
    return [];
  }
}

// Check network reachability for UDP
async function checkNetworkReachability() {
  log('Checking network reachability for UDP...');
  
  // Check if we're in a container
  const inContainer = process.env.REMOTE_CONTAINERS === 'true' || process.env.CODESPACES === 'true';
  info(`Running in container: ${inContainer ? 'YES' : 'NO'}`);
  
  // Get host LAN IP
  try {
    const hostIp = execSync('cat /tmp/host-lan-ip 2>/dev/null || echo "unknown"', { encoding: 'utf-8' }).trim();
    info(`Host LAN IP: ${hostIp}`);
    
    // Check if TURN server is reachable from browser's perspective
    // In container, browsers on host need to reach container's TURN
    // This is tricky because the browser is on the host, not in container
    
    if (inContainer && hostIp !== 'unknown') {
      info('Browser needs to reach TURN at: ' + hostIp + ':3478');
      info('Session server configured with TURN host: ' + hostIp);
    }
  } catch (e) {
    debug(`Could not determine host IP: ${e.message}`);
  }
  
  return true;
}

// Main diagnostic flow
async function runDiagnostics(verbose = false) {
  console.log(`\n${BOLD}${MAGENTA}🎮 1v1 UDP Diagnostic Test${RESET}`);
  console.log(`${DIM}Checking UDP/WebRTC connectivity for 1v1 piece${RESET}\n`);
  
  const results = {
    turnServer: false,
    sessionServer: false,
    udpPorts: false,
    browserConnected: false,
    udpActive: false,
  };
  
  // ─── INFRASTRUCTURE CHECKS ───
  section('INFRASTRUCTURE');
  
  results.turnServer = await checkTurnServer();
  const sessionStatus = await checkSessionServer();
  results.sessionServer = sessionStatus.running;
  results.udpPorts = sessionStatus.udpPorts.length > 0;
  
  await checkNetworkReachability();
  
  // ─── LOG ANALYSIS ───
  section('SESSION SERVER LOGS');
  
  const logStatus = await checkSessionLogs();
  results.udpActive = logStatus.udpActive;
  
  // Show relevant log excerpts if verbose
  if (verbose && logStatus.content) {
    console.log(`\n${DIM}Recent session log excerpt:${RESET}`);
    const lines = logStatus.content.split('\n').filter(l => 
      l.includes('🩰') || l.includes('UDP') || l.includes('geckos')
    ).slice(-20);
    lines.forEach(l => console.log(`  ${DIM}${l}${RESET}`));
  }
  
  // ─── BROWSER CHECKS ───
  section('BROWSER CONNECTIVITY');
  
  try {
    // Open VS Code Simple Browser panel first
    log('Opening AC panel in VS Code...');
    await Artery.openPanelStandalone();
    await new Promise(resolve => setTimeout(resolve, 1500));
    
    const client = new Artery();
    await client.connect();
    results.browserConnected = true;
    pass('Connected to AC via CDP');
    
    // Enable UDP logging
    log('Enabling UDP logging...');
    await client.send('Runtime.evaluate', { 
      expression: 'localStorage.setItem("ac-logs-udp", "true");' 
    });
    
    // Navigate to 1v1
    log('Navigating to 1v1 piece...');
    await client.jump('1v1');
    await new Promise(resolve => setTimeout(resolve, 5000)); // Wait for piece to fully load
    
    // Check UDP status in browser
    const browserStatus = await checkBrowserUDP(client);
    
    // Get UDP-related console logs
    const consoleLogs = await getUDPConsoleLogs(client);
    if (consoleLogs.length > 0) {
      debug('UDP-related console messages:');
      consoleLogs.slice(-10).forEach(l => debug(`  ${l}`));
    }
    
    // Check if UDP connected
    if (browserStatus?.udpConnected) {
      results.udpActive = true;
      pass('UDP connected in browser!');
    }
    
    client.ws?.close();
    
  } catch (e) {
    fail(`Could not connect to AC: ${e.message}`);
  }
  
  // ─── SUMMARY ───
  section('SUMMARY');
  
  const checks = [
    { name: 'TURN Server', status: results.turnServer },
    { name: 'Session Server', status: results.sessionServer },
    { name: 'UDP Ports (10000-10007)', status: results.udpPorts },
    { name: 'Browser CDP Connected', status: results.browserConnected },
    { name: 'UDP Traffic Active', status: results.udpActive },
  ];
  
  let allPass = true;
  for (const check of checks) {
    if (check.status) {
      console.log(`  ${GREEN}✅${RESET} ${check.name}`);
    } else {
      console.log(`  ${RED}❌${RESET} ${check.name}`);
      allPass = false;
    }
  }
  
  // ─── RECOMMENDATIONS ───
  section('RECOMMENDATIONS');
  
  if (!results.turnServer) {
    console.log(`  ${YELLOW}▸${RESET} Start TURN server:`);
    console.log(`    ${DIM}turnserver -c /workspaces/aesthetic-computer/.devcontainer/turnserver.conf --external-ip=$(cat /tmp/host-lan-ip)${RESET}`);
  }
  
  if (!results.sessionServer) {
    console.log(`  ${YELLOW}▸${RESET} Start session server from artery-tui or:`);
    console.log(`    ${DIM}cd session-server && npm run dev${RESET}`);
  }
  
  if (results.turnServer && results.sessionServer && !results.udpActive) {
    console.log(`  ${YELLOW}▸${RESET} Check browser console for WebRTC ICE errors`);
    console.log(`  ${YELLOW}▸${RESET} Verify TURN server is reachable from host browser:`);
    console.log(`    ${DIM}From host: nc -zvu $(cat /tmp/host-lan-ip) 3478${RESET}`);
    console.log(`  ${YELLOW}▸${RESET} Enable UDP logging in browser console:`);
    console.log(`    ${DIM}localStorage.setItem('ac-logs-udp', 'true'); location.reload()${RESET}`);
  }
  
  if (allPass) {
    console.log(`\n  ${GREEN_BG}${WHITE}${BOLD} UDP WORKING ✓ ${RESET}\n`);
  } else {
    console.log(`\n  ${YELLOW_BG}${BOLD} UDP NEEDS ATTENTION ${RESET}\n`);
  }
  
  return results;
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════════════

async function main() {
  const args = process.argv.slice(2);
  const verbose = args.includes('--verbose') || args.includes('-v');
  
  if (args.includes('--help') || args.includes('-h')) {
    console.log(`
🎮 1v1 UDP Diagnostic Test

Usage: node artery/test-1v1-udp.mjs [options]

Options:
  --verbose, -v    Show detailed log excerpts
  --help, -h       Show this help

Checks:
  • TURN server (coturn) status and port binding
  • Session server (geckos.io) WebRTC readiness  
  • UDP port allocation (10000-10007)
  • Browser WebRTC connection state
  • UDP message traffic in session logs
    `);
    process.exit(0);
  }
  
  try {
    await runDiagnostics(verbose);
  } catch (e) {
    console.error(`\n${RED}Error: ${e.message}${RESET}`);
    process.exit(1);
  }
}

main();
