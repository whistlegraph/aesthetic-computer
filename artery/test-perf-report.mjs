#!/usr/bin/env node
/**
 * 📊 AC Perf Report via CDP
 * Gets timing telemetry from the AC client running in VS Code Simple Browser
 * 
 * Usage: node test-perf-report.mjs
 */

import Artery from './artery.mjs';

const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const GREEN = '\x1b[92m';
const RESET = '\x1b[0m';
const DIM = '\x1b[2m';

async function main() {
  const artery = new Artery();
  
  try {
    console.log(`${CYAN}📊 Connecting to AC...${RESET}`);
    await artery.connect();
    
    // Enable console to see the perf report output
    await artery.enableConsole((type, msg) => {
      if (msg.includes('⏱️') || msg.includes('📊') || msg.includes('🥾') || msg.includes('🌐')) {
        console.log(`${DIM}[browser]${RESET} ${msg}`);
      }
    });
    
    // Check if perf module is loaded
    const hasPerfTimings = await artery.eval('typeof window.acPerfTimings !== "undefined"');
    
    if (!hasPerfTimings) {
      console.log(`${YELLOW}⚠️ No perf timings found - perf module may not be loaded${RESET}`);
      console.log(`${DIM}Make sure perf.mjs is imported in bios.mjs${RESET}`);
      artery.close();
      return;
    }
    
    // Get the raw timings
    const timings = await artery.eval('JSON.stringify(window.acPerfTimings, null, 2)');
    console.log(`\n${GREEN}📊 Performance Timings:${RESET}`);
    console.log(timings);
    
    // Get disk worker timings if available
    const hasDiskTimings = await artery.eval('typeof window.acDiskTimings !== "undefined"');
    if (hasDiskTimings) {
      const diskTimings = await artery.eval('JSON.stringify(window.acDiskTimings, null, 2)');
      console.log(`\n${GREEN}📊 Disk Worker Timings:${RESET}`);
      console.log(diskTimings);
    }
    
    // Trigger the formatted report
    console.log(`\n${GREEN}📊 Formatted Report:${RESET}`);
    await artery.eval('window.acPerfReport && window.acPerfReport()');
    
    // Give time for console output to come through
    await new Promise(r => setTimeout(r, 500));
    
    artery.close();
    
  } catch (err) {
    console.error(`❌ Error: ${err.message}`);
    console.log(`${DIM}Make sure AC panel is open in VS Code${RESET}`);
    process.exit(1);
  }
}

main();
