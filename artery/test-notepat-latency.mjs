#!/usr/bin/env node
/**
 * 🎹⏱️ Notepat Latency Tester
 * 
 * Stress test for measuring keyboard input → audio output latency in notepat.
 * This is a regression/performance test for the speaker.mjs audio worklet.
 * 
 * ═══════════════════════════════════════════════════════════════════════════
 * HOW IT WORKS
 * ═══════════════════════════════════════════════════════════════════════════
 * 
 * 1. Connect to AC via CDP (Chrome DevTools Protocol)
 *    - Uses Artery class from artery.mjs (same as waltz/hiphop tests)
 *    - Opens VS Code AC panel via Artery.openPanelStandalone()
 *    - Connects to the AC iframe for remote control
 * 
 * 2. Navigate to notepat piece and activate audio
 *    - client.jump('notepat') navigates to the piece
 *    - client.activateAudio() resumes the AudioContext
 * 
 * 3. Use notepat's internal perfStats for latency measurement
 *    - notepat already tracks `lastKeyTime` (keydown) and `lastSoundTime` (synth call)
 *    - Exposed via `window.__notepat_perfStats`
 *    - This measures JS-side latency (not including audio buffer latency)
 * 
 * 4. Run latency tests in a loop:
 *    a. Clear previous perfStats latency
 *    b. Send keydown event
 *    c. Wait a moment for sound to trigger
 *    d. Read perfStats.latency from notepat
 *    e. Release key, wait for cooldown
 * 
 * 5. Report statistics (mean, median, p95, min, max)
 * 
 * ═══════════════════════════════════════════════════════════════════════════
 * ARCHITECTURE (for future reference)
 * ═══════════════════════════════════════════════════════════════════════════
 * 
 *   Devcontainer                    Host (Windows/Mac)
 *   ┌─────────────────┐             ┌─────────────────────────┐
 *   │ test-notepat-   │   CDP/WS    │ VS Code + Electron      │
 *   │ latency.mjs     │◄───────────►│ ┌─────────────────────┐ │
 *   │                 │             │ │ AC Panel (webview)  │ │
 *   │ Uses Artery     │             │ │ ├─ bios.mjs         │ │
 *   │ class to send   │             │ │ ├─ speaker.mjs      │ │
 *   │ CDP commands    │             │ │ │  (AudioWorklet)   │ │
 *   │                 │             │ │ └─ notepat.mjs      │ │
 *   └─────────────────┘             │ └─────────────────────┘ │
 *                                   └─────────────────────────┘
 * 
 * CDP Connection (via artery.mjs):
 *   - Artery.openPanelStandalone() → opens AC webview panel in VS Code
 *   - new Artery().connect() → finds AC iframe, establishes WebSocket
 *   - client.eval(js) → Runtime.evaluate in the iframe context
 *   - client.send('Input.dispatchKeyEvent', ...) → inject keyboard events
 * 
 * Audio Path:
 *   keydown event → notepat.mjs → sound.play() → bios.mjs triggerSound() 
 *   → speaker.mjs worklet → AudioContext output
 * 
 * Latency Measurement:
 *   notepat tracks perfStats.lastKeyTime (keydown handler)
 *   notepat tracks perfStats.lastSoundTime (makeNoteSound call)
 *   perfStats.latency = lastSoundTime - lastKeyTime
 *   Exposed at window.__notepat_perfStats
 * 
 * ═══════════════════════════════════════════════════════════════════════════
 * USAGE
 * ═══════════════════════════════════════════════════════════════════════════
 * 
 *   node artery/test-notepat-latency.mjs [options]
 * 
 * Options:
 *   --iterations=N   Number of test iterations (default: 20)
 *   --quick          Use quick mode (shorter notes)
 *   --cooldown=MS    Time to wait between tests (default: 300)
 * 
 * Run from Artery TUI:
 *   Press T → navigate to "notepat-latency" → Enter
 * 
 * ═══════════════════════════════════════════════════════════════════════════
 */

import Artery from './artery.mjs';

// ═══════════════════════════════════════════════════════════════════════════
// COLORS
// ═══════════════════════════════════════════════════════════════════════════
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const DIM = '\x1b[2m';
const RED = '\x1b[91m';
const GREEN = '\x1b[92m';
const YELLOW = '\x1b[93m';
const CYAN = '\x1b[96m';
const MAGENTA = '\x1b[95m';
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}⏱️${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const warnLog = (msg) => console.log(`${YELLOW}⚠️  ${msg}${RESET}`);
const errorLog = (msg) => console.log(`${RED}❌ ${msg}${RESET}`);

// ═══════════════════════════════════════════════════════════════════════════
// NOTE KEYS (from notepat - c d e f g a b on keyboard)
// ═══════════════════════════════════════════════════════════════════════════
const NOTE_KEYS = {
  'c': 'c', 'd': 'd', 'e': 'e', 'f': 'f', 'g': 'g', 'a': 'a', 'b': 'b',
  // Upper octave: h i j k l m n
  'C': 'h', 'D': 'i', 'E': 'j', 'F': 'k', 'G': 'l', 'A': 'm', 'B': 'n',
};

// ═══════════════════════════════════════════════════════════════════════════
// SOUND DETECTION (using bios.mjs triggerSound telemetry)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Get current sound trigger count from bios telemetry
 */
async function getTriggerCount(client) {
  try {
    const result = await client.eval(`window.__bios_sound_telemetry?.triggerCount || 0`);
    return result || 0;
  } catch (e) {
    return 0;
  }
}

/**
 * Get recent triggers from bios telemetry
 */
async function getRecentTriggers(client) {
  try {
    const result = await client.eval(`window.__bios_sound_telemetry?.recentTriggers || []`);
    return result || [];
  } catch (e) {
    return [];
  }
}

/**
 * Check if __bios_sound_telemetry is available (bios.mjs runs in main thread)
 */
async function checkTelemetryAvailable(client) {
  try {
    const result = await client.eval(`typeof window.__bios_sound_telemetry === 'object'`);
    return result === true;
  } catch (e) {
    return false;
  }
}

/**
 * Wait for all sounds to stop (based on no new triggers for a while)
 */
async function waitForSilence(client, timeoutMs = 2000) {
  // Just wait a fixed time since we can't track active sounds from bios
  await new Promise(r => setTimeout(r, Math.min(timeoutMs, 300)));
  return true;
}

// ═══════════════════════════════════════════════════════════════════════════
// KEY PRESS / RELEASE (using Runtime.evaluate for velocity support)
// ═══════════════════════════════════════════════════════════════════════════

async function pressKey(client, key, velocity = 127) {
  const code = `Key${key.toUpperCase()}`;
  const keyCode = key.toUpperCase().charCodeAt(0);
  
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keydown', {
          key: '${key}',
          code: '${code}',
          keyCode: ${keyCode},
          which: ${keyCode},
          bubbles: true,
          cancelable: true
        });
        event.velocity = ${velocity};
        window.dispatchEvent(event);
      })()
    `
  });
}

async function releaseKey(client, key) {
  const code = `Key${key.toUpperCase()}`;
  const keyCode = key.toUpperCase().charCodeAt(0);
  
  await client.send('Runtime.evaluate', {
    expression: `
      (function() {
        const event = new KeyboardEvent('keyup', {
          key: '${key}',
          code: '${code}',
          keyCode: ${keyCode},
          which: ${keyCode},
          bubbles: true,
          cancelable: true
        });
        window.dispatchEvent(event);
      })()
    `
  });
}

// ═══════════════════════════════════════════════════════════════════════════
// LATENCY TEST (measures time from keypress to triggerSound call in bios)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Run a single latency measurement
 * Measures time from keypress until bios.triggerSound is called
 * @returns {Promise<{success: boolean, latency?: number, error?: string}>}
 */
async function measureLatency(client, note, options = {}) {
  const { cooldown = 300 } = options;
  
  // 1. Get current trigger count before pressing
  const countBefore = await getTriggerCount(client);
  
  // 2. Get the keyboard key for this note
  const key = NOTE_KEYS[note];
  if (!key) {
    return { success: false, error: `unknown note: ${note}` };
  }
  
  // 3. Press the key and record timestamp
  const pressTime = performance.now();
  await pressKey(client, key);
  
  // 4. Poll until trigger count increases (or timeout)
  const timeout = 500; // 500ms max
  let detected = false;
  let latency = 0;
  
  while (performance.now() - pressTime < timeout) {
    const countNow = await getTriggerCount(client);
    if (countNow > countBefore) {
      latency = performance.now() - pressTime;
      detected = true;
      break;
    }
    await new Promise(r => setTimeout(r, 2)); // Poll every 2ms
  }
  
  // 5. Release the key
  await releaseKey(client, key);
  
  // 6. Check result
  if (!detected) {
    return { success: false, error: 'no sound triggered' };
  }
  
  // 7. Wait for cooldown (let sound play/decay)
  await new Promise(r => setTimeout(r, cooldown));
  
  return { success: true, latency };
}

// ═══════════════════════════════════════════════════════════════════════════
// STATISTICS
// ═══════════════════════════════════════════════════════════════════════════

function calculateStats(values) {
  if (values.length === 0) return null;
  
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;
  
  const variance = values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
  const stdDev = Math.sqrt(variance);
  
  const min = sorted[0];
  const max = sorted[sorted.length - 1];
  const median = sorted.length % 2 === 0
    ? (sorted[sorted.length / 2 - 1] + sorted[sorted.length / 2]) / 2
    : sorted[Math.floor(sorted.length / 2)];
  
  const p95 = sorted[Math.floor(sorted.length * 0.95)];
  const p99 = sorted[Math.floor(sorted.length * 0.99)];
  
  return { mean, median, stdDev, min, max, p95, p99, count: values.length };
}

function printStats(stats, label = 'Latency') {
  if (!stats) {
    console.log(`${DIM}No data${RESET}`);
    return;
  }
  
  console.log(`\n${MAGENTA}═══════════════════════════════════════════════════════════${RESET}`);
  console.log(`${BOLD}${CYAN}📊 ${label} Statistics (${stats.count} samples)${RESET}`);
  console.log(`${MAGENTA}═══════════════════════════════════════════════════════════${RESET}\n`);
  
  const meanColor = stats.mean < 10 ? GREEN : stats.mean < 30 ? YELLOW : RED;
  
  console.log(`  ${BOLD}Mean:${RESET}     ${meanColor}${stats.mean.toFixed(2)}ms${RESET}`);
  console.log(`  ${BOLD}Median:${RESET}   ${stats.median.toFixed(2)}ms`);
  console.log(`  ${BOLD}Std Dev:${RESET}  ${stats.stdDev.toFixed(2)}ms`);
  console.log(`  ${BOLD}Min:${RESET}      ${GREEN}${stats.min.toFixed(2)}ms${RESET}`);
  console.log(`  ${BOLD}Max:${RESET}      ${stats.max < 30 ? YELLOW : RED}${stats.max.toFixed(2)}ms${RESET}`);
  console.log(`  ${BOLD}P95:${RESET}      ${stats.p95?.toFixed(2) || 'N/A'}ms`);
  console.log(`  ${BOLD}P99:${RESET}      ${stats.p99?.toFixed(2) || 'N/A'}ms`);
  console.log();
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN TEST RUNNER
// ═══════════════════════════════════════════════════════════════════════════

async function main() {
  // Parse arguments
  const args = process.argv.slice(2);
  const options = {
    iterations: 20,
    quickMode: false,
    cooldown: 300,
  };
  
  for (const arg of args) {
    if (arg.startsWith('--iterations=') || arg.startsWith('iterations=')) {
      options.iterations = parseInt(arg.split('=')[1]);
    } else if (arg === '--quick' || arg === 'quick') {
      options.quickMode = true;
    } else if (arg.startsWith('--cooldown=') || arg.startsWith('cooldown=')) {
      options.cooldown = parseInt(arg.split('=')[1]);
    }
  }
  
  console.log(`\n${MAGENTA}═══════════════════════════════════════════════════════════${RESET}`);
  console.log(`${BOLD}${CYAN}🎹⏱️ Notepat Latency Tester${RESET}`);
  console.log(`${MAGENTA}═══════════════════════════════════════════════════════════${RESET}\n`);
  
  testLog(`Iterations: ${options.iterations}`);
  testLog(`Quick mode: ${options.quickMode ? 'ON' : 'OFF'}`);
  testLog(`Cooldown: ${options.cooldown}ms`);
  console.log();
  
  try {
    // ═══════════════════════════════════════════════════════════════════════
    // CDP CONNECTION (same pattern as test-generative-waltz.mjs)
    // ═══════════════════════════════════════════════════════════════════════
    
    // Close any existing panel to ensure fresh module loading
    testLog('Closing existing AC panel (if any)...');
    try {
      await Artery.closePanelStandalone();
      await new Promise(r => setTimeout(r, 500));
    } catch (e) {
      // Ignore if no panel open
    }
    
    // Open fresh AC panel
    await Artery.openPanelStandalone();
    await new Promise(r => setTimeout(r, 1000));
    testLog('AC panel opened fresh');
    
    // Create Artery instance and connect via CDP
    const client = new Artery();
    await client.connect();
    testLog('Connected to AC via CDP');
    
    // Navigate to notepat piece
    await client.jump('notepat');
    testLog('Navigated to notepat');
    await new Promise(r => setTimeout(r, 2000));
    
    // Reconnect after navigation (required for piece change)
    client.close();
    await client.connect();
    testLog('Reconnected after navigation');
    await new Promise(r => setTimeout(r, 500));
    
    // Activate the AudioContext (requires user gesture simulation)
    await client.activateAudio();
    testLog('Audio context activated');
    await new Promise(r => setTimeout(r, 500));
    
    // ═══════════════════════════════════════════════════════════════════════
    // VERIFY TELEMETRY IS AVAILABLE
    // ═══════════════════════════════════════════════════════════════════════
    
    // Wait a bit more for piece to fully initialize
    await new Promise(r => setTimeout(r, 1000));
    
    // Debug: check what's exposed
    const debug = await client.eval(`({
      hasBiosTelemetry: typeof window.__bios_sound_telemetry === 'object',
      triggerCount: window.__bios_sound_telemetry?.triggerCount || 0,
      currentPiece: window.location?.hash || window.location?.pathname || 'unknown'
    })`);
    testLog(`Debug: ${JSON.stringify(debug)}`);
    
    const telemetryOk = await checkTelemetryAvailable(client);
    if (!telemetryOk) {
      errorLog('__bios_sound_telemetry not exposed! Bios may not have loaded correctly.');
      process.exit(1);
    }
    testLog('Telemetry available (__bios_sound_telemetry)');
    
    // ═══════════════════════════════════════════════════════════════════════
    // RUN LATENCY TESTS
    // ═══════════════════════════════════════════════════════════════════════
    
    const notes = ['c', 'd', 'e', 'f', 'g', 'a', 'b'];
    const latencies = [];
    const failures = [];
    
    console.log(`\n${CYAN}Running ${options.iterations} latency measurements...${RESET}\n`);
    
    for (let i = 0; i < options.iterations; i++) {
      const note = notes[i % notes.length];
      const result = await measureLatency(client, note, options);
      
      if (result.success) {
        latencies.push(result.latency);
        const color = result.latency < 10 ? GREEN : result.latency < 30 ? YELLOW : RED;
        process.stdout.write(`${color}${result.latency.toFixed(1)}${RESET} `);
      } else {
        failures.push({ iteration: i, note, error: result.error });
        process.stdout.write(`${RED}X${RESET} `);
      }
      
      // Newline every 10
      if ((i + 1) % 10 === 0) {
        console.log();
      }
    }
    
    console.log('\n');
    
    // ═══════════════════════════════════════════════════════════════════════
    // RESULTS
    // ═══════════════════════════════════════════════════════════════════════
    
    if (latencies.length > 0) {
      const stats = calculateStats(latencies);
      printStats(stats, 'Keyboard → Sound Detection Latency');
      
      // Note: This measures JS-side latency (keypress to sound object creation)
      // Does NOT include audio buffer latency to actual audible output
      console.log(`${DIM}Note: This measures keypress → sound object creation.${RESET}`);
      console.log(`${DIM}Actual audio output adds ~10-50ms depending on buffer size.${RESET}`);
      console.log();
    }
    
    if (failures.length > 0) {
      warnLog(`${failures.length} failures:`);
      failures.slice(0, 5).forEach(f => {
        console.log(`  ${DIM}#${f.iteration} ${f.note}: ${f.error}${RESET}`);
      });
      if (failures.length > 5) {
        console.log(`  ${DIM}... and ${failures.length - 5} more${RESET}`);
      }
    }
    
    successLog(`Test complete: ${latencies.length}/${options.iterations} successful`);
    
    // ═══════════════════════════════════════════════════════════════════════
    // CLEANUP (same pattern as test-generative-waltz.mjs)
    // ═══════════════════════════════════════════════════════════════════════
    
    // Return to prompt piece
    await client.jump('prompt');
    testLog('Returned to prompt');
    await new Promise(r => setTimeout(r, 500));
    
    // Close panel and disconnect
    await Artery.closePanelStandalone();
    client.close();
    
    process.exit(failures.length > options.iterations / 2 ? 1 : 0);
    
  } catch (err) {
    errorLog(`Test failed: ${err.message}`);
    console.error(err.stack);
    process.exit(1);
  }
}

// Run the test
main();
