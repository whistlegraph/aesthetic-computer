#!/usr/bin/env node
/**
 * 🔬 Notepat Stability Test
 * 
 * Long-running fuzz test to detect memory leaks and performance degradation.
 * Monitors: heap usage, audio worklet stats, button latency, sounds dict size.
 * 
 * Usage: node test-notepat-stability.mjs [duration] [intensity]
 *   duration  - Test duration in minutes (default: 5)
 *   intensity - Notes per second: low|medium|high|extreme (default: medium)
 */

import Artery from './artery.mjs';

// ═══════════════════════════════════════════════════════════════════════════
// TERMINAL COLORS
// ═══════════════════════════════════════════════════════════════════════════
const RED = '\x1b[91m';
const GREEN = '\x1b[92m';
const YELLOW = '\x1b[93m';
const CYAN = '\x1b[96m';
const MAGENTA = '\x1b[95m';
const WHITE = '\x1b[97m';
const DIM = '\x1b[2m';
const BOLD = '\x1b[1m';
const RESET = '\x1b[0m';
const BG_RED = '\x1b[41m';
const BG_GREEN = '\x1b[42m';
const BG_YELLOW = '\x1b[43m';

// ═══════════════════════════════════════════════════════════════════════════
// TEST CONFIGURATION
// ═══════════════════════════════════════════════════════════════════════════
const INTENSITY_PROFILES = {
  low: { notesPerSec: 2, burstChance: 0.05, maxConcurrent: 2 },
  medium: { notesPerSec: 5, burstChance: 0.1, maxConcurrent: 4 },
  high: { notesPerSec: 10, burstChance: 0.2, maxConcurrent: 6 },
  extreme: { notesPerSec: 20, burstChance: 0.3, maxConcurrent: 10 },
};

// Note keys for fuzzing (lower octave + upper octave)
const NOTE_KEYS = [
  'c', 'v', 'd', 's', 'e', 'f', 'w', 'g', 'r', 'a', 'q', 'b', // lower
  'h', 't', 'i', 'y', 'j', 'k', 'u', 'l', 'o', 'm', 'p', 'n', // upper
];

// Drum keys
const DRUM_KEYS = {
  kick: { key: 'ArrowDown', code: 'ArrowDown', keyCode: 40 },
  snare: { key: ' ', code: 'Space', keyCode: 32 },
  hat: { key: 'ArrowUp', code: 'ArrowUp', keyCode: 38 },
};

// ═══════════════════════════════════════════════════════════════════════════
// TELEMETRY COLLECTION
// ═══════════════════════════════════════════════════════════════════════════

class TelemetryCollector {
  constructor() {
    this.samples = [];
    this.startTime = Date.now();
    this.warnings = [];
    this.baseline = null;
  }

  addSample(sample) {
    sample.elapsed = (Date.now() - this.startTime) / 1000;
    this.samples.push(sample);
    
    // Set baseline from first few samples
    if (this.samples.length === 5 && !this.baseline) {
      this.baseline = {
        heapUsed: this.avgField('heapUsed', 5),
        soundsCount: this.avgField('soundsCount', 5),
        tonestackCount: this.avgField('tonestackCount', 5),
        runningCount: this.avgField('runningCount', 5),
        queueLength: this.avgField('queueLength', 5),
      };
    }
    
    // Check for anomalies
    if (this.baseline && this.samples.length > 10) {
      this.checkAnomalies(sample);
    }
  }

  avgField(field, count) {
    const recent = this.samples.slice(-count);
    const sum = recent.reduce((acc, s) => acc + (s[field] || 0), 0);
    return sum / recent.length;
  }

  checkAnomalies(sample) {
    // Heap grew more than 50% from baseline
    if (sample.heapUsed > this.baseline.heapUsed * 1.5) {
      this.warn(`Heap grew ${((sample.heapUsed / this.baseline.heapUsed - 1) * 100).toFixed(0)}% from baseline`);
    }
    
    // Sounds dict not clearing (stuck sounds)
    if (sample.soundsCount > 10) {
      this.warn(`sounds dict has ${sample.soundsCount} entries (possible stuck sounds)`);
    }
    
    // Tonestack not clearing
    if (sample.tonestackCount > 10) {
      this.warn(`tonestack has ${sample.tonestackCount} entries (possible stuck notes)`);
    }
    
    // Audio worklet queue growing
    if (sample.queueLength > 20) {
      this.warn(`Audio queue has ${sample.queueLength} items (possible backup)`);
    }
    
    // Running instruments not clearing
    if (sample.runningCount > 30) {
      this.warn(`${sample.runningCount} running instruments (possible leak)`);
    }
  }

  warn(msg) {
    const warning = { time: Date.now() - this.startTime, msg };
    this.warnings.push(warning);
    console.log(`${BG_YELLOW}${WHITE} ⚠️ WARNING ${RESET} ${YELLOW}${msg}${RESET}`);
  }

  getReport() {
    if (this.samples.length < 2) return null;
    
    const first5 = this.samples.slice(0, 5);
    const last5 = this.samples.slice(-5);
    
    const avgFirst = (field) => first5.reduce((a, s) => a + (s[field] || 0), 0) / first5.length;
    const avgLast = (field) => last5.reduce((a, s) => a + (s[field] || 0), 0) / last5.length;
    
    return {
      duration: (Date.now() - this.startTime) / 1000,
      samples: this.samples.length,
      warnings: this.warnings.length,
      heap: {
        start: avgFirst('heapUsed'),
        end: avgLast('heapUsed'),
        growth: ((avgLast('heapUsed') / avgFirst('heapUsed')) - 1) * 100,
        peak: Math.max(...this.samples.map(s => s.heapUsed || 0)),
      },
      sounds: {
        avgStart: avgFirst('soundsCount'),
        avgEnd: avgLast('soundsCount'),
        maxSeen: Math.max(...this.samples.map(s => s.soundsCount || 0)),
      },
      tonestack: {
        avgStart: avgFirst('tonestackCount'),
        avgEnd: avgLast('tonestackCount'),
        maxSeen: Math.max(...this.samples.map(s => s.tonestackCount || 0)),
      },
      audioWorklet: {
        avgQueueStart: avgFirst('queueLength'),
        avgQueueEnd: avgLast('queueLength'),
        maxQueue: Math.max(...this.samples.map(s => s.queueLength || 0)),
        avgRunningStart: avgFirst('runningCount'),
        avgRunningEnd: avgLast('runningCount'),
        maxRunning: Math.max(...this.samples.map(s => s.runningCount || 0)),
      },
    };
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// CDP HELPERS
// ═══════════════════════════════════════════════════════════════════════════

async function pressKey(client, key) {
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

async function pressDrum(client, drumType) {
  const drum = DRUM_KEYS[drumType];
  if (!drum) return;
  
  await client.send('Input.dispatchKeyEvent', {
    type: 'keyDown',
    key: drum.key,
    code: drum.code,
    windowsVirtualKeyCode: drum.keyCode,
  });
  
  setTimeout(async () => {
    await client.send('Input.dispatchKeyEvent', {
      type: 'keyUp',
      key: drum.key,
      code: drum.code,
      windowsVirtualKeyCode: drum.keyCode,
    });
  }, 30);
}

async function getTelemetry(client) {
  try {
    const result = await client.send('Runtime.evaluate', {
      expression: `
        (function() {
          const telemetry = {
            // JS Heap from performance.memory (Chrome only)
            heapUsed: performance.memory?.usedJSHeapSize || 0,
            heapTotal: performance.memory?.totalJSHeapSize || 0,
            
            // Notepat state (if available via window.__acTelemetry)
            soundsCount: window.__acTelemetry?.soundsCount || 0,
            tonestackCount: window.__acTelemetry?.tonestackCount || 0,
            trailCount: window.__acTelemetry?.trailCount || 0,
            
            // Speaker worklet state
            queueLength: window.__acTelemetry?.queueLength || 0,
            runningCount: window.__acTelemetry?.runningCount || 0,
            
            // Performance
            fps: window.__acTelemetry?.fps || 0,
            frameTime: window.__acTelemetry?.frameTime || 0,
            
            // Timestamp
            timestamp: Date.now(),
          };
          return JSON.stringify(telemetry);
        })()
      `,
      returnByValue: true,
    });
    
    return JSON.parse(result.result.value);
  } catch (e) {
    return { error: e.message, timestamp: Date.now() };
  }
}

// Inject telemetry hooks into the page
async function injectTelemetryHooks(client) {
  await client.send('Runtime.evaluate', {
    expression: `
      // Create global telemetry object
      window.__acTelemetry = window.__acTelemetry || {
        soundsCount: 0,
        tonestackCount: 0,
        trailCount: 0,
        queueLength: 0,
        runningCount: 0,
        fps: 0,
        frameTime: 0,
        lastUpdate: 0,
      };
      
      // Try to hook into notepat's state
      const hookInterval = setInterval(() => {
        try {
          // These will be set by our modified notepat.mjs
          if (window.__notepat_sounds) {
            window.__acTelemetry.soundsCount = Object.keys(window.__notepat_sounds).length;
          }
          if (window.__notepat_tonestack) {
            window.__acTelemetry.tonestackCount = Object.keys(window.__notepat_tonestack).length;
          }
          if (window.__notepat_trail) {
            window.__acTelemetry.trailCount = Object.keys(window.__notepat_trail).length;
          }
          
          // Speaker telemetry (if exposed)
          if (window.__speaker_telemetry) {
            window.__acTelemetry.queueLength = window.__speaker_telemetry.queueLength || 0;
            window.__acTelemetry.runningCount = window.__speaker_telemetry.runningCount || 0;
          }
          
          window.__acTelemetry.lastUpdate = Date.now();
        } catch (e) {
          // Silently fail if hooks not available
        }
      }, 200);
      
      console.log('🔬 Telemetry hooks injected');
      true;
    `
  });
}

// ═══════════════════════════════════════════════════════════════════════════
// FUZZING LOGIC
// ═══════════════════════════════════════════════════════════════════════════

class NoteFuzzer {
  constructor(client, intensity) {
    this.client = client;
    this.profile = INTENSITY_PROFILES[intensity] || INTENSITY_PROFILES.medium;
    this.activeNotes = new Set();
    this.noteCount = 0;
    this.drumCount = 0;
  }

  randomNote() {
    return NOTE_KEYS[Math.floor(Math.random() * NOTE_KEYS.length)];
  }

  randomDrum() {
    const drums = Object.keys(DRUM_KEYS);
    return drums[Math.floor(Math.random() * drums.length)];
  }

  async playRandomNote() {
    const note = this.randomNote();
    
    // Don't exceed max concurrent
    if (this.activeNotes.size >= this.profile.maxConcurrent) {
      // Release a random active note
      const activeArray = Array.from(this.activeNotes);
      const toRelease = activeArray[Math.floor(Math.random() * activeArray.length)];
      await releaseKey(this.client, toRelease);
      this.activeNotes.delete(toRelease);
    }
    
    await pressKey(this.client, note);
    this.activeNotes.add(note);
    this.noteCount++;
    
    // Schedule release
    const holdTime = 50 + Math.random() * 300;
    setTimeout(async () => {
      if (this.activeNotes.has(note)) {
        await releaseKey(this.client, note);
        this.activeNotes.delete(note);
      }
    }, holdTime);
  }

  async playRandomDrum() {
    const drum = this.randomDrum();
    await pressDrum(this.client, drum);
    this.drumCount++;
  }

  async burst() {
    // Play several notes rapidly
    const burstSize = 3 + Math.floor(Math.random() * 5);
    for (let i = 0; i < burstSize; i++) {
      await this.playRandomNote();
      await new Promise(r => setTimeout(r, 20));
    }
  }

  async releaseAll() {
    for (const note of this.activeNotes) {
      await releaseKey(this.client, note);
    }
    this.activeNotes.clear();
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN TEST
// ═══════════════════════════════════════════════════════════════════════════

async function runStabilityTest(durationMinutes, intensity) {
  console.log(`\n${BOLD}${MAGENTA}🔬 Notepat Stability Test${RESET}`);
  console.log(`${DIM}Duration: ${durationMinutes} minutes | Intensity: ${intensity}${RESET}\n`);
  
  const artery = new Artery();
  
  try {
    // Connect to browser
    console.log(`${CYAN}Connecting to browser...${RESET}`);
    await artery.connect();
    const client = artery.client;
    
    // Navigate to notepat
    console.log(`${CYAN}Loading notepat...${RESET}`);
    await artery.navigateTo('notepat');
    await new Promise(r => setTimeout(r, 2000)); // Wait for piece to load
    
    // Inject telemetry hooks
    console.log(`${CYAN}Injecting telemetry hooks...${RESET}`);
    await injectTelemetryHooks(client);
    await new Promise(r => setTimeout(r, 500));
    
    // Initialize
    const telemetry = new TelemetryCollector();
    const fuzzer = new NoteFuzzer(client, intensity);
    const profile = INTENSITY_PROFILES[intensity];
    
    const durationMs = durationMinutes * 60 * 1000;
    const startTime = Date.now();
    const noteInterval = 1000 / profile.notesPerSec;
    const telemetryInterval = 2000; // Sample every 2 seconds
    
    console.log(`\n${GREEN}▶ Starting ${durationMinutes}-minute fuzz test...${RESET}`);
    console.log(`${DIM}  Notes/sec: ${profile.notesPerSec} | Burst chance: ${profile.burstChance * 100}% | Max concurrent: ${profile.maxConcurrent}${RESET}\n`);
    
    // Progress bar setup
    const progressWidth = 40;
    let lastProgressUpdate = 0;
    
    // Collect initial telemetry
    const initialTelemetry = await getTelemetry(client);
    telemetry.addSample(initialTelemetry);
    
    // Main loop
    let lastNoteTime = 0;
    let lastTelemetryTime = 0;
    
    return new Promise((resolve) => {
      const interval = setInterval(async () => {
        const elapsed = Date.now() - startTime;
        const progress = elapsed / durationMs;
        
        // Update progress bar every second
        if (elapsed - lastProgressUpdate > 1000) {
          const filled = Math.floor(progress * progressWidth);
          const empty = progressWidth - filled;
          const bar = '█'.repeat(filled) + '░'.repeat(empty);
          const mins = Math.floor(elapsed / 60000);
          const secs = Math.floor((elapsed % 60000) / 1000);
          const heapMB = ((telemetry.samples[telemetry.samples.length - 1]?.heapUsed || 0) / 1024 / 1024).toFixed(1);
          
          process.stdout.write(`\r${CYAN}[${bar}]${RESET} ${mins}:${secs.toString().padStart(2, '0')} | Notes: ${fuzzer.noteCount} | Drums: ${fuzzer.drumCount} | Heap: ${heapMB}MB | ⚠️ ${telemetry.warnings.length}`);
          lastProgressUpdate = elapsed;
        }
        
        // Play notes at configured rate
        if (elapsed - lastNoteTime > noteInterval) {
          // Chance for burst
          if (Math.random() < profile.burstChance) {
            await fuzzer.burst();
          } else {
            await fuzzer.playRandomNote();
          }
          
          // Also play drums occasionally
          if (Math.random() < 0.15) {
            await fuzzer.playRandomDrum();
          }
          
          lastNoteTime = elapsed;
        }
        
        // Collect telemetry
        if (elapsed - lastTelemetryTime > telemetryInterval) {
          const sample = await getTelemetry(client);
          telemetry.addSample(sample);
          lastTelemetryTime = elapsed;
        }
        
        // Check if done
        if (elapsed >= durationMs) {
          clearInterval(interval);
          
          // Clean up
          await fuzzer.releaseAll();
          await new Promise(r => setTimeout(r, 500));
          
          // Final telemetry
          const finalSample = await getTelemetry(client);
          telemetry.addSample(finalSample);
          
          // Generate report
          const report = telemetry.getReport();
          
          console.log(`\n\n${BOLD}${MAGENTA}═══════════════════════════════════════════════════════════${RESET}`);
          console.log(`${BOLD}${WHITE}                    📊 STABILITY REPORT                     ${RESET}`);
          console.log(`${MAGENTA}═══════════════════════════════════════════════════════════${RESET}\n`);
          
          console.log(`${CYAN}Duration:${RESET} ${(report.duration / 60).toFixed(1)} minutes`);
          console.log(`${CYAN}Notes played:${RESET} ${fuzzer.noteCount}`);
          console.log(`${CYAN}Drums played:${RESET} ${fuzzer.drumCount}`);
          console.log(`${CYAN}Telemetry samples:${RESET} ${report.samples}`);
          console.log(`${CYAN}Warnings:${RESET} ${report.warnings}`);
          
          console.log(`\n${YELLOW}Memory (Heap):${RESET}`);
          console.log(`  Start: ${(report.heap.start / 1024 / 1024).toFixed(2)} MB`);
          console.log(`  End:   ${(report.heap.end / 1024 / 1024).toFixed(2)} MB`);
          console.log(`  Peak:  ${(report.heap.peak / 1024 / 1024).toFixed(2)} MB`);
          const heapStatus = report.heap.growth < 20 ? `${GREEN}✓ STABLE${RESET}` : 
                           report.heap.growth < 50 ? `${YELLOW}⚠ GROWING${RESET}` : 
                           `${RED}✗ LEAKING${RESET}`;
          console.log(`  Growth: ${report.heap.growth.toFixed(1)}% ${heapStatus}`);
          
          console.log(`\n${YELLOW}Notepat State:${RESET}`);
          console.log(`  sounds dict - avg: ${report.sounds.avgEnd.toFixed(1)}, max: ${report.sounds.maxSeen}`);
          console.log(`  tonestack   - avg: ${report.tonestack.avgEnd.toFixed(1)}, max: ${report.tonestack.maxSeen}`);
          
          console.log(`\n${YELLOW}Audio Worklet:${RESET}`);
          console.log(`  queue length  - avg: ${report.audioWorklet.avgQueueEnd.toFixed(1)}, max: ${report.audioWorklet.maxQueue}`);
          console.log(`  running instr - avg: ${report.audioWorklet.avgRunningEnd.toFixed(1)}, max: ${report.audioWorklet.maxRunning}`);
          
          // Overall verdict
          console.log(`\n${MAGENTA}═══════════════════════════════════════════════════════════${RESET}`);
          const passed = report.heap.growth < 50 && 
                        report.sounds.maxSeen < 20 && 
                        report.tonestack.maxSeen < 20 &&
                        report.warnings < 10;
          
          if (passed) {
            console.log(`${BG_GREEN}${WHITE}${BOLD}                      ✓ TEST PASSED                        ${RESET}`);
          } else {
            console.log(`${BG_RED}${WHITE}${BOLD}                      ✗ TEST FAILED                        ${RESET}`);
            console.log(`\n${RED}Issues detected:${RESET}`);
            if (report.heap.growth >= 50) console.log(`  - Memory leak: heap grew ${report.heap.growth.toFixed(0)}%`);
            if (report.sounds.maxSeen >= 20) console.log(`  - Stuck sounds: max ${report.sounds.maxSeen} in sounds dict`);
            if (report.tonestack.maxSeen >= 20) console.log(`  - Stuck notes: max ${report.tonestack.maxSeen} in tonestack`);
            if (report.warnings >= 10) console.log(`  - ${report.warnings} warnings during test`);
          }
          console.log(`${MAGENTA}═══════════════════════════════════════════════════════════${RESET}\n`);
          
          resolve({ passed, report, warnings: telemetry.warnings });
        }
      }, 10);
    });
    
  } catch (error) {
    console.error(`${RED}Error: ${error.message}${RESET}`);
    throw error;
  } finally {
    artery.close();
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// CLI
// ═══════════════════════════════════════════════════════════════════════════

const args = process.argv.slice(2);
let duration = 5; // Default 5 minutes
let intensity = 'medium';

for (const arg of args) {
  if (['low', 'medium', 'high', 'extreme'].includes(arg)) {
    intensity = arg;
  } else if (!isNaN(parseInt(arg))) {
    duration = parseInt(arg);
  }
}

console.log(`${DIM}Usage: node test-notepat-stability.mjs [duration_minutes] [low|medium|high|extreme]${RESET}`);

runStabilityTest(duration, intensity)
  .then(result => {
    process.exit(result.passed ? 0 : 1);
  })
  .catch(err => {
    console.error(err);
    process.exit(1);
  });
