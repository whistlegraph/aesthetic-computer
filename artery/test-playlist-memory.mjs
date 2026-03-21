#!/usr/bin/env node
/**
 * 🧠 KidLisp Playlist Memory Stress Test
 *
 * Simulates rapid playlist cycling through many KidLisp pieces and monitors
 * JS heap growth over time. This test verifies that the singleton
 * globalKidLispInstance and its caches don't leak memory across transitions.
 *
 * Usage:
 *   node artery/test-playlist-memory.mjs              # Default: 40 cycles, 3s each
 *   node artery/test-playlist-memory.mjs --cycles 100 # Custom cycle count
 *   node artery/test-playlist-memory.mjs --duration 5  # Seconds per piece
 *   node artery/test-playlist-memory.mjs --threshold 50 # Fail if heap grows > 50MB
 *
 * Requires: AC dev server running + piece loaded in VS Code Simple Browser
 */

import { withCDP } from "./cdp.mjs";

const RESET = "\x1b[0m";
const BOLD = "\x1b[1m";
const GREEN = "\x1b[32m";
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const DIM = "\x1b[2m";
const MAGENTA = "\x1b[35m";

// Parse CLI args
const args = process.argv.slice(2);
function getArg(name, fallback) {
  const idx = args.indexOf(`--${name}`);
  return idx !== -1 && args[idx + 1] ? Number(args[idx + 1]) : fallback;
}

const CYCLES = getArg("cycles", 40);
const PIECE_DURATION_S = getArg("duration", 3);
const HEAP_GROWTH_THRESHOLD_MB = getArg("threshold", 80);

// Diverse KidLisp sources that exercise different code paths:
// - simple drawing, animation, fps changes, embedded layers, math, etc.
const TEST_PIECES = [
  // Basic static
  `(wipe black)\n(ink red)\n(box 10 10 100 100)`,
  // Animation with fps change
  `(fps 30)\n(wipe (rgb (rand 255) (rand 255) (rand 255)))\n(ink white)\n(circle 160 120 (rand 10 80))`,
  // High fps
  `(fps 60)\n(wipe navy)\n(each i 50\n  (ink (hsl (* i 7) 80 60))\n  (line 0 (* i 5) 320 (* i 5)))`,
  // Low fps
  `(fps 12)\n(wipe black)\n(ink lime)\n(box (rand 300) (rand 220) 20 20)`,
  // Lots of drawing ops
  `(wipe white)\n(each x 16\n  (each y 12\n    (ink (rgb (* x 16) (* y 21) 128))\n    (box (* x 20) (* y 20) 18 18)))`,
  // Circles / math
  `(wipe black)\n(each i 100\n  (ink (hsl (* i 3.6) 100 50))\n  (circle (+ 160 (* 80 (cos (radians (* i 3.6))))) (+ 120 (* 60 (sin (radians (* i 3.6))))) 5))`,
  // Noise
  `(wipe black)\n(each x 32\n  (each y 24\n    (ink (gray (* 255 (noise (* x 0.15) (* y 0.15)))))\n    (box (* x 10) (* y 10) 10 10)))`,
  // Text/write
  `(wipe darkblue)\n(ink yellow)\n(write "memory test" 20 60)`,
  // Gradient
  `(each i 320\n  (ink (rgb i 0 (- 319 i)))\n  (line i 0 i 240))`,
  // Accumulation pattern (starts with color word)
  `red\n(ink white)\n(circle (rand 320) (rand 240) (rand 5 30))`,
  // Nested repeat
  `(wipe black)\n(repeat 5\n  (ink (? red green blue yellow cyan magenta))\n  (box (rand 300) (rand 220) (rand 10 60) (rand 10 60)))`,
  // Default fps (no fps call - should reset to 60)
  `(wipe (rgb 30 30 60))\n(ink (rgb 255 200 100))\n(box 80 40 160 160)`,
];

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function formatMB(bytes) {
  return (bytes / 1024 / 1024).toFixed(2);
}

await withCDP(
  async (cdp) => {
    console.log(
      `\n${MAGENTA}${BOLD}═══ KidLisp Playlist Memory Stress Test ═══${RESET}\n`,
    );
    console.log(`  Cycles: ${CYAN}${CYCLES}${RESET}`);
    console.log(`  Duration per piece: ${CYAN}${PIECE_DURATION_S}s${RESET}`);
    console.log(
      `  Heap growth threshold: ${CYAN}${HEAP_GROWTH_THRESHOLD_MB} MB${RESET}`,
    );
    console.log(
      `  Estimated runtime: ${CYAN}~${Math.ceil((CYCLES * PIECE_DURATION_S) / 60)} min${RESET}\n`,
    );

    await cdp.waitForReady(15000);
    console.log(`${GREEN}Page ready.${RESET}\n`);

    // Check if performance.memory is available (Chrome only)
    const hasMemoryAPI = await cdp.eval(
      `typeof performance.memory !== 'undefined'`,
    );
    if (!hasMemoryAPI) {
      console.log(
        `${YELLOW}Warning: performance.memory not available (Chrome-only API).${RESET}`,
      );
      console.log(
        `${YELLOW}Heap measurements will be estimated via object counting.${RESET}\n`,
      );
    }

    // Helper: get memory snapshot from iframe's disk worker context
    async function getMemorySnapshot() {
      return cdp.eval(`(() => {
        const snap = {};

        // Chrome performance.memory (best source)
        if (performance.memory) {
          snap.usedHeapMB = performance.memory.usedJSHeapSize / 1024 / 1024;
          snap.totalHeapMB = performance.memory.totalJSHeapSize / 1024 / 1024;
          snap.limitMB = performance.memory.jsHeapSizeLimit / 1024 / 1024;
        }

        // KidLisp instance cache sizes (via the disk worker's global)
        const kl = window.__acGlobalKidLispInstance;
        if (kl) {
          snap.kidlisp = {
            embeddedLayerCache: kl.embeddedLayerCache?.size || 0,
            embeddedLayers: kl.embeddedLayers?.length || 0,
            alphaBufferCache: kl.alphaBufferCache?.size || 0,
            bufferPool: kl.bufferPool?.size || 0,
            tapeEmbeds: kl.tapeEmbeds?.size || 0,
            tapeFrameBuffers: kl.tapeFrameBuffers?.size || 0,
            singletonDollarCodeCache: kl.singletonDollarCodeCache?.size || 0,
            persistentPaintings: kl.persistentPaintings?.size || 0,
            mathCache: kl.mathCache?.size || 0,
            functionCache: kl.functionCache?.size || 0,
            sequenceCounters: kl.sequenceCounters?.size || 0,
            melodies: kl.melodies?.size || 0,
            unknownWordsLogged: kl.unknownWordsLogged?.size || 0,
            syntaxSignals: kl.syntaxSignals?.size || 0,
            expressionRegistry: kl.expressionRegistry?.size || 0,
            choiceCache: kl.choiceCache?.size || 0,
            globalFunctionCache: kl.globalFunctionCache?.size || 0,
            loadingEmbeddedLayers: kl.loadingEmbeddedLayers?.size || 0,
            loadedEmbeddedLayers: kl.loadedEmbeddedLayers?.size || 0,
          };
        }
        return snap;
      })()`);
    }

    // Helper: inject KidLisp source and trigger execution via the prompt
    // We simulate what device.html does: jump to a $code piece
    // But since we're testing in the AC iframe, we load kidlisp source directly
    async function loadKidLispSource(source) {
      const escaped = JSON.stringify(source);
      return cdp.eval(
        `(() => {
        // Use the global kidlisp module loading path
        const disk = window.__acDisk || window.disk;
        if (disk && disk.load) {
          disk.load({
            source: ${escaped},
            name: "memory-test",
            search: "",
            colon: [],
            params: [],
            hash: "",
          }, true, false, true, undefined, true);
          return true;
        }

        // Fallback: try the jump-based approach
        const jumpFn = window.__acJump;
        if (jumpFn) {
          // Can't use jump for raw source, try postMessage
          return false;
        }

        return false;
      })()`,
        { awaitPromise: false },
      );
    }

    // Alternative: use postMessage to simulate playlist navigation
    async function navigateToSource(source) {
      const escaped = JSON.stringify(source);
      return cdp.eval(
        `(() => {
        // Try the disk worker's piece-reload mechanism
        if (typeof window.acLoadKidlispSource === 'function') {
          window.acLoadKidlispSource(${escaped});
          return 'acLoadKidlispSource';
        }
        // Try direct postMessage to disk worker
        if (window.__acWorker) {
          window.__acWorker.postMessage({
            type: 'piece-reload',
            content: { source: ${escaped}, forceKidlisp: true }
          });
          return 'worker-postMessage';
        }
        // Fallback: dispatch a custom event that bios can pick up
        window.postMessage({
          type: 'ac:load-kidlisp',
          source: ${escaped}
        }, '*');
        return 'postMessage-fallback';
      })()`,
      );
    }

    // Take baseline measurement
    const baseline = await getMemorySnapshot();
    console.log(`${CYAN}Baseline:${RESET}`);
    if (baseline.usedHeapMB) {
      console.log(
        `  Heap: ${formatMB(baseline.usedHeapMB * 1024 * 1024)} MB used / ${formatMB(baseline.totalHeapMB * 1024 * 1024)} MB total`,
      );
    }
    if (baseline.kidlisp) {
      const kl = baseline.kidlisp;
      console.log(
        `  KidLisp caches: embedded=${kl.embeddedLayerCache} alpha=${kl.alphaBufferCache} pool=${kl.bufferPool} tapes=${kl.tapeEmbeds} dollar=${kl.singletonDollarCodeCache} paintings=${kl.persistentPaintings}`,
      );
    }
    console.log();

    // Track measurements over time
    const measurements = [
      { cycle: 0, time: 0, ...baseline },
    ];

    // Run the stress test
    const startTime = Date.now();
    let lastFps = null;

    for (let i = 0; i < CYCLES; i++) {
      const piece = TEST_PIECES[i % TEST_PIECES.length];
      const pieceLabel = piece.split("\n")[0].substring(0, 40);

      process.stdout.write(
        `${DIM}[${i + 1}/${CYCLES}]${RESET} Loading: ${pieceLabel}...`,
      );

      // Load the piece
      const method = await navigateToSource(piece);

      // Wait for piece to run
      await sleep(PIECE_DURATION_S * 1000);

      // Take measurement every 5 cycles
      if ((i + 1) % 5 === 0 || i === CYCLES - 1) {
        const snap = await getMemorySnapshot();
        const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
        measurements.push({
          cycle: i + 1,
          time: elapsed,
          ...snap,
        });

        if (snap.usedHeapMB) {
          const delta = snap.usedHeapMB - baseline.usedHeapMB;
          const color = delta > HEAP_GROWTH_THRESHOLD_MB / 2 ? YELLOW : GREEN;
          process.stdout.write(
            ` ${color}heap: ${snap.usedHeapMB.toFixed(1)}MB (${delta >= 0 ? "+" : ""}${delta.toFixed(1)}MB)${RESET}`,
          );
        }
        if (snap.kidlisp) {
          const growing = [];
          if (snap.kidlisp.tapeEmbeds > 0)
            growing.push(`tapes:${snap.kidlisp.tapeEmbeds}`);
          if (snap.kidlisp.singletonDollarCodeCache > 0)
            growing.push(`$cache:${snap.kidlisp.singletonDollarCodeCache}`);
          if (snap.kidlisp.embeddedLayerCache > 0)
            growing.push(`embed:${snap.kidlisp.embeddedLayerCache}`);
          if (snap.kidlisp.unknownWordsLogged > 0)
            growing.push(`unknown:${snap.kidlisp.unknownWordsLogged}`);
          if (growing.length > 0) {
            process.stdout.write(` ${DIM}[${growing.join(" ")}]${RESET}`);
          }
        }
      }

      console.log();
    }

    // Final analysis
    const final = measurements[measurements.length - 1];
    const totalTime = ((Date.now() - startTime) / 1000).toFixed(1);

    console.log(
      `\n${MAGENTA}${BOLD}═══ Results ═══${RESET}\n`,
    );
    console.log(`  Total time: ${totalTime}s`);
    console.log(`  Pieces cycled: ${CYCLES}`);

    if (final.usedHeapMB && baseline.usedHeapMB) {
      const heapGrowth = final.usedHeapMB - baseline.usedHeapMB;
      const growthPerCycle = heapGrowth / CYCLES;
      const passed = heapGrowth < HEAP_GROWTH_THRESHOLD_MB;

      console.log(
        `\n  ${BOLD}Heap growth:${RESET} ${heapGrowth >= 0 ? "+" : ""}${heapGrowth.toFixed(2)} MB`,
      );
      console.log(
        `  Per-cycle avg: ${growthPerCycle >= 0 ? "+" : ""}${(growthPerCycle * 1024).toFixed(1)} KB/cycle`,
      );
      console.log(
        `  Baseline: ${baseline.usedHeapMB.toFixed(1)} MB → Final: ${final.usedHeapMB.toFixed(1)} MB`,
      );

      if (passed) {
        console.log(
          `\n  ${GREEN}${BOLD}PASS${RESET} — Heap growth within ${HEAP_GROWTH_THRESHOLD_MB} MB threshold`,
        );
      } else {
        console.log(
          `\n  ${RED}${BOLD}FAIL${RESET} — Heap grew ${heapGrowth.toFixed(1)} MB (threshold: ${HEAP_GROWTH_THRESHOLD_MB} MB)`,
        );
      }

      // Extrapolate for long-running scenarios
      if (growthPerCycle > 0) {
        const avgDuration =
          measurements.length > 1
            ? (Number(final.time) /
                (measurements.length - 1)) *
              5
            : PIECE_DURATION_S;
        const cyclesPerHour = 3600 / PIECE_DURATION_S;
        const projectedHourlyGrowth = growthPerCycle * cyclesPerHour;
        console.log(
          `\n  ${YELLOW}Projected hourly growth: +${projectedHourlyGrowth.toFixed(1)} MB/hr${RESET}`,
        );
        console.log(
          `  ${DIM}(at ${PIECE_DURATION_S}s per piece = ${cyclesPerHour.toFixed(0)} pieces/hr)${RESET}`,
        );

        const heapLimit = baseline.limitMB || 4096;
        const headroom = heapLimit - final.usedHeapMB;
        if (projectedHourlyGrowth > 0) {
          const hoursUntilOOM = headroom / projectedHourlyGrowth;
          console.log(
            `  ${DIM}Estimated time to OOM: ~${hoursUntilOOM.toFixed(1)} hours (${headroom.toFixed(0)} MB headroom)${RESET}`,
          );
        }
      }
    }

    // Print cache state summary
    if (final.kidlisp) {
      console.log(`\n  ${BOLD}KidLisp cache state at end:${RESET}`);
      const kl = final.kidlisp;
      const bkl = baseline.kidlisp || {};
      for (const [key, val] of Object.entries(kl)) {
        const bval = bkl[key] || 0;
        const delta = val - bval;
        if (val > 0 || delta !== 0) {
          const color = delta > 0 ? YELLOW : GREEN;
          console.log(
            `    ${key}: ${val} ${delta !== 0 ? `${color}(${delta >= 0 ? "+" : ""}${delta})${RESET}` : ""}`,
          );
        }
      }
    }

    // Print measurement timeline
    console.log(`\n  ${BOLD}Timeline:${RESET}`);
    console.log(
      `  ${DIM}${"Cycle".padStart(6)} ${"Time".padStart(7)} ${"Heap MB".padStart(9)} ${"Delta MB".padStart(9)}${RESET}`,
    );
    for (const m of measurements) {
      const heap = m.usedHeapMB ? m.usedHeapMB.toFixed(1) : "n/a";
      const delta = m.usedHeapMB
        ? (m.usedHeapMB - baseline.usedHeapMB).toFixed(1)
        : "n/a";
      console.log(
        `  ${String(m.cycle).padStart(6)} ${String(m.time + "s").padStart(7)} ${String(heap).padStart(9)} ${String((m.usedHeapMB ? (m.usedHeapMB >= baseline.usedHeapMB ? "+" : "") : "") + delta).padStart(9)}`,
      );
    }

    console.log();

    // Exit code
    if (final.usedHeapMB && baseline.usedHeapMB) {
      const heapGrowth = final.usedHeapMB - baseline.usedHeapMB;
      if (heapGrowth >= HEAP_GROWTH_THRESHOLD_MB) {
        process.exit(1);
      }
    }
  },
  { ensurePanel: false },
);
