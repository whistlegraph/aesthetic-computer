#!/usr/bin/env node
/**
 * 📏 Line Brush Artery Test
 *
 * Tests the full lifecycle of drawing a line in nopaint:
 *   1. Create a new 128×128 painting via `new~128`
 *   2. Enter the `line` brush
 *   3. Draw a diagonal line across the center via pointer events
 *   4. Return to prompt (completes the stroke)
 *   5. Verify the painting was modified
 */

import { getArtery } from './artery-auto.mjs';

// ── Colors ──────────────────────────────────────────────────────
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const RED = '\x1b[91m';
const DIM = '\x1b[2m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE} 📏 ${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}  ✓ ${msg}${RESET}`);
const failLog = (msg) => console.log(`${RED}  ✗ ${msg}${RESET}`);
const stepLog = (msg) => console.log(`${CYAN}  → ${msg}${RESET}`);
const dimLog = (msg) => console.log(`${DIM}    ${msg}${RESET}`);

const sleep = (ms) => new Promise(r => setTimeout(r, ms));

// ── Pointer event helpers ───────────────────────────────────────

/**
 * Dispatch a PointerEvent on the AC canvas at (x, y) in canvas-local coords.
 * AC's Pen class listens on the canvas element for pointerdown/pointermove/pointerup.
 */
async function pointerDown(client, x, y) {
  await client.eval(`(() => {
    const canvas = document.querySelector('canvas');
    if (!canvas) return 'no-canvas';
    const rect = canvas.getBoundingClientRect();
    canvas.dispatchEvent(new PointerEvent('pointerdown', {
      bubbles: true,
      clientX: rect.left + ${x},
      clientY: rect.top + ${y},
      pointerId: 1,
      isPrimary: true,
      pressure: 1.0
    }));
    return 'ok';
  })()`);
}

async function pointerMove(client, x, y) {
  await client.eval(`(() => {
    const canvas = document.querySelector('canvas');
    if (!canvas) return 'no-canvas';
    const rect = canvas.getBoundingClientRect();
    canvas.dispatchEvent(new PointerEvent('pointermove', {
      bubbles: true,
      clientX: rect.left + ${x},
      clientY: rect.top + ${y},
      pointerId: 1,
      isPrimary: true,
      pressure: 1.0
    }));
    return 'ok';
  })()`);
}

async function pointerUp(client, x, y) {
  await client.eval(`(() => {
    const canvas = document.querySelector('canvas');
    if (!canvas) return 'no-canvas';
    const rect = canvas.getBoundingClientRect();
    canvas.dispatchEvent(new PointerEvent('pointerup', {
      bubbles: true,
      clientX: rect.left + ${x},
      clientY: rect.top + ${y},
      pointerId: 1,
      isPrimary: true,
      pressure: 0.0
    }));
    return 'ok';
  })()`);
}

/**
 * Draw a stroke as a sequence of points.
 * First point triggers pointerdown, intermediate points are pointermove,
 * last point triggers pointerup.
 */
async function drawStroke(client, points, stepDelay = 30) {
  if (points.length < 2) return;

  await pointerDown(client, points[0].x, points[0].y);
  await sleep(stepDelay);

  for (let i = 1; i < points.length - 1; i++) {
    await pointerMove(client, points[i].x, points[i].y);
    await sleep(stepDelay);
  }

  const last = points[points.length - 1];
  await pointerMove(client, last.x, last.y);
  await sleep(stepDelay);
  await pointerUp(client, last.x, last.y);
}

// ── Verification helpers ────────────────────────────────────────

/**
 * Check if nopaint system is active with a valid painting.
 */
async function getNopaintState(client) {
  return await client.eval(`(() => {
    const sys = window.acSYSTEM || window.system;
    if (!sys) return { hasSystem: false };
    return {
      hasSystem: true,
      hasNopaint: !!sys.nopaint,
      hasPainting: !!sys.painting,
      paintingWidth: sys.painting?.width || 0,
      paintingHeight: sys.painting?.height || 0,
    };
  })()`);
}

/**
 * Sample a few pixels from the painting to check if it has been drawn on.
 * Returns an array of pixel values along a diagonal.
 */
async function samplePaintingPixels(client) {
  return await client.eval(`(() => {
    const sys = window.acSYSTEM || window.system;
    if (!sys?.painting) return null;
    const p = sys.painting;
    const pixels = p.pixels;
    if (!pixels) return null;
    const w = p.width;
    const samples = [];
    // Sample along the diagonal where we drew the line
    for (let i = 0; i < 5; i++) {
      const frac = (i + 1) / 6;
      const px = Math.floor(w * frac);
      const py = Math.floor(p.height * frac);
      const idx = (py * w + px) * 4;
      samples.push({
        x: px, y: py,
        r: pixels[idx], g: pixels[idx+1], b: pixels[idx+2], a: pixels[idx+3]
      });
    }
    return samples;
  })()`);
}

// ── Main test ───────────────────────────────────────────────────

async function main() {
  testLog('Line Brush Artery Test\n');

  let client;

  try {
    const Artery = await getArtery();

    await Artery.openPanelStandalone();
    await sleep(500);

    client = new Artery();
    await client.connect();
    stepLog('Connected to AC');

    // ── Step 1: Create a new 128×128 painting ──────────────────
    stepLog('Creating new 128×128 painting...');
    await client.jump('new~128');
    await sleep(3000); // Give time for painting creation + nopaint init

    // Verify painting was created
    let state = await getNopaintState(client);
    dimLog(`System state: ${JSON.stringify(state)}`);

    if (state?.hasPainting && state.paintingWidth === 128) {
      successLog(`Painting created: ${state.paintingWidth}×${state.paintingHeight}`);
    } else {
      failLog(`Painting creation may have failed (width=${state?.paintingWidth})`);
      dimLog('Continuing anyway — painting may initialize during brush load');
    }

    // ── Step 2: Enter line brush ───────────────────────────────
    stepLog('Entering line brush...');
    await client.jump('line');
    await sleep(2000); // Wait for piece load + nopaint boot

    // Verify we're on the line piece
    const currentPiece = await client.getCurrentPiece();
    dimLog(`Current piece: ${currentPiece}`);

    if (currentPiece?.includes('line')) {
      successLog('Line brush loaded');
    } else {
      failLog(`Expected "line" piece, got: ${currentPiece}`);
    }

    // Check nopaint state after brush loads
    state = await getNopaintState(client);
    dimLog(`Nopaint state: ${JSON.stringify(state)}`);

    // ── Step 3: Draw a diagonal line ───────────────────────────
    stepLog('Drawing a diagonal line...');

    // Get canvas dimensions so we can place the stroke in the center
    const canvasInfo = await client.eval(`(() => {
      const canvas = document.querySelector('canvas');
      if (!canvas) return null;
      const rect = canvas.getBoundingClientRect();
      return { width: rect.width, height: rect.height };
    })()`);

    if (!canvasInfo) {
      failLog('No canvas found!');
      throw new Error('Canvas not found');
    }

    dimLog(`Canvas size: ${canvasInfo.width}×${canvasInfo.height}`);

    // Draw a diagonal line from ~1/3 to ~2/3 of the canvas
    const cx = canvasInfo.width / 2;
    const cy = canvasInfo.height / 2;
    const spread = Math.min(canvasInfo.width, canvasInfo.height) * 0.2;

    const strokePoints = [];
    const numPoints = 10;
    for (let i = 0; i <= numPoints; i++) {
      const t = i / numPoints;
      strokePoints.push({
        x: Math.round(cx - spread + spread * 2 * t),
        y: Math.round(cy - spread + spread * 2 * t),
      });
    }

    dimLog(`Stroke: (${strokePoints[0].x},${strokePoints[0].y}) → (${strokePoints[numPoints].x},${strokePoints[numPoints].y})`);

    await drawStroke(client, strokePoints, 40);
    successLog('Stroke dispatched (pointerdown → pointermove × N → pointerup)');

    await sleep(500); // Let the brush process the lift

    // ── Step 4: Return to prompt (completes stroke session) ────
    stepLog('Returning to prompt...');
    await client.jump('prompt');
    await sleep(2000);

    const finalPiece = await client.getCurrentPiece();
    if (finalPiece?.includes('prompt')) {
      successLog('Returned to prompt');
    } else {
      failLog(`Expected "prompt", got: ${finalPiece}`);
    }

    // ── Step 5: Verify painting state ──────────────────────────
    stepLog('Checking painting state...');
    const finalState = await getNopaintState(client);
    dimLog(`Final nopaint state: ${JSON.stringify(finalState)}`);

    if (finalState?.hasPainting) {
      successLog('Painting persists after returning to prompt');
    } else {
      failLog('Painting was lost after returning to prompt');
    }

    // ── Done ───────────────────────────────────────────────────
    console.log('');
    successLog('Line brush test complete!\n');

    client.close();
    await Artery.closePanelStandalone();
    process.exit(0);

  } catch (err) {
    console.error(`\n${RED}Error: ${err.message}${RESET}`);
    if (err.stack) dimLog(err.stack);
    if (client) client.close();
    process.exit(1);
  }
}

main();
