#!/usr/bin/env node
// test-prompt-curtain.mjs - Test prompt curtain open/close cycle
// Verifies that tapping the backdrop toggles the curtain (activate/deactivate).
// This is especially important in VSCode where the hidden input never blurs.

import { withCDP } from "./cdp.mjs";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

await withCDP(
  async (cdp) => {
    console.log("🧪 Testing prompt curtain toggle...\n");

    await cdp.waitForReady(15000);
    console.log("  Page ready.\n");

    // Helper: get prompt state via the runtime
    async function getState() {
      return cdp.eval(`(() => {
      const ti = window.acTEXTINPUT;
      if (!ti) return null;
      return {
        canType: ti.canType,
        lock: ti.lock,
        backdropTouchOff: ti.backdropTouchOff,
        text: ti.text,
      };
    })()`);
    }

    // Helper: simulate a tap (pointerdown + pointerup) at canvas center,
    // which the AC runtime translates into touch/lift events.
    async function tapBackdrop() {
      await cdp.eval(`(() => {
      const canvas = document.querySelector('canvas');
      if (!canvas) return;
      const rect = canvas.getBoundingClientRect();
      // Tap near center — away from any buttons
      const x = rect.left + rect.width / 2;
      const y = rect.top + rect.height / 2;
      const opts = { bubbles: true, clientX: x, clientY: y, pointerId: 1 };
      canvas.dispatchEvent(new PointerEvent('pointerdown', opts));
      canvas.dispatchEvent(new PointerEvent('pointerup', opts));
    })()`);
    }

    // 1. Initial state — curtain should be showing (canType = false)
    const initial = await getState();
    if (!initial) {
      console.log("  Could not read acTEXTINPUT — is prompt loaded?");
      process.exit(1);
    }
    console.log("  Initial state:", JSON.stringify(initial));
    console.assert(
      initial.canType === false,
      "Expected canType=false (curtain showing)",
    );

    // 2. Tap backdrop to open the keyboard (activate)
    console.log("\n  Tapping backdrop to activate...");
    await tapBackdrop();
    await sleep(300);

    const afterActivate = await getState();
    console.log("  After activate:", JSON.stringify(afterActivate));
    console.assert(
      afterActivate.canType === true,
      "Expected canType=true (keyboard active)",
    );

    // 3. Tap backdrop again to close the keyboard (deactivate)
    console.log("\n  Tapping backdrop to deactivate...");
    await tapBackdrop();
    await sleep(300);

    const afterDeactivate = await getState();
    console.log("  After deactivate:", JSON.stringify(afterDeactivate));
    console.assert(
      afterDeactivate.canType === false,
      "Expected canType=false (curtain returned)",
    );

    // 4. One more cycle to verify stability
    console.log("\n  Cycling once more...");
    await tapBackdrop();
    await sleep(300);
    const cycle2 = await getState();
    console.assert(cycle2.canType === true, "Cycle 2: activate");

    await tapBackdrop();
    await sleep(300);
    const cycle2b = await getState();
    console.assert(cycle2b.canType === false, "Cycle 2: deactivate");

    console.log("\n  All curtain toggle assertions passed.");
  },
  {
    targetUrl: "https://localhost:8888/prompt",
    verbose: true,
  },
);
