// Snap, 2026.01.28
// Camera piece for taking still photos (paintings/snaps)
// Simple workflow: preview camera → tap to capture → save to painting

/* #region 🏁 TODO
  + Now
  - [] Add filters/effects options
  - [] Add countdown timer option
  + Later
  - [] Add selfie mode with face detection
  - [] Support burst mode
  - [] Add crop/frame options before saving
  + Done
  - [x] Basic camera preview
  - [x] Tap anywhere to capture
  - [x] Swap camera button
  - [x] Flash effect on capture
  - [x] Jump to prompt with painting saved
#endregion */

import {
  CaptureButton,
  SwapButton,
  FlashEffect,
  sounds,
} from "./common/cap-ui.mjs";

const { floor, min, max } = Math;

let vid,
  frame,
  facing = "environment",
  capturing = true,
  captured = false;

let captureBtn, swapBtn, flash;
let videoInitialized = false;

// 🥾 Boot
function boot({ ui, params, colon, system }) {
  // Parse parameters
  if (params[0] === "me" || params[0] === "selfie") facing = "user";
  if (colon[0] === "selfie" || colon[0] === "s") facing = "user";

  flash = new FlashEffect();
}

// 🎨 Paint
function paint({
  api,
  wipe,
  ink,
  paste,
  video,
  cameras,
  system,
  screen,
  num: { randIntRange, clamp, rand },
}) {
  // Initialize video feed to match screen dimensions (not painting)
  if (!vid) {
    wipe(0);
    vid = video("camera", {
      width: screen.width,
      height: screen.height,
      facing,
      fit: "contain", // Show full camera view (letterboxed if needed)
    });
    videoInitialized = true;
  }

  // Draw the video on each frame, filling the screen
  if (capturing && !captured) {
    frame = vid(function shader({ x, y }, c) {
      // Subtle sparkle effect
      if (rand() > 0.995) {
        c[0] = clamp(c[0] + randIntRange(30, 80), 0, 255);
        c[1] = clamp(c[1] + randIntRange(30, 80), 0, 255);
        c[2] = clamp(c[2] + randIntRange(30, 80), 0, 255);
      }
    });
  }

  // Paste the video centered on screen (video may have different AR)
  if (frame) {
    // Center the frame on screen
    const offsetX = floor((screen.width - frame.width) / 2);
    const offsetY = floor((screen.height - frame.height) / 2);
    wipe(0); // Clear first
    paste(frame, offsetX, offsetY);
  }

  // Draw UI
  const centerX = floor(screen.width / 2);
  const bottomY = screen.height - 40;

  // Capture button (large circle at bottom center)
  if (!captureBtn) {
    captureBtn = new CaptureButton({
      x: centerX,
      y: bottomY,
      radius: 28,
      type: "snap",
    });
  }
  captureBtn.reposition({ x: centerX, y: bottomY });
  captureBtn.disabled = captured;
  captureBtn.paint(api);

  // Swap button (top right, only if multiple cameras)
  if (cameras > 1) {
    if (!swapBtn) {
      swapBtn = new SwapButton({ x: 0, y: 0, width: 48, height: 20 });
    }
    swapBtn.reposition({ right: 6, bottom: 6, screen });
    swapBtn.disabled = captured;
    swapBtn.paint(api);
  }

  // Hint text
  if (!captured) {
    ink(255, 255, 255, 160).write("tap to snap", {
      x: centerX,
      y: bottomY + 36,
      center: "x",
    });
  } else {
    ink(255, 255, 100, 200).write("SAVED", {
      x: centerX,
      y: bottomY + 36,
      center: "x",
    });
  }

  // Flash effect overlay
  flash.update();
  flash.paint(api);
}

// Bake the captured frame to the painting
function bake({ paste }) {
  if (captured && frame) {
    paste(frame);
  }
}

function act({ event: e, jump, video, cameras, sound, notice, leaving, hud }) {
  // Capture button interaction
  if (captureBtn && !captured) {
    captureBtn.act(e, {
      down: () => sounds.down(sound),
      push: () => {
        // Capture the current frame
        sounds.shutter(sound);
        flash.trigger();
        captured = true;
        capturing = false;
        notice("SNAP!", ["yellow", "white"]);

        // Auto-jump to prompt after a short delay
        setTimeout(() => {
          jump("prompt");
        }, 500);
      },
    });
  }

  // Swap camera button
  if (cameras > 1 && swapBtn && !captured) {
    swapBtn.act(e, {
      down: () => sounds.down(sound),
      push: () => {
        sounds.push(sound);
        swapBtn.disabled = true;
        const faceTo = facing === "user" ? "environment" : "user";
        vid = video("camera:update", { facing: faceTo });
      },
    });
  }

  // Touch anywhere (not on buttons) to capture
  if (e.is("touch") && !captured) {
    const onCaptureBtn = captureBtn?.contains(e.x, e.y);
    const onSwapBtn = swapBtn?.contains(e.x, e.y);

    if (!onCaptureBtn && !onSwapBtn) {
      sounds.down(sound);
    }
  }

  if (e.is("lift") && !captured && !leaving()) {
    const onCaptureBtn = captureBtn?.contains(e.x, e.y);
    const onSwapBtn = swapBtn?.contains(e.x, e.y);

    if (!onCaptureBtn && !onSwapBtn && !hud.currentLabel().btn.down) {
      // Tap anywhere to capture
      sounds.shutter(sound);
      flash.trigger();
      captured = true;
      capturing = false;
      notice("SNAP!", ["yellow", "white"]);

      setTimeout(() => {
        jump("prompt");
      }, 500);
    }
  }

  // Keyboard shortcuts
  if (e.is("keyboard:down:enter") || e.is("keyboard:down: ")) {
    if (!captured) {
      sounds.shutter(sound);
      flash.trigger();
      captured = true;
      capturing = false;
      notice("SNAP!", ["yellow", "white"]);

      setTimeout(() => {
        jump("prompt");
      }, 500);
    }
  }

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) {
    jump("prompt");
  }

  // Camera mode events
  if (e.is("camera:mode:user")) {
    facing = "user";
    if (swapBtn) swapBtn.disabled = false;
    capturing = true;
  }

  if (e.is("camera:mode:environment")) {
    facing = "environment";
    if (swapBtn) swapBtn.disabled = false;
    capturing = true;
  }

  if (e.is("camera:denied")) {
    notice("CAMERA DENIED", ["yellow", "red"]);
    jump("prompt");
  }
}

export { boot, paint, bake, act };

export const system = "nopaint:bake-on-leave";
export const nohud = true; // Minimal HUD for camera view
