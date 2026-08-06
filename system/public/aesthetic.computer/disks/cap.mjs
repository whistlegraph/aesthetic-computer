// Cap, 2026.01.28
// Camera piece for recording videos (caps/tapes)
// Workflow: preview camera → tap to start recording → tap to stop → jump to video

/* #region 🏁 TODO
  + Now
  - [] Add duration limit indicator
  - [] Add filters/effects options
  + Later
  - [] Support frame-mode recording
  - [] Add countdown before recording starts
  - [] Picture-in-picture selfie overlay
  + Done
  - [x] Basic camera preview with screen capture
  - [x] Tap to start/stop recording
  - [x] Recording timer display
  - [x] Microphone capture enabled
  - [x] Swap camera button
  - [x] Jump to video piece on completion
#endregion */

import { RecordingTimer, MicLevel } from "./common/cap-ui.mjs";

const { floor, min, max } = Math;

// Recording state
let vid,
  frame,
  facing = "environment",
  capturing = true;

let isRecording = false;
let recordingStarted = false;
let videoInitialized = false;

// UI elements
let swapBtn, flashBtn, timer, micLevel;
let mic; // Microphone reference
let torchAvailable = false;
let torchEnabled = false;
let torchPending = false;

// Recording config
const DEFAULT_DURATION = 30; // Max recording duration in seconds
const MAX_DURATION = 30; // Server limit for tapes
let micConnected = false; // Track if mic is actually connected
let pendingRecordStart = false; // Track if user wants to record but mic isn't ready

// 🔍 Slide-up-to-zoom (while holding record).
// `zoom` scales the camera frame around the screen center. 1 = no zoom.
// `zoomStartY` is the Y position of the touch that began recording.
// `zoomMax` caps how far the user can zoom in via slide.
// A short upward slide zooms quickly. Zoom follows absolute finger position
// instead of touch-event velocity so irregular mobile event timing cannot
// make the camera scale jump ahead and snap back.
let zoom = 1;
let lastSentZoom = 1;
let zoomStartY = null;
const zoomMax = 6;
const zoomMin = 1;
const zoomSensitivity = 120;

// Blit tracking: paint() runs at display rate (up to 120Hz) but camera
// frames arrive at 30–60fps, so the wipe + paste only need to happen when
// the frame object, zoom, or screen size actually changed.
let pastedFrame = null,
  pastedZoom = 0,
  pastedW = 0,
  pastedH = 0;

// 🥾 Boot
function boot({ ui, params, colon, system, rec, notice }) {
  // Piece modules persist between visits. A re-shoot must never inherit the
  // previous camera frame, recorder flags, or pressed controls.
  vid = undefined;
  frame = undefined;
  facing = "environment";
  capturing = true;
  isRecording = false;
  recordingStarted = false;
  videoInitialized = false;
  swapBtn = undefined;
  flashBtn = undefined;
  mic = undefined;
  micConnected = false;
  pendingRecordStart = false;
  torchAvailable = false;
  torchEnabled = false;
  torchPending = false;
  zoom = 1;
  lastSentZoom = 1;
  zoomStartY = null;
  pastedFrame = null;

  // Parse parameters
  if (params[0] === "me" || params[0] === "selfie") facing = "user";
  if (colon[0] === "selfie" || colon[0] === "s") facing = "user";

  // Check if already recording (shouldn't be, but handle edge case)
  if (rec.recording) {
    notice("ALREADY TAPING", ["yellow", "red"]);
    return;
  }

  // Clear any existing recording
  rec.slate();

  // Initialize timer and mic level
  timer = new RecordingTimer();
  micLevel = new MicLevel();
  // Note: Microphone connection happens in beat() after audio context is activated
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
  rec,
  painting,
  recordingUI,
  ui,
}) {
  // Initialize video feed to match screen dimensions (not painting)
  if (!vid) {
    wipe(0);
    vid = video("camera", {
      width: screen.width,
      height: screen.height,
      facing,
      fit: "contain", // Show full camera view (letterboxed if needed)
      recording: true, // Keep the native camera track suitable for MP4 capture.
    });
    videoInitialized = true;
  }

  // Draw the video on each frame
  if (capturing) {
    // Keep preview pixels unchanged. The former per-pixel vignette walked the
    // entire camera frame in the worker for every update.
    frame = vid();
  }

  // Draw the camera frame, zoomed around the screen center. Zoom crops the
  // frame's middle 1/zoom region and scales the crop up to the full screen —
  // the crop path blits per destination pixel, so a fractional zoom stays
  // realtime (a fractional `scale` paste walks every source pixel through
  // color()+box() and slideshows).
  if (frame) {
    // Keep the live preview independent from iOS hardware constraints. The
    // recorder clone receives hardware zoom; AC renders the matching digital
    // crop here so zoom cannot corrupt or stall the source camera track.
    const stale =
      frame !== pastedFrame ||
      zoom !== pastedZoom ||
      screen.width !== pastedW ||
      screen.height !== pastedH;
    if (stale) {
      pastedFrame = frame;
      pastedZoom = zoom;
      pastedW = screen.width;
      pastedH = screen.height;
      wipe(0); // Clear first — the contain fit can letterbox.
      if (zoom > 1.001) {
        const cropW = frame.width / zoom;
        const cropH = frame.height / zoom;
        paste(frame, 0, 0, {
          crop: {
            x: (frame.width - cropW) / 2,
            y: (frame.height - cropH) / 2,
            w: cropW,
            h: cropH,
          },
          width: screen.width,
          height: screen.height,
        });
      } else {
        const offsetX = floor((screen.width - frame.width) / 2);
        const offsetY = floor((screen.height - frame.height) / 2);
        paste(frame, offsetX, offsetY);
      }
    }
  }

  // 🎬 Draw UI elements to a recording UI overlay (NOT captured in tape).
  // BakTok-style hold-to-record: full screen is the touch target. The
  // overlay is just the swap button, mic indicator, hint, and rec border.
  const uiOverlay = painting(screen.width, screen.height, ($) => {
    const centerX = floor(screen.width / 2);
    const bottomY = screen.height - 28;

    // Swap-camera TextButton, top-right. Only shown when multiple cameras
    // are available and we're not actively (or about to be) recording.
    // Lives in the recordingUI overlay so it isn't baked into the tape,
    // and is anchored top-right so the bottom edge stays clear for the
    // hold-to-record gesture.
    if (cameras > 1 && !isRecording && !pendingRecordStart) {
      if (!swapBtn) {
        swapBtn = new ui.TextButton("Swap", { right: 6, top: 6, screen });
        swapBtn.btn.stickyScrubbing = true;
      }
      swapBtn.reposition({ right: 6, top: 6, screen }, "Swap");
      swapBtn.paint($);
    }

    // Rear-camera torch. It stays available while rolling and lives on the
    // uncaptured overlay, like a camera's physical light switch.
    if (facing === "environment" && torchAvailable) {
      const flashRight = cameras > 1 && !isRecording && !pendingRecordStart
        ? (swapBtn?.width || 42) + 12
        : 6;
      const flashLabel = torchEnabled ? "Flash off" : "Flash";
      if (!flashBtn) {
        flashBtn = new ui.TextButton(flashLabel, {
          right: flashRight,
          top: 6,
          screen,
        });
        flashBtn.btn.stickyScrubbing = true;
      }
      flashBtn.reposition({ right: flashRight, top: 6, screen }, flashLabel);
      flashBtn.disabled = torchPending;
      flashBtn.paint($);
    } else {
      flashBtn = undefined;
    }

    // Recording indicator and timer
    if (isRecording && timer) {
      timer.paint($, {});

      // Mic level indicator
      if (mic && micLevel) {
        micLevel.paint($, { x: 8, y: 8, width: 50, height: 4 });
      }
    }

    // Mic connection status indicator (top left) — drawn as a small pixel
    // mic icon instead of the 🎤 emoji, which the default typeface can't
    // render and falls back to "??" glyphs.
    if (!isRecording) {
      const iconX = 4;
      const iconY = 4;
      let iconColor;
      if (!micConnected) {
        iconColor = pendingRecordStart ? [255, 180, 60] : [255, 200, 80];
      } else {
        iconColor = [80, 255, 120];
      }
      drawMicIcon($, iconX, iconY, iconColor);
      if (micConnected) {
        $.ink(80, 255, 120).box(iconX + 10, iconY + 3, 2, 2);
      } else if (pendingRecordStart) {
        $.ink(255, 180, 60).box(iconX + 10, iconY + 1, 1, 6);
      }
    }

    // Hint text — bottom-centered, swaps copy based on state.
    if (pendingRecordStart) {
      $.ink(255, 200, 80, 255).write("waiting for mic...", {
        x: centerX,
        y: bottomY,
        center: "x",
      });
    } else if (!isRecording) {
      $.ink(255, 255, 255, 200).write("hold to cap", {
        x: centerX,
        y: bottomY,
        center: "x",
      });
    } else {
      const zoomedIn = zoom > 1.02;
      const label = zoomedIn
        ? `● rec ${zoom.toFixed(1)}× — release to stop`
        : "● recording — slide up to zoom, release to stop";
      $.ink(255, 80, 80, 220).write(label, {
        x: centerX,
        y: bottomY,
        center: "x",
      });

      // Zoom bar on the right edge: fills upward as zoom increases,
      // gives a tactile sense of the slide gesture without obscuring
      // the camera frame.
      const barH = floor(screen.height * 0.4);
      const barX = screen.width - 6;
      const barY = floor(screen.height / 2 - barH / 2);
      $.ink(255, 255, 255, 40).box(barX, barY, 2, barH);
      const fillRatio = (zoom - zoomMin) / (zoomMax - zoomMin);
      const fillH = floor(barH * fillRatio);
      $.ink(255, 80, 80, 220).box(
        barX,
        barY + (barH - fillH),
        2,
        fillH,
      );
    }

    // Recording border indicator
    if (isRecording) {
      const borderPulse = 0.5 + 0.5 * Math.sin(Date.now() / 200);
      $.ink(255, 60, 60, floor(120 * borderPulse)).box(
        0,
        0,
        screen.width,
        screen.height,
        "inline",
        3,
      );
    }
  });

  // Add the UI overlay to the recording UI layer (NOT captured in tape)
  recordingUI.add("cap-ui", uiOverlay, 0, 0);
}

// Simulation - update mic levels and check recording timeout
function sim({ sound: { microphone }, rec }) {
  // Track mic connection status
  if (mic && microphone.connected && !micConnected) {
    micConnected = true;
    console.log("🎤 Microphone connected for recording");
  }

  // Update mic level display
  if (mic && micConnected && micLevel) {
    mic.poll();
    micLevel.update(mic.amplitude);
  }

  // Check if recording has exceeded max duration
  if (isRecording && timer && timer.isExpired) {
    stopRecording(rec);
  }
}

function startRecording(rec, notice) {
  if (isRecording) return;
  
  // If mic isn't connected yet, queue the recording start
  if (!micConnected) {
    pendingRecordStart = true;
    notice("WAITING FOR MIC...", ["yellow", "black"]);
    console.log("🎤 Microphone not ready, queuing recording start");
    return;
  }

  isRecording = true;
  recordingStarted = true;
  pendingRecordStart = false;

  // Start the visual timer
  timer.start(MAX_DURATION);

  // Start the actual tape recording with microphone
  // Mic MUST be connected before this for audio to be captured
  rec.rolling(
    {
      type: "video",
      pieceName: "cap",
      pieceParams: "",
      originalCommand: "cap",
      intendedDuration: MAX_DURATION,
      frameMode: false,
      frameCount: null,
      cleanMode: false,
      showTezosStamp: false,
      mystery: false,
      freshSession: true,
      avSync: true,
      // Prefer a browser-native encoded stream for camera clips. BIOS falls
      // back to the frame tape when canvas capture or MP4 MediaRecorder is
      // unavailable, so the rest of AC's recording API stays unchanged.
      compactVideo: true,
    },
    (time) => {
      // Recording started callback
      rec.tapeTimerSet(MAX_DURATION, time, false);
    },
  );

  // Recording status is shown via recording UI overlay (not captured in tape)
}

function stopRecording(rec, jump) {
  if (!isRecording) return;

  isRecording = false;
  timer.stop();

  // Cut the recording and jump to video piece
  rec.cut(() => {
    if (jump) jump("video");
  });
}

function act({ event: e, jump, video, cameras, rec, notice, leaving, hud }) {
  // 📊 Camera diagnostics from bios.mjs → piece-runs via console.log.
  if (e.is("camera:debug")) {
    console.log("📷 cam/init:", JSON.stringify(e));
  }
  if (e.is("camera:debug:frame")) {
    console.log("📷 cam/frame:", JSON.stringify(e));
  }
  if (e.is("camera:capabilities")) {
    torchAvailable = e.content?.torch === true;
    torchEnabled = e.content?.enabled === true;
    torchPending = false;
  }
  if (e.is("camera:torch")) {
    torchAvailable = e.content?.available === true;
    torchEnabled = e.content?.enabled === true;
    torchPending = false;
    if (e.content?.error) {
      console.warn("📷 Flash update failed:", e.content.error);
      notice("FLASH UNAVAILABLE", ["yellow", "red"]);
    }
  }
  if (e.is("camera:zoom")) {
    console.log("📷 zoom:", JSON.stringify(e.content));
  }
  if (e.is("recorder:av-sync")) {
    const offset = e.content?.offsetMs;
    console.log(
      `🎞️ cap A/V ${e.content?.aligned ? "PASS" : "FAIL"}: ` +
        `${Number.isFinite(offset) ? offset.toFixed(1) : "?"}ms`,
    );
  }
  if (e.is("recorder:compact")) {
    console.log("🎥 compact:", JSON.stringify(e.content));
  }

  // Handle microphone connection events
  if (e.is("microphone-connect:success")) {
    micConnected = true;
    console.log("🎤 Microphone connected for cap.mjs");

    // If user already requested recording (held screen before mic ready),
    // start it now.
    if (pendingRecordStart && !isRecording) {
      console.log("🎤 Mic ready - starting queued recording");
      startRecording(rec, notice);
    }
  }

  if (e.is("microphone-connect:failure")) {
    micConnected = false;
    pendingRecordStart = false;
    console.warn("🎤 Microphone connection failed:", e.reason);
    notice("MIC DENIED - RECORDING WITHOUT AUDIO", ["yellow", "red"]);
  }

  const flashWasDown = flashBtn?.down === true;
  if (flashBtn && facing === "environment" && torchAvailable) {
    flashBtn.act(e, {
      push: () => {
        torchPending = true;
        video("camera:update", { torch: !torchEnabled });
      },
    });
  }

  // Swap camera button — only fires when the touch is on the swap button
  // itself, so it doesn't compete with the fullscreen hold-to-record gesture.
  if (cameras > 1 && swapBtn && !isRecording && !pendingRecordStart) {
    swapBtn.act(e, {
      push: () => {
        swapBtn.disabled = true;
        const faceTo = facing === "user" ? "environment" : "user";
        vid = video("camera:update", { facing: faceTo });
      },
    });
  }

  // 🎬 Hold-to-record (BakTok pattern): touch anywhere outside the swap
  // button or HUD label starts recording; releasing stops + jumps to video.
  if (e.is("touch") && !leaving()) {
    const onSwapBtn = swapBtn?.btn?.box?.contains(e);
    const onFlashBtn = flashBtn?.btn?.box?.contains(e);
    const onHud = hud?.currentLabel()?.btn?.down;
    if (!onSwapBtn && !onFlashBtn && !onHud && !isRecording && !pendingRecordStart) {
      // Anchor zoom slider at the touch-down Y so the very first drag
      // pixel is treated as zoom = 1, no jumps.
      zoomStartY = e.y;
      startRecording(rec, notice);
    }
  }

  // 🔍 Slide up while holding to zoom in, slide back down to zoom out.
  // We anchor zoomStartY at the original touch and remap the absolute
  // Y delta each draw event — this means the user can swing freely
  // up and down without accumulating drift across pauses.
  if (e.is("draw") && isRecording && zoomStartY !== null) {
    const dy = zoomStartY - e.y; // up is positive
    const target = 1 + dy / zoomSensitivity;
    zoom = Math.max(zoomMin, Math.min(zoomMax, target));
    if (Math.abs(zoom - lastSentZoom) >= 0.04) {
      video("camera:update", { zoom });
      lastSentZoom = zoom;
    }
  }

  if (e.is("lift") && !leaving()) {
    if (isRecording && !flashWasDown) {
      stopRecording(rec, jump);
    } else if (pendingRecordStart && !flashWasDown) {
      // User released before the mic was ready — cancel the queued start.
      pendingRecordStart = false;
      notice("CANCELLED", ["yellow", "black"]);
    }
    zoomStartY = null;
    zoom = 1;
  }

  // Keyboard shortcut: space toggles record (useful for desktop testing).
  if (e.is("keyboard:down: ")) {
    if (!isRecording && !pendingRecordStart) {
      startRecording(rec, notice);
    } else {
      stopRecording(rec, jump);
    }
  }

  if (e.is("keyboard:down:escape")) {
    if (isRecording) {
      stopRecording(rec, jump);
    } else {
      jump("prompt");
    }
  }

  // Camera mode events
  if (e.is("camera:mode:user")) {
    facing = "user";
    torchAvailable = false;
    torchEnabled = false;
    torchPending = false;
    if (swapBtn) swapBtn.disabled = false;
    capturing = true;
  }

  if (e.is("camera:mode:environment")) {
    facing = "environment";
    torchEnabled = false;
    torchPending = false;
    if (swapBtn) swapBtn.disabled = false;
    capturing = true;
  }

  if (e.is("camera:denied")) {
    notice("CAMERA DENIED", ["yellow", "red"]);
    jump("prompt");
  }

  // Handle recording completion events from the system
  if (e.is("recorder:rolling:ended")) {
    isRecording = false;
    timer?.stop();
  }
}

// 💗 Beat - runs after audio context is activated (user interaction)
function beat({ sound: { microphone } }) {
  // Check if already connected from previous session
  if (microphone.connected && !micConnected) {
    micConnected = true;
    mic = microphone;
    console.log("🎤 Microphone already connected");
  }
  
  // Connect mic if not already connected (will trigger microphone-connect:success event)
  if (!mic && !micConnected) {
    console.log("🎤 Connecting microphone...");
    mic = microphone;
    microphone.connect({
      echoCancellation: false,
      noiseSuppression: false,
      autoGainControl: true,
      volume: 1,
    });
  }
}

// Handle leaving the piece while recording
function leave({ rec, jump, video }) {
  if (torchEnabled) video("camera:update", { torch: false });
  if (isRecording) {
    stopRecording(rec, jump);
  }
}

// Tiny mic glyph: capsule + stand. Roughly 8x9 pixels at origin (x, y).
function drawMicIcon($, x, y, color) {
  $.ink(...color);
  $.box(x + 2, y, 4, 5); // mic capsule
  $.box(x + 1, y + 5, 6, 1); // under-capsule lip
  $.box(x + 4, y + 6, 2, 2); // stem
  $.box(x + 2, y + 8, 6, 1); // base
}

export { boot, paint, sim, act, beat, leave };

// No nopaint system — cap records the screen directly via tape infrastructure.
// We keep the standard HUD corner label ("cap") visible; hiding it (nohud)
// left the corner empty and felt broken.
