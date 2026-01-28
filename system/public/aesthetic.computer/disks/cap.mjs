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

import {
  CaptureButton,
  SwapButton,
  RecordingTimer,
  MicLevel,
  sounds,
} from "./common/cap-ui.mjs";

const { floor, min, max, sqrt } = Math;

// Recording state
let vid,
  frame,
  facing = "environment",
  capturing = true;

let isRecording = false;
let recordingStarted = false;
let videoInitialized = false;

// UI elements
let captureBtn, swapBtn, timer, micLevel;
let mic; // Microphone reference

// Recording config
const DEFAULT_DURATION = 30; // Max recording duration in seconds
const MAX_DURATION = 30; // Server limit for tapes
let micConnected = false; // Track if mic is actually connected
let pendingRecordStart = false; // Track if user wants to record but mic isn't ready

// 🥾 Boot
function boot({ ui, params, colon, system, rec, notice }) {
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

  // Draw the video on each frame
  if (capturing) {
    frame = vid(function shader({ x, y }, c) {
      // Light vignette effect based on frame dimensions
      const cx = screen.width / 2;
      const cy = screen.height / 2;
      const maxDist = sqrt(cx * cx + cy * cy);
      const dist = sqrt((x - cx) ** 2 + (y - cy) ** 2);
      const vignette = 1 - (dist / maxDist) * 0.1;

      c[0] = clamp(floor(c[0] * vignette), 0, 255);
      c[1] = clamp(floor(c[1] * vignette), 0, 255);
      c[2] = clamp(floor(c[2] * vignette), 0, 255);
    });
  }

  // Paste the video centered on screen
  if (frame) {
    const offsetX = floor((screen.width - frame.width) / 2);
    const offsetY = floor((screen.height - frame.height) / 2);
    wipe(0); // Clear first
    paste(frame, offsetX, offsetY);
  }

  // 🎬 Draw UI elements to a recording UI overlay (NOT captured in tape)
  // This ensures the camera feed is recorded but not the buttons/timer
  const uiOverlay = painting(screen.width, screen.height, ($) => {
    const centerX = floor(screen.width / 2);
    const bottomY = screen.height - 40;

    // Capture/record button (large circle at bottom center)
    if (!captureBtn) {
      captureBtn = new CaptureButton({
        x: centerX,
        y: bottomY,
        radius: 28,
        type: "cap",
      });
    }
    captureBtn.reposition({ x: centerX, y: bottomY });
    captureBtn.recording = isRecording;
    captureBtn.disabled = pendingRecordStart; // Disable button while waiting for mic
    captureBtn.paint($);

    // Swap button (bottom right, only if multiple cameras and not recording)
    if (cameras > 1 && !isRecording && !pendingRecordStart) {
      if (!swapBtn) {
        swapBtn = new SwapButton({ x: 0, y: 0, width: 48, height: 20 });
      }
      swapBtn.reposition({ right: 6, bottom: 6, screen });
      swapBtn.paint($);
    }

    // Recording indicator and timer
    if (isRecording && timer) {
      timer.paint($, {});

      // Mic level indicator
      if (mic && micLevel) {
        micLevel.paint($, { x: 8, y: 8, width: 50, height: 4 });
      }
    }
    
    // Mic connection status indicator (top left)
    if (!micConnected && !isRecording) {
      const micIcon = pendingRecordStart ? "🎤⏳" : "🎤";
      $.ink(255, 200, 80).write(micIcon, { x: 4, y: 4 });
    } else if (micConnected && !isRecording) {
      $.ink(80, 255, 120).write("🎤✓", { x: 4, y: 4 });
    }

    // Hint text (positioned ABOVE the button, not below)
    const hintY = bottomY - 46;
    if (pendingRecordStart) {
      $.ink(255, 200, 80, 255).write("waiting for mic...", {
        x: centerX,
        y: hintY,
        center: "x",
      });
    } else if (!isRecording) {
      $.ink(255, 255, 255, 160).write("tap to cap", {
        x: centerX,
        y: hintY,
        center: "x",
      });
    } else {
      $.ink(255, 80, 80, 200).write("● REC - tap to stop", {
        x: centerX,
        y: hintY,
        center: "x",
      });
    }

    // Recording border indicator
    if (isRecording) {
      const borderPulse = 0.5 + 0.5 * Math.sin(Date.now() / 200);
      $.ink(255, 60, 60, floor(100 * borderPulse)).box(
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

function startRecording(rec, sound, notice) {
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
  sounds.recordStart(sound);

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
    },
    (time) => {
      // Recording started callback
      rec.tapeTimerSet(MAX_DURATION, time, false);
    },
  );

  notice("RECORDING", ["red", "white"]);
}

function stopRecording(rec, sound, jump) {
  if (!isRecording) return;

  isRecording = false;
  timer.stop();

  if (sound) sounds.recordStop(sound);

  // Cut the recording and jump to video piece
  rec.cut(() => {
    if (jump) jump("video");
  });
}

function act({ event: e, jump, video, cameras, sound, rec, notice, leaving, hud }) {
  // Handle microphone connection events
  if (e.is("microphone-connect:success")) {
    micConnected = true;
    console.log("🎤 Microphone connected for cap.mjs");
    
    // If user already requested recording, start it now
    if (pendingRecordStart && !isRecording) {
      console.log("🎤 Mic ready - starting queued recording");
      startRecording(rec, sound, notice);
    }
  }
  
  if (e.is("microphone-connect:failure")) {
    micConnected = false;
    pendingRecordStart = false;
    console.warn("🎤 Microphone connection failed:", e.reason);
    notice("MIC DENIED - RECORDING WITHOUT AUDIO", ["yellow", "red"]);
    // Could still allow recording without mic if desired
  }
  
  // Capture/record button interaction
  if (captureBtn) {
    captureBtn.act(e, {
      down: () => sounds.down(sound),
      push: () => {
        if (!isRecording && !pendingRecordStart) {
          startRecording(rec, sound, notice);
        } else if (isRecording) {
          stopRecording(rec, sound, jump);
        } else if (pendingRecordStart) {
          // Cancel pending recording
          pendingRecordStart = false;
          notice("CANCELLED", ["yellow", "black"]);
        }
      },
    });
  }

  // Swap camera button (only when not recording)
  if (cameras > 1 && swapBtn && !isRecording) {
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

  // Touch anywhere (not on buttons) to start/stop
  if (e.is("lift") && !leaving()) {
    const onCaptureBtn = captureBtn?.contains(e.x, e.y);
    const onSwapBtn = swapBtn?.contains(e.x, e.y);
    const onHud = hud?.currentLabel()?.btn?.down;

    if (!onCaptureBtn && !onSwapBtn && !onHud) {
      if (!isRecording && !pendingRecordStart) {
        startRecording(rec, sound, notice);
      } else if (isRecording) {
        stopRecording(rec, sound, jump);
      } else if (pendingRecordStart) {
        pendingRecordStart = false;
        notice("CANCELLED", ["yellow", "black"]);
      }
    }
  }

  // Keyboard shortcuts
  if (e.is("keyboard:down:enter") || e.is("keyboard:down: ")) {
    if (!isRecording && !pendingRecordStart) {
      startRecording(rec, sound, notice);
    } else {
      stopRecording(rec, sound, jump);
    }
  }

  if (e.is("keyboard:down:escape")) {
    if (isRecording) {
      // Stop recording and go to video
      stopRecording(rec, sound, jump);
    } else {
      // Just exit
      jump("prompt");
    }
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
    });
  }
}

// Handle leaving the piece while recording
function leave({ rec, jump }) {
  if (isRecording) {
    stopRecording(rec, null, jump);
  }
}

export { boot, paint, sim, act, beat, leave };

// No nopaint system - cap records the screen directly via tape infrastructure
export const nohud = true; // Minimal HUD for camera view
