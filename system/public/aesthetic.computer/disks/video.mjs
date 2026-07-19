// Video, 23.1.26.12.01
// Play back and be able to export / upload a recorded video.

/* #region ✏️ todo
  + Now
  - [-] Add stamp on export.
  - [] Would I be able hold onto / store the recording similar to a painting
       on the client? So that way refreshing the page can work...
  + Done
  - [x] Add "fuzzy" intro instead of "Get ready..." or a delay or something.
       (Test Pattern)
  - [x] Add "export" or "save" button.
    - [x] Transcode upon tapping export.
      - [x] Reveal download options, based on
           device capability.
  - [x] Factor out / comment or modify th                currentExportType = "";
            } else {
              // Reset flags if no frames availabled video overlay UI code.
  - [x] Fix recording visibility on iOS!
    - [x] Unhandled Promise Rejection
  - [x] Pressing "back" and then going "forward" should restart your recording,
  - [x] Show preview of video immediately when the piece loads if a video
        actually exists.
  - [x] It can be shown in a special DOM layer and this piece can have a clear
        backdrop with a few controls.
  - [x] How to know when we have started printing immediately?
  + Later
  - [] Make multi-scene recordings by not stopping a recording on back / 
       intentionally do multi-scene stuff, perhaps with different configurations
       of images even according to a rhythm, with preloaded piece source code.
  - [] Take advantage of the dead time / transcoding time.
    - [] Show a little game or helpful hint. (💡 @alex)
#endregion */

let postBtn; // POST button for uploading tape
// gif/mp4/zip exports temporarily deprecated in favor of a single Back
// button so cap → back → cap → back is fast. The action handlers below
// stay in place (they no-op because the button refs are undefined) so
// we can re-enable later without diffing this whole file.
let gifBtn;
let mp4Btn;
let zipBtn;
let backBtn; // jumps back to cap so the user can re-shoot quickly

let isPrinting = false;
let isPostingTape = false;
let currentExportType = ""; // Track what's being exported
let currentExportPhase = ""; // Track current phase of export
let exportStatusMessage = ""; // Detailed status message
let printProgress = 0; // Export progress (0-1)
let ellipsisTicker;
let postedTapeCode = null; // Store the tape code after posting for button transformation
let frameCount = 0; // Frame counter for animations like button blinking

let isExportingGIF = false;
let isExportingFrames = false;
let isExportingWebP = false;
let isExportingAnimWebP = false;
let isExportingAPNG = false;

let isLoadingTape = false; // Loading a tape from !code
let tapeLoadPhase = ""; // download, frames, audio
let tapeLoadProgress = 0; // 0-1
let tapeLoadMessage = ""; // Status message

// Video-backed tape (kind:"mp4") playback state. When true the clip plays via
// a DOM <video> in the #underlay (managed by bios) rather than the frame path.
let isMp4Tape = false;
let mp4Playing = true; // best-effort autoplay starts playing
let mp4Progress = 0; // 0-1, from bios tape:playback-progress
let mp4BackBtn = null; // Back-to-chat/prompt button for MP4 tapes

// Tape info from bios
let tapeInfo = null; // { frameCount, totalDuration, hasAudio }
const MAX_TAPE_DURATION = 30; // Must match server limit

// API references needed by receive() function
let apiJump = null; // Stored from boot() for use in receive()
let apiSend = null; // Stored from boot() for sending messages to BIOS

// AudioContext state from BIOS (since sound.bios is false for video piece)
let audioContextState = "suspended"; // suspended, running, closed
let hasAudioContext = false;

// Scrubbing state (STAMPLE-style speed-based scrubbing)
let isScrubbing = false;
let scrubSpeed = 0; // Playback speed: 0=paused, 1=fwd, -1=rev (-4 to +4)
let scrubCurrentProgress = 0; // Position during scrub (0-1)
let wasPlayingBeforeScrub = false;
let inertiaActive = false;
let tapeWaveform = null; // Audio waveform data for visualization
let scrubStripBtn = null; // STAMPLE-like sticky scrub area
let scrubMoved = false; // Prevent tap toggle after a real scrub gesture
let scrubAudioSpeed = 1; // Local estimate of tape audio sampleSpeed

// 🖐️ Touch brake: holding a finger on a playing tape slows it to a stop
// like a record under a hand (pitch bending down with it); letting go spins
// it back up to 1×. A quick tap (released before the brake engages) still
// toggles play/pause.
let brakeHolding = false; // Finger down, speed decaying toward 0
let brakeResume = false; // Finger lifted, speed ramping back to 1
let holdTime = 0; // Seconds the finger has been down without dragging
const BRAKE_ENGAGE_TIME = 0.09; // Hold this long before braking

// 🪀 Elastic drag: displacement from the touch anchor stretches the rate
// around 1× — pull right to speed up, left through 0 into reverse; release
// springs back to 1×. Rate is the instrument, not position. A swipe out of
// a brake hold instead anchors at 0×: swipe direction = playback direction,
// and release glides on inertia.
let elasticAnchorX = null;
let elasticBase = 1; // 1 = stretching around play; 0 = thrown from a halt

// 🖱️ Two-finger scroll drives the scrub too (no tap-drag needed); it eases
// back to 1× when the wheel goes quiet.
let scrollScrubbing = false;
let lastScrollAt = 0;

// 👇 Tap dip: a single tap presses the platter for an instant — the rate
// dips toward 0 and springs back up, one quick audible wow.
let tapDipTime = -1; // ≥0 = seconds elapsed in the dip animation
const TAP_DIP_DURATION = 0.35;
const TAP_DIP_DEPTH = 0.9; // Bottom of the dip reaches 0.1×
let dipBase = 1; // The rate a dip departs from and returns to

// 🅿️ Parked rate: letting go of any gesture keeps the CURRENT rate instead
// of snapping back to 1× — the rate is a setting you performed. Release
// within 0.05 of 1× hands back to normal playback; spacebar always resets.
let sustained = false;
let resumeTarget = 1; // The pre-gesture rate: brake, wheel, and dip return here
const PARK_SNAP = 0.05; // Within this of 1×, a release ends the scrub cleanly

// 🎰 Wheel: a FLICK release lets the platter run free, then it eases down
// like a prize wheel to the pre-flick rate. A gentle release parks instead.
let wheelActive = false;
let flickVel = 0; // Low-passed drag velocity, for telling flicks from lets-go
const FLICK_THRESHOLD = 5; // px/event of recent drag velocity
const FLICK_KICK = 0.12; // Extra rate per px of flick velocity

// 📟 Steady-rate dial: dragging the top-right rate readout vertically sets
// a held steady rate — friction leaves it alone until it's brought back to
// 1× (or spacebar resets).
let rateBtn = null;
let steadyHold = false;
let rateDragStartY = 0;
let rateDragStartRate = 1;

// 🧭 Gesture vector debug: the grab anchor and current finger, so the
// stretch reads as a drawn vector (direction + energy) on screen.
let elasticAnchorY = null;
let penX = null;
let penY = null;

// 🎛️ Deck keys (Pioneer-style): ← → beat-jump; holding ↑/↓ chop-repeats a
// beat fraction. chopActive holds the slice length in beats (0 = off).
let chopActive = 0;
let chopStart = 0;
const BEAT_SEC = 0.5; // 120 BPM

// Scrub physics runs on measured wall time, not an assumed tick rate —
// sim ticks at ~120Hz here, and a fixed 1/60 dt made every commanded
// speed land at roughly half its real value.
let scrubLastSimTime = 0;

// 🧪 Synthtape state (`video scrub`): bios fabricates a deterministic test
// tape so scrub feel can be tuned without recording anything. `auto` runs a
// scripted gesture exercise that grades the physics (🧪 SYNTHSCRUB logs).
let isSynthTape = false;
let synthAuto = false;
let autoScript = null; // Remaining autopilot segments
let autoSeg = null; // Segment currently running
let autoTimer = 0; // Frames elapsed in the current segment
let autoSegStartProgress = 0;
let autoSegStartTime = 0; // Wall clock at segment start, for effective-speed math
let autoDtSum = 0; // Wall time the physics actually integrated this segment
let autoPhysTicks = 0; // Physics executions this segment
let autoSpeedMin = Infinity; // Observed scrubSpeed range this segment
let autoSpeedMax = -Infinity;
let autoPrevPos = null; // Last observed position, for per-tick step anomaly tracing
let autoAnomalies = 0;
let autoMotionSum = 0; // True signed motion integral — wrap-proof, unlike Δposition
let autoChecks = []; // Pass/fail results for the final 🧪 SYNTHSCRUB report

const buttonBottom = 6;
const buttonLeft = 6;
const buttonSpacing = 6;

const debug = false; // Toggle verbose logging for tape export flow

// Handle audio context resume - BIOS will automatically start audio when context becomes available
async function handleAudioContextAndPlay(sound, rec, triggerRender) {
  console.log("🎵 Requesting BIOS to resume audio context - BIOS will auto-start tape audio");
  
  // Send resume request - BIOS will automatically detect tape playing and start audio
  if (apiSend) {
    apiSend({
      type: "audio-context:resume-request",
      content: {
        userGesture: true,
        source: "video-piece-first-tap",
        forceResume: true // Force resume even if state appears to be running
      }
    });
  } else {
    console.warn("🎵 No apiSend available - cannot request audio context resume");
  }
  
  console.log("🎵 Audio context resume requested - BIOS will handle the rest");
  triggerRender();
}

function ensureScrubStripButton(ui, screen, enabled) {
  if (!enabled) {
    scrubStripBtn = null;
    return;
  }
  const bottomControlsHeight = 28;
  const h = Math.max(1, screen.height - bottomControlsHeight);
  if (!scrubStripBtn) {
    scrubStripBtn = new ui.Button(0, 0, screen.width, h);
    scrubStripBtn.stickyScrubbing = true;
    scrubStripBtn.noRolloverActivation = true;
  }
  scrubStripBtn.box.x = 0;
  scrubStripBtn.box.y = 0;
  scrubStripBtn.box.w = screen.width;
  scrubStripBtn.box.h = h;
}

function nudgeTapeAudioSpeed(send, targetSpeed) {
  // Absolute rate: the worklet is set to exactly this speed, so repeated
  // gestures can never accumulate drift the way relative shifts did.
  const clampedTarget = Math.max(-24, Math.min(24, targetSpeed));
  if (Math.abs(clampedTarget - scrubAudioSpeed) < 0.0005) return;
  send({ type: "tape:audio-rate", content: clampedTarget });
  scrubAudioSpeed = clampedTarget;
}

// Enhanced progress tracking
let exportStartTime = 0; // When export started
let progressHistory = []; // Track progress over time for ETA calculation
let currentStatusAlert = null; // Track current status alert for updates

// Completion message display
let completionMessage = ""; // Message to show when export completes
let completionMessageTimer = 0; // Timer for how long to show completion message

let canvasTransparencyEnsured = false; // Ensure DOM canvas stays transparent
let requestPaint = () => {};

// Progress bar mode configuration
// true = Use native extended progress bar (old mode)
// false = Use baked-in VHS progress bar (new tape-style mode with disk.mjs rendering)
const useExtendedProgressBar = true;



// Request WebP creation from main thread (document not available in worker)
// Note: Optimized to handle large frame counts without memory issues
async function createAnimatedWebP(frames, send) {
  try {
    if (frames.length === 0) return null;
    
    // Calculate frame durations in milliseconds
    const frameData = [];
    for (let i = 0; i < frames.length; i++) {
      const frame = frames[i];
      const timestamp = frame[0]; // timestamp is at index 0
      const imageData = frame[1];  // imageData is at index 1
      
      // Calculate duration until next frame (or default to 16.67ms for 60fps if last frame)
      let duration = 16.67; // Default ~60fps
      if (i < frames.length - 1) {
        duration = frames[i + 1][0] - timestamp;
      }
      
      frameData.push({
        timestamp: timestamp,
        duration: Math.max(10, duration), // Minimum 10ms duration
        width: imageData.width,
        height: imageData.height,
        data: imageData.data // Keep as Uint8ClampedArray for transfer efficiency
      });
    }
    
    // Send message to main thread to create WebP
    return new Promise((resolve) => {
      // Send the create-animated-webp message to main thread
      send({
        type: "create-animated-webp",
        content: {
          frames: frameData
        }
      });
      
      // The main thread will handle the WebP creation and download
      // We resolve immediately since the download happens asynchronously
      resolve(true);
    });
    
  } catch (error) {
    console.error("Error preparing WebP data:", error);
    return null;
  }
}

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, rec, gizmo, jump, notice, store, params, send, hud }) {
  // Store API references for use in receive() function
  apiJump = jump;
  apiSend = send;
  
  if (rec.recording) {
    notice("TAPING", ["yellow", "red"]);
    jump("prompt");
    return;
  }
  wipe(0, 0, 0, 0); // Transparent from the start so underlay shows immediately

  // Reset all export states on boot
  isPrinting = false;
  isPostingTape = false;
  currentExportType = "";
  printed = false;
  postedTapeCode = null; // Reset tape code from previous session
  tapeInfo = null; // Reset tape info for new recording
  isScrubbing = false;
  inertiaActive = false;
  brakeHolding = false;
  brakeResume = false;
  holdTime = 0;
  elasticAnchorX = null;
  scrollScrubbing = false;
  tapDipTime = -1;
  lastScrollAt = 0;
  wheelActive = false;
  sustained = false;
  steadyHold = false;
  rateBtn = null;
  chopActive = 0;
  flickVel = 0;
  resumeTarget = 1;
  dipBase = 1;
  scrubSpeed = 0;
  scrubCurrentProgress = 0;
  wasPlayingBeforeScrub = false;
  scrubStripBtn = null;
  scrubMoved = false;
  scrubAudioSpeed = 1;
  isMp4Tape = false;
  mp4Playing = true;
  mp4Progress = 0;
  mp4BackBtn = null;
  isSynthTape = false;
  synthAuto = false;
  autoScript = null;
  autoSeg = null;
  autoTimer = 0;
  autoChecks = [];

  // Check if a tape code was passed (e.g., "video !abc")
  if (params[0] && params[0].startsWith("!")) {
    const tapeCode = params[0].substring(1); // Remove the "!" prefix
    console.log("📼 Loading tape by code:", tapeCode);
    
    // Store the code for HUD display
    postedTapeCode = tapeCode;
    
    // Set loading state
    isLoadingTape = true;
    tapeLoadPhase = "metadata";
    tapeLoadProgress = 0;
    tapeLoadMessage = "FETCHING TAPE INFO";
    
    // Update HUD label
    if (hud) {
      hud.label(`!${tapeCode}`);
    }
    
    // Fetch tape metadata and load
    fetch(`/api/get-tape?code=${tapeCode}`)
      .then(res => {
        if (!res.ok) {
          if (res.status === 404) {
            throw new Error(`Tape !${tapeCode} not found`);
          }
          throw new Error(`Failed to load tape: ${res.status}`);
        }
        return res.json();
      })
      .then(metadata => {
        console.log("📊 Tape metadata:", metadata);
        
        // Check if tape is nuked
        if (metadata.nuked) {
          throw new Error(`Tape !${tapeCode} has been deleted`);
        }

        // Branch on backing store. Video-backed tapes (kind:"mp4", e.g. posted
        // camera-roll clips) play via a DOM <video>; frame-based tapes
        // (kind:"zip", the default) keep the existing frame-extraction path.
        if (metadata.kind === "mp4") {
          isMp4Tape = true;
          tapeLoadPhase = "download";
          tapeLoadProgress = 0;
          tapeLoadMessage = "LOADING VIDEO";

          // /media/tapes/<code>.mp4 redirects to the stored clip (kind-aware
          // route also resolves bare <code>, but the explicit ext is safest).
          const mp4Url = `${location.origin}/media/tapes/${tapeCode}.mp4`;

          console.log("📼 Requesting MP4 tape playback from bios:", mp4Url);
          send({
            type: "tape:play-mp4",
            content: { code: tapeCode, mp4Url, metadata },
          });

          // No frame download to track — clear the loading overlay shortly.
          setTimeout(() => {
            isLoadingTape = false;
            tapeLoadPhase = "";
            tapeLoadProgress = 0;
            tapeLoadMessage = "";
            requestPaint();
          }, 300);
          return;
        }

        tapeLoadPhase = "download";
        tapeLoadProgress = 0;
        tapeLoadMessage = "DOWNLOADING ZIP";

        // Use /media endpoint which handles tape resolution via code
        const zipUrl = `${location.origin}/media/tapes/${tapeCode}`;

        console.log("📦 Requesting tape playback from bios:", zipUrl);

        // Request bios to load and play the tape
        send({
          type: "tape:play",
          content: {
            code: tapeCode,
            zipUrl: zipUrl,
            metadata: metadata
          }
        });
      })
      .catch(err => {
        console.error("❌ Failed to load tape:", err);
        notice("TAPE NOT FOUND", ["red"]);
        isLoadingTape = false;
        tapeLoadPhase = "";
        tapeLoadProgress = 0;
        tapeLoadMessage = "";
      });
  } else if (params[0] === "scrub" || params[0] === "synth") {
    // 🧪 Synthtape: `video scrub [duration] [fps]` — bios fabricates a
    // deterministic test tape (scrolling waveform ruler + seamless musical
    // loop). Variants: `break` = one-bar 16th-note breakbeat (2s default),
    // `bar` = a single 2s bar, a bare number = duration in seconds
    // (2s per bar at 120 BPM). Add `auto` for the scripted 🧪 SYNTHSCRUB
    // exercise, reported to the console.
    const rest = params.slice(1);
    synthAuto = rest.includes("auto");
    const style =
      rest.includes("break") || rest.includes("beat")
        ? "break"
        : rest.includes("house")
          ? "house"
          : rest.includes("dub")
            ? "dub"
            : rest.includes("sine") || rest.includes("line")
              ? "sine"
              : "bed";
    const nums = rest.map(parseFloat).filter((n) => Number.isFinite(n));
    let duration = nums[0] || 8; // Four bars by default, break included
    if (rest.includes("bar")) duration = 2;
    const fps = nums[1] || 30;
    isSynthTape = true;
    isLoadingTape = true;
    tapeLoadPhase = "synth";
    tapeLoadProgress = 0;
    tapeLoadMessage = "SYNTHESIZING TAPE";
    hud?.label(`synthtape ${style}${synthAuto ? " auto" : ""}`);
    send({ type: "tape:play-synth", content: { duration, fps, style } });
  } else {
    // Try to restore cached video from IndexedDB
    store.retrieve("tape", "local:db", (data) => {
      if (data && data.blob) {
        // The video will be automatically presented via rec.present()
      }
    });
    
    rec.present(); // Visually present a recording right away if one exists.
  }
  
  ellipsisTicker = new gizmo.EllipsisTicker();
}

// 🎨 Paint (Executes every display frame)
function paint({
  api,
  wipe,
  ink,
  ui,
  help,
  hud,
  rec,
  screen,
  paintCount,
  needsPaint,
  sound,
  send,
  num,
  clock,
}) {
  if (typeof needsPaint === "function") {
    requestPaint = needsPaint;
  }

  const presenting = rec?.presenting ?? false;
  const playing = rec?.playing ?? false;
  const recPresentProgress = rec?.presentProgress ?? 0;
  const hasRecording = rec?.recorded ?? false;
  const exportAvailable = presenting || hasRecording || recPresentProgress > 0;
  
  // Map playback progress to VHS tape progress bar (during playback only, not export)
  if (playing && !isPrinting && recPresentProgress > 0 && recPresentProgress < 1) {
    rec.tapeProgress = recPresentProgress;
  } else if (!isPrinting) {
    // Clear tape progress when not playing
    rec.tapeProgress = 0;
  }
  
  if (!canvasTransparencyEnsured && api?.canvas) {
    api.canvas.style.backgroundColor = "transparent";
    canvasTransparencyEnsured = true;
  }

  // Always start transparent — underlay video shows through every frame.
  wipe(0, 0, 0, 0);

  // 🔍 DEBUG: sample pixel buffer after wipe to confirm transparency
  const _pc = Number(paintCount);
  if (_pc < 10 || _pc % 60 === 0) {
    const p = screen.pixels;
    if (p?.length >= 4) {
      const r = p[0], g = p[1], b = p[2], a = p[3];
      const mid = Math.floor(p.length / 2);
      const mr = p[mid], mg = p[mid+1], mb = p[mid+2], ma = p[mid+3];
      console.log(`🎬 VIDEO paint #${_pc}: buf=${p.length} (${screen.width}×${screen.height}) corner=[${r},${g},${b},${a}] mid=[${mr},${mg},${mb},${ma}]`);
    }
  }

  // 📼 Video-backed tape (kind:"mp4"): the clip plays in the DOM <video>
  // underlay, so here we just draw the lightweight overlay — a pause glyph
  // when paused, a thin progress bar, and a Back button. No frame UI.
  if (isMp4Tape) {
    if (!mp4Playing) {
      ink(255, 200).write("||", { center: "xy" });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }
    // Thin progress bar along the bottom.
    const barH = 2;
    const barY = screen.height - barH;
    ink(255, 40).box(0, barY, screen.width, barH);
    ink(255, 200, 0).box(0, barY, Math.floor(mp4Progress * screen.width), barH);

    if (!mp4BackBtn) {
      mp4BackBtn = new ui.TextButton("Back", { left: 6, bottom: 6, screen });
    }
    mp4BackBtn.reposition({ left: 6, bottom: 6, screen }, "Back");
    mp4BackBtn.paint(api);

    if (postedTapeCode) hud.label(`!${postedTapeCode}`);
    return true;
  }

  if (presenting && !playing && !isPrinting) {
    // Paused: subtle overlay without a black background
    ink(255, 200).write("||", { center: "xy" });
    ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
  }

  // Draw export buttons - reposition every frame (simple!)
  if (exportAvailable) {
    const disableExports = isPrinting || isPostingTape;

    // Request tape info if we don't have it yet
    if (tapeInfo === null) {
      send({ type: "tape:get-info" });
    }

    // Only show POST button if tape hasn't been posted yet AND duration is within limit
    const tapeWithinDurationLimit = !tapeInfo || (tapeInfo.totalDuration <= MAX_TAPE_DURATION);
    
    if (!postedTapeCode && tapeWithinDurationLimit && !isSynthTape) {
      if (!postBtn) {
        postBtn = new ui.TextButton("POST", { right: 6, bottom: 6, screen });
      }
      
      if (postBtn.txt !== "POST") {
        postBtn.txt = "POST";
      }
      postBtn.reposition({ right: 6, bottom: 6, screen }, "POST");
      postBtn.disabled = disableExports;
      postBtn.paint(api);
    } else {
      // Hide POST button if already posted or tape too long
      postBtn = undefined;
      
      // Show warning if tape is too long
      if (tapeInfo && tapeInfo.totalDuration > MAX_TAPE_DURATION && !postedTapeCode) {
        ink(255, 200, 100, 200).write(
          `Tape too long: ${tapeInfo.totalDuration.toFixed(1)}s (max ${MAX_TAPE_DURATION}s)`,
          { right: 6, bottom: 6, size: 0.75 }
        );
      }
    }

    // 📥 GIF / MP4 / ZIP export trio, stacked in the bottom-right corner —
    // only for tapes recorded from a KidLisp $code piece (atm); everything
    // else keeps just POST + Back. When the POST button is visible the
    // stack sits just above it; otherwise it starts flush with the bottom.
    const isKidlispTape =
      !isSynthTape &&
      !!(tapeInfo?.cachedCode || tapeInfo?.pieceName?.startsWith?.("$"));

    if (isKidlispTape) {
      const exportRowH = 22;
      let exportBottom = postBtn ? 6 + exportRowH : 6;

      if (!zipBtn) {
        zipBtn = new ui.TextButton("ZIP", { right: 6, bottom: exportBottom, screen });
      }
      zipBtn.reposition({ right: 6, bottom: exportBottom, screen }, "ZIP");
      zipBtn.disabled = disableExports;
      zipBtn.paint(api);
      exportBottom += exportRowH;

      if (!mp4Btn) {
        mp4Btn = new ui.TextButton("MP4", { right: 6, bottom: exportBottom, screen });
      }
      mp4Btn.reposition({ right: 6, bottom: exportBottom, screen }, "MP4");
      mp4Btn.disabled = disableExports;
      mp4Btn.paint(api);
      exportBottom += exportRowH;

      if (!gifBtn) {
        gifBtn = new ui.TextButton("GIF", { right: 6, bottom: exportBottom, screen });
      }
      gifBtn.reposition({ right: 6, bottom: exportBottom, screen }, "GIF");
      gifBtn.disabled = disableExports;
      gifBtn.paint(api);
    } else {
      gifBtn = undefined;
      mp4Btn = undefined;
      zipBtn = undefined;
    }

    // 🔙 Back-to-cap button (bottom-left) for the cap → review → re-cap loop.
    if (!backBtn) {
      backBtn = new ui.TextButton("Back", { left: 6, bottom: 6, screen });
    }
    backBtn.reposition({ left: 6, bottom: 6, screen }, "Back");
    backBtn.disabled = disableExports;
    backBtn.paint(api);
  } else {
    postBtn = undefined;
    gifBtn = undefined;
    mp4Btn = undefined;
    zipBtn = undefined;
    backBtn = undefined;
  }

  ensureScrubStripButton(
    ui,
    screen,
    (rec?.presenting ?? false) && !isPrinting && !isPostingTape,
  );
  
  // Update HUD label to show tape code after posting
  if (postedTapeCode) {
    hud.label(`!${postedTapeCode}`);
  }


  // Export progress display (outside of presenting block so it shows during exports)
  if (isPrinting || isLoadingTape) {
    if (useExtendedProgressBar) {
      ink(0, 0, 0, 180).box(0, 0, screen.width, screen.height);
      
      // Handle tape loading progress
      if (isLoadingTape) {
        const h = 16;
        const barWidth = screen.width - 40;
        const barX = 20;
        const barY = screen.height / 2 - h / 2;
        
        // Draw progress bar background
        ink(0, 50).box(barX, barY, barWidth, h);
        
        // Draw progress bar fill
        const fillWidth = Math.floor(barWidth * tapeLoadProgress);
        if (fillWidth > 0) {
          ink(255, 200, 0).box(barX, barY, fillWidth, h, "fill");
        }
        
        // Draw progress bar outline
        ink(255, 200, 0).box(barX, barY, barWidth, h, "outline");
        
        // Draw loading message
        const progressPercent = Math.floor(tapeLoadProgress * 100);
        const statusText = `${tapeLoadMessage} ${progressPercent}%`;
        ink(255, 200, 0).write(statusText, { 
          x: screen.width / 2, 
          y: barY - 10, 
          center: "x" 
        });
        
        return; // Skip normal export progress display
      }
      
      // Extended progress bar mode (enhanced with detailed phases)
      const h = 16; // Paint a printing / transcoding progress bar.
      
      // Enhanced dynamic text based on export type, phase, and progress
      let text = "";
      let phaseProgress = 0; // Progress within current phase
      
      if (currentExportType === "video") {
        if (printProgress < 0.3) {
          text = "PREPARING FRAMES";
          phaseProgress = printProgress / 0.3;
        } else if (printProgress < 0.7) {
          text = "ENCODING VIDEO";
          phaseProgress = (printProgress - 0.3) / 0.4;
        } else if (printProgress < 0.95) {
          text = "TRANSCODING MP4";
          phaseProgress = (printProgress - 0.7) / 0.25;
        } else {
          text = "FINALIZING VIDEO";
          phaseProgress = (printProgress - 0.95) / 0.05;
        }
      } else if (currentExportType === "gif") {
        // Simplified GIF export status
        if (printProgress < 0.4) {
          text = "PROCESSING FRAMES";
          phaseProgress = printProgress / 0.4;
        } else if (printProgress < 0.9) {
          text = "ENCODING GIF";
          phaseProgress = (printProgress - 0.4) / 0.5;
        } else {
          text = "FINALIZING";
          phaseProgress = (printProgress - 0.9) / 0.1;
        }
      } else if (currentExportType === "post") {
        // Tape posting progress with detailed phases
        if (currentExportPhase === "preparing") {
          text = "PREPARING TAPE";
          phaseProgress = Math.min(printProgress / 0.1, 1);
        } else if (currentExportPhase === "processing" || printProgress < 0.75) {
          text = "PROCESSING FRAMES";
          phaseProgress = (printProgress - 0.1) / 0.65;
        } else if (currentExportPhase === "metadata" || (printProgress >= 0.75 && printProgress < 0.85)) {
          text = "ADDING METADATA";
          phaseProgress = (printProgress - 0.75) / 0.1;
        } else if (currentExportPhase === "zipping" || (printProgress >= 0.85 && printProgress < 0.9)) {
          text = "COMPRESSING ZIP";
          phaseProgress = (printProgress - 0.85) / 0.05;
        } else if (currentExportPhase === "uploading" || printProgress >= 0.9) {
          text = "UPLOADING TO CLOUD";
          phaseProgress = (printProgress - 0.9) / 0.1;
        } else {
          text = "CREATING TAPE";
          phaseProgress = printProgress;
        }
      } else if (currentExportType === "frames") {
        if (printProgress < 0.5) {
          text = "PROCESSING FRAMES";
          phaseProgress = printProgress / 0.5;
        } else if (printProgress < 0.9) {
          text = "CREATING ZIP";
          phaseProgress = (printProgress - 0.5) / 0.4;
        } else {
          text = "FINALIZING ZIP";
          phaseProgress = (printProgress - 0.9) / 0.1;
        }
      // Hidden export types - keeping for future use
      /*
      } else if (currentExportType === "webp") {
        if (printProgress < 0.9) {
          text = "CONVERTING TO WEBP";
        } else {
          text = "CREATING ZIP";
        }
      } else if (currentExportType === "animwebp") {
        text = "CREATING ANIMATED WEBP";
      } else if (currentExportType === "apng") {
        text = "CREATING ANIMATED PNG";
      */
      } else {
        text = "PROCESSING";
        phaseProgress = printProgress;
      }
      
      // Use custom status message if provided
      if (exportStatusMessage && exportStatusMessage.trim().length > 0) {
        text = exportStatusMessage.toUpperCase();
      }
      
      // Add simple animated ellipsis
      const dots = Math.floor(Number(paintCount) / 20) % 4;
      text += ".".repeat(dots);

      const barWidth = Math.max(1, Math.floor(printProgress * screen.width)); // Overall progress
      const phaseBarWidth = Math.max(1, Math.floor(phaseProgress * screen.width)); // Phase progress

      // Calculate ETA if we have progress history
      let etaText = "";
      if (progressHistory.length > 1 && printProgress > 0.05) {
        const now = performance.now();
        const elapsed = (now - exportStartTime) / 1000; // seconds
        const remainingProgress = 1 - printProgress;
        const progressRate = printProgress / elapsed; // progress per second
        
        if (progressRate > 0) {
          const eta = Math.ceil(remainingProgress / progressRate);
          if (eta < 60) {
            etaText = ` • ${eta}s remaining`;
          } else {
            const minutes = Math.floor(eta / 60);
            const seconds = eta % 60;
            etaText = ` • ${minutes}m ${seconds}s remaining`;
          }
        }
      }

      // Draw progress bar background
      ink(0, 0, 80, 180)
        .box(0, screen.height / 2 - h / 2, screen.width, h);
      
      // Draw progress bar fill
      ink(0, 0, 255)
        .box(0, screen.height / 2 - h / 2, barWidth, h);
      
      // Draw percentage text on left
      const percentage = Math.floor(printProgress * 100);
      ink(255, 255, 255)
        .write(`${percentage}%`, { x: 8, y: screen.height / 2 - h / 2 + 4 });
      
      // Draw status text above progress bar
      ink(255, 255, 255)
        .write(text, { center: "x", y: screen.height / 2 - h / 2 - 12 });
    } else {
      // Baked-in VHS progress bar mode (rendered in disk.mjs during recording)
      // Progress bar will be rendered by the VHS tape progress system in disk.mjs
      // We just need to send the progress to the recording system
      // The VHS flickering progress bar overlay is handled automatically by the tapeProgress system
    }
  }

  // Show "NO VIDEO" message only when there is truly no video (not exporting, not loading)
  if (!presenting && !exportAvailable && !isLoadingTape && paintCount > 16n) {
    ink(40, 0, 0).box(0, 0, screen.width, screen.height);
    ink(180, 0, 0).write("NO VIDEO", { center: "xy" });
  }
  
  // 📟 Current playback rate, top-right, always visible on a tape.
  if (presenting && !isPrinting && !isLoadingTape) {
    const scrubDriven =
      isScrubbing ||
      inertiaActive ||
      brakeResume ||
      wheelActive ||
      sustained ||
      tapDipTime >= 0;
    const liveRate = scrubDriven ? scrubSpeed : playing ? 1 : 0;
    ink(steadyHold ? [0, 255, 180] : [255, 255, 0]).write(
      `${liveRate.toFixed(2)}x`,
      { x: screen.width - 6, y: 6, right: true },
    );

    // 📟 The readout is also a dial — keep its hit area in place.
    if (!rateBtn) {
      rateBtn = new ui.Button(screen.width - 64, 0, 64, 24);
      rateBtn.stickyScrubbing = true;
      rateBtn.noRolloverActivation = true;
    }
    rateBtn.box.x = screen.width - 64;
    rateBtn.box.y = 0;
    rateBtn.box.w = 64;
    rateBtn.box.h = 24;

    // 🔴 The red marker rides the bottom edge at the actual playback
    // position — drawn from live state so it never lies.
    const livePos = scrubDriven
      ? scrubCurrentProgress
      : (rec?.presentProgress ?? 0);
    ink(255, 51, 68).box(
      Math.floor(livePos * (screen.width - 6)),
      screen.height - 4,
      6,
      4,
    );

    // 🕰️ Net-time unison readout: phase offset vs the AC network clock's
    // grid — green when locked, amber while converging.
    if (tapeInfo?.totalDuration) {
      const durMs = tapeInfo.totalDuration * 1000;
      const nowMs = clock?.time?.()?.getTime?.() ?? Date.now();
      let phaseErr = (nowMs % durMs) / durMs - livePos;
      if (phaseErr > 0.5) phaseErr -= 1;
      else if (phaseErr < -0.5) phaseErr += 1;
      const errMs = Math.round(phaseErr * durMs);
      const locked = Math.abs(errMs) < 60;
      ink(locked ? [0, 255, 120] : [255, 170, 0]).write(
        `sync ${errMs >= 0 ? "+" : ""}${errMs}ms`,
        { x: screen.width - 6, y: 18, right: true },
      );
    }

    // 🧭 Gesture vector: anchor → finger, with the horizontal component
    // (the part that drives the rate) emphasized.
    if (isScrubbing && elasticAnchorX !== null && penX !== null) {
      const ay = elasticAnchorY ?? penY ?? 0;
      ink(255, 255, 255, 70).line(elasticAnchorX, ay, penX, penY ?? ay);
      const fwd = penX >= elasticAnchorX;
      ink(fwd ? [0, 255, 120, 200] : [255, 80, 80, 200]).box(
        Math.min(elasticAnchorX, penX),
        (ay) - 1,
        Math.max(1, Math.abs(penX - elasticAnchorX)),
        3,
      );
      ink(255, 255, 255, 220).box(elasticAnchorX - 2, ay - 2, 5, 5);
      ink(255, 255, 255).write(`${scrubSpeed.toFixed(2)}x`, {
        x: penX + 8,
        y: (penY ?? ay) - 4,
      });
    }

    // ⌨️ Deck keys legend, bottom-right.
    const keyLines = [
      "<- -> beat jump",
      "hold ^ 1/4 chop",
      "hold v 1/8 chop",
      "space reset",
    ];
    keyLines.forEach((l, i) => {
      ink(255, 255, 255, 110).write(l, {
        x: screen.width - 6,
        y: screen.height - 10 - (keyLines.length - i) * 10,
        right: true,
      });
    });
  }

  // Scrub overlay (STAMPLE-style speed-based)
  if ((isScrubbing || inertiaActive || brakeResume || wheelActive) && rec?.presenting) {
    // Waveform fetch (async, cached)
    if (!tapeWaveform && sound.getSampleData) {
      sound.getSampleData("tape:audio").then((data) => {
        if (data?.length) tapeWaveform = data;
      }).catch(() => {});
    }

    // Draw waveform background — only the SEGMENT around the playhead
    // (a ±1s window, matching the ruler's visible span), not the whole tape.
    let audioWaveform = tapeWaveform;
    if (!audioWaveform && sound?.speaker?.waveforms?.left?.length > 0) {
      audioWaveform = sound.speaker.waveforms.left;
    }
    if (audioWaveform?.length > 0 && sound?.paint?.waveform) {
      const dur = tapeInfo?.totalDuration || 10;
      const winFrac = Math.min(1, 2 / dur); // 2s of tape across the screen
      const len = audioWaveform.length;
      const segLen = Math.max(1, Math.floor(winFrac * len));
      const segStart = Math.floor(
        ((((scrubCurrentProgress - winFrac / 2) % 1) + 1) % 1) * len,
      );
      const segment = new Array(segLen);
      for (let i = 0; i < segLen; i++) {
        segment[i] = audioWaveform[(segStart + i) % len];
      }
      const skipN = Math.max(1, Math.floor(segment.length / 128));
      const compressed = num.arrCompress(segment, skipN);
      try {
        sound.paint.waveform(
          api, num.arrMax(compressed), compressed,
          0, 0, screen.width, screen.height,
          [255, 200, 0, 36], { direction: "left-to-right" },
        );
      } catch (_) {}
    }

    // (No overlay playhead line — the tape's own center needle and bottom
    // position bar already show where you are; a third moving line reads
    // as clutter.)
    const c = isScrubbing ? [255, 255, 0, 255] : [100, 200, 255, 220];

    // Speed indicator (stample-style)
    const absSpeed = Math.abs(scrubSpeed);
    const dir = scrubSpeed >= 0 ? "\u2192" : "\u2190"; // → or ←
    const speedLabel = absSpeed < 0.08 ? "\u258c\u258c" : `${dir} ${absSpeed.toFixed(1)}\u00d7`;
    ink(...c).write(speedLabel, { center: "xy" });

    // Progress % at top-right, under the rate readout
    ink(...c).write(
      `${Math.floor(scrubCurrentProgress * 100)}%`,
      { x: screen.width - 8, y: 20, right: true },
    );

    // Drive VHS tape progress bar from scrub position
    rec.tapeProgress = scrubCurrentProgress;
  }
  
  // Show completion message if active
  if (completionMessage && completionMessageTimer > 0) {
    // Create a box for the completion message
    const msgWidth = completionMessage.length * 6 + 20; // Approximate text width
    const msgHeight = 24;
    const x = (screen.width - msgWidth) / 2;
    const y = screen.height / 2 - 60; // Above center
    
    // Background box with slight transparency
    ink(0, 200).box(x - 5, y - 5, msgWidth + 10, msgHeight + 10);
    ink(0, 150).box(x, y, msgWidth, msgHeight, "outline");
    
    // Success message text
    ink(0, 255, 0).write(completionMessage, { center: "x", y: y + 8 });
    
    completionMessageTimer--;
    if (completionMessageTimer <= 0) {
      completionMessage = "";
    }
  }

  return true; // Always keep painting
}

function sim({ needsPaint, rec, send, clock }) {
  ellipsisTicker?.sim();
  frameCount++; // Increment frame counter for animations

  // Measured wall-time per sim tick — the scrub physics scales by this so
  // commanded speed means tape-seconds per wall-second at any tick rate.
  // Sim calls arrive in bursts, so the clamp must stay generous: it only
  // guards against a suspended tab, not ordinary burst gaps.
  const simNow = performance.now();
  const simDt = scrubLastSimTime
    ? Math.min(0.25, (simNow - scrubLastSimTime) / 1000)
    : 1 / 60;
  scrubLastSimTime = simNow;

  // 🧪 Synthtape autopilot: drive the same scrub state the finger would,
  // segment by segment, and grade the resulting motion.
  if (synthAuto && rec?.presenting && !isLoadingTape) {
    autopilot(rec, send, simDt);
  }

  if (typeof needsPaint === "function") {
    requestPaint = needsPaint;
    if (
      isPrinting ||
      isLoadingTape ||
      completionMessageTimer > 0 ||
      (rec?.presenting ?? false) ||
      (rec?.playing ?? false) ||
      (rec?.recording ?? false) ||
      ((rec?.tapeProgress ?? 0) > 0 && (rec?.tapeProgress ?? 0) < 1) ||
      postedTapeCode || // Keep painting when button shows tape code
      isScrubbing || // Keep painting during scrubbing
      inertiaActive || // Keep painting during inertia
      brakeHolding || // Keep painting while the touch brake slows the tape
      brakeResume || // Keep painting while it spins back up
      wheelActive || // Keep painting while the wheel spins down
      sustained || // Keep painting at a parked rate
      tapDipTime >= 0 // Keep painting through a tap dip
    ) {
      needsPaint();
    }
  }
  
  // 🖐️ Touch brake engage: a held (non-dragging) finger on a playing tape
  // starts slowing it to a stop after a short threshold, so a quick tap
  // still toggles play/pause.
  if (
    scrubStripBtn?.down &&
    !isScrubbing &&
    !inertiaActive &&
    !brakeResume &&
    !scrubMoved &&
    rec?.presenting &&
    rec?.playing
  ) {
    holdTime += simDt;
    if (holdTime >= BRAKE_ENGAGE_TIME) {
      brakeHolding = true;
      isScrubbing = true;
      scrubMoved = true; // A brake is a gesture, not a tap — no toggle on lift
      wasPlayingBeforeScrub = true;
      // Brake from the current rate — the parked rate if one is set — and
      // remember it so the release spins back up to it.
      resumeTarget = sustained ? scrubSpeed : 1;
      if (!sustained) scrubCurrentProgress = rec.presentProgress || 0;
      scrubSpeed = resumeTarget;
      sustained = false;
      send({
        type: "recorder:present:seek",
        content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
      });
    }
  } else if (!scrubStripBtn?.down) {
    holdTime = 0;
  }

  // 👇 Tap dip animation: rate sags along a half-sine and springs back to
  // the rate it departed from, then lands (normal play or parked).
  if (tapDipTime >= 0 && rec?.presenting) {
    tapDipTime += simDt;
    const phase = Math.min(1, tapDipTime / TAP_DIP_DURATION);
    scrubSpeed = dipBase * (1 - TAP_DIP_DEPTH * Math.sin(Math.PI * phase));
    if (phase >= 1) {
      // Lands as a seamless park at the departed rate — no handoff jump.
      tapDipTime = -1;
      scrubSpeed = dipBase;
      sustained = true;
    }
  }

  // Speed-based scrubbing physics (STAMPLE-style: drag velocity = playback speed)
  if (
    (isScrubbing ||
      inertiaActive ||
      brakeResume ||
      wheelActive ||
      sustained ||
      tapDipTime >= 0) &&
    rec?.presenting
  ) {
    const totalDuration = tapeInfo?.totalDuration || 10; // seconds
    // Decay/ramp factors below were tuned at 60fps; raise them to the
    // measured tick so the feel is identical at any sim rate.
    const rate = simDt * 60;

    // Advance position at current speed (like stample's pitch-shifted sample)
    scrubCurrentProgress += scrubSpeed * simDt / totalDuration;
    autoDtSum += simDt; // 🧪 Physics wall-time integration, for autopilot grading
    autoPhysTicks += 1;
    autoSpeedMin = Math.min(autoSpeedMin, scrubSpeed);
    autoSpeedMax = Math.max(autoSpeedMax, scrubSpeed);
    autoMotionSum += (scrubSpeed * simDt) / totalDuration;

    // The tape is a loop — wrap at the seam, so holding or scrubbing
    // through the ends keeps going instead of cutting.
    if (scrubCurrentProgress < 0) scrubCurrentProgress += 1;
    else if (scrubCurrentProgress >= 1) scrubCurrentProgress -= 1;

    // Seek frame only — audio managed separately via tape:audio-shift
    send({
      type: "recorder:present:seek",
      content: { progress: scrubCurrentProgress, speedScrub: true },
    });

    // Match tape audio speed to scrub speed (incremental shift like stample)
    nudgeTapeAudioSpeed(send, scrubSpeed);

    // 🖐️ Brake hold: speed sags toward 0 under the finger — deeeessshshvuueee.
    if (brakeHolding && isScrubbing) {
      scrubSpeed *= Math.pow(0.85, rate);
      if (scrubSpeed < 0.02) scrubSpeed = 0;
    }

    // 🖱️ Scroll release: the wheel went quiet — a seamless park at the
    // landed rate (pinned to exactly 1 when close).
    if (scrollScrubbing && isScrubbing && performance.now() - lastScrollAt > 150) {
      scrollScrubbing = false;
      tapDipTime = -1;
      isScrubbing = false;
      if (Math.abs(scrubSpeed - 1) < PARK_SNAP) scrubSpeed = 1;
      sustained = true;
    }

    // 🎰 Wheel spin-down: after a flick the platter runs free, then eases
    // down like a prize wheel to the pre-flick rate.
    if (wheelActive && !isScrubbing) {
      scrubSpeed =
        resumeTarget + (scrubSpeed - resumeTarget) * Math.pow(0.975, rate);
      if (Math.abs(scrubSpeed - resumeTarget) < 0.04) {
        // Lands as a seamless park — the drive keeps running, no handoff.
        wheelActive = false;
        scrubSpeed = resumeTarget;
        sustained = true;
      }
    }

    // 🌀 Friction: even a parked rate isn't forever — it glides home to
    // 1× over a few seconds, like a wheel that always feels the bearing.
    // It never hands off: the drive just converges to exactly 1.0 and
    // keeps driving — seamless by construction.
    if (sustained && !steadyHold && !isScrubbing && !chopActive) {
      scrubSpeed += (1 - scrubSpeed) * (1 - Math.pow(0.9965, rate));
      if (Math.abs(scrubSpeed - 1) < 0.005) scrubSpeed = 1;

      // 🕰️ Net-time phase pull: at rest the loop always lerps toward the
      // AC network clock's phase grid (clock.mjs / /api/clock synced), so
      // every player of this tape converges into unison — a gentle ±5%
      // tempo lean, never a jump.
      if (Math.abs(scrubSpeed - 1) < 0.01) {
        const durMs = totalDuration * 1000;
        const nowMs = clock?.time?.()?.getTime?.() ?? Date.now();
        const target = (nowMs % durMs) / durMs;
        let phaseErr = target - scrubCurrentProgress;
        if (phaseErr > 0.5) phaseErr -= 1;
        else if (phaseErr < -0.5) phaseErr += 1;
        scrubSpeed = 1 + Math.max(-0.05, Math.min(0.05, phaseErr * 0.15));
      }
    }

    // 🌀 Chop repeat: while ↑/↓ is held, loop a beat-fraction slice —
    // glitch stutter, Pioneer-style.
    if (chopActive && rec?.presenting) {
      const chopLen = (chopActive * BEAT_SEC) / totalDuration;
      let rel = scrubCurrentProgress - chopStart;
      if (rel < -0.5) rel += 1;
      else if (rel > 0.5) rel -= 1;
      if (rel >= chopLen || rel < 0) {
        scrubCurrentProgress = chopStart;
        send({
          type: "recorder:present:seek",
          content: { progress: chopStart, speedScrub: true },
        });
        send({ type: "tape:audio-pos", content: chopStart });
      }
    }

    // 🖐️ Brake release: spin back up to the pre-gesture rate.
    if (brakeResume && !isScrubbing) {
      scrubSpeed += (resumeTarget - scrubSpeed) * (1 - Math.pow(0.88, rate));
      if (Math.abs(scrubSpeed - resumeTarget) < 0.05) {
        // Lands as a seamless park — the drive keeps running, no handoff.
        brakeResume = false;
        scrubSpeed = resumeTarget;
        sustained = true;
      }
    }

    // Inertia: speed decays toward 0, then resume normal play
    if (inertiaActive && !isScrubbing) {
      scrubSpeed *= Math.pow(0.88, rate);
      if (Math.abs(scrubSpeed) < 0.04) {
        inertiaActive = false;
        scrubSpeed = 0;
        nudgeTapeAudioSpeed(send, 1);
        send({
          type: "recorder:present:seek",
          content: { progress: scrubCurrentProgress, scrubEnd: true },
        });
        // Pause disabled — the tape always rolls.
      }
    }

    requestPaint();
  }
}

let printed = false;

// ✒ Act (Runs once per user interaction)
function act({
  event: e,
  rec,
  ui,
  download,
  num,
  jump,
  sound,
  zip,
  send,
  store,
  needsPaint,
  screen,
  geo,
}) {
  // Extract synth from sound object for backward compatibility
  const { synth } = sound;
  if (typeof needsPaint === "function") {
    requestPaint = needsPaint;
  }

  const triggerRender = () => requestPaint();
  const exportAvailable =
    (rec?.presenting ?? false) ||
    (rec?.recorded ?? false) ||
    ((rec?.presentProgress ?? 0) > 0);


  // Handle system messages first
  if (handleSystemMessage({ event: e, rec, needsPaint, jump })) {
    return; // Exit early if a system message was handled
  }

  // 📼 Video-backed tape (kind:"mp4") input: Back button exits, a tap
  // anywhere else toggles the DOM <video> play/pause (also satisfies the
  // user-gesture requirement if autoplay was blocked).
  if (isMp4Tape) {
    if (mp4BackBtn) {
      let handledBack = false;
      mp4BackBtn.act(e, {
        push: () => {
          handledBack = true;
          synth({ type: "sine", tone: 700, attack: 0.1, decay: 0.5, volume: 0.5, duration: 0.005 });
          send({ type: "tape:stop" });
          jump("prompt");
        },
      });
      if (handledBack) return;
      if (mp4BackBtn.down) return; // Swallow the press that's on the button.
    }
    if (e.is("touch")) {
      send({ type: "tape:toggle-play-mp4" });
      triggerRender();
    }
    return;
  }

  if (
    !rec.presenting &&
    (isScrubbing || inertiaActive || brakeHolding || brakeResume || wheelActive || sustained)
  ) {
    isScrubbing = false;
    inertiaActive = false;
    brakeHolding = false;
    brakeResume = false;
    wheelActive = false;
    sustained = false;
    holdTime = 0;
    flickVel = 0;
    scrubSpeed = 0;
    scrubMoved = false;
    nudgeTapeAudioSpeed(send, 1);
  }

  if (!rec.printing && !isPrinting) {
    const allowExport = exportAvailable && !isPostingTape;

    // 🔙 Back to cap so the user can immediately re-shoot.
    backBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 500,
          attack: 0.1,
          decay: 0.99,
          volume: 0.6,
          duration: 0.001,
        });
      },
      push: () => {
        synth({
          type: "sine",
          tone: 700,
          attack: 0.1,
          decay: 0.5,
          volume: 0.5,
          duration: 0.005,
        });
        // Drop the current tape and any cached export / playback state
        // so cap.mjs starts clean and stale UI (progress bar, scrub
        // strip, post button) doesn't bleed through to the next visit.
        try {
          rec?.slate?.();
          if (rec) {
            rec.tapeProgress = 0;
            rec.printing = false;
            rec.presenting = false;
            rec.recorded = false;
            rec.presentProgress = 0;
          }
        } catch {}
        isPrinting = false;
        isPostingTape = false;
        isExportingGIF = false;
        isExportingFrames = false;
        isExportingWebP = false;
        isExportingAnimWebP = false;
        isExportingAPNG = false;
        currentExportType = "";
        currentExportPhase = "";
        exportStatusMessage = "";
        printProgress = 0;
        progressHistory = [];
        exportStartTime = 0;
        completionMessage = "";
        completionMessageTimer = 0;
        postedTapeCode = null;
        tapeInfo = null;
        isScrubbing = false;
        inertiaActive = false;
        brakeHolding = false;
        brakeResume = false;
        holdTime = 0;
        scrubSpeed = 0;
        scrubMoved = false;
        scrubCurrentProgress = 0;
        scrubAudioSpeed = 1;
        wasPlayingBeforeScrub = false;
        tapeWaveform = null;
        scrubStripBtn = null;
        backBtn = undefined;
        postBtn = undefined;
        gifBtn = undefined;
        mp4Btn = undefined;
        zipBtn = undefined;
        jump("cap");
      },
    });

    gifBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 500,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        if (!allowExport || isExportingGIF) return;

        isExportingGIF = true;
        isPrinting = true;
        currentExportType = "gif";
        currentExportPhase = "analyzing";
        exportStatusMessage = "STARTING GIF EXPORT";
        printProgress = 0.01;
        exportStartTime = performance.now();
        progressHistory = [];
        gifBtn.disabled = true;

        if (!useExtendedProgressBar && rec) {
          rec.tapeProgress = 0.01;
        }

        triggerRender();

        try {
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              const processedFrames = frameData.frames.map((frame, index) => {
                const [timestamp, imageData, penData] = frame;
                let duration = 16.67;
                if (index < frameData.frames.length - 1) {
                  const nextTimestamp = frameData.frames[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                return {
                  timestamp,
                  originalTimestamp: timestamp,
                  duration,
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data,
                  penData,
                };
              });

              // Send frames in chunks to avoid memory issues
              const CHUNK_SIZE = 500; // Smaller chunks for processed frames (they're larger)
              const totalFrames = processedFrames.length;
              const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
              
              if (totalChunks > 1) {
                console.log(`📦 GIF: Sending ${totalFrames} processed frames in ${totalChunks} chunks`);
                
                // Send first chunk with metadata
                send({
                  type: "create-animated-gif",
                  content: { 
                    frames: processedFrames.slice(0, CHUNK_SIZE),
                    chunkIndex: 0,
                    totalChunks: totalChunks,
                    totalFrames: totalFrames
                  },
                });
                
                // Send remaining chunks
                for (let i = 1; i < totalChunks; i++) {
                  const start = i * CHUNK_SIZE;
                  const end = Math.min(start + CHUNK_SIZE, totalFrames);
                  
                  await new Promise(resolve => setTimeout(resolve, 10));
                  
                  send({
                    type: "create-animated-gif-chunk",
                    content: { 
                      frames: processedFrames.slice(start, end),
                      chunkIndex: i,
                      totalChunks: totalChunks
                    },
                  });
                }
              } else {
                // Single chunk, send normally
                send({
                  type: "create-animated-gif",
                  content: { frames: processedFrames },
                });
              }
            } else {
              console.warn("No frames available for GIF export");
              isPrinting = false;
              isExportingGIF = false;
              currentExportType = "";
              currentExportPhase = "";
              exportStatusMessage = "";
              exportStartTime = 0;
              progressHistory = [];
              gifBtn.disabled = false;
              triggerRender();
            }
          });
        } catch (error) {
          console.error("Error exporting GIF:", error);
          isPrinting = false;
          isExportingGIF = false;
          currentExportType = "";
          currentExportPhase = "";
          exportStatusMessage = "";
          exportStartTime = 0;
          progressHistory = [];
          gifBtn.disabled = false;
          triggerRender();
        }

        synth({
          type: "sine",
          tone: 750,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    mp4Btn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 600,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        if (!allowExport) return;

        isPrinting = true;
        currentExportType = "video";
        currentExportPhase = "preparing";
        exportStatusMessage = "STARTING MP4 EXPORT";
        printProgress = 0.01;
        exportStartTime = performance.now();
        progressHistory = [];
        mp4Btn.disabled = true;

        if (!useExtendedProgressBar && rec) {
          rec.tapeProgress = 0.01;
        }

        triggerRender();

        try {
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              const processedFrames = frameData.frames.map((frame, index) => {
                const [timestamp, imageData] = frame;
                let duration = 16.67;
                if (index < frameData.frames.length - 1) {
                  const nextTimestamp = frameData.frames[index + 1][0];
                  duration = Math.max(8.33, Math.min(50, nextTimestamp - timestamp));
                }
                return {
                  timestamp,
                  originalTimestamp: timestamp,
                  duration,
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data,
                };
              });

              // Send frames in chunks to avoid memory issues
              const CHUNK_SIZE = 500;
              const totalFrames = processedFrames.length;
              const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
              
              if (totalChunks > 1) {
                console.log(`📦 MP4: Sending ${totalFrames} processed frames in ${totalChunks} chunks`);
                
                send({
                  type: "create-animated-mp4",
                  content: { 
                    frames: processedFrames.slice(0, CHUNK_SIZE),
                    chunkIndex: 0,
                    totalChunks: totalChunks,
                    totalFrames: totalFrames
                  },
                });
                
                for (let i = 1; i < totalChunks; i++) {
                  const start = i * CHUNK_SIZE;
                  const end = Math.min(start + CHUNK_SIZE, totalFrames);
                  const chunk = processedFrames.slice(start, end);
                  
                  await new Promise(resolve => setTimeout(resolve, 10));
                  
                  send({
                    type: "create-animated-mp4-chunk",
                    content: { 
                      frames: chunk,
                      chunkIndex: i,
                      totalChunks: totalChunks
                    },
                  });
                }
                
                console.log(`✅ MP4: Finished sending all ${totalChunks} chunks`);
              } else {
                send({
                  type: "create-animated-mp4",
                  content: { frames: processedFrames },
                });
              }
            } else {
              console.warn("No frames available for MP4 export");
              isPrinting = false;
              currentExportType = "";
              currentExportPhase = "";
              exportStatusMessage = "";
              exportStartTime = 0;
              progressHistory = [];
              mp4Btn.disabled = false;
              triggerRender();
            }
          });
        } catch (error) {
          console.error("Error exporting MP4:", error);
          isPrinting = false;
          currentExportType = "";
          currentExportPhase = "";
          exportStatusMessage = "";
          exportStartTime = 0;
          progressHistory = [];
          mp4Btn.disabled = false;
          triggerRender();
        }

        synth({
          type: "sine",
          tone: 800,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    zipBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 650,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        if (!allowExport || isExportingFrames) return;

        isExportingFrames = true;
        isPrinting = true;
        currentExportType = "frames";
        currentExportPhase = "preparing";
        exportStatusMessage = "STARTING ZIP EXPORT";
        printProgress = 0.01;
        exportStartTime = performance.now();
        progressHistory = [];
        zipBtn.disabled = true;

        if (!useExtendedProgressBar && rec) {
          rec.tapeProgress = 0.01;
        }

        triggerRender();

        try {
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              const framesToProcess = frameData.frames.map((frame, index) => {
                const [timestamp, imageData] = frame;
                let duration = 16.67;
                if (index < frameData.frames.length - 1) {
                  const nextTimestamp = frameData.frames[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                return {
                  timestamp,
                  duration,
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data,
                };
              });

              // Send frames in chunks to avoid memory issues
              const CHUNK_SIZE = 500; // Smaller chunks for processed frames
              const totalFrames = framesToProcess.length;
              const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
              
              if (totalChunks > 1) {
                console.log(`📦 ZIP: Sending ${totalFrames} processed frames in ${totalChunks} chunks`);
                
                // Send first chunk with metadata
                send({
                  type: "create-animated-frames-zip",
                  content: { 
                    frames: framesToProcess.slice(0, CHUNK_SIZE),
                    chunkIndex: 0,
                    totalChunks: totalChunks,
                    totalFrames: totalFrames
                  },
                });
                
                // Send remaining chunks
                for (let i = 1; i < totalChunks; i++) {
                  const start = i * CHUNK_SIZE;
                  const end = Math.min(start + CHUNK_SIZE, totalFrames);
                  const chunk = framesToProcess.slice(start, end);
                  
                  // Small delay to prevent overwhelming
                  await new Promise(resolve => setTimeout(resolve, 10));
                  
                  send({
                    type: "create-animated-frames-zip-chunk",
                    content: { 
                      frames: chunk,
                      chunkIndex: i,
                      totalChunks: totalChunks
                    },
                  });
                }
                
                console.log(`✅ ZIP: Finished sending all ${totalChunks} chunks`);
              } else {
                // Single chunk - send normally
                send({
                  type: "create-animated-frames-zip",
                  content: { frames: framesToProcess },
                });
              }
            } else {
              console.warn("No frames available for ZIP export");
              isPrinting = false;
              isExportingFrames = false;
              currentExportType = "";
              currentExportPhase = "";
              exportStatusMessage = "";
              exportStartTime = 0;
              progressHistory = [];
              zipBtn.disabled = false;
              triggerRender();
            }
          });
        } catch (error) {
          console.error("Error exporting ZIP:", error);
          isPrinting = false;
          isExportingFrames = false;
          currentExportType = "";
          currentExportPhase = "";
          exportStatusMessage = "";
          exportStartTime = 0;
          progressHistory = [];
          zipBtn.disabled = false;
          triggerRender();
        }

        synth({
          type: "sine",
          tone: 900,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    // POST button - Upload tape to cloud (ZIP + MongoDB + ATProto)
    postBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 600,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        // If tape was already posted, button is disabled - do nothing
        if (postedTapeCode) {
          return;
        }
        
        if (isPostingTape) return; // Prevent double-posting
        
        isPostingTape = true;
        isPrinting = true;
        currentExportType = "post";
        currentExportPhase = "preparing";
        exportStatusMessage = "PREPARING TAPE";
        printProgress = 0.01;
        exportStartTime = performance.now();
        progressHistory = [];
        postBtn.disabled = true;
        triggerRender();
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0.01;
        }
        
        try {
          // Request frames from the recording system for tape posting
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              // Convert frames to format expected by create-and-post-tape
              const frameRecord = [];
              
              frameData.frames.forEach((frame, index) => {
                const [timestamp, imageData, penData] = frame;
                
                // Calculate duration until next frame
                let duration = 16.67; // Default ~60fps
                if (index < frameData.frames.length - 1) {
                  const nextTimestamp = frameData.frames[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                
                frameRecord.push({
                  timestamp,
                  duration,
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data
                });
              });
              
              // Send frames in chunks to avoid memory issues
              const CHUNK_SIZE = 500;
              const totalFrames = frameRecord.length;
              const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
              
              if (totalChunks > 1) {
                console.log(`📦 POST: Sending ${totalFrames} processed frames in ${totalChunks} chunks`);
                
                send({
                  type: "create-and-post-tape",
                  content: {
                    frames: frameRecord.slice(0, CHUNK_SIZE),
                    piece: "video",
                    rawAudio: frameData.rawAudio,
                    chunkIndex: 0,
                    totalChunks: totalChunks,
                    totalFrames: totalFrames
                  }
                });
                
                for (let i = 1; i < totalChunks; i++) {
                  const start = i * CHUNK_SIZE;
                  const end = Math.min(start + CHUNK_SIZE, totalFrames);
                  const chunk = frameRecord.slice(start, end);
                  
                  await new Promise(resolve => setTimeout(resolve, 10));
                  
                  send({
                    type: "create-and-post-tape-chunk",
                    content: { 
                      frames: chunk,
                      chunkIndex: i,
                      totalChunks: totalChunks
                    },
                  });
                }
                
                console.log(`✅ POST: Finished sending all ${totalChunks} chunks`);
              } else {
                // Send to bios for ZIP creation AND upload
                send({
                  type: "create-and-post-tape",
                  content: {
                    frames: frameRecord,
                    piece: "video",
                    rawAudio: frameData.rawAudio
                  }
                });
              }
              
              // Note: isPrinting will be reset by tape:posted callback
            } else {
              isPrinting = false;
              currentExportType = "";
              postBtn.disabled = false;
              triggerRender();
              isPostingTape = false;
            }
            
          });
        } catch (error) {
          console.error("Error posting tape:", error);
          isPostingTape = false;
          isPrinting = false;
          currentExportType = "";
          postBtn.disabled = false;
          triggerRender();
        }
        
        synth({
          type: "sine",
          tone: 1000,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    // Hidden export options - keeping action handlers for future use
    /*
    // Export WebP animation
    webpBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 600,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        isExportingWebP = true;
        isPrinting = true; // Show progress bar
        currentExportType = "webp"; // Set export type
        webpBtn.disabled = true;
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0;
        }
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              
              // Send request to main thread for WebP creation
              const success = await createAnimatedWebP(frameData.frames, send);
              
              if (success) {
                // Reset flags since WebP creation happens asynchronously in main thread
                isPrinting = false;
                currentExportType = "";
              } else {
                console.warn("Failed to send WebP creation request");
                isPrinting = false;
                currentExportType = "";
              }
            } else {
              console.warn("No frames available for export");
              // Reset flags if no frames available
              isPrinting = false;
              currentExportType = "";
            }
            
            isExportingWebP = false;
            webpBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting WebP:", error);
          isExportingWebP = false;
          isPrinting = false;
          currentExportType = "";
          webpBtn.disabled = false;
        }
        
        synth({
          type: "sine",
          tone: 800,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    // Export animated WebP file (not ZIP)
    animWebpBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 500,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        isExportingAnimWebP = true;
        isPrinting = true; // Show progress bar
        currentExportType = "animwebp"; // Set export type
        animWebpBtn.disabled = true;
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0;
        }
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              
              // Apply frame reduction for very long recordings
              let framesToProcess = frameData.frames;
              const MAX_FRAMES_FOR_WEBP = 1500; // Increased limit - WebP is more efficient than GIF
              
              if (frameData.frames.length > MAX_FRAMES_FOR_WEBP) {
                const skipRatio = frameData.frames.length / MAX_FRAMES_FOR_WEBP;
                framesToProcess = [];
                for (let i = 0; i < frameData.frames.length; i += skipRatio) {
                  framesToProcess.push(frameData.frames[Math.floor(i)]);
                }
                framesToProcess = framesToProcess.slice(0, MAX_FRAMES_FOR_WEBP);
              }
              
              // Process frames
              const processedFrames = framesToProcess.map((frame, index) => {
                const [timestamp, imageData, penData] = frame;
                let duration = 100; // Default 100ms
                
                if (index < framesToProcess.length - 1) {
                  const nextTimestamp = framesToProcess[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                
                return {
                  timestamp: timestamp,
                  originalTimestamp: timestamp,
                  duration: duration,
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data
                };
              });

              // Send frames in chunks to avoid memory issues
              const CHUNK_SIZE = 500;
              const totalFrames = processedFrames.length;
              const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
              
              if (totalChunks > 1) {
                console.log(`📦 WebP: Sending ${totalFrames} processed frames in ${totalChunks} chunks`);
                
                send({
                  type: "create-animated-webp-only",
                  content: {
                    frames: processedFrames.slice(0, CHUNK_SIZE),
                    chunkIndex: 0,
                    totalChunks: totalChunks,
                    totalFrames: totalFrames
                  }
                });
                
                for (let i = 1; i < totalChunks; i++) {
                  const start = i * CHUNK_SIZE;
                  const end = Math.min(start + CHUNK_SIZE, totalFrames);
                  const chunk = processedFrames.slice(start, end);
                  
                  await new Promise(resolve => setTimeout(resolve, 10));
                  
                  send({
                    type: "create-animated-webp-only-chunk",
                    content: { 
                      frames: chunk,
                      chunkIndex: i,
                      totalChunks: totalChunks
                    },
                  });
                }
                
                console.log(`✅ WebP: Finished sending all ${totalChunks} chunks`);
              } else {
                send({
                  type: "create-animated-webp-only",
                  content: { frames: processedFrames }
                });
              }
              
              // Reset flags since animated WebP creation happens asynchronously in main thread
              isPrinting = false;
              currentExportType = "";
            } else {
              // Reset flags if no frames available
              isPrinting = false;
              currentExportType = "";
            }
            
            isExportingAnimWebP = false;
            animWebpBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting animated WebP:", error);
          isExportingAnimWebP = false;
          isPrinting = false;
          currentExportType = "";
          animWebpBtn.disabled = false;
        }
        
        synth({
          type: "sine",
          tone: 800,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });

    // Export APNG file (animated PNG)
    apngBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 450,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: async () => {
        isExportingAPNG = true;
        isPrinting = true; // Show progress bar
        currentExportType = "apng"; // Set export type
        apngBtn.disabled = true;
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0;
        }
        
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              
              // Apply frame reduction for very long recordings
              let framesToProcess = frameData.frames;
              const MAX_FRAMES_FOR_APNG = 1000; // Increased limit for APNG
              
              if (frameData.frames.length > MAX_FRAMES_FOR_APNG) {
                const skipRatio = frameData.frames.length / MAX_FRAMES_FOR_APNG;
                framesToProcess = [];
                for (let i = 0; i < frameData.frames.length; i += skipRatio) {
                  framesToProcess.push(frameData.frames[Math.floor(i)]);
                }
                framesToProcess = framesToProcess.slice(0, MAX_FRAMES_FOR_APNG);
              }
              
              // Process frames
              const processedFrames = framesToProcess.map((frame, index) => {
                const [timestamp, imageData, penData] = frame;
                let duration = 100; // Default 100ms
                
                if (index < framesToProcess.length - 1) {
                  const nextTimestamp = framesToProcess[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                
                return {
                  timestamp: timestamp,
                  originalTimestamp: timestamp,
                  duration: duration,
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data
                };
              });

              // Send frames in chunks to avoid memory issues
              const CHUNK_SIZE = 500;
              const totalFrames = processedFrames.length;
              const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
              
              if (totalChunks > 1) {
                console.log(`📦 APNG: Sending ${totalFrames} processed frames in ${totalChunks} chunks`);
                
                send({
                  type: "create-single-animated-apng",
                  content: {
                    frames: processedFrames.slice(0, CHUNK_SIZE),
                    chunkIndex: 0,
                    totalChunks: totalChunks,
                    totalFrames: totalFrames
                  }
                });
                
                for (let i = 1; i < totalChunks; i++) {
                  const start = i * CHUNK_SIZE;
                  const end = Math.min(start + CHUNK_SIZE, totalFrames);
                  const chunk = processedFrames.slice(start, end);
                  
                  await new Promise(resolve => setTimeout(resolve, 10));
                  
                  send({
                    type: "create-single-animated-apng-chunk",
                    content: { 
                      frames: chunk,
                      chunkIndex: i,
                      totalChunks: totalChunks
                    },
                  });
                }
                
                console.log(`✅ APNG: Finished sending all ${totalChunks} chunks`);
              } else {
                send({
                  type: "create-single-animated-apng",
                  content: { frames: processedFrames }
                });
              }
              
              // Reset flags since APNG creation happens asynchronously in main thread
              isPrinting = false;
              currentExportType = "";
            } else {
              // Reset flags if no frames available
              isPrinting = false;
              currentExportType = "";
            }
            
            isExportingAPNG = false;
            apngBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting APNG:", error);
          isExportingAPNG = false;
          isPrinting = false;
          currentExportType = "";
          apngBtn.disabled = false;
        }
        
        synth({
          type: "sine",
          tone: 700,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });
    */

    // Hidden Clear button action handler - keeping for future use
    /*
    // Clear cached video button
    clearBtn?.act(e, {
      down: () => {
        synth({
          type: "sine",
          tone: 400,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.001,
        });
      },
      push: () => {
        // Clear the cached video from IndexedDB
        store.delete("tape", "local:db", (result) => {
          if (result.data) {
            console.log("📼 Cleared cached video from IndexedDB");
            // Also clear any playback state
            rec.unpresent();
            // Reset the printed flag so we can generate a new video
            printed = false;
          }
        });
        
        synth({
          type: "sine",
          tone: 300,
          attack: 0.1,
          decay: 0.99,
          volume: 0.75,
          duration: 0.005,
        });
      },
    });
    */

    const anyButtonDown = 
      postBtn?.down || 
      mp4Btn?.down || 
      gifBtn?.down || 
      zipBtn?.down;
    
    // ⌨️ Spacebar is the only pause — touch never pauses, but a deliberate
    // keypress can still halt and resume the tape.
    if (e.is("keyboard:down:space") && rec.presenting) {
      // Spacebar also resets any parked/spinning rate back to normal.
      isScrubbing = false;
      inertiaActive = false;
      brakeHolding = false;
      brakeResume = false;
      wheelActive = false;
      sustained = false;
      scrollScrubbing = false;
      tapDipTime = -1;
      steadyHold = false;
      chopActive = 0;
      scrubSpeed = 0;
      nudgeTapeAudioSpeed(send, 1);
      if (rec.playing) rec.pause();
      else rec.play();
      triggerRender();
      return;
    }

    // 🎛️ Deck keys, Pioneer-style: ← → beat-jump the tape (audio included);
    // holding ↑ chop-repeats a quarter beat, ↓ an eighth — release to run on.
    if (rec.presenting && tapeInfo?.totalDuration) {
      const dur = tapeInfo.totalDuration;
      const beatJump = (beats) => {
        ensureDriven(rec, send);
        let p = scrubCurrentProgress + (beats * BEAT_SEC) / dur;
        p = ((p % 1) + 1) % 1;
        scrubCurrentProgress = p;
        send({
          type: "recorder:present:seek",
          content: { progress: p, speedScrub: true },
        });
        send({ type: "tape:audio-pos", content: p });
        triggerRender();
      };
      if (e.is("keyboard:down:arrowleft")) beatJump(-1);
      if (e.is("keyboard:down:arrowright")) beatJump(1);
      if (e.is("keyboard:down:arrowup") && !chopActive) {
        ensureDriven(rec, send);
        chopActive = 0.25;
        chopStart = scrubCurrentProgress;
        send({ type: "tape:audio-pos", content: chopStart });
      }
      if (e.is("keyboard:down:arrowdown") && !chopActive) {
        ensureDriven(rec, send);
        chopActive = 0.125;
        chopStart = scrubCurrentProgress;
        send({ type: "tape:audio-pos", content: chopStart });
      }
      if (e.is("keyboard:up:arrowup") || e.is("keyboard:up:arrowdown")) {
        chopActive = 0;
      }
    }

    // 🖱️ Two-finger scroll scrubs directly — no tap-drag needed. Wheel
    // deltas bend the rate; when the gesture stops it springs back to 1×.
    if (e.is("scroll") && rec.presenting) {
      const d = e.x || e.y || 0; // Horizontal primary, vertical works too
      if (d) {
        if (!isScrubbing) {
          inertiaActive = false;
          brakeHolding = false;
          brakeResume = false;
          wheelActive = false;
          if (!sustained) {
            scrubCurrentProgress =
              rec.presentProgress || scrubCurrentProgress || 0;
          }
          sustained = false;
          wasPlayingBeforeScrub = true;
          isScrubbing = true;
          scrollScrubbing = true;
          if (!rec.playing) rec.play();
          send({
            type: "recorder:present:seek",
            content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
          });
        }
        scrubMoved = true;
        // Expressive range: two-finger scroll can push way past the drag's
        // reach — up to ±24×. Negated so scroll direction matches drag
        // direction (natural scrolling inverts the wheel deltas).
        scrubSpeed = Math.max(-24, Math.min(24, scrubSpeed * 0.6 - d * 0.35));
        lastScrollAt = performance.now();
        nudgeTapeAudioSpeed(send, scrubSpeed);
        triggerRender();
      }
    }

    // 📟 Steady-rate dial: vertical drag on the top-right readout sets a
    // held rate the friction won't touch.
    if (rateBtn && rec.presenting && !isPrinting && !isPostingTape) {
      rateBtn.act(e, {
        down: () => {
          rateDragStartY = e.y ?? 0;
          rateDragStartRate =
            isScrubbing || inertiaActive || brakeResume || wheelActive || sustained || tapDipTime >= 0
              ? scrubSpeed
              : rec.playing
                ? 1
                : 0;
        },
        scrub: () => {
          if (e.y === undefined) return;
          if (!sustained && !isScrubbing) {
            // Engage the drive so the dialed rate actually plays.
            scrubCurrentProgress = rec.presentProgress || scrubCurrentProgress || 0;
            if (!rec.playing) rec.play();
            send({
              type: "recorder:present:seek",
              content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
            });
          }
          inertiaActive = false;
          brakeResume = false;
          wheelActive = false;
          tapDipTime = -1;
          sustained = true;
          steadyHold = true;
          const dyRate = (rateDragStartY - e.y) * 0.03;
          scrubSpeed = Math.max(-24, Math.min(24, rateDragStartRate + dyRate));
          nudgeTapeAudioSpeed(send, scrubSpeed);
          triggerRender();
        },
        up: () => {
          if (Math.abs(scrubSpeed - 1) < PARK_SNAP) {
            scrubSpeed = 1;
            steadyHold = false; // Back at play speed — friction may hold it
          }
          triggerRender();
        },
      });
      if (rateBtn.down) return; // The dial owns this gesture
    }

    if (!anyButtonDown && !isPrinting && !isPostingTape && rec.presenting) {
      ensureScrubStripButton(ui, screen, true);
      scrubStripBtn?.act(e, {
        down: () => {
          if (synthAuto) console.log(`🧪 STRIP DOWN fired (e.type=${e.type})`);
          scrubMoved = false;
          isScrubbing = false;
          inertiaActive = false;
          brakeHolding = false;
          tapDipTime = -1;
          holdTime = 0;
          flickVel = 0;
          elasticAnchorX = e.x ?? null;
          elasticAnchorY = e.y ?? null;
          penX = e.x ?? null;
          penY = e.y ?? null;
          // 🅿️ A parked, spinning, or ramping rate survives a new touch:
          // the gesture starts FROM it — so you can grab a fast wheel and
          // drag it slower — and returns TO it.
          const carrying =
            sustained || wheelActive || brakeResume || scrollScrubbing;
          brakeResume = false;
          scrollScrubbing = false;
          wheelActive = false;
          resumeTarget = carrying ? scrubSpeed : 1;
          // ✊ Grabbing the tape holds it — the drag is anchored at 0×, so
          // a still finger means a still tape and displacement is motion.
          elasticBase = 0;
          if (!carrying) {
            scrubCurrentProgress = rec.presentProgress || 0;
            scrubSpeed = 0;
          }
          wasPlayingBeforeScrub = rec.playing;
          scrubAudioSpeed = 1;
        },
        scrub: () => {
          if (e.x === undefined) return;
          if (brakeHolding) {
            // 🖐️→🪀 Swiping out of a brake hold throws the reel: the rate
            // anchors at 0×, so swipe direction is playback direction.
            brakeHolding = false;
            elasticBase = 0;
            elasticAnchorX = e.x;
          }
          if (elasticAnchorX === null) elasticAnchorX = e.x;
          if (!isScrubbing) {
            isScrubbing = true;
            sustained = false; // The finger owns the rate while it's down
            // STAMPLE-like: start a live source so drag can bend audio immediately.
            if (!rec.playing) rec.play();
            send({
              type: "recorder:present:seek",
              content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
            });
          }
          // ✊ The grabbed tape moves with the hand: displacement from the
          // grab point IS the rate — hold still and it holds still, a third
          // of the screen right drags at 3×, left drags in reverse.
          const dx = e.x - elasticAnchorX;
          penX = e.x;
          penY = e.y ?? penY;
          if (Math.abs(dx) > 2) scrubMoved = true;
          flickVel = flickVel * 0.6 + (e.delta?.x || 0) * 0.4;
          const K = 9 / screen.width;
          scrubSpeed = Math.max(-8, Math.min(8, elasticBase + dx * K));
          nudgeTapeAudioSpeed(send, scrubSpeed);
          triggerRender();
        },
        up: () => {
          if (!isScrubbing) return true;
          isScrubbing = false;
          elasticAnchorX = null;
          elasticBase = 1;
          if (brakeHolding) {
            // 🖐️ Brake release: spin back up to the pre-gesture rate.
            brakeHolding = false;
            brakeResume = true;
          } else if (Math.abs(flickVel) > FLICK_THRESHOLD) {
            // 🎰 Flick: the platter runs free with the throw's momentum,
            // then eases down like a prize wheel to the pre-flick rate.
            scrubSpeed = Math.max(
              -24,
              Math.min(24, scrubSpeed + flickVel * FLICK_KICK),
            );
            nudgeTapeAudioSpeed(send, scrubSpeed);
            wheelActive = true;
          } else {
            // 🅿️ Release parks where you left it. Near 1× it pins to
            // exactly 1 — the scrub drive at 1.0 IS normal playback, so
            // there's no handoff and no jump, ever.
            if (Math.abs(scrubSpeed - 1) < PARK_SNAP) scrubSpeed = 1;
            sustained = true;
            nudgeTapeAudioSpeed(send, scrubSpeed);
          }
          triggerRender();
          return true;
        },
        cancel: () => {
          if (!isScrubbing) return;
          isScrubbing = false;
          inertiaActive = false;
          brakeHolding = false;
          brakeResume = false;
          wheelActive = false;
          sustained = false;
          elasticAnchorX = null;
          scrubSpeed = 0;
          nudgeTapeAudioSpeed(send, 1);
          send({
            type: "recorder:present:seek",
            content: { progress: scrubCurrentProgress, scrubEnd: true },
          });
          // Pause disabled — the tape always rolls.
          triggerRender();
        },
        push: () => {
          if (scrubMoved || isScrubbing || inertiaActive || wheelActive) return;
          if (hasAudioContext && audioContextState === "suspended") {
            handleAudioContextAndPlay(sound, rec, triggerRender);
            return;
          }
          // 👇 A single tap dips the platter: the rate sags toward 0 and
          // springs back — from and to the parked rate if one is set.
          // (Pause stays on spacebar only.)
          if (!rec.playing) rec.play();
          dipBase = sustained ? scrubSpeed : 1;
          sustained = false;
          tapDipTime = 0;
          inertiaActive = false;
          brakeResume = false;
          if (dipBase === 1) scrubCurrentProgress = rec.presentProgress || 0;
          scrubSpeed = dipBase;
          send({
            type: "recorder:present:seek",
            content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
          });
          triggerRender();
        },
      });
    }
  }
}

// 🚧 Signal (Handles messages from the system)
function signal(content) {
  console.log("🎯 Video piece received signal:", content);
  if (content === "recorder:transcoding-done") {
    console.log("🎯 Received transcoding-done signal - completing export");
    
    // Determine which export type just completed
    const completedType = currentExportType || "gif";
    const completionMessages = {
      "gif": "GIF COMPLETED!",
      "video": "MP4 COMPLETED!",
      "frames": "ZIP COMPLETED!",
      "post": "TAPE POSTED!",
      "webp": "WEBP COMPLETED!",
      "animwebp": "ANIMATED WEBP COMPLETED!",
      "apng": "APNG COMPLETED!"
    };
    
    // Show completion message
    completionMessage = completionMessages[completedType] || "EXPORT COMPLETED!";
    completionMessageTimer = 180;
    
    // Reset all export state
    isPrinting = false;
    isExportingGIF = false;
    isExportingFrames = false;
    isExportingWebP = false;
    isExportingAnimWebP = false;
    isExportingAPNG = false;
    isPostingTape = false;
    currentExportType = "";
    currentExportPhase = "";
    exportStatusMessage = "";
    exportStartTime = 0;
    progressHistory = [];
    printProgress = 0;
    
    // Re-enable all buttons
    if (postBtn) postBtn.disabled = false;
    if (gifBtn) gifBtn.disabled = false;
    if (mp4Btn) mp4Btn.disabled = false;
    if (zipBtn) zipBtn.disabled = false;
    
    requestPaint();
  }
}

// 🎯 act (also handles system messages via event.is)
function handleSystemMessage({ event: e, rec, needsPaint, jump }) {
  if (debug) {
    console.log("🎯 Video piece received system event:", e.type, "- isPostingTape:", isPostingTape, e);
  }
  
  if (typeof needsPaint === "function") {
    requestPaint = needsPaint;
  }
  const completeExport = (type, message) => {
    const label = message ?? `${(type || "export").toUpperCase()} COMPLETED!`;
    completionMessage = label;
    completionMessageTimer = 180;
    isPrinting = false;
    isExportingGIF = false;
    isExportingFrames = false;
    isExportingWebP = false;
    isExportingAnimWebP = false;
    isExportingAPNG = false;
    isPostingTape = false;
    currentExportType = "";
    currentExportPhase = "";
    exportStatusMessage = "";
    exportStartTime = 0;
    progressHistory = [];
    if (postBtn) postBtn.disabled = false;
    if (gifBtn) gifBtn.disabled = false;
    if (mp4Btn) mp4Btn.disabled = false;
    if (zipBtn) zipBtn.disabled = false;
    requestPaint();
    printProgress = 0;
    if (!useExtendedProgressBar && rec) {
      rec.tapeProgress = 0;
    }
  };
  
  // Handle upload progress (from 90% to 100% as file uploads)
  if (e.is("upload:progress")) {
    console.log("📤 Upload progress:", e.content);
    
    // Map upload progress from 0-1 to 90-100% of overall progress
    // Transcode goes 0-90%, upload goes 90-100%
    const uploadProgress = typeof e.content === "number" ? e.content : 0;
    printProgress = 0.9 + (uploadProgress * 0.1); // 90% + (0-10%)
    
    console.log(`📊 Upload progress: ${Math.floor(uploadProgress * 100)}% -> Overall: ${Math.floor(printProgress * 100)}%`);
    
    requestPaint();
    return true;
  }
  
  // Handle tape:posted callback (successful tape upload)
  if (e.is("tape:posted")) {
    console.log("✅ Tape posted successfully:", e.content);
    
    const { code, slug } = e.content || {};
    
    // Store the code for HUD label display
    postedTapeCode = code;
    
    // Complete the export flow (sets progress to 100%)
    completeExport("post", code ? `POSTED! !${code}` : "POSTED!");
    
    console.log(`📼 Tape posted: code=!${code}, slug=${slug}`);
    console.log(`📼 POST button will show "POSTED" (disabled), HUD will show !${code}`);
    
    requestPaint(); // Force repaint to show new button state
    
    return true;
  }
  
  // Handle tape:post-error callback (failed tape upload)
  if (e.is("tape:post-error")) {
    console.error("❌ Tape post error:", e.content);
    
    isPrinting = false;
    currentExportType = "";
    postBtn.disabled = false;
    
    completionMessage = "POST FAILED";
    completionMessageTimer = 180; // 3 seconds at 60fps
    
    requestPaint();
    
    return true;
  }
  
  // Handle detailed export status messages
  if (e.is("recorder:export-status")) {
    console.log("🎯 Video piece received status:", e.content);
    
    if (e.content?.message) {
      exportStatusMessage = e.content.message;
    }
    
    if (e.content?.phase) {
      currentExportPhase = e.content.phase;
    }
    requestPaint();
    
    return true;
  }

  // Handle export progress updates for all export types
  if (e.is("recorder:export-progress") || e.is("recorder:transcode-progress")) {
    console.log("🎯 Video piece received progress:", e.is("recorder:export-progress") ? "export-progress" : "transcode-progress", e);
    console.log("🎯 Current export state - isPrinting:", isPrinting, "isPostingTape:", isPostingTape, "isExportingGIF:", isExportingGIF, "currentExportType:", currentExportType);
    
    if (e.progress !== undefined || (e.is("recorder:transcode-progress") && typeof e.content === "number")) {
      // Handle both message formats: {progress, type} and direct number content
      const progress = e.progress !== undefined ? e.progress : (typeof e.content === "object" ? e.content.progress : e.content);
      const exportType = e.is("recorder:export-progress")
        ? (e.content?.type || currentExportType || "gif")
        : currentExportType || "video"; // For export-progress, fall back to current export type
      
      console.log("🎯 Processing progress update:", progress, "for type:", exportType);
      
      // Update status message if provided
      if (e.message || e.content?.message) {
        exportStatusMessage = e.message || e.content.message;
      }
      if (e.phase || e.content?.phase) {
        currentExportPhase = e.phase || e.content.phase;
      }
      
      const isValidExport = 
        (exportType === "gif" && isExportingGIF) ||
        (exportType === "webp" && isExportingWebP) ||
        (exportType === "animwebp" && isExportingAnimWebP) ||
        (exportType === "apng" && isExportingAPNG) ||
        (exportType === "frames" && isExportingFrames) ||
        (exportType === "video" && isPrinting) ||
        (exportType === "post" && (isPrinting || isPostingTape)) ||
        // Handle transcode progress for any active export
        (e.is("recorder:transcode-progress") && (isPrinting || isPostingTape || isExportingGIF || isExportingWebP || isExportingAnimWebP || isExportingAPNG || isExportingFrames));
        
      console.log("🎯 isValidExport check:", isValidExport, "- conditions:", {
        videoAndPrinting: exportType === "video" && isPrinting,
        gifAndExporting: exportType === "gif" && isExportingGIF,
        postAndPosting: exportType === "post" && (isPrinting || isPostingTape),
        transcodeAndAnyExport: e.is("recorder:transcode-progress") && (isPrinting || isPostingTape || isExportingGIF || isExportingWebP || isExportingAnimWebP || isExportingAPNG || isExportingFrames),
        actualExportType: exportType,
        actualIsPostingTape: isPostingTape
      });
        
      if (isValidExport) {
        const oldProgress = printProgress;
        printProgress = progress;
        
        console.log(`📊 Export progress updated: ${Math.floor(progress * 100)}% (was ${Math.floor(oldProgress * 100)}%) - type: ${exportType}`);
        
        // Track progress history for ETA calculation
        const now = performance.now();
        if (exportStartTime === 0) {
          exportStartTime = now;
        }
        
        progressHistory.push({ time: now, progress: progress });
        // Keep only recent history (last 10 seconds)
        progressHistory = progressHistory.filter(h => now - h.time < 10000);
        
        if (rec) {
          // Mirror progress into the VHS overlay system so the red tape bar animates
          rec.tapeProgress = Math.min(Math.max(progress, 0.01), 0.999);
        }

        console.log(`📊 Export progress: ${Math.floor(progress * 100)}% (${exportType}) - ${exportStatusMessage}`);
        requestPaint();

        if (exportType === "frames" && progress >= 0.999 && isExportingFrames) {
          completeExport("frames", "ZIP COMPLETED!");
          if (typeof rec?.present === "function") {
            rec.present();
          }
        }
      }
    }
    return true;
  }

  // Handle export completion for all export types
  if (e.is("recorder:export-complete")) {
    console.log("🎯 Video piece received completion:", e);
    const exportType = e.content?.type; // Get the actual export type from content
    const isValidExportComplete = 
      (exportType === "gif" && isExportingGIF) ||
      (exportType === "webp" && isExportingWebP) ||
      (exportType === "animwebp" && isExportingAnimWebP) ||
      (exportType === "apng" && isExportingAPNG) ||
      (exportType === "frames" && isExportingFrames) ||
      (exportType === "video" && isPrinting);
      
    console.log("🎯 Completion validation:", {
      messageType: e.type,
      exportType: exportType,
      isExportingGIF,
      isPrinting,
      isValidExportComplete
    });
      
    if (isValidExportComplete) {
      console.log(`✅ ${exportType?.toUpperCase() || "Export"} completed successfully!`, e.content?.filename || "");
      completeExport(exportType, `${exportType?.toUpperCase() || "EXPORT"} COMPLETED!`);
      if (typeof rec?.present === "function") {
        rec.present();
      }
    }
    return true;
  }

  if (
    e.is("recorder:present-progress") ||
    e.is("recorder:present-playing") ||
    e.is("recorder:present-paused") ||
    e.is("recorder:presented") ||
    e.is("recorder:unpresented") ||
    e.is("recorder:rolling:ended") ||
    e.is("recorder:printing:started") ||
    e.is("recorder:printed")
  ) {
    requestPaint();
    return true;
  }

  return false;
}

// 📨 Receive (Handles direct messages from the system)
function receive(e) {
  console.log("🎯 Video receive() called with event:", e?.type, e);
  
  if (!e || typeof e.is !== "function") {
    console.warn("🎯 Event missing or e.is() not a function");
    return false;
  }

  // Handle tape info reply from BIOS
  if (e.is("tape:info-reply")) {
    console.log("📼 Received tape info:", e.content);
    tapeInfo = e.content;
    return true;
  }

  // 📼 MP4-tape playback callbacks (kind:"mp4" video-backed tapes).
  if (e.is("tape:mp4-ready")) {
    isMp4Tape = true;
    mp4Playing = true;
    requestPaint();
    return true;
  }
  if (e.is("tape:mp4-playing")) {
    mp4Playing = true;
    requestPaint();
    return true;
  }
  if (e.is("tape:mp4-paused")) {
    mp4Playing = false;
    requestPaint();
    return true;
  }
  if (e.is("tape:playback-progress") && isMp4Tape) {
    const p = e.content?.progress;
    if (typeof p === "number") {
      mp4Progress = p;
      requestPaint();
    }
    return true;
  }

  // Handle AudioContext state updates from BIOS
  if (e.is("tape:audio-context-state")) {
    console.log("🎵 ✅ Video piece received AudioContext state:", e.content);
    audioContextState = e.content?.state || "suspended";
    hasAudioContext = !!e.content?.hasAudio;
    console.log(`🎵 ✅ AudioContext state updated: ${audioContextState}, hasAudio: ${hasAudioContext}`);
    return true;
  }

  // Debug: Log all message types to see what we're missing
  if (e.type && e.type.includes("audio") || e.type && e.type.includes("tape")) {
    console.log("🎯 🎵 AUDIO/TAPE MESSAGE:", e.type, e);
  }

  // Helper function to complete export and reset UI
  const completeExport = (type, message) => {
    const label = message ?? `${(type || "export").toUpperCase()} COMPLETED!`;
    completionMessage = label;
    completionMessageTimer = 180;
    isPrinting = false;
    isExportingGIF = false;
    isExportingFrames = false;
    isExportingWebP = false;
    isExportingAnimWebP = false;
    isExportingAPNG = false;
    isPostingTape = false;
    currentExportType = "";
    currentExportPhase = "";
    exportStatusMessage = "";
    exportStartTime = 0;
    progressHistory = [];
    printProgress = 0;
    if (postBtn) postBtn.disabled = false;
    if (gifBtn) gifBtn.disabled = false;
    if (mp4Btn) mp4Btn.disabled = false;
    if (zipBtn) zipBtn.disabled = false;
    requestPaint();
  };

  // Handle export completion
  if (e.is("recorder:export-complete")) {
    console.log("🎯 Video receive() handling export-complete:", e);
    const exportType = e.content?.type;
    const isValidExportComplete = 
      (exportType === "gif" && isExportingGIF) ||
      (exportType === "webp" && isExportingWebP) ||
      (exportType === "animwebp" && isExportingAnimWebP) ||
      (exportType === "apng" && isExportingAPNG) ||
      (exportType === "frames" && isExportingFrames) ||
      (exportType === "video" && isPrinting);
      
    console.log("🎯 Export completion validation:", {
      exportType,
      isExportingGIF,
      isPrinting,
      isValidExportComplete
    });
      
    if (isValidExportComplete) {
      console.log(`✅ ${exportType?.toUpperCase() || "Export"} completed in receive()!`, e.content?.filename || "");
      completeExport(exportType, `${exportType?.toUpperCase() || "EXPORT"} COMPLETED!`);
    }
    return true;
  }

  // Handle export progress updates for all export types
  if (e.is("recorder:export-progress") || e.is("recorder:transcode-progress")) {
    console.log("🎯 Video piece received progress:", e.is("recorder:export-progress") ? "export-progress" : "transcode-progress", e);
    console.log("🎯 Current export state - isPrinting:", isPrinting, "isPostingTape:", isPostingTape, "isExportingGIF:", isExportingGIF, "currentExportType:", currentExportType);
    
    if (e.progress !== undefined || (e.is("recorder:transcode-progress") && typeof e.content === "number")) {
      // Handle both message formats: {progress, type} and direct number content
      const progress = e.progress !== undefined ? e.progress : (typeof e.content === "object" ? e.content.progress : e.content);
      const exportType = e.is("recorder:export-progress")
        ? (e.content?.type || currentExportType || "gif")
        : currentExportType || "video"; // For export-progress, fall back to current export type
      
      console.log("🎯 Processing progress update:", progress, "for type:", exportType);
      
      // Update status message if provided
      if (e.message || e.content?.message) {
        exportStatusMessage = e.message || e.content.message;
      }
      if (e.phase || e.content?.phase) {
        currentExportPhase = e.phase || e.content.phase;
      }
      
      const isValidExport = 
        (exportType === "gif" && isExportingGIF) ||
        (exportType === "webp" && isExportingWebP) ||
        (exportType === "animwebp" && isExportingAnimWebP) ||
        (exportType === "apng" && isExportingAPNG) ||
        (exportType === "frames" && isExportingFrames) ||
        (exportType === "video" && isPrinting) ||
        (exportType === "post" && (isPrinting || isPostingTape)) ||
        // Handle transcode progress for any active export
        (e.is("recorder:transcode-progress") && (isPrinting || isPostingTape || isExportingGIF || isExportingWebP || isExportingAnimWebP || isExportingAPNG || isExportingFrames));
        
      console.log("🎯 isValidExport check:", isValidExport, "- conditions:", {
        videoAndPrinting: exportType === "video" && isPrinting,
        gifAndExporting: exportType === "gif" && isExportingGIF,
        postAndPosting: exportType === "post" && (isPrinting || isPostingTape),
        transcodeAndAnyExport: e.is("recorder:transcode-progress") && (isPrinting || isPostingTape || isExportingGIF || isExportingWebP || isExportingAnimWebP || isExportingAPNG || isExportingFrames),
        actualExportType: exportType,
        actualIsPostingTape: isPostingTape
      });
        
      if (isValidExport) {
        const oldProgress = printProgress;
        printProgress = progress;
        
        console.log(`📊 GIF Export progress: ${Math.floor(progress * 100)}% (printProgress updated from ${Math.floor(oldProgress * 100)}% to ${Math.floor(printProgress * 100)}%) - type: ${exportType}`);
        
        // Track progress history for ETA calculation
        const now = performance.now();
        if (exportStartTime === 0) {
          exportStartTime = now;
        }
        
        progressHistory.push({ time: now, progress: progress });
        // Keep only recent history (last 10 seconds)
        progressHistory = progressHistory.filter(h => now - h.time < 10000);
        
        // Note: rec is not available in receive() context, VHS overlay updates happen through disk.mjs
        // Mirror progress happens via rec.printProgress in disk.mjs handler

        requestPaint();

        if (exportType === "frames" && progress >= 0.999 && isExportingFrames) {
          // Complete frames export
          completionMessage = "ZIP COMPLETED!";
          completionMessageTimer = 180;
          isPrinting = false;
          isExportingFrames = false;
          currentExportType = "";
          exportStatusMessage = "";
          printProgress = 0;
          if (postBtn) postBtn.disabled = false;
          if (gifBtn) gifBtn.disabled = false;
          if (mp4Btn) mp4Btn.disabled = false;
          if (zipBtn) zipBtn.disabled = false;
        }
      }
    }
    return true;
  }
  
  // Handle other recorder events
  if (e.is("recorder:export-status")) {
    if (e.content?.message) {
      exportStatusMessage = e.content.message;
      requestPaint();
    }
    return true;
  }
  
  // Handle tape loading progress events
  if (e.is("tape:download-progress")) {
    if (isLoadingTape) {
      const { progress, receivedBytes, totalBytes } = e.content || {};
      tapeLoadPhase = "download";
      tapeLoadProgress = progress !== null ? progress * 0.5 : 0; // Download is 0-50%
      
      if (totalBytes && receivedBytes) {
        const mb = (receivedBytes / 1024 / 1024).toFixed(2);
        const totalMb = (totalBytes / 1024 / 1024).toFixed(2);
        tapeLoadMessage = `DOWNLOADING ${mb}/${totalMb} MB`;
      } else {
        tapeLoadMessage = "DOWNLOADING ZIP";
      }
      
      requestPaint();
    }
    return true;
  }
  
  if (e.is("tape:load-progress")) {
    if (isLoadingTape) {
      const { phase, progress, loadedFrames, totalFrames } = e.content || {};
      
      if (phase === "frames") {
        tapeLoadPhase = "frames";
        tapeLoadProgress = 0.5 + (progress * 0.5); // Frames is 50-100%
        tapeLoadMessage = `LOADING FRAMES ${loadedFrames}/${totalFrames}`;
      }
      
      // Check if loading is complete
      if (progress === 1) {
        setTimeout(() => {
          isLoadingTape = false;
          tapeLoadPhase = "";
          tapeLoadProgress = 0;
          tapeLoadMessage = "";
          
          // Request waveform data from the loaded tape audio
          apiSend({ type: "tape:get-waveform" });
          
          requestPaint();
        }, 500); // Brief delay to show 100% completion
      }
      
      requestPaint();
    }
    return true;
  }
  
  // Handle tape:posted callback (successful tape upload)
  if (e.is("tape:posted")) {
    console.log("✅ Tape posted successfully:", e.content);
    
    const { code, slug } = e.content || {};
    
    // Store the code for HUD label display
    postedTapeCode = code;
    
    // Set progress to 100% and complete the export
    printProgress = 1.0;
    completionMessage = code ? `POSTED! !${code}` : "POSTED!";
    completionMessageTimer = 180;
    isPrinting = false;
    isPostingTape = false;
    currentExportType = "";
    currentExportPhase = "";
    exportStatusMessage = "";
    exportStartTime = 0;
    progressHistory = [];
    
    // Re-enable other export buttons, but POST button will be disabled by paint logic
    if (gifBtn) gifBtn.disabled = false;
    if (mp4Btn) mp4Btn.disabled = false;
    if (zipBtn) zipBtn.disabled = false;
    
    console.log(`📼 Tape posted: code=!${code}, slug=${slug}`);
    console.log(`📼 POST button will show "POSTED" (disabled), HUD will show !${code}`);
    
    requestPaint();
    
    return true;
  }
  
  // Handle waveform data response from tape audio
  if (e.is("tape:waveform")) {
    console.log("🌊 Received tape waveform data:", e.content?.length, "samples");
    tapeWaveform = e.content;
    requestPaint();
    return true;
  }
  
  // Handle tape:post-error callback (failed tape upload)
  if (e.is("tape:post-error")) {
    console.error("❌ Tape post error:", e.content);
    
    completionMessage = "UPLOAD FAILED!";
    completionMessageTimer = 180;
    isPrinting = false;
    isPostingTape = false;
    currentExportType = "";
    exportStatusMessage = "";
    printProgress = 0;
    if (postBtn) postBtn.disabled = false;
    if (gifBtn) gifBtn.disabled = false;
    if (mp4Btn) mp4Btn.disabled = false;
    if (zipBtn) zipBtn.disabled = false;
    requestPaint();
    
    return true;
  }
  
  return false;
}

// Called when leaving the video disk (e.g., pressing escape to go back to prompt)
function leave({ send }) {
  console.log("📼 Leaving video disk - stopping tape playback");
  isScrubbing = false;
  inertiaActive = false;
  brakeHolding = false;
  brakeResume = false;
  holdTime = 0;
  elasticAnchorX = null;
  scrollScrubbing = false;
  tapDipTime = -1;
  wheelActive = false;
  sustained = false;
  steadyHold = false;
  flickVel = 0;
  scrubSpeed = 0;
  scrubCurrentProgress = 0;
  scrubStripBtn = null;
  scrubMoved = false;
  scrubAudioSpeed = 1;
  isMp4Tape = false;
  mp4BackBtn = null;
  synthAuto = false;
  autoScript = null;
  autoSeg = null;

  // Stop any playing tape audio/video
  send({ type: "tape:audio-shift", content: 0 });
  send({ type: "tape:stop" });
}

export { boot, paint, sim, act, signal, receive, leave };

// 📚 Library (Useful functions used throughout the piece)

// 🎛️ Make sure the scrub drive owns playback (used by deck keys) — engages
// a sustained 1× drive if nothing else is driving.
function ensureDriven(rec, send) {
  if (
    isScrubbing ||
    sustained ||
    wheelActive ||
    brakeResume ||
    inertiaActive ||
    tapDipTime >= 0
  ) {
    return;
  }
  scrubCurrentProgress = rec.presentProgress || 0;
  scrubSpeed = 1;
  sustained = true;
  if (!rec.playing) rec.play();
  send({
    type: "recorder:present:seek",
    content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
  });
}

// 🧪 Synthtape autopilot — a scripted stand-in for the finger. Each segment
// either lets the tape roll, holds a scrub speed, or releases into inertia,
// then the observed progress delta is graded against the physics. All output
// is console-greppable by 🧪 so a headless run can assert on it.
function autopilotProgress(rec) {
  return isScrubbing || inertiaActive
    ? scrubCurrentProgress
    : (rec.presentProgress ?? scrubCurrentProgress);
}

function autopilot(rec, send, simDt) {
  if (!autoScript) {
    autoScript = [
      { name: "roll", mode: "play", frames: 90 },
      { name: "fast forward", mode: "scrub", speed: 3, frames: 90 },
      { name: "release into inertia", mode: "coast" },
      { name: "reverse", mode: "scrub", speed: -2, frames: 90 },
      { name: "release from reverse", mode: "coast" },
      { name: "slow crawl", mode: "scrub", speed: 0.5, frames: 60 },
      { name: "final release", mode: "coast" },
      { name: "tap dip", mode: "dip" },
      { name: "scratch", mode: "scratch", frames: 150 },
      { name: "scratch release", mode: "coast" },
      { name: "fast scratch", mode: "scratch", frames: 120, hz: 4, amp: 2.2 },
      { name: "fast scratch release", mode: "coast" },
      { name: "touch brake", mode: "brake", frames: 90 },
      { name: "brake release spin-up", mode: "brakeRelease" },
    ];
    console.log(`🧪 SYNTHSCRUB autopilot engaged: ${autoScript.length} segments`);
  }

  if (!autoSeg) {
    autoSeg = autoScript.shift();
    if (!autoSeg) {
      const failed = autoChecks.filter((c) => !c.pass);
      console.log(
        `🧪 SYNTHSCRUB REPORT: ${autoChecks.length - failed.length}/${autoChecks.length} checks passed`,
      );
      autoChecks.forEach((c) =>
        console.log(`🧪 ${c.pass ? "PASS" : "FAIL"} — ${c.name}: ${c.detail}`),
      );
      synthAuto = false;
      return;
    }
    autoTimer = 0;
    autoSegStartProgress = autopilotProgress(rec);
    autoSegStartTime = performance.now();
    autoDtSum = 0;
    autoPhysTicks = 0;
    autoSpeedMin = Infinity;
    autoSpeedMax = -Infinity;
    autoPrevPos = null;
    autoAnomalies = 0;
    autoMotionSum = 0;
    if (autoSeg.mode === "scrub" || autoSeg.mode === "scratch") {
      if (!isScrubbing) {
        inertiaActive = false;
        sustained = false;
        wheelActive = false;
        // Position must be captured BEFORE isScrubbing flips — after the
        // flip autopilotProgress() returns the stale scrub position.
        scrubCurrentProgress = autoSegStartProgress;
        isScrubbing = true;
        wasPlayingBeforeScrub = true; // Keep rolling after each release
        if (!rec.playing) rec.play();
        send({
          type: "recorder:present:seek",
          content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
        });
      }
      scrubSpeed = autoSeg.mode === "scrub" ? autoSeg.speed : 0;
    } else if (autoSeg.mode === "coast") {
      isScrubbing = false;
      if (Math.abs(scrubSpeed) > 0.1) {
        inertiaActive = true;
      } else {
        // Nothing to coast — end the scrub cleanly.
        scrubSpeed = 0;
        inertiaActive = false;
        nudgeTapeAudioSpeed(send, 1);
        send({
          type: "recorder:present:seek",
          content: { progress: scrubCurrentProgress, scrubEnd: true },
        });
      }
    } else if (autoSeg.mode === "brake") {
      // Same engage the held finger triggers in sim. Position captured
      // before the isScrubbing flip, as above.
      inertiaActive = false;
      brakeResume = false;
      sustained = false;
      wheelActive = false;
      resumeTarget = 1;
      scrubCurrentProgress = autoSegStartProgress;
      isScrubbing = true;
      brakeHolding = true;
      scrubMoved = true;
      wasPlayingBeforeScrub = true;
      scrubSpeed = 1;
      if (!rec.playing) rec.play();
      send({
        type: "recorder:present:seek",
        content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
      });
    } else if (autoSeg.mode === "brakeRelease") {
      isScrubbing = false;
      brakeHolding = false;
      brakeResume = true;
    } else if (autoSeg.mode === "dip") {
      // Same state a single tap triggers.
      inertiaActive = false;
      brakeResume = false;
      sustained = false;
      wheelActive = false;
      isScrubbing = false;
      dipBase = 1;
      scrubCurrentProgress = autoSegStartProgress;
      scrubSpeed = 1;
      tapDipTime = 0;
      if (!rec.playing) rec.play();
      send({
        type: "recorder:present:seek",
        content: { progress: scrubCurrentProgress, speedScrub: true, scrubbing: true },
      });
    }
    console.log(
      `🧪 SYNTHSCRUB segment "${autoSeg.name}" from ${(autoSegStartProgress * 100).toFixed(1)}%`,
    );
    return;
  }

  autoTimer += 1;
  if (autoSeg.mode === "scratch") {
    // 🎚️ Musical scratch: the rate rocks like a hand on the reel —
    // amplitude and tempo per segment (default ±3.2× at 2Hz).
    const amp = autoSeg.amp || 3.2;
    const hz = autoSeg.hz || 2;
    scrubSpeed = amp * Math.sin((autoTimer / 120) * Math.PI * 2 * hz);
  }
  if (autoSeg.mode === "scrub") {
    scrubSpeed = autoSeg.speed; // Hold the drag
    // Trace per-tick steps: a step against the commanded direction (or far
    // short of it) means something outside the physics rewrote the position.
    if (autoPrevPos !== null && autoAnomalies < 12) {
      const step = scrubCurrentProgress - autoPrevPos;
      const expectedStep =
        (autoSeg.speed * simDt) / (tapeInfo?.totalDuration || 10);
      if (
        Math.sign(step) !== Math.sign(expectedStep) ||
        Math.abs(step) < Math.abs(expectedStep) * 0.5
      ) {
        autoAnomalies += 1;
        console.log(
          `🧪 STEP ANOMALY tick ${autoTimer}: pos ${(scrubCurrentProgress * 100).toFixed(2)}% step ${(step * 100).toFixed(3)}% expected ${(expectedStep * 100).toFixed(3)}% presentProgress ${((rec.presentProgress ?? 0) * 100).toFixed(2)}%`,
        );
      }
    }
    autoPrevPos = scrubCurrentProgress;
  }

  const done =
    autoSeg.mode === "coast"
      ? (!inertiaActive && !isScrubbing && !brakeResume) || autoTimer > 600
      : autoSeg.mode === "brakeRelease"
        ? !brakeResume || autoTimer > 600
        : autoSeg.mode === "dip"
          ? tapDipTime < 0 || autoTimer > 600
          : autoTimer >= autoSeg.frames;
  if (!done) return;

  const progress = autopilotProgress(rec);
  let delta = progress - autoSegStartProgress;
  // The tape wraps at the seam now — unwrap the delta.
  if (delta > 0.5) delta -= 1;
  else if (delta < -0.5) delta += 1;
  const wallSeconds = (performance.now() - autoSegStartTime) / 1000;
  let pass = true;
  let detail = `Δ ${(delta * 100).toFixed(1)}% over ${wallSeconds.toFixed(2)}s [phys ${autoPhysTicks} ticks, ${autoDtSum.toFixed(2)}s, speed ${autoSpeedMin === Infinity ? "-" : autoSpeedMin.toFixed(2)}..${autoSpeedMax === -Infinity ? "-" : autoSpeedMax.toFixed(2)}, dur ${tapeInfo?.totalDuration ?? "null"}]`;
  if (autoSeg.mode === "play") {
    pass = delta > 0;
    detail += pass ? " (advanced under normal play)" : " (tape did not advance)";
  } else if (autoSeg.mode === "scrub") {
    const totalDuration = tapeInfo?.totalDuration || 10; // Same fallback as the physics
    // Effective speed from the true motion integral — Δposition can't
    // count multiple wraps around a short tape, the integral can.
    const effective =
      (autoMotionSum * totalDuration) / Math.max(wallSeconds, 0.001);
    pass =
      Math.sign(autoMotionSum) === Math.sign(autoSeg.speed) &&
      Math.abs(effective / autoSeg.speed - 1) < 0.25;
    detail += ` effective ${effective.toFixed(2)}× vs commanded ${autoSeg.speed}×`;
  } else if (autoSeg.mode === "coast") {
    pass = !inertiaActive && !isScrubbing && scrubSpeed === 0;
    if (!pass) {
      detail += ` (stuck: speed=${scrubSpeed.toFixed(2)})`;
      // Force a clean state so later segments still run.
      inertiaActive = false;
      scrubSpeed = 0;
      nudgeTapeAudioSpeed(send, 1);
    } else {
      detail += " (inertia converged)";
    }
  } else if (autoSeg.mode === "scratch") {
    const totalDuration = tapeInfo?.totalDuration || 10;
    const amp = autoSeg.amp || 3.2;
    const driftSecs = Math.abs(autoMotionSum * totalDuration);
    pass =
      autoSpeedMax > amp * 0.45 && autoSpeedMin < -amp * 0.45 && driftSecs < 0.7;
    detail += ` (rate swung ${autoSpeedMin.toFixed(1)}..${autoSpeedMax.toFixed(1)}×, net drift ${driftSecs.toFixed(2)}s)`;
  } else if (autoSeg.mode === "dip") {
    const totalDuration = tapeInfo?.totalDuration || 10;
    pass =
      tapDipTime < 0 &&
      autoSpeedMin < 0.3 &&
      autoSpeedMax <= 1.05 &&
      Math.abs(autoMotionSum * totalDuration) < 0.3;
    detail += ` (dipped to ${autoSpeedMin === Infinity ? "?" : autoSpeedMin.toFixed(2)}× and back)`;
  } else if (autoSeg.mode === "brake") {
    pass = scrubSpeed < 0.05;
    detail += ` (speed sagged to ${scrubSpeed.toFixed(3)}×)`;
    // Hold ends here; the next segment releases.
  } else if (autoSeg.mode === "brakeRelease") {
    pass = !brakeResume;
    if (!pass) {
      detail += " (spin-up never converged)";
      brakeResume = false;
      scrubSpeed = 0;
      nudgeTapeAudioSpeed(send, 1);
    } else {
      detail += " (spun back up to 1×)";
    }
  }
  autoChecks.push({ name: autoSeg.name, pass, detail });
  console.log(`🧪 SYNTHSCRUB ${pass ? "PASS" : "FAIL"} — ${autoSeg.name}: ${detail}`);
  autoSeg = null;
}
