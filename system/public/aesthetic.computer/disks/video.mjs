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
let gifBtn;
let mp4Btn;
let zipBtn;

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

// Tape info from bios
let tapeInfo = null; // { frameCount, totalDuration, hasAudio }
const MAX_TAPE_DURATION = 30; // Must match server limit

// API references needed by receive() function
let apiJump = null; // Stored from boot() for use in receive()
let apiSend = null; // Stored from boot() for sending messages to BIOS

// AudioContext state from BIOS (since sound.bios is false for video piece)
let audioContextState = "suspended"; // suspended, running, closed
let hasAudioContext = false;
let audioManuallyActivated = false; // Track if user has manually activated audio

// Scrubbing state (STAMPLE-style lazy following)
let isScrubbing = false;
let scrubSpeed = 1.0; // Playback speed multiplier
let scrubVelocity = 0; // Track scrub velocity for inertia (in progress units per frame, -1 to 1)
let inertiaActive = false;
let scrubStartY = 0; // Y position where scrub started
let lastDragX = 0; // Track last drag position for velocity calculation
let tapeAudioSound = null; // Reference to the playing tape audio sound for pitch shifting
let scrubStartProgress = 0; // Progress when scrub started (0-1)
let scrubAccumulatedDelta = 0; // Accumulated horizontal movement during scrub
let wasPlayingBeforeScrub = false; // Track if video was playing when scrub started
let tapeWaveform = null; // Audio waveform data for visualization

// STAMPLE-style lazy needle that follows the drag with lag
let targetProgress = 0; // Where the user is dragging (yellow line)
let needleProgress = 0; // Where video is actually playing (cyan line - lags behind)
let needleVelocity = 0; // Velocity of the needle (for smooth motion and inertia)

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

  // Always keep canvas transparent when presenting so underlay shows through.
  // Buttons and UI are drawn on top; no opaque fill ever blocks the video.
  if (presenting) {
    wipe(0, 0, 0, 0);
    if (!playing && !isPrinting) {
      // Paused: show "||" indicator without a black background
      ink(255, 200).write("||", { center: "xy" });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }
  }

  if (!presenting && exportAvailable && !isPrinting) {
    wipe(0, 0, 0, 0); // Transparent
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
    
    if (!postedTapeCode && tapeWithinDurationLimit) {
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

    if (!zipBtn) {
      zipBtn = new ui.TextButton("ZIP", { left: 6, bottom: 6, screen });
    }
    zipBtn.reposition({ left: 6, bottom: 6, screen });
    zipBtn.disabled = disableExports;
    zipBtn.paint(api);

    if (!gifBtn) {
      gifBtn = new ui.TextButton("GIF", { left: 38, bottom: 6, screen });
    }
    gifBtn.reposition({ left: 38, bottom: 6, screen });
    gifBtn.disabled = disableExports;
    gifBtn.paint(api);

    if (!mp4Btn) {
      mp4Btn = new ui.TextButton("MP4", { left: 70, bottom: 6, screen });
    }
    mp4Btn.reposition({ left: 70, bottom: 6, screen });
    mp4Btn.disabled = disableExports;
    mp4Btn.paint(api);
  } else {
    postBtn = undefined;
    gifBtn = undefined;
    mp4Btn = undefined;
    zipBtn = undefined;
  }
  
  // Update HUD label to show tape code after posting
  if (postedTapeCode) {
    hud.label(`!${postedTapeCode}`);
  }

  // Show "TAP TO ENABLE AUDIO" prompt if audio context is suspended
  const audioSuspended = !audioManuallyActivated && hasAudioContext && presenting;
  
  
  if (audioSuspended && !isPrinting) {
    const pulse = Math.abs(Math.sin(paintCount * 0.1)); // Pulse effect
    const alpha = Math.floor(128 + pulse * 127); // Fade between 128-255
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    
    // Semi-transparent dark overlay
    ink(0, 0, 0, 160).box(0, 0, screen.width, screen.height);
    
    // Pulsing prompt text
    ink(255, 200, 100, alpha).write("TAP TO ENABLE AUDIO", {
      x: centerX,
      y: centerY,
      center: "xy",
    });
    
    // Smaller instruction text
    ink(200, 200, 200, alpha * 0.8).write("Audio requires user interaction", {
      x: centerX,
      y: centerY + 16,
      center: "xy",
    });
    
    requestPaint?.(); // Keep animating the pulse
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
      const dots = Math.floor(paintCount / 20) % 4;
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

  // Show "NO VIDEO" message when there's no video available
  if (!presenting && paintCount > 16n) {
    wipe(40, 0, 0).ink(180, 0, 0).write("NO VIDEO", { center: "xy" });
  }
  
  // Draw full waveform visualization and scrub overlay
  if ((isScrubbing || inertiaActive) && rec?.presenting) {
    // Calculate current progress position for vertical playhead indicator
    const currentProgress = isScrubbing 
      ? Math.max(0, Math.min(1, scrubStartProgress + (scrubAccumulatedDelta / screen.width)))
      : (rec.presentProgress || 0);
    const progressX = Math.floor(currentProgress * screen.width);
    
    // ALWAYS draw background to show scrub is active
    ink(40, 40, 40, 150).box(0, screen.height * 0.25, screen.width, screen.height * 0.5);
    
    // Check if sound.paint.waveform exists
    if (!sound?.paint?.waveform) {
      ink(255, 100, 100, 255).write("sound.paint.waveform missing", { center: "xy" });
    } else {
      // Try to get the tape audio waveform data
      let audioWaveform = tapeWaveform; // Use cached if available
      
      // If we don't have cached waveform yet, try to get it
      // Tape audio is stored at "tape:audio" in BIOS, not "tape:audio_<code>"
      if (!audioWaveform && postedTapeCode && sound.getSampleData && !tapeWaveform) {
        const tapeAudioId = "tape:audio"; // Tape audio is stored with this fixed ID
        
        // Call getSampleData asynchronously (only once)
        sound.getSampleData(tapeAudioId).then((data) => {
          if (data && data.length > 0) {
            tapeWaveform = data; // Cache it
          }
        }).catch(err => {
          console.log("🌊 ❌ getSampleData error:", err);
        });
      }
      
      // Fallback to live speaker waveform if nothing else available
      if (!audioWaveform && sound?.speaker?.waveforms?.left && sound.speaker.waveforms.left.length > 0) {
        audioWaveform = sound.speaker.waveforms.left;
      }
      
      // Draw full tape waveform if we have the audio buffer data
      if (audioWaveform && audioWaveform.length > 0) {
        // Compress waveform to ~128 points for cleaner visualization
        // arrCompress keeps every Nth element, so calculate N to get ~128 final points
        const targetPoints = 128;
        const skipN = Math.max(1, Math.floor(audioWaveform.length / targetPoints));
        const compressedWaveform = num.arrCompress(audioWaveform, skipN);
        
        // Draw waveform across full screen width, horizontally
        const waveformHeight = screen.height * 0.3; // Smaller height for horizontal layout
        const waveformY = (screen.height - waveformHeight) / 2;
        
        try {
          sound.paint.waveform(
            api,
            num.arrMax(compressedWaveform), // Max of compressed data
            compressedWaveform, // Already compressed
            0,
            waveformY,
            screen.width,
            waveformHeight,
            [255, 200, 0, 64], // Semi-transparent yellow
            { direction: "left-to-right" }, // Horizontal waveform
          );
        } catch (e) {
          console.error(`🌊 ❌ Error drawing waveform:`, e);
          ink(255, 100, 100, 255).write(`Error: ${e.message}`, { center: "xy" });
        }
      } else if (tapeWaveform && tapeWaveform.length > 0) {
        const targetPoints = 128;
        const skipN = Math.max(1, Math.floor(tapeWaveform.length / targetPoints));
        const compressedWaveform = num.arrCompress(tapeWaveform, skipN);
        
        const waveformHeight = screen.height * 0.3;
        const waveformY = (screen.height - waveformHeight) / 2;
        
        try {
          sound.paint.waveform(
            api,
            num.arrMax(compressedWaveform),
            compressedWaveform,
            0,
            waveformY,
            screen.width,
            waveformHeight,
            [255, 200, 0, 64],
            { direction: "left-to-right" },
          );
        } catch (e) {
          console.error(`🌊 ❌ Error drawing cached waveform:`, e);
          ink(255, 100, 100, 255).write(`Error: ${e.message}`, { center: "xy", y: screen.height / 2 + 20 });
        }
      }
    }
    
    // Draw vertical lines for scrub and playback positions (STAMPLE-style)
    
    // Calculate positions for both lines
    const scrubLineWidth = 1;
    
    // 1. YELLOW LINE: Shows where you're dragging (target position)
    const targetX = Math.floor(targetProgress * screen.width);
    const scrubColor = [255, 255, 0, 255]; // Bright yellow
    console.log(`📍 Drawing TARGET line (yellow): x=${targetX}, progress=${targetProgress.toFixed(3)}`);
    ink(...scrubColor).box(targetX, 0, scrubLineWidth, screen.height);
    
    // 2. CYAN LINE: Shows where video is actually playing (needle - lags behind target)
    const needleX = Math.floor(needleProgress * screen.width);
    const needleColor = [0, 255, 255, 255]; // Bright cyan
    console.log(`📍 Drawing NEEDLE line (cyan): x=${needleX}, progress=${needleProgress.toFixed(3)}, lag=${(targetProgress - needleProgress).toFixed(3)}`);
    ink(...needleColor).box(needleX, 0, scrubLineWidth, screen.height);
    
    // 3. Optional: Draw a line connecting them to show the "pull" distance
    if (isScrubbing && Math.abs(targetX - needleX) > 2) {
      ink(255, 255, 255, 128).line(targetX, screen.height / 2, needleX, screen.height / 2);
    }
    
    
    // Draw progress info at top corners (no center label)
    if (isScrubbing) {
      const speedText = `${scrubSpeed > 0 ? "+" : ""}${(scrubSpeed - 1).toFixed(2)}x`;
      const progressText = `${Math.floor(targetProgress * 100)}%`;
      
      ink(255, 200, 0, 255).write(speedText, { x: 8, y: 8 });
      ink(255, 200, 0, 255).write(progressText, { x: screen.width - 8, y: 8, right: true });
    } else if (inertiaActive) {
      const speedText = `${scrubSpeed.toFixed(2)}x`;
      const progressText = `${Math.floor(needleProgress * 100)}%`;
      
      ink(100, 200, 255, 200).write(speedText, { x: 8, y: 8 });
      ink(100, 200, 255, 200).write(progressText, { x: screen.width - 8, y: 8, right: true });
    }
    
    // Override rec.presentProgress for VHS bar to show needle position (where video is playing)
    rec.tapeProgress = needleProgress;
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

function sim({ needsPaint, rec, send }) {
  ellipsisTicker?.sim();
  frameCount++; // Increment frame counter for animations

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
      inertiaActive // Keep painting during inertia
    ) {
      needsPaint();
    }
  }
  
  // STAMPLE-style lazy needle following during scrubbing AND inertia
  // This runs every frame for super smooth physics
  if ((isScrubbing || inertiaActive) && rec?.presenting) {
    // Calculate the "pull" force from target to needle (EXPONENTIAL rubber band effect)
    const lag = targetProgress - needleProgress;
    
    // Exponential spring force - gets stronger the further you pull
    // This creates that super springy, rubber band feel
    const lagSquared = lag * Math.abs(lag); // Square but keep sign (exponential force)
    const springStrength = 0.08; // Base spring strength (MUCH weaker for slower motion)
    const targetVelocity = lagSquared * springStrength;
    
    // Smoothly blend current velocity toward target velocity (very responsive to build momentum)
    const acceleration = 0.6; // How quickly velocity changes (higher = snappier response)
    needleVelocity += (targetVelocity - needleVelocity) * acceleration;
    
    // Apply velocity to position (scale down for slower scrubbing speed)
    const oldNeedle = needleProgress;
    const velocityScale = 0.3; // SLOW DOWN the scrubbing motion (0.3 = 30% speed)
    needleProgress += needleVelocity * velocityScale;
    
    // Apply friction/dampening (less friction = more drift)
    let friction;
    if (isScrubbing) {
      friction = 0.88; // While dragging, moderate friction for springy feel
    } else {
      friction = 0.98; // During inertia, very little friction for LONG drift
    }
    needleVelocity *= friction;
    
    // Clamp needle to valid range but allow slight overshoot for bounce
    const overshootLimit = 0.08; // Allow more overshoot for bouncier feel
    needleProgress = Math.max(-overshootLimit, Math.min(1 + overshootLimit, needleProgress));
    
    // Bounce back from edges with damping
    if (needleProgress < 0) {
      needleProgress = 0;
      needleVelocity *= -0.5; // Stronger bounce back
    } else if (needleProgress > 1) {
      needleProgress = 1;
      needleVelocity *= -0.5; // Stronger bounce back
    }
    
    // Seek video to needle position - pass scrubbing state to avoid audio restart spam
    if (send) {
      send({ 
        type: "recorder:present:seek", 
        content: { 
          progress: Math.max(0, Math.min(1, needleProgress)),
          scrubbing: isScrubbing || inertiaActive  // Tell bios we're in scrub mode
        }
      });
    }
    
    // Stop inertia when velocity becomes very small AND close to target
    if (!isScrubbing && inertiaActive) {
      if (Math.abs(needleVelocity) < 0.0003 && Math.abs(lag) < 0.01) {
        inertiaActive = false;
        needleVelocity = 0;
        targetProgress = needleProgress; // Snap target to needle to avoid drift
        
        // Signal scrub end to resume audio at final position
        if (send) {
          send({ 
            type: "recorder:present:seek", 
            content: { 
              progress: Math.max(0, Math.min(1, needleProgress)),
              scrubEnd: true  // Tell bios to sync audio at final position
            }
          });
        }
        
        // Make sure playback continues at normal speed
        if (!rec.playing) {
          rec.play();
        }
        wasPlayingBeforeScrub = false;
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

  if (!rec.printing && !isPrinting) {
    const allowExport = exportAvailable && !isPostingTape;

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

    // Toggle play/pause when tapping on video (not on buttons)
    // Add scrubbing support: drag to seek, tap to play/pause
    const anyButtonDown = 
      postBtn?.down || 
      mp4Btn?.down || 
      gifBtn?.down || 
      zipBtn?.down;
    
    if (!anyButtonDown && !isPrinting && !isPostingTape && rec.presenting) {
      // Start scrubbing on drag start
      if (e.is("draw") && !isScrubbing) {
        isScrubbing = true;
        scrubStartY = e.y;
        lastDragX = e.x;
        scrubVelocity = 0;
        inertiaActive = false;
        scrubStartProgress = rec.presentProgress || 0; // Capture current progress
        scrubAccumulatedDelta = 0;
        wasPlayingBeforeScrub = rec.playing; // Remember if it was playing
        
        // Initialize STAMPLE-style positions
        targetProgress = scrubStartProgress; // Where you're dragging
        needleProgress = scrubStartProgress; // Where video is playing
        needleVelocity = 0; // Reset velocity for fresh scrub
        
        // DON'T pause playback - let audio continue with pitch shifting
        // if (rec.playing) {
        //   rec.pause();
        // }
        
        console.log(`📼 🎯 SCRUB MODE ACTIVATED at y=${scrubStartY}, progress=${scrubStartProgress.toFixed(3)}, wasPlaying=${wasPlayingBeforeScrub}, audioPlaying=${rec.playing}`);
      }
      
      // Scrub when dragging (adjust playback speed based on drag velocity)
      if (e.drag && isScrubbing) {
        // Calculate velocity from frame-to-frame movement
        const deltaX = e.x - lastDragX;
        lastDragX = e.x;
        
        // Accumulate delta for seeking
        scrubAccumulatedDelta += deltaX;
        
        // Calculate new TARGET progress based on accumulated movement (where you're dragging)
        // Scale: screen width = full video duration
        const progressDelta = scrubAccumulatedDelta / screen.width;
        targetProgress = scrubStartProgress + progressDelta;
        targetProgress = Math.max(0, Math.min(1, targetProgress)); // Clamp to 0-1
        
        // Just update target - let sim() handle the physics
        console.log(`📼 🎯 Scrubbing: delta=${deltaX}, target=${targetProgress.toFixed(3)}, needle=${needleProgress.toFixed(3)}, lag=${(targetProgress - needleProgress).toFixed(3)}`);
        
        triggerRender();
      }
      
      // Handle touch end - detect if it was a tap or a scrub
      if (e.is("lift")) {
        if (isScrubbing) {
          // Scrub ended - activate inertia to let it drift with physics
          const lag = Math.abs(targetProgress - needleProgress);
          const hasVelocity = Math.abs(needleVelocity) > 0.001;
          console.log(`📼 🎯 SCRUB MODE ENDED - needleVelocity: ${needleVelocity.toFixed(4)}, lag: ${lag.toFixed(4)}`);
          
          // Always start inertia when releasing - let the physics system handle the drift
          if (hasVelocity || lag > 0.01) {
            inertiaActive = true;
            console.log(`📼 💨 ✅ INERTIA ACTIVATED! velocity: ${needleVelocity.toFixed(4)}, will drift...`);
          } else {
            console.log(`📼 💨 ❌ No inertia - no movement detected`);
            needleVelocity = 0;
            targetProgress = needleProgress; // Snap together
          }
          
          isScrubbing = false;
          scrubStartY = 0;
          scrubAccumulatedDelta = 0;
        } else {
          // Regular tap - toggle play/pause
          console.log("📼 👆 TAP detected (not scrub)");
          // IMPORTANT: Check if user needs to manually activate audio FIRST
          // When AudioContext needs user gesture, video plays silently (rec.playing = true)
          // but we want the first tap to enable audio, not pause the video
          if (!audioManuallyActivated && hasAudioContext && rec.playing) {
            audioManuallyActivated = true;
            handleAudioContextAndPlay(sound, rec, triggerRender);
            return; // Exit early to prevent further touch handling
          } else if (!rec.playing) {
            // Normal play when audio is ready and video is not playing
            rec.play();
            triggerRender();
          } else {
            // Pause when already playing and audio context is ready
            rec.pause();
            triggerRender();
          }
        }
      }
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
  
  // Stop any playing tape audio/video
  send({ type: "tape:stop" });
}

export { boot, paint, sim, act, signal, receive, leave };

// 📚 Library (Useful functions used throughout the piece)
// ...
