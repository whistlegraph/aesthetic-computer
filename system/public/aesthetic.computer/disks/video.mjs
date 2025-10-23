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

// API references needed by receive() function
let apiJump = null; // Stored from boot() for use in receive()

const buttonBottom = 6;
const buttonLeft = 6;
const buttonSpacing = 6;

const debug = false; // Toggle verbose logging for tape export flow

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
  
  if (rec.recording) {
    notice("TAPING", ["yellow", "red"]);
    jump("prompt");
    return;
  }
  wipe(0);
  
  // Reset all export states on boot
  isPrinting = false;
  isPostingTape = false;
  currentExportType = "";
  printed = false;
  postedTapeCode = null; // Reset tape code from previous session
  
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
        
        // Construct ZIP URL based on bucket and user
        let zipUrl;
        if (metadata.bucket === 'user-aesthetic-computer' && metadata.user) {
          // User tape: include user ID and /video/ subdirectory
          zipUrl = `https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/${encodeURIComponent(metadata.user)}/video/${metadata.slug}.zip`;
        } else {
          // Guest tape: direct slug
          zipUrl = `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${metadata.slug}.zip`;
        }
        
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

  // Clear canvas with transparent wipe so video shows through
  if (presenting) {
    if (playing && !isPrinting) {
      wipe(0, 0, 0, 0); // Transparent so DOM video overlay shows
    } else {
      // Paused or exporting
      wipe(0, 100).ink(255, 200).write(isPrinting ? "EXPORTING" : "||", { center: "xy" });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }
  }

  if (!presenting && exportAvailable && !isPrinting) {
    wipe(0, 0, 0, 0); // Transparent
  }

  // Draw export buttons - reposition every frame (simple!)
  if (exportAvailable) {
    const disableExports = isPrinting || isPostingTape;

    // Only show POST button if tape hasn't been posted yet
    if (!postedTapeCode) {
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
      // Hide POST button after posting (set to undefined so it's not painted)
      postBtn = undefined;
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

function sim({ needsPaint, rec }) {
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
      postedTapeCode // Keep painting when button shows tape code
    ) {
      needsPaint();
    }
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
  sound: { synth },
  zip,
  send,
  store,
  needsPaint,
  screen,
  geo,
}) {
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
    const anyButtonDown = 
      postBtn?.down || 
      mp4Btn?.down || 
      gifBtn?.down || 
      zipBtn?.down;
    
    if (!anyButtonDown && !isPrinting && !isPostingTape) {
      if (e.is("touch:1") && !rec.playing) {
        rec.play();
        triggerRender();
      }
      if (e.is("touch:1") && rec.playing) {
        rec.pause();
        triggerRender();
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
