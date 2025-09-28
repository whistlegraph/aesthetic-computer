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

let btn;
let framesBtn;     // ZIP/Frames export button
// let webpBtn;       // Hidden export button  
// let animWebpBtn;   // Hidden export button
// let apngBtn;       // Hidden export button
let gifBtn;
// let clearBtn;      // Hidden clear button
let isPrinting = false;
let isExportingFrames = false;  // ZIP/Frames export type
// let isExportingWebP = false;    // Hidden export type
// let isExportingAnimWebP = false; // Hidden export type
// let isExportingAPNG = false;    // Hidden export type
let isExportingGIF = false;
let currentExportType = ""; // Track what's being exported
let currentExportPhase = ""; // Track current phase of export
let exportStatusMessage = ""; // Detailed status message
let printProgress = 0; // Export progress (0-1)
let ellipsisTicker;

// Enhanced progress tracking
let exportStartTime = 0; // When export started
let progressHistory = []; // Track progress over time for ETA calculation
let currentStatusAlert = null; // Track current status alert for updates

// Completion message display
let completionMessage = ""; // Message to show when export completes
let completionMessageTimer = 0; // Timer for how long to show completion message

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
function boot({ wipe, rec, gizmo, jump, notice, store }) {
  if (rec.recording) {
    notice("TAPING", ["yellow", "red"]);
    jump("prompt");
    return;
  }
  wipe(0);
  
  // Reset all export states on boot
  isPrinting = false;
  isExportingFrames = false;  // ZIP/Frames export type
  // isExportingWebP = false;    // Hidden export type
  // isExportingAnimWebP = false; // Hidden export type
  // isExportingAPNG = false;    // Hidden export type
  isExportingGIF = false;
  currentExportType = "";
  printed = false;
  
  // Try to restore cached video from IndexedDB
  store.retrieve("tape", "local:db", (data) => {
    if (data && data.blob) {
      // The video will be automatically presented via rec.present()
    }
  });
  
  rec.present(); // Visually present a recording right away if one exists.
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
  rec: { presenting, playing, printProgress, presentProgress },
  screen,
  paintCount,
}) {
  if (presenting) {
    // Always wipe to prevent UI elements from accumulating
    // During playback, use transparent wipe so tape video shows through
    // BUT prevent video playback during exports to improve performance
    if (playing && !isPrinting) {
      wipe(0, 0, 0, 0); // Transparent wipe during playback (only when not exporting)
      // Override corner label to show "|" when playing video
      hud.label("|");
    } else {
      wipe(0, 100).ink(255, 200).write(isPrinting ? "EXPORTING" : "||", { center: "xy " });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }

    // Commented out plain progress bar - now using VHS-style progress bar from disk.mjs
    // if (presentProgress) {
    //   ink(0).box(0, screen.height - 1, screen.width, screen.height - 1);
    //   ink(playing ? "red" : 64).box(
    //     0,
    //     screen.height - 1,
    //     screen.width * presentProgress,
    //     screen.height - 1,
    //   ); // Present a progress bar.
    // }
  }

  // Export progress display (outside of presenting block so it shows during exports)
  if (isPrinting) {
    if (useExtendedProgressBar) {
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
        if (printProgress < 0.2) {
          text = "ANALYZING FRAMES";
          phaseProgress = printProgress / 0.2;
        } else if (printProgress < 0.4) {
          text = "OPTIMIZING COLORS";
          phaseProgress = (printProgress - 0.2) / 0.2;
        } else if (printProgress < 0.7) {
          text = "PROCESSING FRAMES";
          phaseProgress = (printProgress - 0.4) / 0.3;
        } else if (printProgress < 0.9) {
          text = "ENCODING GIF";
          phaseProgress = (printProgress - 0.7) / 0.2;
        } else {
          text = "FINALIZING GIF";
          phaseProgress = (printProgress - 0.9) / 0.1;
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
      
      // Add ellipsis animation - ensure help.repeat is available
      if (ellipsisTicker && help && help.repeat !== undefined) {
        text += ellipsisTicker.text(help.repeat);
      } else {
        // Fallback ellipsis animation if help.repeat is not available
        const dots = paintCount % 60 < 20 ? "..." : paintCount % 60 < 40 ? ".  " : " . ";
        text += dots;
      }

      let barWidth = Math.max(1, printProgress * screen.width); // Overall progress
      let phaseBarWidth = Math.max(1, phaseProgress * screen.width); // Phase progress

      if (printProgress > 0.98 && screen.width - barWidth >= 1) {
        barWidth = screen.width;
      }

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

      // Draw main progress bar (overall progress)
      wipe(0, 0, 80, 180)
        .ink(0)
        .box(0, screen.height / 2 - h / 2, screen.width, h)
        .ink(0, 0, 255)
        .box(0, screen.height / 2 - h / 2, barWidth, h);
        
      // Draw phase progress indicator (lighter blue on top)
      if (phaseProgress > 0 && phaseProgress < 1) {
        ink(100, 150, 255, 150)
          .box(0, screen.height / 2 - h / 2 + 2, phaseBarWidth, h - 4);
      }
      
      // Draw percentage text
      const percentage = Math.floor(printProgress * 100);
      ink(255, 200)
        .write(`${percentage}%`, { x: 8, y: screen.height / 2 - h / 2 + 4 });
        
      // Draw status text centered
      ink(255)
        .write(text + etaText, { center: "x", y: screen.height / 2 - h / 2 - 8 });
      ink(255, 200)
        .write(text, { center: "xy" });
    } else {
      // Baked-in VHS progress bar mode (rendered in disk.mjs during recording)
      // Progress bar will be rendered by the VHS tape progress system in disk.mjs
      // We just need to send the progress to the recording system
      // The VHS flickering progress bar overlay is handled automatically by the tapeProgress system
    }
  }

  // Main video piece content display
  if (presenting) {
    // Show "MP4" and "GIF" buttons side by side
    if (!btn)
      btn = new ui.TextButton("MP4", { right: 6, bottom: 6, screen });
    btn.reposition({ right: 6, bottom: 6, screen });
    btn.paint(api);
    
    if (!gifBtn)
      gifBtn = new ui.TextButton("GIF", { right: 40, bottom: 6, screen });
    gifBtn.reposition({ right: 40, bottom: 6, screen });
    gifBtn.paint(api);
    
    // ZIP/Frames export option for high-resolution frames
    if (!framesBtn)
      framesBtn = new ui.TextButton("ZIP", { right: 6, bottom: 40, screen });
    framesBtn.reposition({ right: 6, bottom: 40, screen });
    framesBtn.paint(api);
    
    // Hidden export options (webpBtn, animWebpBtn, apngBtn, clearBtn) - keeping for future use
    
    // if (!webpBtn)
    //   webpBtn = new ui.TextButton("WebP", { right: 6, bottom: 74, screen });
    // webpBtn.reposition({ right: 6, bottom: 74, screen });
    // webpBtn.paint(api);
    
    // if (!animWebpBtn)
    //   animWebpBtn = new ui.TextButton("AnimWebP", { right: 6, bottom: 108, screen });
    // animWebpBtn.reposition({ right: 6, bottom: 108, screen });
    // animWebpBtn.paint(api);
    
    // if (!apngBtn)
    //   apngBtn = new ui.TextButton("APNG", { right: 6, bottom: 40, screen });
    // apngBtn.reposition({ right: 6, bottom: 40, screen });
    // apngBtn.paint(api);
    
    // if (!clearBtn)
    //   clearBtn = new ui.TextButton("Clear", { right: 6, bottom: 74, screen });
    // clearBtn.reposition({ right: 6, bottom: 74, screen });
    // clearBtn.paint(api);
  } else if (paintCount > 16n) {
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
}

function sim() {
  ellipsisTicker?.sim();
}

let printed = false;

// ✒ Act (Runs once per user interaction)
function act({ event: e, rec, download, num, jump, sound: { synth }, zip, send, store }) {
  // Handle system messages first
  if (handleSystemMessage({ event: e, rec })) {
    return; // Exit early if a system message was handled
  }

  if (!rec.printing && !isExportingGIF) {
    // Download or print (render) a video.
    btn?.act(e, {
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
        isPrinting = true; // Show progress bar
        currentExportType = "video"; // Set export type
        btn.disabled = true;
        
        // Immediately set initial progress and status for instant feedback
        printProgress = 0.01; // Start with minimal progress to trigger display
        exportStatusMessage = "STARTING MP4 EXPORT";
        currentExportPhase = "preparing";
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0.01; // Start with minimal progress
        }
        
        try {
          // Request frames from the recording system (same as GIF)
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating MP4 from", frameData.frames.length, "frames");
              
              // Process all frames - no frame limit restrictions (same as GIF)
              let framesToProcess = frameData.frames;
              
              // Prepare all frames for single MP4 creation request (same format as GIF)
              const processedFrames = framesToProcess.map((frame, index) => {
                const [timestamp, imageData, penData] = frame; // Extract pen data for future use
                
                // Use actual recorded frame duration for perfect timing
                let duration = frame.duration || 16.67; // Use captured duration, fallback to 60fps
                
                // Only calculate from timestamps if no duration was recorded
                if (!frame.duration && index < framesToProcess.length - 1) {
                  const nextTimestamp = framesToProcess[index + 1][0];
                  duration = Math.max(8.33, Math.min(50, nextTimestamp - timestamp)); // Clamp between 120fps and 20fps
                }
                
                return {
                  timestamp: timestamp,
                  originalTimestamp: timestamp,
                  duration: duration,
                  width: imageData.width,
                  height: imageData.height,
                  // Use the original Uint8ClampedArray (same as GIF)
                  data: imageData.data
                };
              });
              
              // Send single request to main thread for MP4 creation (same pattern as GIF)
              send({
                type: "create-animated-mp4",
                content: {
                  frames: processedFrames
                }
              });
              
              console.log("MP4 creation request sent");
              // DON'T reset flags here - wait for completion message (same as GIF)
            } else {
              console.warn("No frames available for MP4 export");
              // Reset flags if no frames available
              isPrinting = false;
              btn.disabled = false;
            }
          });
        } catch (error) {
          console.error("Error exporting MP4:", error);
          // Reset flags on error
          isPrinting = false;
          btn.disabled = false;
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

    // Export ZIP of high-resolution frames
    framesBtn?.act(e, {
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
        isExportingFrames = true;
        isPrinting = true; // Show progress bar
        currentExportType = "frames"; // Set export type
        framesBtn.disabled = true;
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0;
        }
        
        try {
          // Request frames from the recording system for ZIP export
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              // Convert frames to format expected by create-animated-frames-zip
              const frameRecord = [];
              
              frameData.frames.forEach((frame, index) => {
                const [timestamp, imageData, penData] = frame; // Extract pen data for future use
                
                // Calculate duration until next frame (or default to 16.67ms for 60fps if last frame)
                let duration = 16.67; // Default ~60fps
                if (index < frameData.frames.length - 1) {
                  duration = frameData.frames[index + 1][0] - timestamp;
                }
                
                frameRecord.push({
                  timestamp: timestamp,
                  duration: Math.max(10, duration), // Minimum 10ms duration
                  width: imageData.width,
                  height: imageData.height,
                  data: imageData.data // Keep as Uint8ClampedArray
                });
              });
              
              // Send to main thread for ZIP creation with 6x scaling
              send({
                type: "create-animated-frames-zip",
                content: {
                  frames: frameRecord
                }
              });
              
              // Reset flags - download happens asynchronously via main thread
              isPrinting = false;
              currentExportType = "";
            } else {
              // Reset flags if no frames available
              isPrinting = false;
              currentExportType = "";
            }
            
            isExportingFrames = false;
            framesBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting ZIP frames:", error);
          isExportingFrames = false;
          isPrinting = false;
          currentExportType = "";
          framesBtn.disabled = false;
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
              
              // Send request to main thread for animated WebP creation only
              send({
                type: "create-animated-webp-only",
                content: {
                  frames: framesToProcess.map((frame, index) => {
                    const [timestamp, imageData, penData] = frame; // Extract pen data for future use
                    let duration = 100; // Default 100ms
                    
                    if (index < framesToProcess.length - 1) {
                      const nextTimestamp = framesToProcess[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      originalTimestamp: timestamp, // Add this field for timestamp display in WebP overlay
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: imageData.data // Keep as Uint8ClampedArray
                    };
                  })
                }
              });
              
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
              
              // Send request to main thread for APNG creation
              send({
                type: "create-single-animated-apng",
                content: {
                  frames: framesToProcess.map((frame, index) => {
                    const [timestamp, imageData, penData] = frame; // Extract pen data for future use
                    let duration = 100; // Default 100ms
                    
                    if (index < framesToProcess.length - 1) {
                      const nextTimestamp = framesToProcess[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      originalTimestamp: timestamp, // Add this field for timestamp display in APNG overlay
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: imageData.data // Keep as Uint8ClampedArray
                    };
                  })
                }
              });
              
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

    // Export GIF file (animated GIF)
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
        isExportingGIF = true;
        isPrinting = true; // Show progress bar
        currentExportType = "gif"; // Set export type
        gifBtn.disabled = true;
        
        // Immediately set initial progress and status for instant feedback
        printProgress = 0.01; // Start with minimal progress to trigger display
        exportStatusMessage = "STARTING GIF EXPORT";
        currentExportPhase = "analyzing";
        
        // Initialize tape progress for red overlay mode
        if (!useExtendedProgressBar) {
          rec.tapeProgress = 0.01; // Start with minimal progress
        }
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating GIF from", frameData.frames.length, "frames");
              
              // Process all frames - no frame limit restrictions
              let framesToProcess = frameData.frames;
              
              // Prepare all frames for single GIF creation request
              const processedFrames = framesToProcess.map((frame, index) => {
                const [timestamp, imageData, penData] = frame; // Extract pen data too
                let duration = 16.67; // Default 16.67ms for 60fps (same as WebP export)
                
                if (index < framesToProcess.length - 1) {
                  const nextTimestamp = framesToProcess[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                
                // Debug: Log pen data for first few frames and last few frames
                if (index < 5 || index >= framesToProcess.length - 5) {
                  console.log(`🖱️ Frame ${index}: pen data =`, penData ? 
                    `{x: ${penData.x}, y: ${penData.y}, device: ${penData.device}}` : 
                    'null');
                }
                
                return {
                  timestamp: timestamp,
                  originalTimestamp: timestamp, // Add this field for timestamp display in GIF overlay
                  duration: duration,
                  width: imageData.width,
                  height: imageData.height,
                  // Use the original Uint8ClampedArray instead of converting to Array
                  data: imageData.data, // Keep as Uint8ClampedArray for transfer efficiency
                  penData: penData // Preserve pen data for crosshair rendering
                };
              });
              
              // Send single request to main thread for GIF creation
              send({
                type: "create-animated-gif",
                content: {
                  frames: processedFrames
                }
              });
              
              console.log("All GIF batches sent");
              // DON'T reset flags here - wait for completion message
              // The flags will be reset when the GIF is actually finished downloading
              // via the handleSystemMessage function
            } else {
              console.warn("No frames available for GIF export");
              // Reset flags if no frames available
              isPrinting = false;
              isExportingGIF = false;
              gifBtn.disabled = false;
            }
          });
        } catch (error) {
          console.error("Error exporting GIF:", error);
          // Reset flags on error
          isPrinting = false;
          isExportingGIF = false;
          gifBtn.disabled = false;
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

    if (!btn?.down && !btn?.disabled && !gifBtn?.down && !gifBtn?.disabled && !isPrinting) {
      if (e.is("touch:1") && !rec.playing) rec.play();
      if (e.is("touch:1") && rec.playing) rec.pause();
    }
  }
}

// 🚧 Signal (Handles messages from the system)
function signal(content) {
  console.log("🎯 Video piece received signal:", content);
  if (content === "recorder:transcoding-done") {
    console.log("🎯 Received transcoding-done signal - resetting UI");
    isPrinting = false; // Hide progress bar when transcoding is complete
    currentExportType = ""; // Clear export type
    isExportingGIF = false;
    isExportingFrames = false;
    isExportingWebP = false;
    isExportingAnimWebP = false;
    isExportingAPNG = false;
    printProgress = 0;
    if (gifBtn) gifBtn.disabled = false;
    if (framesBtn) framesBtn.disabled = false;
    if (webpBtn) webpBtn.disabled = false;
    if (animWebpBtn) animWebpBtn.disabled = false;
    if (apngBtn) apngBtn.disabled = false;
    if (btn) btn.disabled = false;
  }
}

// 🎯 act (also handles system messages via event.is)
function handleSystemMessage({ event: e, rec }) {
  // Handle detailed export status messages
  if (e.is("recorder:export-status")) {
    console.log("🎯 Video piece received status:", e.content);
    
    if (e.content?.message) {
      exportStatusMessage = e.content.message;
    }
    
    if (e.content?.phase) {
      currentExportPhase = e.content.phase;
    }
    
    return true;
  }

  // Handle export progress updates for all export types
  if (e.is("recorder:export-progress") || e.is("recorder:transcode-progress")) {
    console.log("🎯 Video piece received progress:", e.is("recorder:export-progress") ? "export-progress" : "transcode-progress", e);
    console.log("🎯 Current export state - isPrinting:", isPrinting, "isExportingGIF:", isExportingGIF, "currentExportType:", currentExportType);
    
    if (e.progress !== undefined || (e.is("recorder:transcode-progress") && typeof e.content === "number")) {
      // Handle both message formats: {progress, type} and direct number content
      const progress = e.progress !== undefined ? e.progress : (typeof e.content === "object" ? e.content.progress : e.content);
      const exportType = e.is("recorder:export-progress") ? (e.content?.type || "gif") : "video"; // For export-progress, get type from content
      
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
        // Handle transcode progress for any active export
        (e.is("recorder:transcode-progress") && (isPrinting || isExportingGIF || isExportingWebP || isExportingAnimWebP || isExportingAPNG || isExportingFrames));
        
      console.log("🎯 isValidExport check:", isValidExport, "- conditions:", {
        videoAndPrinting: exportType === "video" && isPrinting,
        gifAndExporting: exportType === "gif" && isExportingGIF,
        transcodeAndAnyExport: e.is("recorder:transcode-progress") && (isPrinting || isExportingGIF || isExportingWebP || isExportingAnimWebP || isExportingAPNG || isExportingFrames)
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
        
        console.log(`📊 Export progress: ${Math.floor(progress * 100)}% (${exportType}) - ${exportStatusMessage}`);
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
      
      // Set completion message to show briefly
      completionMessage = `${exportType?.toUpperCase() || "EXPORT"} COMPLETED!`;
      completionMessageTimer = 180; // Show for 3 seconds at 60fps
      
      // Reset UI flags when export actually completes
      isPrinting = false;
      isExportingGIF = false;
      isExportingWebP = false;
      isExportingAnimWebP = false;
      isExportingAPNG = false;
      isExportingFrames = false;
      currentExportType = "";
      currentExportPhase = "";
      exportStatusMessage = "";
      
      // Reset progress tracking
      exportStartTime = 0;
      progressHistory = [];
      
      // Re-enable all buttons
      if (gifBtn) gifBtn.disabled = false;
      if (webpBtn) webpBtn.disabled = false;
      if (animWebpBtn) animWebpBtn.disabled = false;
      if (apngBtn) apngBtn.disabled = false;
      if (framesBtn) framesBtn.disabled = false;
      if (btn) btn.disabled = false;
      
      printProgress = 0;
      
      // Reset tape progress if using tape-style progress bar
      if (!useExtendedProgressBar) {
        rec.tapeProgress = 0;
      }
      
      // Present the completed video
      rec.present();
    }
    return true;
  }

  return false;
}

export { boot, paint, sim, act, signal };

// 📚 Library (Useful functions used throughout the piece)
// ...
