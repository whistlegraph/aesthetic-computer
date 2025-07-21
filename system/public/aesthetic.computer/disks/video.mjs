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
  - [x] Factor out / comment or modify the old video overlay UI code.
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
let framesBtn;
let webpBtn;
let animWebpBtn;
let apngBtn;
let gifBtn;
let clearBtn;
let isPrinting = false;
let isExportingFrames = false;
let isExportingWebP = false;
let isExportingAnimWebP = false;
let isExportingAPNG = false;
let isExportingGIF = false;
let currentExportType = ""; // Track what's being exported
let ellipsisTicker;

// Request WebP creation from main thread (document not available in worker)
// Note: Optimized to handle large frame counts without memory issues
async function createAnimatedWebP(frames, send) {
  try {
    console.log("Requesting animated WebP creation from main thread for", frames.length, "frames");
    
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
      console.log("WebP creation request sent to main thread");
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
  isExportingFrames = false;
  isExportingWebP = false;
  isExportingAnimWebP = false;
  isExportingAPNG = false;
  isExportingGIF = false;
  currentExportType = "";
  printed = false;
  
  // Try to restore cached video from IndexedDB
  store.retrieve("tape", "local:db", (data) => {
    if (data && data.blob) {
      console.log("📼 Restored cached video from IndexedDB");
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
    if (playing) {
      wipe(0, 0);
      // Override corner label to show "|" when playing video
      hud.label("|");
    }

    if (presentProgress) {
      ink(0).box(0, screen.height - 1, screen.width, screen.height - 1);
      ink(playing ? "red" : 64).box(
        0,
        screen.height - 1,
        screen.width * presentProgress,
        screen.height - 1,
      ); // Present a progress bar.
    }

    if (!playing) {
      wipe(0, 100).ink(255, 200).write("||", { center: "xy " });
      ink(255, 75).box(0, 0, screen.width, screen.height, "inline");
    }

    if (isPrinting) {
      const h = 16; // Paint a printing / transcoding progress bar.
      
      // Dynamic text based on export type and progress
      let text = "";
      if (currentExportType === "video") {
        if (printProgress < 0.5) {
          text = "ENCODING VIDEO";
        } else if (printProgress < 0.9) {
          text = "TRANSCODING MP4";
        } else {
          text = "FINALIZING VIDEO";
        }
      } else if (currentExportType === "gif") {
        if (printProgress < 0.8) {
          text = "PROCESSING FRAMES";
        } else {
          text = "ENCODING GIF";
        }
      } else if (currentExportType === "webp") {
        if (printProgress < 0.9) {
          text = "CONVERTING TO WEBP";
        } else {
          text = "CREATING ZIP";
        }
      } else if (currentExportType === "frames") {
        text = "EXPORTING FRAMES";
      } else if (currentExportType === "animwebp") {
        text = "CREATING ANIMATED WEBP";
      } else if (currentExportType === "apng") {
        text = "CREATING ANIMATED PNG";
      } else {
        text = "PROCESSING";
      }
      
      // Add ellipsis animation - ensure help.repeat is available
      if (ellipsisTicker && help && help.repeat !== undefined) {
        text += ellipsisTicker.text(help.repeat);
      } else {
        // Fallback ellipsis animation if help.repeat is not available
        const dots = paintCount % 60 < 20 ? "..." : paintCount % 60 < 40 ? ".  " : " . ";
        text += dots;
      }

      let barWidth = Math.max(1, printProgress * screen.width); // Ensure at least 1px width

      if (printProgress > 0.98 && screen.width - barWidth >= 1) {
        barWidth = screen.width;
      }

      wipe(0, 0, 80, 180)
        .ink(0)
        .box(0, screen.height / 2 - h / 2, screen.width, h)
        .ink(0, 0, 255)
        .box(0, screen.height / 2 - h / 2, barWidth, h)
        .ink(255, 200)
        .write(text, { center: "xy" });
    } else {
      // Show "Download", "Frames", "WebP", and "Clear" buttons
      if (!btn)
        btn = new ui.TextButton("Download", { right: 6, bottom: 6, screen });
      btn.reposition({ right: 6, bottom: 6, screen });
      btn.paint(api);
      
      if (!framesBtn)
        framesBtn = new ui.TextButton("Frames", { right: 6, bottom: 40, screen });
      framesBtn.reposition({ right: 6, bottom: 40, screen });
      framesBtn.paint(api);
      
      if (!webpBtn)
        webpBtn = new ui.TextButton("WebP", { right: 6, bottom: 74, screen });
      webpBtn.reposition({ right: 6, bottom: 74, screen });
      webpBtn.paint(api);
      
      if (!animWebpBtn)
        animWebpBtn = new ui.TextButton("AnimWebP", { right: 6, bottom: 108, screen });
      animWebpBtn.reposition({ right: 6, bottom: 108, screen });
      animWebpBtn.paint(api);
      
      if (!apngBtn)
        apngBtn = new ui.TextButton("APNG", { right: 6, bottom: 142, screen });
      apngBtn.reposition({ right: 6, bottom: 142, screen });
      apngBtn.paint(api);
      
      if (!gifBtn)
        gifBtn = new ui.TextButton("GIF", { right: 6, bottom: 176, screen });
      gifBtn.reposition({ right: 6, bottom: 176, screen });
      gifBtn.paint(api);
      
      if (!clearBtn)
        clearBtn = new ui.TextButton("Clear", { right: 6, bottom: 210, screen });
      clearBtn.reposition({ right: 6, bottom: 210, screen });
      clearBtn.paint(api);
    }
  } else if (paintCount > 16n) {
    wipe(40, 0, 0).ink(180, 0, 0).write("NO VIDEO", { center: "xy" });
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

  if (!rec.printing && !isExportingFrames && !isExportingWebP && !isExportingAnimWebP && !isExportingAPNG && !isExportingGIF) {
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
      push: () => {
        if (!printed) {
          isPrinting = true;
          currentExportType = "video"; // Set export type
          btn.disabled = true;
          rec.print(() => {
            printed = true;
            btn.disabled = false;
            isPrinting = false;
            currentExportType = ""; // Clear export type
          });
        } else {
          download(`tape-${num.timestamp()}.mp4`);
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

    // Export frames as ZIP
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
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              // Create frame content for zip
              const frameRecord = [];
              
              frameData.frames.forEach((frame, index) => {
                const [timestamp, imageData] = frame;
                // Create a record entry for each frame
                frameRecord.push({
                  timestamp: timestamp,
                  label: `frame-${index.toString().padStart(6, '0')}`,
                  painting: imageData
                });
              });
              
              // Use the zip function to create a download
              const zipped = await zip({ destination: "download", painting: { record: frameRecord } }, (p) => {
                // Progress callback if needed
                console.log("Frame zip progress:", p);
              });
              
              console.log("Frames exported successfully:", zipped);
              // Reset flags on successful completion
              isPrinting = false;
              currentExportType = "";
            } else {
              console.warn("No frames available for export");
              // Reset flags if no frames available
              isPrinting = false;
              currentExportType = "";
            }
            
            isExportingFrames = false;
            framesBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting frames:", error);
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
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating animated WebP from", frameData.frames.length, "frames");
              
              // Send request to main thread for WebP creation
              const success = await createAnimatedWebP(frameData.frames, send);
              
              if (success) {
                console.log("WebP creation request sent successfully");
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
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating animated WebP from", frameData.frames.length, "frames");
              
              // Apply frame reduction for very long recordings
              let framesToProcess = frameData.frames;
              const MAX_FRAMES_FOR_WEBP = 800; // Higher limit for WebP as it's more efficient
              
              if (frameData.frames.length > MAX_FRAMES_FOR_WEBP) {
                console.log(`Reducing frames from ${frameData.frames.length} to ${MAX_FRAMES_FOR_WEBP} for WebP optimization`);
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
                    const [timestamp, imageData] = frame;
                    let duration = 100; // Default 100ms
                    
                    if (index < framesToProcess.length - 1) {
                      const nextTimestamp = framesToProcess[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: imageData.data // Keep as Uint8ClampedArray
                    };
                  })
                }
              });
              
              console.log("Animated WebP creation request sent");
              // Reset flags since animated WebP creation happens asynchronously in main thread
              isPrinting = false;
              currentExportType = "";
            } else {
              console.warn("No frames available for animated WebP export");
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
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating APNG from", frameData.frames.length, "frames");
              
              // Apply frame reduction for very long recordings
              let framesToProcess = frameData.frames;
              const MAX_FRAMES_FOR_APNG = 600; // Reasonable limit for APNG
              
              if (frameData.frames.length > MAX_FRAMES_FOR_APNG) {
                console.log(`Reducing frames from ${frameData.frames.length} to ${MAX_FRAMES_FOR_APNG} for APNG optimization`);
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
                    const [timestamp, imageData] = frame;
                    let duration = 100; // Default 100ms
                    
                    if (index < framesToProcess.length - 1) {
                      const nextTimestamp = framesToProcess[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: imageData.data // Keep as Uint8ClampedArray
                    };
                  })
                }
              });
              
              console.log("APNG creation request sent");
              // Reset flags since APNG creation happens asynchronously in main thread
              isPrinting = false;
              currentExportType = "";
            } else {
              console.warn("No frames available for APNG export");
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
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating GIF from", frameData.frames.length, "frames");
              
              // Apply frame reduction for very long recordings to prevent memory issues
              let framesToProcess = frameData.frames;
              const MAX_FRAMES_FOR_GIF = 500; // Reasonable limit for GIFs
              
              if (frameData.frames.length > MAX_FRAMES_FOR_GIF) {
                console.log(`Reducing frames from ${frameData.frames.length} to ${MAX_FRAMES_FOR_GIF} for GIF optimization`);
                const skipRatio = frameData.frames.length / MAX_FRAMES_FOR_GIF;
                framesToProcess = [];
                for (let i = 0; i < frameData.frames.length; i += skipRatio) {
                  framesToProcess.push(frameData.frames[Math.floor(i)]);
                }
                framesToProcess = framesToProcess.slice(0, MAX_FRAMES_FOR_GIF);
              }
              
              // Prepare all frames for single GIF creation request
              const processedFrames = framesToProcess.map((frame, index) => {
                const [timestamp, imageData] = frame;
                let duration = 100; // Default 100ms
                
                if (index < framesToProcess.length - 1) {
                  const nextTimestamp = framesToProcess[index + 1][0];
                  duration = Math.max(10, nextTimestamp - timestamp);
                }
                
                return {
                  timestamp: timestamp,
                  duration: duration,
                  width: imageData.width,
                  height: imageData.height,
                  // Use the original Uint8ClampedArray instead of converting to Array
                  data: imageData.data // Keep as Uint8ClampedArray for transfer efficiency
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

    if (!btn?.down && !btn?.disabled && !framesBtn?.down && !framesBtn?.disabled && !webpBtn?.down && !webpBtn?.disabled && !animWebpBtn?.down && !animWebpBtn?.disabled && !apngBtn?.down && !apngBtn?.disabled && !clearBtn?.down && !clearBtn?.disabled) {
      if (e.is("touch:1") && !rec.playing) rec.play();
      if (e.is("touch:1") && rec.playing) rec.pause();
    }
  }
}

// 🚧 Signal (Handles messages from the system)
function signal(content) {
  if (content === "recorder:transcoding-done") {
    isPrinting = false; // Hide progress bar when transcoding is complete
    currentExportType = ""; // Clear export type
    isExportingGIF = false;
    isExportingFrames = false;
    isExportingWebP = false;
    isExportingAnimWebP = false;
    isExportingAPNG = false;
    if (gifBtn) gifBtn.disabled = false;
    if (framesBtn) framesBtn.disabled = false;
    if (webpBtn) webpBtn.disabled = false;
    if (animWebpBtn) animWebpBtn.disabled = false;
    if (apngBtn) apngBtn.disabled = false;
  }
}

// 🎯 act (also handles system messages via event.is)
function handleSystemMessage({ event: e, rec }) {
  // Handle GIF export progress updates
  if (e.is("recorder:export-progress")) {
    if (e.progress !== undefined && e.type === "gif" && isExportingGIF) {
      printProgress = e.progress;
      // Don't call needsPaint here as it's handled by the main paint loop
    }
    return true;
  }

  // Handle GIF export completion
  if (e.is("recorder:export-complete")) {
    if (e.type === "gif" && isExportingGIF) {
      // Reset UI flags when export actually completes
      isPrinting = false;
      isExportingGIF = false;
      currentExportType = "";
      gifBtn.disabled = false;
      printProgress = 0;
      
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
