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
let ellipsisTicker;

// Request WebP creation from main thread (document not available in worker)
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
        data: Array.from(imageData.data) // Convert Uint8ClampedArray to regular array for transfer
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
      let text = "PROCESSING";
      text += ellipsisTicker.text(help.repeat);

      let barWidth = printProgress * screen.width;

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
  if (!rec.printing && !isExportingFrames && !isExportingWebP) {
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
          btn.disabled = true;
          rec.print(() => {
            printed = true;
            btn.disabled = false;
            isPrinting = false;
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
            } else {
              console.warn("No frames available for export");
            }
            
            isExportingFrames = false;
            framesBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting frames:", error);
          isExportingFrames = false;
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
              } else {
                console.warn("Failed to send WebP creation request");
              }
            } else {
              console.warn("No frames available for export");
            }
            
            isExportingWebP = false;
            webpBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting WebP:", error);
          isExportingWebP = false;
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
        animWebpBtn.disabled = true;
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating animated WebP from", frameData.frames.length, "frames");
              
              // Send request to main thread for animated WebP creation only
              send({
                type: "create-animated-webp-only",
                content: {
                  frames: frameData.frames.map((frame, index) => {
                    const [timestamp, imageData] = frame;
                    let duration = 100; // Default 100ms
                    
                    if (index < frameData.frames.length - 1) {
                      const nextTimestamp = frameData.frames[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: Array.from(imageData.data)
                    };
                  })
                }
              });
              
              console.log("Animated WebP creation request sent");
            } else {
              console.warn("No frames available for animated WebP export");
            }
            
            isExportingAnimWebP = false;
            animWebpBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting animated WebP:", error);
          isExportingAnimWebP = false;
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
        apngBtn.disabled = true;
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating APNG from", frameData.frames.length, "frames");
              
              // Send request to main thread for APNG creation
              send({
                type: "create-single-animated-apng",
                content: {
                  frames: frameData.frames.map((frame, index) => {
                    const [timestamp, imageData] = frame;
                    let duration = 100; // Default 100ms
                    
                    if (index < frameData.frames.length - 1) {
                      const nextTimestamp = frameData.frames[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: Array.from(imageData.data)
                    };
                  })
                }
              });
              
              console.log("APNG creation request sent");
            } else {
              console.warn("No frames available for APNG export");
            }
            
            isExportingAPNG = false;
            apngBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting APNG:", error);
          isExportingAPNG = false;
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
        gifBtn.disabled = true;
        
        try {
          // Request frames from the recording system
          rec.requestFrames(async (frameData) => {
            if (frameData.frames && frameData.frames.length > 0) {
              console.log("Creating GIF from", frameData.frames.length, "frames");
              
              // Send request to main thread for GIF creation
              send({
                type: "create-animated-gif",
                content: {
                  frames: frameData.frames.map((frame, index) => {
                    const [timestamp, imageData] = frame;
                    let duration = 100; // Default 100ms
                    
                    if (index < frameData.frames.length - 1) {
                      const nextTimestamp = frameData.frames[index + 1][0];
                      duration = Math.max(10, nextTimestamp - timestamp);
                    }
                    
                    return {
                      timestamp: timestamp,
                      duration: duration,
                      width: imageData.width,
                      height: imageData.height,
                      data: Array.from(imageData.data)
                    };
                  })
                }
              });
              
              console.log("GIF creation request sent");
            } else {
              console.warn("No frames available for GIF export");
            }
            
            isExportingGIF = false;
            gifBtn.disabled = false;
          });
        } catch (error) {
          console.error("Error exporting GIF:", error);
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

export { boot, paint, sim, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
