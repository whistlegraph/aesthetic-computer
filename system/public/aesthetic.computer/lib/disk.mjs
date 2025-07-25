// Manages a piece and the transitions between pieces like a
// hypervisor or shell.

/* #region 🏁 todo
#endregion */

import * as quat from "../dep/gl-matrix/quat.mjs";
import * as mat3 from "../dep/gl-matrix/mat3.mjs";
import * as mat4 from "../dep/gl-matrix/mat4.mjs";
import * as vec2 from "../dep/gl-matrix/vec2.mjs";
import * as vec3 from "../dep/gl-matrix/vec3.mjs";
import * as vec4 from "../dep/gl-matrix/vec4.mjs";

import * as graph from "./graph.mjs";
import * as num from "./num.mjs";
import * as text from "./text.mjs";
import * as geo from "./geo.mjs";
import * as gizmo from "./gizmo.mjs";
import * as ui from "./ui.mjs";
import * as help from "./help.mjs";
import * as platform from "./platform.mjs";
import { Ticker, ticker } from "./ticker.mjs";
import { signed as shop } from "./shop.mjs";
import { parse, metadata, inferTitleDesc, updateCode } from "./parse.mjs";
import { Socket } from "./socket.mjs"; // TODO: Eventually expand to `net.Socket`
import { Chat } from "./chat.mjs"; // TODO: Eventually expand to `net.Socket`
import {
  notArray,
  defaultTemplateStringProcessor,
  uint8ArrayToBase64,
  base64ToUint8Array,
} from "./helpers.mjs";
const { pow, abs, round, sin, random, min, max, floor, cos } = Math;
const { keys } = Object;
import { nopaint_boot, nopaint_act, nopaint_is } from "../systems/nopaint.mjs";
import * as prompt from "../systems/prompt-system.mjs";
import * as world from "../systems/world.mjs";
// import { headers } from "./headers.mjs"; // Removed - headers only printed during boot
import { logs } from "./logs.mjs";
import { soundWhitelist } from "./sound/sound-whitelist.mjs";

import { CamDoll } from "./cam-doll.mjs";

import { TextInput, Typeface } from "../lib/type.mjs";

import * as lisp from "./kidlisp.mjs";
import { isKidlispSource, encodeKidlispForUrl, getCachedCode, setCachedCode } from "./kidlisp.mjs"; // Add lisp evaluator.
import { qrcode as qr, ErrorCorrectLevel } from "../dep/@akamfoad/qr/qr.mjs";
import { microtype, MatrixChunky8 } from "../disks/common/fonts.mjs";
import * as chat from "../disks/chat.mjs"; // Import chat everywhere.

// Helper functions to safely access window flags in both main thread and worker contexts
function isQROverlayCacheDisabled() {
  try {
    return typeof window !== 'undefined' && window.acDISABLE_QR_OVERLAY_CACHE;
  } catch (e) {
    return false; // Default to enabled if we can't check
  }
}

function isHUDLabelCacheDisabled() {
  try {
    return typeof window !== 'undefined' && window.acDISABLE_HUD_LABEL_CACHE;
  } catch (e) {
    return false; // Default to enabled if we can't check
  }
}

let tf; // Active typeface global.

// Cache for loaded typefaces to avoid recreating them
const typefaceCache = new Map();

// Helper function to get or create a typeface
async function getOrCreateTypeface(name, preload) {
  if (typefaceCache.has(name)) {
    return typefaceCache.get(name);
  }

  const typeface = new Typeface(name);
  if (preload) {
    await typeface.load(preload);
  }
  typefaceCache.set(name, typeface);
  return typeface;
}

export const noWorker = { onMessage: undefined, postMessage: undefined };

let ROOT_PIECE = "prompt"; // This gets set straight from the host html file for the ac.

let USER; // A holder for the logged in user. (Defined in `boot`)
let sessionStarted = false; // A flag that waits to boot until a session was
//                             found or not.

let LAN_HOST; // The IP address of the hosting machine on the local network.
let SHARE_SUPPORTED; // Whether navigator.share is supported. (For `dl`)
let PREVIEW_OR_ICON; // Whether we are in preview or icon mode. (From boot.)
let VSCODE; // Whether we are running the vscode extesion or not. (From boot.)
let AUDIO_SAMPLE_RATE = 0;
let debug = false; // This can be overwritten on boot.
let visible = true; // Is aesthetic.computer visibly rendering or not?
let cachedKidlispOwner = null; // Stores the user sub for cached KidLisp pieces to show attribution
let backgroundFillColor = null; // Stores the background color for filling new pixels during reframe
let pendingAlsData = null; // Stores ALS data to be sent when ableton piece loads

const projectionMode = (typeof location !== 'undefined' && location.search) ? location.search.indexOf("nolabel") > -1 : false; // Skip loading noise.

import { setDebug } from "../disks/common/debug.mjs";
import { customAlphabet } from "../dep/nanoid/nanoid.js";
// import { update } from "./glaze.mjs";
const alphabet =
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const nanoid = customAlphabet(alphabet, 4);

const defaults = {
  boot: ({ cursor, screen: { width, height }, resolution, api }) => {
    if (location.host.indexOf("botce") > -1) resolution(width, height, 0);
    if (platform.AestheticExtension) resolution(width, height, 0);
    cursor("native");
  }, // aka Setup
  sim: () => false, // A framerate independent of rendering.
  paint: ({ noise16Aesthetic, noise16Sotce, slug, wipe, ink, screen, net, painting, api }) => {
    // TODO: Make this a boot choice via the index.html file?
    if (!projectionMode) {
      if (slug?.indexOf("botce") > -1) {
        noise16Sotce();
      } else {
        noise16Aesthetic();
        if (net.loadFailureText) {
          ink("white").write(
            net.loadFailureText,
            { x: 6, y: 6 },
            [64, 64],
            screen.width - 6,
          );
        }
      }
    }
    
    // Add duration progress bar to default noise if duration tracking is active
    // This ensures the progress bar is rendered regardless of nolabel parameter
    if (durationTotal && durationStartTime !== null) {
      const currentTime = performance.now();
      const elapsed = (currentTime - durationStartTime) / 1000; // Convert to seconds
      const progress = Math.min(elapsed / durationTotal, 1);
      
      // Check if duration is completed
      const completed = progress >= 1;
      
      if (completed && !durationCompleted) {
        durationCompleted = true;
        console.log("⏱️ Duration completed!");
      }
      
      // Update global progress for other systems to use
      durationProgress = progress;
      
      // Update blink state for completed duration
      durationBlinkState = Math.floor(currentTime / 500) % 2 === 0;
      
      // Render the progress bar directly using ink
      const progressWidth = Math.floor(screen.width * progress);
      
      if (progressWidth > 0) {
        if (completed) {
          // Blink between white and dim when completed - use global blink state for consistency
          const blinkColor = durationBlinkState ? "white" : "#888";
          ink(blinkColor).line(0, screen.height - 1, screen.width - 1, screen.height - 1);
        } else {
          // Sample pixels from screen buffer above for individual progress pixels
          const screenY = screen.height - 2; // Sample from line above progress bar
          
          for (let x = 0; x < progressWidth; x++) {
            // Sample pixel from screen buffer at position (x, screenY)
            if (screenY >= 0 && screenY < screen.height && x < screen.width && screen.pixels) {
              const pixelIndex = (screenY * screen.width + x) * 4; // RGBA format
              
              if (pixelIndex + 3 < screen.pixels.length) {
                const r = screen.pixels[pixelIndex];
                const g = screen.pixels[pixelIndex + 1]; 
                const b = screen.pixels[pixelIndex + 2];
                const a = screen.pixels[pixelIndex + 3];
                
                // Use the sampled color for this pixel of the progress bar
                ink(r, g, b, a).plot(x, screen.height - 1);
              } else {
                // Fallback to red if sampling fails
                ink("red").plot(x, screen.height - 1);
              }
            } else {
              // Fallback to red if coordinates are out of bounds
              ink("red").plot(x, screen.height - 1);
            }
          }
        }
      }
    }
  },
  beat: () => false, // Runs every bpm.
  act: () => false, // All user interaction.
  leave: () => false, // Before unload.
  preview: ({ wipe, slug }) => {
    wipe(64).ink(255).write(slug, { center: "xy", size: 1 });
  },
  icon: ({ glaze, wipe, screen }) => {
    glaze({ on: false });
    // console.log(screen.width, screen.height);
    wipe(70, 50, 100)
      .ink(200, 30, 100)
      .box(screen.width / 2, screen.height / 2, 48, 72, "*center");
  },
};

let loadAfterPreamble = null;
let hotSwap = null;

// 🔎 NoPaint
// Inheritable via `export const system = "nopaint"` from any piece.
// Boilerplate for a distributed raster editor.
const nopaint = {
  leave: function leave($) {
    const { store, system, page, screen, flatten } = $;
    if (NPnoOnLeave === false) {
      // ^ This is set when reloading a brush without storing changes. (`N` key)

      if (system.nopaint.bakeOnLeave) {
        // Add bake commands.
        page(system.painting);
        //$activePaintApi = $; // In case of recursive paint functions like `write`.
        bake($);
        // page(screen); // TODO: This should work but it doesnt...
        //                        Seems to have something to do with
        //                        the $activePaintAPI being set
        //                        and it's specific to the `write`
        //                        implementation.
        flatten(); // Force-run the above painting commands.
      }
      // Bake any changes into the existing painting using the screen buffer.
      // Note... this may not include the full API that `paint` has...
      // Which could lead to inconsistencies in `bake` calls.  23.05.04.13.01

      //  const p = system.nopaint.translation; // Offset by the pan position.
      //  page(system.painting).paste(screen, -p.x, -p.y); // TODO: Why the + 1 offset here...
      //  painting.paint(); // TODO: Why is this here?
      //  page(screen);
      //}

      addUndoPainting(system.painting, $.slug);

      // Idea: Check to see if anything actually got painted by doing a diff on
      //       the pixels?

      store["painting"] = {
        width: system.painting.width,
        height: system.painting.height,
        pixels: system.painting.pixels,
      }; //system.painting; // Remember the painting data.

      // console.log("System painting:", system.painting);

      store.persist("painting", "local:db");

      // And its transform.
      store["painting:transform"] = {
        translation: system.nopaint.translation,
        zoom: system.nopaint.zoomLevel,
      };
      store.persist("painting:transform", "local:db");
    } else {
      // Restore a painting if `no`ing.
      const paintings = system.nopaint.undo.paintings;
      page(system.painting)
        .paste(paintings[paintings.length - 1])
        .page(screen);
    }

    NPnoOnLeave = false;
  },
  // 🥞 Bake (to the painting)
  bake: function bake({ paste, system }) {
    paste(system.nopaint.buffer);
  },
};

const undoPaintings = []; // Stores the last two paintings.
let undoPosition = 0;

function addUndoPainting(painting, step = "unspecified") {
  if (!painting) return; // If there is no painting present, silently pass.
  const op = painting.pixels;
  const pixels = new Uint8ClampedArray(op.length);
  pixels.set(op);

  if (undoPaintings.length > undoPosition + 1) {
    undoPaintings.length = undoPosition + 1;
  }

  if (undoPaintings.length > 0) {
    const lastPainting = undoPaintings[undoPaintings.length - 1];

    // Check for equality in the two states.
    // TODO: How long does this take?
    const eq =
      painting.width === lastPainting.width &&
      painting.height === lastPainting.height &&
      pixels.every((value, index) => value === lastPainting.pixels[index]);

    if (eq) {
      // console.log("💩 The undo stack was not changed:", undoPaintings.length);
      return;
    }
  }

  undoPaintings.push({
    pixels,
    width: painting.width,
    height: painting.height,
  });

  if ($commonApi.system.nopaint.recording) {
    $commonApi.system.nopaint.addToRecord({
      label: step,
      painting: {
        pixels,
        width: painting.width,
        height: painting.height,
      },
    });
  }

  undoPosition = undoPaintings.length - 1;

  // Note: This could be extended to increase the size of the
  //       undo stack, and images could be diffed? 23.01.31.01.30
  const maxUndoSteps = 32;
  if (undoPaintings.length > maxUndoSteps) undoPaintings.shift();

  if (debug && logs.painting)
    console.log("💩 Added undo painting...", undoPaintings.length);
}

let system = null; // Used to add built-in templated behaviors like `nopaint`.
let boot = defaults.boot;
let sim = defaults.sim;
let paint = defaults.paint;
let beat = defaults.beat;
let brush, filter; // Only set in the `nopaint` system.
let act = defaults.act;
let leave = defaults.leave;
let preview = defaults.preview;
let icon = defaults.icon;
let bake; // Currently only used by the `nopaint` system.

let leaving = false; // Set to true on first piece.
let leaveLoad; // A callback for loading the next disk after leaving.

let previewMode = false; // Detects ?preview on a piece and yields its
//                          preview function if it exists.
let firstPreviewOrIcon = true;
let iconMode = false; // Detects ?icon on a piece and yields its
//                          icon function if it exists.
let previewOrIconMode;
let hideLabel = false;

let module, loadedModule; // Currently loaded piece module code with an extra reference for `hotSwap`.
let currentPath,
  currentHost,
  currentSearch,
  currentColon,
  currentParams,
  currentHash,
  currentText,
  currentCode,
  currentHUDTxt,
  currentHUDLogicalTxt,
  currentHUDTextColor,
  currentHUDStatusColor = "red",
  currentHUDButton,
  currentHUDScrub = 0,
  currentHUDOffset,
  qrOverlayCache = new Map(), // Cache for QR overlays to prevent regeneration every frame
  hudLabelCache = null; // Cache for HUD label to prevent regeneration every frame

// Make cache globally accessible for character loading system
if (typeof window !== 'undefined') {
  window.qrOverlayCache = qrOverlayCache;
  
  // Clear caches if they are disabled from BIOS
  if (isQROverlayCacheDisabled()) {
    qrOverlayCache.clear();
    console.log("🚫 QR overlay cache disabled and cleared from BIOS");
  }
  
  if (isHUDLabelCacheDisabled()) {
    console.log("🚫 HUD label cache disabled from BIOS");
  }
}
//currentPromptButton;

function updateHUDStatus() {
  if (udp.connected && socket?.connected) {
    currentHUDStatusColor = "lime";
  } else if (udp.connected || socket?.connected) {
    currentHUDStatusColor = "orange";
  } else {
    currentHUDStatusColor = "red";
  }
}

let loading = false;
let reframe;

const sfxProgressReceivers = {},
  sfxSampleReceivers = {},
  sfxDurationReceivers = {},
  sfxKillReceivers = {};
let $sampleCount = 0n;

const signals = []; // Easy messages from embedded DOM content.
const actAlerts = []; // Messages that get put into act and cleared after
// every frame.
let reframed = false;
let formReframing = false; // Just for 3D camera updates.
let needs3DRendering = false; // Track if any pieces are using 3D rendering
let needsCPU3D = false; // Track if software 3D rasterizer is needed (depth buffer)
let depthBufferInitialized = false; // Track if depth buffer is ready

let paintings = {}; // Cached bitmaps from a piece.

let screen;
let currentDisplay; // TODO: Remove this? 22.09.29.11.38
let cursorCode;
let pieceHistoryIndex = -1; // Gets incremented to 0 when first piece loads.
let paintCount = 0n;
let simCount = 0n;
let booted = false;
// let initialSim = true;
let noPaint = false;
let labelBack = false;
let hiccupTimeout; // Prevent multiple hiccups from being triggered at once.

// Duration tracking for playlist progress bar (similar to tape system)
let durationStartTime = null;  // When the piece started (null if no duration) - using performance.now()
let durationTotal = null;      // Total duration in seconds (null if no duration)
let durationProgress = 0;      // Current progress (0-1)
let durationCompleted = false; // Whether duration has completed
let durationBlinkState = false; // For blinking the completed bar
let pageLoadTime = performance.now(); // Time when the page first loaded - using performance.now()

// Unified function to render duration progress bar
function renderDurationProgressBar($api) {
  if (!durationTotal || durationStartTime === null) return null;
  
  const currentTime = performance.now();
  const elapsed = (currentTime - durationStartTime) / 1000; // Convert to seconds
  const progress = Math.min(elapsed / durationTotal, 1);
  
  // Check if duration is completed
  const completed = progress >= 1;
  
  if (completed && !durationCompleted) {
    durationCompleted = true;
    console.log("⏱️ Duration completed!");
  }
  
  // Update global progress for other systems to use
  durationProgress = progress;
  
  // Update blink state for completed duration
  durationBlinkState = Math.floor(currentTime / 500) % 2 === 0;
  
  // Debug log to check screen pixels availability
  console.log("🎨 Duration progress bar render check:", {
    hasScreenPixels: !!$api.screen.pixels,
    screenWidth: $api.screen.width,
    screenHeight: $api.screen.height,
    pixelsLength: $api.screen.pixels ? $api.screen.pixels.length : 'undefined',
    progress: progress,
    progressWidth: Math.floor($api.screen.width * progress)
  });
  
  // Create duration progress bar with pixel sampling from screen buffer above
  const durationProgressBarPainting = $api.painting($api.screen.width, 1, ($) => {
    // Background (dark)
    $.ink("black").box(0, 0, $api.screen.width, 1);
    
    // Progress bar - sample individual pixels from screen buffer above
    const progressWidth = Math.floor($api.screen.width * progress);
    if (progressWidth > 0) {
      if (completed) {
        // Blink between red and dim red when completed - same as timecode
        const color = durationBlinkState ? [255, 0, 0] : [128, 0, 0];
        $.ink(color).box(0, 0, $api.screen.width, 1); // Full width when complete
      } else {
        // Normal progress - sample pixels from different screen buffer lines over time for color map effect
        for (let x = 0; x < progressWidth; x++) {
          // Calculate which screen line to sample from based on progress and x position
          // This creates a color map effect where each pixel samples from a different time/position
          const timeOffset = (progress * $api.screen.height); // How far through the screen based on time
          const pixelOffset = (x / $api.screen.width) * $api.screen.height; // Position offset for this pixel
          const screenY = Math.floor((timeOffset + pixelOffset) % $api.screen.height); // Wrap around screen height
          
          // Sample pixel from screen buffer at position (x, screenY)
          if (screenY >= 0 && screenY < $api.screen.height && x < $api.screen.width) {
            const pixelIndex = (screenY * $api.screen.width + x) * 4; // RGBA format
            
            if ($api.screen.pixels && pixelIndex + 3 < $api.screen.pixels.length) {
              const r = $api.screen.pixels[pixelIndex];
              const g = $api.screen.pixels[pixelIndex + 1]; 
              const b = $api.screen.pixels[pixelIndex + 2];
              const a = $api.screen.pixels[pixelIndex + 3];
              
              // Debug logging for first few pixels
              if (x < 3) {
                console.log(`🎨 Pixel sampling x=${x}, screenY=${screenY}, pixelIndex=${pixelIndex}, rgba=(${r},${g},${b},${a}), screenSize=${$api.screen.width}x${$api.screen.height}, bufferLength=${$api.screen.pixels.length}`);
              }
              
              // Use the sampled color for this pixel of the progress bar
              $.ink(r, g, b, a).plot(x, 0);
            } else {
              // Fallback to blue if sampling fails
              if (x < 3) {
                console.log(`🔵 Blue fallback x=${x}, screenY=${screenY}, pixelIndex=${pixelIndex}, hasPixels=${!!$api.screen.pixels}, bufferLength=${$api.screen.pixels ? $api.screen.pixels.length : 'undefined'}`);
              }
              $.ink("blue").plot(x, 0);
            }
          } else {
            // Fallback to green if coordinates are out of bounds
            if (x < 3) {
              console.log(`🟢 Green fallback x=${x}, screenY=${screenY}, screenSize=${$api.screen.width}x${$api.screen.height}`);
            }
            $.ink("green").plot(x, 0);
          }
        }
      }
    }
  });
  
  // Return the painting if created successfully
  if (durationProgressBarPainting && durationProgressBarPainting.pixels && durationProgressBarPainting.pixels.length > 0) {
    return {
      x: 0,
      y: $api.screen.height - 1, // Position at bottom of screen (1px tall)
      img: {
        width: durationProgressBarPainting.width,
        height: durationProgressBarPainting.height,
        pixels: durationProgressBarPainting.pixels
      }
    };
  }
  
  return null;
}

let storeRetrievalResolutions = {},
  storeDeletionResolutions = {};

// There are two instances of Socket that run in parallel...
let socket, socketStartDelay; // Socket server for each piece.

// ❤️‍🔥 TODO: Explose these somehow to the $commonApi.

// TODO: Extract `chat` into an external class.

const chatDebug = (typeof location !== 'undefined' && location.host) && (
  location.host === "local.aesthetic.computer" ||
  location.host === "localhost:8888" ||
  location.host === "aesthetic.local:8888"
);
const chatClient = new Chat(chatDebug, send);

let udp = {
    send: (type, content) => {
      send({ type: "udp:send", content: { type, content } });
    },
    receive: ({ type, content }) => {
      // console.log("🩰 Received `piece` message from UDP:", type, content);

      // 🧚 Ambient cursor (fairies) support.
      if (type === "fairy:point" /*&& socket?.id !== id*/ && visible) {
        fairies.push({ x: content.x, y: content.y });
        return;
      }

      udpReceive?.(type, content);
    },
    kill: (outageSeconds) => {
      udp.connected = false;
      send({ type: "udp:disconnect", content: { outageSeconds } });
    },
    connected: false,
  },
  udpReceive = undefined;

let scream = null; // 😱 Allow priviledged users to send alerts to everyone.
//                       (A great end<->end socket + redis test.)
let screaming = false;
let screamingTimer; // Keep track of scream duration.

const fairies = []; // Render cursor points of other active users,
//                              dumped each frame.

let glazeEnabled = false; // Keep track of whether glaze is on or off.

// Check if a pixel buffer has any non-transparent pixels
function checkForVisiblePixels(pixelBuffer) {
  // Check every 4th byte (alpha channel) to see if any pixels are visible
  for (let i = 3; i < pixelBuffer.length; i += 4) {
    if (pixelBuffer[i] > 0) { // Alpha > 0 means visible
      return true;
    }
  }
  return false;
}

// *** Dark Mode ***
// (By @tarighian)
// Pass `true` or `false` to override or `default` to the system setting.
function darkMode(enabled) {
  if (enabled === "default") {
    darkMode($commonApi.dark || false);
    store.delete("dark-mode");
    actAlerts.push($commonApi.dark ? "dark-mode" : "light-mode");
    return $commonApi.dark;
  } else {
    // true or false
    store["dark-mode"] = enabled;
    store.persist("dark-mode");
    $commonApi.dark = enabled;
    actAlerts.push($commonApi.dark ? "dark-mode" : "light-mode");
    return enabled;
  }
}

// *** Store ***
// This object is used to store and retrieve data across disks
// The `local` method encodes everything as a JSON string.

// Note: It strangely also contains an API which could be redefined
//       unintentionally. 22.10.15.01.57
// 22.11.25.11.07: The methods should be refactored out of the storage object / not share keys.
const store = {
  persist: function (key, method = "local") {
    // Send data over the thread through a key in this object.
    send({
      type: "store:persist",
      content: {
        key,
        data: this[key],
        method,
      },
    });
    // TODO: Turn the existing key into a retrieval function / promise?
  },
  retrieve: function (key, method = "local") {
    const promise = new Promise((resolve) => {
      storeRetrievalResolutions[key] = resolve;
    });

    send({ type: "store:retrieve", content: { key, method } });

    return promise;
  },
  delete: function (key, method = "local") {
    // Remove the key from the ram store, no matter what the method.
    // console.log("🗑️ Store delete called for key:", key);
    delete store[key];

    const promise = new Promise((resolve) => {
      storeDeletionResolutions[key] = resolve;
    });

    send({
      type: "store:delete",
      content: {
        key,
        method,
      },
    });
    return promise;
  },
};

// Promise based API calls (through `bios` and back)
let fileImport;
let serverUpload, serverUploadProgressReporter;
let zipCreation;
let authorizationRequest;
let fileOpenRequest;
let fileEncodeRequest;
let gpuResponse;
let web3Response;

// Other
let activeVideo; // TODO: Eventually this can be a bank to store video textures.
let videoDeviceCount = 0;
let lastActiveVideo;
let videoSwitching = false;
let preloadPromises = {};
let inFocus;
let loadFailure;

// 1. ✔ API

// TODO: Eventually add a wiggle bank so all wiggles are indexed
//       and start at random angles.
// let wiggler = 0;
let wiggleAngle = 0;

// TODO; Change this to true and update all brushes.
// let NPdontPaintOnLeave = false;
let NPnoOnLeave = false;

// 🔴 Recorder (Singleton)
class Recorder {
  printProgress = 0;
  presentProgress = 0;
  printing = false; // Set by a callback from `bios`.
  printed = false; // "
  recording = false; // "
  rollingCallback;
  recorded = false; // "
  presenting = false; // "
  playing = false; // "
  cutCallback;
  printCallback;
  framesCallback;
  loadCallback;

  tapeTimerStart;
  tapeProgress = 0;
  tapeTimerDuration;

  // Duration progress for timed pieces (from query parameter)
  durationTimerStart;
  durationProgress = 0;
  durationTimerDuration;
  durationProgressCompleted = false;

  videoOnLeave = false;

  constructor() {}

  tapeTimerSet(seconds, time) {
    // console.log("🎬 tapeTimerSet called with:", { seconds, time });
    this.tapeTimerStart = time;
    this.tapeTimerDuration = seconds;
  }

  tapeTimerStep({ needsPaint, sound: { time } }) {
    if (!this.tapeTimerDuration) {
      // Debug: Only log occasionally to avoid spam
      // if (this.recording && Math.random() < 0.01) {
      //   console.log("🎬 tapeTimerStep: No duration set but recording is active:", {
      //     recording: this.recording,
      //     tapeTimerStart: this.tapeTimerStart,
      //     tapeTimerDuration: this.tapeTimerDuration
      //   });
      // }
      return;
    }
    const timeElapsed = time - this.tapeTimerStart;
    const rawProgress = timeElapsed / this.tapeTimerDuration;
    // Clamp progress between 0 and 1 to prevent negative bar widths
    this.tapeProgress = Math.max(0, Math.min(1, rawProgress));
    needsPaint();
    const secondsOver = timeElapsed - this.tapeTimerDuration;
    
    // Debug logging for early progress to see if timer is working
    // if (this.tapeProgress > 0 && this.tapeProgress < 0.1 && Math.random() < 0.1) {
    //   console.log("🎬 Tape progress early:", {
    //     progress: this.tapeProgress,
    //     timeElapsed: timeElapsed,
    //     duration: this.tapeTimerDuration
    //   });
    // }
    
    // Debug logging when we're at or near completion
    // if (this.tapeProgress >= 0.95) {
    //   console.log("🎬 Tape near completion:", {
    //     progress: this.tapeProgress,
    //     timeElapsed: timeElapsed,
    //     duration: this.tapeTimerDuration,
    //     secondsOver: secondsOver,
    //     shouldComplete: this.tapeProgress >= 1 && secondsOver > 0.15
    //   });
    // }
    
    // Run for an extra 150 milliseconds.
    if (this.tapeProgress >= 1 && secondsOver > 0.15) {
      // console.log("🎬 Tape timer complete! Cutting and jumping to video...");
      this.tapeProgress = 0;
      this.tapeTimerStart = null;
      this.tapeTimerDuration = null;
      this.cut(() => {
        // console.log("🎬 Cut complete, jumping to video...");
        $commonApi.jump("video");
      });
    }
  }

  slate() {
    send({ type: "recorder:slate" }); // Kill the MediaRecorder instance.
    // TODO: Should printing and playing also be set to false?
    //$commonApi.rec.printing = false; // "
    $commonApi.rec.recording = false; // Reset this singleton.
    $commonApi.rec.recordingStartTime = undefined; // Clear animation start time
    $commonApi.rec.animationFrame = undefined; // Clear animation frame counter
    $commonApi.rec.recorded = false; //
    $commonApi.rec.printed = false; // "
    $commonApi.rec.printProgress = 0; // "
    
    // Reset tape progress state for re-entrant recordings
    this.tapeProgress = 0;
    this.tapeTimerStart = null;
    this.tapeTimerDuration = null;
  }

  rolling(opts, cb) {
    // console.log("🎬 rolling called with:", { opts, cb: cb ? "callback provided" : "no callback" });
    $commonApi.rec.recording = true; // Set recording state immediately for progress bar
    $commonApi.rec.recordingStartTime = performance.now(); // Set animation start time
    $commonApi.rec.animationFrame = 0; // Reset animation frame counter
    
    send({ type: "recorder:rolling", content: opts });
    this.rollingCallback = cb;
  }

  cut(cb) {
    $commonApi.rec.cutCallback = cb;
    this.tapeProgress = 0;
    this.tapeTimerStart = null;
    this.tapeTimerDuration = null;
    send({ type: "recorder:cut" });
  }

  print(cb) {
    $commonApi.rec.printCallback = cb;
    send({ type: "recorder:print" });
  }

  present(noplay = false) {
    this.presentProgress = 0;
    send({ type: "recorder:present", content: { noplay } });
  }

  unpresent() {
    send({ type: "recorder:unpresent" });
  }

  play() {
    send({ type: "recorder:present:play" });
  }

  pause() {
    send({ type: "recorder:present:pause" });
  }

  requestFrames(cb) {
    $commonApi.rec.framesCallback = cb;
    send({ type: "recorder:request-frames" });
  }
}

let cachedAPI; // 🪢 This is a bit hacky. 23.04.21.14.59

// GC OPTIMIZATION: Function to invalidate cached API objects when needed
function invalidateAPICache() {
  if (cachedAPI) cachedAPI._frameInvalid = true;
}

// GC OPTIMIZATION: Object pools for frequently allocated objects
const objectPools = {
  // Pool for small arrays (up to 10 elements)
  smallArrayPool: [],
  getSmallArray() {
    return this.smallArrayPool.pop() || [];
  },
  releaseSmallArray(arr) {
    if (arr.length <= 10) {
      arr.length = 0;
      this.smallArrayPool.push(arr);
    }
  },
  
  // Pool for reusable objects with common properties
  eventDataPool: [],
  getEventData() {
    const obj = this.eventDataPool.pop();
    if (obj) {
      // Clear previous properties
      Object.keys(obj).forEach(key => delete obj[key]);
      return obj;
    }
    return {};
  },
  releaseEventData(obj) {
    if (this.eventDataPool.length < 5) {
      this.eventDataPool.push(obj);
    }
  }
};

const hourGlasses = [];

async function uploadPainting(picture, progress, handle, filename) {
  if (typeof picture === "string") {
    return { url: picture }; // Assume a URL and just return it.
  } else {
    // Assume picture is a buffer with `{ pixels, width, height }`
    // Upload it to the temporary bucket.
    filename ||= `painting-${num.timestamp()}.png`;

    try {
      const data = await $commonApi.upload(
        filename,
        {
          pixels: picture.pixels,
          width: picture.width,
          height: picture.height,
        },
        (p) => {
          console.log("Painting upload progress:", p);
          progress?.(p);
        },
        !handle ? "art" : undefined, // Store in temporary if no HANDLE.
      );
      console.log("🪄 Painting uploaded:", data.slug, data.ext, data.url);
      return data;
    } catch (err) {
      console.error("🪄 Painting upload failed:", err);
    }
  }
}

// Returns whether leaving a piece and also can overwrite the value.
function isLeaving(set) {
  if (set === true || set === false) leaving = set;
  return leaving;
}

let docs; // Memorized by `requestDocs`.

let baseTime = Date.now(); // Virtual clock base
let baseReal = Date.now(); // Real time at last baseTime
let clockFetching = false;
let lastServerTime = undefined;
let clockOffset = 0; // Smoothed offset from server

const $commonApi = {
  lisp, //  A global reference to the `kidlisp` evalurator.
  undef: undefined, // A global api shorthand for undefined.
  clock: {
    offset: function () {
      if (clockFetching) return;

      clockFetching = true;
      const t0 = Date.now();

      fetch("/api/clock")
        .then((response) => {
          if (!response.ok) {
            return response.text().then((err) => {
              clockFetching = false;
              throw new Error(
                `Failed to fetch offset: ${response.status} ${err}`,
              );
            });
          }

          return response.text().then((serverTimeISO) => {
            const t1 = Date.now();
            const serverTime = new Date(serverTimeISO).getTime();

            // Assume serverTime is the midpoint of request
            const rtt = t1 - t0;
            const approxClientMidpoint = t0 + rtt / 2;
            const targetOffset = serverTime - approxClientMidpoint;

            // Blend the clock offset gradually (e.g. 10% of the way each resync)
            const blendFactor = 0.25;
            clockOffset += (targetOffset - clockOffset) * blendFactor;

            // Recompute base time to keep virtual time in sync
            baseTime = Date.now() + clockOffset;
            baseReal = Date.now();

            // console.log('synced')

            lastServerTime = serverTime;
            clockFetching = false;
          });
        })
        .catch((err) => {
          console.error("Clock:", err);
          clockFetching = false;
        });
    },

    resync: function () {
      $commonApi.clock.offset();
    },

    time: function () {
      return new Date(baseTime + (Date.now() - baseReal));
    },
  },

  // Enable Pointer Lock
  penLock: () => {
    send({ type: "pen:lock" });
  },
  chat: chatClient.system,
  dark: undefined, // If we are in dark mode.
  theme: {
    light: {
      wipeBG: 150,
      wipeNum: 200,
    },
    dark: {
      wipeBG: 32,
      wipeNum: 64,
    },
  },
  glaze: function (content) {
    if (glazeEnabled === content.on) return; // Prevent glaze from being fired twice...
    glazeEnabled = content.on;
    glazeAfterReframe = { type: "glaze", content };
  },

  jump: function jump(to, ahistorical = false, alias = false) {
    // let url;
    console.log("🐎 Jump:", to);
    if (leaving) {
      console.log("🚪🐴 Jump cancelled, already leaving...");
      return;
    }
    const jumpOut =
      to.startsWith("out:") || (to.startsWith("http") && platform.Aesthetic);

    if (shop.indexOf(to) > -1) to = "/" + to; // Jump out for shop products.

    if (
      ((to.startsWith("http") || to.startsWith("/")) && !to.endsWith(".mjs")) ||
      jumpOut
    ) {
      to = to.replace("out:", "");
      try {
        // url = new URL(to);
        $commonApi.net.web(to, jumpOut);
        return;
      } catch (e) {
        // Could not construct a valid url from the jump, so we will be
        // running a local aesthetic.computer piece.
        return;
      }
    } else {
      leaving = true;
    }

    function loadLine() {
      load(parse(to), ahistorical, alias, false, callback);
    }

    let callback;
    leaveLoad = () => {
      // Intercept returns to the prompt when taping from a piece directly.
      if ($commonApi.rec.videoOnLeave && to.split("~")[0] === "prompt") {
        to = "video";
        $commonApi.rec.videoOnLeave = false;
        $commonApi.rec.cut(loadLine);
      } else {
        loadLine(); // Or just load normally.
      }
    };
    return (cb) => (callback = cb);
  },
  canShare: false, // Whether navigator.share is enabled for mobile devices.
  leaving: isLeaving,
  handle: () => {
    return HANDLE;
  },
  notice: (msg, color = ["white", "green"], opts) => {
    notice = msg;
    noticeColor = color;
    noticeOpts = opts;
    const sound = {};
    if (color[0] === "yellow" && color[1] === "red") sound.tone = 300;
    noticeBell(cachedAPI, sound);
  },
  // ⌛ Delay a function by `time` number of sim steps.
  delay: (fun, time) => {
    hourGlasses.push(new gizmo.Hourglass(time, { completed: () => fun() }));
  },
  // Different syntax than `delay` but the same with looped behavior.
  blink: (time, fun) => {
    hourGlasses.push(
      new gizmo.Hourglass(time, { completed: () => fun(), autoFlip: true }),
    );
  },
  // 🎨 Set the background fill color used during reframe operations
  backgroundFill: (color) => {
    if (color === null || color === undefined) {
      backgroundFillColor = null;
    } else {
      backgroundFillColor = color;
    }
  },
  // 🎟️ Open a ticketed paywall on the page.
  ticket: (name) => {
    send({ type: "ticket-wall", content: name });
  },
  // 🪙 Mint a url or the `pixels` that get passed into the argument to a
  // network of choice.
  mint: async (picture, progress, params) => {
    console.log("🪙 Minting...", picture);
    // Determine if picture is a string or an object.

    // A record will be attached if one exists via the prompt and the user is
    // logged in.
    let filename;
    let zipped;
    if (picture.record && HANDLE) {
      const record = picture.record;
      filename = `painting-${record[record.length - 1].timestamp}.png`;
      // Set filename based on record.

      zipped = await $commonApi.zip(
        { destination: "upload", painting: { record } },
        (p) => {
          console.log("🤐 Zip progress:", p);
          progress?.(p);
        },
      );

      console.log("🤐 Zipped:", zipped);
    }

    const data = await uploadPainting(picture, progress, HANDLE, filename);

    let description;

    if (picture.width && picture.height) {
      description = `A ${picture.width}x${picture.height} pixel painting made on [aesthetic computer](https://aesthetic.computer).`;
    } else {
      description = `A painting made on [aesthetic computer](https://aesthetic.computer).`;
    }

    if (data) {
      // Redirect to Zora.
      if (HANDLE && zipped) {
        description = `[\` ${HANDLE}/${data.slug}\`](https://aesthetic.computer/painting~${HANDLE}/${data.slug})\n\n${description}`;
      }

      if (HANDLE) data.slug = `${HANDLE}/painting/${data.slug}`;

      const pixels = `https://aesthetic.computer/api/pixel/2048:contain/${data.slug}.${data.ext}`;

      // TODO: Also add the `zip` here?
      // Look into writing custom chunks into PNGS: https://chat.openai.com/c/0ed77e57-c5d2-4827-a733-c024da4bebd7

      $commonApi.jump(
        encodeURI(
          `https://zora.co/create/single-edition?image=${pixels}&name=${
            params[0] || "Untitled Painting"
          }&symbol=$${data.slug}&description=${description}`,
        ),
      );
    }
  },
  // 🖨️ Print either a url or the `pixels` that get passed into
  // the argument, with N quantity.
  print: async (picture, quantity = 1, progress) => {
    console.log("🖨️ Printing:", picture, "Quantity:", quantity);
    const data = await uploadPainting(picture, progress);
    let pixels;
    if (data && data.slug) {
      pixels = `${data.slug}.${data.ext}`;
    } else if (data) {
      pixels = data.url;
    } else {
      $commonApi.notice("UPLOAD ERROR", ["red", "yellow"]);
      return;
    }

    try {
      const headers = { "Content-Type": "application/json" };
      try {
        // Include authorization token if logged in.
        const token = await $commonApi.authorize(); // Get user token.
        if (token) headers.Authorization = `Bearer ${token}`;
      } catch (err) {} // Handled up-stream.

      const res = await fetch(`/api/print?new=true&pixels=${pixels}`, {
        method: "POST",
        headers,
        body: JSON.stringify({ quantity, slug: $commonApi.slug }),
        // TODO: Add order info here. ^
      });

      const data = await res.json();
      if (!res.ok)
        throw new Error(
          `🖨️ Print: HTTP error! Status: ${JSON.stringify(data)}`,
        );
      console.log("🖨️ Print order:", data);
      $commonApi.jump(data.location); // Redirect to checkout.
    } catch (error) {
      console.error("🖨️ Print order error:", error);
    }
  },
  // Create a zip file of specified content. (Used for storing painting data.)
  zip: (content, progress) => {
    const prom = new Promise((resolve, reject) => {
      zipCreation = { resolve, reject };
    });

    if (content.destination === "upload") {
      serverUploadProgressReporter = progress;
      serverUploadProgressReporter?.(0);
    }

    send({ type: "zip", content });
    return prom;
  },
  // Track device motion.
  motion: {
    on: false,
    start: () => {
      send({ type: "motion:start" });
    },
    stop: () => {
      send({ type: "motion:stop" });
      // TODO: Automatically stop when changing a disk?
    },
    current: {}, // Will get replaced by an update event.
  },
  // Speak an `utterance` aloud.
  speak: function speak(utterance, voice = "female:18", mode = "cloud", opts) {
    return send({ type: "speak", content: { utterance, voice, mode, opts } });
  },
  // Broadcast an event through the entire act system.
  act: (event, data = {}) => {
    // console.log("Acted:", event); Show the synthetic event.
    data.is = (e) => e === event;
    cachedAPI.event = data;
    try {
      act(cachedAPI);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
  },
  // 🚥 `Get` api
  // Retrieve media assets from a user account.
  get: {
    picture: (url) => {
      return $commonApi.net.preload(
        encodeURI(url),
        true,
        undefined,
        // byOpts,
      );
    },
    painting: (code, opts) => {
      return {
        by: async function (handle = "anon", byOpts) {
          // Add support for pulling paintings from the `art` bucket...
          const extension = opts?.record ? "zip" : "png";
          if (handle === "anon") {
            return $commonApi.net.preload(
              encodeURI(`https://art.aesthetic.computer/${code}.${extension}`),
              true,
              undefined,
              byOpts,
            );
          } else {
            return $commonApi.net.preload(
              `/media/${handle}/painting/${code}.${extension}`,
              true,
              undefined,
              byOpts,
            );
          }
        },
      };
    },
  },
  // ***Actually*** upload a file to the server.
  // 📓 The file name can have `media-` which will sort it on the server into
  // a directory via `presigned-url.js`.
  upload: async (filename, data, progress, bucket) => {
    const prom = new Promise((resolve, reject) => {
      serverUpload = { resolve, reject };
    });
    serverUploadProgressReporter = progress;
    serverUploadProgressReporter?.(0);

    console.log("Painting data:", data);

    send({ type: "upload", content: { filename, data, bucket } });
    return prom;
  },
  code: {
    channel: (chan) => {
      codeChannel = chan; // Set the current `codeChannel`.
      store["code-channel"] = codeChannel; // Store and keep it in the browser.
      store.persist("code-channel");
      if (!codeChannel || codeChannel?.length === 0) {
        console.log("📭 Code channel cleared!");
      } else {
        console.log("📬 Code channel set to:", codeChannel);
      }
      socket.send("code-channel:sub", codeChannel);
      // Tell any parent iframes that the channel has been updated.
      send({
        type: "post-to-parent",
        content: { type: "setCode", value: codeChannel },
      });
      // ❤️‍🔥
      // TODO: Should return a promise here, and wait for a `code-channel:subbed`
      //       event, that way users get better confirmation if the socket
      //       doesn't go through / there is a server issue. 23.07.04.18.01
    },
  },
  // File should be { type, data } where type is "png", "webp", "jef", etc.
  encode: async (file) => {
    const prom = new Promise((resolve, reject) => {
      fileEncodeRequest = { resolve, reject };
    });
    send({ type: "file-encode:request", content: file });
    return prom;
  },
  file: async () => {
    const prom = new Promise((resolve, reject) => {
      fileOpenRequest = { resolve, reject };
    });
    send({ type: "file-open:request" });
    return prom;
  },
  // Authorize a user.
  authorize: async () => {
    // TODO: This should always fail while running user code.
    // console.log("Sending auth request...");
    const prom = new Promise((resolve, reject) => {
      authorizationRequest = { resolve, reject };
    });
    send({ type: "authorization:request" });
    return prom;
  }, // Get a token for a logged in user.
  // Hand-tracking. 23.04.27.10.19 TODO: Move eventually.
  hand: { mediapipe: { screen: [], world: [], hand: "None" } },
  hud: {
    label: (text, color, offset, logical) => {
      currentHUDTxt = text;
      currentHUDLogicalTxt =
        logical ||
        (typeof text === "string"
          ? text.replace(
              /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/g,
              "",
            )
          : text);
      if (color) {
        currentHUDTextColor = graph.findColor(color);
      } else {
        // Use default white color when no color is specified
        currentHUDTextColor = currentHUDTextColor || [255, 200, 240];
      }
      currentHUDOffset = offset;
    },
    currentStatusColor: () => currentHUDStatusColor,
    currentLabel: () => ({ text: currentHUDTxt, btn: currentHUDButton }),
    labelBack: () => {
      labelBack = true;
      // Persist labelBack state via the store system which works across worker boundaries
      store["aesthetic-labelBack"] = "true";
      // Also sync to main thread sessionStorage for browser back navigation
      send({ type: "labelBack:set", content: true });
      console.log(
        "🔗 Worker: Setting labelBack and persisting to store + syncing to main thread",
      );
    },
  },
  send,
  platform,
  history: [], // Populated when a disk loads and sets the former piece.
  // Trigger background music.
  // Eventually add an "@" style parameter similar to what a stamp system would have.
  bgm: {
    set: function (trackNumber, volume) {
      send({ type: "bgm-change", content: { trackNumber, volume } });
    },
    stop: () => send({ type: "bgm-stop" }),
    data: {},
  },
  system: {
    // prompt: { input: undefined }, Gets set in `prompt_boot`.
    world: {
      // Populated in `world_boot` of `world.mjs`.
      teleported: false,
      telepos: undefined,
      teleport: (to, telepos) => {
        $commonApi.system.world.teleported = true;
        $commonApi.system.world.telepos = telepos;
        $commonApi.jump(to);
      },
    },
    nopaint: {
      //boot: nopaint_boot, // TODO: Why are these in the commonApi? 23.02.12.14.26
      // act: nopaint_act,
      buffer: null, // An overlapping brush buffer that gets drawn on top of the
      //              painting.
      recording: false,
      record: [], // Store a recording here.
      gestureRecord: [], // Store the active gesture.
      startRecord: function (fullText) {
        const sys = $commonApi.system;
        sys.nopaint.record = []; // Clear any existing recording.
        sys.nopaint.recording = true;
        sys.nopaint.addToRecord({
          label: fullText || "start",
          painting: {
            pixels: new Uint8ClampedArray(sys.painting.pixels),
            width: sys.painting.width,
            height: sys.painting.height,
          },
        });
        // console.log("🖌️🔴 Now recording:", sys.nopaint.record);
      },
      addToRecord: function (record) {
        record.timestamp = num.timestamp(); // Insert the timestamp data.
        record.gesture = $commonApi.system.nopaint.gestureRecord.slice();
        if (record.gesture.length === 0) delete record.gesture;
        $commonApi.system.nopaint.gestureRecord = [];
        $commonApi.system.nopaint.record.push(record);
        store["painting:record"] = $commonApi.system.nopaint.record;
        store.persist("painting:record", "local:db");
        // console.log("🖌️🟠 Recorded a step:", record.label);
      },
      is: nopaint_is,
      undo: { paintings: undoPaintings },
      needsBake: false,
      needsPresent: false,
      bakeOnLeave: false,
      addUndoPainting,
      // Regresses the system painting to a previous state.
      // Or the reverse... ("yes")
      no: ({ system, store, needsPaint }, yes = false) => {
        const paintings = system.nopaint.undo.paintings;

        let dontRecord = false;

        if (yes) {
          // ⏩ Fast-forward mode.
          undoPosition += 1;
          if (undoPosition > paintings.length - 1) {
            undoPosition = paintings.length - 1;
            dontRecord = true;
          }
        } else {
          // ⏪ Rewind mode.
          undoPosition -= 1;
          if (undoPosition < 0) {
            undoPosition = 0;
            dontRecord = true;
          }
        }

        if (paintings.length > 1) {
          // Copy over the old picture here...
          const p = paintings[undoPosition];

          const op = p.pixels;
          const pixels = new Uint8ClampedArray(op.length);
          pixels.set(op);

          store["painting"] = {
            width: p.width,
            height: p.height,
            pixels,
          };

          const resolutionChange =
            paintings[0].width !== paintings[1].width ||
            paintings[0].height !== paintings[1].height;

          // 🦢 Swap mode.
          // 'no' should swap...
          // const temp = paintings[0];
          // paintings[0] = paintings[1];
          // paintings[1] = temp;

          store.persist("painting", "local:db");

          system.painting = store["painting"];

          if (system.nopaint.recording && dontRecord === false) {
            const label = yes ? "yes" : "no";
            system.nopaint.addToRecord({
              label, //,
              // painting: {
              //   width: system.painting.width,
              //   height: system.painting.height,
              //   pixels: new Uint8Array(system.painting.pixels),
              // },
            });
          }

          if (resolutionChange) {
            system.nopaint.resetTransform({ system });
            system.nopaint.storeTransform(store, system);
          }

          needsPaint();
        }
      },
      // Center the picture within the screen / default translation.
      resetTransform: ({ system: sys }) => {
        sys.nopaint.zoomLevel = 1;

        if (!sys.painting) {
          sys.nopaint.translation = { x: 0, y: 0 };
          return;
        }

        sys.nopaint.translation.x = floor(
          screen.width / 2 - sys.painting.width / 2,
        );
        sys.nopaint.translation.y = floor(
          screen.height / 2 - sys.painting.height / 2,
        );
      },
      storeTransform: (store, sys) => {
        store["painting:transform"] = {
          translation: sys.nopaint.translation,
          zoom: sys.nopaint.zoomLevel,
        };
        store.persist("painting:transform", "local:db");
      },
      translation: { x: 0, y: 0 },
      zoomLevel: 1,
      translate: ({ system }, x, y) => {
        system.nopaint.translation.x += x;
        system.nopaint.translation.y += y;
      },
      // zoom: ({ system }, dir) => {
      //   system.nopaint.zoomLevel += dir === "in" ? 1 : -1;
      //   console.log("🔭 Zoom level:", system.nopaint.zoomLevel);
      //   if (system.nopaint.zoomLevel <= 0) system.nopaint.zoomLevel = 1;
      //   // TODO: Adjust the translation based on system.nopaint.brush.x and y
      //   //       Which would serve as the zoom origin point.
      // },
      zoom: ({ system }, dir, cursor) => {
        // Store old zoom level
        const oldZoomLevel = system.nopaint.zoomLevel;

        // Adjust the zoom level
        system.nopaint.zoomLevel += dir === "in" ? 1 : -1;

        // Ensure zoom level is at least 1
        if (system.nopaint.zoomLevel <= 0) system.nopaint.zoomLevel = 1;

        // Calculate the scaling factor based on the change in zoom levels
        const scale = system.nopaint.zoomLevel / oldZoomLevel;

        // Adjust the translation based on the scaling factor and the cursor's position
        system.nopaint.translation.x = floor(
          cursor.x + (system.nopaint.translation.x - cursor.x) * scale,
        );
        system.nopaint.translation.y = floor(
          cursor.y + (system.nopaint.translation.y - cursor.y) * scale,
        );
      },
      brush: { x: 0, y: 0, dragBox: undefined },
      transform: (p) => {
        return {
          x: (p.x - nopaintAPI.translation.x) / nopaintAPI.zoomLevel,
          y: (p.y - nopaintAPI.translation.y) / nopaintAPI.zoomLevel,
        };
      },
      // Similar to `updateBrush` but for arbitrary points,
      // with no change to `system.nopaint.brush`.
      pointToPainting: ({ system, pen }) => {
        const zoom = system.nopaint.zoomLevel;
        const x = floor(((pen?.x || 0) - system.nopaint.translation.x) / zoom);
        const y = floor(((pen?.y || 0) - system.nopaint.translation.y) / zoom);

        return { x, y };
      },
      updateBrush: ({ pen, system }, act) => {
        // TODO: Use `pointToPainting` above. 23.10.11.08.49
        // let { x, y } = system.nopaint.pointToPainting({ system });
        const zoom = system.nopaint.zoomLevel;
        const x = floor(((pen?.x || 0) - system.nopaint.translation.x) / zoom);
        const y = floor(((pen?.y || 0) - system.nopaint.translation.y) / zoom);

        if (act === "touch") system.nopaint.startDrag = { x, y };

        const dragBox = new geo.Box(
          system.nopaint.startDrag.x,
          system.nopaint.startDrag.y,
          x -
            system.nopaint.startDrag.x +
            (x >= system.nopaint.startDrag.x ? 1 : -1),
          y -
            system.nopaint.startDrag.y +
            (y >= system.nopaint.startDrag.y ? 1 : -1),
        );

        system.nopaint.brush = { x, y, dragBox, pressure: pen.pressure };
      },

      // Helper to display the existing painting on the screen, with an
      // optional pan amount, that returns an adjusted pen pointer as `brush`.

      // TODO: - [] Add Zoom
      //       - [] And Rotation!

      present: (
        { system, screen, wipe, paste, ink, slug, dark, theme },
        tx,
        ty,
      ) => {
        system.nopaint.needsPresent = false;

        const x = tx || system.nopaint.translation.x;
        const y = ty || system.nopaint.translation.y;

        system.nopaint.translation = { x, y };

        const fullbleed =
          x === 0 &&
          y === 0 &&
          screen.width <= system.painting.width &&
          screen.height <= system.painting.height;

        if (fullbleed) {
          // If we are not panned and the painting fills the screen.
          paste(system.painting).paste(system.nopaint.buffer);
        } else {
          // If we are panned or the painting is a custom resolution.

          wipe(theme[dark ? "dark" : "light"].wipeBG)
            .paste(system.painting, x, y, system.nopaint.zoomLevel)
            .paste(system.nopaint.buffer, x, y, system.nopaint.zoomLevel)
            .ink(128)
            .box(
              x,
              y,
              system.painting.width * system.nopaint.zoomLevel,
              system.painting.height * system.nopaint.zoomLevel,
              "outline",
            );
        }

        // Graph `zoomLevel`
        if (system.nopaint.zoomLevel !== 1 && slug !== "prompt")
          ink(255, 127).write(`${system.nopaint.zoomLevel}x`, { x: 6, y: 18 });

        return {
          x,
          y, //,
          //brush: { x: (pen?.x || 0) - x, y: (pen?.y || 0) - y },
        };
      },
      // Kill an existing painting.
      noBang: async (
        { system, store, needsPaint, painting, theme, dark },
        res = { w: screen.width, h: screen.height },
      ) => {
        // console.log("deleting...");
        const deleted = await store.delete("painting", "local:db");
        await store.delete("painting:resolution-lock", "local:db");
        await store.delete("painting:transform", "local:db");
        system.nopaint.undo.paintings.length = 0; // Reset undo stack.
        system.painting = null;
        system.nopaint.resetTransform({ system, screen }); // Reset transform.
        needsPaint();

        // Make a blank painting.
        // I don't like that these getters will not re-associate.
        system.painting = painting(res.w, res.h, ($) => {
          $.wipe(theme[dark ? "dark" : "light"].wipeNum);
        });
        store["painting"] = $commonApi.system.painting;

        // Clear any existing painting recording in RAM and
        // storage.
        await store.delete("painting:record", "local:db");
        if (system.nopaint.recording) {
          system.nopaint.recording = false;
          system.nopaint.record.length = 0;
          console.log("🖌️🛑 Recording cleared.");
        }

        return deleted;
      },
      // Replace a painting entirely, remembering the last one.
      // (This will always enable fixed resolution mode.)
      replace: (
        { system, screen, store, needsPaint },
        painting,
        message = "(replace)",
      ) => {
        system.painting = painting; // Update references.
        store["painting"] = system.painting;
        store.persist("painting", "local:db"); // Persist to storage.
        store["painting:resolution-lock"] = true;
        store.persist("painting:resolution-lock", "local:db");
        system.nopaint.resetTransform({ system, screen }); // Reset transform.
        system.nopaint.storeTransform(store, system);
        system.nopaint.addUndoPainting(system.painting, message);
        system.nopaint.needsPresent = true;
        needsPaint();
      },
      abort: () => (NPnoOnLeave = true),
    },
  },
  // Paint all queued rendering commands immediately.
  flatten: () => {
    return painting.paint(true);
  },
  connect: () => {
    const p = new Promise((resolve, reject) => {
      web3Response = { resolve, reject };
    });
    send({ type: "web3-connect" });
    return p;
  },
  wiggle: function (n, level = 0.2, speed = 6) {
    wiggleAngle = (wiggleAngle + 1 * speed) % 360;
    const osc = sin(num.radians(wiggleAngle));
    return n + n * level * osc;
  },
  dark: true, // Dark mode. (Gets set on startup and on any change.)
  darkMode, // Toggle dark mode or set to `true` or `false`.
  // content: added programmatically: see Content class
  gpuReady: false,
  gpu: {
    message: (content) => {
      const p = new Promise((resolve, reject) => {
        gpuResponse = { resolve, reject };
      });
      send({ type: "gpu-event", content });
      return p;
    },
  },
  // Deprecated in favor of `bios` -> `hitboxes`. (To support iOS)
  // clipboard: {
  //   copy: (text) => {
  //     send({ type: "copy", content: text });
  //   },
  // },
  text: {
    capitalize: text.capitalize,
    reverse: text.reverse,
    // Get the pixel width of a string of characters.
    width: (text, customTypeface = null) => {
      if (Array.isArray(text)) text = text.join(" ");
      // Handle string typeface names
      if (typeof customTypeface === "string") {
        customTypeface = typefaceCache.get(customTypeface) || tf;
      }
      const activeTypeface = customTypeface || tf;
      return text.length * activeTypeface.blockWidth;
    },
    height: (text, customTypeface = null) => {
      // Get the pixel height of a string of characters.
      // Handle string typeface names
      if (typeof customTypeface === "string") {
        customTypeface = typefaceCache.get(customTypeface) || tf;
      }
      const activeTypeface = customTypeface || tf;
      return activeTypeface.blockHeight;
    },
    // Return a text's bounding box.
    box: (
      text,
      pos = { x: 0, y: 0 },
      bounds,
      scale = 1,
      wordWrap = true,
      customTypeface = null,
    ) => {
      if (!text) {
        console.warn("⚠️ No text for `box`.");
        return;
      }
      pos = { ...pos };
      let run = 0;
      // Handle string typeface names
      if (typeof customTypeface === "string") {
        customTypeface = typefaceCache.get(customTypeface) || tf;
      }
      const activeTypeface = customTypeface || tf;
      const blockWidth = activeTypeface.blockWidth * abs(scale);

      const lines = [[]];
      let line = 0;

      if (bounds === undefined) bounds = (text.length + 2) * blockWidth;

      function newLine() {
        run = 0;
        line += 1;
        lines[line] = [];
      }

      function characterWrap(word, preserveSpaceBefore = false) {
        let needsSpace = preserveSpaceBefore && run > 0;

        for (let i = 0; i < word.length; i++) {
          const char = word[i];
          const charLen = blockWidth;
          const spaceLen = needsSpace ? blockWidth : 0;

          if (run + spaceLen + charLen > bounds) {
            newLine();
            needsSpace = false; // Don't add space at start of new line
          }

          if (!lines[line].length) {
            // Start of line - add space if needed, then character
            if (needsSpace) {
              lines[line].push(" " + char);
              run += spaceLen + charLen;
              needsSpace = false;
            } else {
              lines[line].push(char);
              run += charLen;
            }
          } else {
            // Continuing existing word - add space if needed, then character
            if (needsSpace) {
              lines[line][lines[line].length - 1] += " " + char;
              run += spaceLen + charLen;
              needsSpace = false;
            } else {
              lines[line][lines[line].length - 1] += char;
              run += charLen;
            }
          }
        }
      }
      if (wordWrap) {
        const splitWords = text.split(" ");
        const words = [];
        for (let i = 0; i < splitWords.length; i++) {
          if (splitWords[i] === "" && i > 0 && splitWords[i - 1] === "") {
            words[words.length - 1] += " ";
          } else {
            words.push(splitWords[i]);
          }
        }
        words.forEach((word, wordIndex) => {
          const wordLen = word.length * blockWidth;
          const spaceWidth = blockWidth;

          if (wordLen >= bounds) {
            // Pass true to preserve space if this isn't the first word
            characterWrap(word, wordIndex > 0);
            // Don't add extra space width after character wrapping
            return;
          }
          if (word.includes("\n")) {
            const segs = word.split("\n");
            segs.forEach((seg, i) => {
              const segLen = seg.length * blockWidth;
              // For segments after a newline (i > 0), always start a new line
              if (i > 0) newLine();
              // For boundary checking, account for space only if this isn't the first item on the line
              const spaceNeeded = run > 0 ? spaceWidth : 0;
              if (run + spaceNeeded + segLen >= bounds) newLine();
              lines[line].push(seg);
              run += segLen + (wordIndex < words.length - 1 ? spaceWidth : 0); // Only add space if not the last word
            });
          } else {
            // For boundary checking, account for space only if this isn't the first word on the line
            const spaceNeeded = run > 0 ? spaceWidth : 0;
            if (run + spaceNeeded + wordLen >= bounds) newLine();
            lines[line].push(word);
            run += wordLen + (wordIndex < words.length - 1 ? spaceWidth : 0); // Only add space if not the last word
          }
        });
      } else {
        characterWrap(text);
      }

      const blockMargin = 1;
      const blockHeight =
        ((customTypeface || activeTypeface).blockHeight + blockMargin) * scale;

      if (lines.length >= 1 && pos.center && pos.center.indexOf("y") !== -1) {
        pos.y =
          $activePaintApi.screen.height / 2 -
          (lines.length * blockHeight) / 2 +
          blockHeight / 2 +
          (pos.y || 0);
      }

      const height = lines.length * blockHeight;
      const box = { x: pos.x, y: pos.y, width: bounds, height };

      if (lines[0]?.[0].startsWith("test:")) console.log(lines);
      return { pos, box, lines };
    },
  },
  num: {
    add: num.add,
    wrap: num.wrap,
    even: num.even,
    odd: num.odd,
    clamp: num.clamp,
    rand: num.rand,
    randInt: num.randInt,
    randInd: num.randInd,
    randIntArr: num.randIntArr,
    randIntRange: num.randIntRange,
    rangedInts: num.rangedInts,
    multiply: num.multiply,
    perlin: num.perlin,
    dist: num.dist,
    dist3d: num.dist3d,
    radians: num.radians,
    degrees: num.degrees,
    lerp: num.lerp,
    map: num.map,
    arrMax: num.arrMax,
    arrCompress: num.arrCompress,
    Track: num.Track,
    timestamp: num.timestamp,
    p2: num.p2,
    midp: num.midp,
    number: num.number,
    intersects: num.intersects,
    signedCeil: num.signedCeil,
    signedFloor: num.signedFloor,
    vec2: vec2,
    vec3: vec3,
    vec4: vec4,
    mat3: mat3,
    mat4: mat4,
    quat: quat,
    parseColor: num.parseColor,
    findColor: num.findColor,
    saturate: num.saturate,
    desaturate: num.desaturate,
    shiftRGB: num.shiftRGB,
    rgbToHexStr: num.rgbToHexStr,
    hexToRgb: num.hexToRgb,
    blend: num.blend,
    rgbToHsl: num.rgbToHsl,
    hslToRgb: num.hslToRgb,
    rainbow: num.rainbow,
  },
  geo: {
    Box: geo.Box,
    DirtyBox: geo.DirtyBox,
    Grid: geo.Grid,
    Circle: geo.Circle,
    linePointsFromAngle: geo.linePointsFromAngle,
    pointFrom: geo.pointFrom,
    Race: geo.Race,
    Quantizer: geo.Quantizer,
  },
  ui: {
    Button: ui.Button,
    TextButton: ui.TextButton,
    TextInput: TextInput,
  },
  help: {
    choose: help.choose,
    flip: help.flip,
    repeat: help.repeat,
    every: help.every,
    any: help.any,
    anyIndex: help.anyIndex,
    anyKey: help.anyKey,
    resampleArray: help.resampleArray,
    each: help.each,
    shuffleInPlace: help.shuffleInPlace,
    serializePainting: (painting) => {
      if (!painting) return;
      const pixels = uint8ArrayToBase64(painting.pixels);
      return { width: painting.width, height: painting.height, pixels };
    },
    deserializePainting: (painting) => {
      if (!painting) return;
      const pixels = base64ToUint8Array(painting.pixels);
      return { width: painting.width, height: painting.height, pixels };
    },
  },
  gizmo: {
    Hourglass: gizmo.Hourglass,
    EllipsisTicker: gizmo.EllipsisTicker,
    Ticker: Ticker,
    ticker: ticker,
  },
  rec: new Recorder(),
  net: {
    signup: () => {
      send({ type: "signup" });
    },
    login: () => {
      store.delete("handle");
      send({ type: "login" });
    }, // { email }
    logout: () => {
      store.delete("handle");
      send({ type: "logout" });
      $commonApi.broadcast("logout:success");
      chatClient.system?.server?.send("logout");
      // Send a "logout" message here to the chat server.

      // TODO: And probably the session server as well in
      //       the future. 24.05.23.21.27
    },
    pieces: (typeof location !== 'undefined' && location.protocol && location.host) 
      ? `${location.protocol}//${location.host}/aesthetic.computer/disks`
      : '/aesthetic.computer/disks',
    parse, // Parse a piece slug.
    // lan: // Set dynamically.
    // host: // Set dynamically.
    // loadFailureText: // Set dynamically.
    // Make a user authorized / signed request to the api.
    // Used both in `motd` and `handle`.
    requestDocs: async () => {
      if (typeof docs === "object") return Promise.resolve(docs);
      return fetch("/docs.json")
        .then((response) => {
          if (response.status !== 200) {
            throw new Error("Network failure: " + response.status);
          }
          return response.json();
        })
        .then((d) => {
          docs = d;
          return docs;
        })
        .catch((err) => console.error("🔴 📚 Couldn't get docs:", err));
    },
    userRequest: async (method, endpoint, body) => {
      try {
        const token = await $commonApi.authorize(); // Get user token.
        if (!token) throw new Error("🧖 Not logged in.");

        const headers = {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
        };

        const options = { method, headers };
        if (body) options.body = JSON.stringify(body);
        const response = await fetch(endpoint, options);

        if (response.status === 500) {
          try {
            const json = await response.json();
            return { status: response.status, ...json };
          } catch (e) {
            return { status: response.status, message: response.statusText };
          }
        } else {
          const clonedResponse = response.clone();
          try {
            return {
              ...(await clonedResponse.json()),
              status: response.status,
            };
          } catch {
            return { status: response.status, body: await response.text() };
          }
        }
      } catch (error) {
        console.error("🚫 Error:", error);
        return { message: "unauthorized" };
      }
    },
    // Generic request function that can handle both authenticated and anonymous requests
    request: async (method, endpoint, body, options = {}) => {
      try {
        const { requireAuth = false, anonymous = false } = options;
        
        const headers = {
          "Content-Type": "application/json",
        };

        // Try authentication unless explicitly anonymous
        if (!anonymous) {
          try {
            const token = await $commonApi.authorize();
            if (token) {
              headers.Authorization = `Bearer ${token}`;
            } else if (requireAuth) {
              throw new Error("🧖 Authentication required but not logged in.");
            }
          } catch (authError) {
            if (requireAuth) {
              throw authError;
            }
            // Continue with anonymous request if auth fails and not required
          }
        }

        const fetchOptions = { method, headers };
        if (body) fetchOptions.body = JSON.stringify(body);
        
        const response = await fetch(endpoint, fetchOptions);

        if (response.status === 500) {
          try {
            const json = await response.json();
            return { status: response.status, ...json };
          } catch (e) {
            return { status: response.status, message: response.statusText };
          }
        } else {
          const clonedResponse = response.clone();
          try {
            return {
              ...(await clonedResponse.json()),
              status: response.status,
            };
          } catch {
            return { status: response.status, body: await response.text() };
          }
        }
      } catch (error) {
        console.error("🚫 Request error:", error);
        const errorMessage = error?.message || error?.toString() || "Request failed";
        return { status: 0, message: errorMessage };
      }
    },
    // Loosely connect the UDP receiver.
    udp: (receive) => {
      udpReceive = receive;
      return udp;
    },
    hiccup: (hiccupIn = 5, outageSeconds = 5) => {
      console.log("😵 Hiccuping in:", hiccupIn, "seconds.");
      clearTimeout(hiccupTimeout);
      hiccupTimeout = setTimeout(() => {
        console.log("😶‍🌫️ Hiccup!");
        chatClient.system?.server.kill(outageSeconds); // Disconnect from chat.
        socket?.kill(outageSeconds); // Diconnect from socket session.
        udp?.kill(outageSeconds); // Disconnect from UDP.
      }, hiccupIn * 1000);
    },
    // Preload a typeface for use with string-based font names
    preloadTypeface: async (name) => {
      if (typefaceCache.has(name)) {
        return typefaceCache.get(name);
      }
      return await getOrCreateTypeface(name, $commonApi.net.preload);
    },
  },
  needsPaint: () => {
    noPaint = false;
    if (system === "nopaint") $commonApi.system.nopaint.needsPresent = true;
  }, // TODO: Does "paint" needs this?
  store,
  pieceCount: -1, // Incs to 0 when the first piece (usually the prompt) loads.
  //                 Increments by 1 each time a new piece loads.
  debug,
  logs,
};

chatClient.$commonApi = $commonApi; // Add a reference to the `Chat` module.

const nopaintAPI = $commonApi.system.nopaint;

// Broadcast to other tabs in the same origin.
const channel = new BroadcastChannel("aesthetic.computer");

channel.onmessage = (event) => {
  console.log(`🗼 Got broadcast: ${event.data}`);
  processMessage(event.data);
};

async function processMessage(msg) {
  if (logs.messaging) console.log(`🗼 Processing broadcast: ${msg}`);
  if (msg.startsWith("handle:updated")) {
    // 👰‍♀️ Update the user handle if it changed.
    const newHandle = msg.split(":").pop();
    HANDLE = "@" + newHandle;
    send({ type: "handle", content: HANDLE });
    store["handle:received"] = true;
    store["handle"] = newHandle;
    // store.persist("handle");
    return;
  }

  // Refresh the window if we logged in or out from another tab.
  if (msg === "login:success" && !USER) {
    $commonApi.net.refresh();
    return;
  }

  if (msg === "logout:success" && USER) {
    $commonApi.net.refresh(); // 🗒️ This should always be fast enough?
    return;
  }
}

$commonApi.broadcast = (msg) => {
  processMessage(msg); // Process locally.
  channel.postMessage(msg);
};

// Spawn a session backend for a piece.
async function session(slug, forceProduction = false, service) {
  let endPoint = "/session/" + slug;
  const params = { service };
  if (forceProduction) params.forceProduction = 1;
  endPoint += "?" + new URLSearchParams(params);

  const req = await fetch(endPoint);

  let session;
  if (req.status === 200 || req.status === 304) {
    session = await req.text().then((text) => {
      try {
        return JSON.parse(text);
      } catch (e) {
        return text;
      }
    });
  } else {
    session = await req.text();
  }

  if (typeof session === "string") return session;

  //if (debug && logs.session) {
  // console.log(
  //   `🐕‍🦺 Session: ${slug} - ${session.backend || session.name || session.url}`,
  // );
  //}
  // Return the active session if the server knows it's "Ready", otherwise
  // wait for the one we requested to spin up.
  // (And in debug mode we just get a local url from "/session" so no need
  // to check that.)
  if (session.state === "Ready" || (debug && !forceProduction)) {
    return session;
  } else {
    let eventSource = new EventSource(
      `https://api.jamsocket.com/backend/${session.name}/status/stream`,
      // See also: https://docs.jamsocket.com/api-docs/#get-a-backends-status-stream
    );

    return new Promise((resolve, reject) => {
      eventSource.onmessage = (event) => {
        const update = JSON.parse(event.data);
        const colors = {
          Ready: "🟢",
          Loading: "🟠",
          Starting: "🟡",
        };
        const color = colors[update.state] || "🔵";

        if (update.state === "Ready") {
          if (logs.session)
            console.log(color + `\`${slug}\` Backend:`, update.state);
        }

        if (update.state === "Loading") {
          currentHUDStatusColor = "red";
        } else if (update.state === "Ready") {
          currentHUDStatusColor = "yellow";
        } else if (update.state === "Starting") {
          currentHUDStatusColor = "orange";
        } else {
          currentHUDStatusColor = "brown";
        }

        $commonApi.needsPaint(); // Make sure the label gets updated.

        if (update.state === "Ready") {
          eventSource.close(); // Close the event stream handler.
          resolve(session);
        } else {
          if (update.state !== "Loading" && update.state !== "Starting") {
            eventSource.close(); // Close the event stream handler.
          }
        }
      };
    });
  }
}

// Just for "update".
const $updateApi = {};

// 🖼 Painting

// Pre-fab models:
const QUAD = {
  type: "quad",
  positions: [
    // Triangle 1 (Left Side)
    [-1, -1, 0, 1], // Bottom Left
    [-1, 1, 0, 1], // Top Left
    [1, 1, 0, 1], // Top Right
    // Triangle 2 (Right Side)
    [-1, -1, 0, 1], // Bottom Left
    [1, -1, 0, 1], // Bottom Right
    [1, 1, 0, 1], // Top Right
  ],
  indices: [
    // These are not re-used for now.
    // One
    0, 1, 2,
    //Two
    3, 4, 5,
  ],
};

// A cube of lines.
const CUBEL = {
  type: "line",
  positions: [
    // Back
    [-0.5, -0.5, 0.5, 1], // Down
    [-0.5, 0.5, 0.5, 1],

    [-0.5, 0.5, 0.5, 1], // Across
    [0.5, 0.5, 0.5, 1],

    [0.5, 0.5, 0.5, 1], // Up
    [0.5, -0.5, 0.5, 1],

    [0.5, -0.5, 0.5, 1], // Back
    [-0.5, -0.5, 0.5, 1],
    // Front
    [-0.5, -0.5, -0.5, 1], // Down
    [-0.5, 0.5, -0.5, 1],

    [-0.5, 0.5, -0.5, 1], // Across
    [0.5, 0.5, -0.5, 1],

    [0.5, 0.5, -0.5, 1], // Up
    [0.5, -0.5, -0.5, 1],

    [0.5, -0.5, -0.5, 1], // Back
    [-0.5, -0.5, -0.5, 1],
    // Bars (back to front)
    [-0.5, -0.5, 0.5, 1], // Top Left
    [-0.5, -0.5, -0.5, 1],

    [-0.5, 0.5, 0.5, 1], // Bottom Left
    [-0.5, 0.5, -0.5, 1],

    [0.5, 0.5, 0.5, 1], // Up
    [0.5, 0.5, -0.5, 1],

    [0.5, -0.5, 0.5, 1], // Back
    [0.5, -0.5, -0.5, 1],
  ],
};

const ORIGIN = {
  type: "line",
  positions: [
    [-0.5, 0, 0, 1], // Horizontal X
    [0.5, 0, 0, 1],
    [0, 0, -0.5, 1], // Horizontal Z
    [0, 0, 2, 1],
    [0, -0.5, 0, 1], // Vertical
    [0, 0.5, 0, 1],
  ],
  colors: [
    [255, 0, 0, 255],
    [255, 0, 0, 255],
    [0, 255, 0, 255],
    [0, 255, 0, 255],
    [0, 0, 255, 255],
    [0, 0, 255, 255],
  ],
};

const TRI = {
  type: "triangle",
  positions: [
    [-1, -1, 0, 1], // Bottom Left
    [0, 1, 0, 1], // Top Left
    [1, -1, 0, 1], // Top Right
    // Triangle 2 (Right Side)
  ],
  indices: [0, 1, 2],
};

const LINE = {
  type: "line",
  positions: [
    [0, 0, 0, 1], // Bottom
    [0, 1, 0, 1], // Top
  ],
  indices: [0, 1],
};

// Inputs: (r, g, b), (r, g, b, a) or an array of those.
//         (rgb) for grayscale or (rgb, a) for grayscale with alpha.
//         Or hex with "#000000" or "0x000000" or 0x000000.
// TODO: Add `erase` anc all css color alpha support. 23.07.20.14.45
// TODO: Add transparency and short hex to hex support.
// TODO: Add better hex support via: https://stackoverflow.com/a/53936623/8146077

function ink() {
  return graph.color(...graph.findColor(...arguments));
}

function ink2() {
  if (arguments[0] === null) {
    return graph.color2(null);
  } else {
    return graph.color2(...graph.findColor(...arguments));
  }
}

// 🔎 PAINTAPI
const $paintApi = {
  // 1. Composite functions (that use $activePaintApi)
  //    (Must be unwrapped)
  // Prints a line of text using the default / current global font.
  // Argument options:
  // text, pos: {x, y, center}, bg (optional)

  // Parameters:
  // text, x, y, options
  // text, pos, bg, bounds, wordWrap = true, typeface = null
  write: function () {
    let text = arguments[0],
      pos,
      bg,
      bounds,
      wordWrap = true,
      customTypeface = null;
    if (text === undefined || text === null || text === "" || !tf)
      return $activePaintApi; // Fail silently if no text.

    text =
      typeof text === "object" && text !== null
        ? JSON.stringify(text)
        : text.toString();

    // Assume: text, x, y, options
    if (typeof arguments[1] === "number") {
      pos = { x: arguments[1], y: arguments[2] };
      const options = arguments[3];
      bg = options?.bg;
      bounds = options?.bounds;
      wordWrap = options?.wordWrap === undefined ? wordWrap : options.wordWrap;
      customTypeface = options?.typeface;
    } else {
      pos = arguments[1];
      bg = arguments[2];
      bounds = arguments[3];
      wordWrap = arguments[4] === undefined ? wordWrap : arguments[4];
      customTypeface = arguments[5];
    }

    // Convert string font name to Typeface instance if needed
    if (typeof customTypeface === "string") {
      // Check if we have a cached typeface first
      if (typefaceCache.has(customTypeface)) {
        customTypeface = typefaceCache.get(customTypeface);
      } else {
        // Create a new typeface and load it immediately if it's MatrixChunky8 or unifont
        console.log(`🔄 Creating new typeface: ${customTypeface}`);
        const newTypeface = new Typeface(customTypeface);
        
        // Load the typeface asynchronously for BDF fonts
        if (newTypeface.data?.bdfFont || customTypeface === "unifont") {
          newTypeface.load($commonApi.net.preload, () => {
            // Force repaint when new glyphs are loaded
            if (typeof window !== 'undefined' && window.$activePaintApi?.needsPaint) {
              window.$activePaintApi.needsPaint();
            }
          });
        }
        
        typefaceCache.set(customTypeface, newTypeface);
        customTypeface = newTypeface;
      }
    } // 🎨 Color code processing
    // Check for color codes like \\blue\\, \\red\\, \\255,20,147\\, \\255,20,147,128\\ etc.
    const colorCodeRegex =
      /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/g;
    const hasColorCodes = text.includes("\\");

    // Check if inline color processing is disabled via options
    const noInlineColor =
      (typeof arguments[1] === "object" && arguments[1]?.noInlineColor) ||
      (typeof arguments[3] === "object" && arguments[3]?.noInlineColor);

    // console.log("🎨🎨🎨 DISK.MJS WRITE DEBUG:", {
    //   textLength: text.length,
    //   hasColorCodes,
    //   noInlineColor,
    //   textPreview: text.slice(0, 100),
    //   containsRgb: text.includes("rgb("),
    //   stackTrace: noInlineColor ? new Error().stack.split('\n')[1].trim() : null
    // });

    if (hasColorCodes && !noInlineColor) {
      // console.log("🎨 Color Processing Debug: Starting color code processing");
      // console.log("🎨 Original text:", text);
      // console.log("🎨 Color regex:", colorCodeRegex);
      // console.log("🎨 Text length:", text.length);
      // console.log("🎨 Last 20 chars:", text.slice(-20));

      // Remember the current ink color to restore it later
      const originalColor = $activePaintApi.inkrn();

      // Process color codes into per-character color array
      let cleanText = "";
      let charColors = [];
      let currentColor = null;

      // GC OPTIMIZATION: Avoid repeated string operations where possible
      let hasComma = false;
      let rgbValues = null;

      // Split text by color codes and process each segment
      const segments = text.split(colorCodeRegex);
      // console.log("🎨 Split segments:", segments);

      for (let i = 0; i < segments.length; i++) {
        if (i % 2 === 0) {
          // This is regular text
          const segment = segments[i];
          // console.log(`🎨 Processing text segment ${i}: "${segment}"`);
          for (let j = 0; j < segment.length; j++) {
            cleanText += segment[j];
            charColors.push(currentColor);
          }
        } else {
          // This is a color name (from the captured group)
          const colorName = segments[i];

          // console.log("🎨 Color Debug: Processing color:", colorName);

          // GC OPTIMIZATION: Check for comma once, reuse array when possible
          hasComma = colorName.includes(",");
          if (hasComma) {
            // Reuse array if it exists, otherwise create new one (support RGBA)
            if (!rgbValues) rgbValues = [0, 0, 0, 255];
            
            let idx = 0;
            let num = 0;
            let valid = true;
            
            // Parse RGB/RGBA values manually to avoid split/map allocation
            for (let k = 0; k < colorName.length && idx < 4; k++) {
              const char = colorName[k];
              if (char >= '0' && char <= '9') {
                num = num * 10 + (char.charCodeAt(0) - 48);
              } else if (char === ',' || k === colorName.length - 1) {
                if (num < 0 || num > 255) {
                  valid = false;
                  break;
                }
                rgbValues[idx++] = num;
                num = 0;
              } else if (char !== ' ') {
                valid = false;
                break;
              }
            }
            
            if (valid && (idx === 3 || idx === 4)) {
              // Default alpha to 255 if only RGB provided
              if (idx === 3) rgbValues[3] = 255;
              // Always slice to 4 components to include alpha
              currentColor = rgbValues.slice(0, 4);
              // console.log("🎨 RGB/RGBA Debug: Successfully set color array:", currentColor);
            } else {
              currentColor = colorName; // Fallback to string if parsing fails
              // console.log("🎨 RGB/RGBA Debug: Parsing failed, using string:", colorName);
            }
          } else {
            currentColor = colorName;
            // console.log("🎨 Color Debug: Using string color:", colorName);
          }
        }
      }

      // console.log("🎨 Final processing results:");
      // console.log("🎨 Clean text:", cleanText);
      // console.log("🎨 Character colors:", charColors);

      // Check if we have any actual text to display after removing color codes
      if (cleanText.trim().length === 0) {
        return $activePaintApi; // Exit silently if no text content remains
      }

      // Now use the original text processing logic but with per-character colors
      const scale = pos?.size || 1;

      if (bounds) {
        const tb = $commonApi.text.box(
          cleanText,
          pos,
          bounds,
          scale,
          wordWrap,
          customTypeface,
        );
        if (!tb || !tb.lines) {
          return $activePaintApi; // Exit silently if text.box fails
        }

        // Simple character-by-character mapping for wrapped lines
        let cleanTextIndex = 0;

        tb.lines.forEach((line, lineIndex) => {
          const joinedLine = line.join(" ");
          const lineColors = [];

          // Map each character in the line to its color from the original charColors array
          for (let charIndex = 0; charIndex < joinedLine.length; charIndex++) {
            // Use the color from the original mapping, or null if we're past the end
            if (cleanTextIndex < charColors.length) {
              lineColors.push(charColors[cleanTextIndex]);
            } else {
              lineColors.push(null);
            }
            cleanTextIndex++;
          }

          (customTypeface || tf)?.print(
            $activePaintApi,
            tb.pos,
            lineIndex,
            joinedLine,
            bg,
            lineColors,
          );
        });
      } else {
        // Break on `\n` and handle separate lines
        if (cleanText.indexOf("\n") !== -1) {
          const lines = cleanText.split("\n");
          const lineHeightGap = 2;
          let charIndex = 0;
          
          console.log("🎨 NEWLINE DEBUG:");
          console.log("🎨 cleanText:", JSON.stringify(cleanText));
          console.log("🎨 cleanText.length:", cleanText.length);
          console.log("🎨 charColors.length:", charColors.length);
          console.log("🎨 lines:", lines);
          
          lines.forEach((line, index) => {
            console.log(`🎨 Line ${index}: "${line}" (length: ${line.length})`);
            console.log(`🎨 charIndex before: ${charIndex}`);
            
            const lineColors = charColors.slice(
              charIndex,
              charIndex + line.length,
            );
            
            console.log(`🎨 lineColors for line ${index}:`, lineColors.slice(0, 10), '...');
            
            (customTypeface || tf)?.print(
              $activePaintApi,
              {
                x: pos?.x,
                y: pos
                  ? pos.y +
                    index * (customTypeface || tf).blockHeight +
                    lineHeightGap
                  : undefined,
              },
              0,
              line,
              bg,
              lineColors,
            );
            // Move to next line: skip past current line + the newline character (except for last line)
            charIndex += line.length;
            if (index < lines.length - 1) {
              charIndex += 1; // Skip the newline character for all but the last line
            }
            console.log(`🎨 charIndex after: ${charIndex}`);
          });
        } else {
          (customTypeface || tf)?.print(
            $activePaintApi,
            pos,
            0,
            cleanText,
            bg,
            charColors,
          );
        }
      }

      // Restore the original ink color
      $activePaintApi.ink(...originalColor);

      return $activePaintApi;
    }

    // 🎁 Original code for text without color codes
    // If noInlineColor is true, strip color codes from the text first
    if (noInlineColor && hasColorCodes) {
      text = text.replace(/\\[a-zA-Z]+\\/g, "");
    }

    // See if the text length is greater than the bounds, and if it is then
    // print on a new line.
    const scale = pos?.size || 1;

    if (bounds) {
      const tb = $commonApi.text.box(
        text,
        pos,
        bounds,
        scale,
        wordWrap,
        customTypeface,
      ); // TODO: Get the current ink color, memoize it, and make it static here.
      //       23.10.12.22.04
      tb.lines.forEach((line, index) => {
        // console.log(line, index);
        (customTypeface || tf)?.print(
          $activePaintApi,
          tb.pos,
          index,
          line.join(" "),
          bg,
        );
      });
    } else {
      // Break on `\n` and handle separate lines
      if (text.indexOf("\n") !== -1) {
        const lines = text.split("\n"); // Split text on new line characters
        const lineHeightGap = 2;
        lines.forEach((line, index) => {
          (customTypeface || tf)?.print(
            $activePaintApi,
            {
              x: pos?.x,
              y: pos
                ? pos.y +
                  index * (customTypeface || tf).blockHeight +
                  lineHeightGap
                : undefined,
            },
            0,
            line,
            bg,
          );
          // Adjust `lineHeight` as needed based on your text spacing
        });
      } else {
        //if (text === "POW") console.log($activePaintApi.screen); 24.12.10.07.26 - Get write working with deferred rendering and page.
        (customTypeface || tf)?.print($activePaintApi, pos, 0, text, bg); // Or print a single line.
      }
    }

    return $activePaintApi;
  },
  // 2. Image Utilities
  clonePixels: graph.cloneBuffer,
  colorsMatch: graph.colorsMatch,
  color: graph.findColor,
  resize: graph.resize,
  // 3. 3D Classes & Objects
  Camera: graph.Camera,
  Form: graph.Form,
  Dolly: graph.Dolly,
  TRI,
  QUAD,
  LINE,
  CUBEL,
  ORIGIN,
};

// TODO: Eventually move this to `num`. 24.07.23.18.52
function normalizeAngle(angle) {
  return ((angle % 360) + 360) % 360;
}

let turtleAngle = 270;
let turtleDown = false;
let turtlePosition = { x: 0, y: 0 };

// This is where I map the API functions that anyone can use, to the internal
// code that represents them...

// Rendering of 3D forms.

const formsToClear = [];
let backgroundColor3D = [0, 0, 0, 255];
let formsSent = {}; // TODO: This should be cleared more often...

// `cpu: true` enabled software rendering
function form(
  forms,
  cam = $commonApi.system.fps.doll.cam,
  { cpu, background } = {
    cpu: true,
    keep: true,
    background: backgroundColor3D,
  },
) {
  // Exit silently if no forms are present.
  if (forms === undefined || forms?.length === 0) return;

  // Mark that 3D rendering is being used
  if (!needs3DRendering) {
    needs3DRendering = true;
  }
  
  // Track if CPU 3D rendering (software rasterizer) is needed for depth buffer
  if (cpu === true && !needsCPU3D) {
    needsCPU3D = true;
    // If depth buffer hasn't been initialized yet, we'll need a reframe
    if (!depthBufferInitialized) {
      // The depth buffer will be created on the next frame cycle
    }
  }

  if (cpu === true) {
    if (formReframing) {
      cam.resize();
      formReframing = false;
    }
    // GC OPTIMIZATION: Use for loop instead of filter + forEach
    if (Array.isArray(forms)) {
      for (let i = 0; i < forms.length; i++) {
        const form = forms[i];
        if (form) form.graph(cam);
      }
    } else {
      forms.graph(cam);
    }
  } else {
    // GPU forms.
    if (!Array.isArray(forms)) forms = [forms];

    // GC OPTIMIZATION: Use for loop instead of forEach
    for (let i = 0; i < formsToClear.length; i++) {
      delete formsSent[formsToClear[i]];
    }
    formsToClear.length = 0;

    // Build a list of forms to send, ignoring already sent ones by UID.
    const formsToSend = [];

    // GC OPTIMIZATION: Use for loop instead of filter + forEach
    for (let i = 0; i < forms.length; i++) {
      const form = forms[i];
      if (!form) continue;
      // Clear out any trash in `formsSent` that do not have IDs left in forms.
      //if (formsSent[forms.uid])

      // A. If the form has not been sent yet...
      if (formsSent[form.uid] === undefined && form.vertices.length > 0) {
        // Set the form to expire automatically if keep is false.
        formsToSend.push(form);
        formsSent[form.uid] = form;
        //console.log("Forms sent:", Object.keys(formsSent).length);
        form.gpuVerticesSent = form.vertices.length;
        form.gpuReset = false;
      } else {
        // B. If the form has been sent, but the form has changed and
        //    needs a partial state update or is simply being redrawn.
        let msgCount = 0;

        if (form.gpuRecolored === true) {
          formsToSend.push({
            update: "form:color",
            uid: form.uid,
            color: form.color,
          });
          msgCount += 1;
          form.gpuRecolored = false;
        }

        // Transform the geometry.
        if (form.gpuTransformed === true) {
          formsToSend.push({
            update: "form:transform",
            uid: form.uid,
            rotation: form.rotation,
            position: form.position,
            scale: form.scale,
          });
          form.gpuTransformed = false;
          msgCount += 1;
        }

        if (form.vertices.length > form.gpuVerticesSent || form.gpuReset) {
          // Add vertices to buffered forms.
          formsToSend.push({
            update: "form:buffered:add-vertices",
            uid: form.uid,
            reset: form.gpuReset,
            vertices: form.vertices.slice(form.gpuVerticesSent),
            length: form.vertices.length, // TODO: These aren't being used anymore / they are generated from the GPU.
            pastLength: form.gpuVerticesSent,
          });

          // Update form state now that we are sending the message.
          // TODO: Put these both under a "gpu" object in form.
          // console.log(form.gpuReset);
          form.gpuReset = false;
          form.gpuVerticesSent = form.vertices.length;
          msgCount += 1;
        }

        if (msgCount === 0) {
          // Simply tell the system we are still drawing the form... otherwise
          // it will get cleared.
          formsToSend.push({
            update: "form:touch",
            uid: form.uid,
          });
        }
      }
    }

    if (formsToSend.length === 0) return;

    // console.log("Sending form...", performance.now())

    // Only send a background update if the value changed.
    if (background !== backgroundColor3D) {
      send({
        type: "gpu-event",
        content: {
          type: "background-change",
          content: background,
        },
      });
      backgroundColor3D = background;
    }

    send({
      type: "forms",
      content: {
        forms: formsToSend,
        cam: {
          position: cam.position,
          rotation: cam.rotation,
          scale: cam.scale,
          fov: cam.fov,
          near: cam.near,
          far: cam.far,
        },
        color: graph.color(),
      },
    });

    // paintFormsResolution?.();
    // return new Promise((resolve) => {
    // paintFormsResolution = resolve;
    // });
  }
}

// Used by `paste` and `stamp` to prefetch bitmaps of the network as needed.
// Occurs also when loading a piece's source code.
function prefetchPicture(code) {
  if (paintings[code] === "fetching") return;

  paintings[code] = "fetching";

  if (code.startsWith("http")) {
    $commonApi.get
      .picture(code)
      .then(({ img }) => (paintings[code] = img))
      .catch(() => delete paintings[code]);
  } else {
    const [author, timestamp] = code.split("/");
    $commonApi.get
      .painting(timestamp)
      .by(author)
      .then(({ img }) => (paintings[code] = img))
      .catch(() => delete paintings[code]);
  }
}

const $paintApiUnwrapped = {
  // Turtle graphics: 🐢 crawl, left, right, up, down, goto, face
  // Move the turtle forward based on angle.
  crawl: (steps = 1) => {
    const x2 = turtlePosition.x + steps * cos(num.radians(turtleAngle));
    const y2 = turtlePosition.y + steps * sin(num.radians(turtleAngle));
    if (turtleDown) {
      graph.line(turtlePosition.x, turtlePosition.y, x2, y2);
      // console.log($activePaintApi.line);
      // console.log("turtle down lining!", turtlePosition, x2, y2);
    }
    turtlePosition.x = x2;
    turtlePosition.y = y2;
    // console.log("🐢 Crawl:", steps);
    return { x: turtlePosition.x, y: turtlePosition.y };
  },
  // Turn turtle left n degrees.
  left: (d = 1) => {
    turtleAngle = normalizeAngle(turtleAngle - d);
    return turtleAngle;
  },
  // Turn turtle right n degrees.
  right: (d = 1) => {
    turtleAngle = normalizeAngle(turtleAngle + d);
    return turtleAngle;
  },
  // Turtle pen up.
  up: () => {
    turtleDown = false;
    // console.log("🐢 Up");
  },
  // Turtle pen down.
  down: () => {
    turtleDown = true;
    // console.log("🐢 Down");
  },
  // Teleport the turtle position.
  goto: (x = screen.width / 2, y = screen.height / 2) => {
    if (turtleDown) {
      graph.line(turtlePosition.x, turtlePosition.y, x, y);
    }
    turtlePosition.x = x;
    turtlePosition.y = y;
    return { x: turtlePosition.x, y: turtlePosition.y };
  },
  face: (angle = 0) => {
    turtleAngle = normalizeAngle(angle);
    return turtleAngle;
  },
  // Shortcuts
  // l: graph.line,
  // i: ink,
  // Defaults
  blend: graph.blendMode,
  page: function () {
    if (arguments[0]?.api) {
      // console.log("New paint api?", arguments[0].api);
      // $activePaintApi = arguments[0].api;
    }
    // console.log(arguments);

    // const oldScreen = $activePaintApi.screen;
    // Mock out the screen here using the arguments.
    $activePaintApi.screen = {
      width: arguments[0].width,
      height: arguments[0].height,
    };
    //console.log(
    //  "Updated active paint api:",
    //  $activePaintApi.screen.width,
    //  $activePaintApi.screen.height,
    //);
    // }
    graph.setBuffer(...arguments);
  },
  edit: graph.changePixels, // Edit pixels by pasing a callback.
  // Color
  ink: function () {
    const out = ink(...arguments);
    twoDCommands.push(["ink", ...out]);
  },
  ink2: function () {
    const out = ink2(...arguments);
    twoDCommands.push(["ink2", ...(out || [])]);
  },
  // inkrn: () => graph.c.slice(), // Get current inkColor.
  // 2D
  wipe: function () {
    const cc = graph.c.slice(0);
    ink(...arguments);
    graph.clear();
    twoDCommands.push(["wipe", ...graph.c]);
    ink(...cc);
  },
  // Erase the screen.
  clear: function () {
    const cc = graph.c.slice(0);
    ink(0, 0);
    graph.clear();
    ink(...cc);
  },
  copy: graph.copy,
  paste: function paste() {
    if (typeof arguments[0] === "string") {
      // Check to see if the bitmap has been cached by this piece already.
      const code = arguments[0];
      if (paintings[code] && paintings[code] !== "fetching") {
        graph.paste(paintings[code], ...[...arguments].slice(1));
      } else if (paintings[code] !== "fetching") {
        prefetchPicture(code);
      }
    } else {
      graph.paste(...arguments);
    }
  },
  // Similar to paste, but always draws from the center of x, y.
  // Has partial support for {center, bottom}. 24.02.15.12.19
  stamp: function stamp() {
    let params;
    // Parse the parameters and lay out the stamp.
    function makeLayout() {
      if (typeof params[0] === "object") {
        const layout = params[0];
        if (layout.center === "x") {
          params[0] = $activePaintApi.screen.width / 2;
        } else {
          params[0] = 0;
        }
        if (layout.bottom !== undefined) {
          params[1] =
            $activePaintApi.screen.height -
            layout.bottom -
            paintings[code].height / 2;
        } else {
          params[1] = 0;
        }
      }
    }
    if (typeof arguments[0] === "string") {
      // Check to see if the bitmap has been cached by this piece already.
      const code = arguments[0];
      params = [...arguments].slice(1);
      if (paintings[code] && paintings[code] !== "fetching") {
        makeLayout();
        graph.stamp(paintings[code], ...params);
      } else if (paintings[code] !== "fetching") {
        prefetchPicture(code);
      }
    } else {
      params = [...arguments].slice(1);
      if (params.length === 0) params = [0, 0];
      makeLayout();
      graph.stamp(arguments[0], ...params);
    }
  },
  pixel: graph.pixel,
  plot: function () {
    if (arguments.length === 1) {
      graph.plot(arguments[0].x, arguments[0].y);
    } else {
      graph.plot(...arguments);
    }
  }, // TODO: Should this be renamed to set?
  flood: graph.flood,
  point: function () {
    const out = graph.point(...arguments);
    twoDCommands.push(["point", ...out]);
  },
  line: graph.line,
  lineAngle: graph.lineAngle,
  pline: graph.pline,
  pppline: graph.pixelPerfectPolyline,
  oval: graph.oval,
  circle: graph.circle,
  poly: graph.poly,
  box: graph.box,
  shape: graph.shape,
  grid: graph.grid,
  draw: graph.draw,
  printLine: graph.printLine, // TODO: This is kind of ugly and I need a state machine for type.
  form,
  pan: graph.pan,
  unpan: graph.unpan,
  resetpan: graph.resetpan,
  savepan: graph.savepan,
  loadpan: graph.loadpan,
  mask: graph.mask,
  unmask: graph.unmask,
  steal: graph.steal,
  putback: graph.putback,
  skip: graph.skip,
  scroll: graph.scroll,
  spin: graph.spin,
  sort: graph.sort,
  zoom: graph.zoom,
  blur: graph.blur,
  shear: graph.shear,
  noise16: graph.noise16,
  noise16DIGITPAIN: graph.noise16DIGITPAIN,
  noise16Aesthetic: graph.noise16Aesthetic,
  noise16Sotce: graph.noise16Sotce,
  noiseTinted: graph.noiseTinted,
  // glaze: ...
};

// TODO: Eventually restructure this a bit. 2021.12.16.16.0
//       Should global state like color and transform be stored here?

let $activePaintApi;

let paintingAPIid = 0n;

const twoDCommands = [];
graph.twoD(twoDCommands); // Set a global for passing 2d commands here.

class Painting {
  #layers = [];
  #layer = 0;
  api = {};
  inkrn;

  // panrn; // In order for this feature to work, translation needs to be stored outside of graph / captured differently?

  constructor() {
    Object.assign(this.api, $paintApi);
    const p = this;

    p.api.index = paintingAPIid;
    paintingAPIid += 1n;

    p.inkrn = graph.c.slice(); // Init global state machine read-outs.
    p.pagern = graph.getBuffer();
    // p.panrn = graph.getPan();

    // Filter for and then wrap every rendering behavior of $paintApi into a
    // system to be deferred in groups, using layer.
    // ⛓️ This wrapper also makes the paint API chainable.

    function globals(k, args) {
      if (k === "ink") p.inkrn = [...args].flat();
      if (k === "page") p.pagern = args[0];
      // TODO: 😅 Add other state globals like line thickness? 23.1.25
    }

    for (const k in $paintApiUnwrapped) {
      if (typeof $paintApiUnwrapped[k] === "function") {
        // Wrap and then transfer to #api.
        p.api[k] = function () {
          globals(k, arguments); // Keep track of global state, like ink, via `inkrn`.
          // Create layer if necessary.
          if (notArray(p.#layers[p.#layer])) p.#layers[p.#layer] = [];
          // Add each deferred paint api function to the layer, to be run
          // all at once in `paint` on each frame update.
          p.#layers[p.#layer].push([
            k,
            () => {
              globals(k, arguments); // Update globals again on chainable calls.
              $paintApiUnwrapped[k](...arguments);
            },
          ]);
          return p.api;
        };
      }
    }

    // Allows grouping & composing painting order using an AofA (Array of Arrays).
    // n: 0-n (Cannot be negative.)
    // fun: A callback that contains $paintApi commands or any other code.
    this.api.layer = function (n) {
      p.#layer = n;
      // TODO: ❤️‍🔥 Current layer needs to be set on each API state...!
      return p.api;
    };

    // Creates a new pixel buffer with its own layering wrapper / context
    // on top of the base painting API.
    this.api.painting = function painting() {
      const oldActivePaintApi = $activePaintApi;
      const painting = new Painting();
      $activePaintApi = painting.api;
      // Mock out the screen here using the arguments.
      $activePaintApi.screen = {
        width: arguments[0],
        height: arguments[1],
        // pix gets added in the makeBuffer...
      };
      const pix = graph.makeBuffer(...arguments, painting, $activePaintApi);
      $activePaintApi = oldActivePaintApi;
      return pix;
    };

    this.api.pixel = function () {
      return graph.pixel(...arguments);
    };

    this.api.inkrn = () => this.inkrn; // Return current ink color.
    // this.api.panrn = graph.getPan; // Return current panTranslation.
    this.api.pagern = () => this.pagern; // Return current page buffer.

    // This links to abstract, solitary graph functions that do not need
    // to be wrapped or deferred for rendering.
    // TODO: Maybe these functions should be under a graphics algorithms label?
    this.api.abstract = { bresenham: graph.bresenham };
  }

  // Paints every layer.
  //async paint(immediate = false) {
  paint(immediate = false) {
    for (let layer of this.#layers) {
      layer ||= []; // Evaporate empty layers.
      for (const paint of layer) {
        // if (immediate) {
        // console.log("Label:", paint[0]);
        paint[1]();
        // } else {
        // await paint();
        // }
      }
    }
    this.#layers.length = 0;
    this.#layer = 0;
  }
}

const painting = new Painting();

let glazeAfterReframe;

// *** Resolution ***
// Accepts width, height and gap either as numbers or as
// an object with those keys.
//
// Usage: resolution(64);
//        resolution(320, 240);
//        resolution(display); // "display" is a global object whose width
//                                 and height matches the hardware display
//                                 hosting aesthetic.computer.
let lastGap = 8;
$commonApi.resolution = function (width, height = width, gap = 8) {
  if (typeof width === "object") {
    const props = width;
    height = props.height;
    width = props.width || props.height;
    gap = props.gap === 0 ? 0 : props.gap || 8;
  }

  if (typeof width === "number" && typeof height === "number") {
    width = round(width);
    height = round(height);
  }

  // Don't do anything if there is no change and no gap update.
  if (screen.width === width && screen.height === height && gap === lastGap) {
    console.log("🖼 Resolution: No change needed, returning early");
    return;
  }
  
  console.log("🖼 Resolution: Changing from", { oldWidth: screen.width, oldHeight: screen.height }, "to", { width, height });

  lastGap = gap;

  // width = width || currentDisplay.innerWidth;
  // height = height || currentDisplay.innerHeight;

  // TODO: Paint anything that needs to be painted before resizing...
  // TODO: Does this even work right now?
  painting.paint();

  if (width === undefined && height === undefined) {
    // 1. Generate a new width and height.
    width = round(currentDisplay.width / currentDisplay.subdivisions);
    height = round(currentDisplay.height / currentDisplay.subdivisions);
    // Build a reframe request that will be sent to the main thread, mirroring this.
    reframe = {
      width: undefined,
      height: undefined,
      gap,
    };
  } else {
    // 2. Manually set the width and height.
    reframe = { width, height, gap };
  }

  // console.log(
  //   "🖼 Reframe to:",
  //   width,
  //   height,
  //   "from",
  //   screen.width,
  //   screen.height,
  // );

  // 3. Assign the generated or manual width and height.
  const oldScreen = {
    width: screen.width,
    height: screen.height,
    pixels: screen.pixels,
  };

  screen.width = width;
  screen.height = height;

  // Only initialize depth buffer if CPU 3D rendering is actually being used
  if (needsCPU3D) {
    graph.depthBuffer.length = screen.width * screen.height;
    graph.depthBuffer.fill(Number.MAX_VALUE);
    depthBufferInitialized = true;
  } else {
    // Clear depth buffer if CPU 3D is no longer needed to free memory
    if (depthBufferInitialized) {
      graph.depthBuffer.length = 0;
      depthBufferInitialized = false;
    }
  }
  
  // Write buffer is rarely used, keep it minimal
  graph.writeBuffer.length = 0;
  // graph.writeBuffer.fill(Number.MAX_VALUE);

  screen.pixels = new Uint8ClampedArray(screen.width * screen.height * 4);
  
  // Fill screen buffer with background color if available, otherwise use white
  if (backgroundFillColor) {
    // Resolve the color to RGBA values
    let fillColor;
    try {
      // Use graph.findColor to resolve the color to RGBA
      fillColor = graph.findColor(backgroundFillColor);
      // Ensure we have 4 components (RGBA), defaulting alpha to 255 if missing
      if (fillColor.length === 3) {
        fillColor = [fillColor[0], fillColor[1], fillColor[2], 255];
      }
    } catch (e) {
      fillColor = [255, 255, 255, 255];
    }
    
    // Fill the buffer with the resolved color
    for (let i = 0; i < screen.pixels.length; i += 4) {
      screen.pixels[i] = fillColor[0];     // Red
      screen.pixels[i + 1] = fillColor[1]; // Green  
      screen.pixels[i + 2] = fillColor[2]; // Blue
      screen.pixels[i + 3] = fillColor[3]; // Alpha
    }
  } else {
    // Default behavior: fill with white
    screen.pixels.fill(255);
  }

  graph.setBuffer(screen);
  graph.paste({
    painting: oldScreen,
    crop: new geo.Box(0, 0, oldScreen.width, oldScreen.height),
  });
};

// Add new content to the DOM.
// (Requires `send`)
class Content {
  nodes = [];
  #id = 0;

  constructor() {}

  // Make a request to add new content to the DOM.
  add(content) {
    // if (debug) console.log("📃 Adding content:", content);
    this.nodes.push({ id: this.#id });
    this.#id = this.nodes.length - 1;
    send({ type: "content-create", content: { id: this.#id, content } });
    return this.nodes[this.nodes.length - 1];
  }

  remove() {
    send({ type: "content-remove" });
    this.nodes = [];
    this.#id = 0;
  }

  receive({ id, response }) {
    this.nodes[id].response = response;
  }

  //update({ id, msg }) {
  //  send({ type: "content-update", content: { id, msg } });
  //}
}

// 🔈 Sound

// Microphone State (Audio Input)
class Microphone {
  amplitude = 0;
  waveform = [];
  pitch = 0;
  connected = false; // Flips to true on a callback message from `bios`.
  recording = false;
  recordingPromise;
  permission = "";

  // Note: can send `{monitor: true}` in `options` for audio feedback.
  connect(options) {
    send({ type: "microphone", content: options });
    return this;
  }

  disconnect() {
    send({ type: "microphone", content: { detach: true } });
  }

  poll() {
    send({ type: "get-microphone-amplitude" });
    send({ type: "get-microphone-waveform" });
    send({ type: "get-microphone-pitch" });
  }

  // Start recording.
  rec() {
    this.recording = true;
    send({ type: "microphone-record" });
  }

  // Stop recording.
  cut() {
    const prom = new Promise((resolve, reject) => {
      this.recordingPromise = { resolve, reject };
    });
    send({ type: "microphone-cut" });
    this.recording = false;
    return prom;
  }
}

class Speaker {
  waveforms = { left: [], right: [] };
  amplitudes = { left: [], right: [] };

  poll() {
    send({ type: "get-waveforms" });
    send({ type: "get-amplitudes" });
  }
}

let sound,
  // soundClear, // Used by receivedBeat and defined in first frame update.
  soundId = 0n, // Increment each sound / give it an id in the `bios`.
  soundTime; // Used by `$sound.synth` for global timing.

sound = {
  bpm: undefined,
  sounds: [],
  bubbles: [],
  kills: [],
};

const speaker = new Speaker();
const microphone = new Microphone();

// 2. ✔ Loading the disk.
let originalHost;
let firstLoad = true;

let notice, noticeTimer, noticeColor, noticeOpts; // Renders a full-screen notice on piece-load if present.

async function load(
  parsed, // If parsed is not an object, then assume it's source code.
  fromHistory = false,
  alias = false,
  devReload = false,
  loadedCallback,
  forceKidlisp = false, // Force interpretation as kidlisp even without prefix
) {
  let fullUrl, source;
  let params,
    search,
    colon,
    hash,
    path,
    host = originalHost,
    text,
    slug;

  // console.log("🧩 Loading:", parsed, "dev:", devReload);

  if (loading === false) {
    loading = true;
  } else {
    // TODO: If the piece is different, then there should be a way to abort
    //       or ignore a current load.
    console.warn(
      "Coudn't load:",
      parsed.path || parsed.name,
      "(Already loading.)",
    );
    return true;
  }

  // Reload a previously sideloaded piece on subsequent loads.
  if (
    !parsed.source &&
    store["publishable-piece"] &&
    parsed.piece === store["publishable-piece"].slug
  ) {
    parsed.source = store["publishable-piece"].source;
    parsed.name = store["publishable-piece"].slug;
  }

  // 🕸️ Loading over the network from a parsed path object with no source code.
  if (!parsed.source) {
    params = parsed.params;
    path = parsed.path;
    search = parsed.search;
    colon = parsed.colon;
    hash = parsed.hash;
    host = parsed.host;
    slug = parsed.text;

    // 👱 Route to the `profile` piece if we are just hitting an empty
    // username.
    if (slug.startsWith("@") && slug.indexOf("/") === -1) {
      params = [slug, ...params]; // Rewrite all params for `@user` slug urls.
      //slug = "profile"; // Go to `profile` instead of the `@user`.
      const hiddenSlug = "profile";
      // Rewrite path to `profile`.
      console.log("Profile Path:", path);
      path = [...path.split("/").slice(0, -1), hiddenSlug].join("/");
    }

    // if (debug) console.log(debug ? "🟡 Development" : "🟢 Production");
    if (host === "") host = originalHost;
    loadFailure = undefined;
    host = host.replace(/\/$/, ""); // Remove any trailing slash from host.
    
    // 🎯 Auto-detect $prefixed cached codes (e.g., aesthetic.computer/$OrqM)
    // Check both path and slug for $prefixed patterns
    if ((path && path.startsWith("$")) || (slug && slug.startsWith("$"))) {
      const cacheSlug = path && path.startsWith("$") ? path : slug;
      const cacheId = cacheSlug.slice(1); // Remove the $ prefix to get the actual nanoid
      
      // Hit the store-kidlisp endpoint with GET request to retrieve cached source
      try {
        const cacheUrl = location.protocol + "//" + host + "/api/store-kidlisp?code=" + cacheId;
        const response = await fetch(cacheUrl);
        if (!response.ok) {
          throw new Error(`Failed to load cached KidLisp: ${response.status}`);
        }
        const cacheData = await response.json();
        
        if (cacheData.source) {
          // Set up variables to jump to source loading branch
          source = cacheData.source;
          params = parsed.params || [];
          search = parsed.search;
          colon = parsed.colon || [];
          hash = parsed.hash;
          slug = cacheSlug; // Keep the $prefixed version as the slug for URL display
          forceKidlisp = true;
          
          // Store cached KidLisp owner info for HUD attribution
          if (cacheData.user) {
            cachedKidlispOwner = cacheData.user;
          } else {
            cachedKidlispOwner = null;
          }
          
          // Jump directly to source processing - skip URL construction entirely
        } else {
          throw new Error("No source found in cached KidLisp response");
        }
      } catch (error) {
        console.error("❌ Failed to load cached KidLisp via auto-detection:", error);
        loadFailure = error.message;
        loading = false;
        return;
      }
    }
    
    //                                 Note: This fixes a preview bug on teia.art. 2022.04.07.03.00    if (path === "") path = ROOT_PIECE; // Set bare path to what "/" maps to.
    // if (path === firstPiece && params.length === 0) params = firstParams;

    // Only construct fullUrl if we haven't already extracted source from nanoid pattern
    if (!source) {
      // Check if the path already has a .lisp extension and use it directly, otherwise default to .mjs
      if (path.endsWith(".lisp")) {
        fullUrl = location.protocol + "//" + host + "/" + path + "#" + Date.now();
      } else {
        fullUrl =
          location.protocol +
          "//" +
          host +
          "/" +
          path +
          ".mjs" +
          "#" +
          Date.now();
      }
      // The hash `time` parameter busts the cache so that the environment is
      // reset if a disk is re-entered while the system is running.
      // Why a hash? See also: https://github.com/denoland/deno/issues/6946#issuecomment-668230727
      // if (debug) console.log("🕸", fullUrl);
    }
  } else {
    // 📃 Loading with provided source code.
    // This could either be JavaScript or LISP.

    if (
      devReload === true &&
      parsed.codeChannel &&
      parsed.codeChannel !== codeChannel
    ) {
      console.warn(
        "🙅 Not reloading, code channel invalid:",
        codeChannel || "N/A",
      );
      return;
    }
    // console.log("📃 Loading from source:", JSON.stringify(parsed));
    // console.log("📃 Source content to run:", JSON.stringify(parsed.source));
    source = parsed.source;
    params = parsed.params;
    search = parsed.search;
    colon = parsed.colon || [];
    hash = parsed.hash; // tood: these probably don't work? 24.07.09.23.46
    host = parsed.host;
    slug = parsed.name; // not 'text' for this.

    if (slug !== "(...)") path = parsed.path; //"aesthetic.computer/disks/" + slug;
    // 📓 Might need to fill in hash, path, or slug here. 23.06.24.18.49
  }

  let prefetches; // Will be acted on after `hotSwap`.

  // 🅱️ Load the piece.

  // const moduleLoadTime = performance.now();

  let blobUrl, sourceCode, originalCode;
  try {
    // If this is a reload (with no source change) then just create a new
    // blobURL off the old source.
    // TODO: Cache piece code locally / in an intelligent way,
    //       and then receive socket updates when it changes on the server?
    if (
      slug?.split("~")[0] === currentText?.split("~")[0] &&
      sourceCode == currentCode &&
      !devReload
    ) {
      const blob = new Blob([currentCode], { type: "application/javascript" });
      blobUrl = URL.createObjectURL(blob);
      sourceCode = currentCode;
      originalCode = sourceCode;
    } else {
      let sourceToRun;
      if (fullUrl) {
        // Check if we have embedded source for this piece
        if ($commonApi.embeddedSource && $commonApi.embeddedSource.path === path) {
          if (logs.loading) console.log("📦 Using embedded source for:", path);
          sourceToRun = $commonApi.embeddedSource.source;
          // Clear embedded source after use to prevent conflicts
          $commonApi.embeddedSource = null;
        } else {
          let response;
          if (logs.loading) console.log("📥 Loading from url:", fullUrl);
          response = await fetch(fullUrl);
          if (response.status === 404 || response.status === 403) {
            const extension = path.endsWith(".lisp") ? ".lisp" : ".mjs";
            const anonUrl =
              location.protocol +
              "//" +
              "art.aesthetic.computer" +
              "/" +
              path.split("/").pop() +
              extension +
              "#" +
              Date.now();
            if (logs.loading)
              console.log("🧑‍🤝‍🧑 Attempting to load piece from anon url:", anonUrl);
            response = await fetch(anonUrl);
            if (response.status === 404 || response.status === 403)
              throw new Error(response.status);
          }
          sourceToRun = await response.text();
        }
      } else {
        sourceToRun = source;
      }

      // 🔥 Idea
      // One should be able to drag a piece in, then be able to load the piece
      // go back to the prompt, and return to it and it should still load
      // the modded code!      // Then refresh should be able to function as well?
      // ⚠️ Detect if we are running `kidlisp` or JavaScript syntax.
      // Note: This may not be the most reliable way to detect `kidlisp`?
      // 🚗 Needs to know if the source was from a prompt with a lisp module.
      // console.log("🔍 Checking if kidlisp source:", JSON.stringify(sourceToRun));
      if (
        sourceToRun.startsWith("(") ||
        sourceToRun.startsWith(";") ||
        forceKidlisp ||
        slug === "(...)" ||
        path === "(...)" ||
        path.endsWith(".lisp")
      ) {
        // Only use basic detection, not the broader isKidlispSource function
        // which can incorrectly detect JavaScript as kidlisp, unless forceKidlisp is true
        // or this came from parse function as kidlisp (slug/path === "(...)")
        // Assume lisp.
        sourceCode = sourceToRun;
        originalCode = sourceCode;
        // Clear cached owner for non-cached KidLisp
        cachedKidlispOwner = null;
        // Don't cache .lisp files since they're already stored as files
        const isLispFile = path && path.endsWith(".lisp");
        loadedModule = lisp.module(sourceToRun, isLispFile);

        if (devReload) {
          store["publishable-piece"] = {
            slug,
            source: sourceToRun,
            ext: "lisp",
          };
          if (logs.loading)
            console.log("💌 Publishable:", store["publishable-piece"]);
        }
      } else {
        if (devReload) {
          store["publishable-piece"] = { slug, source: sourceToRun };
          if (logs.loading)
            console.log("💌 Publishable:", store["publishable-piece"].slug);
        }

        originalCode = sourceToRun;
        const updatedCode = updateCode(sourceToRun, host, debug);

        prefetches = updatedCode
          .match(/"(@\w[\w.]*\/[^"]*)"/g)
          ?.map((match) => match.slice(1, -1)); // for "@name/code".

        const blob = new Blob([updatedCode], {
          type: "application/javascript",
        });

        blobUrl = URL.createObjectURL(blob);
        sourceCode = updatedCode;
        loadedModule = await import(blobUrl);
      }
    }
  } catch (err) {
    console.log("🟡 Error loading mjs module:", err);
    // Look for lisp files if the mjs file is not found, but only if we weren't already trying to load a .lisp file
    if (fullUrl && !fullUrl.includes(".lisp")) {
      try {
        fullUrl = fullUrl.replace(".mjs", ".lisp");
        let response;
        if (logs.loading) console.log("📥 Loading lisp from url:", fullUrl);
        response = await fetch(fullUrl);
        console.log("🤖 Response:", response);

        if (response.status === 404 || response.status === 403) {
          const anonUrl =
            location.protocol +
            "//" +
            "art.aesthetic.computer" +
            "/" +
            (path ? path.split("/").pop() : "unknown") +
            ".lisp" +
            "#" +
            Date.now();
          console.log("🧑‍🤝‍🧑 Attempting to load piece from anon url:", anonUrl);
          response = await fetch(anonUrl);

          console.log("🚏 Response:", response);

          if (response.status === 404 || response.status === 403)
            throw new Error(response.status);
        }
        sourceCode = await response.text();
        // console.log("📓 Source:", sourceCode);
        originalCode = sourceCode;
        // Clear cached owner for file-loaded KidLisp
        cachedKidlispOwner = null;
        // Don't cache .lisp files since they're already stored as files  
        const isLispFile = (path && path.endsWith(".lisp")) || (fullUrl && fullUrl.includes(".lisp"));
        loadedModule = lisp.module(sourceCode, isLispFile);
        if (devReload) {
          store["publishable-piece"] = {
            slug,
            source: sourceCode,
            ext: "lisp",
          };
          // console.log("💌 Publishable:", store["publishable-piece"]);
        }
      } catch (err) {
        // 🧨 Continue with current module if one has already loaded.
        console.error(
          `😡 "${path}" load failure:`,
          err,
          "💾 First load:",
          firstLoad,
        );
        loadFailure = err;
        $commonApi.net.loadFailureText = err.message + "\n" + sourceCode;
        loading = false;

        // Reset labelBack and leaving when piece load fails completely to ensure proper navigation
        labelBack = false;
        // Clear the labelBack from store when load fails
        delete store["aesthetic-labelBack"];
        leaving = false;

        // Only return a 404 if the error type is correct.
        if (firstLoad && (err.message === "404" || err.message === "403")) {
          $commonApi.jump(`404~${slug}`);
        } else {
          $commonApi.notice(":(", ["red", "yellow"]);
        }
        return false;
      }
    } else {
      // If we were already trying to load a .lisp file and it failed, just propagate the error
      console.error(
        `😡 "${path}" load failure:`,
        err,
        "💾 First load:",
        firstLoad,
      );
      loadFailure = err;
      $commonApi.net.loadFailureText = err.message + "\n" + sourceCode;
      loading = false;

      // Reset labelBack and leaving when piece load fails completely to ensure proper navigation
      labelBack = false;
      // Clear the labelBack from store when load fails
      delete store["aesthetic-labelBack"];
      leaving = false;

      // Only return a 404 if the error type is correct.
      if (firstLoad && (err.message === "404" || err.message === "403")) {
        $commonApi.jump(`404~${slug}`);
      } else {
        $commonApi.notice(":(", ["red", "yellow"]);
      }
      return false;
    }
  }

  // console.log("Module load time:", performance.now() - moduleLoadTime, module);
  // 🧨 Fail out if no module is found.
  if (loadedModule === undefined) {
    loading = false;
    leaving = false;
    return false;
  }

  // 🧩 Piece code has been loaded...
  //    Now we can instantiate the piece.

  pieceHistoryIndex += fromHistory === true ? 0 : 1; // Adjust the history.

  if (!debug && !firstLoad) {
    // console.clear();
    // headers(); // Headers already printed during boot - no need to print again
  }

  // console.log("🧩", path, "🌐", host);

  $commonApi.net.devReload = devReload; // Expose to the piece if it was
  // reloaded by the developer, for special logic per piece.

  // Add debug to the common api.
  $commonApi.debug = debug;

  // Add reload to the common api.
  $commonApi.reload = function reload({
    piece,
    name,
    source,
    codeChannel,
  } = {}) {
    // console.log("⚠️ Reloading:", piece, name, source);

    if (loading) {
      console.log("🟡 A piece is already loading.");
      return;
    }
    if (piece === "*refresh*") {
      //  padding: 8px; border-radius: 2px;console.log("💥️ Restarting system...");
      send({ type: "refresh" }); // Refresh the browser.
    } else if (piece === "*piece-reload*") {
      // console.log("🎨 Reloading current piece...");
      // Reload the current piece without refreshing the entire page
      $commonApi.load(
        {
          path: currentPath,
          host: currentHost,
          search: currentSearch,
          colon: currentColon,
          params: currentParams,
          hash: currentHash,
          text: currentText,
        },
        true, // fromHistory - don't add to history stack
        alias,
        true, // devReload
      );
    } else if (name && source) {
      // TODO: Check for existence of `name` and `source` is hacky. 23.06.24.19.27
      // TODO: 🔥 This should somehow keep current commands or params, etc.

      // console.log(
      //   "🪷 Current: params:",
      //   currentParams,
      //   "text:",
      //   currentText,
      //   "path:",
      //   currentPath,
      // );

      // Note: This is used for live development via the socket server.
      $commonApi.load({ source, name, codeChannel }, false, false, true); // Load source code.
    } /*if (piece === "*" || piece === undefined /*|| currentText === piece*/ /*) {*/ else {
      // console.log("💾️ Reloading:", piece, "Params:", currentParams);
      // $commonApi.pieceCount = -1; // Reset pieceCount on developer reload.
      //                             (This can be disabled while testing pieces
      //                              that rely on pieceCount increments)
      // ❤️‍🔥 TODO: Reloading the same piece should not effect piece count.

      $commonApi.load(
        {
          path: currentPath,
          host: currentHost,
          search: currentSearch,
          colon: currentColon,
          params: currentParams,
          hash: currentHash,
          text: currentText,
        },
        // Use the existing contextual values when live-reloading in debug mode.
        true, // (fromHistory) ... never add any reload to the history stack
        alias,
        true, // devReload
      );
    } //else if (piece !== undefined) {
    //console.log("Reloading pieceeeeeeee:", piece, parse(piece));
    //$commonApi.load(parse(piece), false, false, true);
    //}
  };

  // Start the socket server
  // TODO: Before we load the disk, in case of needing to reload remotely on failure? 23.01.27.12.48
  let receiver; // Handles incoming messages from the socket.
  const forceProd = false; // For testing prod socket servers in development.
  // TOOD: Hoist this to an environment variable?

  // Requests a session-backend and connects via websockets.
  function startSocket() {
    if (
      //parsed.search?.startsWith("preview") ||
      //parsed.search?.startsWith("icon")
      previewOrIconMode
    ) {
      console.log("🧦 Sockets disabled, just grabbing screenshots. 😃");
      return;
    }
    // Never open socket server in icon / preview mode.
    // if (debug && logs.session) console.log("🫂 Finding session server...");
    socket = new Socket(debug, send); // Then redefine and make a new socket.

    const monolith = undefined; // "monolith"; // or `undefined` for horizontal scaling via
    // jamstack

    session(slug, forceProd, monolith)
      .then((sesh) => {
        if (typeof sesh === "string") throw new Error(sesh); // Cancel if error.
        const url = new URL(sesh.url); // Parse the url.
        const udpUrl = new URL(sesh.udp); // Parse the udp url.

        // console.log("Session URL:", url);

        // 🩰 UDP... (via `bios`)
        send({
          type: "udp:connect",
          content: {
            url: `https://${udpUrl.hostname}`,
            port: udpUrl.port, //debug && !forceProd ? 8889 : 443,
          },
        });

        let slugBroadcastInterval;

        // 🕸️ Web Sockets
        socket?.connect(
          url.host + url.pathname,
          (id, type, content) => {
            // Globally receivable messages...
            // (There are also some messages handled in `Socket`)
            // 😱 Scream at everyone who is connected!
            if (type === "scream" && socket?.id !== id) {
              console.log("😱 Scream:", content, "❗");
              scream = content;
            }
            // 🧩 Pieces get all other messages not caught in `Socket`.
            receiver?.(id, type, content); // Run the piece receiver.
          },
          $commonApi.reload,
          "wss",
          () => {
            // 🔩 Connected! (Post-connection logic.)
            // if (USER) socket?.send("login", { user: USER });

            // Broadcast current location.
            if (HANDLE) {
              // console.log("🗼 Broadcasting slug:", currentText, "for:", HANDLE);
              socket?.send("location:broadcast", {
                handle: HANDLE,
                slug: currentText,
              });
              slugBroadcastInterval = setInterval(() => {
                socket?.send("location:broadcast", {
                  handle: HANDLE,
                  slug: "*keep-alive*",
                });
              }, 2500);
            }

            // Subscribe to code-channel as needed.
            if (codeChannel) socket?.send("code-channel:sub", codeChannel);

            updateHUDStatus();
            $commonApi.needsPaint();
            codeChannelAutoLoader?.();
            // setTimeout(function () {
            //   currentHUDStatusColor = undefined;
            // }, 250);
          },
          () => {
            // 💔 Disconnected! (Post-disconnection logic.)
            updateHUDStatus();
            clearInterval(slugBroadcastInterval);
          },
        );
      })
      .catch((err) => {
        console.error("Session connection error:", err);
      });
  }

  // End the socket connection before switching pieces if one exists.
  // socket?.kill();
  // udp?.kill();
  // socket = undefined;

  // Delay session server by .75 seconds in order to prevent redundant
  //  connections being opened as pieces are quickly re-routing and jumping.
  clearTimeout(socketStartDelay);
  socket?.kill(); // Kill any already open socket from a previous disk.
  udp?.kill();
  socket = undefined;
  socketStartDelay = setTimeout(() => startSocket(), 250);

  $commonApi.net.socket = function (receive) {
    receiver = receive || (() => {});
    if (!socket) {
      // Just in case we init. in a `boot` before the timeout fires above.
      clearTimeout(socketStartDelay);
      startSocket();
    } else {
      // Return the server then send an already connected message.
      setTimeout(() => {
        if (socket?.id) receiver(socket.id, "connected:already");
      }, 10);
    }
    return socket;
  };

  // ***Client Metadata Fields***
  // Set default metadata fields for SEO and sharing,
  // (requires serverside prerendering, also via `index.js`).
  let meta;

  if (alias === false) {
    // Parse any special piece metadata.
    const { title, desc, ogImage, twitterImage, icon } = metadata(
      location.host, // "aesthetic.computer",
      slug,
      loadedModule.meta?.({
        ...parsed,
        num: $commonApi.num,
        store: $commonApi.store,
      }) || inferTitleDesc(originalCode),
    );

    meta = {
      title,
      desc, // Note: This doesn't auto-update externally hosted module descriptions, and may never need to? 22.07.19.06.00
      img: {
        og: ogImage,
        twitter: twitterImage,
        icon,
      },
      url: "https://aesthetic.computer/" + slug,
    };
  }

  // Add meta to the common api so the data can be overridden as needed.
  $commonApi.meta = (data) => send({ type: "meta", content: data });

  // Rewrite a new URL / parameter path without affecting the history.
  $commonApi.net.rewrite = (path, historical = false) => {
    if (historical) $commonApi.history.push(path);
    send({ type: "rewrite-url-path", content: { path, historical } }); // Jump the browser to a new url.
  };

  // Add host to the networking api.
  $commonApi.net.host = host;

  // Jump the browser to a new url.
  $commonApi.net.web = (url, jumpOut) => {
    send({ type: "web", content: { url, blank: jumpOut } });
  };

  $commonApi.net.refresh = () => {
    send({ type: "refresh" });
  };

  // Tell the system to wait until preloading is finished before painting.
  $commonApi.net.waitForPreload = () => {
    send({ type: "wait-for-preload", content: true });
  };

  // Tell the system that all preloading is done.
  $commonApi.net.preloaded = () => {
    send({ type: "preload-ready", content: true });
  };

  $commonApi.content = new Content();

  $commonApi.dom = {};

  $commonApi.dom.clear = () => {
    $commonApi.content.remove();
  };

  $commonApi.dom.html = (strings, ...vars) => {
    const processed = defaultTemplateStringProcessor(strings, ...vars);
    $commonApi.content.add(processed);
  };

  $commonApi.dom.css = (strings, ...vars) => {
    const processed = defaultTemplateStringProcessor(strings, ...vars);
    $commonApi.content.add(`<style>${processed}</style>`);
  };

  $commonApi.dom.javascript = (strings, ...vars) => {
    const processed = defaultTemplateStringProcessor(strings, ...vars);
    $commonApi.content.add(`<script>${processed}</script>`);
  };
  // 💾 Uploading + Downloading
  // Add download event to trigger a file download from the main thread.
  $commonApi.download = (filename, data, modifiers) => {
    send({ type: "download", content: { filename, data, modifiers } });
  };

  // * Preload *
  // Add preload to the boot api.
  // Accepts paths local to the original disk server, full urls, and demos.
  // Usage:   preload("demo:drawings/2021.12.12.17.28.16.json") // pre-included
  //          preload("https://myserver.com/test.json") // remote
  //          preload("drawings/default.json") // hosted with disk
  // Results: preload().then((r) => ...).catch((e) => ...) // via promise

  $commonApi.net.preload = async function (
    path,
    parseJSON = true,
    progressReport,
    options = {},
  ) {
    let extension;

    const rejection = (reject) => {
      reject(new DOMException("Aborted", "AbortError"));
    };

    if (soundWhitelist.includes(path)) {
      extension = "ogg";
    } else {
      if (typeof path === "object") {
        extension = path.extension;
        path = path.path;
      } else {
        extension = path.split(".").pop().split("?")[0];
      }

      if (path.indexOf("/") === 0) path = path.slice(1);

      try {
        const url = new URL(path);
        if (url.protocol === "demo:") {
          path = `/demo/${url.pathname}`;
        } else if (url.protocol !== "https:") {
        }
      } catch {
        path = `${location.protocol}//${location.host}/${path}`;
      }
    }

    if (extension === "xml" || extension === undefined) {
      return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();

        options.signal?.addEventListener("abort", () => {
          xhr.abort();
          rejection(reject);
        });

        xhr.open("GET", path, true);
        xhr.onprogress = function (event) {
          const progress = min(event.loaded / event.total, 1);
          if (debug && logs.download) {
            console.log(`💈 XML Download: ${progress * 100}%`);
          }
          progressReport?.(progress);
        };
        xhr.onload = function () {
          if (xhr.status === 200 || xhr.status === 304) {
            resolve(xhr.responseXML || xhr.responseText);
          } else {
            reject(xhr.status);
          }
        };
        xhr.onerror = reject;
        xhr.send();
      });
    } else if (extension === "json") {
      return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();

        options.signal?.addEventListener("abort", () => {
          xhr.abort();
          rejection(reject);
        });

        xhr.open("GET", path, true);
        xhr.onprogress = function (event) {
          const progress = min(event.loaded / event.total, 1);
          if (debug && logs.download) {
            console.log(`💈 JSON Download: ${progress * 100}%`);
          }
          progressReport?.(progress);
        };
        xhr.onload = function () {
          if (xhr.status === 200 || xhr.status === 304) {
            resolve(parseJSON ? JSON.parse(xhr.response) : xhr.response);
          } else {
            reject(xhr.status);
          }
        };
        xhr.onerror = reject;
        xhr.send();
      });
    } else if (
      extension === "webp" ||
      extension === "jpg" ||
      extension === "gif" ||
      extension === "jpeg" ||
      extension === "png"
    ) {
      return new Promise((resolve, reject) => {
        if (options.signal?.aborted) {
          rejection(reject);
          return;
        }

        send({ type: "load-bitmap", content: path });
        preloadPromises[path] = { resolve, reject };

        options.signal?.addEventListener("abort", () => {
          send({ type: "load:abort", content: path });
          rejection(reject);
        });
      });
    } else if (
      extension === "m4a" ||
      extension === "ogg" ||
      extension === "webm" ||
      extension === "wav" ||
      extension === "mp3"
    ) {
      return new Promise((resolve, reject) => {
        if (options.signal?.aborted) {
          rejection(reject);
          return;
        }

        send({ type: "sfx:load", content: path });
        preloadPromises[path] = { resolve, reject };

        options.signal?.addEventListener("abort", () => {
          rejection(reject);
        });
      });
    } else if (extension === "zip") {
      return new Promise((resolve, reject) => {
        if (options.signal?.aborted) {
          rejection(reject);
          return;
        }

        send({ type: "zip:load", content: path });
        preloadPromises[path] = { resolve, reject };

        options.signal?.addEventListener("abort", () => {
          rejection(reject);
        });
      });
    }
  };

  $commonApi.slug = slug;
  $commonApi.piece = slug?.split("~")[0].split(":")[0];
  $commonApi.query = Object.fromEntries(new URLSearchParams(search));
  $commonApi.params = params || [];
  $commonApi.colon = colon || [];

  // Initialize duration tracking from query parameters
  if ($commonApi.query.duration) {
    const duration = parseFloat($commonApi.query.duration);
    if (!isNaN(duration) && duration > 0) {
      durationTotal = duration;
      durationStartTime = pageLoadTime; // Start from page load time immediately
      durationProgress = 0;
      durationCompleted = false;
      durationBlinkState = false;
      console.log("⏱️ Duration parameter detected:", duration, "seconds - starting from page load time:", pageLoadTime);
    }
  }

  $commonApi.load = async function () {
    // Load a piece, wrapping it in a leave function so a final frame
    // plays back.
    leaving = true;

    return new Promise((resolve) => {
      leaveLoad = async () => {
        const loaded = await load(...arguments);
        resolve(loaded); // Resolve with `true` or `false`.
      };
    });
  };

  // 💡 Eventually this could merge with net.web so there is one command
  //    to either go to a piece within the system if one loads... or an entirely
  //    different url somehow! 23.02.07.21.21

  $commonApi.alias = function alias(name, colon, params) {
    $commonApi.jump(
      name +
        colon.map((c) => `:` + c).join("") +
        params.map((p) => `~` + p).join(""),
      true,
      true,
    );
  };

  // Go back to the previous piece, or to the prompt if there is no history.
  $commonApi.back = () => {
    if (pieceHistoryIndex > 0) {
      send({
        type: "back-to-piece",
        content: {
          targetPiece: $commonApi.history[$commonApi.history.length - 1],
        },
      });
    } else {
      $commonApi.jump("prompt");
    }
  };

  if (!alias) $commonApi.pieceCount += 1; // Don't bump pieceCount if aliased.

  // Load typeface if it hasn't been yet.
  // (This only has to happen when the first piece loads.)
  if (!tf) {
    tf = await new Typeface().load($commonApi.net.preload);
  } else {
    // 🔤 Reset typeface to default font for each piece load
    if (tf.name !== "font_1") {
      tf = await new Typeface("font_1").load($commonApi.net.preload);
    }
  }
  $commonApi.typeface = tf; // Expose a preloaded typeface globally.
  ui.setTypeface(tf); // Set the default `ui` typeface.

  // Add API to allow pieces to switch typefaces
  $commonApi.setTypeface = async function (typefaceName) {
    if (typefaceName && typefaceName !== tf.name) {
      tf = await new Typeface(typefaceName).load($commonApi.net.preload);
      $commonApi.typeface = tf; // Update the global reference
      ui.setTypeface(tf); // Update UI typeface
    }
  };

  // Initialize MatrixChunky8 font for QR code text rendering
  if (!typefaceCache.has("MatrixChunky8")) {
    const matrixFont = new Typeface("MatrixChunky8");
    await matrixFont.load($commonApi.net.preload); // Important: call load() to initialize the proxy system
    typefaceCache.set("MatrixChunky8", matrixFont);
  }

  /**
   * @function video
   * @descrption Make a live video feed. Returns an object that links to current frame.
   * @param {string} type - "camera" or "camera-update" or see below. 💡
   * @param {object} options - *unimplemented* { src, width, height }
   */

  let videoTimeout;

  $commonApi.video = function (type, options) {
    // TODO: ❤️‍🔥 Prevent fast multiple taps while camera is updating...

    // TODO: Options could eventually be { src, width, height }
    // const vid = video("youtube-link");
    // const vid = video("tiktok:@whistlegraph");
    // https://codepen.io/oceangermanique/pen/LqaPgO

    if (videoSwitching === false) {
      if (type === "camera:update") {
        lastActiveVideo = activeVideo || lastActiveVideo;
        activeVideo = null;
      }

      clearTimeout(videoTimeout);
      videoTimeout = setTimeout(() => {
        send({ type: "video", content: { type, options } });
      }, 50);

      if (type === "camera:update") videoSwitching = true;
    }

    // Return an object that can grab whatever the most recent frame of
    // video was.
    return videoFrame;
  };

  function videoFrame(shader) {
    if (activeVideo) {
      const { width, pixels } = activeVideo;

      if (shader) {
        for (let i = 0; i < pixels.length; i += 4) {
          const c = pixels.subarray(i, i + 4);
          const p = { x: (i / 4) % width, y: floor(i / 4 / width) };
          shader(p, c);
        }
      }
    } else if (lastActiveVideo) {
      // Make it all red...
      const { pixels } = lastActiveVideo;
      for (let i = 0; i < pixels.length; i += 4) {
        pixels[i] = 255;
        pixels[i + 1] = 0;
        pixels[i + 2] = 0;
        pixels[i + 3] = 255;
      }
    }
    return activeVideo || lastActiveVideo;
  }

  // This function actually hotSwaps out the piece via a callback from `bios` once fully loaded via the `loading-complete` message.
  hotSwap = () => {
    loadedCallback?.(); // Run the optional load callback. (See also: `jump`)

    $commonApi.rec.loadCallback?.(); // Start any queued tape.
    $commonApi.rec.loadCallback = null;

    module = loadedModule;

    // Reset 3D rendering tracking when a new piece loads
    needs3DRendering = false;
    needsCPU3D = false;

    if (!module.system?.startsWith("world"))
      $commonApi.system.world.teleported = false;

    // 📚 nopaint system
    if (
      module.system?.startsWith("nopaint") ||
      typeof module?.brush === "function" ||
      typeof module?.filter === "function"
    ) {
      // If there is no painting is in ram, then grab it from the local store,
      // or generate one.
      const modsys = module.system || "nopaint";

      // TODO: 🖌️💬 Integrate nopaint system with chat.
      // console.log(chat);

      const chatEnabled = false;

      $commonApi.system.nopaint.bakeOnLeave =
        modsys.split(":")[1] === "bake-on-leave"; // The default is to `bake` at the end of each gesture aka `bake-on-lift`.

      boot = ($) => {
        const booter = module.boot || nopaint_boot;
        booter($);
        if (chatEnabled) chat.boot($);
      };

      sim = module.sim || defaults.sim;
      paint = ($) => {
        if (module.paint) {
          const painted = module.paint($);
          $.system.nopaint.needsPresent = true;

          // TODO: Pass in extra arguments here that flag the wipe.
          if (chatEnabled) chat.paint($, { embedded: true }); // Render any chat interface necessary.

          return painted;
        }
      };
      beat = module.beat || defaults.beat;
      brush = module.brush;
      filter = module.filter;
      act = ($) => {
        nopaint_act($); // Inherit base functionality.
        if (module.act) {
          return module.act($);
        } else {
          return defaults.act($);
        }
      };
      leave = ($) => {
        module.leave?.($); // Run the custom leave.
        nopaint.leave($); // And the inherited default leave from nopaint.
      };
      bake = module.bake || nopaint.bake;
      system = "nopaint";
    } else if (module.system?.startsWith("prompt")) {
      // 📖 prompt system
      // Default wrap to "word" if using `prompt:character`.
      const wrap =
        module.wrap ||
        (module.system.indexOf("character") > -1 ? "word" : undefined);

      if (module.scheme) {
        if (
          module.scheme.dark === undefined &&
          module.scheme.light === undefined
        ) {
          const defaultScheme = { ...module.scheme };
          module.scheme.dark = module.scheme.light = defaultScheme;
          keys(module.scheme).forEach((key) => {
            if (key !== "dark" && key !== "light") delete module.scheme[key];
          });
        }
      }

      boot = async ($) => {
        await prompt.prompt_boot(
          $,
          {
            prompt: module.prompt,
            program: {
              before: module.before,
              after: module.after,
            },
            hint: module.system.split(":").slice(1).join(":"), // See `ask.ts`.
            forgetful: module.forgetful || false,
            memory: module.memory || Infinity,
            gutterMax: module.gutterMax,
            lineSpacing: module.lineSpacing,
          },
          module.reply,
          module.halt,
          module.scheme,
          wrap,
          module.copied,
          module.activated,
        );
        await module.boot?.($);
      };

      sim = ($) => {
        module.sim?.($);
        prompt.prompt_sim($);
      };

      paint = ($) => {
        let noPaint = module.paint?.($); // Carry the return.

        // Paint an illustration if it exists and `paint` is not defined.
        if (!module.paint && module.illustration) {
          $.wipe(...module.scheme.dark.background.slice(0, 3));
          $.stamp(module.illustration, { center: "x", bottom: 0 });
          noPaint = true;
        }

        const promptPainted = prompt.prompt_paint($);
        return noPaint || promptPainted;
      };

      beat = module.beat || defaults.beat;

      act = ($) => {
        module.act?.($);
        prompt.prompt_act($);
      };

      leave = ($) => {
        module.leave?.($);
        prompt.prompt_leave($);
      };

      system = "prompt";
    } else if (module.system?.startsWith("world")) {
      // 🗺️ world system
      boot = async ($) => {
        await world.world_boot($, module.world);
        await module.boot?.($);
      };

      sim = ($) => {
        world.world_sim($);
        module.sim?.($);
      };

      paint = ($) => {
        if (!world.coversScreen($.screen)) module.background?.($);
        world.world_paint($, module.paint, module.curtain);
      };

      beat = module.beat || defaults.beat;

      act = ($) => {
        world.world_act($);
        module.act?.($);
      };

      leave = ($) => {
        world.world_leave($);
        module.leave?.($);
      };

      system = "world";
    } else if (module.system?.startsWith("fps")) {
      // 🧊 fps system
      let doll;

      boot = ($) => {
        doll = new CamDoll($.Camera, $.Dolly, {
          fov: 80,
          z: 0,
          y: 0,
          sensitivity: 0.002,
        });
        $commonApi.system.fps = { doll };
        module?.boot?.($);
      };

      sim = ($) => {
        doll?.sim();
        module?.sim?.($);
      };

      act = ($) => {
        doll?.act($.event);
        module?.act?.($);
      };

      paint = module.paint || defaults.paint;
      leave = module.leave || defaults.leave;

      system = "fps";
    } else if (module.system?.startsWith("game")) {
      // 🎲 🎮 game system
      console.log("game!");
      // TODO: ⚠️ Make game template. 25.06.05.09.19
    } else {
      // 🧩 piece
      boot = module.boot || defaults.boot;
      sim = module.sim || defaults.sim;
      paint = module.paint || defaults.paint;
      beat = module.beat || defaults.beat;
      act = module.act || defaults.act;
      leave = module.leave || defaults.leave;
      system = module.system || null;

      // delete $commonApi.system.name; // No system in use.
    }

    preview = module.preview || defaults.preview; // Set preview method.
    icon = module.icon || defaults.icon; // Set preview method.

    // ♻️ Reset global state for this piece.
    paintCount = 0n;
    paintingAPIid = 0n;
    simCount = 0n;
    booted = false;
    
    // Reset tape playback state to prevent stale state across piece transitions
    $commonApi.rec.playing = false;
    $commonApi.rec.presenting = false;
    
    // initialSim = true;
    activeVideo = null;
    videoSwitching = false;
    lastActiveVideo = null;
    keys(preloadPromises).forEach((key) => preloadPromises[key].reject(key));
    preloadPromises = {};
    noPaint = false;
    formsSent = {}; // Clear 3D list for GPU.
    currentPath = path;
    currentHost = host;
    currentSearch = search;
    // console.log("Set currentSearch to:", search);
    firstPreviewOrIcon = true;
    hideLabel = parsed.search?.includes("nolabel") || false;
    currentColon = colon;
    currentParams = params;
    currentHash = hash;
    // sound = null;
    glazeEnabled = null;
    // soundClear = null;
    hourGlasses.length = 0;
    // labelBack = false; // Now resets after a jump label push. 25.03.22.21.36

    // Restore labelBack state from store if it exists
    if (store["aesthetic-labelBack"] === "true") {
      labelBack = true;
      // Only clear labelBack if we're loading the target piece from browser back navigation
      // AND we're not loading a kidlisp piece (which gets reloaded during back navigation)
      const isKidlispPiece =
        (slug && lisp.isKidlispSource(slug)) || slug === "(...)";

      if (fromHistory && labelBack && !isKidlispPiece) {
        // Clear labelBack since we've successfully navigated back to a non-kidlisp piece
        labelBack = false;
        delete store["aesthetic-labelBack"];
        send({ type: "labelBack:clear", content: false });
      }
    }

    // Reset pan translation state for new piece
    graph.unpan();

    // Reset pan translation state for new piece
    graph.unpan();

    // Also reset pan state in a delayed manner to handle any timing issues
    setTimeout(() => {
      graph.unpan();
    }, 0);

    previewMode = parsed.search?.startsWith("preview") || false;
    iconMode = parsed.search?.startsWith("icon") || false;

    // console.log("🔴 PREVIEW OR ICON:", PREVIEW_OR_ICON, "Preview mode:", previewMode, "Icon mode:", iconMode);
    // console.log("📑 Search:", parsed.search);
    // console.log("🖼️ ICON MODE:", iconMode);

    previewOrIconMode = previewMode || iconMode;
    paintings = {}; // Reset painting cache.
    prefetches?.forEach((p) => prefetchPicture(p)); // Prefetch parsed media.
    graph.color2(null); // Remove any secondary color that was added from another piece.

    //  Reset turtle state.
    turtleAngle = 270;
    turtleDown = false;
    turtlePosition = { x: screen.width / 2, y: screen.height / 2 };

    //$api.fps = function (newFps) {
    send({ type: "fps-change", content: undefined });
    //};

    // 🪧 See if notice needs to be shown.
    if ($commonApi.query.notice === "success") {
      notice = "PRINTED!";
      noticeColor = ["white", "green"];
      noticeBell(cachedAPI);
    } else if ($commonApi.query.notice === "cancel") {
      notice = "CANCELLED";
      noticeColor = ["yellow", "red"];
      noticeBell(cachedAPI, { tone: 300 });
    } else if ($commonApi.query.notice === "email-verified") {
      notice = "Email verified!";
      noticeColor = ["white", "blue"];
      noticeBell(cachedAPI, { tone: 300 });
    } else if ($commonApi.query.notice?.length > 0) {
      notice = $commonApi.query.notice;
      noticeColor = ["white", "green"];
      noticeBell(cachedAPI, { tone: 300 });
    }

    if (!alias) {
      // Convert tildes to spaces for display in the corner label
      const displaySlug = slug.replace(/~/g, " ");
      currentHUDTxt = displaySlug; // Update hud if this is not an alias.
      currentHUDLogicalTxt = displaySlug; // Set logical text to the same space-separated version
      
      // Special handling for cached KidLisp: show $prefixed code in URL but full source in HUD label
      if (displaySlug.startsWith("$") && sourceCode && forceKidlisp) {
        currentHUDLogicalTxt = sourceCode; // Show full KidLisp source in HUD label
        // console.log("🎯 Cached KidLisp: URL shows $prefixed code, HUD shows full source");
      }
    }
    if (module.nohud || system === "prompt" || parsed.search?.includes("nohud")) {
      currentHUDTxt = undefined;
      currentHUDLogicalTxt = undefined;
    }
    currentHUDOffset = undefined; // Always reset these to the defaults.
    currentHUDTextColor = undefined;
    currentHUDStatusColor = "red"; //undefined;
    currentHUDButton = undefined;
    currentHUDScrub = 0;
    // currentPromptButton = undefined;

    // Push last piece to a history list, skipping prompt and repeats.
    // Also skip kidlisp pieces when navigating back to the prompt.
    // Add to history unless:
    // 1. Coming from history navigation (fromHistory=true)
    // 2. No current text or current text is "prompt"
    // 3. Current text is already the last item in history
    // 4. Current text is a kidlisp piece and we're navigating to prompt (backspace scenario)
    // 5. We're loading prompt with kidlisp parameters (backspace navigation)
    const isKidlispCurrent = currentText && lisp.isKidlispSource(currentText);
    const isPromptWithKidlisp =
      slug.startsWith("prompt~") && slug.includes("(");
    const isKidlispTarget = slug && lisp.isKidlispSource(slug);

    // Special case: Always add chat to history when navigating to kidlisp
    const shouldAddChatToHistory = currentText === "chat" && isKidlispTarget;

    const shouldAddToHistory =
      !fromHistory &&
      currentText &&
      currentText !== "prompt" &&
      currentText !== $commonApi.history[$commonApi.history.length - 1] &&
      (!isKidlispCurrent || shouldAddChatToHistory) &&
      !isPromptWithKidlisp;

    if (shouldAddToHistory) {
      $commonApi.history.push(currentText);
    }

    currentText = slug; // Keep original slug with tildes for URL navigation
    currentCode = sourceCode;

    if (screen) screen.created = true; // Reset screen to created if it exists.

    cursorCode = "precise"; // Set default cursor.

    if (firstLoad === true) {
      firstLoad = false;
      // firstPiece = path;
      // firstParams = params;
      // firstSearch = search;
    }
  };

  const loadedContent = {
    path,
    host,
    search,
    params,
    hash,
    text: slug,
    pieceCount: $commonApi.pieceCount,
    pieceHasSound: true, // TODO: Make this an export flag for pieces that don't want to enable the sound engine. 23.07.01.16.40
    // 📓 Could also disable the sound engine if the flag is false on a subsequent piece, but that would never really make practical sense?
    fromHistory,
    alias,
    meta,
    taping: $commonApi.rec.loadCallback !== null || $commonApi.rec.recording, // 🎏 Hacky flag. 23.09.17.05.09
    // noBeat: beat === defaults.beat,
  };

  send({
    type: "disk-loaded",
    content: loadedContent,
  });

  return true; // Loaded successfully.
}

const isWorker = typeof importScripts === "function";

// ***Bootstrap***
// Start by responding to a load message, then change
// the message response to makeFrame.
if (isWorker) {
  onmessage = makeFrame;
} else {
  noWorker.onMessage = (d) => makeFrame({ data: d });
}

// The main messaging function to comumunicate back with the main thread.
function send(data, shared = []) {
  if (isWorker) {
    if (shared[0] === undefined) shared = [];
    postMessage(data, shared);
  } else {
    noWorker.postMessage({ data });
  }
}

// Used to subscribe to live coding / development reloads.
let codeChannel, codeChannelAutoLoader;

// 4. ✔ Respond to incoming messages, and probably produce a frame.
// Boot procedure:
// First `paint` happens after `boot`, then any `act` and `sim`s each frame
// before `paint`ing occurs. One `sim` always happens after `boot` and before
// any `act`. `paint` can return false to stop drawing every display frame,
// then, it must be manually restarted via `needsPaint();`).  2022.01.19.01.08
// 🔥
// TODO: makeFrame is no longer a great name for this function, which actually
//       receives every message from the main thread, one of which renders a
//       frame.
// TODO: Make simple needsPaint example.
// TODO: Try to remove as many API calls from here as possible.

async function makeFrame({ data: { type, content } }) {
  // Runs once on boot.
  if (type === "init-from-bios") {
    debug = content.debug;
    setDebug(content.debug);
    ROOT_PIECE = content.rootPiece;

    USER = content.user;
    // if (USER) socket.send("login", { user: USER });

    LAN_HOST = content.lanHost;
    SHARE_SUPPORTED = content.shareSupported;
    PREVIEW_OR_ICON = content.previewOrIcon;
    VSCODE = content.vscode;

    // Store embedded source for optimization
    $commonApi.embeddedSource = content.embeddedSource;

    microphone.permission = content.microphonePermission;

    $commonApi.canShare = SHARE_SUPPORTED;
    $commonApi.vscode = VSCODE; // Add vscode flag to the common api.
    $commonApi.net.lan = LAN_HOST;
    $commonApi.user = USER;
    $commonApi.net.iframe = content.iframe;
    $commonApi.net.sandboxed = content.sandboxed;

    codeChannelAutoLoader = null;
    codeChannel = await store.retrieve("code-channel");
    if (!codeChannel || codeChannel?.length === 0) {
      codeChannel = nanoid();
      store["code-channel"] = codeChannel;
      store.persist("code-channel");
    }

    // console.log("💻 Code channel:", codeChannel);

    // Always send the codeChannel up to any parent window.
    codeChannelAutoLoader = () => {
      send({
        type: "post-to-parent",
        content: { type: "setCode", value: codeChannel },
      });
      codeChannelAutoLoader = null;
    };

    // console.log("Init:", content);
    // await handle(); // Get the user's handle.
    // console.log("🟢 Loading after preamble:", content.parsed);

    originalHost = content.parsed.host;
    loadAfterPreamble = () => {
      loadAfterPreamble = null;
      load(content.parsed); // Load after some of the default frames run.
    };

    if (PREVIEW_OR_ICON) {
      console.log("💬 Chat disabled, just grabbing screenshots. 😃");
    } else {
      chatClient.connect("system"); // Connect to `system` chat.
    }

    send({ type: "disk-defaults-loaded" });
    return;
  }

  if (type === "logout:broadcast:subscribe") {
    console.log("🏃‍♂️ Broadcasting logout:", content);
    socket?.send("logout:broadcast:subscribe", content);
    return;
  }

  // Get visualViewport update, for keyboard overlays, etc.
  if (type === "viewport-height:changed" && booted) {
    const $api = cachedAPI;
    const data = { ...content };
    Object.assign(data, {
      is: (e) => e === type,
    });
    $api.event = data;
    try {
      act($api);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
    return;
  }

  // Receive a midi input message.
  if (type === "midi:keyboard" && booted) {
    // console.log("🎹 Keyboard:", content.data);
    const $api = cachedAPI;
    const data = { ...content };
    Object.assign(data, {
      device: "midi:keyboard",
      is: (e) => e === type,
    });
    $api.event = data;
    try {
      act($api);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
    return;
  }

  if (type === "audio:sample-rate") {
    AUDIO_SAMPLE_RATE = content;
    return;
  }

  // Update the logged in user after initialization.
  if (type === "session:started") {
    // console.log("🟢 Session starting...");
    USER = content.user;
    $commonApi.user = USER; // User will be set to "null" here
    //                         it it doesn't exist.

    if (USER) {
      // console.log("Getting handle...");
      await handle(); // Get the user's handle.
      // console.log("Handle recived:", HANDLE);
      console.log(
        `👋 Welcome back %c${HANDLE || USER.email}`,
        `color: yellow; background: rgba(10, 20, 40);`,
      );
      // Broadcast to other tabs...
      $commonApi.broadcast("login:success");
    } else {
      // console.log("🔐 You are not logged in.");
    }
    sessionStarted = true;
    return;
  }

  // Confirming if the pen has been locked or unlocked by the Pointer Lock API.
  if (type === "pen:locked" || type === "pen:unlocked") {
    actAlerts.push(type);
    return;
  }

  // Capture a link from the docs system.
  if (type === "docs:link") {
    console.log("📚 Doc link captured:", content);
    $commonApi.jump("prompt~" + content);
    return;
  }

  // Capture the browser scroll wheel / scroll effect.
  if (type === "scroll") {
    const $api = cachedAPI;
    const data = { ...content };
    Object.assign(data, {
      device: "wheel",
      is: (e) => e === type,
    });
    $api.event = data;
    try {
      act($api);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
    return;
  }

  // Jump to any piece slug from the bios.
  if (type === "jump") {
    console.log("🏃 Jumping to:", content);
    let ahistorical, alias;
    if (content.ahistorical === undefined) {
      ahistorical = true;
    } else ahistorical = content.ahistorical;
    if (content.alias === undefined) {
      alias = true;
    } else alias = content.alias;
    $commonApi.jump(content.piece, ahistorical, alias);
    return;
  }

  // Create a notice.
  if (type === "notice") {
    $commonApi.notice(content, ["white", "maroon"]);
    return;
  }

  if (type === "loading-complete") {
    leaving = false;
    invalidateAPICache(); // Invalidate cached APIs when piece changes
    hotSwap?.(); // Actually swap out the piece functions and reset the state.
    loading = false;
    return;
  }

  if (type === "udp:receive") {
    udp.receive(content);
    return;
  }

  if (type === "udp:connected") {
    udp.connected = true;
    updateHUDStatus();
    $commonApi.needsPaint();
    return;
  }

  if (type === "udp:disconnected") {
    udp.connected = false;
    updateHUDStatus();
    $commonApi.needsPaint();
    return;
  }

  if (type === "microphone-disconnect") {
    microphone.connected = false;
    return;
  }

  // if (type === "hand-tracking-data") {
  // $commonApi.hand = { mediapipe: content };
  // return;
  // }

  // Load the source code for a dropped `.mjs` or `.lisp` file.
  if (type === "dropped:piece") {
    // Parse the dropped piece name and attach the source code
    // Strip the .mjs or .lisp extension from the filename before parsing
    const pieceName = content.name.replace(/\.(mjs|lisp)$/, "");
    const isLisp = content.name.endsWith(".lisp");
    console.log(
      "📁 Dropped piece:",
      content.name,
      `(${content.source?.length || 0} chars)`,
      isLisp ? "- KidLisp" : "- JavaScript",
    );
    const parsed = parse(pieceName);
    parsed.source = content.source;
    // Set parsed.name to the same value as parsed.text for dropped pieces
    // This is needed because the load function uses parsed.name when source is provided
    parsed.name = parsed.text;
    // For .lisp files, force KidLisp interpretation
    load(parsed, false, false, true, undefined, isLisp);
    return;
  }

  // Handle dropped Ableton Live Set (.als) files.
  if (type === "dropped:als") {
    console.log(
      "🎵 Dropped ALS file:",
      content.name,
      `(${content.xmlData?.length || 0} chars XML)`,
    );
    
    // Send the XML data to the current piece if it's the ableton piece
    if ($commonApi.piece === "ableton" && cachedAPI) {
      // First send a loading event
      const $api = cachedAPI;
      let data = {};
      Object.assign(data, {
        device: "none",
        is: (e) => e === "ableton:als:loading",
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ ALS Loading Act failure...", e);
      }
      
      // Then send the actual data
      data = { 
        type: "ableton:als:loaded",
        xmlData: content.xmlData,
        name: content.name,
        project: content.project // 🎵 Forward the parsed project data!
      };
      Object.assign(data, {
        device: "none",
        is: (e) => e === "ableton:als:loaded",
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ ALS Act failure...", e);
      }
    } else {
      // Load the ableton piece and send the data as an act event once loaded
      const parsed = parse("ableton");
      load(parsed, false, false, false, () => {
        // This callback runs after the piece is loaded
        const $api = cachedAPI;
        if ($api) {
          const data = { 
            type: "ableton:als:loaded",
            xmlData: content.xmlData,
            name: content.name,
            project: content.project // 🎵 Forward the parsed project data!
          };
          Object.assign(data, {
            device: "none",
            is: (e) => e === "ableton:als:loaded",
          });
          $api.event = data;
          try {
            act($api);
          } catch (e) {
            console.warn("️ ✒ ALS Load Act failure...", e);
          }
        }
      });
    }
    return;
  }

  // Handle dropped WAV audio files.
  if (type === "dropped:wav") {
    console.log(
      "🔊 Dropped WAV file:",
      content.name,
      `(${content.size || 0} bytes)`,
    );
    
    // Send the WAV data to the current piece if it's the ableton piece
    if ($commonApi.piece === "ableton" && cachedAPI) {
      // First send a loading event
      const $api = cachedAPI;
      let data = {};
      Object.assign(data, {
        device: "none",
        is: (e) => e === "ableton:wav:loading",
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ WAV Loading Act failure...", e);
      }
      
      // Then send the actual data
      data = { 
        name: content.name,
        originalName: content.originalName,
        size: content.size,
        id: content.id // Pass the audio ID for playback
      };
      Object.assign(data, {
        device: "none",
        is: (e) => e === "ableton:wav:loaded",
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ WAV Act failure...", e);
      }
    } else {
      // Load the ableton piece and send the data as an act event once loaded
      const parsed = parse("ableton");
      load(parsed, false, false, false, () => {
        // This callback runs after the piece is loaded
        const $api = cachedAPI;
        if ($api) {
          const data = { 
            wavData: content.data,
            name: content.name,
            originalName: content.originalName,
            audioId: content.audioId // Pass the audio ID for playback
          };
          Object.assign(data, {
            device: "none",
            is: (e) => e === "ableton:wav:loaded",
          });
          $api.event = data;
          try {
            act($api);
          } catch (e) {
            console.warn("️ ✒ WAV Load Act failure...", e);
          }
        }
      });
    }
    return;
  }

  if (type === "dropped:bitmap") {
    if (currentPath === "aesthetic.computer/disks/prompt") {
      $commonApi.system.nopaint.replace(
        { system: $commonApi.system, store, needsPaint: $commonApi.needsPaint },
        content.source,
      );
    } else {
      const $api = cachedAPI;
      const data = { name: content.name, painting: content.source };
      Object.assign(data, {
        device: "none",
        is: (e) => e === type,
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ Act failure...", e);
      }
    }
    return;
  }

  // 🗣️ An act that fires when an utterance has ended in the Web Speech API.
  if (type === "speech:completed") {
    actAlerts.push("speech:completed");
    return;
  }

  // When inputting text into the prompt.
  if (
    type === "prompt:text:replace" ||
    type === "prompt:text:select" ||
    type === "prompt:text:cursor"
  ) {
    const $api = cachedAPI;
    const data = { ...content };
    Object.assign(data, {
      device: "none",
      is: (e) => e === type,
    });
    $api.event = data;
    try {
      act($api);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
    return;
  }

  // Handles: clipboard:paste:pasted, clipboard:paste:pasted:empty
  if (type.startsWith("paste:pasted")) {
    actAlerts.push("clipboard:" + type);
    return;
  }

  if (type === "paste:failed") {
    actAlerts.push("clipboard:paste:failed");
    return;
  }

  if (type === "copy:copied") {
    actAlerts.push("clipboard:copy:copied");
    return;
  }

  if (type === "aesthetic-parent:focused") {
    actAlerts.push("aesthetic-parent:focused");
    return;
  }

  if (type === "copy:failed") {
    actAlerts.push("clipboard:copy:failed");
    return;
  }

  if (type === "upload:progress") {
    serverUploadProgressReporter?.(content); // Report file upload progress if needed.
    return;
  }

  if (type === "focus-change") {
    if (!cachedAPI) return; // Hacky... 23.04.21.14.59
    const $api = cachedAPI;
    if (content !== inFocus) {
      inFocus = content;
      const data = {};
      Object.assign(data, {
        device: "none",
        is: (e) => e === (inFocus === true ? "focus" : "defocus"),
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ Act failure...", e);
      }
    }
  }

  if (type === "visibility-change") {
    // 🧨 Just in case of a regression... 23.06.02.21.12
    //    Because the `bios` focus event changed from visibility behavior.
    // if (!lastActAPI) return; // Hacky... 23.04.21.14.59
    // const $api = lastActAPI; // Focus change events have an empty API.
    // if (content !== inFocus) {
    //   inFocus = content;
    //   const data = {};
    //   Object.assign(data, {
    //     device: "none",
    //     is: (e) => e === (inFocus === true ? "focus" : "defocus"),
    //   });
    //   $api.event = data;
    //   try {
    //     act($api);
    //   } catch (e) {
    //     console.warn("️ ✒ Act failure...", e);
    //   }
    // }
    visible = content;
  }

  if (type === "before-unload") {
    // This has to be synchronous (no workers) to work, and is also often unreliable.
    // I should not design around using this event, other than perhaps
    // sending a beacon at the end. 22.11.03.14.53
    // See also: https://developer.mozilla.org/en-US/docs/Web/API/Navigator/sendBeacon

    /*
    try {
      leave({ store, screen, system: $commonApi.system }); // Trigger leave.
    } catch (e) {
      console.warn("👋 Leave failure...", e);
    }
    */
    return;
  }

  // Get the updated device motion.
  if (type === "motion:update") {
    $commonApi.motion.on = true;
    $commonApi.motion.current = content;
    return;
  }

  if (type === "motion:enabled") {
    $commonApi.motion.on = true;
    return;
  }

  if (type === "gpu-rendered-once") {
    $commonApi.gpuReady = true;
    return;
  }

  if (type === "gpu-forms-removed") {
    // Delete forms from the sent list that have been removed from the GPU scene.
    content.forEach((id) => {
      formsToClear.push(id);
    });
    return;
  }

  if (type === "dark-mode") {
    darkMode(content.enabled);
    return;
  }

  if (type === "forms:baked") {
    //console.log("🍞 Forms baked:", content);
    //noPaint = false;

    // if (content.pixels) {
    //  graph.paste(content, 0, 0, 1, true);
    // }

    // paintFormsResolution?.();
    return;
  }

  // Media Recorder Events

  if (type === "recorder:transcode-progress") {
    if (debug) console.log("📼 Recorder: Transcoding", content);
    $commonApi.rec.printProgress = content;
    if (content === 1) {
      send({ type: "signal", content: "recorder:transcoding-done" });
      // TODO: Is this the best place for this signal to be sent?
      //       Maybe it should go back in the BIOS? 22.08.19.13.44
    }
    return;
  }

  if (
    type === "recorder:rolling:started" ||
    type === "recorder:rolling:resumed"
  ) {
    // console.log("🎬 Rolling started/resumed, invoking callback with time:", content.time);
    $commonApi.rec.recording = true;
    $commonApi.rec.rollingCallback?.(content.time);
    return;
  }

  if (type === "recorder:rolling:ended") {
    $commonApi.rec.recording = false;
    $commonApi.rec.recordingStartTime = undefined; // Clear animation start time
    $commonApi.rec.animationFrame = undefined; // Clear animation frame counter
    $commonApi.rec.recorded = true; // Also cleared when a recording "slates".
    $commonApi.rec.cutCallback?.();
    return;
  }

  if (type === "recorder:printing:started") {
    return;
  }

  if (type === "recorder:printed") {
    $commonApi.rec.printed = true;
    $commonApi.rec.printCallback?.(content);
    return;
  }

  if (type === "recorder:presented") {
    $commonApi.rec.presenting = true;
    return;
  }

  if (type === "recorder:presented:failure") {
    $commonApi.rec.presenting = false;
    return;
  }

  if (type === "recorder:frames-response") {
    $commonApi.rec.framesCallback?.(content);
    return;
  }

  if (type === "recorder:present-progress") {
    $commonApi.rec.presentProgress = content;
    return;
  }

  if (type === "recorder:present-playing") {
    $commonApi.rec.playing = true;
    return;
  }

  if (type === "recorder:present-paused") {
    // console.log("🎬 Tape playback paused - resuming pixel transfer");
    $commonApi.rec.playing = false;
    return;
  }

  if (type === "recorder:unpresented") {
    // console.log("🎬 Tape playback ended - resuming pixel transfer");
    $commonApi.rec.presenting = false;
    $commonApi.rec.playing = false;
    return;
  }

  if (type === "signal") {
    signals.push(content);
    return;
  }

  if (type === "store:retrieved") {
    storeRetrievalResolutions[content.key]?.(content.data);
    delete storeRetrievalResolutions[content.key];
    return;
  }

  if (type === "store:deleted") {
    storeDeletionResolutions[content.key]?.(content.data);
    delete storeDeletionResolutions[content.key];
    return;
  }

  if (type === "content-created") {
    $commonApi.content.receive(content);
    return;
  }

  if (type === "leave") {
    //const $api = {};
    console.log("🏃‍♂️ Leave:", content);
    return;
  }

  if (type === "sfx:killed") {
    sfxKillReceivers[content.id]?.();
    return;
  }

  if (type === "sfx:got-sample-data") {
    sfxSampleReceivers[content.id]?.(content.data);
    return;
  }

  if (type === "sfx:got-duration") {
    sfxDurationReceivers[content.id]?.(content.duration);
    return;
  }

  if (type === "sfx:progress:report") {
    sfxProgressReceivers[content.id]?.(content); // Resolve the progress report.
    return;
  }

  if (type === "microphone-amplitude") {
    microphone.amplitude = content;
    return;
  }

  if (type === "microphone-waveform") {
    microphone.waveform = content;
    return;
  }

  if (type === "waveforms") {
    speaker.waveforms = content;
    return;
  }

  if (type === "amplitudes") {
    speaker.amplitudes = content;
    return;
  }

  if (type === "microphone-pitch") {
    microphone.pitch = content;
    return;
  }

  if (type === "microphone-recording:complete") {
    microphone.recordingPromise?.resolve(content);
    return;
  }

  if (type === "microphone-connect:success") {
    microphone.connected = true;
    actAlerts.push("microphone-connect:success");
    return;
  }

  if (type === "microphone-connect:failure") {
    microphone.connected = false;
    actAlerts.push({ name: "microphone-connect:failure", ...content });
    return;
  }

  // 1a. Import // One send (returns afterwards)
  // Here we are receiving file data from main thread that was requested
  // by $api.upload 😱. We check to see if the upload promise exists and then
  // use it and/or throw it away.
  if (type === "import" && fileImport) {
    if (content.result === "success") {
      fileImport?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("File failed to load:", content.data);
      fileImport?.reject(content.data);
    }
    fileImport = undefined;
    return;
  }

  // Resolve a web3 connection message.
  if (type === "web3-connect-response" && web3Response) {
    if (content.result === "success") {
      web3Response?.resolve(content.id);
    } else if (content.result === "error") {
      web3Response?.reject("error");
    }
    web3Response = undefined;
    return;
  }

  // Resolve a gpu message
  if (type === "gpu-response" && gpuResponse) {
    if (content.result === "success") {
      gpuResponse?.resolve(content.data);
    } else if (content.result === "error") {
      gpuResponse?.reject(content.data);
    }
    gpuResponse = undefined;
    return;
  }

  // Resolve a server uploaded file.
  if (type === "upload" && serverUpload) {
    if (content.result === "success") {
      serverUpload?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("File failed to load:", content);
      serverUpload?.reject(content.data);
    }
    serverUpload = undefined;
    return;
  }

  if (type === "zipped" && zipCreation) {
    if (content.result === "success") {
      zipCreation.resolve(content.data);
    } else if (content.result === "error") {
      console.error("Zip failed to be created:", content);
      zipCreation?.reject(content.data);
    }
    zipCreation = undefined;
    return;
  }

  // Run when a painting record ZIP is succesfully parsed after being
  // dragged into the A.C window.
  if (type === "painting:record:dropped") {
    // Replace the active nopaint record with the loaded one.
    // $commonApi.system.nopaint.recording = true;
    $commonApi.system.nopaint.record = content;
    if ($commonApi.slug !== "painting") $commonApi.jump("painting");
    return;
  }

  // Resolve a locally requested file.
  if (type === "file-open:response" && fileOpenRequest) {
    if (content.result === "success") {
      fileOpenRequest?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("Failed to open file.", content);
      fileOpenRequest?.reject(content.data);
    }
    fileOpenRequest = undefined;
    return;
  }

  // Resolve a file encoding request.
  if (type === "file-encode:response" && fileEncodeRequest) {
    if (content.result === "success") {
      fileEncodeRequest?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("Failed to encode file.", content);
      fileEncodeRequest?.reject(content.data);
    }
    fileEncodeRequest = undefined;
    return;
  }

  // Resolve an authorization request.
  if (type === "authorization:response" && authorizationRequest) {
    if (content.result === "success") {
      authorizationRequest?.resolve(content.data);
    } else if (content.result === "error") {
      authorizationRequest?.reject(content.data);
    }
    authorizationRequest = undefined;
    return;
  }

  // 1b. Video frames.
  if (type === "video-frame") {
    if (!videoSwitching) activeVideo = content;
    return;
  }

  if (type === "video-devices") {
    videoDeviceCount = content;
    $commonApi.cameras = videoDeviceCount;
    return;
  }

  if (type === "camera:updated") {
    videoSwitching = false;
    actAlerts.push("camera:mode:" + content);
    return;
  }

  if (type === "camera:denied") {
    actAlerts.push("camera:denied");
    return;
  }

  // 1c. Loading from History
  if (type === "history-load") {
    if (debug && logs.history) console.log("⏳ History:", content);
    // Restore labelBack state if it was passed from main thread history navigation
    if (content.labelBack) {
      labelBack = true;
      console.log("🔗 Worker: Restored labelBack from history navigation");
    }
    $commonApi.load(content, true);
    return;
  }

  // 1d. Loading Bitmaps
  if (type === "loaded-bitmap-success") {
    preloadPromises[content.url]?.resolve(content);
    delete preloadPromises[content];
    return;
  }

  if (type === "loaded-bitmap-rejection") {
    if (debug) console.error("🖼️ Bitmap load failure:", content);
    preloadPromises[content.url]?.reject(content.url);
    delete preloadPromises[content.url];
    return;
  }

  // 1e. Loading Sound Effects
  if (type === "loaded-sfx-success") {
    if (debug && logs.audio) console.log("Sound load success:", content);
    if (debug && logs.audio)
      console.log("Resolving preload promise for:", content.sfx);
    preloadPromises[content.sfx]?.resolve(content.sfx);
    delete preloadPromises[content];
    return;
  }

  if (type === "loaded-sfx-rejection") {
    if (debug && logs.audio) console.error("Sound load failure:", content);
    preloadPromises[content.sfx]?.reject(content.sfx);
    delete preloadPromises[content.sfx];
    return;
  }

  // 1f. Loading ZIP files.
  if (type === "loaded-zip-success") {
    if (debug) console.log("🤐 Zip load success:", content.url);
    preloadPromises[content.url]?.resolve(content.data);
    delete preloadPromises[content.url];
    return;
  }

  if (type === "loaded-zip-rejection") {
    if (debug) console.warn("🤐 Zip load failure:", content.url);
    preloadPromises[content.url]?.reject(content.url);
    delete preloadPromises[content.url];
    return;
  }

  // Request a repaint (runs when the window is resized.)
  if (type === "needs-paint") {
    noPaint = false;
    return;
  }

  if (type === "reframed") {
    // Always update the currentDisplay settings for synchronous
    // screen buffer updates.
    currentDisplay = {
      width: content.innerWidth,
      height: content.innerHeight,
      subdivisions: content.subdivisions,
    };
    $commonApi.display = currentDisplay;

    // Only trigger a reframe event if we have already passed `boot` (painted
    // at least once)
    if (booted) {
      reframed = true;
      formReframing = true;
    }
    return;
  }

  // 1. Beat
  if (type === "beat") {
    if (!sound) return; // Just in case no `frame` has been sent yet.
    try {
      beat($activePaintApi);
    } catch (e) {
      console.warn(" 💗 Beat failure...", e);
    }

    send({ type: "beat", content: sound });
    // soundClear?.();
    sound.sounds.length = 0; // Empty the sound command buffer.
    sound.bubbles.length = 0;
    sound.kills.length = 0;
    return;
  }

  // 2. Frame
  // Where each piece action (boot, sim, paint, etc...) is run.
  if (type === "frame") {
    // Take hold of a previously worker transferrable screen buffer
    // and re-assign it.
    let pixels;
    if (content.pixels) {
      pixels = new Uint8ClampedArray(content.pixels);
      if (screen) {
        // Check if we need to apply background fill to the incoming pixels
        if (backgroundFillColor && pixels.length > 0) {
          try {
            // Use graph.findColor to resolve the color to RGBA
            let fillColor = graph.findColor(backgroundFillColor);
            // Ensure we have 4 components (RGBA), defaulting alpha to 255 if missing
            if (fillColor.length === 3) {
              fillColor = [fillColor[0], fillColor[1], fillColor[2], 255];
            }
            
            // Check if pixels are currently transparent (all zeros) and fill only those
            for (let i = 0; i < pixels.length; i += 4) {
              // Check if pixel is transparent (alpha = 0)
              if (pixels[i + 3] === 0) {
                pixels[i] = fillColor[0];     // Red
                pixels[i + 1] = fillColor[1]; // Green  
                pixels[i + 2] = fillColor[2]; // Blue
                pixels[i + 3] = fillColor[3]; // Alpha
              }
            }
          } catch (e) {
            // Silently continue if color resolution fails
          }
        }
        
        screen.pixels = pixels;
      }
    }

    // 🔙 Abstracted backspace logic for reuse between keyboard and touch-scrub
    // Debounce mechanism to prevent double backspace triggers
    let lastBackspaceTime = 0;
    
    function triggerBackspaceAction() {
      const now = Date.now();
      if (now - lastBackspaceTime < 300) { // 300ms debounce
        // console.log("🎬 Backspace debounced - ignoring rapid fire");
        return;
      }
      lastBackspaceTime = now;
      
      $commonApi.sound.synth({
        tone: 800,
        beats: 0.1,
        attack: 0.01,
        decay: 0.5,
        volume: 0.15,
      });

      // Immediately reset pan state for backspace navigation
      graph.unpan();

      send({ type: "keyboard:unlock" });

      // Always go to prompt for editing, regardless of labelBack setting
      // This ensures backspace always returns to editable input
      let promptSlug = "prompt";
      
      // 🎬 Special case: If coming from video piece after a tape command,
      // return to the original tape command instead of "video"
      // Check both currentText and currentPath to catch all ways of getting to video
      let content;
      if ((currentText === "video" || currentPath === "aesthetic.computer/disks/video") && store["tape:originalCommand"]) {
        // console.log("🎬 Backspace from video piece - using original tape command:", store["tape:originalCommand"]);
        // console.log("🎬 Debug - isKidlispSource(originalCommand):", lisp.isKidlispSource(store["tape:originalCommand"]));
        content = store["tape:originalCommand"];
        // Don't clear the stored command yet - wait until we successfully navigate away
        // store.delete("tape:originalCommand");
      } else if (currentText && currentText.startsWith("$") && currentCode && lisp.isKidlispSource(currentCode)) {
        // This is a cached KidLisp piece - use the full source code for backspace
        content = currentCode;
      } else {
        // Use currentText which contains the original slug with tildes (e.g., "rect~red")
        content = currentText;
      }
      
      if (content) {
        // For backspace navigation, we want to pass KidLisp source directly without encoding
        // since this is for editing, not URL sharing
        if (lisp.isKidlispSource(content)) {
          // Don't encode for backspace - pass raw source for editing
          promptSlug += "~" + content;
        } else {
          // For regular piece names, replace spaces with underscores temporarily
          // This is simpler and safer than URL encoding for internal navigation
          const escapedContent = content.replace(/ /g, "_SPACE_");
          promptSlug += "~" + escapedContent;
        }
      }
      
      // Clear the stored tape command after successful navigation setup
      if (content === store["tape:originalCommand"]) {
        store.delete("tape:originalCommand");
      }
      
      $commonApi.jump(promptSlug);
      send({ type: "keyboard:open" });
    }

    // 🌟 Global Keyboard Shortcuts (these could also be seen via `act`)
    // GC OPTIMIZATION: Use for loop instead of forEach
    for (let i = 0; i < content.keyboard.length; i++) {
      const data = content.keyboard[i];
      if (currentText && currentText.indexOf("botce") > -1) continue; // No global keys on `botce`. 23.11.12.23.38
      if (data.name.indexOf("keyboard:down") === 0) {
        // [Escape] (Deprecated on 23.05.22.19.33)
        // If not on prompt, then move backwards through the history of
        // previously loaded pieces in a session.
        // if (
        //   data.key === "Escape" &&
        //   currentPath !== "aesthetic.computer/disks/prompt"
        // ) {
        //   if (pieceHistoryIndex > 0) {
        //     send({ type: "back-to-piece" });
        //   } else {
        //     // Load the prompt automatically.
        //     // $api.load("prompt"); Disabled on 2022.05.07.03.45
        //   }
        // }

        if (data.key === "$" || data.key === "Home") {
          if (data.ctrl || data.alt) {
            const sys = $commonApi.system;
            // Make it a painting.
            sys.nopaint.replace(
              cachedAPI,
              graph.cloneBuffer(screen),
              "$creenshot",
            );
            $commonApi.jump("prompt");
          } else {
            downloadScreenshot(); // 🖼️ Take a screenshot.
          }
          $commonApi.sound.synth({
            tone: 1600,
            duration: 0.02,
            attack: 0.01,
            decay: 0.5,
            volume: 0.25,
          });
        }

        // ⛈️ Jump back to the `prompt` from anywhere..
        if (
          (data.key === "`" ||
            data.key === "Enter" ||
            data.key === "Backspace" ||
            data.key === "Escape") &&
          system !== "prompt" &&
          system !== "world" &&
          currentText !== "chat" &&
          currentText !== "laer-klokken" &&
          currentText !== "sign" &&
          currentPath !== "aesthetic.computer/disks/prompt"
        ) {
          if (data.key === "Backspace") {
            triggerBackspaceAction();
          } else {
            $commonApi.sound.synth({
              tone: 1200,
              beats: 0.1,
              attack: 0.01,
              decay: 0.5,
              volume: 0.15,
            });

            send({ type: "keyboard:unlock" });
            if (!labelBack) {
              $commonApi.jump("prompt")(() => {
                send({ type: "keyboard:open" });
              });
            } else {
              if ($commonApi.history.length > 0) {
                send({
                  type: "back-to-piece",
                  content: {
                    targetPiece:
                      $commonApi.history[$commonApi.history.length - 1],
                  },
                });
              } else {
                $commonApi.jump("prompt")(() => {
                  send({ type: "keyboard:open" });
                });
              }
            }
          }
        }

        // [Ctrl + X]
        // Enter and exit fullscreen mode.
        if (data.key === "x" && data.ctrl && currentText !== "notepat") {
          send({ type: "fullscreen-enable" });
        }
      }
    }

    // Add 'loading' status to $commonApi.
    $commonApi.loading = loading; // Let the piece know if we are already
    //                               loading another piece.

    // Globalize any background music data, retrievable via bgm.data
    $commonApi.bgm.data = {
      amplitude: content.audioMusicAmplitude,
      sample: content.audioMusicSampleData,
    };

    // Hand-tracking
    if (content.hand) $commonApi.hand = { mediapipe: content.hand };

    // Pens
    if (content.pen) {
      const primaryPointer = help.findKeyAndValue(
        content.pen.pointers,
        "isPrimary",
        true,
      );

      // Returns all [pens] if n is undefined, or can return a specific pen by 1 based index.
      // [pens] are sorted by `pointerIndex`

      // TODO: Including "help.findKeyAndValue" seems to bring a lot of
      //       allocation here because it keeps the whole API around?
      //       Re-test this when pointers is not empty! 22.11.12.20.02
      const pointers = content.pen.pointers;
      const pointersValues = Object.values(pointers);

      // GC OPTIMIZATION: Use for loop instead of forEach for dragBox processing
      for (let i = 0; i < pointersValues.length; i++) {
        const p = pointersValues[i];
        if (p.dragBox) p.dragBox = geo.Box.from(p.dragBox);
      }

      // GC OPTIMIZATION: Use manual loop instead of reduce
      const pens = [];
      for (let i = 0; i < pointersValues.length; i++) {
        const value = pointersValues[i];
        pens[value.pointerNumber] = value;
      }

      // if (pens.length > 0 && debug)
      //   console.log("Pens:", pens, content.pen.events);

      $commonApi.pens = function (n) {
        if (n === undefined) return pens;
        return help.findKeyAndValue(pointers, "pointerNumber", n - 1) || {};
      };

      if (pointersValues.length > 1 && primaryPointer)
        primaryPointer.multipen = true; // Set a flag for multipen activity on main pen API object.

      $commonApi.pen = primaryPointer;

      if (
        screen &&
        primaryPointer &&
        (primaryPointer.delta?.x !== 0 || primaryPointer.delta?.y !== 0)
      ) {
        //socket?.send("ambient-pen:point", {
        udp?.send("fairy:point", {
          x: primaryPointer.x / screen.width,
          y: primaryPointer.y / screen.height,
        });
      }
    }

    // 🕶️ VR Pen
    $commonApi.pen3d = content.pen3d?.pen;

    // Add upload event to allow the main thread to open a file chooser.
    // type: Accepts N mimetypes or file extensions as comma separated string.
    // Usage: upload(".jpg").then((data) => ( ... )).catch((err) => ( ... ));
    $commonApi.sideload = (type) => {
      const prom = new Promise((resolve, reject) => {
        fileImport = { resolve, reject };
      });
      send({ type: "import", content: type });
      return prom;
    };

    // 🤖 Sim // no send
    $commonApi.seconds = function (s) {
      return s * 120; // TODO: Get 120 dynamically from the Loop setting. 2022.01.13.23.28
    };

    // 🔈 Sound
    // TODO: Most of the $sound api doesn't need to be generated per
    //       frame. 24.01.14.15.19

    // For reference in `freq` below.
    const noteFrequencies = {
      c: 16.35,
      "c#": 17.32,
      db: 17.32,
      d: 18.35,
      "d#": 19.45,
      eb: 19.45,
      e: 20.6,
      f: 21.83,
      "f#": 23.12,
      gb: 23.12,
      g: 24.5,
      "g#": 25.96,
      ab: 25.96,
      a: 27.5,
      "a#": 29.14,
      bb: 29.14,
      b: 30.87,
    };

    const $sound = {
      time: content.audioTime,
      // Get the bpm with bpm() or set the bpm with bpm(newBPM).
      bpm: function (newBPM) {
        if (newBPM) sound.bpm = newBPM;
        return sound.bpm;
      },
      enabled: () => {
        return AUDIO_SAMPLE_RATE > 0;
      },
      // Compute the frequency of a musical note.
      // 🗒️ Can take a number or formatted octave string such as 5C# or even C#5 for C sharp in fifth octave.
      freq: function (input) {
        // console.log("🎵 Note to check:", input);
        // Return if it's just a number or parses as one.
        if (typeof input === "number") return input;
        if (input === null || input === undefined) return null;
        if (!isNaN(parseFloat(input)) && isFinite(input)) return Number(input);

        let octave, note;
        input = input.toLowerCase(); // Downcase everything.

        // Check if the first character is a digit to determine if an octave is provided at the beginning
        if (!isNaN(input.charAt(0))) {
          // The first character is the octave
          octave = parseInt(input.charAt(0), 10);
          note = input.substring(1);
        } else if (!isNaN(input.charAt(input.length - 1))) {
          // The last character is the octave
          octave = parseInt(input.charAt(input.length - 1), 10);
          note = input.substring(0, input.length - 1);
        } else {
          // If no octave is provided, default to octave 4
          octave = 4;
          note = input;
        }

        // Replace 's' with '#' and trailing 'f' with 'b', but only for note strings of length 2
        if (note.length === 2) {
          note = note.replace("s", "#").replace(/f$/, "b");
        }

        const frequency = noteFrequencies[note]; // Look up freq for the note.
        if (!frequency) {
          console.error("🎵 Note lookup failed:", {
            originalInput: input,
            parsedNote: note,
            parsedOctave: octave,
            availableNotes: Object.keys(noteFrequencies),
          });
          throw new Error(
            `Note not found in the list: "${note}" (from input: "${input}")`,
          );
        }

        // Calculate the frequency for the given octave
        // Limit octave to reasonable audio range (0-9)
        if (octave < 0) {
          console.warn(`🎵 Octave too low: ${octave}, clamping to 0`);
          octave = 0;
        }
        if (octave > 9) {
          console.warn(`🎵 Octave too high: ${octave}, clamping to 9`);
          octave = 9;
        }

        const finalFreq = frequency * Math.pow(2, octave);

        // Final sanity check on frequency range (20 Hz to 20000 Hz)
        if (finalFreq < 20 || finalFreq > 20000) {
          console.warn(
            `🎵 Frequency out of audible range: ${finalFreq.toFixed(2)} Hz for ${input}`,
          );
        }

        return finalFreq;
      },
      // Calculate a musical note from a frequency.
      note: function (frequency) {
        let closestNote = "",
          minDiff = Infinity;
        for (let octave = 0; octave <= 8; octave++) {
          for (let note in noteFrequencies) {
            const noteFrequency = noteFrequencies[note] * Math.pow(2, octave);
            const diff = Math.abs(frequency - noteFrequency);
            if (diff < minDiff) {
              minDiff = diff;
              closestNote = note + octave;
            }
          }
        }

        return closestNote.toUpperCase();
      },
      // MIDI
      midi: {
        connect: () => send({ type: "midi:connect" }),
        // Convert MIDI note number to note string
        note: function (midiNumber) {
          const noteNames = [
            "C",
            "C#",
            "D",
            "D#",
            "E",
            "F",
            "F#",
            "G",
            "G#",
            "A",
            "A#",
            "B",
          ];
          const octave = floor(midiNumber / 12) - 1;
          const noteIndex = midiNumber % 12;
          return noteNames[noteIndex] + octave;
        },
      },
      // Rendering
      paint: {
        bars: function paintSound(
          { ink, box, screen, num },
          amplitude,
          waveform,
          x,
          y,
          width,
          height,
          color,
          options = { noamp: false, nobounds: false },
        ) {
          const yMid = round(y + (height - 2) / 2),
            yMax = round((height - 2) / 2);
          let lw = options.noamp ? 0 : 4; // levelWidth;
          const xStep = (width - lw) / waveform.length;

          // Vertical bounds.
          if (!options.nobounds) {
            ink("yellow")
              .line(x + lw, y, x + width - 1, y)
              .line(x + lw, y + height, x + width - 1, y + height);
          }

          // Level meter.
          if (!options.noamp) {
            ink("black").box(x, y, lw, height + 1);
            ink("green").box(x, y + height, lw, -amplitude * height);
          }

          // Filled waveform
          const waves = waveform.map((v, i) => {
            if (v < -1) v = -1;
            if (v > 1) v = 1;
            return [x + lw + i * xStep, yMid + v * yMax];
          });

          ink(options.secondaryColor || "black").box(
            x + lw,
            y + 1,
            width - lw,
            height - 1,
          );

          ink(options.primaryColor || "white");

          let remainder = 0;
          let totalWidthCovered = 0;

          waves.forEach((point, index) => {
            let bx = x + lw + totalWidthCovered;
            if (bx >= x + width) return;
            // Compute the pixel-aligned width for the current bar.
            let barWidth = Math.floor(xStep + remainder);
            remainder = (xStep + remainder) % 1; // Collect the fractional remainder.
            // Ensure we don't exceed the full width for the last bar.
            if (index === waves.length - 1 || bx + barWidth >= x + width)
              barWidth = x + width - bx;
            box(
              bx,
              y + point[1] + 1 - y,
              barWidth,
              y + (height - 1) - point[1],
            );
            totalWidthCovered += barWidth;
          });

          // Waveform
          // ink("lime", 255).poly(
          //   waveform.map((v, i) => [x + lw + i * xStep, yMid + v * yMax]),
          // );

          // TODO: Fill a point above this line and below.
          // ink("blue").flood(x + 7, y + 1);
          // ink("teal").flood(x + 7, y + height - 2);

          // const my = screen.height - mic.amplitude * screen.height;
          // ink("yellow", 128).line(0, my, screen.width, my); // Horiz. line for amplitude.
        },

        // Paints a waveform with a bounding box based on amplitude.
        waveform: function paintWaveform(
          { ink },
          amplitude,
          waveform,
          x,
          y,
          width,
          height,
          color = "yellow",
          options,
        ) {
          if (waveform?.length < 1) return;
          const direction = options?.direction || "left-to-right";
          if (direction === "left-to-right") {
            const xStep = width / (waveform.length - 1);

            const yMid = y + height / 2,
              yMax = height / 2;

            ink(color, 128).poly(
              waveform.map((v, i) => {
                const p = [x + i * xStep, yMid + (v || 0) * yMax];
                return p;
              }),
            );
          } else if (direction === "bottom-to-top") {
            const yStep = height / (waveform.length - 1);
            const xMid = x + width / 2,
              xMax = width;

            ink("blue", 128).poly(
              waveform.map((v, i) => {
                const p = [xMid + (v || 0) * xMax, y + height - i * yStep];
                return p;
              }),
            );
          } else {
            console.warn("🌊 Unsupported direction.");
          }
        },
      },
    };

    $sound.microphone = microphone;
    $sound.speaker = speaker;
    $sound.sampleRate = AUDIO_SAMPLE_RATE;

    // TODO: Generalize square and bubble calls.
    // TODO: Move this stuff to a "sound" module.
    sound.bpm = content.audioBpm;

    // Clear synchronized audio triggers.
    // soundClear = () => {
    // sound.sounds.length = 0;
    // sound.bubbles.length = 0;
    // sound.kills.length = 0;
    // };

    // Trigger a named audio sample to playback in the `bios`.
    // options: { volume: 0-n, pan: 0-2?, loop: Bool, ...(there is more) }

    $sound.getSampleData = async function getSampleData(id) {
      const prom = new Promise((resolve, reject) => {
        sfxSampleReceivers[id] = resolve;
        return { resolve, reject };
      });
      send({ type: "sfx:get-sample-data", content: { id } });
      return prom;
    };

    $sound.getDuration = async function getDuration(id) {
      const prom = new Promise((resolve, reject) => {
        sfxDurationReceivers[id] = resolve;
        return { resolve, reject };
      });
      send({ type: "sfx:get-duration", content: { id } });
      return prom;
    };

    soundTime = content.audioTime;

    $sound.play = function play(sfx, options, callbacks) {
      const id = sfx + "_" + $sampleCount; // A *unique id for this sample.
      $sampleCount += 1n;

      send({ type: "sfx:play", content: { sfx, id, options } });

      const playingSound = {
        options, // Allow the options passed to BIOS to be inspected.
        startedAt: soundTime, // performance.now(),
        killed: false,
        kill: (fade) => {
          send({ type: "sfx:kill", content: { id, fade } });
        },
        progress: async () => {
          if (playingSound.killed) return { progress: 0 };
          const prom = new Promise((resolve, reject) => {
            sfxProgressReceivers[id] = resolve;
            return { resolve, reject };
          });
          send({ type: "sfx:progress", content: { id } });
          return prom;
        },
        update: function (properties) {
          if (properties.shift) {
            send({
              type: "sfx:update",
              content: { id, properties },
            });
          }
        },
      };

      sfxKillReceivers[id] = () => {
        callbacks?.kill?.();
        playingSound.killed = true;
      };

      return playingSound;
    };

    $sound.skip = function () {
      send({ type: "beat:skip" });
    };

    $sound.at = function (timeToRun, callback) {
      // TODO: Finish this implementation.
      // timeToRun;
      // content.audioTime;
    };
    $sound.synth = function synth({
      tone = 440, // hz, or musical note
      type = "square", // "sine", "triangle", "square", "sawtooth", "custom"
      // "noise-white" <-ignores tone
      duration = 0.1, // In seconds... (where beats is a shortcut)
      beats = undefined, // 🧧 Should this be deprecated?
      attack = 0.01, // How quickly the sound starts.
      decay = 0.9, // A multiplier for when the sound fades.
      volume,
      pan = 0,
      generator = null, // Custom waveform generator function for type "custom"
      toneShift = 0, // Hz shift to add to the tone frequency
    } = {}) {
      // Debug: Log synth function call with toneShift
      if (toneShift !== 0) {
        console.log(`🎵 DISK DEBUG: synth() called with toneShift: ${toneShift}Hz, tone: ${tone}`);
      }
      
      const id = soundId;
      if (volume === undefined) volume = 1;
      if (duration === "🔁") duration = Infinity; // First emoji in the API. 24.07.03.02.26
      if (beats === undefined && duration !== undefined)
        beats = (duration * sound.bpm) / 60;

      tone = $sound.freq(tone);
      // Apply toneShift to the final frequency
      if (toneShift !== 0) {
        const originalTone = tone;
        tone += toneShift;
        console.log(`🎵 DISK DEBUG: Applied toneShift ${toneShift}Hz: ${originalTone}Hz -> ${tone}Hz`);
      }
      // console.log("⛈️ Tone:", tone);
      // Add generator to sound data for custom type
      const soundData = { id, type, tone, beats, attack, decay, volume, pan };
      if (type === "custom" && generator) {
        soundData.generator = generator.toString(); // Convert function to string for postMessage
      }

      sound.sounds.push(soundData);
      soundId += 1n;
      let seconds;
      if (beats === undefined && duration !== undefined) seconds = duration;
      else seconds = (60 / sound.bpm) * beats;
      // console.log("Beats:", beats, "Duration:", duration, "Seconds:", seconds, "BPM:", sound.bpm);

      const end = soundTime + seconds;

      return {
        startedAt: soundTime, // performance.now(),
        id,
        kill: function (fade) {
          sound.kills.push({ id, fade });
        },
        progress: function (time) {
          return 1 - max(0, end - time) / seconds;
        },
        update: function (properties) {
          if (properties.tone) properties.tone = $sound.freq(properties.tone);
          send({
            type: "synth:update",
            content: { id, properties },
          });
        },
        updateGenerator: function (newGenerator) {
          if (type === "custom") {
            send({
              type: "update-generator",
              content: { id, generator: newGenerator.toString() }, // Convert function to string
            });
          }
        },
      };
    };
    $sound.bubble = function ({ radius, rise, volume = 1, pan = 0 } = {}) {
      const id = soundId;
      sound.bubbles.push({ id, radius: radius, rise, volume, pan });
      soundId += 1n;

      return {
        startedAt: soundTime,
        id,
        kill: function (fade) {
          sound.kills.push({ id, fade });
        },
        update: function (properties) {
          send({
            type: "bubble:update",
            content: { id, properties },
          });
        },
      };
    };

    $sound.kill = function (id, fade) {
      sound.kills.push({ id, fade });
    };

    $commonApi.sound = $sound;

    // System beep.
    $commonApi.beep = (tone = 1200) => {
      $sound.synth({
        tone,
        beats: 0.1,
        attack: 0.01,
        decay: 0.5,
        volume: 0.25,
      });
    };

    // Act & Sim (Occurs after first boot and paint, `boot` occurs below.)
    if (booted && paintCount > 0n /*&& !leaving*/) {
      // GC OPTIMIZATION: Reuse cached API object instead of recreating every frame
      let $api = cachedAPI;
      if (!$api || $api._frameInvalid) {
        $api = {};
        keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
        keys($updateApi).forEach((key) => ($api[key] = $updateApi[key]));
        keys(painting.api).forEach((key) => ($api[key] = painting.api[key]));
        $api.api = $api; // Add a reference to the whole API.
        $api._frameInvalid = false;
      }

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      $api.inFocus = inFocus;

      $api.screen = {
        width: content.width,
        height: content.height,
        pixels: screen.pixels,
      };

      $api.cursor = (code) => (cursorCode = code);

      // 📻 Signaling
      $api.signal = (content) => {
        send({ type: "signal", content });
      };

      // Deprecated on 23.07.01.15.31 (Remove later if no regressions.)
      // if (initialSim) {
      //   console.log("initial", initialSim, content.updateCount, 'paintcount', paintCount);
      //   simCount += 1n;
      //   $api.simCount = simCount;
      //   try {
      //     sim($api);
      //   } catch (e) {
      //     console.warn("🧮 Sim failure...", e);
      //   }
      //   initialSim = false;
      // } else

      if (content.updateCount > 0 && paintCount > 0n) {
        // Run `sim` the number of times as requested from `bios`.
        for (let i = content.updateCount; i--; ) {
          simCount += 1n;
          $api.simCount = simCount;
          try {
            sim($api);
            noticeTimer?.step(); // Globally tick the noticeTimer if it exists.
            // ⌛ Run through all the global hourglass timers.
            for (let i = hourGlasses.length - 1; i >= 0; i--) {
              hourGlasses[i].step();
              if (hourGlasses[i].complete && !hourGlasses[i].autoFlip)
                hourGlasses.splice(i, 1);
            }
            $api.rec.tapeTimerStep($api);
          } catch (e) {
            console.warn("🧮 Sim failure...", e);
          }
        }
      }

      // 🌟 Act
      // *Device Event Handling*

      // TODO: Shouldn't all these events come in as part of one array to
      //       keep their order of execution across devices?
      // TODO: Could "device" be removed in favor of "device:event" strings and
      //       if needed, a device method?

      // Window Events

      // Reframing the piece... (resizing the window).
      if (reframed === true) {
        $api.event = {
          device: "none",
          is: (e) => e === "reframed",
        };
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
        reframed = false;

        // Global reframings.
        // currentPromptButton?.reposition({
        //   left: 6,
        //   bottom: 6,
        //   screen: $api.screen,
        // });
      }

      // If a disk failed to load, then notify the disk that loaded it
      // by checking to see if loadFailure has anything set.
      if (loadFailure) {
        $api.event = {
          error: loadFailure,
          is: (e) => e === "load-error",
        };
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
        send({ type: "load-failure" });
        loadFailure = undefined;
      }

      // Signaling
      if (signals.length) {
        const data = { signal: signals };
        Object.assign(data, {
          device: "none",
          is: (e) => e === "signal",
        });
        $api.event = data;
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
        signals.length = 0;
      }

      // Keyboard Paste Event
      // if (content.clipboardText) {
      //   const data = { text: content.clipboardText };
      //   Object.assign(data, {
      //     device: "none",
      //     is: (e) => e === "pasted:text",
      //   });
      //   $api.event = data;
      //   try {
      //     act($api);
      //   } catch (e) {
      //     console.warn("️ ✒ Act failure...", e);
      //   }
      // }

      // *** Pen Events ***
      // Ingest all pen input events by running act for each event.
      // TODO: I could also be transforming pen coordinates here...
      // TODO: Keep track of lastPen to see if it changed.
      // GC OPTIMIZATION: Use for loop instead of forEach
      if (content.pen?.events) {
        for (let i = 0; i < content.pen.events.length; i++) {
          const data = content.pen.events[i];
          Object.assign(data, {
            device: data.device,
            is: (e) => {
              let [name, pointer] = e.split(":");
              if (pointer) {
                if (pointer === "any") {
                  return name === data.name;
                } else {
                  return name === data.name && data.index === parseInt(pointer);
                }
              } else {
                return name === data.name && data.isPrimary === true;
              }
            },
          });
        //console.log(data)
        $api.event = data;
        // 🌐🖋️️ Global pen events.
        try {
          // Always check to see if there was a tap on the corner.
          const { event: e, jump, send, sound, system, piece } = $api;
          let originalColor;
          let masked = false;

          if (
            e.is("touch:5") &&
            piece !== "notepat" &&
            piece !== "stample" &&
            piece !== "toss"
          ) {
            sound.synth({
              tone: 1600,
              duration: 0.02,
              attack: 0.01,
              decay: 0.5,
              volume: 0.25,
            });
            system.nopaint.replace(
              cachedAPI,
              graph.cloneBuffer(screen),
              "$creenshot",
            );
            jump("prompt");
          }

          // Corner prompt button.
          currentHUDButton?.act(e, {
            down: () => {
              originalColor = currentHUDTextColor;
              currentHUDScrub = 0;
              currentHUDTextColor = [0, 255, 0];
              send({ type: "keyboard:enabled" }); // Enable keyboard flag.
              send({ type: "keyboard:unlock" });
              $api.needsPaint();

              // Mask unless we are in the camera.
              if ($api.slug !== "camera") masked = true;

              $api.sound.synth({
                tone: 600,
                beats: 0.1,
                attack: 0.01,
                decay: 0.5,
                volume: 0.25,
              });
            },
            push: (btn) => {
              const shareWidth = tf.blockWidth * "share ".length;

              // Dynamic caret width calculation based on prompt length
              // Short prompts (4 chars or less) get a minimal threshold
              // Longer prompts get a more stretched out threshold
              const promptLength = currentHUDTxt.length;
              const baseCaretWidth = tf.blockWidth + 2;
              const maxCaretWidth = tf.blockWidth * 3; // 3 characters worth

              let caretWidth;
              if (promptLength <= 4) {
                // Very short prompts like "line" get minimal threshold
                caretWidth = baseCaretWidth;
              } else if (promptLength <= 8) {
                // Medium prompts get moderate threshold
                caretWidth = Math.floor(baseCaretWidth * 1.5);
              } else {
                // Long prompts get stretched threshold, capped at max
                caretWidth = Math.min(
                  maxCaretWidth,
                  Math.floor(promptLength * tf.blockWidth * 0.25),
                );
              }

              // Don't allow normal push behavior if we've been scrubbing
              if (btn.scrubbing) {
                btn.actions.cancel?.();
                return;
              }

              // Handle left threshold (backspace) case
              if (currentHUDScrub === -caretWidth) {
                // Trigger backspace action using the abstracted function
                triggerBackspaceAction();
                $api.needsPaint();
                masked = true;
                currentHUDScrub = 0;
                return;
              }

              // Check if we're releasing during scrub but not at a threshold
              if (currentHUDScrub > 0 && currentHUDScrub < shareWidth) {
                btn.actions.cancel?.();
                return;
              }
              if (currentHUDScrub < 0 && currentHUDScrub > -caretWidth) {
                btn.actions.cancel?.();
                return;
              }

              $api.sound.synth({
                tone: 1200,
                beats: 0.1,
                attack: 0.01,
                decay: 0.5,
                volume: 0.15,
              });
              if (!labelBack) {
                jump("prompt");
              } else {
                if ($commonApi.history.length > 0) {
                  send({
                    type: "back-to-piece",
                    content: {
                      targetPiece:
                        $commonApi.history[$commonApi.history.length - 1],
                    },
                  });
                } else {
                  jump("prompt");
                }
              }
              $api.needsPaint();
              masked = true;
              currentHUDScrub = 0;
            },
            scrub: (btn) => {
              if (piece === "share") return; // No need to share scrub while in share.

              if (btn.over || currentHUDScrub !== 0) {
                currentHUDScrub += e.delta.x;
                // Mark that we're actively scrubbing
                btn.scrubbing = true;
              }

              const shareWidth = tf.blockWidth * "share ".length;

              // Dynamic caret width calculation based on prompt length
              // Short prompts (4 chars or less) get a minimal threshold
              // Longer prompts get a more stretched out threshold
              const promptLength = currentHUDTxt.length;
              const baseCaretWidth = tf.blockWidth + 2;
              const maxCaretWidth = tf.blockWidth * 3; // 3 characters worth

              let caretWidth;
              if (promptLength <= 4) {
                // Very short prompts like "line" get minimal threshold
                caretWidth = baseCaretWidth;
              } else if (promptLength <= 8) {
                // Medium prompts get moderate threshold
                caretWidth = Math.floor(baseCaretWidth * 1.5);
              } else {
                // Long prompts get stretched threshold, capped at max
                caretWidth = Math.min(
                  maxCaretWidth,
                  Math.floor(promptLength * tf.blockWidth * 0.25),
                );
              }

              // Update button width for positive scrub
              if (currentHUDScrub >= 0) {
                btn.box.w =
                  tf.blockWidth * currentHUDTxt.length + currentHUDScrub;
                // console.log(btn.b);
              }

              // Clamp scrub values within bounds
              if (currentHUDScrub >= shareWidth) {
                currentHUDScrub = shareWidth;
                currentHUDTextColor = [255, 255, 0]; // Yellow for share threshold
              } else if (currentHUDScrub <= -caretWidth) {
                currentHUDScrub = -caretWidth;
                currentHUDTextColor = [255, 255, 0]; // Yellow for backspace threshold
              } else if (btn.scrubbing) {
                // Once scrubbing has started, stay red until threshold is reached
                currentHUDTextColor = [255, 0, 0]; // Red for scrubbing (not at threshold yet)
              } else if (currentHUDScrub === 0) {
                if (btn.over) {
                  currentHUDTextColor = [0, 255, 0]; // Green when hovering (only when not scrubbing)
                } else {
                  currentHUDTextColor = [255, 0, 0]; // Default red
                }
              }
            },
            cancel: () => {
              currentHUDTextColor = originalColor;

              const shareWidth = tf.blockWidth * "share ".length;

              // Dynamic caret width calculation based on prompt length
              // Short prompts (4 chars or less) get a minimal threshold
              // Longer prompts get a more stretched out threshold
              const promptLength = currentHUDTxt.length;
              const baseCaretWidth = tf.blockWidth + 2;
              const maxCaretWidth = tf.blockWidth * 3; // 3 characters worth

              let caretWidth;
              if (promptLength <= 4) {
                // Very short prompts like "line" get minimal threshold
                caretWidth = baseCaretWidth;
              } else if (promptLength <= 8) {
                // Medium prompts get moderate threshold
                caretWidth = Math.floor(baseCaretWidth * 1.5);
              } else {
                // Long prompts get stretched threshold, capped at max
                caretWidth = Math.min(
                  maxCaretWidth,
                  Math.floor(promptLength * tf.blockWidth * 0.25),
                );
              }

              console.log("scrub:", currentHUDScrub, shareWidth, -caretWidth);

              if (currentHUDScrub === shareWidth) {
                $api.sound.synth({
                  tone: 1800,
                  beats: 0.15,
                  attack: 0.01,
                  decay: 0.5,
                  volume: 0.15,
                });
                $api.sound.synth({
                  tone: 1800 / 2,
                  beats: 0.15 * 2,
                  attack: 0.01,
                  decay: 0.5,
                  volume: 0.1,
                });
                // Use tilde separator for proper URL structure: share~(encoded_kidlisp)
                let contentToShare = currentHUDLogicalTxt || currentHUDTxt;
                // Strip "share " prefix if it exists to avoid double-encoding
                if (contentToShare && contentToShare.startsWith("share ")) {
                  contentToShare = contentToShare.substring(6); // Remove "share " prefix
                }

                // Check if the content is already kidlisp source - if so, don't re-encode
                if (lisp.isKidlispSource(contentToShare)) {
                  // Content is already kidlisp, encode it properly for sharing
                  $api.jump(
                    "share~" + lisp.encodeKidlispForUrl(contentToShare),
                  );
                } else {
                  // Content is not kidlisp, share as-is
                  $api.jump("share~" + contentToShare);
                }
                return;
              } else if (currentHUDScrub === -caretWidth) {
                // Trigger backspace action using the abstracted function
                triggerBackspaceAction();
                return;
              }

              // Only cancel and play cancel sound if we haven't reached a threshold
              currentHUDScrub = 0;
              // Reset scrubbing flag on any button
              if (currentHUDButton) currentHUDButton.scrubbing = false;

              // Disable/lock keyboard for normal cancel (not threshold actions)
              // Threshold actions (share/backspace) handle their own keyboard state
              send({ type: "keyboard:disabled" }); // Disable keyboard flag.
              send({ type: "keyboard:lock" });

              $api.needsPaint();
              $api.sound.synth({
                tone: 200,
                beats: 0.1,
                attack: 0.01,
                decay: 0.5,
                volume: 0.15,
              });
            },
            rollover: (btn) => {
              if (btn) {
                send({ type: "keyboard:unlock" });
                if (btn.down) {
                  currentHUDTextColor = [0, 255, 0];
                }
              }
            },
            rollout: (btn) => {
              // console.log("rolled out...");
              // Only change color if the button was actually pressed/down
              if (btn && btn.down) {
                currentHUDTextColor = [200, 80, 80];
              }
              // Don't lock keyboard if we just triggered backspace
              send({ type: "keyboard:lock" });
            },
          });

          if (!masked) act($api); // Run the act function for all pen events.
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      }
    }

      // *** 3D Pen Events ***
      // GC OPTIMIZATION: Use for loop instead of forEach
      if (content.pen3d?.events) {
        for (let i = 0; i < content.pen3d.events.length; i++) {
          const data = content.pen3d.events[i];
          Object.assign(data, {
            is: (e) => {
              let [prefix, event, pointer] = e.split(":");
              if (
                prefix === "3d" &&
                event === data.name &&
                (pointer === undefined || parseInt(pointer) === data.pointer)
              )
                return true;
            },
          });
          $api.event = data;
          try {
            act($api);
          } catch (e) {
            console.warn("️ ✒ Act failure...", e);
          }
        }
      }

      // Ingest all keyboard input events by running act for each event.
      // GC OPTIMIZATION: Use for loop instead of forEach
      if (content.keyboard) {
        for (let i = 0; i < content.keyboard.length; i++) {
          const data = content.keyboard[i];
          Object.assign(data, {
            device: "keyboard",
            is: (e) => {
              const parts = e.split(":");
              if (parts.length > 2) {
                // Check for an exact match if `keyboard:action:?`
                return data.name === e;
              } else {
                // Or a subtring match if `keyboard:action`
                return data.name.indexOf(e) === 0;
              }
            },
          });
          $api.event = data;
          try {
            act($api); // Execute piece shortcut.
          } catch (e) {
            console.warn("️ ✒ Act failure...", e);
          }
        }
      }

      // Ingest all gamepad input events by running act for each event.
      // GC OPTIMIZATION: Use for loop instead of forEach  
      if (content.gamepad) {
        for (let i = 0; i < content.gamepad.length; i++) {
          const data = content.gamepad[i];
          Object.assign(data, {
            device: "gamepad",
            is: (e) => data.name.indexOf(e) === 0,
          });
          $api.event = data;
          try {
            act($api); // Execute piece shortcut.
          } catch (e) {
            console.warn("️ ✒ Act failure...", e);
          }
        }
      }

      // *** Act Alerts *** (Custom events defined in here.)
      // These do not run in the initial loader / preview piece.
      // GC OPTIMIZATION: Use for loop instead of forEach
      for (let i = 0; i < actAlerts.length; i++) {
        const action = actAlerts[i];
        // Check if `name`'s not a string, and if not, attach arbitrary data.
        let name,
          extra = {};
        if (typeof action !== "string") {
          // Make extra be an object with every key on action other than 'name'.
          ({ name, ...extra } = action);
        } else {
          name = action;
        }

        // GC OPTIMIZATION: Reuse event data object when possible
        if (!$api._eventData) {
          $api._eventData = {
            name: null,
            is: null,
            of: null,
          };
        }
        
        $api._eventData.name = name;
        $api._eventData.is = (e) => e === name;
        $api._eventData.of = (e) => name.startsWith(e);
        
        // Add extra properties directly
        const extraKeys = Object.keys(extra);
        for (let j = 0; j < extraKeys.length; j++) {
          const key = extraKeys[j];
          $api._eventData[key] = extra[key];
        }

        $api.event = $api._eventData;
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      }
      //if (actAlerts.length > 0) console.log(actAlerts, booted);
      actAlerts.length = 0; // Clear act alerts.
    }

    // 🖼 Paint
    if (content.needsRender) {
      // NOTE: Using fresh API object for paint to ensure HUD/overlay updates work properly
      const $api = {};
      keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      keys(painting.api).forEach((key) => ($api[key] = painting.api[key]));
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      // Object.assign($api, $commonApi);
      // Object.assign($api, painting.api);

      $api.paintCount = Number(paintCount);

      $api.inFocus = content.inFocus;
      
      // Add cached KidLisp owner info for attribution in HUD
      $api.cachedKidlispOwner = cachedKidlispOwner;

      // Make a screen buffer or resize it automatically if it doesn't exist.

      if (
        !screen ||
        screen.width !== content.width ||
        screen.height !== content.height
      ) {
        // Create the pixel buffer
        let newPixels;
        if (pixels) {
          newPixels = pixels;
        } else {
          newPixels = new Uint8ClampedArray(content.width * content.height * 4);
          
          // Fill with background color if available, otherwise leave as transparent (zeros)
          if (backgroundFillColor) {
            try {
              // Use graph.findColor to resolve the color to RGBA
              let fillColor = graph.findColor(backgroundFillColor);
              // Ensure we have 4 components (RGBA), defaulting alpha to 255 if missing
              if (fillColor.length === 3) {
                fillColor = [fillColor[0], fillColor[1], fillColor[2], 255];
              }
              
              // Fill the buffer with the resolved color
              for (let i = 0; i < newPixels.length; i += 4) {
                newPixels[i] = fillColor[0];     // Red
                newPixels[i + 1] = fillColor[1]; // Green  
                newPixels[i + 2] = fillColor[2]; // Blue
                newPixels[i + 3] = fillColor[3]; // Alpha
              }
            } catch (e) {
              // Silently continue if color resolution fails
            }
          }
        }

        screen = {
          pixels: newPixels,
          width: content.width,
          height: content.height,
          load: function load(name) {
            if (store[name]?.pixels) {
              this.pixels = new Uint8ClampedArray(store[name].pixels);
              this.width = store[name].width;
              this.height = store[name].height;
              $commonApi.resize(this.width, this.height);
              return true;
            } else {
              return false;
            }
          },
          save: function save(name) {
            store[name] = {
              pixels: new Uint8ClampedArray(this.pixels),
              width: this.width,
              height: this.height,
            };
          },
        };

        screen["resized"] = true; // Screen change type.

        // Only initialize depth buffer if CPU 3D rendering is being used
        if (needsCPU3D) {
          graph.depthBuffer.length = screen.width * screen.height;
          graph.depthBuffer.fill(Number.MAX_VALUE);
          depthBufferInitialized = true;
        }

        // Write buffer is rarely used, keep it minimal
        graph.writeBuffer.length = 0;
        // graph.writeBuffer.fill(0);
      }

      // Only clear depth buffer if CPU 3D rendering is active
      if (needsCPU3D && depthBufferInitialized) {
        graph.depthBuffer.fill(Number.MAX_VALUE); // Clear depthbuffer.
      }
      // Write buffer is rarely used, keep it minimal
      graph.writeBuffer.length = 0;

      $api.screen = screen;
      $api.screen.center = { x: screen.width / 2, y: screen.height / 2 };

      $api.fps = function (newFps) {
        send({ type: "fps-change", content: newFps });
      };

      $api.cursor = (code) => (cursorCode = code);

      graph.setBuffer(screen);

      // API Stops being modified here...
      /*if (!$activePaintApi)*/ $activePaintApi = $api;

      // TODO: Set bpm from boot.
      /*
      $api.sound = {
        time: content.time,
        bpm: function (newBPM) {
          if (newBPM) {
            content.bpm[0] = newBPM;
          }
          return content.bpm[0];
        },
      };
       */

      // TODO: Boot's painting is currently bound by whatever dirtyBox gets
      //       set to at the end of `paint`.

      // Run boot only once before painting for the first time.
      if (paintCount === 0n && loading === false) {
        const dark = await store.retrieve("dark-mode"); // Read dark mode.
        if (dark === true || dark === false) $commonApi.dark = dark;

        // System specific preloaders.
        //if ($commonApi?.system?.name === "nopaint" || currentText === "prompt") {

        // Create a new painting if one doesn't already exist.
        if (!store["painting"]) {
          store["painting"] =
            (await store.retrieve("painting", "local:db")) ||
            painting.api.painting(screen.width, screen.height, ($) => {
              $.wipe(64);
            });

          store["painting:resolution-lock"] = await store.retrieve(
            "painting:resolution-lock",
            "local:db",
          );

          store["painting:transform"] = await store.retrieve(
            "painting:transform",
            "local:db",
          );

          addUndoPainting(store["painting"]);
        }

        const sys = $commonApi.system;
        sys.painting = store["painting"];

        // Set the painting record if one is in storage.
        if (!sys.nopaint.recording) {
          sys.nopaint.record =
            (await store.retrieve("painting:record", "local:db")) || [];

          if (sys.nopaint.record.length === 0) {
            $commonApi.system.nopaint.startRecord("new");
          }

          sys.nopaint.recording = sys.nopaint.record.length > 0;
        }

        sys.nopaint.translation =
          store["painting:transform"]?.translation || sys.nopaint.translation;
        sys.nopaint.zoomLevel =
          store["painting:transform"]?.zoom || sys.nopaint.zoomLevel;

        try {
          if (system === "nopaint") nopaint_boot($api);
          await boot($api);
          booted = true;
        } catch (e) {
          console.warn("🥾 Boot failure...", e);
        }
        send({ type: "disk-loaded-and-booted" });
      }

      // Paint a frame, which can return false to enable caching via noPaint and by
      // default returns undefined (assume a repaint).
      // Once paint returns false and noPaint is marked true, `needsPaint` must be called.
      // Note: Always marked false on a disk's first frame.

      let painted = false;
      let dirtyBox;

      // Render a thumbnail instead of the piece.
      if (previewMode) {
        try {
          // Assign a default resolution on first preview,
          // which can be over-ridden using `resolution` inside the
          // `preview` function.
          if (firstPreviewOrIcon) {
            if (currentSearch === "preview") {
              $api.resolution(1200 / 8, 630 / 8, 0);
            } else {
              $api.resolution(
                ...currentSearch
                  .split("=")[1]
                  .split("x")
                  .map((n) => floor(parseInt(n) / 8)),
                0,
              );
            }
            firstPreviewOrIcon = false;
          }

          preview($api);
          painting.paint(true);
          painted = true;
          paintCount += 1n;
        } catch (err) {
          console.warn("🖼️ Preview failure...", err);
          previewMode = false;
          previewOrIconMode = previewMode || iconMode;
        }
      } else if (iconMode) {
        // Render a favicon instead of the piece.
        try {
          if (firstPreviewOrIcon) {
            $api.resolution(128, 128, 0);
            if (currentSearch === "icon") {
              $api.resolution(128, 128, 0);
            } else {
              console.log("Current:", currentSearch);
              $api.resolution(
                ...currentSearch
                  .split("=")[1]
                  .split("x")
                  .map((n) => parseInt(n)),
                0,
              );
            }
            firstPreviewOrIcon = false;
          }
          icon($api);
          painting.paint(true);
          painted = true;
          paintCount += 1n;
        } catch (err) {
          console.warn("🪷 Icon failure...", err);
          iconMode = false;
          previewOrIconMode = previewMode || iconMode;
        }
      }

      // Attempt a paint.
      if (
        previewMode === false &&
        iconMode === false &&
        (noPaint === false || scream || fairies.length > 0) &&
        booted
      ) {
        let paintOut;

        // Restore kidlisp's accumulated pan state from previous frame
        $api.loadpan();

        try {
          // 📓 Bake any painting from the nopaint system before anything else.
          if (system === "nopaint") {
            const np = $api.system.nopaint;
            // No Paint: baking

            if (
              (brush || filter) &&
              $api.pen?.drawing /*&& currentHUDButton.down === false*/
            ) {
              const brushFilterApi = { ...$api };
              if (currentHUDButton.down === false) {
                brushFilterApi.pen = $api.system.nopaint.brush;
                if (brush) {
                  // $api.page($api.system.nopaint.buffer);
                  $api.page($api.system.painting);
                  // 🔥
                  // TODO: Use the pen data here to get an interpolation,
                  // then pan to each interpolated point and repaint.
                  // console.log(
                  //   "🖌️ Brush:",
                  //   brushFilterApi.pen,
                  //   "🖊️ Pen:",
                  //   $api.pen,
                  // );
                  brush(brushFilterApi);
                }
                if (filter) {
                  $api.page($api.system.painting);
                  filter(brushFilterApi);
                }
                $api.page(screen);
              }
            }

            if (np.needsBake === true && bake) {
              $api.page($api.system.painting);
              bake($api);
              $api.page($api.screen);
              np.present($api);
              np.needsBake = false;
            } else if (np.is("painting") || np.needsPresent) {
              np.present($api); // No Paint: prepaint
            }
          } // All: Paint
          paintOut = paint($api); // Returns `undefined`, `false`, or `DirtyBox`.

          // Save kidlisp's accumulated pan state for next frame
          $api.savepan();
          // Reset pan for system UI rendering
          $api.resetpan();
        } catch (e) {
          console.warn("🎨 Paint failure...", e);
        }

        // `DirtyBox` and `undefined` always set `noPaint` to `true`.
        noPaint =
          paintOut === false || (paintOut !== undefined && paintOut !== true);

        // Run everything that was queued to be painted, then devour paintLayers.
        //await painting.paint();

        // Upper layer.
        const { page, layer, ink, needsPaint, pieceCount } = $api;

        page($api.screen); // Make sure we're on the right bufer.

        layer(1000); // Always make sure this stuff draws on top.

        // const piece = $api.slug?.split("~")[0];
        // if (
        //   !previewMode &&
        //   !iconMode &&
        //   !hideLabel &&
        //   system !== "prompt" &&
        //   piece !== "textfence" &&
        //   piece !== "bleep" &&
        //   piece !== undefined &&
        //   piece.length > 0 &&
        //   piece !== "painting" &&
        //   pieceCount > 0
        // ) {
        // currentPromptButton =
        //   currentPromptButton ||
        //   new $api.ui.TextButton("Back", {
        //     left: 6,
        //     bottom: 6,
        //     screen: $api.screen,
        //   });
        // currentPromptButton.paint($api);
        // }

        // 😱 Scream - Paint a scream if it exists.
        // TODO: Should this overlay after the fact and not force a paint? 23.05.23.19.21
        //       Yes probably, because of layering issues?
        if (scream || screaming) {
          ink("yellow").write(scream, { x: 6, y: 18 }, "red");
          //ink("red").write(scream, { x: 6 + 1, y: 18 + 1 });

          /*
          ink(255)
            .wipe(255, 0, 0)
            .write(
              scream,
              { center: "xy", size: 3, thickness: 1 },
              undefined,
              $api.screen.width - 8,
              needsPaint(),
            );
          */
          if (!screaming) {
            screaming = true;
            clearTimeout(screamingTimer);
            screamingTimer = setTimeout(() => {
              screaming = false;
              scream = null;
            }, 1000);
          }
        }

        // 🧚 Ambient Pen Points - Paint if they exist.
        fairies.forEach(({ x, y }) => {
          ink().point(x * screen.width, y * screen.height);
        });
        if (fairies.length > 0) {
          needsPaint();
          // if (system === "nopaint") $api.system.nopaint.needsPresent = true;
        }
        fairies.length = 0;

        // 🔴 Show a cross-piece "Recording" indicator.
        //    Currently only implemented for `painting:record`. 23.08.20.21.36
        if (
          $api.system.nopaint.recording &&
          !hideLabel &&
          pieceHistoryIndex > -1 &&
          !loading
        ) {
          // ink("red").box(screen.width - 3, 1, 2);
        }

        // Show a notice if necessary.
        // Commented out - prompt character preview in HUD label replaces center preview
        // if (notice) {
        //   ink(noticeColor[0])
        //     //.pan(help.choose(-1, 0, 1), help.choose(-1, 0, 1))
        //     .write(
        //       notice,
        //       { center: "xy", size: 2 },
        //       // { center: "x", y: 32, size: 2 },
        //       noticeColor[1],
        //       $api.screen.width - 8,
        //       noticeOpts?.wrap === "char" ? false : true,
        //     );
        //   //.unpan();
        // }

        layer(0);

        painting.paint(true);
        painted = true;
        paintCount = paintCount + 1n;

        if (paintOut) dirtyBox = paintOut;

        delete screen.resized; // Remove status from screen after painting.
        delete screen.created;

        //console.log("bake")
        //send({ type: "3d-bake" });
      }

      // 🏷️ corner-label: Draw any Global UI / HUD in an overlay buffer that will get
      //           composited by the other thread.

      // TODO: ❤️‍🔥 Why is this being composited by a different thread?
      //       Also... where do I put a scream?

      // System info label (addressability).
      // Use persistent module-level cache instead of local variable
      let label = hudLabelCache;
      
      // Cache invalidation: track state that affects HUD label appearance
      const cacheDisabled = isHUDLabelCacheDisabled();
      const currentHUDState = {
        text: currentHUDTxt,
        scrub: currentHUDScrub,
        textColor: JSON.stringify(currentHUDTextColor),
        supportsInlineColor: $commonApi?.hud?.supportsInlineColor,
        disablePieceNameColoring: $commonApi?.hud?.disablePieceNameColoring,
        // Track frame-based changes for animation/timing-sensitive content
        frameHash: $commonApi?.hud?.frameHash || 0,
        // Add timestamp to force invalidation when caching is disabled
        timestamp: cacheDisabled ? performance.now() : 0
      };
      
      // Invalidate label cache if any visual state has changed
      // Or if caching is globally disabled from BIOS
      const stateChanged = label && label.lastHUDState && 
                          JSON.stringify(currentHUDState) !== JSON.stringify(label.lastHUDState);
      
      // Enhanced debug logging for cache invalidation with detailed comparison
      // Removed debug logs since HUD syntax highlighting is now working correctly
      
      if (!label || 
          !label.lastHUDState || 
          cacheDisabled ||
          stateChanged) {
        
        // Removed debug logs since HUD caching is working correctly
        label = null; // Clear cached label to force regeneration
        hudLabelCache = null; // Clear module-level cache too
      }
      
      const piece = currentHUDTxt?.split("~")[0];
      const defo = 6; // Default offset

      // Removed debug logs since HUD generation is working correctly

      if (
        !previewMode &&
        !iconMode &&
        !hideLabel &&
        piece !== undefined &&
        piece.length > 0
      ) {
        try {
          // Removed debug logs since HUD generation is working correctly
          
          // Use the actual rendered width and height from text.box, not a naive estimate
          // For label sizing, use the color-code-stripped text to avoid false wrapping
          let cleanText = currentHUDTxt.replace(
            /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/g,
            "",
          );
          
          // Removed debug logs since text processing is working correctly
          
          const labelBounds = $api.text.box(
            cleanText,
            undefined,
            $api.screen.width - $api.typeface.blockWidth,
          );
          
          // Removed debug logs since label bounds calculation is working correctly
        // Use the actual width of the longest rendered line
        let w = Math.max(
          ...labelBounds.lines.map((lineArr) => {
            // Preserve spaces when joining words on a line, matching the rendering logic
            return (
              (Array.isArray(lineArr) ? lineArr.join(" ") : lineArr).length *
              tf.blockWidth
            );
          }),
        );

        // Adjust width based on scrub direction
        if (currentHUDScrub >= 0) {
          w += currentHUDScrub; // Positive scrub extends to the right
        } else {
          // Negative scrub: add space for sliding caret animation
          // Reduce the extra space to prevent clipping and make caret start closer
          w += tf.blockWidth * 3; // Add space for the caret box and sliding range (reduced from 6 to 3)
        }
        // Use the actual number of rendered lines for height
        // Use only the actual text height for a tighter fit (no extra margin)
        const scale = 1; // HUD label always uses scale 1
        // Match the text.box rendering logic: blockHeight = (tf.blockHeight + 1) * scale
        const h = labelBounds.lines.length * (tf.blockHeight + 1) * scale;
        // Don't force full width for video piece - let it size naturally for "|" label
        // if (piece === "video") w = screen.width;
        label = $api.painting(w, h, ($) => {
          // Ensure label renders with clean pan state
          $.resetpan();

          // Clear the entire label area first to prevent artifacts
          $.ink([0, 0, 0, 0]).box(0, 0, w, h); // Transparent clear

          // Always use the original currentHUDTxt for the HUD label, preserving all color codes and user content
          let text = currentHUDTxt;

          // Check if scrub has reached max threshold (share width) to disable syntax coloring
    const shareWidth = tf.blockWidth * "share ".length;
    const disableSyntaxColoring = currentHUDScrub >= shareWidth;
    
          // Detect inline color codes (e.g., \\yellow\\c\\red\\d\\rgb(255,20,147)\\e), but disable if at max scrub
          const colorRegexTest = /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/g.test(text);
          const hasInlineColor =
            !disableSyntaxColoring && colorRegexTest;

          // Draw a visible background box for debugging the label bounds
          // $.ink([0,0,0,64]).box(0, 0, w, h); // semi-transparent black - commented out to remove backdrop
          // Draw shadow/outline in black, but always strip color codes for the shadow
          function stripColorCodes(str) {
            return str.replace(
              /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/g,
              "",
            );
          }
          // For the shadow, always strip color codes and disable inline color processing
          $.ink(0).write(
            stripColorCodes(text), // Always strip color codes for shadow
            {
              x: 1 + (currentHUDScrub >= 0 ? currentHUDScrub : 0),
              y: 1,
              noInlineColor: true,
            }, // Add flag to disable inline color processing
            undefined,
            $api.screen.width - $api.typeface.blockWidth,
          );
          // For the foreground, parse color codes and render per-character colors
          const colorCodeRegex =
            /\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/g;
          let charColors = [];
          let currentColor = null;

          // Use stripColorCodes result for consistent rendering (same as shadow)
          const renderText = hasInlineColor ? stripColorCodes(text) : text;

          // Calculate text width for caret positioning - handle multiline properly
          let textWidth, animationY;
          if (labelBounds.lines.length > 1) {
            // For multiline text, use the width of the last line
            const lastLineArray =
              labelBounds.lines[labelBounds.lines.length - 1];
            const lastLineText = Array.isArray(lastLineArray)
              ? lastLineArray.join(" ")
              : lastLineArray;
            textWidth = lastLineText.length * tf.blockWidth;
            animationY = (labelBounds.lines.length - 1) * (tf.blockHeight + 1);
          } else {
            // For single line text, use total width
            textWidth = renderText.length * tf.blockWidth;
            animationY = 0;
          }

          // Build color array by stepping through the original text and mapping colors to cleaned positions
          let cleanIndex = 0;
          let originalIndex = 0;

          // Helper function to convert color names to RGB arrays
          function colorNameToRGB(colorName) {
            if (!colorName) return null;
            // Use the graph's parseColor if available
            if ($api.graph && $api.graph.parseColor) {
              try {
                const rgb = $api.graph.parseColor([colorName]);
                if (rgb && Array.isArray(rgb) && rgb.length >= 3) {
                  return [rgb[0], rgb[1], rgb[2]]; // Return just RGB without alpha
                }
              } catch (e) {
                // Fall through to fallback if parseColor fails
              }
            }
            // Fallback color mappings
            const colorMap = {
              white: [255, 255, 255],
              black: [0, 0, 0],
              red: [255, 0, 0],
              green: [0, 255, 0],
              blue: [0, 0, 255],
              yellow: [255, 255, 0],
              cyan: [0, 255, 255],
              magenta: [255, 0, 255],
              orange: [255, 165, 0],
              purple: [128, 0, 128],
              pink: [255, 192, 203],
              brown: [165, 42, 42],
              gray: [128, 128, 128],
              grey: [128, 128, 128],
              goldenrod: [218, 165, 32],
            };
            return colorMap[colorName.toLowerCase()] || [255, 255, 255];
          }

          // Parse color codes and build character color array
          while (originalIndex < text.length && cleanIndex < cleanText.length) {
            // Check if we're at the start of a color code
            if (text[originalIndex] === "\\") {
              // Find the end of the color code
              const colorMatch = text
                .slice(originalIndex)
                .match(/^\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+(?:,[0-9]+)?)\\/);
              if (colorMatch) {
                // Update current color (convert to RGB)
                const colorName = colorMatch[1];
                // Check if it's an RGB/RGBA value like "255,20,147" or "128,128,128,64"
                if (colorName.includes(",")) {
                  const rgbaValues = colorName
                    .split(",")
                    .map((v) => parseInt(v.trim()));
                  if (
                    (rgbaValues.length === 3 || rgbaValues.length === 4) &&
                    rgbaValues.every((v) => !isNaN(v) && v >= 0 && v <= 255)
                  ) {
                    currentColor = rgbaValues;
                  } else {
                    currentColor = colorNameToRGB(colorName);
                  }
                } else {
                  currentColor = colorNameToRGB(colorName);
                }
                // Skip over the color code
                originalIndex += colorMatch[0].length;
                continue;
              }
            }

            // This is a regular character, assign the current color
            charColors[cleanIndex] = currentColor;
            cleanIndex++;
            originalIndex++;
          }

          // Fill any remaining positions with the current color
          while (cleanIndex < cleanText.length) {
            charColors[cleanIndex] = currentColor;
            cleanIndex++;
          }

          // Override piece name prefix colors to use currentHUDTextColor
          // Find the piece name (everything before the first space) and force it to use HUD color
          // Check if piece name coloring should be disabled (for custom syntax highlighting)
          const spaceIndex = renderText.indexOf(" ");
          if (disableSyntaxColoring) {
            // When syntax coloring is disabled, use currentHUDTextColor for all text
            for (let i = 0; i < renderText.length; i++) {
              charColors[i] = currentHUDTextColor || [255, 200, 240];
            }
          } else if (!$commonApi?.hud?.disablePieceNameColoring) {
            // Only override piece name colors if not explicitly disabled
            if (spaceIndex > 0) {
              // Override colors for the piece name (0 to spaceIndex) to use HUD color
              for (let i = 0; i < spaceIndex; i++) {
                charColors[i] = currentHUDTextColor || [255, 200, 240]; // Use actual HUD color
              }
            } else if (renderText.length > 0) {
              // If no space found, the entire text is the piece name
              for (let i = 0; i < renderText.length; i++) {
                charColors[i] = currentHUDTextColor || [255, 200, 240]; // Use actual HUD color
              }
            }
          }

          // Patch: ensure charColors matches renderText length
          if (charColors.length < renderText.length) {
            // Pad with last color
            const padColor =
              charColors.length > 0 ? charColors[charColors.length - 1] : null;
            while (charColors.length < renderText.length)
              charColors.push(padColor);
          } else if (charColors.length > renderText.length) {
            // Truncate if too long (should not happen)
            charColors = charColors.slice(0, renderText.length);
          }

          // Handle multi-line rendering with proper color mapping
          if (hasInlineColor) {
            // Build a comprehensive character-to-color mapping for the cleaned text
            const textCharColors = new Array(renderText.length);
            
            // Parse color codes from the original text and map to renderText positions
            let renderIndex = 0;
            let originalIndex = 0;
            let currentColor = null;
            
            while (originalIndex < text.length) {
              // Check if we're at the start of a color code
              if (text[originalIndex] === "\\") {
                const colorMatch = text
                  .slice(originalIndex)
                  .match(/^\\([a-zA-Z]+(?:\([^)]*\))?|[0-9]+,[0-9]+,[0-9]+)\\/);
                if (colorMatch) {
                  // Update current color
                  const colorName = colorMatch[1];
                  if (colorName.includes(",")) {
                    const rgbValues = colorName
                      .split(",")
                      .map((v) => parseInt(v.trim()));
                    if (
                      rgbValues.length === 3 &&
                      rgbValues.every((v) => !isNaN(v) && v >= 0 && v <= 255)
                    ) {
                      currentColor = rgbValues;
                    } else {
                      currentColor = colorNameToRGB(colorName);
                    }
                  } else {
                    currentColor = colorNameToRGB(colorName);
                  }
                  // Skip over the color code
                  originalIndex += colorMatch[0].length;
                  continue;
                }
              }
              
              // This is a regular character - assign color and advance render index only if within bounds
              if (renderIndex < renderText.length) {
                textCharColors[renderIndex] = currentColor;
                renderIndex++;
              }
              originalIndex++;
            }
            
            // Fill any remaining positions with the current color
            while (renderIndex < renderText.length) {
              textCharColors[renderIndex] = currentColor;
              renderIndex++;
            }

            // Now render each line using the text layout from labelBounds
            let textPosition = 0;
            
            for (let lineIndex = 0; lineIndex < labelBounds.lines.length; lineIndex++) {
              const lineArray = labelBounds.lines[lineIndex];
              const lineText = Array.isArray(lineArray) ? lineArray.join(" ") : lineArray;
              const lineY = lineIndex * (tf.blockHeight + 1);
              
              // Extract colors for this specific line
              const lineColors = [];
              for (let i = 0; i < lineText.length; i++) {
                if (textPosition + i < textCharColors.length) {
                  lineColors.push(textCharColors[textPosition + i]);
                } else {
                  lineColors.push(currentColor); // Fallback to last color
                }
              }
              
              // Advance position past this line's characters
              textPosition += lineText.length;
              
              // Skip whitespace that was consumed by line breaking
              while (textPosition < renderText.length && /\s/.test(renderText[textPosition])) {
                textPosition++;
              }

              // Render the line with per-character colors
              if (tf?.print) {
                tf.print(
                  $,
                  { x: currentHUDScrub >= 0 ? currentHUDScrub : 0, y: lineY },
                  0,
                  lineText,
                  undefined,
                  lineColors,
                );
              } else {
                // Fallback if tf.print doesn't exist
                if (debugColorParsing) {
                  console.log("🎨 Fallback rendering (no tf.print)");
                }
                $.ink(currentHUDTextColor || [255, 200, 240]).write(lineText, {
                  x: currentHUDScrub >= 0 ? currentHUDScrub : 0,
                  y: lineY,
                });
              }
            }
          } else {
            // Fallback to regular rendering without color codes
            const writeOptions = {
              x: currentHUDScrub >= 0 ? currentHUDScrub : 0,
              y: 0,
            };
            // Disable inline color processing when syntax coloring is disabled
            if (disableSyntaxColoring) {
              writeOptions.noInlineColor = true;
            }
            $.ink(currentHUDTextColor || [255, 200, 240]).write(
              disableSyntaxColoring ? stripColorCodes(text) : text,
              writeOptions,
              undefined,
              $api.screen.width - $api.typeface.blockWidth,
            );
          }

          if (currentHUDScrub > 0) {
            const shareWidth = tf.blockWidth * "share ".length;
            // Draw shadow for 'share' in black
            $.ink(0).write("share", {
              x: 1 + currentHUDScrub - shareWidth,
              y: animationY + 1,
            });
            // Draw 'share' in the HUD color
            $.ink(currentHUDTextColor || [255, 200, 240]).write("share", {
              x: 0 + currentHUDScrub - shareWidth,
              y: animationY,
            });
          } else if (currentHUDScrub < 0) {
            // Dissolving particle animation: particles fly from far right, assembling into caret
            // Dynamic caret width calculation based on prompt length
            // Short prompts (4 chars or less) get a minimal threshold
            // Longer prompts get a more stretched out threshold
            const promptLength = currentHUDTxt.length;
            const baseCaretWidth = tf.blockWidth + 2;
            const maxCaretWidth = tf.blockWidth * 3; // 3 characters worth

            let caretThreshold;
            if (promptLength <= 4) {
              // Very short prompts like "line" get minimal threshold
              caretThreshold = baseCaretWidth;
            } else if (promptLength <= 8) {
              // Medium prompts get moderate threshold
              caretThreshold = Math.floor(baseCaretWidth * 1.5);
            } else {
              // Long prompts get stretched threshold, capped at max
              caretThreshold = Math.min(
                maxCaretWidth,
                Math.floor(promptLength * tf.blockWidth * 0.25),
              );
            }

            const scrubProgress = Math.abs(currentHUDScrub) / caretThreshold;
            const clampedProgress = Math.min(scrubProgress, 1);

            const caretAreaWidth = tf.blockWidth; // Use actual typeface block width
            const caretAreaHeight = tf.blockHeight; // Use actual typeface block height
            const particleRange = tf.blockWidth * 12; // Much larger range for dramatic particle effect

            // Clear the entire animation area first
            $.ink([0, 0, 0, 0]).box(
              textWidth - 5,
              animationY - 1,
              caretAreaWidth + particleRange + 20,
              caretAreaHeight + 4,
            );

            // At the threshold, show complete caret block in theme-appropriate color
            if (clampedProgress >= 1) {
              // Use theme-appropriate block color: white for dark theme, black for light theme
              const blockColor = $api.dark ? [255, 255, 255] : [0, 0, 0]; // Theme-appropriate prompt block color

              // Draw complete caret block with shadow
              $.ink([0, 0, 0, 128]).box(
                textWidth + 1,
                animationY + 1,
                caretAreaWidth,
                caretAreaHeight,
              ); // Shadow
              $.ink(blockColor).box(
                textWidth,
                animationY,
                caretAreaWidth,
                caretAreaHeight,
              ); // Block
            } else {
              // Create particle swarm that flies from far right with improved math
              const totalParticles = Math.floor(
                caretAreaWidth * caretAreaHeight * 1.5,
              ); // Dense particle swarm
              const particlesToShow = Math.floor(
                clampedProgress * totalParticles,
              );

              for (let i = 0; i < particlesToShow; i++) {
                // Use consistent deterministic random for each particle based on index
                const seed = i * 2.3456789; // Different seed for better distribution
                const rnd1 =
                  Math.abs(Math.sin(seed * 12.9898) * 43758.5453) % 1;
                const rnd2 = Math.abs(Math.sin(seed * 78.233) * 43758.5453) % 1;
                const rnd3 = Math.abs(Math.sin(seed * 37.719) * 43758.5453) % 1;
                const rnd4 = Math.abs(Math.sin(seed * 94.673) * 43758.5453) % 1;
                const rnd5 = Math.abs(Math.sin(seed * 15.428) * 43758.5453) % 1;

                // Final destination: randomly distributed across caret area
                const finalX = textWidth + rnd1 * caretAreaWidth;
                const finalY = animationY + rnd2 * caretAreaHeight;

                // Starting position: much further right with more spread
                const startX =
                  textWidth + caretAreaWidth + rnd3 * particleRange + 30;
                const startY = finalY + (rnd4 - 0.5) * caretAreaHeight * 1.5; // More Y variation

                // Each particle has unique timing and speed
                const particleDelay = rnd1 * 0.4; // Stagger particle launches over 40% of animation
                const particleSpeed = 0.8 + rnd5 * 0.4; // Speed varies from 0.8 to 1.2

                // Check if this particle should be active yet
                if (clampedProgress > particleDelay) {
                  const particleAge = clampedProgress - particleDelay;
                  const normalizedAge = particleAge / (1.0 - particleDelay); // Normalize to 0-1
                  const movementProgress = Math.min(
                    normalizedAge * particleSpeed,
                    1,
                  );

                  // Smooth easing for more natural movement (ease-out)
                  const easedProgress = 1 - Math.pow(1 - movementProgress, 2);

                  // Current position with smooth interpolation
                  const currentX = startX - (startX - finalX) * easedProgress;
                  const currentY = startY + (finalY - startY) * easedProgress;

                  // Distance-based alpha for particle trail effect
                  const totalDistance = startX - finalX;
                  const remainingDistance = Math.abs(currentX - finalX);
                  const proximityFactor = 1 - remainingDistance / totalDistance;

                  // Multi-layer alpha calculation for natural fading
                  const baseAlpha = Math.min(255, proximityFactor * 220 + 35);
                  const speedAlpha = Math.min(255, easedProgress * 160 + 60);
                  const randomAlpha = 200 + rnd4 * 55; // Slight alpha variation per particle
                  const finalAlpha = Math.min(
                    255,
                    Math.max(baseAlpha, speedAlpha) * (randomAlpha / 255),
                  );

                  // Color with slight variation for more organic look
                  const redChannel = Math.floor(220 + rnd5 * 35); // Red varies 220-255
                  const particleColor = [redChannel, 0, 0, finalAlpha];

                  // Draw the particle
                  $.ink(particleColor).point(
                    Math.floor(currentX),
                    Math.floor(currentY),
                  );

                  // Add particle trails for particles that are moving fast
                  if (
                    easedProgress > 0.1 &&
                    easedProgress < 0.9 &&
                    rnd3 > 0.6
                  ) {
                    const trailX = currentX + (startX - currentX) * 0.1; // Slight trail behind
                    const trailAlpha = finalAlpha * 0.3;
                    $.ink([redChannel, 0, 0, trailAlpha]).point(
                      Math.floor(trailX),
                      Math.floor(currentY),
                    );
                  }

                  // When particles are very close to destination, add clustering effect
                  if (easedProgress >= 0.85) {
                    const clusterSize = Math.floor(rnd2 * 3) + 1; // 1-3 pixels cluster
                    for (let c = 0; c < clusterSize; c++) {
                      const clusterX = Math.floor(finalX) + (c % 2);
                      const clusterY = Math.floor(finalY) + Math.floor(c / 2);
                      const clusterAlpha = Math.min(255, finalAlpha + 40);
                      $.ink([255, 0, 0, clusterAlpha]).point(
                        clusterX,
                        clusterY,
                      );
                    }
                  }
                }
              }
            }
          }
        });

        // Store current state in the label object for cache invalidation
        // Always store state for comparison logic, even when caching is disabled
        if (label) {
          label.lastHUDState = currentHUDState;
          hudLabelCache = label; // Persist to module-level cache
        }

        // Video piece should be flush left (0px) but keep vertical offset
        // Use $commonApi.piece instead of piece (which comes from HUD text)
        if ($commonApi.piece === "video") currentHUDOffset = { x: 0, y: 6 };
        if (!currentHUDOffset) currentHUDOffset = { x: defo, y: defo };

        currentHUDButton =
          currentHUDButton ||
          new $api.ui.Button({
            x: 0,
            y: 0,
            w: w + currentHUDOffset.x,
            h: h + currentHUDOffset.y,
          });

        // $commonApi.hud.currentLabel = {
        //   text: currentHUDTxt,
        //   btn: currentHUDButton,
        // };
        } catch (error) {
          console.error('🏷️ HUD Generation: EXCEPTION occurred:', error);
          label = null; // Ensure label is null on error
        }
      }

      // Return frame data back to the main thread.
      let sendData = { width: screen.width, height: screen.height };

      // CRITICAL: Skip pixel transfer during tape playback to prevent overlay on video
      // BUT: Allow video piece to continue rendering its UI during tape playback
      // Also check for presenting state to handle cached tapes and multiple runs
      const isVideoPiece = currentPath === "video" || currentPath === "aesthetic.computer/disks/video";
      const skipPixelsDuringTapePlayback = !isVideoPiece && ($commonApi.rec.playing || $commonApi.rec.presenting);

      // Tack on the tape progress bar pixel buffer if necessary.
      // Skip progress bar during tape playback to avoid overlay conflicts
      if (!skipPixelsDuringTapePlayback && ($api.rec.tapeProgress || ($api.rec.recording && $api.rec.tapeTimerDuration))) {
        const progress = $api.rec.tapeProgress || 0;
        
        // IMPORTANT: Access the main screen buffer OUTSIDE the painting context
        // because inside painting(), $api.screen refers to the painting's own buffer, not the main screen
        const mainScreenPixels = screen.pixels; // This is the actual frame content
        const mainScreenWidth = screen.width;
        const mainScreenHeight = screen.height;
        
        const currentProgressWidth = Math.floor(mainScreenWidth * progress);
        
        // Create tape progress bar painting with VHS-style red glow
        const tapeProgressBarPainting = $api.painting(mainScreenWidth, 1, ($) => {
          // Animation frame for VHS effects - increased speed for more vibes
          const animFrame = Number($api.paintCount || 0n);
          
          // Special color override for first and last frames
          const isFirstFrame = progress <= 0.01; // First 1% of progress
          const isLastFrame = progress >= 0.99;  // Last 1% of progress
          
          // Calculate smooth alpha fade - progress bar fades from 25% to 75% for longer clean content viewing
          let progressBarAlpha = 1.0; // Default to fully visible
          
          if (progress >= 0.20 && progress <= 0.30) {
            // Fade out from 20% to 30% (10% fade-out period)
            progressBarAlpha = 1.0 - ((progress - 0.20) / 0.10);
          } else if (progress > 0.30 && progress < 0.70) {
            // Fully hidden from 30% to 70% (40% hidden period)
            progressBarAlpha = 0.0;
          } else if (progress >= 0.70 && progress <= 0.80) {
            // Fade in from 70% to 80% (10% fade-in period)
            progressBarAlpha = (progress - 0.70) / 0.10;
          }
          
          // Draw VHS-style progress bar pixel by pixel
          for (let x = 0; x < mainScreenWidth; x++) {
            let baseR, baseG, baseB;
            
            if (x < currentProgressWidth) {
              // FILLED AREA - VHS red with analog glow and scan lines
              
              if (isFirstFrame) {
                // FIRST FRAME - Fully green across entire bar
                baseR = 0;
                baseG = 255;
                baseB = 0;
              } else if (isLastFrame) {
                // LAST FRAME - Fully red across entire bar
                baseR = 255;
                baseG = 0;
                baseB = 0;
              } else {
                // NORMAL FRAMES - VHS red with effects
                // Base VHS red intensity - brighter and more consistent
                let redIntensity = 255;
                
                // Enhanced VHS scan line effect with more movement
                const scanLine = Math.sin(animFrame * 0.5 + x * 0.6) * 0.15 + 0.85;
                
                // Intensified analog glow effect - more pronounced waves
                const glowPhase = (animFrame * 0.15 + x * 0.12) % (Math.PI * 2);
                const analogGlow = Math.sin(glowPhase) * 0.2 + 0.8;
                
                // Enhanced VHS tracking distortion with more character
                const tracking = Math.sin(animFrame * 0.08 + x * 0.03) * 0.1 + 0.9;
                
                // Secondary glow wave for more complexity
                const secondaryGlow = Math.sin(animFrame * 0.25 + x * 0.2) * 0.1 + 0.9;
                
                // Combine all VHS effects with brighter base
                redIntensity = Math.floor(redIntensity * scanLine * analogGlow * tracking * secondaryGlow);
                
                // Bright VHS red color - keep it pure red, no orange bleeding
                baseR = Math.max(200, Math.min(255, redIntensity));
                
                // Pure red - minimal green and blue for clean bright red
                baseG = Math.floor(baseR * 0.05); // Very minimal green
                baseB = Math.floor(baseR * 0.02); // Almost no blue
                
                // Enhanced VHS bloom effect - pure red blooms only
                if (Math.random() < 0.025) { // 2.5% chance of bloom
                  const bloomIntensity = 0.8 + Math.random() * 0.2;
                  baseR = Math.floor(255 * bloomIntensity);
                  baseG = Math.floor(20 * bloomIntensity); // Keep bloom red too
                  baseB = Math.floor(10 * bloomIntensity);
                }
              }
              
            } else {
              // EMPTY AREA - Enhanced VHS static/noise background
              
              // More pronounced VHS static with additional frequency layers
              const staticNoise = Math.sin(animFrame * 1.2 + x * 0.2) * 0.04 + 0.06;
              const staticNoise2 = Math.sin(animFrame * 0.7 + x * 0.3) * 0.02 + 0.03;
              
              // More frequent static sparkles for increased vibes
              if (Math.random() < 0.002) { // 0.2% chance of static
                baseR = Math.floor(50 * Math.random());
                baseG = Math.floor(25 * Math.random());
                baseB = Math.floor(15 * Math.random());
              } else {
                // Enhanced static background with more character
                baseR = Math.floor(25 * (staticNoise + staticNoise2));
                baseG = Math.floor(12 * (staticNoise + staticNoise2));
                baseB = Math.floor(8 * (staticNoise + staticNoise2));
              }
            }
            
            // SINGLE bright leading pixel - allow it to go beyond the edge when progress is at 100%
            // Check if this should be the leader pixel (current progress position, even if beyond screen)
            const leaderPixelPosition = Math.floor(mainScreenWidth * progress);
            const isLeaderPixel = x === leaderPixelPosition || (leaderPixelPosition >= mainScreenWidth && x === mainScreenWidth - 1);
            
            if (isLeaderPixel) {
              if (isFirstFrame) {
                // First frame - bright green leader
                baseR = 0;
                baseG = 255;
                baseB = 0;
              } else if (isLastFrame) {
                // Last frame - bright red leader  
                baseR = 255;
                baseG = 0;
                baseB = 0;
              } else {
                // Check if we're in the fade period (20%-80%) to enable special blinking
                const isInFadePeriod = progress >= 0.20 && progress <= 0.80;
                
                if (isInFadePeriod) {
                  // During fade period - cycle through yellow, lime, and other colors for visibility
                  const colorCycle = Math.floor(animFrame * 0.3) % 4; // Slower color cycling
                  
                  switch (colorCycle) {
                    case 0:
                      baseR = 255; baseG = 255; baseB = 0; // Yellow
                      break;
                    case 1:
                      baseR = 0; baseG = 255; baseB = 0; // Lime
                      break;
                    case 2:
                      baseR = 255; baseG = 128; baseB = 0; // Orange
                      break;
                    case 3:
                      baseR = 0; baseG = 255; baseB = 255; // Cyan
                      break;
                  }
                } else {
                  // Normal behavior outside fade period - super bright white-hot leader with pulsing
                  const leaderPulse = Math.sin(animFrame * 0.6) * 0.2 + 0.8;
                  
                  baseR = 255;
                  baseG = Math.floor(255 * leaderPulse); // Bright white-hot leader
                  baseB = Math.floor(255 * leaderPulse);
                }
              }
            }
            
            // Apply alpha fade to final colors, with beacon-like leader pixel
            let finalAlpha = progressBarAlpha;
            if (isLeaderPixel) {
              if (progress >= 0.20 && progress <= 0.80) {
                // During fade period - beacon-like signal marker (strong but not full opacity)
                finalAlpha = 0.8; // Bright beacon for progress indication
              } else {
                // Outside fade period - still visible beacon
                finalAlpha = Math.min(progressBarAlpha, 0.9); // Strong beacon at 90% opacity
              }
            }
            
            // Use proper alpha blending
            $.ink(baseR, baseG, baseB, Math.floor(finalAlpha * 255)).box(x, 0, 1, 1);
          }
        });
        
        // Ensure the painting was created successfully before adding to sendData
        if (tapeProgressBarPainting && tapeProgressBarPainting.pixels && tapeProgressBarPainting.pixels.length > 0) {
          // Structure the data to match what bios.mjs expects (same as label format)
          sendData.tapeProgressBar = {
            x: 0,
            y: $api.screen.height - 1, // Position at bottom of screen (1px tall)
            img: {
              width: tapeProgressBarPainting.width,
              height: tapeProgressBarPainting.height,
              pixels: tapeProgressBarPainting.pixels
            }
          };
        } else {
          console.warn("🎬 Tape progress bar painting FAILED to create:", {
            painting: !!tapeProgressBarPainting,
            pixels: !!tapeProgressBarPainting?.pixels,
            pixelsLength: tapeProgressBarPainting?.pixels?.length
          });
        }
      } else {
        // Debug: Log when tape progress is 0 or undefined during recording
        if ($api.rec.recording && false) { // Disabled verbose logging
          // console.log("🎬 Recording active but no tapeProgress:", {
          //   recording: $api.rec.recording,
          //   tapeProgress: $api.rec.tapeProgress,
          //   tapeTimerStart: $api.rec.tapeTimerStart,
          //   tapeTimerDuration: $api.rec.tapeTimerDuration
          // });
        }
      }

      // Add the duration progress bar if duration tracking is active
      // This shows a 1px bar at the bottom similar to tape, but for timed pieces
      // NOTE: Moved this after screen.pixels is ready for proper pixel sampling
      // const durationProgressBarData = renderDurationProgressBar($api);
      // if (durationProgressBarData && !skipPixelsDuringTapePlayback) {
      //   sendData.durationProgressBar = durationProgressBarData;
      // }

      maybeLeave();

      // TODO: Write this up to the data in `painting`.
      sendData.TwoD = { code: twoDCommands };

      // Attach a label buffer if necessary.
      if (label) {
        const hasVisiblePixels = checkForVisiblePixels(label.pixels);
        if (hasVisiblePixels) {
          sendData.label = {
            x: currentHUDOffset.x,
            y: currentHUDOffset.y,
            img: (({ width, height, pixels }) => ({ width, height, pixels }))(
              label,
            ),
          };
        }
      }      // 🔲 Generate QR code overlay for KidLisp pieces
      let qrOverlay;
      
      // Clear QR cache if caching is disabled to prevent memory buildup
      if (isQROverlayCacheDisabled() && qrOverlayCache.size > 0) {
        qrOverlayCache.clear();
      }
      
      // Detect if this is a KidLisp piece
      const sourceCode = currentHUDLogicalTxt || currentHUDTxt; // Get the source from HUD text
      
      // More inclusive KidLisp detection for QR code generation
      const isKidlispPiece = (currentPath && lisp.isKidlispSource(currentPath)) || 
                             currentPath === "(...)" ||
                             (sourceCode && (
                               sourceCode.startsWith("(") || 
                               sourceCode.startsWith(";") ||
                               sourceCode.includes("\n") ||
                               // Check for common KidLisp commands (even single line)
                               /^(wipe|ink|line|box|circle|rect|def|later|scroll|resolution|gap|frame|brush|clear|cls|help|reset|dot|pixel|stamp|paste|copy|move|rotate|scale|translate|fill|stroke|point|arc|bezier|noise|random|sin|cos|tan|sqrt|abs|floor|ceil|round|min|max|pow|log|exp|atan2|dist|lerp|map|norm|constrain|hue|sat|bright|alpha|red|green|blue|rgb|hsb|gray|background|foreground|text|font|size|width|height|mouseX|mouseY|keyCode|key|frameCount|time|second|minute|hour|day|month|year|millis|fps|deltaTime)\s/.test(sourceCode)
                             ));
      
      
      if (isKidlispPiece && sourceCode) {
        try {
          // Check if this source has been cached using the global registry
          const cachedCode = getCachedCode(sourceCode);
          if (cachedCode) {
            // Send the cached code to main thread for tape naming
            send({ type: "kidlisp:cached-code", content: cachedCode });
            
            // Use cache key based on cached code to avoid regenerating the same QR
            const cacheKey = `qr_${cachedCode}`;
            
            // Get the font before checking cache to ensure text will render properly
            const font = typefaceCache.get("MatrixChunky8");
            
            // Check if all characters are loaded in the font
            let allCharsLoaded = false;
            if (font && font.glyphs) {
              allCharsLoaded = true;
              for (const char of cachedCode) {
                const glyph = font.glyphs[char];
                // For BDF fonts like MatrixChunky8, check if glyph has pixels data
                // For other fonts, check for commands or pixels
                const isValidGlyph = glyph && (
                  (glyph.pixels && Array.isArray(glyph.pixels)) ||  // BDF font with pixel data
                  (glyph.commands && Array.isArray(glyph.commands)) ||  // Vector font with commands
                  (glyph.resolution && glyph.pixels)  // Alternative BDF format
                );
                
                if (!isValidGlyph) {
                  allCharsLoaded = false;
                  break;
                }
              }
            }
            
            // Check if this QR overlay is already cached (unless caching is disabled or font not loaded)
            const isQRCacheDisabled = isQROverlayCacheDisabled();
            const hasQRCache = qrOverlayCache.has(cacheKey);
            
            // ALWAYS generate fresh QR overlay to match HUD label live update behavior
            // Force cache bypass to ensure QR text updates live like HUD labels
            const forceCacheBypass = true;
            
            if (!isQRCacheDisabled && allCharsLoaded && hasQRCache && !forceCacheBypass) {
              const cachedQrData = qrOverlayCache.get(cacheKey);
              
              // Recalculate position for current screen size (handles reframing)
              const overlayWidth = cachedQrData.width;
              const overlayHeight = cachedQrData.height;
              const margin = 4;
              const startX = screen.width - overlayWidth - margin;
              const startY = screen.height - overlayHeight - margin;
              
              // Create fresh overlay for transfer from cached data
              qrOverlay = {
                width: cachedQrData.width,
                height: cachedQrData.height,
                pixels: new Uint8ClampedArray(cachedQrData.basePixels) // Fresh copy for transfer
              };
              
              // Add QR overlay to sendData
              sendData.qrOverlay = {
                x: startX,
                y: startY,
                img: qrOverlay
              };
            } else {
              // Always use prompt.ac for QR codes (even in dev/local)
              let url = "https://prompt.ac";
              
              // Use the cached nanoid code with $ prefix for a much shorter URL
              url += `/$${cachedCode}`;
              
              // Generate QR code with medium error correction for better scannability
              const cells = qr(url, { errorCorrectLevel: ErrorCorrectLevel.M }).modules;
              
              // Calculate size and position for bottom-right corner with 4px margin
              const margin = 4;
              const cellSize = 1; // 1 pixel per cell for smallest possible size
              const qrSize = cells.length * cellSize;
            
            // Position in bottom-right corner
            let startX = screen.width - qrSize - margin - 1; // Account for shadow width
            const textHeight = 12; // Space for MatrixChunky8 8px font with shadow (8px + 4px padding)
            const totalHeight = qrSize + textHeight;
            let startY = screen.height - totalHeight - margin; // Move 1px closer to bottom for balanced margins
            
            // Create QR overlay using painting API with extra space for shadow
            const textAreaHeight = 9;
            const qrOffsetY = textAreaHeight; // QR starts right after text area (no gap)
            const canvasHeight = qrOffsetY + qrSize + 2; // Ensure room for bottom shadow
            const generatedQR = $api.painting(qrSize + 1, canvasHeight, ($) => {
              // Draw QR code at offset position to make room for text above
              for (let y = 0; y < cells.length; y++) {
                for (let x = 0; x < cells.length; x++) {
                  const isBlack = cells[y][x];
                  if (isBlack) {
                    $.ink("black");
                  } else {
                    $.ink("white");
                  }
                  $.box(x * cellSize, (y * cellSize) + qrOffsetY, cellSize); // Add qrOffsetY to move QR down
                }
              }
              
              // QR text style configuration
              const useBackdrop = true; // Set to true for backdrop style, false for shadow style
              
              // Prepare text for rendering
              const codeToRender = `$${cachedCode}`;
              
              // Calculate actual rendered width for mathematical centering
              // Get character advances from the font definition
              const advances = font?.data?.advances || typefaceCache.get("MatrixChunky8")?.data?.advances || {};
              
              // Calculate text dimensions and positioning
              let actualTextWidth = 0;
              for (const char of codeToRender) {
                actualTextWidth += advances[char] || 4; // fallback to 4px
              }
              
              // Text area configuration
              const textPaddingLeft = 1; // Left padding
              const textPaddingRight = 0; // No right padding for tighter fit
              const textAreaWidth = actualTextWidth + textPaddingLeft + textPaddingRight;
              const textAreaHeight = 9; // Height reduced by 1px (was 10)
              
              // Position text area above QR code (which is now at qrOffsetY)
              const textAreaX = qrSize - textAreaWidth; // Still flush right
              const textAreaY = 0; // At the top of the canvas
              
              // Text position within the text area (right-aligned with left padding only)
              const textX = textAreaX + textPaddingLeft; // Only left padding
              const textY = textAreaY + 1; // Vertical centering within smaller text area (was 2)
              
              // Render text with appropriate style
              if (useBackdrop) {
                // Draw black background for text area (sized to fit text)
                $.ink("black"); // Black background
                $.box(textAreaX, textAreaY, textAreaWidth, textAreaHeight);
                
                // Render white text (no rotation for now)
                $.ink("white"); // White text on black background
                try {
                  $.write(codeToRender, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");
                } catch (error) {
                  console.warn(`🔤 MatrixChunky8 failed, using fallback:`, error);
                  $.write(codeToRender, { x: textX, y: textY });
                }
              } else {
                // Shadow style: black shadow first, then white text
                // Draw black shadow (1px offset) - no rotation for now
                $.ink("black");
                try {
                  $.write(codeToRender, { x: textX + 1, y: textY + 1 }, undefined, undefined, false, "MatrixChunky8");
                } catch (error) {
                  $.write(codeToRender, { x: textX + 1, y: textY + 1 });
                }
                
                // Draw white text on top - no rotation for now
                $.ink("white");
                try {
                  $.write(codeToRender, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");
                } catch (error) {
                  $.write(codeToRender, { x: textX, y: textY });
                }
              }
              
              // Draw gray shadow along the right side and bottom of the entire overlay
              $.ink("gray", 128);
              // Right shadow - offset 1px down from top like a drop shadow
              $.box(qrSize, 1, 1, qrOffsetY + qrSize);
              // Bottom shadow - offset 1px from left edge like a drop shadow
              $.box(1, qrOffsetY + qrSize, qrSize, 1);
            });
            
            // Don't cache QR overlay if font isn't fully loaded yet (text label needs to re-render)
            // Cache the base QR data (not the transferable pixels)
            const qrData = {
              width: generatedQR.width,
              height: generatedQR.height,
              basePixels: new Uint8ClampedArray(generatedQR.pixels) // Keep a safe copy for caching
            };
            
            // Recalculate position for current screen size (handles reframing)
            const overlayWidth = qrData.width;
            const overlayHeight = qrData.height;
            startX = screen.width - overlayWidth - margin;
            startY = screen.height - overlayHeight - margin; // Removed +1 to prevent label shadow overlap
            
            // Cache the QR data for this piece (not the transferable version)
            // Only cache if caching is not disabled AND font is fully loaded
            if (!isQRCacheDisabled && allCharsLoaded) {
              qrOverlayCache.set(cacheKey, qrData);
            }
            
            // Create fresh overlay for transfer each time
            qrOverlay = {
              width: qrData.width,
              height: qrData.height,
              pixels: new Uint8ClampedArray(qrData.basePixels) // Fresh copy for transfer
            };
            
            // Add QR overlay to sendData with exact position
            sendData.qrOverlay = {
              x: startX,
              y: startY,
              img: qrOverlay
            };
            }
          } else {
            // No cached code yet - QR will appear once caching is complete
          }
        } catch (err) {
          console.warn("Failed to generate QR overlay:", err);
        }
      }

      let transferredPixels;

      // Check to see if we have a dirtyBox to render from.
      const croppedBox = dirtyBox?.croppedBox?.(screen);

      if (!skipPixelsDuringTapePlayback && croppedBox?.w > 0 && croppedBox?.h > 0) {
        transferredPixels = dirtyBox.crop(screen);
        sendData.pixels = transferredPixels;
        sendData.dirtyBox = croppedBox;
      } else if (!skipPixelsDuringTapePlayback && painted === true) {
        // TODO: Toggling this causes a flicker in `line`... but helps prompt. 2022.01.29.13.21
        // Otherwise render everything if we drew anything!
        transferredPixels = screen.pixels;
        sendData.pixels = transferredPixels;
      }

      // Add the duration progress bar if duration tracking is active
      // This shows a 1px bar at the bottom similar to tape, but for timed pieces
      // NOW the screen buffer is ready for proper pixel sampling
      const durationProgressBarData = renderDurationProgressBar($api);
      if (durationProgressBarData && !skipPixelsDuringTapePlayback) {
        sendData.durationProgressBar = durationProgressBarData;
      }

      // Add timecode display (00:00 / 1:20) on bottom left - shows regardless of nohud/progress bar
      // Show timecode for duration tracking but NOT during tape recording
      if ((durationTotal && durationStartTime !== null) && !$api.rec.recording) {
        // Use duration tracking only (exclude tape recording)
        const recordingDuration = durationTotal;
        const recordingStartTime = durationStartTime;
        
        console.log("🕐 Starting timecode generation...", {
          recordingDuration,
          recordingStartTime,
          skipPixelsDuringTapePlayback,
          tf: !!tf,
          tfBlockWidth: tf?.blockWidth,
          tfBlockHeight: tf?.blockHeight,
          isRecording: $api.rec.recording
        });
        
        const currentTime = performance.now();
        const elapsed = recordingStartTime ? Math.max(0, (currentTime - recordingStartTime) / 1000) : 0; // Convert to seconds, clamp to 0
        
        // Clamp elapsed to duration when completed to hold at end time
        const clampedElapsed = Math.min(elapsed, recordingDuration);
        
        const elapsedMins = Math.floor(clampedElapsed / 60);
        const elapsedSecs = Math.floor(clampedElapsed % 60);
        const totalMins = Math.floor(recordingDuration / 60);
        const totalSecs = Math.floor(recordingDuration % 60);
        
        const timecodeText = `${elapsedMins.toString().padStart(2, '0')}:${elapsedSecs.toString().padStart(2, '0')} / ${totalMins.toString().padStart(2, '0')}:${totalSecs.toString().padStart(2, '0')}`;
        
        console.log("🕐 Timecode details:", {
          recordingDuration,
          clampedElapsed,
          elapsed,
          elapsedMins,
          elapsedSecs,
          totalMins,
          totalSecs,
          timecodeText,
          textLength: timecodeText.length
        });
        
        // Check if duration is complete for blinking
        const progress = Math.min(elapsed / recordingDuration, 1);
        const completed = progress >= 1;
        
        // Debug completion state
        if (completed) {
          console.log("🕐 Duration completed! Blinking red timecode");
        }
        
        // Use the current default typeface (tf) for consistency
        if (tf) {
          // Calculate text dimensions for positioning - ensure enough width for timecode
          const textWidth = Math.max(timecodeText.length * tf.blockWidth, 100); // Minimum 100px width
          const textHeight = tf.blockHeight;
          
          // Minimal padding, no background box
          const padding = 1; // Reduced padding
          const shadowOffset = 1;
          const timecodeWidth = textWidth + padding * 2 + shadowOffset;
          const timecodeHeight = textHeight + padding * 2 + shadowOffset;
          
          console.log("🕐 Timecode painting dimensions:", {
            textWidth,
            textHeight,
            padding,
            shadowOffset,
            timecodeWidth,
            timecodeHeight,
            timecodeText,
            textLength: timecodeText.length,
            tfBlockWidth: tf.blockWidth,
            tfBlockHeight: tf.blockHeight,
            completed
          });

          const timecodePainting = $api.painting(timecodeWidth, timecodeHeight, ($) => {
            console.log("🕐 Inside timecode painting function");
            
            // No background box - just transparent
            
            // Draw shadow first (offset by 1px down and right) - using same pattern as HUD
            $.ink(0).write(
              timecodeText,
              {
                x: padding + shadowOffset,
                y: padding + shadowOffset,
                noInlineColor: true,
              },
              undefined,
              timecodeWidth, // Wrap width
            );
            
            // Draw main text - blink red when completed, white when normal
            if (completed) {
              // Use global blink state to sync with progress bar
              const textColor = durationBlinkState ? [255, 0, 0] : [128, 0, 0]; // Bright red to dim red
              console.log("🕐 Drawing completed timecode:", {
                text: timecodeText,
                color: textColor,
                blinkState: durationBlinkState,
                completed
              });
              $.ink(textColor[0], textColor[1], textColor[2]).write(
                timecodeText,
                {
                  x: padding,
                  y: padding,
                },
                undefined,
                timecodeWidth, // Wrap width
              );
            } else {
              // Normal white text
              console.log("🕐 Drawing normal timecode:", {
                text: timecodeText,
                color: [255, 255, 255],
                completed
              });
              $.ink(255, 255, 255).write(
                timecodeText,
                {
                  x: padding,
                  y: padding,
                },
                undefined,
                timecodeWidth, // Wrap width
              );
            }
            
            console.log("🕐 Finished drawing text in timecode painting");
          });
          
          console.log("🕐 Timecode painting created:", {
            painting: !!timecodePainting,
            pixels: !!timecodePainting?.pixels,
            pixelsLength: timecodePainting?.pixels?.length,
            width: timecodePainting?.width,
            height: timecodePainting?.height
          });          // Add timecode to sendData if created successfully
          if (timecodePainting && timecodePainting.pixels && timecodePainting.pixels.length > 0) {
            const timecodeData = {
              x: 3, // 3px margin from left edge
              y: $api.screen.height - timecodeHeight - 4, // Position above progress bar (2px margin + progress bar height)
              img: {
                width: timecodePainting.width,
                height: timecodePainting.height,
                pixels: timecodePainting.pixels
              }
            };
            
            sendData.durationTimecode = timecodeData;
            
            console.log("🕐 Timecode added to sendData:", {
              x: timecodeData.x,
              y: timecodeData.y,
              width: timecodeData.img.width,
              height: timecodeData.img.height,
              pixelsLength: timecodeData.img.pixels.length,
              screenHeight: $api.screen.height,
              calculatedY: timecodeData.y
            });
          } else {
            console.warn("🕐 Timecode painting failed:", {
              painting: !!timecodePainting,
              pixels: !!timecodePainting?.pixels,
              pixelsLength: timecodePainting?.pixels?.length
            });
          }
        } else {
          console.warn("🕐 No typeface (tf) available for timecode rendering");
        }
      }

      // Optional messages to send.
      if (painted === true) sendData.paintChanged = true;
      if (loading === true) sendData.loading = true;

      // These fields are one time `signals`.
      if (reframe || glazeAfterReframe) {
        sendData.reframe = reframe || glazeAfterReframe !== undefined;
        if (glazeAfterReframe) {
          send(glazeAfterReframe);
          glazeAfterReframe = undefined;
        }
      }

      if (cursorCode) sendData.cursorCode = cursorCode;

      // Safely handle pixel buffer conversion and transfer
      if (sendData.pixels) {
        // Convert TypedArray to ArrayBuffer if needed
        if (sendData.pixels.buffer) {
          sendData.pixels = sendData.pixels.buffer;
        }
        // Validate the ArrayBuffer is not detached
        if (!sendData.pixels || sendData.pixels.byteLength === 0) {
          sendData.pixels = undefined;
        }
      } else if (!skipPixelsDuringTapePlayback && content.pixels) {
        // Only use content.pixels if not during tape playback to avoid stale overlay
        sendData.pixels = content.pixels;
      }

      let transferredObjects = [];
      const transferredBufferSet = new Set(); // Track unique buffers to avoid duplicates
      
      // Only add pixels to transfer if we actually have valid pixel data
      if (sendData.pixels && sendData.pixels instanceof ArrayBuffer && sendData.pixels.byteLength > 0) {
        if (!transferredBufferSet.has(sendData.pixels)) {
          transferredObjects.push(sendData.pixels);
          transferredBufferSet.add(sendData.pixels);
        }
      }

      // Safely add label buffer to transfer array
      if (sendData.label?.img?.pixels?.buffer) {
        const labelBuffer = sendData.label.img.pixels.buffer;
        if (labelBuffer instanceof ArrayBuffer && labelBuffer.byteLength > 0 && !transferredBufferSet.has(labelBuffer)) {
          transferredObjects.push(labelBuffer);
          transferredBufferSet.add(labelBuffer);
        }
      }

      // Safely add QR overlay buffer to transfer array
      if (sendData.qrOverlay?.img?.pixels?.buffer) {
        const qrBuffer = sendData.qrOverlay.img.pixels.buffer;
        if (qrBuffer instanceof ArrayBuffer && qrBuffer.byteLength > 0 && !transferredBufferSet.has(qrBuffer)) {
          transferredObjects.push(qrBuffer);
          transferredBufferSet.add(qrBuffer);
        }
      }

      // Safely add tape progress bar buffer to transfer array
      if (sendData.tapeProgressBar?.img?.pixels?.buffer) {
        const tapeBuffer = sendData.tapeProgressBar.img.pixels.buffer;
        if (tapeBuffer instanceof ArrayBuffer && tapeBuffer.byteLength > 0 && !transferredBufferSet.has(tapeBuffer)) {
          transferredObjects.push(tapeBuffer);
          transferredBufferSet.add(tapeBuffer);
        }
      }

      // Safely add duration timecode buffer to transfer array
      if (sendData.durationTimecode?.img?.pixels?.buffer) {
        const timecodeBuffer = sendData.durationTimecode.img.pixels.buffer;
        if (timecodeBuffer instanceof ArrayBuffer && timecodeBuffer.byteLength > 0 && !transferredBufferSet.has(timecodeBuffer)) {
          transferredObjects.push(timecodeBuffer);
          transferredBufferSet.add(timecodeBuffer);
        }
      }

      // console.log("TO:", transferredObjects);
      // console.log("Sent data:", sendData);

      sendData.sound = sound;

      // Safely send the render message with comprehensive error handling
      try {
        // Final validation: ensure no buffers are detached before sending
        const validTransferredObjects = transferredObjects.filter(buffer => {
          if (buffer instanceof ArrayBuffer && buffer.byteLength > 0) {
            try {
              // Try to create a view to test if buffer is detached
              new Uint8Array(buffer, 0, 1);
              return true;
            } catch (e) {
              console.warn("🟡 Skipping detached buffer from transfer list");
              return false;
            }
          }
          return false;
        });
        
        send({ type: "render", content: sendData }, validTransferredObjects);
      } catch (error) {
        console.warn("🚨 Failed to send render message:", error);
        // Attempt to send without transferred objects as fallback
        try {
          send({ type: "render", content: sendData }, []);
        } catch (fallbackError) {
          console.error("🚨 Critical: Failed to send render message even without transfers:", fallbackError);
        }
      }

      sound.sounds.length = 0; // Empty the sound command buffer.
      sound.bubbles.length = 0;
      sound.kills.length = 0;

      twoDCommands.length = 0; // Empty the 2D GPU command buffer.

      // Flush the `signals` after sending.
      if (reframe) reframe = undefined;
      if (cursorCode) cursorCode = undefined;
    } else {
      // Send update (sim).
      maybeLeave();
      // TODO: How necessary is this - does any info ever need to actually
      //       get sent? 23.01.06.16.02

      // console.log(pixels);
      
      // Safely handle pixels buffer for update message
      let updatePixelsBuffer = null;
      if (pixels?.buffer && pixels.buffer instanceof ArrayBuffer && pixels.buffer.byteLength > 0) {
        updatePixelsBuffer = pixels.buffer;
      }
      
      try {
        send(
          {
            type: "update",
            content: {
              didntRender: true,
              loading,
              pixels: updatePixelsBuffer,
              width: content.width,
              height: content.height,
              sound,
            },
          },
          updatePixelsBuffer ? [updatePixelsBuffer] : [],
        );
      } catch (error) {
        console.warn("🚨 Failed to send update message:", error);
        // Attempt to send without transferred objects as fallback
        try {
          send(
            {
              type: "update",
              content: {
                didntRender: true,
                loading,
                pixels: null, // Don't include pixels in fallback
                width: content.width,
                height: content.height,
                sound,
              },
            },
            [],
          );
        } catch (fallbackError) {
          console.error("🚨 Critical: Failed to send update message even without transfers:", fallbackError);
        }
      }
    }

    // Wait 8 frames of the default piece before loading the initial piece.
    // And also make sure the session has been queried.
    // console.log(sessionStarted);
    if (
      paintCount > 8n &&
      (sessionStarted || PREVIEW_OR_ICON || $commonApi.net.sandboxed)
    ) {
      //if (loadAfterPreamble) {
      // TODO: WHy does enabling this make the icon work?
      // console.log("💾 Loading after the preamble...");
      //}

      loadAfterPreamble?.(); // Start loading after the first disk if necessary.
    }

    // soundClear?.();

    // ***Frame State Reset***
    // Reset video transcoding / print progress.

    //console.log(performance.now() - frameTime, "ms");

    //performance.mark("b");

    //performance.measure("a", "b");
    //console.log("Frame perf:", performance.getEntriesByType("measure")[0].duration);
    //performance.clearMarks();
    //performance.clearMeasures();
  }
}

// 📚 Utilities

// Get the active user's handle from the server if one exists, updating
// $commonApi.handle
let HANDLE;
// TODO: Cache this in localStorage and clear it on log in and log out?
//       24.05.23.22.16

async function handle() {
  if (USER) {
    // TODO: Check to see if this is in localStorage or not...
    const storedHandle = store["handle"]; // || (await store.retrieve("handle"));

    // console.log("Stored handle...", storedHandle);

    if (storedHandle) {
      const newHandle = "@" + storedHandle;
      if (HANDLE === newHandle) return;
      HANDLE = "@" + storedHandle;
      send({ type: "handle", content: HANDLE });
      store["handle:received"] = true;
      // console.log("Retrieved handle from store:", storedHandle);
      return; // Leave early if a stored handle was found.
    }

    try {
      const response = await fetch(`/handle?for=${USER.sub}`);
      if (response.status === 200) {
        const data = await response.json();
        const newHandle = "@" + data.handle;
        if (newHandle === HANDLE) return;
        HANDLE = newHandle;
        send({ type: "handle", content: HANDLE });
        store["handle:received"] = true;
        store["handle"] = data.handle;
        // store.persist("handle"); // Maybe this shouldn't persist.
      } else {
        // console.warn(await response.text());
        store["handle:failed"] = true;
      }
    } catch (error) {
      console.error(error);
      store["handle:failed"] = true;
    }
  }
}

// Tell the `bios` to pull a screenshot of the next frame.
function downloadScreenshot() {
  send({
    type: "$creenshot",
    content: {
      filename: `$creenshot-${num.timestamp()}.png`,
      modifiers: { scale: 6 },
    },
  });
}

// Run the piece's "leave" function which will trigger
// a new load before sending off the final frame.
function maybeLeave() {
  // 🚪 Leave (Skips act and sim and paint...)
  if (leaving && leaveLoad) {
    try {
      leave({ ...painting.api, screen, ...$commonApi }); // Trigger leave.
    } catch (e) {
      console.warn("👋 Leave failure...", e);
    }
    leaveLoad();
    leaveLoad = null;
  }
}

// Play a sound when "notice" fires.
const noticeBell = (api, { tone } = { tone: 600 }) => {
  api.beep(tone);

  noticeTimer = new gizmo.Hourglass(160, {
    completed: () => {
      notice = "";
      noticeOpts = null;
      noticeTimer = null;
      $commonApi.needsPaint();
    },
  });
};
