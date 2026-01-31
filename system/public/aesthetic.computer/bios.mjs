// 💻 BIOS

// 📦 All Imports
import * as Loop from "./lib/loop.mjs";
import * as perf from "./lib/perf.mjs";
import { Pen } from "./lib/pen.mjs";
import { Box } from "./lib/geo.mjs";
import { Keyboard } from "./lib/keyboard.mjs";
import { Gamepad } from "./lib/gamepad.mjs";
import { startCapturingMotion, stopCapturingMotion } from "./lib/motion.mjs";
import { speak, speakAPI } from "./lib/speech.mjs";
import * as UI from "./lib/ui.mjs";
import * as Glaze from "./lib/glaze.mjs";
import { apiObject, extension } from "./lib/helpers.mjs";
import { choose } from "./lib/help.mjs";
import { parse, slug } from "./lib/parse.mjs";
import { isKidlispSource, encodeKidlispForUrl, getSyntaxHighlightingColors, isKidlispConsoleEnabled, postKidlispConsoleImage, getCachedCode } from "./lib/kidlisp.mjs";
import * as Store from "./lib/store.mjs";
import * as MIDI from "./lib/midi.mjs";
import * as USB from "./lib/usb.mjs";
import { 
  renderVHSProgressBar,
  calculateVHSEffects,
  getLeaderPixelColor,
  blendColorWithVHS
} from "./disks/common/tape-player.mjs";
import {
  MetaBrowser,
  iOS,
  Android,
  TikTok,
  Safari,
  Aesthetic,
  AestheticExtension,
} from "./lib/platform.mjs";
import { headers } from "./lib/headers.mjs";
import { logs, log } from "./lib/logs.mjs";
import { checkPackMode } from "./lib/pack-mode.mjs";
import { captureFrame, formatTimestamp } from "./lib/frame-capture.mjs";
import { soundWhitelist } from "./lib/sound/sound-whitelist.mjs";
import { timestamp, radians } from "./lib/num.mjs";
import * as graph from "./lib/graph.mjs";
import * as WebGPU from "./lib/webgpu.mjs";
import { initGPU, switchBackend } from "./lib/gpu/index.mjs"; // 🎨 New backend system (auto-registers backends)
import { createWebGLBlitter } from "./lib/webgl-blit.mjs";

// import * as TwoD from "./lib/2d.mjs"; // 🆕 2D GPU Renderer.
const TwoD = undefined;

const { assign, keys } = Object;
const { round, floor, min, max } = Math;
const { isFinite } = Number;

// 🔒 Sandbox Detection - Detect if we're running in a restrictive iframe sandbox
// (like objkt.com's default mode which lacks allow-same-origin)
const isOpaqueOrigin = (() => {
  try {
    // In sandboxed iframes without allow-same-origin, origin is 'null' string
    if (window.origin === 'null') return true;
    // Also check if we can access localStorage (blocked in opaque origins)
    localStorage.getItem('__sandbox_test__');
    return false;
  } catch (e) {
    return true;
  }
})();

// Export to globalThis so pieces can detect sandbox mode
globalThis.acIsSandboxed = isOpaqueOrigin;

// Default to WebGL composite unless explicitly disabled
if (globalThis.acUseWebGLComposite === undefined) {
  globalThis.acUseWebGLComposite = true;
}

// Log once if we detect sandbox restrictions
if (isOpaqueOrigin) {
  console.warn('⚠️ Running in sandboxed iframe (opaque origin) - some features disabled, performance may be reduced');
}

// 🎹 DAW Sync (for Max for Live integration)
// The global window.acDaw* functions are defined in index.mjs HTML before modules load.
// This allows M4L's executejavascript to call them immediately.
// bios.mjs just connects the send function via window.acDawConnect().
let _dawBpm = 60;
let _dawSampleRate = null; // 🎹 Store DAW sample rate for AudioContext creation

// When set, BIOS will not update the URL on piece loads.
// Used to keep a stable URL while a merry pipeline is running.
let frozenUrlPath = null;

// Called from boot() to connect the send function
function _dawConnectSend(sendFunc, updateMetronome) {
  if (window.acDawConnect) {
    console.log("🎹 _dawConnectSend called, sendFunc:", sendFunc?.toString?.().substring(0, 100));
    // Wrap sendFunc to also update metronome and capture sample rate
    const wrappedSend = (msg) => {
      console.log("🎹 BIOS wrappedSend called:", msg.type);
      if (msg.type === "daw:tempo" && updateMetronome) {
        updateMetronome(msg.content.bpm);
      }
      // 🎹 Capture DAW sample rate for AudioContext creation
      if (msg.type === "daw:samplerate" && msg.content?.samplerate) {
        _dawSampleRate = msg.content.samplerate;
        console.log("🎹 BIOS captured DAW sample rate:", _dawSampleRate);
      }
      console.log("🎹 Calling sendFunc now...");
      sendFunc(msg);
      console.log("🎹 sendFunc returned for:", msg.type);
    };
    window.acDawConnect(wrappedSend);
  }
}

// 📼 Tape Playback Classes (for multi-tape transitions)

class Tape {
  constructor(id) {
    this.id = id;
    this.code = null;
    this.metadata = null;
    
    // Frame data
    this.frames = []; // Array of ImageBitmap objects
    this.timingData = []; // [{ frame, filename, duration, timestamp }, ...]
    this.frameIndex = 0;
    this.lastFrameTime = 0;
    
    // Audio data
    this.audioBuffer = null; // Web Audio AudioBuffer
    this.audioSource = null; // AudioBufferSourceNode
    this.audioGainNode = null; // GainNode for volume control
    this.audioStartTime = 0;
    this.audioStartedAtFrame = 0;
    
    // Playback state (persistent across scroll)
    this.isLoading = false;
    this.isPlaying = false;
    this.isPaused = false;
    this.wasPlayingBeforeScroll = false; // Remember state when scrolling away
    this.savedProgress = 0; // Save progress when pausing/scrolling away
    this.loadProgress = 0;
    this.loadPhase = ""; // download, unpack, frames, audio
    
    // Rendering state (managed by TapeManager)
    this.yOffset = 0; // Vertical offset in pixels (for scrolling)
    this.alpha = 0; // Opacity 0-1
    this.volume = 0; // Audio volume 0-1
  }

  // Start playback (audio only - frames rendered by TapeManager)
  play(audioContext) {
    if (this.isPlaying || !this.audioBuffer) return;
    
    this.isPlaying = true;
    this.isPaused = false;
    this.wasPlayingBeforeScroll = true; // Remember we were playing
    this.lastFrameTime = performance.now();
    
    // Create audio source
    this.audioSource = audioContext.createBufferSource();
    this.audioSource.buffer = this.audioBuffer;
    
    // Create gain node for volume control (for crossfade) - only if it doesn't exist
    if (!this.audioGainNode) {
      this.audioGainNode = audioContext.createGain();
      this.audioGainNode.gain.value = this.volume;
      this.audioGainNode.connect(audioContext.destination);
    } else {
      this.audioGainNode.gain.value = this.volume;
    }
    
    this.audioSource.connect(this.audioGainNode);
    
    // Start audio from saved progress (for resume after scroll)
    const startOffset = (this.frameIndex / this.frames.length) * this.audioBuffer.duration;
    this.audioStartTime = audioContext.currentTime - startOffset;
    this.audioStartedAtFrame = this.frameIndex;
    this.audioSource.start(0, startOffset);
    
    // Loop audio when it ends
    this.audioSource.onended = () => {
      if (this.isPlaying && !this.isPaused) {
        this.frameIndex = 0; // Reset to beginning
        this.play(audioContext); // Restart
      }
    };
  }

  pause() {
    this.isPlaying = false;
    this.isPaused = true;
    this.wasPlayingBeforeScroll = false; // Manually paused
    this.savedProgress = this.getProgress();
    this.audioSource?.stop();
  }

  stop() {
    this.isPlaying = false;
    this.isPaused = false;
    this.audioSource?.stop();
    this.audioSource = null;
    // Don't reset frameIndex - keep position for resume
    this.savedProgress = this.getProgress();
  }

  // Pause when scrolling away (but remember we were playing)
  pauseForScroll() {
    if (this.isPlaying) {
      this.wasPlayingBeforeScroll = true;
      this.savedProgress = this.getProgress();
      this.audioSource?.stop();
      this.isPlaying = false;
    }
  }

  // Resume when scrolling back
  resumeFromScroll(audioContext) {
    if (this.wasPlayingBeforeScroll && !this.isPlaying) {
      // Restore saved position
      if (this.savedProgress > 0) {
        this.setProgress(this.savedProgress);
      }
      this.play(audioContext);
    }
  }

  // Seek to specific frame (for scrubbing)
  seekToFrame(frameIndex, audioContext) {
    if (frameIndex < 0 || frameIndex >= this.frames.length) return;
    
    this.frameIndex = frameIndex;
    
    // If playing, restart audio from new position
    if (this.isPlaying && this.audioBuffer && audioContext) {
      this.audioSource?.stop();
      
      const progress = frameIndex / this.frames.length;
      const audioOffset = progress * this.audioBuffer.duration;
      
      this.audioSource = audioContext.createBufferSource();
      this.audioSource.buffer = this.audioBuffer;
      this.audioSource.connect(this.audioGainNode);
      this.audioStartTime = audioContext.currentTime - audioOffset;
      this.audioStartedAtFrame = frameIndex;
      this.audioSource.start(0, audioOffset);
    }
  }

  // Set playback progress (0-1)
  setProgress(progress) {
    const clampedProgress = Math.max(0, Math.min(1, progress));
    const targetFrame = Math.floor(clampedProgress * this.frames.length);
    this.frameIndex = Math.min(targetFrame, this.frames.length - 1);
    this.savedProgress = clampedProgress;
  }

  // Get current progress (0-1)
  getProgress() {
    if (this.frames.length === 0) return 0;
    return this.frameIndex / this.frames.length;
  }

  // Set volume (for crossfade)
  setVolume(volume) {
    this.volume = Math.max(0, Math.min(1, volume));
    if (this.audioGainNode) {
      this.audioGainNode.gain.value = this.volume;
    }
  }

  // Update to next frame (called by TapeManager)
  updateFrame(now) {
    if (!this.isPlaying || this.frames.length === 0) return;
    
    const timeSinceLastFrame = now - this.lastFrameTime;
    if (this.timingData[this.frameIndex]) {
      const frameDuration = this.timingData[this.frameIndex].duration || 33.33; // Default 30fps
      
      if (timeSinceLastFrame >= frameDuration) {
        this.frameIndex = (this.frameIndex + 1) % this.frames.length;
        this.lastFrameTime = now;
      }
    }
  }

  cleanup() {
    this.audioSource?.stop();
    this.audioSource = null;
    this.audioGainNode = null;
    this.frames = [];
    this.timingData = [];
  }
}

class TapeManager {
  constructor(canvas, audioContext, maxTapes = 6) {
    this.canvas = canvas;
    this.ctx = canvas.getContext("2d");
    this.audioContext = audioContext;
    this.tapes = new Map(); // Map<id, Tape>
    this.maxTapes = maxTapes;
    this.activeId = null;
    this.preloadQueue = []; // Array of tape IDs to load
    this.isPreloading = false;
    this.activeProgress = 0; // Current progress of active tape (exposed to disk)
  }

  // Create or get tape
  getTape(id) {
    if (!this.tapes.has(id)) {
      this.tapes.set(id, new Tape(id));
    }
    return this.tapes.get(id);
  }

  // Set active tape and start playback
  setActive(id) {
    // Pause previous active (but keep state)
    if (this.activeId && this.tapes.has(this.activeId)) {
      const prevTape = this.tapes.get(this.activeId);
      prevTape.pauseForScroll(); // Pause but remember we were playing
      prevTape.volume = 0;
    }
    
    this.activeId = id;
    const activeTape = this.tapes.get(id);
    
    if (activeTape && !activeTape.isLoading) {
      // Resume playback if it was playing before we scrolled away
      activeTape.resumeFromScroll(this.audioContext);
      activeTape.volume = 1;
      activeTape.yOffset = 0;
      activeTape.alpha = 1;
    }
  }

  // Render frame for single tape (compositing on main canvas)
  renderFrame(now, send) {
    if (!this.canvas || !this.ctx) return;
    
    // Update all playing tapes
    for (const tape of this.tapes.values()) {
      if (tape.isPlaying) {
        tape.updateFrame(now);
      }
    }
    
    // Update active progress and send to disk (for tv.mjs progress bar)
    if (this.activeId && send) {
      const activeTape = this.tapes.get(this.activeId);
      if (activeTape) {
        this.activeProgress = activeTape.getProgress();
        
        // Send progress update to disk every frame
        send({
          type: "tape:playback-progress",
          content: {
            tapeId: this.activeId,
            progress: this.activeProgress,
            frameIndex: activeTape.frameIndex,
            totalFrames: activeTape.frames.length,
            isPlaying: activeTape.isPlaying,
            isPaused: activeTape.isPaused
          }
        });
      }
    }
    
    // Composite all visible tapes onto canvas
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    
    for (const tape of this.tapes.values()) {
      if (tape.alpha > 0 && tape.frames.length > 0) {
        const frame = tape.frames[tape.frameIndex];
        if (frame) {
          this.ctx.globalAlpha = tape.alpha;
          
          // Calculate aspect-ratio-preserving dimensions (object-fit: contain)
          const canvasAspect = this.canvas.width / this.canvas.height;
          const frameAspect = frame.width / frame.height;
          
          let drawWidth, drawHeight, drawX, drawY;
          
          if (frameAspect > canvasAspect) {
            // Frame is wider - fit to width
            drawWidth = this.canvas.width;
            drawHeight = this.canvas.width / frameAspect;
            drawX = 0;
            drawY = (this.canvas.height - drawHeight) / 2 + tape.yOffset;
          } else {
            // Frame is taller - fit to height
            drawHeight = this.canvas.height;
            drawWidth = this.canvas.height * frameAspect;
            drawX = (this.canvas.width - drawWidth) / 2;
            drawY = tape.yOffset;
          }
          
          this.ctx.drawImage(
            frame,
            drawX,
            drawY,
            drawWidth,
            drawHeight
          );
        }
      }
    }
    
    this.ctx.globalAlpha = 1;
  }

  // Update audio crossfade based on tape positions
  updateAudioCrossfade(tapePositions) {
    // tapePositions = [{ id, yOffset, alpha }, ...]
    const centerY = this.canvas.height / 2;
    const fadeDistance = this.canvas.height;
    
    for (const pos of tapePositions) {
      const tape = this.tapes.get(pos.id);
      if (!tape) continue;
      
      // Calculate volume based on distance from center
      const distanceFromCenter = Math.abs(pos.yOffset + this.canvas.height / 2 - centerY);
      const volume = Math.max(0, 1 - (distanceFromCenter / fadeDistance));
      
      tape.yOffset = pos.yOffset;
      tape.alpha = pos.alpha;
      tape.setVolume(volume);
    }
  }

  // Evict furthest tape from cache
  evictFurthestTape(currentIndex, tapeIndices) {
    if (this.tapes.size <= this.maxTapes) return;
    
    let furthestId = null;
    let furthestDistance = -1;
    
    for (const [id, tape] of this.tapes) {
      const tapeIndex = tapeIndices.findIndex(t => t.id === id);
      if (tapeIndex === -1) continue;
      
      const distance = Math.abs(tapeIndex - currentIndex);
      if (distance > furthestDistance) {
        furthestDistance = distance;
        furthestId = id;
      }
    }
    
    if (furthestId) {
      const tape = this.tapes.get(furthestId);
      tape?.cleanup();
      this.tapes.delete(furthestId);
      console.log(`📼 Evicted tape ${furthestId} (distance: ${furthestDistance})`);
    }
  }

  cleanup() {
    for (const tape of this.tapes.values()) {
      tape.cleanup();
    }
    this.tapes.clear();
    this.activeId = null;
  }
}

let pendingUrlRewrite = null;
let rewriteFocusListenerAttached = false;

function performHistoryRewrite(path, historical) {
  // Skip history manipulation in pack mode (blob/srcdoc context)
  if (checkPackMode()) return;
  
  if (historical) {
    console.log("Rewriting to:", path);
    history.pushState("", document.title, path);
  } else {
    history.replaceState("", document.title, path);
  }
}

function resolveBackgroundFillSpec(colorLike) {
  if (colorLike === undefined || colorLike === null) return null;

  let resolved;
  try {
    resolved = graph.findColor(colorLike);
  } catch (err) {
    return null;
  }

  if (!Array.isArray(resolved) || resolved.length === 0) {
    return null;
  }

  const first = resolved[0];
  if (typeof first === "string" && first.startsWith("fade:")) {
    const fadeInfo = graph.parseFadeColor(resolved);
    if (fadeInfo) {
      return {
        type: "fade",
        fadeInfo,
      };
    }
    return null;
  }

  const clampChannel = (value, fallback = 0) => {
    if (typeof value !== "number" || !Number.isFinite(value)) return fallback;
    return Math.max(0, Math.min(255, Math.round(value)));
  };

  const r = clampChannel(resolved[0], 0);
  const g = clampChannel(resolved[1], r);
  const b = clampChannel(resolved[2], r);
  const a = clampChannel(resolved[3], 255);

  return {
    type: "solid",
    rgba: [r, g, b, a],
  };
}

function computeFadePositionForPixel(x, y, width, height, fadeInfo) {
  if (!fadeInfo || !width || !height) return 0;

  const maxX = Math.max(width - 1, 0);
  const maxY = Math.max(height - 1, 0);
  const direction = fadeInfo.direction;

  if (typeof direction === "number" && Number.isFinite(direction)) {
    return graph.calculateAngleFadePosition(
      x,
      y,
      0,
      0,
      maxX,
      maxY,
      direction,
    );
  }

  const numeric = direction !== undefined ? parseFloat(direction) : NaN;
  if (!Number.isNaN(numeric) && Number.isFinite(numeric)) {
    return graph.calculateAngleFadePosition(
      x,
      y,
      0,
      0,
      maxX,
      maxY,
      numeric,
    );
  }

  const safeDiv = (value, denom) => (denom <= 0 ? 0 : value / denom);

  switch (direction) {
    case "horizontal-reverse":
      return safeDiv(maxX - x, maxX);
    case "vertical":
      return safeDiv(y, maxY);
    case "vertical-reverse":
      return safeDiv(maxY - y, maxY);
    case "diagonal": {
      const dx = safeDiv(x, maxX);
      const dy = safeDiv(y, maxY);
      return (dx + dy) / 2;
    }
    case "diagonal-reverse": {
      const dx = safeDiv(maxX - x, maxX);
      const dy = safeDiv(maxY - y, maxY);
      return (dx + dy) / 2;
    }
    case "horizontal":
    default:
      return safeDiv(x, maxX);
  }
}

function fillFadeExpansionsOnCanvas(ctx, width, height, oldWidth, oldHeight, fadeInfo) {
  const clampChannel = (value) => {
    if (typeof value !== "number" || !Number.isFinite(value)) return 0;
    return Math.max(0, Math.min(255, Math.round(value)));
  };

  if (width > oldWidth) {
    const rightWidth = width - oldWidth;
    const imageData = ctx.createImageData(rightWidth, height);
    const data = imageData.data;

    for (let y = 0; y < height; y += 1) {
      for (let x = 0; x < rightWidth; x += 1) {
        const globalX = oldWidth + x;
        const globalY = y;
        const t = computeFadePositionForPixel(globalX, globalY, width, height, fadeInfo);
        const [r = 0, g = 0, b = 0, a = 255] = graph.getLocalFadeColor(t, globalX, globalY, fadeInfo);
        const i = (y * rightWidth + x) * 4;
        data[i] = clampChannel(r);
        data[i + 1] = clampChannel(g);
        data[i + 2] = clampChannel(b);
        data[i + 3] = clampChannel(a || 255);
      }
    }

    ctx.putImageData(imageData, oldWidth, 0);
  }

  if (height > oldHeight) {
    const bottomHeight = height - oldHeight;
    const bottomWidth = width > oldWidth ? oldWidth : width;

    if (bottomWidth > 0 && bottomHeight > 0) {
      const imageData = ctx.createImageData(bottomWidth, bottomHeight);
      const data = imageData.data;

      for (let y = 0; y < bottomHeight; y += 1) {
        for (let x = 0; x < bottomWidth; x += 1) {
          const globalX = x;
          const globalY = oldHeight + y;
          const t = computeFadePositionForPixel(globalX, globalY, width, height, fadeInfo);
          const [r = 0, g = 0, b = 0, a = 255] = graph.getLocalFadeColor(t, globalX, globalY, fadeInfo);
          const i = (y * bottomWidth + x) * 4;
          data[i] = clampChannel(r);
          data[i + 1] = clampChannel(g);
          data[i + 2] = clampChannel(b);
          data[i + 3] = clampChannel(a || 255);
        }
      }

      ctx.putImageData(imageData, 0, oldHeight);
    }
  }
}

// 🎬 GIF Encoder Configuration
// Set to true to use gifenc by default, false for gif.js
const DEFAULT_USE_GIFENC = true;

// 🚫 Cache Control Flags
// Set these to true to disable caching for dynamic content updates
window.acDISABLE_HUD_LABEL_CACHE = true; // Disable HUD label caching for dynamic coloring
window.acDISABLE_QR_OVERLAY_CACHE = true; // Disable QR overlay caching for dynamic updates

const diskSends = [];
let diskSendsConsumed = false;

// Store original URL parameters for refresh functionality
let preservedParams = {};

window.acDISK_SEND = function (message) {
  !diskSendsConsumed ? diskSends.push(message) : window.acSEND(message);
};

// 🔊 Master volume control (for desktop app sliders, etc.)
// NOTE: These must be at module scope so window.AC.setMasterVolume works before boot()
let masterVolume = 1;
let speakerProcessorNode = null; // Will be set by boot()
let backgroundMusicEl = null; // Will be set by boot()
let backgroundMusicBaseVolume = 1;
let streamAudio = {}; // Will be populated by boot()
let audioStarting = false;

function clampVolume(value) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) return 1;
  return Math.max(0, Math.min(1, numeric));
}

function applyMasterVolume(nextValue) {
  masterVolume = clampVolume(nextValue);

  if (speakerProcessorNode?.port) {
    speakerProcessorNode.port.postMessage({ type: "volume", value: masterVolume });
  }

  if (backgroundMusicEl) {
    backgroundMusicEl.volume = backgroundMusicBaseVolume * masterVolume;
  }

  for (const id in streamAudio) {
    const stream = streamAudio[id];
    if (stream?.audio) {
      const base = Number.isFinite(stream.baseVolume) ? stream.baseVolume : 1;
      stream.audio.volume = base * masterVolume;
    }
  }

  return masterVolume;
}

window.AC = window.AC || {};
window.AC.setMasterVolume = (value) => applyMasterVolume(value);
window.AC.getMasterVolume = () => masterVolume;
window.acSetMasterVolume = window.AC.setMasterVolume;
window.acGetMasterVolume = window.AC.getMasterVolume;

// 🔍 CDP Debug State - Expose piece state for browser automation
// Pieces can call `_send({ type: "debug:state", content: {...} })` to update this
window.acPieceState = null;
window.acGetState = () => ({
  piece: window.acSTARTING_PIECE,
  user: window.acUSER,
  debug: window.acDEBUG,
  pieceState: window.acPieceState,
  paused: window.__acLoopPaused || false,
  timestamp: Date.now(),
});

function consumeDiskSends(send) {
  if (diskSendsConsumed) return;
  diskSends.forEach((message) => send(message));
  diskSends.length = 0;
  diskSendsConsumed = true;
}

// 🔌 USB
USB.initialize();

// 💾 Boot the system and load a disk.
async function boot(parsed, bpm = 60, resolution, debug) {
  const bootStartTime = performance.now();
  perf.markBootStart();
  headers(); // Print console headers with auto-detected theme.

  // Expose Loop control to window for boot.mjs
  // Track pause state for kidlisp console snapshots
  window.acPAUSE = () => {
    window.__acLoopPaused = true;
    Loop.pause();
  };
  window.acRESUME = () => {
    window.__acLoopPaused = false;
    // Reset snap timer so first snap happens 5s after resume
    window._lastKidlispSnapTime = performance.now();
    Loop.resume();
  };
  // Clear KidLisp bake layers (exposed for soft-stop)
  window.acCLEAR_BAKE_LAYERS = () => {
    if (window.__acGlobalKidLispInstance?.clearBakedLayers) {
      window.__acGlobalKidLispInstance.clearBakedLayers();
    }
  };
  
  // Force immediate reframe (used by kidlisp.com after panel resize ends)
  window.acREFRAME = () => {
    needsReframe = true;
  };

  // Notify parent of boot progress and update the boot log overlay
  if (window.acBOOT_LOG) {
    window.acBOOT_LOG("initializing graphics");
  } else if (window.parent) {
    window.parent.postMessage({ 
      type: "boot-log", 
      message: "initializing graphics" 
    }, "*");
  }

  // Store original URL parameters for refresh functionality from the resolution object
  preservedParams = {};
  if (resolution.gap === 0) preservedParams.nogap = "true"; // gap: 0 means nogap was true
  if (resolution.nolabel === true) preservedParams.nolabel = "true";
  if (resolution.tv === true) preservedParams.tv = "true";
  if (resolution.device === true) preservedParams.device = "true";
  if (resolution.highlight) preservedParams.highlight = resolution.highlight === true ? "true" : resolution.highlight;
  
  // Only preserve density/zoom/duration if they were actually in the URL (not from localStorage)
  const currentParams = new URLSearchParams(location.search);
  if (currentParams.has("density")) preservedParams.density = currentParams.get("density");
  if (currentParams.has("zoom")) preservedParams.zoom = currentParams.get("zoom");
  if (currentParams.has("duration")) preservedParams.duration = currentParams.get("duration");

  if (debug) {
    if (window.isSecureContext) {
      // console.log("🔒 Secure");
    } else {
      console.warn("🔓 Insecure");
    }
  }

  window.acCONTENT_EVENTS = [];

  let HANDLE; // Populated with the user's handle from `disk`.

  let pen,
    keyboard,
    gamepad,
    keyboardFocusLock = false,
    keyboardSoftLock = false;
  let handData; // Hand-tracking.

  let frameCount = 0n;
  let now = 0;

  let diskSupervisor;
  let currentPiece = null; // Gets set to a path after `loaded`.
  let currentPieceHasKeyboard = false;

  // Media Recorder
  let mediaRecorder;
  let recordedFrames = [];
  let recordedPieceChanges = []; // Track piece changes during recording
  const mediaRecorderChunks = [];
  let mediaRecorderDuration = 0,
    mediaRecorderStartTime,
    mediaRecorderFrameCount = 0; // Frame counter for performance optimization
  let needs$creenshot = false; // Flag when a capture is requested.
  
  // Raw audio capture for tape playback
  let rawAudioProcessor = null;
  let rawAudioData = [];
  let rawAudioSampleRate = 44100;
  
  // Dynamic FPS detection for display-rate independent recording
  let detectedDisplayFPS = 60; // Default fallback
  let lastFrameTime = 0;
  let frameTimes = [];
  const FPS_SAMPLE_SIZE = 30; // Number of frames to sample for FPS detection

  // Duration Progress Bar (for timed pieces from query parameter)
  let durationProgressStartTime = undefined;
  let durationProgressDuration = resolution.duration; // Duration in seconds from query parameter
  let durationProgressCompleted = false;

  // 🎮 Game Boy Emulator (main thread)
  let gameboyEmulator = null;
  let currentGameboyPixels = null;
  let currentGameboyROM = null;

  // Events
  let whens = {};

  // Register core signal handlers
  whens["recorder:cut"] = async function() {
    
    if (!mediaRecorder) {
      console.warn(`No mediaRecorder available during cut - sending rolling:ended anyway`);
      send({ type: "recorder:rolling:ended" });
      return;
    }
    
    if (debug && logs.recorder) console.log("✂️ Recorder: Cut");
    
    try {
      // Safety check to prevent NaN duration
      if (mediaRecorderStartTime !== undefined) {
        mediaRecorderDuration += performance.now() - mediaRecorderStartTime;
      } else {
        console.warn("Warning: mediaRecorderStartTime is undefined during cut, cannot calculate duration");
        // Set a minimal duration to prevent NaN
        if (mediaRecorderDuration === undefined || isNaN(mediaRecorderDuration)) {
          mediaRecorderDuration = 100; // Fallback to 100ms
        }
      }
      
      // mediaRecorder?.stop();
      mediaRecorder?.pause(); // Single clips for now.

      // Store the tape data to IndexedDB for persistence across page refreshes
      const blob = new Blob(mediaRecorderChunks, {
        type: mediaRecorder.mimeType,
      });
      
      // Convert raw audio data to serializable format for storage
      let rawAudioArrays = null;
      if (rawAudioData.length > 0 && audioContext) {
        try {
          const totalSamples = rawAudioData.length * 4096; // 4096 samples per chunk
          const leftChannelData = new Float32Array(totalSamples);
          const rightChannelData = new Float32Array(totalSamples);
          
          let sampleIndex = 0;
          for (let i = 0; i < rawAudioData.length; i++) {
            const chunk = rawAudioData[i];
            for (let j = 0; j < chunk.left.length; j++) {
              if (sampleIndex < totalSamples) {
                leftChannelData[sampleIndex] = chunk.left[j];
                rightChannelData[sampleIndex] = chunk.right[j];
                sampleIndex++;
              }
            }
          }
          
          rawAudioArrays = {
            left: leftChannelData,
            right: rightChannelData,
            sampleRate: rawAudioSampleRate,
            totalSamples: totalSamples
          };
          
          console.log(`🎵 Created raw audio arrays: ${totalSamples} samples, ${totalSamples / rawAudioSampleRate}s duration`);
        } catch (error) {
          console.error("Error creating raw audio arrays:", error);
        }
      }
      
      try {
        // Don't store frames in IndexedDB to avoid memory issues with long recordings
        // Frames are kept in memory (recordedFrames) for the current session only
        await receivedChange({
          data: {
            type: "store:persist",
            content: {
              key: "tape",
              method: "local:db",
              data: {
                blob,
                duration: mediaRecorderDuration,
                // frames: recordedFrames, // Excluded to prevent out-of-memory errors
                timestamp: Date.now(),
                rawAudio: rawAudioArrays, // Add raw audio arrays for playback
              },
            },
          },
        });

        if (debug && logs.recorder) console.log("📼 Stored tape to IndexedDB (without frame data)");
      } catch (storageError) {
        console.error("Error storing tape to IndexedDB:", storageError);
        // Continue despite storage error
      }

      // Ensure the rolling:ended signal is sent which triggers the cutCallback
      send({ type: "recorder:rolling:ended" });
      
    } catch (error) {
      console.error("Error in cut operation:", error);
      // Still send the ended signal even if something fails
      send({ type: "recorder:rolling:ended" });
    }
  };

  // Video storage
  const videos = [];

  // Media preloading tracker (for cancellations).
  const mediaPathsLoading = {};

  // Rendering

  // Wrap everything in an #aesthetic-computer div.
  const wrapper = document.createElement("div");
  wrapper.id = "aesthetic-computer";

  // 🖥️ Our main display surface. (Software Renderer)
  const canvas = document.createElement("canvas");
  const ctx = canvas.getContext("2d", { willReadFrequently: true });

  // 🖥️🔌 WebGPU 2D Renderer Canvas
  const webgpuCanvas = document.createElement("canvas");
  webgpuCanvas.dataset.type = "webgpu";
  webgpuCanvas.style.position = "absolute";
  webgpuCanvas.style.top = "0";
  webgpuCanvas.style.left = "0";
  webgpuCanvas.style.zIndex = "1"; // Place above main canvas
  webgpuCanvas.style.pointerEvents = "none"; // Let events pass through to main canvas
  webgpuCanvas.style.display = "none"; // Hidden by default until WebGPU is enabled

  // 🖥️🎨 WebGL Composite Canvas (blitted pixel buffer)
  const webglCompositeCanvas = document.createElement("canvas");
  webglCompositeCanvas.dataset.type = "webgl-composite";
  webglCompositeCanvas.style.position = "absolute";
  webglCompositeCanvas.style.top = "0";
  webglCompositeCanvas.style.left = "0";
  webglCompositeCanvas.style.zIndex = "0";
  webglCompositeCanvas.style.pointerEvents = "none";
  webglCompositeCanvas.style.display = "none";

  // � DOM-based Stats Overlay (always on top of everything)
  const statsOverlay = document.createElement("div");
  statsOverlay.id = "ac-stats-overlay";
  statsOverlay.style.cssText = `
    position: absolute;
    top: 4px;
    left: 4px;
    z-index: 99999;
    pointer-events: none;
    font-family: monospace;
    font-size: 11px;
    line-height: 1.3;
    padding: 6px 8px;
    background: rgba(0, 0, 0, 0.75);
    color: #fff;
    border-radius: 4px;
    display: none;
    white-space: pre;
  `;
  let statsOverlayEnabled = false;
  let statsOverlayData = { backend: "CPU", lines: 0 };
  let statsFps = 0;
  let statsFrameCount = 0;
  let statsLastTime = performance.now();

  // Reframe debug logging - disabled by default, enable with window.acReframeDebug = true
  // if (typeof window !== "undefined" && window.acReframeDebug === undefined) {
  //   window.acReframeDebug = true;
  // }

  let webglBlitter = null;
  let captureCompositeCanvas = null;
  let captureCompositeCtx = null;

  function updateStatsOverlay() {
    if (!statsOverlayEnabled && !window.acReportFpsToParent) return;
    const now = performance.now();
    statsFrameCount++;
    if (now - statsLastTime >= 1000) {
      statsFps = statsFrameCount;
      statsFrameCount = 0;
      statsLastTime = now;
      // Report FPS to parent window if requested
      if (window.acReportFpsToParent && window.parent !== window) {
        window.parent.postMessage({ type: 'ac:fps-report', fps: statsFps }, '*');
      }
    }
    if (!statsOverlayEnabled) return;
    const lines = [
      `${statsOverlayData.backend || "GPU"}`,
      `FPS: ${statsFps}`,
    ];
    if (statsOverlayData.lines) lines.push(`Lines: ${statsOverlayData.lines}`);
    if (statsOverlayData.extra) lines.push(statsOverlayData.extra);
    statsOverlay.textContent = lines.join("\n");
  }

  function shouldUseWebGLComposite(content) {
    if (!content) return false;
    if (content.webgpuEnabled) return false;
    return content.webglCompositeEnabled === true || globalThis.acUseWebGLComposite === true;
  }

  function ensureCaptureCompositeCanvas(width, height) {
    if (!captureCompositeCanvas) {
      captureCompositeCanvas = document.createElement("canvas");
      captureCompositeCtx = captureCompositeCanvas.getContext("2d");
    }
    if (captureCompositeCanvas.width !== width) captureCompositeCanvas.width = width;
    if (captureCompositeCanvas.height !== height) captureCompositeCanvas.height = height;
    return captureCompositeCtx;
  }

  // �🖥️🔌 Secondary main display surface. (3D GPU Renderer)
  // TODO: Eventually deprecate the software renderer in favor of this?
  //       (Or even reuse the same tag if pieces swap.)
  //       TODO: Would it be possible for pieces to use both... and why?
  //             (Probably Not)
  TwoD?.initialize(wrapper);

  // An extra canvas reference for passing through or buffering video recording streams.
  let streamCanCtx;
  let paintToStreamCanvas = false;
  let startTapePlayback,
    stopTapePlayback,
    pauseTapePlayback,
    resumeTapePlayback,
    seekTapePlayback;

  let audioContextResuming = false; // Flag to track when AudioContext resume is in progress
  let audioContextResumeTimestamps = {}; // Store resume timestamps separately
  let dawAudioResumed = false; // Track if we've already auto-resumed AudioContext for DAW mode
  let tapeSyncPosition = null; // Position to sync tape audio to (0-1), set by video piece
  let currentTapePosition = 0; // Current tape playback position (0-1), updated by main loop
  
  // Make resume timestamps accessible globally
  window.audioContextResumeTimestamps = audioContextResumeTimestamps;

  let shareFile, shareFileCallback; // A temporary storage container for a pre-prepped
  // file download to use on a user interaction.

  // A layer for modal messages such as "audio engine is off".
  const modal = document.createElement("div");
  modal.id = "modal";

  // A ui canvas for rendering a native resolution ui on top of everything.
  const uiCanvas = document.createElement("canvas");
  const uiCtx = uiCanvas.getContext("2d");
  uiCanvas.dataset.type = "ui";

  const debugCanvas = document.createElement("canvas");
  const debugCtx = debugCanvas.getContext("2d");
  debugCanvas.dataset.type = "debug";

  // A buffer for nicer resolution switches, nice when moving from
  // low resolution back to high resolution. Could eventually be used
  // for transition effects.
  const freezeFrameCan = document.createElement("canvas");
  const ffCtx = freezeFrameCan.getContext("2d");
  freezeFrameCan.dataset.type = "freeze";
  // Add smooth fade transition for seamless resize
  freezeFrameCan.style.transition = "opacity 50ms ease-out";
  freezeFrameCan.style.willChange = "opacity";
  freezeFrameCan.style.position = "absolute";
  freezeFrameCan.style.zIndex = "10"; // Above all other canvases during reframe
  freezeFrameCan.style.pointerEvents = "none";

  // Freeze overlay canvas - preserves HUD/labels during resize
  const freezeOverlayCan = document.createElement("canvas");
  const freezeOverlayCtx = freezeOverlayCan.getContext("2d");
  freezeOverlayCan.style.position = "absolute";
  freezeOverlayCan.style.zIndex = "11"; // Above freeze frame
  freezeOverlayCan.style.pointerEvents = "none";
  freezeOverlayCan.style.transition = "opacity 50ms ease-out";

  // A buffer for corner label overlays.
  const overlayCan = document.createElement("canvas");
  const octx = overlayCan.getContext("2d");
  overlayCan.style.position = "absolute";
  overlayCan.style.top = "0";
  overlayCan.style.left = "0";
  overlayCan.style.zIndex = "2";
  overlayCan.style.pointerEvents = "none";
  overlayCan.style.display = "none";

  // 🎬 Recording UI Canvas - NOT included in tape captures
  // This is for piece UIs like cap.mjs that need to show controls during recording
  // but should not have those controls recorded in the tape itself
  const recordingUICan = document.createElement("canvas");
  const recordingUICtx = recordingUICan.getContext("2d");
  recordingUICan.style.position = "absolute";
  recordingUICan.style.top = "0";
  recordingUICan.style.left = "0";
  recordingUICan.style.zIndex = "3"; // Above overlay canvas
  recordingUICan.style.pointerEvents = "none";
  recordingUICan.style.display = "none";
  recordingUICan.dataset.type = "recording-ui"; // Mark for identification

  // Reusable canvas for dirtyBox updates
  let dirtyBoxCanvas = document.createElement("canvas");
  let dirtyBoxCtx = dirtyBoxCanvas.getContext("2d");

  // Track overlay canvas dimensions to avoid unnecessary resizing
  let overlayCanvasWidth = 0;
  let overlayCanvasHeight = 0;

  let imageData;
  let fixedWidth, fixedHeight;
  let projectedWidth, projectedHeight;
  let canvasRect;

  // A post-process / effects layer.
  let glaze = { on: false };
  let currentGlaze;

  const glazeComposite = document.createElement("canvas");
  const glazeCompositeCtx = glazeComposite.getContext("2d");

  let needsReframe = false;
  let needsReappearance = false;
  let reframeJustCompleted = false; // Track when we just finished reframing
  let dimensionMismatchCount = 0; // Track freeze loop iterations during reframe
  let freezeFrame = false,
    freezeFrameFrozen = false,
    freezeFrameGlaze = false,
    glazeReady = true; // Track when glaze shaders have finished loading after resize
  let awaitingReframePixels = false; // Keep freeze frame until valid pixels arrive
  let reframeDrawn = false;
  let reframeDrawnAt = 0;
  let lastReframeTime = 0; // Track when the last reframe was requested for debouncing

  function markReframePixelsReceived(source, width, height) {
    if (!awaitingReframePixels) return;
    if (width && height) {
      if (width !== canvas.width || height !== canvas.height) {
        if (window.acReframeDebug) {
          console.log("⚠️ REFRAME: pixel size mismatch", {
            source,
            width,
            height,
            canvasWidth: canvas.width,
            canvasHeight: canvas.height,
          });
        }
        return;
      }
    }
    awaitingReframePixels = false;
    if (window.acReframeDebug) {
      console.log("✅ REFRAME: pixels received", { source, width, height });
    }
  }

  const screen = apiObject("pixels", "width", "height");
  let subdivisions = 1; // Gets set in frame.

  // Track UI scaling state to avoid redundant operations
  let currentUiScale = 0;
  let uiContextScaled = false;

  const REFRAME_DELAY = 0; // Instant reframe (was 80ms)
  let curReframeDelay = REFRAME_DELAY;
  let lastGap = undefined;
  // Use URL parameter, or acPACK_DENSITY (for bundles), or default to 2
  let density = resolution.density !== undefined 
    ? resolution.density 
    : (window.acPACK_DENSITY !== undefined 
        ? window.acPACK_DENSITY 
        : 2);

  const startGap =
    location.host.indexOf("botce") > -1 || AestheticExtension ? 0 : 8;

  // Runs one on boot & every time display resizes to adjust the framebuffer.
  function frame(width, height, gap) {
    // Notify parent on first frame setup
    if (!imageData) {
      if (window.acBOOT_LOG) {
        window.acBOOT_LOG("setting up display");
      } else if (window.parent) {
        window.parent.postMessage({ 
          type: "boot-log", 
          message: "setting up display" 
        }, "*");
      }
    }

    gap === 0
      ? document.body.classList.add("nogap")
      : document.body.classList.remove("nogap");

    if (gap === undefined) gap = lastGap ?? startGap;
    lastGap = gap;

    // Cache the current canvas if needed (only if not already frozen - resize handler handles the capture now)
    if (
      freezeFrame &&
      !freezeFrameFrozen && // Skip if already captured in resize handler
      imageData &&
      imageData.data &&
      imageData.data.buffer &&
      imageData.data.buffer.byteLength > 0 &&
      !underlayFrame // Don't show freeze frame during tape playback
    ) {
      if (debug && logs.frame) {
        console.log(
          "🥶 Freezing:",
          freezeFrame,
          imageData.width,
          imageData.height,
        );
      }

      // Size freeze frame canvas to match imageData dimensions (the actual pixel content)
      freezeFrameCan.width = imageData.width;
      freezeFrameCan.height = imageData.height;
      
      // Position freeze frame to cover entire wrapper area
      // Using 100% ensures it covers regardless of canvas scaling/positioning
      freezeFrameCan.style.width = "100%";
      freezeFrameCan.style.height = "100%";
      freezeFrameCan.style.left = "0";
      freezeFrameCan.style.top = "0";
      freezeFrameCan.style.imageRendering = "pixelated";

      // Capture the current frame content
      if (freezeFrameGlaze) {
        Glaze.freeze(ffCtx);
        freezeFrameGlaze = false;
      } else {
        // Best approach: use imageData directly if available (avoids WebGL readback issues)
        if (imageData && imageData.data && imageData.data.buffer && imageData.data.buffer.byteLength > 0) {
          try {
            ffCtx.putImageData(imageData, 0, 0);
          } catch (e) {
            // Fallback to canvas copy if putImageData fails
            const webglCompositeIsActive = webglBlitter?.isReady() && webglCompositeCanvas.style.display !== "none";
            const freezeSource = webglCompositeIsActive ? webglCompositeCanvas : canvas;
            ffCtx.drawImage(freezeSource, 0, 0);
          }
        } else {
          // Fallback: capture from appropriate canvas
          const webglCompositeIsActive = webglBlitter?.isReady() && webglCompositeCanvas.style.display !== "none";
          const freezeSource = webglCompositeIsActive ? webglCompositeCanvas : canvas;
          ffCtx.drawImage(freezeSource, 0, 0);
        }
      }

      if (!wrapper.contains(freezeFrameCan)) {
        wrapper.append(freezeFrameCan);
      } else {
        freezeFrameCan.style.removeProperty("opacity");
      }
      
      // Force browser to paint the freeze frame before we continue
      // This ensures it's visible before we clear the canvases
      freezeFrameCan.offsetHeight;

      freezeFrameFrozen = true;
    }

    // Find the width and height of our default screen and native projection.
    width = width || fixedWidth;
    height = height || fixedHeight;

    const gapSize = gap * window.devicePixelRatio;

    subdivisions = 1;

    if (width === undefined && height === undefined) {
      // Automatically set and frame a reasonable resolution.
      // Or pull from density.
      let ratio = density || window.devicePixelRatio;
      if (!density && window.devicePixelRatio === 1) ratio = 3; // Always force a screen density of 3 on non-retina displays.
      subdivisions = ratio;

      width =
        floor(window.innerWidth / subdivisions) - floor(gapSize / subdivisions);
      height =
        floor(window.innerHeight / subdivisions) -
        floor(gapSize / subdivisions);

      if (TikTok) height -= gap * 3;

      projectedWidth = round(width * density);
      projectedHeight = round(height * density);
    } else {
      // Or do it manually if both width and height are defined.
      fixedWidth = width;
      fixedHeight = height;

      const scale = min(window.innerWidth / width, window.innerHeight / height);

      projectedWidth = round(width * scale - gapSize);
      projectedHeight = round(height * scale - gapSize);
    }

    if (debug && logs.frame)
      console.info(
        "🖼 Frame:",
        width,
        height,
        "🖥 Window:",
        window.innerWidth,
        window.innerHeight,
      );

    // Send a message about this new width and height to any hosting frames.
    // parent.postMessage({ width: projectedWidth, height: projectedHeight }, "*");

    // TODO: Changing this width and height here will clear the canvas... which is no good...

    // ffCtx.drawImage(canvas, 0, 0);

    // TODO: How can I copy the pixels from canvas before changing it's width
    //       and height, and then copy them back after changing it here?
    // ctx.drawImage(canvas, 0, 0);

    // Create a temporary canvas
    const tempCanvas = document.createElement("canvas");
    const tempCtx = tempCanvas.getContext("2d");

    // Copy existing canvas contents from clean imageData (not from canvas which may have overlays)
    tempCanvas.width = canvas.width;
    tempCanvas.height = canvas.height;
    if (
      imageData &&
      imageData.width === canvas.width &&
      imageData.height === canvas.height &&
      imageData.data &&
      imageData.data.buffer &&
      imageData.data.buffer.byteLength > 0
    ) {
      // Use clean imageData to avoid copying overlays (only if data is not detached)
      tempCtx.putImageData(imageData, 0, 0);
    } else {
      // Fallback to copying from canvas if imageData doesn't match or data is detached
      tempCtx.drawImage(canvas, 0, 0);
    }

    // Hide rendering canvases before resize to prevent black flash
    // (resizing a canvas clears it, and the freeze frame will provide visual continuity)
    // Note: Keep overlay canvas visible - it contains UI elements that should stay visible during resize
    const wasWebglVisible = webglCompositeCanvas.style.display !== "none";
    const wasWebgpuVisible = webgpuCanvas.style.display !== "none";
    if (wasWebglVisible) webglCompositeCanvas.style.display = "none";
    if (wasWebgpuVisible) webgpuCanvas.style.display = "none";

    // Resize the original canvas
    canvas.width = width;
    canvas.height = height;

    // Resize WebGPU canvas to match
    webgpuCanvas.width = width;
    webgpuCanvas.height = height;

    // Resize WebGL composite and overlay canvases to match
    webglCompositeCanvas.width = width;
    webglCompositeCanvas.height = height;
    overlayCan.width = width;
    overlayCan.height = height;
    recordingUICan.width = width;
    recordingUICan.height = height;

    // Restore the clean pixels onto the resized canvas
    ctx.drawImage(tempCanvas, 0, 0);

    // 🎨 REFRAME BACKGROUND FIX: Fill any new extended areas with background color
    // But skip during very early initialization to prevent purple flash
    const now = performance.now();
    const isVeryEarlyLoad = !globalThis.pageLoadTime || (now - globalThis.pageLoadTime) < 100;
    
    // Initialize page load time on first run
    if (!globalThis.pageLoadTime) {
      globalThis.pageLoadTime = now;
    }
    
    if ((width > tempCanvas.width || height > tempCanvas.height) && !isVeryEarlyLoad) {
      const skipReframeFill =
        !!underlayFrame ||
        (currentPiece && (
          currentPiece.includes("/disks/replay") ||
          currentPiece.includes("/disks/video")
        ));

      if (skipReframeFill) {
        if (width > tempCanvas.width) {
          ctx.clearRect(tempCanvas.width, 0, width - tempCanvas.width, height);
        }
        if (height > tempCanvas.height) {
          ctx.clearRect(0, tempCanvas.height, width, height - tempCanvas.height);
        }
      } else {
        const coerceToRGB = (color) => {
          if (!color) return null;
          try {
            if (typeof globalThis.graph?.color?.coerce === "function") {
              const rgb = globalThis.graph.color.coerce(color);
              if (Array.isArray(rgb) && rgb.length >= 3) {
                return [rgb[0], rgb[1], rgb[2]];
              }
            }
          } catch (_) {
            // graph.color.coerce isn't always available (e.g. non-KidLisp pieces)
          }

          try {
            coerceToRGB.ctx = coerceToRGB.ctx || document.createElement("canvas").getContext("2d", { willReadFrequently: true });
            const parseCtx = coerceToRGB.ctx;
            parseCtx.fillStyle = "#000";
            parseCtx.clearRect(0, 0, 1, 1);
            parseCtx.fillStyle = color;
            parseCtx.fillRect(0, 0, 1, 1);
            const data = parseCtx.getImageData(0, 0, 1, 1).data;
            return [data[0], data[1], data[2]];
          } catch (_) {
            return null;
          }
        };

        const sampleExistingPixel = () => {
          if (!tempCanvas.width || !tempCanvas.height) {
            return null;
          }

          const samplePoints = [
            [0, 0],
            [Math.max(tempCanvas.width - 1, 0), 0],
            [0, Math.max(tempCanvas.height - 1, 0)],
            [Math.max(tempCanvas.width - 1, 0), Math.max(tempCanvas.height - 1, 0)],
            [Math.floor(tempCanvas.width / 2), Math.floor(tempCanvas.height / 2)],
          ];

          for (const [sx, sy] of samplePoints) {
            try {
              const data = tempCtx.getImageData(sx, sy, 1, 1).data;
              if (data[3] !== 0) {
                return [data[0], data[1], data[2]];
              }
            } catch (_) {
              // Canvas may be tainted; abort sampling altogether
              break;
            }
          }

          return null;
        };

        let persistentColor = null;
        try {
          persistentColor = typeof globalThis.getPersistentFirstLineColor === "function"
            ? globalThis.getPersistentFirstLineColor()
            : null;
        } catch (_) {
          persistentColor = null;
        }

        let fillSpec = resolveBackgroundFillSpec(persistentColor);
        let fallbackColor = null;

        if (!fillSpec && typeof globalThis.globalKidLispInstance?.getBackgroundFillColor === "function") {
          fallbackColor = globalThis.globalKidLispInstance.getBackgroundFillColor();
          fillSpec = resolveBackgroundFillSpec(fallbackColor);
        }

        let appliedFade = false;
        let fillRGB = null;

        if (fillSpec?.type === "fade") {
          fillFadeExpansionsOnCanvas(
            ctx,
            width,
            height,
            tempCanvas.width,
            tempCanvas.height,
            fillSpec.fadeInfo,
          );
          appliedFade = true;
        } else if (fillSpec?.type === "solid") {
          fillRGB = fillSpec.rgba.slice(0, 3);
        }

        if (!fillSpec) {
          fillRGB = coerceToRGB(persistentColor);
          if (!fillRGB && fallbackColor !== null) {
            fillRGB = coerceToRGB(fallbackColor);
          }
        }

        if (!appliedFade) {
          if (!fillRGB && imageData && imageData.data && imageData.data.length >= 4) {
            const data = imageData.data;
            for (let i = 0; i < data.length; i += 4) {
              if (data[i + 3] !== 0) {
                fillRGB = [data[i], data[i + 1], data[i + 2]];
                break;
              }
            }
          }

          if (!fillRGB) {
            fillRGB = sampleExistingPixel();
          }

          if (!fillRGB) {
            // Fallback to neutral black if everything else fails
            fillRGB = [0, 0, 0];
          }

          const [r, g, b] = fillRGB;
          ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;

          if (width > tempCanvas.width) {
            ctx.fillRect(tempCanvas.width, 0, width - tempCanvas.width, height);
          }

          if (height > tempCanvas.height) {
            ctx.fillRect(0, tempCanvas.height, width, height - tempCanvas.height);
          }
        }
      }
    }

    tempCanvas.width = glazeComposite.width;
    tempCanvas.height = glazeComposite.height;

    tempCtx.drawImage(glazeComposite, 0, 0);

    glazeComposite.width = canvas.width;
    glazeComposite.height = canvas.height;
    glazeCompositeCtx.drawImage(tempCanvas, 0, 0);

    uiCanvas.width = projectedWidth * window.devicePixelRatio;
    uiCanvas.height = projectedHeight * window.devicePixelRatio;

    // Reset UI scaling state when canvas size changes
    uiContextScaled = false;
    currentUiScale = 0;

    // Horizontal and vertical offsetting of the wrapper.
    // In nogap mode, fill the entire viewport to avoid subpixel gaps
    if (gap === 0) {
      wrapper.style.top = "0";
      wrapper.style.left = "0";
      wrapper.style.width = "100%";
      wrapper.style.height = "100%";
      
      // Canvases also fill 100% in nogap mode to avoid subpixel rounding gaps
      canvas.style.width = "100%";
      canvas.style.height = "100%";
      webglCompositeCanvas.style.width = "100%";
      webglCompositeCanvas.style.height = "100%";
      webgpuCanvas.style.width = "100%";
      webgpuCanvas.style.height = "100%";
      overlayCan.style.width = "100%";
      overlayCan.style.height = "100%";
      recordingUICan.style.width = "100%";
      recordingUICan.style.height = "100%";
      uiCanvas.style.width = "100%";
      uiCanvas.style.height = "100%";
    } else if (TikTok) {
      wrapper.style.top = `${8 * 1.5}px`;
      wrapper.style.left = (window.innerWidth - projectedWidth) / 2 + "px";
      wrapper.style.width = projectedWidth + "px";
      wrapper.style.height = projectedHeight + "px";
      
      canvas.style.width = projectedWidth + "px";
      canvas.style.height = projectedHeight + "px";
      webglCompositeCanvas.style.width = projectedWidth + "px";
      webglCompositeCanvas.style.height = projectedHeight + "px";
      webgpuCanvas.style.width = projectedWidth + "px";
      webgpuCanvas.style.height = projectedHeight + "px";
      overlayCan.style.width = projectedWidth + "px";
      overlayCan.style.height = projectedHeight + "px";
      recordingUICan.style.width = projectedWidth + "px";
      recordingUICan.style.height = projectedHeight + "px";
      uiCanvas.style.width = projectedWidth + "px";
      uiCanvas.style.height = projectedHeight + "px";
    } else {
      wrapper.style.top = (window.innerHeight - projectedHeight) / 2 + "px";
      wrapper.style.left = (window.innerWidth - projectedWidth) / 2 + "px";
      wrapper.style.width = projectedWidth + "px";
      wrapper.style.height = projectedHeight + "px";
      
      canvas.style.width = projectedWidth + "px";
      canvas.style.height = projectedHeight + "px";
      webglCompositeCanvas.style.width = projectedWidth + "px";
      webglCompositeCanvas.style.height = projectedHeight + "px";
      webgpuCanvas.style.width = projectedWidth + "px";
      webgpuCanvas.style.height = projectedHeight + "px";
      overlayCan.style.width = projectedWidth + "px";
      overlayCan.style.height = projectedHeight + "px";
      recordingUICan.style.width = projectedWidth + "px";
      recordingUICan.style.height = projectedHeight + "px";
      uiCanvas.style.width = projectedWidth + "px";
      uiCanvas.style.height = projectedHeight + "px";
    }

    if (debug) {
      debugCanvas.width = projectedWidth;
      debugCanvas.height = projectedHeight;
      debugCanvas.style.width = projectedWidth + "px";
      debugCanvas.style.height = projectedHeight + "px";
    }

    if (
      imageData &&
      imageData.data &&
      imageData.data.buffer &&
      imageData.data.buffer.byteLength > 0
    ) {
      ctx.putImageData(imageData, 0, 0);
    } else {
      imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
      // This will have zero alpha.
    }

    // Store clean pixel data for worker communication (before any overlays are drawn)
    assign(screen, { pixels: imageData.data, width, height });

    TwoD?.frame(width, height, wrapper); // Reframe the 2D GPU layer.

    // Add the canvas, modal, and uiCanvas when we first boot up.
    if (!wrapper.contains(canvas)) {
      perf.markBoot("canvas-setup-start");
      wrapper.append(canvas);
      wrapper.append(webglCompositeCanvas);
      wrapper.append(modal);

      const bumper = document.createElement("div");
      bumper.id = "bumper";
      modal.append(bumper);

      wrapper.append(overlayCan);
      wrapper.append(recordingUICan); // Recording UI canvas (NOT captured in tape)
      wrapper.append(uiCanvas);
      if (debug) wrapper.append(debugCanvas);
      wrapper.append(webgpuCanvas); // Add WebGPU canvas (initially hidden)
      wrapper.append(statsOverlay); // Add stats overlay (highest z-index)
      document.body.append(wrapper);
      perf.markBoot("dom-appended");

      // Initialize WebGPU 2D renderer with dedicated canvas (skip in PACK mode)
      if (!window.acPACK_MODE) {
        initWebGPU(webgpuCanvas).catch(err => {
          console.warn("⚠️ WebGPU initialization failed:", err);
        });
      }

      if (!webglBlitter) {
        webglBlitter = createWebGLBlitter(webglCompositeCanvas);
        const ok = webglBlitter.init();
        if (!ok) webglBlitter = null;
      }

      const fonts = [
        "berkeley-mono-variable.css",
        "ywft-processing-regular.css",
        "ywft-processing-light.css",
        "ywft-processing-bold.css",
      ];

      // Load fonts post-boot.
      fonts.forEach((font) => {
        const link = document.createElement("link");
        link.rel = "stylesheet";
        link.crossOrigin = "anonymous";
        
        // Use origin-aware font loading
        let fontUrl;
        try {
          // Check if we're in OBJKT/PACK mode (sandboxed with bundled fonts)
          if (window.acPACK_MODE) {
            // In OBJKT mode, use relative paths to bundled fonts
            fontUrl = "./type/webfonts/" + font;
          } else if (window.acSPIDER) {
            // In SPIDER mode, leech fonts from aesthetic.computer
            fontUrl = "https://aesthetic.computer/type/webfonts/" + font;
          } else {
            // Check if we're in development environment
            const isDevelopment = location.hostname === 'localhost' && location.port;
            if (isDevelopment) {
              // In development, fonts are served from the root /type/webfonts/ path
              fontUrl = "/type/webfonts/" + font;
            } else {
              // In production or sandboxed iframe, use the standard path
              fontUrl = "/type/webfonts/" + font;
            }
          }
        } catch (err) {
          // Fallback to standard path if there's any error
          fontUrl = "/type/webfonts/" + font;
        }
        
        link.href = fontUrl;
        document.body.append(link);
      });

      if ("fonts" in document) {
        (async () => {
          try {
            // Wait for stylesheet to load first
            await new Promise((resolve) => {
              const checkFonts = () => {
                // Check if at least one font face is available
                const fontFaces = Array.from(document.fonts);
                if (fontFaces.length > 0) {
                  resolve();
                } else {
                  setTimeout(checkFonts, 50);
                }
              };
              checkFonts();
            });

            // Now load the fonts explicitly
            await Promise.all([
              document.fonts.load("1em YWFTProcessing-Light"),
              document.fonts.load("1em YWFTProcessing-Regular"),
              document.fonts.load("bold 1em YWFTProcessing-Regular"),
              document.fonts.load("normal 1em YWFTProcessing-Light"),
            ]);
          } catch (error) {
            console.warn("⚠️ Font loading during boot failed:", error);
            // Fallback to fonts.ready
            await document.fonts.ready;
          }
        })();
        // document.fonts.load("1em Berkeley Mono Variable").then(() => {
        //   setTimeout(() => {
        //     document.getElementById("console")?.classList.remove("hidden");
        //   }, 250);
        // });
      }

      let timeout;
      let lastWidth = window.innerWidth;
      let lastHeight = window.innerHeight;

      window.addEventListener("resize", (e) => {
        if (
          lastWidth === window.innerWidth &&
          lastHeight === window.innerHeight
        ) {
          return;
        }

        lastWidth = window.innerWidth;
        lastHeight = window.innerHeight;

        // Check to see if we are in "native-cursor" mode and hide
        // #aesthetic.computer for the resize if we aren't.
        if (document.body.classList.contains("native-cursor") === false) {
          // wrapper.classList.add("hidden");
        }

        window.clearTimeout(timeout); // Small timer to save on performance.
        timeout = setTimeout(() => {
          // Capture freeze frame before reframing to prevent black flicker
          if (imageData && imageData.data && imageData.data.buffer && imageData.data.buffer.byteLength > 0) {
            freezeFrame = true;
            freezeFrameGlaze = glaze.on;
            
            // Capture freeze frame content BEFORE resizing (resizing clears canvas)
            // Only resize if dimensions are different to avoid unnecessary clears
            if (freezeFrameCan.width !== imageData.width || freezeFrameCan.height !== imageData.height) {
              freezeFrameCan.width = imageData.width;
              freezeFrameCan.height = imageData.height;
            }
            
            // Capture current frame to freeze frame canvas
            if (freezeFrameGlaze) {
              Glaze.freeze(ffCtx);
            } else {
              try {
                ffCtx.putImageData(imageData, 0, 0);
              } catch (e) {
                // Fallback to canvas copy
                const webglCompositeIsActive = webglBlitter?.isReady() && webglCompositeCanvas.style.display !== "none";
                const freezeSource = webglCompositeIsActive ? webglCompositeCanvas : canvas;
                ffCtx.drawImage(freezeSource, 0, 0);
              }
            }
            
            // Also capture overlay canvas (HUD/labels) if it has content
            if (overlayCan.width > 0 && overlayCan.height > 0 && overlayCan.style.display !== "none") {
              freezeOverlayCan.width = overlayCan.width;
              freezeOverlayCan.height = overlayCan.height;
              freezeOverlayCtx.drawImage(overlayCan, 0, 0);
              freezeOverlayCan.style.width = "100%";
              freezeOverlayCan.style.height = "100%";
              freezeOverlayCan.style.left = "0";
              freezeOverlayCan.style.top = "0";
              freezeOverlayCan.style.imageRendering = "pixelated";
              if (!wrapper.contains(freezeOverlayCan)) {
                wrapper.append(freezeOverlayCan);
              }
              freezeOverlayCan.style.removeProperty("opacity");
            }
            
            // Position freeze frame
            freezeFrameCan.style.width = "100%";
            freezeFrameCan.style.height = "100%";
            freezeFrameCan.style.left = "0";
            freezeFrameCan.style.top = "0";
            freezeFrameCan.style.imageRendering = "pixelated";
            
            // Ensure freeze frame is visible
            if (!wrapper.contains(freezeFrameCan)) {
              wrapper.append(freezeFrameCan);
            }
            freezeFrameCan.style.removeProperty("opacity");
            
            // Force paint before continuing
            freezeFrameCan.offsetHeight;
            freezeFrameFrozen = true;
          }
          awaitingReframePixels = true;
          reframeDrawn = false;
          reframeDrawnAt = 0;
          lastReframeTime = performance.now();
          if (window.acReframeDebug) {
            console.log("🧊 REFRAME: awaiting pixels", {
              width: imageData?.width,
              height: imageData?.height,
            });
          }
          needsReframe = true; // This makes zooming work / not work.
          curReframeDelay = REFRAME_DELAY;
        }, curReframeDelay); // Is this needed?
      });

      // Prevent canvas touchstart events from triggering magnifying glass on
      // iOS Safari, unless a link is pressed.
      wrapper.addEventListener(
        "touchstart",
        function (event) {
          if (
            (document.hasFocus() &&
              !ticketWrapper &&
              event.target.tagName !== "A" &&
              event.target.tagName !== "IMG") ||
            event.touches.length > 2 // Prevent undo pop-up in Mobile Safari.
          ) {
            event.preventDefault();
          }
        },
        false,
      );
    }

    canvasRect = canvas.getBoundingClientRect();

    // Glaze.clear();

    // A native resolution canvas for drawing cursors, system UI, and effects.
    if (glaze.on) {
      glazeReady = false; // Mark glaze as not ready during shader reload
      currentGlaze = Glaze.on(
        canvas.width,
        canvas.height,
        canvasRect,
        projectedWidth,
        projectedHeight,
        wrapper,
        glaze.type,
        () => {
          glazeReady = true; // Glaze shaders loaded, safe to show
          Glaze.unfreeze(); // Make glaze canvas visible immediately
          send({ type: "needs-paint" }); // Render a frame through glaze
        },
      );
    } else {
      Glaze.off();
      glaze.on = false;
      canvas.style.removeProperty("opacity");
    }

    needsReframe = false;
    reframeJustCompleted = true; // Mark that we just completed a reframe
    needsReappearance = true; // Only for `native-cursor` mode.
    // Send reframed BEFORE needs-paint so worker updates dimensions before painting
    if (underlayFrame) {
      console.log('📤 REFRAME: Sending reframe message to worker. New dimensions:', width, 'x', height);
    }
    awaitingReframePixels = true;
    reframeDrawn = false;
    reframeDrawnAt = 0;
    send({
      type: "reframed",
      content: {
        innerWidth: window.innerWidth,
        innerHeight: window.innerHeight,
        subdivisions,
        width,  // Include the new screen dimensions so the worker can update immediately
        height,
      },
    });
    send({ type: "needs-paint" });
  }

  // Used by `disk` to set the metatags by default when a piece loads. It can
  // be overridden using `meta` inside of `boot` for any given piece.
  // TODO: Some meta tags in practice use image_url & icon_url it seems.
  //       (Like in `hell_-world` or `freaky-flowers`) 23.10.25.20.32
  function setMetatags(meta) {
    if (meta?.title) {
      // Don't override title in OBJKT mode - let the pack-time title remain
      if (!checkPackMode()) {
        document.title = meta.title;
      }
      const ogTitle = document.querySelector('meta[name="og:title"]');
      if (ogTitle) ogTitle.content = meta.title;
      const twitterTitle = document.querySelector('meta[name="twitter:title"]');
      if (twitterTitle) twitterTitle.content = meta.title;
    }
    if (meta?.desc) {
      const ogDesc = document.querySelector('meta[name="og:description"]');
      if (ogDesc) ogDesc.content = meta.desc;
      const desc = document.querySelector('meta[name="description"]');
      if (desc) desc.content = meta.desc;
    }
    if (meta?.img?.og) {
      const ogImg = document.querySelector('meta[name="og:image"]');
      if (ogImg) ogImg.content = meta.img.og;
    }
    if (meta?.img?.twitter) {
      const twitterImg = document.querySelector('meta[name="twitter:image"]');
      if (twitterImg) twitterImg.content = meta.img.twitter;
    }

    const icon = document.querySelector('link[rel="icon"]');

    if (icon) {
      if (meta?.icon_url) {
        if (icon.href !== meta.icon_url) icon.href = meta.icon_url;
      } else if (meta?.img?.icon) {
        if (icon.href !== meta.img.icon) icon.href = meta.img.icon;
      }
    }

    if (meta?.url) {
      // This might need to be conditional / opt-in?
      // document.querySelector('meta[name="twitter:player"').content = meta.url;
    }
  }

  // *** External Library Dependency Injection ***

  // FFMPEG.WASM
  async function loadFFmpeg() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/ffmpeg/ffmpeg.min.js";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug && logs.deps) console.log("📼 FFmpeg has loaded.", FFmpeg);
        resolve(FFmpeg);
      };
      document.head.appendChild(script);
    });
    // return await import(`/aesthetic.computer/dep/@ffmpeg/ffmpeg-core.js`);
  }

  async function loadJSZip() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/jszip.min.js";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug && logs.deps)
          console.log("🤐 JSZip has loaded.", window.JSZip);
        resolve(window.JSZip);
      };

      document.head.appendChild(script);
    });
  }

  async function loadStripe() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "https://js.stripe.com/v3/";
      script.crossOrigin = "anonymous";

      script.onerror = function (err) {
        reject(err, s);
      };

      script.onload = function handleScriptLoaded() {
        if (debug && logs.deps)
          console.log("🦓 Stripe has loaded.", window.Stripe);
        resolve(window.Stripe);
      };

      document.head.appendChild(script);
    });
  }

  // THREE.JS (With a thin wrapper called ThreeD).
  let ThreeD;
  let ThreeDBakeQueue = [];
  async function loadThreeD() {
    ThreeD = await import(`./lib/3d.mjs`);
    ThreeD.initialize(
      wrapper,
      Loop.mainLoop,
      receivedDownload,
      receivedUpload,
      send,
    );
  }

  // WebGPU 2D Renderer
  let webGPUInitialized = false;
  let activeGPUBackend = null; // 🎨 Current renderer backend
  
  async function initWebGPU(canvas) {
    if (webGPUInitialized) return;
    
    // Try to initialize with fallback chain
    const preferredBackend = localStorage.getItem("ac-gpu-backend") || "webgpu";
    activeGPUBackend = await initGPU(canvas, preferredBackend);
    
    if (activeGPUBackend) {
      webGPUInitialized = true;
      log.gpu.success?.(`GPU renderer ready: ${activeGPUBackend.getName()}`);
      // Expose for debugging/artery access
      window.GPU = activeGPUBackend;
      window.switchGPUBackend = async (name) => {
        const newBackend = await switchBackend(canvas, name);
        if (newBackend) {
          activeGPUBackend = newBackend;
          window.GPU = newBackend;
          log.gpu.success?.(`Switched to: ${name}`);
          return true;
        }
        return false;
      };
    } else {
      log.gpu.warn?.("No GPU backend available");
    }
  }

  // Web3
  async function loadWeb3() {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/web3/web3.min.js";

      script.onerror = (err) => reject(err, s);

      script.onload = function handleScriptLoaded() {
        if (debug) console.log("🕸️3️⃣ Ready...");
        resolve(Web3);
      };

      document.head.appendChild(script);
    });
  }

  // Tezos Wallet (Beacon SDK + Taquito)
  let tezosWallet = null;
  let tezosTk = null;
  let tezosLoading = null;
  
  // 🔷 Persistent wallet state (survives piece transitions)
  const walletState = {
    connected: false,
    address: null,
    balance: null,
    network: "ghostnet",
    domain: null,
  };
  
  // Broadcast wallet state to disk.mjs
  function broadcastWalletState() {
    send({ type: "wallet:state", content: { ...walletState } });
  }
  
  // 🔷 Tezos Wallet Connection (Beacon-free)
  // Uses direct communication with wallet extensions and localStorage for session persistence
  
  const TEZOS_RPC = {
    mainnet: "https://mainnet.api.tez.ie",
    ghostnet: "https://ghostnet.ecadinfra.com",
  };
  
  // Restore wallet session from localStorage
  async function restoreWalletSession() {
    try {
      const savedSession = localStorage.getItem("ac:tezos:session");
      if (savedSession) {
        const session = JSON.parse(savedSession);
        log.wallet.debug("Restoring wallet session:", session.address.slice(0, 8) + "...", "walletType:", session.walletType);
        
        walletState.connected = true;
        walletState.address = session.address;
        walletState.network = session.network || "ghostnet";
        walletState.walletType = session.walletType || "unknown";
        
        const rpcUrl = TEZOS_RPC[walletState.network] || TEZOS_RPC.ghostnet;
        
        // Recreate the tezosWallet object with sendOperations for signing
        if (session.walletType === "temple") {
          // Recreate Temple client for signing operations
          const templeClient = createTempleClient();
          templeClient.init();
          
          // Verify Temple is still available
          const available = await templeClient.isAvailable();
          if (available) {
            tezosWallet = {
              _client: templeClient,
              _rpcUrl: rpcUrl,
              _network: walletState.network,
              pkh: () => session.address,
              sign: async (payload) => {
                const result = await templeClient.request({
                  type: "SIGN_REQUEST",
                  payload,
                  sourcePkh: session.address
                });
                return result?.signature;
              },
              sendOperations: async (operations) => {
                const result = await templeClient.request({
                  type: "OPERATION_REQUEST",
                  sourcePkh: session.address,
                  opParams: operations
                });
                return result?.opHash;
              }
            };
            log.wallet.success("Temple wallet restored");
          } else {
            log.wallet.debug("Temple extension not available, session stale");
            walletState.connected = false;
            clearWalletSession();
            broadcastWalletState();
            return false;
          }
        } else if (session.walletType === "kukai") {
          // For Kukai, we need to re-import Beacon SDK
          // This is lazy - it will be re-established on first signing attempt
          tezosWallet = {
            _rpcUrl: rpcUrl,
            _network: walletState.network,
            _needsReconnect: true, // Flag to trigger reconnection on first use
            pkh: () => session.address,
            sendOperations: async (operations) => {
              // Reconnect via Beacon on first signing attempt
              log.wallet.log("Kukai needs reconnection for signing...");
              throw new Error("Kukai session expired - please reconnect wallet");
            }
          };
          log.wallet.debug("Kukai wallet session restored (signing requires reconnection)");
        }
        
        // Fetch fresh balance and domain
        Promise.all([
          fetchTezosBalanceForBios(session.address, walletState.network),
          fetchTezosDomain(session.address, walletState.network)
        ]).then(([balance, domain]) => {
          walletState.balance = balance;
          walletState.domain = domain;
          broadcastWalletState();
        });
        
        broadcastWalletState();
        return true;
      }
      log.wallet.verbose("No saved wallet session");
    } catch (err) {
      log.wallet.debug("restoreWalletSession error:", err.message);
    }
    return false;
  }
  
  // Helper to create Temple client (used by both connect and restore)
  function createTempleClient() {
    return {
      _reqId: 0,
      _pending: new Map(),
      
      init() {
        window.addEventListener("message", this._handleMessage.bind(this));
      },
      
      _handleMessage(event) {
        if (event.source !== window || event.origin !== window.origin) return;
        const data = event.data || {};
        
        if (data.type === "TEMPLE_PAGE_RESPONSE") {
          const { payload, reqId } = data;
          if (this._pending.has(reqId)) {
            const { resolve, reject } = this._pending.get(reqId);
            this._pending.delete(reqId);
            if (payload?.error) {
              reject(new Error(payload.error.message || payload.error));
            } else {
              resolve(payload);
            }
          }
        }
        
        if (data.type === "TEMPLE_PAGE_ERROR_RESPONSE") {
          const { payload, reqId } = data;
          if (this._pending.has(reqId)) {
            const { reject } = this._pending.get(reqId);
            this._pending.delete(reqId);
            reject(new Error(payload?.message || "Temple error"));
          }
        }
      },
      
      async request(payload) {
        const reqId = ++this._reqId;
        return new Promise((resolve, reject) => {
          this._pending.set(reqId, { resolve, reject });
          
          window.postMessage({
            type: "TEMPLE_PAGE_REQUEST",
            payload,
            reqId
          }, window.origin);
          
          setTimeout(() => {
            if (this._pending.has(reqId)) {
              this._pending.delete(reqId);
              reject(new Error("Temple request timeout"));
            }
          }, 120000);
        });
      },
      
      async isAvailable() {
        try {
          const response = await Promise.race([
            this.request("PING"),
            new Promise((_, reject) => setTimeout(() => reject(new Error("timeout")), 3000))
          ]);
          return response === "PONG";
        } catch {
          return false;
        }
      },
      
      async connect(networkType) {
        const response = await this.request({
          type: "PERMISSION_REQUEST",
          network: networkType,
          appMeta: { name: "Aesthetic Computer" },
          force: true
        });
        return response;
      },
      
      async getCurrentPermission() {
        try {
          const response = await this.request({ type: "GET_CURRENT_PERMISSION_REQUEST" });
          return response;
        } catch {
          return null;
        }
      }
    };
  }
  
  // Save wallet session to localStorage
  function saveWalletSession(address, network, walletType) {
    localStorage.setItem("ac:tezos:session", JSON.stringify({
      address,
      network,
      walletType,
      timestamp: Date.now(),
    }));
  }
  
  // Clear wallet session
  function clearWalletSession() {
    localStorage.removeItem("ac:tezos:session");
  }
  
  // Initialize Tezos (no external SDK needed for basic operations)
  async function loadTezos(network = "ghostnet") {
    const rpcUrl = TEZOS_RPC[network] || TEZOS_RPC.ghostnet;
    
    // Create a minimal wallet interface for RPC operations
    if (!tezosWallet) {
      tezosWallet = {
        _rpcUrl: rpcUrl,
        _network: network,
      };
      tezosTk = { rpcUrl, network };
      console.log("🔷 Tezos RPC ready:", network);
    }
    
    return { wallet: tezosWallet, tezos: tezosTk };
  }
  
  // Connect wallet via Temple Wallet browser extension
  // Using direct postMessage communication with Temple's content script
  async function connectTezosWallet(network = "ghostnet", options = {}) {
    const rpcUrl = TEZOS_RPC[network] || TEZOS_RPC.ghostnet;
    const walletType = options.walletType || "temple";
    
    console.log(`🔷 Attempting ${walletType} wallet connection...`);
    
    // Kukai wallet via Beacon SDK
    if (walletType === "kukai") {
      return connectKukaiWallet(network, rpcUrl);
    }
    
    // Temple wallet via direct postMessage - use shared client creator
    const templeClient = createTempleClient();
    templeClient.init();
    
    // Check if Temple extension is available
    console.log("🔷 Checking for Temple extension...");
    const available = await templeClient.isAvailable();
    console.log("🔷 Temple Wallet available:", available);
    
    if (!available) {
      throw new Error("Temple Wallet not found. Install it from templewallet.com");
    }
    
    try {
      // Check for existing permission first
      const existingPerm = await templeClient.getCurrentPermission();
      console.log("🔷 Existing permission:", existingPerm);
      
      let address, publicKey;
      
      if (existingPerm?.pkh) {
        // Already connected
        address = existingPerm.pkh;
        publicKey = existingPerm.publicKey;
        console.log("🔷 Using existing connection:", address);
      } else {
        // Request new connection - this should open Temple popup
        console.log("🔷 Requesting connection...");
        const connectResult = await templeClient.connect(network === "mainnet" ? "mainnet" : "ghostnet");
        console.log("🔷 Connect result:", connectResult);
        
        if (connectResult?.type === "PERMISSION_RESPONSE") {
          address = connectResult.pkh;
          publicKey = connectResult.publicKey;
        } else if (connectResult?.pkh) {
          address = connectResult.pkh;
          publicKey = connectResult.publicKey;
        }
      }
      
      if (!address) {
        throw new Error("Connection cancelled or failed");
      }
      
      console.log("🔷 Temple connected:", address);
      
      walletState.connected = true;
      walletState.address = address;
      walletState.network = network;
      walletState.walletType = "temple";
      
      // Store client reference for signing operations
      tezosWallet = {
        _client: templeClient,
        _rpcUrl: rpcUrl,
        _network: network,
        _publicKey: publicKey,
        pkh: () => address,
        sign: async (payload) => {
          const result = await templeClient.request({
            type: "SIGN_REQUEST",
            payload,
            sourcePkh: address
          });
          return result?.signature;
        },
        sendOperations: async (operations) => {
          const result = await templeClient.request({
            type: "OPERATION_REQUEST",
            sourcePkh: address,
            opParams: operations
          });
          return result?.opHash;
        }
      };
      
      saveWalletSession(address, network, "temple");
      
      // Note: Tezos address persistence to MongoDB disabled - no auth token access in bios
      // TODO: Implement proper auth token access if server-side persistence is needed
      
      // Fetch balance and domain
      Promise.all([
        fetchTezosBalanceForBios(address, network),
        fetchTezosDomain(address, network)
      ]).then(([balance, domain]) => {
        console.log("🔷 Fetched balance:", balance, "domain:", domain);
        walletState.balance = balance;
        walletState.domain = domain;
        broadcastWalletState();
      }).catch(err => {
        console.warn("🔷 Failed to fetch balance/domain:", err);
      });
      
      broadcastWalletState();
      return address;
      
    } catch (err) {
      console.log("🔷 Temple connection error:", err?.message || err);
      throw new Error(err?.message || "Temple connection cancelled");
    }
  }
  
  // 🥝 Kukai Wallet Connection via Beacon SDK
  // Kukai/Other wallets via Beacon SDK
  // NOTE: Beacon SDK v4.6.3 has IndexedDB metrics issues
  // Using older stable version v4.0.12
  let beaconClient = null;
  let beaconNetwork = null;
  
  async function connectKukaiWallet(network, rpcUrl) {
    console.log("🥝 Connecting via Beacon SDK...");
    
    try {
      // Use older stable version that doesn't have metrics issues
      const beacon = await import("https://esm.sh/@airgap/beacon-sdk@4.0.12?bundle");
      const { DAppClient, NetworkType, PermissionScope } = beacon;
      
      const networkType = network === "mainnet" ? NetworkType.MAINNET : NetworkType.GHOSTNET;
      
      // Create new client if network changed or doesn't exist
      if (!beaconClient || beaconNetwork !== network) {
        // Clear old client if exists
        if (beaconClient) {
          try {
            await beaconClient.destroy();
          } catch (e) {}
        }
        
        beaconClient = new DAppClient({
          name: "Aesthetic Computer",
          preferredNetwork: networkType,
        });
        beaconNetwork = network;
        
        console.log("🥝 Beacon SDK loaded");
      }
      
      // Request permissions
      const permissions = await beaconClient.requestPermissions({
        scopes: [PermissionScope.SIGN, PermissionScope.OPERATION_REQUEST],
      });
      
      const address = permissions.address;
      const publicKey = permissions.publicKey;
      
      console.log("🥝 Kukai connected:", address);
      
      walletState.connected = true;
      walletState.address = address;
      walletState.network = network;
      walletState.walletType = "kukai";
      
      // Store client reference for signing
      tezosWallet = {
        _client: beaconClient,
        _rpcUrl: rpcUrl,
        _network: network,
        _publicKey: publicKey,
        pkh: () => address,
        sign: async (payload) => {
          const result = await beaconClient.requestSignPayload({
            signingType: "raw",
            payload: payload,
          });
          return result.signature;
        },
        sendOperations: async (operations) => {
          const result = await beaconClient.requestOperation({
            operationDetails: operations,
          });
          return result.transactionHash;
        },
      };
      
      saveWalletSession(address, network, "kukai");
      
      // Note: Tezos address persistence to MongoDB disabled - no auth token access in bios  
      // TODO: Implement proper auth token access if server-side persistence is needed
      
      // Fetch balance and domain
      Promise.all([
        fetchTezosBalanceForBios(address, network),
        fetchTezosDomain(address, network)
      ]).then(([balance, domain]) => {
        walletState.balance = balance;
        walletState.domain = domain;
        broadcastWalletState();
      });
      
      broadcastWalletState();
      return address;
      
    } catch (err) {
      console.log("🥝 Kukai connection error:", err?.message || err);
      throw new Error(err?.message || "Kukai connection cancelled");
    }
  }
  
  // Generate a Beacon pairing URI for mobile wallet connection
  // This creates a QR code that Temple/Kukai mobile can scan
  async function generatePairingUri(network = "ghostnet") {
    console.log("🔷 Generating pairing URI for mobile wallet...");
    
    try {
      // Generate our own P2P pairing request matching Beacon SDK format
      // Load dependencies
      const bs58check = await import("https://esm.sh/bs58check@3.0.1");
      const { generateKeyPairFromSeed } = await import("https://esm.sh/@stablelib/ed25519@1.0.3");
      const { hash } = await import("https://esm.sh/@stablelib/blake2b@1.0.1");
      
      // Generate a seed and keypair (matches Beacon's getKeypairFromSeed)
      const seed = crypto.randomUUID();
      const seedHash = hash(new TextEncoder().encode(seed), 32);
      const keyPair = generateKeyPairFromSeed(seedHash);
      
      // Public key as hex (32 bytes = 64 hex chars)
      const publicKey = Array.from(keyPair.publicKey).map(b => b.toString(16).padStart(2, '0')).join('');
      
      // Generate a unique ID
      const id = crypto.randomUUID();
      
      // Build P2P pairing request (matches Beacon SDK P2PPairingRequest class)
      const pairingRequest = {
        type: "p2p-pairing-request",
        id: id,
        name: "Aesthetic Computer",
        publicKey: publicKey,
        version: "3", // Beacon protocol version
        relayServer: "beacon-node-1.sky.papers.tech"
      };
      
      console.log("🔷 P2P Pairing request:", pairingRequest);
      console.log("🔷 Public key length:", publicKey.length);
      
      // Serialize with bs58check (how Beacon does it in Serializer.ts)
      const jsonStr = JSON.stringify(pairingRequest);
      const jsonBytes = new TextEncoder().encode(jsonStr);
      
      // bs58check.encode expects a Uint8Array
      const encoded = bs58check.default.encode(jsonBytes);
      
      console.log("🔷 Serialized pairing code:", encoded);
      console.log("🔷 Code length:", encoded.length);
      
      // Store the keypair for when we receive the response
      window._beaconPairingKeyPair = keyPair;
      window._beaconPairingRequest = pairingRequest;
      
      // Return the serialized code for QR generation
      return encoded;
      
    } catch (err) {
      console.log("🔷 Pairing error:", err?.message || err);
      throw new Error(err?.message || "Failed to generate pairing code");
    }
  }
  
  // Sign a message with the connected wallet
  // NOTE: Temple wallet doesn't support message signing via postMessage API
  // This function is kept for future use with wallets that support it
  async function signTezosMessage(message) {
    if (!walletState.connected || !tezosWallet) {
      throw new Error("Wallet not connected");
    }
    
    // Convert message to hex payload (Micheline format)
    const encoder = new TextEncoder();
    const bytes = encoder.encode(message);
    const hexPayload = "05" + "01" + bytes.length.toString(16).padStart(8, "0") + 
      Array.from(bytes).map(b => b.toString(16).padStart(2, "0")).join("");
    
    console.log("🔷 Signing message:", message);
    console.log("🔷 Hex payload:", hexPayload);
    
    try {
      const signature = await tezosWallet.sign(hexPayload);
      console.log("🔷 Signature:", signature);
      return signature;
    } catch (err) {
      console.log("🔷 Sign error:", err?.message || err);
      throw new Error(err?.message || "Signing cancelled");
    }
  }
  
  async function disconnectTezosWallet() {
    console.log("🔷 Disconnecting Tezos wallet");
    
    // If Beacon client exists, clear its permissions
    if (beaconClient) {
      try {
        await beaconClient.clearActiveAccount();
      } catch (e) {}
    }
    clearWalletSession();
    
    walletState.connected = false;
    walletState.address = null;
    walletState.balance = null;
    walletState.domain = null;
    walletState.walletType = null;
    
    broadcastWalletState();
  }
  
  async function getTezosAddress() {
    return walletState.address;
  }
  
  // Fetch Tezos balance from RPC (for bios.mjs wallet API)
  async function fetchTezosBalanceForBios(address, network = "ghostnet") {
    try {
      const rpcUrl = network === "mainnet" 
        ? "https://mainnet.api.tez.ie"
        : "https://ghostnet.ecadinfra.com";
      const res = await fetch(`${rpcUrl}/chains/main/blocks/head/context/contracts/${address}/balance`);
      if (res.ok) {
        const balanceMutez = await res.json();
        return parseInt(balanceMutez) / 1_000_000; // Convert mutez to tez
      }
    } catch (e) {
      console.warn("Failed to fetch Tezos balance:", e);
    }
    return null;
  }
  
  // Update user's Tezos address in MongoDB profile (called after wallet connection)
  // NOTE: Currently disabled - getToken() is not available in bios.mjs context
  // TODO: Implement if server-side persistence is needed via proper auth flow
  /*
  async function updateUserTezosAddress(address, network) {
    try {
      const token = await getToken();
      if (!token) {
        console.log("⚠️  Not logged in - skipping Tezos address save");
        return;
      }

      const response = await fetch("/api/update-tezos-address", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify({ address, network }),
      });

      if (response.ok) {
        console.log(`✅ Saved Tezos address to profile: ${address}`);
      } else {
        console.warn("Failed to save Tezos address:", await response.text());
      }
    } catch (err) {
      console.warn("Error saving Tezos address:", err);
    }
  }
  */

  // Resolve .tez domain for an address using TzKT API (more reliable than Tezos Domains GraphQL)
  // NOTE: Always use mainnet API since .tez domains are only registered on mainnet
  async function fetchTezosDomain(address, _network = "ghostnet") {
    try {
      // Always use mainnet TzKT API - .tez domains only exist on mainnet
      const apiBase = "https://api.tzkt.io";
      
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), 5000);
      
      // Look for domains where this address is set AND has reverse=true (primary domain)
      const url = `${apiBase}/v1/domains?address=${address}&reverse=true&select=name`;
      const res = await fetch(url, { signal: controller.signal });
      
      clearTimeout(timeout);
      
      if (res.ok) {
        const data = await res.json();
        // Returns array of domain strings (not objects) when using select=name
        if (data && data.length > 0 && data[0]) {
          const domain = data[0]; // Direct string, not data[0].name
          return domain;
        }
      }
    } catch (e) {
      // Silent - domain lookup is optional
    }
    return null;
  }
  
  async function callTezosContract(contractAddress, method, args, amount = 0) {
    // Use the connected wallet to send operations
    if (!walletState.connected || !tezosWallet) {
      throw new Error("Wallet not connected - use 'wallet' command first");
    }
    
    console.log("🔷 Calling contract:", contractAddress, method, "amount:", amount);
    
    // Build the operation in Temple/Beacon format
    // Temple expects: to, amount (number), mutez: true, parameter (singular)
    const operation = {
      kind: "transaction",
      to: contractAddress,
      amount: Math.floor(amount * 1_000_000), // Convert XTZ to mutez (as number)
      mutez: true,
      parameter: {
        entrypoint: method,
        value: args
      }
    };
    
    console.log("🔷 Operation:", JSON.stringify(operation, null, 2).slice(0, 500));
    
    // Use wallet's sendOperations method (works for both Temple and Beacon)
    const opHash = await tezosWallet.sendOperations([operation]);
    
    console.log("🔷 Transaction submitted:", opHash);
    return opHash;
  }

  // UDP / Geckos Networking
  let UDP;
  async function loadUDP() {
    // 🎒 PACK mode: Return no-op UDP to disable networking in offline bundles
    if (window.acPACK_MODE) {
      return {
        connect: () => {},
        send: () => {},
        disconnect: () => {},
      };
    }
    if (!UDP) {
      // if (debug) console.log("🌐 Loading UDP networking library...");
      const udpModule = await import('./lib/udp.mjs');
      UDP = udpModule.UDP;
      // if (debug) console.log("🌐 UDP Ready...");
    }
    return UDP;
  }

  // 2. 🔈 Audio
  const sound = {
    bpm: new Float32Array(1),
  };

  const sfx = {}; // Buffers of sound effects that have been loaded.
  const sfxPlaying = {}; // Sound sources that are currently playing.
  const sfxLoaded = {}; // Sound sources that have been buffered and loaded.
  const sfxCompletionCallbacks = {}; // Completion callbacks for sound effects.
  // NOTE: streamAudio is now at module scope for master volume control

  // const sfX // ...

  const sfxCancel = [];
  speakAPI.sfx = sfx;
  // TODO: Some of these need to be kept (like system ones) and others need to
  // be destroyed after pieces change.

  let updateMetronome,
    beatSkip,
    activateSound,
    activatedSoundCallback,
    triggerSound,
    updateBubble,
    updateSound,
    killSound,
    killAllSound,
    clearSoundSampleCache,
    requestSpeakerWaveforms,
    requestSpeakerAmplitudes,
    requestSpeakerFrequencies,
    sendRoomMessage,
    attachMicrophone,
    detachMicrophone,
    audioContext,
    audioStreamDest,
    sfxStreamGain,
    micStreamGain,
    micGainNode,
    speakerGain;

  // NOTE: speakerProcessorNode, masterVolume, backgroundMusicBaseVolume, streamAudio,
  // clampVolume, and applyMasterVolume are now at module scope for window.AC.setMasterVolume

  let requestMicrophoneAmplitude,
    requestMicrophoneWaveform,
    requestMicrophonePitch,
    requestMicrophoneRecordingStart,
    requestMicrophoneRecordingStop,
    requestMicrophoneRecordingBuffer;

  // 🔬 Sound Telemetry for latency testing (exposed via window.__bios_sound_telemetry)
  const soundTelemetry = {
    triggerCount: 0,           // Total sounds triggered
    lastTriggerTime: 0,        // performance.now() of last trigger
    recentTriggers: [],        // Last 50 trigger timestamps with sound IDs
    maxRecentTriggers: 50,
  };
  if (typeof window !== 'undefined') {
    window.__bios_sound_telemetry = soundTelemetry;
    console.log('🔬 BIOS: Exposed __bios_sound_telemetry on window');
  }

  // TODO: Eventually this would be replaced with a more dynamic system.

  const backgroundTrackURLs = [
    "0 - analog multiplication",
    "1 - castlecowards",
    "2 - epanodos clinamen",
    "3 - for not being able",
    "4 - pantoum chain rhyme",
    "5 - they sit so nicely",
    "6 - vociferatings witchbefooled",
    "7 - an accuracy which it seems as impossible to attain",
    "8 - bivariate beamforming",
    "9 - and the three of them began to make",
    "10 - or perhaps destroyed",
    "11 - sunsmidnought",
    "12 - improvements design",
    "13 - consideration",
    "14 - magellanic clouds",
    "15 - syncopation demotic",
    "16 - textual criticism ambiguity",
  ];

  let backgroundMusicEl;
  backgroundMusicEl = document.createElement("audio");
  backgroundMusicEl.id = "background-music";
  backgroundMusicEl.crossOrigin = "anonymous";
  wrapper.appendChild(backgroundMusicEl);

  let analyserCtx, analyserSrc, analyser, frequencyData;
  let currentBackgroundTrack;

  function playBackgroundMusic(n, volume) {
    if (currentBackgroundTrack !== n && !isNaN(n)) {
      let origin;

      if (window.production === true) {
        origin = "https://assets.aesthetic.computer/bgm/";
      } else {
        origin = "/assets/bgm/";
      }

      const ext = Safari ? "m4a" : "ogg";
      backgroundMusicEl.src = origin + backgroundTrackURLs[n] + "." + ext;
      backgroundMusicBaseVolume = clampVolume(volume);
      backgroundMusicEl.volume = backgroundMusicBaseVolume * masterVolume;
      if (audioContext) {
        backgroundMusicEl.play();
      }
      currentBackgroundTrack = n;
    }
  }

  function stopBackgroundMusic() {
    currentBackgroundTrack = null;
    backgroundMusicEl.src = "";
  }

  function startSound() {
    if (audioStarting) return;
    if (audioContext && audioContext.state !== "closed" && !window.__acForceStartSound) {
      return;
    }

    audioStarting = true;

    if (navigator.audioSession) navigator.audioSession.type = "ambient";

    // Notify parent of boot progress
    if (window.acBOOT_LOG) {
      window.acBOOT_LOG("starting audio system");
    } else if (window.parent) {
      window.parent.postMessage({ 
        type: "boot-log", 
        message: "starting audio system" 
      }, "*");
    }

    // 🎵 AUDIO INITIALIZATION LOGGING - Critical for tracking audio timing setup
    const audioStartTimestamp = performance.now();
    // console.log(`🎵 AUDIO_INIT_START: ${audioStartTimestamp.toFixed(3)}ms`);

    // Main audio feed
    // 🎹 In DAW mode, use Ableton's sample rate; otherwise try high sample rate on Chrome desktop
    let targetSampleRate;
    const requestedSampleRate =
      typeof window !== "undefined" && Number.isFinite(window.__acSampleRate)
        ? window.__acSampleRate
        : null;
    if (_dawSampleRate) {
      // DAW mode: match Ableton's sample rate for proper sync
      targetSampleRate = _dawSampleRate;
      console.log("🎹 DAW mode: Using Ableton sample rate:", targetSampleRate);
    } else if (requestedSampleRate) {
      targetSampleRate = requestedSampleRate;
    } else {
      // Force 48kHz for consistent audio across all platforms
      // (Dynamic reinit to change sample rates was causing audio breakage)
      targetSampleRate = 48000;
    }
    
    let latencyHint =
      (typeof window !== "undefined" && window.__acLatencyHint) ||
      "interactive"; // "interactive" = lowest latency, "balanced" = default, "playback" = prioritize battery

    if (latencyHint === "low" || latencyHint === "min") {
      latencyHint = 0.005;
    }

    audioContext = new AudioContext({
      latencyHint,
      sampleRate: targetSampleRate,
    });

    // 🎵 AUDIO CONTEXT LOGGING - Track timing characteristics
    // console.log(`🎵 AUDIO_CONTEXT_CREATED: sampleRate=${audioContext.sampleRate}, state=${audioContext.state}, baseLatency=${audioContext.baseLatency?.toFixed(6) || 'N/A'}s, outputLatency=${audioContext.outputLatency?.toFixed(6) || 'N/A'}s`);
    // console.log(`🎵 AUDIO_TIMING_INIT: currentTime=${audioContext.currentTime.toFixed(6)}s, creation_delay=${(performance.now() - audioStartTimestamp).toFixed(3)}ms`);

    acDISK_SEND({
      type: "audio:sample-rate",
      content: audioContext.sampleRate,
    });

    // Notify pieces of initial AudioContext state
    if (!window.acPACK_MODE) {
      console.log("🎵 BIOS sending initial AudioContext state:", { state: audioContext.state, hasAudio: true });
    }
    acDISK_SEND({ 
      type: "tape:audio-context-state", 
      content: { 
        state: audioContext.state,
        hasAudio: true
      } 
    });



    // Process any queued sounds now that audioContext is available
    processPendingSfx();

    // Main audio feed
    // audioContext = new AudioContext({
    //   latencyHint: "interactive",
    //   sampleRate: Math.min(
    //     192000,
    //     new (window.AudioContext || window.webkitAudioContext)().sampleRate,
    //   ),
    // });

    // BGM Analyser
    if (analyserSrc) {
      try {
        analyserSrc.disconnect();
      } catch (e) {
        console.warn("🎵 Audio init: failed to disconnect analyser source", e);
      }
    }

    analyserCtx = new AudioContext();
    analyserSrc = analyserCtx.createMediaElementSource(backgroundMusicEl);
    analyser = analyserCtx.createAnalyser();
    analyser.fftSize = 256; // See also: https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/frequencyBinCount

    analyserSrc.connect(analyser);
    analyser.connect(analyserCtx.destination);
    frequencyData = new Uint8Array(analyser.frequencyBinCount);

    // console.log("Sound started.");

    speakAPI.audioContext = audioContext; // Make audioContext global, for
    //                                       `speech` and perhaps other
    //                                       modules. 23.08.17.12.31

    audioStreamDest = audioContext.createMediaStreamDestination();
    sfxStreamGain = audioContext.createGain();
    sfxStreamGain.gain.value = 1;
    sfxStreamGain.connect(audioStreamDest);

    speakerGain = audioContext.createGain();
    speakerGain.gain.value = 1;
    speakerGain.connect(audioContext.destination);

    if (audioContext.state === "running") {
      // audioContext.suspend();
    }

    // TODO: Check to see if there is support for AudioWorklet or not...
    //       and and use ScriptProcessorNode as a fallback. 2022.01.13.21.00

    // Microphone Input Processor
    // (Gets attached via a message from the running disk.)
    attachMicrophone = async (data) => {
      if (navigator.audioSession)
        navigator.audioSession.type = "play-and-record"; // play-and-record";

      let micStream;
      let reason;
      try {
        // Allow pieces to configure audio constraints via data parameter
        const audioConstraints = {
          echoCancellation: data?.echoCancellation ?? false,
          noiseSuppression: data?.noiseSuppression ?? false,
          autoGainControl: data?.autoGainControl ?? false,
          latency: data?.latency ?? 0,
        };
        
        // Request higher sample rate if on Chrome desktop for lower latency
        const isChrome = /Chrome/.test(navigator.userAgent) && !/Edge/.test(navigator.userAgent);
        const isDesktop = !/Mobile|Android|iPhone|iPad/.test(navigator.userAgent);
        if (isChrome && isDesktop) {
          // Try multiple sample rates in order of preference
          audioConstraints.sampleRate = { 
            ideal: 192000,
            min: 48000 // Fall back to 48kHz if higher not supported
          };
          audioConstraints.sampleSize = { ideal: 24 }; // Request 24-bit if available
        }
        
        micStream = await navigator.mediaDevices.getUserMedia({
          audio: audioConstraints,
        });
      } catch (err) {
        if (debug) console.warn("🎙 Microphone disabled:", err);
        reason = err.message;
      }

      // return;

      if (!micStream) {
        send({ type: "microphone-connect:failure", content: { reason } });
        return;
      }

      const micNode = new MediaStreamAudioSourceNode(audioContext, {
        mediaStream: micStream,
      });
      
      // Send latency info to disk for display (when monitoring)
      if (data?.monitor) {
        const tracks = micStream.getAudioTracks();
        const micLatency = tracks[0]?.getSettings()?.latency || 0;
        const baseLatency = audioContext.baseLatency || 0;
        const outputLatency = audioContext.outputLatency || 0;
        const processingLatency = 128 / audioContext.sampleRate; // One quantum
        
        const inputMs = Math.round(micLatency * 1000);
        const processingMs = Math.round(processingLatency * 1000 * 10) / 10; // One decimal
        const outputMs = Math.round(baseLatency * 1000);
        const totalMs = inputMs + processingMs + outputMs;
        
        send({ 
          type: "audio:latency-info", 
          content: {
            input: inputMs,
            processing: processingMs,
            output: outputMs,
            total: Math.round(totalMs)
          }
        });
      }

      // TODO: Why can't there be separate audioWorklet modules?
      await audioContext.audioWorklet.addModule(
        "/aesthetic.computer/lib/microphone.mjs",
      );

      const micProcessorNode = new AudioWorkletNode(
        audioContext,
        "microphone-processor",
        {
          outputChannelCount: [2],
          processorOptions: { 
            debug,
            enablePitch: data?.enablePitch ?? true // Allow disabling pitch detection for lower CPU usage
          },
        },
      );

      // Receive messages from the microphone processor thread.
      micProcessorNode.port.onmessage = (e) => {
        const msg = e.data;

        if (msg.type === "amplitude") {
          send({ type: "microphone-amplitude", content: msg.content });
        }

        if (msg.type === "waveform") {
          // console.log("Mic waveform:", msg.content);
          send({ type: "microphone-waveform", content: msg.content });
        }

        if (msg.type === "pitch") {
          send({ type: "microphone-pitch", content: msg.content });
        }

        if (msg.type === "recording-buffer") {
          send({ type: "microphone-recording-buffer", content: msg.content });
        }

        if (msg.type === "recording:complete") {
          // console.log("Recording complete:", msg.content);
          // Turn this into a sample with a playback ID here and send
          // the sample ID back.
          const id = "microphone-recording" + "_" + performance.now();

          //if (debug)
          //  console.log("🔈 Buffer length:", msg.content.recording?.length);

          // Create an empty mono AudioBuffer (1 channel)
          const buffer = audioContext.createBuffer(
            1,
            msg.content.recording.length,
            audioContext.sampleRate,
          );
          const channel = buffer.getChannelData(0); // Ref to the first channel.
          channel.set(msg.content.recording);
          // Copy your Float32Array data into the buffer's channel

          // console.log("Recording:", msg.content.recording);

          sfx[id] = buffer; // Set the sfx id so the sfx system
          //                   can play back the sample.

          send({
            type: "microphone-recording:complete",
            content: { id, data: msg.content.recording },
          });
        }
      };

      requestMicrophoneRecordingStart = () => {
        micProcessorNode.port.postMessage({ type: "record:start" });
      };

      requestMicrophoneRecordingStop = () => {
        micProcessorNode.port.postMessage({ type: "record:stop" });
      };

      // Request data / send message to the mic processor thread.
      requestMicrophoneAmplitude = () => {
        micProcessorNode.port.postMessage({ type: "get-amplitude" });
      };

      requestMicrophoneWaveform = () => {
        micProcessorNode.port.postMessage({ type: "get-waveform" });
      };

      requestMicrophonePitch = () => {
        micProcessorNode.port.postMessage({ type: "get-pitch" });
      };

      requestMicrophoneRecordingBuffer = () => {
        micProcessorNode.port.postMessage({ type: "get-recording-buffer" });
      };

      micStreamGain = audioContext.createGain();
      micGainNode = audioContext.createGain();

      // Connect mic to the mediaStream.

      // TODO: How to automatically dip the mic gain node?

      // micNode.connect(micProcessorNode);
      micNode.connect(micGainNode);
      micGainNode.connect(micProcessorNode);

      micProcessorNode.connect(micStreamGain);
      micStreamGain.connect(audioStreamDest);
      // speakerGain.gain.value = 0.35;
      // micStreamGain.gain.value = 1.5;

      micGainNode.gain.value = 1;
      micStreamGain.gain.value = 1;
      sfxStreamGain.gain.value = 1;

      // Connect to the speaker if we are monitoring audio.
      if (data?.monitor === true)
        micProcessorNode.connect(audioContext.destination);

      // Setup microphone detachment function.
      detachMicrophone = () => {
        if (navigator.audioSession) navigator.audioSession.type = "ambient";
        micProcessorNode.disconnect();
        micNode.disconnect();
        micStream.getTracks().forEach((t) => t.stop());
        // speakerGain.gain.value = 1;
        sfxStreamGain.gain.value = 1;
        if (debug) console.log("🎙💀 Microphone:", "Detached");
        send({ type: "microphone-disconnect" });
      };

      // Send a message back to `disk` saying the microphone is connected.
      send({ type: "microphone-connect:success" });
    };

    // Sound Synthesis Processor
    try {
      // Check if we're in PACK mode
      const isPackMode = (typeof window !== 'undefined' && window.acPACK_MODE) ||
                        (typeof globalThis !== 'undefined' && globalThis.acPACK_MODE);
      
      (async () => {
        try {
          let workletUrl;
          
          if (isPackMode) {
            // In PACK mode, load the bundled worklet from VFS blob URL
            if (window.VFS && window.VFS['lib/speaker-bundled.mjs']) {
              const workletSource = window.VFS['lib/speaker-bundled.mjs'].content;
              const blob = new Blob([workletSource], { type: 'application/javascript' });
              workletUrl = URL.createObjectURL(blob);
              if (debug) console.log("🎭 Using bundled speaker worklet from VFS");
            } else {
              console.warn("🎭 PACK mode: speaker-bundled.mjs not found in VFS, audio synth will not work");
              return;
            }
          } else {
            // Normal mode - load from server
            const baseUrl = "/aesthetic.computer/lib/speaker.mjs";
            const cacheBuster = /*debug ?*/ `?time=${new Date().getTime()}`; // : "";
            workletUrl = baseUrl + cacheBuster;
          }
          
          // 🎵 WORKLET LOADING LOGGING
          const workletLoadStart = performance.now();
          // console.log(`🎵 WORKLET_LOAD_START: Loading ${workletUrl}`);
          
          await audioContext.audioWorklet.addModule(workletUrl);
          
          // Clean up blob URL if we created one
          if (isPackMode && workletUrl.startsWith('blob:')) {
            URL.revokeObjectURL(workletUrl);
          }
        
        const workletLoadTime = performance.now() - workletLoadStart;
        // console.log(`🎵 WORKLET_LOADED: Took ${workletLoadTime.toFixed(3)}ms`);

        const speakerProcessor = new AudioWorkletNode(
          audioContext,
          "speaker-processor",
          {
            outputChannelCount: [2],
            processorOptions: { bpm: sound.bpm, debug: true },
          },
        );

        speakerProcessorNode = speakerProcessor;

        // 🎵 WORKLET INITIALIZATION LOGGING
        // console.log(`🎵 WORKLET_CREATED: bpm=${sound.bpm}, audioTime=${audioContext.currentTime.toFixed(6)}s, totalSetupTime=${(performance.now() - audioStartTimestamp).toFixed(3)}ms`);

        beatSkip = function () {
          speakerProcessor.port.postMessage({ type: "beat:skip" });
        };

        updateMetronome = function (newBPM) {
          speakerProcessor.port.postMessage({ type: "new-bpm", data: newBPM });
        };

        triggerSound = function (sound) {
          // 🔬 Telemetry: Record this sound trigger for latency testing
          const triggerTime = performance.now();
          soundTelemetry.triggerCount++;
          soundTelemetry.lastTriggerTime = triggerTime;
          soundTelemetry.recentTriggers.push({
            id: sound.id,
            type: sound.type,
            timestamp: triggerTime,
          });
          if (soundTelemetry.recentTriggers.length > soundTelemetry.maxRecentTriggers) {
            soundTelemetry.recentTriggers.shift();
          }

          speakerProcessor.port.postMessage({ type: "sound", data: sound });

          return {
            progress: () => {
              // console.log("🟠 Get progress of sound...", sound);
              speakerProcessor.port.postMessage({
                type: "get-progress",
                content: sound.id,
              });
            },
            kill: (fade) => {
              killSound(sound.id, fade);
            },
            update: (properties) => {
              // console.log(
              //   "Update needed for sound:",
              //   sound,
              //   "With:",
              //   properties,
              // );
              updateSound?.({ id: sound.id, properties });
            },
          };
        };

        // Set flag to indicate worklet is ready
        window.audioWorkletReady = true;

        // Apply speaker performance mode hint if provided
        const speakerPerfMode =
          typeof window !== "undefined" ? window.__acSpeakerPerformanceMode : null;
        if (speakerPerfMode) {
          speakerProcessor.port.postMessage({
            type: "performance:mode",
            content: { mode: speakerPerfMode },
          });
        }

        // 🔬 Speaker telemetry (queue/running sizes) for stability tests
        if (typeof window !== 'undefined') {
          window.__speaker_telemetry = window.__speaker_telemetry || {
            queueLength: 0,
            runningCount: 0,
            performanceMode: 'auto',
            lastUpdate: 0,
          };
        }

        updateBubble = function (bubble) {
          speakerProcessor.port.postMessage({ type: "bubble", data: bubble });
        };

        killSound = function (id, fade) {
          // console.log("📢 Kill:", id, "Fade:", fade);
          delete sfxPlaying[id];
          delete sfxCompletionCallbacks[id]; // Clean up completion callback
          speakerProcessor.port.postMessage({
            type: "kill",
            data: { id, fade },
          });
        };

        updateSound = function (data) {
          speakerProcessor.port.postMessage({ type: "update", data });
        };

        killAllSound = function () {
          speakerProcessor.port.postMessage({ type: "kill:all" });
        };

        clearSoundSampleCache = function () {
          speakerProcessor.port.postMessage({ type: "cache:clear" });
          for (const k in sfxLoaded) delete sfxLoaded[k];
        };

        // 🏠 Room/Reverb control
        sendRoomMessage = function (type, data) {
          speakerProcessor.port.postMessage({ type, data });
        };

        // Request data / send message to the mic processor thread.
        requestSpeakerWaveforms = function () {
          speakerProcessor.port.postMessage({ type: "get-waveforms" });
        };

        requestSpeakerAmplitudes = function () {
          speakerProcessor.port.postMessage({ type: "get-amplitudes" });
        };

        requestSpeakerFrequencies = function () {
          speakerProcessor.port.postMessage({ type: "get-frequencies" });
        };

        speakerProcessor.port.onmessage = ({ data: msg }) => {
          if (msg.type === "waveforms") {
            send({ type: "waveforms", content: msg.content });
            return;
          }

          if (msg.type === "amplitudes") {
            send({ type: "amplitudes", content: msg.content });
            return;
          }

          if (msg.type === "frequencies") {
            send({ type: "frequencies", content: msg.content });
            return;
          }

          if (msg.type === "metronome") {
            diskSupervisor.requestBeat?.(msg.content); // Update metronome.
            return;
          }

          if (msg.type === "progress") {
            // Send sound progress to the disk.
            // console.log("Received progress for:", msg);
            send({
              type: "sfx:progress:report",
              content: msg.content,
            });
            return;
          }

          if (msg.type === "killed") {
            // Call the completion callback if it exists
            const completionCallback = sfxCompletionCallbacks[msg.content.id];
            if (completionCallback) {
              completionCallback();
              delete sfxCompletionCallbacks[msg.content.id];
            }

            send({ type: "sfx:killed", content: msg.content });
            return;
          }

          if (msg.type === "telemetry") {
            if (typeof window !== 'undefined') {
              window.__speaker_telemetry = window.__speaker_telemetry || {};
              window.__speaker_telemetry.queueLength = msg.content.queueLength || 0;
              window.__speaker_telemetry.runningCount = msg.content.runningCount || 0;
              window.__speaker_telemetry.performanceMode = msg.content.performanceMode || 'auto';
              window.__speaker_telemetry.lastUpdate = Date.now();
            }
            return;
          }
          
          // 🎛️ VST Bridge Mode - forward audio samples to native plugin
          if (msg.type === "vst:samples") {
            // Call the VST sample callback if registered
            if (window.__vstSampleCallback) {
              window.__vstSampleCallback(msg.content.left, msg.content.right);
            }
            return;
          }
          
          if (msg.type === "vst:enabled") {
            console.log("🎛️ VST Bridge Mode enabled in speaker worklet");
            return;
          }
          
          if (msg.type === "vst:disabled") {
            console.log("🎛️ VST Bridge Mode disabled in speaker worklet");
            return;
          }
        };
        
        // 🎛️ Expose VST Bridge API on window.AC for native plugin integration
        window.AC = window.AC || {};
        window.AC.enableVSTBridge = function(sampleCallback) {
          console.log("🎛️ Enabling VST Audio Bridge...");
          window.__vstSampleCallback = sampleCallback;
          speakerProcessor.port.postMessage({ type: "vst:enable" });
        };
        window.AC.disableVSTBridge = function() {
          console.log("🎛️ Disabling VST Audio Bridge...");
          window.__vstSampleCallback = null;
          speakerProcessor.port.postMessage({ type: "vst:disable" });
        };
        window.AC.isVSTMode = function() {
          // Check if we're running inside a VST plugin
          return new URLSearchParams(window.location.search).get('vst') === 'true';
        };

        speakerProcessor.connect(sfxStreamGain); // Connect to the mediaStream.
        speakerProcessor.connect(speakerGain);
        console.log("🔊 Speaker processor connected to audio graph!");

        applyMasterVolume(masterVolume);

        activatedSoundCallback?.();

        // Process any queued sound effects now that audioContext is available
        processPendingSfx();

        modal.classList.remove("on");
        audioStarting = false;
        } catch (workletError) {
          console.error("🎵 BIOS: Audio worklet setup failed:", workletError);
        audioStarting = false;
        }
      })();
    } catch (e) {
      console.error("🎵 BIOS: Sound failed to initialize:", e);
    audioStarting = false;
    }

    function enableAudioPlayback(skip = false) {
      try {
        if (backgroundMusicEl.paused && currentBackgroundTrack !== null) {
          backgroundMusicEl.play().catch(error => {
            // Audio playback failed, probably due to browser policy
            console.log("Background music play failed (user gesture required):", error.message);
          });
        }
        if (!skip && ["suspended", "interrupted"].includes(audioContext.state)) {
          audioContext.resume().catch(error => {
            // AudioContext resume failed, probably due to browser policy  
            console.log("AudioContext resume failed (user gesture required):", error.message);
          });
        }
      } catch (error) {
        console.log("Audio playback initialization failed:", error.message);
      }
    }

    //enableAudioPlayback(true);
    window.addEventListener("pointerdown", () => enableAudioPlayback());
    window.addEventListener("keydown", () => enableAudioPlayback());
  }

  // 🔍 Density Keyboard Controls (Cmd/Ctrl + / - / 0)
  // Uses standard zoom keys - browser may also zoom, but density will change too
  const DENSITY_MIN = 0.5;
  const DENSITY_MAX = 32;
  const DENSITY_STEP = 0.5;
  const DENSITY_DEFAULT = 2;

  function changeDensity(direction) {
    const current = density;
    let newDensity;
    
    if (direction === 0) {
      // Reset to default
      newDensity = DENSITY_DEFAULT;
    } else {
      newDensity = current + (direction * DENSITY_STEP);
      newDensity = Math.max(DENSITY_MIN, Math.min(DENSITY_MAX, newDensity));
      newDensity = Math.round(newDensity * 2) / 2; // Round to nearest 0.5
    }
    
    if (newDensity === current) return;
    
    // Update module-level density variable
    density = newDensity;
    window.acPACK_DENSITY = newDensity;
    
    // Persist to localStorage
    try {
      localStorage.setItem("ac-density", newDensity.toString());
    } catch {}
    
    // Trigger reframe to apply new density
    frame();
    
    console.log(`🔍 Density: ${newDensity}`);
  }

  // Keyboard listener for density controls (capture phase to intercept early)
  window.addEventListener("keydown", (e) => {
    const isMeta = e.metaKey || e.ctrlKey;
    if (!isMeta) return;
    
    // Cmd/Ctrl + Plus/Equal (increase density)
    if (e.key === "=" || e.key === "+") {
      e.preventDefault();
      e.stopPropagation();
      changeDensity(1); // Increase
    } 
    // Cmd/Ctrl + Minus (decrease density)
    else if (e.key === "-") {
      e.preventDefault();
      e.stopPropagation();
      changeDensity(-1); // Decrease
    } 
    // Cmd/Ctrl + 0 (reset density)
    else if (e.key === "0") {
      e.preventDefault();
      e.stopPropagation();
      changeDensity(0); // Reset to default
    }
  }, { capture: true });

  // 🎯 Auto-density message handler - listen for density change requests from KidLisp
  window.addEventListener("message", (e) => {
    if (e.data && e.data.type === 'ac-density-change' && typeof e.data.density === 'number') {
      const newDensity = e.data.density;
      if (newDensity !== density) {
        density = newDensity;
        window.acPACK_DENSITY = newDensity;
        try {
          localStorage.setItem("ac-density", newDensity.toString());
        } catch {}
        frame();
        // console.log(`🎯 Auto-density applied: ${newDensity}`);
      }
    }
    // FPS reporting for parent window (device.kidlisp.com)
    if (e.data && e.data.type === 'ac:request-fps') {
      window.acReportFpsToParent = true;
    }
  });

  // Play a sound back through the sfx system.
  // 🌡️ TODO: `sfx` could be scraped for things that need to be decoded
  //          upon audio activation. This would probably be helpful
  //          in terms of creating a sampler and asynchronously
  //          decoding all the sounds after an initial tap.

  async function playSfx(id, soundData, options, completed) {
    
    if (audioContext) {
      
      if (sfxCancel.includes(id)) {
        // console.log("🎵 BIOS playSfx cancelled for:", id);
        sfxCancel.length = 0;
        return;
      }

      // Handle stream option - audio should be silent for streaming
      if (options?.stream) {
        console.log("🎵 BIOS stream option detected, audio will be silent:", soundData);
        // Create a dummy playback object for tracking
        sfxPlaying[id] = {
          kill: () => {
            // No-op for silent stream audio
          }
        };
        return;
      }

      // Instantly decode the audio before playback if it hasn't been already.
      if (debug && logs.sound) console.log("🎵 BIOS attempting to decode sfx:", soundData);
      await decodeSfx(soundData);
      if (debug && logs.sound) console.log("🎵 BIOS decode complete, sfx type now:", typeof sfx[soundData]);

      if (sfx[soundData] instanceof ArrayBuffer) {
        console.log("🎵 BIOS sfx still ArrayBuffer, returning early");
        return;
      }

      if (!sfx[soundData]) {
        // console.log("🎵 BIOS sfx not found after decode, queuing:", soundData);
        // `console.log("🎵 BIOS sfx not found, queuing:", soundData);
        // Queue the sound effect to be played once it's loaded
        pendingSfxQueue.push({
          id,
          soundData,
          options,
          completed,
          queuedAt: Date.now(),
        });
        return;
      }

      const channels = [];
      for (let i = 0; i < sfx[soundData].numberOfChannels; i += 1) {
        channels.push(sfx[soundData].getChannelData(i)); // Get raw Float32Array.
      }

      const sample = {
        channels,
        sampleRate: sfx[soundData].sampleRate,
        length: sfx[soundData].length,
      };

      if (debug && logs.sound) {
        console.log("🎵 BIOS sample prepared:", {
          soundData,
          sampleChannels: sample.channels.length,
          sampleRate: sample.sampleRate,
          length: sample.length,
          triggerSoundAvailable: !!triggerSound
        });
      }

      // TODO: ⏰ Memoize the buffer data after first playback so it doesn't have to
      //          keep being sent on every playthrough. 25.02.15.08.22

      // console.log("👮 Sample ID:", id, "Sound data:", soundData);

      // Calculate speed from pitch if pitch is provided (frequency in Hz)
      // Assumes sample's base pitch is A4 (440 Hz) - speed = pitch / 440
      let speed = 1;
      if (isFinite(options?.speed)) {
        speed = options.speed;
      } else if (isFinite(options?.pitch)) {
        const basePitch = options?.basePitch || 440; // Default to A4
        speed = options.pitch / basePitch;
      }

      const playResult = triggerSound?.({
        id,
        type: "sample",
        options: {
          buffer: sfxLoaded[soundData] ? soundData : sample, // Alternatively send a memoized code using a lookup table.
          label: soundData, // TODO: 🚩 This needs to be invalidated by `tape`.
          // TODO: 🚩 Cached speaker sounds need to be dumped on a piece swap.
          from: isFinite(options?.from) ? options.from : 0,
          to: isFinite(options?.to) ? options.to : 1,
          speed,
          loop: options?.loop || false,
        },
        volume: isFinite(options?.volume) ? options.volume : 1,
        pan: isFinite(options?.pan) ? options.pan : 0,
        // options: { buffer: sample },
        // ⏰ TODO: If duration / 'beats' is not specified then use speed.
        // beats: undefined, // ((sample.length / sample.sampleRate) * sound.bpm / 60),
        // attack: 0, // 🩷 TODO: These should have saner defaults.
        // decay: 0,
      });

      // console.log("🎵 BIOS triggerSound result:", {
      //   id,
      //   playResult,
      //   playResultType: typeof playResult
      // });

      sfxPlaying[id] = playResult;

      // Store the completion callback if provided
      if (completed) {
        sfxCompletionCallbacks[id] = completed;
      }

      if (triggerSound) sfxLoaded[soundData] = true;
    } else {
      // Queue the sound effect to be played once audioContext is available
      pendingSfxQueue.push({
        id,
        soundData,
        options,
        completed,
        queuedAt: Date.now(),
      });
    }
  }

  speakAPI.playSfx = playSfx;

  // TODO: Add mute
  // function mute() {
  //   audioContext.suspend();
  //   // Or... audioContext.resume();
  // }

  // Try to load the disk boilerplate as a worker first.
  // Safari and FF support is coming for worker module imports: https://bugs.webkit.org/show_bug.cgi?id=164860
  //const worker = new Worker("./aesthetic.computer/lib/disk.js", {
  //  type: "module",
  //});
  const fullPath =
    (window.acPACK_MODE ? "./aesthetic.computer/lib/disk.mjs" : "/aesthetic.computer/lib/disk.mjs") +
    window.location.search +
    "#" +
    Date.now(); // bust the cache. This prevents an error related to Safari loading workers from memory.

  const sandboxed =
    (window.origin === "null" || !window.origin || window.acPACK_MODE || window.acSPIDER) && !window.acVSCODE;

  // 🕷️ SPIDER MODE: Debug sandboxed state
  if (window.acSPIDER) {
    console.log("🕷️ SPIDER: Sandboxed check:", {
      origin: window.origin,
      acPACK_MODE: window.acPACK_MODE,
      acVSCODE: window.acVSCODE,
      sandboxed
    });
  }

  const microphonePermission = await checkMicrophonePermission();

  // Extract embedded source if available
  let embeddedSource = null;
  try {
    const embeddedScript = document.getElementById("embedded-source");
    if (embeddedScript) {
      embeddedSource = JSON.parse(embeddedScript.textContent);
    }
  } catch (err) {
    console.warn("⚠️ Failed to parse embedded source:", err);
  }

  const firstMessage = {
    type: "init-from-bios",
    content: {
      parsed,
      debug,
      debugHud: false,
      rootPiece: window.acSTARTING_PIECE,
      user: window.acUSER,
      lanHost: window.acLAN_HOST,
      iframe: window.self !== window.top,
      sandboxed,
      shareSupported: (iOS || Android) && navigator.share !== undefined,
      previewOrIcon: window.acPREVIEW_OR_ICON,
      vscode: window.acVSCODE,
      objktMode: window.acPACK_MODE || false,
      objktKidlispCodes: window.objktKidlispCodes || globalThis.objktKidlispCodes || {},
      microphonePermission,
      resolution,
      embeddedSource,
      noauth: window.acNOAUTH || false,
    },
  };

  // 🔍 Debug logging flags (opt-in; avoids spamming console during normal runs)
  const DEBUG_MESSAGE_FLOW = !!window.acDEBUG_MESSAGE_FLOW;

  const onMessage = (m) => {
    // Log all non-frame messages at the earliest point
    if (DEBUG_MESSAGE_FLOW && m?.data?.type && m.data.type !== "frame") {
      console.log(`🔍 BIOS onMessage: type="${m.data.type}"`);
    }
    receivedChange(m);
  };

  let send = (msg) => {
    console.warn("Send has not been wired yet!", msg);
  };

  const TAPE_PREVIEW_MAX_FRAMES = 90;
  const TAPE_PREVIEW_WIDTH = 256;
  const TAPE_PREVIEW_HEIGHT = 192;
  let tapePreviewCanvas = null;
  let tapePreviewCtx = null;

  function getTapePreviewContext(width = TAPE_PREVIEW_WIDTH, height = TAPE_PREVIEW_HEIGHT) {
    if (typeof OffscreenCanvas !== "undefined") {
      if (!(tapePreviewCanvas instanceof OffscreenCanvas)) {
        tapePreviewCanvas = new OffscreenCanvas(width, height);
      }
      if (tapePreviewCanvas.width !== width) tapePreviewCanvas.width = width;
      if (tapePreviewCanvas.height !== height) tapePreviewCanvas.height = height;
      tapePreviewCtx = tapePreviewCanvas.getContext("2d");
      return tapePreviewCtx;
    }

    if (!tapePreviewCanvas && typeof document !== "undefined") {
      tapePreviewCanvas = document.createElement("canvas");
    }
    if (tapePreviewCanvas) {
      if (tapePreviewCanvas.width !== width) tapePreviewCanvas.width = width;
      if (tapePreviewCanvas.height !== height) tapePreviewCanvas.height = height;
      tapePreviewCtx = tapePreviewCanvas.getContext("2d");
    }
    return tapePreviewCtx;
  }

  async function buildTapePreviewFrames(sourceFrames, limit = TAPE_PREVIEW_MAX_FRAMES) {
    const previews = [];
    if (!Array.isArray(sourceFrames) || sourceFrames.length === 0) return previews;

    const total = sourceFrames.length;
    const step = Math.max(1, Math.floor(total / limit));

    for (let i = 0; i < total && previews.length < limit; i += step) {
      const frame = sourceFrames[i];
      if (!frame) continue;
      
      // Keep original frames at full resolution for Ken Burns panning
      // No downscaling - just select frames and pass them through
      previews.push(frame);
    }

    return previews;
  }

  //  👷️ Always use workers if they are supported, except for
  //     when we are in VR (MetaBrowser).
  // Disable workers if we are in a sandboxed iframe.
  // Test if workers are actually available instead of just checking sandboxed state
  let workersEnabled = true;
  try {
    // Try to create a simple test worker to see if they're supported
    if (typeof Worker === 'undefined') {
      workersEnabled = false;
    }
  } catch (e) {
    workersEnabled = false;
  }
  
  // Override: force disable workers for OBJKT, SPIDER, and other sandboxed environments
  if (sandboxed || window.origin === "null" || window.acSPIDER) {
    // Disable workers in any sandboxed environment, including OBJKT and SPIDER
    workersEnabled = false;
    if (debug) console.log("🚫 Workers disabled due to sandboxed/null origin environment");
  }
  
  // Workers enabled logging removed for cleaner console

  // Notify parent that we're loading the disk
  const diskLoadStartTime = performance.now();
  const bootElapsed = Math.round(diskLoadStartTime - bootStartTime);
  perf.markBoot("disk-load-start");
  if (window.acBOOT_LOG) {
    window.acBOOT_LOG(`loading disk: ${parsed.text || 'prompt'} (${bootElapsed}ms)`);
  } else if (window.parent) {
    window.parent.postMessage({ 
      type: "boot-log", 
      message: `loading disk: ${parsed.text || 'prompt'} (${bootElapsed}ms)` 
    }, "*");
  }

  if (/*!MetaBrowser &&*/ workersEnabled) {
    perf.markBoot("worker-create-start");
    
    let workerFailed = false; // Guard to prevent double-initialization
    let workerReady = false;  // Track if worker has responded
    let workerInitialized = false; // Guard to prevent sending firstMessage multiple times
    let retryCount = 0;
    const isLocalhost = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1';
    // On localhost, fail fast (1 retry) since HTTP proxy is flaky - noWorker fallback works fine
    // In production, retry more since network issues are usually transient
    const maxRetries = isLocalhost ? 1 : 3;
    
    // Note: Can't use blob URLs for workers because dynamic imports inside the worker
    // (like loading pieces) would resolve relative to blob: origin which fails.
    // Workers need to load via HTTP so their dynamic imports work correctly.
    
    const createWorker = () => {
      const worker = new Worker(new URL(fullPath, window.location.href), {
        type: "module",
      });
      
      // Rewire things a bit if workers with modules are not supported (Firefox).
      worker.onerror = async (err) => {
        if (workerFailed) return; // Already handling fallback
        if (workerInitialized) {
          // Worker already started successfully - this is a late error, don't double-init
          console.warn("🟡 Worker error after init:", err.message || "(no message)");
          return;
        }
        
        // Detect transient/network errors that are worth retrying
        const isTransientError = !err.message;
        const isNetworkError = err.message?.includes('ERR_') || 
                               err.message?.includes('Failed to fetch') ||
                               err.message?.includes('NetworkError') ||
                               err.message?.includes('Content-Length');
        
        // Try retrying the worker a couple times for transient/network errors
        if (retryCount < maxRetries && (isTransientError || isNetworkError)) {
          retryCount++;
          const delay = isLocalhost ? 100 : 200 * retryCount; // Fast retry on localhost
          // Only log retry attempts in non-localhost (localhost proxy issues are expected)
          if (!isLocalhost) {
            console.warn(`📦 Worker load failed (attempt ${retryCount}/${maxRetries}), retrying in ${delay}ms...`);
          }
          worker.terminate();
          await new Promise(r => setTimeout(r, delay));
          createWorker();
          return;
        }
        
        // Log error only for real errors with messages (not transient proxy issues on localhost)
        if (!isTransientError) {
          console.error("🛑 Disk worker error:", err);
          console.error("🚨 Message:", err.message);
          console.error("🚨 Filename:", err.filename || "(no filename)");
          console.error("🚨 Line:", err.lineno, "Col:", err.colno);
        }
        
        workerFailed = true;
        worker.terminate(); // Clean up failed worker
        
        // https://bugzilla.mozilla.org/show_bug.cgi?id=1247687
        // Try to use WebSocket module loader for the fallback import (avoids HTTP proxy issues)
        let module;
        const loader = window.acModuleLoader;
        if (isLocalhost && loader?.connected && loader.blobUrls?.has('lib/disk.mjs')) {
          // Use already-loaded blob URL from WebSocket bundle
          const blobUrl = loader.blobUrls.get('lib/disk.mjs');
          module = await import(blobUrl);
        } else {
          // Fall back to HTTP import
          if (!isLocalhost) console.warn("🟡 Worker failed, using noWorker mode");
          module = await import(`./lib/disk.mjs`);
        }
        module.noWorker.postMessage = (e) => onMessage(e); // Define the disk's postMessage replacement.
        send = (e) => module.noWorker.onMessage(e); // Hook up our post method to disk's onmessage replacement.
        window.acSEND = send; // Make the message handler global, used in `speech.mjs` and also useful for debugging.
        send(firstMessage);
        consumeDiskSends(send);
      };

      if (worker.postMessage) {
        // console.log("🟢 Worker");
        send = (e, shared) => {
          if (workerFailed) return; // Don't send if worker has failed
          worker.postMessage(e, shared);
        };
        window.acSEND = send; // Make the message handler global, used in `speech.mjs` and also useful for debugging.
        
        // 🔍 Debug utility: Toggle HUD hitbox visualization from console
        window.toggleHudDebug = () => {
          console.log("🔍 toggleHudDebug(): sending debug:hud-hitbox:toggle");
          send({ type: "debug:hud-hitbox:toggle" });
        };
        
        worker.onmessage = (e) => {
          if (workerFailed) return; // Ignore messages if we've switched to fallback
          
          // Handle worker-ready signal from disk.mjs
          if (e.data?.type === "worker-ready") {
            if (workerInitialized) return; // Already initialized (shouldn't happen)
            workerInitialized = true;
            workerReady = true;
            perf.markBoot("worker-connected");
            
            // Notify parent that worker is connected
            const workerConnectTime = performance.now();
            const workerElapsed = Math.round(workerConnectTime - diskLoadStartTime);
            if (window.acBOOT_LOG) {
              window.acBOOT_LOG(`connecting to worker (${workerElapsed}ms)`);
            } else if (window.parent) {
              window.parent.postMessage({ 
                type: "boot-log", 
                message: `connecting to worker (${workerElapsed}ms)` 
              }, "*");
            }

            // NOW send the initial message - disk.mjs is ready to receive it
            perf.markBoot("first-message-sent");
            send(firstMessage);
            consumeDiskSends(send);
            return;
          }
          
          workerReady = true;
          onMessage(e);
        };
      }
    };
    
    // Start the first worker attempt
    createWorker();
  } else {
    // B. No Worker Mode
    if (debug) console.log("🔴 No Worker");
    let module;
    try {
      module = await import(`./lib/disk.mjs`);
    } catch (err) {
      console.warn("Module load error:", err);
    }
    module.noWorker.postMessage = (e) => onMessage(e); // Define the disk's postMessage replacement.
    send = (e) => module.noWorker.onMessage(e); // Hook up our post method to disk's onmessage replacement.
    window.acSEND = send; // Make the message handler global, used in `speech.mjs` and also useful for debugging.
  }

  // The initial message sends the path and host to load the disk.
  // Note: In worker mode, this is now handled inside the if(worker.postMessage) block
  // to ensure proper timing. In no-worker mode, it's handled here.
  if (!workersEnabled) {
    send(firstMessage);
    consumeDiskSends(send);
  }

  // 🛑 HALT Detection - Watchdog for unresponsive disk worker
  let lastPongTime = Date.now();
  let haltDetected = false;
  let watchdogStarted = false;
  const HALT_TIMEOUT_MS = 12000; // 12 seconds before triggering HALT (more forgiving)
  const PING_INTERVAL_MS = 2000; // Ping every 2 seconds
  let missedPongs = 0; // Track consecutive missed pongs

  // Start the watchdog - called after worker first responds
  function startWatchdog() {
    if (watchdogStarted || !workersEnabled) return;
    watchdogStarted = true;
    lastPongTime = Date.now(); // Reset the pong time when watchdog actually starts
    missedPongs = 0;
    console.log("🐕 Watchdog started after worker initialization");
    
    // Send periodic pings to the worker
    setInterval(() => {
      if (!haltDetected) {
        send({ type: "watchdog:ping", content: { timestamp: Date.now() } });
      }
    }, PING_INTERVAL_MS);

    // Check for HALT condition - require multiple missed pongs
    setInterval(() => {
      if (haltDetected) return;
      const timeSinceLastPong = Date.now() - lastPongTime;
      if (timeSinceLastPong > PING_INTERVAL_MS * 1.5) {
        missedPongs++;
        console.warn("🐕 Missed pong #" + missedPongs + " (last pong " + Math.round(timeSinceLastPong / 1000) + "s ago)");
      } else {
        missedPongs = 0; // Reset on successful pong
      }
      // Only HALT after sustained unresponsiveness
      if (timeSinceLastPong > HALT_TIMEOUT_MS && missedPongs >= 4) {
        haltDetected = true;
        console.error("🛑 HALT DETECTED! Worker unresponsive for", timeSinceLastPong, "ms after", missedPongs, "missed pongs");
        triggerHaltSequence();
      }
    }, PING_INTERVAL_MS);
  }

  // Expose startWatchdog globally so receivedChange can call it
  window.acStartWatchdog = startWatchdog;

  // Handle pong responses from worker
  window.acHandleWatchdogPong = () => {
    lastPongTime = Date.now();
  };

  // HALT sequence: show black screen with clickable reload
  function triggerHaltSequence() {
    // Create HALT overlay - black background, white text, click to reload
    const overlay = document.createElement("div");
    overlay.id = "halt-overlay";
    overlay.style.cssText = `
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background: #000;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      font-family: monospace;
      z-index: 999999;
      cursor: pointer;
    `;
    overlay.innerHTML = `
      <div style="color: #fff; font-size: 6vw; font-weight: bold; margin-bottom: 20px;">
        ⚠️ HALT
      </div>
      <div style="color: #aaa; font-size: 2vw; margin-bottom: 40px;">
        Worker became unresponsive
      </div>
      <div style="color: #fff; font-size: 3vw; padding: 20px 40px; border: 2px solid #fff; border-radius: 8px;">
        Click anywhere to reload
      </div>
    `;
    
    // Click anywhere to reload
    overlay.addEventListener("click", () => {
      window.location.reload();
    });
    
    // Also allow pressing any key to reload
    document.addEventListener("keydown", () => {
      window.location.reload();
    }, { once: true });
    
    document.body.appendChild(overlay);
  }

  // 🎮 Initialize Game Boy emulator in main thread
  // (Placed after worker setup so `send` is properly wired)
  async function initGameboy() {
    if (!window.WasmBoy) {
      console.log("🎮 Loading wasmboy library...");
      // Load wasmboy library dynamically
      const wasmBoyModule = await import('./dep/wasmboy/wasmboy.ts.esm.js');
      console.log("🎮 wasmBoyModule:", wasmBoyModule);
      console.log("🎮 wasmBoyModule.WasmBoy:", wasmBoyModule.WasmBoy);
      console.log("🎮 wasmBoyModule keys:", Object.keys(wasmBoyModule));
      window.WasmBoy = wasmBoyModule.WasmBoy;
    }
    
    if (!gameboyEmulator) {
      console.log("🎮 Initializing Game Boy emulator with invisible canvas...");
      
      // Create a canvas but make it completely invisible
      const hiddenCanvas = document.createElement('canvas');
      hiddenCanvas.width = 160;
      hiddenCanvas.height = 144;
      hiddenCanvas.id = 'wasmboy-hidden-canvas';
      
      // Make canvas completely invisible using multiple methods
      hiddenCanvas.style.cssText = `
        position: fixed !important;
        left: -99999px !important;
        top: -99999px !important;
        width: 1px !important;
        height: 1px !important;
        visibility: hidden !important;
        opacity: 0 !important;
        pointer-events: none !important;
        z-index: -99999 !important;
        display: none !important;
      `;
      
      // Add comprehensive CSS to hide any wasmboy canvases
      if (!document.getElementById('wasmboy-hide-style')) {
        const style = document.createElement('style');
        style.id = 'wasmboy-hide-style';
        style.textContent = `
          canvas[width="160"][height="144"] {
            display: none !important;
            visibility: hidden !important;
            opacity: 0 !important;
            position: fixed !important;
            left: -99999px !important;
            top: -99999px !important;
            pointer-events: none !important;
            z-index: -99999 !important;
          }
          #wasmboy-hidden-canvas {
            display: none !important;
          }
        `;
        document.head.appendChild(style);
      }
      
      document.body.appendChild(hiddenCanvas);
      window.gameboyCanvas = hiddenCanvas; // Store reference for cleanup
      
      gameboyEmulator = window.WasmBoy; // It's a singleton, not a constructor
      
      await gameboyEmulator.config({
        headless: false, // Must be false for updateGraphicsCallback to work
        isAudioEnabled: true,
        updateGraphicsCallback: (imageDataArray) => {
          // Direct access to wasmboy's pixel buffer - much more efficient than canvas extraction
          if (imageDataArray && imageDataArray.length === 92160) { // 160 * 144 * 4 = 92160 RGBA pixels
            // Send the raw pixel data directly along with ROM metadata
            acDISK_SEND({
              type: "gameboy:frame-data", 
              content: { 
                pixels: imageDataArray,
                romName: currentGameboyROM?.originalName || "unknown",
                title: currentGameboyROM?.title || currentGameboyROM?.name || "unknown",
                isGameBoyColor: currentGameboyROM?.isGameBoyColor || false,
                customLabel: currentGameboyROM?.customLabel // Pass through custom label if provided
              }
            });
          }
        },
        updateAudioCallback: (audioContext, audioBufferSourceNode) => {
          // Route audio through AC's speaker system by connecting to sfxStreamGain
          if (sfxStreamGain && audioBufferSourceNode) {
            try {
              // Check if audio contexts match
              if (audioBufferSourceNode.context === sfxStreamGain.context) {
                audioBufferSourceNode.connect(sfxStreamGain);
              } else {
                console.log("🎮 Audio context mismatch - Game Boy audio isolated");
                // Let wasmboy handle its own audio output
              }
            } catch (error) {
              console.log("🎮 Game Boy audio connection failed:", error.message);
            }
          }
          return audioBufferSourceNode;
        }
      }, hiddenCanvas); // Pass the hidden canvas to setCanvas during config
      
      // Disable WasmBoy's default joypad handling so we can use our custom controls
      gameboyEmulator.disableDefaultJoypad();
      console.log("🎮 Disabled default joypad - using AC custom controls");
      
      console.log("🎮 Game Boy emulator initialized");
    }
  }

  // 🎮 Load a Game Boy ROM
  async function loadGameboyROM(romData) {
    try {
      if (!gameboyEmulator) {
        await initGameboy();
      }
      
      console.log("🎮 Loading ROM:", romData.originalName);
      
      // Reset emulator state before loading new ROM
      try {
        await gameboyEmulator.pause();
        await gameboyEmulator.reset();
        
        // Clear the canvas to ensure clean slate
        const canvas = gameboyEmulator.getCanvas();
        if (canvas) {
          const ctx = canvas.getContext('2d');
          if (ctx) {
            ctx.fillStyle = '#ffffff';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
          }
        }
        
        console.log("🎮 Emulator reset and canvas cleared");
      } catch (error) {
        console.log("🎮 Could not reset emulator:", error);
      }
      
      // Convert ArrayBuffer to Uint8Array for wasmboy
      const romBytes = new Uint8Array(romData.romData);
      await gameboyEmulator.loadROM(romBytes);
      await gameboyEmulator.play();
      
      // Try to get cartridge info/metadata from wasmboy
      try {
        const cartridgeInfo = await gameboyEmulator.getCartridgeInfo();
        console.log("🎮 Cartridge Info:", cartridgeInfo);
        
        // Add cartridge metadata to ROM data
        romData.cartridgeInfo = cartridgeInfo;
        romData.title = cartridgeInfo.titleAsString?.trim() || romData.name;
        romData.isGameBoyColor = cartridgeInfo.isGameBoyColor || romData.isGameBoyColor;
      } catch (error) {
        console.log("🎮 Could not get cartridge info:", error);
        romData.title = romData.name;
      }
      
      currentGameboyROM = romData;
      console.log("🎮 ROM loaded and playing:", romData.originalName);
      
      // Navigate to gameboy disk using AC's jump system
      console.log("🎮 About to jump to gameboy piece");
      send({ 
        type: "jump", 
        content: { 
          piece: "gameboy",
          ahistorical: false, // Add to history so user can go back
          alias: true
        }
      });
      console.log("🎮 Jump command sent to gameboy piece");
      
    } catch (error) {
      console.error("🎮 Failed to load ROM:", error);
    }
  }

  // 🎮 Handle Game Boy input from worker
  function handleGameboyInput(joypadState) {
    if (gameboyEmulator && currentGameboyROM) {
      // WasmBoy.setJoypadState expects an object with UPPERCASE keys
      // It internally converts them to the format the core needs
      const wasmBoyJoypadState = {
        UP: joypadState.up || false,
        RIGHT: joypadState.right || false,
        DOWN: joypadState.down || false,
        LEFT: joypadState.left || false,
        A: joypadState.a || false,
        B: joypadState.b || false,
        SELECT: joypadState.select || false,
        START: joypadState.start || false
      };
      gameboyEmulator.setJoypadState(wasmBoyJoypadState);
    }
  }

  // Beat

  // Set the default bpm.
  sound.bpm = bpm;

  // 🎹 DAW Sync (for Max for Live integration)
  // Only connect if ?daw query param is present (for M4L browser-based embedding)
  const hasDawParam = new URLSearchParams(window.location.search).has("daw");
  if (hasDawParam) {
    _dawConnectSend(send, updateMetronome);
  }

  function requestBeat(time) {
    send({
      type: "beat",
      content: { time, bpm: sound.bpm },
    });
  }

  // Called inside of `requestFrame`, and on the `beat` message.
  function updateSynths(content) {
    function beat() {
      if (sound.bpm !== content.bpm) {
        sound.bpm = content.bpm;
        updateMetronome(sound.bpm);
      }
      for (const sound of content.sounds) triggerSound(sound);
      for (const bubble of content.bubbles) updateBubble(bubble);
      for (const item of content.kills) killSound(item.id, item.fade);
    }

    if (
      (!triggerSound || audioContext?.state !== "running") &&
      (sound.bpm !== content.bpm ||
        content.sounds.length > 0 ||
        content.bubbles.length > 0 ||
        content.kills.length > 0)
    ) {
      activatedSoundCallback = beat;
      // 📓 Hold a single update frame if audio cuts out or is just beginning.
      return;
    } else beat();
  }

  // Update & Render
  let frameAlreadyRequested = false;

  // 🛑 Frame pump stall watchdog
  // If bios requests a frame and the worker never replies with render/update,
  // the UI can appear to freeze silently. Detect that and surface an error.
  // 🚀 OPTIMIZED: Uses frame counting instead of setTimeout/clearTimeout per frame
  const FRAME_STALL_FRAMES = 480; // ~8 seconds at 60fps
  let frameStallArmedAt = 0;
  let frameStallArmedFrame = 0n;
  let framesSinceLastRender = 0;

  function clearFrameStallWatchdog() {
    framesSinceLastRender = 0;
  }

  function showFrameStallOverlay(message) {
    // Minimal DOM fallback so the user sees *something* even if the worker is dead.
    try {
      const existing = document.getElementById("ac-stall-overlay");
      if (existing) existing.remove();
      const overlay = document.createElement("div");
      overlay.id = "ac-stall-overlay";
      overlay.style.position = "fixed";
      overlay.style.inset = "0";
      overlay.style.zIndex = "2147483647";
      overlay.style.background = "rgba(0,0,0,0.92)";
      overlay.style.color = "white";
      overlay.style.font = "14px/1.35 monospace";
      overlay.style.padding = "16px";
      overlay.style.whiteSpace = "pre-wrap";
      overlay.textContent = message;
      document.body.appendChild(overlay);
    } catch (e) {
      // Ignore overlay failures.
    }
  }

  function triggerFrameStall(message) {
    if (window.acFRAME_STALL_TRIPPED) return;
    window.acFRAME_STALL_TRIPPED = true;

    console.error("🛑 BIOS frame pump stalled:", message);
    showFrameStallOverlay(message);

    // Release the latch so the main loop can keep pumping if possible.
    frameAlreadyRequested = false;
    clearFrameStallWatchdog();

    // Attempt to route to a dedicated error piece (MatrixChunky8) if the worker is alive.
    try {
      const encoded = encodeURIComponent(message);
      send({
        type: "jump",
        content: {
          piece: `error~${encoded}`,
          ahistorical: true,
          alias: true,
        },
      });
    } catch (e) {
      // If the worker is dead/unresponsive, the DOM overlay remains.
    }
  }

  function armFrameStallWatchdog() {
    // 🚀 OPTIMIZED: Just increment counter, check threshold
    if (document.visibilityState && document.visibilityState !== "visible") {
      framesSinceLastRender = 0;
      return;
    }
    
    framesSinceLastRender++;
    
    if (framesSinceLastRender >= FRAME_STALL_FRAMES && frameAlreadyRequested) {
      const piece = typeof currentPiece === "string" ? currentPiece : "(unknown piece)";
      triggerFrameStall(
        `SYSTEM STALL\n\nNo render/update from disk after requesting a frame.\n\nPiece: ${piece}\nFrame: ${String(frameStallArmedFrame)}\nWaited: ~${Math.round(framesSinceLastRender / 60)}s`,
      );
    }
  }

  function requestFrame(needsRender, updateCount, nowUpdate) {
    now = nowUpdate;

    // Reframe should happen immediately when needed, not wait for render timing
    if (needsReframe) {
      frame(undefined, undefined, lastGap);
      pen?.retransformPosition();
      return;
    }

    if (frameAlreadyRequested) return;

    frameAlreadyRequested = true;
    frameCount += 1n;
    armFrameStallWatchdog();

    // 📊 Update DOM stats overlay if enabled
    updateStatsOverlay();

    // TODO: 📏 Measure performance of frame: test with different resolutions.

    // console.log("Sending frame...", frameCount, performance.now())

    // Grab a sample of any playing background music, calculate the frequency
    // and send as needed.
    let amplitude = 0;
    if (backgroundMusicEl.paused === false) {
      // Get the frequency data from the audio element
      analyser.getByteFrequencyData(frequencyData);

      // Calculate the maximum amplitude in the frequency data for this period.
      for (let i = 0; i < frequencyData.length; i += 1) {
        if (frequencyData[i] > amplitude) {
          amplitude = frequencyData[i];
        }
      }
    }

    // Transferrable objects
    let transferrableObjects = [];
    let pixelsBuffer = null;
    
    // Only transfer pixels buffer if it's not detached
    try {
      if (screen.pixels.buffer.byteLength > 0) {
        transferrableObjects = [screen.pixels.buffer];
        pixelsBuffer = screen.pixels.buffer;
      } else {
        // Buffer is detached, create a fresh copy from imageData
        if (imageData && imageData.data && imageData.data.buffer.byteLength > 0) {
          pixelsBuffer = imageData.data.slice().buffer;
        } else {
          // Last resort: create empty buffer with correct size
          const emptyPixels = new Uint8ClampedArray(canvas.width * canvas.height * 4);
          pixelsBuffer = emptyPixels.buffer;
        }
        transferrableObjects = [pixelsBuffer];
      }
    } catch (e) {
      // If we can't access the buffer at all, create a fresh copy from imageData or empty buffer
      try {
        if (imageData && imageData.data && imageData.data.buffer.byteLength > 0) {
          pixelsBuffer = imageData.data.slice().buffer;
        } else {
          // Last resort: create empty buffer with correct size
          const emptyPixels = new Uint8ClampedArray(canvas.width * canvas.height * 4);
          pixelsBuffer = emptyPixels.buffer;
        }
        transferrableObjects = [pixelsBuffer];
      } catch (fallbackError) {
        console.error("Failed to create fallback buffer:", fallbackError);
        // Create minimal empty buffer as absolute last resort
        const emptyPixels = new Uint8ClampedArray(64 * 64 * 4); // 64x64 fallback
        pixelsBuffer = emptyPixels.buffer;
        transferrableObjects = [pixelsBuffer];
      }
    }

    // TODO: Eventually make frequencyData transferrable?
    // if (frequencyData) {
    // transferrableObjects.push(frequencyData.buffer);
    // }

    send(
      {
        type: "frame",
        content: {
          needsRender,
          updateCount,
          pixels: pixelsBuffer,
          audioTime: audioContext?.currentTime || 0,
          audioBpm: sound.bpm, // TODO: Turn this into a messaging thing.
          audioMusicAmplitude: amplitude,
          audioMusicSampleData: amplitude > 0 ? frequencyData : [],
          width: canvas.width,
          height: canvas.height,
          // TODO: Do all fields of `pointer` need to be sent? 22.09.19.23.30
          pen: { 
            events: resolution.tv ? [] : (pen?.events || []), // Skip pen events in TV mode
            pointers: pen?.pointers || {} 
          },
          pen3d: ThreeD?.pollControllers(), // TODO: Implement pointers in 3D.
          hand: handData,
          keyboard: keyboard.events,
          gamepad: gamepad.events,           // Keep for backwards compat
          gamepads: gamepad.eventsByGamepad, // NEW: Separate streams per gamepad
          paused: window.__acLoopPaused || false, // Send loop paused state to worker
          // clipboardText: pastedText,
        },
      },
      transferrableObjects,
    );

    // if (Object.keys(pen.pointers).length > 1) {
    //   console.log(pen.events, pen.pointers);
    // }

    //pastedText = undefined; // Clear any pasted text.

    pen?.updatePastPositions();

    // Time budgeting stuff...
    //const updateDelta = performance.now() - updateNow;
    //console.log("Update Budget: ", round((updateDelta / updateRate) * 100));
    // TODO: Output this number graphically.

    // const renderNow = performance.now();
    // const renderDelta = performance.now() - renderNow;
    // console.log("Render Budget: ", round((renderDelta / renderRate) * 100));

    // TODO: Output this number graphically.

    //render3d();
    // Clear pen events.
    if (pen) pen.events.length = 0;
    if (ThreeD?.penEvents) ThreeD.penEvents.length = 0;

    // Clear keyboard and gamepad events.
    keyboard.events.length = 0;
    gamepad.clearEvents();
  }

  let frameCached = false;
  let pixelsDidChange = false; // TODO: Can this whole thing be removed? 2021.11.28.03.50

  
  let contentFrame;
  let ticketWrapper;
  let underlayFrame;  // const bakedCan = document.createElement("canvas", {
  //  willReadFrequently: true,
  // });
  let tapeManager; // Multi-tape manager for smooth transitions (tv.mjs)

    async function reinitAudioSystem(options = {}) {
      const { latencyHint, sampleRate, speakerPerformanceMode } = options;

      if (latencyHint !== undefined) {
        window.__acLatencyHint = latencyHint;
      }
      if (Number.isFinite(sampleRate)) {
        window.__acSampleRate = sampleRate;
      }
      if (speakerPerformanceMode) {
        window.__acSpeakerPerformanceMode = speakerPerformanceMode;
      }

      try {
        speakerProcessorNode?.port?.postMessage?.({ type: "kill:all" });
      } catch (e) {
        console.warn("🎵 Audio reinit: failed to kill sounds", e);
      }

      while (audioStarting) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }

      try {
        analyserSrc?.disconnect?.();
        analyser?.disconnect?.();
        await analyserCtx?.close?.();
      } catch (e) {
        console.warn("🎵 Audio reinit: failed to close analyser context", e);
      }

      try {
        if (backgroundMusicEl) {
          backgroundMusicEl.pause?.();
          backgroundMusicEl.src = "";
          backgroundMusicEl.remove?.();
        }
      } catch (e) {
        console.warn("🎵 Audio reinit: failed to reset background music", e);
      }

      backgroundMusicEl = document.createElement("audio");
      backgroundMusicEl.id = "background-music";
      backgroundMusicEl.crossOrigin = "anonymous";
      wrapper.appendChild(backgroundMusicEl);
      currentBackgroundTrack = null;

      try {
        if (audioContext) {
          await audioContext.close();
        }
      } catch (e) {
        console.warn("🎵 Audio reinit: failed to close AudioContext", e);
      }

      audioContext = null;
      speakerProcessorNode = null;
      window.audioWorkletReady = false;

      window.__acForceStartSound = true;
      startSound();
      window.__acForceStartSound = false;

      // Wait for the worklet to be fully initialized before returning
      const maxWaitMs = 5000;
      const startWait = performance.now();
      while (!window.audioWorkletReady && performance.now() - startWait < maxWaitMs) {
        await new Promise((resolve) => setTimeout(resolve, 20));
      }
      
      if (!window.audioWorkletReady) {
        console.error("🎵 Audio reinit: Timed out waiting for worklet to initialize");
      } else {
        console.log("🎵 Audio reinit complete, sample rate:", audioContext?.sampleRate);
      }
    }

  // *** Received Frame ***
  async function receivedChange({ data: { type, content } }) {
    // 🔍 DEBUG: Log ALL incoming messages to trace what's being received
    if (DEBUG_MESSAGE_FLOW && type !== "frame") {
      console.log(`🔍 BIOS receivedChange: type="${type}"`);
    }
    
    // Relay boot-log messages to parent window and update the overlay
    if (type === "boot-log") {
      // console.log("📢 BIOS relaying boot-log:", content);
      // Start the watchdog when the worker first responds (signals it's alive)
      if (content === "worker received load" && window.acStartWatchdog) {
        window.acStartWatchdog();
      }
      // Update the boot log overlay
      if (window.acBOOT_LOG) {
        window.acBOOT_LOG(content);
      }
      // Also relay to parent for embedded contexts
      if (window.parent) {
        window.parent.postMessage({ type: "boot-log", message: content }, "*");
      }
      return;
    }
    
    // 📄 Boot file content - display source in boot canvas
    if (type === "boot-file") {
      // NOTE: Rendering/tokenizing source for boot UI can be surprisingly expensive in some contexts.
      // Keep it opt-in to avoid freezing the main thread during piece loads.
      const DEBUG_BOOT_FILES = window.acDEBUG_BOOT_FILES === true;

      if (DEBUG_BOOT_FILES) {
        console.log(
          `🔍 BIOS: Received boot-file for "${content?.filename}", source length = ${content?.source?.length}`,
        );
      }

      if (DEBUG_BOOT_FILES && window.acBOOT_ADD_FILE && content?.filename && content?.source) {
        // Use setTimeout to make this non-blocking - prevents hangs from tokenizer regex
        setTimeout(() => {
          try {
            if (DEBUG_BOOT_FILES) {
              console.log(`🔍 BIOS: boot-file setTimeout executing for "${content?.filename}"`);
            }
            window.acBOOT_ADD_FILE(content.filename, content.source);
            if (DEBUG_BOOT_FILES) {
              console.log(`🔍 BIOS: boot-file acBOOT_ADD_FILE completed for "${content?.filename}"`);
            }
          } catch (e) {
            console.warn("boot-file display error:", e);
          }
        }, 0);
      }
      return;
    }

    // 📊 Receive disk worker timing data (silent)
    if (type === "disk-timings") {
      window.acDiskTimings = content;
      return;
    }

    // 🛑 Watchdog pong response from worker
    if (type === "watchdog:pong") {
      if (window.acHandleWatchdogPong) window.acHandleWatchdogPong();
      return;
    }

    // 🎹 Debug messages from disk worker (DAW sync debugging)
    if (type === "disk:debug") {
      console.log("🎹 BIOS received disk:debug from worker:", content);
      return;
    }

    // 🔍 Debug state from pieces for CDP automation
    if (type === "debug:state") {
      window.acPieceState = content;
      return;
    }
    
    // 🎹 DAW phase update - auto-resume AudioContext once for DAW mode
    if (type === "daw:phase:ack" && !dawAudioResumed) {
      if (audioContext && audioContext.state === "suspended") {
        dawAudioResumed = true;
        console.log("🎹 DAW phase: Auto-resuming AudioContext...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed from phase! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }
    
    // 🎹 DAW sync acknowledgements from disk worker
    if (type === "daw:tempo:ack") {
      console.log("🎹🎹🎹 BIOS received daw:tempo:ack from worker! BPM:", content?.bpm);
      // 🎹 Auto-resume AudioContext for DAW mode (jweb~ has no user interaction)
      if (audioContext && audioContext.state === "suspended") {
        console.log("🎹 DAW mode: Auto-resuming AudioContext...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed for DAW mode! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }
    if (type === "daw:transport:ack") {
      console.log("🎹🎹🎹 BIOS received daw:transport:ack from worker! Playing:", content?.playing);
      // 🎹 Auto-resume AudioContext for DAW mode (jweb~ has no user interaction)
      if (audioContext && audioContext.state === "suspended") {
        console.log("🎹 DAW mode: Auto-resuming AudioContext (from transport)...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed for DAW mode! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }

    // 🎹 Debug messages from disk worker (DAW sync debugging)
    if (type === "disk:debug") {
      console.log("🎹 BIOS received disk:debug from worker:", content);
      return;
    }
    
    // 🎹 DAW phase update - auto-resume AudioContext once for DAW mode
    if (type === "daw:phase:ack" && !dawAudioResumed) {
      if (audioContext && audioContext.state === "suspended") {
        dawAudioResumed = true;
        console.log("🎹 DAW phase: Auto-resuming AudioContext...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed from phase! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }
    
    // 🎹 DAW sync acknowledgements from disk worker
    if (type === "daw:tempo:ack") {
      console.log("🎹🎹🎹 BIOS received daw:tempo:ack from worker! BPM:", content?.bpm);
      // 🎹 Auto-resume AudioContext for DAW mode (jweb~ has no user interaction)
      if (audioContext && audioContext.state === "suspended") {
        console.log("🎹 DAW mode: Auto-resuming AudioContext...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed for DAW mode! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }
    if (type === "daw:transport:ack") {
      console.log("🎹🎹🎹 BIOS received daw:transport:ack from worker! Playing:", content?.playing);
      // 🎹 Auto-resume AudioContext for DAW mode (jweb~ has no user interaction)
      if (audioContext && audioContext.state === "suspended") {
        console.log("🎹 DAW mode: Auto-resuming AudioContext (from transport)...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed for DAW mode! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }

    // 🎹 Debug messages from disk worker (DAW sync debugging)
    if (type === "disk:debug") {
      console.log("🎹 BIOS received disk:debug from worker:", content);
      return;
    }
    
    // 🎹 DAW phase update - auto-resume AudioContext once for DAW mode
    if (type === "daw:phase:ack" && !dawAudioResumed) {
      if (audioContext && audioContext.state === "suspended") {
        dawAudioResumed = true;
        console.log("🎹 DAW phase: Auto-resuming AudioContext...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed from phase! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }
    
    // 🎹 DAW sync acknowledgements from disk worker
    if (type === "daw:tempo:ack") {
      console.log("🎹🎹🎹 BIOS received daw:tempo:ack from worker! BPM:", content?.bpm);
      // 🎹 Auto-resume AudioContext for DAW mode (jweb~ has no user interaction)
      if (audioContext && audioContext.state === "suspended") {
        console.log("🎹 DAW mode: Auto-resuming AudioContext...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed for DAW mode! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }
    if (type === "daw:transport:ack") {
      console.log("🎹🎹🎹 BIOS received daw:transport:ack from worker! Playing:", content?.playing);
      // 🎹 Auto-resume AudioContext for DAW mode (jweb~ has no user interaction)
      if (audioContext && audioContext.state === "suspended") {
        console.log("🎹 DAW mode: Auto-resuming AudioContext (from transport)...");
        audioContext.resume().then(() => {
          console.log("🎹 AudioContext resumed for DAW mode! State:", audioContext.state);
        }).catch(e => console.warn("🎹 AudioContext resume failed:", e));
      }
      return;
    }

    // Helper function to generate appropriate filenames for tape recordings
    function generateTapeFilename(extension, suffix = "") {
      const options = window.currentRecordingOptions || {
        pieceName: "tape",
        pieceParams: "",
        originalCommand: ""
      };
      
      // Debug logging for filename issues
      console.log("🎬 generateTapeFilename debug:", {
        extension,
        suffix,
        options,
        baseName: options.pieceName,
        cachedCode: options.cachedCode,
        HANDLE,
        location: location?.host || "unknown"
      });
      
      let baseName = options.pieceName || "tape";
      // Handle special cases for kidlisp codes
      if (baseName === "$code") {
        // For kidlisp source code, use the cached code if available
        if (options.cachedCode) {
          baseName = `$${options.cachedCode}`;
        } else {
          // Try to extract a $code from the original command as fallback
          const codeMatch = options.originalCommand.match(/\$([a-zA-Z0-9]+)/);
          if (codeMatch) {
            baseName = `$${codeMatch[1]}`;
          } else {
            // No $code available, use "kidlisp" instead of literal "$code"
            baseName = "kidlisp";
          }
        }
      } else if (baseName.startsWith("$") && options.cachedCode) {
        // If baseName is already a $code (like $erl) and we have cached code,
        // don't apply the cached code again to avoid duplication
      } else if (baseName === options.cachedCode) {
        // If baseName matches cachedCode exactly (like "clock" === "clock"),
        // don't apply cachedCode again to avoid duplication like "clockclock"
      }
      // Note: If pieceName already has $ prefix (from cached code), use it as-is
      
      // Add parameters if they exist
      const params = options.pieceParams || "";
      const paramsStr = params ? params.replace(/~/g, "-") : "";
      
      // Include user handle if available for personalized filenames
      // Use cached computed timestamp for perfect filename/visual synchronization,
      // otherwise fall back to recording start timestamp or current timestamp
      let fileTimestamp;
      let durationPart = "";
      
      if (window.firstFrameComputedTimestamp && extension === "gif") {
        // For GIF recordings, use the exact same computed timestamp as shown in the first frame
        // Use the same formatting as the visual timestamp (no zero-padding except milliseconds)
        const d = new Date(window.firstFrameComputedTimestamp);
        const year = d.getFullYear();
        const month = d.getMonth() + 1; // getMonth() returns 0-11
        const day = d.getDate();
        const hour = d.getHours();
        const minute = d.getMinutes();
        const second = d.getSeconds();
        const millisecond = d.getMilliseconds();
        
        // Match visual timestamp format exactly: no zero-padding except for milliseconds
        fileTimestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
        
        // Check if this was a frame-based recording (check frameMode first, then fallback to gifDurationMs)
        const isFrameBased = window.currentRecordingOptions && 
                            window.currentRecordingOptions.frameMode && 
                            window.currentRecordingOptions.frameCount;
        
        if (isFrameBased) {
          // For frame-based recordings, use frame count with 'f' suffix
          const frameCount = window.currentRecordingOptions.frameCount;
          durationPart = `-${frameCount}f`;
        } else if (window.gifDurationMs) {
          // For time-based recordings, use seconds with 's' suffix
          const totalSeconds = Math.round(window.gifDurationMs / 1000 * 10) / 10; // Round to 1 decimal place
          durationPart = `-${totalSeconds}s`;
        }
        
        console.log(`🎬 Using cached computed timestamp for GIF filename: ${d.toISOString()}${durationPart ? ` (duration: ${durationPart})` : ''}`);
      } else if (window.recordingStartTimestamp && extension === "gif") {
        // Fallback to recording start timestamp if no cached computed timestamp
        // Use the same formatting as the visual timestamp (no zero-padding except milliseconds)
        const d = new Date(window.recordingStartTimestamp);
        const year = d.getFullYear();
        const month = d.getMonth() + 1;
        const day = d.getDate();
        const hour = d.getHours();
        const minute = d.getMinutes();
        const second = d.getSeconds();
        const millisecond = d.getMilliseconds();
        
        // Match visual timestamp format exactly: no zero-padding except for milliseconds
        fileTimestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
        
        // Check if this was a frame-based recording (check frameMode first, then fallback to gifDurationMs)
        const isFrameBased = window.currentRecordingOptions && 
                            window.currentRecordingOptions.frameMode && 
                            window.currentRecordingOptions.frameCount;
        
        if (isFrameBased) {
          // For frame-based recordings, use frame count with 'f' suffix
          const frameCount = window.currentRecordingOptions.frameCount;
          durationPart = `-${frameCount}f`;
        } else if (window.gifDurationMs) {
          // For time-based recordings, use seconds with 's' suffix
          const totalSeconds = Math.round(window.gifDurationMs / 1000 * 10) / 10; // Round to 1 decimal place
          durationPart = `-${totalSeconds}s`;
        }
        
        console.log(`🎬 Using recording start timestamp for GIF filename: ${d.toISOString()}${durationPart ? ` (duration: ${durationPart})` : ''}`);
      } else {
        // For other files or when no recording timestamp is available
        // Add safety check for longer recordings
        try {
          fileTimestamp = timestamp();
        } catch (error) {
          console.warn("⚠️ Timestamp generation failed, using simple fallback:", error.message);
          const now = new Date();
          fileTimestamp = `${now.getFullYear()}.${now.getMonth()+1}.${now.getDate()}.${now.getHours()}.${now.getMinutes()}.${now.getSeconds()}`;
        }
      }
      
      // Assemble filename parts: [@handle-][pieceName[params]-]timestamp-duration[suffix].extension
      // Skip command entirely in mystery mode
      const parts = [];
      
      // Add handle prefix if user has one
      if (HANDLE && typeof HANDLE === 'string' && HANDLE.length > 0) {
        console.log(`🎬 Adding handle to filename: "${HANDLE}"`);
        // Ensure handle doesn't already have @ prefix to avoid @@
        const handlePrefix = HANDLE.startsWith('@') ? HANDLE : `@${HANDLE}`;
        parts.push(handlePrefix);
      }
      
      // Add piece name with parameters (skip entirely if mystery mode)
      if (!options.mystery && baseName && typeof baseName === 'string') {
        console.log(`🎬 Adding piece name to filename: "${baseName}" (params: "${paramsStr}")`);
        parts.push(baseName + paramsStr);
      } else if (options.mystery) {
        console.log(`🎬 Skipping piece name due to mystery mode`);
      } else {
        console.log(`🎬 Skipping piece name - baseName: "${baseName}", type: ${typeof baseName}`);
      }
      
      // Add timestamp with duration - ensure these are valid strings
      if (fileTimestamp && typeof fileTimestamp === 'string') {
        parts.push(fileTimestamp + durationPart + suffix);
      } else {
        // Emergency fallback timestamp
        const emergency = Date.now().toString();
        console.warn("⚠️ Using emergency timestamp fallback:", emergency);
        parts.push(emergency + suffix);
      }
      
      // Final safety check and assembly
      const validParts = parts.filter(part => part && typeof part === 'string' && part.length > 0);
      if (validParts.length === 0) {
        console.warn("⚠️ No valid filename parts, using emergency fallback");
        return `tape-${Date.now()}.${extension}`;
      }
      
      return validParts.join("-") + "." + extension;
    }

    if (type === "pen:lock") {
      // console.log("🖋️ Request pen lock...");
      wrapper.requestPointerLock();
      return;
    }

    // 🎮 Handle Game Boy input from worker
    if (type === "gameboy:input") {
      handleGameboyInput(content);
      return;
    }

    // 🎮 Handle ROM loading request from disk (e.g., slgb piece)
    if (type === "gameboy:load-rom") {
      console.log("🎮 BIOS received ROM load request:", content);
      loadGameboyROM(content);
      return;
    }

    if (type === "usb:connect") {
      USB.connectToUsb();
      return;
    }

    if (type === "midi:connect") {
      MIDI.initialize(); // Start 🎹 Detection.
      return;
    }

    // Handle audio context resume request from video piece
    if (type === "audio-context:resume-request") {
      if (!window.acPACK_MODE) console.log("🎵 BIOS received audio context resume request:", content);
      if (audioContext && audioContext.state === "suspended") {
        if (!window.acPACK_MODE) console.log("🎵 BIOS resuming audio context from user gesture");
        const resumeStartTime = performance.now();
        audioContext.resume().then(() => {
          const resumeEndTime = performance.now();
          window.audioContextResumeTimestamps.requestedAt = resumeStartTime;
          window.audioContextResumeTimestamps.completedAt = resumeEndTime;
          if (!window.acPACK_MODE) console.log("🎵 BIOS audio context resumed, new state:", audioContext.state);
          
          // Notify video piece of successful resume
          if (!window.acPACK_MODE) console.log("🎵 BIOS sending updated AudioContext state after resume:", { state: audioContext.state, hasAudio: true });
          send({ 
            type: "tape:audio-context-state", 
            content: { 
              state: audioContext.state,
              hasAudio: true
            } 
          });
        }).catch(error => {
          console.warn("🎵 BIOS audio context resume failed:", error.message);
        });
      } else if (audioContext) {
        if (!window.acPACK_MODE) console.log("🎵 BIOS audio context already running:", audioContext.state);
        // Send current state back to video piece
        send({ 
          type: "tape:audio-context-state", 
          content: { 
            state: audioContext.state,
            hasAudio: true
          } 
        });
      } else {
        console.log("🎵 BIOS no audio context available");
      }
      return;
    }



    // Respond to a request to send a message through to the iMessage extension.
    if (type === "imessage-extension:send") {
      let body = content.body.pixels
        ? await bufferToBlob(content.body.pixels, undefined, {
            scale: 6,
            dataURL: true,
          })
        : content.body;
      const message = { type: content.type, body };
      const packedMessage = JSON.stringify(message);
      if (debug) console.log("🗨️ Sending to iMessage:", packedMessage);
      window.webkit?.messageHandlers?.iMessageExtension.postMessage(
        packedMessage,
      );
      return;
    }

    if (type === "ios:send") {
      iOSAppSend({ type: content.type, body: content.body });
      return;
    }

    // Window control requests from pieces (e.g., prompt.mjs running in worker)
    if (type === "window:open") {
      const url = content?.url || content?.href || content?.path;
      const index = content?.index ?? 0;
      const total = content?.total ?? 1;
      if (url) {
        const isElectron = /Electron/i.test(navigator.userAgent || "");
        // Use the Electron webview preload API if available (preferred)
        if (isElectron && window.acElectron?.openWindow) {
          console.log('[bios] Using acElectron.openWindow:', url, index, total);
          window.acElectron.openWindow({ url, index, total });
        } else if (isElectron) {
          // Fallback: window.open triggers setWindowOpenHandler in main.js
          console.log('[bios] Using window.open for new window:', url);
          try {
            window.open(url, "_blank");
          } catch (err) {
            console.warn('[bios] window.open failed:', err);
          }
        } else {
          // Browser: just open a new tab
          window.open(url, "_blank", "noopener");
        }
      }
      return;
    }

    if (type === "window:close") {
      const isElectron = /Electron/i.test(navigator.userAgent || "");
      if (isElectron) {
        // Use the Electron webview preload API if available (preferred)
        if (window.acElectron?.closeWindow) {
          console.log('[bios] Using acElectron.closeWindow');
          window.acElectron.closeWindow();
        } else {
          // Fallback: window.open with ac://close triggers setWindowOpenHandler
          console.log('[bios] Using window.open ac://close');
          try {
            const result = window.open("ac://close", "_blank");
            if (!result) {
              // If popup blocked, try navigation (will-navigate handler catches it)
              location.href = "ac://close";
            }
          } catch (err) {
            location.href = "ac://close";
          }
        }
      } else {
        try {
          window.close();
        } catch (err) {
          // ignore
        }
      }
      return;
    }

    // Post a message to a potential iframe parent, like in the VSCode extension.
    if (type === "post-to-parent") {
      // Note: kidlisp-code-created and setCode contain cached $identifiers (like "nece"),
      // not actual source code. The actual source is set via kidlisp-reload in boot.mjs.
      // if (debug) console.log("🏃‍♂️ Posting up to parent...", content);
      if (window.parent) window.parent.postMessage(content, "*");
      return;
    }

    // Connect to a UDP server,
    // which will pass messages to the disk runner.
    if (type === "udp:connect") {
      const udp = await loadUDP();
      udp.connect(content.port, content.url, send, content.turnHost);
      return;
    }

    // Send a message to the UDP server.
    if (type === "udp:send") {
      const udp = await loadUDP();
      udp.send(content);
      return;
    } // Disconect from the UDP server.
    if (type === "udp:disconnect") {
      const udp = await loadUDP();
      udp.disconnect(content.outageSeconds);
      return;
    }

    if (type === "ticket-wall") {
      if (!window.Stripe) await loadStripe();
      let pretext = ``;
      let button = `buy ticket`;
      let color = `white;`;
      let desc = `Chat now or check email for tickets.<br><br><span id="stripe">Processed by <a href="https://stripe.com">Stripe</a></span>`;

      if (content.item === "botce") {
        pretext = `
        <div id="pretext">
          <img style="image-rendering: pixelated;" cross-origin="anonymous" src="https://sotce-media.aesthetic.computer/botce-b.gif">
          <div id="pretext-bullets">
            <span id="desc">visit with <code>botce</code></span>
            <ul id="features">
              <li><code>botce</code> is a helpful ai</li>
              <li><code>botce</code> has advice</li>
              <!--<li><code>botce</code> is here until 11/25</li>-->
            </ul>
          </div>
        </div>
        <style>
         #pretext {
           display: flex;
           flex-direction: row;
           padding-bottom: 1.5em;
         }
         #pretext-bullets {
          display: flex;
           flex-direction: column;
           color: black;
         }
         #pretext img {
          /* border: 2px solid white; */
          border-radius: 10%;
          max-width: 30%;
          width: 100%;
          height: 100%;
         }
         #features {
          padding-left: 1.7em;
          margin-top: 0.4em;
          list-style-type: none;
         }
         #desc {
          color: #30313D;
          margin: 0 auto 0 1em;
          background: rgba(255, 100, 100, 0.5);me
          border-radius: 6px;
          padding: 8px 12px;
         }
         code {
          font-weight: bold;
         }
        </style>
        `;

        button = "$6";
        color = "rgb(255, 200, 200, 0.95)";
      }

      const template = `
        <link rel="stylesheet" href="aesthetic.computer/checkout.css" />
        <form style="background: ${color};" id="payment-form" class=${content.item}>
          ${pretext}
          <div id="link-authentication-element">
          </div>
          <div id="payment-element">
          </div>
          <!-- <div id="payment-request-button"></div> -->
          <button id="submit">
            <div class="spinner hidden" id="spinner">Processing...</div>
            <span id="button-text">${button}</span>
          </button>
          <div id="payment-description">${desc}</div>
          <div id="payment-message-wrapper">
            <div id="payment-message" class="hidden"></div>
          </div>
        </form>
      `;
      ticketWrapper = document.createElement("div");
      ticketWrapper.id = "ticket";
      ticketWrapper.innerHTML = template;
      ticketWrapper.classList.add("hidden");
      wrapper.appendChild(ticketWrapper);
      const { ticket } = await import(`./lib/ticket.mjs`);
      ticket(content.from, content.item, () => {
        ticketWrapper.classList.remove("hidden");
      }); // Open the ticket overlay.
      return;
    }

    if (type === "handle") {
      HANDLE = content; // Set the global HANDLE const to the user handle.
      return;
    }

    if (type === "kidlisp:cached-code") {
      // Update recording options with the newly cached code
      if (window.currentRecordingOptions) {
        window.currentRecordingOptions.cachedCode = content;
        // If pieceName was initially set to "$code", update it to the actual cached code
        if (window.currentRecordingOptions.pieceName === "$code") {
          window.currentRecordingOptions.pieceName = `$${content}`;
          console.log(`🎬 Updated pieceName to: $${content}`);
        }
      }
      return;
    }

    // 🎵 Clock piece cached code - forward back to disk.mjs for QR display
    if (type === "clock:cached") {
      // Forward to disk.mjs so it can display the QR code
      send({ type: "clock:cached", content });
      // Also update recording options if active
      if (window.currentRecordingOptions) {
        window.currentRecordingOptions.cachedCode = content?.code;
        if (window.currentRecordingOptions.pieceName === "*code") {
          window.currentRecordingOptions.pieceName = `*${content?.code}`;
          console.log(`🎬 Updated pieceName to: *${content?.code}`);
        }
      }
      return;
    }

    // Handle export events forwarded from worker via send() fallback
    if (type === "export-event-fallback") {
      // Route export events to current piece through actEvents
      if (actEvents && content && content.eventType && content.eventContent) {
        const event = {
          is: (eventType) => eventType === content.eventType,
          type: content.eventType,
          content: content.eventContent,
          progress: content.eventType === "recorder:transcode-progress" ? content.eventContent : undefined
        };
        
        console.log("🔗 ✅ Routed fallback export event to piece:", content.eventType);
        actEvents.add(event);
      } else {
        console.log("🔗 ❌ Cannot route fallback event - missing actEvents or content:", !!actEvents, !!content);
      }
      return;
    }

    // Handle direct export events from worker (when actEvents unavailable during export)
    if (
      type === "recorder:export-status" ||
      type === "recorder:export-progress" ||
      type === "recorder:transcode-progress" ||
      type === "recorder:export-complete"
    ) {
      // Route direct export events to current piece through actEvents
      if (actEvents) {
        const event = {
          is: (eventType) => eventType === type,
          type: type,
          content: content,
          progress: type === "recorder:transcode-progress" ? content : undefined
        };
        
        console.log("🔗 ✅ Routed direct export event to piece:", type);
        actEvents.add(event);
      }
      return;
    }

    // Sync labelBack state between worker and main thread
    if (type === "labelBack:set") {
      mainThreadLabelBack = true;
      window.safeSessionStorageSet("aesthetic-labelBack", "true");
      console.log("🔗 Main thread: Set labelBack in sessionStorage");
      return;
    }

    if (type === "labelBack:source") {
      // Store the source piece when labelBack is activated
      const sourcePiece = content?.sourcePiece;
      if (sourcePiece) {
        window.safeSessionStorageSet("aesthetic-labelBack-source", sourcePiece);
        console.log(`🔗 Main thread: Stored labelBack source: ${sourcePiece}`);
      }
      return;
    }

    if (type === "labelBack:clear") {
      mainThreadLabelBack = false;
      window.safeSessionStorageRemove("aesthetic-labelBack");
      window.safeSessionStorageRemove("aesthetic-labelBack-source");
      console.log("🔗 Main thread: Cleared labelBack from sessionStorage");
      return;
    }

    // Helper function to ensure fonts are loaded before use
    let fontsLoaded = false;
    async function ensureFontsLoaded() {
      if (fontsLoaded) {
        return; // Fonts already loaded
      }

      if ("fonts" in document) {
        try {
          // Force font loading with multiple strategies
          const fontPromises = [
            document.fonts.load("1em YWFTProcessing-Regular"),
            document.fonts.load("1em YWFTProcessing-Light"),
            document.fonts.load("bold 1em YWFTProcessing-Regular"),
            document.fonts.load("normal 1em YWFTProcessing-Light"),
          ];

          await Promise.all(fontPromises);
          console.log("✅ Fonts loaded for tape overlay");

          // Additional verification: check if fonts are actually available
          const fontFaces = Array.from(document.fonts);
          const hasRegular = fontFaces.some((font) =>
            font.family.includes("YWFTProcessing-Regular"),
          );
          const hasLight = fontFaces.some((font) =>
            font.family.includes("YWFTProcessing-Light"),
          );

          if (!hasRegular || !hasLight) {
            console.warn(
              "⚠️ Font verification failed - fonts may not be fully loaded",
            );
            // Wait a bit more and try again
            await new Promise((resolve) => setTimeout(resolve, 500));
            await Promise.all(fontPromises);
          }

          fontsLoaded = true; // Mark fonts as loaded
        } catch (error) {
          console.warn("⚠️ Failed to load fonts for tape overlay:", error);
          // Fallback: wait for document.fonts.ready
          await document.fonts.ready;
          fontsLoaded = true; // Mark as loaded even if fallback was used
        }
      }
    }

    // Helper function to add the sideways AC stamp (same as video recording)
    async function addAestheticComputerStamp(ctx, canvasWidth, canvasHeight, progress = 0, frameData = null, frameMetadata = null, frameIndex = null, totalFrames = null) {
      // Skip all stamps in clean mode
      if (window.currentRecordingOptions?.cleanMode) {
        console.log("🎬 🧹 Skipping Aesthetic Computer stamp in clean mode");
        return;
      }
      
      // Ensure fonts are loaded before drawing
      await ensureFontsLoaded();

      // 2. Set up the font.
      const typeSize = Math.min(
        18,
        Math.max(12, Math.floor(canvasHeight / 40)),
      );
      const gap = typeSize * 0.75;

      ctx.save(); // Save the current state of the canvas

      // Calculate stamp visibility for timestamp fade coordination
      let bothStampsInvisible = false;
      const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
      
      if (isFrameBasedRecording) {
        // For frame-based recordings longer than 60 frames: use alternating side timing logic
        const frameCount = window.currentRecordingOptions?.frameCount || 0;
        
        if (frameCount > 60 && frameIndex !== null) {
          const cyclesPerGif = 2;
          const frameProgress = frameIndex / frameCount;
          const cyclePosition = (frameProgress * cyclesPerGif) % 1.0;
          
          // Check if we're in the "both off" phases (0-20% and 40-60%)
          if ((cyclePosition < 0.2) || (cyclePosition >= 0.4 && cyclePosition < 0.6)) {
            bothStampsInvisible = true;
          }
        }
      } else {
        // Time-based recordings
        const cyclesPerGif = 2;
        const cyclePosition = (progress * cyclesPerGif) % 1.0;
        
        // Check if we're in the "both off" phases (0-20% and 40-60%)
        if ((cyclePosition < 0.2) || (cyclePosition >= 0.4 && cyclePosition < 0.6)) {
          bothStampsInvisible = true;
        }
      }

      // Add film camera style timestamp at bottom-left corner
      addFilmTimestamp(ctx, canvasWidth, canvasHeight, typeSize, progress, frameData, frameMetadata, frameIndex, totalFrames, bothStampsInvisible);

      // Add Tezos watermark if enabled
      if (window.currentRecordingOptions?.showTezosStamp) {
        // Calculate both stamp visibility for Tezos blink sync
        let showLeftStamp = false;
        let showRightStamp = false;
        const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
        
        if (isFrameBasedRecording) {
          showLeftStamp = true;
          showRightStamp = true;
        } else {
          const cyclesPerGif = 2;
          const cyclePosition = (progress * cyclesPerGif) % 1.0;
          
          if (cyclePosition < 0.2) {
            // Phase 1 (0-20%): Both off
            showLeftStamp = false;
            showRightStamp = false;
          } else if (cyclePosition < 0.4) {
            // Phase 2 (20-40%): Both on
            showLeftStamp = true;
            showRightStamp = true;
          } else if (cyclePosition < 0.6) {
            // Phase 3 (40-60%): Both off
            showLeftStamp = false;
            showRightStamp = false;
          } else if (cyclePosition < 0.8) {
            // Phase 4 (60-80%): Left only
            showLeftStamp = true;
            showRightStamp = false;
          } else {
            // Phase 5 (80-100%): Right only
            showLeftStamp = false;
            showRightStamp = true;
          }
        }
        
        // Removed addTezosStamp from top right - now positioned with timestamp
      }

      // Add Tezos watermark to top-right if enabled (but not when right stamp is showing)
      if (window.currentRecordingOptions?.showTezosStamp) {
        const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
        
        // Use the SAME visibility logic as the actual stamps to avoid conflicts
        let showRightStamp = false;
        
        if (isFrameBasedRecording) {
          // For frame-based recordings longer than 60 frames: use alternating side timing logic
          const frameCount = window.currentRecordingOptions?.frameCount || 0;
          
          if (frameCount > 60 && frameIndex !== null) {
            // 5-phase pattern across total frame count
            const cyclesPerGif = 2;
            const frameProgress = frameIndex / frameCount;
            const cyclePosition = (frameProgress * cyclesPerGif) % 1.0;
            
            if (cyclePosition < 0.2) {
              showRightStamp = false;
            } else if (cyclePosition < 0.4) {
              showRightStamp = true;
            } else if (cyclePosition < 0.6) {
              showRightStamp = false;
            } else if (cyclePosition < 0.8) {
              showRightStamp = false; // Left only
            } else {
              showRightStamp = true; // Right only
            }
          } else {
            // For shorter recordings: always show both stamps, so Tezos should never show
            showRightStamp = true;
          }
        } else {
          // Time-based recordings: use progress-based visibility (SAME as stamp logic)
          const cyclesPerGif = 2;
          const cyclePosition = (progress * cyclesPerGif) % 1.0;
          
          if (cyclePosition < 0.2) {
            showRightStamp = false; // Both off
          } else if (cyclePosition < 0.4) {
            showRightStamp = true; // Both on
          } else if (cyclePosition < 0.6) {
            showRightStamp = false; // Both off
          } else if (cyclePosition < 0.8) {
            showRightStamp = false; // Left only
          } else {
            showRightStamp = true; // Right only
          }
        }
        
        // Calculate fade alpha for Tezos logo (inverse of right stamp)
        let tezosFadeAlpha = 1.0;
        
        if (isFrameBasedRecording) {
          const frameCount = window.currentRecordingOptions?.frameCount || 0;
          
          if (frameCount > 60 && frameIndex !== null) {
            const cyclesPerGif = 2;
            const frameProgress = frameIndex / frameCount;
            const cyclePosition = (frameProgress * cyclesPerGif) % 1.0;
            
            // Tezos is visible when right stamp is NOT visible, with longer fades
            if (cyclePosition < 0.15) {
              tezosFadeAlpha = 1.0; // Fully visible
            } else if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
              tezosFadeAlpha = 1.0 - ((cyclePosition - 0.15) / 0.10); // Fade out as right stamp fades in
            } else if (cyclePosition > 0.25 && cyclePosition < 0.35) {
              tezosFadeAlpha = 0.0; // Hidden while both stamps on
            } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
              tezosFadeAlpha = (cyclePosition - 0.35) / 0.10; // Fade in as stamps fade out
            } else if (cyclePosition > 0.45 && cyclePosition < 0.75) {
              tezosFadeAlpha = 1.0; // Visible during off phases and left-only
            } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
              tezosFadeAlpha = 1.0 - ((cyclePosition - 0.75) / 0.10); // Fade out as right stamp fades in
            } else {
              tezosFadeAlpha = 0.0; // Hidden during right-only phase
            }
          } else {
            tezosFadeAlpha = 0.0; // Hidden for shorter recordings
          }
        } else {
          // Time-based recordings
          const cyclesPerGif = 2;
          const cyclePosition = (progress * cyclesPerGif) % 1.0;
          
          if (cyclePosition < 0.15) {
            tezosFadeAlpha = 1.0;
          } else if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
            tezosFadeAlpha = 1.0 - ((cyclePosition - 0.15) / 0.10);
          } else if (cyclePosition > 0.25 && cyclePosition < 0.35) {
            tezosFadeAlpha = 0.0;
          } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
            tezosFadeAlpha = (cyclePosition - 0.35) / 0.10;
          } else if (cyclePosition > 0.45 && cyclePosition < 0.75) {
            tezosFadeAlpha = 1.0;
          } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
            tezosFadeAlpha = 1.0 - ((cyclePosition - 0.75) / 0.10);
          } else {
            tezosFadeAlpha = 0.0;
          }
        }
        
        // Clamp alpha and show with fade
        tezosFadeAlpha = Math.max(0.0, Math.min(1.0, tezosFadeAlpha));
        
        if (tezosFadeAlpha > 0.0) {
          addTezosStamp(ctx, canvasWidth, canvasHeight, typeSize, isFrameBasedRecording, true, true, frameIndex, tezosFadeAlpha);
        }
      }

      drawTextAtPosition(0, 90); // Left
      drawTextAtPosition(canvasWidth, -90); // Right

      ctx.restore();
      ctx.globalAlpha = 1;

      function drawTextAtPosition(positionX, deg) {
        // Skip fade animations for frame-based recordings
        const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
        
        // Debug logging
        if (frameIndex === 0) {
          console.log("🎬 Stamp visibility debug:", {
            isFrameBasedRecording,
            frameMode: window.currentRecordingOptions?.frameMode,
            showTezosStamp: window.currentRecordingOptions?.showTezosStamp
          });
        }
        
        let showLeftStamp = false;
        let showRightStamp = false;
        
        if (isFrameBasedRecording) {
          // For frame-based recordings longer than 60 frames: use alternating side timing logic
          const frameCount = window.currentRecordingOptions?.frameCount || 0;
          
          if (frameCount > 60 && frameIndex !== null) {
            // 5-phase pattern across total frame count: both off, both on, both off, left only, right only
            const cyclesPerGif = 2; // Pattern loops 2 times across the full recording
            const frameProgress = frameIndex / frameCount; // 0-1 progress through recording
            const cyclePosition = (frameProgress * cyclesPerGif) % 1.0; // 0-1 within current cycle
            
            if (cyclePosition < 0.2) {
              // Phase 1 (0-20%): Both off
              showLeftStamp = false;
              showRightStamp = false;
            } else if (cyclePosition < 0.4) {
              // Phase 2 (20-40%): Both on
              showLeftStamp = true;
              showRightStamp = true;
            } else if (cyclePosition < 0.6) {
              // Phase 3 (40-60%): Both off
              showLeftStamp = false;
              showRightStamp = false;
            } else if (cyclePosition < 0.8) {
              // Phase 4 (60-80%): Left only
              showLeftStamp = true;
              showRightStamp = false;
            } else {
              // Phase 5 (80-100%): Right only
              showLeftStamp = false;
              showRightStamp = true;
            }
          } else {
            // For shorter frame recordings: always show both stamps
            showLeftStamp = true;
            showRightStamp = true;
          }
        } else {
          // 5-phase pattern stretched across GIF duration: both off, both on, both off, left only, right only
          // Use progress (0-1) to determine cycle position - allows multiple loops per GIF
          const cyclesPerGif = 2; // Pattern loops 2 times across the full GIF duration
          const cyclePosition = (progress * cyclesPerGif) % 1.0; // 0-1 within current cycle
          
          if (cyclePosition < 0.2) {
            // Phase 1 (0-20%): Both off
            showLeftStamp = false;
            showRightStamp = false;
          } else if (cyclePosition < 0.4) {
            // Phase 2 (20-40%): Both on
            showLeftStamp = true;
            showRightStamp = true;
          } else if (cyclePosition < 0.6) {
            // Phase 3 (40-60%): Both off
            showLeftStamp = false;
            showRightStamp = false;
          } else if (cyclePosition < 0.8) {
            // Phase 4 (60-80%): Left only
            showLeftStamp = true;
            showRightStamp = false;
          } else {
            // Phase 5 (80-100%): Right only
            showLeftStamp = false;
            showRightStamp = true;
          }
        }
        
        // Determine if this specific stamp should show with fade transitions
        let stampFadeAlpha = 0.0;
        
        // Calculate fade alpha for smooth transitions instead of hard cuts
        const fadeSpeed = 0.05; // How much of each phase is fade transition (5%)
        
        if (deg === 90) {
          // Left side - calculate fade alpha
          if (showLeftStamp) {
            stampFadeAlpha = 1.0;
          } else {
            stampFadeAlpha = 0.0;
          }
          
          // Add fade transitions at boundaries
          if (isFrameBasedRecording) {
            // For frame-based: add quick fades at phase transitions
            const frameCount = window.currentRecordingOptions?.frameCount || 0;
            if (frameCount > 60 && frameIndex !== null) {
              const cyclesPerGif = 2;
              const frameProgress = frameIndex / frameCount;
              const cyclePosition = (frameProgress * cyclesPerGif) % 1.0;
              
              // Quick fade in/out at transition points - longer fades for visibility
              if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
                // Fade in to "both on" phase (10% fade period)
                stampFadeAlpha = (cyclePosition - 0.15) / 0.10; // 0 to 1 over 10% of cycle
              } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
                // Fade out from "both on" phase (10% fade period)
                stampFadeAlpha = 1.0 - ((cyclePosition - 0.35) / 0.10); // 1 to 0 over 10% of cycle
              } else if (cyclePosition >= 0.55 && cyclePosition <= 0.65) {
                // Fade in to "left only" phase (10% fade period)
                stampFadeAlpha = (cyclePosition - 0.55) / 0.10; // 0 to 1 over 10% of cycle
              } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
                // Fade out from "left only" phase (10% fade period)
                stampFadeAlpha = 1.0 - ((cyclePosition - 0.75) / 0.10); // 1 to 0 over 10% of cycle
              }
            }
          } else {
            // Time-based recordings: add fade transitions (longer fades)
            const cyclesPerGif = 2;
            const cyclePosition = (progress * cyclesPerGif) % 1.0;
            
            if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
              stampFadeAlpha = (cyclePosition - 0.15) / 0.10;
            } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
              stampFadeAlpha = 1.0 - ((cyclePosition - 0.35) / 0.10);
            } else if (cyclePosition >= 0.55 && cyclePosition <= 0.65) {
              stampFadeAlpha = (cyclePosition - 0.55) / 0.10;
            } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
              stampFadeAlpha = 1.0 - ((cyclePosition - 0.75) / 0.10);
            }
          }
        } else if (deg === -90) {
          // Right side - calculate fade alpha
          if (showRightStamp) {
            stampFadeAlpha = 1.0;
          } else {
            stampFadeAlpha = 0.0;
          }
          
          // Add fade transitions at boundaries  
          if (isFrameBasedRecording) {
            const frameCount = window.currentRecordingOptions?.frameCount || 0;
            if (frameCount > 60 && frameIndex !== null) {
              const cyclesPerGif = 2;
              const frameProgress = frameIndex / frameCount;
              const cyclePosition = (frameProgress * cyclesPerGif) % 1.0;
              
              if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
                stampFadeAlpha = (cyclePosition - 0.15) / 0.10;
              } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
                stampFadeAlpha = 1.0 - ((cyclePosition - 0.35) / 0.10);
              } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
                stampFadeAlpha = (cyclePosition - 0.75) / 0.10;
              }
            }
          } else {
            const cyclesPerGif = 2;
            const cyclePosition = (progress * cyclesPerGif) % 1.0;
            
            if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
              stampFadeAlpha = (cyclePosition - 0.15) / 0.10;
            } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
              stampFadeAlpha = 1.0 - ((cyclePosition - 0.35) / 0.10);
            } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
              stampFadeAlpha = (cyclePosition - 0.75) / 0.10;
            }
          }
        }
        
        // Clamp alpha to valid range
        stampFadeAlpha = Math.max(0.0, Math.min(1.0, stampFadeAlpha));
        
        if (stampFadeAlpha <= 0.0) {
          return; // Skip drawing this stamp
        }

        ctx.save();
        ctx.translate(positionX, 0);
        ctx.rotate((deg * Math.PI) / 180); // Convert degrees to radians
        
        // Separate positioning for left and right sides for better spacing
        const leftYDist = 0.10; // Closer to corner (was 0.18)
        const rightYDist = 0.08; // Closer to corner, but still avoid Tezos overlap (was 0.15)

        // Use smaller size to match timestamp better  
        ctx.font = `${typeSize * 1.4}px YWFTProcessing-Regular`;
        
        // Always use Aesthetic.Computer text
        const text = "Aesthetic.Computer";
        const measured = ctx.measureText(text);
        const textWidth = measured.width;

        // Helper function to render text with multicolored shooting star dot
        function renderTextWithStarDot(ctx, fullText, x, y, baseColor, alpha, offsetX, offsetY, frameIndex) {
          const parts = fullText.split('.');
          if (parts.length !== 2) {
            // Fallback to normal rendering if not split correctly
            ctx.fillText(fullText, x + offsetX, y + offsetY);
            return;
          }
          
          const beforeDot = parts[0]; // "Aesthetic"
          const afterDot = parts[1];  // "Computer"
          
          // Measure parts
          const beforeWidth = ctx.measureText(beforeDot).width;
          const dotWidth = ctx.measureText('.').width;
          
          // Render "Aesthetic"
          ctx.fillStyle = baseColor;
          ctx.globalAlpha = alpha;
          ctx.fillText(beforeDot, x + offsetX, y + offsetY);
          
          // Render CRT-style piercing dot - bright, small, sharp like asteroids
          const dotX = x + beforeWidth + offsetX;
          const dotY = y + offsetY;
          
          // CRT cathode ray piercing colors - bright, sharp, high-contrast
          const crtColors = ["#FFFFFF", "#00FFFF", "#FFFF00", "#FF0000", "#00FF00", "#FF00FF"];
          let dotColor;
          let dotAlpha = alpha;
          
          if (frameIndex !== null) {
            // Frame-based CRT flicker animation - faster changes for piercing effect
            const colorIndex = Math.floor(frameIndex / 1) % crtColors.length; // Change color every frame
            dotColor = crtColors[colorIndex];
            
            // Sharp, bright pulse - no gentle curves, pure CRT intensity
            dotAlpha = alpha * (frameIndex % 2 === 0 ? 1.0 : 0.9); // Sharp flicker between 90% and 100%
          } else {
            // Time-based fallback - faster CRT-style changes
            const timeIndex = Math.floor(Date.now() / 100) % crtColors.length; // Change every 100ms
            dotColor = crtColors[timeIndex];
            dotAlpha = alpha * (Math.floor(Date.now() / 100) % 2 === 0 ? 1.0 : 0.9); // Sharp time-based flicker
          }
          
          ctx.fillStyle = dotColor;
          ctx.globalAlpha = dotAlpha;
          ctx.fillText('.', dotX, dotY);
          
          // Render "Computer"
          ctx.fillStyle = baseColor;
          ctx.globalAlpha = alpha;
          ctx.fillText(afterDot, dotX + dotWidth, dotY);
        }

        ["red", "lime", "blue", "white"].forEach((color) => {
          // Skip fade animations for frame-based recordings
          const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
          
          // Frame-based glitch effects for frame recordings
          let glitchAlpha = 1.0;
          if (isFrameBasedRecording && frameIndex !== null) {
            // Different glitch patterns for each color layer
            if (color === "red") {
              glitchAlpha = (frameIndex + 1) % 5 === 0 ? 0.2 : 1.0; // Dim every 5th frame
            } else if (color === "lime") {
              glitchAlpha = (frameIndex + 2) % 8 === 0 ? 0.0 : 1.0; // Off every 8th frame (offset)
            } else if (color === "blue") {
              glitchAlpha = (frameIndex + 3) % 6 === 0 ? 0.4 : 1.0; // Dim every 6th frame (offset)
            } else if (color === "white") {
              glitchAlpha = (frameIndex + 1) % 4 === 0 ? 0.6 : 1.0; // Dim every 4th frame
            }
          }
          
          let offsetX, offsetY;
          if (color !== "white") {
            ctx.globalAlpha = (isFrameBasedRecording ? 0.5 * glitchAlpha : 0.5) * stampFadeAlpha; // Restore multicolor balance
            offsetX = choose([-1, -2, 0, 1, 2]); // More offsets for multicolor flickering
            offsetY = choose([-1, -2, 0, 1, 2]); // More offsets for multicolor flickering
          } else {
            ctx.globalAlpha = (isFrameBasedRecording ? 0.85 * glitchAlpha : choose([0.75, 0.85, 0.95])) * stampFadeAlpha; // Brighter white for dot visibility
            offsetX = choose([-1, 0, 1]); // Some offsets for multicolor effect
            offsetY = choose([-1, 0, 1]); // Some offsets for multicolor effect
            color = choose(["white", "white", "white", "magenta", "yellow"]);
          }

          ctx.fillStyle = color;

          if (deg === 90) {
            // Left side - move lower (closer to bottom)
            renderTextWithStarDot(
              ctx,
              text,
              floor(canvasHeight * (1 - leftYDist) - textWidth),
              -floor(gap),
              color,
              ctx.globalAlpha,
              offsetX,
              offsetY,
              frameIndex
            );
          } else if (deg === -90) {
            // Right side - move higher (closer to top)
            renderTextWithStarDot(
              ctx,
              text,
              -Math.floor(canvasHeight * rightYDist + textWidth),
              Math.floor(-gap),
              color,
              ctx.globalAlpha,
              offsetX,
              offsetY,
              frameIndex
            );
          }
        });

        if (HANDLE) {
          ctx.font = `${typeSize * 1.3}px YWFTProcessing-Light`; // Smaller handle text (was 1.5)
          const handleWidth = textWidth / 2 + ctx.measureText(HANDLE).width / 2;
          const handleSpace = typeSize * 2.0; // More spacing (was 1.5)
          
          // Skip fade animations for frame-based recordings
          const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
          
          // Restore original 4-layer system with warmer color palette
          ["red", "lime", "blue", "white"].forEach((color) => {
            // Frame-based glitch effects for frame recordings
            let glitchAlpha = 1.0;
            if (isFrameBasedRecording && frameIndex !== null) {
              // Different glitch patterns for handle text (offset from main text)
              if (color === "red") {
                glitchAlpha = (frameIndex + 2) % 7 === 0 ? 0.1 : 1.0; // Off every 7th frame (offset)
              } else if (color === "lime") {
                glitchAlpha = (frameIndex + 3) % 5 === 0 ? 0.3 : 1.0; // Dim every 5th frame (offset)
              } else if (color === "blue") {
                glitchAlpha = (frameIndex + 4) % 9 === 0 ? 0.0 : 1.0; // Off every 9th frame (offset)
              } else if (color === "white") {
                glitchAlpha = (frameIndex + 2) % 3 === 0 ? 0.4 : 1.0; // Dim every 3rd frame (offset)
              }
            }
            
            let offsetX, offsetY;
            if (color !== "white") {
              ctx.globalAlpha = (isFrameBasedRecording ? 0.6 * glitchAlpha : 0.6) * stampFadeAlpha; // Less bright to keep multicolor look
              offsetX = choose([-1, -2, 0, 1, 2]); // More offsets for multicolor flickering
              offsetY = choose([-1, -2, 0, 1, 2]); // More offsets for multicolor flickering
            } else {
              ctx.globalAlpha = (isFrameBasedRecording ? 0.8 * glitchAlpha : choose([0.7, 0.8, 0.9])) * stampFadeAlpha; // Keep bright white layer
              offsetX = choose([-1, 0, 1]); // Some offsets for multicolor effect
              offsetY = choose([-1, 0, 1]); // Some offsets for multicolor effect
              color = choose(["white", "white", "white", "magenta", "yellow"]); // Original warmer palette
            }

            ctx.fillStyle = color;

            if (deg === 90) {
              // Handle - left side positioning (lower)
              ctx.fillText(
                HANDLE,
                floor(canvasHeight * (1 - leftYDist) - handleWidth + offsetY),
                -floor(offsetX + handleSpace + gap),
              );
            } else if (deg === -90) {
              // Handle - right side positioning (higher)
              ctx.fillText(
                HANDLE,
                -Math.floor(canvasHeight * rightYDist + handleWidth + offsetY),
                Math.floor(offsetX - handleSpace - gap),
              );
            }
          });
        }

        // Add Tezos stamp as a third element (only for frame-based recordings)
        // Remove the old timestamp code from here since it's now in addFilmTimestamp

        ctx.restore();
      }

      // Helper function to add Tezos support stamp - Vegas-style magical watermark
      function addTezosStamp(ctx, canvasWidth, canvasHeight, typeSize, isFrameBasedRecording, showLeftStamp = true, showRightStamp = true, frameIndex = null, fadeAlpha = 1.0) {
        ctx.save();
        
        // Show Tezos when either stamp is visible (hide only when both are off)
        if (!showLeftStamp && !showRightStamp) {
          ctx.restore();
          return;
        }
        
        // Much smaller watermark size with thinner font
        const billSize = Math.max(32, Math.floor(canvasHeight / 16)); // Bit smaller (was 44 and /11)
        ctx.font = `${billSize}px YWFTProcessing-Light, Arial, "Segoe UI Symbol", "Apple Color Emoji", sans-serif`;
        
        // Use the official Tezos symbol
        const tezosSymbol = "ꜩ"; // U+A729 (official Tezos symbol)
        
        const textWidth = ctx.measureText(tezosSymbol).width;
        
        // Position with more margin from edges
        const marginX = Math.max(12, Math.floor(canvasHeight / 70)); // More margin (was /100)
        const marginY = Math.max(15, Math.floor(canvasHeight / 60)); // More margin (was /80)
        const x = Math.floor(canvasWidth - textWidth - marginX); // Pixel boundary
        const y = Math.floor(marginY + billSize * 0.6); // More snug position (was 0.7), pixel boundary
        
        // Gentler shake animation for tiny logo
        const billFrameSeed = Math.floor(Date.now() / 400); // Slower, calmer (was 200ms)
        const billShakeX = Math.floor((billFrameSeed % 3) - 1); // Range -1 to 1, much gentler (was -3 to 3)
        const billShakeY = Math.floor(((billFrameSeed + 4) % 3) - 1); // Range -1 to 1, much gentler (was -3 to 3)
        
        // Vegas-style magical color cycling
        const colorCycle = Math.floor(Date.now() / 150) % 6; // Fast color cycling
        let magicColors;
        
        switch (colorCycle) {
          case 0: magicColors = ["gold", "yellow", "white"]; break;
          case 1: magicColors = ["cyan", "blue", "white"]; break;
          case 2: magicColors = ["magenta", "red", "white"]; break;
          case 3: magicColors = ["lime", "green", "white"]; break;
          case 4: magicColors = ["orange", "red", "yellow"]; break;
          case 5: magicColors = ["purple", "blue", "cyan"]; break;
        }
        
        // Draw with chaotic magical effects for maximum energy
        magicColors.forEach((color, index) => {
          let offsetX, offsetY;
          
          // Frame-based glitch effects for frame recordings
          let glitchAlpha = 1.0;
          if (isFrameBasedRecording && frameIndex !== null) {
            // Different glitch patterns for Tezos watermark layers
            if (index === 0) {
              glitchAlpha = (frameIndex + 5) % 13 === 0 ? 0.0 : (frameIndex + 5) % 4 === 0 ? 0.2 : 1.0; // Off every 13th, dim every 4th
            } else if (index === 1) {
              glitchAlpha = (frameIndex + 6) % 11 === 0 ? 0.1 : (frameIndex + 6) % 6 === 0 ? 0.4 : 1.0; // Different pattern
            } else {
              glitchAlpha = (frameIndex + 7) % 7 === 0 ? 0.3 : (frameIndex + 7) % 15 === 0 ? 0.0 : 1.0; // Complex pattern
            }
          }
          
          // Set watermark blend mode for authentic look
          ctx.globalCompositeOperation = index === 0 ? 'multiply' : index === 1 ? 'screen' : 'overlay';
          
          if (index < 2) {
            ctx.globalAlpha = (isFrameBasedRecording ? 0.35 * glitchAlpha : 0.35) * fadeAlpha; // Apply fade alpha
            offsetX = Math.floor(choose([-1, 0, 1])); // Much gentler offsets (was chaotic -4 to 5)
            offsetY = Math.floor(choose([-1, 0, 1])); // Much gentler offsets (was chaotic -4 to 5)
          } else {
            ctx.globalAlpha = (isFrameBasedRecording ? 0.45 * glitchAlpha : 0.45) * fadeAlpha; // Apply fade alpha
            offsetX = Math.floor(choose([0, 1])); // Minimal offsets for readability (was -3 to 3)
            offsetY = Math.floor(choose([0, 1])); // Minimal offsets for readability (was -3 to 3)
            // Keep white layer more magical
            if (color === "white") {
              color = choose(["white", "white", "yellow", "cyan", "magenta", "lime"]); // More magical colors
            }
          }
          
          ctx.fillStyle = color;
          ctx.fillText(
            tezosSymbol, 
            x + offsetX + billShakeX, 
            y + offsetY + billShakeY
          );
          
          // Reset blend mode after each layer
          ctx.globalCompositeOperation = 'source-over';
        });
        
        ctx.restore();
      }

      // Helper function to get syntax highlighting colors from kidlisp module
      function getSyntaxHighlightColors() {
        // Get cached kidlisp source code
        const cachedCode = window.currentRecordingOptions?.cachedCode;
        if (!cachedCode) {
          // Fallback to default red if no code available
          return [{ type: "default", r: 220, g: 60, b: 60, weight: 1.0 }];
        }
        
        // Use the centralized syntax highlighting from kidlisp module
        const colors = getSyntaxHighlightingColors(cachedCode);
        
        // Debug logging to show we're using kidlisp syntax highlighting
        console.log("🎨 Using kidlisp syntax highlighting:", {
          sourceLength: cachedCode.length,
          tokenCount: colors.length,
          colors: colors.slice(0, 5) // Show first 5 colors
        });
        
        return colors;
      }
      
      // Helper function to draw cyan crosshair cursor overlay
      function addCyanCrosshair(ctx, canvasWidth, canvasHeight, penData, scale = 1) {
        if (!penData || penData.x === undefined || penData.y === undefined) {
          return; // No pen data available
        }
        
        // Scale pen coordinates to match canvas size
        const x = Math.round(penData.x * scale);
        const y = Math.round(penData.y * scale);
        
        // Skip drawing if cursor is outside canvas bounds
        if (x < 0 || x >= canvasWidth || y < 0 || y >= canvasHeight) {
          return;
        }
        
        ctx.save();
        
        // Cyan crosshair settings
        const crosshairColor = "rgba(0, 255, 255, 0.8)"; // Bright cyan with slight transparency
        const shadowColor = "rgba(0, 0, 0, 0.5)"; // Black shadow for visibility
        const lineWidth = 2;
        const shadowOffset = 1;
        const crossSize = Math.max(8, Math.floor(Math.min(canvasWidth, canvasHeight) / 60)); // Responsive size
        
        // Draw shadow first
        ctx.strokeStyle = shadowColor;
        ctx.lineWidth = lineWidth + shadowOffset;
        ctx.lineCap = "round";
        
        ctx.beginPath();
        // Horizontal line shadow
        ctx.moveTo(x - crossSize + shadowOffset, y + shadowOffset);
        ctx.lineTo(x + crossSize + shadowOffset, y + shadowOffset);
        // Vertical line shadow
        ctx.moveTo(x + shadowOffset, y - crossSize + shadowOffset);
        ctx.lineTo(x + shadowOffset, y + crossSize + shadowOffset);
        ctx.stroke();
        
        // Draw main crosshair
        ctx.strokeStyle = crosshairColor;
        ctx.lineWidth = lineWidth;
        
        ctx.beginPath();
        // Horizontal line
        ctx.moveTo(x - crossSize, y);
        ctx.lineTo(x + crossSize, y);
        // Vertical line
        ctx.moveTo(x, y - crossSize);
        ctx.lineTo(x, y + crossSize);
        ctx.stroke();
        
        ctx.restore();
      }

      // Helper function to get syntax color at a specific position in the progress bar
      function getSyntaxColorAtPosition(syntaxColors, position) {
        if (syntaxColors.length === 0) {
          return { r: 220, g: 60, b: 60 }; // Red fallback
        }

        if (syntaxColors.length === 1) {
          return syntaxColors[0];
        }

        // Calculate which color segment we're in based on cumulative weights
        let totalWeight = syntaxColors.reduce(
          (sum, color) => sum + color.weight,
          0,
        );
        let currentWeight = 0;

        for (let i = 0; i < syntaxColors.length; i++) {
          const color = syntaxColors[i];
          const segmentStart = currentWeight / totalWeight;
          const segmentEnd = (currentWeight + color.weight) / totalWeight;

          if (position >= segmentStart && position <= segmentEnd) {
            // Optionally blend with adjacent colors for smoother transitions
            if (i < syntaxColors.length - 1 && position > segmentEnd - 0.1) {
              const nextColor = syntaxColors[i + 1];
              const blendFactor = (position - (segmentEnd - 0.1)) / 0.1;
              return {
                r: Math.floor(
                  color.r * (1 - blendFactor) + nextColor.r * blendFactor,
                ),
                g: Math.floor(
                  color.g * (1 - blendFactor) + nextColor.g * blendFactor,
                ),
                b: Math.floor(
                  color.b * (1 - blendFactor) + nextColor.b * blendFactor,
                ),
              };
            }
            return color;
          }

          currentWeight += color.weight;
        }

        // Fallback to last color if position >is beyond the end
        return syntaxColors[syntaxColors.length - 1];
      }
      
      // Helper function to draw cyan crosshair cursor overlay
      function addCyanCrosshair(ctx, canvasWidth, canvasHeight, penData, scale = 1) {
        if (!penData || penData.x === undefined || penData.y === undefined) {
          return; // No pen data available
        }
        
        // Scale pen coordinates to match canvas size
        const x = Math.round(penData.x * scale);
        const y = Math.round(penData.y * scale);
        
        // Skip drawing if cursor is outside canvas bounds
        if (x < 0 || x >= canvasWidth || y < 0 || y >= canvasHeight) {
          return;
        }
        
        ctx.save();
        
        // Cyan crosshair settings
        const crosshairColor = "rgba(0, 255, 255, 0.8)"; // Bright cyan with slight transparency
        const shadowColor = "rgba(0, 0, 0, 0.5)"; // Black shadow for visibility
        const lineWidth = 2;
        const shadowOffset = 1;
        const crossSize = Math.max(8, Math.floor(Math.min(canvasWidth, canvasHeight) / 60)); // Responsive size
        
        // Draw shadow first
        ctx.strokeStyle = shadowColor;
        ctx.lineWidth = lineWidth + shadowOffset;
        ctx.lineCap = "round";
        
        ctx.beginPath();
        // Horizontal line shadow
        ctx.moveTo(x - crossSize + shadowOffset, y + shadowOffset);
        ctx.lineTo(x + crossSize + shadowOffset, y + shadowOffset);
        // Vertical line shadow
        ctx.moveTo(x + shadowOffset, y - crossSize + shadowOffset);
        ctx.lineTo(x + shadowOffset, y + crossSize + shadowOffset);
        ctx.stroke();
        
        // Draw main crosshair
        ctx.strokeStyle = crosshairColor;
        ctx.lineWidth = lineWidth;
        
        ctx.beginPath();
        // Horizontal line
        ctx.moveTo(x - crossSize, y);
        ctx.lineTo(x + crossSize, y);
        // Vertical line
        ctx.moveTo(x, y - crossSize);
        ctx.lineTo(x, y + crossSize);
        ctx.stroke();
        
        ctx.restore();
      }

      // Film camera style timestamp at bottom-left corner
      function addFilmTimestamp(ctx, canvasWidth, canvasHeight, typeSize, progress = 0, frameData = null, frameMetadata = null, frameIndex = null, totalFrames = null, bothStampsInvisible = false) {
        // Calculate smooth alpha fade based on progress - same for both frame and time recordings
        let timestampAlpha = 1.0; // Default to fully visible
        
        // Smooth fade transitions: visible at start, fade out towards middle, visible at end
        if (progress < 0.15) {
          // Fully visible at start (0-15%)
          timestampAlpha = 1.0;
        } else if (progress >= 0.15 && progress <= 0.35) {
          // Fade out from 15% to 35% (20% fade-out period)
          const fadeProgress = (progress - 0.15) / 0.20;
          timestampAlpha = 1.0 - Math.pow(fadeProgress, 2); // Smooth quadratic fade out
        } else if (progress > 0.35 && progress < 0.65) {
          // Fully hidden from 35% to 65% (30% hidden period)
          timestampAlpha = 0.0;
        } else if (progress >= 0.65 && progress <= 0.85) {
          // Fade in from 65% to 85% (20% fade-in period)
          const fadeProgress = (progress - 0.65) / 0.20;
          timestampAlpha = Math.pow(fadeProgress, 2); // Smooth quadratic fade in
        } else {
          // Fully visible at end (85%-100%)
          timestampAlpha = 1.0;
        }
        
        // Skip drawing timecode if it's completely transparent
        if (timestampAlpha <= 0.0) {
          return;
        }
        
        // Define progress bar height consistently across all contexts
        const progressBarHeight = 1;
        
        // Define timestamp margin for consistent spacing
        const timestampMargin = Math.max(8, Math.floor(canvasHeight / 50)); // Responsive margin
        
        // NEW: Use actual real-world timestamp when this frame was originally captured
        let timestamp;
        if (frameMetadata && frameMetadata.originalTimestamp) {
          // Display the actual time when this frame was captured
          const frameDate = new Date(frameMetadata.originalTimestamp);
          const year = frameDate.getFullYear();
          const month = frameDate.getMonth() + 1; // getMonth() returns 0-11
          const day = frameDate.getDate();
          const hour = frameDate.getHours();
          const minute = frameDate.getMinutes();
          const second = frameDate.getSeconds();
          const millisecond = frameDate.getMilliseconds();
          
          // Format as: year.month.day.hour.minute.second.milliseconds (no zero-padding except milliseconds)
          timestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
        } else if (frameMetadata && frameMetadata.gifElapsedSeconds !== undefined) {
          // For GIF playback: use actual GIF elapsed time
          timestamp = `${frameMetadata.gifElapsedSeconds.toFixed(1)}s`;
        } else if (mediaRecorderStartTime !== undefined) {
          // During recording: show seconds elapsed since recording started
          const elapsedMs = performance.now() - mediaRecorderStartTime;
          const elapsedSeconds = elapsedMs / 1000;
          timestamp = `${elapsedSeconds.toFixed(1)}s`;
        } else if (window.recordingStartTimestamp && progress >= 0) {
          // Use progress to map to intended recording duration
          let actualRecordingDurationMs = 5000; // Default fallback
          
          // Prioritize intended duration for accurate real-time mapping
          if (window.currentRecordingOptions?.intendedDuration) {
            actualRecordingDurationMs = window.currentRecordingOptions.intendedDuration * 1000;
          } else if (mediaRecorderDuration && mediaRecorderDuration > 0) {
            actualRecordingDurationMs = mediaRecorderDuration;
          } else if (window.gifDurationMs && window.gifDurationMs > 0) {
            actualRecordingDurationMs = window.gifDurationMs;
          }
          
          // Calculate elapsed seconds
          const elapsedSeconds = (progress * actualRecordingDurationMs) / 1000;
          timestamp = `${elapsedSeconds.toFixed(1)}s`;
        } else {
          // Fallback: show current time
          const now = new Date();
          const year = now.getFullYear();
          const month = now.getMonth() + 1;
          const day = now.getDate();
          const hour = now.getHours();
          const minute = now.getMinutes();
          const second = now.getSeconds();
          const millisecond = now.getMilliseconds();
          
          timestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
        }

        ctx.save();
        
        // Remove shake/jitter effect - keep timestamp steady
        const shakeX = 0; // No more shaking
        const shakeY = 0; // No more shaking
        
        // Position timestamp above the progress bar with proper margin, more flush left and larger
        // Adjust margin to make room for Tezos logo if enabled
        let adjustedMargin = timestampMargin * 0.5;
        if (window.currentRecordingOptions?.showTezosStamp) {
          const tezosSize = Math.max(20, Math.floor(typeSize * 1.5 * 1.2));
          adjustedMargin = timestampMargin * 1.8; // Less space since we moved Tezos closer
        }
        const timestampY = Math.floor(canvasHeight - progressBarHeight - timestampMargin * 0.6 - (typeSize * 0.2) - 4); // Move UP 4px more
        const timestampX = Math.floor(typeSize * 0.5); // Move back right a bit (was 0.3)
        
        // Make timestamp smaller and more readable
        const baseWatermarkSize = Math.floor(typeSize * 1.7); // Smaller timestamp (was 2.0)
        // Keep timestamp same size for both frame and time recordings
        const watermarkSize = baseWatermarkSize;
        
        // Draw multiple layered versions for that vibey aesthetic.computer stamp effect
        ["red", "lime", "blue", "white"].forEach((color, index) => {
          let offsetX, offsetY;
          if (color !== "white") {
            ctx.globalAlpha = 0.45 * timestampAlpha; // Simple multicolor layers
            offsetX = choose([-3, -5, 0, 3, 5]); // Larger offsets for bigger text
            offsetY = choose([-3, -5, 0, 3, 5]);
          } else {
            ctx.globalAlpha = 0.65 * timestampAlpha; // Consistent white layer
            offsetX = choose([-1, 0, 1]);
            offsetY = choose([-1, 0, 1]);
            color = "white"; // Consistent white color, no blinking
          }

          ctx.fillStyle = color;
          ctx.font = `bold ${watermarkSize}px YWFTProcessing-Regular`;
          
          ctx.fillText(
            timestamp,
            timestampX + offsetX + shakeX,
            timestampY + offsetY + shakeY,
          );
        });
        
        // Removed Tezos logo from timestamp area - moving back to top-right

        ctx.restore();
      }

      // Helper function to sample representative colors from frame data
      function sampleFrameColors(frameData, canvasWidth, canvasHeight, numColors = 5) {
        if (!frameData || frameData.length < 4) {
          // Fallback to original colors if no frame data
          return ["#FF4444", "#FF6666", "#FFAA44", "#FF8888", "#FFCCCC"];
        }

        const sampledColors = [];
        const totalPixels = canvasWidth * canvasHeight;
        
        // Sample colors from different regions of the frame
        let samplePositions;
        if (numColors <= 5) {
          samplePositions = [0.1, 0.3, 0.5, 0.7, 0.9]; // Original 5 positions
        } else {
          // Generate more evenly distributed positions for higher numColors
          samplePositions = [];
          for (let i = 0; i < numColors; i++) {
            samplePositions.push((i + 1) / (numColors + 1)); // Evenly distributed from ~0.1 to ~0.9
          }
        }

        for (let i = 0; i < numColors; i++) {
          const samplePos = Math.floor(samplePositions[i % samplePositions.length] * totalPixels);
          const pixelIndex = Math.min(samplePos * 4, frameData.length - 4);
          
          const r = frameData[pixelIndex];
          const g = frameData[pixelIndex + 1];
          const b = frameData[pixelIndex + 2];
          
          // Enhance the colors to make them more vibrant for the progress bar
          const enhancedR = Math.min(255, Math.floor(r * 1.2));
          const enhancedG = Math.min(255, Math.floor(g * 1.2));
          const enhancedB = Math.min(255, Math.floor(b * 1.2));
          
          const hexColor = `#${enhancedR.toString(16).padStart(2, '0')}${enhancedG.toString(16).padStart(2, '0')}${enhancedB.toString(16).padStart(2, '0')}`;
          sampledColors.push(hexColor);
        }

        // If all sampled colors are too dark, add some brightness
        const avgBrightness = sampledColors.reduce((sum, color) => {
          const r = parseInt(color.slice(1, 3), 16);
          const g = parseInt(color.slice(3, 5), 16);
          const b = parseInt(color.slice(5, 7), 16);
          return sum + (r + g + b) / 3;
        }, 0) / sampledColors.length;

        if (avgBrightness < 60) {
          // Mix in some brighter colors if the frame is too dark
          return sampledColors.map((color, i) => {
            if (i % 2 === 0) {
              return choose(["#FF6666", "#FFAA44", "#FF8888", "#FFCCCC", "#66FF66", "#6666FF"]);
            }
            return color;
          });
        }

        return sampledColors;
      }

      // Helper function for choosing random values (simplified version)
      function choose(options) {
        return options[Math.floor(Math.random() * options.length)];
      }
    }

    // Helper function to upscale pixels with nearest neighbor
    function upscalePixels(imageData, originalWidth, originalHeight, scale) {
      if (scale === 1) {
        return imageData;
      }

      const scaledWidth = originalWidth * scale;
      const scaledHeight = originalHeight * scale;
      const scaledImageData = new Uint8ClampedArray(
        scaledWidth * scaledHeight * 4,
      );

      for (let y = 0; y < scaledHeight; y++) {
        for (let x = 0; x < scaledWidth; x++) {
          const sourceX = Math.floor(x / scale);
          const sourceY = Math.floor(y / scale);
          const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
          const targetIndex = (y * scaledWidth + x) * 4;

          scaledImageData[targetIndex] = imageData[sourceIndex]; // R
          scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
          scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
          scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
        }
      }

      return scaledImageData;
    }

    // Helper function to add stamp to pixel data by rendering to a canvas first
    async function addStampToPixelData(pixelData, width, height, scale = 1, progress = 0, frameMetadata = null) {
      // Create a temporary canvas to render the stamp
      const tempCanvas = document.createElement("canvas");
      const tempCtx = tempCanvas.getContext("2d");

      tempCanvas.width = width * scale;
      tempCanvas.height = height * scale;

      // Put the original image data
      const scaledData = upscalePixels(pixelData, width, height, scale);
      const imageData = new ImageData(
        scaledData,
        width * scale,
        height * scale,
      );
      tempCtx.putImageData(imageData, 0, 0);

      // Add the stamp (await to ensure fonts are loaded)
      await addAestheticComputerStamp(tempCtx, width * scale, height * scale, progress, pixelData, frameMetadata);

      // Get the stamped image data back
      const stampedImageData = tempCtx.getImageData(
        0,
        0,
        width * scale,
        height * scale,
      );
      return stampedImageData.data;
    }

    // 🎬 Create animated WebP from frame data
    if (type === "create-animated-webp") {
      console.log(
        "📼 Creating animated WebP from",
        content.frames.length,
        "frames",
      );

      // Send initial progress to show the progress bar immediately
      send({
        type: "recorder:transcode-progress",
        content: 0.01, // 1% to start
      });

      try {
        // Create a canvas for frame processing
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");

        if (content.frames.length === 0) {
          console.warn("No frames provided for WebP creation");
          return;
        }

        // Set canvas size based on first frame
        const firstFrame = content.frames[0];
        canvas.width = firstFrame.width;
        canvas.height = firstFrame.height;
        console.log(`🖼️ Canvas size: ${canvas.width}x${canvas.height}`);

        // For now, create a zip of WebP frames with timing info
        // TODO: Implement proper animated WebP encoding when browser support improves
        if (!window.JSZip) await loadJSZip();
        const zip = new window.JSZip();

        // Add timing info
        const timingInfo = content.frames.map((frame, i) => ({
          frame: i,
          duration: frame.duration,
          timestamp: frame.timestamp,
        }));

        zip.file("timing.json", JSON.stringify(timingInfo, null, 2));
        console.log(
          "📋 Added timing.json with",
          timingInfo.length,
          "frame entries",
        );

        // Convert each frame to WebP and add to zip
        for (let i = 0; i < content.frames.length; i++) {
          const frame = content.frames[i];

          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          // Create ImageData from frame data
          const imageData = new ImageData(
            new Uint8ClampedArray(frame.data),
            frame.width,
            frame.height,
          );

          // Draw to canvas and convert to WebP
          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp like in video recordings (await to ensure fonts are loaded)
          const currentProgress = (i + 1) / content.frames.length;
          await addAestheticComputerStamp(ctx, frame.width, frame.height, currentProgress, frame.data, frame, i, content.frames.length);

          // Convert to WebP blob
          const webpBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/webp", 0.8);
          });

          // Add to zip
          zip.file(`frame_${i.toString().padStart(4, "0")}.webp`, webpBlob);
        }

        console.log("📦 Generating ZIP file...");

        // Generate and download the zip
        const zipBlob = await zip.generateAsync({ type: "blob" });
        const filename = generateTapeFilename("zip", "-webp");

        console.log(
          `💾 ZIP generated: ${filename} (${Math.round((zipBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: zipBlob });

        console.log("🎬 WebP frames exported successfully as ZIP");
      } catch (error) {
        console.error("Error creating animated WebP:", error);
      }
      return;
    }

    // 🎬 Create single animated WebP file
    if (type === "create-single-animated-webp") {
      console.log(
        "🎞️ Creating animated image from",
        content.frames.length,
        "frames",
      );

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for animated image creation");
          return;
        }

        // Try WebP animation first using webpxmux.js
        console.log("🔄 Creating animated WebP using webpxmux.js library");

        // Load webpxmux.js if not already loaded
        if (!window.WebPXMux) {
          console.log("📦 Loading webpxmux.js library...");
          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src =
              "https://cdn.jsdelivr.net/npm/webpxmux@1.0.2/dist/webpxmux.min.js";
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
          });
        }

        console.log(
          `🖼️ Canvas size: ${content.frames[0].width}x${content.frames[0].height}`,
        );

        // Initialize WebPXMux
        const xMux = window.WebPXMux(
          "https://cdn.jsdelivr.net/npm/webpxmux@1.0.2/dist/webpxmux.wasm",
        );
        await xMux.waitRuntime();

        // Convert our frame data to WebPXMux format
        const frames = [];

        // Create a canvas for rendering stamps
        const tempCanvas = document.createElement("canvas");
        const tempCtx = tempCanvas.getContext("2d");
        tempCanvas.width = content.frames[0].width;
        tempCanvas.height = content.frames[0].height;

        for (let i = 0; i < content.frames.length; i++) {
          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          const frame = content.frames[i];

          // Create ImageData from frame data and draw to canvas
          const imageData = new ImageData(
            new Uint8ClampedArray(frame.data),
            frame.width,
            frame.height,
          );
          tempCtx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp like in other video recordings
          const currentProgress = (i + 1) / content.frames.length;
          await addAestheticComputerStamp(tempCtx, frame.width, frame.height, currentProgress, frame.data, frame);

          // Get the stamped image data back
          const stampedImageData = tempCtx.getImageData(
            0,
            0,
            frame.width,
            frame.height,
          );
          const stampedData = stampedImageData.data;

          // Convert frame data to RGBA Uint32Array format expected by webpxmux
          const rgba = new Uint32Array(frame.width * frame.height);

          for (let j = 0; j < rgba.length; j++) {
            const pixelIndex = j * 4;
            const r = stampedData[pixelIndex];
            const g = stampedData[pixelIndex + 1];
            const b = stampedData[pixelIndex + 2];
            const a = stampedData[pixelIndex + 3];

            // Pack RGBA into 32-bit integer (0xRRGGBBAA format)
            rgba[j] = (r << 24) | (g << 16) | (b << 8) | a;
          }

          frames.push({
            duration: Math.max(frame.duration || 100, 10),
            isKeyframe: i === 0, // First frame is keyframe
            rgba: rgba,
          });
        }

        console.log("🔄 Encoding animated WebP...");

        const webpFrames = {
          frameCount: frames.length,
          width: content.frames[0].width,
          height: content.frames[0].height,
          loopCount: 0, // Infinite loop
          bgColor: 0xffffffff, // White background
          frames: frames,
        };

        const webpData = await xMux.encodeFrames(webpFrames);

        const webpBlob = new Blob([webpData], { type: "image/webp" });
        const filename = generateTapeFilename("webp", "-animated");

        console.log(
          `💾 Animated WebP generated: ${filename} (${Math.round((webpBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: webpBlob });

        console.log("🎬 Animated WebP exported successfully!");
      } catch (error) {
        console.error("Error creating animated WebP:", error);
        console.log("🔄 Falling back to animated PNG (APNG)");

        try {
          // Fallback to APNG using UPNG.js
          console.log("🔄 Creating animated PNG (APNG) - fallback option");

          // Load pako (required for UPNG.js compression) and UPNG.js library
          if (!window.pako) {
            console.log("📦 Loading pako compression library...");
            await new Promise((resolve, reject) => {
              const script = document.createElement("script");
              script.src =
                "https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js";
              script.onload = resolve;
              script.onerror = reject;
              document.head.appendChild(script);
            });
          }

          if (!window.UPNG) {
            console.log("📦 Loading UPNG.js library...");
            await new Promise((resolve, reject) => {
              const script = document.createElement("script");
              script.src = "https://cdn.jsdelivr.net/npm/upng-js@2.1.0/UPNG.js";
              script.onload = resolve;
              script.onerror = reject;
              document.head.appendChild(script);
            });
          }

          console.log(
            `🖼️ Canvas size: ${content.frames[0].width}x${content.frames[0].height}`,
          );

          // Convert frames to the format UPNG expects
          const frameBuffers = [];
          const delays = [];

          for (let i = 0; i < content.frames.length; i++) {
            const frame = content.frames[i];

            if (i % 50 === 0 || i === content.frames.length - 1) {
              console.log(
                `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
              );
            }

            // UPNG expects RGBA data as ArrayBuffer
            const frameData = new Uint8Array(frame.data);
            frameBuffers.push(frameData.buffer);

            // Convert to milliseconds for APNG delays
            delays.push(Math.max(frame.duration || 100, 10));
          }

          console.log("🔄 Encoding animated PNG...");

          // Create animated PNG using UPNG
          const apngBuffer = window.UPNG.encode(
            frameBuffers,
            content.frames[0].width,
            content.frames[0].height,
            0, // 0 = lossless, or use 256 for lossy
            delays,
          );

          const apngBlob = new Blob([apngBuffer], { type: "image/png" });
          const filename = generateTapeFilename("png", "-animated");

          console.log(
            `💾 Animated PNG generated: ${filename} (${Math.round((apngBlob.size / 1024 / 1024) * 100) / 100} MB)`,
          );

          // Use the existing download function
          receivedDownload({ filename, data: apngBlob });

          console.log("🎬 Animated PNG (APNG) exported successfully!");
        } catch (apngError) {
          console.error("Error creating animated PNG:", apngError);

          // Final fallback: create a static WebP of the first frame
          console.log("🔄 Falling back to static WebP of first frame");
          try {
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d");
            const firstFrame = content.frames[0];

            canvas.width = firstFrame.width;
            canvas.height = firstFrame.height;

            const imageData = new ImageData(
              new Uint8ClampedArray(firstFrame.data),
              firstFrame.width,
              firstFrame.height,
            );

            ctx.putImageData(imageData, 0, 0);

            const webpBlob = await new Promise((resolve) => {
              canvas.toBlob(resolve, "image/webp", 0.9);
            });

            const filename = generateTapeFilename("webp", "-static");
            receivedDownload({ filename, data: webpBlob });

            console.log("📸 Static WebP fallback exported successfully");
          } catch (fallbackError) {
            console.error("Error in fallback WebP creation:", fallbackError);
          }
        }
      }
      return;
    }

    // 🎬 Handle WebP frame chunks
    if (type === "create-animated-webp-only-chunk") {
      if (window.webpFrameChunks) {
        window.webpFrameChunks.push(content.frames);
        window.webpReceivedChunks++;
        
        console.log(
          `🎞️ Received WebP chunk ${window.webpReceivedChunks}/${window.webpTotalChunks}:`,
          content.frames.length,
          "frames"
        );

        if (window.webpReceivedChunks === window.webpTotalChunks) {
          console.log("🎞️ All WebP chunks received, reassembling...");
          const allFrames = window.webpFrameChunks.flat();
          console.log("🎞️ Total frames after reassembly:", allFrames.length);

          delete window.webpFrameChunks;
          delete window.webpTotalChunks;
          delete window.webpReceivedChunks;

          receivedChange({
            data: {
              type: "create-animated-webp-only",
              content: { frames: allFrames }
            }
          });
        }
      }
      return;
    }

    // 🎬 Create animated WebP only (no fallback to APNG)
    if (type === "create-animated-webp-only") {
      if (content.totalChunks && content.totalChunks > 1) {
        console.log(
          `🎞️ Receiving WebP frames in chunks (1/${content.totalChunks}):`,
          content.frames.length,
          "frames"
        );
        window.webpFrameChunks = [content.frames];
        window.webpTotalChunks = content.totalChunks;
        window.webpReceivedChunks = 1;
        return;
      }

      console.log(
        "🎞️ Creating animated WebP from",
        content.frames.length,
        "frames",
      );

      // Helper function to upscale pixels with nearest neighbor - with memory safety
      function upscalePixels(rgba, originalWidth, originalHeight, scale) {
        if (scale === 1) {
          return { rgba: rgba, actualScale: 1 };
        }

        const scaledWidth = originalWidth * scale;
        const scaledHeight = originalHeight * scale;
        const totalPixels = scaledWidth * scaledHeight;

        // Safety check to prevent array allocation errors
        const maxSafeArraySize = 100 * 1024 * 1024; // 100M elements max
        if (totalPixels > maxSafeArraySize) {
          console.warn(
            `⚠️ Scaled array too large (${totalPixels} pixels), falling back to 1x scaling`,
          );
          return { rgba: rgba, actualScale: 1 };
        }

        let scaledRgba;
        try {
          scaledRgba = new Uint32Array(totalPixels);
        } catch (error) {
          console.warn(
            `⚠️ Failed to allocate scaled array: ${error.message}, falling back to 1x scaling`,
          );
          return { rgba: rgba, actualScale: 1 };
        }

        for (let y = 0; y < scaledHeight; y++) {
          for (let x = 0; x < scaledWidth; x++) {
            const sourceX = Math.floor(x / scale);
            const sourceY = Math.floor(y / scale);
            const sourceIndex = sourceY * originalWidth + sourceX;
            const targetIndex = y * scaledWidth + x;
            scaledRgba[targetIndex] = rgba[sourceIndex];
          }
        }

        return { rgba: scaledRgba, actualScale: scale };
      }

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for animated WebP creation");
          return;
        }

        console.log("🔄 Creating animated WebP using webpxmux.js library");

        // Load webpxmux.js using local dep files
        if (!window.WebPXMux) {
          console.log("📦 Loading webpxmux.js library...");
          console.log(
            "📍 Script URL: /aesthetic.computer/dep/webpxmux/webpxmux.min.js",
          );

          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src = "/aesthetic.computer/dep/webpxmux/webpxmux.min.js";
            script.type = "text/javascript";

            script.onload = () => {
              console.log("✅ webpxmux.js script loaded successfully");
              console.log(
                "🔍 Checking window.WebPXMux:",
                typeof window.WebPXMux,
              );
              if (window.WebPXMux) {
                console.log("✅ WebPXMux constructor found on window");
              } else {
                console.error(
                  "❌ WebPXMux not found on window after script load",
                );
              }
              resolve();
            };

            script.onerror = (error) => {
              console.error("❌ Failed to load webpxmux.js script");
              console.error("📍 Error event:", error);
              console.error("📍 Script src:", script.src);
              console.error("📍 Script type:", script.type);
              reject(error);
            };

            console.log("📎 Appending script to document head");
            document.head.appendChild(script);
          });
        } else {
          console.log("✅ webpxmux.js already loaded");
        }

        console.log(
          `🖼️ Canvas size: ${content.frames[0].width}x${content.frames[0].height} -> checking memory requirements...`,
        );

        // Pre-calculate optimal scale to avoid memory issues
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const frameCount = content.frames.length;

        // Ultra-conservative memory management for WebP animations
        // Each frame needs significant memory overhead beyond just pixel data
        const baseMemoryLimit = 5 * 1024 * 1024; // 5M pixels base limit (much smaller)

        // Much more aggressive scaling penalties for large frame counts
        let framePenalty = 1;
        if (frameCount > 500)
          framePenalty = 16; // 500+ frames: divide by 16
        else if (frameCount > 300)
          framePenalty = 12; // 300+ frames: divide by 12
        else if (frameCount > 200)
          framePenalty = 8; // 200+ frames: divide by 8
        else if (frameCount > 100)
          framePenalty = 6; // 100+ frames: divide by 6
        else if (frameCount > 50)
          framePenalty = 4; // 50+ frames: divide by 4
        else if (frameCount > 25) framePenalty = 2; // 25+ frames: divide by 2

        const adjustedMemoryLimit = Math.max(
          baseMemoryLimit / framePenalty,
          256 * 1024, // Minimum 256K pixels (very small for safety)
        );

        // Start with smaller default scaling
        let optimalScale = frameCount > 400 ? 1 : 2; // Default to 2x for more animations, only 1x for very large ones
        let scaledPixels =
          originalWidth * originalHeight * optimalScale * optimalScale;

        console.log(
          `🧮 Conservative memory calculation: ${frameCount} frames, penalty: ${framePenalty}x, ${scaledPixels} pixels per frame (${optimalScale}x), limit: ${Math.round(adjustedMemoryLimit / 1024)}K pixels`,
        );

        // More aggressive downscaling if needed
        if (scaledPixels > adjustedMemoryLimit) {
          optimalScale = 1;
          scaledPixels = originalWidth * originalHeight;
          console.warn(
            `⚠️ Forcing 1x scaling for ${frameCount} frames to prevent memory errors`,
          );

          // If even 1x is too large, we have a problem
          if (scaledPixels > adjustedMemoryLimit) {
            console.error(
              `❌ Video too large even at 1x scaling: ${Math.round(scaledPixels / 1024)}K pixels > ${Math.round(adjustedMemoryLimit / 1024)}K limit`,
            );
            throw new Error(
              `Video dimensions too large for WebP encoding: ${originalWidth}x${originalHeight} with ${frameCount} frames`,
            );
          }
        }

        console.log(
          `📏 Using ${optimalScale}x scaling: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Initialize WebPXMux
        console.log("🔧 Initializing WebPXMux...");
        console.log(
          "📍 WASM URL: /aesthetic.computer/dep/webpxmux/webpxmux.wasm",
        );

        const xMux = window.WebPXMux(
          "/aesthetic.computer/dep/webpxmux/webpxmux.wasm",
        );
        console.log("✅ WebPXMux instance created:", xMux);

        console.log("⏳ Waiting for WebAssembly runtime...");
        await xMux.waitRuntime();
        console.log("✅ WebAssembly runtime ready!");

        // Verify xMux instance methods are available
        console.log("🔍 Verifying xMux methods:", {
          hasEncodeFrames: typeof xMux.encodeFrames === "function",
          hasWaitRuntime: typeof xMux.waitRuntime === "function",
          xMuxKeys: Object.keys(xMux),
        });

        // Convert our frame data to WebPXMux format with smart upscaling
        const frames = [];

        for (let i = 0; i < content.frames.length; i++) {
          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          const frame = content.frames[i];

          // Convert frame data to RGBA Uint32Array format expected by webpxmux
          let rgba = new Uint32Array(frame.width * frame.height);
          const data = frame.data;

          for (let j = 0; j < rgba.length; j++) {
            const pixelIndex = j * 4;
            const r = data[pixelIndex];
            const g = data[pixelIndex + 1];
            const b = data[pixelIndex + 2];
            const a = data[pixelIndex + 3];

            // Pack RGBA into 32-bit integer (0xRRGGBBAA format)
            rgba[j] = (r << 24) | (g << 16) | (b << 8) | a;
          }

          // Add AC stamp to the frame data by rendering through canvas
          const currentProgress = (i + 1) / content.frames.length;
          const stampedPixelData = await addStampToPixelData(
            new Uint8ClampedArray(data),
            frame.width,
            frame.height,
            optimalScale,
            currentProgress,
            frame,
          );

          // Convert stamped pixel data back to RGBA Uint32Array
          rgba = new Uint32Array(
            frame.width * frame.height * optimalScale * optimalScale,
          );
          for (let j = 0; j < rgba.length; j++) {
            const pixelIndex = j * 4;
            const r = stampedPixelData[pixelIndex];
            const g = stampedPixelData[pixelIndex + 1];
            const b = stampedPixelData[pixelIndex + 2];
            const a = stampedPixelData[pixelIndex + 3];

            // Pack RGBA into 32-bit integer (0xRRGGBBAA format)
            rgba[j] = (r << 24) | (g << 16) | (b << 8) | a;
          }

          // Skip the original upscaling since we already did it with the stamp
          const upscaleResult = { rgba: rgba, actualScale: optimalScale };

          // Update actual scale if upscaling failed
          if (upscaleResult.actualScale !== optimalScale && i === 0) {
            console.warn(
              `⚠️ Upscaling failed, using ${upscaleResult.actualScale}x instead of ${optimalScale}x`,
            );
            optimalScale = upscaleResult.actualScale; // Update for remaining frames
          }

          frames.push({
            duration: Math.max(frame.duration || 16.67, 10), // Default 60fps (~16.67ms)
            isKeyframe: i === 0, // First frame is keyframe
            rgba: upscaleResult.rgba,
          });

          // Aggressive memory cleanup
          rgba.fill(0);
          rgba = null; // Help GC
        }

        console.log("🔄 Encoding animated WebP...");

        // Use the actual scale that was achieved (may be different if upscaling failed)
        const actualWidth =
          frames.length > 0
            ? Math.sqrt(
                frames[0].rgba.length / (originalWidth * originalHeight),
              ) * originalWidth
            : originalWidth;
        const actualHeight =
          frames.length > 0
            ? Math.sqrt(
                frames[0].rgba.length / (originalWidth * originalHeight),
              ) * originalHeight
            : originalHeight;

        const webpFrames = {
          frameCount: frames.length,
          width: Math.round(actualWidth),
          height: Math.round(actualHeight),
          loopCount: 0, // Infinite loop
          bgColor: 0xffffffff, // White background
          frames: frames,
        };

        console.log("� WebP frames data structure:", {
          frameCount: webpFrames.frameCount,
          width: webpFrames.width,
          height: webpFrames.height,
          loopCount: webpFrames.loopCount,
          bgColor: webpFrames.bgColor.toString(16),
          firstFrameDuration: webpFrames.frames[0]?.duration,
          firstFrameRgbaLength: webpFrames.frames[0]?.rgba?.length,
          totalFrames: webpFrames.frames.length,
        });

        console.log("�🔧 Calling xMux.encodeFrames...");
        const webpData = await xMux.encodeFrames(webpFrames);
        console.log("✅ WebP encoding complete! Data length:", webpData.length);

        const webpBlob = new Blob([webpData], { type: "image/webp" });
        const filename = generateTapeFilename("webp", "-animated");

        console.log(
          `💾 Animated WebP generated: ${filename} (${Math.round((webpBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: webpBlob });

        console.log("🎬 Animated WebP exported successfully!");
      } catch (error) {
        console.error("Error creating animated WebP:", error);
        console.log("🔄 Falling back to static WebP of first frame");

        try {
          const canvas = document.createElement("canvas");
          const ctx = canvas.getContext("2d");
          const firstFrame = content.frames[0];

          canvas.width = firstFrame.width;
          canvas.height = firstFrame.height;

          const imageData = new ImageData(
            new Uint8ClampedArray(firstFrame.data),
            firstFrame.width,
            firstFrame.height,
          );

          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp to fallback WebP as well (await to ensure fonts are loaded)
          await addAestheticComputerStamp(
            ctx,
            firstFrame.width,
            firstFrame.height,
            0,
            firstFrame.data,
            firstFrame,
          );

          const webpBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/webp", 0.9);
          });

          const filename = generateTapeFilename("webp", "-static");
          receivedDownload({ filename, data: webpBlob });

          console.log("📸 Static WebP fallback exported successfully");
        } catch (fallbackError) {
          console.error("Error in fallback WebP creation:", fallbackError);
        }
      }
      return;
    }

    // 🎬 Handle APNG frame chunks
    if (type === "create-single-animated-apng-chunk") {
      if (window.apngFrameChunks) {
        window.apngFrameChunks.push(content.frames);
        window.apngReceivedChunks++;
        
        console.log(
          `🎞️ Received APNG chunk ${window.apngReceivedChunks}/${window.apngTotalChunks}:`,
          content.frames.length,
          "frames"
        );

        if (window.apngReceivedChunks === window.apngTotalChunks) {
          console.log("🎞️ All APNG chunks received, reassembling...");
          const allFrames = window.apngFrameChunks.flat();
          console.log("🎞️ Total frames after reassembly:", allFrames.length);

          delete window.apngFrameChunks;
          delete window.apngTotalChunks;
          delete window.apngReceivedChunks;

          receivedChange({
            data: {
              type: "create-single-animated-apng",
              content: { frames: allFrames }
            }
          });
        }
      }
      return;
    }

    // 🎬 Create APNG only
    if (type === "create-single-animated-apng") {
      if (content.totalChunks && content.totalChunks > 1) {
        console.log(
          `🎞️ Receiving APNG frames in chunks (1/${content.totalChunks}):`,
          content.frames.length,
          "frames"
        );
        window.apngFrameChunks = [content.frames];
        window.apngTotalChunks = content.totalChunks;
        window.apngReceivedChunks = 1;
        return;
      }

      console.log("🎞️ Creating APNG from", content.frames.length, "frames");

      // Helper function to upscale pixels with nearest neighbor (same as WebP)
      function upscalePixels(imageData, originalWidth, originalHeight, scale) {
        if (scale === 1) {
          return imageData;
        }

        const scaledWidth = originalWidth * scale;
        const scaledHeight = originalHeight * scale;
        const scaledImageData = new Uint8ClampedArray(
          scaledWidth * scaledHeight * 4,
        );

        for (let y = 0; y < scaledHeight; y++) {
          for (let x = 0; x < scaledWidth; x++) {
            const sourceX = Math.floor(x / scale);
            const sourceY = Math.floor(y / scale);
            const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
            const targetIndex = (y * scaledWidth + x) * 4;

            scaledImageData[targetIndex] = imageData[sourceIndex]; // R
            scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
            scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
            scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
          }
        }

        return scaledImageData;
      }

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for APNG creation");
          return;
        }

        console.log("🔄 Creating animated PNG (APNG)");

        // Pre-calculate optimal scale for APNG (same logic as WebP)
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const frameCount = content.frames.length;

        // Be more conservative with memory limits for large frame counts
        const baseMemoryLimit = 25 * 1024 * 1024; // 25M pixels base limit
        const adjustedMemoryLimit = Math.max(
          baseMemoryLimit / Math.max(1, frameCount / 500), // Reduce limit for many frames
          1024 * 1024, // Minimum 1M pixels
        );

        let optimalScale = 4;
        let scaledPixels =
          originalWidth * originalHeight * optimalScale * optimalScale;

        console.log(
          `🧮 APNG Memory calculation: ${frameCount} frames, ${scaledPixels} pixels per frame (4x), limit: ${Math.round(adjustedMemoryLimit / 1024)}K pixels`,
        );

        if (scaledPixels > adjustedMemoryLimit) {
          optimalScale = 2;
          scaledPixels =
            originalWidth * originalHeight * optimalScale * optimalScale;
          console.warn(
            `⚠️ 4x scaling would exceed memory limit for ${frameCount} frames, using 2x scaling`,
          );

          if (scaledPixels > adjustedMemoryLimit) {
            optimalScale = 1;
            console.warn(
              `⚠️ Even 2x scaling too large for ${frameCount} frames, using original size (1x)`,
            );
          }
        }

        console.log(
          `📏 APNG Using ${optimalScale}x scaling: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Load pako (required for UPNG.js compression) and UPNG.js library
        if (!window.pako) {
          console.log("📦 Loading pako compression library...");
          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src =
              "https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js";
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
          });
        }

        if (!window.UPNG) {
          console.log("📦 Loading UPNG.js library...");
          await new Promise((resolve, reject) => {
            const script = document.createElement("script");
            script.src = "https://cdn.jsdelivr.net/npm/upng-js@2.1.0/UPNG.js";
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
          });
        }

        console.log(
          `🖼️ Canvas size: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Convert frames to the format UPNG expects with upscaling
        const frameBuffers = [];
        const delays = [];

        for (let i = 0; i < content.frames.length; i++) {
          const frame = content.frames[i];

          if (i % 50 === 0 || i === content.frames.length - 1) {
            console.log(
              `🎞️ Processing APNG frame ${i + 1}/${content.frames.length} (${Math.round(((i + 1) / content.frames.length) * 100)}%)`,
            );
          }

          // Apply upscaling and add AC stamp to frame data
          const originalData = new Uint8ClampedArray(frame.data);
          const currentProgress = (i + 1) / content.frames.length;
          const scaledData = await addStampToPixelData(
            originalData,
            frame.width,
            frame.height,
            optimalScale,
            currentProgress,
            frame,
          );

          // UPNG expects RGBA data as ArrayBuffer
          const frameData = new Uint8Array(scaledData);
          frameBuffers.push(frameData.buffer);

          // Convert to milliseconds for APNG delays with 60fps default
          delays.push(Math.max(frame.duration || 16.67, 10)); // Default 60fps (~16.67ms)
        }

        console.log("🔄 Encoding animated PNG...");

        // Create animated PNG using UPNG with scaled dimensions
        const apngBuffer = window.UPNG.encode(
          frameBuffers,
          originalWidth * optimalScale, // Use scaled width
          originalHeight * optimalScale, // Use scaled height
          0, // 0 = lossless, or use 256 for lossy
          delays,
        );

        const apngBlob = new Blob([apngBuffer], { type: "image/png" });
        const filename = generateTapeFilename("png", "-animated");

        console.log(
          `💾 Animated PNG generated: ${filename} (${Math.round((apngBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: apngBlob });

        console.log("🎬 Animated PNG (APNG) exported successfully!");
      } catch (error) {
        console.error("Error creating APNG:", error);
        console.log("🔄 Falling back to static PNG of first frame");

        try {
          const canvas = document.createElement("canvas");
          const ctx = canvas.getContext("2d");
          const firstFrame = content.frames[0];

          canvas.width = firstFrame.width;
          canvas.height = firstFrame.height;

          const imageData = new ImageData(
            new Uint8ClampedArray(firstFrame.data),
            firstFrame.width,
            firstFrame.height,
          );

          ctx.putImageData(imageData, 0, 0);

          // Add sideways AC stamp to fallback PNG as well (await to ensure fonts are loaded)
          await addAestheticComputerStamp(
            ctx,
            firstFrame.width,
            firstFrame.height,
            0,
            firstFrame.data,
            firstFrame,
          );

          const pngBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/png");
          });

          const filename = generateTapeFilename("png", "-static");
          receivedDownload({ filename, data: pngBlob });

          console.log("📸 Static PNG fallback exported successfully");
        } catch (fallbackError) {
          console.error("Error in fallback PNG creation:", fallbackError);
        }
      }
      return;
    }

    // 🎬 Handle ZIP frame chunks
    if (type === "create-animated-frames-zip-chunk") {
      if (window.zipFrameChunks) {
        window.zipFrameChunks.push(content.frames);
        window.zipReceivedChunks++;
        
        console.log(
          `📦 Received ZIP chunk ${window.zipReceivedChunks}/${window.zipTotalChunks}:`,
          content.frames.length,
          "frames"
        );

        if (window.zipReceivedChunks === window.zipTotalChunks) {
          console.log("📦 All ZIP chunks received, reassembling...");
          const allFrames = window.zipFrameChunks.flat();
          console.log("📦 Total frames after reassembly:", allFrames.length);

          // Clean up chunk tracking
          delete window.zipFrameChunks;
          delete window.zipTotalChunks;
          delete window.zipReceivedChunks;

          // Process as if we received all frames at once
          receivedChange({
            data: {
              type: "create-animated-frames-zip",
              content: { frames: allFrames }
            }
          });
        }
      }
      return;
    }

    // 🎬 Create ZIP of high-resolution frames
    if (type === "create-animated-frames-zip") {
      // Handle chunked frame transfer for large recordings
      if (content.totalChunks && content.totalChunks > 1) {
        console.log(
          `📦 Receiving ZIP frames in chunks (1/${content.totalChunks}):`,
          content.frames.length,
          "frames"
        );
        window.zipFrameChunks = [content.frames];
        window.zipTotalChunks = content.totalChunks;
        window.zipReceivedChunks = 1;
        return;
      }

      console.log(
        "📦 Creating ZIP of high-resolution frames from",
        content.frames.length,
        "frames",
      );

      // Send initial progress to show the progress bar immediately
      send({
        type: "recorder:transcode-progress",
        content: 0.01, // 1% to start
      });

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for ZIP creation");
          return;
        }

        // Load JSZip if not already loaded
        if (!window.JSZip) await loadJSZip();
        const zip = new window.JSZip();

        // Use 6x scaling for ultra-high resolution frames
        const scale = 6;
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const scaledWidth = originalWidth * scale;
        const scaledHeight = originalHeight * scale;

        console.log(
          `📏 Using ${scale}x scaling: ${originalWidth}x${originalHeight} -> ${scaledWidth}x${scaledHeight}`,
        );

        // Create a canvas for frame processing
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");
        canvas.width = scaledWidth;
        canvas.height = scaledHeight;

        // Process each frame and add to ZIP
        for (let i = 0; i < content.frames.length; i++) {
          const frame = content.frames[i];

          // Update progress
          const progress = (i / content.frames.length) * 0.9 + 0.1; // 10-100%
          send({
            type: "recorder:transcode-progress",
            content: progress,
          });

          if (i % 10 === 0 || i === content.frames.length - 1) {
            console.log(
              `🖼️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(progress * 100)}%)`,
            );
          }

          // Create ImageData from frame data
          const imageData = new ImageData(
            new Uint8ClampedArray(frame.data),
            frame.width,
            frame.height,
          );

          // Clear canvas and draw scaled frame
          ctx.clearRect(0, 0, scaledWidth, scaledHeight);
          
          // Create a temporary canvas for the original frame
          const tempCanvas = document.createElement("canvas");
          const tempCtx = tempCanvas.getContext("2d");
          tempCanvas.width = frame.width;
          tempCanvas.height = frame.height;
          
          // Put original image data on temp canvas
          tempCtx.putImageData(imageData, 0, 0);
          
          // Add stamp to the original frame with correct frame metadata for timestamp
          await addAestheticComputerStamp(
            tempCtx,
            frame.width,
            frame.height,
            0, // Progress is not used for individual frame timestamps
            frame.data,
            frame, // Frame metadata with timestamp
            i, // Frame index for timestamp calculation
            content.frames.length, // Total frames for timestamp calculation
          );
          
          // Scale up the stamped frame using nearest neighbor
          ctx.imageSmoothingEnabled = false;
          ctx.drawImage(tempCanvas, 0, 0, scaledWidth, scaledHeight);

          // Convert to PNG blob
          const pngBlob = await new Promise((resolve) => {
            canvas.toBlob(resolve, "image/png");
          });

          // Add to ZIP with timestamp-based filename
          let filename;
          if (frame.timestamp !== undefined) {
            // Create proper date/time timestamp from frame timestamp
            const frameDate = new Date(frame.timestamp);
            const year = frameDate.getFullYear();
            const month = frameDate.getMonth() + 1;
            const day = frameDate.getDate();
            const hour = frameDate.getHours();
            const minute = frameDate.getMinutes();
            const second = frameDate.getSeconds();
            const millisecond = frameDate.getMilliseconds();
            
            // Use same format as main tape filename: year.month.day.hour.minute.second.millisecond
            const dateTimestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
            filename = `frame_${dateTimestamp}.png`;
          } else {
            // Fallback to index-based naming if timestamp is missing
            const frameNumber = String(i).padStart(4, "0");
            filename = `frame_${frameNumber}.png`;
          }
          zip.file(filename, pngBlob);
        }

        // Function to convert raw audio data to WAV format
        function createWavFromRawAudio(rawAudioChunks, sampleRate = 48000) {
          if (!rawAudioChunks || rawAudioChunks.length === 0) {
            return null;
          }

          // Calculate total samples
          const totalSamples = rawAudioChunks.length * 4096; // 4096 samples per chunk
          const numChannels = 2; // Stereo
          const bytesPerSample = 2; // 16-bit
          const byteRate = sampleRate * numChannels * bytesPerSample;
          const blockAlign = numChannels * bytesPerSample;
          const dataSize = totalSamples * numChannels * bytesPerSample;
          const fileSize = 36 + dataSize;

          // Create WAV file buffer
          const buffer = new ArrayBuffer(44 + dataSize);
          const view = new DataView(buffer);

          // WAV header
          const writeString = (offset, string) => {
            for (let i = 0; i < string.length; i++) {
              view.setUint8(offset + i, string.charCodeAt(i));
            }
          };

          writeString(0, 'RIFF');
          view.setUint32(4, fileSize, true);
          writeString(8, 'WAVE');
          writeString(12, 'fmt ');
          view.setUint32(16, 16, true); // fmt chunk size
          view.setUint16(20, 1, true); // PCM format
          view.setUint16(22, numChannels, true);
          view.setUint32(24, sampleRate, true);
          view.setUint32(28, byteRate, true);
          view.setUint16(32, blockAlign, true);
          view.setUint16(34, 16, true); // bits per sample
          writeString(36, 'data');
          view.setUint32(40, dataSize, true);

          // Convert float32 audio data to 16-bit PCM
          let offset = 44;
          for (let i = 0; i < rawAudioChunks.length; i++) {
            const chunk = rawAudioChunks[i];
            for (let j = 0; j < chunk.left.length; j++) {
              // Left channel
              const leftSample = Math.max(-1, Math.min(1, chunk.left[j]));
              view.setInt16(offset, leftSample * 0x7FFF, true);
              offset += 2;
              
              // Right channel
              const rightSample = Math.max(-1, Math.min(1, chunk.right[j]));
              view.setInt16(offset, rightSample * 0x7FFF, true);
              offset += 2;
            }
          }

          return new Blob([buffer], { type: 'audio/wav' });
        }

        // Add timing information as JSON
        const timingInfo = content.frames.map((frame, i) => {
          let filename;
          if (frame.timestamp !== undefined) {
            const frameDate = new Date(frame.timestamp);
            const year = frameDate.getFullYear();
            const month = frameDate.getMonth() + 1;
            const day = frameDate.getDate();
            const hour = frameDate.getHours();
            const minute = frameDate.getMinutes();
            const second = frameDate.getSeconds();
            const millisecond = frameDate.getMilliseconds();
            
            const dateTimestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${millisecond.toString().padStart(3, "0")}`;
            filename = `frame_${dateTimestamp}.png`;
          } else {
            filename = `frame_${String(i).padStart(4, "0")}.png`;
          }
          
          return {
            frame: i,
            filename: filename,
            duration: frame.duration,
            timestamp: frame.timestamp,
          };
        });
        
        zip.file("timing.json", JSON.stringify(timingInfo, null, 2));
        
        // Add metadata
        const metadata = {
          originalSize: { width: originalWidth, height: originalHeight },
          scaledSize: { width: scaledWidth, height: scaledHeight },
          scale: scale,
          frameCount: content.frames.length,
          totalDuration: content.frames.reduce((sum, f) => sum + (f.duration || 100), 0),
          exportedAt: new Date().toISOString(),
          pieceChanges: recordedPieceChanges, // Track piece changes during recording
        };
        
        console.log(`🎬 📍 Piece changes included in metadata: ${metadata.pieceChanges.length} changes`);
        
        zip.file("metadata.json", JSON.stringify(metadata, null, 2));

        // Add audio file if available - convert raw audio to WAV
        try {
          if (rawAudioData && rawAudioData.length > 0) {
            console.log("🎵 Converting raw audio data to WAV for ZIP...");
            const wavBlob = createWavFromRawAudio(rawAudioData, rawAudioSampleRate);
            if (wavBlob) {
              zip.file("soundtrack.wav", wavBlob);
              console.log("🎵 Audio added to ZIP as soundtrack.wav");
            } else {
              console.log("🎵 Failed to create WAV from raw audio data");
            }
          } else {
            console.log("🎵 No raw audio data available for ZIP export");
          }
        } catch (audioError) {
          console.warn("🎵 Could not add audio to ZIP:", audioError);
        }

        console.log("🗜️ Generating ZIP file...");
        const zipBlob = await zip.generateAsync({ type: "blob" });
        
        const filename = generateTapeFilename("zip");
        
        console.log(
          `💾 ZIP file generated: ${filename} (${Math.round((zipBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Use the existing download function
        receivedDownload({ filename, data: zipBlob });

        console.log("📦 High-resolution frames ZIP exported successfully!");
        
        // Send completion
        send({
          type: "recorder:transcode-progress",
          content: 1.0,
        });
        
      } catch (error) {
        console.error("Error creating frames ZIP:", error);
      }
      return;
    }

    // Helper function to convert raw audio data to WAV format
    function createWavFromRawAudio(rawAudioChunks, sampleRate = 48000) {
      if (!rawAudioChunks || rawAudioChunks.length === 0) {
        return null;
      }

      // Calculate total samples
      const totalSamples = rawAudioChunks.length * 4096; // 4096 samples per chunk
      const numChannels = 2; // Stereo
      const bytesPerSample = 2; // 16-bit
      const byteRate = sampleRate * numChannels * bytesPerSample;
      const blockAlign = numChannels * bytesPerSample;
      const dataSize = totalSamples * numChannels * bytesPerSample;
      const fileSize = 36 + dataSize;

      // Create WAV file buffer
      const buffer = new ArrayBuffer(44 + dataSize);
      const view = new DataView(buffer);

      // WAV header
      const writeString = (offset, string) => {
        for (let i = 0; i < string.length; i++) {
          view.setUint8(offset + i, string.charCodeAt(i));
        }
      };

      writeString(0, 'RIFF');
      view.setUint32(4, fileSize, true);
      writeString(8, 'WAVE');
      writeString(12, 'fmt ');
      view.setUint32(16, 16, true); // fmt chunk size
      view.setUint16(20, 1, true); // PCM format
      view.setUint16(22, numChannels, true);
      view.setUint32(24, sampleRate, true);
      view.setUint32(28, byteRate, true);
      view.setUint16(32, blockAlign, true);
      view.setUint16(34, 16, true); // bits per sample
      writeString(36, 'data');
      view.setUint32(40, dataSize, true);

      // Convert float32 audio data to 16-bit PCM
      let offset = 44;
      for (let i = 0; i < rawAudioChunks.length; i++) {
        const chunk = rawAudioChunks[i];
        for (let j = 0; j < chunk.left.length; j++) {
          // Left channel
          const leftSample = Math.max(-1, Math.min(1, chunk.left[j]));
          view.setInt16(offset, leftSample * 0x7FFF, true);
          offset += 2;
          
          // Right channel
          const rightSample = Math.max(-1, Math.min(1, chunk.right[j]));
          view.setInt16(offset, rightSample * 0x7FFF, true);
          offset += 2;
        }
      }

      return new Blob([buffer], { type: 'audio/wav' });
    }

    // 🎬 Handle POST tape frame chunks
    if (type === "create-and-post-tape-chunk") {
      if (window.postFrameChunks) {
        window.postFrameChunks.push(content.frames);
        window.postReceivedChunks++;
        
        console.log(
          `📼 Received POST chunk ${window.postReceivedChunks}/${window.postTotalChunks}:`,
          content.frames.length,
          "frames"
        );

        if (window.postReceivedChunks === window.postTotalChunks) {
          console.log("📼 All POST chunks received, reassembling...");
          const allFrames = window.postFrameChunks.flat();
          console.log("📼 Total frames after reassembly:", allFrames.length);

          const rawAudio = window.postRawAudio;
          const piece = window.postPiece;

          delete window.postFrameChunks;
          delete window.postTotalChunks;
          delete window.postReceivedChunks;
          delete window.postRawAudio;
          delete window.postPiece;

          receivedChange({
            data: {
              type: "create-and-post-tape",
              content: { frames: allFrames, rawAudio, piece }
            }
          });
        }
      }
      return;
    }

    // 🎬 Create ZIP and POST tape to cloud (MongoDB + ATProto)
    if (type === "create-and-post-tape") {
      if (content.totalChunks && content.totalChunks > 1) {
        console.log(
          `📼 Receiving POST frames in chunks (1/${content.totalChunks}):`,
          content.frames.length,
          "frames"
        );
        window.postFrameChunks = [content.frames];
        window.postTotalChunks = content.totalChunks;
        window.postReceivedChunks = 1;
        window.postRawAudio = content.rawAudio; // Store rawAudio from first chunk
        window.postPiece = content.piece; // Store piece name from first chunk
        return;
      }

      console.log(
        "📼 Creating and posting tape from",
        content.frames.length,
        "frames",
      );

      // Send initial progress with status
      send({
        type: "recorder:export-status",
        content: { message: "PREPARING FRAMES", phase: "preparing" },
      });
      send({
        type: "recorder:transcode-progress",
        content: 0.01,
      });

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for tape posting");
          send({ type: "tape:post-error", content: { error: "No frames" } });
          return;
        }

        // Load JSZip if not already loaded
        if (!window.JSZip) await loadJSZip();
        const zip = new window.JSZip();

        // Use original resolution (no scaling for tape uploads)
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;

        console.log(
          `📏 Creating tape ZIP at original resolution: ${originalWidth}x${originalHeight}`,
        );

        // Create a canvas for frame processing
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");
        canvas.width = originalWidth;
        canvas.height = originalHeight;

        // Timing data for timing.json
        const timingData = [];
        let totalDuration = 0;

        // Process each frame and add to ZIP
        for (let i = 0; i < content.frames.length; i++) {
          const frame = content.frames[i];

          // Update progress (10-75%)
          const progress = (i / content.frames.length) * 0.65 + 0.1;
          
          // Send detailed status updates
          if (i % 10 === 0 || i === content.frames.length - 1) {
            send({
              type: "recorder:export-status",
              content: {
                message: `PROCESSING FRAME ${i + 1}/${content.frames.length}`,
                phase: "processing",
              },
            });
          console.log(
            `🖼️ Processing frame ${i + 1}/${content.frames.length} (${Math.round(progress * 100)}%)`,
          );
        }
        
        const progressMessage = {
          type: "recorder:transcode-progress",
          content: progress,
        };
        console.log("📤 BIOS sending progress message:", progressMessage);
        send(progressMessage);          // Create ImageData from frame data
          const imageData = new ImageData(
            new Uint8ClampedArray(frame.data),
            frame.width,
            frame.height,
          );

          // Clear canvas and put original frame
          ctx.clearRect(0, 0, originalWidth, originalHeight);
          ctx.putImageData(imageData, 0, 0);
          
          // Note: No stamp or overlay for tape recording - just pure original frames
          
          // Convert to PNG blob (original resolution)
          const pngBlob = await new Promise((resolve) => {
            canvas.toBlob((blob) => resolve(blob), "image/png");
          });

          // Add to ZIP with frame number
          const filename = `frame-${String(i).padStart(5, "0")}.png`;
          zip.file(filename, pngBlob);

          // Add timing info
          timingData.push({
            frame: i,
            filename,
            duration: frame.duration,
            timestamp: frame.timestamp,
          });
          
          totalDuration += frame.duration;
        }

        // Add timing.json (75-80%)
        send({
          type: "recorder:export-status",
          content: { message: "CREATING TIMING DATA", phase: "metadata" },
        });
        send({
          type: "recorder:transcode-progress",
          content: 0.75,
        });
        
        zip.file("timing.json", JSON.stringify(timingData, null, 2));

        // Add metadata.json (80-85%)
        send({
          type: "recorder:export-status",
          content: { message: "ADDING METADATA", phase: "metadata" },
        });
        send({
          type: "recorder:transcode-progress",
          content: 0.8,
        });
        
        // Generate full timestamp slug (for authenticated users)
        const token = await authorize().catch(() => null);
        let slug;
        if (token) {
          // User upload: use full timestamp format like paintings
          slug = new Date()
            .toISOString()
            .replace(/[-:]/g, ".")
            .replace("T", ".")
            .replace("Z", "") // Remove trailing Z
            .split(".").slice(0, 7).join("."); // Include milliseconds: YYYY.MM.DD.HH.MM.SS.mmm
        } else {
          // Guest upload: server will generate nanoid slug
          slug = null;
        }
        
        const metadata = {
          resolution: { width: originalWidth, height: originalHeight },
          frameCount: content.frames.length,
          totalDuration: totalDuration / 1000, // Convert from milliseconds to seconds
          piece: content.piece || "video",
          exportedAt: new Date().toISOString(),
          audioSampleRate: content.rawAudio?.sampleRate || null, // Store original audio sample rate
          pieceChanges: recordedPieceChanges, // Track piece changes during recording
          slug: slug, // Include slug for backend to use for filename
        };
        
        console.log(`🎬 📍 Piece changes included in metadata: ${metadata.pieceChanges.length} changes`);
        
        zip.file("metadata.json", JSON.stringify(metadata, null, 2));

        // Add audio file if available - convert raw audio to WAV
        if (content.rawAudio && content.rawAudio.left && content.rawAudio.right) {
          try {
            console.log("🎵 Converting raw audio data to WAV for tape ZIP...");
            console.log(`🎵 Audio: ${content.rawAudio.totalSamples} samples at ${content.rawAudio.sampleRate}Hz`);
            
            // Convert Float32Arrays to the format expected by createWavFromRawAudio
            const audioChunks = [];
            const chunkSize = 4096;
            const numChunks = Math.ceil(content.rawAudio.totalSamples / chunkSize);
            
            for (let i = 0; i < numChunks; i++) {
              const start = i * chunkSize;
              const end = Math.min(start + chunkSize, content.rawAudio.totalSamples);
              const left = content.rawAudio.left.slice(start, end);
              const right = content.rawAudio.right.slice(start, end);
              audioChunks.push({ left, right });
            }
            
            const wavBlob = createWavFromRawAudio(audioChunks, content.rawAudio.sampleRate);
            if (wavBlob) {
              zip.file("soundtrack.wav", wavBlob);
              console.log("🎵 Audio added to ZIP as soundtrack.wav");
            } else {
              console.log("🎵 Failed to create WAV from raw audio data");
            }
          } catch (audioError) {
            console.warn("🎵 Could not add audio to ZIP:", audioError);
          }
        } else {
          console.log("🎵 No raw audio data available for tape ZIP export");
        }

        // Generate ZIP blob (85-90%)
        send({
          type: "recorder:export-status",
          content: { message: "COMPRESSING ZIP", phase: "zipping" },
        });
        send({
          type: "recorder:transcode-progress",
          content: 0.85,
        });
        
        console.log("🗜️ Generating ZIP file...");
        const zipBlob = await zip.generateAsync({ type: "blob" });
        
        // Generate filename using slug (already generated above for metadata)
        let filename;
        
        if (slug) {
          // User upload: use slug.zip format (slug already contains full timestamp)
          filename = `${slug}.zip`;
        } else {
          // Guest upload: use generic name, server will generate nanoid
          filename = "tape.zip";
        }
        
        console.log(
          `💾 ZIP file generated: ${filename} (${Math.round((zipBlob.size / 1024 / 1024) * 100) / 100} MB)`,
        );

        // Upload to cloud (90-100%)
        send({
          type: "recorder:export-status",
          content: { message: "UPLOADING TO CLOUD", phase: "uploading" },
        });
        send({
          type: "recorder:transcode-progress",
          content: 0.90,
        });
        
        // Use existing receivedUpload pattern with metadata
        receivedUpload(
          { filename, data: zipBlob },
          "tape:posted",  // Success callback type
          metadata,       // Pass metadata for database
        );

        console.log("📼 Tape ZIP created and queued for upload!");
        
      } catch (error) {
        console.error("Error creating/posting tape:", error);
        send({ 
          type: "tape:post-error", 
          content: { error: error.message } 
        });
      }
      return;
    }

    // 🎬 Handle GIF frame chunks
    if (type === "create-animated-gif-chunk") {
      if (window.gifFrameChunks) {
        window.gifFrameChunks.push(content.frames);
        window.gifReceivedChunks++;
        
        console.log(
          `🎞️ Received GIF chunk ${window.gifReceivedChunks}/${window.gifTotalChunks}:`,
          content.frames.length,
          "frames"
        );

        if (window.gifReceivedChunks === window.gifTotalChunks) {
          console.log("🎞️ All GIF chunks received, reassembling...");
          const allFrames = window.gifFrameChunks.flat();
          console.log("🎞️ Total frames after reassembly:", allFrames.length);

          // Clean up chunk tracking
          delete window.gifFrameChunks;
          delete window.gifTotalChunks;
          delete window.gifReceivedChunks;

          // Process as if we received all frames at once
          // Trigger the create-animated-gif handler with complete frames
          receivedChange({
            data: {
              type: "create-animated-gif",
              content: { frames: allFrames }
            }
          });
        }
      }
      return;
    }

    // 🎬 Create animated GIF
    if (type === "create-animated-gif") {
      // Handle chunked frame transfer for large recordings
      if (content.totalChunks && content.totalChunks > 1) {
        console.log(
          `🎞️ Receiving GIF frames in chunks (1/${content.totalChunks}):`,
          content.frames.length,
          "frames"
        );
        window.gifFrameChunks = [content.frames];
        window.gifTotalChunks = content.totalChunks;
        window.gifReceivedChunks = 1;
        return;
      }

      console.log(
        "🎞️ Creating animated GIF from",
        content.frames.length,
        "frames",
      );

      // Calculate GIF duration for filename - ALWAYS prioritize intended duration for real-time accuracy
      // When user says "tape 15" they expect exactly 15 seconds of real wall-clock time
      let totalDurationMs = 0;
      if (window.currentRecordingOptions?.intendedDuration) {
        // Use the EXACT duration specified in the tape command for perfect real-time mapping
        totalDurationMs = window.currentRecordingOptions.intendedDuration * 1000; // Convert to milliseconds
        console.log(`🎬 GIF duration from EXACT intended tape duration: ${window.currentRecordingOptions.intendedDuration}s (${totalDurationMs}ms) for real-time accuracy`);
      } else if (mediaRecorderDuration && mediaRecorderDuration > 0) {
        // Fallback to actual recording duration if no intended duration available
        totalDurationMs = mediaRecorderDuration; // mediaRecorderDuration is already in milliseconds
        console.log(`🎬 GIF duration from measured recording duration: ${Math.round(totalDurationMs / 1000 * 10) / 10}s (${totalDurationMs}ms)`);
      } else if (content.frames && content.frames.length > 0) {
        // Final fallback: sum frame durations if no recording duration available
        totalDurationMs = content.frames.reduce((sum, frame) => {
          return sum + (frame.duration || 100); // Default to 100ms if no duration
        }, 0);
        console.log(`🎬 GIF duration from frame durations: ${Math.round(totalDurationMs / 1000 * 10) / 10}s (${totalDurationMs}ms)`);
      }
      window.gifDurationMs = totalDurationMs;

      // Set the recording start timestamp to match the first frame's visual timestamp
      // so the filename matches the first frame timestamp exactly
      // We'll cache the actual computed timestamp from the first frame
      window.firstFrameComputedTimestamp = null; // Reset cache
      
      if (content.frames.length > 0) {
        const firstFrame = content.frames[0];
        console.log(`🎬 Will use computed timestamp from first frame processing for GIF filename`);
        // Don't modify recordingStartTimestamp here - it's needed for proper video playback timing
        // The filename timestamp will be calculated during frame processing instead
      } else {
        console.warn(`⚠️ No frames available, using current time for filename`);
        window.recordingStartTimestamp = Date.now();
      }

      // GIF encoder selection flag
      // Default: gifenc with Floyd-Steinberg dithering enabled
      
      // gifenc is the default GIF encoder (fast, efficient, beautiful dithering)
      // Set window.acUSE_DITHERING = false to disable dithering for faster encoding
      if (window.acUSE_DITHERING === undefined) {
        window.acUSE_DITHERING = true; // Default to dithering enabled
      }
      
      const useDithering = window.acUSE_DITHERING === true;
      
      console.log(`🎨 GIF Encoder: gifenc with ${useDithering ? '32-color palette + Floyd-Steinberg dithering' : '256-color fixed palette (no dithering)'}`);
      if (useDithering) {
        console.log(`💡 To disable dithering, run: window.acUSE_DITHERING = false`);
      } else {
        console.log(`💡 To enable 32-color dithered mode, run: window.acUSE_DITHERING = true`);
      }


      // Helper function to draw exact AC cursor overlay (matches pen.mjs "precise" cursor)
      function addCyanCrosshair(ctx, canvasWidth, canvasHeight, penData, scale = 1) {
        if (!penData || penData.x === undefined || penData.y === undefined) {
          return; // No pen data available
        }
        
        // Scale pen coordinates to match canvas size
        const x = Math.round(penData.x * scale);
        const y = Math.round(penData.y * scale);
        
        // Skip drawing if cursor is outside canvas bounds
        if (x < 0 || x >= canvasWidth || y < 0 || y >= canvasHeight) {
          return;
        }
        
        ctx.save();
        
        // AC cursor settings at 1.5x scale for good visibility without being too large
        const cursorScale = 1.25; // Moderate scale for optimal visibility in GIFs
        const radius = 2 * cursorScale;     // White center circle radius
        const gap = 7.5 * cursorScale;      // Gap from center to crosshair start
        const to = 10 * cursorScale;        // Length of crosshair lines
        const lineWidth = 4 * cursorScale;  // Crosshair line width
        
        // Shadow offset for visibility
        const offsetX = 2 * cursorScale;
        const offsetY = 2 * cursorScale;
        
        ctx.lineCap = "round";
        
        // Draw shadow graphics first
        ctx.save();
        ctx.translate(x + offsetX, y + offsetY);
        
        // Shadow circle in center
        ctx.beginPath();
        ctx.arc(0, 0, radius, 0, 2 * Math.PI);
        ctx.fillStyle = "rgba(0, 0, 0, 0.5)"; // Half-opacity black shadow
        ctx.fill();
        
        // Shadow crosshair lines
        ctx.beginPath();
        ctx.moveTo(0, -gap);
        ctx.lineTo(0, -to);
        ctx.moveTo(0, gap);
        ctx.lineTo(0, to);
        ctx.moveTo(-gap, 0);
        ctx.lineTo(-to, 0);
        ctx.moveTo(gap, 0);
        ctx.lineTo(to, 0);
        
        ctx.strokeStyle = "rgba(0, 0, 0, 0.5)"; // Half-opacity black shadow
        ctx.lineWidth = lineWidth;
        ctx.stroke();
        ctx.restore();
        
        // Draw main cursor graphics
        ctx.save();
        ctx.translate(x, y);
        
        // White center circle
        ctx.beginPath();
        ctx.arc(0, 0, radius, 0, 2 * Math.PI);
        ctx.fillStyle = "white";
        ctx.fill();
        
        // Cyan crosshair lines (exact AC color)
        ctx.beginPath();
        ctx.moveTo(0, -gap); // Top
        ctx.lineTo(0, -to);
        ctx.moveTo(0, gap);  // Bottom
        ctx.lineTo(0, to);
        ctx.moveTo(-gap, 0); // Left
        ctx.lineTo(-to, 0);
        ctx.moveTo(gap, 0);  // Right
        ctx.lineTo(to, 0);
        
        ctx.strokeStyle = "rgb(0, 255, 255)"; // Exact AC cyan color
        ctx.lineWidth = lineWidth;
        ctx.stroke();
        ctx.restore();
        
        ctx.restore();
      }

      try {
        // Load gifenc library if not already loaded
        if (!window.gifenc) {
          console.log("📦 Loading gifenc library...");
          const { GIFEncoder, quantize, applyPalette, prequantize } = await import(
            "/aesthetic.computer/dep/gifenc/gifenc.esm.js"
          );
          window.gifenc = { GIFEncoder, quantize, applyPalette, prequantize };
          console.log("✅ gifenc library loaded successfully");
        }

        // Smart scaling for GIF output based on final size
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const frameCount = content.frames.length;

        // Always use 3x scaling for GIFs
        const optimalScale = 3;
        console.log(
          `📏 Using 3x scaling for GIF (gifenc): ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // 🎯 Properly resample frames to 30fps for optimal GIF size/quality balance
        let processedFrames = content.frames;
        const targetGifFPS = 30;
        
        if (content.frames.length > 0) {
          const totalDuration = content.frames[content.frames.length - 1].timestamp - content.frames[0].timestamp;
          const actualFrameRate = Math.round((content.frames.length / totalDuration) * 1000);
          
          // EARLY HIGH REFRESH RATE DETECTION - before resampling!
          let shouldUseHighRefreshRateNormalization = false;
          if (content.frames.length > 1) {
            const avgOriginalTiming = totalDuration / (content.frames.length - 1);
            console.log(`🎞️ EARLY TIMING ANALYSIS: ${content.frames.length} frames, totalDuration=${totalDuration.toFixed(2)}ms, avgTiming=${avgOriginalTiming.toFixed(2)}ms (${(1000/avgOriginalTiming).toFixed(1)}fps)`);
            
            // Detect high refresh rate from original capture (before resampling)
            if (avgOriginalTiming < 13.5) { // Less than 13.5ms indicates >74fps
              shouldUseHighRefreshRateNormalization = true;
              window.earlyHighRefreshRateDetected = true;
              console.log(`🎞️ EARLY HIGH REFRESH RATE DETECTED: ${(1000/avgOriginalTiming).toFixed(1)}fps capture - will normalize to 60fps timing`);
            } else {
              window.earlyHighRefreshRateDetected = false;
              console.log(`🎞️ EARLY NORMAL REFRESH RATE: ${(1000/avgOriginalTiming).toFixed(1)}fps capture - will use original timing`);
            }
          }
          
          if (actualFrameRate > targetGifFPS && totalDuration > 0) {
            // Calculate how many frames we need for 30fps
            const targetFrameCount = Math.round((totalDuration / 1000) * targetGifFPS);
            processedFrames = [];
            
            console.log(`🎬 Resampling GIF from ${actualFrameRate}fps to ${targetGifFPS}fps (${content.frames.length} -> ${targetFrameCount} frames)`);
            
            // REGULAR EVENLY-SPACED frame selection (no blending - use actual frames for crisp motion)
            // Use perfectly even index spacing and round to nearest frame
            const step = (content.frames.length - 1) / (targetFrameCount - 1);
            
            for (let i = 0; i < targetFrameCount; i++) {
              // Calculate exact fractional index and round to nearest frame
              const exactIndex = i * step;
              const frameIndex = Math.round(exactIndex);
              
              // Clamp to valid range
              const safeIndex = Math.min(Math.max(0, frameIndex), content.frames.length - 1);
              
              // Calculate evenly-spaced timestamp for this frame
              const evenTimestamp = content.frames[0].timestamp + (i / (targetFrameCount - 1)) * totalDuration;
              
              // Use actual frame (no blending) for crisp, frame-aligned motion
              const correctedFrame = { ...content.frames[safeIndex] };
              correctedFrame.originalTimestamp = evenTimestamp;
              
              processedFrames.push(correctedFrame);
            }
            
            console.log(`🎬 GIF frame count: ${content.frames.length} -> ${processedFrames.length} frames (evenly spaced every ${step.toFixed(2)} frames)`);
          } else {
            console.log(`🎬 No resampling needed: ${actualFrameRate}fps <= ${targetGifFPS}fps target`);
            // Initialize early high refresh rate detection for non-resampled case
            if (content.frames.length > 1) {
              const avgOriginalTiming = totalDuration / (content.frames.length - 1);
              window.earlyHighRefreshRateDetected = avgOriginalTiming < 13.5;
              console.log(`🎞️ EARLY TIMING (no resampling): avgTiming=${avgOriginalTiming.toFixed(2)}ms, highRefresh=${window.earlyHighRefreshRateDetected}`);
            } else {
              window.earlyHighRefreshRateDetected = false;
            }
          }
        }

        // Helper function to upscale pixels with nearest neighbor
        function upscalePixels(
          imageData,
          originalWidth,
          originalHeight,
          scale,
        ) {
          if (scale === 1) {
            return imageData;
          }

          const scaledWidth = originalWidth * scale;
          const scaledHeight = originalHeight * scale;
          const scaledImageData = new Uint8ClampedArray(
            scaledWidth * scaledHeight * 4,
          );

          for (let y = 0; y < scaledHeight; y++) {
            for (let x = 0; x < scaledWidth; x++) {
              const sourceX = Math.floor(x / scale);
              const sourceY = Math.floor(y / scale);
              const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
              const targetIndex = (y * scaledWidth + x) * 4;

              scaledImageData[targetIndex] = imageData[sourceIndex]; // R
              scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
              scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
              scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
            }
          }

          return scaledImageData;
        }

        // Use gifenc encoder
        console.log("🎞️ Processing frames for GIF with gifenc...");
        
        // Send initial status
        send({
          type: "recorder:export-status",
          content: { 
            type: "gif", 
            phase: "analyzing", 
            message: "Analyzing frames" 
          }
        });
        
        const { GIFEncoder, applyPalette } = window.gifenc;
          
          // Calculate a better initial capacity based on frame count and size
          const estimatedFrameSize = content.frames[0].width * content.frames[0].height * optimalScale * optimalScale;
          const estimatedSize = content.frames.length * estimatedFrameSize * 0.5; // Rough estimate
          const initialCapacity = Math.max(4096, Math.min(estimatedSize, 1024 * 1024)); // Between 4KB and 1MB
          
          const gif = GIFEncoder({
            auto: true, // Auto mode for simpler usage
            initialCapacity: initialCapacity // Pre-allocate buffer for better performance
          });
          
          const finalFrames = [];
          
          // Check if cursor actually moved during recording to avoid static cursor overlay
          let showCursor = false;
          if (processedFrames.length > 1) {
            const firstFrame = processedFrames[0];
            const movementThreshold = 5; // pixels - minimum movement to show cursor
            
            if (firstFrame.penData && firstFrame.penData.x !== undefined && firstFrame.penData.y !== undefined) {
              const startX = firstFrame.penData.x;
              const startY = firstFrame.penData.y;
              
              // Check if cursor moved significantly from starting position in any frame
              for (let i = 1; i < processedFrames.length; i++) {
                const frame = processedFrames[i];
                if (frame.penData && frame.penData.x !== undefined && frame.penData.y !== undefined) {
                  const deltaX = Math.abs(frame.penData.x - startX);
                  const deltaY = Math.abs(frame.penData.y - startY);
                  const distance = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
                  
                  if (distance > movementThreshold) {
                    showCursor = true;
                    console.log(`🎯 Cursor moved ${distance.toFixed(1)}px from start - will show cursor overlay`);
                    break;
                  }
                }
              }
              
              if (!showCursor) {
                console.log(`🎯 Cursor stayed within ${movementThreshold}px of start position - hiding cursor overlay`);
              }
            }
          }
          
          // Send frame processing status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "processing", 
              message: "Processing frames" 
            }
          });
          
          // Calculate per-frame timing using KidLisp FPS timeline or fallback methods
          console.log("🎞️ === GIFENC TIMING ANALYSIS START ===");
          console.log(`🎬 GIF Encoding Debug - KidLisp FPS Timeline:`, window.currentRecordingOptions?.kidlispFpsTimeline);
          console.log(`🎬 GIF Encoding Debug - Recording Options:`, window.currentRecordingOptions);
          
          // Helper function to get FPS at a specific timestamp
          function getFpsAtTimestamp(timestamp, fpsTimeline, fallbackFps = 60) {
            if (!fpsTimeline || fpsTimeline.length === 0) {
              return fallbackFps;
            }
            
            // Find the last FPS change before or at this timestamp
            let activeFps = fallbackFps;
            for (const fpsChange of fpsTimeline) {
              if (fpsChange.timestamp <= timestamp) {
                activeFps = fpsChange.fps;
              } else {
                break; // Timeline should be chronological
              }
            }
            return activeFps;
          }
          
          // Process each frame with individual timing
          const frameDelays = [];
          const fpsTimeline = window.currentRecordingOptions?.kidlispFpsTimeline;
          let fallbackFps = window.currentRecordingOptions?.kidlispFps || 60;
          
          // Detect high refresh rate displays for non-KidLisp pieces
          if (!fpsTimeline && processedFrames.length > 1) {
            const totalOriginalDuration = processedFrames[processedFrames.length - 1].timestamp - processedFrames[0].timestamp;
            const avgFrameTiming = totalOriginalDuration / (processedFrames.length - 1);
            
            console.log(`🎞️ GIFENC TIMING DEBUG: totalDuration=${totalOriginalDuration.toFixed(2)}ms, frames=${processedFrames.length}, avgTiming=${avgFrameTiming.toFixed(2)}ms (${(1000/avgFrameTiming).toFixed(1)}fps)`);
            
            // Check if early high refresh rate detection was triggered (stored in window)
            const earlyHighRefreshDetected = window.earlyHighRefreshRateDetected;
            console.log(`🎞️ Early high refresh rate detection result: ${earlyHighRefreshDetected}`);
            
            // Use early detection result or fallback to current detection
            const isHighRefreshRate = earlyHighRefreshDetected || avgFrameTiming < 13.5;
            
            if (isHighRefreshRate) {
              // For high refresh rate displays, normalize to 30fps instead of 60fps for proper playback speed
              fallbackFps = 30; // Use 30fps for smoother, slower GIF playback on high refresh displays
              console.log(`🎞️ HIGH REFRESH RATE DETECTED in gifenc (${earlyHighRefreshDetected ? 'early detection' : 'fallback detection'}), normalizing to 30fps instead of 60fps`);
            } else {
              console.log(`🎞️ Normal refresh rate detected in gifenc (${(1000/avgFrameTiming).toFixed(1)}fps), using original timing`);
            }
          }
          
          // Calculate frame delays with intended duration taking absolute priority
          if (window.currentRecordingOptions?.intendedDuration && processedFrames.length > 0) {
            // PRIORITY: Use intended duration for perfect real-time accuracy (e.g., "tape 5" = 5 second playback)
            const totalIntendedMs = window.currentRecordingOptions.intendedDuration * 1000;
            const uniformDelay = Math.round(totalIntendedMs / processedFrames.length);
            console.log(`🎞️ USING INTENDED DURATION: ${window.currentRecordingOptions.intendedDuration}s for ${processedFrames.length} frames = ${uniformDelay}ms per frame`);
            
            // Set all frame delays to the uniform delay for real-time accuracy
            for (let i = 0; i < processedFrames.length; i++) {
              frameDelays.push(uniformDelay);
            }
          } else {
            // FALLBACK: Use FPS-based timing only when no intended duration is specified
            for (let i = 0; i < processedFrames.length; i++) {
              const frame = processedFrames[i];
              const frameTimestamp = frame.timestamp;
              
              // Get the active FPS for this specific frame
              const activeFps = getFpsAtTimestamp(frameTimestamp, fpsTimeline, fallbackFps);
              const frameDelay = Math.round(1000 / activeFps);
              frameDelays.push(frameDelay);
              
              if (i < 5 || i % 20 === 0) { // Log first few frames and every 20th frame
                console.log(`🎞️ Frame ${i}: timestamp=${frameTimestamp.toFixed(2)}ms, fps=${activeFps}, delay=${frameDelay}ms, fallbackFps=${fallbackFps}`);
              }
            }
          }
          
          console.log(`🎞️ Using per-frame KidLisp timing. Frame delays range: ${Math.min(...frameDelays)}ms to ${Math.max(...frameDelays)}ms`);
          console.log(`🎞️ All frame delays:`, frameDelays.slice(0, 10), frameDelays.length > 10 ? `... (${frameDelays.length} total)` : '');
          console.log(`🎞️ DEFAULT DELAY: ${Math.round(1000 / fallbackFps)}ms (${fallbackFps}fps)`);
          
          // Default delay for frames without timeline data (fallback)
          const defaultDelay = Math.round(1000 / fallbackFps);
          
          // Process each frame
          for (let index = 0; index < processedFrames.length; index++) {
            const frame = processedFrames[index];
            const frameDelay = frameDelays[index] || defaultDelay;
            
            // Create temporary canvas for adding progress bar at original size
            const tempCanvas = document.createElement("canvas");
            const tempCtx = tempCanvas.getContext("2d");
            tempCanvas.width = originalWidth;
            tempCanvas.height = originalHeight;
            
            // Put original frame data
            const originalImageData = new Uint8ClampedArray(frame.data);
            const tempImageData = new ImageData(originalImageData, originalWidth, originalHeight);
            tempCtx.putImageData(tempImageData, 0, 0);
            
            // Add 2px VHS-style progress bar at the bottom (before scaling) with transparency buildup
            // Skip progress bar in clean mode (tape:neat recordings)
            if (!window.currentRecordingOptions?.cleanMode) {
              const barProgress = (index + 1) / processedFrames.length;
              
              // Use shared VHS progress bar renderer
              renderVHSProgressBar({
                ctx: tempCtx,
                width: originalWidth,
                height: originalHeight,
                imageData: originalImageData,
                progress: barProgress,
                animFrame: index,
              });
            }
            
            // Get the frame data with progress bar for scaling
            const frameWithProgressBar = tempCtx.getImageData(0, 0, originalWidth, originalHeight);
            
            // Now create the final scaled canvas
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d");
            canvas.width = originalWidth * optimalScale;
            canvas.height = originalHeight * optimalScale;

            // Scale the frame with progress bar
            const scaledImageData = upscalePixels(
              frameWithProgressBar.data,
              originalWidth,
              originalHeight,
              optimalScale,
            );

            const imageData = new ImageData(
              scaledImageData,
              originalWidth * optimalScale,
              originalHeight * optimalScale,
            );

            ctx.putImageData(imageData, 0, 0);

            // Add cyan crosshair cursor overlay if pen data is available AND cursor moved during recording
            if (showCursor && frame.penData) {
              addCyanCrosshair(ctx, canvas.width, canvas.height, frame.penData, optimalScale);
            }

            // Add sideways AC stamp like in video recordings
            const progress = (index + 1) / processedFrames.length;
            
            // For GIF: calculate elapsed time based on the actual delay being used in encoding
            // This ensures timestamp matches the actual GIF playback speed
            const gifElapsedSeconds = (index * frameDelay) / 1000;
            const gifFrameMetadata = {
              ...frame,
              gifElapsedSeconds: gifElapsedSeconds // Pass actual GIF timing
            };
            
            await addAestheticComputerStamp(
              ctx,
              originalWidth * optimalScale,
              originalHeight * optimalScale,
              progress,
              frame.data,
              gifFrameMetadata, // Use enhanced metadata with GIF timing
              index, // frameIndex
              processedFrames.length // totalFrames
            );

            // Get RGBA data for gifenc
            const frameImageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            
            finalFrames.push({
              data: frameImageData.data,
              width: canvas.width,
              height: canvas.height
            });

            // Send detailed progress for frame processing (first 40% of total progress)
            const frameProgress = (index + 1) / processedFrames.length;
            const totalProgress = frameProgress * 0.4; // Frames take up 40% of total progress
            
            // Update more frequently for smoother progress - every 5 frames or key milestones
            if ((index + 1) % 5 === 0 || index === processedFrames.length - 1) {
              // Use the same progress mechanism as MP4 export for consistency
              send({
                type: "recorder:transcode-progress",
                content: totalProgress
              });
            }
            
            // Log less frequently to avoid console spam
            if ((index + 1) % 50 === 0 || index === processedFrames.length - 1) {
              console.log(
                `🎞️ Processed frame ${index + 1}/${processedFrames.length} (${Math.round(frameProgress * 100)}%)`,
              );
            }
          }
          
          // Send color optimization status
          send({
            type: "recorder:transcode-progress",
            content: 0.5
          });
          
          console.log("🔄 Encoding GIF with gifenc (fixed palette)...");
          
          // Create palette based on dithering mode
          let palette = [];
          
          if (useDithering) {
            // 128-color palette for dithered mode: 8×4×4 RGB (good balance between quality and file size)
            console.log(`🎨 Using 128-color palette with Floyd-Steinberg dithering...`);
            for (let r = 0; r < 8; r++) {
              for (let g = 0; g < 4; g++) {
                for (let b = 0; b < 4; b++) {
                  palette.push([
                    Math.floor(r * 255 / 7),  // 8 steps: 0, 36, 73, 109, 146, 182, 219, 255
                    Math.floor(g * 255 / 3),  // 4 steps: 0, 85, 170, 255
                    Math.floor(b * 255 / 3)   // 4 steps: 0, 85, 170, 255
                  ]);
                }
              }
            }
          } else {
            // 256-color palette for non-dithered mode: 8×8×4 RGB
            console.log(`🎨 Using perceptually optimized 256-color palette (no dithering)...`);
            // Perceptually distributed color cube: 8 red × 8 green × 4 blue = 256 colors
            // More green levels because human eyes are most sensitive to green
            // Fewer blue levels because we're less sensitive to blue variations
            for (let r = 0; r < 8; r++) {
              for (let g = 0; g < 8; g++) {
                for (let b = 0; b < 4; b++) {
                  palette.push([
                    Math.floor(r * 255 / 7),  // 8 steps: 0, 36, 73, 109, 146, 182, 219, 255
                    Math.floor(g * 255 / 7),  // 8 steps: 0, 36, 73, 109, 146, 182, 219, 255
                    Math.floor(b * 255 / 3)   // 4 steps: 0, 85, 170, 255
                  ]);
                }
              }
            }
          }
          
          // Floyd-Steinberg dithering function
          // Applies error diffusion to create smooth gradients with limited palette
          function floydSteinbergDither(imageData, palette, width, height) {
            const pixels = new Uint8ClampedArray(imageData); // Copy data
            
            // Helper to find nearest palette color
            function nearestColor(r, g, b) {
              let minDist = Infinity;
              let nearest = palette[0];
              
              for (const color of palette) {
                const dr = r - color[0];
                const dg = g - color[1];
                const db = b - color[2];
                const dist = dr * dr + dg * dg + db * db;
                
                if (dist < minDist) {
                  minDist = dist;
                  nearest = color;
                }
              }
              
              return nearest;
            }
            
            // Apply Floyd-Steinberg error diffusion
            for (let y = 0; y < height; y++) {
              for (let x = 0; x < width; x++) {
                const idx = (y * width + x) * 4;
                
                const oldR = pixels[idx];
                const oldG = pixels[idx + 1];
                const oldB = pixels[idx + 2];
                
                // Find nearest palette color
                const newColor = nearestColor(oldR, oldG, oldB);
                
                // Set new color
                pixels[idx] = newColor[0];
                pixels[idx + 1] = newColor[1];
                pixels[idx + 2] = newColor[2];
                
                // Calculate error
                const errR = oldR - newColor[0];
                const errG = oldG - newColor[1];
                const errB = oldB - newColor[2];
                
                // Distribute error to neighboring pixels (Floyd-Steinberg kernel)
                // Right pixel (x+1, y): 7/16 of error
                if (x + 1 < width) {
                  const rightIdx = (y * width + (x + 1)) * 4;
                  pixels[rightIdx] = Math.max(0, Math.min(255, pixels[rightIdx] + errR * 7 / 16));
                  pixels[rightIdx + 1] = Math.max(0, Math.min(255, pixels[rightIdx + 1] + errG * 7 / 16));
                  pixels[rightIdx + 2] = Math.max(0, Math.min(255, pixels[rightIdx + 2] + errB * 7 / 16));
                }
                
                // Bottom-left pixel (x-1, y+1): 3/16 of error
                if (x > 0 && y + 1 < height) {
                  const blIdx = ((y + 1) * width + (x - 1)) * 4;
                  pixels[blIdx] = Math.max(0, Math.min(255, pixels[blIdx] + errR * 3 / 16));
                  pixels[blIdx + 1] = Math.max(0, Math.min(255, pixels[blIdx + 1] + errG * 3 / 16));
                  pixels[blIdx + 2] = Math.max(0, Math.min(255, pixels[blIdx + 2] + errB * 3 / 16));
                }
                
                // Bottom pixel (x, y+1): 5/16 of error
                if (y + 1 < height) {
                  const bottomIdx = ((y + 1) * width + x) * 4;
                  pixels[bottomIdx] = Math.max(0, Math.min(255, pixels[bottomIdx] + errR * 5 / 16));
                  pixels[bottomIdx + 1] = Math.max(0, Math.min(255, pixels[bottomIdx + 1] + errG * 5 / 16));
                  pixels[bottomIdx + 2] = Math.max(0, Math.min(255, pixels[bottomIdx + 2] + errB * 5 / 16));
                }
                
                // Bottom-right pixel (x+1, y+1): 1/16 of error
                if (x + 1 < width && y + 1 < height) {
                  const brIdx = ((y + 1) * width + (x + 1)) * 4;
                  pixels[brIdx] = Math.max(0, Math.min(255, pixels[brIdx] + errR * 1 / 16));
                  pixels[brIdx + 1] = Math.max(0, Math.min(255, pixels[brIdx + 1] + errG * 1 / 16));
                  pixels[brIdx + 2] = Math.max(0, Math.min(255, pixels[brIdx + 2] + errB * 1 / 16));
                }
              }
            }
            
            return pixels;
          }
          
          // Send encoding status
          send({
            type: "recorder:export-status",
            content: { 
              type: "gif", 
              phase: "encoding", 
              message: "Encoding GIF" 
            }
          });
          
          try {
            // Calculate correct delay to preserve intended duration
          // Use window.gifDurationMs which is already calculated with proper fallbacks
          const intendedDurationMs = window.gifDurationMs || 1000; // Fallback to 1s if not set
          const intendedDurationSeconds = intendedDurationMs / 1000;
          const targetDelayMs = intendedDurationMs / finalFrames.length;
          
          // gifenc expects delay in MILLISECONDS (it converts to centiseconds internally)
          const delayInMilliseconds = Math.floor(targetDelayMs);
          const totalPlaybackSeconds = (finalFrames.length * delayInMilliseconds) / 1000;
          const playbackFps = 1000 / delayInMilliseconds;
          
          console.log(`🎞️ Encoding ${finalFrames.length} frames to match ${intendedDurationSeconds.toFixed(2)}s duration`);
          console.log(`🎞️ Calculated delay: ${delayInMilliseconds}ms (${playbackFps.toFixed(1)}fps)`);
          console.log(`🎞️ GIF will play for ${totalPlaybackSeconds.toFixed(2)}s (${finalFrames.length} frames × ${delayInMilliseconds}ms)`);
          
          // Encode frames with optimized settings
          for (let i = 0; i < finalFrames.length; i++) {
            const frame = finalFrames[i];
            
            // Apply dithering if enabled
            let frameData = frame.data;
            if (useDithering) {
              frameData = floydSteinbergDither(frame.data, palette, frame.width, frame.height);
              
              if (i === 0) {
                console.log(`🎨 Applied Floyd-Steinberg dithering to frames`);
              }
            }
            
            const index = applyPalette(frameData, palette, "rgb565"); // rgb565 for better quality with fixed palette
            
            if (i === 0) {
              console.log(`🎞️ First frame options:`, {
                delay: delayInMilliseconds,
                hasPalette: true,
                repeat: 0,
                transparent: false,
                dispose: -1,
                dithered: useDithering,
                paletteSize: palette.length
              });
            }
            
            gif.writeFrame(index, frame.width, frame.height, {
              palette: i === 0 ? palette : undefined, // Only include palette for first frame (global palette)
              delay: delayInMilliseconds, // gifenc expects MILLISECONDS (converts to centiseconds internally)
              repeat: i === 0 ? 0 : undefined, // Set infinite loop only on first frame
              transparent: false, // Disable transparency for smaller file size
              dispose: -1 // Use default dispose method for better compression
            });
            
            // Progress updates for encoding (40% to 90% of total progress)
            if ((i + 1) % 5 === 0 || i === finalFrames.length - 1) {
              const encodingProgress = (i + 1) / finalFrames.length;
              const totalProgress = 0.4 + (encodingProgress * 0.5); // 40-90% of total
              console.log(`🔄 GIF encoding progress: ${Math.round(encodingProgress * 100)}%`);
              
              // Use the same progress mechanism as MP4 export for consistency
              send({
                type: "recorder:transcode-progress",
                content: totalProgress
              });
              
              // Yield control to allow UI updates (every 5 frames)
              await new Promise(resolve => setTimeout(resolve, 0));
            }
          }
          
          // Send 100% completion progress before generating the blob
          send({
            type: "recorder:transcode-progress",
            content: 1.0
          });
          
          const gifBytes = gif.bytes();
          const blob = new Blob([gifBytes], { type: "image/gif" });
          
          console.log(
            `💾 GIF generated with gifenc: ${Math.round((blob.size / 1024 / 1024) * 100) / 100} MB`,
          );

          const filename = generateTapeFilename("gif");
          receivedDownload({ filename, data: blob });

          console.log("🎬 Animated GIF exported successfully with gifenc!");

          // Send completion events
          send({
            type: "recorder:export-complete",
            content: { 
              type: "gif",
              filename: filename,
              size: blob.size
            }
          });
          
          send({
            type: "signal", 
            content: "recorder:transcoding-done"
          });
          } catch (error) {
            console.error("Error creating animated GIF:", error);
            console.log("🔄 Falling back to static GIF of first frame");

                try {
              const progressBarHeight = 3; // Define progress bar height for fallback
              const canvas = document.createElement("canvas");
              const ctx = canvas.getContext("2d");
              const firstFrame = content.frames[0];

              canvas.width = firstFrame.width;
              canvas.height = firstFrame.height + progressBarHeight;

              const imageData = new ImageData(
                new Uint8ClampedArray(firstFrame.data),
                firstFrame.width,
                firstFrame.height,
              );

              ctx.putImageData(imageData, 0, 0);

              // Add cyan crosshair cursor overlay if pen data is available
              if (firstFrame.penData) {
                addCyanCrosshair(ctx, canvas.width, canvas.height, firstFrame.penData, 1); // No scaling for fallback
              }

              // Add sideways AC stamp to fallback GIF as well (await to ensure fonts are loaded)
              await addAestheticComputerStamp(
                ctx,
                firstFrame.width,
                firstFrame.height + progressBarHeight,
                0,
                firstFrame.data,
                firstFrame,
              );

              const gifBlob = await new Promise((resolve) => {
                canvas.toBlob(resolve, "image/gif");
              });

              const filename = generateTapeFilename("gif", "-static");
              receivedDownload({ filename, data: gifBlob });

              console.log("📸 Static GIF fallback exported successfully");
            } catch (fallbackError) {
              console.error("Error in fallback GIF creation:", fallbackError);
            }
          }
      } catch (error) {
        console.error("Error in GIF processing:", error);
      }
      return;
    }

    // 🎬 Handle MP4 frame chunks
    if (type === "create-animated-mp4-chunk") {
      if (window.mp4FrameChunks) {
        window.mp4FrameChunks.push(content.frames);
        window.mp4ReceivedChunks++;
        
        console.log(
          `🎞️ Received MP4 chunk ${window.mp4ReceivedChunks}/${window.mp4TotalChunks}:`,
          content.frames.length,
          "frames"
        );

        if (window.mp4ReceivedChunks === window.mp4TotalChunks) {
          console.log("🎞️ All MP4 chunks received, reassembling...");
          const allFrames = window.mp4FrameChunks.flat();
          console.log("🎞️ Total frames after reassembly:", allFrames.length);

          delete window.mp4FrameChunks;
          delete window.mp4TotalChunks;
          delete window.mp4ReceivedChunks;

          // Process reassembled frames by calling handler recursively
          receivedChange({
            data: {
              type: "create-animated-mp4",
              content: { frames: allFrames }
            }
          });
        }
      }
      return;
    }

    // 🎬 Create animated MP4 from frame data (same pipeline as GIF)
    if (type === "create-animated-mp4") {
      if (content.totalChunks && content.totalChunks > 1) {
        console.log(
          `🎞️ Receiving MP4 frames in chunks (1/${content.totalChunks}):`,
          content.frames.length,
          "frames"
        );
        window.mp4FrameChunks = [content.frames];
        window.mp4TotalChunks = content.totalChunks;
        window.mp4ReceivedChunks = 1;
        return;
      }

      console.log(
        "🎞️ Creating animated MP4 from",
        content.frames.length,
        "frames",
      );

      try {
        if (content.frames.length === 0) {
          console.warn("No frames provided for MP4 creation");
          return;
        }

        console.log("🔄 Creating MP4 with MediaRecorder API");

        // Send initial progress to show the progress bar immediately
        send({
          type: "recorder:transcode-progress",
          content: 0.01, // 1% to start
        });

        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "preparing", 
            message: "Preparing MP4 export" 
          }
        });

        // Use higher scaling for clean mode: 6x vs 3x for ultra crisp output
        const originalWidth = content.frames[0].width;
        const originalHeight = content.frames[0].height;
        const optimalScale = window.currentRecordingOptions?.cleanMode ? 6 : 3;

        console.log(
          `📏 Using ${optimalScale}x scaling for ${window.currentRecordingOptions?.cleanMode ? 'ultra-crisp clean mode' : 'standard'} MP4: ${originalWidth}x${originalHeight} -> ${originalWidth * optimalScale}x${originalHeight * optimalScale}`,
        );

        // Create canvas for frame processing (same as GIF)
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");
        canvas.width = originalWidth * optimalScale;
        canvas.height = originalHeight * optimalScale;
        ctx.imageSmoothingEnabled = false;

        // Use fixed 60fps for MP4 files to avoid timing issues
        const targetFrameRate = 60;
        const targetFrameInterval = 1000 / targetFrameRate; // 16.67ms per frame
        
        // Calculate original recording timing for reference
        let totalOriginalDuration = 0;
        if (content.frames.length > 1) {
          totalOriginalDuration = content.frames[content.frames.length - 1].timestamp - content.frames[0].timestamp;
        } else if (content.frames.length === 1) {
          totalOriginalDuration = 100; // Default 100ms for single frame
        }
        
        // For MP4 export, we need to match the audio timing exactly
        // Calculate the original recording frame rate and duration
        const recordingDuration = content.frames[content.frames.length - 1].timestamp - content.frames[0].timestamp;
        const originalFrameRate = Math.round((content.frames.length / recordingDuration) * 1000);
        
        // 🎯 Resample frames to target frame rate for optimal MP4 playback
        // Use same logic as GIF to handle high refresh rate displays (120Hz -> 60fps)
        let processedFrames = content.frames;
        const targetMp4FPS = 60; // Target 60fps for smooth MP4 playback
        
        if (originalFrameRate > targetMp4FPS && recordingDuration > 0) {
          // Calculate how many frames we need for target fps
          const targetFrameCount = Math.round((recordingDuration / 1000) * targetMp4FPS);
          processedFrames = [];
          
          console.log(`🎬 Resampling MP4 from ${originalFrameRate}fps to ${targetMp4FPS}fps (${content.frames.length} -> ${targetFrameCount} frames)`);
          
          // Properly resample frames evenly across the entire duration
          for (let i = 0; i < targetFrameCount; i++) {
            // Calculate the exact timestamp we want for this frame
            // Ensure we span from first frame to last frame inclusive
            let targetTimestamp;
            if (i === targetFrameCount - 1) {
              // For the last frame, use the actual last timestamp to ensure we capture the end
              targetTimestamp = content.frames[content.frames.length - 1].timestamp;
            } else {
              // For all other frames, distribute evenly across the duration
              targetTimestamp = content.frames[0].timestamp + (i / (targetFrameCount - 1)) * recordingDuration;
            }
            
            // Find the closest source frame to this timestamp
            let closestIndex = 0;
            let minDistance = Math.abs(content.frames[0].timestamp - targetTimestamp);
            
            for (let j = 1; j < content.frames.length; j++) {
              const distance = Math.abs(content.frames[j].timestamp - targetTimestamp);
              if (distance < minDistance) {
                minDistance = distance;
                closestIndex = j;
              }
            }
            
            processedFrames.push(content.frames[closestIndex]);
            
            // Send progress updates during resampling (first 5% of total progress)
            if (i % 100 === 0 || i === targetFrameCount - 1) {
              const resampleProgress = (i / targetFrameCount) * 0.05; // 0-5%
              send({
                type: "recorder:transcode-progress",
                content: resampleProgress,
              });
            }
            
            // Debug logging for first and last few frames
            if (i < 3 || i >= targetFrameCount - 3) {
              console.log(`🎬 Frame ${i}: target=${targetTimestamp.toFixed(1)}ms, closest=${content.frames[closestIndex].timestamp.toFixed(1)}ms, index=${closestIndex}`);
            }
          }
          
          console.log(`🎬 MP4 frame count: ${content.frames.length} -> ${processedFrames.length} frames`);
        } else {
          console.log(`🎬 No resampling needed: ${originalFrameRate}fps <= ${targetMp4FPS}fps target`);
        }

        // Use consistent 60fps timing for smooth playback
        const totalFrames = processedFrames.length;
        const exportFrameRate = 60; // 60fps for smooth playback
        const frameDuration = 1000 / exportFrameRate; // 16.67ms per frame
        
        console.log(
          `🎬 MP4 export timing: ${recordingDuration.toFixed(1)}ms original, ${totalFrames} frames`
        );
        console.log(
          `🎬 Export frame rate: ${exportFrameRate}fps (${frameDuration.toFixed(1)}ms per frame)`
        );

        // Pre-render all scaled frames with stamps before encoding
        console.log("🎨 Setting up streaming frame processing (render-on-demand)...");
        
        // Instead of pre-rendering all frames, we'll render them on-demand during encoding
        // This saves massive amounts of memory for high-resolution exports
        
        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "rendering", 
            message: "Pre-rendering frames" 
          }
        });

        // Instead of pre-rendering all frames, we'll render them on-demand during encoding
        // This saves massive amounts of memory for high-resolution exports
        
        // Pixel-perfect upscaling function (same as GIF export)
        function upscalePixels(imageData, originalWidth, originalHeight, scale) {
          if (scale === 1) {
            return imageData;
          }

          const scaledWidth = originalWidth * scale;
          const scaledHeight = originalHeight * scale;
          const scaledImageData = new Uint8ClampedArray(
            scaledWidth * scaledHeight * 4,
          );

          for (let y = 0; y < scaledHeight; y++) {
            for (let x = 0; x < scaledWidth; x++) {
              const sourceX = Math.floor(x / scale);
              const sourceY = Math.floor(y / scale);
              const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
              const targetIndex = (y * scaledWidth + x) * 4;

              scaledImageData[targetIndex] = imageData[sourceIndex]; // R
              scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
              scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
              scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
            }
          }

          return scaledImageData;
        }
        
        // Function to render a specific frame on-demand
        async function renderFrameOnDemand(frameIndex) {
          const frame = processedFrames[frameIndex];
          if (!frame) return null;

          // Create temporary canvas for adding progress bar at original size (before scaling)
          const tempCanvas = document.createElement("canvas");
          const tempCtx = tempCanvas.getContext("2d");
          tempCanvas.width = frame.width;
          tempCanvas.height = frame.height;
          
          // Put original frame data
          const originalImageData = new Uint8ClampedArray(frame.data);
          const tempImageData = new ImageData(originalImageData, frame.width, frame.height);
          tempCtx.putImageData(tempImageData, 0, 0);
          
          // Add 2px VHS-style progress bar at the bottom (before scaling)
          // Skip progress bar in clean mode (tape:neat recordings)
          if (!window.currentRecordingOptions?.cleanMode) {
            const barProgress = (frameIndex + 1) / processedFrames.length;
            
            // Use shared VHS progress bar renderer
            renderVHSProgressBar({
              ctx: tempCtx,
              width: frame.width,
              height: frame.height,
              imageData: originalImageData,
              progress: barProgress,
              animFrame: frameIndex,
            });
          }
          
          // Get the frame data with progress bar for scaling
          const frameWithProgressBar = tempCtx.getImageData(0, 0, frame.width, frame.height);

          // Now scale up the frame WITH progress bar using pixel-perfect upscaling
          const scaledImageData = upscalePixels(
            frameWithProgressBar.data,
            frame.width,
            frame.height,
            optimalScale,
          );

          // Create a canvas at the SCALED size for adding stamps
          const scaledCanvas = document.createElement("canvas");
          const scaledCtx = scaledCanvas.getContext("2d");
          scaledCanvas.width = originalWidth * optimalScale;
          scaledCanvas.height = originalHeight * optimalScale;
          
          // Put the scaled frame data onto the canvas
          const scaledImageDataObj = new ImageData(
            scaledImageData,
            originalWidth * optimalScale,
            originalHeight * optimalScale,
          );
          scaledCtx.putImageData(scaledImageDataObj, 0, 0);

          // Now add the stamp at the SCALED dimensions (so text/bars are correct size)
          const progress = (frameIndex + 1) / processedFrames.length;
          await addAestheticComputerStamp(
            scaledCtx,
            originalWidth * optimalScale,
            originalHeight * optimalScale,
            progress,
            scaledImageData, // Pass scaled data
            frame,
            frameIndex,
            processedFrames.length
          );

          // Get the final ImageData with stamps
          const finalImageData = scaledCtx.getImageData(
            0, 0, 
            originalWidth * optimalScale,
            originalHeight * optimalScale
          );
          
          // Clean up temporary canvases immediately
          tempCanvas.width = 1;
          tempCanvas.height = 1;
          scaledCanvas.width = 1;
          scaledCanvas.height = 1;
          
          return finalImageData;
        }

        const preRenderedFrames = null; // No longer pre-rendering frames
        
        console.log(`✅ Ready for streaming frame processing of ${processedFrames.length} frames`);

        // Send encoding status
        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "encoding", 
            message: "Encoding MP4" 
          }
        });

        // Create MediaRecorder for MP4 export with H.264 codec
        const canvasStream = canvas.captureStream(targetFrameRate);
        
        // For MP4 export, we'll create video-only first, then combine with audio using Web Audio API
        // This avoids the sync issues of real-time recording
        
        let mimeType;
        // Detect browser for codec selection
        const isSafari = /^((?!chrome|android).)*safari/i.test(navigator.userAgent);
        const isChrome = /chrome/i.test(navigator.userAgent) && !isSafari;
        
        console.log(`🔍 Browser detection: Safari=${isSafari}, Chrome=${isChrome}`);
        
        // Browser-specific codec candidates
        let mp4Candidates;
        
        if (isSafari) {
          // Safari on macOS has better codec support
          mp4Candidates = [
            "video/mp4; codecs=avc1.640028,mp4a.40.2", // H.264 High + AAC-LC (Twitter optimal)
            "video/mp4; codecs=avc1.4D401E,mp4a.40.2", // H.264 Main + AAC-LC
            "video/mp4; codecs=avc1.42E01E,mp4a.40.2", // H.264 Baseline + AAC-LC
            "video/mp4; codecs=avc1.640028", // H.264 High only
            "video/mp4; codecs=avc1.4D401E", // H.264 Main only
            "video/mp4; codecs=avc1.42E01E", // H.264 Baseline only
            "video/mp4", // Generic MP4
          ];
        } else {
          // Chrome/Chromium - more conservative approach
          mp4Candidates = [
            "video/mp4; codecs=avc1.42E01E", // H.264 Baseline only (most compatible)
            "video/mp4; codecs=avc1.4D401E", // H.264 Main Profile 
            "video/mp4; codecs=avc1.42001E", // H.264 Baseline 3.0
            "video/mp4", // Generic MP4 (let browser choose audio codec)
            "video/mp4; codecs=avc1.42E01E,mp4a.40.5", // H.264 + AAC-HE (sometimes works)
            "video/mp4; codecs=avc1.42E01E,mp4a.40.2", // H.264 + AAC-LC (often fails in Chrome)
          ];
        }
        
        let mp4Found = false;
        for (const candidate of mp4Candidates) {
          console.log(`🔍 Testing codec: ${candidate}`);
          if (MediaRecorder.isTypeSupported(candidate)) {
            mimeType = candidate;
            mp4Found = true;
            console.log(`✅ Using MP4 codec: ${candidate}`);
            console.log(`🎯 Browser: ${isSafari ? 'Safari' : isChrome ? 'Chrome' : 'Unknown'}`);
            break;
          } else {
            console.log(`❌ Codec not supported: ${candidate}`);
          }
        }
        
        // Fallback to WebM if no MP4 works
        if (!mp4Found) {
          if (MediaRecorder.isTypeSupported("video/webm; codecs=vp9,opus")) {
            mimeType = "video/webm; codecs=vp9,opus"; // VP9 + Opus (high quality)
            console.log("⚠️ MP4 not supported, using WebM VP9 + Opus");
          } else if (MediaRecorder.isTypeSupported("video/webm; codecs=vp8,opus")) {
            mimeType = "video/webm; codecs=vp8,opus"; // VP8 + Opus (compatible)
            console.log("⚠️ MP4 not supported, using WebM VP8 + Opus");
          } else if (MediaRecorder.isTypeSupported("video/webm")) {
            mimeType = "video/webm"; // WebM fallback
            console.log("⚠️ Falling back to WebM (default codecs)");
          } else if (MediaRecorder.isTypeSupported("video/webm; codecs=vp8")) {
            mimeType = "video/webm; codecs=vp8"; // VP8 fallback (avoid VP9)
            console.log("⚠️ Falling back to WebM VP8");
          } else {
            console.error("🔴 No supported video mimetypes found");
            return;
          }
        }

        // Check if we have recorded audio for combination
        let hasRecordedAudio = false;
        let audioBuffer = null;
        
        console.log("🔊 Checking for recorded audio...");
        console.log("🔊 sfx[\"tape:audio\"] exists:", !!sfx["tape:audio"]);
        console.log("🔊 sfx[\"tape:audio\"] type:", typeof sfx["tape:audio"]);
        console.log("🔊 sfx[\"tape:audio\"] constructor:", sfx["tape:audio"]?.constructor?.name);
        
        if (sfx["tape:audio"]) {
          try {
            // Check if it's already an AudioBuffer
            if (sfx["tape:audio"] instanceof AudioBuffer) {
              console.log("🔊 ✅ Audio is already an AudioBuffer!");
              audioBuffer = sfx["tape:audio"];
              hasRecordedAudio = true;
              console.log("🔊 Audio buffer duration:", audioBuffer.duration, "seconds");
              console.log("🔊 Audio buffer channels:", audioBuffer.numberOfChannels);
              console.log("🔊 Audio buffer sample rate:", audioBuffer.sampleRate);
              console.log("🔊 Recorded audio available for MP4 export");
            } else if (sfx["tape:audio"] instanceof ArrayBuffer || (sfx["tape:audio"] && sfx["tape:audio"].byteLength !== undefined)) {
              console.log("🔊 Audio is ArrayBuffer, decoding...");
              // Create a temporary audio context to decode the audio
              const tempAudioContext = new AudioContext();
              console.log("🔊 Temp audio context created, sample rate:", tempAudioContext.sampleRate);
              
              console.log("🔊 Starting audio decode...");
              audioBuffer = await tempAudioContext.decodeAudioData(sfx["tape:audio"].slice());
              hasRecordedAudio = true;
              console.log("🔊 ✅ Audio decoded successfully!");
              console.log("🔊 Audio buffer duration:", audioBuffer.duration, "seconds");
              console.log("🔊 Audio buffer channels:", audioBuffer.numberOfChannels);
              console.log("🔊 Audio buffer sample rate:", audioBuffer.sampleRate);
              console.log("🔊 Recorded audio available for MP4 export");
            } else {
              console.log("🔊 ❌ Unknown audio format:", typeof sfx["tape:audio"], sfx["tape:audio"]?.constructor?.name);
            }
          } catch (error) {
            console.error("🔊 ❌ Failed to process recorded audio:", error);
            console.log("🔊 Error details:", error.message);
          }
        } else {
          console.log("🔊 ❌ No recorded audio found in sfx[\"tape:audio\"]");
        }

        // If we have audio, create a combined stream; otherwise video-only
        let finalStream = canvasStream;
        let audioDestination = null;
        
        console.log("🔊 Setting up final stream...");
        console.log("🔊 hasRecordedAudio:", hasRecordedAudio);
        console.log("🔊 audioBuffer exists:", !!audioBuffer);
        
        if (hasRecordedAudio && audioBuffer) {
          try {
            console.log("🔊 Creating export audio context...");
            // Create audio context for export
            const exportAudioContext = new AudioContext();
            console.log("🔊 Export audio context created, sample rate:", exportAudioContext.sampleRate);
            console.log("🔊 Export audio context state:", exportAudioContext.state);
            
            // Resume audio context if suspended with timeout
            if (exportAudioContext.state === 'suspended') {
              console.log("🔊 Resuming suspended audio context...");
              try {
                // Add timeout to prevent hanging - increased for longer recordings
                const resumePromise = exportAudioContext.resume();
                const timeoutPromise = new Promise((_, reject) => 
                  setTimeout(() => reject(new Error('Audio context resume timeout')), 15000) // Increased to 15 seconds
                );
                
                await Promise.race([resumePromise, timeoutPromise]);
                console.log("🔊 Audio context resumed, state:", exportAudioContext.state);
              } catch (error) {
                console.warn("🔊 ⚠️ Audio context resume failed:", error.message);
                // Continue without audio if resume fails
                hasRecordedAudio = false;
                audioBuffer = null;
                exportAudioContext.close();
              }
            }
            
            if (hasRecordedAudio && audioBuffer) {
              audioDestination = exportAudioContext.createMediaStreamDestination();
              console.log("🔊 Audio destination created");
              console.log("🔊 Audio destination stream:", audioDestination.stream);
              console.log("🔊 Audio destination stream tracks:", audioDestination.stream.getTracks().length);
            }
            
            // Create combined stream
            finalStream = new MediaStream();
            console.log("🔊 Combined stream created");
            
            // Add video tracks
            const videoTracks = canvasStream.getVideoTracks();
            console.log("🔊 Video tracks found:", videoTracks.length);
            videoTracks.forEach((track, index) => {
              finalStream.addTrack(track);
              console.log(`🔊 Added video track ${index}:`, track.kind, track.enabled, track.readyState);
            });
            
            // Add audio tracks from our export destination
            const audioTracks = audioDestination.stream.getAudioTracks();
            console.log("🔊 Audio tracks found:", audioTracks.length);
            audioTracks.forEach((track, index) => {
              finalStream.addTrack(track);
              console.log(`🔊 Added audio track ${index}:`, track.kind, track.enabled, track.readyState);
            });
            
            console.log("🔊 Final stream tracks:", finalStream.getTracks().length);
            console.log("🔊 Final stream video tracks:", finalStream.getVideoTracks().length);
            console.log("🔊 Final stream audio tracks:", finalStream.getAudioTracks().length);
            
            console.log("🔊 ✅ Created combined audio/video stream for MP4 export");
          } catch (error) {
            console.error("🔊 ❌ Failed to create combined stream, falling back to video-only:", error);
            console.log("🔊 Error details:", error.message);
            finalStream = canvasStream;
            hasRecordedAudio = false;
          }
        } else {
          console.log("🔊 Using video-only stream (no audio available)");
        }


        // Test codec support before creating MediaRecorder
        console.log("🔊 Testing codec support...");
        console.log("🔊 Original mimeType:", mimeType);
        console.log("🔊 Codec support test:", MediaRecorder.isTypeSupported(mimeType));

        let selectedMimeType = mimeType;
        let mp4Supported = MediaRecorder.isTypeSupported(mimeType);
        let webmVp8Opus = "video/webm; codecs=vp8,opus";
        let webmVp9Opus = "video/webm; codecs=vp9,opus";
        let webmBasic = "video/webm";
        let fallbackUsed = false;

        if (!mp4Supported) {
          console.warn("🔴 MP4/H.264/AAC not supported for MediaRecorder in this browser. Falling back to WebM/VP8+Opus.");
          if (MediaRecorder.isTypeSupported(webmVp8Opus)) {
            selectedMimeType = webmVp8Opus;
            fallbackUsed = true;
            console.log("🔊 ✅ Using WebM/VP8+Opus");
          } else if (MediaRecorder.isTypeSupported(webmVp9Opus)) {
            selectedMimeType = webmVp9Opus;
            fallbackUsed = true;
            console.log("🔊 ✅ Using WebM/VP9+Opus");
          } else if (MediaRecorder.isTypeSupported(webmBasic)) {
            selectedMimeType = webmBasic;
            fallbackUsed = true;
            console.log("🔊 ✅ Using basic WebM");
          } else {
            selectedMimeType = "";
            fallbackUsed = true;
            console.warn("🔴 No supported codecs found for MediaRecorder. Browser default will be used (may fail).");
          }
        }

        // Adaptive bitrate based on content and duration
        // For shorter recordings, we can afford higher quality
        // For longer recordings, reduce bitrate to keep file sizes manageable
        const estimatedDuration = (mediaRecorderDuration || 5000) / 1000; // Convert to seconds
        const frameCount = recordedFrames?.length || 300; // Estimate if not available
        
        // Analyze content complexity by sampling frame differences (only for shorter recordings)
        let contentComplexity = 1.0; // Default complexity multiplier
        
        // Skip complex analysis for longer recordings to avoid issues
        if (recordedFrames && recordedFrames.length > 10 && estimatedDuration <= 30) {
          try {
            let totalDifference = 0;
            let samples = 0;
            const sampleStep = Math.max(1, Math.floor(recordedFrames.length / 10)); // Sample every N frames
            
            for (let i = sampleStep; i < recordedFrames.length && samples < 10; i += sampleStep) {
              const prevFrame = recordedFrames[i - sampleStep][1];
              const currentFrame = recordedFrames[i][1];
              
              // Safety check for valid frame data
              if (!prevFrame?.data || !currentFrame?.data) continue;
              
              // Quick difference check using a subset of pixels
              let pixelDiff = 0;
              const checkStep = 100; // Check every 100th pixel for speed
              const maxPixels = Math.min(prevFrame.data.length, currentFrame.data.length);
              
              for (let p = 0; p < maxPixels; p += checkStep * 4) {
                pixelDiff += Math.abs(prevFrame.data[p] - currentFrame.data[p]);
              }
              totalDifference += pixelDiff;
              samples++;
            }
            
            if (samples > 0) {
              const avgDifference = totalDifference / samples;
              // Normalize complexity (typical values range from 0-10000+)
              contentComplexity = Math.min(2.0, Math.max(0.5, avgDifference / 5000));
            }
          } catch (error) {
            console.warn("⚠️ Content complexity analysis failed, using default:", error.message);
            contentComplexity = 1.0; // Fallback to default
          }
        }
        
        if (debug) console.log(`📊 Content complexity analysis: ${contentComplexity.toFixed(2)}x (1.0 = normal, >1.0 = high motion/detail)`);
        
        let videoBitrate;
        let audioBitrate = 128000; // 128 kbps - good quality audio
        
        // Clean mode: Use highest quality settings regardless of duration
        if (window.currentRecordingOptions?.cleanMode) {
          videoBitrate = Math.round(200000000 * contentComplexity); // 200 Mbps base for clean mode
          audioBitrate = 320000; // 320 kbps audio for clean mode (ultra high quality)
          videoBitrate = Math.min(videoBitrate, 200000000); // Max 200 Mbps for clean mode
          console.log(`🎬 🧹 Clean mode: Using ultra-high bitrate ${(videoBitrate/1000000).toFixed(1)}Mbps video, ${audioBitrate/1000}kbps audio`);
        } else if (estimatedDuration <= 10) {
          // Short recordings (≤10s): High quality
          videoBitrate = Math.round(24000000 * contentComplexity); // 24 Mbps base
        } else if (estimatedDuration <= 30) {
          // Medium recordings (10-30s): High quality
          videoBitrate = Math.round(24000000 * contentComplexity);  // 24 Mbps base
        } else {
          // Longer recordings (>30s): Maintain good quality
          if (estimatedDuration <= 60) {
            videoBitrate = 24000000;  // Fixed 24 Mbps for 30-60s recordings
          } else {
            videoBitrate = 24000000;  // Fixed 24 Mbps for very long recordings
            audioBitrate = 128000;    // Keep 128 kbps audio for consistency
          }
        }
        
        // Ensure minimum 24 Mbps for all non-clean recordings
        if (!window.currentRecordingOptions?.cleanMode) {
          videoBitrate = Math.max(videoBitrate, 24000000); // Minimum 24 Mbps
        }
        
        if (debug) console.log(`📊 Adaptive bitrate: ${(videoBitrate/1000000).toFixed(1)}Mbps video, ${audioBitrate/1000}kbps audio (duration: ${estimatedDuration.toFixed(1)}s)`);

        const recorderOptions = selectedMimeType ? {
          mimeType: selectedMimeType,
          videoBitsPerSecond: videoBitrate,
          audioBitsPerSecond: audioBitrate,
        } : {
          videoBitsPerSecond: videoBitrate,
          audioBitsPerSecond: audioBitrate,
        };

        // Add frame rate constraint for consistent 60fps output
        if (finalStream.getVideoTracks().length > 0) {
          const videoTrack = finalStream.getVideoTracks()[0];
          const settings = videoTrack.getSettings();
          if (settings.frameRate && settings.frameRate > 60) {
            console.log(`🎬 Constraining video track from ${settings.frameRate}fps to 60fps`);
            videoTrack.applyConstraints({
              frameRate: { ideal: 60, max: 60 }
            }).catch(err => console.warn("Failed to apply frame rate constraint:", err));
          }
        }

        if (fallbackUsed) {
          console.warn("⚠️ Only WebM output is possible in this browser. If you need MP4, you must transcode after recording.");
        }

        console.log("🔊 Creating MediaRecorder with options:", recorderOptions);
        const videoRecorder = new MediaRecorder(finalStream, recorderOptions);
        
        console.log("🔊 MediaRecorder created");
        console.log("🔊 MediaRecorder mimeType:", videoRecorder.mimeType);
        console.log("🔊 MediaRecorder stream:", videoRecorder.stream);
        console.log("🔊 MediaRecorder stream tracks:", videoRecorder.stream.getTracks().length);
        console.log("🔊 MediaRecorder stream video tracks:", videoRecorder.stream.getVideoTracks().length);
        console.log("🔊 MediaRecorder stream audio tracks:", videoRecorder.stream.getAudioTracks().length);
        
        // Debug track states
        finalStream.getTracks().forEach((track, index) => {
          console.log(`🔊 Track ${index}: ${track.kind} enabled=${track.enabled} muted=${track.muted} readyState=${track.readyState}`);
        });

        const chunks = [];
        let chunkCount = 0;
        videoRecorder.ondataavailable = function (e) {
          if (e.data && e.data.size > 0) {
            chunks.push(e.data);
            chunkCount++;
            if (chunkCount % 50 === 0) {
              console.log(`📦 Received ${chunkCount} data chunks (${(e.data.size / 1024).toFixed(1)}KB each)`);
            }
          }
        };

        // Add error handling 
        videoRecorder.onerror = function (e) {
          console.error("MediaRecorder error:", e);
          console.error("Error details:", e.error);
          console.error("Used mimeType:", selectedMimeType);
          console.error("MediaRecorder state:", videoRecorder.state);
        };

        // Add state change monitoring
        videoRecorder.onstart = function () {
          // MediaRecorder started successfully
        };

        videoRecorder.onpause = function () {
          // MediaRecorder paused
        };

        videoRecorder.onresume = function () {
          console.log("🎬 ▶️ MediaRecorder resumed");
        };

        // Send encoding status
        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "encoding", 
            message: "Encoding MP4" 
          }
        });

        // Set up audio source if we have recorded audio
        let audioSource = null;
        let audioStarted = false;
        
        console.log("🔊 Setting up audio source...");
        console.log("🔊 hasRecordedAudio:", hasRecordedAudio);
        console.log("🔊 audioBuffer exists:", !!audioBuffer);
        console.log("🔊 audioDestination exists:", !!audioDestination);
        
        if (hasRecordedAudio && audioBuffer && audioDestination) {
          try {
            console.log("🔊 Creating audio source from buffer...");
            // Create audio source from the decoded buffer
            audioSource = audioDestination.context.createBufferSource();
            audioSource.buffer = audioBuffer;
            audioSource.connect(audioDestination);
            
            console.log("🔊 Audio source created and connected");
            console.log("🔊 Audio source buffer:", audioSource.buffer);
            console.log("🔊 Audio source context:", audioSource.context);
            console.log("🔊 Audio source context state:", audioSource.context.state);
            console.log("🔊 ✅ Audio source prepared for synchronized playback");
          } catch (error) {
            console.error("🔊 ❌ Failed to create audio source:", error);
            console.log("🔊 Error details:", error.message);
            audioSource = null;
          }
        } else {
          console.log("🔊 ❌ Cannot create audio source - missing requirements");
          console.log("🔊   hasRecordedAudio:", hasRecordedAudio);
          console.log("🔊   audioBuffer:", !!audioBuffer);
          console.log("🔊   audioDestination:", !!audioDestination);
        }

        // Fast playback of pre-rendered frames with synchronized audio
        let frameIndex = 0;
        const audioBufferDuration = audioBuffer ? audioBuffer.duration * 1000 : processedFrames.length * frameDuration;
        
        // Use original frame timing for perfect sync instead of artificial even spacing
        const useOriginalTiming = content.frames && content.frames.length > 0 && content.frames[0].timestamp !== undefined;
        
        if (useOriginalTiming) {
          console.log("🔊 Using original frame timing for perfect audio sync");
          console.log("🔊 Audio duration:", audioBufferDuration.toFixed(1), "ms");
          console.log("🔊 Original recording duration:", (content.frames[content.frames.length - 1].timestamp - content.frames[0].timestamp).toFixed(1), "ms");
        } else {
          console.log("🔊 Using calculated frame timing - audioDuration:", audioBufferDuration.toFixed(1), "ms");
          console.log("🔊 Frame timing setup - actualFrameDuration:", (audioBufferDuration / processedFrames.length).toFixed(3), "ms");
        }
        console.log("🔊 Frame timing setup - totalFrames:", processedFrames.length);
        
        // Track timing for frame synchronization
        let startTime;
        
        // Define the frame rendering function with streaming approach
        async function renderNextFrame() {
          if (frameIndex >= processedFrames.length) {
            console.log(`🎬 ✅ Rendered all ${frameIndex} frames for MP4 export`);
            try {
              videoRecorder.stop();
            } catch (error) {
              console.error("Error stopping MediaRecorder:", error);
            }
            return;
          }

          // Start audio playback on first frame for perfect sync
          if (!audioStarted && audioSource) {
            try {
              // Resume audio context if suspended
              if (audioSource.context.state === 'suspended') {
                const resumeStartTime = performance.now();
                audioSource.context.resume().then(() => {
                  const resumeEndTime = performance.now();
                  window.audioContextResumeTimestamps.requestedAt = resumeStartTime;
                  window.audioContextResumeTimestamps.completedAt = resumeEndTime;
                  audioSource.start(0);
                  audioStarted = true;
                }).catch((resumeError) => {
                  console.error("Failed to resume audio context:", resumeError);
                  // Try to start anyway
                  audioSource.start(0);
                  audioStarted = true;
                });
              } else {
                audioSource.start(0);
                audioStarted = true;
              }
            } catch (error) {
              console.error("Failed to start audio source:", error);
            }
          }

          // Render frame on-demand to save memory
          const frameImageData = await renderFrameOnDemand(frameIndex);
          if (frameImageData) {
            ctx.putImageData(frameImageData, 0, 0);
          } else {
            console.warn(`Failed to render frame ${frameIndex}, using blank frame`);
            ctx.fillStyle = 'black';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
          }
          
          // Log every 100 frames to track progress
          if (frameIndex % 100 === 0) {
            console.log(`🎬 Rendered frame ${frameIndex}/${processedFrames.length}`);
          }
          
          // Send progress update
          if (frameIndex % 30 === 0 || frameIndex === processedFrames.length - 1) {
            const progress = (frameIndex + 1) / processedFrames.length;
            
            send({
              type: "recorder:transcode-progress",
              content: 0.05 + (progress * 0.90), // 5% to 95% (resampling was 0-5%)
            });
          }

          frameIndex++;
          
          // Calculate next frame time - force 60fps output timing
          const targetFrameRate = 60;
          const frameDuration = 1000 / targetFrameRate; // 16.67ms per frame
          
          let targetTime;
          if (useOriginalTiming && frameIndex < content.frames.length) {
            // Use original timing but clamp to 60fps maximum
            const baseTimestamp = content.frames[0].timestamp;
            const currentFrameTimestamp = content.frames[Math.min(frameIndex, content.frames.length - 1)].timestamp;
            const originalDelta = currentFrameTimestamp - baseTimestamp;
            
            // Ensure we don't exceed 60fps (minimum 16.67ms between frames)
            const minFrameTime = frameIndex * frameDuration;
            targetTime = Math.max(originalDelta, minFrameTime);
          } else {
            // Force exactly 60fps timing
            targetTime = frameIndex * frameDuration;
          }
          
          const currentTime = performance.now() - startTime;
          const delay = Math.max(0, targetTime - currentTime);
          
          setTimeout(renderNextFrame, delay);
        }

        // Handle recording completion - define this before starting
        videoRecorder.onstop = async function () {
          console.log(`🎬 MediaRecorder stopped - received ${chunks.length} chunks, total size: ${(chunks.reduce((sum, c) => sum + c.size, 0) / 1024 / 1024).toFixed(2)}MB`);
          
          // Clean up audio resources
          if (audioSource) {
            try {
              audioSource.disconnect();
            } catch (error) {
              // Ignore disconnect errors as they're usually normal
            }
          }
          if (audioDestination) {
            try {
              await audioDestination.context.close();
            } catch (error) {
              // Ignore close errors as they're usually normal
            }
          }
          
          send({
            type: "recorder:export-status",
            content: { 
              type: "video", 
              phase: "finalizing", 
              message: "Finalizing MP4" 
            }
          });

          const blob = new Blob(chunks, { type: mimeType });

          // Determine file extension based on actual codec used
          const extension = selectedMimeType.includes('webm') ? 'webm' : 'mp4';
          const filename = generateTapeFilename(extension);
          console.log("🎬 Animated MP4 Export - currentRecordingOptions:", JSON.stringify(window.currentRecordingOptions, null, 2));
          
          receivedDownload({ filename, data: blob });

          console.log(`🎬 ✅ Animated ${extension.toUpperCase()} exported successfully! ${processedFrames.length} frames → ${blob.size / 1024 / 1024}MB`);


          // Send completion message to video piece
          send({
            type: "recorder:export-complete",
            content: { type: "video", filename }
          });
        };
        
        // Start recording and begin frame rendering after a small delay
        console.log("🎬 Starting MediaRecorder...");
        console.log("🎬 MediaRecorder state before start:", videoRecorder.state);
        console.log("🎬 Stream active before start:", finalStream.active);
        console.log("🎬 Stream tracks before start:", finalStream.getTracks().map(t => `${t.kind}: ${t.readyState}`));
        
        try {
          console.log("🎬 About to start MediaRecorder with timeslice:", 100);
          console.log("🎬 MediaRecorder mimeType:", videoRecorder.mimeType);
          console.log("🎬 Final stream tracks before start:", finalStream.getTracks().length);
          console.log("🎬 Final stream video tracks before start:", finalStream.getVideoTracks().length);
          console.log("🎬 Final stream audio tracks before start:", finalStream.getAudioTracks().length);
          
          videoRecorder.start(100);
          
          console.log("🎬 MediaRecorder state after start():", videoRecorder.state);
          console.log("🎬 Stream active after start:", finalStream.active);
          
          // Verify tracks are still active after MediaRecorder start
          console.log("🎬 Final stream tracks after start:", finalStream.getTracks().length);
          finalStream.getTracks().forEach((track, index) => {
            console.log(`🎬 Track ${index} after start: ${track.kind} enabled=${track.enabled} muted=${track.muted} readyState=${track.readyState}`);
          });
          
          // Wait for MediaRecorder to start before beginning frame rendering
          setTimeout(() => {
            console.log("🎬 Beginning frame rendering...");
            console.log("🎬 MediaRecorder state at frame start:", videoRecorder.state);
            console.log("🎬 Stream active at frame start:", finalStream.active);
            console.log("🎬 Stream tracks at frame start:", finalStream.getTracks().map(t => `${t.kind}: ${t.readyState}`));
            
            // Check if any data has been received yet
            console.log("🎬 Chunks collected so far:", chunks.length);
            
            startTime = performance.now(); // Initialize timing reference
            renderNextFrame();
          }, 100); // 100ms delay to let MediaRecorder initialize
          
          // Add a longer timeout to check for data collection issues
          setTimeout(() => {
            console.log("🎬 🔍 DEBUG: 2 second check - chunks collected:", chunks.length);
            console.log("🎬 🔍 MediaRecorder state:", videoRecorder.state);
            console.log("🎬 🔍 Stream active:", finalStream.active);
            console.log("🎬 🔍 Stream tracks:", finalStream.getTracks().map(t => `${t.kind}: ${t.readyState}`));
          }, 2000);
          
        } catch (startError) {
          console.error("🎬 ❌ Failed to start MediaRecorder:", startError);
          
          // Clean up audio resources
          if (audioSource && !audioStarted) {
            audioSource.disconnect();
          }
          if (exportAudioContext && exportAudioContext.state !== 'closed') {
            exportAudioContext.close();
          }
          
          console.warn("⚠️ MP4 recording failed to start. Please try again.");
          return;
        }

      } catch (error) {
        console.error("Error creating animated MP4:", error);
        
        // Send error status
        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "error", 
            message: "MP4 export failed" 
          }
        });
      }
      return;
    }

    // Zip up some data and download it.
    if (type === "zip") {
      if (!window.JSZip) await loadJSZip();
      const zip = new window.JSZip(); // https://github.com/Stuk/jszip

      if (content.painting) {
        const steps = [];
        const images = {};

        // Encode `painting:recording` format.
        content.painting.record.forEach((step) => {
          const format = `${step.timestamp} - ${step.label}`;
          const encodedStep = { step: format };
          if (step.gesture?.length > 0) encodedStep.gesture = step.gesture;
          steps.push(encodedStep);
          if (step.painting) {
            images[format] = bufferToBlob(step.painting, "image/png");
          }
        });

        const stepFile = JSON.stringify(steps); // Encode a JSON file for steps.

        zip.file("painting.json", stepFile);

        // Add all images based on step and index.
        keys(images).forEach((label) => {
          zip.file(`${label}.png`, images[label]);
        });

        const finalTimestamp =
          content.painting.record[content.painting.record.length - 1].timestamp;

        const zipped = await zip.generateAsync({ type: "blob" });
        const filename = `painting-${finalTimestamp}.zip`;

        if (content.destination === "download") {
          // See also: `receivedDownload`.
          const a = document.createElement("a");
          a.href = URL.createObjectURL(zipped);
          a.target = "_blank";
          a.download = filename; // Remove any extra paths.
          a.click();
          URL.revokeObjectURL(a.href);
          send({ type: "zipped", content: { result: "success", data: true } });
        } else if (content.destination === "upload") {
          // TODO: Put this on the S3 server somewhere...
          console.log("🤐 Uploading zip...", zipped);
          receivedUpload({ filename, data: zipped }, "zipped");
        }
      } else {
        send({ type: "zipped", content: { result: "error", data: false } });
      }

      return;
    }

    // Load a zip from a URL and return its unpacked contents to the piece.
    if (type === "zip:load") {
      fetch(decodeURI(content))
        .then((response) => {
          // console.log("Response", response);
          if (response.status === 200 || response.status === 304) {
            return response.arrayBuffer();
          } else {
            throw new Error(`Zip not found. Status: ${response.status}`);
          }
        })
        .then(async (buffer) => {
          if (!window.JSZip) await loadJSZip();
          const record = await unzip(buffer);
          if (record.length === 0) throw new Error("Record is an empty array");
          send({
            type: "loaded-zip-success",
            content: { url: content, data: record },
          });
        })
        .catch((error) => {
          send({ type: "loaded-zip-rejection", content: { url: content } });
        });
      return;
    }

    // 📼 Load and play tape from URL (for replay piece)
    if (type === "tape:play") {
      console.log("📼 Loading and playing tape:", content);
      
      (async () => {
        try {
          const { code, zipUrl, metadata: tapeMeta } = content;
          
          // Fetch the ZIP file
          let knownTotalBytes;
          try {
            const headResponse = await fetch(decodeURI(zipUrl), { method: "HEAD" });
            if (headResponse?.ok) {
              const headLength = headResponse.headers.get("content-length");
              if (headLength) {
                const parsedLength = Number(headLength);
                if (Number.isFinite(parsedLength) && parsedLength > 0) {
                  knownTotalBytes = parsedLength;
                }
              }
            }
          } catch (headError) {
            console.warn("📼 Unable to fetch tape HEAD for size", headError);
          }

          const response = await fetch(decodeURI(zipUrl));
          if (!response.ok) {
            throw new Error(`Tape ZIP not found. Status: ${response.status}`);
          }
          
          let arrayBuffer;
          const headerContentLength = response.headers.get("content-length");
          if (!knownTotalBytes && headerContentLength) {
            const parsedLength = Number(headerContentLength);
            if (Number.isFinite(parsedLength) && parsedLength > 0) {
              knownTotalBytes = parsedLength;
            }
          }

          const exposedTotalBytes = Number.isFinite(knownTotalBytes) ? knownTotalBytes : null;
          if (response.body) {
            const reader = response.body.getReader();
            const chunks = [];
            let receivedBytes = 0;
            let lastReportedProgress = -1;
            let lastReportedBytes = 0;

            send({
              type: "tape:download-progress",
              content: {
                code,
                progress: exposedTotalBytes ? 0 : null,
                receivedBytes: 0,
                totalBytes: exposedTotalBytes,
              },
            });

            while (true) {
              const { done, value } = await reader.read();
              if (done) break;
              chunks.push(value);
              receivedBytes += value.length;

              const hasKnownTotal = Number.isFinite(knownTotalBytes) && knownTotalBytes > 0;
              const progress = hasKnownTotal
                ? Math.min(receivedBytes / knownTotalBytes, 1)
                : null;

              const shouldReport = hasKnownTotal
                ? progress !== null && (progress - lastReportedProgress >= 0.01 || progress === 1)
                : receivedBytes - lastReportedBytes >= 262144 || receivedBytes === 0;

              if (shouldReport) {
                send({
                  type: "tape:download-progress",
                  content: {
                    code,
                    progress,
                    receivedBytes,
                    totalBytes: exposedTotalBytes,
                  },
                });
                lastReportedBytes = receivedBytes;
                if (progress !== null) {
                  lastReportedProgress = progress;
                }
              }
            }

            const concatenated = new Uint8Array(receivedBytes);
            let offset = 0;
            for (const chunk of chunks) {
              concatenated.set(chunk, offset);
              offset += chunk.length;
            }
            arrayBuffer = concatenated.buffer;
            if (!knownTotalBytes && receivedBytes > 0) {
              knownTotalBytes = receivedBytes;
            }

            send({
              type: "tape:download-progress",
              content: {
                code,
                progress: Number.isFinite(knownTotalBytes) && knownTotalBytes > 0 ? 1 : null,
                receivedBytes,
                totalBytes: Number.isFinite(knownTotalBytes) && knownTotalBytes > 0 ? knownTotalBytes : null,
              },
            });
          } else {
            arrayBuffer = await response.arrayBuffer();
            const receivedBytes = arrayBuffer.byteLength;
            if (!knownTotalBytes && receivedBytes > 0) {
              knownTotalBytes = receivedBytes;
            }
            send({
              type: "tape:download-progress",
              content: {
                code,
                progress: Number.isFinite(knownTotalBytes) && knownTotalBytes > 0 ? 1 : null,
                receivedBytes,
                totalBytes: Number.isFinite(knownTotalBytes) && knownTotalBytes > 0 ? knownTotalBytes : null,
              },
            });
          }

          console.log(`📼 ZIP downloaded: ${(arrayBuffer.byteLength / 1024 / 1024).toFixed(2)} MB`);
          
          // Parse ZIP with JSZip
          if (!window.JSZip) await loadJSZip();
          send({
            type: "tape:load-progress",
            content: { code, phase: "unpacking", progress: 0 },
          });
          const zip = await window.JSZip.loadAsync(arrayBuffer);
          
          console.log("📼 ZIP files:", Object.keys(zip.files));
          
          // Extract timing.json
          let timingData = [];
          const timingFile = zip.file("timing.json");
          if (timingFile) {
            const timingText = await timingFile.async("text");
            timingData = JSON.parse(timingText);
            console.log(`📼 Timing data: ${timingData.length} frames`);
          }
          
          // Extract metadata.json for audio sample rate
          let tapeMetadata = {};
          const metadataFile = zip.file("metadata.json");
          if (metadataFile) {
            const metadataText = await metadataFile.async("text");
            tapeMetadata = JSON.parse(metadataText);
            console.log(`📼 Metadata:`, tapeMetadata);
          }
          
          // Extract frame PNGs
          const frameFiles = Object.keys(zip.files)
            .filter(name => name.startsWith('frame-') && name.endsWith('.png'))
            .sort((a, b) => {
              const aNum = parseInt(a.match(/frame-(\d+)/)[1]);
              const bNum = parseInt(b.match(/frame-(\d+)/)[1]);
              return aNum - bNum;
            });
          
          console.log(`📼 Loading ${frameFiles.length} frames into recordedFrames...`);
          
          // Clear existing recorded frames
          recordedFrames.length = 0;
          
          const defaultFrameDuration = 1000 / 30; // Assume ~30fps when timing data is missing
          const baseTimestamp = (() => {
            for (let i = 0; i < timingData.length; i += 1) {
              const entry = timingData[i];
              if (entry && Number.isFinite(entry.timestamp)) {
                return entry.timestamp;
              }
            }
            return null;
          })();

          // Load frames as ImageData
          let fallbackElapsed = 0;
          let lastFrameProgress = 0;
          for (let i = 0; i < frameFiles.length; i++) {
            const frameFile = zip.file(frameFiles[i]);
            const frameBlob = await frameFile.async("blob");
            const frameBitmap = await createImageBitmap(frameBlob);
            
            // Create canvas to extract ImageData
            const tempCanvas = document.createElement("canvas");
            tempCanvas.width = frameBitmap.width;
            tempCanvas.height = frameBitmap.height;
            const tempCtx = tempCanvas.getContext("2d");
            tempCtx.drawImage(frameBitmap, 0, 0);
            const imageData = tempCtx.getImageData(0, 0, frameBitmap.width, frameBitmap.height);
            
            // Use timing from timing.json if available
            const frameTiming = timingData[i];
            const frameDuration = frameTiming && Number.isFinite(frameTiming.duration)
              ? frameTiming.duration
              : defaultFrameDuration;
            let timestamp;

            if (frameTiming && Number.isFinite(frameTiming.timestamp) && baseTimestamp !== null) {
              timestamp = frameTiming.timestamp - baseTimestamp;
              if (!Number.isFinite(timestamp) || timestamp < 0) {
                timestamp = fallbackElapsed;
              }
            } else {
              timestamp = fallbackElapsed;
            }

            recordedFrames.push([timestamp, imageData]);
            fallbackElapsed = timestamp + frameDuration;
            
            if (i % 50 === 0 || i === frameFiles.length - 1) {
              console.log(`  Loaded ${i + 1}/${frameFiles.length} frames`);
            }

            if (frameFiles.length > 0) {
              const progress = Math.min((i + 1) / frameFiles.length, 1);
              if (progress - lastFrameProgress >= 0.01 || progress === 1) {
                send({
                  type: "tape:load-progress",
                  content: {
                    code,
                    phase: "frames",
                    progress,
                    loadedFrames: i + 1,
                    totalFrames: frameFiles.length,
                  },
                });
                lastFrameProgress = progress;
              }
            }
          }

          if (frameFiles.length === 0) {
            send({
              type: "tape:load-progress",
              content: {
                code,
                phase: "frames",
                progress: 1,
                loadedFrames: 0,
                totalFrames: 0,
              },
            });
          }
          
          // Calculate total duration from timing data
          if (timingData.length === 0) {
            console.log("📼 Timing data missing; using fallback frame duration");
          }

          if (recordedFrames.length > 0) {
            const lastFrameTimestamp = recordedFrames[recordedFrames.length - 1][0];
            const lastFrameTiming = timingData.length > 0 ? timingData[Math.min(timingData.length - 1, recordedFrames.length - 1)] : null;
            const lastFrameDuration = lastFrameTiming && Number.isFinite(lastFrameTiming.duration)
              ? lastFrameTiming.duration
              : defaultFrameDuration;

            mediaRecorderDuration = Math.max(0, lastFrameTimestamp + lastFrameDuration);
            console.log(`📼 Calculated duration: ${mediaRecorderDuration}ms (normalized)`);
          } else {
            mediaRecorderDuration = 0;
            console.warn("📼 No frames loaded from tape; duration set to 0");
          }
          
          // Extract and load audio if available
          const soundtrackFile = zip.file("soundtrack.wav");
          if (soundtrackFile) {
            console.log("📼 Found soundtrack.wav, loading audio...");
            try {
              const audioBlob = await soundtrackFile.async("blob");
              const audioArrayBuffer = await audioBlob.arrayBuffer();
              
              // Lazy load: use existing global audioContext or create one only when needed
              let ctx = window.audioContext || audioContext;
              
              if (!ctx) {
                // No AudioContext exists yet - create one with optimal settings
                const targetSampleRate = tapeMetadata.audioSampleRate || 48000;
                console.log(`📼 Lazy-creating AudioContext at ${targetSampleRate}Hz for tape playback`);
                try {
                  ctx = new (window.AudioContext || window.webkitAudioContext)({
                    sampleRate: targetSampleRate,
                    latencyHint: "playback", // Optimize for playback vs. interactive
                  });
                  // Store globally for future use
                  window.audioContext = ctx;
                  audioContext = ctx;
                  console.log(`📼 AudioContext created: ${ctx.sampleRate}Hz, state: ${ctx.state}`);
                } catch (e) {
                  console.error("📼 Failed to create AudioContext:", e);
                  throw new Error("AudioContext not available - audio playback will be silent");
                }
              } else {
                console.log(`📼 Using existing AudioContext: ${ctx.sampleRate}Hz, state: ${ctx.state}`);
              }
              
              // Resume AudioContext if it's suspended (browser autoplay policy)
              if (ctx.state === 'suspended') {
                console.log("📼 AudioContext suspended - will resume on user interaction");
                console.log("📼 Skipping audio resume to allow video playback");
                // Don't await ctx.resume() - it may hang without user gesture
                // Video will play silently, audio will start when user interacts
              }
              
              // Store raw audio data for potential re-decoding when AudioContext becomes running
              window.tapeAudioArrayBuffer = audioArrayBuffer.slice();
              
              const audioBuffer = await ctx.decodeAudioData(audioArrayBuffer);
              sfx["tape:audio"] = audioBuffer;
              console.log(`📼 Audio loaded: ${audioBuffer.duration.toFixed(2)}s, ${audioBuffer.numberOfChannels} channels, ${audioBuffer.sampleRate}Hz`);
              console.log(`📼 Duration comparison - Video: ${(mediaRecorderDuration / 1000).toFixed(2)}s, Audio: ${audioBuffer.duration.toFixed(2)}s, Δ: ${Math.abs(audioBuffer.duration - mediaRecorderDuration / 1000).toFixed(2)}s`);
            } catch (error) {
              console.error("📼 Error loading audio:", error);
              console.warn("📼 Tape will play without audio");
            }
          } else {
            console.log("📼 No soundtrack.wav found in tape");
          }
          
          // Trigger presentation using existing underlay system
          // Instead of send({ type: "recorder:present" }), directly invoke presentation
          // since we're already in the bios context with frames loaded
          
          // First, send notification to disk that tape is loaded
          send({
            type: "tape:loaded",
            content: {
              code,
              framesReady: recordedFrames.length,
              totalFrames: frameFiles.length,
              streaming: false,
            },
          });
          
          console.log("📼 Tape loaded and ready to play");
          
          // Send AudioContext state to video piece so it knows about audio
          console.log("📼 🎵 Sending AudioContext state to video piece:", { state: audioContext?.state, hasAudio: !!audioContext });
          send({ 
            type: "tape:audio-context-state", 
            content: { 
              state: audioContext?.state,
              hasAudio: !!audioContext
            } 
          });
          
          // Now trigger presentation by calling the same logic as recorder:present
          // We'll use a setTimeout to allow the tape:loaded message to be processed first
          setTimeout(async () => {
            console.log("📼 Triggering presentation for loaded tape");
            await receivedChange({ data: { type: "recorder:present", content: {} } });
          }, 100);
        } catch (error) {
          console.error("📼 Error loading/playing tape:", error);
          send({ type: "tape:error", content: error.message });
        }
      })();
      
      return;
    }

    // 📼 Get tape information (duration, frame count, etc.)
    if (type === "tape:get-info") {
      const info = {
        frameCount: recordedFrames.length,
        totalDuration: 0,
        hasAudio: !!window.tapeAudioArrayBuffer,
      };
      
      // Calculate total duration from frame timestamps
      if (recordedFrames.length > 1) {
        const firstTimestamp = recordedFrames[0][0];
        const lastTimestamp = recordedFrames[recordedFrames.length - 1][0];
        info.totalDuration = (lastTimestamp - firstTimestamp) / 1000; // Convert to seconds
      }
      
      // Reply back to the disk
      send({ type: "tape:info-reply", content: info });
      return;
    }

    // 📼 Stop tape playback (when leaving video disk)
    if (type === "tape:stop") {
      console.log("📼 Stopping tape playback");
      stopTapePlayback?.();
      tapeManager?.cleanup();
      tapeManager = null;
      return;
    }

    // 📼 Toggle play/pause for a tape (for tv.mjs tap gesture)
    if (type === "tape:toggle-play") {
      if (!tapeManager) return;
      
      const { tapeId } = content;
      const tape = tapeManager.tapes.get(tapeId);
      
      if (tape && !tape.isLoading) {
        if (tape.isPlaying) {
          tape.pause();
          console.log(`⏸️ Paused tape ${tapeId}`);
        } else {
          tape.play(tapeManager.audioContext);
          console.log(`▶️ Playing tape ${tapeId}`);
        }
      }
      
      return;
    }

    // 📼 Seek tape to specific progress (for tv.mjs scrubbing)
    if (type === "tape:seek") {
      if (!tapeManager) return;
      
      const { tapeId, progress } = content;
      const tape = tapeManager.tapes.get(tapeId);
      
      if (tape && !tape.isLoading) {
        const frameIndex = Math.floor(progress * tape.frames.length);
        tape.seekToFrame(frameIndex, tapeManager.audioContext);
        console.log(`⏩ Seeked tape ${tapeId} to ${(progress * 100).toFixed(1)}% (frame ${frameIndex})`);
      }
      
      return;
    }

    // 📼 Preload a tape into cache (for tv.mjs smart preloading)
    if (type === "tape:preload") {
      console.log("📼 Preloading tape:", content);
      console.log("📼 Received zipUrl:", content.zipUrl);
      console.log("📼 Metadata.zip:", content.metadata?.zip);
      
      (async () => {
        try {
          const { tapeId, code, zipUrl, metadata: tapeMeta } = content;
          
          // Create underlayFrame if needed
          if (!underlayFrame) {
            underlayFrame = document.createElement("div");
            underlayFrame.id = "underlay";
            const frameCan = document.createElement("canvas");
            frameCan.width = 1920;
            frameCan.height = 1080;
            underlayFrame.appendChild(frameCan);
            underlayFrame.style.cssText = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: -1; pointer-events: none;";
            document.body.insertBefore(underlayFrame, document.body.firstChild);
            console.log("📼 Created underlayFrame for TapeManager");
          }
          
          // Ensure audioContext exists
          if (!audioContext) {
            audioContext = new (window.AudioContext || window.webkitAudioContext)();
            console.log("📼 Created AudioContext for TapeManager");
          }
          
          // Initialize tape manager if needed
          if (!tapeManager) {
            const frameCan = underlayFrame.querySelector("canvas");
            if (frameCan) {
              const maxTapes = window.innerWidth > 768 ? 12 : 6;
              tapeManager = new TapeManager(frameCan, audioContext, maxTapes);
              globalThis.tapeManager = tapeManager; // Expose to disks for progress access (globalThis works in workers)
              console.log(`📼 Initialized TapeManager (max ${maxTapes} tapes)`);
              
              // Start independent render loop for TapeManager
              function renderTapeManager() {
                if (tapeManager) {
                  tapeManager.renderFrame(performance.now(), send);
                  requestAnimationFrame(renderTapeManager);
                }
              }
              requestAnimationFrame(renderTapeManager);
              console.log("📼 Started TapeManager render loop");
            } else {
              console.error("📼 Cannot initialize TapeManager - no canvas found");
              return;
            }
          }
          
          if (!tapeManager) {
            console.warn("📼 Cannot preload - TapeManager not initialized");
            return;
          }
          
          const tape = tapeManager.getTape(tapeId);
          if (tape.frames.length > 0) {
            console.log(`📼 Tape ${tapeId} already loaded`);
            send({
              type: "tape:preloaded",
              content: { tapeId, frameCount: tape.frames.length }
            });
            
            // If requestFrames is true, send frames to disk
            if (content.requestFrames) {
              const previewFrames = await buildTapePreviewFrames(tape.frames);
              const framesToSend = previewFrames.length > 0 ? previewFrames : tape.frames.slice(0, TAPE_PREVIEW_MAX_FRAMES);
              console.log(`📼 Sending ${framesToSend.length}/${tape.frames.length} preview frames to disk (cached)`);
              send({
                type: "tape:frames",
                content: {
                  tapeId,
                  frames: framesToSend,
                  preview: previewFrames.length > 0,
                  totalFrames: tape.frames.length
                }
              });
            }
            return;
          }
          
          tape.isLoading = true;
          tape.code = code;
          tape.metadata = tapeMeta;
          
          // Send initial load progress
          send({
            type: "tape:load-progress",
            content: { code, phase: "downloading", progress: 0 }
          });
          
          // Fetch ZIP
          console.log(`📼 About to fetch ZIP for tape ${code} (${tapeId}) from:`, zipUrl);
          const response = await fetch(decodeURI(zipUrl));
          if (!response.ok) {
            console.error(`📼 ZIP fetch failed for ${code}: ${response.status} from URL:`, zipUrl);
            throw new Error(`Tape ZIP not found. Status: ${response.status}`);
          }
          
          const arrayBuffer = await response.arrayBuffer();
          console.log(`📼 Preload ZIP downloaded: ${(arrayBuffer.byteLength / 1024 / 1024).toFixed(2)} MB`);
          
          send({
            type: "tape:load-progress",
            content: { code, phase: "unpacking", progress: 0.3 }
          });
          
          // Parse ZIP
          if (!window.JSZip) await loadJSZip();
          const zip = await window.JSZip.loadAsync(arrayBuffer);
          
          send({
            type: "tape:load-progress",
            content: { code, phase: "frames", progress: 0.5 }
          });
          
          // Extract frames
          const frameFiles = Object.keys(zip.files)
            .filter(name => name.startsWith('frame-') && name.endsWith('.png'))
            .sort((a, b) => {
              const aNum = parseInt(a.match(/frame-(\d+)/)[1]);
              const bNum = parseInt(b.match(/frame-(\d+)/)[1]);
              return aNum - bNum;
            });
          
          let loadedFrames = 0;
          for (const filename of frameFiles) {
            const frameFile = zip.file(filename);
            const blob = await frameFile.async("blob");
            const bitmap = await createImageBitmap(blob);
            tape.frames.push(bitmap);
            loadedFrames++;
            
            // Send progress every 50 frames
            if (loadedFrames % 50 === 0 || loadedFrames === frameFiles.length) {
              send({
                type: "tape:load-progress",
                content: {
                  code,
                  phase: "frames",
                  progress: 0.5 + (loadedFrames / frameFiles.length) * 0.3,
                  loadedFrames,
                  totalFrames: frameFiles.length
                }
              });
            }
          }
          
          // Extract timing
          const timingFile = zip.file("timing.json");
          if (timingFile) {
            const timingText = await timingFile.async("text");
            tape.timingData = JSON.parse(timingText);
          }
          
          send({
            type: "tape:load-progress",
            content: { code, phase: "audio", progress: 0.9 }
          });
          
          // Extract audio
          const audioFile = zip.file("soundtrack.wav");
          if (audioFile && audioContext) {
            const audioBlob = await audioFile.async("blob");
            const audioArrayBuffer = await audioBlob.arrayBuffer();
            tape.audioBuffer = await audioContext.decodeAudioData(audioArrayBuffer);
            console.log(`📼 Preloaded audio: ${tape.audioBuffer.duration.toFixed(2)}s`);
          }
          
          tape.isLoading = false;
          console.log(`📼 Preloaded tape ${tapeId}: ${tape.frames.length} frames`);
          
          send({
            type: "tape:load-progress",
            content: { code, phase: "complete", progress: 1 }
          });
          
          send({
            type: "tape:preloaded",
            content: { tapeId, frameCount: tape.frames.length }
          });
          
          // If requestFrames is true, send frames to disk for preview
          if (content.requestFrames) {
            const previewFrames = await buildTapePreviewFrames(tape.frames);
            const framesToSend = previewFrames.length > 0 ? previewFrames : tape.frames.slice(0, TAPE_PREVIEW_MAX_FRAMES);
            console.log(`📼 Sending ${framesToSend.length}/${tape.frames.length} preview frames to disk`);
            send({
              type: "tape:frames",
              content: {
                tapeId,
                frames: framesToSend,
                preview: previewFrames.length > 0,
                totalFrames: tape.frames.length
              }
            });
          }
        } catch (error) {
          console.error("📼 Error preloading tape:", error);
          send({ type: "tape:preload-error", content: { tapeId: content.tapeId, error: error.message } });
        }
      })();
      
      return;
    }

    // 📼 Request frames from a loaded tape (for disk-side preview/playback)
    if (type === "tape:request-frames") {
      const { tapeId } = content;
      
      if (!tapeManager) {
        console.warn("📼 Cannot send frames - TapeManager not initialized");
        return;
      }
      
      const tape = tapeManager.getTape(tapeId);
      if (tape && tape.frames.length > 0) {
        const previewFrames = await buildTapePreviewFrames(tape.frames);
        const framesToSend = previewFrames.length > 0 ? previewFrames : tape.frames.slice(0, TAPE_PREVIEW_MAX_FRAMES);
        console.log(`📼 Sending ${framesToSend.length}/${tape.frames.length} preview frames to disk for ${tapeId}`);
        send({
          type: "tape:frames",
          content: {
            tapeId,
            frames: framesToSend,
            preview: previewFrames.length > 0,
            totalFrames: tape.frames.length
          }
        });
      } else {
        console.warn(`📼 Cannot send frames for ${tapeId} - not loaded`);
      }
      
      return;
    }

    // 📼 Update tape scroll offsets and alpha (for tv.mjs smooth transitions)
    if (type === "tape:scroll-offset") {
      if (!tapeManager) return;
      
      const { tapePositions } = content; // [{ id, yOffset, alpha }, ...]
      tapeManager.updateAudioCrossfade(tapePositions);
      
      return;
    }

    // 📼 Scrub to specific progress within tape (for tv.mjs horizontal gestures)
    if (type === "tape:scrub") {
      if (!tapeManager) return;
      
      const { tapeId, progress } = content;
      const tape = tapeManager.tapes.get(tapeId);
      
      if (tape && !tape.isLoading) {
        const targetFrame = Math.floor(progress * tape.frames.length);
        tape.seekToFrame(targetFrame, tapeManager.audioContext);
        console.log(`📼 Scrubbed tape ${tapeId} to ${(progress * 100).toFixed(1)}%`);
      }
      
      return;
    }

    // 📼 Get current playback progress (for tv.mjs UI)
    if (type === "tape:get-progress") {
      if (!tapeManager) return;
      
      const { tapeId } = content;
      const tape = tapeManager.tapes.get(tapeId);
      
      if (tape) {
        send({
          type: "tape:progress-reply",
          content: {
            tapeId,
            progress: tape.getProgress(),
            isPlaying: tape.isPlaying,
            isPaused: tape.isPaused
          }
        });
      }
      
      return;
    }

    // 📼 Set active tape (for tv.mjs when transitioning)
    if (type === "tape:set-active") {
      if (!tapeManager) return;
      
      const { tapeId } = content;
      tapeManager.setActive(tapeId);
      console.log(`📼 Set active tape: ${tapeId}`);
      
      return;
    }

    // 📼 🎵 Adjust tape audio pitch (for scrubbing)
    if (type === "tape:audio-shift") {
      // Find the currently playing tape audio
      const tapeAudioId = Object.keys(sfxPlaying).find(id => id.startsWith("tape:audio_"));
      if (tapeAudioId && sfxPlaying[tapeAudioId]) {
        const shift = typeof content === "number" ? content : 0;
        // console.log(`📼 🎵 Shifting tape audio pitch: ${shift.toFixed(3)}`);
        sfxPlaying[tapeAudioId].update({ shift });
      }
      return;
    }

    // 📼 Load and parse tape ZIP from URL (for replay piece)
    if (type === "tape:load-zip") {
      console.log("📼 Loading tape ZIP from:", content);
      
      (async () => {
        try {
          // Fetch the ZIP file
          const response = await fetch(decodeURI(content));
          if (!response.ok) {
            throw new Error(`Tape ZIP not found. Status: ${response.status}`);
          }
          
          const arrayBuffer = await response.arrayBuffer();
          console.log(`📼 ZIP downloaded: ${(arrayBuffer.byteLength / 1024 / 1024).toFixed(2)} MB`);
          
          // Parse ZIP with JSZip
          if (!window.JSZip) await loadJSZip();
          const zip = await window.JSZip.loadAsync(arrayBuffer);
          
          console.log("📼 ZIP files:", Object.keys(zip.files));
          
          // Extract metadata.json
          let metadata = null;
          const metadataFile = zip.file("metadata.json");
          if (metadataFile) {
            const metadataText = await metadataFile.async("text");
            metadata = JSON.parse(metadataText);
          }
          
          // Extract timing.json
          let timing = null;
          const timingFile = zip.file("timing.json");
          if (timingFile) {
            const timingText = await timingFile.async("text");
            timing = JSON.parse(timingText);
          }
          
          // Extract frame blobs
          const frameFiles = Object.keys(zip.files)
            .filter(name => name.startsWith('frame-') && name.endsWith('.png'))
            .sort((a, b) => {
              const aNum = parseInt(a.match(/frame-(\d+)/)[1]);
              const bNum = parseInt(b.match(/frame-(\d+)/)[1]);
              return aNum - bNum;
            });
          
          console.log(`📼 Extracting ${frameFiles.length} frames...`);
          const frameBlobs = [];
          for (const filename of frameFiles) {
            const frameFile = zip.file(filename);
            const blob = await frameFile.async("blob");
            frameBlobs.push(blob);
          }
          
          // Send parsed data back to worker
          send({
            type: "tape:zip-parsed",
            content: { metadata, timing, frameBlobs },
          });
          
          console.log("📼 Tape ZIP parsed and sent to worker");
        } catch (error) {
          console.error("📼 Error loading/parsing tape ZIP:", error);
          send({ type: "tape:zip-error", content: error.message });
        }
      })();
      
      return;
    }

    // Capture device motion.
    if (type === "motion:start") {
      startCapturingMotion();
      return;
    }

    if (type === "motion:stop") {
      stopCapturingMotion();
      return;
    }

    // Speech synthesis. (local and remote)
    if (type === "speak") {
      speak(content.utterance, content.voice, content.mode, content.opts);
      return;
    }

    // Show a classic DOM / window style alert box.
    if (type === "alert") {
      window.alert(content);
      return;
    }

    // Add a DOM event hitbox for the `Button Hitboxes`
    // event listener on the document.
    // 📓 Adding the same label multiple times will have no additional effect.
    if (type === "button:hitbox:add") {
      if (hitboxes[content.label] !== undefined) return;

      let state = "up";
      // Event handler for each button press.
      hitboxes[content.label] = async (e) => {
        const frame = canvas.getBoundingClientRect();
        const xscale = projectedWidth / canvas.width;
        const yscale = projectedHeight / canvas.height;
        const hitbox = Box.from({
          x: frame.left + content.box.x * xscale,
          y: frame.top + content.box.y * yscale,
          w: content.box.w * xscale,
          h: content.box.h * yscale,
        });

        // 📓 Uncomment to debug the hitboxes and see how they line up.
        // const dbg = Box.from({
        //   x: content.box.x * xscale,
        //   y: content.box.y * yscale,
        //   w: content.box.w * xscale,
        //   h: content.box.h * yscale,
        // });
        // debugCtx.fillStyle = "red";
        // debugCtx.globalAlpha = 0.5;
        // debugCtx.fillRect(dbg.x, dbg.y, dbg.w, dbg.h);

        const hit = hitbox.contains({ x: e.x, y: e.y });

        if (e.type === "pointerup" && state === "down" && hit) {
          // This is pretty specific to the "copy" clipboard
          // stuff for now. 23.06.16.15.03
          // console.log("🔘 Button tap label:", content.label);

          // Handle QR corner tap (toggle to fullscreen)
          if (content.label === "qr-corner") {
            console.log("🔘 QR corner tapped - toggling to fullscreen");
            send({ type: "qr:toggle-fullscreen" });
          }

          // Handle QR fullscreen tap (toggle back to corner)
          if (content.label === "qr-fullscreen") {
            console.log("🔘 QR fullscreen tapped - toggling to corner");
            send({ type: "qr:toggle-fullscreen" });
          }

          if (content.label === "copy") {
            try {
              await navigator.clipboard.writeText(content.message);
              send({ type: "copy:copied" });
            } catch (err) {
              console.warn("📋 Clipboard copy failed:", err);
              if (window.parent) {
                console.log("📋 Trying via message...");
                window.parent.postMessage(
                  { type: "clipboard:copy", value: content.message },
                  "*",
                );
              } else {
                send({ type: "copy:failed" });
              }
            }
          }

          // Paste should always happen on a pointerdown.
          if (content.label === "paste") {
            try {
              let pastedText = await navigator.clipboard.readText();
              // This routes through to the `pasted:text` event in `disk`.
              // where `pastedText` is sent on the next frame.
              if (pastedText.length === 0) {
                send({ type: "paste:pasted:empty" });
              } else {
                // Insert pasted text at current caret position.
                if (keyboard) {
                  const start = keyboard.input.selectionStart;
                  const end = keyboard.input.selectionEnd;
                  const selLen = end - start;

                  const beforeCursor = keyboard.input.value.substring(0, start);
                  const afterCursor = keyboard.input.value.substring(
                    selLen > 0 ? end : start,
                  );

                  // Enforce 256 character limit for prompt input
                  const PROMPT_CHAR_LIMIT = 256;
                  const currentLen = beforeCursor.length + afterCursor.length;
                  const availableSpace = Math.max(0, PROMPT_CHAR_LIMIT - currentLen);
                  if (pastedText.length > availableSpace) {
                    pastedText = pastedText.slice(0, availableSpace);
                  }

                  keyboard.input.value =
                    beforeCursor + pastedText + afterCursor;
                  
                  // Also enforce limit on final result
                  if (keyboard.input.value.length > PROMPT_CHAR_LIMIT) {
                    keyboard.input.value = keyboard.input.value.slice(0, PROMPT_CHAR_LIMIT);
                  }

                  const newCursorPosition = start + pastedText.length;
                  keyboard.input.setSelectionRange(
                    newCursorPosition,
                    newCursorPosition,
                  );

                  send({
                    type: "prompt:text:replace",
                    content: {
                      text: keyboard.input.value,
                      cursor: keyboard.input.selectionStart,
                    },
                  });

                  if (document.activeElement !== keyboard.input) {
                    keyboard.input.focus();
                  }
                }
              }
              // send({
              //   type:
              //     pastedText.length > 0 ? "paste:pasted" : "paste:pasted:empty",
              // });
            } catch (err) {
              console.warn(err);
              send({ type: "paste:failed" });
            }
          }

          state = "up";
        } else if (e.type === "pointerdown" && hit) {
          state = "down";
        } else if (e.type === "pointerup" && !hit) {
          state = "up";
        }
      };

      return;
    }

    // Remove a hitbox via its label.
    if (type === "button:hitbox:remove") {
      delete hitboxes[content];
      return;
    }

    // Direct copy text to clipboard (re-enabled for chat modal and other uses)
    // Mobile Safari requires execCommand fallback as clipboard API often fails
    if (type === "copy") {
      let success = false;
      
      // Try modern clipboard API first
      if (navigator.clipboard?.writeText) {
        try {
          await navigator.clipboard.writeText(content);
          success = true;
        } catch (err) {
          console.warn("📋 Clipboard API failed, trying fallback:", err);
        }
      }
      
      // Fallback: Use execCommand with a temporary textarea (works on mobile Safari)
      if (!success) {
        try {
          const textarea = document.createElement("textarea");
          textarea.value = content;
          textarea.style.position = "fixed";
          textarea.style.left = "-9999px";
          textarea.style.top = "-9999px";
          textarea.style.opacity = "0";
          textarea.setAttribute("readonly", ""); // Prevent keyboard popup on mobile
          document.body.appendChild(textarea);
          
          // iOS Safari specific: need to select with setSelectionRange
          textarea.focus();
          textarea.setSelectionRange(0, textarea.value.length);
          
          success = document.execCommand("copy");
          document.body.removeChild(textarea);
          
          if (!success) {
            console.warn("📋 execCommand copy failed");
          }
        } catch (err) {
          console.warn("📋 Fallback copy failed:", err);
        }
      }
      
      if (success) {
        send({ type: "copy:copied" });
      } else if (window.parent && window.self !== window.top) {
        // Try via parent message for embedded contexts
        window.parent.postMessage(
          { type: "clipboard:copy", value: content },
          "*",
        );
      } else {
        send({ type: "copy:failed" });
      }
      return;
    }

    // Authenticate / signup or login a user.
    if (type === "login") {
      if (window.self !== window.top) {
        window.parent.postMessage({ type: "login" }, "*");
      } else {
        window.acLOGIN?.();
      }
      return;
    }

    if (type === "signup") {
      if (window.self === window.top) {
        window.acLOGIN?.("signup");
      } else {
        console.log("🟠 Cannot sign up in an embedded view.");
      }
      return;
    }

    if (type === "logout") {
      if (window.acTOKEN) {
        if (window.parent) {
          window.parent.postMessage({ type: "logout" }, "*");
          window.safeLocalStorageRemove("session-aesthetic");
        }
        // Just use the logout services of the host.
      } else {
        window.acLOGOUT?.();
        window.flutter_inappwebview?.callHandler("closeWebview"); // Close A.C. webview on logout inside of Autonomy wallet.
      }
      return;
    }

    // Send a locally opened file across the thread.
    if (type === "file-open:request") {
      const file = await openFile();
      send({
        type: "file-open:response",
        content: { data: file, result: file ? "success" : "error" },
      });
      return;
    }

    // Send a locally opened file across the thread.
    if (type === "file-encode:request") {
      let file;
      if (content.type === "png")
        file = await bufferToBlob(content.file, "image/png", content.modifiers);
      send({
        type: "file-encode:response",
        content: { data: file, result: file ? "success" : "error" },
      });
      return;
    }

    // Send a user authorization token (or undefined) across the thread.
    if (type === "authorization:request") {
      // console.log("Getting token...");
      const token = await authorize();
      // console.log("Failure token:", token);
      send({
        type: "authorization:response",
        content: { data: token || null, result: token ? "success" : "error" },
      });
      return;
    }

    // *** Route to different functions if this change is not a full frame update.
    if (type === "load-failure" && MetaBrowser) {
      document.querySelector("#software-keyboard-input")?.blur();
      return;
    }

    // if (type === "alert-popup:instagram" && Instagram) {
    //   window.alert(content);
    //   return;
    // }

    // Connect to an ethereum wallet extension.
    if (type === "web3-connect") {
      if (window.ethereum) {
        const addresses = await (typeof window.ethereum.request === "function"
          ? window.ethereum.request({ method: "eth_requestAccounts" })
          : window.ethereum.enable());

        const address = addresses[0];
        await loadWeb3(); // Load the web3.js library.
        // const w3 = new Web3(window.ethereum);

        // From: https://github.com/web3/web3.js/issues/2683#issuecomment-1304496119
        async function ensReverse(address) {
          const web3 = new Web3("https://eth.public-rpc.com/");
          const namehash = await web3.eth.call({
            to: "0x084b1c3c81545d370f3634392de611caabff8148", // ENS: Reverse Registrar
            data: web3.eth.abi.encodeFunctionCall(
              {
                name: "node",
                type: "function",
                inputs: [{ type: "address", name: "addr" }],
              },
              [address],
            ),
          });
          return web3.eth.abi.decodeParameter(
            "string",
            await web3.eth.call({
              to: "0xa2c122be93b0074270ebee7f6b7292c7deb45047", // ENS: Default Reverse Resolver
              data: web3.eth.abi.encodeFunctionCall(
                {
                  name: "name",
                  type: "function",
                  inputs: [{ type: "bytes32", name: "hash" }],
                },
                [namehash],
              ),
            }),
          );
        }

        const ensName = await ensReverse(address);
        const id = ensName || address;
        if (debug) console.log("🕸️3️⃣ Connected to:", id);
        send({
          type: "web3-connect-response",
          content: { result: "success", id },
        });
      } else {
        send({ type: "web3-connect-response", content: { result: "error" } });
        console.warn(
          "🔴 Web3 is unavailable. Please install an Ethereum wallet or enable your extension.",
        );
      }
      return;
    }

    // Connect to a Tezos wallet (Beacon SDK)
    if (type === "tezos-connect") {
      try {
        const network = content?.network || "ghostnet";
        const address = await connectTezosWallet(network);
        send({
          type: "tezos-connect-response",
          content: { result: "success", address },
        });
      } catch (err) {
        console.error("🔴 Tezos connect error:", err);
        send({
          type: "tezos-connect-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    // Disconnect Tezos wallet
    if (type === "tezos-disconnect") {
      try {
        await disconnectTezosWallet();
        send({
          type: "tezos-disconnect-response",
          content: { result: "success" },
        });
      } catch (err) {
        send({
          type: "tezos-disconnect-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    // Get current Tezos wallet address
    if (type === "tezos-address") {
      try {
        const address = await getTezosAddress();
        send({
          type: "tezos-address-response",
          content: { result: "success", address },
        });
      } catch (err) {
        send({
          type: "tezos-address-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    // Sign a message with connected Tezos wallet
    if (type === "tezos-sign") {
      try {
        const { message } = content;
        const signature = await signTezosMessage(message);
        send({
          type: "tezos-sign-response",
          content: { result: "success", signature, message },
        });
      } catch (err) {
        console.error("🔴 Tezos sign error:", err);
        send({
          type: "tezos-sign-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    // Call a Tezos contract method
    if (type === "tezos-call") {
      try {
        const { contractAddress, method, args, amount } = content;
        const opHash = await callTezosContract(contractAddress, method, args, amount || 0);
        send({
          type: "tezos-call-response",
          content: { result: "success", opHash },
        });
      } catch (err) {
        console.error("🔴 Tezos call error:", err);
        send({
          type: "tezos-call-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    // 🔷 Wallet API messages (for wallet.mjs piece)
    // Get current wallet state (used when pieces load)
    if (type === "wallet:get-state") {
      broadcastWalletState();
      return;
    }
    
    if (type === "wallet:connect") {
      try {
        const network = content?.network || "ghostnet";
        const options = {
          address: content?.address,
          network,
          walletType: content?.walletType || "temple",
          useTemple: content?.useTemple,
        };
        const address = await connectTezosWallet(network, options);
        // connectTezosWallet already updates walletState and broadcasts
      } catch (err) {
        console.error("🔴 Wallet connect error:", err);
        walletState.connected = false;
        walletState.error = err.message;
        broadcastWalletState();
      }
      return;
    }

    if (type === "wallet:disconnect") {
      try {
        await disconnectTezosWallet();
        // disconnectTezosWallet already updates walletState and broadcasts
      } catch (err) {
        console.error("🔴 Wallet disconnect error:", err);
      }
      return;
    }

    if (type === "wallet:refresh-balance") {
      try {
        if (walletState.address && walletState.connected) {
          const balance = await fetchTezosBalanceForBios(walletState.address, walletState.network);
          walletState.balance = balance;
          broadcastWalletState();
        }
      } catch (err) {
        console.error("🔴 Wallet balance refresh error:", err);
      }
      return;
    }

    // Get pairing URI for mobile wallet QR code
    if (type === "wallet:get-pairing-uri") {
      try {
        const network = content?.network || "ghostnet";
        const pairingInfo = await generatePairingUri(network);
        send({
          type: "wallet:pairing-uri-response",
          content: { result: "success", pairingInfo },
        });
      } catch (err) {
        console.error("🔴 Wallet pairing error:", err);
        send({
          type: "wallet:pairing-uri-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    // Sign a message with the connected wallet
    if (type === "wallet:sign") {
      try {
        const message = content?.message || "Hello from Aesthetic Computer!";
        const signature = await signTezosMessage(message);
        send({
          type: "wallet:sign-response",
          content: { result: "success", signature, message },
        });
      } catch (err) {
        console.error("🔴 Wallet sign error:", err);
        send({
          type: "wallet:sign-response",
          content: { result: "error", error: err.message },
        });
      }
      return;
    }

    if (type === "url-freeze") {
      const freeze = !!content?.freeze;
      if (freeze) {
        frozenUrlPath = content?.path || window.location.pathname + window.location.search + window.location.hash;
        try {
          if (!checkPackMode()) {
            window.history.replaceState({}, "", frozenUrlPath);
          }
        } catch (e) {
          /* Ignore in restricted context */
        }
      } else {
        frozenUrlPath = null;
      }
      return;
    }

    if (type === "rewrite-url-path") {
      const newPath = content.path;
      const historical = !!content.historical;

      if (typeof document !== "undefined" && !document.hasFocus()) {
        pendingUrlRewrite = { path: newPath, historical };
        if (typeof window !== "undefined" && !rewriteFocusListenerAttached) {
          rewriteFocusListenerAttached = true;
          window.addEventListener(
            "focus",
            () => {
              rewriteFocusListenerAttached = false;
              const pending = pendingUrlRewrite;
              pendingUrlRewrite = null;
              if (pending) {
                performHistoryRewrite(pending.path, pending.historical);
              }
            },
            { once: true },
          );
        }
      } else {
        performHistoryRewrite(newPath, historical);
      }

      return;
    }

    if (type === "bgm-change") {
      playBackgroundMusic(content.trackNumber, content.volume || 1);
      return;
    }

    if (type === "bgm-stop") {
      stopBackgroundMusic();
      return;
    }

    if (type === "disk-defaults-loaded") {
      // Pen (also handles touch & pointer events)
      pen = new Pen((x, y) => {
        const p = {
          x: floor(((x - canvasRect.x) / projectedWidth) * screen.width),
          y: floor(((y - canvasRect.y) / projectedHeight) * screen.height),
        };
        return p;
      });

      // ⌨️ Keyboard
      keyboard = new Keyboard();
      {
        // console.log("⌨️ 🤖 Initializing Virtual Keyboard");
        /**
         * Insert a hidden input element that is used to toggle the software
         * keyboard on touchscreen devices like iPhones and iPads.
         * *Only works in "disks/prompt".
         */
        let keyboardOpen = false;
        let keyboardOpenMethod;
        const input = document.createElement("textarea");
        const form = document.createElement("form");
        form.id = "software-keyboard-input-form";
        form.style.opacity = 0;
        input.style.position = "absolute";
        input.id = "software-keyboard-input";
        input.autocapitalize = "none";
        // input.autofocus = true;
        // input.type = "text";
        input.autocomplete = "off";
        input.style.opacity = 0;
        input.style.width = 0 + "px";
        input.style.height = 0 + "px";
        // input.setAttribute("readonly", true);
        // input.enterkeyhint = "go"; // Why doesn't this work?

        // 📓 Uncomment to debug text editing form synchronization.
        // form.style.opacity = 1;
        // input.style.zIndex = 100;
        // input.style.top = "50px";
        // input.style.left = "100px";
        // input.style.opacity = 1;
        // input.style.width = 200 + "px";
        // input.style.height = 50 + "px";

        form.append(input);
        wrapper.append(form);

        keyboard.focusHandler = function (e) {
          if (!currentPieceHasKeyboard) return;
          if (keyboardFocusLock || keyboardSoftLock) return;
          if (
            document.activeElement !== input &&
            e.key !== "`" &&
            e.key !== "Escape"
          ) {
            keyboardOpenMethod = "keyboard";
            input.focus();

            if (e.key.length !== 1 || e.ctrl) {
              send({
                type: "prompt:text:replace",
                content: { text: "", cursor: 0, mute: true },
              });
            }

            return true;
          } else if (e.key === "Enter" && e.shiftKey === false) {
            // input.blur(); // Deprecated 23.07.29.17.44
            return false;
          }
        };

        keyboard.input = input;

        // Generate an "Enter" keyboard event if the form was submitted.
        // - Don't use the submit event if we are sandboxed though!
        // - Which is required for the Meta Browser keyboard 23.02.08.12.30

        const enterEvent = {
          name: "keyboard:down:enter",
          key: "Enter",
          repeat: false,
          shift: false,
          alt: false,
          ctrl: false,
        };

        form.addEventListener("submit", (e) => {
          e.preventDefault();
        });

        form.addEventListener("keydown", (e) => {
          if (e.key === "Enter") {
            e.preventDefault();

            const enter = { ...enterEvent };
            enter.shift = e.shiftKey;
            enter.alt = e.altKey;
            enter.ctrl = e.ctrlKey;
            keyboard.events.push(enter);

            if (
              (input.value === "dl" || input.value === "download") &&
              shareFile
            ) {
              const share = () => {
                navigator
                  .share({
                    files: [shareFile],
                    title: "Share Painting",
                    // text: "Share your painting!",
                  })
                  .then(() => {
                    console.log("📥 Share was successful.");
                    shareFile = null;
                  })
                  .catch((error) => {
                    console.log("📥 Sharing failed:", error);
                    shareFile = null;
                  });
                shareFileCallback = null;
              };

              if (shareFile) {
                share();
              } else {
                shareFileCallback = share;
              }
            }
          } else if (e.key === "Home") {
            e.preventDefault();
            const home = { name: "keyboard:down:home", key: "Home" };
            home.shift = e.shiftKey;
            home.alt = e.altKey;
            home.ctrl = e.ctrlKey;
            keyboard.events.push(home);
          } else if (e.key === "Tab") {
            e.preventDefault();
            const tab = { name: "keyboard:down:tab", key: "Tab" };
            tab.shift = e.shiftKey;
            tab.alt = e.altKey;
            tab.ctrl = e.ctrlKey;
            keyboard.events.push(tab);
          } /*else if (
            // Don't send the backtick unless we are on the prompt.
            e.key === "`" &&
            currentPiece.split("/").pop() !== "prompt"
          ) {
            e.preventDefault();
            keyboard.events.push({ name: "keyboard:down:`", key: "`" });
          }*/
        });

        input.addEventListener("beforeinput", (e) => {
          // console.log("Input Type:", e.inputType, input, e);

          const pressedKeys = [];

          if (e.inputType === "deleteContentBackward") {
            // console.log(e.inputType, e.target.value);
            // alert(e.inputType);
            // pressedKeys.push("Backspace");
          } else if (
            ["insertText", "insertCompositionText"].includes(e.inputType)
          ) {
            // Sanitize input if it arrives in chunks... like if it was dictated.
            // This is still basic, and is usable in the Meta Quest Browser. 22.10.24.17.07
            // let sanitizedInput = input;
            // if (input.length > 1) {
            //   sanitizedInput = input
            //     .trim()
            //     .toLowerCase()
            //     .replace(",", "")
            //     .replace(".", "");
            //   console.log("👄 Spoken / pasted input:", sanitizedInput);
            // }
            // [...sanitizedInput].forEach((chr) => pressedKeys.push(chr));
          }

          pressedKeys.forEach((pk) => {
            keyboard.events.push({
              name: "keyboard:down:" + pk.toLowerCase(),
              key: pk,
              repeat: false,
              shift: false,
              alt: false,
              ctrl: false,
            });
          });
        });

        function handleInput(e) {
          input.removeEventListener("input", handleInput);

          let text = e.target.value;

          // Replace curly single and double quotes with straight quotes
          text = text
            .replace(/[\u2018\u2019]/g, "'")
            .replace(/[\u201C\u201D]/g, '"');

          // Enforce 256 character limit for prompt input
          const PROMPT_CHAR_LIMIT = 256;
          if (text.length > PROMPT_CHAR_LIMIT) {
            text = text.slice(0, PROMPT_CHAR_LIMIT);
          }

          e.target.value = text;

          send({
            type: "prompt:text:replace",
            content: {
              text: text,
              cursor: Math.min(input.selectionStart, PROMPT_CHAR_LIMIT),
            },
          });

          input.addEventListener("input", handleInput);
        }

        input.addEventListener("input", handleInput);

        input.addEventListener("keydown", (e) => {
          if (keyboardFocusLock) {
            e.preventDefault();
            return;
          }

          if (e.key === "ArrowLeft" || e.key === "ArrowRight") {
            let cursor =
              input.selectionDirection === "backward"
                ? input.selectionStart
                : input.selectionEnd;

            const selectionLength = input.selectionEnd - input.selectionStart;

            if (!e.shiftKey && selectionLength > 1) {
              cursor =
                e.key === "ArrowLeft"
                  ? max(0, input.selectionStart)
                  : input.selectionEnd;
            } else {
              cursor += e.key === "ArrowLeft" ? -1 : 1;
              cursor = max(0, min(input.value.length, cursor));
            }

            let start, end;
            if (e.shiftKey) {
              if (e.key === "ArrowLeft") {
                if (input.selectionDirection === "backward") {
                  start = cursor;
                  end = input.selectionEnd;
                } else {
                  start = input.selectionStart;
                  end =
                    cursor > input.selectionStart ? cursor : input.selectionEnd;
                }
              } else {
                if (input.selectionDirection === "forward") {
                  start = input.selectionStart;
                  end = cursor;
                } else {
                  end =
                    cursor < input.selectionEnd ? cursor : input.selectionStart;
                  start = cursor;
                }
              }
            }

            send({
              type: "prompt:text:cursor",
              content: {
                cursor,
                start,
                end,
              },
            });
          }
        });

        window.addEventListener("blur", (e) => {
          input.blur();
        });

        window.addEventListener("pointerdown", (e) => {
          if (currentPieceHasKeyboard) {
            e.preventDefault();
          }
          if (!document.hasFocus()) window.focus();
        });

        window.addEventListener("pointerup", (e) => {
          //if (keyboard.needsImmediateOpen) {
          //  keyboard.needsImmediateOpen = false;
          //  return;
          //}
          // console.log("🌬️ Blurrred", "Target:", e.target);

          // return;

          if (e.target === window) {
            // console.log("WINDOW BLURRED");
            e.preventDefault();
            return; // This prevents.
          }

          if (currentPieceHasKeyboard) e.preventDefault();

          if (
            currentPieceHasKeyboard &&
            !keyboardFocusLock &&
            !keyboardSoftLock
          ) {
            if (keyboardOpen) {
              // console.log("Target:", e.target);
              if (MetaBrowser && e.target !== window) {
                // Skip dragging the finger outside of the Meta Browser.
              } else {
                console.log("⌨️🔴 [bios pointerup] calling input.blur() | keyboardOpen:", keyboardOpen, "target:", e.target?.tagName);
                input.blur();
              }
            } else {
              keyboardOpenMethod = "pointer";
              // input.removeAttribute("readonly");
              console.log("⌨️🟢 [bios pointerup] calling input.focus() | keyboardOpen:", keyboardOpen);
              window.focus();
              input.focus();
            }
          }
        });

        input.addEventListener("focus", (e) => {
          console.log("⌨️🟢 [input focus event] keyboardOpen was:", keyboardOpen, "method:", keyboardOpenMethod);
          if (keyboardOpen) return;
          // input.removeAttribute("readonly");
          keyboardOpen = true;
          keyboard.events.push({
            name: "keyboard:open",
            method: keyboardOpenMethod,
          });
          keyboardOpenMethod = undefined;
          console.log("⌨️🟢 [input focus event] pushed keyboard:open event");
        });

        input.addEventListener("blur", (e) => {
          console.log("⌨️🔴 [input blur event] keyboardOpen was:", keyboardOpen, new Error().stack);
          // input.setAttribute("readonly", true);
          // const temp = input.value;
          // input.value = "";
          // input.value = temp;
          keyboardOpen = false;
          keyboard.events.push({ name: "keyboard:close" });
          console.log("⌨️🔴 [input blur event] pushed keyboard:close event");
        });

        window.addEventListener("blur", (e) => {
          // console.log("blurred window...");
          // keyboardOpen = false;
          // keyboard.events.push({ name: "keyboard:close" });
        });
      }

      // 🎮 Gamepad
      gamepad = new Gamepad();

      // Turn off all layers onbeforeunload. (Prevents a white flicker in chrome.)
      window.addEventListener("beforeunload", (e) => {
        send({ type: "before-unload" });
        wrapper.classList.add("reloading");
        
        // 🎮 Clean up GameBoy emulator on page unload
        if (gameboyEmulator) {
          try {
            console.log("🎮 Cleaning up GameBoy emulator on page unload");
            gameboyEmulator.pause();
            // Remove the hidden canvas
            if (window.gameboyCanvas) {
              window.gameboyCanvas.remove();
              window.gameboyCanvas = null;
            }
          } catch (error) {
            console.log("🎮 Error during GameBoy cleanup:", error);
          }
        }
      });

      // Listen for resize events on the visual viewport
      window.visualViewport.addEventListener("resize", () => {
        const y = window.visualViewport.height;
        window.acDISK_SEND({ type: "viewport-height:changed", content: { y } });
      }); // 🌒 Detect light or dark mode.
      // See also: https://flaviocopes.com/javascript-detect-dark-mode,
      //           https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

      // Skip initial OS theme detection if embedded in kidlisp.com (will receive theme from parent)
      if (window.matchMedia && !window.acWAIT_FOR_PARENT_THEME) {
        if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
          document.documentElement.style.setProperty("color-scheme", "dark");
          send({ type: "dark-mode", content: { enabled: true } });
        } else {
          document.documentElement.style.setProperty("color-scheme", "light");
          send({ type: "dark-mode", content: { enabled: false } });
        }
      }

      // Listen for OS theme changes (but respect manual override from kidlisp.com)
      if (window.matchMedia) {
        window
          .matchMedia("(prefers-color-scheme: dark)")
          .addEventListener("change", (event) => {
            // Skip if theme was manually set from kidlisp.com
            if (window.acMANUAL_THEME_OVERRIDE) {
              console.log('🎨 [bios.mjs] Ignoring OS theme change - manual override active');
              return;
            }
            if (event.matches) {
              document.documentElement.style.setProperty(
                "color-scheme",
                "dark",
              );
              send({ type: "dark-mode", content: { enabled: true } });
            } else {
              document.documentElement.style.setProperty(
                "color-scheme",
                "light",
              );
              send({ type: "dark-mode", content: { enabled: false } });
            }
          });
      }

      // 📋 User pasting of content.
      //window.addEventListener("paste", (event) => {
      // pastedText = event.clipboardData.getData("text/plain");
      //});

      // 🖥️ Display (Load the display, with 0 margin if sandboxed)
      frame(
        resolution?.width,
        resolution?.height,
        resolution?.gap ?? (sandboxed ? 0 : undefined),
      );

      // 🔊 Sound
      // TODO: Disable sound engine entirely... unless it is enabled by a disk. 2022.04.07.03.33
      // Only start this after a user-interaction to prevent warnings.
      // Exception: device mode auto-starts audio (for FF1 and display devices)

      activateSound = () => {
        startSound();
        window.removeEventListener("keydown", activateSound);
        window.removeEventListener("pointerdown", activateSound);
      };

      // 📺 Device mode: Auto-start audio for display devices like FF1
      // These devices can't provide user gestures, so we force audio on immediately
      if (resolution.device === true) {
        console.log("📺 Device mode: Auto-starting audio context");
        startSound();
      }

      diskSupervisor = { requestBeat, requestFrame };

      // ➰ Core Loops for User Input, Music, Object Updates, and Rendering
      Loop.start(
        () => {
          // TODO: What is this now?
          // pen.poll();
          // TODO: Key.input();
          // TODO: Voice.input();
        },
        function (needsRender, updateTimes, now) {
          // 🖱️ OBJKT mode: Hide CSS cursor after 2 seconds of inactivity
          if (window.acPACK_MODE && pen?.pointers[1]) {
            const pointer = pen.pointers[1];
            const timeSinceMove = performance.now() - (pointer.lastMoveTime || 0);
            const shouldShowCursor = timeSinceMove < 2000;
            
            if (shouldShowCursor && document.body.style.cursor === 'none') {
              // Use simple CSS cursor in PACK mode to avoid SVG 404
              document.body.style.cursor = 'default';
            } else if (!shouldShowCursor && document.body.style.cursor !== 'none') {
              document.body.style.cursor = 'none';
            }
          }

          // TODO: How can I get the pen data into the disk and back
          //       to Three.JS as fast as possible? 22.10.26.23.25
          diskSupervisor.requestFrame?.(needsRender, updateTimes, now);

          if (ThreeD?.status.alive === true && ThreeDBakeQueue.length > 0) {
            ThreeD.collectGarbage();
            // Bake all forms, while keeping track of baked forms, and any form that is missing after the queue ends needs to be cleared.
            const touchedForms = [];
            ThreeDBakeQueue.forEach((baker) => touchedForms.push(...baker()));
            ThreeD.checkForRemovedForms(touchedForms);
            ThreeDBakeQueue.length = 0;
            ThreeD?.render(now);
          }

          TwoD?.render();
        },
      );
    } // 💾 Disk Loading
    // Initialize some global stuff after the first piece loads.
    // Unload some already initialized stuff if this wasn't the first load.
    if (type === "disk-loaded") {
      console.log(`🔍 BIOS: Received disk-loaded for "${content.text}", path="${content.path}"`);
      
      // 🧹 Clean up any GPU/stats/glaze state from previous piece
      // This ensures cleanup even if the previous piece's leave() failed
      let needsGPUCleanup = false;
      
      if (statsOverlayEnabled) {
        statsOverlayEnabled = false;
        statsOverlay.style.display = "none";
        needsGPUCleanup = true;
      }
      if (webgpuCanvas.style.display !== "none") {
        webgpuCanvas.style.display = "none";
        needsGPUCleanup = true;
      }
      if (canvas.style.visibility === "hidden") {
        canvas.style.visibility = "visible";
        needsGPUCleanup = true;
      }
      // Reset glaze state only if it was actually on
      if (glaze.on) {
        Glaze.clear(); // Clear WebGL buffer to prevent stale content
        Glaze.off();
        glaze.on = false;
        // Glaze pieces (like KidLisp) need a reframe to properly refresh the screen
        needsReframe = true;
      }
      canvas.style.removeProperty("opacity");
      // Clear freeze frame if stuck
      if (freezeFrame || freezeFrameFrozen) {
        if (wrapper.contains(freezeFrameCan)) {
          freezeFrameCan.remove();
        }
        freezeFrame = false;
        freezeFrameGlaze = false;
        freezeFrameFrozen = false;
      }
      
      // 🎨 Force a reframe only if coming from a GPU piece (WebGPU canvas was visible)
      // This prevents breaking the GOL transition on normal disk loads
      if (needsGPUCleanup) {
        needsReframe = true;
      }
      
      try {
        // Clear any active parameters once the disk has been loaded.
        // Skip URL manipulation in pack mode (sandboxed iframe)
        if (!checkPackMode() && !frozenUrlPath) {
          // Special handling for prompt piece with kidlisp content
          if (
            content.path === "aesthetic.computer/disks/prompt" &&
            content.params &&
            content.params.length > 0 &&
            isKidlispSource(content.params[0])
          ) {
            // For prompt piece with kidlisp parameters, preserve the prompt~ URL structure
            const encodedContent = encodeKidlispForUrl(content.params[0]);
            const encodedPath = "/prompt~" + encodedContent;
            // Use pushState instead of replaceState to preserve history navigation
            try {
              if (!content.fromHistory) {
                window.history.pushState({}, "", encodedPath);
              } else {
                window.history.replaceState({}, "", encodedPath);
              }
            } catch (e) { /* Ignore in restricted context */ }
          } else if (content.text && isKidlispSource(content.text)) {
            // For standalone kidlisp pieces, use centralized URL encoding
            const encodedPath = "/" + encodeKidlispForUrl(content.text);
            // Use pushState instead of replaceState to preserve history navigation
            try {
              if (!content.fromHistory) {
                window.history.pushState({}, "", encodedPath);
              } else {
                window.history.replaceState({}, "", encodedPath);
              }
            } catch (e) { /* Ignore in restricted context */ }
          } else if (content.clockShortcode) {
            // ⏰ For clock shortcodes (*wibe, *abc, etc.), display the shortcode URL
            const clockPath = "/" + content.clockShortcode;
            try {
              if (!content.fromHistory) {
                window.history.pushState({}, "", clockPath);
              } else {
                window.history.replaceState({}, "", clockPath);
              }
            } catch (e) { /* Ignore in restricted context */ }
          } else {
            // For regular pieces, clear parameters but keep the basic path structure
            // Preserve DAW-related params for M4L integration
            const currentParams = new URLSearchParams(window.location.search);
            const dawParams = new URLSearchParams();
            for (const param of ['daw', 'density', 'nogap', 'width', 'height']) {
              if (currentParams.has(param)) {
                dawParams.set(param, currentParams.get(param));
              }
            }
            const queryString = dawParams.toString();
            const newUrl = window.location.pathname + (queryString ? '?' + queryString : '');
            try {
              window.history.replaceState({}, "", newUrl);
            } catch (e) { /* Ignore in restricted context */ }
          }
        }

      // if (currentPiece !== null) firstPiece = false;
      // console.log("🎮 currentPiece CHANGING FROM:", currentPiece, "TO:", content.path);
      
      // 🎮 GameBoy emulator lifecycle management
      if (gameboyEmulator) {
        if (currentPiece && currentPiece.includes('gameboy') && !content.path.includes('gameboy')) {
          // Leaving gameboy piece - pause emulator
          console.log("🎮 Leaving gameboy piece - pausing emulator");
          try {
            gameboyEmulator.pause();
          } catch (error) {
            console.log("🎮 Error pausing emulator:", error);
          }
        } else if (!currentPiece?.includes('gameboy') && content.path.includes('gameboy')) {
          // Entering gameboy piece - resume emulator if ROM is loaded
          console.log("🎮 Entering gameboy piece - resuming emulator");
          try {
            if (currentGameboyROM) {
              gameboyEmulator.play();
            }
          } catch (error) {
            console.log("🎮 Error resuming emulator:", error);
          }
        }
      }
      
      currentPiece = content.path;
      // console.log("🎮 currentPiece SET TO:", currentPiece);
      
      // 🎬 Track piece changes during recording
      if (mediaRecorder && mediaRecorder.state === "recording" && window.recordingStartTimestamp) {
        const changeTimestamp = Date.now();
        const relativeTime = changeTimestamp - window.recordingStartTimestamp;
        
        recordedPieceChanges.push({
          timestamp: changeTimestamp,
          relativeTime: relativeTime,
          path: content.path,
          params: content.params,
          text: content.text,
        });
        
        console.log(`🎬 📍 Piece change recorded at ${relativeTime}ms:`, content.path);
      }
      
      // Don't disable keyboard for prompt piece (check if path contains 'prompt')
      if (content.path && !content.path.includes("prompt")) {
        currentPieceHasKeyboard = false;
      }
      
      if (keyboard) keyboard.input.value = "";

      if (!content.taping) {
        detachMicrophone?.(); // Remove any attached microphone unless we
        //                       are taping 📼.
      }

      // Kill any previously loading media.
      keys(mediaPathsLoading).forEach((key) => mediaPathsLoading[key].abort());

      killAllSound?.(); // Kill any pervasive sounds in `speaker`.
      clearSoundSampleCache?.();

      // ⚠️ Remove any sounds that aren't in the whitelist.
      const sfxKeys = keys(sfx);
      sfxKeys.forEach((key) => {
        if (key !== sound) delete sfx[key];
      });
      if (sfxKeys.length > 0 && logs.audio && debug)
        console.log("🔉 SFX Cleaned up:", sfx);

      // Stop any playing samples.
      keys(sfxPlaying).forEach((sfx) => sfxPlaying[sfx]?.kill());

      // Reset preloading.
      window.waitForPreload = false;
      window.preloaded = false;

      // Clear any 3D content.
      ThreeD?.clear();

      // Kill the 3D engine.
      ThreeD?.kill();

      // Clear any DOM hitboxes added for Buttons that need
      // user interaction to trigger browser APIs. (Like clipboard)
      hitboxes = {};

      // Clear any DOM content that was added by a piece.
      contentFrame?.remove(); // Remove the contentFrame if it exists.
      contentFrame = undefined;

      // Clear any ticket overlay that was added by a piece.
      ticketWrapper?.remove();
      ticketWrapper = undefined;

      underlayFrame?.remove(); // Remove the underlayFrame if it exists.
      underlayFrame = undefined;
      stopTapePlayback?.();

      // Remove any event listeners added by the content frame.
      // Guard: this may be undefined in some embedded/early-load contexts.
      window.acCONTENT_EVENTS?.forEach?.((e) => e());
      window.acCONTENT_EVENTS = []; // And clear all events from the list.

      // Remove existing video tags.
      videos.forEach(({ video, buffer, getAnimationRequest }) => {
        console.log("🎥 Removing:", video, buffer, getAnimationRequest());

        if (video.srcObject) {
          const stream = video.srcObject;
          const tracks = stream.getTracks();
          tracks.forEach((track) => track.stop());
        }

        video.remove();

        // buffer.remove();
        cancelAnimationFrame(getAnimationRequest());
        handData = undefined; // Clear any handData.
      });

      videos.length = 0;
      // Note: Any other disk state cleanup that needs to take place on unload
      //       should happen here.

      // Reset the framing to a system default when unloading a disk if using
      // a customized resolution.
      // TODO: Do disks with custom resolutions need to be reset
      //       if they are being reloaded?

      if (fixedWidth && fixedHeight) {
        freezeFrame = true;
        freezeFrameGlaze = glaze.on;

        freezeFrameCan.width = imageData.width;
        freezeFrameCan.height = imageData.height;

        fixedWidth = undefined;
        fixedHeight = undefined;
        needsReframe = true;
      }

      if (lastGap !== 0) {
        // lastGap = 0; No longer needed... 22.10.04.15.28
        freezeFrame = true;
        freezeFrameCan.width = imageData.width;
        freezeFrameCan.height = imageData.height;
        needsReframe = true;
      }

      // Turn off glaze.
      glaze.on = false;
      canvas.style.removeProperty("opacity");

      if (pen) pen.events.length = 0; // Clear pen events.
      keyboard.events.length = 0; // Clear keyboard events.
      gamepad.clearEvents(); // Clear gamepad events.

      // Clear when events but preserve core signal handlers.
      const coreHandlers = {};
      if (whens["recorder:cut"]) {
        coreHandlers["recorder:cut"] = whens["recorder:cut"];
        // console.log("📻 Preserving recorder:cut handler during reset");
      }
      whens = coreHandlers;

      // Close (defocus) software keyboard if we are NOT entering the prompt.
      if (content.text && content.text.split("~")[0] !== "prompt") {
        document.querySelector("#software-keyboard-input")?.blur();
      }
      // keyboard.events.push({ name: "keyboard:close" });

      setMetatags(content.meta);

      // TODO: Make this automatic for pieces that use 3d.
      if (
        content.text === "wand" ||
        content.text?.indexOf("wand") === 0 ||
        content.text === "oldwand" ||
        content.text?.indexOf("oldwand") === 0
      ) {
        loadThreeD();
      }

      // Show an "audio engine: off" message.
      //if (content.noBeat === false && audioContext?.state !== "running") {
      //bumper.innerText = "audio engine off";
      //modal.classList.add("on");
      //}

      // Clear the ThreeD buffer.
      // ThreeD.clear();      // Emit a push state for the old disk if it was not the first. This is so
      // a user can use browser history to switch between disks.
      if ((content.pieceCount > 0 || content.alias === true) && !frozenUrlPath) {
        if (content.fromHistory === false /*&& window.origin !== "null"*/) {
          // Handle URL encoding for different piece types
          let urlPath;
          if (content.text === "/prompt") {
            urlPath = "/";
          } else if (
            content.path === "aesthetic.computer/disks/painting" &&
            content.hash &&
            /^[A-Za-z0-9]{3,12}$/.test(content.hash)
          ) {
            // Preserve short painting codes as a pure hash URL
            urlPath = "/#" + content.hash;
          } else if (
            content.path === "aesthetic.computer/disks/prompt" &&
            content.params &&
            content.params.length > 0 &&
            isKidlispSource(content.params[0])
          ) {
            // For prompt piece with kidlisp parameters, preserve the prompt~ URL structure
            urlPath = "/prompt~" + encodeKidlispForUrl(content.params[0]);
          } else if (isKidlispSource(content.text)) {
            // For standalone kidlisp pieces, use centralized URL encoding
            urlPath = "/" + encodeKidlispForUrl(content.text);
          } else {
            // For regular pieces, use normal text
            // Encode {# as {%23 to preserve stample codes in clock syntax
            // e.g., clock~{#abc}cdefg becomes clock~{%23abc}cdefg in URL
            let encodedText = content.text;
            if (encodedText && encodedText.includes("{#")) {
              encodedText = encodedText.replace(/\{#/g, "{%23");
            }
            urlPath = "/" + encodedText;
          }

          history.pushState(
            "",
            document.title,
            urlPath, // Replace "prompt" with "/".
          );
          window.parent?.postMessage(
            {
              type: "url:updated",
              slug: (() => {
                if (content.text?.startsWith("/")) {
                  return content.text.slice(1);
                } else if (
                  content.path === "aesthetic.computer/disks/painting" &&
                  content.hash &&
                  /^[A-Za-z0-9]{3,12}$/.test(content.hash)
                ) {
                  return "#" + content.hash;
                } else if (
                  content.path === "aesthetic.computer/disks/prompt" &&
                  content.params &&
                  content.params.length > 0 &&
                  isKidlispSource(content.params[0])
                ) {
                  return "prompt~" + encodeKidlispForUrl(content.params[0]);
                } else if (isKidlispSource(content.text)) {
                  return encodeKidlispForUrl(content.text);
                } else {
                  return content.text;
                }
              })(),
            },
            "*",
          );
        } // Replace the state if we are running an aliased `load` or `jump`.
        // (That doesn't avoid the history stack.)
        // Note: History state changes do not work in a sandboxed iframe!
        if (
          content.fromHistory === true &&
          content.alias === false //&&
          // window.origin !== "null"
        ) {
          try {
            // Handle URL encoding for different piece types
            let urlPath;
            if (content.text === "/prompt") {
              urlPath = "/";
            } else if (
              content.path === "aesthetic.computer/disks/painting" &&
              content.hash &&
              /^[A-Za-z0-9]{3,12}$/.test(content.hash)
            ) {
              urlPath = "/#" + content.hash;
            } else if (
              content.path === "aesthetic.computer/disks/prompt" &&
              content.params &&
              content.params.length > 0 &&
              isKidlispSource(content.params[0])
            ) {
              // For prompt piece with kidlisp parameters, preserve the prompt~ URL structure
              urlPath = "/prompt~" + encodeKidlispForUrl(content.params[0]);
            } else if (isKidlispSource(content.text)) {
              // For standalone kidlisp pieces, use centralized URL encoding
              urlPath = "/" + encodeKidlispForUrl(content.text);
            } else {
              // For regular pieces, use normal encoding
              urlPath = "/" + content.text;
            }

            if (!checkPackMode()) {
              history.replaceState("", document.title, urlPath);
            }
          } catch (err) {
            console.warn("⚠️ Couldn't change url state. Going too fast!? ➿🚗");
          }
        }
      }

      UI.spinnerReset(); // Reset the timer on the yellow UI loading spinner.

      if (content.pieceHasSound && !audioContext) {
        // Enable sound engine on interaction.
        window.addEventListener("keydown", activateSound, { once: true });
        window.addEventListener("pointerdown", activateSound, { once: true });
      }

      } catch (err) {
        console.error("🛑 BIOS disk-loaded handler error:", err);
      } finally {
        if (DEBUG_MESSAGE_FLOW) {
          console.log(`🔍 BIOS: About to send loading-complete for ${content.text}`);
        }
        try {
          send({ type: "loading-complete" });
          // 🎨 Force immediate repaint after disk loads to clear previous piece's frame
          send({ type: "needs-paint" });
        } catch (e) {
          console.error("🛑 BIOS failed to send loading-complete:", e);
        }
        if (DEBUG_MESSAGE_FLOW) {
          console.log(`🔍 BIOS: loading-complete sent for ${content.text}`);
        }
      }
      return;
    }

    if (type === "forms") {
      const willBake = content.cam !== undefined;

      if (willBake) {
        // if (ThreeD?.status.alive === false) ThreeD.initialize(wrapper);

        // Add / update forms in a queue and then run all the bakes in render.
        ThreeDBakeQueue.push(() => {
          return ThreeD?.bake(content, screen, {
            width: projectedWidth,
            height: projectedHeight,
          });
        });

        //send({ type: "forms:baked", content: true });
      } else {
        //send({ type: "forms:baked", content: false });
      }

      // TODO: Measure the time this takes.
      //const pixels = ThreeD.bake(content, screen, {width: projectedWidth, height: projectedHeight});
      // bakedCan.width = screen.width;
      // bakedCan.height = screen.height;
      // bakedCtx.drawImage(ThreeD.domElement, 0, 0);
      // const pixels = bakedCtx.getImageData(0, 0, screen.width, screen.height).data;

      // send({
      //   type: "forms:baked",
      //   content: { width: screen.width, height: screen.height, pixels },
      // }, [pixels]);

      return;
    }

    if (type === "$creenshot") {
      needs$creenshot = (data) => receivedDownload({ ...content, data });
      return;
    }

    if (type === "keyboard:enabled") {
      currentPieceHasKeyboard = true;
      keyboardFocusLock = false;
      keyboardSoftLock = false;
      // console.log("enabling keyboard...");
      return;
    }

    if (type === "keyboard:disabled") {
      currentPieceHasKeyboard = false;
      return;
    }

    if (type === "keyboard:close") {
      // if (keyboardFocusLock) return; // Deprecated: 23.10.02.23.18
      console.log("⌨️🔴 [bios keyboard:close handler] calling keyboard.input.blur()");
      keyboard?.input.blur();
      return;
    }

    if (type === "keyboard:open") {
      console.log("⌨️🟢 [bios keyboard:open handler] calling keyboard.input.focus()");
      if (keyboardFocusLock) return;
      keyboardFocusLock = false;
      currentPieceHasKeyboard = true;
      keyboard?.input.focus();
      // if (keyboard) keyboard.needsImmediateOpen = true; // For iOS.
      return;
    }

    // Prevents any touch or keyboard activation events directly on the input.
    if (type === "keyboard:soft-lock") {
      keyboardSoftLock = true;
      console.log("⌨️🔒 [bios] keyboard:soft-lock received, keyboardSoftLock =", keyboardSoftLock);
    }

    if (type === "keyboard:soft-unlock") {
      keyboardSoftLock = false;
      keyboardFocusLock = false;
      console.log("⌨️🔓 [bios] keyboard:soft-unlock received, keyboardSoftLock =", keyboardSoftLock);
    }

    if (type === "keyboard:lock") {
      keyboardFocusLock = true;
      if (logs.hid && debug) console.log("⌨️ Virtual Keyboard: Locked");
      return;
    }

    if (type === "keyboard:unlock") {
      keyboardFocusLock = false;
      if (logs.hid && debug) console.log("⌨️ Virtual Keyboard: Unlocked");
      return;
    }

    if (type === "keyboard:cursor") {
      const input = keyboard.input;
      // Get the current position of the caret

      // Quit if keyboard is not open.
      if (document.activeElement !== keyboard.input) return;

      if (typeof content === "number") {
        const currentPosition =
          input.selectionDirection === "backward"
            ? input.selectionStart
            : input.selectionEnd;
        let newPosition = currentPosition + content;
        if (newPosition < 0) newPosition = 0;
        if (newPosition > input.value.length) newPosition = input.value.length;
        input.setSelectionRange(newPosition, newPosition);
      } else {
        // Set based on a specific value.
        input.setSelectionRange(content.cursor, content.cursor);
      }
      return;
    }

    if (type === "keyboard:text:replace") {
      const input = keyboard.input;
      input.value = content.text;
      if (content.cursor && document.activeElement === keyboard.input)
        input.setSelectionRange(content.cursor, content.cursor);
      return;
    }

    if (type === "gpu-event") {
      ThreeD?.handleEvent(content);
      return;
    }

    // 📊 DOM Stats Overlay Control
    if (type === "stats-overlay") {
      if (content.enabled !== undefined) {
        statsOverlayEnabled = content.enabled;
        statsOverlay.style.display = statsOverlayEnabled ? "block" : "none";
        if (statsOverlayEnabled) {
          statsLastTime = performance.now();
          statsFrameCount = 0;
        }
      }
      if (content.data) {
        Object.assign(statsOverlayData, content.data);
      }
      return;
    }

    if (type === "webgpu-command") {
      // Use new backend system if available, fallback to old WebGPU
      if (activeGPUBackend) {
        activeGPUBackend.handleCommand(content);
      } else {
        WebGPU.handleCommand(content);
      }
      return;
    }

    // Adding custom DOM content.
    if (type === "content-create") {
      // Create a DOM container, if it doesn't already exist,
      // and add it here along with the requested content in the
      // template
      if (!contentFrame) {
        contentFrame = document.createElement("div");
        contentFrame.id = "content";
        wrapper.appendChild(contentFrame);
        contentFrame.innerHTML += content.content; // Add content to contentFrame.
      } else {
        contentFrame.innerHTML += content.content; // Add content to contentFrame.
      }

      // Evaluate any added scripts inside of contentFrame.
      // TODO: This should only evaluate new scripts, as they are added...
      // It should also run if new scripts are added with the `html` function.
      const script = contentFrame.querySelector("script");

      if (script && !script.dataset.evaluated) {
        if (script?.src.length > 0) {
          const s = document.createElement("script");
          s.type = "module";
          // s.onload = callback; // s.onerror = callback;

          // The hash `time` parameter busts the cache so that the environment is
          // reset if a disk is re-entered while the system is running.
          // Why a hash? See also: https://github.com/denoland/deno/issues/6946#issuecomment-668230727
          s.src = script.src + "#" + Date.now();
          contentFrame.appendChild(s); // Re-insert the new script tag.
          script.remove(); // Remove old script element.
          s.dataset.evaluated = true;
        } else if (script?.innerText.length > 0) {
          window.eval(script.innerText);
          script.dataset.evaluated = true;
        }
      }

      send({
        type: "content-created",
        content: { id: content.id, response: "Content was made!" }, // TODO: Return an API / better object?
      });

      return;
    }

    // Removing custom DOM content.
    if (type === "content-remove") {
      // Clear any DOM content that was added by a piece.
      contentFrame?.remove(); // Remove the contentFrame if it exists.
      contentFrame = undefined;
      // Remove any event listeners added by the content frame.
      window?.acCONTENT_EVENTS.forEach((e) => e());
      window.acCONTENT_EVENTS = []; // And clear all events from the list.
      return;
    }

    if (type === "signal") {
      if (debug) console.log("📻 Signal received:", content);
      if (typeof content === "string") content = { type: content };
      
      console.log(`📻 Signal processing: type="${content.type}", handler exists:`, !!whens[content.type]);
      
      if (whens[content.type]) {
        try {
          console.log(`📻 Calling signal handler for: ${content.type}`);
          await whens[content.type](content.content);
          console.log(`📻 Signal handler completed for: ${content.type}`);
        } catch (error) {
          console.error(`📻 Error in signal handler for ${content.type}:`, error);
          console.error(`📻 Stack trace:`, error.stack);
        }
      } else {
        console.warn(`📻 No handler found for signal: ${content.type}`);
      }
    }

    // 📦 Storage

    // Can store data to localStorage (user settings),
    //                   indexedDB (large files)
    //                   remote (with user account or anonymous)

    // *** 🏪 Store: Persist ***
    if (type === "store:persist") {
      // Local Storage
      if (content.method === "local") {
        try {
          window.safeLocalStorageSet(content.key, JSON.stringify(content.data));
        } catch (e) {
          // console.warn(e);
        }

        if (debug && logs.store)
          console.log("📦 Persisted locally:", content, localStorage);
      }

      // IndexedDB
      // Potentially use this library: github.com/jakearchibald/idb
      // For images: https://hacks.mozilla.org/2012/02/storing-images-and-files-in-indexeddb/
      // Using: https://github.com/jakearchibald/idb
      // See also: web.dev/indexeddb-best-practices
      if (content.method === "local:db") {
        await Store.set(content.key, content.data);
        // const set = await Store.set(content.key, content.data);
        // const get = await Store.get(content.key);
        if (debug && logs.store)
          console.log("📦 Persisted on local:db:", content);
      }

      if (content.method === "remote:temporary") {
        // Upload to S3 with a code.
      }

      if (content.method === "remote:permanent") {
        // Combine token-gated web3 authentication here along
        // with IPFS storage.
        // This would be used as part of a `mint` function. 22.10.04.11.31
      }

      return;
    }

    // Store: Retrieve
    if (type === "store:retrieve") {
      if (content.method === "local") {
        let data;

        if (!window.acLOCALSTORAGE_BLOCKED) {
          try {
            const item = window.safeLocalStorageGet(content.key);
            if (item) data = JSON.parse(item);
          } catch (err) {
            console.warn("📦 Retrieval error:", err);
            // Probably in a sandboxed environment here...
          }
        }

        if (debug && logs.store)
          console.log("📦 Retrieved local data:", content.key, data);

        send({
          type: "store:retrieved",
          content: { key: content.key, data },
        });
      }

      if (content.method === "local:db") {
        const retrievedContent = await Store.get(content.key);
        if (debug && logs.store)
          console.log(
            "📦 Retrieved local:db data:",
            content.key,
            retrievedContent,
          );
        send({
          type: "store:retrieved",
          content: { key: content.key, data: retrievedContent },
        });
      }

      return;
    }

    // Store: Delete
    if (type === "store:delete") {
      if (content.method === "local") {
        if (debug && logs.store)
          console.log("📦 Delete local data:", content.key);
        window.safeLocalStorageRemove(content.key);
        send({
          type: "store:deleted",
          content: { key: content.key, data: true },
        });
        // Just assume this is deleted.
      }

      if (content.method === "local:db") {
        const hasKey = (await Store.keys())?.includes(content.key);
        let deleted;

        if (hasKey) {
          await Store.del(content.key);
          const alteredKeys = await Store.keys();
          deleted = !alteredKeys.includes(content.key);
        } else {
          deleted = false;
        }

        if (debug && logs.store)
          console.log("📦 Delete local:db data:", content.key, deleted);
        send({
          type: "store:deleted",
          content: { key: content.key, data: true },
        });
      }
      return;
    }

    if (type === "meta") {
      setMetatags(content);
      return;
    }

    if (type === "refresh") {
      // Reconstruct URL with preserved parameters (nogap, nolabel, duration)
      const currentUrl = new URL(window.location);

      // Add preserved parameters back to the URL
      if (preservedParams.nogap) {
        currentUrl.searchParams.set("nogap", preservedParams.nogap);
      }
      if (preservedParams.nolabel) {
        currentUrl.searchParams.set("nolabel", preservedParams.nolabel);
      }
      if (preservedParams.duration) {
        currentUrl.searchParams.set("duration", preservedParams.duration);
      }

      // Update the URL and reload
      window.location.href = currentUrl.toString();
      return;
    }

    if (type === "web") {
      // console.log("Jumping to:", content.url, content.blank);
      if (content.blank === true) {
        if (Aesthetic) {
          iOSAppSend({ type: "url", body: content.url });
        } else if (window.acVSCODE && window.parent !== window) {
          // In VSCode extension, send message to parent to handle external URL
          window.parent.postMessage(
            {
              type: "openExternal",
              url: content.url,
            },
            "*",
          );
        } else {
          window.open(content.url); // Open URL in a new tab
        }
      } else {
        window.location.href = content.url; // Redirect in the current tab
      }
      return;
    }

    if (type === "preload-ready") {
      window.preloaded = true;
      return;
    }

    if (type === "wait-for-preload") {
      window.waitForPreload = true;
      return;
    }

    // Deprecated in favor of frame-synced communication.
    if (type === "beat") {
      updateSynths(content);
      return;
    }

    if (type === "beat:skip") {
      beatSkip?.();
      return;
    }
    if (type === "synth:update") {
      updateSound?.(content);
      return;
    }

    if (type === "bubble:update") {
      updateSound?.(content);
      return;
    }

    // 🏠🧩 Room/Reverb + Glitch control messages
    if (
      type === "room:toggle" ||
      type === "room:set" ||
      type === "room:get" ||
      type === "glitch:toggle" ||
      type === "glitch:set" ||
      type === "glitch:get"
    ) {
      sendRoomMessage?.(type, content);
      return;
    }

    if (type === "audio:reinit") {
      await reinitAudioSystem(content || {});
      return;
    }

    if (type === "download") {
      receivedDownload(content);
      return;
    }

    if (type === "upload") {
      // Extract recordingSlug if present in content
      const { recordingSlug, ...uploadData } = content;
      console.log("🔍 UPLOAD MESSAGE HANDLER: recordingSlug=", recordingSlug, "content keys=", Object.keys(content));
      receivedUpload(uploadData, "upload", null, recordingSlug);
      return;
    }

    if (type === "import") {
      receivedImport(content);
      return;
    }

    if (type === "microphone") {
      receivedMicrophone(content);
      return;
    }

    if (type === "notifications:web") {
      window.acRequestNotifications?.(content);
      return;
    }

    if (type === "microphone-record") {
      requestMicrophoneRecordingStart?.();
      return;
    }

    if (type === "microphone-cut") {
      requestMicrophoneRecordingStop?.();
      return;
    }

    if (type === "get-microphone-amplitude") {
      requestMicrophoneAmplitude?.();
      return;
    }

    if (type === "get-microphone-waveform") {
      requestMicrophoneWaveform?.();
      return;
    }

    if (type === "get-microphone-recording-buffer") {
      requestMicrophoneRecordingBuffer?.();
      return;
    }

    if (type === "get-waveforms") {
      requestSpeakerWaveforms?.();
      return;
    }

    if (type === "get-amplitudes") {
      requestSpeakerAmplitudes?.();
      return;
    }

    if (type === "get-frequencies") {
      requestSpeakerFrequencies?.();
      return;
    }

    if (type === "get-microphone-pitch") {
      requestMicrophonePitch?.();
      return;
    }

    if (type === "video") {
      receivedVideo(content);
      return;
    }

  // Audio-visual recording of the main audio track and microphone.
  if (type === "recorder:rolling") {
    // mediaRecorderBlob = null; // Clear the current blob when we start recording.

    // Store recording metadata for filename generation
    let recordingOptions = {};
    let actualContent = content;
    
    if (typeof content === "object") {
      // New format with piece metadata
      recordingOptions = {
        pieceName: content.pieceName || "tape",
        pieceParams: content.pieceParams || "",
        originalCommand: content.originalCommand || "",
        intendedDuration: content.intendedDuration || null, // Store intended duration from tape command
        mystery: content.mystery || false, // Store mystery flag to hide command in filename
        frameMode: content.frameMode || false, // Store frame-based recording mode
        frameCount: content.frameCount || null, // Store target frame count
        kidlispFps: content.kidlispFps || null, // Store KidLisp framerate from fps function
        cleanMode: content.cleanMode || false, // Store clean mode flag (no overlays, no progress bar)
        showTezosStamp: content.showTezosStamp || false // Store Tezos stamp visibility
      };
      actualContent = content.type || "video";
    } else {
      // Legacy string format - default to "tape"
      recordingOptions = {
        pieceName: "tape",
        pieceParams: "",
        originalCommand: "",
        intendedDuration: null,
        mystery: false,
        cleanMode: false, // Default to false for legacy recordings
        kidlispFps: null // Initialize for KidLisp framerate updates
      };
      actualContent = content;
    }
    
    // Store for use during export
    window.currentRecordingOptions = recordingOptions;
    console.log("🎬 Recording options set:", JSON.stringify(recordingOptions, null, 2));

    const colonSplit = actualContent.split(":");
    // tiktokVideo = colonSplit[1] === "tiktok";
    actualContent = colonSplit[0];

    // Always clear frames and underlay when starting new recording
    // (Must happen before the paused check to avoid early return skipping cleanup)
    recordedFrames.length = 0;
    recordedPieceChanges.length = 0; // Clear piece changes tracking
    startTapePlayback = undefined;
    console.log("🎬 Initialized recording state");
    
    // Clear any active underlay from previous playback
    if (underlayFrame) {
      const media = underlayFrame.querySelector("video, audio");
      if (media?.src) URL.revokeObjectURL(media.src);
      underlayFrame?.remove();
      underlayFrame = undefined;
      console.log("🎬 Cleared underlay from previous playback");
    }

    if (mediaRecorder && mediaRecorder.state === "paused") {
        mediaRecorder.resume();
        mediaRecorderStartTime = performance.now();
        // Initialize recording start timestamp for frame recording if not already set
        if (!window.recordingStartTimestamp) {
          window.recordingStartTimestamp = Date.now();
        }
        send({
          type: "recorder:rolling:resumed",
          content: {
            mime: mediaRecorder.mimeType,
            time: audioContext?.currentTime,
          },
        });
        if (debug && logs.recorder)
          console.log("🔴 Recorder: Resumed", actualContent);
        return;
      }

      if (mediaRecorder && mediaRecorder.state !== "paused") {
        stop();
      }

      function stop() {
        
        // Properly stop and clean up MediaRecorder before trashing it
        if (mediaRecorder && mediaRecorder.state !== "inactive") {
          mediaRecorder.stop();
        }
        mediaRecorder = undefined; // ❌ Trash the recorder.
        mediaRecorderStartTime = undefined;
        mediaRecorderDuration = undefined; // Reset to undefined for clean initialization
        mediaRecorderChunks.length = 0;
        
        // Clean up raw audio processor
        if (rawAudioProcessor) {
          try {
            rawAudioProcessor.disconnect();
            rawAudioProcessor = null;
          } catch (error) {
            console.warn("Error disconnecting raw audio processor:", error);
          }
        }
        rawAudioData = [];
        
        // Clear recording timestamp for next recording
        if (window.recordingStartTimestamp) {
          delete window.recordingStartTimestamp;
        }
        if (window.currentTapeProgress !== undefined) {
          delete window.currentTapeProgress;
        }
        if (window.lastVideoProgress !== undefined) {
          delete window.lastVideoProgress;
        }
        // Clear cached tape data when stopping to start a new recording
        Store.del("tape").catch(() => {}); // Clear cache, ignore errors
      }

      let mimeType;

      // if (content === "audio" || content === "video") {

      // if (MediaRecorder.isTypeSupported(content + "/mp4")) {
      //   mimeType = content + "/mp4"; // This is the setup for Safari.
      // } else if (MediaRecorder.isTypeSupported(content + "/webm")) {
      //   mimeType = content + "/webm"; // And for Chrome & Firefox.
      //   // mimeType = content + "/webm;codecs=h264"; // Possible optimization.
      // } else {
      //   console.error("🔴 Mimetypes mp4 and webm are unsupported.");
      // }

      // } else {
      //   console.error("🔴 Option must be 'audio' or 'video'.");
      // }

      // Set the audio recorder mimetypes.
      // TODO: Should WAV also be here?
      if (MediaRecorder.isTypeSupported("audio/webm;codecs=opus")) {
        mimeType = "audio/webm;codecs=opus";
      } else if (MediaRecorder.isTypeSupported("audio/ogg;codecs=vorbis")) {
        mimeType = "audio/ogg;codecs=vorbis";
      } else if (MediaRecorder.isTypeSupported("audio/aac")) {
        mimeType = "audio/aac";
      }

      let options = { mimeType };

      if (actualContent === "video") {
        // Start recording audio.
        try {
          mediaRecorder = new MediaRecorder(audioStreamDest.stream, options);
          console.log("🎬 MediaRecorder created successfully:", mediaRecorder.state);
        } catch (error) {
          console.error("MediaRecorder creation failed:", error);
          return;
        }
        
        // Set up raw audio capture for playback - but don't connect yet
        if (audioContext && sfxStreamGain) {
          try {
            rawAudioData = [];
            rawAudioSampleRate = audioContext.sampleRate;
            
            // Create a script processor node to capture raw audio
            rawAudioProcessor = audioContext.createScriptProcessor(4096, 2, 2);
            let rawAudioStartOffset = 0; // Track when audio capture actually starts
            
            rawAudioProcessor.onaudioprocess = function(event) {
              // Only start capturing after MediaRecorder has started
              if (mediaRecorderStartTime === undefined) return;
              
              const inputBuffer = event.inputBuffer;
              const leftChannel = inputBuffer.getChannelData(0);
              const rightChannel = inputBuffer.getChannelData(1);
              
              // Store the audio data with timing information
              rawAudioData.push({
                left: new Float32Array(leftChannel),
                right: new Float32Array(rightChannel),
                timestamp: performance.now() - mediaRecorderStartTime // Relative to recording start
              });
            };
            
            console.log("🎵 Raw audio capture prepared (will connect when recording starts)");
          } catch (error) {
            console.error("Raw audio capture setup failed:", error);
          }
        }
      }

      // 🗺️ mediaRecorder:Start
      if (mediaRecorder) {
        console.log("🎬 Setting up MediaRecorder callbacks");
        mediaRecorder.onstart = function () {
          // mediaRecorderResized = false;
          mediaRecorderStartTime = performance.now();
          // Initialize recording start timestamp for frame recording
          window.recordingStartTimestamp = Date.now();
          window.currentTapeProgress = 0;
          window.lastVideoProgress = 0;
          
          // DON'T connect raw audio processor immediately anymore
          // We'll connect it when we detect actual audio playback starting
          // This accounts for pieces like wipppps that have audio start detection delays
          if (rawAudioProcessor && sfxStreamGain) {
            console.log("🎵 Raw audio processor ready - will connect when audio actually starts");
            
            // Set up audio start detection to sync raw audio capture properly
            let audioConnected = false;
            let audioStartDetectionInterval;
            let audioStartDetectionTimeout;
            let detectionStartTime = performance.now();
            
            // Monitor for actual audio playback by checking multiple indicators
            audioStartDetectionInterval = setInterval(() => {
              try {
                if (!audioConnected) {
                  let shouldConnect = false;
                  
                  // Method 1: Check if tape progress is advancing (indicates actual playback)
                  if (window.currentTapeProgress && window.currentTapeProgress > 0) {
                    shouldConnect = true;
                    console.log("🎵 Audio detected via tape progress:", window.currentTapeProgress);
                  }
                  
                  // Method 2: Check the sfxStreamGain for connected nodes (actual audio flow)
                  if (!shouldConnect && sfxStreamGain && sfxStreamGain.numberOfOutputs > 0) {
                    // Audio nodes are connected to sfxStreamGain, indicating audio is flowing
                    shouldConnect = true;
                    console.log("🎵 Audio detected via sfxStreamGain connections");
                  }
                  
                  // Method 3: Simple delay-based fallback - after 100ms, assume audio is starting
                  // This handles cases where we can't detect programmatically but need to stay close
                  const detectionTime = performance.now() - detectionStartTime;
                  if (!shouldConnect && detectionTime > 100) {
                    shouldConnect = true;
                    console.log("🎵 Audio connection triggered by 100ms fallback");
                  }
                  
                  if (shouldConnect) {
                    // Audio is actually playing - connect the processor now
                    sfxStreamGain.connect(rawAudioProcessor);
                    // ALSO connect microphone stream if available for mic audio capture
                    if (micStreamGain) {
                      micStreamGain.connect(rawAudioProcessor);
                      console.log("🎵 Microphone stream connected to raw audio capture");
                    }
                    rawAudioProcessor.connect(audioContext.destination);
                    audioConnected = true;
                    
                    const detectionDelay = performance.now() - mediaRecorderStartTime;
                    console.log(`🎵 Raw audio capture connected after ${detectionDelay.toFixed(1)}ms detection delay`);
                    
                    // Clean up detection
                    clearInterval(audioStartDetectionInterval);
                    clearTimeout(audioStartDetectionTimeout);
                  }
                }
              } catch (error) {
                console.warn("Audio start detection error:", error);
              }
            }, 16);
            
            // Fallback: Connect after 1 second regardless to prevent lost audio
            audioStartDetectionTimeout = setTimeout(() => {
              if (!audioConnected && rawAudioProcessor && sfxStreamGain) {
                try {
                  sfxStreamGain.connect(rawAudioProcessor);
                  // ALSO connect microphone stream if available for mic audio capture
                  if (micStreamGain) {
                    micStreamGain.connect(rawAudioProcessor);
                    console.log("🎵 Microphone stream connected to raw audio capture (fallback)");
                  }
                  rawAudioProcessor.connect(audioContext.destination);
                  console.log("🎵 Raw audio capture connected after 1s timeout fallback");
                } catch (error) {
                  console.error("Fallback audio connection failed:", error);
                }
              }
              clearInterval(audioStartDetectionInterval);
            }, 1000);
          }
          
          // Clear KidLisp FPS timeline for new recording
          window.kidlispFpsTimeline = [];
          console.log("🎬 Cleared KidLisp FPS timeline for new recording");
          
          // 🎵 ENSURE AUDIO CONTEXT IS ACTIVATED FOR PROGRAMMATIC PLAYBACK
          // This is critical for pieces like wipppps that need to auto-play during recording
          if (audioContext && audioContext.state !== 'running') {
            console.log("🎵 TAPE_START: Activating audio context for programmatic playback, current state:", audioContext.state);
            const resumeStartTime = performance.now();
            audioContext.resume().then(() => {
              const resumeEndTime = performance.now();
              window.audioContextResumeTimestamps.requestedAt = resumeStartTime;
              window.audioContextResumeTimestamps.completedAt = resumeEndTime;
              console.log("🎵 TAPE_START: Audio context resumed, new state:", audioContext.state);
              
              // Notify video piece of AudioContext state change
              send({ 
                type: "tape:audio-context-state", 
                content: { 
                  state: audioContext.state,
                  hasAudio: true
                } 
              });
            }).catch(error => {
              console.warn("🎵 TAPE_START: Audio context resume failed:", error.message);
            });
          } else if (audioContext) {
            console.log("🎵 TAPE_START: Audio context already running, state:", audioContext.state);
            // Even if running, explicitly call resume to ensure user gesture activation
            const resumeStartTime = performance.now();
            audioContext.resume().then(() => {
              const resumeEndTime = performance.now();
              window.audioContextResumeTimestamps.requestedAt = resumeStartTime;
              window.audioContextResumeTimestamps.completedAt = resumeEndTime;
              
              // Notify video piece of AudioContext state change
              send({ 
                type: "tape:audio-context-state", 
                content: { 
                  state: audioContext.state,
                  hasAudio: true
                } 
              });
            }).catch(error => {
              console.log("🎵 TAPE_START: Audio context re-resume (user gesture activation):", error.message);
            });
          }
          
          // Audio unlock tone removed - it was causing unwanted sound on piece load
          
          // 🎵 PRELOAD AND DECODE PIECE AUDIO FOR INSTANT PLAYBACK
          // For pieces like wipppps that need immediate audio, force decode their audio assets
          if (actualContent.pieceName === 'wipppps') {
            try {
              console.log("🎵 TAPE_START: Pre-decoding wipppps audio for instant playback");
              // Force decode the wipppps audio file to ensure it's ready as Web Audio buffer
              const wippppsUrl = "https://assets.aesthetic.computer/wipppps/zzzZWAP.wav";
              
              // Check if already decoded, if not, trigger decode
              if (!sfx[wippppsUrl] || typeof sfx[wippppsUrl] === 'string') {
                console.log("🎵 TAPE_START: Triggering decode for", wippppsUrl);
                decodeSfx(wippppsUrl).then(() => {
                  console.log("🎵 TAPE_START: wipppps audio decoded successfully as Web Audio buffer");
                }).catch(error => {
                  console.log("🎵 TAPE_START: wipppps audio decode failed:", error);
                });
              } else {
                console.log("🎵 TAPE_START: wipppps audio already decoded as Web Audio buffer");
              }
            } catch (error) {
              console.log("🎵 TAPE_START: wipppps audio pre-decode error:", error.message);
            }
          }
          
          console.log(`🎬 🔴 Recording STARTED at ${mediaRecorderStartTime}, frame capture enabled, recordedFrames: ${recordedFrames.length}`);
          
          send({
            type: "recorder:rolling:started",
            content: {
              mime: mediaRecorder.mimeType,
              time: audioContext?.currentTime,
              cleanMode: recordingOptions.cleanMode,
            },
          });
          if (debug && logs.recorder)
            console.log("🔴 Recorder: Rolling", actualContent);

          // window.addEventListener("resize", () => (mediaRecorderResized = true), {
          // once: true,
          // });
        };
      }

      // 🗺️ mediaRecorder:Stop (Recorder Printing)
      if (mediaRecorder) {
        mediaRecorder.onstop = async function (evt) {
          stop();
        // recordingDuration = (performance.now() - recordingStartTime) / 1000;

        // Reset global streamCanvas state.
        // streamCanvasContext = undefined;
        // resizeToStreamCanvas = null;

        // let blob = new Blob(chunks, {
        //  type: options.mimeType,
        // });

        // Load FFmpeg so the recording can be transcribed to a proper video format.
        // if (content === "video") {
        //   if (options.mimeType === "video/mp4") {
        //     console.warn("Encoding can be skipped!");
        //     // TODO: Skip encoding.
        //   }

        //   const { createFFmpeg, fetchFile } = await loadFFmpeg();

        //   let transcodeProgress = 0;

        //   const ffmpeg = createFFmpeg({
        //     log: debug,
        //     progress: (p) => {
        //       // Send a message to the piece that gives the transcode progress.
        //       let time = p.time;
        //       if (time === undefined) {
        //         if (transcodeProgress === 0) {
        //           time = 0;
        //         } else {
        //           time = recordingDuration;
        //         }
        //       }
        //       transcodeProgress = min(1, time / recordingDuration);
        //       send({
        //         type: "recorder:transcode-progress",
        //         content: transcodeProgress,
        //       });
        //     },
        //   });

        //   ffmpeg.setLogging(debug); // Enable ffmpeg logging only if we are in `debug` mode.

        //   await ffmpeg.load();
        //   ffmpeg.FS("writeFile", "input.video", await fetchFile(blob));

        //   await ffmpeg.run(
        //     "-i",
        //     "input.video",
        //     "-movflags",
        //     "+faststart",
        //     "-vf",
        //     // General shaving to make even sides (required by the pixel format)
        //     // "pad=ceil(iw/2)*2:ceil(ih/2)*2",
        //     // TikTok
        //     //"fps=30, scale=1080x1920:flags=neighbor:force_original_aspect_ratio=decrease, pad=1080:1920:(ow-iw)/2:(oh-ih)/2",
        //     "fps=30",
        //     "output.mp4",
        //   );
        //   // Notes on these options:
        //   // width expression: https://stackoverflow.com/a/20848224/8146077
        //   // scaling: https://trac.ffmpeg.org/wiki/Scaling
        //   // general info: https://avpres.net/FFmpeg/im_H264

        //   const file = ffmpeg.FS("readFile", "output.mp4");

        //   blob = new Blob([file.buffer], { type: "video/mp4" }); // Re-assign blob.
        // }

        // Add the recording wrapper to the DOM, among other recordings that may exist.

        // if (debug) console.log("📼 Recorder: Printed");

        // mediaRecorderBlob = blob;

        // TODO: Store an index into the blob if its an audio clip or loaded sample.

        // send({ type: "recorder:printed", content: { id: "test-sample-id" } });
        // TODO: Can send the download code back here...
        // send({ type: "recorder:uploaded", code });

        // mediaRecorderBlob = new Blob(mediaRecorderChunks, {
        //   type: mediaRecorder.mimeType,
        // });

        // if (content === "video") {
        //   await receivedChange({
        //     data: {
        //       type: "store:persist",
        //       content: {
        //         key: "tape",
        //         method: "local:db",
        //         data: {
        //           blob: mediaRecorderBlob,
        //           duration: mediaRecorderDuration,
        //         },
        //       },
        //     },
        //   });
        // }

        // send({ type: "recorder:rolling:ended" });
        };

        mediaRecorder.ondataavailable = function (e) {
          if (e.data && e.data.size > 0) mediaRecorderChunks.push(e.data);
        };

        // Always cut off mediaRecorders on unload.
        window.addEventListener("unload", function () {
          mediaRecorder?.stop();
        });

        window.addEventListener("beforeunload", function () {
          mediaRecorder?.stop();
        });
      }

      //if (content === "video") {
      // Start media recorder once svg loads.
      //svgCursor.onload = function (e) {
      // Use small chunk sizes. (`1000` broke TikTok)
      //  mediaRecorder.start(100);
      //};
      //svgCursor.src = "/aesthetic.computer/cursors/precise.svg";
      //} else {
      // console.log("Start audio recording...");

      // Clear any cached tape data and audio before starting new recording
      Store.del("tape").catch(() => {}); // Clear cache, ignore errors
      delete sfx["tape:audio"]; // Clear any cached audio data
      mediaRecorderDuration = 0; // Reset duration for new recording
      mediaRecorderFrameCount = 0; // Reset frame capture counter for optimization

      console.log("🎬 Starting MediaRecorder, state before start:", mediaRecorder.state);
      mediaRecorder.start(100);
      console.log("🎬 MediaRecorder.start() called, state after start:", mediaRecorder.state);
      //}
      return;
    }

    if (type === "recorder:present") {
      // Check for cached video if no active recording AND no recorded frames
      if (
        (!mediaRecorder || mediaRecorder.state !== "paused") &&
        recordedFrames.length === 0
      ) {
        const cachedTape = await Store.get("tape");
        
        if (cachedTape && cachedTape.blob) {
          console.log("🎬 Cached tape structure:", {
            hasBlob: !!cachedTape.blob,
            blobSize: cachedTape.blob?.size,
            blobType: cachedTape.blob?.type,
            hasRawAudio: !!cachedTape.rawAudio,
            rawAudioSamples: cachedTape.rawAudio?.totalSamples,
            rawAudioLeftType: cachedTape.rawAudio?.left?.constructor?.name,
            rawAudioRightType: cachedTape.rawAudio?.right?.constructor?.name,
            frameCount: cachedTape.frames?.length,
            duration: cachedTape.duration,
            hasAudioContext: !!audioContext
          });
          
          // Use raw audio arrays to create AudioBuffer for playback if available
          if (cachedTape.rawAudio && audioContext) {
            try {
              console.log("🎬 📼 Creating AudioBuffer from raw audio data");
              const rawAudio = cachedTape.rawAudio;
              const audioBuffer = audioContext.createBuffer(2, rawAudio.totalSamples, rawAudio.sampleRate);
              
              audioBuffer.getChannelData(0).set(rawAudio.left);
              audioBuffer.getChannelData(1).set(rawAudio.right);
              
              sfx["tape:audio"] = audioBuffer;
              console.log(`🎬 📼 Successfully created AudioBuffer: ${rawAudio.totalSamples} samples, ${rawAudio.totalSamples / rawAudio.sampleRate}s duration`);
            } catch (error) {
              console.error("🎬 📼 Failed to create AudioBuffer from raw audio:", error);
              // Fall back to blob for export only
              sfx["tape:audio"] = await blobToArrayBuffer(cachedTape.blob);
            }
          } else if (cachedTape.rawAudio && !audioContext) {
            // Have rawAudio but no audioContext yet - store it for later conversion
            console.warn("🎬 📼 Raw audio available but no AudioContext yet - will create on first play");
            sfx["tape:audio"] = cachedTape.rawAudio; // Store rawAudio object directly
          } else if (audioContext) {
            // Try to decode the compressed audio blob
            try {
              console.log("🎬 📼 No raw audio available, attempting to decode compressed audio blob");
              const arrayBuffer = await blobToArrayBuffer(cachedTape.blob);
              const audioBuffer = await audioContext.decodeAudioData(arrayBuffer);
              sfx["tape:audio"] = audioBuffer;
              console.log(`🎬 📼 Successfully decoded audio: ${audioBuffer.duration}s, ${audioBuffer.numberOfChannels} channels`);
            } catch (error) {
              console.error("🎬 📼 Failed to decode audio blob:", error);
              // Store the blob ArrayBuffer for export only (won't be playable)
              sfx["tape:audio"] = await blobToArrayBuffer(cachedTape.blob);
              console.log("🎬 📼 Stored blob for export only (not playable)");
            }
          } else {
            // Store the blob for export purposes but we won't be able to play it
            sfx["tape:audio"] = await blobToArrayBuffer(cachedTape.blob);
          }

          // Restore frame data if available
          if (cachedTape.frames) {
            recordedFrames.length = 0;
            recordedFrames.push(...cachedTape.frames);
          }

          // Set duration if available
          if (cachedTape.duration) {
            mediaRecorderDuration = cachedTape.duration;
          }
        }
      }

      if (
        (mediaRecorder && mediaRecorder.state === "paused") ||
        recordedFrames.length > 0
      ) {
        // Handle live recording or cached video
        if (mediaRecorder && mediaRecorder.state === "paused") {
          const blob = new Blob(mediaRecorderChunks, {
            type: mediaRecorder.mimeType,
          });
          
          // Use raw audio data for live playback
          if (rawAudioData.length > 0 && audioContext) {
            try {
              console.log("🎬 📼 Creating AudioBuffer from live raw audio data");
              const totalSamples = rawAudioData.length * 4096; // 4096 samples per chunk
              const audioBuffer = audioContext.createBuffer(2, totalSamples, rawAudioSampleRate);
              
              const leftChannelData = audioBuffer.getChannelData(0);
              const rightChannelData = audioBuffer.getChannelData(1);
              
              let sampleIndex = 0;
              for (let i = 0; i < rawAudioData.length; i++) {
                const chunk = rawAudioData[i];
                for (let j = 0; j < chunk.left.length; j++) {
                  if (sampleIndex < totalSamples) {
                    leftChannelData[sampleIndex] = chunk.left[j];
                    rightChannelData[sampleIndex] = chunk.right[j];
                    sampleIndex++;
                  }
                }
              }
              
              sfx["tape:audio"] = audioBuffer;
              console.log(`🎬 📼 Successfully created live AudioBuffer: ${totalSamples} samples, ${totalSamples / rawAudioSampleRate}s duration`);
            } catch (error) {
              console.error("🎬 📼 Failed to create live AudioBuffer:", error);
              // Store compressed data as fallback for export
              sfx["tape:audio"] = await blobToArrayBuffer(blob);
            }
          } else {
            console.log("🎬 📼 No raw audio data available, storing compressed blob for export only");
            sfx["tape:audio"] = await blobToArrayBuffer(blob);
          }
        }

        underlayFrame = document.createElement("div");
        underlayFrame.id = "underlay";

        const frameCan = document.createElement("canvas");
        const fctx = frameCan.getContext("2d");

        if (recordedFrames.length > 0) {
          frameCan.width = recordedFrames[0][1].width;
          frameCan.height = recordedFrames[0][1].height;
        }

        startTapePlayback = (
          transmitProgress = true,
          doneCb,
          stream,
          render,
          overrideDurationMs = null, // Optional duration override for MP4 export
        ) => {
          if (recordedFrames.length === 0) {
            console.error("🎬 ❌ No recorded frames to play back!");
            return;
          }
          
          // Use overrideDurationMs if provided (for MP4 export), otherwise use mediaRecorderDuration
          const playbackDurationMs = overrideDurationMs || mediaRecorderDuration;
          
          let f = 0;
          let playbackStart;
          let playbackProgress = 0;
          let continuePlaying = true;
          let stopped = false;
          // Track total recording time for video export duration control
          let totalRecordingStart = null;

          let tapeSoundId;

          stopTapePlayback = () => {
            continuePlaying = false;
            stopped = true;
            // Kill all tape audio instances
            Object.keys(sfxPlaying).forEach(id => {
              if (id.startsWith("tape:audio_")) {
                sfxPlaying[id]?.kill();
                delete sfxPlaying[id];
              }
            });
          };

          let pauseStart;
          let pausedAtPosition; // Store the position when paused
          let isResuming = false; // Track if we're resuming from pause

          pauseTapePlayback = () => {
            continuePlaying = false;
            pauseStart = performance.now();
            // Store current playback position
            pausedAtPosition = (pauseStart - playbackStart) / mediaRecorderDuration;
            
            // Kill all tape audio instances since pause is not available
            Object.keys(sfxPlaying).forEach(id => {
              if (id.startsWith("tape:audio_")) {
                sfxPlaying[id]?.kill();
                delete sfxPlaying[id];
              }
            });
            send({ type: "recorder:present-paused" });
          };

          resumeTapePlayback = () => {
            if (stopped) {
              send({ type: "recorder:present-playing" });
              return startTapePlayback(true);
            }
            continuePlaying = true;
            isResuming = true; // Flag that we're resuming to prevent audio restart

            // Mark the resume time to prevent timing adjustment compensation
            const resumeTime = performance.now();
            window.tapeResumeTimestamp = resumeTime;

            // Calculate how long we were paused and adjust playback start time FIRST
            const pauseDuration = resumeTime - pauseStart;
            playbackStart += pauseDuration;

            // Restart the audio from the paused position
            const workletReady = window.audioWorkletReady === true;
            const audioContextReady = audioContext?.state === "running";
            if (!render && workletReady && audioContextReady && mediaRecorderDuration > 0) {
              const clampedPosition = Math.max(0, Math.min(1, pausedAtPosition || 0));

              // Kill any existing tape audio before starting new one
              Object.keys(sfxPlaying).forEach(id => {
                if (id.startsWith("tape:audio_")) {
                  sfxPlaying[id]?.kill();
                  delete sfxPlaying[id];
                }
              });

              tapeSoundId = "tape:audio_" + performance.now();
              playSfx(tapeSoundId, "tape:audio", {
                from: clampedPosition, // Start from the paused position
              });
            }

            window.requestAnimationFrame(update);
            send({ type: "recorder:present-playing" });
          };

          // Track scrubbing state to avoid constant audio restarts
          let isScrubbing = false;
          let lastSeekProgress = 0;
          
          seekTapePlayback = (progress, { scrubbing = false, scrubEnd = false } = {}) => {
            // Seek to a specific position in the tape (0.0 to 1.0)
            // If scrubbing=true, we mute audio instead of restarting it every frame
            // If scrubEnd=true, we sync audio to final position
            
            // Clamp progress to valid range
            progress = Math.max(0, Math.min(1, progress));
            
            // Calculate target frame
            const targetFrame = Math.floor(progress * recordedFrames.length);
            f = Math.max(0, Math.min(recordedFrames.length - 1, targetFrame));
            
            // Update playback progress
            playbackProgress = progress;
            pausedAtPosition = progress;
            lastSeekProgress = progress;
            
            // Adjust playback start time to match new position
            const currentTime = performance.now();
            playbackStart = currentTime - (progress * playbackDurationMs);
            
            // Update display with new frame
            if (recordedFrames[f]) {
              fctx.putImageData(recordedFrames[f][1], 0, 0);
              send({ type: "recorder:present-progress", content: progress });
            }
            
            const workletReady = window.audioWorkletReady === true;
            const audioContextReady = audioContext?.state === "running";
            
            // Handle scrubbing state transitions
            if (scrubbing && !isScrubbing) {
              // Starting scrub - mute audio but don't kill it
              isScrubbing = true;
              Object.keys(sfxPlaying).forEach(id => {
                if (id.startsWith("tape:audio_")) {
                  sfxPlaying[id]?.kill();
                  delete sfxPlaying[id];
                }
              });
            } else if (scrubEnd && isScrubbing) {
              // Ending scrub - restart audio at final position
              isScrubbing = false;
              if (!render && workletReady && audioContextReady && mediaRecorderDuration > 0 && continuePlaying) {
                tapeSoundId = "tape:audio_" + performance.now();
                playSfx(tapeSoundId, "tape:audio", {
                  from: progress,
                });
              }
            } else if (!scrubbing && !isScrubbing) {
              // Normal seek (not scrubbing) - sync audio immediately
              if (!render && workletReady && audioContextReady && mediaRecorderDuration > 0) {
                // Kill existing audio
                Object.keys(sfxPlaying).forEach(id => {
                  if (id.startsWith("tape:audio_")) {
                    sfxPlaying[id]?.kill();
                    delete sfxPlaying[id];
                  }
                });
                
                // Restart audio from new position if currently playing
                if (continuePlaying) {
                  tapeSoundId = "tape:audio_" + performance.now();
                  playSfx(tapeSoundId, "tape:audio", {
                    from: progress,
                  });
                }
              }
            }
            // During active scrubbing (scrubbing=true, isScrubbing=true), we don't touch audio
          };

          async function update() {
            if (!continuePlaying || !underlayFrame) {
              return;
            }

            // Hang detection: track time spent in this update cycle
            const updateStartTime = performance.now();
            let frameProcessingTime = 0;
            
            // Update last activity time for watchdog
            window.lastVideoUpdateTime = updateStartTime;
            
            // Handle tab visibility changes - pause time accumulation when hidden
            if (document.hidden && !window.tapePlaybackPaused) {
              window.tapePlaybackPaused = true;
              window.tapePlaybackPauseTime = updateStartTime;
            } else if (!document.hidden && window.tapePlaybackPaused) {
              // Tab became visible again - adjust playbackStart to skip the hidden time
              const pausedDuration = updateStartTime - window.tapePlaybackPauseTime;
              playbackStart += pausedDuration;
              window.tapePlaybackPaused = false;
              console.log(`⏸️ Tab was hidden for ${pausedDuration.toFixed(0)}ms, adjusted playback timing`);
            }

            if (f === 0) {
              // Reset pause state at beginning
              window.tapePlaybackPaused = false;
              window.tapePlaybackPauseTime = 0;
              
              // Set playback timing BEFORE starting audio to ensure sync
              if (!isResuming) {
                playbackStart = performance.now();
                playbackProgress = 0;
              }
              
              // Clear resume flag after handling frame 0
              isResuming = false;
            }

            // Handle audio playback - check if audio should be playing but isn't
            const shouldHaveAudio = !render && mediaRecorderDuration > 0;
            const hasAudioPlaying = Object.keys(sfxPlaying).some(id => id.startsWith("tape:audio_"));
            const audioContextReady = audioContext?.state === "running";
            const workletReady = window.audioWorkletReady === true;
            
            if (shouldHaveAudio && !hasAudioPlaying && audioContextReady && workletReady) {
              // Calculate CURRENT position BEFORE any decoding
              const currentPlaybackProgress = performance.now() - playbackStart; // ms
              const currentAudioPosition = mediaRecorderDuration > 0
                ? Math.max(0, Math.min(1, currentPlaybackProgress / mediaRecorderDuration))
                : 0;
              
              // Check if we need to re-decode with running AudioContext
              if (window.tapeAudioArrayBuffer && !window.tapeAudioDecodedWithRunningContext) {
                try {
                  const freshAudioBuffer = await audioContext.decodeAudioData(window.tapeAudioArrayBuffer.slice());
                  sfx["tape:audio"] = freshAudioBuffer;
                  window.tapeAudioDecodedWithRunningContext = true;
                } catch (error) {
                  console.error("🎵 Fresh decode failed:", error);
                }
              }
              
              // Use the position we calculated BEFORE decoding
              const audioPosition = currentAudioPosition;
              
              // Update global tape position for auto-start sync
              currentTapePosition = audioPosition;
              
              // Kill any existing tape audio before starting new one
              Object.keys(sfxPlaying).forEach(id => {
                if (id.startsWith("tape:audio_")) {
                  sfxPlaying[id]?.kill();
                  delete sfxPlaying[id];
                }
              });
              
              tapeSoundId = "tape:audio_" + performance.now();
              await playSfx(tapeSoundId, "tape:audio", { from: audioPosition });
            }

            // Resize fctx here if the width and
            // height is different.
            const pic = recordedFrames[f][1];
            if (
              fctx.canvas.width !== pic.width ||
              fctx.canvas.height !== pic.height
            ) {
              fctx.canvas.width = pic.width;
              fctx.canvas.height = pic.height;
            }

            fctx.putImageData(recordedFrames[f][1], 0, 0);

            // 📼 Render multi-tape manager frames (for tv.mjs smooth transitions)
            if (tapeManager) {
              tapeManager.renderFrame(performance.now());
            }

              const renderStartTime = performance.now();
            
              try {
                render?.(frameCan, playbackProgress / playbackDurationMs, f); // Render a video as needed, using this canvas and current frame index.
              } catch (error) {
                console.error(`Render function failed at frame ${f} (${((f / (recordedFrames.length - 1)) * 100).toFixed(2)}%):`, error);
                // Continue processing even if render fails
              }
            
              frameProcessingTime = performance.now() - renderStartTime;
            
            // Enhanced warning for slow frame processing in critical zone
            if (frameProcessingTime > 200) {
              console.warn(`Slow frame processing: ${frameProcessingTime.toFixed(2)}ms for frame ${f}`);
            }
            
            // Check for hang in critical 40% zone
            const currentProgress = f / (recordedFrames.length - 1);
            
            // Update global tape position for auto-start sync
            currentTapePosition = currentProgress;
            
            if (currentProgress > 0.35 && currentProgress < 0.45 && frameProcessingTime > 200) {
              console.warn(`CRITICAL ZONE SLOW PROCESSING: ${frameProcessingTime.toFixed(2)}ms at ${(currentProgress * 100).toFixed(2)}% (frame ${f})`);
            }

            playbackProgress = performance.now() - playbackStart;

            // Advance frames.
            if (f === 0) {
              f += 1;
            }
            
            // Convert absolute timestamps back to relative timestamps for playback timing
            const firstFrameTimestamp = recordedFrames[0][0];
            
            // Calculate how long we should wait for the current frame using original recorded timing
            let targetFrameTime = recordedFrames[f][0] - firstFrameTimestamp;
            
            // Additional debug to check if timestamps look reasonable
            if (f === 1) {
              const frameDiff = recordedFrames[1][0] - recordedFrames[0][0];
              if (frameDiff > 10000) {
                console.warn(`🎬 ⚠️ Frame timestamps seem to be absolute timestamps (${frameDiff}ms gap), this will cause playback issues!`);
              }
            }
            
            // Advance frames while playback has progressed past the current frame's time
            if (f >= recordedFrames.length - 1) {
              // For video export, don't loop - complete when all frames are processed
              if (doneCb && render) {
                console.log(`🎬 📹 Video export reaching completion - final frame processed`);
                send({ type: "recorder:present-progress", content: 1 });
                return doneCb(); // Completed video export.
              }
              
              // For normal playback, loop
              f = 0;
              // Reset playback timing for the loop
              playbackStart = performance.now();
              playbackProgress = 0;
              // Clear resume flag when looping
              isResuming = false;
              
              // Restart audio for the loop (only during normal playback)
              if (!render) {
                // Kill any existing tape audio before starting new one
                Object.keys(sfxPlaying).forEach(id => {
                  if (id.startsWith("tape:audio_")) {
                    sfxPlaying[id]?.kill();
                    delete sfxPlaying[id];
                  }
                });
                
                tapeSoundId = "tape:audio_" + performance.now();
                await playSfx(tapeSoundId, "tape:audio");
                console.log("🎵 Audio restarted for loop");
              } else {
                console.log("🎵 Skipping audio restart during video export");
              }
            } else {
              // Improved frame advancement logic with better hang detection and refresh rate handling
              const frameAdvancementStart = performance.now();
              let frameAdvancementCount = 0;
              const maxFrameJump = 10; // Limit frame jumps to prevent instability
              
              // Calculate time since last update to detect refresh rate and suspension
              const timeSinceLastUpdate = window.lastTapeUpdateTime 
                ? (updateStartTime - window.lastTapeUpdateTime) 
                : 16.67; // Assume 60fps if first frame
              window.lastTapeUpdateTime = updateStartTime;
              
              // Detect and clamp extreme time jumps (from tab suspension, varying refresh rates, etc.)
              // Maximum realistic frame time is ~200ms (5fps) - anything beyond suggests suspension
              const maxRealisticFrameTime = 200;
              const clampedTimeSinceUpdate = Math.min(timeSinceLastUpdate, maxRealisticFrameTime);
              
              if (timeSinceLastUpdate > maxRealisticFrameTime) {
                // Large gap detected. Normally this indicates tab suspension and we
                // compensate by advancing playbackStart. However, if the gap was
                // caused by a user-gesture AudioContext resume or manual pause/resume,
                // we should NOT perform that compensation.
                const resumeCompletedAt = window.audioContextResumeTimestamps?.completedAt;
                const tapeResumeAt = window.tapeResumeTimestamp;
                const resumeGraceMs = 1000; // If resume completed within the last 1s, treat gap as resume
                
                const skipCompensation = 
                  (resumeCompletedAt && (updateStartTime - resumeCompletedAt) < resumeGraceMs) ||
                  (tapeResumeAt && (updateStartTime - tapeResumeAt) < resumeGraceMs);
                
                if (skipCompensation) {
                  // Clear the timestamp after using it
                  if (tapeResumeAt && (updateStartTime - tapeResumeAt) < resumeGraceMs) {
                    window.tapeResumeTimestamp = null;
                  }
                } else {
                  console.log(`⏰ Large time gap detected: ${timeSinceLastUpdate.toFixed(0)}ms, clamped to ${maxRealisticFrameTime}ms to prevent fast-forward`);
                  // Adjust playbackStart to compensate for the suspended time
                  const suspendedTime = timeSinceLastUpdate - maxRealisticFrameTime;
                  playbackStart += suspendedTime;
                  console.log(`⏰ Adjusted playback timing by ${suspendedTime.toFixed(0)}ms`);
                }
              }
              
              // Recalculate playback progress after potential adjustment
              playbackProgress = performance.now() - playbackStart;
              
              while (playbackProgress > targetFrameTime && f < recordedFrames.length - 1) {
                f = f + 1;
                frameAdvancementCount++;
                
                // Prevent infinite loops with smaller threshold
                if (frameAdvancementCount > maxFrameJump) {
                  console.warn(`🎬 ⚠️ Frame advancement limit reached: ${frameAdvancementCount} frames, breaking loop for stability`);
                  break;
                }
                
                // Recalculate target time for the new frame
                targetFrameTime = recordedFrames[f][0] - firstFrameTimestamp;
                
                // Check for suspicious timestamp values
                if (isNaN(targetFrameTime) || targetFrameTime < 0) {
                  console.error(`🎬 🚨 INVALID TIMESTAMP at frame ${f}: ${targetFrameTime}, firstFrame: ${firstFrameTimestamp}, frameTime: ${recordedFrames[f][0]}`);
                  break;
                }
                
                // Additional safety: if we're jumping too far ahead, slow down
                if (frameAdvancementCount > 5) {
                  // Break early for large jumps to maintain stability
                  break;
                }
              }
              
              const frameAdvancementTime = performance.now() - frameAdvancementStart;
            }

            if (transmitProgress) {
              // Use frame-based progress for video export, time-based for normal playback
              let currentProgress;
              if (render) {
                // For video export, use stable frame-based progress
                currentProgress = Math.max(0, Math.min(1, f / (recordedFrames.length - 1)));
                // Ensure progress always advances
                if (currentProgress <= (window.lastVideoProgress || 0)) {
                  currentProgress = (window.lastVideoProgress || 0) + 0.001;
                }
                window.lastVideoProgress = currentProgress;
              } else {
                // For normal playback, use time-based progress
                currentProgress = playbackProgress / playbackDurationMs;
              }
              
              // Store global progress for overlay breathing pattern
              window.currentTapeProgress = currentProgress;
              
              // Enhanced debugging for hang detection
              if (render) {
                console.log(`🎬 📊 Frame ${f}/${recordedFrames.length - 1}, Progress: ${(currentProgress * 100).toFixed(2)}%`);
                
                // Check for potential hang conditions
                if (f > 0 && currentProgress > 0.35 && currentProgress < 0.45) {
                  console.log(`🎬 ⚠️ Critical zone detected - frame timing: ${targetFrameTime}ms, playback: ${playbackProgress}ms`);
                  console.log(`🎬 ⚠️ Frame timestamp: ${recordedFrames[f][0]}, First frame: ${firstFrameTimestamp}`);
                  
                  // Memory monitoring in critical zone
                  if (performance.memory) {
                    const memInfo = performance.memory;
                    console.log(`🎬 💾 Memory: Used ${(memInfo.usedJSHeapSize / 1024 / 1024).toFixed(2)}MB / ${(memInfo.totalJSHeapSize / 1024 / 1024).toFixed(2)}MB`);
                  }
                }
                
                // Log every 10% progress for general monitoring
                if (f % Math.floor(recordedFrames.length / 10) === 0) {
                  console.log(`🎬 🔄 Progress checkpoint: ${(currentProgress * 100).toFixed(1)}% (frame ${f})`);
                }
              }
              
              send({
                type: "recorder:present-progress",
                content: currentProgress,
              });
            }

            window.requestAnimationFrame(update);
            
            // Check total update time and warn if excessive
            const totalUpdateTime = performance.now() - updateStartTime;
            if (totalUpdateTime > 50) {
              console.warn(`🎬 ⚠️ Very slow update cycle: ${totalUpdateTime.toFixed(2)}ms at frame ${f} (${((f / (recordedFrames.length - 1)) * 100).toFixed(2)}%)`);
            }
          }

          update();
        };

        startTapePlayback();

        // CRITICAL: Clear freeze frame that would layer above the underlay video (z-index 3 > 0)
        if (freezeFrame || freezeFrameFrozen || wrapper.contains(freezeFrameCan)) {
          freezeFrameCan.remove();
          freezeFrame = false;
          freezeFrameGlaze = false;
          freezeFrameFrozen = false;
        }

        // Style the underlay to be positioned BEHIND everything
        // z-index: -1 places it below the main canvas (1), glaze (2), and UI (6)
        underlayFrame.style.cssText = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: -1; pointer-events: none;";

        underlayFrame.appendChild(frameCan);
        // Insert at the very beginning of body to ensure it's below the wrapper
        document.body.insertBefore(underlayFrame, document.body.firstChild);
        
        // Hide only the opaque compositor layers during presentation
        // Keep the main canvas visible - the piece uses wipe(0,0,0,0) for transparency
        const glazeCan = Glaze.getCan();
        if (glazeCan) glazeCan.style.display = "none";
        if (webglCompositeCanvas) webglCompositeCanvas.style.display = "none";
        if (overlayCan) overlayCan.style.display = "none";
        canvas.style.visibility = "visible";

        // Make backgrounds transparent so the underlay shows through (avoid checkerboard bleed)
        document.body.style.background = "transparent";
        document.body.style.backgroundImage = "none";
        wrapper.style.background = "transparent";
        
        send({ type: "recorder:presented" });
        send({ type: "recorder:present-playing" });
      } else {
        if (debug && logs.recorder)
          console.error(
            "📼 No media recorder or cached video to present from!",
          );
        send({ type: "recorder:presented:failure" });
      }
      return;
    }

    if (type === "recorder:present:play") {
      if (underlayFrame) resumeTapePlayback?.();
      return;
    }

    if (type === "recorder:present:pause") {
      if (underlayFrame && pauseTapePlayback) {
        pauseTapePlayback?.();
      }
      return;
    }

    if (type === "recorder:present:seek") {
      if (underlayFrame && seekTapePlayback) {
        // Support both simple progress value and object with scrubbing options
        if (typeof content === "object") {
          seekTapePlayback?.(content.progress, { 
            scrubbing: content.scrubbing, 
            scrubEnd: content.scrubEnd 
          });
        } else {
          const progress = content; // 0.0 to 1.0
          seekTapePlayback?.(progress);
        }
      }
      return;
    }

    if (type === "recorder:unpresent") {
      if (underlayFrame) {
        const media = underlayFrame.querySelector("video, audio");
        if (media?.src) URL.revokeObjectURL(media.src);
        underlayFrame?.remove();
        underlayFrame = undefined;
        
        // Restore compositor layers visibility
        const glazeCan = Glaze.getCan();
        if (glazeCan) glazeCan.style.display = "";
        if (webglCompositeCanvas) webglCompositeCanvas.style.removeProperty("display");
        if (overlayCan) overlayCan.style.removeProperty("display");
        canvas.style.removeProperty("visibility");

        // Restore backgrounds
        document.body.style.removeProperty("background");
        document.body.style.removeProperty("background-image");
        wrapper.style.removeProperty("background");
        
        send({ type: "recorder:unpresented" });
      }
      return;
    }

    // 🎞️ 🎥 Exporting stamped media.
    if (type === "recorder:print") {
      if (!mediaRecorder) return;
      
      // Send initial status
      send({
        type: "recorder:export-status",
        content: { 
          type: "video", 
          phase: "preparing", 
          message: "Preparing video export" 
        }
      });
      
      send({ type: "recorder:present-playing" });

      let mimeType;
      const content = "video";
      if (MediaRecorder.isTypeSupported(content + "/mp4")) {
        mimeType = content + "/mp4"; // This is the setup for Safari.
      } else if (MediaRecorder.isTypeSupported(content + "/webm")) {
        mimeType = content + "/webm"; // And for Chrome & Firefox.
      } else {
        console.error("🔴 Mimetypes mp4 and webm are unsupported.");
      }

      streamCanCtx = document.createElement("canvas").getContext("2d", {
        alpha: false,
        willReadFrequently: true,
      });
      const sctx = streamCanCtx;

      // TODO: `tiktokVideo` could eventually be defined by an export option
      //        instead of in `recorder:rolling`.
      //if (tiktokVideo) {
      // Portrait Mode / TikTok (this is the default for recording)
      // This is hardcoded at half 720p for TikTok right now.
      // sctx.canvas.width = 1080; //720 / 2;
      // sctx.canvas.height = 1920; //1280 / 2;
      //} else {
      const originalWidth = canvas.width;
      const originalHeight = canvas.height;
      const aspectRatio = originalWidth / originalHeight;

      let newWidth = originalWidth * 4;
      let newHeight = originalHeight * 4;

      if (newWidth > 1080) {
        newWidth = originalWidth * 2;
        newHeight = newWidth / aspectRatio;
      }

      if (newHeight > 1920) {
        newHeight = originalHeight * 2;
        newWidth = newHeight * aspectRatio;
      }
      
      // Add progress bar height to the final export canvas
      const progressBarHeight = 3;
      
      sctx.canvas.width = newWidth;
      sctx.canvas.height = newHeight + progressBarHeight;
      //}

      sctx.imageSmoothingEnabled = false; // Must be set after resize.

      // Draw initial black frame to ensure canvas stream has content
      sctx.fillStyle = 'black';
      sctx.fillRect(0, 0, sctx.canvas.width, sctx.canvas.height);

      const canvasStream = sctx.canvas.captureStream(30); // Specify framerate
      const tapeRenderStreamDest = audioContext.createMediaStreamDestination();

      tapeRenderStreamDest.stream.getAudioTracks().forEach((track) => {
        canvasStream.addTrack(track);
      });

      const options = {
        mimeType,
        videoBitsPerSecond: 5000000,
        audioBitsPerSecond: 256000,
      };

      const videoRecorder = new MediaRecorder(canvasStream, options);

      const chunks = [];

      videoRecorder.ondataavailable = function (e) {
        console.log("📹 Video data available:", e.data.size, "bytes");
        if (e.data && e.data.size > 0) chunks.push(e.data);
      };

      function startRendering() {
        stopTapePlayback?.();
        
        console.log(`🎬 📹 Starting video export with ${recordedFrames.length} recorded frames`);
        
        // Send encoding start status
        send({
          type: "recorder:export-status",
          content: { 
            type: "video", 
            phase: "encoding", 
            message: "Starting video encoding" 
          }
        });
        
        videoRecorder.start(100);
        
        // Enhanced MediaRecorder monitoring
        let lastDataTime = performance.now();
        const monitoringInterval = setInterval(() => {
          const timeSinceLastData = performance.now() - lastDataTime;
          console.log(`🎬 📊 MediaRecorder state: ${videoRecorder.state}, Time since last data: ${timeSinceLastData}ms`);
          
          if (timeSinceLastData > 5000 && videoRecorder.state === "recording") {
            console.warn(`🎬 ⚠️ No data received for ${timeSinceLastData}ms - possible hang detected`);
          }
        }, 2000);
        
        // Add video export watchdog
        window.lastVideoUpdateTime = performance.now();
        window.videoExportWatchdog = setInterval(() => {
          const timeSinceLastUpdate = performance.now() - (window.lastVideoUpdateTime || 0);
          if (timeSinceLastUpdate > 10000) { // 10 second timeout
            console.error(`🎬 🚨 VIDEO EXPORT HANG DETECTED! No update for ${timeSinceLastUpdate}ms`);
            console.error(`🎬 🚨 Attempting to restart or abort export...`);
            
            // Try to stop the current process
            try {
              if (videoRecorder.state === "recording") {
                console.log(`🎬 🚨 Force stopping MediaRecorder...`);
                videoRecorder.stop();
              }
            } catch (e) {
              console.error(`🎬 ❌ Failed to stop MediaRecorder:`, e);
            }
            
            // Clear the watchdog to prevent spam
            clearInterval(window.videoExportWatchdog);
            window.videoExportWatchdog = null;
          }
        }, 5000);
        
        // Update last data time when data is received
        const originalOnDataAvailable = videoRecorder.ondataavailable;
        videoRecorder.ondataavailable = function(e) {
          lastDataTime = performance.now();
          console.log(`🎬 📊 Data received: ${e.data.size} bytes at progress ${window.currentTapeProgress || 0}`);
          if (originalOnDataAvailable) originalOnDataAvailable.call(this, e);
        };
        
        // Clear monitoring when done
        const originalOnStop = videoRecorder.onstop;
        videoRecorder.onstop = function(e) {
          clearInterval(monitoringInterval);
          if (window.videoExportWatchdog) {
            clearInterval(window.videoExportWatchdog);
            window.videoExportWatchdog = null;
            console.log(`🎬 🐕 Watchdog cleared - MediaRecorder stopped`);
          }
          if (originalOnStop) originalOnStop.call(this, e);
        };
        
        // Pixel-perfect upscaling function (same as GIF export)
        function upscalePixels(imageData, originalWidth, originalHeight, scale) {
          if (scale === 1) {
            return imageData;
          }

          const scaledWidth = originalWidth * scale;
          const scaledHeight = originalHeight * scale;
          const scaledImageData = new Uint8ClampedArray(
            scaledWidth * scaledHeight * 4,
          );

          for (let y = 0; y < scaledHeight; y++) {
            for (let x = 0; x < scaledWidth; x++) {
              const sourceX = Math.floor(x / scale);
              const sourceY = Math.floor(y / scale);
              const sourceIndex = (sourceY * originalWidth + sourceX) * 4;
              const targetIndex = (y * scaledWidth + x) * 4;

              scaledImageData[targetIndex] = imageData[sourceIndex]; // R
              scaledImageData[targetIndex + 1] = imageData[sourceIndex + 1]; // G
              scaledImageData[targetIndex + 2] = imageData[sourceIndex + 2]; // B
              scaledImageData[targetIndex + 3] = imageData[sourceIndex + 3]; // A
            }
          }

          return scaledImageData;
        }
        
        startTapePlayback(
          true,
          () => {
            console.log(`🎬 📹 Video export completed, stopping recorder`);
            // Clear watchdog when export completes
            if (window.videoExportWatchdog) {
              clearInterval(window.videoExportWatchdog);
              window.videoExportWatchdog = null;
              console.log(`🎬 🐕 Watchdog cleared - export completed normally`);
            }
            // Stop video recorder immediately when all frames are processed
            videoRecorder.stop();
          },
          tapeRenderStreamDest,
          // 🎫 Renders and watermarks the frames for export.
          function renderTape(can, progress, frameIndex) {
            // Helper function for choosing random values
            function choose(options) {
              return options[Math.floor(Math.random() * options.length)];
            }
            
            // Use the actual frame index from playback instead of separate counter
            const currentFrameIndex = frameIndex !== undefined ? frameIndex : f;
            
            // Log progress every 60 frames to reduce console spam
            if (currentFrameIndex % 60 === 0 || currentFrameIndex <= 5) {
              console.log(`🎬 📹 Rendering frame ${currentFrameIndex + 1}/${recordedFrames.length} (${(progress * 100).toFixed(1)}%)`);
            }
            
            const progressBarHeight = 3;
            
            // Use fixed 3x upscaling like GIF export for pixel-perfect rendering
            const optimalScale = 3;
            const originalWidth = can.width;
            const originalHeight = can.height;
            const scaledWidth = originalWidth * optimalScale;
            const scaledHeight = originalHeight * optimalScale;
            
            // Set screen canvas to match scaled dimensions
            if (sctx.canvas.width !== scaledWidth || sctx.canvas.height !== scaledHeight) {
              sctx.canvas.width = scaledWidth;
              sctx.canvas.height = scaledHeight;
            }

            sctx.clearRect(0, 0, scaledWidth, scaledHeight);

            // Get source canvas as ImageData
            const sourceCtx = can.getContext('2d');
            const sourceImageData = sourceCtx.getImageData(0, 0, originalWidth, originalHeight);
            
            // Apply pixel-perfect upscaling (same as GIF export)
            const scaledData = upscalePixels(sourceImageData.data, originalWidth, originalHeight, optimalScale);
            
            // Create ImageData for the scaled result
            const scaledImageData = new ImageData(
              new Uint8ClampedArray(scaledData),
              scaledWidth,
              scaledHeight
            );
            
            // Put the pixel-perfect upscaled image onto the output canvas
            sctx.putImageData(scaledImageData, 0, 0);

            if (pen?.pointers[1]) {
              const pointer = pen.pointers[1];
              const originalX = pointer.x;
              const originalY = pointer.y;
              // Scale cursor position by optimalScale
              const scaledX = originalX * optimalScale;
              const scaledY = originalY * optimalScale;

              if (pointer.device === "mouse") {
                sctx.drawImage(
                  svgCursor,
                  floor(scaledX - 12),
                  floor(scaledY - 12),
                  svgCursor.naturalWidth,
                  svgCursor.naturalHeight,
                );
              } else {
                // Draw cyan crosshair for touch devices
                const crosshairSize = 16;
                const crosshairThickness = 2;
                
                sctx.globalAlpha = 0.8;
                sctx.strokeStyle = "cyan";
                sctx.lineWidth = crosshairThickness;
                sctx.lineCap = "round";
                
                // Draw crosshair lines
                sctx.beginPath();
                // Horizontal line
                sctx.moveTo(scaledX - crosshairSize, scaledY);
                sctx.lineTo(scaledX + crosshairSize, scaledY);
                // Vertical line
                sctx.moveTo(scaledX, scaledY - crosshairSize);
                sctx.lineTo(scaledX, scaledY + crosshairSize);
                sctx.stroke();
                
                // Draw center dot
                sctx.fillStyle = "cyan";
                sctx.beginPath();
                sctx.arc(scaledX, scaledY, 2, 0, 2 * Math.PI);
                sctx.fill();
                
                sctx.globalAlpha = 1;
              }
            }

            // if (pen.pointers[1]) {
            // console.log(pen.pointers[1]);

            // TODO: Draw a circle based on pen.points[1].x and y
            //       that fits inside of the scaled can drawImage
            //       below, because these ranges are within
            //       the original width and height before the aspect
            //       ratio scale.
            // }

            // 2. Set up the font.
            const typeSize = min(18, max(12, floor(sctx.canvas.height / 40)));
            // const textHeight = typeSize;
            const gap = typeSize * 0.75;

            // Film camera style timestamp at bottom-left corner
            function addFilmTimestamp(
              sctx,
              canvasWidth,
              canvasHeight,
              typeSize,
              progress, // Export progress for the progress bar
            ) {
              // Skip all timestamp and progress bar overlays in clean mode (tape:neat)
              if (window.currentRecordingOptions?.cleanMode) {
                return; // Exit early, don't draw any overlays
              }
              
              // Always show timestamp and progress bar for the full duration of the work (no breathing pattern)
              let showTimecodeAndProgressBar = true; // Always visible throughout the entire duration
              
              // Progress bar height (should match what's set above)
              const progressBarHeight = 20;
              
              // Calculate the content area height (canvas height minus progress bar)
              const contentHeight = sctx.canvas.height - progressBarHeight;
              
              // Calculate simple second count starting from 0
              let elapsedSeconds = 0;
              
              if (mediaRecorderStartTime !== undefined) {
                // During recording: show seconds elapsed since recording started
                const elapsedMs = performance.now() - mediaRecorderStartTime;
                elapsedSeconds = elapsedMs / 1000;
              } else if (window.recordingStartTimestamp && progress >= 0) {
                // Use progress to map to intended recording duration
                let actualRecordingDurationMs = 5000; // Default fallback
                
                // Prioritize intended duration for accurate real-time mapping
                if (window.currentRecordingOptions?.intendedDuration) {
                  actualRecordingDurationMs = window.currentRecordingOptions.intendedDuration * 1000;
                } else if (mediaRecorderDuration && mediaRecorderDuration > 0) {
                  actualRecordingDurationMs = mediaRecorderDuration;
                } else if (window.gifDurationMs && window.gifDurationMs > 0) {
                  actualRecordingDurationMs = window.gifDurationMs;
                }
                
                // Calculate elapsed seconds
                elapsedSeconds = (progress * actualRecordingDurationMs) / 1000;
              } else {
                // Fallback: show 0 seconds (for non-recording contexts)
                elapsedSeconds = 0;
              }

              // Format as simple seconds with one decimal place
              const timestamp = `${elapsedSeconds.toFixed(1)}s`;

              sctx.save();

              // Only draw timecode and progress bar if they should be visible (breathing pattern)
              if (showTimecodeAndProgressBar) {
                // Use original typeSize for timestamp
                sctx.font = `bold ${typeSize}px YWFTProcessing-Regular`;
                
                // Smooth fade transitions: visible at start, fade out towards middle, visible at end
                let timestampAlpha = 1.0; // Default to fully visible
                let timestampColor = "white"; // Consistent white color
                
                // Apply fade pattern based on progress (same as other addFilmTimestamp)
                if (progress < 0.15) {
                  // Fully visible at start (0-15%)
                  timestampAlpha = 1.0;
                } else if (progress >= 0.15 && progress <= 0.35) {
                  // Fade out from 15% to 35% (20% fade-out period)
                  const fadeProgress = (progress - 0.15) / 0.20;
                  timestampAlpha = 1.0 - Math.pow(fadeProgress, 2); // Smooth quadratic fade out
                } else if (progress > 0.35 && progress < 0.65) {
                  // Fully hidden from 35% to 65% (30% hidden period)
                  timestampAlpha = 0.0;
                } else if (progress >= 0.65 && progress <= 0.85) {
                  // Fade in from 65% to 85% (20% fade-in period)
                  const fadeProgress = (progress - 0.65) / 0.20;
                  timestampAlpha = Math.pow(fadeProgress, 2); // Smooth quadratic fade in
                } else {
                  // Fully visible at end (85%-100%)
                  timestampAlpha = 1.0;
                }
                
                // Skip drawing if completely transparent
                if (timestampAlpha <= 0.0) {
                  sctx.restore();
                  return;
                }
                
                sctx.globalAlpha = timestampAlpha;
                sctx.fillStyle = timestampColor;

                const timestampMargin = typeSize * 0.5; // Small margin from edges

                // Smoother, gentler shake effect
                const frameBasedSeed = Math.floor(Date.now() / 300); // Slower change (was 200ms)
                const shakeX = (frameBasedSeed % 3) - 1; // Range -1 to 1 (was -3 to 3)
                const shakeY = ((frameBasedSeed + 2) % 3) - 1; // Range -1 to 1, offset for variation

                // Position timestamp in the content area (not in the progress bar area)
                const timestampY = contentHeight - timestampMargin - typeSize * 0.5;

                sctx.fillText(
                  timestamp,
                  timestampMargin + shakeX,
                  timestampY + shakeY,
                );

                // Draw progress bar in the extended area at the bottom
                const progressBarY = contentHeight; // Start right after content
                const progressBarWidth = Math.floor(progress * canvasWidth);
                
                // Fill the entire progress bar area with a dark background
                sctx.fillStyle = "#000000";
                sctx.fillRect(0, progressBarY, canvasWidth, progressBarHeight);
                
                // Sample colors from the source frame for the progress bar
                let frameColors = []; // Start empty, no fallback yet
                try {
                  // Get frame data from the source canvas ('can' parameter)
                  const tempCtx = can.getContext('2d');
                  const frameImageData = tempCtx.getImageData(0, 0, can.width, can.height);
                  
                  // Sample colors across the width of the progress bar for pixel-by-pixel variety
                  const numSamples = Math.min(progressBarWidth, 50); // Cap at 50 samples for performance
                  
                  if (numSamples > 0 && frameImageData.data.length > 0) {
                    for (let i = 0; i < numSamples; i++) {
                      const xPos = Math.floor((i / Math.max(numSamples - 1, 1)) * (can.width - 1));
                      const yPos = Math.floor(can.height * 0.5); // Sample from middle row
                      const pixelIndex = (yPos * can.width + xPos) * 4;
                      
                      if (pixelIndex >= 0 && pixelIndex < frameImageData.data.length - 3) {
                        const r = frameImageData.data[pixelIndex];
                        const g = frameImageData.data[pixelIndex + 1];
                        const b = frameImageData.data[pixelIndex + 2];
                        
                        // Very subtle enhancement - just barely brighten for visibility
                        const subtleBoost = 1.05; // Very minimal boost (5% instead of 10-40%)
                        const enhancedR = Math.min(255, Math.floor(r * subtleBoost));
                        const enhancedG = Math.min(255, Math.floor(g * subtleBoost));
                        const enhancedB = Math.min(255, Math.floor(b * subtleBoost));
                        
                        const hexColor = `#${enhancedR.toString(16).padStart(2, '0')}${enhancedG.toString(16).padStart(2, '0')}${enhancedB.toString(16).padStart(2, '0')}`;
                        frameColors.push(hexColor);
                      }
                    }
                  }
                } catch (error) {
                  console.warn("Failed to sample frame colors:", error);
                }
                
                // Fallback to subtle natural colors if sampling failed or produced no colors
                if (frameColors.length === 0) {
                  // Use more natural, muted colors as fallback
                  const naturalColors = ["#888888", "#999999", "#AAAAAA", "#BBBBBB", "#CCCCCC"];
                  const numFallbackSegments = Math.min(progressBarWidth, 20);
                  for (let i = 0; i < numFallbackSegments; i++) {
                    frameColors.push(choose(naturalColors));
                  }
                }
                
                // Draw the progress bar with frame-sampled colors
                if (frameColors.length === 1) {
                  // Single color fallback
                  sctx.fillStyle = frameColors[0];
                  sctx.fillRect(0, progressBarY, progressBarWidth, progressBarHeight);
                } else {
                  // Draw pixel-by-pixel or segment-by-segment with sampled colors
                  const segmentWidth = progressBarWidth / frameColors.length;
                  for (let i = 0; i < frameColors.length; i++) {
                    const segmentX = i * segmentWidth;
                    const actualSegmentWidth = Math.ceil(segmentWidth); // Ensure no gaps
                    
                    sctx.fillStyle = frameColors[i];
                    sctx.fillRect(segmentX, progressBarY, actualSegmentWidth, progressBarHeight);
                  }
                }
                
                // Add a subtle border
                sctx.strokeStyle = "#333333";
                sctx.lineWidth = 1;
                sctx.strokeRect(0, progressBarY, canvasWidth, progressBarHeight);
              }

              sctx.restore();
            }

            sctx.save(); // Save the current state of the canvas

            // Skip all stamp rendering in clean mode but still do frame processing
            if (!window.currentRecordingOptions?.cleanMode) {
              // Calculate stamp visibility for timestamp fade coordination
              let bothStampsInvisible = false;
              const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
              
              if (isFrameBasedRecording) {
                // For frame-based recordings longer than 60 frames: use alternating side timing logic
                const frameCount = window.currentRecordingOptions?.frameCount || 0;
                
                if (frameCount > 60 && frameIndex !== null) {
                  const cyclesPerGif = 2;
                  const frameProgress = frameIndex / frameCount;
                  const cyclePosition = (frameProgress * cyclesPerGif) % 1.0;
                  
                  // Check if we're in the "both off" phases (0-20% and 40-60%)
                  if ((cyclePosition < 0.2) || (cyclePosition >= 0.4 && cyclePosition < 0.6)) {
                    bothStampsInvisible = true;
                  }
                }
              } else {
                // Time-based recordings
                const cyclesPerGif = 2;
                const cyclePosition = (progress * cyclesPerGif) % 1.0;
                
                // Check if we're in the "both off" phases (0-20% and 40-60%)
                if ((cyclePosition < 0.2) || (cyclePosition >= 0.4 && cyclePosition < 0.6)) {
                  bothStampsInvisible = true;
                }
              }

              // Add film camera style timestamp at bottom-left corner
              addFilmTimestamp(
                sctx,
                sctx.canvas.width,
                sctx.canvas.height,
                typeSize,
                progress, // Pass progress to the timestamp function
                null, // frameData
                null, // frameMetadata  
                frameIndex, // frameIndex
                null, // totalFrames
                bothStampsInvisible, // bothStampsInvisible
              );

              // Removed Tezos watermark from top right - now positioned with timestamp
              
              // Add Tezos watermark to top-right if enabled (but not when right stamp is showing)
              if (window.currentRecordingOptions?.showTezosStamp) {
                const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
              
              // Use the SAME visibility logic as the actual stamps to avoid conflicts
              let showRightStamp = false;
              
              if (isFrameBasedRecording) {
                // For frame recordings: always show both stamps, so Tezos should never show
                showRightStamp = true;
              } else {
                // Time-based recordings: use progress-based visibility (SAME as stamp logic)
                const cyclesPerGif = 2;
                const cyclePosition = (progress * cyclesPerGif) % 1.0;
                
                if (cyclePosition < 0.2) {
                  showRightStamp = false; // Both off
                } else if (cyclePosition < 0.4) {
                  showRightStamp = true; // Both on
                } else if (cyclePosition < 0.6) {
                  showRightStamp = false; // Both off
                } else if (cyclePosition < 0.8) {
                  showRightStamp = false; // Left only
                } else {
                  showRightStamp = true; // Right only
                }
              }
              
              // Calculate fade alpha for Tezos logo (inverse of right stamp)
              let tezosFadeAlpha = 1.0;
              
              if (isFrameBasedRecording) {
                tezosFadeAlpha = 0.0; // Hidden for frame recordings
              } else {
                // Time-based recordings
                const cyclesPerGif = 2;
                const cyclePosition = (progress * cyclesPerGif) % 1.0;
                
                if (cyclePosition < 0.15) {
                  tezosFadeAlpha = 1.0;
                } else if (cyclePosition >= 0.15 && cyclePosition <= 0.25) {
                  tezosFadeAlpha = 1.0 - ((cyclePosition - 0.15) / 0.10);
                } else if (cyclePosition > 0.25 && cyclePosition < 0.35) {
                  tezosFadeAlpha = 0.0;
                } else if (cyclePosition >= 0.35 && cyclePosition <= 0.45) {
                  tezosFadeAlpha = (cyclePosition - 0.35) / 0.10;
                } else if (cyclePosition > 0.45 && cyclePosition < 0.75) {
                  tezosFadeAlpha = 1.0;
                } else if (cyclePosition >= 0.75 && cyclePosition <= 0.85) {
                  tezosFadeAlpha = 1.0 - ((cyclePosition - 0.75) / 0.10);
                } else {
                  tezosFadeAlpha = 0.0;
                }
              }
              
              // Clamp alpha and show with fade
              tezosFadeAlpha = Math.max(0.0, Math.min(1.0, tezosFadeAlpha));
              
              if (tezosFadeAlpha > 0.0) {
                addTezosStamp(sctx, sctx.canvas.width, sctx.canvas.height, typeSize, isFrameBasedRecording, true, true, null, tezosFadeAlpha);
              }
            }

            drawTextAtPosition(0, 90); // Left side stamp (rotated 90 degrees)
            drawTextAtPosition(sctx.canvas.width, -90); // Right side stamp (rotated -90 degrees)
            
            } else {
              console.log("🎬 🧹 Skipping all stamp rendering in clean mode");
            }

            sctx.restore();
            sctx.globalAlpha = 1;

            function drawTextAtPosition(positionX, deg) {
              sctx.save();
              sctx.translate(positionX, 0);
              sctx.rotate(radians(deg));
            const leftYDist = 0.10; // Closer to corner (was 0.18)
            const rightYDist = 0.08; // Closer to corner, but still avoid Tezos overlap (was 0.15)

              // Use smaller size to match timestamp better
              sctx.font = `${typeSize * 1.4}px YWFTProcessing-Regular`;
              
              // Always use Aesthetic.Computer text
              const text = "Aesthetic.Computer";
              const measured = sctx.measureText(text);
              const textWidth = measured.width;

              // Helper function to render text with multicolored shooting star dot
              function renderTextWithStarDot(sctx, fullText, x, y, baseColor, alpha, offsetX, offsetY, frameIndex) {
                const parts = fullText.split('.');
                if (parts.length !== 2) {
                  // Fallback to normal rendering if not split correctly
                  sctx.fillText(fullText, x + offsetX, y + offsetY);
                  return;
                }
                
                const beforeDot = parts[0]; // "Aesthetic"
                const afterDot = parts[1];  // "Computer"
                
                // Measure parts
                const beforeWidth = sctx.measureText(beforeDot).width;
                const dotWidth = sctx.measureText('.').width;
                
                // Render "Aesthetic"
                sctx.fillStyle = baseColor;
                sctx.globalAlpha = alpha;
                sctx.fillText(beforeDot, x + offsetX, y + offsetY);
                
                // Render CRT-style piercing dot - bright, small, sharp like asteroids
                const dotX = x + beforeWidth + offsetX;
                const dotY = y + offsetY;
                
                // CRT cathode ray piercing colors - bright, sharp, high-contrast
                const crtColors = ["#FFFFFF", "#00FFFF", "#FFFF00", "#FF0000", "#00FF00", "#FF00FF"];
                let dotColor;
                let dotAlpha = alpha;
                
                if (frameIndex !== null) {
                  // Frame-based CRT flicker animation - faster changes for piercing effect
                  const colorIndex = Math.floor(frameIndex / 1) % crtColors.length; // Change color every frame
                  dotColor = crtColors[colorIndex];
                  
                  // Sharp, bright pulse - no gentle curves, pure CRT intensity
                  dotAlpha = alpha * (frameIndex % 2 === 0 ? 1.0 : 0.9); // Sharp flicker between 90% and 100%
                } else {
                  // Time-based fallback - faster CRT-style changes
                  const timeIndex = Math.floor(Date.now() / 100) % crtColors.length; // Change every 100ms
                  dotColor = crtColors[timeIndex];
                  dotAlpha = alpha * (Math.floor(Date.now() / 100) % 2 === 0 ? 1.0 : 0.9); // Sharp time-based flicker
                }
                
                sctx.fillStyle = dotColor;
                sctx.globalAlpha = dotAlpha;
                sctx.fillText('.', dotX, dotY);
                
                // Render "Computer"
                sctx.fillStyle = baseColor;
                sctx.globalAlpha = alpha;
                sctx.fillText(afterDot, dotX + dotWidth, dotY);
              }

              ["red", "lime", "blue", "white"].forEach((color) => {
                let offsetX, offsetY;
                if (color !== "white") {
                  sctx.globalAlpha = 0.5; // Restore multicolor balance (was 0.55)
                  offsetX = choose(-1, -2, 0, 1, 2); // More offsets for multicolor flickering
                  offsetY = choose(-1, -2, 0, 1, 2); // More offsets for multicolor flickering
                } else {
                  sctx.globalAlpha = choose(0.75, 0.85, 0.95); // Brighter white for dot visibility (was 0.55, 0.65, 0.75)
                  offsetX = choose(-1, 0, 1); // Some offsets for multicolor effect
                  offsetY = choose(-1, 0, 1); // Some offsets for multicolor effect
                  color = choose(
                    "white",
                    "white",
                    "white",
                    "magenta",
                    "yellow",
                  );
                }

                sctx.fillStyle = color;

                if (deg === 90) {
                  // LEFT SIDE: Main text positioning
                  renderTextWithStarDot(
                    sctx,
                    text,
                    floor(sctx.canvas.height * (1 - leftYDist) - textWidth),
                    -floor(gap),
                    color,
                    sctx.globalAlpha,
                    offsetX,
                    offsetY,
                    frameIndex
                  );
                } else if (deg === -90) {
                  // RIGHT SIDE: Main text positioning
                  renderTextWithStarDot(
                    sctx,
                    text,
                    -floor(sctx.canvas.height * rightYDist),
                    floor(gap * 3),
                    color,
                    sctx.globalAlpha,
                    offsetX,
                    offsetY,
                    frameIndex
                  );
                }
              });

              if (HANDLE) {
                sctx.font = `${typeSize * 1.3}px YWFTProcessing-Light`; // Smaller handle (was 1.5)
                sctx.fillStyle = choose("yellow", "red", "blue");
                let offsetX, offsetY;
                const handleWidth =
                  textWidth / 2 + sctx.measureText(HANDLE).width / 2;
                const handleSpace = typeSize * 2.0; // More spacing (was 1.5)
                offsetX = choose(-1, 0, 1);
                offsetY = choose(-1, 0, 1);

                if (deg === 90) {
                  // LEFT SIDE: Handle positioning
                  sctx.fillText(
                    HANDLE,
                    floor(
                      sctx.canvas.height * (1 - leftYDist) - handleWidth + offsetY,
                    ),
                    -floor(offsetX + handleSpace + gap),
                  );
                } else if (deg === -90) {
                  // RIGHT SIDE: Handle positioning
                  sctx.fillText(
                    HANDLE,
                    -floor(sctx.canvas.height * rightYDist + handleWidth + offsetY),
                    floor(offsetX + handleSpace + gap * 3),
                  );
                }
              }

              // Remove the old timestamp code from here since it's now in addFilmTimestamp

              sctx.restore();
            }

            // Film camera style timestamp at bottom-left corner
            function addFilmTimestamp(
              sctx,
              canvasWidth,
              canvasHeight,
              typeSize,
              progress, // Export progress for the progress bar
              frameData = null,
              frameMetadata = null,
              frameIndex = null,
              totalFrames = null,
              bothStampsInvisible = false,
            ) {
              // Progress bar height (should match what's set in renderTape)
              const progressBarHeight = 20;
              
              // Calculate the original content area height
              const originalCanvasHeight = sctx.canvas.height - progressBarHeight;
              
              // Calculate simple second count starting from 0
              let elapsedSeconds = 0;
              
              if (mediaRecorderStartTime !== undefined) {
                // During recording: show seconds elapsed since recording started
                const elapsedMs = performance.now() - mediaRecorderStartTime;
                elapsedSeconds = elapsedMs / 1000;
              } else if (window.recordingStartTimestamp && progress >= 0) {
                // Use progress to map to intended recording duration
                let actualRecordingDurationMs = 5000; // Default fallback
                
                // Prioritize intended duration for accurate real-time mapping
                if (window.currentRecordingOptions?.intendedDuration) {
                  actualRecordingDurationMs = window.currentRecordingOptions.intendedDuration * 1000;
                } else if (mediaRecorderDuration && mediaRecorderDuration > 0) {
                  actualRecordingDurationMs = mediaRecorderDuration;
                } else if (window.gifDurationMs && window.gifDurationMs > 0) {
                  actualRecordingDurationMs = window.gifDurationMs;
                }
                
                // Calculate elapsed seconds
                elapsedSeconds = (progress * actualRecordingDurationMs) / 1000;
              } else {
                // Fallback: show 0 seconds (for non-recording contexts)
                elapsedSeconds = 0;
              }

              // Format as simple seconds with one decimal place
              const timestamp = `${elapsedSeconds.toFixed(1)}s`;

              sctx.save();
              // Make timestamp smaller to match first system
              const timestampSize = typeSize * 1.7; // Smaller timestamp (was 2.0)
              sctx.font = `bold ${timestampSize}px YWFTProcessing-Regular`;
              
              // Adjust margin to make room for Tezos logo if enabled
              let adjustedMargin = typeSize * 0.5;
              if (window.currentRecordingOptions?.showTezosStamp) {
                adjustedMargin = typeSize * 1.8; // Less space since we moved Tezos closer
              }
              
              // Calculate timestamp alpha based on progress only, independent of side stamps
              let timestampAlpha = 0.8; // Default to more opaque (was 0.45)
              
              // Add the same smooth fade logic as first system
              const isFrameBasedRecording = window.currentRecordingOptions?.frameMode;
              
              if (!isFrameBasedRecording) {
                // Smooth fade transitions with longer periods
                if (progress < 0.15) {
                  // Fully visible at start
                  timestampAlpha = 0.8;
                } else if (progress >= 0.15 && progress <= 0.35) {
                  // Longer fade out from 15% to 35% (20% fade-out period)
                  const fadeProgress = (progress - 0.15) / 0.20;
                  timestampAlpha = 0.8 * (1.0 - Math.pow(fadeProgress, 2)); // Smooth quadratic fade out
                } else if (progress > 0.35 && progress < 0.65) {
                  // Fully hidden from 35% to 65% (30% hidden period)
                  timestampAlpha = 0.0;
                } else if (progress >= 0.65 && progress <= 0.85) {
                  // Longer fade in from 65% to 85% (20% fade-in period)
                  const fadeProgress = (progress - 0.65) / 0.20;
                  timestampAlpha = 0.8 * Math.pow(fadeProgress, 2); // Smooth quadratic fade in
                } else {
                  // Fully visible at end (85%-100%)
                  timestampAlpha = 0.8;
                }
              }
              
              sctx.globalAlpha = timestampAlpha;
              sctx.fillStyle = "white"; // Clean white timestamp, no blinking

              // Remove shake effect - keep timestamp steady
              const shakeX = 0; // No more shaking
              const shakeY = 0; // No more shaking

              // Position timestamp in the main content area - move up 4px more  
              const timestampY = originalCanvasHeight - adjustedMargin * 0.6 - typeSize * 0.2 - 4; // Move UP 4px more

              sctx.fillText(
                timestamp,
                typeSize * 0.5 + shakeX, // Move back right a bit (was 0.3)
                timestampY + shakeY,
              );
              
              // Removed Tezos logo from timestamp area - moved back to top-right

              // Draw progress bar in the extended area at the bottom of the scaled content
              const progressBarY = originalCanvasHeight; // Start right after original content
              const progressBarWidth = Math.floor(progress * canvasWidth);
              
              // Fill the entire progress bar area with a dark background
              sctx.fillStyle = "#000000";
              sctx.fillRect(0, progressBarY, canvasWidth, progressBarHeight);
              
              // Sample colors from the source frame for the progress bar
              let frameColors = ["#FF0000"]; // Red fallback
              try {
                // Add timeout protection for this operation
                const colorSamplingStart = performance.now();
                
                // Get frame data from the source canvas ('can' parameter)
                const tempCtx = can.getContext('2d');
                const frameImageData = tempCtx.getImageData(0, 0, can.width, can.height);
                
                const colorSamplingTime = performance.now() - colorSamplingStart;
                if (colorSamplingTime > 50) {
                  console.warn(`🎬 ⚠️ Slow color sampling: ${colorSamplingTime.toFixed(2)}ms at progress ${progress.toFixed(4)}`);
                }
                
                // Sample colors across the width of the progress bar for pixel-by-pixel variety
                const numSamples = Math.min(progressBarWidth, 50); // Reduced from 100 to 50 for performance
                frameColors = [];
                
                for (let i = 0; i < numSamples; i++) {
                  const xPos = Math.floor((i / numSamples) * can.width);
                  const yPos = Math.floor(can.height * 0.5); // Sample from middle row
                  const pixelIndex = (yPos * can.width + xPos) * 4;
                  
                  if (pixelIndex < frameImageData.data.length - 4) {
                    const r = frameImageData.data[pixelIndex];
                    const g = frameImageData.data[pixelIndex + 1];
                    const b = frameImageData.data[pixelIndex + 2];
                    
                    // Enhance colors for visibility
                    const enhancedR = Math.min(255, Math.floor(r * 1.3));
                    const enhancedG = Math.min(255, Math.floor(g * 1.3));
                    const enhancedB = Math.min(255, Math.floor(b * 1.3));
                    
                    const hexColor = `#${enhancedR.toString(16).padStart(2, '0')}${enhancedG.toString(16).padStart(2, '0')}${enhancedB.toString(16).padStart(2, '0')}`;
                    frameColors.push(hexColor);
                  } else {
                    frameColors.push("#FF0000"); // Red fallback
                  }
                }
              } catch (error) {
                console.error("🎬 ❌ Frame color sampling failed at progress", progress.toFixed(4), error);
                frameColors = ["#FF0000"]; // Red fallback
              }
              
              // Draw the progress bar with frame-sampled colors
              try {
                const progressBarDrawStart = performance.now();
                
                if (frameColors.length === 1) {
                  // Single color fallback
                  sctx.fillStyle = frameColors[0];
                  sctx.fillRect(0, progressBarY, progressBarWidth, progressBarHeight);
                } else {
                  // Draw pixel-by-pixel or segment-by-segment with sampled colors
                  const segmentWidth = progressBarWidth / frameColors.length;
                  for (let i = 0; i < frameColors.length; i++) {
                    const segmentX = i * segmentWidth;
                    const actualSegmentWidth = Math.ceil(segmentWidth); // Ensure no gaps
                    
                    sctx.fillStyle = frameColors[i];
                    sctx.fillRect(segmentX, progressBarY, actualSegmentWidth, progressBarHeight);
                  }
                }
                
                // Add a subtle border
                sctx.strokeStyle = "#333333";
                sctx.lineWidth = 1;
                sctx.strokeRect(0, progressBarY, canvasWidth, progressBarHeight);
                
                const progressBarDrawTime = performance.now() - progressBarDrawStart;
                if (progressBarDrawTime > 10) {
                  console.warn(`🎬 ⚠️ Slow progress bar draw: ${progressBarDrawTime.toFixed(2)}ms at progress ${progress.toFixed(4)}`);
                }
              } catch (error) {
                console.error(`🎬 ❌ Progress bar drawing failed at progress ${progress.toFixed(4)}:`, error);
              }

              sctx.restore();
            }

            // Send detailed progress with phase information
            let phase = "encoding";
            let message = "Encoding video";
            
            if (progress < 0.3) {
              phase = "preparing";
              message = `Preparing frame ${currentFrameIndex + 1}`;
            } else if (progress < 0.7) {
              phase = "encoding";
              message = `Encoding frame ${currentFrameIndex + 1}`;
            } else if (progress < 0.95) {
              phase = "transcoding";
              message = "Transcoding MP4";
            } else {
              phase = "finalizing";
              message = "Finalizing video";
            }

            send({
              type: "recorder:transcode-progress",
              content: progress,
            });
            
            // Send detailed status every few frames to avoid spam
            if (currentFrameIndex % 5 === 0 || progress >= 0.95) {
              send({
                type: "recorder:export-status",
                content: { 
                  type: "video", 
                  phase: phase, 
                  message: message 
                }
              });
            }
          },
        );
      }

      const svgCursor = new Image();
      svgCursor.onload = (e) => startRendering();
      svgCursor.src = "/aesthetic.computer/cursors/precise.svg";

      // Video rendered, now download...
      videoRecorder.onstop = async function (e) {
        console.log("📹 Video recording stopped. Chunks collected:", chunks.length);
        console.log("📹 Total video data size:", chunks.reduce((total, chunk) => total + chunk.size, 0), "bytes");
        
        const blob = new Blob(chunks, { type: videoRecorder.mimeType });
        console.log("📹 Final video blob size:", blob.size, "bytes, type:", blob.type);
        
        const filename = generateTapeFilename("mp4");
        console.log("🎬 MP4 Export - currentRecordingOptions:", JSON.stringify(window.currentRecordingOptions, null, 2));

        // Prepare raw audio data for storage if available
        let rawAudioForStorage = null;
        if (rawAudioData.length > 0) {
          const totalSamples = rawAudioData.length * 4096;
          const leftChannel = new Float32Array(totalSamples);
          const rightChannel = new Float32Array(totalSamples);
          
          let sampleIndex = 0;
          for (let i = 0; i < rawAudioData.length; i++) {
            const chunk = rawAudioData[i];
            for (let j = 0; j < chunk.left.length; j++) {
              if (sampleIndex < totalSamples) {
                leftChannel[sampleIndex] = chunk.left[j];
                rightChannel[sampleIndex] = chunk.right[j];
                sampleIndex++;
              }
            }
          }
          
          rawAudioForStorage = {
            left: leftChannel,
            right: rightChannel,
            totalSamples: sampleIndex,
            sampleRate: rawAudioSampleRate
          };
          console.log(`🎵 Prepared raw audio for storage: ${sampleIndex} samples at ${rawAudioSampleRate}Hz`);
        }

        // Store video with frame data for complete persistence
        const storeData = {
          blob,
          duration: mediaRecorderDuration,
          frames: recordedFrames, // Include frame data for WebP/Frame exports
          rawAudio: rawAudioForStorage, // Include raw audio for playback
          timestamp: Date.now(),
          filename, // Store the generated filename
        };
        
        // Debug: Check frame data before storage
        if (recordedFrames.length > 0) {
          const firstFrame = recordedFrames[0];
          console.log("💾 Storing tape with frames:", {
            frameCount: recordedFrames.length,
            firstFrameTimestamp: firstFrame[0],
            firstFrameTimestampType: typeof firstFrame[0],
            firstFrameStructure: Array.isArray(firstFrame) ? `Array(${firstFrame.length})` : typeof firstFrame,
            hasRawAudio: !!rawAudioForStorage
          });
        }
        
        await receivedChange({
          data: {
            type: "store:persist",
            content: {
              key: "tape",
              method: "local:db",
              data: storeData,
            },
          },
        });

        // 📥 Download the video.
        receivedDownload({ filename, data: blob });
        
        // Send completion message
        send({
          type: "recorder:export-complete",
          content: { type: "video", filename }
        });
        
        send({ type: "recorder:printed", content: { id: filename } });
        stopTapePlayback?.();
        send({ type: "recorder:present-paused" });
        // startTapePlayback(true);
        // pauseTapePlayback?.();
      };

      send({ type: "recorder:printing:started" });
      return;
    }

    if (type === "recorder:slate") {
      // Always clear cached tape data when slating, regardless of media type
      await Store.del("tape");
      mediaRecorder?.stop();
      return;
    }

    // Request recorded frames for export
    if (type === "recorder:request-frames") {
      if (recordedFrames.length > 0) {
        // Try to get raw audio from cached tape
        let rawAudio = null;
        try {
          const cachedTape = await Store.get("tape");
          if (cachedTape && cachedTape.rawAudio) {
            rawAudio = cachedTape.rawAudio;
            console.log(`🎵 Including raw audio in frame response: ${rawAudio.totalSamples} samples`);
          }
        } catch (error) {
          console.warn("Could not retrieve raw audio from cache:", error);
        }
        
        // Send frames in chunks to avoid memory issues with large recordings
        const CHUNK_SIZE = 1000; // Send 1000 frames at a time
        const totalFrames = recordedFrames.length;
        const totalChunks = Math.ceil(totalFrames / CHUNK_SIZE);
        
        console.log(`📦 Sending ${totalFrames} frames in ${totalChunks} chunks of ${CHUNK_SIZE} frames each`);
        
        // Send first chunk with metadata
        send({
          type: "recorder:frames-response",
          content: { 
            frames: recordedFrames.slice(0, CHUNK_SIZE),
            rawAudio: rawAudio, // Include audio data with first chunk
            chunkIndex: 0,
            totalChunks: totalChunks,
            totalFrames: totalFrames
          },
        });
        
        // Send remaining chunks
        for (let i = 1; i < totalChunks; i++) {
          const start = i * CHUNK_SIZE;
          const end = Math.min(start + CHUNK_SIZE, totalFrames);
          const chunk = recordedFrames.slice(start, end);
          
          console.log(`📦 Sending chunk ${i + 1}/${totalChunks} (frames ${start}-${end-1})`);
          
          send({
            type: "recorder:frames-chunk",
            content: { 
              frames: chunk,
              chunkIndex: i,
              totalChunks: totalChunks
            },
          });
          
          // Small delay between chunks to prevent overwhelming the worker
          await new Promise(resolve => setTimeout(resolve, 10));
        }
        
        console.log(`✅ Finished sending all ${totalChunks} chunks`);
      } else {
        send({
          type: "recorder:frames-response",
          content: { frames: [] },
        });
      }
      return;
    }

    // Load a bitmap off the network.
    if (type === "load-bitmap") {
      // 🖼️ Check for embedded painting bitmaps first (pack mode / bundled HTML)
      console.log("🖼️ BIOS load-bitmap received:", content);
      console.log("🖼️ acEMBEDDED_PAINTING_BITMAPS:", window.acEMBEDDED_PAINTING_BITMAPS ? Object.keys(window.acEMBEDDED_PAINTING_BITMAPS) : "NOT SET");
      console.log("🖼️ acPAINTING_CODE_MAP:", window.acPAINTING_CODE_MAP || "NOT SET");
      if (window.acEMBEDDED_PAINTING_BITMAPS && window.acPAINTING_CODE_MAP) {
        // Try to find this URL in the embedded paintings by matching the slug
        for (const [code, info] of Object.entries(window.acPAINTING_CODE_MAP)) {
          console.log("🖼️ Checking code:", code, "slug:", info.slug, "includes:", content.includes(info.slug));
          if (content.includes(info.slug)) {
            const embeddedBitmap = window.acEMBEDDED_PAINTING_BITMAPS[code] ||
                                  window.acEMBEDDED_PAINTING_BITMAPS['#' + code];
            console.log("🖼️ Found slug match! embeddedBitmap:", embeddedBitmap ? `${embeddedBitmap.width}x${embeddedBitmap.height}` : "NOT FOUND");
            if (embeddedBitmap) {
              console.log("🖼️ Serving embedded painting for:", content, "→ #" + code);
              // Clone the pixels buffer since it will be transferred
              const pixelsCopy = new Uint8ClampedArray(embeddedBitmap.pixels);
              send(
                {
                  type: "loaded-bitmap-success",
                  content: { url: content, img: {
                    width: embeddedBitmap.width,
                    height: embeddedBitmap.height,
                    pixels: pixelsCopy
                  }},
                },
                [pixelsCopy.buffer],
              );
              return;
            }
          }
        }
      }
      
      const controller = new AbortController();
      mediaPathsLoading[content] = controller;

      const img = document.createElement("img");
      img.src = content;
      img.crossOrigin = "anonymous";

      const onLoad = async () => {
        const bitmap = await toBitmap(img);
        send(
          {
            type: "loaded-bitmap-success",
            content: { url: content, img: bitmap },
          },
          [bitmap.pixels.buffer],
        );
        img.removeEventListener("error", onError);
        // Clean up the loading tracker since the load completed successfully
        delete mediaPathsLoading[content];
      };

      const onError = (err) => {
        // console.error(err);
        send({ type: "loaded-bitmap-rejection", content: { url: content } });
        img.removeEventListener("load", onLoad); // Remove the other listener too
        // Clean up the loading tracker since the load failed
        delete mediaPathsLoading[content];
      };

      img.addEventListener("load", onLoad);
      img.addEventListener("error", onError);

      controller.signal.addEventListener("abort", () => {
        if (debug) console.log("🖼️ Aborted image load:", content);
        img.removeEventListener("load", onLoad);
        img.removeEventListener("error", onError);
        // Update src to stop current loading.
        img.src =
          "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7";
        // Clean up the loading tracker since the load was aborted
        delete mediaPathsLoading[content];
      });

      return;
    }

    // 🎬 Load an animated WebP and decode all frames
    if (type === "load-animated-webp") {
      console.log("🎬 BIOS load-animated-webp received:", content);
      
      // Helper to convert webpxmux Uint32Array to RGBA Uint8ClampedArray
      // Each Uint32 element contains one pixel in RGBA format: 0xRRGGBBAA
      // We need RGBA for canvas: R, G, B, A bytes
      const convertToRGBA = (u32arr) => {
        const rgba = new Uint8ClampedArray(u32arr.length * 4);
        for (let i = 0; i < u32arr.length; i++) {
          const pixel = u32arr[i];
          // RGBA format: 0xRRGGBBAA
          const r = (pixel >>> 24) & 0xff;
          const g = (pixel >>> 16) & 0xff;
          const b = (pixel >>> 8) & 0xff;
          const a = pixel & 0xff;
          const j = i * 4;
          rgba[j] = r;
          rgba[j + 1] = g;
          rgba[j + 2] = b;
          rgba[j + 3] = a;
        }
        return rgba;
      };
      
      (async () => {
        try {
          // Start fetch immediately (don't wait for library)
          const fetchPromise = fetch(content).then(async (response) => {
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            return new Uint8Array(await response.arrayBuffer());
          });
          
          // Load webpxmux library in parallel with fetch (use CDN for reliability)
          if (!window.WebPXMux) {
            console.log("🎬 Loading webpxmux library...");
            await new Promise((resolve, reject) => {
              const script = document.createElement("script");
              script.src = "https://cdn.jsdelivr.net/npm/webpxmux@0.0.2/dist/webpxmux.min.js";
              script.onload = resolve;
              script.onerror = (e) => reject(new Error("Failed to load webpxmux library"));
              document.head.appendChild(script);
            });
          }
          
          // Cache the initialized xMux instance for reuse (use CDN wasm)
          if (!window._webpxMuxInstance) {
            const xMux = window.WebPXMux("https://cdn.jsdelivr.net/npm/webpxmux@0.0.2/dist/webpxmux.wasm");
            await xMux.waitRuntime();
            window._webpxMuxInstance = xMux;
          }
          const xMux = window._webpxMuxInstance;
          
          // Wait for fetch to complete
          const webpData = await fetchPromise;
          console.log("🎬 Fetched WebP data:", webpData.length, "bytes");
          
          const frames = await xMux.decodeFrames(webpData);
          console.log("🎬 Decoded animated WebP:", frames.frameCount, "frames,", frames.width, "x", frames.height);
          
          if (frames.frameCount > 1) {
            // Convert frames to transferable format
            const frameData = frames.frames.map(f => ({
              duration: f.duration,
              isKeyframe: f.isKeyframe,
              // Convert Uint32Array BGRA to Uint8ClampedArray RGBA
              pixels: convertToRGBA(f.rgba)
            }));
            
            // Transfer all pixel buffers
            const transfers = frameData.map(f => f.pixels.buffer);
            
            send({
              type: "loaded-animated-webp-success",
              content: {
                url: content,
                width: frames.width,
                height: frames.height,
                frameCount: frames.frameCount,
                loopCount: frames.loopCount,
                frames: frameData
              }
            }, transfers);
          } else {
            // Single frame - just decode as bitmap
            const bitmap = await xMux.decodeWebP(webpData);
            // Convert Uint32Array BGRA to Uint8ClampedArray RGBA
            const pixels = convertToRGBA(bitmap.rgba);
            
            send({
              type: "loaded-animated-webp-success",
              content: {
                url: content,
                width: bitmap.width,
                height: bitmap.height,
                frameCount: 1,
                loopCount: 0,
                frames: [{
                  duration: 0,
                  isKeyframe: true,
                  pixels: pixels
                }]
              }
            }, [pixels.buffer]);
          }
        } catch (err) {
          console.error("🎬 Animated WebP decode failed:", err);
          send({
            type: "loaded-animated-webp-rejection",
            content: { url: content, error: err.message }
          });
        }
      })();
      
      return;
    }

    // Abort a loading piece of media if it exists.
    // TODO: Only implemented on bitmaps for now. 23.10.02.15.13
    if (type === "load:abort") {
      mediaPathsLoading[content]?.abort();
      return;
    }

    // Load a sound from a url with instant playback support.
    if (type === "sfx:load") {
      if (debug && logs.audio)
        console.log("🔈 BIOS received sfx:load request for:", content);

      let internal = false;

      for (let wl of soundWhitelist) {
        if (content === wl) {
          internal = true;
          break;
        }
      }

      let url;
      if (internal) {
        const ext = Safari ? "m4a" : "ogg";
        url = `/sounds/AeCo_${content}.${ext}`;
        if (window.production === true) {
          url = `https://assets.aesthetic.computer` + url;
        } else {
          url = `/assets` + url;
        }
      } else url = content;

      if (debug && logs.audio)
        console.log("🔈 BIOS loading sound from URL:", url);

      // Enhanced instant playback audio loading strategy
      const audioId = content;

      // Strategy 1: Immediate HTML5 Audio for instant playback
      const htmlAudioElement = new Audio();
      htmlAudioElement.crossOrigin = "anonymous";
      htmlAudioElement.preload = "auto";
      htmlAudioElement.src = url;

      // Store the HTML5 audio for instant playback
      sfx[audioId + "_html5"] = htmlAudioElement;

      if (debug && logs.audio)
        console.log(
          "🔈 BIOS stored HTML5 audio element for:",
          audioId + "_html5",
        );

      // Strategy 2: Background fetch and decode for high-quality playback
      fetch(url)
        .then((response) => {
          return response.arrayBuffer();
        })
        .then(async (arrayBuffer) => {
          if (debug && logs.audio)
            console.log("🔈 BIOS fetched audio data for:", audioId);
          try {
            if (!audioContext) {
              sfx[audioId] = arrayBuffer;
              if (debug && logs.audio)
                console.log(
                  "🔈 BIOS stored raw audio buffer (no audioContext):",
                  audioId,
                );
              // Process any queued sounds that might be waiting for this file
              processPendingSfx();
            } else {
              // Background decode the buffer
              const audioBuffer =
                await audioContext.decodeAudioData(arrayBuffer);
              sfx[audioId] = audioBuffer;

              if (debug && logs.audio)
                console.log(
                  "🔈 BIOS decoded and stored audio buffer:",
                  audioId,
                );

              // Clean up HTML5 audio once high-quality buffer is ready
              if (sfx[audioId + "_html5"]) {
                delete sfx[audioId + "_html5"];
              }

              if (debug && logs.audio)
                console.log(
                  "🔈 Background decoded for high-quality playback:",
                  audioId,
                );

              // Process any queued sounds that might be waiting for this file
              processPendingSfx();
            }

            send({
              type: "loaded-sfx-success",
              content: { url, sfx: audioId },
            });
            if (debug && logs.audio)
              console.log(
                "🔈 BIOS sent loaded-sfx-success (background):",
                audioId,
              );
          } catch (error) {
            if (debug && logs.audio)
              console.error("Background audio decoding failed:", error);
            // Keep the HTML5 audio as fallback
            send({
              type: "loaded-sfx-success",
              content: { url, sfx: audioId },
            });
          }
        })
        .catch((error) => {
          // Keep the HTML5 audio as fallback
          send({
            type: "loaded-sfx-success",
            content: { url, sfx: audioId },
          });
        });

      // Immediately signal success with instant playback capability
      send({
        type: "loaded-sfx-success",
        content: { url, sfx: audioId, instantPlayback: true },
      });
      if (debug && logs.audio)
        console.log("🔈 BIOS sent loaded-sfx-success (instant):", audioId);

      return;
    }

    // Trigger a sound to playback.
    if (type === "sfx:play") {
      playSfx(content.id, content.sfx, content.options);
      return;
    }

    if (type === "sfx:register") {
      const { id, data, sampleRate } = content || {};
      if (!id || !data || !Array.isArray(data) || data.length === 0) return;

      const safeRate =
        typeof sampleRate === "number" && Number.isFinite(sampleRate)
          ? sampleRate
          : audioContext?.sampleRate || 48000;
      const samples = Float32Array.from(data);

      if (audioContext) {
        const buffer = audioContext.createBuffer(1, samples.length, safeRate);
        buffer.getChannelData(0).set(samples);
        sfx[id] = buffer;
      } else {
        sfx[id] = {
          left: Array.from(samples),
          right: Array.from(samples),
          totalSamples: samples.length,
          sampleRate: safeRate,
        };
      }
      if (sfxLoaded && typeof sfxLoaded === "object") {
        sfxLoaded[id] = false;
      }
      return;
    }

    // 🔄 Live buffer update - updates sample data for currently playing sounds
    if (type === "sfx:update-sample") {
      console.log(`🔴 BIOS: Received sfx:update-sample for ${content?.id}`);
      const { id, data, sampleRate } = content || {};
      if (!id || !data || !Array.isArray(data) || data.length === 0) return;

      const safeRate =
        typeof sampleRate === "number" && Number.isFinite(sampleRate)
          ? sampleRate
          : audioContext?.sampleRate || 48000;
      const samples = Float32Array.from(data);

      // Update the local sfx cache
      if (audioContext) {
        const buffer = audioContext.createBuffer(1, samples.length, safeRate);
        buffer.getChannelData(0).set(samples);
        sfx[id] = buffer;
      } else {
        sfx[id] = {
          left: Array.from(samples),
          right: Array.from(samples),
          totalSamples: samples.length,
          sampleRate: safeRate,
        };
      }

      // Send update to speaker worklet for running sounds
      if (speakerProcessorNode) {
        speakerProcessorNode.port.postMessage({
          type: "sample:update",
          data: {
            label: id,
            buffer: {
              channels: [samples],
              length: samples.length,
              sampleRate: safeRate,
            },
          },
        });
      }
      return;
    }

    if (type === "sfx:update") {
      sfxPlaying[content.id]?.update(content.properties);
      return;
    }

    if (type === "sfx:get-sample-data") {
      async function checkForSampleData() {
        if (audioContext) {
          const audioBuffer = await decodeSfx(content.id);

          if (!audioBuffer) {
            setTimeout(checkForSampleData, 100);
          } else {
            const dataFloat32 = audioBuffer.getChannelData(0);
            const data = Array.from(dataFloat32);

            send({
              type: "sfx:got-sample-data",
              content: { id: content.id, data },
            });
          }
        } else {
          setTimeout(checkForSampleData, 100);
          // await checkForSampleData();
        }
      }

      checkForSampleData();
      return;
    }

    if (type === "sfx:get-duration") {
      async function checkForDuration() {
        if (audioContext) {
          const audioBuffer = await decodeSfx(content.id);

          if (!audioBuffer) {
            setTimeout(checkForDuration, 100);
          } else {
            // Just send duration, not the expensive sample data
            send({
              type: "sfx:got-duration",
              content: { id: content.id, duration: audioBuffer.duration },
            });
          }
        } else {
          setTimeout(checkForDuration, 100);
        }
      }

      checkForDuration();
      return;
    }

    // Stop a playing sound or sample if it exists,
    // with an optional 'after' parameter for a fade out.
    if (type === "sfx:kill") {
      sfxPlaying[content.id]?.kill(content.fade);
      return;
    }

    //if (type === "sfx:fade") {
    // sfxPlaying[content.id]?.kill();
    //  return;
    //}

    // Report progress of a playing sound back to the disk.
    if (type === "sfx:progress") {
      sfxPlaying[content.id]?.progress();
      return;
    }

    // 🎵 Streaming Audio (Radio, etc.)
    // Start playing a streaming audio URL
    if (type === "stream:play") {
      const { id, url, volume } = content;
      
      // Stop any existing stream with this id
      if (streamAudio[id]) {
        streamAudio[id].audio.pause();
        streamAudio[id].audio.src = "";
        delete streamAudio[id];
      }
      
      const audio = new Audio();
      audio.crossOrigin = "anonymous";
      audio.src = url;
      const baseVolume = clampVolume(volume ?? 1);
      audio.volume = baseVolume * masterVolume;
      
      // Connect to AudioContext for analysis if available
      if (audioContext) {
        try {
          const source = audioContext.createMediaElementSource(audio);
          const analyser = audioContext.createAnalyser();
          analyser.fftSize = 256;
          source.connect(analyser);
          analyser.connect(audioContext.destination);
          streamAudio[id] = { audio, analyser, source, baseVolume };
        } catch (e) {
          // If already connected, just store the audio
          streamAudio[id] = { audio, baseVolume };
        }
      } else {
        streamAudio[id] = { audio, baseVolume };
      }
      
      audio.play().then(() => {
        send({ type: "stream:playing", content: { id } });
      }).catch(err => {
        console.warn("🎵 Stream play failed:", err);
        send({ type: "stream:error", content: { id, error: err.message } });
      });
      
      return;
    }
    
    // Pause a streaming audio
    if (type === "stream:pause") {
      const { id } = content;
      if (streamAudio[id]?.audio) {
        streamAudio[id].audio.pause();
        send({ type: "stream:paused", content: { id } });
      }
      return;
    }
    
    // Resume a paused streaming audio
    if (type === "stream:resume") {
      const { id } = content;
      if (streamAudio[id]?.audio) {
        streamAudio[id].audio.play().then(() => {
          send({ type: "stream:playing", content: { id } });
        }).catch(err => {
          send({ type: "stream:error", content: { id, error: err.message } });
        });
      }
      return;
    }
    
    // Stop and remove streaming audio
    if (type === "stream:stop") {
      const { id } = content;
      if (streamAudio[id]) {
        streamAudio[id].audio.pause();
        streamAudio[id].audio.src = "";
        delete streamAudio[id];
        send({ type: "stream:stopped", content: { id } });
      }
      return;
    }
    
    // Set volume of streaming audio
    if (type === "stream:volume") {
      const { id, volume } = content;
      if (streamAudio[id]?.audio) {
        const baseVolume = clampVolume(volume);
        streamAudio[id].baseVolume = baseVolume;
        streamAudio[id].audio.volume = baseVolume * masterVolume;
      }
      return;
    }
    
    // Get frequency data for visualization
    if (type === "stream:frequencies") {
      const { id } = content;
      if (streamAudio[id]?.analyser) {
        const analyser = streamAudio[id].analyser;
        const dataArray = new Uint8Array(analyser.frequencyBinCount);
        analyser.getByteFrequencyData(dataArray);
        send({ type: "stream:frequencies-data", content: { id, data: Array.from(dataArray) } });
      } else {
        send({ type: "stream:frequencies-data", content: { id, data: [] } });
      }
      return;
    }
    
    // Get waveform (time-domain) data for visualization (fallback for iOS Safari)
    if (type === "stream:waveform") {
      const { id } = content;
      if (streamAudio[id]?.analyser) {
        const analyser = streamAudio[id].analyser;
        const dataArray = new Uint8Array(analyser.frequencyBinCount);
        analyser.getByteTimeDomainData(dataArray);
        send({ type: "stream:waveform-data", content: { id, data: Array.from(dataArray) } });
      } else {
        send({ type: "stream:waveform-data", content: { id, data: [] } });
      }
      return;
    }

    if (type === "fullscreen-enable") {
      curReframeDelay = 0;
      enableFullscreen();
      return;
    }

    if (type === "fps-change") {
      Loop.frameRate(content);
      return;
    }

    if (type === "glaze") {
      if (debug && logs.glaze) {
        console.log("🪟 Glaze:", content, "Type:", content.type || "prompt");
      }
      const wasOn = glaze.on;
      glaze = content;
      if (glaze.on === false) {
        Glaze.clear(); // Clear WebGL buffer before hiding to prevent stale content
        Glaze.off();
        canvas.style.removeProperty("opacity");
      } else if (glaze.on === true && !wasOn) {
        // 🧊 Clear any stuck freeze frame before enabling glaze
        // This fixes a race condition where navigating back to a glaze piece
        // (like prompt in dark mode) could leave the previous piece's freeze frame
        // visible because glazeReady was false while shaders were loading.
        if (freezeFrame || freezeFrameFrozen) {
          if (wrapper.contains(freezeFrameCan)) {
            freezeFrameCan.remove();
          }
          canvas.style.removeProperty("opacity");
          freezeFrame = false;
          freezeFrameGlaze = false;
          freezeFrameFrozen = false;
        }
        // Glaze was just enabled - set flag to trigger reframe on next paint cycle
        needsReframe = true;
        canvas.style.opacity = 0; // Hide main canvas until glaze is ready
        // Force a paint cycle to ensure the reframe happens
        send({ type: "needs-paint" });
      }
      // Note: Glaze gets turned on only on a call to `resize` or `gap` via a piece,
      // or when enabling glaze after it was off (handled above).
      return;
    }

    if (type === "disk-loaded-and-booted") {
      perf.markBoot("disk-loaded-and-booted");
      
      // Skip preload marker on default init piece, and toggle it if necessary.
      if (currentPiece !== null && !window.waitForPreload)
        window.preloaded = true;
      //if (debug && logs.loading)
      //  console.log("⏳ Preloaded:", window.preloaded ? "✅" : "❌");
      
      // Note: Boot canvas is now hidden by "piece-paint-ready" signal instead
      // This allows the noise16 default to show through the boot overlay
      
      // 🔷 Auto-restore Tezos wallet session (Beacon stores in localStorage)
      restoreWalletSession().catch(() => {}); // Fire and forget, errors logged inside
      
      // Notify parent that disk is ready
      if (window.parent) {
        window.parent.postMessage({ 
          type: "boot-log", 
          message: `ready: ${currentPiece || 'prompt'}` 
        }, "*");
        window.parent.postMessage({ type: "ready" }, "*");
      }
      
      // Print perf report
      perf.printReport();
      
      // Fallback: if piece uses default paint (no custom paint), still hide boot canvas after a short delay
      // This handles edge cases where piece-paint-ready is never sent
      setTimeout(() => {
        if (window.acHIDE_BOOT_LOG) {
          window.acHIDE_BOOT_LOG();
        }
      }, 500);
    }
    
    // 🎨 Hide boot canvas when piece's paint function takes over from default noise16
    if (type === "piece-paint-ready") {
      if (window.acHIDE_BOOT_LOG) {
        window.acHIDE_BOOT_LOG();
      }
      consumeDiskSends(send);
      return;
    }
    if (type === "back-to-piece") {
      // Check if we have labelBack state active
      if (mainThreadLabelBack) {
        // Clear the labelBack state since we're handling the navigation
        mainThreadLabelBack = false;
        window.safeSessionStorageRemove("aesthetic-labelBack");

        // Navigate directly to the target piece from worker instead of using history.back()
        // This avoids the reload cycle for kidlisp pieces
        const targetPiece = content?.targetPiece || "chat"; // Use target from worker or fallback to chat

        // Update the URL and load the target piece directly
        const targetUrl = new URL(window.location);
        targetUrl.pathname = "/" + targetPiece;
        window.history.pushState(null, "", targetUrl);

        const parsed = parse(targetPiece);
        send({
          type: "history-load",
          content: parsed,
        });
      } else {
        history.back();
      }
      return false;
    }

    if (type === "disk-unload") {
      return;
    }

    // 🌟 Update & Render (Compositing)
    // IMPORTANT: Always release the frame pump latch for render/update.
    // If we return early (pixel mismatch) or throw during compositing, bios can stop
    // requesting frames entirely (frameAlreadyRequested stays true).
    if (!(type === "render" || type === "update")) return;
    if (!content) {
      frameAlreadyRequested = false;
      clearFrameStallWatchdog();
      return;
    }

    try {

    // Mark first render message from worker
    if (!window._firstRenderReceived) {
      window._firstRenderReceived = true;
      perf.markBoot("first-render-from-worker");
    }

    if (content.TwoD) {
      TwoD?.pack(content.TwoD);
    }

    updateSynths(content.sound); // 🔈 Trigger any audio that was called upon.

    // 🖥️ Compositing - OPTIMIZED for zero-copy pixel buffer transfer

    // TEMPORARILY DISABLE PIXEL OPTIMIZER to debug black buffer dragging issue
    /*
    if (!window.pixelOptimizer) {
      try {
        const { pixelOptimizer } = await import('./lib/graphics-optimizer.mjs');
        window.pixelOptimizer = pixelOptimizer;
      } catch (err) {
        console.warn('🟡 Graphics optimizer not available, using fallback:', err);
      }
    }
    */

    // This is a bit messy compared to what happens inside of content.reframe -> frame below. 22.10.27.02.05
    if (
      content.pixels?.byteLength > 0 &&
      content.width === screen.width &&
      content.height === screen.height
    ) {
      // 🚀 OPTIMIZATION: Use zero-copy ImageData creation when possible
      if (window.pixelOptimizer) {
        try {
          imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels, 
            content.width, 
            content.height
          );
          // Update screen reference to use the zero-copy buffer
          assign(screen, {
            pixels: imageData.data,
            width: content.width,
            height: content.height,
          });
          markReframePixelsReceived("zero-copy", content.width, content.height);
          window.pixelOptimizer.stats.framesProcessed++;
        } catch (err) {
          console.warn('🟡 Zero-copy optimization failed, using fallback:', err);
          // Fallback to original method
          screen.pixels = new Uint8ClampedArray(content.pixels);
          let width = screen.width;
          let height = screen.height;
          const expectedLength = width * height * 4;
          
          // Handle reframeJustCompleted separately so it's not blocked by content.reframe
          if (reframeJustCompleted) {
            if (screen.pixels.length === expectedLength) {
              imageData = new ImageData(screen.pixels, width, height);
              if (underlayFrame) {
                console.log('🔄 REFRAME PATH (pixelOptimizer): Created fresh imageData with dimensions:', width, 'x', height);
              }
              markReframePixelsReceived("pixelOptimizer-reframe", width, height);
            }
            reframeJustCompleted = false;
          } else if (
            !content.reframe &&
            screen.pixels.length === expectedLength
          ) {
            imageData = new ImageData(screen.pixels, width, height);
            markReframePixelsReceived("pixelOptimizer", width, height);
            if (underlayFrame) {
              // console.log("🎬 Fallback ImageData created during tape playback");
            }
          }
        }
      } else {
        // Original fallback code
        screen.pixels = new Uint8ClampedArray(content.pixels);
        let width = screen.width;
        let height = screen.height;
        const expectedLength = width * height * 4;

        // Handle reframeJustCompleted separately so it's not blocked by content.reframe
        if (reframeJustCompleted) {
          if (screen.pixels.length === expectedLength) {
            imageData = new ImageData(screen.pixels, width, height);
            markReframePixelsReceived("fallback-reframe", width, height);
          }
          reframeJustCompleted = false;
        } else if (
          !content.reframe &&
          screen.pixels.length === expectedLength
        ) {
          imageData = new ImageData(screen.pixels, width, height);
          markReframePixelsReceived("fallback", width, height);
        }
        // REMOVED: Special blocking case for underlayFrame + content.reframe
        // This was preventing imageData updates during tape playback reframes
      }
    } else if (reframeJustCompleted && content.pixels?.byteLength > 0) {
      // Special case: after reframe with new dimensions, create ImageData even if screen dimensions don't match yet
      if (content.width !== canvas.width || content.height !== canvas.height) {
        if (window.acReframeDebug) {
          console.log("⚠️ REFRAME: post-reframe size mismatch, deferring", {
            contentWidth: content.width,
            contentHeight: content.height,
            canvasWidth: canvas.width,
            canvasHeight: canvas.height,
          });
        }
      } else {
        try {
          if (window.pixelOptimizer) {
            // 🚀 OPTIMIZATION: Zero-copy reframe handling
            imageData = window.pixelOptimizer.createImageDataZeroCopy(
              content.pixels,
              content.width,
              content.height
            );
            // Update screen dimensions to match
            assign(screen, {
              pixels: imageData.data,
              width: content.width,
              height: content.height,
            });
            markReframePixelsReceived("post-reframe-zero-copy", content.width, content.height);
            window.pixelOptimizer.stats.framesProcessed++;
          } else {
            // Original fallback
            const newPixels = new Uint8ClampedArray(content.pixels);
            imageData = new ImageData(newPixels, content.width, content.height);
            // Update screen dimensions to match
            assign(screen, {
              pixels: imageData.data,
              width: content.width,
              height: content.height,
            });
            markReframePixelsReceived("post-reframe", content.width, content.height);
          }
          reframeJustCompleted = false;
        } catch (err) {
          console.warn("⚠️ Failed to create post-reframe ImageData:", err);
          reframeJustCompleted = false;
        }
      }
    }

    // old threed garbage collection (remove)

    // Check for a change in resolution.
    if (content.reframe) {
      // Reframe the captured pixels.
      frame(content.reframe.width, content.reframe.height, content.reframe.gap);
      pen?.retransformPosition();
    }

    if (content.cursorCode) pen?.setCursorCode(content.cursorCode);

    // Abort the render if pixels don't match.
    if (
      content.dirtyBox === undefined &&
      content.pixels?.length !== undefined &&
      content.pixels?.length !== screen.pixels.length
    ) {
      if (underlayFrame) console.log("🛑 VIDEO: Aborted render - pixel buffer mismatch!", "Content:", content.pixels.length, "Screen:", screen.pixels.length);
      console.warn("Aborted render. Pixel buffers did not match.");
      console.log(
        "Content pixels:",
        content.pixels.length,
        "Screen:",
        screen.pixels.length,
        content.didntRender,
        content.reframe,
        "Freeze:",
        freezeFrame,
      );
      frameAlreadyRequested = false; // 🗨️ Tell the system we are ready for another frame.
      clearFrameStallWatchdog();
      return;
    }

    let dirtyBoxBitmapCan;

    // 👌 Otherwise, grab all the pixels, or some, if `dirtyBox` is present.
    if (content.dirtyBox) {
      // 🅰️ Cropped update - OPTIMIZED for zero-copy and async rendering
      if (window.pixelOptimizer) {
        try {
          // Use optimized dirty box rendering
          const imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels,
            content.dirtyBox.w,
            content.dirtyBox.h
          );

          // Reuse the dirtyBox canvas instead of creating new ones
          if (
            dirtyBoxCanvas.width !== imageData.width ||
            dirtyBoxCanvas.height !== imageData.height
          ) {
            dirtyBoxCanvas.width = imageData.width;
            dirtyBoxCanvas.height = imageData.height;
          }

          // Use async rendering for better performance
          if (window.pixelOptimizer.asyncRenderingSupported && imageData.width * imageData.height > 1024) {
            window.pixelOptimizer.renderImageDataAsync(imageData, dirtyBoxCtx, 0, 0);
          } else {
            dirtyBoxCtx.putImageData(imageData, 0, 0);
          }
          
          dirtyBoxBitmapCan = dirtyBoxCanvas; // Reference the reused canvas
          
        } catch (err) {
          console.warn('🟡 Optimized dirty box rendering failed, using fallback:', err);
          // Fallback to original method
          const imageData = new ImageData(
            new Uint8ClampedArray(content.pixels),
            content.dirtyBox.w,
            content.dirtyBox.h,
          );

          if (
            dirtyBoxCanvas.width !== imageData.width ||
            dirtyBoxCanvas.height !== imageData.height
          ) {
            dirtyBoxCanvas.width = imageData.width;
            dirtyBoxCanvas.height = imageData.height;
          }

          dirtyBoxCtx.putImageData(imageData, 0, 0);
          dirtyBoxBitmapCan = dirtyBoxCanvas;
        }
      } else {
        // Original fallback code
        const imageData = new ImageData(
          new Uint8ClampedArray(content.pixels), // Is this the only necessary part?
          content.dirtyBox.w,
          content.dirtyBox.h,
        );

        // Reuse the dirtyBox canvas instead of creating new ones
        if (
          dirtyBoxCanvas.width !== imageData.width ||
          dirtyBoxCanvas.height !== imageData.height
        ) {
          dirtyBoxCanvas.width = imageData.width;
          dirtyBoxCanvas.height = imageData.height;
        }

        dirtyBoxCtx.putImageData(imageData, 0, 0);
        dirtyBoxBitmapCan = dirtyBoxCanvas; // Reference the reused canvas

        // Use this alternative once it's faster. 2022.01.29.02.46
        // const dbCtx = dirtyBoxBitmapCan.getContext("bitmaprenderer");
        // dbCtx.transferFromImageBitmap(dirtyBoxBitmap);
      }
    } else if (content.paintChanged && content.pixels && !content.reframe) {
      // 🅱️ Normal full-screen update (skip during reframe to avoid dimension mismatch) - OPTIMIZED
      if (window.pixelOptimizer) {
        try {
          // Use zero-copy optimization for full screen updates
          imageData = window.pixelOptimizer.createImageDataZeroCopy(
            content.pixels,
            content.width,
            content.height
          );
        } catch (err) {
          console.warn('🟡 Zero-copy full screen update failed, using fallback:', err);
          // Fallback to original method
          const pixelArray = new Uint8ClampedArray(content.pixels);
          const expectedLength = content.width * content.height * 4;

          if (pixelArray.length === expectedLength || reframeJustCompleted) {
            if (pixelArray.length === expectedLength) {
              imageData = new ImageData(pixelArray, content.width, content.height);
            }
            if (reframeJustCompleted) {
              reframeJustCompleted = false;
            }
          }
        }
      } else {
        // Original fallback code
        const pixelArray = new Uint8ClampedArray(content.pixels);
        const expectedLength = content.width * content.height * 4;

        // Only create ImageData if pixel buffer length matches expected dimensions
        // Be more lenient right after reframe completion to restore animation
        if (pixelArray.length === expectedLength || reframeJustCompleted) {
          if (pixelArray.length === expectedLength) {
            imageData = new ImageData(pixelArray, content.width, content.height);
          }
          // Reset flag regardless of whether ImageData creation succeeded
          if (reframeJustCompleted) {
            reframeJustCompleted = false;
          }
        }
      }
    }

    pixelsDidChange = content.paintChanged || false;

    const useWebGLComposite = shouldUseWebGLComposite(content);
    const webglCompositeActive = useWebGLComposite && webglBlitter?.isReady();
    let overlayTargetCtx = webglCompositeActive ? octx : ctx;

    // ✨ UI Overlay (Composite) Layer
    // This currently paints corner labels and tape progress bars only.
    // (So they can be skipped for recordings?)
    let paintOverlays = {};

    // Frame-persistent overlay cache that survives reframes
    if (!window.framePersistentOverlayCache) {
      window.framePersistentOverlayCache = {};
    }

    // Per-overlay canvas cache to prevent canvas thrashing
    if (!window.overlayCanvasCache) {
      window.overlayCanvasCache = {};
    }

    function buildOverlay(name, o) {
      // Skip ALL overlays in clean mode (no stamps, no progress bars, no labels)
      if (window.currentRecordingOptions?.cleanMode) {
        return;
      }
      
      // Only log reframe operations to debug flicker
      const isHudOverlay = name === "label" || name === "qrOverlay" || name === "qrCornerLabel" || name === "qrFullscreenLabel" || name === "authorOverlay";

      // Skip tape progress bar in clean mode only
      if (name === "tapeProgressBar" && window.currentRecordingOptions?.cleanMode) {
        console.log("🎬 📼 Skipping tape progress bar in clean mode");
        return;
      }

      if (!o || !o.img) {
        // During reframes, if overlay data is missing but we have a cached version, use it
        // EXCEPT for tapeProgressBar, merryProgressBar, durationProgressBar, durationTimecode and qrOverlay which should never use cached versions
        if (content.reframe && window.framePersistentOverlayCache[name] && name !== "tapeProgressBar" && name !== "merryProgressBar" && name !== "durationProgressBar" && name !== "durationTimecode" && name !== "qrOverlay" && name !== "qrCornerLabel" && name !== "qrFullscreenLabel" && name !== "authorOverlay") {
          paintOverlays[name] = window.framePersistentOverlayCache[name];
          return;
        }
        return;
      }

      // Create or reuse dedicated canvas for this overlay
      if (!window.overlayCanvasCache[name]) {
        window.overlayCanvasCache[name] = {
          canvas: document.createElement("canvas"),
          lastKey: null,
        };
        window.overlayCanvasCache[name].ctx =
          window.overlayCanvasCache[name].canvas.getContext("2d");
      }

      const overlayCache = window.overlayCanvasCache[name];
      
      // Create a content-aware cache key that includes pixel data hash for HUD overlays
      let currentKey = `${o.img.width}x${o.img.height}_${o.x}_${o.y}`;
      
      // For HUD overlays (like labels), disable caching to ensure real-time updates for KidLisp syntax highlighting
      if (isHudOverlay && name === "label") {
        overlayCache.lastKey = null; // Force regeneration every frame like QR overlay
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For tape progress bar, completely disable ALL caching to ensure every frame is painted
      if (name === "tapeProgressBar") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For merry progress bar, completely disable ALL caching to ensure every frame is painted
      if (name === "merryProgressBar") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For duration progress bar, completely disable ALL caching to ensure every frame is painted
      if (name === "durationProgressBar") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For duration timecode, completely disable ALL caching to ensure every frame is painted
      if (name === "durationTimecode") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For QR overlay, completely disable ALL caching to allow text label font loading
      if (name === "qrOverlay") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For QR corner label, completely disable ALL caching to allow text label font loading
      if (name === "qrCornerLabel") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }
      
      // For QR fullscreen label, completely disable ALL caching to allow text label font loading
      if (name === "qrFullscreenLabel") {
        overlayCache.lastKey = null; // Force regeneration every frame
        delete window.framePersistentOverlayCache[name]; // Clear persistent cache
        currentKey += `_${performance.now()}`; // Force unique key every time
      }

      // Only rebuild if overlay actually changed
      // Force rebuild every frame for tape progress bar, merry progress bar, duration progress bar, duration timecode and QR overlay (no caching)
      if (
        name !== "tapeProgressBar" &&
        name !== "merryProgressBar" &&
        name !== "durationProgressBar" &&
        name !== "durationTimecode" &&
        name !== "qrOverlay" &&
        name !== "qrCornerLabel" &&
        name !== "qrFullscreenLabel" &&
        overlayCache.lastKey === currentKey &&
        window.framePersistentOverlayCache[name]
      ) {
        paintOverlays[name] = window.framePersistentOverlayCache[name];
        return;
      }
      
      // DEBUG: Log cache invalidation for HUD overlays
      if (isHudOverlay && overlayCache.lastKey && overlayCache.lastKey !== currentKey) {
        // Cache invalidation tracking removed for cleaner console output
      }

      overlayCache.lastKey = currentKey;

      // Debug: Log when creating paint function for qrFullscreenLabel
      if (name === "qrFullscreenLabel") {
        console.log(`🔍 Creating paint function for qrFullscreenLabel`);
      }

      paintOverlays[name] = () => {
        const canvas = overlayCache.canvas;
        const overlayCtx = overlayCache.ctx;

        overlayCtx.imageSmoothingEnabled = false;

        // Resize this overlay's dedicated canvas if needed
        if (canvas.width !== o.img.width || canvas.height !== o.img.height) {
          canvas.width = o.img.width;
          canvas.height = o.img.height;
        }

        try {
          let imageData;
          // Add debug logging for durationTimecode
          if (name === "durationTimecode") {
            console.log(`🕐 Creating ImageData for timecode:`, {
              pixelsType: typeof o.img.pixels,
              pixelsLength: o.img.pixels.length,
              width: o.img.width,
              height: o.img.height,
              expectedLength: o.img.width * o.img.height * 4
            });
          }
          
          // Add debug logging for qrFullscreenLabel and qrOverlay
          if (name === "qrFullscreenLabel") {
            console.log(`🔍 Creating ImageData for qrFullscreenLabel:`, {
              pixelsType: typeof o.img.pixels,
              pixelsLength: o.img.pixels.length,
              width: o.img.width,
              height: o.img.height,
              expectedLength: o.img.width * o.img.height * 4
            });
          }
          

          
          // Use graphics optimizer if available, fallback to traditional method
          if (window.pixelOptimizer) {
            imageData = window.pixelOptimizer.createImageDataZeroCopy(
              o.img.pixels.buffer || o.img.pixels,
              o.img.width,
              o.img.height,
            );
          } else {
            imageData = new ImageData(
              o.img.pixels,
              o.img.width,
              o.img.height,
            );
          }
          overlayCtx.putImageData(imageData, 0, 0);
          
          if (name === "durationTimecode") {
            console.log(`🕐 Successfully created and painted ImageData for timecode`);
          }
          
          if (name === "qrFullscreenLabel") {
            console.log(`🔍 Successfully created and painted ImageData for qrFullscreenLabel`);
          }
        } catch (error) {
          console.error(`❌ Error creating ImageData for ${name}:`, error);
          return;
        }

        // Paint overlay to main canvas (ctx is the main canvas context)
        if (name === "tapeProgressBar") {
          overlayTargetCtx.drawImage(canvas, o.x, o.y);
        } else {
          // Add debug logging for durationTimecode
          if (name === "durationTimecode") {
            console.log(`🕐 Drawing durationTimecode to main canvas at (${o.x}, ${o.y}) with size ${canvas.width}x${canvas.height}`);
          }
          
          // Add debug logging for qrFullscreenLabel
          if (name === "qrFullscreenLabel") {
            console.log(`🔍 Drawing qrFullscreenLabel to main canvas at (${o.x}, ${o.y}) with size ${canvas.width}x${canvas.height}`);
          }
          
          overlayTargetCtx.drawImage(canvas, o.x, o.y);
          
          if (name === "durationTimecode") {
            console.log(`🕐 Finished drawing durationTimecode`);
          }
          
          if (name === "qrFullscreenLabel") {
            console.log(`🔍 Finished drawing qrFullscreenLabel`);
          }
        }
      };

      // Debug logging for tape progress bar painter creation
      if (name === "tapeProgressBar" && false) { // Disabled verbose logging
        console.log(`🏗️ Created painter for "${name}":`, {
          painterExists: !!paintOverlays[name],
          canvasSize: `${overlayCache.canvas.width}x${overlayCache.canvas.height}`
        });
      }

      // Don't cache QR overlay painters to allow animation
      // Don't cache tapeProgressBar, merryProgressBar or durationProgressBar painters either - force regeneration every frame
      // Don't cache authorOverlay as it changes per piece
      if (isHudOverlay && name !== "qrOverlay" && name !== "qrCornerLabel" && name !== "qrFullscreenLabel" && name !== "authorOverlay" && name !== "tapeProgressBar" && name !== "merryProgressBar" && name !== "durationProgressBar") {
        window.framePersistentOverlayCache[name] = paintOverlays[name];
      }
    }

    // 🎬 Recording UI overlay painters (rendered to recordingUICan, NOT captured in tapes)
    let recordingUIOverlays = {};
    
    function buildRecordingUIOverlay(name, o) {
      if (!o || !o.img) return;
      
      // Create or reuse dedicated canvas for this overlay
      if (!window.recordingUIOverlayCache) {
        window.recordingUIOverlayCache = {};
      }
      if (!window.recordingUIOverlayCache[name]) {
        window.recordingUIOverlayCache[name] = {
          canvas: document.createElement("canvas"),
          lastKey: null,
        };
        window.recordingUIOverlayCache[name].ctx =
          window.recordingUIOverlayCache[name].canvas.getContext("2d");
      }
      
      const overlayCache = window.recordingUIOverlayCache[name];
      const currentKey = `${o.img.width}x${o.img.height}_${o.x}_${o.y}_${performance.now()}`;
      
      // Always regenerate recording UI overlays (no caching)
      overlayCache.lastKey = null;
      
      if (overlayCache.canvas.width !== o.img.width) {
        overlayCache.canvas.width = o.img.width;
      }
      if (overlayCache.canvas.height !== o.img.height) {
        overlayCache.canvas.height = o.img.height;
      }
      
      const overlayCtx = overlayCache.ctx;
      overlayCtx.imageSmoothingEnabled = false;
      
      const imageData = new ImageData(
        new Uint8ClampedArray(o.img.pixels),
        o.img.width,
        o.img.height,
      );
      overlayCtx.putImageData(imageData, 0, 0);
      
      // Create painter function that draws to recordingUICtx
      recordingUIOverlays[name] = () => {
        if (o.opacity !== undefined && o.opacity < 1) {
          recordingUICtx.globalAlpha = o.opacity;
        }
        recordingUICtx.drawImage(overlayCache.canvas, o.x, o.y);
        if (o.opacity !== undefined && o.opacity < 1) {
          recordingUICtx.globalAlpha = 1;
        }
      };
    }

    buildOverlay("label", content.label);
    buildOverlay("qrOverlay", content.qrOverlay);
    buildOverlay("qrCornerLabel", content.qrCornerLabel);
    buildOverlay("qrFullscreenLabel", content.qrFullscreenLabel);
    buildOverlay("authorOverlay", content.authorOverlay); // 👤 Author attribution for KidLisp pieces
    buildOverlay("merryProgressBar", content.merryProgressBar); // 🎄 Merry pipeline progress bar
    buildOverlay("tapeProgressBar", content.tapeProgressBar);
    buildOverlay("durationProgressBar", content.durationProgressBar);
    buildOverlay("durationTimecode", content.durationTimecode);
    buildOverlay("hitboxDebug", content.hitboxDebug); // Debug overlay for HUD hitbox visualization
    
    // 🎬 Build recording UI overlays (NOT captured in recordings)
    if (content.recordingUI) {
      for (const [name, overlayData] of Object.entries(content.recordingUI)) {
        buildRecordingUIOverlay(name, overlayData);
      }
    }
    
    // Debug: Log overlay data reception
    if (content.durationTimecode) {
      console.log("🕐 BIOS received durationTimecode:", {
        x: content.durationTimecode.x,
        y: content.durationTimecode.y,
        width: content.durationTimecode.img?.width,
        height: content.durationTimecode.img?.height,
        pixelsLength: content.durationTimecode.img?.pixels?.length,
        hasPixels: !!content.durationTimecode.img?.pixels
      });
    }
    // console.log("🖼️ Received overlay data:", {
    //   hasLabel: !!content.label,
    //   hasQR: !!content.qrOverlay,
    //   hasTapeProgress: !!content.tapeProgressBar,
    //   labelDetails: content.label ? {
    //     x: content.label.x,
    //     y: content.label.y,
    //     width: content.label.img?.width,
    //     height: content.label.img?.height,
    //     pixelLength: content.label.img?.pixels?.length,
    //     hasPixels: !!content.label.img?.pixels
    //   } : null
    // });

    function draw() {
      // 🎯 Dynamic FPS Detection for display-rate independent recording
      const currentTime = performance.now();
      if (lastFrameTime > 0) {
        const frameTime = currentTime - lastFrameTime;
        frameTimes.push(frameTime);
        
        // Keep only recent frame timings for accurate FPS detection
        if (frameTimes.length > FPS_SAMPLE_SIZE) {
          frameTimes.shift();
        }
        
        // Calculate actual display FPS after we have enough samples
        if (frameTimes.length >= FPS_SAMPLE_SIZE) {
          const averageFrameTime = frameTimes.reduce((sum, time) => sum + time, 0) / frameTimes.length;
          detectedDisplayFPS = Math.round(1000 / averageFrameTime);
          // console.log(`🎯 Detected display FPS: ${detectedDisplayFPS}`);
        }
      }
      lastFrameTime = currentTime;
      
      // During tape playback, keep canvas fully opaque but transparent in its pixels
      // (the video underlay sits behind; the piece uses wipe(0,0,0,0) for transparency)
      if (underlayFrame) {
        canvas.style.removeProperty("opacity");
        canvas.style.removeProperty("mix-blend-mode");
        canvas.style.background = "transparent";
      } else {
        // Restore canvas styling when not playing tape
        canvas.style.removeProperty("opacity");
        canvas.style.removeProperty("mix-blend-mode");
        canvas.style.removeProperty("background");
      }

      // �🅰️ Draw updated content from the piece.

      const db = content.dirtyBox;
      if (db) {
        if (!underlayFrame && webglCompositeActive && imageData) {
          if (canvas.style.visibility !== "hidden") {
            canvas.style.visibility = "hidden";
          }
          if (webglCompositeCanvas.style.display === "none") {
            webglCompositeCanvas.style.display = "block";
          }
          if (overlayCan.style.display === "none") {
            overlayCan.style.display = "block";
          }
          if (webgpuCanvas.style.display !== "none") {
            webgpuCanvas.style.display = "none";
          }
          webglBlitter.render(imageData);
        } else {
          if (canvas.style.visibility !== "visible") {
            canvas.style.visibility = "visible";
          }
          if (webglCompositeCanvas.style.display !== "none") {
            webglCompositeCanvas.style.display = "none";
          }
          if (overlayCan.style.display !== "none") {
            overlayCan.style.display = "none";
          }
          ctx.drawImage(dirtyBoxBitmapCan, db.x, db.y);
          if (glaze.on) Glaze.update(dirtyBoxBitmapCan, db.x, db.y);
        }
      } else if (
        pixelsDidChange ||
        needs$creenshot ||
        mediaRecorder?.state === "recording"
      ) {
        // 🚀 OPTIMIZATION: Async bitmap rendering with safety checks
        let skipImmediateOverlays = false; // Flag to skip immediate overlay painting for async rendering
        const isRecording =
          mediaRecorder?.state === "recording" &&
          mediaRecorderStartTime !== undefined;
        const isPlaybackOnly =
          underlayFrame && !isRecording && !needs$creenshot;

        if (
          imageData &&
          imageData.data &&
          imageData.data.buffer &&
          imageData.data.buffer.byteLength > 0 &&
          imageData.width === ctx.canvas.width &&
          imageData.height === ctx.canvas.height
        ) {
          // Dimensions match - clear mismatch counter and log if we just recovered
          if (underlayFrame && dimensionMismatchCount > 0) {
            console.log('✅ REFRAME: Dimension sync restored after', dimensionMismatchCount, 'mismatched frames. Canvas:', ctx.canvas.width, 'x', ctx.canvas.height);
            dimensionMismatchCount = 0;
          }
          // Use async rendering for better performance (except during tape playback for immediate UI)
          const forceSynchronousRendering = isRecording || needs$creenshot;
          
          // Skip CPU rendering if WebGPU is enabled
          if (content.webgpuEnabled) {
            // Switch backend if piece requests a specific one
            if (content.gpuBackend && activeGPUBackend?.getName() !== content.gpuBackend) {
              log.gpu.debug?.(`Switching to requested backend: ${content.gpuBackend}`);
              window.switchGPUBackend?.(content.gpuBackend).catch(err => {
                log.gpu.warn?.(`Failed to switch to ${content.gpuBackend}:`, err);
              });
            }
            
            // Hide CPU canvas and rely on WebGPU surface
            if (canvas.style.visibility !== "hidden") {
              canvas.style.visibility = "hidden";
            }
            if (webgpuCanvas.style.display === "none") {
              webgpuCanvas.style.display = "block";
            }
            if (webglCompositeCanvas.style.display !== "none") {
              webglCompositeCanvas.style.display = "none";
            }
            if (overlayCan.style.display !== "none") {
              overlayCan.style.display = "none";
            }
            skipImmediateOverlays = true; // Overlays will be handled separately
          } else if (webglCompositeActive) {
            if (canvas.style.visibility !== "hidden") {
              canvas.style.visibility = "hidden";
            }
            if (webglCompositeCanvas.style.display === "none") {
              webglCompositeCanvas.style.display = "block";
            }
            if (overlayCan.style.display === "none") {
              overlayCan.style.display = "block";
            }
            if (webgpuCanvas.style.display !== "none") {
              webgpuCanvas.style.display = "none";
            }
            webglBlitter.render(imageData);
            skipImmediateOverlays = false;
          } else if (underlayFrame) {
            if (canvas.style.visibility !== "visible") {
              canvas.style.visibility = "visible";
            }
            if (webgpuCanvas.style.display !== "none") {
              webgpuCanvas.style.display = "none";
            }
            if (webglCompositeCanvas.style.display !== "none") {
              webglCompositeCanvas.style.display = "none";
            }
            if (overlayCan.style.display !== "none") {
              overlayCan.style.display = "none";
            }
            // Force sync rendering during tape playback for immediate UI updates
            ctx.putImageData(imageData, 0, 0);
          } else if (!forceSynchronousRendering && window.pixelOptimizer && window.pixelOptimizer.asyncRenderingSupported) {
            try {
              if (canvas.style.visibility !== "visible") {
                canvas.style.visibility = "visible";
              }
              if (webgpuCanvas.style.display !== "none") {
                webgpuCanvas.style.display = "none";
              }
              if (webglCompositeCanvas.style.display !== "none") {
                webglCompositeCanvas.style.display = "none";
              }
              if (overlayCan.style.display !== "none") {
                overlayCan.style.display = "none";
              }
              // Non-blocking async rendering
              window.pixelOptimizer.renderImageDataAsync(imageData, ctx, 0, 0).then(() => {
                // Paint overlays after async fallback rendering completes
                if (paintOverlays["label"]) paintOverlays["label"]();
                if (paintOverlays["qrOverlay"]) paintOverlays["qrOverlay"]();
                if (paintOverlays["qrCornerLabel"]) paintOverlays["qrCornerLabel"]();
                if (paintOverlays["qrFullscreenLabel"]) paintOverlays["qrFullscreenLabel"]();
                if (paintOverlays["authorOverlay"]) paintOverlays["authorOverlay"]();
                if (paintOverlays["tapeProgressBar"] && !window.currentRecordingOptions?.cleanMode) paintOverlays["tapeProgressBar"]();
                if (paintOverlays["durationProgressBar"]) paintOverlays["durationProgressBar"]();
                if (paintOverlays["hitboxDebug"]) paintOverlays["hitboxDebug"](); // Debug overlay (green hitbox)
              }).catch(err => {
                console.warn('🟡 Fallback async rendering failed:', err);
                ctx.putImageData(imageData, 0, 0);
                skipImmediateOverlays = false;
              });
              skipImmediateOverlays = true; // Skip immediate overlays; they'll paint in async callback
            } catch (err) {
              if (canvas.style.visibility !== "visible") {
                canvas.style.visibility = "visible";
              }
              if (webgpuCanvas.style.display !== "none") {
                webgpuCanvas.style.display = "none";
              }
              if (webglCompositeCanvas.style.display !== "none") {
                webglCompositeCanvas.style.display = "none";
              }
              if (overlayCan.style.display !== "none") {
                overlayCan.style.display = "none";
              }
              ctx.putImageData(imageData, 0, 0);
              skipImmediateOverlays = false;
            }
          } else {
            if (canvas.style.visibility !== "visible") {
              canvas.style.visibility = "visible";
            }
            if (webgpuCanvas.style.display !== "none") {
              webgpuCanvas.style.display = "none";
            }
            if (webglCompositeCanvas.style.display !== "none") {
              webglCompositeCanvas.style.display = "none";
            }
            if (overlayCan.style.display !== "none") {
              overlayCan.style.display = "none";
            }
            ctx.putImageData(imageData, 0, 0);
            skipImmediateOverlays = false;
          }
        } else {
          // Dimension mismatch - handle differently for tape playback vs normal
          if (underlayFrame) {
            // During tape playback, keep the canvas at correct size and wait for matching data
            dimensionMismatchCount++;
            if (dimensionMismatchCount === 1 || dimensionMismatchCount % 10 === 0) {
              console.log('⏸️ REFRAME: Dimension mismatch #' + dimensionMismatchCount + '. Canvas:', ctx.canvas.width, 'x', ctx.canvas.height, '| ImageData:', imageData?.width, 'x', imageData?.height);
            }
            skipImmediateOverlays = true; // Don't paint overlays
            // Keep requesting paint so we get fresh data with correct dimensions
            setTimeout(() => send({ type: "needs-paint" }), 0);
          } else {
            // For normal pieces, try to recover by getting fresh imageData from canvas
            if (
              imageData &&
              imageData.data &&
              imageData.data.buffer &&
              (imageData.data.buffer.byteLength === 0 ||
                imageData.width !== ctx.canvas.width ||
                imageData.height !== ctx.canvas.height)
            ) {
              imageData = ctx.getImageData(
                0,
                0,
                ctx.canvas.width,
                ctx.canvas.height,
              );
              if (imageData.data.buffer.byteLength > 0) {
                // Use async rendering for better performance (except during tape playback for immediate UI)
                const forceSynchronousRendering = isRecording || needs$creenshot;
                if (!forceSynchronousRendering && window.pixelOptimizer && window.pixelOptimizer.asyncRenderingSupported) {
                  try {
                    window.pixelOptimizer.renderImageDataAsync(imageData, ctx, 0, 0).then(() => {
                      if (paintOverlays["label"]) paintOverlays["label"]();
                      if (paintOverlays["qrOverlay"]) paintOverlays["qrOverlay"]();
                      if (paintOverlays["qrCornerLabel"]) paintOverlays["qrCornerLabel"]();
                      if (paintOverlays["qrFullscreenLabel"]) paintOverlays["qrFullscreenLabel"]();
                      if (paintOverlays["authorOverlay"]) paintOverlays["authorOverlay"]();
                      if (paintOverlays["tapeProgressBar"] && !window.currentRecordingOptions?.cleanMode) paintOverlays["tapeProgressBar"]();
                      if (paintOverlays["durationProgressBar"]) paintOverlays["durationProgressBar"]();
                      if (paintOverlays["hitboxDebug"]) paintOverlays["hitboxDebug"]();
                    }).catch(err => {
                      console.warn('🟡 Async rendering failed:', err);
                      ctx.putImageData(imageData, 0, 0);
                      skipImmediateOverlays = false;
                    });
                    skipImmediateOverlays = true; // Skip immediate overlays; they'll paint in async callback
                  } catch (err) {
                    console.warn('🟡 Async render setup failed, using fallback:', err);
                    ctx.putImageData(imageData, 0, 0);
                    skipImmediateOverlays = false;
                  }
                } else {
                  ctx.putImageData(imageData, 0, 0);
                  skipImmediateOverlays = false;
                }
              }
            }
          }
        }

        // Store clean pixel data for worker communication (before overlays)
        if (
          !isPlaybackOnly &&
          imageData &&
          imageData.data &&
          imageData.data.buffer &&
          imageData.data.buffer.byteLength > 0
        ) {
          assign(screen, {
            pixels: new Uint8ClampedArray(imageData.data),
            width: ctx.canvas.width,
            height: ctx.canvas.height,
          });
        }

        if (freezeFrameFrozen && !awaitingReframePixels) {
          reframeDrawn = true;
          reframeDrawnAt = performance.now();
        }

        // Check if we need to record frames (before painting overlays)
        // const isRecording determined earlier for overlay decisions

        // 📸 Capture clean screenshot data BEFORE overlays are painted (only when needed)
        let cleanScreenshotData = null;
        if (needs$creenshot) {
          if (webglCompositeActive && webglCompositeCanvas) {
            const capCtx = ensureCaptureCompositeCanvas(
              ctx.canvas.width,
              ctx.canvas.height,
            );
            capCtx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
            capCtx.drawImage(webglCompositeCanvas, 0, 0);
            cleanScreenshotData = capCtx.getImageData(
              0,
              0,
              ctx.canvas.width,
              ctx.canvas.height,
            );
          } else {
            cleanScreenshotData = ctx.getImageData(
              0,
              0,
              ctx.canvas.width,
              ctx.canvas.height,
            );
          }
        }

        // Paint overlays (but exclude tape progress from recordings)
        if (webglCompositeActive) {
          octx.clearRect(0, 0, octx.canvas.width, octx.canvas.height);
        }
        
        // console.log("🎨 Available overlay painters:", Object.keys(paintOverlays));
        let tapeProgressPainter = paintOverlays["tapeProgressBar"] || null;
        if (!tapeProgressPainter && content.tapeProgressBar) {
          buildOverlay("tapeProgressBar", content.tapeProgressBar);
          tapeProgressPainter = paintOverlays["tapeProgressBar"] || null;
        }
        let paintTapeProgressAfterCapture = false;

        if (paintOverlays["label"]) {
          if (!skipImmediateOverlays || isRecording || needs$creenshot) {
            paintOverlays["label"]();
          } else {
            // console.log("🏷️ Skipping immediate label overlay painting (async mode)");
          }
        } else {
          // Label overlay painter not found (no logging)
        }

        if (!skipImmediateOverlays && paintOverlays["qrOverlay"]) {
          paintOverlays["qrOverlay"]();
        }

        // 👤 Paint author overlay (bottom-left attribution for KidLisp pieces)
        if (!skipImmediateOverlays && paintOverlays["authorOverlay"]) {
          paintOverlays["authorOverlay"]();
        }

        // Paint hitbox debug overlay immediately (green, shows button hitbox)
        if (!skipImmediateOverlays && paintOverlays["hitboxDebug"]) {
          paintOverlays["hitboxDebug"]();
        }

        // Paint merry progress bar immediately (at the top, green theme)
        if (paintOverlays["merryProgressBar"]) {
          paintOverlays["merryProgressBar"]();
        } else if (content.merryProgressBar) {
          buildOverlay("merryProgressBar", content.merryProgressBar);
          if (paintOverlays["merryProgressBar"]) {
            paintOverlays["merryProgressBar"]();
          }
        }

        // 🎄⏰ Merry UTC debug overlay (top-right) - DISABLED
        // Useful for verifying that `mo`/`merryo` pipelines are aligned to the same UTC authority as `clock`.
        // if (!window.currentRecordingOptions?.cleanMode && content.merryUTC) {
        //   try {
        //     const utcLine1 = `UTC ${content.merryUTC.time}`;
        //     const utcLine2 = `cycle ${content.merryUTC.cycle}s · ${content.merryUTC.index}/${content.merryUTC.total} · ${content.merryUTC.piece}`;

        //     overlayTargetCtx.save();
        //     overlayTargetCtx.globalAlpha = 0.95;
        //     overlayTargetCtx.font = `12px YWFTProcessing-Regular, Arial, sans-serif`;
        //     overlayTargetCtx.textBaseline = "top";

        //     const pad = 6;
        //     const lineH = 14;
        //     const w1 = overlayTargetCtx.measureText(utcLine1).width;
        //     const w2 = overlayTargetCtx.measureText(utcLine2).width;
        //     const boxW = Math.ceil(Math.max(w1, w2) + pad * 2);
        //     const boxH = Math.ceil(lineH * 2 + pad * 2);
        //     const x = Math.max(0, overlayTargetCtx.canvas.width - boxW - 8);
        //     const y = 8;

        //     // Backplate
        //     overlayTargetCtx.fillStyle = "rgba(0, 0, 0, 0.55)";
        //     overlayTargetCtx.fillRect(x, y, boxW, boxH);

        //     // Text
        //     overlayTargetCtx.fillStyle = "white";
        //     overlayTargetCtx.fillText(utcLine1, x + pad, y + pad);
        //     overlayTargetCtx.fillText(utcLine2, x + pad, y + pad + lineH);
        //     overlayTargetCtx.restore();
        //   } catch (e) {
        //     // Silent fail: debug-only overlay
        //   }
        // }

        // Paint tape progress bar immediately (not affected by async skip)
        // Skip in clean mode (neat tapes)
        if (tapeProgressPainter && !window.currentRecordingOptions?.cleanMode) {
          if (isRecording) {
            paintTapeProgressAfterCapture = true;
          } else {
            tapeProgressPainter();
          }
        }

        // Paint duration progress bar immediately (not affected by async skip)
        if (paintOverlays["durationProgressBar"]) {
          paintOverlays["durationProgressBar"]();
        } else if (content.durationProgressBar) {
          buildOverlay("durationProgressBar", content.durationProgressBar);
          if (paintOverlays["durationProgressBar"]) {
            paintOverlays["durationProgressBar"]();
          }
        }

        // Paint duration timecode immediately (not affected by async skip)
        if (paintOverlays["durationTimecode"]) {
          paintOverlays["durationTimecode"]();
        } else if (content.durationTimecode) {
          buildOverlay("durationTimecode", content.durationTimecode);
          if (paintOverlays["durationTimecode"]) {
            paintOverlays["durationTimecode"]();
          }
        }

        // 📼 Capture frame data AFTER HUD overlays but BEFORE tape progress bar (including HUD in recording)
        if (isRecording) {
          // 🎯 Capture EVERY frame - no time-based throttling for maximum quality
          // GIF export will downsample to 30fps, MP4 will use all frames
          mediaRecorderFrameCount = (mediaRecorderFrameCount || 0) + 1;
          
          // Update recording progress for tape progress bar breathing pattern
          const relativeTimestamp = performance.now() - mediaRecorderStartTime;
          const targetDuration = window.currentRecordingOptions?.duration || 3000; // Default 3 seconds if not set
          window.currentTapeProgress = Math.min(1, relativeTimestamp / targetDuration);
          
          // Convert relative timestamp to absolute timestamp (milliseconds since epoch)
          const absoluteTimestamp =
            (Number.isFinite(window.recordingStartTimestamp)
              ? window.recordingStartTimestamp
              : Date.now()) + relativeTimestamp;
          let frameDataWithHUD;
          if (webglCompositeActive && webglCompositeCanvas) {
            const capCtx = ensureCaptureCompositeCanvas(
              ctx.canvas.width,
              ctx.canvas.height,
            );
            capCtx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
            capCtx.drawImage(webglCompositeCanvas, 0, 0);
            capCtx.drawImage(overlayCan, 0, 0);
            frameDataWithHUD = capCtx.getImageData(
              0,
              0,
              ctx.canvas.width,
              ctx.canvas.height,
            );
          } else {
            frameDataWithHUD = ctx.getImageData(
              0,
              0,
              ctx.canvas.width,
              ctx.canvas.height,
            );
          }
          
          // Capture pen position data for crosshair rendering during export
          // Only capture pen data if cursor is within canvas bounds
          const penData = pen?.pointers[1] ? (() => {
            const pointer = pen.pointers[1];
            const x = pointer.x;
            const y = pointer.y;
            const canvasWidth = ctx.canvas.width;
            const canvasHeight = ctx.canvas.height;
            
            // Check if pen is within canvas bounds
            if (x < 0 || x >= canvasWidth || y < 0 || y >= canvasHeight) {
              return null; // Don't capture pen data when cursor is off-screen
            }
            
            return {
              x: x,
              y: y,
              device: pointer.device
            };
          })() : null;
          
          recordedFrames.push([absoluteTimestamp, frameDataWithHUD, penData]);

          if (!Number.isFinite(absoluteTimestamp) || !Number.isFinite(relativeTimestamp)) {
            console.warn("🎬 ⚠️ Invalid frame timestamps detected", {
              absoluteTimestamp,
              relativeTimestamp,
              recordingStartTimestamp: window.recordingStartTimestamp,
            });
          }

          if (
            mediaRecorderFrameCount <= 5 ||
            mediaRecorderFrameCount % 60 === 0
          ) {
            const firstFrameTimestamp = recordedFrames[0]?.[0];
            const latestDelta =
              firstFrameTimestamp !== undefined
                ? absoluteTimestamp - firstFrameTimestamp
                : 0;
            console.log(
              "🎬 Frame capture progress:",
              `${recordedFrames.length} frames total — latest Δ ${latestDelta.toFixed(2)}ms`,
            );
          }
        }

        //  Return clean screenshot data (without overlays)
        if (needs$creenshot) {
          needs$creenshot(cleanScreenshotData);
          needs$creenshot = null;
        }

        // Paint tape progress bar after capture (but skip in clean mode for neat tapes)
        if (paintTapeProgressAfterCapture && tapeProgressPainter && !window.currentRecordingOptions?.cleanMode) {
          tapeProgressPainter();
        }

        // 🎬 Paint recording UI overlays to recordingUICan (NOT captured in recordings)
        // These are UI elements that should be visible on screen but not in the tape
        const hasRecordingUIOverlays = Object.keys(recordingUIOverlays).length > 0;
        if (hasRecordingUIOverlays) {
          recordingUICtx.clearRect(0, 0, recordingUICan.width, recordingUICan.height);
          for (const name of Object.keys(recordingUIOverlays)) {
            recordingUIOverlays[name]();
          }
          if (recordingUICan.style.display === "none") {
            recordingUICan.style.display = "block";
          }
        } else {
          if (recordingUICan.style.display !== "none") {
            recordingUICan.style.display = "none";
          }
        }

        if (glaze.on) {
          ThreeD?.pasteTo(glazeCompositeCtx);
          const glazeSource = webglCompositeActive ? webglCompositeCanvas : canvas;
          glazeCompositeCtx.drawImage(glazeSource, 0, 0);
          Glaze.update(glazeComposite);
        }

        // TODO: Is this actually updating with a blank image at first? How to prevent the glaze.clear flicker? 2022.6.8
      }

      if (glaze.on) {
        Glaze.render(now, pen?.normalizedPosition(canvasRect));
      } else {
        Glaze.off();
        canvas.style.removeProperty("opacity");
      }

      // 🅱️ Draw anything from the system UI layer on top.

      const dpi = window.devicePixelRatio;

      // Only scale UI context when DPI changes
      if (!uiContextScaled || currentUiScale !== dpi) {
        if (uiContextScaled) {
          uiCtx.resetTransform();
        }
        uiCtx.scale(dpi, dpi);
        currentUiScale = dpi;
        uiContextScaled = true;
      }

      // Combine clear operations for better performance
      // Modified to avoid clearing overlay areas - label overlay is typically at (6,6) with small dimensions
      if (content.label) {
        // If there's a label overlay, clear around it instead of over it
        const labelX = content.label.x;
        const labelY = content.label.y;
        const labelW = content.label.img?.width || 0;
        const labelH = content.label.img?.height || 0;
        
        // Clear top-left area but avoid the label overlay region
        if (labelX > 0) {
          uiCtx.clearRect(0, 0, labelX, 64); // Left of label
        }
        if (labelY > 0) {
          uiCtx.clearRect(0, 0, 64, labelY); // Above label
        }
        if (labelX + labelW < 64) {
          uiCtx.clearRect(labelX + labelW, 0, 64 - (labelX + labelW), 64); // Right of label
        }
        if (labelY + labelH < 64) {
          uiCtx.clearRect(0, labelY + labelH, 64, 64 - (labelY + labelH)); // Below label
        }
      } else {
        // No label overlay, clear normally
        uiCtx.clearRect(0, 0, 64, 64); // Top left
      }
      
      uiCtx.clearRect(0, uiCtx.canvas.height / dpi - 64, 64, 64); // Bottom left
      uiCtx.clearRect(uiCtx.canvas.width / dpi - 64, 0, 64, 64); // Top right

      pen?.render(uiCtx, canvasRect); // ️ 🐭 Draw the cursor.

      // Show the spinner on any piece other than the first, and never
      // on the prompt.
      if (
        content.loading === true &&
        currentPiece !== null &&
        currentPiece !== "aesthetic.computer/disks/prompt"
      ) {
        UI.spinner(uiCtx, now);
      }

      if (debug && frameCached && content.loading !== true) UI.cached(uiCtx); // Pause icon.

      // Note: We keep the transform scaled for the next frame to avoid redundant operations
    }

    if (
      pixelsDidChange ||
      needs$creenshot ||
      mediaRecorder?.state === "recording" ||
      (pen && pen.changedInPiece) ||
      content.tapeProgressBar // Force draw when tape progress bar is active
    ) {
      frameCached = false;
      if (pen) pen.changedInPiece = false;
      draw();
    } else if (frameCached === false) {
      frameCached = true;
      draw();
      //console.log("Caching frame...");
      // } else if (content.loading === true && debug === true) {
    } else if (content.loading === true) {
      draw();
    } else if (frameCached === true) {
      //draw(); // TODO: This is causing stuttering.
      // console.log("Cached...");
    }

    // Hide the freezeFrame - wait for glaze to be ready if it's enabled
    // Also ensure resize has stabilized by waiting 100ms after last reframe request
    const reframeStabilized = performance.now() - lastReframeTime > 100;
    if (freezeFrame && freezeFrameFrozen && !awaitingReframePixels && reframeDrawn && reframeStabilized && (!glaze.on || glazeReady)) {
      if (glaze.on === false) {
        canvas.style.removeProperty("opacity");
      }
      // Fade out freeze frame and overlay smoothly then remove
      freezeFrameCan.style.opacity = "0";
      freezeOverlayCan.style.opacity = "0";
      setTimeout(() => {
        if (wrapper.contains(freezeFrameCan)) {
          freezeFrameCan.remove();
          freezeFrameCan.style.removeProperty("opacity");
        }
        if (wrapper.contains(freezeOverlayCan)) {
          freezeOverlayCan.remove();
          freezeOverlayCan.style.removeProperty("opacity");
        }
      }, 60); // Match CSS transition duration
      if (window.acReframeDebug) {
        console.log("✅ REFRAME: freeze frame removed");
      }
      freezeFrame = false;
      freezeFrameGlaze = false;
      freezeFrameFrozen = false;
    }

    if (glaze.on && glazeReady) {
      Glaze.unfreeze();
    } else if (!glaze.on) {
      canvas.style.removeProperty("opacity");
    }

    if (needsReappearance /* && wrapper.classList.contains("hidden")*/) {
      // wrapper.classList.remove("hidden");
      needsReappearance = false;
    }

    // 📸 PACK mode: Log frame to console every 5 seconds (time-based, framerate independent)
    // Skip entirely if snapshots are suppressed (bundles set KIDLISP_SUPPRESS_SNAPSHOT_LOGS = true)
    if (window.acPACK_MODE && window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS !== true && canvas) {
      const packNow = performance.now();
      const packTimeSinceLastSnap = window._lastPackSnapTime ? (packNow - window._lastPackSnapTime) : Infinity;
      if (packTimeSinceLastSnap >= 5000) { // 5 seconds
        window._lastPackSnapTime = packNow;
        try {
          const { dataUrl, displayWidth, displayHeight, dimensions } = captureFrame(canvas, {
            scaleFactor: 3,
            displayMax: 200
          });
          const ts = formatTimestamp();
          
          // Get piece code (use acPACK_PIECE for the short name like "roz")
          const pieceName = window.acPACK_PIECE || 'piece';
          const pieceCode = pieceName.startsWith('$') ? pieceName : `$${pieceName}`;
          
          console.log(
            `%c📸 ${pieceCode} %c@ ${ts} %c• Frame ${frameCount} %c[${dimensions.width}×${dimensions.height}]`,
            `color: #4ecdc4; font-weight: bold; font-size: 11px; font-family: monospace;`,
            `color: #f8b500; font-size: 10px; font-family: monospace;`,
            `color: #888; font-size: 10px; font-family: monospace;`,
            `color: #666; font-size: 10px; font-family: monospace;`
          );
          console.log(
            `%c `,
            `font-size: 1px; padding: ${displayHeight/2}px ${displayWidth/2}px; background: url("${dataUrl}") no-repeat center; background-size: ${displayWidth}px ${displayHeight}px; image-rendering: pixelated;`
          );
        } catch (e) {
          // Silently fail - frame capture is not essential
        }
      }
    }

    // 📸 KidLisp.com console: Auto-snap - initial at frame 30 after play, then every 5 seconds
    const consoleEnabled = isKidlispConsoleEnabled();
    const notPaused = !window.__acLoopPaused;
    if (consoleEnabled && canvas && notPaused) {
      const now = performance.now();
      const kidlispCode = window.__acCurrentKidlispCode;
      // Valid KidLisp code can be just a plain word (e.g., "gray", "red", "purple")
      // which implicitly wipes the screen with that color, so we only check for
      // non-empty content, not specific syntax like parentheses or commas.
      const hasKidlispCode = kidlispCode && kidlispCode.trim().length > 0;
      
      if (hasKidlispCode) {
        // Track when code changes - reset the initial snap tracking
        if (window._lastKidlispCodeForSnap !== kidlispCode) {
          window._lastKidlispCodeForSnap = kidlispCode;
          window._kidlispCodeStartFrame = frameCount;
          window._kidlispInitialSnapTaken = false;
        }
        
        const framesSinceCodeStart = typeof frameCount === "bigint" 
          ? Number(frameCount - window._kidlispCodeStartFrame)
          : frameCount - window._kidlispCodeStartFrame;
        
        // Initial snapshot at frame 30 after code starts
        const shouldTakeInitialSnap = !window._kidlispInitialSnapTaken && framesSinceCodeStart >= 30;
        
        // Subsequent snapshots every 5 seconds (only after initial snap)
        const timeSinceLastSnap = window._lastKidlispSnapTime ? (now - window._lastKidlispSnapTime) : Infinity;
        const shouldTakePeriodicSnap = window._kidlispInitialSnapTaken && timeSinceLastSnap >= 5000;
        
        if (shouldTakeInitialSnap || shouldTakePeriodicSnap) {
          if (shouldTakeInitialSnap) {
            window._kidlispInitialSnapTaken = true;
          }
          window._lastKidlispSnapTime = now;
          try {
            const { dataUrl, displayWidth, displayHeight, dimensions } = captureFrame(canvas, {
              scaleFactor: 3,
              displayMax: 200
            });
            const ts = formatTimestamp();
          
            // Get the source code and look up its cached $code identifier
            const embeddedSource = window.__acCurrentKidlispCode || null;
            // Prefer codeId passed from kidlisp.com, then fall back to getCachedCode lookup
            // This ensures the correct $code is used when loading from URL
            const passedCodeId = window.__acCurrentKidlispCodeId || null;
            const cachedCodeId = embeddedSource ? getCachedCode(embeddedSource) : null;
            const codeId = passedCodeId || cachedCodeId || null;
            // Get user handle if available (e.g., "jeffrey")
            const userHandle = window.acHANDLE ? `@${window.acHANDLE}` : null;
            
            // Build the piece label: $code if available, otherwise fall back to piece path
            const pieceLabel = codeId ? `$${codeId}` : (currentPiece?.split('/')?.pop() || 'piece');
            
            // Build filename like: $code-@handle-timestamp.png
            // Example: $nece-@jeffrey-2025.12.17.15.30.45.123.png
            // Using the standard timestamp() format from num.mjs
            const fileTs = timestamp();
            
            const filenameParts = [pieceLabel];
            if (userHandle) filenameParts.push(userHandle);
            filenameParts.push(fileTs);
            const filename = filenameParts.join('-') + '.png';
            
            const suppressSnapConsoleLogs = window.KIDLISP_SUPPRESS_SNAPSHOT_LOGS === true;
            
            // Log to browser console (optional)
            if (!suppressSnapConsoleLogs) {
              console.log(
                `%c📸 ${pieceLabel} %c@ ${ts} %c• Frame ${frameCount} %c[${dimensions.width}×${dimensions.height}]`,
                `color: #4ecdc4; font-weight: bold; font-size: 11px; font-family: monospace;`,
                `color: #f8b500; font-size: 10px; font-family: monospace;`,
                `color: #888; font-size: 10px; font-family: monospace;`,
                `color: #666; font-size: 10px; font-family: monospace;`
              );
              console.log(
                `%c `,
                `font-size: 1px; padding: ${displayHeight/2}px ${displayWidth/2}px; background: url("${dataUrl}") no-repeat center; background-size: ${displayWidth}px ${displayHeight}px; image-rendering: pixelated;`
              );
            }
            
            // Send to kidlisp.com console
            postKidlispConsoleImage(dataUrl, {
              frameCount: Number(frameCount),
              timestamp: ts,
              pieceCode: codeId, // The $code identifier (without $)
              pieceLabel,              // Full label with $ (e.g., "$nece")
              filename,                // Full filename (e.g., "$nece-@jeffrey-12-17-02-PM.png")
              userHandle,              // User handle (e.g., "@jeffrey")
              dimensions,
              embeddedSource
            });
          } catch (e) {
            // Silently fail - auto-snap is not essential
          }
        }
      }
    }

    } catch (err) {
      console.error("🛑 BIOS compositing error (render/update):", err);
    } finally {
      frameAlreadyRequested = false; // 🗨️ Signal readiness for the next frame.
      clearFrameStallWatchdog();
    }
    // if (lastRender) console.log(performance.now() - lastRender)
    // lastRender = performance.now()
  } // End of receivedChange function

  // 📤 Reads a file and uploads it to the server.
  async function receivedUpload(
    { filename, data, bucket },
    callbackMessage = "upload",
    metadata = null,
    recordingSlug = null,  // Optional recording slug for anonymous paintings
  ) {
    console.log("📤 Uploading file:", filename, typeof data || "...");
    const ext = extension(filename);
    let MIME = "application/octet-stream"; // Default content type.

    if (ext === "json") {
      // JSON
      MIME = "application/json";

      // Parse JSON strings if they haven't been already, including support for `bigints`.
      if (typeof data !== "string") {
        data = JSON.stringify(
          data,
          (k, v) => (typeof v === "bigint" ? v.toString() : v),
          // 2 // Also make sure we indent by 2 spaces so it's nicely formatted.
        );
      }
    }

    if (ext === "mjs") MIME = "application/javascript";
    if (ext === "lisp") MIME = "text/x-lisp";

    if (ext === "obj") MIME = "application/object";
    if (ext === "glb") MIME = "model/gltf-binary";

    if (ext === "mp4") {
      MIME = "video/mp4";
      // data = mediaRecorderBlob;
    }

    if (ext === "png") {
      MIME = "image/png";
      data = await bufferToBlob(data, MIME); // Could be adding modifiers here...
    }

    if (ext === "zip") MIME = "application/zip";

    let prefetchURL = "/presigned-upload-url/" + ext;

    if (bucket === "wand") prefetchURL += "/" + filename + "/" + bucket; // Add filename info.

    // if (bucket === undefined) prefetchURL += "/" + filename; // "art" bucket.
    // 📓 This is handled on the server if an empty bucket is sent.

    // Authorization: Check to see if we will use a user or a guest bucket.
    const headers = {};

    // If no bucket is specified, then try and use the "user" bucket.
    let userMedia = false,
      token;
    if (!bucket) {
      console.log(`🔐 receivedUpload: Calling authorize() for ${filename}`);
      token = await authorize();
      console.log(`🔐 receivedUpload: authorize() returned token=${token ? 'valid' : 'null/undefined'}`);
      if (token) {
        userMedia = true;
        bucket = "user";
        headers.Authorization = `Bearer ${token}`;
        // This filename gets sorted into the user bucket via their own
        // directory upon uploading.
        // Otherwise if there is no authorization, we just send an empty filename
        // slug with an extension and an identifier gets generated via nanoid on
        // the server.
        prefetchURL += "/" + filename + "/" + bucket; // Add filename info.
      } else {
        console.log(`🔓 receivedUpload: No token, uploading as guest`);
      }
    }

    function error(err) {
      send({
        type: callbackMessage,
        content: {
          result: "error",
          data: null,
        },
      });
    }

    // Now send a request to the server...
    fetch(prefetchURL, { headers })
      .then(async (res) => {
        const resData = await res.json();
        
        // Check for error response
        if (resData.error || !resData.uploadURL) {
          console.error("❌ Presigned URL error:", resData);
          throw new Error(resData.error || resData.details || "Failed to get upload URL");
        }

        const presignedUrl = resData.uploadURL;

        // Probably the download code... maybe something else if a custom
        // name is used.
        const url = new URL(presignedUrl);
        const filename = url.pathname.split("/").pop();
        const slug = filename.substring(0, filename.lastIndexOf("."));
        const path = url.pathname.slice(1); // Remove prepending "/";
        
        // Log clean URL without query params
        const cleanUrl = `${url.origin}${url.pathname}`;
        if (debug) console.log("🔐 Presigned URL:", cleanUrl);

        const xhr = new XMLHttpRequest();
  xhr.open("PUT", presignedUrl, true);
  // The presigned URL already encodes the required signed headers.
  // Set headers that match what's in the signature.
  
  // Set Content-Type based on extension to match server's presigned URL
  let mimeType;
  if (ext === "png") mimeType = "image/png";
  else if (ext === "zip") mimeType = "application/zip";
  else if (ext === "mjs") mimeType = "application/javascript; charset=utf-8";
  else if (ext === "lisp") mimeType = "text/x-lisp; charset=utf-8";
  else if (ext === "mp4") mimeType = "video/mp4";
  else if (ext === "json") mimeType = "application/json";
  else if (ext === "gltf") mimeType = "model/gltf+json";
  else if (ext === "glb") mimeType = "model/gltf-binary";
  else if (ext === "obj") mimeType = "application/object";
  
  if (mimeType) {
    xhr.setRequestHeader("Content-Type", mimeType);
  }
  
  xhr.setRequestHeader("Content-Disposition", "inline");
  
  // Include ACL header to match presigned URL signature (all files should be public-read)
  xhr.setRequestHeader("x-amz-acl", "public-read");

  // Create the blob without specifying a MIME so the browser typically won't
  // add a Content-Type header that wasn't included in the signature.
  const blob = new Blob([data]);

        xhr.upload.addEventListener("progress", (event) => {
          console.log(`Uploaded ${event.loaded} of ${blob.size} bytes...`);
          const uploadProgress = event.loaded / event.total;
          
          // Send generic upload progress
          send({
            type: "upload:progress",
            content: uploadProgress,
          });
          
          // For tape uploads (ZIP with metadata), also send as transcode-progress in 90-100% range
          if (ext === "zip" && metadata) {
            const transcodeProgress = 0.90 + (uploadProgress * 0.10); // Map 0-100% upload to 90-100% overall
            send({
              type: "recorder:transcode-progress",
              content: transcodeProgress,
            });
            console.log(`📤 Upload: ${Math.floor(uploadProgress * 100)}% -> Overall: ${Math.floor(transcodeProgress * 100)}%`);
          }
        });

        // Browser is online, send the request
        xhr.onerror = error;

        xhr.onreadystatechange = async function () {
          if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
            // Handle tape posting (ZIP files with metadata) - works for BOTH user and guest
            if (ext === "zip" && metadata) {
              if (debug) {
                console.log(
                  "📼 Adding tape to database:",
                  slug,
                  path,
                  ext,
                  metadata,
                );
              }

              const headers = {
                "Content-Type": "application/json",
              };
              
              // Add authorization if user is logged in
              if (userMedia && token) {
                headers.Authorization = `Bearer ${token}`;
              }

              const options = { method: "POST", headers };
              options.body = JSON.stringify({ slug, ext, metadata });
              
              try {
                console.log("📼 Calling api/track-tape with:", { slug, ext, metadata });
                const added = await fetch("api/track-tape", options);
                
                if (!added.ok) {
                  throw new Error(`HTTP ${added.status}: ${added.statusText}`);
                }
                
                const addedData = await added.json();
                if (debug) console.log("📼 Tape added to database...", addedData);
                
                // Extract code from response
                if (addedData.code) {
                  console.log(`📼 Tape code: !${addedData.code}`);
                  
                  // Send success callback with code
                  console.log(`📼 Sending ${callbackMessage} event with code:`, addedData.code);
                  send({
                    type: callbackMessage,
                    content: { 
                      result: "success", 
                      code: addedData.code,
                      slug: addedData.slug || slug,
                      url: url.toString(),
                      ext 
                    },
                  });
                  console.log(`📼 ${callbackMessage} event sent`);
                  
                  // Log clean URL without query params
                  const responseUrl = new URL(xhr.responseURL);
                  const cleanResponseUrl = `${responseUrl.origin}${responseUrl.pathname}`;
                  if (debug) console.log("✔️ Tape uploaded and posted:", cleanResponseUrl);
                  return; // Exit early for tape posting
                } else {
                  console.error("❌ No code in response from api/track-tape:", addedData);
                  send({
                    type: "tape:post-error",
                    content: { error: "No code returned from server" },
                  });
                  return;
                }
              } catch (err) {
                console.error("❌ Failed to add tape to database:", err);
                send({
                  type: "tape:post-error",
                  content: { error: err.message },
                });
                return;
              }
            }
            
            // Handle regular media (paintings, pieces, kidlisp - both authenticated and anonymous)
            if (
              ext === "png" ||  // All paintings (user and anonymous)
              ext === "mjs" ||  // Pieces
              ext === "lisp"    // KidLisp
            ) {
              // TODO: Go ahead and add this media to the database.
              if (debug) {
                console.log(
                  "🗞️ Adding media to the database:",
                  slug,
                  path,
                  ext,
                );
              }

              // TODO: Write an authorized POST request that contains the slug
              //       to "api/track-media"
              const headers = {
                "Content-Type": "application/json",
              };
              
              // Only add Authorization header if we have a valid token
              if (token) {
                headers.Authorization = `Bearer ${token}`;
              }

              const options = { method: "POST", headers };
              const body = { slug, ext };
              
              console.log(`🔍 PRE-CHECK: recordingSlug=${recordingSlug}, userMedia=${userMedia}, ext=${ext}`);
              
              // If this is an anonymous painting with a recording, send the combined slug
              console.log(`🔍 Checking combined slug: recordingSlug=${recordingSlug}, userMedia=${userMedia}, ext=${ext}`);
              if (recordingSlug && !userMedia && ext === "png") {
                body.slug = `${slug}:${recordingSlug}`;
                console.log(`🔗 Creating anonymous painting with combined slug: ${body.slug}`);
              } else {
                console.log(`⚠️  Not creating combined slug - one of these is false:`, {
                  hasRecordingSlug: !!recordingSlug,
                  isAnonymous: !userMedia,
                  isPNG: ext === "png"
                });
              }
              
              options.body = JSON.stringify(body);
              const added = await fetch("api/track-media", options);
              
              // Check for HTTP errors
              if (!added.ok) {
                console.error(`❌ track-media HTTP error: ${added.status} ${added.statusText}`);
              }
              
              const addedData = await added.json();
              console.log("🗞️ track-media response:", addedData, "status:", added.status);
              
              // Create data object first
              let data = { slug, url: url.toString(), ext };
              
              // Extract code from response if present
              if (addedData.code) {
                console.log(`🎨 Painting code: #${addedData.code}`);
                data.code = addedData.code; // Add code to return data
              } else {
                console.error("❌ No code received from track-media! Response:", addedData);
              }

              if (!userMedia && (ext === "mjs" || ext === "lisp")) {
                data.url =
                  "https://art.aesthetic.computer/" + data.slug + "." + data.ext;
              }

              send({
                type: callbackMessage,
                content: { result: "success", data },
              });
            } else {
              // For non-painting files or when not logged in
              let data = { slug, url: url.toString(), ext };

              if (!userMedia && (ext === "mjs" || ext === "lisp")) {
                data.url =
                  "https://art.aesthetic.computer/" + data.slug + "." + data.ext;
              }

              send({
                type: callbackMessage,
                content: { result: "success", data },
              });
            }

            if (debug) console.log("✔️ File uploaded:", xhr.responseURL);
          }
        };

        try {
          xhr.send(blob, { type: MIME });
        } catch (err) {
          error(err);
        }
      })
      .catch((err) => {
        if (debug) console.log("⚠️ Failed to get presigned URL:", err);
        error(err);
      });
  }

  // Request and open local file from the user.
  // TODO: Only supports images for now.
  // TODO: Make sure this works on mobile platforms.
  async function openFile() {
    pen?.up(); // Synthesize a pen `up` event so it doesn't stick
    //            due to the modal.
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "image/*";
    input.style.position = "absolute";
    input.style.left = "-9999px"; // Position off-screen

    return new Promise((resolve, reject) => {
      // Simulate click event on a visible element
      const button = document.createElement("button");
      button.style.opacity = 0;
      button.onclick = () => {
        input.click();
        document.body.removeChild(button);
      };

      document.body.appendChild(button);
      button.click();

      input.onchange = () => {
        const file = input.files[0];
        if (!file) {
          reject("No file was selected!");
        } else if (!file.type.startsWith("image/")) {
          reject("Selected file is not an image.");
        } else {
          const reader = new FileReader();

          reader.onload = async () => {
            const blob = new Blob([reader.result], { type: file.type });
            resolve(await toBitmap(blob));
          };
          reader.onerror = (error) => {
            reject(error);
          };
          reader.readAsArrayBuffer(file);
        }
      };
    });
  }

  async function authorize() {
    // Skip authorization in SPIDER mode (read-only IPFS environment)
    if (window.acSPIDER) {
      return null;
    }
    
    // Skip authorization if noauth parameter is present (e.g., for kidlisp.com editor)
    const urlParams = new URLSearchParams(window.location.search);
    console.log(`🔐 authorize() called. Search: ${window.location.search}, noauth=${urlParams.has('noauth')}, acNOAUTH=${window.acNOAUTH}`);
    if (urlParams.has('noauth') || window.acNOAUTH) {
      console.log('🔕 Skipping auth (noauth parameter present)');
      return null;
    }
    
    let token;
    try {
      token = window.acTOKEN;
      console.log(`🔐 authorize(): window.acTOKEN=${token ? token.substring(0, 20) + '...' : 'undefined'}`);

      if (token) {
        // console.log("🔐 Hosted token found...");

        try {
          // Attempt to fetch user info using the token
          window.auth0Client.token = token;
          await window.auth0Client.getUser();
          console.log("✅🔐 Token validation succeeded!");
        } catch (error) {
          console.error("🔴🔐 Token is invalid or expired - clearing token");
          // Clear the invalid token to prevent infinite loop
          window.acTOKEN = null;
          // Trigger logout to clear the invalid session
          if (window.parent) {
            window.parent.postMessage({ type: "logout" }, "*");
          }
          // Return null - don't reload, just continue without auth
          return null;
        }
      } else {
        // If acTOKEN is not available, get a new one
        console.log("🔐 No acTOKEN, calling getTokenSilently...");
        token = await window.auth0Client.getTokenSilently();
        console.log(`🔐 getTokenSilently returned: ${token ? 'token received' : 'null'}`);
        // await window.auth0Client.getUser();
        // console.log("✅ Token is valid");
      }

      // console.log("🔐 Authorized");
    } catch (err) {
      // console.log("🔐️ ❌ Unauthorized", err);
      token = null; // Ensure token is null on any error
    }
    return token;
  }

  // Reads the extension off of filename to determine the mimetype and then
  // handles the data accordingly and downloads the file in the browser.
  // Downloads both cached files via `data` and network stored files for
  // users and guests.
  async function receivedDownload({ filename, data, modifiers }) {
    console.log("💾 📥 receivedDownload called!");
    console.log("💾 📥 - filename:", filename);
    console.log("💾 📥 - data type:", typeof data);
    console.log("💾 📥 - data instanceof Blob:", data instanceof Blob);
    if (data instanceof Blob) {
      console.log("💾 📥 - blob size:", data.size);
      console.log("💾 📥 - blob type:", data.type);
    }
    console.log("💾 📥 - modifiers:", modifiers);
    
    console.log("💾 Downloading:", filename);
    // if (data) console.log("Data:", typeof data);
    // if (modifiers.sharing === true) presharingFile = true;

    let object, blob;
    let MIME = "application/octet-stream"; // Default content type.
    const ext = extension(filename);

    if (ext === "glb") {
      MIME = "model/gltf+binary";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (ext === "json" || ext === "gltf") {
      // ✍️ Text + 3D
      // JSON
      MIME = "application/json";
      // GLTF
      if (extension(filename === "gltf")) MIME = "model/gltf+json"; // Hacky conditional above...

      // Parse JSON strings if they haven't been already, including support for `bigints`.
      if (typeof data !== "string") {
        data = JSON.stringify(
          data,
          (k, v) => (typeof v === "bigint" ? v.toString() : v),
          // 2 // Also make sure we indent by 2 spaces so it's nicely formatted.
        );
      }
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (ext === "png" || ext === "webp" || ext === "gif") {
      // 🖼️ Images
      MIME = "image/png"; // PNG

      if (extension(filename) === "webp") {
        MIME = "image/webp";
      } else if (extension(filename) === "gif") {
        MIME = "image/gif";
      }

      if (data) {
        // Download locally if data is provided.
        if (data instanceof Blob) {
          // If data is already a blob, use it directly
          blob = data;
          object = URL.createObjectURL(blob);
        } else {
          // Otherwise, convert pixel data to blob
          blob = await bufferToBlob(data, MIME, modifiers);
          object = URL.createObjectURL(blob, { type: MIME });
        }
      } else {
        // Or from the storage network.
        // Check to see if filename has user handle data.
        const hasEmailOrHandle = filename.split("/")[0].indexOf("@") > -1;
        if (hasEmailOrHandle) {
          // Apply origin-aware URL construction for media files
          try {
            const isDevelopment = location.hostname === 'localhost' && location.port;
            if (isDevelopment) {
              object = `https://localhost:${location.port}/media/${filename}`;
              if (debug) console.log("🖼️ Media URL (dev):", { filename, object, hostname: location.hostname, port: location.port });
            } else {
              object = `/media/${filename}`;
              if (debug) console.log("🖼️ Media URL (prod):", { filename, object });
            }
          } catch (err) {
            // Fallback if there's any error
            object = `/media/${filename}`;
            console.warn("🖼️ Media URL fallback:", { filename, object, err });
          }
        } else {
          object = `https://art.aesthetic.computer/${filename}`;
        }
      }
    } else if (ext === "mp4" || ext === "webm") {
      // TODO: ⚠️ `webm` could eventually mean audio here...
      // 🎥 Video
      // Use stored data from the global Media Recorder.
      let tape;
      let tapeData;
      
      console.log("💾 🎥 Processing video download:", { ext, filename, dataIsBlobAlready: data instanceof Blob });
      
      if (data instanceof Blob) {
        // If data is already a blob (from video export), use it directly
        tape = data;
        tapeData = null;
        console.log("💾 🎥 Using blob data directly:", { size: tape.size, type: tape.type });
      } else {
        // Otherwise get from storage
        tapeData = data || (await Store.get("tape"));
        tape = tapeData?.blob;
        console.log("💾 🎥 Retrieved from storage:", { 
          hasTapeData: !!tapeData, 
          hasTape: !!tape, 
          tapeSize: tape?.size,
          tapeType: tape?.type 
        });
      }

      // Restore frame data if available for WebP/Frame exports
      if (tapeData?.frames && recordedFrames.length === 0) {
        recordedFrames.length = 0; // Clear existing
        recordedFrames.push(...tapeData.frames);
        console.log(
          "📼 Restored",
          recordedFrames.length,
          "frames from cached video",
        );
        // Debug: Check what the loaded frames look like
        if (recordedFrames.length > 0) {
          const firstFrame = recordedFrames[0];
          console.log("🔍 First loaded frame:", {
            structure: Array.isArray(firstFrame) ? `Array(${firstFrame.length})` : typeof firstFrame,
            timestamp: Array.isArray(firstFrame) ? firstFrame[0] : 'N/A',
            timestampType: Array.isArray(firstFrame) ? typeof firstFrame[0] : 'N/A',
            hasImageData: Array.isArray(firstFrame) && firstFrame[1] && typeof firstFrame[1] === 'object'
          });
        }
      }

      // Use stored filename if available, otherwise fall back to provided filename
      if (tapeData?.filename) {
        filename = tapeData.filename;
      } else {
        // 🫲 Make sure the container matches the extension.
        const tapeMIME = tape?.type; // Check the tape's blob's type.
        if (tapeMIME?.indexOf("webm") > -1) {
          filename = filename.replace(".mp4", ".webm"); // Replaces ".mp4" set from `video`.
        } else {
          filename = filename.replace(".webm", ".mp4");
        }
      }

      if (tape) {
        object = URL.createObjectURL(tape);
        console.log("💾 🎥 Created object URL for video:", { 
          objectUrlCreated: !!object,
          tapeSize: tape.size,
          tapeType: tape.type,
          finalFilename: filename
        });
      } else {
        console.warn("💾 🎥 No tape blob available! This will cause download issues.");
        // console.warn(
        //   "🕸️ No local video available... Trying art bucket:",
        //   filename,
        // );
        // object = `https://art.aesthetic.computer/${filename}`;
      }
    } else if (ext === "mjs") {
      MIME = "application/javascript; charset=utf-8";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (ext === "html" || ext === "htm") {
      // 🌐 HTML files
      MIME = "text/html; charset=utf-8";
      object = URL.createObjectURL(new Blob([data], { type: MIME }));
    } else if (ext === "zip") {
      MIME = "application/zip";
      object = URL.createObjectURL(data, { type: MIME });
    }

    // Fetch download url from `/presigned-download-url?for=${filename}` if we
    // don't already have a blob string.

    if (object && !object.startsWith("blob:")) {
      console.log("💾 🌐 Fetching presigned download URL for:", filename);
      console.log("💾 🌐 Current object URL:", object);
      try {
        const response = await fetch(`/presigned-download-url?for=${filename}`);
        const json = await response.json();
        console.log("💾 🌐 Presigned URL response:", json);
        if (json.url && json.url !== "example.com") {
          object = json.url;
        } else {
          console.warn("💾 🌐 Invalid presigned URL received, keeping original:", json);
        }
      } catch (err) {
        console.warn("💾 🌐 Presigned URL fetch failed, keeping original object:", err);
      }
    }

    // Check if navigator.share is supported and we are either on
    // iOS or Android
    // console.log("🧑‍🤝‍🧑 Sharing:", modifiers?.sharing, "Capable:", navigator.share);
    if ((iOS || Android) && modifiers?.sharing === true && navigator.share) {
      shareFile = new File(
        [blob || new Blob([data], { type: MIME })],
        filename.split("/").pop(),
        { type: MIME, lastModified: new Date().getTime() },
      );
      shareFileCallback?.(); // Run the callback if necessary, which should
      // prevent any special race conditions.
    } else {
      // Fallback to download if navigator.share is not supported
      const a = document.createElement("a");
      a.href = object;
      a.target = "_blank";
      a.download = filename.split("/").pop(); // Remove any extra paths.

      // Add the link to DOM for better browser compatibility with large files
      document.body.appendChild(a);

      console.log(
        `💾 Triggering download: ${filename.split("/").pop()} (${blob ? `${Math.round((blob.size / 1024 / 1024) * 100) / 100} MB` : "unknown size"})`,
      );

      // Safari-specific handling
      const isSafari = /^((?!chrome|android).)*safari/i.test(navigator.userAgent);
      if (isSafari && (ext === "mp4" || ext === "webm")) {
        console.log("🍎 Safari: Using enhanced video download handling");
        
        // Force user interaction for Safari
        a.style.display = 'none';
        a.rel = 'noopener';
        
        // Try click with user gesture
        try {
          a.click();
        } catch (e) {
          console.log("🍎 Safari: Standard click failed, trying workaround");
          // Fallback: open in new tab for manual save
          window.open(object, '_blank');
        }
      } else {
        a.click();
      }

      // Clean up after a delay to ensure download starts
      setTimeout(() => {
        document.body.removeChild(a);
        if (typeof a.href !== "string") URL.revokeObjectURL(a.href);
      }, 1000);
    }

    // Picture in Picture: Image Download UI? 22.11.24.08.51
    //const container = document.createElement('div');
    //const iframe = document.createElement('iframe');

    //container.id = "pip-wrapper";
    //iframe.id = "pip";
    //iframe.src = "/blank";

    //container.append(iframe);
    //wrapper.append(container);
  }

  // Used above in `receivedUpload` and `receivedDownload` to generate image files.

  // Add a crop to square modifier.

  async function bufferToBlob(data, MIME = "image/png", modifiers) {
    // 🔐 Guard: In sandboxed iframes (opaque origin), toBlob/toDataURL will fail
    // Return null early to prevent SecurityError exceptions
    if (isOpaqueOrigin) {
      console.warn('⚠️ bufferToBlob skipped: canvas export blocked in sandboxed iframe');
      return null;
    }

    let can;
    // Encode a pixel buffer as a png.
    // See also: https://stackoverflow.com/questions/11112321/how-to-save-canvas-as-png-image

    const imageData = data.data
      ? data
      : new ImageData(data.pixels, data.width, data.height);
    // Convert to imageData if it isn't already.

    can = document.createElement("canvas");
    const ctx = can.getContext("2d");

    if (modifiers?.cropToScreen) {
      can.width = screen.width;
      can.height = screen.height;
    } else {
      can.width = imageData.width;
      can.height = imageData.height;
    }

    if (modifiers?.crop === "square") {
      // debugger;
    }

    ctx.putImageData(imageData, 0, 0);

    // Scale or modify the image as needed.
    if ((modifiers?.scale !== 1 && modifiers?.scale > 0) || modifiers?.flipY) {
      const scale = modifiers?.scale || 1;
      const flipY = modifiers?.flipY;
      const can2 = document.createElement("canvas");
      const ctx2 = can2.getContext("2d");
      can2.width = can.width * scale;
      can2.height = can.height * scale;
      ctx2.imageSmoothingEnabled = false;
      if (flipY) {
        ctx2.scale(1, -1);
        ctx2.drawImage(can, 0, 0, can2.width, -can2.height);
      } else {
        ctx2.drawImage(can, 0, 0, can2.width, can2.height);
      }
      can = can2;
    }

    const blob = await new Promise((resolve) => {
      if (modifiers?.dataURL === true) {
        resolve(can.toDataURL(MIME));
      } else {
        can.toBlob(resolve, MIME, 100);
      }
    });
    return blob;
  }

  // Opens a file chooser that is filtered by a given extension / mimetype list.
  // And sends the text contents of an individual file back to the disk.
  function receivedImport(type) {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = type;

    input.onchange = (e) => {
      // Grab the only selected file in the file input.
      const file = e.target.files[0];

      // Does type match nothing in the comma separated `input.accept` list?
      const noMatch = type.split(",").every((t) => {
        return t !== file.type && t !== `.${extension(file.name)}`;
      });

      // Relay error if chosen file does not match the `input.accept` list.
      if (noMatch) {
        send({
          type: "import",
          content: {
            result: "error",
            data: `Chosen file was not of type "${type}"`,
          },
        });
        return;
      }

      // Read the file.
      const reader = new FileReader();
      reader.readAsText(file);

      // Send the content back to the disk once the file loads.
      reader.onload = (e) => {
        send({
          type: "import",
          content: { result: "success", data: e.target.result },
        });
      };

      // Relay an error if the file fails to load for any reason.
      reader.onerror = () => {
        send({
          type: "import",
          content: { result: "error", data: reader.error },
        });
      };
    };

    input.click();
  }

  // Connects the Microphone to the current audioContext.
  function receivedMicrophone(data = {}) {
    if (data.detach) {
      detachMicrophone?.();
    } else {
      attachMicrophone?.(data);
    }
  }

  // Takes a request for a video and then either uses a media query (for a camera)
  // or loads a video file from a given url (unimplemented).

  // Then it puts that into a new video tag and starts playing it,
  // sending the disk the thread frames as they update (optional).

  // This module also is used to pull data from frames for
  // features like hand-tracking.
  let videoResize; // Holds a function defined after initialization.
  let handAPI;
  //let handLandmarker, HandLandmarker, FilesetResolver, vision;
  async function receivedVideo({ type, options }) {
    // if (debug) console.log("🎥 Type:", type, options);

    if (type === "camera:update") videoResize?.(options);

    if (type === "camera") {
      // TODO: Give video and canvas a unique identifier that
      //       will create a link in the worker so that frame updates
      //       for multiple videos can be routed simultaneously.
      const video = document.createElement("video");

      // Camera properties.
      let facingMode = options.facing || "user",
        zoom = 1;
      
      // Fit mode: "cover" crops to fill, "contain" letterboxes to fit
      const fitMode = options.fit || "cover";

      video.id = "camera-feed";
      video.autoplay = true; // Allow video footage to play automatically.
      video.setAttribute("playsinline", ""); // Only for iOS.
      video.setAttribute("muted", ""); // Don't include audio with video.

      const hands = options.hands === true; // Hand-tracking globals.
      let handVideoTime = -1;
      const useLegacyHandsAPI = true; // Performance of both libraries is
      //                                 equivalent on iPhone 14 Pro but vastly
      //                                 different on iPhone 13 Pro. 23.05.12.14.23

      const buffer = document.createElement("canvas");
      let animationRequest;

      function getAnimationRequest() {
        return animationRequest;
      }

      videos.push({ video, buffer, getAnimationRequest });

      buffer.width = options.width || 1280;
      buffer.height = options.height || 720;

      const bufferCtx = buffer.getContext("2d", { willReadFrequently: true });

      wrapper.appendChild(video);

      video.style = `position: absolute;
                     top: 0;
                     left: 0;
                     opacity: 0;
                     transform: scaleX(${facingMode === "user" ? -1 : 1});
                     width: 100%;`;

      buffer.style = `position: absolute;
                      opacity: 0;`;

      let settings, stream, videoTrack;
      let facingModeChange = false;

      try {
        // Grab video from the user using a requested width and height based
        // on the frame size.
        let cWidth = options.width,
          cHeight = options.height;

        async function getDevice(facingModeChoice) {
          // Swap width and height on iOS. (Implementation default differences.)
          // Set a height / width aspect ratio on iOS because
          // of implementation differences.

          // console.log("Trying: Width:", cWidth, "Height:", cHeight);

          const constraints = {
            facingMode: facingModeChoice,
            frameRate: { ideal: 30 },
          };

          // Calculate target aspect ratio for better matching
          const targetAR = cWidth / cHeight;

          if (
            (iOS || Android) &&
            window.matchMedia("(orientation: portrait)").matches &&
            (facingModeChoice === "environment" || facingModeChoice === "user")
            // &&
            // firstVideo
          ) {
            const temp = cWidth;
            cWidth = cHeight;
            cHeight = temp;
            // firstVideo = false;
          }

          // alert(cWidth + " " + cHeight);

          // Request ideal dimensions with aspect ratio hint
          constraints.width = { ideal: cWidth };
          constraints.height = { ideal: cHeight };
          
          // Add aspect ratio constraint if supported (helps get better match)
          if (targetAR > 0 && isFinite(targetAR)) {
            constraints.aspectRatio = { ideal: targetAR };
          }

          stream = await navigator.mediaDevices.getUserMedia({
            video: { ...constraints },
            audio: false,
          });

          video.srcObject = stream;
          videoTrack = stream.getVideoTracks()[0];
          // const capabilities = videoTrack.getCapabilities();
          settings = videoTrack.getSettings();

          // console.log(
          //   "Got: Width:",
          //   settings.width,
          //   "Height:",
          //   settings.height,
          // ); // ❤️‍🔥

          // Update global facingMode in case different from requested.
          facingMode = videoTrack.getConstraints().facingMode;

          const devices = await navigator.mediaDevices.enumerateDevices();
          const videoDevices = devices.filter(
            (device) => device.kind === "videoinput",
          );

          send({ type: "video-devices", content: videoDevices.length });

          if (debug) {
            videoDevices.forEach((device, index) => {
              if (index === 0) {
                console.log(
                  `Camera ${index + 1} (usually environment):`,
                  device.label,
                  device,
                );
              } else if (index === 1) {
                console.log(`Camera ${index + 1} (usually user):`, device);
              } else {
                console.log(`Camera ${index + 1} (additional camera):`, device);
              }
            });
          }
        }

        await getDevice(facingMode);

        video.addEventListener(
          "loadedmetadata",
          () => {
            video.play();
            if (debug)
              console.log("🎥 Resolution:", buffer.width, buffer.height);
          },
          { once: true },
        );

        // Resizing the video after creation. (Window resize or device rotate.)
        videoResize = async function ({ width, height, facing }) {
          cancelAnimationFrame(getAnimationRequest());

          try {
            const sizeChange = !isNaN(width) && !isNaN(height);

            if (sizeChange) {
              video.addEventListener(
                "loadedmetadata",
                () => {
                  buffer.width = cWidth;
                  buffer.height = cHeight;
                  process();
                  send({ type: "camera:updated" });
                  if (debug)
                    console.log("🎥 Resolution:", buffer.width, buffer.height);
                },
                { once: true },
              );

              if (iOS || Android) {
                await getDevice(facing);
              } else {
                video.srcObject = null; // Refresh the video `srcObject`.
                await videoTrack.applyConstraints({
                  width: { ideal: cWidth },
                  height: { ideal: cHeight },
                });
              }
            }

            if (settings.facingMode !== facing) {
              facingModeChange = true;
              await getDevice(facing);
              facingModeChange = false;

              video.addEventListener(
                "canplay",
                () => {
                  process();
                  send({ type: "camera:updated", content: facingMode });
                },
                { once: true },
              );
            } else {
            }

            // video.srcObject = stream;
            // if (!sizeChange && !facingModeChange) process();
          } catch (error) {
            process();
            if (debug) console.warn("🎥 Resolution update failed.", error);
          }
        };

        // ✋ Optional Hand-tracking (only load once)
        if (hands === true && !handAPI) {
          if (useLegacyHandsAPI) {
            // Load older mediapipe lib.
            const script = document.createElement("script");
            script.src = "/aesthetic.computer/dep/@mediapipe/hands/hands.js";
            script.crossOrigin = "anonymous";

            script.onload = function () {
              const config = {
                locateFile: (file) => {
                  return `aesthetic.computer/dep/@mediapipe/hands/${file}`;
                },
              };

              handAPI = { hands: new Hands(config) }; // Globally def. handAPI.
              window.handAPI = handAPI; // For production debugging.

              handAPI.hands.setOptions({
                selfieMode: false,
                maxNumHands: 1,
                modelComplexity: 0,
                minDetectionConfidence: 0.5,
                minTrackingConfidence: 0.5,
              });

              handAPI.hands.onResults((data) => {
                diagram({
                  screen: data.multiHandLandmarks[0] || [],
                  world: data.multiHandWorldLandmarks[0] || [],
                  hand: data.multiHandedness[0]?.label.toLowerCase() || "none",
                });
              });
            };

            document.head.appendChild(script);
          } else {
            if (!handAPI.HandLandmarker) {
              const { HandLandmarker, FilesetResolver } = await import(
                "/aesthetic.computer/dep/@mediapipe/tasks-vision/vision_bundle.js"
              );

              const vision = await FilesetResolver.forVisionTasks(
                "/aesthetic.computer/dep/@mediapipe/tasks-vision/wasm",
              );

              handAPI.HandLandmarker = HandLandmarker;
              handAPI.vision = vision;
            }

            if (!handAPI.hl) {
              handAPI.hl = await handAPI.HandLandmarker.createFromOptions(
                handAPI.vision,
                {
                  baseOptions: {
                    modelAssetPath: "../models/hand_landmarker.task",
                    delegate: "GPU", // or "CPU"
                  },
                  canvas: document.createElement("canvas"),
                  runningMode: "VIDEO",
                  //runningMode: "LIVE_STREAM",
                  minHandDetectionConfidence: 0.25,
                  minHandPresenceConfidence: 0.25,
                  minTrackingConfidence: 0.25,
                  numHands: 1,
                },
              );
            }
          }
        }

        process(); // Start processing data.
      } catch (err) {
        send({ type: "camera:denied" });
        console.log(err);
      }

      function diagram(hand) {
        if (facingMode === "user") {
          // hand.screen.forEach((l) => (l.x = 1 - l.x));
          // Reverse handedness because our data is mirrored.
          // if (hand.handedness === "left") {
          //   hand.handedness = "right";
          // } else if (hand.handedness === "right") {
          //   hand.handedness = "left";
          // }
        }
        handData = hand;
      }

      function process() {
        cancelAnimationFrame(getAnimationRequest());
        if (facingModeChange) return;
        // cancelAnimationFrame(getAnimationRequest());
        // TODO: Video effects / filter kernels could be added here...
        // 💡 For GPU backed visuals. 23.04.29.20.47

        // Send frames by default.
        if (facingMode === "user" || (!iOS && !Android)) {
          bufferCtx.translate(buffer.width / 2, buffer.height / 2);
          const zoom = 1;
          // if (hands) {
          // bufferCtx.scale(zoom, zoom);
          // } else {
          bufferCtx.scale(-zoom, zoom);
          // }
          bufferCtx.translate(-buffer.width / 2, -buffer.height / 2);
        }

        // 🤚 Track Hands on the GPU if flagged.
        if (hands === true && handAPI) {
          if (handVideoTime !== video.currentTime && video.videoWidth > 0) {
            handVideoTime = video.currentTime;
            if (useLegacyHandsAPI && !handAPI?.legacyProcessing) {
              handAPI.hands?.send({ image: bufferCtx.canvas }).then(() => {
                handAPI.legacyProcessing = false;
                // Don't process more than one frame at a time.
              });
              handAPI.legacyProcessing = true;
            } else {
              // const data = handAPI.hl?.detectForVideo(video, handVideoTime);
              // TODO: This will no longer work. 23.5.24
              //       Check the other `diagram` call.
              // diagram(data?.landmarks[0] || []);
            }
            // send({type: "hand-tracking-data", content: landmarks});
          }
        }

        // Drawing a video frame to the buffer (mirrored, proportion adjusted).
        const videoAR = video.videoWidth / video.videoHeight;
        const bufferAR = buffer.width / buffer.height;
        let outWidth,
          outHeight,
          outX = 0,
          outY = 0;

        if (fitMode === "contain") {
          // "contain" mode: letterbox to fit entire video in buffer
          if (videoAR >= bufferAR) {
            // Video is wider - fit to width, letterbox top/bottom
            outWidth = buffer.width;
            outHeight = outWidth / videoAR;
          } else {
            // Video is taller - fit to height, letterbox left/right
            outHeight = buffer.height;
            outWidth = outHeight * videoAR;
          }
        } else {
          // "cover" mode (default): crop to fill buffer completely
          if (videoAR <= bufferAR) {
            // Tall to wide.
            outWidth = buffer.width;
            outHeight = outWidth / videoAR;
          } else {
            // Wide to tall.
            outHeight = buffer.height;
            outWidth = outHeight * videoAR;
          }
        }

        outY = (buffer.height - outHeight) / 2; // Adjusting position.
        outX = (buffer.width - outWidth) / 2;

        bufferCtx.drawImage(video, outX, outY, outWidth, outHeight);
        bufferCtx.resetTransform();

        if (options.hidden !== true) {
          const pixels = bufferCtx.getImageData(
            0,
            0,
            buffer.width,
            buffer.height,
          );

          send(
            {
              type: "video-frame",
              content: {
                width: pixels.width,
                height: pixels.height,
                pixels: pixels.data,
              },
            },
            [pixels.data.buffer],
          );
        }

        animationRequest = requestAnimationFrame(process);
      }
    }
  }

  // Pointer Lock 🔫
  let pointerLockCursor = null;
  
  // Create the pointer lock cursor element
  function createPointerLockCursor() {
    // Remove any existing cursor first
    const existing = document.getElementById("pointer-lock-cursor");
    if (existing) existing.remove();
    
    const cursor = document.createElement("div");
    cursor.id = "pointer-lock-cursor";
    cursor.innerHTML = `
      <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 25 25">
        <path d="
            M 13,3 L 13,6 
            M 13,20 L 13,23 
            M 6,13 L 3,13 
            M 20,13 L 23,13
        " stroke="black" stroke-width="4" stroke-linecap="round"/>
        <circle cx="13" cy="13" r="2" fill="black" />
        <path d="
            M 12,2 L 12,5 
            M 12,19 L 12,22 
            M 5,12 L 2,12 
            M 19,12 L 22,12
        " stroke="#00FFFF" stroke-width="4" stroke-linecap="round"/>
        <circle cx="12" cy="12" r="2" fill="#ffffff" />
      </svg>
    `;
    
    // Use setAttribute for better compatibility
    cursor.setAttribute("style", `
      position: fixed !important;
      top: 50% !important;
      left: 50% !important;
      width: 24px !important;
      height: 24px !important;
      transform: translate(-50%, -50%) !important;
      pointer-events: none !important;
      z-index: 999999 !important;
      display: none !important;
      user-select: none !important;
      -webkit-user-select: none !important;
    `.replace(/\s+/g, ' ').trim());
    
    document.body.appendChild(cursor);
    // console.log("🎯 Pointer lock cursor element created with inline SVG", cursor);
    // console.log("🎯 Cursor element styles:", cursor.getAttribute("style"));
    // console.log("🎯 Document body children count:", document.body.children.length);
    return cursor;
  }
  
  document.addEventListener("pointerlockchange", () => {
    const isLocked = document.pointerLockElement === wrapper;
    // console.log("🔒 Pointer lock change:", isLocked);
    // console.log("🔒 Pointer lock element:", document.pointerLockElement);
    // console.log("🔒 Wrapper element:", wrapper);
    
    // Create cursor element if it doesn't exist
    if (!pointerLockCursor) {
      pointerLockCursor = createPointerLockCursor();
    }
    
    // Show/hide the cursor based on pointer lock state
    if (isLocked) {
      pointerLockCursor.style.setProperty("display", "block", "important");
      // console.log("🎯 Showing pointer lock cursor");
      // console.log("🎯 Cursor element display:", pointerLockCursor.style.display);
      // console.log("🎯 Cursor element in DOM:", document.getElementById("pointer-lock-cursor"));
    } else {
      pointerLockCursor.style.setProperty("display", "none", "important");
      // console.log("🎯 Hiding pointer lock cursor");
    }
    
    send({
      type: isLocked ? "pen:locked" : "pen:unlocked",
    });
  });

  // document.addEventListener("pointerlockerror", () => {
  // console.error("Pointer lock failed!");
  //});

  // Window Scroll 📜
  window.addEventListener("wheel", function (event) {
    send({
      type: "scroll",
      content: {
        x: event.deltaX / subdivisions,
        y: event.deltaY / subdivisions,
      },
    });
  });

  // Window Focus
  window.addEventListener("focus", function (e) {
    send({ type: "focus-change", content: true });
  });

  // Window Blur
  window.addEventListener("blur", function (e) {
    send({ type: "focus-change", content: false });
  });

  // Window Visibility
  document.addEventListener("visibilitychange", function () {
    if (!document.hidden) wrapper.classList.remove("reloading");
    // if (document.hidden) mediaRecorder?.stop();
    send({
      type: "visibility-change",
      content: !document.hidden,
    });
  });

  // 🚨 Signal (Used to pass messages via window... important for embedded HTML
  //           `content` used within pieces that needs communication with the
  //           main system)

  // Send signals to the running piece.
  window.signal = function (message) {
    if (debug) console.log("🚨 Signal sent:", message);
    send({
      type: "signal",
      content: message,
    });
  };

  // Receive signals from the piece & assign callbacks.
  // These get flushed between pieces.
  // Note: These are useful for
  window.when = function (message, callback) {
    whens[message] = callback;
  };

  // 📚 History
  // TODO: Extract all the history features into a class of some kind?
  // TODO: Eventually add an API so that a disk can list all the history of
  //       a user's session. This could also be used for autocompletion of
  //       pieces / up + down arrow prev-next etc.

  // Track labelBack state in main thread (persists across worker reloads)
  let mainThreadLabelBack =
    window.safeSessionStorageGet("aesthetic-labelBack") === "true";

  window.onpopstate = function (e) {
    if (
      document.location.hash === "#debug" ||
      document.location.hash === "#nodebug"
    ) {
      document.location.reload();
    }

    const sluggy = slug(document.location.href);
    if (sluggy === "prompt") keyboard?.input.focus();

    const parsed = parse(sluggy || window.acSTARTING_PIECE);

    // Restore labelBack state for history navigation
    if (mainThreadLabelBack) {
      console.log("🔗 Main thread restoring labelBack for history navigation");
      parsed.labelBack = true;
      // Clear the state after using it for navigation
      mainThreadLabelBack = false;
      window.safeSessionStorageRemove("aesthetic-labelBack");
      console.log(
        "🔗 Main thread: Cleared labelBack after using it for history navigation",
      );
    }

    send({
      type: "history-load",
      content: parsed,
    });
  };

  // Fullscreen
  // Note: This doesn't work in Safari because you can't fullscreen the body element.
  //       (Or anything other than a video element?) 22.2.13

  const requestFullscreen =
    document.body.requestFullscreen || wrapper.webkitRequestFullscreen;

  // const exitFullscreen =
  //   document.exitFullscreen || document.webkitExitFullscreen;

  // Tries to toggle fullscreen. Must be called within a user interaction.
  function enableFullscreen() {
    const fullscreenElement =
      document.fullscreenElement || document.webkitFullscreenElement;

    if (!fullscreenElement) {
      requestFullscreen.apply(document.body)?.catch((e) => console.error(e));
    } else {
      // exitFullscreen();
    }
  }

  document.body.onfullscreenchange = (event) => {
    const fullscreenElement =
      document.fullscreenElement || document.webkitFullscreenElement;

    if (fullscreenElement) {
      console.log("😱 Entered fullscreen mode!", fullscreenElement);
    } else {
      console.log("😱 Leaving fullscreen mode!");
    }
  };

  // 🔘 Button Hitboxes
  // (Created for 📋 Clipboard Events)
  let hitboxes = {};
  window.addEventListener("pointerup", async (e) => {
    keys(hitboxes).forEach((key) => hitboxes[key]?.(e));
  });

  window.addEventListener("pointerdown", async (e) => {
    keys(hitboxes).forEach((key) => hitboxes[key]?.(e));
  });

  // 📄 Drag and Drop File API

  // Drag over...
  document.body.addEventListener("dragover", function (e) {
    e.stopPropagation();
    e.preventDefault();
    e.dataTransfer.dropEffect = "copy"; // Show as copy
    // copy, move, link, or none
  });

  document.body.addEventListener("drop", async function (e) {
    e.stopPropagation();
    e.preventDefault();
    const files = e.dataTransfer.files; // Get the file(s).
    
    // Process multiple files if dropped together
    if (files.length > 0) {
      console.log(`💧 Dropped ${files.length} file(s)`);
      
      // Check for ALS + WAV combination
      let alsFile = null;
      let wavFile = null;
      
      // Scan all files to identify types
      for (let i = 0; i < files.length; i++) {
        const file = files[i];
        const ext = extension(file.name);
        console.log(`💧 File ${i + 1}: ${file.name} (${ext})`);
        
        if (ext === "als" && !alsFile) {
          alsFile = file;
        } else if (ext === "wav" && !wavFile) {
          wavFile = file;
        }
      }
      
      // If we have both ALS and WAV, process them as a pair
      if (alsFile && wavFile) {
        console.log("🎵🔊 Processing ALS + WAV combination:", alsFile.name, "+", wavFile.name);
        
        // Process ALS file first
        await processDroppedFile(alsFile);
        
        // Then process WAV file
        await processDroppedFile(wavFile);
        
        return; // Exit early - we've handled the multi-file drop
      }
      
      // Otherwise, process files individually (existing behavior)
      for (let i = 0; i < files.length; i++) {
        await processDroppedFile(files[i]);
      }
    }
  });

  // Extract file processing logic into reusable function
  async function processDroppedFile(file) {
    const ext = extension(file.name);
    console.log("💧 Processing:", file.name, ext);
    
    // 📦 KidLisp bundle (.lisp.html) - extract embedded source and run it
    if (file.name.endsWith('.lisp.html')) {
      console.log("📦 Detected KidLisp bundle:", file.name);
      const reader = new FileReader();
      reader.onload = async function (e) {
        try {
          let htmlContent = e.target.result;
          
          // Check if it's gzip compressed (has DecompressionStream)
          if (htmlContent.includes("DecompressionStream('gzip')")) {
            // Extract base64 gzip data and decompress
            const base64Match = htmlContent.match(/base64,([A-Za-z0-9+/=]+)/);
            if (base64Match) {
              const base64Data = base64Match[1];
              const binaryString = atob(base64Data);
              const bytes = new Uint8Array(binaryString.length);
              for (let i = 0; i < binaryString.length; i++) {
                bytes[i] = binaryString.charCodeAt(i);
              }
              // Decompress gzip
              const blob = new Blob([bytes]);
              const ds = new DecompressionStream('gzip');
              const decompressedStream = blob.stream().pipeThrough(ds);
              htmlContent = await new Response(decompressedStream).text();
            }
          }
          
          // Extract KidLisp source from the decompressed HTML
          // Look for: window.EMBEDDED_KIDLISP_SOURCE = "..."
          // or: window.acKIDLISP_SOURCE = "..."
          let source = null;
          const embeddedMatch = htmlContent.match(/window\.EMBEDDED_KIDLISP_SOURCE\s*=\s*"([^"]+)"/);
          const acMatch = htmlContent.match(/window\.acKIDLISP_SOURCE\s*=\s*"([^"]+)"/);
          
          if (embeddedMatch) {
            source = embeddedMatch[1].replace(/\\n/g, '\n').replace(/\\"/g, '"');
          } else if (acMatch) {
            source = acMatch[1].replace(/\\n/g, '\n').replace(/\\"/g, '"');
          }
          
          if (source) {
            // Extract piece name from filename: $piece-@author-timestamp.lisp.html
            // Keep the $ prefix for KidLisp pieces
            const nameMatch = file.name.match(/^(\$[^-]+)/);
            const pieceName = nameMatch ? nameMatch[1] : file.name.replace('.lisp.html', '');
            
            console.log("📦 Extracted KidLisp source from bundle:", pieceName, source.substring(0, 50) + "...");
            send({
              type: "dropped:piece",
              content: {
                name: pieceName,
                source: source,
                isKidLisp: true,
                fromBundle: true
              },
            });
          } else {
            console.warn("📦 Could not find KidLisp source in bundle");
          }
        } catch (err) {
          console.error("📦 Error processing KidLisp bundle:", err);
        }
      };
      reader.readAsText(file);
      return;
    }
    
    // 🗒️ Source code file.
    if (ext === "mjs" || ext === "lisp") {
        const reader = new FileReader();
        reader.onload = function (e) {
          send({
            type: "dropped:piece",
            content: {
              name: file.name.replace(/\.(mjs|lisp)$/, ""),
              source: e.target.result,
            },
          });
        };

        reader.readAsText(file);
        // 🖼️ Image file
      } else if (
        ext === "png" ||
        ext === "jpeg" ||
        ext === "jpg" ||
        ext === "gif" ||
        ext === "webp"
      ) {
        const bitmap = await toBitmap(file);
        send({
          type: "dropped:bitmap",
          content: {
            name: file.name.replace("." + ext, ""),
            source: bitmap,
          },
        });
        // 🖼️⌛ Recorded Painting (or other complex media) / 🎮 Game Boy ROMs in ZIP
      } else if (ext === "zip") {
        const reader = new FileReader();
        reader.onload = async function (e) {
          const data = e.target.result;
          if (!window.JSZip) await loadJSZip();
          
          try {
            const zip = new window.JSZip();
            const zipContent = await zip.loadAsync(data);
            
            // Look for Game Boy ROM files first
            const romFiles = [];
            zipContent.forEach((relativePath, file) => {
              const ext = extension(file.name);
              if (ext === "gb" || ext === "gbc") {
                romFiles.push(file);
              }
            });
            
            if (romFiles.length > 0) {
              // Handle Game Boy ROMs from zip
              console.log(`🎮 Found ${romFiles.length} Game Boy ROM(s) in zip:`, romFiles.map(f => f.name));
              
              // If multiple ROMs, let user choose (for now, just take the first one)
              const romFile = romFiles[0];
              const romData = await romFile.async("arraybuffer");
              const ext = extension(romFile.name);
              
              console.log("🎮 BIOS: Extracted Game Boy ROM from zip:", romFile.name, `(${romData.byteLength} bytes)`);
              
              const gameRomData = {
                name: romFile.name.replace(/\.(gb|gbc)$/, ""),
                originalName: romFile.name,
                romData: romData, // ArrayBuffer
                isGameBoyColor: ext === "gbc"
              };
              
              // Handle ROM loading directly in main thread
              loadGameboyROM(gameRomData);
              return; // Exit early, we found a ROM
            }
            
            // If no ROM files found, try processing as painting record
            const record = await unzip(data);
            if (record) {
              send({ type: "painting:record:dropped", content: record });
            }
            
          } catch (error) {
            console.error("❌ Failed to process ZIP file:", error);
          }
        };
        reader.readAsArrayBuffer(file);
      // 🔊 Audio file (.wav)
      } else if (ext === "wav") {
        const reader = new FileReader();
        reader.onload = async function (e) {
          try {
            console.log("🔊 BIOS: Dropped WAV file:", file.name, `(${e.target.result.byteLength} bytes)`);
            
            // Create a unique ID for this WAV file
            const wavId = "dropped-wav-" + file.name.replace(/\.wav$/, "") + "-" + performance.now();
            const arrayBuffer = e.target.result;
            const fileSize = arrayBuffer.byteLength; // Capture size before storing
            
            console.log("🔊 BIOS: Storing WAV in sfx cache with ID:", wavId);
            // Store the WAV data in the sfx cache for decoding and playback
            sfx[wavId] = arrayBuffer;
            
            console.log("🔊 BIOS: Starting immediate WAV decoding...");
            // Trigger immediate decoding
            await decodeSfx(wavId);
            console.log("🔊 BIOS: WAV decoding complete, sending to piece...");
            
            send({
              type: "dropped:wav",
              content: {
                name: file.name.replace(/\.wav$/, ""),
                originalName: file.name,
                size: fileSize,
                id: wavId // Include the audio ID for playback
              },
            });
            
            console.log("🔊 BIOS: WAV processing complete for:", wavId);
          } catch (error) {
            console.error("❌ BIOS: Failed to process WAV file:", error);
          }
        };
        reader.readAsArrayBuffer(file);
      // 🎵 Ableton Live Set file (ZIP archive containing XML)
      } else if (ext === "als") {
        
        // Parse XML and extract key project structure using robust regex patterns
        function parseAbletonProject(xmlString) {
          try {
            console.log("🎵 BIOS parsing ALS XML data:", xmlString.length, "characters");
            
            const structure = {
              version: "Unknown",
              creator: "Unknown", 
              tracks: [],
              tempo: "Unknown",
              sceneCount: 0,
              sampleRate: "Unknown",
              lastModDate: "Unknown"
            };
            
            // Extract Ableton version info
            const abletonMatch = xmlString.match(/<Ableton[^>]*MajorVersion="([^"]*)"[^>]*MinorVersion="([^"]*)"[^>]*Creator="([^"]*)"/);
            if (abletonMatch) {
              structure.version = `${abletonMatch[1]}.${abletonMatch[2]}`;
              structure.creator = abletonMatch[3];
            }
            
            // Extract sample rate for accurate timing calculations
            const sampleRateMatch = xmlString.match(/<SampleRate[^>]*Value="([^"]*)"/);
            if (sampleRateMatch) {
              structure.sampleRate = parseFloat(sampleRateMatch[1]) || 44100;
            }
            
            // Extract last modified date
            const lastModMatch = xmlString.match(/<LastModDate[^>]*Value="([^"]*)"/);
            if (lastModMatch) {
              structure.lastModDate = lastModMatch[1];
            }
            
            // Extract tempo - Enhanced with multiple strategies
            let tempo = "Unknown";
            
            // Strategy 1: Look for global tempo in LiveSet
            const globalTempoMatch = xmlString.match(/<LiveSet[^>]*>[\s\S]*?<MasterTrack[^>]*>[\s\S]*?<DeviceChain[^>]*>[\s\S]*?<Tempo[^>]*>[\s\S]*?<Manual[^>]*Value="([^"]*)"[\s\S]*?<\/Tempo>/);
            if (globalTempoMatch) {
              tempo = parseFloat(globalTempoMatch[1]) || "Unknown";
              console.log(`🎵 BIOS found global tempo: ${tempo} BPM`);
            }
            
            // Strategy 2: Manual tempo value (enhanced pattern)
            if (tempo === "Unknown") {
              const tempoMatch = xmlString.match(/<Manual[^>]*Value="([^"]*)"/);
              if (tempoMatch) {
                const tempoValue = parseFloat(tempoMatch[1]);
                // Only accept reasonable tempo values (between 60-300 BPM)
                if (tempoValue && tempoValue >= 60 && tempoValue <= 300) {
                  tempo = tempoValue;
                  console.log(`🎵 BIOS found manual tempo: ${tempo} BPM`);
                }
              }
            }
            
            // Strategy 3: MasterTrack tempo (enhanced)
            if (tempo === "Unknown") {
              const masterTempoMatch = xmlString.match(/<MasterTrack[^>]*>[\s\S]*?<Tempo[^>]*>[\s\S]*?<Manual[^>]*Value="([^"]*)"[\s\S]*?<\/Tempo>/);
              if (masterTempoMatch) {
                tempo = parseFloat(masterTempoMatch[1]) || "Unknown";
                console.log(`🎵 BIOS found MasterTrack tempo: ${tempo} BPM`);
              }
            }
            
            structure.tempo = tempo;
            
            // Count scenes
            const sceneMatches = xmlString.match(/<Scene[^>]*>/g);
            structure.sceneCount = sceneMatches ? sceneMatches.length : 0;
            
            // Extract tracks - simplified for now to avoid complexity
            const trackTypes = ['GroupTrack', 'MidiTrack', 'AudioTrack', 'ReturnTrack'];
            console.log("🎵 BIOS starting track detection...");
            
            trackTypes.forEach(trackType => {
              const trackRegex = new RegExp(`<${trackType}[^>]*>([\\s\\S]*?)</${trackType}>`, 'g');
              let match;
              
              while ((match = trackRegex.exec(xmlString)) !== null) {
                const trackContent = match[1];
                
                // Extract track name
                const nameMatch = trackContent.match(/<(?:EffectiveName|UserName)[^>]*Value="([^"]*)"/);
                const trackName = nameMatch ? nameMatch[1] : `${trackType} ${structure.tracks.length + 1}`;
                
                console.log(`🎵 BIOS found track: "${trackName}" (${trackType})`);
                
                // Extract MIDI notes for MIDI tracks
                let midiNotes = [];
                let clips = [];
                
                if (trackType === "MidiTrack") {
                  // Look for MIDI notes in clips
                  const clipMatches = trackContent.match(/<MidiClip[^>]*>[\s\S]*?<\/MidiClip>/g);
                  if (clipMatches) {
                    clipMatches.forEach((clipContent, clipIndex) => {
                      // Extract notes from this clip
                      const noteMatches = clipContent.match(/<KeyTrack[^>]*>[\s\S]*?<Notes>[\s\S]*?<\/Notes>/g);
                      if (noteMatches) {
                        noteMatches.forEach(noteSection => {
                          const individualNotes = noteSection.match(/<MidiNoteEvent[^>]*Time="([^"]*)"[^>]*Duration="([^"]*)"[^>]*Velocity="([^"]*)"[^>]*>/g);
                          if (individualNotes) {
                            individualNotes.forEach(noteEvent => {
                              const timeMatch = noteEvent.match(/Time="([^"]*)"/);
                              const durationMatch = noteEvent.match(/Duration="([^"]*)"/);
                              const velocityMatch = noteEvent.match(/Velocity="([^"]*)"/);
                              
                              if (timeMatch && durationMatch && velocityMatch) {
                                midiNotes.push({
                                  time: parseFloat(timeMatch[1]) || 0,
                                  duration: parseFloat(durationMatch[1]) || 0,
                                  velocity: parseFloat(velocityMatch[1]) || 0,
                                  clip: clipIndex
                                });
                              }
                            });
                          }
                        });
                      }
                      
                      // Extract clip info
                      const clipNameMatch = clipContent.match(/<Name[^>]*Value="([^"]*)"/);
                      const clipTimeMatch = clipContent.match(/<CurrentStart[^>]*Value="([^"]*)"/);
                      const clipLengthMatch = clipContent.match(/<Length[^>]*Value="([^"]*)"/);
                      
                      clips.push({
                        name: clipNameMatch ? clipNameMatch[1] : `Clip ${clipIndex + 1}`,
                        start: clipTimeMatch ? parseFloat(clipTimeMatch[1]) : 0,
                        length: clipLengthMatch ? parseFloat(clipLengthMatch[1]) : 0
                      });
                    });
                  }
                } else if (trackType === "AudioTrack") {
                  // Look for audio clips
                  const audioClipMatches = trackContent.match(/<AudioClip[^>]*>[\s\S]*?<\/AudioClip>/g);
                  if (audioClipMatches) {
                    audioClipMatches.forEach((clipContent, clipIndex) => {
                      const clipNameMatch = clipContent.match(/<Name[^>]*Value="([^"]*)"/);
                      const clipTimeMatch = clipContent.match(/<CurrentStart[^>]*Value="([^"]*)"/);
                      const clipLengthMatch = clipContent.match(/<Length[^>]*Value="([^"]*)"/);
                      
                      clips.push({
                        name: clipNameMatch ? clipNameMatch[1] : `Audio Clip ${clipIndex + 1}`,
                        start: clipTimeMatch ? parseFloat(clipTimeMatch[1]) : 0,
                        length: clipLengthMatch ? parseFloat(clipLengthMatch[1]) : 0
                      });
                    });
                  }
                }
                
                structure.tracks.push({
                  name: trackName.replace(/^[#\s]*/, ''), // Remove leading # and spaces
                  type: trackType,
                  clipCount: clips.length,
                  clips: clips,
                  midiNotes: midiNotes,
                  noteCount: midiNotes.length
                });
              }
            });
            
            console.log(`🎵 BIOS ✅ Parsing complete: ${structure.tracks.length} tracks found`);
            return structure;
          } catch (e) {
            console.warn("🎵 BIOS Failed to parse XML:", e);
            return null;
          }
        }
        
        const reader = new FileReader();
        reader.onload = async function (e) {
          try {
            const data = e.target.result;
            const uint8Data = new Uint8Array(data);
            
            console.log("🔍 ALS file signature:", Array.from(uint8Data.slice(0, 4)).map(b => b.toString(16).padStart(2, '0')).join(' '));
            
            // Check if it's a ZIP file (signature: 50 4b 03 04)
            if (uint8Data[0] === 0x50 && uint8Data[1] === 0x4b && uint8Data[2] === 0x03 && uint8Data[3] === 0x04) {
              console.log("📦 ALS file is a ZIP archive, loading JSZip...");
              
              // Load JSZip if not already loaded
              if (!window.JSZip) await loadJSZip();
              
              const zip = new window.JSZip();
              const zipData = await zip.loadAsync(data);
              
              console.log("📂 ZIP contents:", Object.keys(zipData.files));
              
              // Look for the main ALS XML file (usually named after the project)
              let xmlContent = null;
              let xmlFileName = null;
              
              // Try to find XML files in the ZIP
              for (const fileName of Object.keys(zipData.files)) {
                const file = zipData.files[fileName];
                if (!file.dir && (fileName.endsWith('.xml') || fileName.endsWith('.als') || fileName === 'Project.xml')) {
                  console.log(`📄 Found XML file: ${fileName}`);
                  try {
                    // Try different extraction methods
                    console.log("🔍 Trying string extraction...");
                    xmlContent = await file.async("string");
                    console.log(`🔍 String extraction result (first 200 chars): ${xmlContent.substring(0, 200)}`);
                    
                    // If the content looks binary/garbled, try extracting as uint8array and decompress
                    if (xmlContent.charCodeAt(0) > 127 || !xmlContent.includes('<')) {
                      console.log("🔍 Content appears binary, trying binary extraction and decompression...");
                      const binaryData = await file.async("uint8array");
                      console.log(`🔍 Binary data length: ${binaryData.length}`);
                      
                      try {
                        // Try gzip decompression with pako
                        console.log("🔍 Trying gzip decompression...");
                        const decompressed = pako.inflate(binaryData, { to: 'string' });
                        console.log(`🔍 Gzip decompression result (first 200 chars): ${decompressed.substring(0, 200)}`);
                        if (decompressed.includes('<')) {
                          xmlContent = decompressed;
                        }
                      } catch (e) {
                        console.log("🔍 Gzip failed, trying deflate...");
                        try {
                          const decompressed = pako.inflateRaw(binaryData, { to: 'string' });
                          console.log(`🔍 Deflate decompression result (first 200 chars): ${decompressed.substring(0, 200)}`);
                          if (decompressed.includes('<')) {
                            xmlContent = decompressed;
                          }
                        } catch (e2) {
                          console.warn("❌ Both gzip and deflate decompression failed:", e2);
                        }
                      }
                    }
                    
                    xmlFileName = fileName;
                    break;
                  } catch (e) {
                    console.warn(`❌ Failed to extract ${fileName}:`, e);
                  }
                }
              }
              
              // If no XML found, try the first non-directory file
              if (!xmlContent) {
                for (const fileName of Object.keys(zipData.files)) {
                  const file = zipData.files[fileName];
                  if (!file.dir) {
                    console.log(`📄 Trying file: ${fileName}`);
                    const content = await file.async("string");
                    if (content.includes('<?xml') || content.includes('<Ableton')) {
                      xmlContent = content;
                      xmlFileName = fileName;
                      break;
                    }
                  }
                }
              }
              
              if (xmlContent) {
                console.log(`✅ Successfully extracted XML from ${xmlFileName} (${xmlContent.length} chars)`);
                console.log("🎵 🚀 BIOS sending dropped:als event with content:", {
                  name: file.name.replace(/\.als$/, ""),
                  xmlData: xmlContent.length + " chars"
                });
                send({
                  type: "dropped:als",
                  content: {
                    name: file.name.replace(/\.als$/, ""),
                    xmlData: xmlContent,
                  },
                });
              } else {
                console.error("❌ No XML content found in ALS ZIP file");
              }
            } else {
              // Fall back to compression methods for non-ZIP ALS files
              // Load pako if not already loaded
              if (!window.pako) {
                console.log("📦 Loading pako compression library for ALS file...");
                await new Promise((resolve, reject) => {
                  const script = document.createElement("script");
                  script.src =
                    "https://cdn.jsdelivr.net/npm/pako@2.1.0/dist/pako.min.js";
                  script.onload = resolve;
                  script.onerror = reject;
                  document.head.appendChild(script);
                });
              }
              
              let decompressedData = null;
              
              // Try different decompression methods
              const methods = [
                { name: 'zlib (inflate)', fn: () => pako.inflate(uint8Data, { to: 'string' }) },
                { name: 'gzip (ungzip)', fn: () => pako.ungzip(uint8Data, { to: 'string' }) },
                { name: 'raw deflate (inflateRaw)', fn: () => pako.inflateRaw(uint8Data, { to: 'string' }) }
              ];
              
              for (const method of methods) {
                try {
                  console.log(`🧪 Trying ${method.name}...`);
                  decompressedData = method.fn();
                  console.log(`✅ Successfully decompressed using ${method.name}`);
                  break;
                } catch (err) {
                  console.log(`❌ ${method.name} failed:`, err.message);
                }
              }
              
              if (decompressedData) {
                // Parse the XML data into a structured object
                const parsedProject = parseAbletonProject(decompressedData);
                
                console.log("🎵 🚀 BIOS sending dropped:als event with parsed content:", {
                  name: file.name.replace(/\.als$/, ""),
                  project: parsedProject ? `${parsedProject.tracks.length} tracks, ${parsedProject.tempo} BPM` : "parsing failed"
                });
                
                send({
                  type: "dropped:als",
                  content: {
                    name: file.name.replace(/\.als$/, ""),
                    xmlData: decompressedData,
                    project: parsedProject, // Send parsed structure
                  },
                });
              } else {
                console.error("❌ All decompression methods failed for ALS file");
                // Try to read as plain text in case it's not compressed
                try {
                  const textData = new TextDecoder().decode(uint8Data);
                  if (textData.includes('<?xml') || textData.includes('<Ableton')) {
                    console.log("📄 File appears to be uncompressed XML");
                    
                    // Parse the XML data into a structured object
                    const parsedProject = parseAbletonProject(textData);
                    
                    console.log("🎵 🚀 BIOS sending dropped:als event with uncompressed parsed content:", {
                      name: file.name.replace(/\.als$/, ""),
                      project: parsedProject ? `${parsedProject.tracks.length} tracks, ${parsedProject.tempo} BPM` : "parsing failed"
                    });
                    
                    send({
                      type: "dropped:als",
                      content: {
                        name: file.name.replace(/\.als$/, ""),
                        xmlData: textData,
                        project: parsedProject, // Send parsed structure
                      },
                    });
                  } else {
                    console.error("File doesn't appear to be XML either");
                  }
                } catch (textErr) {
                  console.error("Failed to read as text:", textErr);
                }
              }
            }
          } catch (error) {
            console.error("Failed to process ALS file:", error);
          }
        };
        reader.readAsArrayBuffer(file);
      // 🎮 Game Boy ROM file (.gb, .gbc)
      } else if (ext === "gb" || ext === "gbc") {
        const reader = new FileReader();
        reader.onload = function (e) {
          console.log("🎮 BIOS: Dropped Game Boy ROM:", file.name, `(${e.target.result.byteLength} bytes)`);
          
          const romData = {
            name: file.name.replace(/\.(gb|gbc)$/, ""),
            originalName: file.name,
            romData: e.target.result, // ArrayBuffer
            isGameBoyColor: ext === "gbc"
          };
          
          // Handle ROM loading directly in main thread
          loadGameboyROM(romData);
        };
        reader.readAsArrayBuffer(file);
    }
  }

  // Instantly decode the audio before playback if it hasn't been already.
  async function decodeSfx(sound) {
    // console.log("🎵 BIOS decodeSfx called for:", sound, "type:", typeof sfx[sound]);
    
    // If sound is already being decoded, wait a bit and return
    if (decodingInProgress.has(sound)) {
      // console.log("🎵 BIOS decodeSfx already in progress for:", sound);
      // Wait a moment and check again
      await new Promise((resolve) => setTimeout(resolve, 10));
      return sfx[sound];
    }

    // Handle raw audio object (from cached tape when audioContext wasn't ready)
    if (sfx[sound] && typeof sfx[sound] === 'object' && sfx[sound].left && sfx[sound].right && sfx[sound].totalSamples) {
      console.log("🎬 📼 Converting cached raw audio to AudioBuffer");
      decodingInProgress.add(sound);
      
      try {
        // Ensure audioContext is initialized
        if (!audioContext) {
          if (activateSound) {
            activateSound();
            await new Promise((resolve) => setTimeout(resolve, 100));
          }
        }
        
        if (audioContext) {
          const rawAudio = sfx[sound];
          const audioBuffer = audioContext.createBuffer(2, rawAudio.totalSamples, rawAudio.sampleRate);
          audioBuffer.getChannelData(0).set(rawAudio.left);
          audioBuffer.getChannelData(1).set(rawAudio.right);
          sfx[sound] = audioBuffer;
          console.log(`🎬 📼 Successfully converted raw audio to AudioBuffer: ${rawAudio.totalSamples} samples`);
          decodingInProgress.delete(sound);
          return sfx[sound];
        } else {
          console.error("🎬 📼 Cannot convert raw audio: AudioContext not available");
          decodingInProgress.delete(sound);
          return null;
        }
      } catch (error) {
        console.error("🎬 📼 Failed to convert raw audio:", error);
        decodingInProgress.delete(sound);
        return null;
      }
    }

    if (sfx[sound] instanceof ArrayBuffer) {
      // console.log("🎵 BIOS decodeSfx starting decode for ArrayBuffer:", sound);
      // Mark as being decoded to prevent concurrent decode attempts
      decodingInProgress.add(sound);

      // Ensure audioContext is initialized before trying to decode
      if (!audioContext) {
        // console.log("🔊 Initializing audio context for WAV decoding...");
        if (activateSound) {
          activateSound();
          // Wait a moment for audio context to initialize
          await new Promise((resolve) => setTimeout(resolve, 100));
        }
      }

      let audioBuffer;
      let buf = null; // Declare outside try block for error logging
      try {
        buf = sfx[sound];
        sfx[sound] = null;
        
        if (buf && audioContext) {
          // console.log("🎵 BIOS decoding audio data for:", sound, "buffer size:", buf.byteLength);
          audioBuffer = await audioContext.decodeAudioData(buf);
          if (debug && logs.audio) console.log("🔈 Decoded:", sound);
          // console.log("🎵 BIOS decode successful:", sound, "audioBuffer:", !!audioBuffer);
          sfx[sound] = audioBuffer;

          // Process any queued sounds that might be waiting for this file
          processPendingSfx();

          return sfx[sound];
        } else {
          console.error("🎵 BIOS decode failed - missing buffer or audioContext:", !!buf, !!audioContext);
        }
      } catch (err) {
        console.error("🔉 [DECODE] Decode error:", {
          error: err,
          sound: sound,
          bufferType: typeof buf,
          bufferByteLength: buf?.byteLength,
          isArrayBuffer: buf instanceof ArrayBuffer
        });
        sfx[sound] = null; // Clear the failed audio data
      } finally {
        // Always remove from decoding set when done
        decodingInProgress.delete(sound);
      }
    } else {
      return sfx[sound];
    }
  }

  // Queue for sounds that need to be played once audio context is available
  let pendingSfxQueue = [];

  // Track sounds that are currently being decoded to prevent multiple decode attempts
  const decodingInProgress = new Set();

  // Process any queued sound effects once audio context is ready
  function processPendingSfx() {
    if (audioContext && pendingSfxQueue.length > 0) {
      const remaining = [];
      const currentTime = Date.now();

      pendingSfxQueue.forEach(
        ({ id, soundData, options, completed, queuedAt }) => {
          // Add timestamp if not present
          const queueTime = queuedAt || currentTime;
          const timeInQueue = currentTime - queueTime;

          // Only play sounds that have been loaded into the sfx cache
          if (sfx[soundData] && !(sfx[soundData] instanceof ArrayBuffer)) {
            playSfx(id, soundData, options, completed);
          } else if (timeInQueue < 5000) {
            // Only retry for 5 seconds
            remaining.push({
              id,
              soundData,
              options,
              completed,
              queuedAt: queueTime,
            });
          }
        },
      );

      pendingSfxQueue = remaining;

      // If there are still sounds waiting, check again soon
      if (pendingSfxQueue.length > 0) {
        setTimeout(processPendingSfx, 100);
      }
    }
  }

  // Utilities

  // Convert an img or blob object to an ac formatted bitmap  / "painting".
  async function toBitmap(imgOrBlob) {
    const img = await createImageBitmap(imgOrBlob);
    const canvas = document.createElement("canvas");
    canvas.width = img.width;
    canvas.height = img.height;
    const ctx = canvas.getContext("2d");
    ctx.drawImage(img, 0, 0);
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    return {
      width: imageData.width,
      height: imageData.height,
      pixels: imageData.data,
    };
  }

  // Unzip a file.
  async function unzip(data) {
    try {
      const zip = await window.JSZip.loadAsync(data);

      // Detect type of media based on presence of "steps" file...
      const steps = JSON.parse(await zip.file("painting.json")?.async("text"));
      const record = [];

      if (steps) {
        // TODO: Parse the JSON from steps.
        const lines = steps; // Remove timestamp.

        // Load `painting:recording` step text format.
        for (let i = 0; i < lines.length; i += 1) {
          const components = lines[i].step.split(" - ");
          const step = { timestamp: components[0], label: components[1] };
          if (lines[i].gesture?.length > 0) step.gesture = lines[i].gesture;
          const picture = zip.file(`${lines[i].step}.png`);

          if (picture) {
            const blob = await picture.async("blob");
            step.painting = await toBitmap(blob);
          }
          record.push(step);
        }

        return record;
      } else {
        console.warn("🤐 Could not detect ZIP media type.");
        return record;
      }
    } catch (err) {
      console.error("🤐 Error reading ZIP:", err);
      return record;
    }
  }

  // Convert a blob oobject to an ArrayBuffer.
  function blobToArrayBuffer(blob) {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onloadend = () => resolve(reader.result);
      reader.onerror = reject;
      reader.readAsArrayBuffer(blob);
    });
  }

  window.iMessageExtensionResize = (mode) => {
    console.log("📱 iMessage Extension Resized:", mode);
    window.acSEND({ type: "imessage-extension:resized", content: { mode } });
  };

  window.iOSAppSwitchPiece = (piece) => {
    console.log("📱 iOS Switch Piece:", piece);
    window.acSEND({
      type: "jump",
      content: { piece, ahistorical: false, alias: false },
    });
  };
} // End of boot function

function iOSAppSend(message) {
  const packedMessage = JSON.stringify(message);
  console.log("📱 Sending to iOS App:", packedMessage);
  window.webkit?.messageHandlers?.iOSApp.postMessage(packedMessage);
}

async function checkMicrophonePermission() {
  try {
    const permissionStatus = await navigator.permissions.query({
      name: "microphone",
    });
    // console.log('Microphone permission status:', permissionStatus.state);
    if (permissionStatus.state === "granted") {
      // console.log('Microphone access is granted.');
    } else if (permissionStatus.state === "denied") {
      // console.log('Microphone access is denied.');
    } else {
      // console.log(`Microphone access is in prompt state (user hasn't decided yet).`);
    }
    return permissionStatus.state; // 'granted', 'denied', or 'prompt'
  } catch (error) {
    console.error("Permission query error:", error);
    return null;
  }
}

export { boot };
