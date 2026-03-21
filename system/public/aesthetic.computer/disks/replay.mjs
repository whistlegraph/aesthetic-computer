// Replay, 2025.01.18
// View and playback any aesthetic.computer tape recording.

/* #region 📚 README 
  Load and play back tape recordings from their ZIP files.
  Usage:
    - `replay` - blank viewer
    - `!code` or `replay !code` - load tape by code (e.g., `!abc`)
    - `!code:show` - lightbox mode (no UI)
  
  The ! symbol flashes like a tape light, marking playback commands.
#endregion */

/* #region 🏁 TODO 
  - [] Add playback controls (play/pause, scrubbing)
  - [] Add download button for ZIP
  - [] Add frame-by-frame navigation
  - [] Add speed controls
  - [] Support audio playback from soundtrack.wav
  - [] Add loop option
#endregion */

import { 
  deriveProgressState, 
  renderLoadingProgressBar,
  renderStreamingBadge,
  formatMegabytes 
} from "./common/tape-player.mjs";

const { floor, sin, max, min } = Math;

let tapeCode; // Short code for this tape (e.g., "abc")
let tapeMetadata; // Metadata from MongoDB
let tapeZip; // JSZip instance with loaded tape
let frames = []; // Array of loaded frame ImageBitmaps
let timingData = []; // Timing information from timing.json
let metadata = {}; // Metadata from metadata.json
let currentFrame = 0;
let playing = false;
let loading = false;
let error = null;
let lastFrameTime = 0;
let audioBuffer = null; // Web Audio buffer for soundtrack
let audioSource = null; // Current audio source node
let audioContext = null; // Web Audio context
let audioStartTime = 0; // When audio playback started
let audioStartedAtFrame = 0; // Which frame audio started at

let label = "replay viewer";
let showMode = false;

let ellipsisTicker;

let downloadProgress = 0;
let downloadProgressKnown = false;
let downloadBytes = 0;
let downloadTotalBytes = 0;
let loadProgress = 0;
let loadedFramesCount = 0;
let totalFramesCount = 0;
let loadingPhase = "download";
let progressActive = false;
let downloadComplete = false;
let framesComplete = false;
let streamingInProgress = false;

const clamp01 = (value) => max(0, min(1, Number.isFinite(value) ? value : 0));
const PROGRESS_ALERT_STEP = 0.01;

let requestPaint = () => {};
let lastScreenWidth = 0;
let lastScreenHeight = 0;
let lastHudLabelText = null;
let hudBackActive = false;
let lastNoticeStage = null;
let lastActProgressStage = null;
let lastActProgressValue = -1;
let lastActIndeterminate = false;
let hasAnnouncedReady = false;
let hasAnnouncedError = false;

function updateHudLabel(text, { force = false } = {}) {
  if (!$hud) return;
  const nextLabel = text ?? label ?? "replay viewer";

  if (typeof $hud.label === "function" && (force || nextLabel !== lastHudLabelText)) {
    $hud.label(nextLabel, undefined, 0);
    lastHudLabelText = nextLabel;
  }

  if (showMode && typeof $hud.labelBack === "function" && (force || !hudBackActive)) {
    $hud.labelBack();
    hudBackActive = true;
  }
}

function sendActAlert(payload) {
  if (!$send || payload === undefined || payload === null) return;

  if (typeof payload === "string") {
    $send({ type: "piece:act-alert", content: payload });
    return;
  }

  if (typeof payload.name !== "string" || payload.name.length === 0) return;

  const cleaned = { name: payload.name };
  const fields = ["stage", "progress", "status", "detail", "code", "indeterminate"];
  fields.forEach((key) => {
    if (payload[key] !== undefined && payload[key] !== null) {
      cleaned[key] = payload[key];
    }
  });

  $send({ type: "piece:act-alert", content: cleaned });
}

function resetProgressAnnouncers() {
  lastNoticeStage = null;
  lastActProgressStage = null;
  lastActProgressValue = -1;
  lastActIndeterminate = false;
  hasAnnouncedReady = false;
  hasAnnouncedError = false;
}

function maybeBroadcastProgressToAct(state) {
  if (!state) return;

  const { stage, overallProgress, statusText, detailText, isIndeterminate } = state;

  const progressValue = isIndeterminate ? -1 : clamp01(overallProgress || 0);
  const stageChanged = stage !== lastActProgressStage;
  const indeterminateChanged = isIndeterminate !== lastActIndeterminate;

  let shouldSend = stageChanged || indeterminateChanged;

  if (!isIndeterminate) {
    if (progressValue >= 1 || lastActProgressValue < 0) {
      shouldSend = true;
    } else if (progressValue - lastActProgressValue >= PROGRESS_ALERT_STEP) {
      shouldSend = true;
    }
  }

  if (!shouldSend) return;

  lastActProgressStage = stage;
  lastActIndeterminate = isIndeterminate;
  if (!isIndeterminate) {
    lastActProgressValue = progressValue;
  }

  sendActAlert({
    name: "replay:progress",
    code: tapeCode ? `!${tapeCode}` : undefined,
    stage,
    progress: isIndeterminate ? null : progressValue,
    status: statusText,
    detail: detailText,
    indeterminate: isIndeterminate || undefined,
  });
}

// Store disk API references
let $send, $net, $hud;

// 🥾 Boot
function boot({
  params,
  colon,
  net,
  ui,
  screen,
  hud,
  gizmo,
  hash,
  query,
  send,
  store,
}) {
  // Store API references for use in other functions
  $send = send;
  $net = net;
  $hud = hud;
  
  let showFlagFromParam = false;

  if (params[0] && params[0].startsWith('!')) {
    const [codeToken, ...flags] = params[0].split(':');
    showFlagFromParam = flags.includes('show');
    params[0] = codeToken; // Normalize param for downstream logic
  }

  showMode = colon[0] === "show" || showFlagFromParam;
  
  console.log(`📼 replay.mjs boot - params:`, params, `hash:`, hash);
  
  ellipsisTicker = new gizmo.EllipsisTicker();
  
  // Initialize Web Audio context
  if (typeof AudioContext !== 'undefined') {
    audioContext = new AudioContext();
  } else if (typeof webkitAudioContext !== 'undefined') {
    audioContext = new webkitAudioContext();
  }

  // Check for code in hash (e.g., #abc or !abc)
  if (hash && hash.length > 0) {
    const code = hash.replace(/^[#!]/, ''); // Strip # or ! prefix
    console.log(`📼 Loading tape by code from hash: !${code}`);
    loadTapeByCode(code);
  } else if (params[0] && params[0].startsWith('!')) {
    const code = params[0].slice(1); // Strip prefix
    console.log(`📼 Loading tape by code from param: !${code}`);
    loadTapeByCode(code);
  } else {
    label = "replay viewer";
    updateHudLabel(label, { force: true });
    error = "Enter a tape code like: !abc";
  }
  
  if (showMode) {
    updateHudLabel(label, { force: true });
  }
  
}

// Load tape metadata and send play command to bios
async function loadTapeByCode(code) {
  if (!code) return;
  
  tapeCode = code;
  label = `!${code}`;
  updateHudLabel(label, { force: true });
  loading = true;
  playing = false;
  error = null;
  downloadProgressKnown = false;
  downloadProgress = 0;
  downloadBytes = 0;
  downloadTotalBytes = 0;
  loadProgress = 0;
  loadedFramesCount = 0;
  totalFramesCount = 0;
  loadingPhase = "download";
  progressActive = true;
  downloadComplete = false;
  framesComplete = false;
  streamingInProgress = false;
  resetProgressAnnouncers();
  sendActAlert({ name: "replay:loading", code: `!${code}` });
  
  try {
    // Fetch tape metadata from MongoDB
    console.log(`📡 Fetching tape metadata for code: ${code}`);
    const metadataResponse = await fetch(`/api/get-tape?code=${code}`);
    
    if (!metadataResponse.ok) {
      if (metadataResponse.status === 404) {
        throw new Error(`Tape !${code} not found.\nMake sure the tape exists in the database.`);
      } else {
        throw new Error(`Failed to load tape: ${metadataResponse.status} ${metadataResponse.statusText}`);
      }
    }
    
    tapeMetadata = await metadataResponse.json();
    console.log(`📊 Tape metadata:`, tapeMetadata);
    
    // Check if tape is nuked
    if (tapeMetadata.nuked) {
      throw new Error(`Tape !${code} has been deleted`);
    }
    
    // Construct ZIP URL based on bucket and user
    let zipUrl;
    if (tapeMetadata.bucket === 'user-aesthetic-computer' && tapeMetadata.user) {
      // User tape: include user ID and /video/ subdirectory
      zipUrl = `https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/${encodeURIComponent(tapeMetadata.user)}/video/${tapeMetadata.slug}.zip`;
    } else {
      // Guest tape: direct slug
      zipUrl = `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${tapeMetadata.slug}.zip`;
    }
    
    console.log(`📦 Requesting tape playback from bios: ${zipUrl}`);
    
    // Request bios to load and play the tape in underlay
    $send({ type: "tape:play", content: { code, zipUrl, metadata: tapeMetadata } });
    requestPaint();
    
  } catch (err) {
    console.error(`❌ Error loading tape metadata:`, err);
    error = err.message;
    loading = false;
    loadingPhase = "error";
    progressActive = false;
    streamingInProgress = false;
    downloadComplete = false;
    framesComplete = false;
    if (!hasAnnouncedError) {
      hasAnnouncedError = true;
      sendActAlert({
        name: "replay:error",
        code: tapeCode ? `!${tapeCode}` : undefined,
        detail: error,
      });
    }
  }
}

// 📡 Receive messages from bios
function receive(e) {
  const { type, content } = e;
  
  if (type === "tape:download-progress") {
    progressActive = true;
    const progressValue = content?.progress;
    const hasProgress = typeof progressValue === "number" && Number.isFinite(progressValue);
    const receivedBytes = typeof content?.receivedBytes === "number" ? content.receivedBytes : null;
    const totalBytesValue = typeof content?.totalBytes === "number" && Number.isFinite(content.totalBytes) && content.totalBytes > 0
      ? content.totalBytes
      : null;

    if (hasProgress) {
      downloadProgress = clamp01(progressValue);
      downloadProgressKnown = true;
    } else if (totalBytesValue && typeof receivedBytes === "number" && Number.isFinite(receivedBytes)) {
      downloadProgress = clamp01(receivedBytes / totalBytesValue);
      downloadProgressKnown = true;
    } else {
      if (!downloadProgressKnown) {
        downloadProgress = 0;
      }
      downloadProgressKnown = false;
    }

    if (typeof receivedBytes === "number" && Number.isFinite(receivedBytes)) {
      downloadBytes = receivedBytes;
    }

    if (totalBytesValue) {
      downloadTotalBytes = totalBytesValue;
    } else if (content?.totalBytes === null) {
      downloadTotalBytes = 0;
    }

    if (loadingPhase !== "frames" && loadingPhase !== "presenting" && loadingPhase !== "unpacking") {
      loadingPhase = "download";
    }
    if (!playing) {
      loading = true;
    }
    if (typeof progressValue === "number" && progressValue >= 1) {
      downloadComplete = true;
    } else if (!hasProgress && totalBytesValue && typeof receivedBytes === "number" && receivedBytes >= totalBytesValue) {
      downloadComplete = true;
    }
    requestPaint();
    return;
  }

  if (type === "tape:load-progress") {
    progressActive = true;
    const rawProgress = typeof content?.progress === "number" && Number.isFinite(content.progress)
      ? content.progress
      : null;
    const loadedFrames = typeof content?.loadedFrames === "number" && Number.isFinite(content.loadedFrames)
      ? content.loadedFrames
      : null;
    const totalFrames = typeof content?.totalFrames === "number" && Number.isFinite(content.totalFrames)
      ? content.totalFrames
      : null;

    let derivedProgress = rawProgress;
    if (!Number.isFinite(derivedProgress) && totalFrames && totalFrames > 0 && loadedFrames !== null) {
      derivedProgress = loadedFrames / totalFrames;
    }

    loadProgress = clamp01(Number.isFinite(derivedProgress) ? derivedProgress : 0);
    if (loadedFrames !== null) {
      loadedFramesCount = loadedFrames;
    }
    if (totalFrames !== null) {
      totalFramesCount = totalFrames;
    }
    loadingPhase = content?.phase || "frames";
    if (!playing) {
      loading = true;
    }
    if (!framesComplete) {
      if (typeof content?.progress === "number" && content.progress >= 1) {
        framesComplete = true;
      } else if (totalFramesCount > 0 && loadedFramesCount >= totalFramesCount) {
        framesComplete = true;
      }
    }
    if (framesComplete && streamingInProgress && playing) {
      streamingInProgress = false;
      progressActive = false;
    }
    requestPaint();
    return;
  }

  console.log(`📼 replay.mjs received message:`, type, content);
  
  if (type === "tape:loaded") {
    const streaming = Boolean(content?.streaming);
    const framesReady = typeof content?.framesReady === "number" && Number.isFinite(content.framesReady)
      ? content.framesReady
      : null;
    const totalFrames = typeof content?.totalFrames === "number" && Number.isFinite(content.totalFrames)
      ? content.totalFrames
      : null;

    playing = true;
    downloadProgress = 1;
    downloadProgressKnown = true;
    downloadComplete = true;

    if (totalFrames !== null) {
      totalFramesCount = totalFrames;
    }
    if (framesReady !== null) {
      loadedFramesCount = framesReady;
    } else if (!streaming && totalFramesCount > 0) {
      loadedFramesCount = totalFramesCount;
    }

    if (streaming) {
      streamingInProgress = true;
      loadingPhase = "frames";
      progressActive = true;
    } else {
      streamingInProgress = false;
      framesComplete = true;
      loadProgress = 1;
      loadingPhase = "presenting";
      progressActive = true;
    }

    loading = true;
    console.log(`✅ Tape loaded and playing!${streaming ? " (streaming frames)" : ""}`);
    if (!hasAnnouncedReady) {
      hasAnnouncedReady = true;
      sendActAlert({ name: "replay:ready", code: tapeCode ? `!${tapeCode}` : undefined });
    }
    requestPaint();
  } else if (type === "tape:error") {
    console.error(`❌ Error from bios:`, content);
    error = content;
    loading = false;
    loadingPhase = "error";
    downloadProgressKnown = false;
    downloadProgress = 0;
    loadProgress = 0;
    downloadBytes = 0;
    downloadTotalBytes = 0;
    loadedFramesCount = 0;
    totalFramesCount = 0;
    progressActive = false;
    streamingInProgress = false;
    downloadComplete = false;
    framesComplete = false;
    if (!hasAnnouncedError) {
      hasAnnouncedError = true;
      sendActAlert({
        name: "replay:error",
        code: tapeCode ? `!${tapeCode}` : undefined,
        detail: typeof content === "string" ? content : undefined,
      });
    }
    requestPaint();
  } else if (type === "recorder:presented") {
    // Underlay is ready and presenting
    loading = false;
    playing = true;
    loadProgress = 1;
    downloadProgress = 1;
    loadingPhase = "presenting";
    progressActive = false;
    streamingInProgress = false;
    framesComplete = true;
    console.log(`✅ Underlay presented, tape playing!`);
    if (!hasAnnouncedReady) {
      hasAnnouncedReady = true;
      sendActAlert({ name: "replay:ready", code: tapeCode ? `!${tapeCode}` : undefined });
    }
    requestPaint();
  } else if (type === "recorder:present-playing") {
    playing = true;
    loading = false;
    console.log(`✅ Playback resumed`);
    requestPaint();
  } else if (type === "recorder:present-paused") {
    playing = false;
    console.log(`⏸️ Playback paused`);
    requestPaint();
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, ui, help, rec, num, text, needsPaint, sound }) {
  const centerX = floor(screen.width / 2);
  const centerY = floor(screen.height / 2);
  const now = Date.now();
  const time = now / 1000;
  if (typeof needsPaint === "function") {
    requestPaint = needsPaint;
  }

  // Get playback state from rec object (provided by bios)
  const presenting = rec?.presenting ?? false;
  const isPlaying = rec?.playing ?? false;
  
  // Check if audio context needs user interaction
  const audioSuspended = sound?.bios?.audioContext?.state === "suspended";

  const recPresentProgress = rec?.presentProgress ?? null;
  let progressState = progressActive ? deriveProgressState({
    phase: loadingPhase,
    downloadProgress,
    downloadProgressKnown,
    downloadBytes,
    downloadTotalBytes,
    loadProgress,
    loadedFramesCount,
    totalFramesCount,
    timeSeconds: time,
    error,
  }) : null;

  const screenChanged =
    screen.width !== lastScreenWidth || screen.height !== lastScreenHeight;

  if (screenChanged) {
    lastScreenWidth = screen.width;
    lastScreenHeight = screen.height;
    updateHudLabel(label, { force: true });
    requestPaint();
  }

  if (loading || presenting || isPlaying) {
    requestPaint();
  }

  if (progressState) {
    maybeBroadcastProgressToAct(progressState);
  }
  
  // If tape is actively playing in underlay, allow loading overlay to clear
  if (presenting && isPlaying && loading) {
    loading = false;
    playing = true;
  }

  if (!loading) {
    progressState = null;
  }

  if (progressState) {
    const hudParts = [];
    hudParts.push(label ?? "replay viewer");
    if (!progressState.isIndeterminate) {
      const overallPct = Math.floor(clamp01(progressState.overallProgress || 0) * 100);
      hudParts.push(`${overallPct}%`);
    } else if (Number.isFinite(downloadBytes) && downloadBytes > 0) {
      const receivedText = formatMegabytes(downloadBytes);
      if (receivedText) {
        hudParts.push(receivedText);
      }
    }
    hudParts.push(progressState.statusText);
    const nextHudLabel = hudParts.filter(Boolean).join(" • ");
    updateHudLabel(nextHudLabel);

    if (ui && typeof ui.notice === "function") {
      if (progressState.stage !== lastNoticeStage) {
        const noticeParts = [];
        if (tapeCode) {
          noticeParts.push(`!${tapeCode}`);
        }
        noticeParts.push(progressState.statusText.toUpperCase());
        if (!progressState.isIndeterminate) {
          const overallPct = Math.floor(clamp01(progressState.overallProgress || 0) * 100);
          noticeParts.push(`${overallPct}%`);
        }
        let noticeColors;
        if (progressState.stage === "download") {
          noticeColors = ["cyan", "blue"];
        } else if (progressState.stage === "frames") {
          noticeColors = ["orange", "yellow"];
        } else if (progressState.stage === "presenting") {
          noticeColors = ["lime", "green"];
        }
        ui.notice(noticeParts.join(" • "), noticeColors);
        lastNoticeStage = progressState.stage;
      }
    }
  } else if (!loading && !error && lastHudLabelText !== label) {
    updateHudLabel(label, { force: true });
    lastNoticeStage = null;
  }
  
  // Show loading state (only if not presenting)
  if (presenting) {
    if (Number.isFinite(recPresentProgress)) {
      rec.tapeProgress = clamp01(recPresentProgress);
    }
  } else if (rec && typeof rec.tapeProgress === "number" && rec.tapeProgress !== 0) {
    rec.tapeProgress = 0;
  }

  if (loading) {
    wipe(0, 32, 64);

    const displayCode = tapeCode ? `!${tapeCode}` : "!code";
    const state = progressState ?? deriveProgressState({
      phase: loadingPhase,
      downloadProgress,
      downloadProgressKnown,
      downloadBytes,
      downloadTotalBytes,
      loadProgress,
      loadedFramesCount,
      totalFramesCount,
      timeSeconds: time,
      error,
    });

    // Use shared loading progress bar renderer
    renderLoadingProgressBar({
      ink,
      write,
      screen,
      num,
      help,
      ellipsisTicker,
      progressState: state,
      header: displayCode,
      centerY,
    });

    // Show frame count for frames stage
    if (state?.stage === "frames" && totalFramesCount > 0) {
      const frameRatio = clamp01(totalFramesCount ? loadedFramesCount / totalFramesCount : 0);
      const frameLine = `${loadedFramesCount}/${totalFramesCount} frames (${Math.floor(frameRatio * 100)}%)`;
      const barTop = centerY + 8;
      const barHeight = 4;
      ink(200, 230, 255).write(frameLine, {
        x: centerX,
        y: barTop + barHeight + 28,
        size: 1,
        center: "x",
      });
    }

    return;
  }
  
  // Show error state
  if (error) {
    if (ui && typeof ui.notice === "function" && lastNoticeStage !== "error") {
      const noticeParts = [];
      if (tapeCode) {
        noticeParts.push(`!${tapeCode}`);
      }
      noticeParts.push("LOAD ERROR");
      ui.notice(noticeParts.join(" • "), ["yellow", "red"]);
      lastNoticeStage = "error";
    }
    if (lastHudLabelText !== `${label} • ERROR`) {
      updateHudLabel(`${label} • ERROR`);
    }
    wipe(0, 32, 64); // Dark background
    ink(255, 100, 100);
    const errorLines = error.split('\n');
    errorLines.forEach((line, i) => {
      write(line, { 
        x: centerX, 
        y: centerY + (i * 12) - floor(errorLines.length * 6),
        size: 1,
        center: "x"
      });
    });
    return;
  }
  
  // Tape is playing in underlay - use transparent wipe so video shows through
  if (presenting && isPlaying) {
    wipe(0, 0, 0, 0); // Transparent wipe - let underlay show through

    // Use shared streaming badge renderer
    renderStreamingBadge({
      ink,
      write,
      screen,
      loadedFramesCount,
      totalFramesCount,
    });
    
    // Show "TAP TO ENABLE AUDIO" prompt if audio context is suspended
    if (audioSuspended) {
      const pulse = Math.abs(Math.sin(time * 2)); // Pulse effect
      const alpha = 128 + Math.floor(pulse * 127); // Fade between 128-255
      
      // Semi-transparent dark overlay
      ink(0, 0, 0, 160).box(0, 0, screen.width, screen.height);
      
      // Pulsing prompt text
      ink(255, 200, 100, alpha);
      write("TAP TO ENABLE AUDIO", {
        x: centerX,
        y: centerY,
        size: 1,
        center: "xy",
      });
      
      // Smaller instruction text
      ink(200, 200, 200, alpha * 0.8);
      write("Audio requires user interaction", {
        x: centerX,
        y: centerY + 20,
        size: 0.5,
        center: "xy",
      });
      
      requestPaint(); // Keep animating the pulse
    }

    return;
  }
}

// 🧮 Sim
function sim({ clock } = {}) {
  if (!loading && !progressActive) return;

  let tickTime;
  if (clock && typeof clock.time === "function") {
    tickTime = clock.time();
  } else if (clock instanceof Date) {
    tickTime = clock;
  } else if (typeof clock === "number") {
    tickTime = new Date(clock);
  }

  ellipsisTicker?.update(tickTime ?? new Date());
}

// 🎪 Act
function act({ event: e, sound }) {
  // Keyboard controls
  if (e.is("keyboard:down")) {
    if (e.key === " ") {
      // Spacebar: toggle play/pause
      togglePlayback(sound);
      return false; // Prevent default
    }
  }
  
  // Touch/tap controls
  if (e.is("touch") || e.is("pen:down")) {
    // Always use async togglePlayback to handle audio context properly
    togglePlayback(sound);
    return false; // Prevent default
  }
}

// Toggle play/pause
async function togglePlayback(sound) {
  // Resume audio context if needed and wait for it
  if (sound?.bios?.audioContext?.state === "suspended") {
    console.log("🎵 Resuming audio context before playback toggle");
    try {
      await sound.bios.audioContext.resume();
      console.log("🎵 Audio context resumed successfully before playback");
    } catch (err) {
      console.warn("🎵 Failed to resume audio context:", err);
    }
  }
  
  if (playing) {
    $send({ type: "recorder:present:pause" });
  } else {
    $send({ type: "recorder:present:play" });
  }
}

// 👋 Leave
function leave() {
  // Stop playback when leaving
  if (playing) {
    $send({ type: "recorder:present:pause" });
  }
  $send({ type: "recorder:unpresent" });
  playing = false;
  loading = false;
  progressActive = false;
  streamingInProgress = false;
  downloadComplete = false;
  framesComplete = false;
  resetProgressAnnouncers();
}

// 📰 Meta
function meta({ params, hash } = {}) {
  let code;
  if (hash && hash.length > 0) {
    code = hash.replace(/^[#!]/, "");
  } else if (params && params[0] && params[0].startsWith("!")) {
    code = params[0].slice(1);
  }

  if (code) {
    return {
      title: `!${code} • Aesthetic Computer`,
      desc: `Tape recording !${code} on Aesthetic Computer`,
    };
  }

  return {
    title: "replay",
    desc: "Replay aesthetic.computer tape recordings.",
  };
}

export { boot, sim, paint, act, leave, meta, receive };
