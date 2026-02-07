// `aesthetic.computer` Bootstrap, 23.02.16.19.23
// Don't clear console if we're in an embedded/iframe context (like kidlisp.com editor)
// if (window === window.top) {
//   console.clear();
// }

// 📦 Early WebSocket module loader initialization
// This runs before anything else to establish connection ASAP
// Skip in PACK mode (NFT bundles use import maps)
let moduleLoader = null;
let moduleLoaderReady = Promise.resolve(false); // Promise that resolves when loader is ready

if (!window.acPACK_MODE) {
  try {
    // Dynamic import of module-loader (this one file loads via HTTP)
    const { moduleLoader: ml } = await import("./module-loader.mjs");
    moduleLoader = ml;
    
    // 🚀 Start connecting immediately with shorter timeout (800ms)
    // Store the promise so we can await it before importing modules
    moduleLoaderReady = moduleLoader.init(800).then(connected => {
      if (connected) {
        // Show connection status in boot canvas (static indicator)
        if (window.acBootCanvas?.setSessionConnected) {
          window.acBootCanvas.setSessionConnected(true);
        }
        // Prefetch disk.mjs for when bios needs it
        moduleLoader.prefetch(["lib/disk.mjs"]);
      }
      return connected;
    });
    
    // Store globally for use by importWithRetry
    window.acModuleLoader = moduleLoader;
  } catch (err) {
    console.warn("📦 Module loader not available:", err.message);
  }
}

// �🔧 Register Service Worker for module caching (production + dev)
// Skip in PACK mode (NFT bundles) and sandboxed iframes
if ('serviceWorker' in navigator && !window.acPACK_MODE && window === window.top) {
  navigator.serviceWorker.register('/sw.js', { scope: '/' })
    .then((registration) => {
      // Check for updates periodically (every 5 minutes)
      setInterval(() => registration.update(), 5 * 60 * 1000);
      
      // Handle updates
      registration.addEventListener('updatefound', () => {
        const newWorker = registration.installing;
        newWorker?.addEventListener('statechange', () => {
          if (newWorker.state === 'installed' && navigator.serviceWorker.controller) {
            console.log('🔧 SW: New version available, will activate on reload');
          }
        });
      });
    })
    .catch((err) => {
      // SW registration failed - not critical, continue without caching
      console.warn('🔧 SW: Registration failed:', err.message);
    });
}

// Track boot timing
const bootStartTime = performance.now();
window.acBOOT_START_TIME = bootStartTime;

// 🧾 Boot telemetry (logs to /api/boot-log)
const bootTelemetry = (() => {
  const bootId = (crypto?.randomUUID?.() || `${Date.now()}-${Math.random().toString(16).slice(2)}`);
  const queue = [];
  let flushTimer = null;
  let started = false;
  const meta = {
    bootId,
    startedAt: new Date().toISOString(),
    href: typeof location !== "undefined" ? location.href : null,
    host: typeof location !== "undefined" ? location.host : null,
    path: typeof location !== "undefined" ? location.pathname : null,
    search: typeof location !== "undefined" ? location.search : null,
    hash: typeof location !== "undefined" ? location.hash : null,
    userAgent: navigator?.userAgent,
    language: navigator?.language,
    timezone: Intl?.DateTimeFormat?.().resolvedOptions?.().timeZone,
    embedded: window !== window.top,
    packMode: Boolean(window.acPACK_MODE),
    localDev: typeof location !== "undefined" && location.hostname === "localhost",
    user: window.acUSER ? {
      sub: window.acUSER?.sub,
      handle: window.acUSER?.handle,
    } : null,
  };

  async function sendBootEvent(phase, data = {}) {
    try {
      await fetch("/api/boot-log", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ bootId, phase, meta, data }),
        keepalive: true,
      });
    } catch {
      // ignore telemetry errors
    }
  }

  function enqueue(message, level = "info") {
    const elapsed = Math.round(performance.now() - bootStartTime);
    queue.push({ message, level, elapsed, at: Date.now() });
    if (queue.length >= 10) flush("batch");
    else if (!flushTimer) flushTimer = setTimeout(() => flush("timer"), 1500);
  }

  async function flush(reason = "manual") {
    if (flushTimer) {
      clearTimeout(flushTimer);
      flushTimer = null;
    }
    if (!queue.length) return;
    const events = queue.splice(0, queue.length);
    await sendBootEvent("log", { reason, events });
  }

  return {
    bootId,
    meta,
    start: async () => {
      if (started) return;
      started = true;
      await sendBootEvent("start", { bootStartTime: Date.now() });
    },
    enqueue,
    flush,
    error: async (error) => {
      await sendBootEvent("error", { error });
    },
    complete: async (data = {}) => {
      await flush("complete");
      await sendBootEvent("complete", data);
    },
  };
})();

bootTelemetry.start();

// Get the boot canvas element (if present in the DOM)
const bootCanvas = document.getElementById("boot-canvas");
const MAX_BOOT_LINES = 12; // Maximum lines to show
let lastBootMessage = ""; // Track last message to prevent duplicates

// Helper to send boot progress messages to parent and update the UI overlay
// Store timing data globally for debugging
window._bootTimings = window._bootTimings || [];

// Buffer boot logs until the boot canvas logger is ready
const pendingBootMessages = [];
let bootCanvasReady = false;
let bootCanvasPoll = null;

function flushBootCanvasLogs() {
  if (bootCanvasReady) return;
  if (window.acBOOT_LOG_CANVAS) {
    bootCanvasReady = true;
    if (pendingBootMessages.length > 0) {
      for (const msg of pendingBootMessages) {
        window.acBOOT_LOG_CANVAS(msg);
      }
      pendingBootMessages.length = 0;
    }
    if (bootCanvasPoll) {
      clearInterval(bootCanvasPoll);
      bootCanvasPoll = null;
    }
  }
}

function ensureBootCanvasPoll() {
  if (bootCanvasReady || bootCanvasPoll) return;
  bootCanvasPoll = setInterval(flushBootCanvasLogs, 50);
  // Stop polling after 30s to avoid runaway timers in edge cases
  setTimeout(() => {
    if (bootCanvasPoll && !bootCanvasReady) {
      clearInterval(bootCanvasPoll);
      bootCanvasPoll = null;
    }
  }, 30000);
}

function bootLog(message) {
  // Skip logging in PACK mode (NFT bundles should be silent)
  if (window.acPACK_MODE) return;
  
  // Skip verbose console logging when embedded (kidlisp.com iframe)
  const isEmbedded = window !== window.top;
  
  // Prevent duplicate messages
  if (message === lastBootMessage) return;
  lastBootMessage = message;
  
  const elapsed = Math.round(performance.now() - bootStartTime);
  // Only log to console when not embedded (reduce noise in kidlisp.com)
  if (!isEmbedded) {
    console.log(`🚀 [BOOT] ${message} (+${elapsed}ms)`);
  }
  window._bootTimings.push({ message, elapsed });
  bootTelemetry.enqueue(message, "info");
  
  // Update the boot canvas (via the canvas animation system)
  if (window.acBOOT_LOG_CANVAS) {
    window.acBOOT_LOG_CANVAS(message);
  } else {
    pendingBootMessages.push(message);
    ensureBootCanvasPoll();
  }
  
  // Also send to parent for embedded contexts
  if (window.parent) {
    window.parent.postMessage({ type: "boot-log", message }, "*");
  }
}

// Hide the boot log overlay (called when boot completes)
function hideBootLog() {
  if (window.acBootCanvas?.hide) {
    window.acBootCanvas.hide();
  }
  const elapsedTotal = Math.round(performance.now() - bootStartTime);
  bootTelemetry.complete({
    elapsedTotal,
    timings: window._bootTimings || [],
  });
}

// Show connection error and retry UI
let retryCount = 0;
const MAX_RETRIES = 5;
const RETRY_DELAY = 2000;

function showConnectionError(error) {
  retryCount++;
  const message = retryCount <= MAX_RETRIES 
    ? `⚠️ connection error - retrying (${retryCount}/${MAX_RETRIES})...`
    : `❌ connection failed - please refresh`;
  bootLog(message);
  bootTelemetry.error({
    message: error?.message || String(error),
    retryCount,
    maxRetries: MAX_RETRIES,
  });
  
  if (retryCount <= MAX_RETRIES) {
    setTimeout(() => {
      bootLog(`🔄 retry attempt ${retryCount}...`);
      window.location.reload();
    }, RETRY_DELAY);
  }
}

// Global error handler for module load failures
window.addEventListener('error', (event) => {
  if (event.message?.includes('net::ERR_') || 
      event.message?.includes('Failed to fetch') ||
      event.message?.includes('timed out') ||
      event.message?.includes('NetworkError')) {
    showConnectionError(event.error || event.message);
    event.preventDefault();
  }
});

// Handle unhandled promise rejections (for dynamic imports)
window.addEventListener('unhandledrejection', (event) => {
  const reason = event.reason?.message || String(event.reason);
  if (reason.includes('net::ERR_') || 
      reason.includes('Failed to fetch') ||
      reason.includes('timed out') ||
      reason.includes('NetworkError')) {
    showConnectionError(event.reason);
    event.preventDefault();
  }
  
  // Detect stale module cache errors (export not found, usually after deploy)
  // These require a hard refresh to clear the browser's module cache
  if (reason.includes('does not provide an export') ||
      reason.includes('SyntaxError') ||
      reason.includes('Unexpected token')) {
    showFatalBootError(event.reason);
    event.preventDefault();
  }
});

// Show fatal boot error with red flash and auto-refresh
let fatalErrorCount = 0;
const MAX_FATAL_RETRIES = 2;

function showFatalBootError(error) {
  fatalErrorCount++;
  const errorMsg = error?.message || String(error);
  
  // Trigger red error mode on boot canvas
  if (window.acBootCanvas?.setErrorMode) {
    window.acBootCanvas.setErrorMode(true, errorMsg);
  }
  
  bootLog(`❌ module error - refreshing...`);
  bootTelemetry.error({
    type: 'fatal-module-error',
    message: errorMsg,
    retryCount: fatalErrorCount,
    maxRetries: MAX_FATAL_RETRIES,
  });
  
  if (fatalErrorCount <= MAX_FATAL_RETRIES) {
    // Flash red for a moment, then hard refresh to clear module cache
    setTimeout(() => {
      // Force hard refresh by adding cache-bust param
      const url = new URL(window.location.href);
      url.searchParams.set('_bust', Date.now().toString());
      window.location.replace(url.toString());
    }, 1500);
  } else {
    bootLog(`❌ boot failed - please clear cache`);
  }
}

// Expose hideBootLog globally so bios can call it when fully ready
window.acHIDE_BOOT_LOG = hideBootLog;

// Expose bootLog globally so bios and disk can update the overlay
window.acBOOT_LOG = bootLog;

// Fetch moods-of-the-day for the boot canvas (fallback in case HTML boot didn't set it)
async function fetchBootMoodOfDay() {
  if (!window.acBootCanvas || window.acBootCanvas.motd) return;
  try {
    const res = await fetch("/api/mood/moods-of-the-day", {
      cache: "no-store",
      headers: { Accept: "application/json" },
    });
    if (!res.ok) return;
    const data = await res.json();
    if (data?.mood) {
      window.acBootCanvas.motd = data.mood;
      if (data.handle) window.acBootCanvas.motdHandle = data.handle;
    }
  } catch {
    // ignore boot mood errors
  }
}

fetchBootMoodOfDay();

// 📊 Auth0/Session timing telemetry - exposed globally for CDP inspection
window.acAuthTiming = {
  bootStart: bootStartTime,
  auth0ScriptLoadStart: null,
  auth0ScriptLoadEnd: null,
  auth0ClientCreateStart: null,
  auth0ClientCreateEnd: null,
  isAuthenticatedStart: null,
  isAuthenticatedEnd: null,
  getTokenStart: null,
  getTokenEnd: null,
  getUserStart: null,
  getUserEnd: null,
  userExistsFetchStart: null,
  userExistsFetchEnd: null,
  sessionStartedSent: null,
  // Computed durations (populated after boot)
  durations: {},
  // Helper to compute all durations
  computeDurations() {
    const t = window.acAuthTiming;
    const d = t.durations;
    if (t.auth0ScriptLoadStart && t.auth0ScriptLoadEnd) 
      d.scriptLoad = t.auth0ScriptLoadEnd - t.auth0ScriptLoadStart;
    if (t.auth0ClientCreateStart && t.auth0ClientCreateEnd)
      d.clientCreate = t.auth0ClientCreateEnd - t.auth0ClientCreateStart;
    if (t.isAuthenticatedStart && t.isAuthenticatedEnd)
      d.isAuthenticated = t.isAuthenticatedEnd - t.isAuthenticatedStart;
    if (t.getTokenStart && t.getTokenEnd)
      d.getToken = t.getTokenEnd - t.getTokenStart;
    if (t.getUserStart && t.getUserEnd)
      d.getUser = t.getUserEnd - t.getUserStart;
    if (t.userExistsFetchStart && t.userExistsFetchEnd)
      d.userExistsFetch = t.userExistsFetchEnd - t.userExistsFetchStart;
    if (t.bootStart && t.sessionStartedSent)
      d.totalToSessionStarted = t.sessionStartedSent - t.bootStart;
    return d;
  },
  // Pretty print summary
  summary() {
    const d = this.computeDurations();
    return `Auth0 Timing:
  Script Load: ${d.scriptLoad?.toFixed(0) || 'N/A'}ms
  Client Create: ${d.clientCreate?.toFixed(0) || 'N/A'}ms
  isAuthenticated: ${d.isAuthenticated?.toFixed(0) || 'N/A'}ms
  getTokenSilently: ${d.getToken?.toFixed(0) || 'N/A'}ms
  getUser: ${d.getUser?.toFixed(0) || 'N/A'}ms
  userExists fetch: ${d.userExistsFetch?.toFixed(0) || 'N/A'}ms
  Total to session:started: ${d.totalToSessionStarted?.toFixed(0) || 'N/A'}ms`;
  }
};

// Listen for retry messages from parent (VS Code extension)
window.addEventListener('message', (event) => {
  if (event.data?.type === 'boot-retry') {
    bootLog(`🔄 reconnecting (${event.data.attempt}/${event.data.maxAttempts})...`);
  }
});

bootLog("initializing aesthetic.computer");

// Check if we're embedded in kidlisp.com or similar iframe context
// If so, we'll wait for theme from parent instead of using OS preference
const isEmbeddedInKidlisp = window.parent !== window && 
  (window.location.search.includes('nolabel=true') || window.location.search.includes('nogap=true'));
if (isEmbeddedInKidlisp) {
  window.acWAIT_FOR_PARENT_THEME = true;
}

// Alert the parent we are initialized (but not fully ready yet).
if (window.parent !== window) {
  window.parent.postMessage({ type: "init" }, "*");
  // Send ready message early so VS Code extension doesn't keep reloading
  window.parent.postMessage({ type: "ready" }, "*");
  // Send kidlisp-ready for editor controls (boot is complete enough to accept commands)
  window.parent.postMessage({ type: "kidlisp-ready", ready: true }, "*");
}

// Early message listener for kidlisp messages that arrive before full boot
// (The main `receive` listener is added at the end of boot.mjs)
const earlyKidlispQueue = [];
function earlyKidlispReceiver(e) {
  const type = e.data?.type;
  if (type === "kidlisp-console-enable") {
    earlyKidlispQueue.push({ type: "kidlisp-console-enable" });
  }
}
window.addEventListener("message", earlyKidlispReceiver);

// Process early queued messages once acSEND is available
function processEarlyKidlispQueue() {
  if (earlyKidlispQueue.length === 0) return;
  if (window.acSEND) {
    for (const msg of earlyKidlispQueue) {
      if (msg.type === "kidlisp-console-enable") {
        window.__acKidlispConsoleEnabled = true;
        window.acSEND({ type: "kidlisp-console-enable" });
        window.parent.postMessage({ type: "kidlisp-console-enabled" }, "*");
      }
    }
    earlyKidlispQueue.length = 0;
  } else {
    setTimeout(processEarlyKidlispQueue, 50);
  }
}

// 🎹 DAW mode: Send ready signal to Max/MSP via jweb~ outlet
// This triggers the Live API sync (tempo, transport observers)
if (window.max?.outlet) {
  window.max.outlet("ready", 1);
  console.log("🎹 Sent ready signal to Max");
}

// List of legitimate query parameters that should be preserved
const LEGITIMATE_PARAMS = [
  'icon', 'preview', 'signup', 'supportSignUp', 'success', 'code', 
  'supportForgotPassword', 'message', 'vscode', 'nogap', 'nolabel', 
  'density', 'zoom', 'duration', 'session-aesthetic', 'session-sotce', 'notice', 'tv', 'highlight',
  'noauth', 'nocache', 'daw', 'width', 'height', 'desktop', 'device', 'perf', 'auto-scale', 'solo'
];

// Auth0 parameters that need to be temporarily processed but then removed
const AUTH0_PARAMS = ['state', 'error', 'error_description'];

// Extract only legitimate query parameters from URL, preserving kidlisp ? characters in path
function extractLegitimateParams(fullUrl) {
  const params = new URLSearchParams();
  
  // Look for legitimate parameters at the end of the URL
  for (const paramName of LEGITIMATE_PARAMS) {
    const regex = new RegExp(`[?&]${paramName}=([^&]*)`);
    const match = fullUrl.match(regex);
    if (match) {
      params.set(paramName, decodeURIComponent(match[1]));
    }
    
    // Also check for boolean parameters (without =value)
    const boolRegex = new RegExp(`[?&]${paramName}(?=[&]|$)`);
    if (boolRegex.test(fullUrl)) {
      params.set(paramName, 'true');
    }
  }
  
  return params;
}

// Remove Auth0 parameters from URL
function cleanAuth0Params(url) {
  const urlObj = new URL(url);
  
  // Always remove these Auth0 parameters
  AUTH0_PARAMS.forEach(param => {
    urlObj.searchParams.delete(param);
  });
  
  // Only remove 'code' if 'state' is also present (indicating Auth0 callback)
  if (urlObj.searchParams.has('state')) {
    urlObj.searchParams.delete('code');
  }
  
  return urlObj.pathname + (urlObj.searchParams.toString() ? '?' + urlObj.searchParams.toString() : '');
}

// Create a clean URL without legitimate query parameters
function createCleanUrl(fullUrl) {
  let cleanUrl = fullUrl;
  
  // Remove legitimate parameters but preserve ? in kidlisp content
  for (const paramName of LEGITIMATE_PARAMS) {
    const regexWithValue = new RegExp(`[?&]${paramName}=([^&]*)`);
    const regexBool = new RegExp(`[?&]${paramName}(?=[&]|$)`);
    cleanUrl = cleanUrl.replace(regexWithValue, '');
    cleanUrl = cleanUrl.replace(regexBool, '');
  }
  
  // Clean up any trailing ? or & that might be left
  cleanUrl = cleanUrl.replace(/[?&]+$/, '');
  
  return cleanUrl;
}

const legitParams = extractLegitimateParams(window.location.href);
const previewOrIcon =
  legitParams.has("icon") ||
  legitParams.has("preview");

window.acPREVIEW_OR_ICON = previewOrIcon;

const url = new URL(location);

function cleanUrlParams(url, params) {
  const queryString = params.toString();
  history.pushState(
    {},
    "",
    url.pathname + (queryString ? "?" + queryString : ""),
  );
}

// Test localStorage access early to detect sandbox restrictions
bootLog("checking storage access");
let localStorageBlocked = false;
try {
  // Test both read and write access
  const testKey = 'ac-sandbox-test';
  localStorage.setItem(testKey, 'test');
  localStorage.getItem(testKey);
  localStorage.removeItem(testKey);
} catch (err) {
  localStorageBlocked = true;
  console.warn('🏜️ localStorage access blocked (sandboxed iframe):', err.message);
}

// Test sessionStorage access
let sessionStorageBlocked = false;
try {
  const testKey = 'ac-session-test';
  sessionStorage.setItem(testKey, 'test');
  sessionStorage.getItem(testKey);
  sessionStorage.removeItem(testKey);
} catch (err) {
  sessionStorageBlocked = true;
  console.warn('🏜️ sessionStorage access blocked (sandboxed iframe):', err.message);
}

// Make storage state globally available
window.acLOCALSTORAGE_BLOCKED = localStorageBlocked;
window.acSESSIONSTORAGE_BLOCKED = sessionStorageBlocked;

// Safe localStorage functions
function safeLocalStorageGet(key) {
  if (localStorageBlocked) return null;
  try {
    return localStorage.getItem(key);
  } catch (err) {
    console.warn('localStorage access failed:', err.message);
    return null;
  }
}

function safeLocalStorageSet(key, value) {
  if (localStorageBlocked) return false;
  try {
    localStorage.setItem(key, value);
    return true;
  } catch (err) {
    console.warn('localStorage write failed:', err.message);
    return false;
  }
}

function safeLocalStorageRemove(key) {
  if (localStorageBlocked) return false;
  try {
    localStorage.removeItem(key);
    return true;
  } catch (err) {
    console.warn('localStorage remove failed:', err.message);
    return false;
  }
}

// Safe sessionStorage functions
function safeSessionStorageGet(key) {
  if (sessionStorageBlocked) return null;
  try {
    return sessionStorage.getItem(key);
  } catch (err) {
    console.warn('sessionStorage access failed:', err.message);
    return null;
  }
}

function safeSessionStorageSet(key, value) {
  if (sessionStorageBlocked) return false;
  try {
    sessionStorage.setItem(key, value);
    return true;
  } catch (err) {
    console.warn('sessionStorage write failed:', err.message);
    return false;
  }
}

function safeSessionStorageRemove(key) {
  if (sessionStorageBlocked) return false;
  try {
    sessionStorage.removeItem(key);
    return true;
  } catch (err) {
    console.warn('sessionStorage remove failed:', err.message);
    return false;
  }
}

// Make safe functions globally available
window.safeLocalStorageGet = safeLocalStorageGet;
window.safeLocalStorageSet = safeLocalStorageSet;
window.safeLocalStorageRemove = safeLocalStorageRemove;
window.safeSessionStorageGet = safeSessionStorageGet;
window.safeSessionStorageSet = safeSessionStorageSet;
window.safeSessionStorageRemove = safeSessionStorageRemove;

// 📧 Check to see if the user clicked an 'email' verified link.
{
  const params = extractLegitimateParams(window.location.href);
  if (
    params.get("supportSignUp") === "true" &&
    params.get("success") === "true" &&
    params.get("code") === "success"
  ) {
    params.delete("supportSignUp");
    params.delete("supportForgotPassword");
    params.delete("message");
    params.delete("success");
    params.delete("code");
    params.set("notice", "email-verified");
    cleanUrlParams(url, params);
  }
}

{
  const params = extractLegitimateParams(window.location.href);
  const vscode =
    params.get("vscode") === "true" ||
    safeLocalStorageGet("vscode") === "true";

  if (vscode) {
    params.delete("vscode");
    cleanUrlParams(url, params);
    window.acVSCODE = true;
    safeLocalStorageSet("vscode", "true");
  }
}

// Included as a <script> tag to boot the system on a webpage. (Loads `bios`)
// Dynamic import with retry for network resilience
// Uses WebSocket module loader with dependency bundling when available
const IMPORT_MAX_RETRIES = 3;
const IMPORT_RETRY_DELAY = 1500;

async function importWithRetry(modulePath, retries = IMPORT_MAX_RETRIES, useWsBundle = false) {
  const loader = window.acModuleLoader;
  const isLocalhost = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1';
  const retryDelay = isLocalhost ? 300 : IMPORT_RETRY_DELAY;
  let triedWs = false;

  const waitForWsConnection = async () => {
    if (!loader?.connecting) return;
    await Promise.race([
      loader.connecting,
      new Promise(resolve => setTimeout(resolve, 400))
    ]);
  };

  const tryLoadViaWs = async () => {
    if (!loader?.loadWithDeps) throw new Error('ws-unavailable');
    triedWs = true;
    await waitForWsConnection();
    if (!loader.connected) throw new Error('ws-not-connected');
    const relativePath = modulePath.replace(/^\.\//, '').split('?')[0];
    const blobUrl = await loader.loadWithDeps(relativePath, 5000);
    if (blobUrl && blobUrl.startsWith('blob:')) {
      return await import(blobUrl);
    }
    throw new Error('ws-missing-blob');
  };

  const tryLoadViaHttp = async () => import(modulePath);

  const raceToSuccess = async (promises) => new Promise((resolve, reject) => {
    let pending = promises.length;
    let lastErr = null;
    for (const promise of promises) {
      promise.then(resolve).catch((err) => {
        lastErr = err;
        pending -= 1;
        if (pending === 0) reject(lastErr);
      });
    }
  });

  // Parallel paths on localhost or when explicitly requested
  if (useWsBundle || isLocalhost) {
    try {
      return await raceToSuccess([tryLoadViaWs(), tryLoadViaHttp()]);
    } catch (err) {
      // Fall through to retry loop
    }
  } else if (loader?.connected) {
    // Opportunistic WS load even if boot decided not to wait
    try {
      return await tryLoadViaWs();
    } catch (err) {
      // Fall through to HTTP
    }
  }
  
  // Standard HTTP import with retry
  for (let attempt = 1; attempt <= retries; attempt++) {
    try {
      const module = await tryLoadViaHttp();
      return module;
    } catch (err) {
      const isNetworkError = err.message?.includes('net::ERR_') || 
                             err.message?.includes('Failed to fetch') ||
                             err.message?.includes('timed out') ||
                             err.message?.includes('NetworkError') ||
                             err.message?.includes('Content-Length');
      
      if (isNetworkError && !triedWs) {
        try {
          return await tryLoadViaWs();
        } catch (wsErr) {
          // Continue to retry loop
        }
      }

      if (attempt < retries && isNetworkError) {
        bootLog(`⚠️ module load failed - retry ${attempt}/${retries}`);
        await new Promise(resolve => setTimeout(resolve, retryDelay));
      } else {
        throw err;
      }
    }
  }
}

// Load core modules with retry support
// Use cache-busting query params to ensure fresh code on LAN/remote devices
// Skip cache-busting in PACK_MODE (NFT bundles use import maps with fixed paths)
// Skip cache-busting on localhost for faster dev reloads (use hard refresh when needed)
// In PACK_MODE, use bare specifiers (no ./) so import maps work from blob URLs
const isLocalhost = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1';
const cacheBust = window.acPACK_MODE || isLocalhost ? '' : `?v=${Date.now()}`;
const pathPrefix = window.acPACK_MODE ? '' : './';

// Helper to fetch and display source file in boot canvas
// Uses WebSocket module loader cache when available to avoid HTTP proxy errors
async function fetchAndShowSource(path, displayName) {
  if (window.acPACK_MODE) return; // Skip in PACK mode
  try {
    const loader = window.acModuleLoader;
    const modulePath = path.replace(/^\.\//, ''); // Strip ./ prefix
    
    // Try to get from bundle contents first (loaded via WebSocket, avoids HTTP entirely)
    if (loader?.getSourceContent) {
      const bundleContent = loader.getSourceContent(modulePath);
      if (bundleContent) {
        if (window.acBOOT_ADD_FILE) {
          window.acBOOT_ADD_FILE(displayName || path, bundleContent);
        }
        return;
      }
    }
    
    // Try IndexedDB cache next
    if (loader?.db) {
      const cached = await loader.getCachedModule(modulePath);
      if (cached?.content) {
        if (window.acBOOT_ADD_FILE) {
          window.acBOOT_ADD_FILE(displayName || path, cached.content);
        }
        return;
      }
    }
    
    // Fallback to HTTP fetch (may fail on localhost due to proxy issues)
    // Only try HTTP if not on localhost (where proxy is flaky)
    const isLocalhost = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1';
    if (!isLocalhost) {
      const response = await fetch(path);
      if (response.ok) {
        const text = await response.text();
        if (window.acBOOT_ADD_FILE) {
          window.acBOOT_ADD_FILE(displayName || path, text);
        }
      }
    }
  } catch (e) {
    // Ignore fetch errors - this is just visual
  }
}

let boot, parse, slug;
try {
  // 🚀 Wait for WebSocket module loader (max 800ms, already started at top of file)
  // This ensures we USE the WebSocket when available instead of falling back to HTTP
  const useWsBundle = await moduleLoaderReady;
  const loader = window.acModuleLoader;
  
  // Update boot canvas with session status
  if (typeof window.setSessionConnected === 'function') {
    window.setSessionConnected(useWsBundle || false);
  }
  
  bootLog("loading core modules" + (useWsBundle ? ' ⚡' : ''));
  
  const [biosModule, parseModule] = await Promise.all([
    importWithRetry(`${pathPrefix}bios.mjs${cacheBust}`, IMPORT_MAX_RETRIES, useWsBundle),
    importWithRetry(`${pathPrefix}lib/parse.mjs${cacheBust}`, IMPORT_MAX_RETRIES, useWsBundle)
  ]);
  boot = biosModule.boot;
  parse = parseModule.parse;
  slug = parseModule.slug;
  
  // Show source files in boot canvas (non-blocking)
  fetchAndShowSource(`${pathPrefix}bios.mjs`, 'bios.mjs');
  fetchAndShowSource(`${pathPrefix}lib/parse.mjs`, 'lib/parse.mjs');
} catch (err) {
  bootLog(`❌ critical module load failed`);
  showConnectionError(err);
  throw err;
}

// 0. Environment Configuration

let debug;

window.preloaded = false; // This gets set to true either automatically or
// manually by a disk. It's used by the thumbnail
// system to know when to take screenshots of each
// piece.

// Check for the debug constant in index.html which overrides all defaults.
// (And assumes we are not in a production environment)
if (window.acDEBUG === true || window.acDEBUG === false) {
  debug = window.acDEBUG;
} else if (
  window.location.hostname === "aesthetic.computer" ||
  window.location.hostname.endsWith(".ac") ||
  window.location.hostname === "m2w2.whistlegraph.com" ||
  window.acPACK_MODE // Disable debug mode in OBJKT packages
) {
  debug = false; // Turn debugging off by default in production and OBJKT mode.
  window.production = true;
} else {
  debug = true; // Turn debuging on by default everywhere else.
  // TODO: This should eventually be upgraded for IPFS exports.
}

// Check to see if we have a "#debug" hash.
if (window.location.hash === "#debug") debug = true;
if (window.location.hash === "#nodebug") debug = false;

window.acDEBUG = debug; // Set window.acDEBUG again just in case any code relies
// on it down the line. Should it need to? 22.07.15.00.21

// Get the current LAN host if it exists...
if (window.acDEBUG) window.acLAN_HOST = document.body.dataset.lanHost;

// 🥾 Boot the aesthetic.computer system.

// Or it can be set by a custom host...
if (location.hostname === "m2w2.whistlegraph.com")
  window.acSTARTING_PIECE = "wg~m2w2";
if (location.hostname === "botce.ac") window.acSTARTING_PIECE = "botce";
if (
  location.hostname === "wipppps.world" ||
  location.hostname === "www.wipppps.world"
) {
  window.acSTARTING_PIECE = "wipppps";
}

if (window.acSTARTING_PIECE === undefined) window.acSTARTING_PIECE = "prompt";

// In OBJKT/PACK mode and SPIDER mode, always use acSTARTING_PIECE instead of URL slug
let originalUrl = location.href;
let sluggedUrl = null;

if (!window.acPACK_MODE && !window.acSPIDER) {
  sluggedUrl = slug(originalUrl) || window.acSTARTING_PIECE;
} else {
  sluggedUrl = window.acSTARTING_PIECE;
}

const rawHashCandidate =
  typeof window !== "undefined" && window.location && window.location.hash
    ? window.location.hash.slice(1)
    : typeof window !== "undefined" && window.acSTARTING_HASH
      ? window.acSTARTING_HASH
      : "";
const hashCandidate = rawHashCandidate ? rawHashCandidate.trim() : "";
const loweredHashCandidate = hashCandidate.toLowerCase();
const hashLooksLikePaintingCode =
  hashCandidate.length > 0 &&
  /^[A-Za-z0-9]{3,12}$/.test(hashCandidate) &&
  loweredHashCandidate !== "debug" &&
  loweredHashCandidate !== "nodebug";

// Allow stample~#code to pass the code as a param instead of stripping it.
if (
  hashLooksLikePaintingCode &&
  sluggedUrl &&
  (sluggedUrl === "stample" || sluggedUrl.startsWith("stample~"))
) {
  sluggedUrl = `stample~%23${hashCandidate}`;
  if (typeof window !== "undefined") {
    window.acSTARTING_PIECE = "stample";
  }
}

// Allow clock with {#code} syntax - append the hash back into the braces
// e.g., clock~{#abc}cdefg where #abc}cdefg was stripped becomes clock~{%23abc}cdefg
// The hash contains everything after # including } and the notes
if (
  hashCandidate.length > 0 &&
  sluggedUrl &&
  (sluggedUrl.startsWith("clock~") || sluggedUrl.startsWith("clock:"))
) {
  // Check if the slug has an unclosed brace (the # was stripped from inside {#...})
  if (sluggedUrl.includes("{") && !sluggedUrl.includes("}")) {
    // The hash contains: "abc}cdefg" - we need to reconstruct as {%23abc}cdefg
    // Reinsert the hash with URL-encoded #
    sluggedUrl = sluggedUrl + `%23${hashCandidate}`;
  }
}

if (
  hashLooksLikePaintingCode &&
  (!sluggedUrl || sluggedUrl === "" || sluggedUrl === "prompt")
) {
  sluggedUrl = "painting";
  if (typeof window !== "undefined") {
    window.acSTARTING_PIECE = window.acSTARTING_PIECE || "painting";
    window.acSTARTING_HASH = hashCandidate;
  }
}

// if (window.acDEBUG || window.acVSCODE) {
//   console.log("🧭 URL pipeline", {
//     originalUrl,
//     sluggedUrl,
//   });
// }

const pieceToLoad = sluggedUrl;
bootLog(`parsing: ${pieceToLoad || 'prompt'}`);
const parsed = parse(pieceToLoad);

if (hashLooksLikePaintingCode && hashCandidate && !parsed.hash) {
  parsed.hash = hashCandidate;
}

// if (window.acDEBUG || window.acVSCODE) {
//   console.log("🧭 Parsed result", {
//     text: parsed?.text,
//     search: parsed?.search,
//     hash: parsed?.hash,
//     params: parsed?.params,
//   });
// }

// Preserve the original search parameters that were stripped by slug()
if (location.search) {
  parsed.search = location.search;
}

const bpm = 120; // Set the starting bpm. Is this still necessary?
// Wait for fonts to load before booting.

// Parse the query string to detect both ?nogap and ?nolabel parameters
const params = extractLegitimateParams(window.location.href);

// Set global flags for noauth and nocache
if (params.has("noauth")) window.acNOAUTH = true;
if (params.has("nocache")) window.acNOCACHE = true;

const nogap = params.has("nogap") || params.has("desktop") || location.search.includes("nogap") || location.host.includes("wipppps.world");

// Check for nolabel parameter (no localStorage persistence)
const nolabel = params.has("nolabel") || params.has("desktop") || location.search.includes("nolabel");

// Check for desktop mode (Electron app) - combines nogap + nolabel
const desktop = params.has("desktop") || location.search.includes("desktop");

// Check for device parameter early (needed for density handling)
// FF1/display device mode - auto-enables audio, combines tv+nogap+noauth
const deviceParamEarly = params.has("device") || location.search.includes("device");
const deviceMode = deviceParamEarly === true || deviceParamEarly === "true";

// Check for density parameter with localStorage persistence
// In device mode, DON'T persist to localStorage (device has its own density setting)
const densityParam = params.get("density");
let density;

if (densityParam) {
  // URL parameter provided - use it
  density = parseFloat(densityParam);
  // Only persist to localStorage if NOT in device mode
  if (!deviceMode) {
    safeLocalStorageSet("ac-density", density.toString());
  }
} else {
  // No URL parameter - check localStorage for saved density (only if not in device mode)
  if (!deviceMode) {
    const savedDensity = safeLocalStorageGet("ac-density");
    density = savedDensity ? parseFloat(savedDensity) : undefined;
  }
}

// Check for zoom parameter (device/navigator zoom level) with localStorage persistence
const zoomParam = params.get("zoom");
let zoom;

if (zoomParam) {
  // URL parameter provided - use it and save to localStorage
  zoom = parseFloat(zoomParam);
  safeLocalStorageSet("ac-zoom", zoom.toString());
} else {
  // No URL parameter - check localStorage for saved zoom
  const savedZoom = safeLocalStorageGet("ac-zoom");
  zoom = savedZoom ? parseFloat(savedZoom) : undefined;
}

// Check for duration parameter (for timed pieces with progress bars) with localStorage persistence
const durationParam = params.get("duration");
let duration;

if (durationParam) {
  // URL parameter provided - use it and save to localStorage
  duration = parseFloat(durationParam);
  // Duration is in seconds, validate it's a positive number
  if (isNaN(duration) || duration <= 0) {
    duration = undefined;
    safeLocalStorageRemove("ac-duration"); // Remove invalid duration
  } else {
    safeLocalStorageSet("ac-duration", duration.toString());
    // Optional: Remove duration from URL after consumption (uncomment next 2 lines)
    // params.delete("duration");
    // cleanUrlParams(url, params);
  }
} else {
  // No URL parameter - check localStorage for saved duration
  const savedDuration = safeLocalStorageGet("ac-duration");
  if (savedDuration) {
    duration = parseFloat(savedDuration);
    // Validate saved duration
    if (isNaN(duration) || duration <= 0) {
      duration = undefined;
      safeLocalStorageRemove("ac-duration");
    }
  }
}

// Check for tv parameter (disables touch/keyboard input for TV display mode)
const tvParam = params.has("tv") || location.search.includes("tv");
const tv = tvParam === true || tvParam === "true";

// Use the device mode flag computed earlier (for density handling)
const device = deviceMode;

// In device mode, default density to 8 if not explicitly set (matches FF1's typical setting)
if (device && density === undefined) {
  density = 8;
}

// Check for highlight parameter (enables HUD background highlighting with optional color)
const highlightParam = params.get("highlight");
const highlight = highlightParam ? (highlightParam === "true" ? true : highlightParam) : false;

// Check for perf parameter (enables KidLisp performance/FPS HUD overlay)
const perfParam = params.has("perf") || location.search.includes("perf");
const perf = perfParam === true || perfParam === "true";

// Check for auto-scale parameter (enables automatic density scaling to maintain target FPS)
const autoScaleParam = params.has("auto-scale") || location.search.includes("auto-scale");
const autoScale = autoScaleParam === true || autoScaleParam === "true";

// Check for solo parameter (locks piece in place, prevents navigation away)
const soloParam = params.has("solo") || location.search.includes("solo");
const solo = soloParam === true || soloParam === "true";

// Note: zoom parameter is available but not automatically applied to avoid text rendering issues
// It's passed to the boot function for selective use

// Apply nogap class immediately to prevent flash
if (nogap) {
  document.body.classList.add("nogap");
}

// Apply desktop class for Electron-specific styling
if (desktop) {
  document.body.classList.add("desktop");
}

// Force dark background when in VSCode (like when not embedded)
if (window.acVSCODE) {
  document.body.classList.add("vscode");
}

// Pass the parameters directly without stripping them
bootLog(`booting: ${parsed?.text || 'prompt'}`);
boot(parsed, bpm, { gap: nogap ? 0 : undefined, nolabel, density, zoom, duration, tv, highlight, desktop, device, perf, autoScale, solo }, debug);

// Start processing any early kidlisp messages that arrived before boot completed
processEarlyKidlispQueue();

// noauth mode should skip all authentication (for embedded iframes like kidlisp.com editor)
let sandboxed = (window.origin === "null" && !window.acVSCODE) || localStorageBlocked || sessionStorageBlocked || window.acPACK_MODE || window.acSPIDER || window.acNOAUTH;

// 🚀 Fast-path for anonymous users: check localStorage before loading Auth0
// Auth0 SPA SDK stores auth state with keys starting with "@@auth0spajs@@"
let likelyLoggedIn = false;
if (!sandboxed && !localStorageBlocked) {
  try {
    // Check if Auth0 has any cached auth state
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (key && key.startsWith('@@auth0spajs@@')) {
        likelyLoggedIn = true;
        break;
      }
    }
  } catch (e) {
    // localStorage access failed, assume not logged in
  }
}

// If noauth mode OR no Auth0 cache found, skip auth entirely
const skipAuth = window.acNOAUTH || (!likelyLoggedIn && !sandboxed && !location.search.includes('code=') && !location.search.includes('state='));

// Define login/logout functions when skipping initial auth, for on-demand login
if (skipAuth && !sandboxed && !window.acNOAUTH) {
  window.acLOGIN = async (mode) => {
    // Lazy-load Auth0 if not already loaded
    if (!window.auth0Client) {
      console.log("🔐 Loading Auth0 on-demand for login...");
      await loadAuth0Script();
      await setupAuth0Client();
    }
    const opts = { prompt: "login" };
    if (mode === "signup") opts.screen_hint = mode;
    window.auth0Client.loginWithRedirect({ authorizationParams: opts });
  };

  window.acLOGOUT = () => {
    console.log("⚠️ Not logged in, nothing to log out from.");
  };
}

if (skipAuth && !sandboxed) {
  if (window === window.top) console.log("🚀 Fast boot: no auth cache found, skipping Auth0");
  bootLog("skipping auth (anonymous user)");
  window.acDISK_SEND({
    type: "session:started",
    content: { user: null },
  });
} else if (window.acNOAUTH) {
  if (window === window.top) console.log("🔕 noauth mode: sending session:started immediately");
  window.acDISK_SEND({
    type: "session:started",
    content: { user: null },
  });
}

// #region 🔐 Auth0: Universal Login & Authentication
function loadAuth0Script() {
  return new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
    script.crossOrigin = "anonymous";
    document.head.appendChild(script);

    script.onload = () => {
      // console.log("🟢 Auth0 loaded.");
      resolve();
    };

    script.onerror = () => {
      console.error("🔴 Error loading Auth0.");
      reject(new Error("Script loading failed"));
    };
  });
}

// Setup Auth0 client - can be called on-demand or during boot
async function setupAuth0Client() {
  if (window.auth0Client) return window.auth0Client; // Already set up
  
  const clientId = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
  window.acAuthTiming.auth0ClientCreateStart = performance.now();
  bootLog("initializing auth0 client");

  const auth0Client = await window.auth0?.createAuth0Client({
    domain: "https://hi.aesthetic.computer",
    clientId,
    cacheLocation: "localstorage",
    useRefreshTokens: true,
    authorizationParams: { redirect_uri: window.location.origin },
  });
  window.acAuthTiming.auth0ClientCreateEnd = performance.now();
  bootLog(`auth0 client created (${Math.round(window.acAuthTiming.auth0ClientCreateEnd - window.acAuthTiming.auth0ClientCreateStart)}ms)`);

  window.auth0Client = auth0Client;
  return auth0Client;
}

// Call this function at the desired point in your application
// 🚀 Only load Auth0 if user likely has an auth session (or is doing OAuth callback)
if (!sandboxed && !skipAuth) {
  // 🛡️ Track whether session:started was sent in the auth flow
  // Hoisted so .catch() can use it as a fallback
  let _authSessionSent = false;
  window.acAuthTiming.auth0ScriptLoadStart = performance.now();
  bootLog("loading auth0 script");
  loadAuth0Script()
    .then(async () => {
      window.acAuthTiming.auth0ScriptLoadEnd = performance.now();
      bootLog(`auth0 script loaded (${Math.round(window.acAuthTiming.auth0ScriptLoadEnd - window.acAuthTiming.auth0ScriptLoadStart)}ms)`);
      if (!sandboxed && window.auth0 && !previewOrIcon) {
      const auth0Client = await setupAuth0Client();

      if (
        location.search.includes("state=") &&
        (location.search.includes("code=") ||
          location.search.includes("error="))
      ) {
        try {
          // console.log("🔐 Handling auth0 redirect...");
          await auth0Client.handleRedirectCallback();
        } catch (e) {
          console.error("🔐", e);
        }
        
        // Ensure Auth0 parameters are completely cleaned from URL
        // Skip in pack mode (sandboxed iframe)
        if (!window.acPACK_MODE && window.location?.origin !== 'null') {
          try {
            const cleanUrl = cleanAuth0Params(window.location.href);
            window.history.replaceState({}, document.title, cleanUrl);
          } catch (e) { /* Ignore in restricted context */ }
        }
      }

      const params = extractLegitimateParams(window.location.href);
      let param = params.get("session-aesthetic");

      {
        // Handle any sotce tenancy from the vscode extension by storing
        // the param in localStorage.
        let sotceParam = params.get("session-sotce");
        if (sotceParam && sotceParam !== "null" && sotceParam !== "retrieve") {
          safeLocalStorageSet("session-sotce", sotceParam);
        }
      }

      if (param === "null") {
        // Returns "null" as a string if logged out.
        safeLocalStorageRemove("session-aesthetic");
      } else if (param === "retrieve" || param === null) {
        // null as a type if a check is needed
        param = safeLocalStorageGet("session-aesthetic");
      }

      const sessionParams = param;
      let encodedSession = sessionParams;
      if (encodedSession === "null") encodedSession = undefined;
      let pickedUpSession;

      let isAuthenticated;
      if (!encodedSession) {
        window.acAuthTiming.isAuthenticatedStart = performance.now();
        isAuthenticated = await auth0Client.isAuthenticated();
        window.acAuthTiming.isAuthenticatedEnd = performance.now();
        bootLog(`auth0 isAuthenticated check (${Math.round(window.acAuthTiming.isAuthenticatedEnd - window.acAuthTiming.isAuthenticatedStart)}ms): ${isAuthenticated}`);
      }

      if (encodedSession) {
        const sessionJsonString = atob(decodeURIComponent(encodedSession));
        const session = JSON.parse(sessionJsonString);
        // Use the session information to authenticate, if it exists.
        // console.log("🥀 Session data:", session);
        if (session.accessToken && session.account) {
          // 🟩 Confirm the session access token and data is live here...
          try {
            const response = await fetch("/api/authorized", {
              method: "GET",
              headers: {
                Authorization: `Bearer ${session.accessToken}`,
              },
            });

            if (response.ok) {
              const result = await response.json();
              if (result.authorized) {
                // console.log("✅ Session is active and token is valid.");
                pickedUpSession = true;
              } else {
                // console.log("⚠️ Token is invalid or expired.");
                pickedUpSession = false;
              }
            } else {
              // console.log(
              //   "⚠️ Token validation request failed with status:",
              //   response.status,
              // );
              pickedUpSession = false;
            }
          } catch (err) {
            // console.error("❌ Error validating token:", err);
            pickedUpSession = false;
          }

          if (!pickedUpSession) {
            if (window.parent)
              window.parent.postMessage({ type: "logout" }, "*");
            // 🛡️ Still send session:started so disk.mjs can proceed with loading
            _authSessionSent = true;
            window.acDISK_SEND({
              type: "session:started",
              content: { user: null },
            });
            return;
          }

          window.acTOKEN = session.accessToken; // Only set using this flow.
          window.acUSER = {
            email: session.account.label,
            sub: session.account.id,
          };
          // Will get passed to the first message by the piece runner.
          // console.log("🌻 Picked up session!", window.acTOKEN, window.acUSER);
          _authSessionSent = true;
          window.acDISK_SEND({
            type: "session:started",
            content: { user: window.acUSER },
          });
        }

        if (sessionParams) {
          safeLocalStorageSet("session-aesthetic", encodedSession);
          params.delete("session-aesthetic"); // Remove the 'session' parameters
          params.delete("session-sotce");
          
          // Also ensure any remaining Auth0 parameters are cleaned
          AUTH0_PARAMS.forEach(param => params.delete(param));
          // Only remove 'code' if this appears to be an Auth0 callback
          if (params.has('state')) {
            params.delete('code');
          }
          
          const cleanUrl = url.pathname + (params.toString() ? "?" + params.toString() : "");
          history.pushState({}, "", cleanUrl);
        }
      }

      // const iframe = window.self !== window.top;

      window.acLOGIN = async (mode) => {
        const opts = { prompt: "login" }; // Never skip the login screen.
        if (mode === "signup") opts.screen_hint = mode;

        //if (!iframe) {
        auth0Client.loginWithRedirect({ authorizationParams: opts });
        //} else {
        // window.parent.postMessage(
        //   {
        //     type: "externallyAuthenticate",
        //     authUrl: "https://hi.aesthetic.computer",
        //   },
        //   "*",
        // );
        // console.log("🔐 Logging in with popup...");
        // auth0Client
        // .loginWithPopup()
        // .then(() => {
        // console.log("🔐 Logged in with popup");
        // })
        // .catch((error) => {
        // console.error("🔐 Popup login error:", error);
        // });
        //}
      };

      if (location.pathname === "/hi") window.acLOGIN();

      // Redirect to signup with a query parameter.
      const legitParams = extractLegitimateParams(window.location.href);
      if (legitParams.has("signup")) window.acLOGIN("signup");

      window.acLOGOUT = () => {
        if (isAuthenticated) {
          console.log("🔐 Logging out...");
          window.acSEND({
            type: "logout:broadcast:subscribe",
            content: { user: window.acUSER },
          });
          auth0Client.logout({
            logoutParams: { returnTo: window.location.origin },
          });
        } else {
          console.log("🔐 Already logged out!");
        }
      };

      if (isAuthenticated && !pickedUpSession) {
        try {
          // const options = localStorage.getItem("aesthetic:refresh-user")
          // ? { cacheMode: "off" }
          // : undefined;
          window.acAuthTiming.getTokenStart = performance.now();
          await auth0Client.getTokenSilently();
          window.acAuthTiming.getTokenEnd = performance.now();
          bootLog(`auth0 getTokenSilently (${Math.round(window.acAuthTiming.getTokenEnd - window.acAuthTiming.getTokenStart)}ms)`);
          // if (options) localStorage.removeItem("aesthetic:refresh-user");

          // console.log("🗝️ Got fresh auth token.");
        } catch (error) {
          console.log("🔐️ ❌ Unauthorized", error);
          console.error(
            "Failed to retrieve token silently. Logging out.",
            error,
          );
          // Redirect the user to logout if the token has failed.
          auth0Client.logout();
          isAuthenticated = false;
        }
      }

      if (isAuthenticated && !pickedUpSession) {
        // TODO: How long does this await actually take? 23.07.11.18.55
        window.acAuthTiming.getUserStart = performance.now();
        let userProfile = await auth0Client.getUser();
        window.acAuthTiming.getUserEnd = performance.now();
        bootLog(`auth0 getUser (${Math.round(window.acAuthTiming.getUserEnd - window.acAuthTiming.getUserStart)}ms)`);
        window.acAuthTiming.userExistsFetchStart = performance.now();
        const userExists = await fetch(
          `/user?from=${encodeURIComponent(userProfile.email)}&tenant=aesthetic&withHandle=true`,
        );
        const u = await userExists.json();
        window.acAuthTiming.userExistsFetchEnd = performance.now();
        bootLog(`userExists fetch (${Math.round(window.acAuthTiming.userExistsFetchEnd - window.acAuthTiming.userExistsFetchStart)}ms)`);
        if (!u.sub || !userProfile.email_verified) {
          try {
            await window.auth0Client.getTokenSilently({ cacheMode: "off" });
            userProfile = await auth0Client.getUser();
          } catch (err) {
            console.warn("🧔 Could not retrieve user from network.");
          }
        }
        // Merge handle from user lookup into the profile
        if (u.handle) {
          userProfile.handle = u.handle;
        }
        window.acUSER = userProfile; // Will get passed to the first message by the piece runner.
        window.acAuthTiming.sessionStartedSent = performance.now();
        _authSessionSent = true;
        window.acDISK_SEND({
          type: "session:started",
          content: { user: window.acUSER },
        });
      } else if (!pickedUpSession) {
        // console.log("🗝️ Not authenticated.");
        window.acAuthTiming.sessionStartedSent = performance.now();
        _authSessionSent = true;
        window.acDISK_SEND({
          type: "session:started",
          content: { user: null },
        });
      }

      window.acAuthTiming.computeDurations();
      const totalAuthTime = window.acAuthTiming.durations.totalToSessionStarted || 
                           (performance.now() - window.acAuthTiming.auth0ScriptLoadStart);
      bootLog(`auth0 complete (${Math.round(totalAuthTime)}ms total auth time)`);
      // Log full summary to console for debugging
      console.log(window.acAuthTiming.summary());
    } else {
      // 🛡️ Auth block was skipped (window.auth0 missing, sandboxed, or previewOrIcon)
      // Still need to send session:started so disk.mjs can proceed
      if (!_authSessionSent) {
        _authSessionSent = true;
        window.acDISK_SEND({
          type: "session:started",
          content: { user: null },
        });
      }
    }
  })
  .catch((error) => {
    console.error("Failed to load Auth0 script:", error);
    // 🛡️ Always send session:started even on auth failure so disk.mjs can proceed
    if (!_authSessionSent) {
      _authSessionSent = true;
      window.acDISK_SEND({
        type: "session:started",
        content: { user: null },
      });
    }
  });
} else {
  // Auth0 disabled in OBJKT mode
}
// #endregion

// ***Incoming Message Responder***

// - At the moment it is just for a work-in-progress figma widget but any
//   window messages to be received here.
// TODO: Finish FigJam Widget with iframe message based input & output.
//         See also: https://www.figma.com/plugin-docs/working-with-images/

// setTimeout(function() {
//   console.log("attempting a real focus...")
//   document.querySelector("#software-keyboard-input").focus();
// }, 4000);

function receive(event) {
  // console.log("🌟 Event:", event);
  if (event.data?.type === "aesthetic-parent:focused") {
    window.acSEND?.({ type: "aesthetic-parent:focused" });
    return;
  }
  if (event.data?.type === "figma-image-input") {
    // TODO: Build image with width and height.
    console.log("Bytes:", event.data.bytes.length);
    return;
  }
  if (event.data?.type === "clipboard:copy:confirmation") {
    // Receive a clipboard copy confirmation from a hosted frame.
    // (vscode extension)
    window.acSEND?.({ type: "copy:copied" });
    return;
  }
  if (event.data?.type === "setSession") {
    // Use the session information to authenticate, if it exists.
    const session = event.data.session;
    // console.log("🥀 Session data:", session);
    if (session.accessToken && session.account) {
      window.acTOKEN = session.accessToken; // Only set using this flow.
      window.acUSER = { name: session.account.label, sub: session.account.id }; // Will get passed to the first message by the piece runner.
      console.log("🌻 Picked up session!", window.acTOKEN, window.acUSER);
      window.acSEND({
        type: "session:started",
        content: { user: window.acUSER },
      });
    }
    return;
  } else if (event.data?.type === "clearSession" && window.acTOKEN) {
    window.location.reload();
    return;
  } else if (event.data?.type === "kidlisp-reload") {
    // Live reload from kidlisp.com editor
    const code = event.data.code;
    const codeId = event.data.codeId; // The $code identifier (e.g., "nece")
    const createCode = event.data.createCode; // Flag to enable code creation
    const authToken = event.data.authToken; // Token from kidlisp.com login
    const enableTrace = event.data.enableTrace; // Flag to enable execution trace
    if (code) {
      // Track the kidlisp code for console snapshots
      window.__acCurrentKidlispCode = code;
      // Only update codeId if explicitly provided - preserve existing value otherwise
      // This prevents stale values from being lost when reload messages don't include codeId
      if (codeId !== undefined) {
        window.__acCurrentKidlispCodeId = codeId || null;
      }
      // Enable/disable execution trace based on visualization mode
      window.__acKidlispTraceEnabled = enableTrace || false;
      // Reset snap timer so first snap happens ~5s after new code loads
      // (set to now, so the 5s countdown starts fresh)
      window._lastKidlispSnapTime = performance.now();
      window.acSEND({
        type: "piece-reload",
        content: { source: code, codeId: codeId, createCode: createCode, authToken: authToken, enableTrace: enableTrace }
      });
    }
    return;
  } else if (event.data?.type === "kidlisp-slide") {
    // Slide mode update - update source but preserve state/buffers
    const code = event.data.code;
    if (code) {
      window.__acCurrentKidlispCode = code;
      window.acSEND({
        type: "piece-slide",
        content: { source: code }
      });
    }
    return;
  } else if (event.data?.type === "kidlisp-audio") {
    // Audio data from jukebox - store directly on window for instant access
    // AND forward to disk worker for KidLisp
    const data = event.data.data;
    if (data) {
      // Store on window for direct access (lowest latency path)
      window.__acAudioData = data;
      // Also send to worker (for backward compatibility)
      window.acSEND({
        type: "kidlisp-audio",
        content: data
      });
    }
    return;
  } else if (event.data?.type === "kidlisp-pause") {
    window.acPAUSE?.();
    return;
  } else if (event.data?.type === "kidlisp-resume") {
    window.acRESUME?.();
    return;
  } else if (event.data?.type === "kidlisp-soft-stop") {
    // Soft stop - pause execution but keep the $code tab open
    // This allows resuming the same code without creating a new tab
    window.acPAUSE?.();
    window.acCLEAR_BAKE_LAYERS?.(); // Clear bake layers on stop
    return;
  } else if (event.data?.type === "kidlisp-stop") {
    window.acRESUME?.(); // Ensure we are running so we can load the empty piece.
    // Clear tracked kidlisp code when stopping
    window.__acCurrentKidlispCode = null;
    window.acSEND({
      type: "piece-reload",
      content: { source: "kidlisp", createCode: false }
    });
    return;
  } else if (event.data?.type === "kidlisp-reframe") {
    // Immediate reframe request from kidlisp.com (called after panel resize ends)
    if (window.acREFRAME) {
      window.acREFRAME();
    }
    return;
  } else if (event.data?.type === "kidlisp-ping") {
    // Respond to ping requests from kidlisp.com to confirm we're ready
    if (window.parent !== window) {
      window.parent.postMessage({
        type: 'kidlisp-ready',
        ready: true
      }, '*');
    }
    return;
  } else if (event.data?.type === "kidlisp-console-enable") {
    // Enable KidLisp-only console channel for kidlisp.com.
    // Check if already enabled (may have been handled by early listener)
    if (window.__acKidlispConsoleEnabled) {
      return;
    }
    window.__acKidlispConsoleEnabled = true;
    if (window.parent !== window) {
      window.parent.postMessage({ type: "kidlisp-console-enabled" }, "*");
    }

    // Forward into the disk worker too (KidLisp evaluation usually runs there).
    if (window.acSEND) {
      window.acSEND({ type: "kidlisp-console-enable" });
    } else {
      // Worker not ready yet - queue and retry when acSEND becomes available.
      const tryQueue = () => {
        if (window.acSEND) {
          window.acSEND({ type: "kidlisp-console-enable" });
        } else {
          setTimeout(tryQueue, 50);
        }
      };
      tryQueue();
    }
    return;
  } else if (event.data?.type === "kidlisp-theme") {
    // Theme sync from kidlisp.com editor
    const theme = event.data.theme; // 'light' or 'dark'
    const isDark = theme === 'dark';
    document.body.classList.toggle('light-theme', !isDark);
    document.documentElement.style.setProperty("color-scheme", theme);
    // Set flag to prevent OS theme changes from overriding
    window.acMANUAL_THEME_OVERRIDE = true;
    // Tell bios.mjs/worker about the theme change
    window.acSEND?.({ type: "dark-mode", content: { enabled: isDark } });
    return;
  } else if (event.data?.type === "keep-mint-prepare") {
    // Handle mint preparation request from kidlisp.com
    // This runs in the iframe which has auth cookies
    handleKeepMintPrepare(event.data);
    return;
  } else if (event.data?.startsWith?.("docs:")) {
    window.acSEND({
      type: "docs:link",
      content: event.data.split(":").pop(),
    });
    return;
  }
}

// Handle keep-mint-prepare from kidlisp.com
// Makes authenticated API call since iframe has session cookies
async function handleKeepMintPrepare(data) {
  const { imageDataUrl, imageFilename, metadataJson, tezosAddress, requestId } = data;
  
  try {
    // Create FormData for the API request
    const formData = new FormData();
    
    // Convert dataURL to blob
    const imageBlob = await fetch(imageDataUrl).then(r => r.blob());
    formData.append('image', imageBlob, imageFilename || 'mint-image.png');
    formData.append('metadata', metadataJson);
    formData.append('tezosAddress', tezosAddress);
    
    // Make authenticated API call
    const response = await fetch('/api/keep-mint', {
      method: 'POST',
      credentials: 'include',
      body: formData
    });
    
    const result = await response.json();
    
    if (response.ok && result.success) {
      // Send prepared data back to parent
      window.parent.postMessage({
        type: 'keep-mint-prepared',
        requestId,
        ...result
      }, '*');
    } else {
      window.parent.postMessage({
        type: 'keep-mint-error',
        requestId,
        error: result.message || result.error || 'Mint preparation failed'
      }, '*');
    }
  } catch (error) {
    window.parent.postMessage({
      type: 'keep-mint-error',
      requestId,
      error: error.message || 'Network error during mint preparation'
    }, '*');
  }
}

window.addEventListener("message", receive);

// Forward post-to-parent messages (used by kidlisp.mjs for trace and other messages)
window.addEventListener("message", (event) => {
  if (event.data?.type === "post-to-parent" && event.data.content && window.parent !== window) {
    window.parent.postMessage(event.data.content, "*");
  }
});

// 🔔 Subscribe to web / client notifications.
// TODO: Test this to make sure it's skipped in the native apps,
//       and factor it out. 24.02.26.19.29

async function initNotifications() {
try {
  const { initializeApp } = await import(
    "https://www.gstatic.com/firebasejs/10.8.0/firebase-app.js"
  );

  const { getMessaging, onMessage, getToken } = await import(
    "https://www.gstatic.com/firebasejs/10.8.0/firebase-messaging.js"
  );

  async function getFirebaseClientConfig() {
    // 1) Optional: page can inject config at runtime.
    if (window.AC_FIREBASE_CONFIG?.apiKey) return window.AC_FIREBASE_CONFIG;

    // 2) Default: fetch from Netlify function so secrets aren't in git.
    try {
      const res = await fetch("/api/firebase-config", {
        cache: "no-store",
        headers: { Accept: "application/json" },
      });
      if (!res.ok) return null;
      return await res.json();
    } catch {
      return null;
    }
  }

  async function setupNotifications() {
    const firebaseConfig = await getFirebaseClientConfig();
    if (!firebaseConfig?.apiKey) return;

    const app = initializeApp(firebaseConfig);
    const messaging = getMessaging(app);

    getToken(messaging, {
      vapidKey:
        "BDEh3JDD1xZo7eQU00TgsYb_o8ENJlpU-ovbZzWoCOu4AOeFJD8PVbZ3pif_7rMEk65Uj00-lwdXgc3qJVLp4ys",
    })
      .then((token) => {
        if (token) {
          // Send the token to your server and update the UI if necessary
          function subscribe(token, topic, then) {
            fetch("/api/subscribe-to-topic", {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({ token, topic }),
            })
              .then((response) => {
                if (!response.ok) throw new Error("Bad response.");
                return response.json(); // Parse the JSON in the response
              })
              .then((data) => {
                // console.log("Subcribed to:", data.topic);
                then?.(); // Subscribe to another topic if necessary.
              })
              .catch((error) => {
                console.error("🚫 Topic subscription error:", error);
              });
          }

          subscribe(token, "scream", () => subscribe(token, "mood"));

          onMessage(messaging, (payload) => {
            console.log(
              "🗨️ Client notification received. ",
              payload.notification,
            );
          });
        } else {
          console.warn("🔔 No registration token available.");
        }
      })
      .catch((err) => {
        // console.warn("🔔 An error occurred while retrieving token.", err);
      });
  }

  // Call the setup function  
  await setupNotifications();
} catch (err) {
  console.warn("🔥 Could not initialize firebase notifications:", err);
}
}

let notificationsInitialized = false;

window.acRequestNotifications = function requestNotifications() {
  if (notificationsInitialized) return;
  if (previewOrIcon || sandboxed) return;
  notificationsInitialized = true;
  initNotifications();
};
// Force redeploy Thu Dec 25 18:42:51 PST 2025
