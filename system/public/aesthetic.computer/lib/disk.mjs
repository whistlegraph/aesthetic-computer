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
import { nopaint_boot, nopaint_act, nopaint_is, nopaint_renderPerfHUD, nopaint_triggerBakeFlash } from "../systems/nopaint.mjs";
import { getPreserveFadeAlpha, setPreserveFadeAlpha } from "./fade-state.mjs";
import * as prompt from "../systems/prompt-system.mjs";
import * as world from "../systems/world.mjs";
import { headers } from "./headers.mjs";
import { logs } from "./logs.mjs";
import { soundWhitelist } from "./sound/sound-whitelist.mjs";

import { CamDoll } from "./cam-doll.mjs";

import { TextInput, Typeface } from "../lib/type.mjs";

import * as lisp from "./kidlisp.mjs";
import { isKidlispSource, fetchCachedCode, getCachedCode, initPersistentCache, getCachedCodeMultiLevel } from "./kidlisp.mjs"; // Add lisp evaluator.
import { qrcode as qr, ErrorCorrectLevel } from "../dep/@akamfoad/qr/qr.mjs";
import { microtype, MatrixChunky8 } from "../disks/common/fonts.mjs";

function matrixDebugEnabled() {
  if (typeof window !== "undefined" && window?.acMatrixDebug) return true;
  if (typeof globalThis !== "undefined" && globalThis?.acMatrixDebug)
    return true;
  return false;
}

function inkFloodLoggingEnabled() {
  if (typeof globalThis !== "undefined" && globalThis.AC_LOG_INK_COLORS) return true;
  if (typeof process !== "undefined" && process.env?.AC_LOG_INK_COLORS === "1") return true;
  return false;
}

function inkFloodLogPrefix() {
  let label = null;
  if (typeof process !== "undefined" && process.env?.AC_LOG_INK_LABEL) {
    label = process.env.AC_LOG_INK_LABEL;
  } else if (typeof globalThis !== "undefined" && globalThis.AC_LOG_INK_LABEL) {
    label = globalThis.AC_LOG_INK_LABEL;
  }
  return label ? `[${label}] ` : "";
}

function cloneColorForLog(color) {
  if (Array.isArray(color)) return Array.from(color);
  return color;
}

function cloneArgsForLog(args) {
  return Array.from(args).map((arg) => {
    if (Array.isArray(arg)) return Array.from(arg);
    if (arg && typeof arg === "object") {
      try {
        return JSON.parse(JSON.stringify(arg));
      } catch (err) {
        return String(arg);
      }
    }
    return arg;
  });
}

if (typeof globalThis !== "undefined") {
  if (globalThis.acMatrixDebug === undefined) {
    globalThis.acMatrixDebug = true;
  }
  if (typeof window !== "undefined" && window.acMatrixDebug === undefined) {
    window.acMatrixDebug = globalThis.acMatrixDebug;
  }
}

// Helper function to safely check for sandboxed environments in both main thread and worker contexts
function isSandboxed() {
  try {
    // In workers, window is undefined but we can still check self.origin or location
    if (typeof window !== "undefined") {
      return window.origin === "null";
    } else if (typeof self !== "undefined" && self.origin) {
      return self.origin === "null";
    } else if (typeof location !== "undefined" && location.origin) {
      return location.origin === "null";
    } else {
      return false; // Default to not sandboxed if we can't determine
    }
  } catch (err) {
    return false; // Default to not sandboxed if there's an error
  }
}

// Helper function to get safe protocol and hostname for URL construction
function getSafeUrlParts() {
  try {
    const sandboxed = isSandboxed();
    
    if (sandboxed) {
      return {
        protocol: "https:",
        hostname: "aesthetic.computer"
      };
    } else {
      // Try to get location info from various contexts
      let loc = null;
      if (typeof location !== "undefined") {
        loc = location;
      } else if (typeof self !== "undefined" && self.location) {
        loc = self.location;
      } else if (typeof window !== "undefined" && window.location) {
        loc = window.location;
      }
      
      if (loc) {
        return {
          protocol: loc.protocol,
          hostname: loc.hostname || loc.host
        };
      } else {
        // Fallback if no location available
        return {
          protocol: "https:",
          hostname: "aesthetic.computer"
        };
      }
    }
  } catch (err) {
    // Fallback to defaults if there's any error
    return {
      protocol: "https:",
      hostname: "aesthetic.computer"
    };
  }
}

let tf; // Active typeface global.

// Cache for loaded typefaces to avoid recreating them
const typefaceCache = new Map();

const DEFAULT_TYPEFACE_BLOCK_WIDTH = 6;
const DEFAULT_TYPEFACE_BLOCK_HEIGHT = 10;
const HUD_LABEL_TEXT_MARGIN = 0;
const KIDLISP_HUD_WRAP_CHARACTER_LIMIT = 28;
const MIN_HUD_WRAP_WIDTH = 120;

function resolveTypefaceInstance(typefaceRef) {
  if (!typefaceRef) return undefined;
  if (typefaceRef instanceof Typeface) return typefaceRef;
  if (typeof typefaceRef === "string") {
    if (typefaceCache.has(typefaceRef)) {
      return typefaceCache.get(typefaceRef);
    }
    const instance = new Typeface(typefaceRef);
    typefaceCache.set(typefaceRef, instance);
    return instance;
  }
  if (typeof typefaceRef === "object" && typeof typefaceRef?.getGlyph === "function") {
    return typefaceRef;
  }
  return undefined;
}

function ensureTypefaceLoaded(typeface) {
  if (!typeface || typeof typeface.load !== "function") return;
  const requiresLoad =
    typeface.data?.bdfFont ||
    typeface.name === "unifont" ||
    typeface.name === "MatrixChunky8";

  if (!requiresLoad) return;

  if (!typeface.__loadPromise) {
    typeface.__loadPromise = typeface.load($commonApi.net.preload, () => {
      if (typeof window !== "undefined" && window.$activePaintApi?.needsPaint) {
        window.$activePaintApi.needsPaint();
      }
    });
  }

  return typeface.__loadPromise;
}

function getTypefaceForMeasurement(typefaceName) {
  if (!typefaceName) return tf;
  const resolved = resolveTypefaceInstance(typefaceName);
  return resolved || tf;
}

function writeHudLabelText(
  $, 
  text,
  {
    x = 0,
    y = 0,
    typefaceName,
    preserveColors = true,
    bounds,
    wordWrap,
  } = {},
) {
  if (text === undefined || text === null || text === "") return;

  const content = preserveColors ? text : stripColorCodes(text);
  const effectiveBounds = typeof bounds === "number" && bounds > 0 ? bounds : undefined;
  const shouldWrap = wordWrap === undefined ? effectiveBounds !== undefined : wordWrap;
  
  $.write(
    content,
    { x, y },
    undefined,
    effectiveBounds,
    shouldWrap,
    typefaceName,
  );

  return $;
}

// Helper function to determine if a color is dark (needs light shadow) or light (needs dark shadow)
function isColorDark(colorStr) {
  
  let rgb;
  
  // Handle RGB comma-separated format like "0,0,0" or "255,0,0"
  if (typeof colorStr === 'string' && colorStr.includes(',')) {
    const parts = colorStr.split(',').map(n => parseInt(n.trim(), 10));
    rgb = parts;
  } 
  // Handle named colors
  else if (typeof colorStr === 'string') {
    const lower = colorStr.toLowerCase();
    const resolved = graph.findColor(lower);
    if (Array.isArray(resolved)) {
      rgb = resolved;
    } else {
      // For unknown colors, assume they're light
      return false;
    }
  }
  // Handle array format [r, g, b]
  else if (Array.isArray(colorStr)) {
    rgb = colorStr;
  }
  
  if (!rgb || rgb.length < 3) return false;
  
  // Calculate relative luminance using the formula from WCAG
  // https://www.w3.org/TR/WCAG20/#relativeluminancedef
  const [r, g, b] = rgb;
  const luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
  
  // If luminance is less than 0.5, it's a dark color
  return luminance < 0.5;
}

// Helper function to get shadow color for a specific text color
const BRIGHT_SHADOW_RGB = "220,220,220";

function getShadowColorForText(colorStr) {
  if (!colorStr) return "64,64,64"; // Default to dark gray shadow
  
  let rgb;
  let normalizedCommand = null;
  if (typeof colorStr === "string") {
    normalizedCommand = colorStr.trim().toLowerCase();
    // Treat complex command-based color strings (fade, scroll, random, etc.) as needing bright shadows
    if (/[:(]/.test(normalizedCommand) || normalizedCommand.includes("?")) {
      return BRIGHT_SHADOW_RGB;
    }
  }
  
  // Parse the color string to get RGB values
  if (typeof colorStr === 'string' && colorStr.includes(',')) {
    const parts = colorStr.split(',').map(n => parseInt(n.trim(), 10));
    rgb = parts;
  } 
  else if (typeof colorStr === 'string') {
    const lower = colorStr.toLowerCase();
    const resolved = graph.findColor(lower);
    if (Array.isArray(resolved)) {
      rgb = resolved;
    } else {
      // Fallback to dark gray for unknown colors
      return "64,64,64";
    }
  }
  else if (Array.isArray(colorStr)) {
    rgb = colorStr;
  }
  
  if (!rgb || rgb.length < 3) return "64,64,64";
  
  const [r, g, b] = rgb;

  // Handle very dark colors (near black) before channel-specific logic
  if (r <= 24 && g <= 24 && b <= 24) {
    return BRIGHT_SHADOW_RGB;
  }
  
  // 🎨 Special case: Detect RGB channel colors and return channel-tinted shadows
  // Red channel: R value, zero G and B
  if (r >= 0 && g === 0 && b === 0) {
    // Red channel shadow - darker red, stronger and more saturated
    const shadowR = Math.max(32, Math.round(r * 0.4)); // Keep at least 40% intensity
    return `${shadowR},0,0`; // Pure red shadow
  }
  
  // Green channel: zero R, G value, zero B
  if (r === 0 && g >= 0 && b === 0) {
    // Green channel shadow - darker green
    const shadowG = Math.max(32, Math.round(g * 0.4));
    return `0,${shadowG},0`; // Pure green shadow
  }
  
  // Blue channel: zero R, G value (75% of B), B value (deepskyblue pattern: 0, 191, 255)
  // Detects pattern where g = b * 0.75 (within rounding tolerance)
  if (r === 0 && b >= 0 && g >= 0) {
    const expectedG = Math.round(b * 0.75);
    // Allow 1 pixel tolerance for rounding
    if (Math.abs(g - expectedG) <= 1) {
      // Blue/deepskyblue channel shadow - darker blue with cyan tint
      const shadowG = Math.max(24, Math.round(g * 0.4));
      const shadowB = Math.max(32, Math.round(b * 0.4));
      return `0,${shadowG},${shadowB}`; // Cyan-blue shadow
    }
  }
  
  // Check if the color is dark - if so, lighten it for shadow
  if (isColorDark(colorStr)) {
    // Render near-white shadows under very dark text for readability
    if (r <= 24 && g <= 24 && b <= 24) {
      return "255,255,255";
    }

    // Lighten the color by mixing with white (stronger mix for dark colors)
    const factor = 0.85; // Mix 85% with white for a much lighter shadow
    const shadowR = Math.round(r + (255 - r) * factor);
    const shadowG = Math.round(g + (255 - g) * factor);
    const shadowB = Math.round(b + (255 - b) * factor);
    return `${shadowR},${shadowG},${shadowB}`;
  }
  
  // For light colors, darken by mixing with dark gray
  const factor = 0.6; // Mix 60% darker for shadow
  const shadowR = Math.round(r * (1 - factor));
  const shadowG = Math.round(g * (1 - factor));
  const shadowB = Math.round(b * (1 - factor));
  return `${shadowR},${shadowG},${shadowB}`;
}

// Helper function to replace color codes in text with shadow colors
function replaceColorCodesWithShadows(text, defaultTextColor = "white") {
  if (!text || !textContainsColorCodes(text)) return text;
  
  let currentTextColor = defaultTextColor;
  
  COLOR_CODE_MATCH_REGEX.lastIndex = 0;
  return text.replace(COLOR_CODE_MATCH_REGEX, (match, colorStr) => {
    if (!colorStr) return match;
    
    const normalized = colorStr.trim();
    const lower = normalized.toLowerCase();
    
    if (lower === "reset" || lower === "default" || lower === "base") {
      currentTextColor = defaultTextColor;
    } else {
      currentTextColor = normalized;
    }
    
    // Get the appropriate shadow color for this text color
    const shadowColor = getShadowColorForText(currentTextColor);
    
    // Return the shadow color code
    return `\\${shadowColor}\\`;
  });
}

function drawHudLabelText(
  $, 
  text,
  {
    x = 0,
    y = 0,
    typefaceName,
    textColor = "white",
    shadowColor,
    shadowOffsetX = 1,
    shadowOffsetY = 1,
    preserveColors = true,
    bounds,
    wordWrap,
  } = {},
) {
  if (!text) return;

  const containsColorCodes = textContainsColorCodes(text);
  const shouldPreserveColors = preserveColors || ((typefaceName === "MatrixChunky8" || typefaceName === "unifont") && containsColorCodes);

  const effectiveBounds = typeof bounds === "number" && bounds > 0 ? bounds : undefined;
  const shouldWrap = wordWrap === undefined ? effectiveBounds !== undefined : wordWrap;


  let effectiveShadowColor = shadowColor;
  if (!effectiveShadowColor && textColor) {
    effectiveShadowColor = getShadowColorForText(textColor);
  }
  if (!effectiveShadowColor) {
    effectiveShadowColor = "black";
  }


  const shouldRenderShadow = effectiveShadowColor && !(
    (typefaceName === "MatrixChunky8" || typefaceName === "unifont") &&
    matrixDebugEnabled()
  );

  if (shouldRenderShadow) {
    // If the text has color codes and we're preserving colors, use dynamic shadows
    if (shouldPreserveColors && containsColorCodes) {
      // Replace color codes with appropriate shadow colors
      const shadowText = replaceColorCodesWithShadows(text, textColor);
      
      writeHudLabelText($, shadowText, {
        x: x + shadowOffsetX,
        y: y + shadowOffsetY,
        typefaceName,
        preserveColors: true, // Keep the shadow color codes
        bounds: effectiveBounds,
        wordWrap: shouldWrap,
      });
    } else {
      // Original behavior: single shadow color for entire text
      $.ink(effectiveShadowColor);
      writeHudLabelText($, text, {
        x: x + shadowOffsetX,
        y: y + shadowOffsetY,
        typefaceName,
        preserveColors: false,
        bounds: effectiveBounds,
        wordWrap: shouldWrap,
      });
    }
  }

  if (textColor) {
    $.ink(textColor);
  }

  const content = shouldPreserveColors ? text : stripColorCodes(text);
  $.write(
    content,
    { x, y },
    undefined,
    effectiveBounds,
    shouldWrap,
    typefaceName,
  );
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
let TV_MODE = false; // Whether running in TV mode (disables touch/keyboard input)
let HIGHLIGHT_MODE = false; // Whether HUD highlighting is enabled
let HIGHLIGHT_COLOR = "64,64,64"; // Default highlight color (gray)
let AUDIO_SAMPLE_RATE = 0;
let debug = false; // This can be overwritten on boot.
let nopaintPerf = false; // Performance panel for nopaint system debugging (disabled by default)
let visible = true; // Is aesthetic.computer visibly rendering or not?

// 🎯 Global KidLisp Instance - Single source of truth for all KidLisp operations
let globalKidLispInstance = null;

// 🎨 Persistent first line color storage - survives instance resets
let persistentFirstLineColor = null;

// 🎨 Global function to store background color from KidLisp (worker-safe)
function storePersistentFirstLineColor(color) {
  persistentFirstLineColor = color;
  // Also store in window if available
  if (typeof window !== "undefined" && window.setPersistentFirstLineColor) {
    window.setPersistentFirstLineColor(color);
  }
}

// 🎨 Global function to get background color for reframe (worker-safe)
function getPersistentFirstLineColor() {

  return persistentFirstLineColor;
}

// 🎨 Make storage function globally available for KidLisp
if (typeof globalThis !== "undefined") {
  globalThis.storePersistentFirstLineColor = storePersistentFirstLineColor;
  globalThis.getPersistentFirstLineColor = getPersistentFirstLineColor;
}

// Global function to set persistent first line color
if (typeof window !== "undefined") {
  window.setPersistentFirstLineColor = function(color) {
    persistentFirstLineColor = color;
  };

  // Global function to get persistent first line color
  window.getPersistentFirstLineColor = function() {
    return persistentFirstLineColor;
  };
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

function fillExpandedWithSolidPixels(screen, width, height, oldWidth, oldHeight, rgba) {
  const [r, g, b] = rgba;

  const applyPixel = (x, y) => {
    const i = (y * width + x) * 4;
    screen.pixels[i] = r;
    screen.pixels[i + 1] = g;
    screen.pixels[i + 2] = b;
    screen.pixels[i + 3] = 255;
  };

  if (width > oldWidth) {
    for (let y = 0; y < height; y++) {
      for (let x = oldWidth; x < width; x++) {
        applyPixel(x, y);
      }
    }
  }

  if (height > oldHeight) {
    const bottomLimit = width > oldWidth ? oldWidth : width;
    for (let y = oldHeight; y < height; y++) {
      for (let x = 0; x < bottomLimit; x++) {
        applyPixel(x, y);
      }
    }
  }
}

function fillExpandedWithFadePixels(screen, width, height, oldWidth, oldHeight, fadeInfo) {
  const applyPixel = (x, y) => {
    const t = computeFadePositionForPixel(x, y, width, height, fadeInfo);
    const [r = 0, g = 0, b = 0, a = 255] = graph.getLocalFadeColor(t, x, y, fadeInfo);
    const i = (y * width + x) * 4;
    screen.pixels[i] = Math.max(0, Math.min(255, Math.round(r)));
    screen.pixels[i + 1] = Math.max(0, Math.min(255, Math.round(g)));
    screen.pixels[i + 2] = Math.max(0, Math.min(255, Math.round(b)));
    screen.pixels[i + 3] = Math.max(0, Math.min(255, Math.round(a || 255)));
  };

  if (width > oldWidth) {
    for (let y = 0; y < height; y++) {
      for (let x = oldWidth; x < width; x++) {
        applyPixel(x, y);
      }
    }
  }

  if (height > oldHeight) {
    const bottomLimit = width > oldWidth ? oldWidth : width;
    for (let y = oldHeight; y < height; y++) {
      for (let x = 0; x < bottomLimit; x++) {
        applyPixel(x, y);
      }
    }
  }
}

const projectionMode = location.search.indexOf("nolabel") > -1; // Skip loading noise.

import { setDebug } from "../disks/common/debug.mjs";
import { customAlphabet } from "../dep/nanoid/nanoid.js";
import { setPackMode, getPackMode, checkPackMode } from "./pack-mode.mjs";
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
  paint: ({ noise16Aesthetic, noise16Sotce, slug, wipe, ink, screen, net }) => {
    // TODO: Make this a boot choice via the index.html file?
    if (!projectionMode) {
      if (slug?.indexOf("wipppps") > -1) {
        wipe("black");
      } else if (slug?.indexOf("botce") > -1) {
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
  },
  beat: () => false, // Runs every bpm.
  act: () => false, // All user interaction.
  leave: () => false, // Before unload.
  receive: () => false, // Handle messages from BIOS (file drops, etc.)
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

      // 🎨 Broadcast painting update to other tabs
      $commonApi.broadcastPaintingUpdate("updated", {
        source: "leave",
        slug: $.slug
      });

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

// 🎞️ Piece-level FPS control (timing-based, doesn't affect global render loop)
let pieceFPS = null; // Target FPS for the current piece (null = no limit)
let lastPaintTime = 0; // Last time paint() was executed
let lastPaintOut = undefined; // Store last paintOut to preserve correct noPaint behavior
let shouldSkipPaint = false; // Whether to skip this frame's paint call
let pieceFrameCount = 0; // Frame counter that only increments when piece actually paints

let boot = defaults.boot;
let sim = defaults.sim;
let paint = defaults.paint;
let beat = defaults.beat;
let brush, lift, filter; // Only set in the `nopaint` system.
let act = defaults.act;
let leave = defaults.leave;
let receive = defaults.receive; // Handle messages from BIOS
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
let hideLabelViaTab = false; // Track if label is hidden via tab key toggle

// Helper function to toggle HUD visibility (used by Tab key and center tap)
function toggleHUDVisibility(isDoubleTap = false) {
  const currentTime = performance.now();
  
  if (hudAnimationState.animating) {
    // Animation in progress: reverse direction and continue from current position
    const elapsed = currentTime - hudAnimationState.startTime;
    const progress = Math.min(elapsed / hudAnimationState.duration, 1.0);
    
    // Flip the target state
    hudAnimationState.visible = !hudAnimationState.visible;
    
    // Update the remembered state for when QR fullscreen is turned off
    if (hudAnimationState.qrFullscreen) {
      hudAnimationState.cornersVisibleBeforeFullscreen = hudAnimationState.visible;
    }
    
    // Restart animation from current position by adjusting the start time
    // If we were 30% through a hide animation, start the show animation at 70% progress
    const remainingProgress = 1.0 - progress;
    hudAnimationState.startTime = currentTime - (remainingProgress * hudAnimationState.duration);
    
    // Special double-tap: immediately show HUD if hiding
    if (isDoubleTap && !hudAnimationState.visible) {
      hudAnimationState.animating = false;
      hudAnimationState.visible = true;
      hudAnimationState.opacity = 1.0;
      hudAnimationState.slideOffset = { x: 0, y: 0 };
      hudAnimationState.qrSlideOffset = { x: 0, y: 0 };
      
      // Update remembered state
      if (hudAnimationState.qrFullscreen) {
        hudAnimationState.cornersVisibleBeforeFullscreen = true;
      }
    }
    
    $commonApi.sound.synth({
      type: "sine",
      tone: hudAnimationState.visible ? 600 : 400,
      duration: 0.15,
      attack: 0.1,
      decay: 0.8,
      volume: 0.2,
    });
  } else {
    // No animation in progress: start new animation
    hudAnimationState.animating = true;
    hudAnimationState.startTime = currentTime;
    hudAnimationState.visible = !hudAnimationState.visible;
    
    // Update the remembered state for when QR fullscreen is turned off
    if (hudAnimationState.qrFullscreen) {
      hudAnimationState.cornersVisibleBeforeFullscreen = hudAnimationState.visible;
    }
    
    $commonApi.sound.synth({
      type: "sine",
      tone: hudAnimationState.visible ? 600 : 400,
      duration: 0.15,
      attack: 0.1,
      decay: 0.8,
      volume: 0.2,
    });
  }
}

// Helper function to toggle QR fullscreen mode for KidLisp pieces
function toggleQRFullscreen() {
  try {
    // Only allow QR fullscreen for KidLisp pieces that have QR codes
    const sourceCode = currentText || currentHUDTxt;
    console.log("🔍 [QR Toggle] sourceCode:", sourceCode, "currentPath:", currentPath);
    
    // Use the centralized KidLisp detection from kidlisp.mjs
    const isInlineKidlispPiece = (currentPath && lisp.isKidlispSource(currentPath) && !currentPath.endsWith('.lisp')) ||
                          currentPath === "(...)" ||
                          (sourceCode && sourceCode.startsWith("$")) ||
                          (currentPath && currentPath.includes("/disks/$")) ||
                          (sourceCode && lisp.isKidlispSource(sourceCode));
    
    console.log("🔍 [QR Toggle] isInlineKidlispPiece:", isInlineKidlispPiece);
    
    if (!isInlineKidlispPiece) {
      console.log("⚠️ [QR Toggle] Not a KidLisp piece, skipping QR fullscreen toggle");
      return;
    }
    
    if (!hudAnimationState.qrFullscreen) {
      // Turning ON fullscreen QR
      console.log("✅ [QR Toggle] Enabling fullscreen mode");
      
      // Remember current corner visibility state
      hudAnimationState.cornersVisibleBeforeFullscreen = hudAnimationState.visible;
      
      // If corners are currently visible, animate them out for fullscreen QR
      if (hudAnimationState.visible) {
        // Check if animation is already in progress
        if (hudAnimationState.animating) {
          // Animation in progress: reverse it
          const currentTime = performance.now();
          const elapsed = currentTime - hudAnimationState.startTime;
          const remaining = Math.max(0, hudAnimationState.duration - elapsed);
          hudAnimationState.startTime = currentTime - remaining;
        } else {
          // No animation in progress: start new animation
          hudAnimationState.animating = true;
          hudAnimationState.startTime = performance.now();
        }
        hudAnimationState.visible = false;
      }
      
      hudAnimationState.qrFullscreen = true;
      
      // Remove corner QR hitbox when going fullscreen
      send({ type: "button:hitbox:remove", content: "qr-corner" });
      
    } else {
      // Turning OFF fullscreen QR
      console.log("✅ [QR Toggle] Disabling fullscreen mode");
      hudAnimationState.qrFullscreen = false;
      
      // Remove fullscreen QR hitbox
      send({ type: "button:hitbox:remove", content: "qr-fullscreen" });
      
      // Restore corner visibility to what it was before fullscreen QR was activated
      if (hudAnimationState.cornersVisibleBeforeFullscreen && !hudAnimationState.visible) {
        // Check if animation is already in progress
        if (hudAnimationState.animating) {
          // Animation in progress: reverse it
          const currentTime = performance.now();
          const elapsed = currentTime - hudAnimationState.startTime;
          const remaining = Math.max(0, hudAnimationState.duration - elapsed);
          hudAnimationState.startTime = currentTime - remaining;
        } else {
          // No animation in progress: start new animation
          hudAnimationState.animating = true;
          hudAnimationState.startTime = performance.now();
        }
        hudAnimationState.visible = true;
      }
    }
    
    // Play sound feedback
    if ($commonApi?.sound?.synth) {
      $commonApi.sound.synth({
        type: "sine",
        tone: hudAnimationState.qrFullscreen ? 500 : 250,
        duration: 0.12,
        attack: 0.1,
        decay: 0.9,
        volume: 0.25,
      });
    }
  } catch (err) {
    console.error("❌ [QR Toggle] Error toggling QR fullscreen:", err);
  }
}

let hudAnimationState = {
  visible: !getPackMode(), // Hidden by default in OBJKT mode, visible otherwise
  animating: false,
  startTime: 0,
  duration: 500, // 500ms animation
  opacity: 1.0,
  slideOffset: { x: 0, y: 0 },     // HUD label offset (slides to top-left)
  qrSlideOffset: { x: 0, y: 0 },   // QR overlay offset (slides to bottom-right)
  labelWidth: 120,                 // HUD label width - updated when HUD is drawn
  labelHeight: 40,                 // HUD label height - updated when HUD is drawn
  qrSize: 80,                      // QR overlay size for bounding box animations
  lastTabTime: 0,                  // Track last tab press for double-tap detection
  qrFullscreen: false,             // Track if QR code is in fullscreen mode
  cornersVisibleBeforeFullscreen: true // Remember corner state before fullscreen QR
};

let module, loadedModule; // Currently loaded piece module code with an extra reference for `hotSwap`.

// 🎨 Enhanced painting change detection with immediate pixel monitoring
let lastPaintingHash = null;
let paintingChangeCheckInterval = null;
let frameBasedMonitoring = false;

// Performance optimization: Use requestAnimationFrame for more efficient monitoring
function enableFrameBasedMonitoring() {
  if (frameBasedMonitoring) return;
  frameBasedMonitoring = true;
  
  let lastDimensions = null;
  
  function checkPaintingChanges() {
    if ($commonApi.system?.painting && !$commonApi._processingBroadcast) {
      const currentHash = generatePaintingHash($commonApi.system.painting);
      const currentDimensions = {
        width: $commonApi.system.painting.width,
        height: $commonApi.system.painting.height
      };
      
      // Add detailed logging for nopaint interference detection
      const isNopaintActive = $commonApi.system?.nopaint?.is?.("painting");
      const nopaintBuffer = $commonApi.system?.nopaint?.buffer;
      
      if (lastPaintingHash !== null && currentHash !== lastPaintingHash) {
        // Check if this is a dimension change (resize operation)
        const isDimensionChange = lastDimensions && 
          (lastDimensions.width !== currentDimensions.width || 
           lastDimensions.height !== currentDimensions.height);
        
        // Skip broadcasting during active nopaint operations to prevent interference
        if (isNopaintActive) {
          console.log(`🚫 SKIPPING broadcast during nopaint operation to prevent live preview interference`);
        } else {
          // Immediate broadcast with appropriate source
          $commonApi.broadcastPaintingUpdateImmediate("frame_update", {
            source: isDimensionChange ? "resize" : "frame_monitor",
            hash: currentHash.substr(0,8)
          });
        }
      }
      
      lastPaintingHash = currentHash;
      lastDimensions = currentDimensions;
    }
    
    if (frameBasedMonitoring) {
      requestAnimationFrame(checkPaintingChanges);
    }
  }
  
  requestAnimationFrame(checkPaintingChanges);
}

// 🎨 Broadcast throttling to prevent infinite loops
let lastBroadcastTime = 0;
let broadcastThrottleDelay = 100; // Minimum ms between broadcasts
let currentPath,
  currentHost,
  currentSearch,
  currentColon,
  currentParams,
  currentHash,
  currentText,
  currentCode,
  currentHUDTxt,
  currentHUDPlainTxt,  // Plain text version without color codes
  currentOriginalCodeId, // Track original $code identifier for sharing
  currentHUDTextColor,
  currentHUDStatusColor = "red",
  currentHUDButton,
  currentHUDButtonActive = false, // Global flag to block other button interactions when HUD is active
  currentHUDButtonDirectTouch = false, // Track if HUD button was directly tapped (not rolled over)
  currentHUDScrub = 0,
  currentHUDLabelFontName,
  currentHUDLabelBlockWidth = tf?.blockWidth ?? DEFAULT_TYPEFACE_BLOCK_WIDTH,
  currentHUDLabelBlockHeight = tf?.blockHeight ?? DEFAULT_TYPEFACE_BLOCK_HEIGHT,
  currentHUDShareWidth = (tf?.blockWidth ?? DEFAULT_TYPEFACE_BLOCK_WIDTH) * "share ".length,
  currentHUDLabelMeasuredWidth = 0,
  currentHUDLeftPad = 0,
  currentHUDOffset,
  qrOverlayCache = new Map(), // Cache for QR overlays to prevent regeneration every frame
  hudLabelCache = null; // Cache for HUD label to prevent regeneration every frame

let lastMatrixChunkyHighlightLog = null;
let lastMatrixChunkyWriteDiagnosticLog = null;
let lastMatrixChunkyPixelLog = null;



// Helper functions to safely access window flags in both main thread and worker contexts
function isQROverlayCacheDisabled() {
  try {
    return typeof window !== 'undefined' && window.acDISABLE_QR_OVERLAY_CACHE;
  } catch (e) {
    return false; // Default to enabled (caching on)
  }
}

function isHUDLabelCacheDisabled() {
  try {
    return typeof window !== 'undefined' && window.acDISABLE_HUD_LABEL_CACHE;
  } catch (e) {
    return false; // Default to enabled if we can't check
  }
}

// Make cache globally accessible for character loading system
if (typeof window !== 'undefined') {
  window.qrOverlayCache = qrOverlayCache;
  
  // Clear caches if they are disabled from BIOS
  if (isQROverlayCacheDisabled()) {
    qrOverlayCache.clear();
    // console.log("🚫 QR overlay cache disabled and cleared from BIOS");
  }
  
  // if (isHUDLabelCacheDisabled()) {
  //   console.log("🚫 HUD label cache disabled from BIOS");
  // }
}
//currentPromptButton;

const COLOR_CODE_MATCH_REGEX = /\\([^\\]+)\\/g;
const COLOR_CODE_TEST_REGEX = /\\[^\\]+\\/;

// Utility function to strip color codes from text
function stripColorCodes(str) {
  if (!str) return str;
  // Remove all \\color\\ sequences including:
  // - Named colors: \\red\\, \\blue\\, \\cyan\\
  // - RGB values: \\255,20,147\\, \\192,192,192\\
  // - Complex patterns: \\color(args)\\
  COLOR_CODE_MATCH_REGEX.lastIndex = 0;
  return str.replace(
    COLOR_CODE_MATCH_REGEX,
    "",
  );
}

function textContainsColorCodes(str) {
  if (!str) return false;
  return COLOR_CODE_TEST_REGEX.test(str);
}

function hasKidLispMarkers(text) {
  if (!text) return false;
  return (
    text.includes("ink ") ||
    text.includes("line ") ||
    text.includes("box ") ||
    text.includes("circle ") ||
    text.includes("spin ") ||
    text.includes("(") ||
    textContainsColorCodes(text) ||
    /\d+s\.\.\./.test(text) ||
    /\?\s/.test(text)
  );
}

function detectKidLispPiece({ currentPath, currentHUDTxt, currentText, cleanText }) {
  const sourceCode = currentText || currentHUDTxt;

  return (
    (currentPath && lisp.isKidlispSource(currentPath) && !currentPath.endsWith('.lisp')) ||
    currentPath === "(...)" ||
    (sourceCode && sourceCode.startsWith("$")) ||
    (currentPath && (currentPath.includes("/disks/$") || currentPath.includes("$"))) ||
    (sourceCode && lisp.isKidlispSource && lisp.isKidlispSource(sourceCode)) ||
    hasKidLispMarkers(currentHUDTxt) ||
    textContainsColorCodes(cleanText) ||
    hasKidLispMarkers(sourceCode)
  );
}

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
  sfxKillReceivers = {},
  sfxDurationReceivers = {};
let $sampleCount = 0n;

const signals = []; // Easy messages from embedded DOM content.
const actAlerts = []; // Messages that get put into act and cleared after
// every frame.
let reframed = false;
let formReframing = false; // Just for 3D camera updates.

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

let storeRetrievalResolutions = {},
  storeDeletionResolutions = {};

// There are two instances of Socket that run in parallel...
let socket, socketStartDelay; // Socket server for each piece.

// ❤️‍🔥 TODO: Explose these somehow to the $commonApi.

// TODO: Extract `chat` into an external class.

const chatDebug =
  location.host === "local.aesthetic.computer" ||
  location.host === "localhost:8888" ||
  location.host === "aesthetic.local:8888";
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

// Duration tracking for playlist progress bar (similar to tape system)
let durationStartTime = null;  // When the piece started (null if no duration) - using performance.now()
let durationTotal = null;      // Total duration in seconds (null if no duration)
let durationProgress = 0;      // Current progress (0-1)
let durationCompleted = false; // Whether duration has completed
let durationBlinkState = false; // For blinking the completed bar
let pageLoadTime = performance.now(); // Time when the page first loaded - using performance.now()

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
  tapeFrameMode = false; // Whether we're recording based on frames instead of time
  tapeFrameStart = 0; // Starting frame count
  tapeFrameTarget = 0; // Target number of frames to record

  videoOnLeave = false;

  constructor() {}

  tapeTimerSet(durationOrFrames, time, isFrameMode = false) {
    if (isFrameMode) {
      // Frame-based recording
      this.tapeFrameMode = true;
      this.tapeFrameStart = Number($commonApi.paintCount || 0n);
      this.tapeFrameTarget = durationOrFrames;
      this.tapeTimerStart = null;
      this.tapeTimerDuration = null;
      
      console.log(`🎬 Starting frame-based recording: ${durationOrFrames} frames (starting at frame ${this.tapeFrameStart})`);
      
      // Set a longer failsafe for frame-based recording (frames could take a while)
      if (this.failsafeTimeout) {
        clearTimeout(this.failsafeTimeout);
      }
      
      this.failsafeTimeout = setTimeout(() => {
        console.warn(`🎬 ⚠️ Frame-based failsafe triggered! Recording may have stalled.`);
        this.tapeProgress = 0;
        this.tapeFrameMode = false;
        this.tapeFrameStart = 0;
        this.tapeFrameTarget = 0;
        
        if (typeof this.cut === 'function') {
          this.cut(() => {
            $commonApi.jump("video");
          });
        } else {
          console.warn(`🎬 ⚠️ Cut function not available in failsafe, manual jump to video`);
          $commonApi.jump("video");
        }
        this.failsafeTimeout = null;
      }, Math.max(30000, durationOrFrames * 1000)); // At least 30 seconds, or 1 second per frame
    } else {
      // Time-based recording (existing behavior)
      this.tapeFrameMode = false;
      this.tapeTimerStart = time;
      this.tapeTimerDuration = durationOrFrames;
      
      // Add a failsafe timer as backup in case tapeTimerStep stops being called
      if (this.failsafeTimeout) {
        clearTimeout(this.failsafeTimeout);
      }
      
      // Set a failsafe that will trigger cut if the normal timer fails
      this.failsafeTimeout = setTimeout(() => {
        console.warn(`🎬 ⚠️ Failsafe timer triggered! Normal timer may have failed.`);
        if (this.tapeTimerDuration && this.tapeTimerStart) {
          this.tapeProgress = 0;
          this.tapeTimerStart = null;
          this.tapeTimerDuration = null;
          
          if (typeof this.cut === 'function') {
            this.cut(() => {
              $commonApi.jump("video");
            });
          } else {
            console.warn(`🎬 ⚠️ Cut function not available in failsafe, manual jump to video`);
            $commonApi.jump("video");
          }
        }
        this.failsafeTimeout = null;
      }, (durationOrFrames + 1) * 1000); // Add 1 second buffer to the failsafe
    }
  }

  tapeTimerStep({ needsPaint, sound: { time } }) {
    if (this.tapeFrameMode) {
      // Frame-based recording
      if (!this.tapeFrameTarget) return;
      
      const currentFrame = Number($commonApi.paintCount || 0n);
      const framesPassed = currentFrame - this.tapeFrameStart;
      
      this.tapeProgress = framesPassed / this.tapeFrameTarget;
      needsPaint();
      
      // Check if we've reached or exceeded the target frame count
      if (framesPassed >= this.tapeFrameTarget) {
        // Clear failsafe since normal timer completed
        if (this.failsafeTimeout) {
          clearTimeout(this.failsafeTimeout);
          this.failsafeTimeout = null;
        }
        
        this.tapeProgress = 0;
        this.tapeFrameMode = false;
        this.tapeFrameStart = 0;
        this.tapeFrameTarget = 0;
        
        console.log(`🎬 Frame-based recording complete: ${framesPassed} frames recorded`);
        
        // Add safety check for callback existence
        if (typeof this.cut === 'function') {
          this.cut(() => {
            $commonApi.jump("video");
          });
        } else {
          console.warn(`🎬 ⚠️ Cut function not available, manual jump to video`);
          $commonApi.jump("video");
        }
      }
    } else {
      // Time-based recording (existing behavior)
      if (!this.tapeTimerDuration) return;
      
      // Enhanced timing with race condition protection
      this.tapeProgress = (time - this.tapeTimerStart) / this.tapeTimerDuration;
      needsPaint();
      
      const secondsOver =
        this.tapeProgress * this.tapeTimerDuration - this.tapeTimerDuration;
      
      // Run for an extra 150 milliseconds.
      if (this.tapeProgress >= 1 && secondsOver > 0.15) {
        // Clear failsafe since normal timer completed
        if (this.failsafeTimeout) {
          clearTimeout(this.failsafeTimeout);
          this.failsafeTimeout = null;
        }
        
        this.tapeProgress = 0;
        this.tapeTimerStart = null;
        this.tapeTimerDuration = null;
        
        // Add safety check for callback existence
        if (typeof this.cut === 'function') {
          this.cut(() => {
            $commonApi.jump("video");
          });
        } else {
          console.warn(`🎬 ⚠️ Cut function not available, manual jump to video`);
          $commonApi.jump("video");
        }
      }
    }
  }

  slate() {
    send({ type: "recorder:slate" }); // Kill the MediaRecorder instance.
    // TODO: Should printing and playing also be set to false?
    //$commonApi.rec.printing = false; // "
    $commonApi.rec.recording = false; // Reset this singleton.
    $commonApi.rec.recorded = false; //
    $commonApi.rec.printed = false; // "
    $commonApi.rec.printProgress = 0; // "
    this.tapeFrameMode = false;
    this.tapeFrameStart = 0;
    this.tapeFrameTarget = 0;
  }

  rolling(opts, cb) {
    send({ type: "recorder:rolling", content: opts });
    this.rollingCallback = cb;
  }

  cut(cb) {
    $commonApi.rec.cutCallback = cb;
    this.tapeProgress = 0;
    this.tapeTimerStart = null;
    this.tapeTimerDuration = null;
    this.tapeFrameMode = false;
    this.tapeFrameStart = 0;
    this.tapeFrameTarget = 0;
    send({ type: "signal", content: "recorder:cut" });
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

// 🤖 Robo Class - For sending synthetic events through the act system
class Robo {
  constructor() {
    this.currentAPI = null;
  }

  // Set the current API context (called from makeFrame)
  setAPI(api) {
    this.currentAPI = api;
  }

  // Send synthetic events directly to the nopaint system
  sendEvent(eventType, coordinates = {}) {
    if (!this.currentAPI) {
      console.warn("🤖 Robo: No API context available");
      return;
    }

    // Ensure pen object exists and initialize if needed
    if (!this.currentAPI.pen) {
      this.currentAPI.pen = { x: 0, y: 0, px: 0, py: 0, pressure: 0.5, device: "robot", delta: { x: 0, y: 0 } };
      console.log("🤖 Robo: Initialized pen object for robot events");
    }

    // Update the pen object with robot event data including pressure
    const x = coordinates.x ?? 0;
    const y = coordinates.y ?? 0;
    const px = coordinates.px ?? coordinates.x ?? 0;
    const py = coordinates.py ?? coordinates.y ?? 0;
    const pressure = coordinates.pressure ?? 0.5;

    this.currentAPI.pen.x = x;
    this.currentAPI.pen.y = y;
    this.currentAPI.pen.px = px;
    this.currentAPI.pen.py = py;
    this.currentAPI.pen.pressure = pressure;
    this.currentAPI.pen.device = "robot";
    
    // Update delta if it exists
    if (this.currentAPI.pen.delta) {
      this.currentAPI.pen.delta.x = x - px;
      this.currentAPI.pen.delta.y = y - py;
    }

    // Create event payload that mirrors real input events
    const eventPayload = {
      device: "robot",
      type: eventType,
      x,
      y,
      px,
      py,
      pressure,
      delta: {
        x: x - px,
        y: y - py,
      },
    };

    try {
      this.currentAPI.act(eventType, eventPayload);
      console.log(`🤖 Robo: Dispatched ${eventType} via act() at (${x}, ${y})`);
    } catch (error) {
      console.error("🤖 Robo: Error dispatching event:", error);
    }
  }

  // Convenience methods for common events
  touch(x, y) {
    this.sendEvent("touch:1", { x, y, pressure: 0.5 });
  }

  draw(x, y, px, py) {
    this.sendEvent("draw:1", { x, y, px, py, pressure: 0.5 });
  }

  lift(x, y) {
    this.sendEvent("lift:1", { x, y, pressure: 0.5 });
  }

  // Generic method for any event type
  act(eventType, coordinates) {
    this.sendEvent(eventType, coordinates);
  }
}

const $commonApi = {
  lisp, //  A global reference to the `kidlisp` evalurator.
  undef: undefined, // A global api shorthand for undefined.
  clock: {
    offset: function () {
      if (clockFetching) return;

      // Skip API calls in OBJKT mode
      if (getPackMode()) {
        clockFetching = false;
        return;
      }

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
  
  // Toggle HUD visibility (same as Tab key functionality)
  toggleHUD: function (isDoubleTap = false) {
    toggleHUDVisibility(isDoubleTap);
  },

  jump: function jump(to, ahistorical = false, alias = false) {
    // let url;

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
        console.log("🐎 Jumping to web URL:", to);
        $commonApi.net.web(to, jumpOut);
        return;
      } catch (e) {
        // Could not construct a valid url from the jump, so we will be
        // running a local aesthetic.computer piece.
        console.log("🐎 URL construction failed, treating as local piece:", e.message);
        return;
      }
    } else {
      leaving = true;
      graph.unmask(); // Clear any active mask when leaving a piece
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
        if (to.split("~")[0] === "prompt" &&
            globalKidLispInstance?.clearBakedLayers) {
          globalKidLispInstance.clearBakedLayers();
          graph.unmask(); // Clear mask when returning to prompt from KidLisp
        }
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
            // Use the same origin-aware URL construction logic as module loading
            const { protocol, hostname } = getSafeUrlParts();

            let baseUrl;
            // Check if we're in a development environment (localhost with port)
            const isDevelopment =
              hostname === "localhost" &&
              typeof location !== "undefined" &&
              location.port;
            if (isDevelopment) {
              // Use the local development server
              baseUrl = `${protocol}//${hostname}:${location.port}`;
            } else {
              // Use the production server for sandboxed iframes or production
              baseUrl = `https://aesthetic.computer`;
            }

            const sanitizedHandle = `${handle ?? ""}`.replace(/^@+/, "");
            const mediaUrl = `${baseUrl}/media/@${sanitizedHandle}/painting/${code}.${extension}`;
            console.log("🖼️ Media URL constructed:", mediaUrl);
            return $commonApi.net.preload(
              mediaUrl,
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
    label: (text, color, offset, plainTextOverride) => {
      currentHUDTxt = text;
      // Use plainTextOverride if provided, otherwise strip color codes from text
      currentHUDPlainTxt = plainTextOverride || stripColorCodes(text);  // Store plain text version
      
      // If this is a painting code (#xyz), update currentPath and currentText for extension display
      if (currentPath === "aesthetic.computer/disks/painting" && text && text.startsWith("#")) {
        currentPath = `aesthetic.computer/disks/painting~${text}`;
        currentText = `painting~${text}`;
      }
      
      if (!color) {
        currentHUDTextColor = currentHUDTextColor || graph.findColor(color);
      } else {
        currentHUDTextColor = graph.findColor(color);
      }
      currentHUDOffset = offset;
      
      // Calculate and store dimensions for animation when directly called
      // (This ensures kidlisp pieces have proper animation geometry)
      if (currentHUDTxt && currentHUDTxt.length > 0) {
        // Use plain text (without color codes) for dimension calculations
        const textForMeasurement = currentHUDPlainTxt || currentHUDTxt;
        
        // Detect if this is a KidLisp piece for more generous width allowance
        const sourceCode = currentText || currentHUDTxt;
        const isKidlispPiece = (currentPath && lisp?.isKidlispSource && lisp.isKidlispSource(currentPath) && !currentPath.endsWith('.lisp')) ||
                             currentPath === "(...)" ||
                             (sourceCode && sourceCode.startsWith("$")) ||
                             (currentPath && currentPath.includes("/disks/$")) ||
                             (sourceCode && lisp?.isKidlispSource && lisp.isKidlispSource(sourceCode));
        
        // Use full screen width for text wrapping
        const maxWidth = cachedAPI.screen.width;
          
        const labelBounds = cachedAPI.text.box(
          textForMeasurement,
          undefined,
          maxWidth,
          1, // scale
          true, // wordWrap
          // TODO: This should check useTinyHudLabel like the main corner label, but that's not accessible here
          // For now, use default font to maintain consistency with current behavior
          undefined // Use default font - will need to be updated when useTinyHudLabel is globally accessible
        );
        
        // Use the actual computed width from text.box instead of character count
        const measuredWidth = labelBounds.box.width;
  const fallbackShareWidth = (currentHUDLabelBlockWidth || DEFAULT_TYPEFACE_BLOCK_WIDTH) * "share ".length;
  const shareWidth = Math.max(currentHUDShareWidth || 0, fallbackShareWidth);
        currentHUDLeftPad = shareWidth;
        const baseLabelWidth = measuredWidth + shareWidth;
        const scrubExtension = Math.max(0, currentHUDScrub);
        const h = labelBounds.box.height + cachedAPI.typeface.blockHeight;

        currentHUDLabelMeasuredWidth = baseLabelWidth;
        hudAnimationState.labelWidth = baseLabelWidth + scrubExtension;
        hudAnimationState.labelHeight = h;
      }
    },
    currentStatusColor: () => currentHUDStatusColor,
    currentLabel: () => ({ 
      text: currentHUDTxt, 
      plainText: currentHUDPlainTxt,  // Include plain text version
      btn: currentHUDButton 
    }),
    labelBack: () => {
      labelBack = true;
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

          // 🎨 Broadcast painting update to other tabs
          $commonApi.broadcastPaintingUpdate("updated", {
            source: "yes-no-decision"
          });

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

        if (act === "touch") {
          system.nopaint.startDrag = { x, y };
        }

        // Ensure startDrag exists before creating dragBox
        if (!system.nopaint.startDrag) {
          system.nopaint.startDrag = { x, y };
        }

        const dragBox = new geo.Box(
          system.nopaint.startDrag.x,
          system.nopaint.startDrag.y,
          x - system.nopaint.startDrag.x,
          y - system.nopaint.startDrag.y,
        );

        system.nopaint.brush = { x, y, dragBox, pressure: pen?.pressure || 0.5 };
        
        // Call needsPaint during any pen movement to ensure continuous painting and HUD updates
        $commonApi.needsPaint();
      },

      // Helper to display the existing painting on the screen, with an
      // optional pan amount, that returns an adjusted pen pointer as `brush`.

      // TODO: - [] Add Zoom
      //       - [] And Rotation!

      present: (
        { system, screen, wipe, paste, ink, slug, dark, theme, blend },
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
          paste(system.painting);
          paste(system.nopaint.buffer);
        } else {
          // If we are panned or the painting is a custom resolution.

          wipe(theme[dark ? "dark" : "light"].wipeBG)
            .paste(system.painting, x, y, system.nopaint.zoomLevel);
            
          // Normal alpha blending for overlay buffer
          paste(system.nopaint.buffer, x, y, system.nopaint.zoomLevel);
          
          ink(128)
            .box(
              x,
              y,
              system.painting.width * system.nopaint.zoomLevel,
              system.painting.height * system.nopaint.zoomLevel,
              "outline",
            );
        }

        // 🎯 DEBUG: Draw dragBox overlay if one exists (COMMENTED OUT)
        /*
        if (system.nopaint.brush?.dragBox) {
          const dragBox = system.nopaint.brush.dragBox;
          
          // Apply zoom and translation to the dragBox coordinates
          const overlayX = (dragBox.x * system.nopaint.zoomLevel) + x;
          const overlayY = (dragBox.y * system.nopaint.zoomLevel) + y;
          const overlayW = dragBox.w * system.nopaint.zoomLevel;
          const overlayH = dragBox.h * system.nopaint.zoomLevel;
          
          // Draw the dragBox outline in bright green with some transparency
          ink(0, 255, 0, 180).box(overlayX, overlayY, overlayW, overlayH, "outline");
          
          // Draw corner indicators
          const cornerSize = 4;
          ink(0, 255, 0, 220);
          // Top-left corner
          ink().box(overlayX - cornerSize/2, overlayY - cornerSize/2, cornerSize, cornerSize);
          // Top-right corner  
          ink().box(overlayX + overlayW - cornerSize/2, overlayY - cornerSize/2, cornerSize, cornerSize);
          // Bottom-left corner
          ink().box(overlayX - cornerSize/2, overlayY + overlayH - cornerSize/2, cornerSize, cornerSize);
          // Bottom-right corner
          ink().box(overlayX + overlayW - cornerSize/2, overlayY + overlayH - cornerSize/2, cornerSize, cornerSize);
          
          // Display coordinate info
          const coordText = `${dragBox.x},${dragBox.y} ${dragBox.w}x${dragBox.h}`;
          ink(0, 255, 0, 200).write(coordText, { 
            x: overlayX, 
            y: Math.max(overlayY - 15, 5) // Position above the box, but not off screen
          }, undefined, undefined, false, "MatrixChunky8");
        }
        */

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
        
        // Store only the pixel data, not the full painting object with functions
        store["painting"] = {
          width: system.painting.width,
          height: system.painting.height,
          pixels: system.painting.pixels,
        };
        
        // Store the new painting BEFORE broadcasting
        store.persist("painting", "local:db");
        
        // 🎨 Broadcast painting cleared to other tabs
        $commonApi.broadcastPaintingUpdate("cleared", {
          source: "clear",
          width: res.w,
          height: res.h,
          resetTransform: true // Flag that receiving tabs should reset their transform
        });

        // Clear any existing painting recording in RAM and
        // storage.
        await store.delete("painting:record", "local:db");
        if (system.nopaint.recording) {
          system.nopaint.recording = false;
          system.nopaint.record.length = 0;
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
        
        // Store only the pixel data, not the full painting object
        store["painting"] = {
          width: system.painting.width,
          height: system.painting.height,
          pixels: system.painting.pixels,
        };
        
        store.persist("painting", "local:db"); // Persist to storage.
        
        // 🎨 Broadcast painting replacement to other tabs
        $commonApi.broadcastPaintingUpdate("replaced", {
          source: "replace",
          message: message
        });
        
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
    width: (text) => {
      if (Array.isArray(text)) text = text.join(" ");
      
      // Use the current typeface for accurate width calculation
      const useTypeface = tf;
      if (!useTypeface) {
        return text.length * DEFAULT_TYPEFACE_BLOCK_WIDTH;
      }

      const isProportional =
        useTypeface?.data?.proportional === true ||
        !!useTypeface?.data?.advances ||
        !!useTypeface?.data?.bdfFont;

      if (isProportional && typeof useTypeface.getAdvance === "function") {
        let totalWidth = 0;
        for (const char of text) {
          if (char === "\n" || char === "\r") continue;
          const advance = useTypeface.getAdvance(char);
          totalWidth += typeof advance === "number" ? advance : useTypeface.blockWidth;
        }
        return totalWidth;
      } else {
        // For monospace fonts, use the typeface's block width
        const blockWidth = useTypeface.blockWidth || DEFAULT_TYPEFACE_BLOCK_WIDTH;
        return text.length * blockWidth;
      }
    },
    height: (text) => {
      // Get the pixel height of a string of characters.
      return 10;
    },
    // Return a text's bounding box.
    box: (text, pos = { x: 0, y: 0 }, bounds, scale = 1, wordWrap = true, fontName) => {

      if (!text) {
        console.warn("⚠️ No text for `box`.");
        return;
      }

      pos = { ...pos };
      const absScale = abs(scale ?? 1);

      let useTypeface = getTypefaceForMeasurement(fontName) || tf;
      if (!useTypeface) {
        useTypeface = tf;
      }

      const baseBlockWidth =
        useTypeface?.blockWidth ?? tf?.blockWidth ?? DEFAULT_TYPEFACE_BLOCK_WIDTH;
      const baseBlockHeight =
        useTypeface?.blockHeight ?? tf?.blockHeight ?? DEFAULT_TYPEFACE_BLOCK_HEIGHT;

      const blockHeight = baseBlockHeight * absScale;

      const isProportional =
        useTypeface?.data?.proportional === true ||
        !!useTypeface?.data?.advances ||
        !!useTypeface?.data?.bdfFont;

      const getAdvanceWidth = (char) => {
        if (!char) return baseBlockWidth * absScale;
        if (isProportional && typeof useTypeface?.getAdvance === "function") {
          const raw = useTypeface.getAdvance(char);
          if (typeof raw === "number") {
            return raw * absScale;
          }
        }
        return baseBlockWidth * absScale;
      };

      const getCharWidth = (char) => {
        if (!char || char === "\n" || char === "\r") return 0;
        if (char === "\t") {
          return getAdvanceWidth(" ") * 4;
        }
        return getAdvanceWidth(char);
      };

      if (bounds === undefined) {
        const sampleWidth = getAdvanceWidth("0");
        bounds = (text.length + 2) * sampleWidth;
      }
      if (!(bounds > 0)) {
        bounds = Number.POSITIVE_INFINITY;
      }

      const lines = [""];
      const charMap = [[]];
      let line = 0;
      let run = 0;
      let maxWidth = 0;

      const commitLineWidth = () => {
        if (run > maxWidth) {
          maxWidth = run;
        }
      };

      const newLine = () => {
        commitLineWidth();
        line += 1;
        lines[line] = "";
        charMap[line] = [];
        run = 0;
      };

      const appendChar = (char, sourceIndex) => {
        const width = getCharWidth(char);
        const renderedChar = char === "\t" ? " " : char;
        lines[line] += renderedChar;
        charMap[line].push(sourceIndex);
        run += width;
        if (run > maxWidth) {
          maxWidth = run;
        }
      };

      if (!wordWrap) {
        for (let idx = 0; idx < text.length; idx += 1) {
          const char = text[idx];
          if (char === "\r") continue;
          if (char === "\n") {
            newLine();
            continue;
          }
          appendChar(char, idx);
        }
      } else {
        const tokens = [];
        let idx = 0;
        while (idx < text.length) {
          const char = text[idx];
          if (char === "\r") {
            idx += 1;
            continue;
          }
          if (char === "\n") {
            tokens.push({ type: "newline", indices: [idx] });
            idx += 1;
            continue;
          }
          if (char === " " || char === "\t") {
            let textBuffer = "";
            const indices = [];
            while (idx < text.length) {
              const c = text[idx];
              if (c !== " " && c !== "\t") break;
              textBuffer += c;
              indices.push(idx);
              idx += 1;
            }
            tokens.push({ type: "whitespace", text: textBuffer, indices });
            continue;
          }
          let textBuffer = "";
          const indices = [];
          while (idx < text.length) {
            const c = text[idx];
            if (c === "\n" || c === " " || c === "\t" || c === "\r") break;
            textBuffer += c;
            indices.push(idx);
            idx += 1;
          }
          if (textBuffer.length > 0) {
            tokens.push({ type: "word", text: textBuffer, indices });
          }
        }

        tokens.forEach((token) => {
          if (token.type === "newline") {
            newLine();
            return;
          }

          if (token.type === "whitespace") {
            for (let i = 0; i < token.text.length; i += 1) {
              const char = token.text[i];
              const sourceIndex = token.indices[i];
              const width = getCharWidth(char);
              if (run > 0 && run + width > bounds) {
                newLine();
              }
              appendChar(char, sourceIndex);
            }
            return;
          }

          if (token.type === "word") {
            const chars = token.text.split("");
            const widths = chars.map((char) => getCharWidth(char));
            const wordWidth = widths.reduce((acc, value) => acc + value, 0);

            if (bounds > 0 && wordWidth > bounds) {
              chars.forEach((char, charIdx) => {
                const width = widths[charIdx];
                if (run > 0 && run + width > bounds) {
                  newLine();
                }
                appendChar(char, token.indices[charIdx]);
              });
              return;
            }

            if (run > 0 && run + wordWidth > bounds) {
              newLine();
            }

            chars.forEach((char, charIdx) => {
              appendChar(char, token.indices[charIdx]);
            });
          }
        });
      }

      commitLineWidth();

      const lineHeightGap = 1 * absScale;
      const finalBlockHeight = blockHeight + lineHeightGap;

      if (lines.length >= 1 && pos.center && pos.center.indexOf("y") !== -1) {
        pos.y =
          $activePaintApi.screen.height / 2 -
          (lines.length * finalBlockHeight) / 2 +
          finalBlockHeight / 2 +
          (pos.y || 0);
      }

      const height = lines.length * finalBlockHeight;

      const lineWidths = lines.map((lineText, index) => {
        if (!lineText) return 0;
        const indices = charMap[index] || [];
        let width = 0;
        for (let i = 0; i < lineText.length; i += 1) {
          const sourceIndex = indices[i];
          if (typeof sourceIndex === "number" && sourceIndex >= 0 && sourceIndex < text.length) {
            width += getCharWidth(text[sourceIndex]);
          } else {
            width += getCharWidth(lineText[i]);
          }
        }
        return width;
      });

      const maxLineWidth = lineWidths.reduce((acc, value) => Math.max(acc, value), 0);

      const box = { x: pos.x, y: pos.y, width: maxLineWidth, height };

      return { pos, box, lines, lineWidths, lineHeight: finalBlockHeight, charMap };
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
    zebra: num.zebra,
    resetZebraCache: num.resetZebraCache,
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
  gizmo: { Hourglass: gizmo.Hourglass, EllipsisTicker: gizmo.EllipsisTicker, Ticker: gizmo.Ticker },
  rec: new Recorder(),
  robo: new Robo(),
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
    pieces: `${(() => {
      const { protocol, hostname } = getSafeUrlParts();
      return `${protocol}//${hostname}`;
    })()}/aesthetic.computer/disks`,
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
    // Remote debugging: Send log messages to session server for debugging on any device
    log: function(levelOrFilename, ...args) {
      // Handle both old API (filename, content) and new API (level, ...args)
      if (levelOrFilename.startsWith('/tmp/') || levelOrFilename.includes('.log')) {
        // Old API: filename, content
        const filename = levelOrFilename;
        const content = args[0];
        
        // Send to session server if socket is available (removed verbose console log)
        if (socket && socket.send) {
          socket.send("dev-log", {
            level: "INFO",
            message: `${filename}: ${content}`,
            device: navigator.userAgent || "unknown",
            timestamp: Date.now()
          });
        }
      } else {
        // New API: level, ...args
        const level = levelOrFilename;
        
        // Serialize objects and create single-line message
        const serializedArgs = args.map(arg => {
          if (typeof arg === 'object' && arg !== null) {
            try {
              return JSON.stringify(arg);
            } catch (e) {
              return String(arg);
            }
          }
          return String(arg);
        });
        
        const message = serializedArgs.join(" ");
        
        // Always log locally first
        if (level === "warn") {
          console.warn(...args);
        } else if (level === "error") {
          console.error(...args);
        } else {
          console.log(...args);
        }
        
        // Send to session server if socket is available
        if (socket && socket.send) {
          socket.send("dev-log", {
            level: level.toUpperCase(),
            message: message,
            device: navigator.userAgent || "unknown",
            timestamp: Date.now()
          });
        }
      }
    },
  },
  needsPaint: () => {
    noPaint = false;
    if (system === "nopaint") {
      $commonApi.system.nopaint.needsPresent = true;
    }
  }, // TODO: Does "paint" needs this?
  
  store,
  pieceCount: -1, // Incs to 0 when the first piece (usually the prompt) loads.
  //                 Increments by 1 each time a new piece loads.
  debug,
  nopaintPerf,
};

// Convenience methods for different log levels
$commonApi.net.log.info = (...args) => $commonApi.net.log("info", ...args);
$commonApi.net.log.warn = (...args) => $commonApi.net.log("warn", ...args);
$commonApi.net.log.error = (...args) => $commonApi.net.log("error", ...args);

chatClient.$commonApi = $commonApi; // Add a reference to the `Chat` module.

const nopaintAPI = $commonApi.system.nopaint;

// 🎨 Start painting change monitoring for cross-tab sync
startPaintingChangeMonitoring();

// Broadcast to other tabs in the same origin.
const channel = new BroadcastChannel("aesthetic.computer");

channel.onmessage = (event) => {
  processMessage(event.data);
};

async function processMessage(msg) {
  if (logs.messaging) console.log(`🗼 Processing broadcast: ${msg}`);
  
  // 🎨 Handle painting updates (both new JSON format and legacy string format)
  if (msg.startsWith("painting:") || (msg.startsWith("{") && msg.includes('"type":"painting:updated"'))) {
    const isNopaintActive = $commonApi.system?.nopaint?.is?.("painting");
    if (isNopaintActive) {
      console.log(`🚫 CROSS-TAB INTERFERENCE: Receiving painting update during nopaint operation - this may disrupt live preview!`, {
        msgPreview: msg.substring(0, 100) + '...',
        nopaintState: 'painting'
      });
    }
    await handlePaintingUpdate(msg);
    return;
  }
  
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

// 🎨 Handle painting update messages from other tabs
async function handlePaintingUpdate(msg) {
  try {
    // Parse the JSON message
    const data = JSON.parse(msg);
    
    if (data.type !== "painting:updated") return;
    
    // Skip if this message is from the current tab
    if (data.tabId === $commonApi._tabId) {
      console.log(`🎨 SKIPPED: Own message (${data.action})`);
      return;
    }
    
    if (!$commonApi.system) {
      console.log(`🎨 SKIPPED: No system available`);
      return;
    }

    // 📐 Handle resize events specifically
    if ((data.action === "resized" && data.metadata?.source === "screen_resize") || 
        (data.action === "updated" && (data.source === "resize" || data.source === "crop"))) {
      const width = data.width || data.metadata?.width;
      const height = data.height || data.metadata?.height;
      console.log(`📐 DIMENSION CHANGE EVENT: ${width}x${height} (source: ${data.source || data.metadata?.source})`);
      
      // For dimension change events, we need to wait for a "storage_complete" event
      $commonApi._processingResize = true;
      $commonApi._awaitingResizeStorage = { width, height, timestamp: data.timestamp };
      
      console.log(`📐 Waiting for storage completion for ${data.source} ${width}x${height}...`);
      return; // Don't continue with regular painting sync for dimension change events
    }
    
    // 💾 Handle storage completion events
    if (data.action === "storage_complete" && (data.source === "resize" || data.source === "crop")) {
      const awaitingResize = $commonApi._awaitingResizeStorage;
      console.log(`🪝 RECEIVED storage_complete:`, {
        source: data.source,
        timestamp: data.timestamp,
        awaitingResize: awaitingResize,
        timestampMatch: awaitingResize && data.timestamp === awaitingResize.timestamp
      });
      
      if (awaitingResize && data.timestamp === awaitingResize.timestamp) {
        console.log(`📐 Storage completed for ${data.source} ${awaitingResize.width}x${awaitingResize.height}, applying...`);
        
        try {
          const storedPainting = await store.retrieve("painting", "local:db");
          if (storedPainting) {
            // Update system painting with the stored version
            $commonApi.system.painting = {
              width: storedPainting.width,
              height: storedPainting.height,
              pixels: new Uint8ClampedArray(storedPainting.pixels)
            };
            
            // Store the painting data  
            store["painting"] = {
              width: storedPainting.width,
              height: storedPainting.height,
              pixels: storedPainting.pixels
            };
            
            // Update painting hash
            lastPaintingHash = generatePaintingHash($commonApi.system.painting);
            
            // Update KidLisp with new painting reference
            const kidlispInstance = getGlobalKidLisp();
            if (kidlispInstance) {
              kidlispInstance.setAPI($commonApi);
              console.log(`🎯 KidLisp API refreshed with ${data.source} painting: ${storedPainting.width}x${storedPainting.height}`);
            }
            
            // Force repaint with new dimensions
            if ($commonApi.system.nopaint) {
              $commonApi.system.nopaint.needsPresent = true;
            }
            $commonApi.needsPaint();
            
            console.log(`📐 Screen updated after ${data.source}: ${storedPainting.width}x${storedPainting.height}`);
          }
        } catch (error) {
          console.error(`📐 ERROR applying ${data.source} after storage completion:`, error);
        }
        
        // Clean up
        delete $commonApi._awaitingResizeStorage;
        $commonApi._processingResize = false;
        return;
      }
    }

    // Set processing flag to prevent feedback loops
    $commonApi._processingBroadcast = true;
    
    try {
      // Load painting from storage (lightweight messages don't contain pixel data)
      const storedPainting = await store.retrieve("painting", "local:db");
      if (storedPainting) {
        $commonApi.system.painting = {
          width: storedPainting.width,
          height: storedPainting.height,
          pixels: new Uint8ClampedArray(storedPainting.pixels)
        };
        
        // Store only the pixel data, not the full painting object
        store["painting"] = {
          width: storedPainting.width,
          height: storedPainting.height,
          pixels: storedPainting.pixels
        };
        
        // Update painting hash to prevent false positives
        lastPaintingHash = generatePaintingHash($commonApi.system.painting);
        
        // 🎯 Update KidLisp with the new painting reference
        const kidlispInstance = getGlobalKidLisp();
        if (kidlispInstance) {
          kidlispInstance.setAPI($commonApi);
          console.log(`🎯 KidLisp API refreshed with updated painting`);
        }
        
        // Force repaint
        if ($commonApi.system.nopaint) {
          $commonApi.system.nopaint.needsPresent = true;
        }
        $commonApi.needsPaint();
        
        // 🆕 Force nopan (reset transform) for clear/new actions or explicit flag
        if (((data.action === "cleared" || data.action === "new") || data.resetTransform) && $commonApi.system.nopaint) {
          console.log(`🎯 ${data.action?.toUpperCase() || 'TRANSFORM_RESET'}: Forcing nopan (reset transform) on receiver`);
          $commonApi.system.nopaint.resetTransform({ 
            system: $commonApi.system, 
            screen: $commonApi.screen 
          });
        }
        
        console.log(`🎨 PAINTING SYNCED: ${storedPainting.width}x${storedPainting.height}`, {
          hash: generatePaintingHash($commonApi.system.painting)?.substr(0, 8),
          fromHash: data.hash || 'unknown',
          firstPixels: Array.from($commonApi.system.painting.pixels.slice(0, 12)) // First 3 pixels (RGBA)
        });
      } else {
        // No painting found in storage
      }
    } catch (storageError) {
      console.error(`🎨 ERROR loading from storage:`, storageError);
    }
    
  } catch (error) {
    console.error(`🎨 ERROR handling painting update:`, error);
    
    // Fallback to old string-based format
    if (typeof msg === "string" && msg.startsWith("painting:")) {
      const action = msg.split(":")[1];
      console.log(`🎨 FALLBACK: Processing legacy format ${action}`);
      
      // Legacy handling - load from storage
      try {
        const storedPainting = await store.retrieve("painting", "local:db");
        if (storedPainting) {
          $commonApi.system.painting = {
            width: storedPainting.width,
            height: storedPainting.height,
            pixels: new Uint8ClampedArray(storedPainting.pixels)
          };
          
          // Store only the pixel data, not the full painting object
          store["painting"] = {
            width: storedPainting.width,
            height: storedPainting.height,
            pixels: storedPainting.pixels
          };
          
          const kidlispInstance = getGlobalKidLisp();
          if (kidlispInstance) {
            kidlispInstance.setAPI($commonApi);
          }
          
          if ($commonApi.system.nopaint) {
            $commonApi.system.nopaint.needsPresent = true;
          }
          $commonApi.needsPaint();
        }
      } catch (storageError) {
        console.error(`🎨 ERROR in fallback storage load:`, storageError);
      }
    }
  } finally {
    // Clear processing flag regardless of success or error
    setTimeout(() => {
      $commonApi._processingBroadcast = false;
    }, 50);
  }
}

$commonApi.broadcast = (msg) => {
  processMessage(msg); // Process locally.
  channel.postMessage(msg);
};

// 🎨 Broadcast painting updates to other tabs
$commonApi.broadcastPaintingUpdate = (action, data = {}) => {
  if (!$commonApi.system?.painting) return;
  
  // Check if we're in a nopaint operation and log it
  const isNopaintActive = $commonApi.system?.nopaint?.is?.("painting");
  if (isNopaintActive) {
    console.log(`🚫 NOPAINT INTERFERENCE: Attempting to broadcast "${action}" during nopaint operation - this may disrupt live preview!`, {
      action,
      source: data.source,
      nopaintState: 'painting'
    });
  }
  
  // Throttle broadcasts to prevent excessive messages
  const now = Date.now();
  if (now - lastBroadcastTime < broadcastThrottleDelay) {
    return;
  }
  lastBroadcastTime = now;
  
  // Generate unique tab ID to prevent self-processing
  if (!$commonApi._tabId) {
    $commonApi._tabId = Math.random().toString(36).substr(2, 9);
  }
  
  // Create lightweight notification message (no pixel data)
  const message = {
    type: "painting:updated",
    action,
    tabId: $commonApi._tabId,
    timestamp: now,
    width: $commonApi.system.painting.width,
    height: $commonApi.system.painting.height,
    ...data
  };
  
  // Only send to other tabs (exclude self)
  channel.postMessage(JSON.stringify(message));
};

// 🚀 Immediate painting broadcast - bypasses throttling for instant sync
$commonApi.broadcastPaintingUpdateImmediate = (action, data = {}) => {
  if (!$commonApi.system?.painting || $commonApi._processingBroadcast) return;
  
  // Check for nopaint interference
  const isNopaintActive = $commonApi.system?.nopaint?.is?.("painting");
  if (isNopaintActive) {
    console.log(`🚫 IMMEDIATE NOPAINT INTERFERENCE: Attempting immediate broadcast "${action}" during nopaint operation!`, {
      action,
      source: data.source,
      nopaintState: 'painting'
    });
  }
  
  // Performance: Skip if we just broadcasted very recently (debounce)
  const now = Date.now();
  if (now - (lastBroadcastTime || 0) < 16) { // ~60fps max broadcast rate
    return;
  }
  
  // Generate unique tab ID if needed
  if (!$commonApi._tabId) {
    $commonApi._tabId = Math.random().toString(36).substr(2, 9);
  }
  
  const paintingHash = generatePaintingHash($commonApi.system.painting);
  const message = {
    type: "painting:updated",
    action,
    tabId: $commonApi._tabId,
    timestamp: now,
    width: $commonApi.system.painting.width,
    height: $commonApi.system.painting.height,
    hash: paintingHash?.substr(0, 8),
    immediate: true,
    ...data
  };
  
  // Immediate broadcasting silently
  
  // Store painting immediately (async to not block)
  setTimeout(() => {
    store["painting"] = {
      width: $commonApi.system.painting.width,
      height: $commonApi.system.painting.height,
      pixels: $commonApi.system.painting.pixels,
    };
    store.persist("painting", "local:db");
    
    // 🪝 Fire storage completion hook for resize and crop events
    if (data.source === "resize" || data.source === "crop") {
      const storageCompleteMessage = {
        type: "painting:updated",
        action: "storage_complete",
        source: data.source,
        tabId: message.tabId,
        timestamp: message.timestamp,
        width: $commonApi.system.painting.width,
        height: $commonApi.system.painting.height
      };
      
      channel.postMessage(JSON.stringify(storageCompleteMessage));
      // console.log(`🪝 STORAGE COMPLETE hook fired for ${data.source} ${$commonApi.system.painting.width}x${$commonApi.system.painting.height}`, {
      //   timestamp: message.timestamp,
      //   tabId: message.tabId.substr(0, 4) + '...'
      // });
    }
    
  }, 0);
  
  // Update hash to prevent duplicate detection
  lastPaintingHash = paintingHash;
  lastBroadcastTime = now;
  
  channel.postMessage(JSON.stringify(message));
};

// 🎨 Painting change detection system
function generatePaintingHash(painting) {
  if (!painting?.pixels) return null;
  
  // Simple hash using width, height, and sample of pixels
  let hash = painting.width * 31 + painting.height * 37;
  
  // Sample every 100th pixel to create a lightweight hash
  for (let i = 0; i < painting.pixels.length; i += 100) {
    hash = ((hash << 5) - hash + painting.pixels[i]) & 0xffffffff;
  }
  
  return hash.toString(36);
}

function startPaintingChangeMonitoring() {
  if (paintingChangeCheckInterval) return; // Already monitoring
  
  // Painting change monitoring initialized silently
  
  // Initialize hash baseline if painting exists
  if ($commonApi.system?.painting) {
    lastPaintingHash = generatePaintingHash($commonApi.system.painting);
  }
  
  // Enable frame-based monitoring for immediate detection
  enableFrameBasedMonitoring();
  
  // Keep interval-based monitoring as backup (lower frequency)
  paintingChangeCheckInterval = setInterval(() => {
    if (!$commonApi.system?.painting || frameBasedMonitoring) return; // Skip if frame monitoring is active
    
    const currentHash = generatePaintingHash($commonApi.system.painting);
    
    // Only broadcast if hash actually changed AND we're not currently processing a broadcast
    if (lastPaintingHash !== null && 
        currentHash !== lastPaintingHash && 
        !$commonApi._processingBroadcast) {
      
      // Set flag to prevent feedback loops during broadcast processing
      $commonApi._processingBroadcast = true;
      
      // Store updated painting
      store["painting"] = {
        width: $commonApi.system.painting.width,
        height: $commonApi.system.painting.height,
        pixels: $commonApi.system.painting.pixels,
      };
      store.persist("painting", "local:db");
      
      // Broadcast the change
      $commonApi.broadcastPaintingUpdate("updated", {
        source: "interval_monitor",
        hash: currentHash.substr(0,8)
      });
      
      // Clear the processing flag after a brief delay
      setTimeout(() => {
        $commonApi._processingBroadcast = false;
      }, 100);
    }
    
    lastPaintingHash = currentHash;
  }, 1000); // Slower backup monitoring (1 second)
}

function stopPaintingChangeMonitoring() {
  frameBasedMonitoring = false; // Stop frame-based monitoring
  if (paintingChangeCheckInterval) {
    clearInterval(paintingChangeCheckInterval);
    paintingChangeCheckInterval = null;
  }
}

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
  const foundColor = graph.findColor(...arguments);
  if (inkFloodLoggingEnabled()) {
    console.log(
      `${inkFloodLogPrefix()}🖍️ INK DEBUG`,
      {
        args: cloneArgsForLog(arguments),
        resolved: cloneColorForLog(foundColor)
      }
    );
  }
  const result = graph.color(...foundColor);
  if (inkFloodLoggingEnabled()) {
    console.log(
      `${inkFloodLogPrefix()}🖍️ INK APPLIED`,
      {
        color: cloneColorForLog(result)
      }
    );
  }
  return result;
}

function ink2() {
  if (arguments[0] === null) {
    return graph.color2(null);
  } else {
    return graph.color2(...graph.findColor(...arguments));
  }
}

// 🎯 Global KidLisp Instance Management
function initializeGlobalKidLisp(api) {
  if (!globalKidLispInstance) {
    // console.log("🚀 Initializing global KidLisp instance");
    globalKidLispInstance = new lisp.KidLisp();
    globalKidLispInstance.setAPI(api);
  }
  return globalKidLispInstance;
}

function getGlobalKidLisp() {
  return globalKidLispInstance;
}

// 🎵 Update KidLisp audio globals (safe for worker contexts)
function updateKidLispAudio(audioData) {
  if (globalKidLispInstance && globalKidLispInstance.updateAudioGlobals) {
    globalKidLispInstance.updateAudioGlobals(audioData);
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
  // text, x, y, options, wordWrap, customTypeface
  // text, pos, bg, bounds, wordWrap = true, customTypeface
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

    // Assume: text, x, y, options, wordWrap, customTypeface
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

    if (customTypeface) {
      const resolvedTypeface = resolveTypefaceInstance(customTypeface);
      if (resolvedTypeface) {
        ensureTypefaceLoaded(resolvedTypeface);
        customTypeface = resolvedTypeface;
      }
    }

    // 🎨 Color code processing
    const hasColorCodes = textContainsColorCodes(text);

    if (hasColorCodes) {


      // Remember the current ink color to restore it later
      const originalColor = $activePaintApi.inkrn();

      // Process color codes into per-character color array
      let cleanText = "";
      let charColors = [];
      let currentColor = null;

      // Split text by color codes and process each segment
      COLOR_CODE_MATCH_REGEX.lastIndex = 0;
      const segments = text.split(COLOR_CODE_MATCH_REGEX);

      for (let i = 0; i < segments.length; i++) {
        if (i % 2 === 0) {
          // This is regular text
          const segment = segments[i];
          for (let j = 0; j < segment.length; j++) {
            cleanText += segment[j];
            charColors.push(currentColor);
            

          }
        } else {
          // This is a color name or RGB value (from the captured group)
          const colorStr = segments[i];

          if (!colorStr) {
            continue;
          }

          const normalized = colorStr.trim();
          const lower = normalized.toLowerCase();

          if (!normalized) {
            continue;
          }

          if (lower === "reset" || lower === "default" || lower === "base") {
            currentColor = null;
            continue;
          }

          if (normalized.includes(",")) {
            // RGB/RGBA format like "255,0,0" or "255,0,0,128"
            const parts = normalized.split(",").map((n) => {
              const parsed = parseInt(n.trim(), 10);
              return Number.isFinite(parsed) ? parsed : 0;
            });
            while (parts.length < 3) parts.push(0);
            if (parts.length === 3) parts.push(255);
            currentColor = parts.slice(0, 4);
          } else if (lower === "transparent" || lower === "clear") {
            currentColor = [0, 0, 0, 0];
          } else {
            const resolved = graph.findColor(normalized);
            if (Array.isArray(resolved)) {
              currentColor = resolved.slice();
            } else if (resolved && typeof resolved === "object") {
              currentColor = { ...resolved };
            } else if (resolved !== undefined) {
              currentColor = resolved;
            } else {
              currentColor = null;
            }
          }
          

        }
      }

      COLOR_CODE_MATCH_REGEX.lastIndex = 0;

      // Check if we have any actual text to display after removing color codes  
      if (cleanText.trim().length === 0) {
        return $activePaintApi; // Exit silently if no text content remains
      }



      // Render with colors - same logic as original text processing
      const scale = pos?.size || 1;

      if (bounds) {
        const tb = $commonApi.text.box(cleanText, pos, bounds, scale, wordWrap, customTypeface);
        if (!tb || !tb.lines) {
          return $activePaintApi; // Exit silently if text.box fails
        }
        
        const charMap = tb.charMap || [];

        tb.lines.forEach((lineText, index) => {
          const renderedLine = typeof lineText === "string" ? lineText : lineText?.join?.(" ") || "";
          const sourceIndices = charMap[index] || [];
          const lineColors = [];

          for (let i = 0; i < renderedLine.length; i++) {
            const sourceIndex = sourceIndices[i];
            if (
              typeof sourceIndex === "number" &&
              sourceIndex >= 0 &&
              sourceIndex < charColors.length
            ) {
              lineColors.push(charColors[sourceIndex]);
            } else {
              lineColors.push(null);
            }
          }

          (customTypeface || tf)?.print(
            $activePaintApi,
            tb.pos,
            index,
            renderedLine,
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
          
          lines.forEach((line, index) => {
            const lineColors = charColors?.slice(
              charIndex,
              charIndex + line.length,
            );
            (customTypeface || tf)?.print(
              $activePaintApi,
              {
                x: pos?.x,
                y: pos
                  ? pos.y + index * (customTypeface || tf).blockHeight + lineHeightGap
                  : undefined,
              },
              0,
              line,
              bg,
              lineColors,
            );
            charIndex += line.length + 1; // +1 for the newline character
          });
        } else {
          (customTypeface || tf)?.print($activePaintApi, pos, 0, cleanText, bg, charColors);
        }
      }

      const typefaceName = typeof customTypeface === "string" ? customTypeface : customTypeface?.name;
      if (typefaceName === "MatrixChunky8" || typefaceName === "unifont") {
        const hasColorAssignments = charColors?.some((color) => {
          if (!color) return false;
          if (Array.isArray(color)) {
            return color.some((component) => component !== null && component !== undefined);
          }
          if (typeof color === "object") {
            return Object.keys(color).length > 0;
          }
          return typeof color === "string";
        });

        if (!hasColorAssignments) {
          const snippet = cleanText.length > 120 ? `${cleanText.slice(0, 117)}…` : cleanText;
          if (lastMatrixChunkyWriteDiagnosticLog !== snippet) {
            lastMatrixChunkyWriteDiagnosticLog = snippet;
            console.warn("🎨 MatrixChunky8 HUD charColors missing", {
              snippet,
              charColorsLength: charColors?.length,
              cleanTextLength: cleanText.length,
            });
          }
        }
      }

      // Restore the original ink color
      $activePaintApi.ink(...originalColor);
      return $activePaintApi;
    }

    // 🎁 Original code for text without color codes
    // See if the text length is greater than the bounds, and if it is then
    // print on a new line.
    const scale = pos?.size || 1;

    if (bounds) {
      const tb = $commonApi.text.box(text, pos, bounds, scale, wordWrap, customTypeface); // TODO: Get the current ink color, memoize it, and make it static here.
      //       23.10.12.22.04
      tb.lines.forEach((lineText, index) => {
        const renderedLine = typeof lineText === "string" ? lineText : lineText?.join?.(" ") || "";
        (customTypeface || tf)?.print($activePaintApi, tb.pos, index, renderedLine, bg);
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
                ? pos.y + index * (customTypeface || tf).blockHeight + lineHeightGap
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
        
        const actualFont = customTypeface || tf;
        actualFont?.print($activePaintApi, pos, 0, text, bg); // Or print a single line.
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
) {  // Exit silently if no forms are present.
  if (forms === undefined || forms?.length === 0) return;

  if (cpu === true) {
    if (formReframing) {
      cam.resize();
      formReframing = false;
    }
    if (Array.isArray(forms))
      forms.filter(Boolean).forEach((form) => form.graph(cam));
    else forms.graph(cam);
  } else {
    // GPU forms.
    if (!Array.isArray(forms)) forms = [forms];

    // Clear out any forms that need deleting.
    formsToClear.forEach((id) => delete formsSent[id]);
    formsToClear.length = 0;

    // Build a list of forms to send, ignoring already sent ones by UID.
    const formsToSend = [];

    forms.filter(Boolean).forEach((form) => {
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
          //console.log(form.gpuReset);
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
    });

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

  console.log("🖼️ Prefetching...", code);
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
    // Update the existing screen object's properties instead of replacing it
    // This preserves methods like load, save, center that are attached to screen
    $activePaintApi.screen.width = arguments[0].width;
    $activePaintApi.screen.height = arguments[0].height;
    $activePaintApi.screen.pixels = arguments[0].pixels;
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
    
    // Preserve fade alpha during wipe operations to prevent clearing it
    const preserveFadeAlpha = getPreserveFadeAlpha?.() || false;
    if (!preserveFadeAlpha && typeof setPreserveFadeAlpha === 'function') {
      setPreserveFadeAlpha(true);
    }
    
    // Default to white if no arguments provided
    if (arguments.length === 0) {
      ink(255, 255, 255);
    } else {
      ink(...arguments);
    }
    
    // 🎨 REFRAME FIX: Use explicit canvas fill to ensure extended areas are covered
    // For reframe operations, graph.clear() may not cover new extended areas
    graph.withForceReplaceMode(() => {
      if (screen && screen.width && screen.height) {
        // Fill the entire canvas area explicitly to ensure reframe extensions are covered
        $paintApiUnwrapped.box(0, 0, screen.width, screen.height);
      } else {
        // Fallback to standard clear if screen dimensions unavailable
        graph.clear();
      }
    });
    
    twoDCommands.push(["wipe", ...graph.c]);
    ink(...cc);
    
    // Restore previous preservation state
    if (!preserveFadeAlpha && typeof setPreserveFadeAlpha === 'function') {
      setPreserveFadeAlpha(false);
    }
    
    // 🍞 LAYER 0: Also clear layer 0 if it exists (for KidLisp)
    // This ensures wipe() clears the persistent layer, not just the screen
    if (this.kidlispInstance?.layer0) {
      this.kidlispInstance.layer0.pixels.fill(0);
    }
  },
  // Set background fill color for reframe operations (especially for KidLisp pieces)
  backgroundFill: function (color) {
    // This function should fill transparent areas with the background color
    // without clearing existing painted content.
    // 
    // For now, we use a simple approach: only do a full wipe during initial boot,
    // but skip it during reframe to preserve content.
    
    const cc = graph.c.slice(0); // Save current ink color
    
    if (arguments.length === 0) {
      ink(255, 255, 255); // Default to white
    } else {
      ink(...arguments);
    }
    
    // TODO: Implement proper transparent-area-only filling
    // For now, always do a full clear
    graph.clear();
    twoDCommands.push(["backgroundFill", ...graph.c]);
    ink(...cc); // Restore previous ink color
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
  line: function() {
    return graph.line(...arguments);
  },
  lineAngle: graph.lineAngle,
  pline: graph.pline,
  pppline: graph.pixelPerfectPolyline,
  oval: graph.oval,
  circle: graph.circle,
  tri: graph.tri,
  poly: graph.poly,
  box: graph.box,
  shape: graph.shape,
  grid: graph.grid,
  draw: graph.draw,
  setShowClippedWireframes: graph.setShowClippedWireframes,
  clearWireframeBuffer: graph.clearWireframeBuffer,
  drawBufferedWireframes: graph.drawBufferedWireframes,
  getRenderStats: graph.getRenderStats,
  printLine: graph.printLine, // TODO: This is kind of ugly and I need a state machine for type.
  form,
  pan: graph.pan,
  unpan: graph.unpan,
  savepan: graph.savepan,
  loadpan: graph.loadpan,
  mask: graph.mask,
  unmask: graph.unmask,
  steal: graph.steal,
  putback: graph.putback,  skip: graph.skip,
  scroll: graph.scroll,
  spin: graph.spin,
  sort: graph.sort,
  zoom: graph.zoom,
  suck: graph.suck,
  blur: function(radius = 1) {
    // 🔧 FIX: Ensure blur operates on current buffer context
    // When called from within a painting() context, the graph module
    // should already have the correct buffer set via setBuffer()
    return graph.blur(radius);
  },
  sharpen: function(strength = 1) {
    // Apply sharpening filter to enhance edges and details
    // When called from within a painting() context, the graph module
    // should already have the correct buffer set via setBuffer()
    return graph.sharpen(strength);
  },
  invert: function() {
    // Invert RGB colors (255 - value) while preserving alpha
    // When called from within a painting() context, the graph module
    // should already have the correct buffer set via setBuffer()
    return graph.invert();
  },
  contrast: graph.contrast,
  shear: graph.shear,
  resetScrollState: function() {
    // Reset scroll accumulator - used by burn to ensure clean state
    return graph.resetScrollState();
  },
  noise16: graph.noise16,
  noise16DIGITPAIN: graph.noise16DIGITPAIN,
  noise16Aesthetic: graph.noise16Aesthetic,
  noise16Sotce: graph.noise16Sotce,
  noiseTinted: graph.noiseTinted,
  // 🎯 Simplified KidLisp integration using global singleton instance
  kidlisp: function kidlisp(x = 0, y = 0, width, height, source, options = {}) {
    // Initialize global instance if needed
    if (!globalKidLispInstance) {
      initializeGlobalKidLisp($activePaintApi);
    }
    
    // Default dimensions to screen size if not provided
    if (!width) width = $activePaintApi.screen.width;
    if (!height) height = $activePaintApi.screen.height;
    
    // console.log(`🎯 Simple kidlisp call: ${width}x${height} at (${x},${y})`);
    
    // Extract options
    const { noCache = false } = options;
    
    try {
      // Initialize persistent paintings cache if needed  
      if (!globalKidLispInstance.persistentPaintings) {
        globalKidLispInstance.persistentPaintings = new Map();
      }
      
      // For dollar codes, check if we already have a resolved version cached
      let resolvedSource = source;
      if (source && source.startsWith && source.startsWith('$') && source.length > 1) {
        const cacheId = source.slice(1);
        
        // Use a singleton-specific cache to avoid conflicts with normal prompt loading
        if (!globalKidLispInstance.singletonDollarCodeCache) {
          globalKidLispInstance.singletonDollarCodeCache = new Map();
        }
        
        // Track loading states to prevent concurrent requests
        if (!globalKidLispInstance.loadingDollarCodes) {
          globalKidLispInstance.loadingDollarCodes = new Set();
        }
        
        // First check if we have a painting with the resolved source already
        if (globalKidLispInstance.singletonDollarCodeCache.has(cacheId)) {
          resolvedSource = globalKidLispInstance.singletonDollarCodeCache.get(cacheId);
          // Note: regionKey will be set later after accumulation detection
          
          // If we already have this resolved painting, we'll check after regionKey is set
          // console.log(`🎯 Using cached source: ${resolvedSource}`);
        } else if (globalKidLispInstance.loadingDollarCodes.has(cacheId)) {
          // Already loading this dollar code, wait for it to complete
          // console.log(`🎯 ${source} already loading, waiting...`);
          return null;
        } else {
          // Start loading and mark as loading
          // console.log(`🎯 Loading ${source} for first time...`);
          globalKidLispInstance.loadingDollarCodes.add(cacheId);
          
          getCachedCodeMultiLevel(cacheId).then(loadedSource => {
            if (loadedSource) {
              // console.log(`🎯 Cached source for ${cacheId}:`, loadedSource);
              globalKidLispInstance.singletonDollarCodeCache.set(cacheId, loadedSource);
            } else {
              // console.warn(`❌ Could not load source for ${cacheId}`);
            }
          }).catch(error => {
            console.error(`❌ Error loading ${cacheId}:`, error);
          }).finally(() => {
            // Remove from loading set when done (success or failure)
            globalKidLispInstance.loadingDollarCodes.delete(cacheId);
          });
          
          // Return early - no painting this frame, wait for cache
          return null;
        }
      }
      
      // 🎨 AUTO-DETECT ACCUMULATION: Check if source starts with a color word
      const colorWords = ['red', 'blue', 'green', 'yellow', 'purple', 'orange', 'cyan', 'magenta', 'black', 'white', 'gray', 'grey', 'brown', 'pink'];
      const firstWord = resolvedSource.trim().split(/\s+/)[0]?.toLowerCase();
      const startsWithColor = colorWords.includes(firstWord);
      
      // Auto-generate accumulation settings
      const accumulate = startsWithColor;
      const accumulateKey = accumulate ? `auto_${x}_${y}_${width}_${height}_${firstWord}` : null;
      
      // Create persistent painting key with accumulation support
      let regionKey;
      if (accumulate && accumulateKey) {
        regionKey = `${x},${y},${width},${height}:ACCUMULATE:${accumulateKey}`;
      } else {
        regionKey = `${x},${y},${width},${height}:${resolvedSource}`;
      }
      
      // Check if the code contains frame-dependent randomization commands
      const hasFrameDependentCommands = /\(\s*ink\s*\)|\(\s*color\s*\)|\(\s*rand\s*\)/.test(resolvedSource);
      
      // Check if we need to reset (resolved source contains 'wipe')
      const shouldReset = resolvedSource.includes('wipe') && !accumulate; // Don't reset in accumulation mode
      
      // Check if source contains animation-related commands that need fresh execution
      const animationCommands = ['rainbow', 'zebra', 'time', 'random', 'noise', 'clock', 'scroll', 'zoom', 'contrast', 'fade'];
      const hasTimingCommands = /\d+\.?\d*s\b/.test(resolvedSource); // Detect timing like "0.15s", "1s", etc.
      const hasAnimationCommands = animationCommands.some(cmd => resolvedSource.includes(cmd));
      const isDollarCode = source && source.startsWith && source.startsWith('$'); // Dollar codes should always refresh
      // Don't force fresh execution for timing commands - they need to run continuously
      // In accumulate mode, prefer building on existing surface unless explicitly forced
      const needsFreshExecution = !accumulate && (noCache || (isDollarCode && !hasTimingCommands) || (hasAnimationCommands && !hasTimingCommands));
      
      let painting;
      
      // Create fresh painting if: reset needed, no cached version exists, or animation commands need updating
      if (shouldReset || needsFreshExecution || !globalKidLispInstance.persistentPaintings.has(regionKey)) {
        // Create fresh painting (first time, after wipe, or for animations)
        const reason = shouldReset ? 'wipe command' : needsFreshExecution ? 'animation content' : 'first time';
        // console.log(`🎨 Creating fresh painting for key: ${regionKey.slice(0, 50)}... (${reason})`);
        
        // For animations, start with previous frame if available (unless wiping)
        const previousPainting = !shouldReset && globalKidLispInstance.persistentPaintings.has(regionKey) 
          ? globalKidLispInstance.persistentPaintings.get(regionKey) 
          : null;
        
        painting = $activePaintApi.painting(width, height, (api) => {
          // For animations, paste previous frame first to maintain transformations
          if (previousPainting && needsFreshExecution && !shouldReset) {
            // console.log(`🎨 Pasting previous frame for animation continuity`);
            api.paste(previousPainting);
          }
          
          globalKidLispInstance.setAPI(api);
          
          // Add basic timing support for KidLisp commands
          if (!api.clock) {
            api.clock = { time: () => new Date() };
          }
          // Override zoom and scroll functions to implement actual effects in painting context
          const originalZoom = api.zoom;
          const originalScroll = api.scroll || $activePaintApi.scroll;
          
          api.zoom = (...args) => {
            // console.log(`🔍 Zoom called in painting context: args=${JSON.stringify(args)}`);
            // Just call the original zoom function - it will operate on the current buffer
            if (originalZoom && typeof originalZoom === 'function') {
              return originalZoom(...args);
            }
          };
          
          api.scroll = (dx, dy) => {
            // console.log(`📜 Scroll called in painting context: dx=${dx}, dy=${dy}`);
            // Call the real scroll function from the main paint API
            if (originalScroll && typeof originalScroll === 'function') {
              return originalScroll(dx, dy);
            } else if ($activePaintApi.scroll && typeof $activePaintApi.scroll === 'function') {
              return $activePaintApi.scroll(dx, dy);
            }
          };
          // Add randomization support for ink() and other commands
          if (!api.num) {
            api.num = $activePaintApi.num || { 
              random: () => Math.random(),
              randInt: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min,
              rainbow: num.rainbow, // Use the actual rainbow function from num.mjs
              zebra: num.zebra // Use the actual zebra function from num.mjs
            };
          }
          // Add color support for proper ink randomization
          if (!api.color) {
            api.color = $activePaintApi.color || {
              random: () => [
                Math.floor(Math.random() * 256),
                Math.floor(Math.random() * 256), 
                Math.floor(Math.random() * 256)
              ]
            };
          }
          
          // 🎨 Add CSS color functions to make unquoted color words work
          // Import CSS colors from num.mjs and add them as functions to the API
          const cssColors = num.cssColors;
          if (cssColors) {
            Object.keys(cssColors).forEach(colorName => {
              if (!api[colorName]) {
                api[colorName] = () => cssColors[colorName];
              }
            });
          }
          
          // Add rainbow and zebra as top-level functions for unquoted usage
          if (!api.rainbow) {
            api.rainbow = () => num.rainbow();
          }
          if (!api.zebra) {
            api.zebra = () => num.zebra();
          }
          // Reset KidLisp color state to prevent cross-contamination between regions
          globalKidLispInstance.currentInk = null;
          
          // Save original state before configuring execution mode
          const originalInEmbedPhase = globalKidLispInstance.inEmbedPhase;
          const originalIsNestedInstance = globalKidLispInstance.isNestedInstance;
          const originalEmbeddedLayers = globalKidLispInstance.embeddedLayers;
          
          // Check if source contains timing expressions that need proper scheduling
          const hasTimingExpressions = /\d+\.?\d*s(\.\.\.|!)?/.test(resolvedSource);
          // Check if source contains scroll/zoom that needs deferred execution
          const hasScrollZoom = /\(\s*(scroll|zoom)\s/.test(resolvedSource);
          

          
          if (hasTimingExpressions) {
            // console.log(`🎯 Detected timing expressions, preserving normal execution flow`);
            // Don't force immediate execution for timing expressions
            globalKidLispInstance.embeddedLayers = null; // Still clear embedded layers
          } else if (hasScrollZoom) {
            // console.log(`🎯 Detected scroll/zoom, allowing deferred execution`);
            // Allow scroll/zoom to use deferred execution but clear embedded layers
            globalKidLispInstance.inEmbedPhase = false; // Allow deferring for scroll/zoom
            globalKidLispInstance.isNestedInstance = false; // Allow deferring for scroll/zoom
            globalKidLispInstance.embeddedLayers = null; // Clear any leftover embedded layers
          } else {
            // console.log(`🎯 No timing/scroll/zoom detected, forcing immediate execution`);
            // Force immediate execution mode for simplified kidlisp() calls without timing or scroll/zoom
            globalKidLispInstance.inEmbedPhase = true; // Prevent deferring
            globalKidLispInstance.isNestedInstance = true; // Enable immediate execution
            globalKidLispInstance.embeddedLayers = null; // Clear any leftover embedded layers
          }
          
          executeLispCode(resolvedSource, api, false); // false = not accumulating, fresh painting
          
          // Restore original state to avoid interfering with other KidLisp operations
          globalKidLispInstance.inEmbedPhase = originalInEmbedPhase;
          globalKidLispInstance.isNestedInstance = originalIsNestedInstance;
          globalKidLispInstance.embeddedLayers = originalEmbeddedLayers;
          
          globalKidLispInstance.setAPI($activePaintApi);
        });
        // Always cache the painting for next frame continuity (even for animations)
        globalKidLispInstance.persistentPaintings.set(regionKey, painting);
      } else {
        // Build on existing painting (accumulate effects)
        const existingPainting = globalKidLispInstance.persistentPaintings.get(regionKey);
        // console.log(`🎨 Accumulating on existing painting for key: ${regionKey.slice(0, 50)}...`);
        
        painting = $activePaintApi.painting(width, height, (api) => {
          // Paste previous state first
          api.paste(existingPainting);
          // Then add new effects on top
          globalKidLispInstance.setAPI(api);
          
          // Add basic timing support for KidLisp commands
          if (!api.clock) {
            api.clock = { time: () => new Date() };
          }
          // Add scroll and zoom support for animations
          const originalZoom = api.zoom;
          const originalScroll = api.scroll || $activePaintApi.scroll;
          
          api.zoom = (...args) => {
            // console.log(`🔍 Zoom called in accumulation context: args=${JSON.stringify(args)}`);
            if (originalZoom && typeof originalZoom === 'function') {
              return originalZoom(...args);
            }
          };
          
          api.scroll = (dx, dy) => {
            // console.log(`📜 Scroll called in accumulation context: dx=${dx}, dy=${dy}`);
            if (originalScroll && typeof originalScroll === 'function') {
              return originalScroll(dx, dy);
            } else if ($activePaintApi.scroll && typeof $activePaintApi.scroll === 'function') {
              return $activePaintApi.scroll(dx, dy);
            }
          };
          // Add randomization support for ink() and other commands
          if (!api.num) {
            api.num = $activePaintApi.num || { 
              random: () => Math.random(),
              randInt: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min,
              rainbow: num.rainbow, // Use the actual rainbow function from num.mjs
              zebra: num.zebra // Use the actual zebra function from num.mjs
            };
          }
          // Add color support for proper ink randomization
          if (!api.color) {
            api.color = $activePaintApi.color || {
              random: () => [
                Math.floor(Math.random() * 256),
                Math.floor(Math.random() * 256), 
                Math.floor(Math.random() * 256)
              ]
            };
          }
          
          // 🎨 Add CSS color functions to make unquoted color words work
          // Import CSS colors from num.mjs and add them as functions to the API
          const cssColors = num.cssColors;
          if (cssColors) {
            Object.keys(cssColors).forEach(colorName => {
              if (!api[colorName]) {
                api[colorName] = () => cssColors[colorName];
              }
            });
          }
          
          // Add rainbow and zebra as top-level functions for unquoted usage
          if (!api.rainbow) {
            api.rainbow = () => num.rainbow();
          }
          if (!api.zebra) {
            api.zebra = () => num.zebra();
          }
          // Reset KidLisp color state to prevent cross-contamination between regions
          globalKidLispInstance.currentInk = null;
          
          // Save original state before configuring execution mode
          const originalInEmbedPhase = globalKidLispInstance.inEmbedPhase;
          const originalIsNestedInstance = globalKidLispInstance.isNestedInstance;
          const originalEmbeddedLayers = globalKidLispInstance.embeddedLayers;
          
          // Check if source contains timing expressions that need proper scheduling
          const hasTimingExpressions = /\d+\.?\d*s(\.\.\.|!)?/.test(resolvedSource);
          // Check if source contains scroll/zoom that needs deferred execution
          const hasScrollZoom = /\(\s*(scroll|zoom)\s/.test(resolvedSource);
          
          // Only log occasionally to reduce console spam

          
          if (hasTimingExpressions) {
            // console.log(`🎯 Detected timing expressions in accumulation, preserving normal execution flow`);
            // Don't force immediate execution for timing expressions
            globalKidLispInstance.embeddedLayers = null; // Still clear embedded layers
          } else if (hasScrollZoom) {
            // console.log(`🎯 Detected scroll/zoom in accumulation, allowing deferred execution`);
            // Allow scroll/zoom to use deferred execution but clear embedded layers
            globalKidLispInstance.inEmbedPhase = false; // Allow deferring for scroll/zoom
            globalKidLispInstance.isNestedInstance = false; // Allow deferring for scroll/zoom
            globalKidLispInstance.embeddedLayers = null; // Clear any leftover embedded layers
          } else {
            // console.log(`🎯 No timing/scroll/zoom in accumulation, forcing immediate execution`);
            // Force immediate execution mode for simplified kidlisp() calls without timing or scroll/zoom
            globalKidLispInstance.inEmbedPhase = true; // Prevent deferring
            globalKidLispInstance.isNestedInstance = true; // Enable immediate execution
            globalKidLispInstance.embeddedLayers = null; // Clear any leftover embedded layers
          }
          
          executeLispCode(resolvedSource, api, true); // true = accumulating on existing painting
          
          // Restore original state to avoid interfering with other KidLisp operations
          globalKidLispInstance.inEmbedPhase = originalInEmbedPhase;
          globalKidLispInstance.isNestedInstance = originalIsNestedInstance;
          globalKidLispInstance.embeddedLayers = originalEmbeddedLayers;
          
          globalKidLispInstance.setAPI($activePaintApi);
        });
        // Update the cache with the new accumulated state (unless noCache is true or animation content)
        if (!noCache || !needsFreshExecution) {
          globalKidLispInstance.persistentPaintings.set(regionKey, painting);
        }
      }
      
      // Paste the painting to the specified location
      if ($activePaintApi.paste && painting) {
        // console.log(`🎯 Pasting painting to (${x},${y})`);
        $activePaintApi.paste(painting, x, y);
      }
      
      return painting;
      
    } catch (error) {
      console.error("🚫 Simple KidLisp error:", error);
      
      // Draw error directly to main screen
      const originalInk = $activePaintApi.ink();
      $activePaintApi.ink(255, 0, 0);
      if ($activePaintApi.write) {
        $activePaintApi.write("KidLisp Error", x, y);
      }
      $activePaintApi.ink(originalInk);
      
      return null;
    }
  },
  // 🎵 Update KidLisp audio globals (accessible to all pieces)
  updateKidLispAudio: updateKidLispAudio,
  // glaze: ...
};

// 🎨 Expose wipe function globally for KidLisp reframe operations
if (typeof globalThis !== "undefined") {
  globalThis.$paintApiUnwrapped = $paintApiUnwrapped;
  globalThis.wipe = $paintApiUnwrapped.wipe;
}

// Helper function to execute KidLisp code
function executeLispCode(source, api, isAccumulating = false) {
  try {
    // Parse and evaluate the source directly without going through module lifecycle
    // console.log(`🔍 Parsing KidLisp source:`, source);
    
    // Clear previous first-line color state for fresh detection

    globalKidLispInstance.firstLineColor = null;
    
    globalKidLispInstance.parse(source);
    // console.log(`🔍 Generated AST:`, globalKidLispInstance.ast);
    
    if (globalKidLispInstance.ast) {
      // Detect first-line color but only apply it if not accumulating
      globalKidLispInstance.detectFirstLineColor();

      if (globalKidLispInstance.firstLineColor && !isAccumulating) {
        // console.log(`🎨 Applying first-line color background: ${globalKidLispInstance.firstLineColor}`);
        api.wipe(globalKidLispInstance.firstLineColor);
      }
      
      // Execute the parsed AST using the main evaluate method
      // console.log(`🔍 Executing AST using main evaluate method...`);
      // console.log(`🔍 AST contains:`, globalKidLispInstance.ast.map(expr => Array.isArray(expr) ? expr[0] : expr));
      
      // Set up proper timing environment by ensuring clock and frameCount are available
      if (!api.clock) {
        api.clock = { time: () => new Date() };
      }
      
      // Increment frame count for timing expressions
      if (typeof globalKidLispInstance.frameCount !== 'number') {
        globalKidLispInstance.frameCount = 0;
      }
      globalKidLispInstance.frameCount++;
      
      // Normal KidLisp evaluation (dollar codes already resolved)
      const result = globalKidLispInstance.evaluate(globalKidLispInstance.ast, api, globalKidLispInstance.localEnv);
      // console.log(`🔍 Evaluation result:`, result);
    } else {
      // console.log(`⚠️ No AST generated from source`);
    }
  } catch (evalError) {
    console.error("🚫 KidLisp evaluation error:", evalError);
    // Draw error indicator
    api.wipe(60, 0, 0);
    api.ink(255, 255, 255);
    if (api.write) {
      api.write("KidLisp Eval Error", 5, 15);
    }
  }
}

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
      if (k === "page") p.pagern = args[0];
      // TODO: 😅 Add other state globals like line thickness? 23.1.25
    }

    for (const k in $paintApiUnwrapped) {
      if (typeof $paintApiUnwrapped[k] === "function") {
        // Wrap and then transfer to #api.
        p.api[k] = function () {
          if (k === "ink") {
            $paintApiUnwrapped[k](...arguments);
            p.inkrn = graph.c.slice();
          } else {
            globals(k, arguments); // Keep track of global state, like page, via `pagern`.
          }
          // Create layer if necessary.
          if (notArray(p.#layers[p.#layer])) p.#layers[p.#layer] = [];
          const callArgs = arguments;
          // Add each deferred paint api function to the layer, to be run
          // all at once in `paint` on each frame update.
          p.#layers[p.#layer].push([
            k,
            () => {
              if (k === "ink") {
                $paintApiUnwrapped[k](...callArgs);
                p.inkrn = graph.c.slice();
              } else {
                globals(k, callArgs); // Update globals again on chainable calls.
                $paintApiUnwrapped[k](...callArgs);
              }
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
  if (screen.width === width && screen.height === height && gap === lastGap)
    return;

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

  // Reset / recreate the depth buffer. (This is only used for the 3D software renderer in `graph`)
  graph.depthBuffer.length = screen.width * screen.height;
  graph.depthBuffer.fill(Number.MAX_VALUE);
  graph.writeBuffer.length = 0; //screen.width * screen.height;
  // graph.writeBuffer.fill(Number.MAX_VALUE);

  screen.pixels = new Uint8ClampedArray(screen.width * screen.height * 4);
  screen.pixels.fill(255);

  graph.setBuffer(screen);
  graph.paste({
    painting: oldScreen,
    crop: new geo.Box(0, 0, oldScreen.width, oldScreen.height),
  });
  
  // 🎨 Fill any new pixels with background color after screen expansion
  if (width > oldScreen.width || height > oldScreen.height) {
    const persistentColor = getPersistentFirstLineColor();
    console.log("🎨 Resolution function: Screen expansion detected");
    console.log("🎨 Resolution function: Old screen:", oldScreen.width, "x", oldScreen.height);
    console.log("🎨 Resolution function: New screen:", width, "x", height);
    console.log("🎨 Resolution function: Persistent color found:", persistentColor);

    let fillSpec = resolveBackgroundFillSpec(persistentColor);
    if (!fillSpec && typeof globalKidLispInstance?.getBackgroundFillColor === "function") {
      const fallbackColor = globalKidLispInstance.getBackgroundFillColor();
      console.log("🎨 Post-reframe: Fallback background candidate:", fallbackColor);
      fillSpec = resolveBackgroundFillSpec(fallbackColor);
    }

    if (fillSpec) {
      if (fillSpec.type === "fade") {
        console.log("🎨 Post-reframe: Applying fade background to expansions");
        fillExpandedWithFadePixels(
          screen,
          width,
          height,
          oldScreen.width,
          oldScreen.height,
          fillSpec.fadeInfo,
        );
      } else {
        console.log("🎨 Post-reframe: Filling expansions with solid color", fillSpec.rgba);
        fillExpandedWithSolidPixels(
          screen,
          width,
          height,
          oldScreen.width,
          oldScreen.height,
          fillSpec.rgba,
        );
      }
    } else {
      console.log("🎨 Post-reframe: No background fill spec available for expansion");
    }
  } else {
    console.log("🎨 Resolution function: No screen expansion needed");
  }
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
  frequencies = { left: [], right: [] };
  beat = { detected: false, strength: 0, timestamp: 0 }; // Add beat detection data

  poll() {
    send({ type: "get-waveforms" });
    send({ type: "get-amplitudes" });
    send({ type: "get-frequencies" });
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

// 🎮 Game Boy Emulator API
const gameboy = {
  frame: null,         // Current frame data (Uint8ClampedArray)
  width: 160,          // Game Boy screen width
  height: 144,         // Game Boy screen height  
  romName: null,       // Currently loaded ROM name
  isPlaying: false     // Whether emulator is running
};

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
    //                                 Note: This fixes a preview bug on teia.art. 2022.04.07.03.00    if (path === "") path = ROOT_PIECE; // Set bare path to what "/" maps to.
    // if (path === firstPiece && params.length === 0) params = firstParams;

    // Check if the path already has a .lisp extension and use it directly, otherwise default to .mjs
    // Handle sandboxed environments where location.protocol might be "null:"
    // For aesthetic.computer pieces, determine the correct server
    const { protocol, hostname } = getSafeUrlParts();
    
    // If we're loading an aesthetic.computer piece, choose the appropriate server
    let baseUrl;
    if (path.startsWith('aesthetic.computer/')) {
      // Check if we're in OBJKT mode - use local bundled files
      if (getPackMode()) {
        // In OBJKT mode, use relative paths to load bundled pieces
        baseUrl = ".";
      } else {
        // Check if we're in a development environment (localhost with port)
        const isDevelopment = hostname === 'localhost' && typeof location !== 'undefined' && location.port;
        if (isDevelopment) {
          // Use the local development server
          baseUrl = `${protocol}//${hostname}:${location.port}`;
        } else if (hostname.includes('aesthetic.computer')) {
          // If we're on any aesthetic.computer subdomain, use the same origin to avoid CORS
          baseUrl = `${protocol}//${hostname}`;
        } else {
          // Use the production server for sandboxed iframes or production
          baseUrl = `https://aesthetic.computer`;
        }
      }
    } else {
      baseUrl = `${protocol}//${hostname}`;
    }
    
    // if (debug) console.log("🔍 Debug getSafeUrlParts:", { protocol, hostname, baseUrl, isSandboxed: isSandboxed(), path, isDevelopment: hostname === 'localhost' && typeof location !== 'undefined' && location.port });
    
    // Check if path already includes the hostname to avoid double paths
    let resolvedPath = path;
    if (getPackMode() && path.startsWith('aesthetic.computer/')) {
      // In OBJKT mode, keep the full path since files are bundled with directory structure
      resolvedPath = path;
    } else if (baseUrl === 'https://aesthetic.computer' && path.startsWith('aesthetic.computer/')) {
      // Only strip "aesthetic.computer/" if we're using the main production domain
      resolvedPath = path.substring('aesthetic.computer/'.length);
    }
    
    // if (debug) console.log("🔍 Debug path resolution:", { originalPath: path, resolvedPath, hostname, baseUrl });
    
    if (path.endsWith('.lisp')) {
      if (getPackMode()) {
        // In OBJKT mode, use absolute path from iframe origin
        fullUrl = "/" + resolvedPath + "#" + Date.now();
      } else {
        fullUrl = baseUrl + "/" + resolvedPath + "#" + Date.now();
      }
    } else {
      if (getPackMode()) {
        // In OBJKT mode, navigate up from lib directory to aesthetic.computer root, then to target
        const relativePath = resolvedPath.startsWith('aesthetic.computer/') 
          ? resolvedPath.substring('aesthetic.computer/'.length)
          : resolvedPath;
        fullUrl = "../" + relativePath + ".mjs" + "#" + Date.now();
      } else {
        fullUrl = baseUrl + "/" + resolvedPath + ".mjs" + "#" + Date.now();
      }
    }
    // The hash `time` parameter busts the cache so that the environment is
    // reset if a disk is re-entered while the system is running.
    // Why a hash? See also: https://github.com/denoland/deno/issues/6946#issuecomment-668230727
    // if (debug) console.log("🕸", fullUrl);
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
      
      // 💾 Check if this is a cached kidlisp code (starts with $ and has content after it)
      if (slug && slug.startsWith("$") && slug.length > 1) {
        const cacheId = slug.slice(1); // Remove $ prefix
        if (logs.loading) console.log("💾 Loading cached kidlisp code:", cacheId);
        try {
          sourceToRun = await getCachedCodeMultiLevel(cacheId);
          if (!sourceToRun) {
            throw new Error(`Cached code not found: ${cacheId}`);
          }
          // Track the original $code identifier for sharing
          currentOriginalCodeId = slug; // Keep the full $code format
          if (logs.loading) console.log("✅ Successfully loaded cached code:", cacheId);
        } catch (error) {
          console.error("❌ Failed to load cached code:", cacheId, error);
          throw new Error(`Failed to load cached code: ${cacheId}`);
        }
      } else if (fullUrl) {
        // In OBJKT mode, try direct import first for .mjs files to avoid CSP issues
        // Extract the filename from URL, handling ./ prefix and hash fragments
        const urlWithoutHash = fullUrl.split('#')[0];
        const filename = urlWithoutHash.split('/').pop();
        
        if (getPackMode() && filename.endsWith('.mjs')) {
          // In OBJKT mode, skip dynamic import and use fetch directly since files are bundled locally
          // Will proceed to fetch() below
        }
        
        let response;
        if (logs.loading) console.log("📥 Loading from url:", fullUrl);
        // if (debug) console.log("🔍 Debug: Constructed fullUrl:", fullUrl);
        response = await fetch(fullUrl);        if (response.status === 404 || response.status === 403) {
          const extension = path.endsWith('.lisp') ? '.lisp' : '.mjs';
          // Handle sandboxed environments for anon URL construction
          const { protocol } = getSafeUrlParts();
          const anonUrl =
            protocol +
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
        (path && path.endsWith(".lisp")) ||
        (slug && slug.startsWith("$") && slug.length > 1) // Cached codes are always kidlisp
      ) {
        // Only use basic detection, not the broader isKidlispSource function
        // which can incorrectly detect JavaScript as kidlisp, unless forceKidlisp is true
        // or this came from parse function as kidlisp (slug/path === "(...)")
        // Assume lisp.
        // console.log(
        //   "🐍 Lisp piece detected (slug:",
        //   slug,
        //   "path:",
        //   path,
        //   "forceKidlisp:",
        //   forceKidlisp,
        //   ")",
        // );
        sourceCode = sourceToRun;
        originalCode = sourceCode;
        
        // Initialize persistent cache for $codes (only needs to be done once)
        initPersistentCache(store);
        
        loadedModule = lisp.module(sourceToRun, path && path.endsWith(".lisp"));

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

        blobUrl = URL.createObjectURL(blob);        sourceCode = updatedCode;
        loadedModule = await import(blobUrl);
      }
    }
  } catch (err) {
    console.log("🟡 Error loading mjs module:", err);
    // Look for lisp files if the mjs file is not found, but only if we weren't already trying to load a .lisp file
    if (fullUrl && !fullUrl.includes('.lisp')) {
      try {
        fullUrl = fullUrl.replace(".mjs", ".lisp");
        let response;
        if (logs.loading) console.log("📥 Loading lisp from url:", fullUrl);
        response = await fetch(fullUrl);
        console.log("🤖 Response:", response);

      if (response.status === 404 || response.status === 403) {
        // Handle sandboxed environments for anon URL construction
        const { protocol } = getSafeUrlParts();
        const anonUrl =
          protocol +
          "//" +
          "art.aesthetic.computer" +
          "/" +
          path.split("/").pop() +
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
      loadedModule = lisp.module(sourceCode, true); // This is loading a .lisp file
      
      if (devReload) {
        store["publishable-piece"] = { slug, source: sourceCode, ext: "lisp" };
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
    headers($commonApi.dark); // Clear console and re-print headers if we are in production.
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
    // Skip socket connections in OBJKT mode
    if (getPackMode()) {
      return;
    }
    
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
    
    // Check if we're in OBJKT mode and have colophon data
    let objktContext = null;
    if (checkPackMode() && typeof window !== 'undefined' && window.acOBJKT_COLOPHON) {
      objktContext = { author: window.acOBJKT_COLOPHON.build.author };
    }
    
    const { title, desc, ogImage, twitterImage, icon } = metadata(
      location.host, // "aesthetic.computer",
      slug,
      loadedModule.meta?.({
        ...parsed,
        num: $commonApi.num,
        store: $commonApi.store,
      }) || inferTitleDesc(originalCode),
      location.protocol, // Pass the current protocol
      objktContext // Pass OBJKT context if available
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
        // Handle sandboxed environments for path construction
        const { protocol, hostname } = getSafeUrlParts();
        const originalPath = path;
        
        // Apply the same origin-aware logic as in module loading
        let baseUrl;
        if (path.startsWith('aesthetic.computer/')) {
          // Check if we're in a development environment (localhost with port)
          const isDevelopment = hostname === 'localhost' && typeof location !== 'undefined' && location.port;
          if (isDevelopment) {
            // Use the local development server
            baseUrl = `${protocol}//${hostname}:${location.port}`;
          } else if (hostname.includes('aesthetic.computer')) {
            // If we're on any aesthetic.computer subdomain, use the same origin to avoid CORS
            baseUrl = `${protocol}//${hostname}`;
          } else {
            // Use the production server for sandboxed iframes or production
            baseUrl = `https://aesthetic.computer`;
          }
          
          // Only strip "aesthetic.computer/" if we're using the main production domain
          if (baseUrl === 'https://aesthetic.computer') {
            path = path.substring('aesthetic.computer/'.length);
          }
        } else {
          baseUrl = `${protocol}//${hostname}`;
        }
        
        path = `${baseUrl}/${path}`;
        
        // Removed debug log for font glyph preload
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
  $commonApi.hash = hash;

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
    graph.unmask(); // Clear any active mask when leaving a piece

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
      send({ type: "back-to-piece" });
    } else {
      $commonApi.jump("prompt");
    }
  };

  if (!alias) $commonApi.pieceCount += 1; // Don't bump pieceCount if aliased.

  // Load typeface if it hasn't been yet.
  // (This only has to happen when the first piece loads.)
  if (!tf) tf = await new Typeface(/*"unifont"*/).load($commonApi.net.preload);
  $commonApi.typeface = tf; // Expose a preloaded typeface globally.
  ui.setTypeface(tf); // Set the default `ui` typeface.

  // Initialize HUD metrics now that the primary typeface is available
  currentHUDLabelBlockWidth = tf.blockWidth;
  currentHUDLabelBlockHeight = tf.blockHeight;
  currentHUDShareWidth = tf.blockWidth * "share ".length;

  // Initialize MatrixChunky8 font for QR code text rendering and keep it warm across pieces
  let matrixFont = typefaceCache.get("MatrixChunky8");

  if (!matrixFont) {
    matrixFont = new Typeface("MatrixChunky8");
    await matrixFont.load($commonApi.net.preload); // Important: call load() to initialize the proxy system
    typefaceCache.set("MatrixChunky8", matrixFont);
  }

  if (matrixFont && !matrixFont.__preloadedCommonGlyphs) {
    // Pre-load common QR code characters to avoid fallback during rendering
    const commonQRChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$";

    // Access each character to trigger the Proxy loading mechanism and collect promises
    const glyphPromises = [];

    for (const char of commonQRChars) {
      // Accessing the glyph triggers the Proxy to start loading it
      const glyph = matrixFont.glyphs[char];

      // If it's a Promise, collect it to wait for loading
      if (glyph && typeof glyph.then === "function") {
        glyphPromises.push(glyph);
      }
    }

    // Wait for all common glyphs to finish loading
    if (glyphPromises.length > 0) {
      try {
        await Promise.all(glyphPromises);
      } catch (e) {
        console.warn("🔤 Some MatrixChunky8 glyphs failed to preload:", e);
      }
    }

    matrixFont.__preloadedCommonGlyphs = true;
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

    // 🧹 Reset KidLisp baked layers when returning to the prompt to avoid stale flashes
    if (module?.system?.startsWith("prompt") &&
        globalKidLispInstance?.clearBakedLayers) {
      globalKidLispInstance.clearBakedLayers();
      graph.unmask(); // Clear mask when returning to prompt from KidLisp
    }

    if (!module.system?.startsWith("world"))
      $commonApi.system.world.teleported = false;

    // 📚 nopaint system
    if (
      module.system?.startsWith("nopaint") ||
      typeof module?.brush === "function" ||
      typeof module?.lift === "function" ||
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

      boot = async ($) => {
        // Reset scroll state when a new piece boots
        graph.resetScrollState();
        // Reset mask state to prevent masks from persisting across pieces
        graph.unmask();
        
        const booter = module.boot || nopaint_boot;
        booter($);
        if (chatEnabled) {
          try {
            const chatModule = await import("../disks/chat.mjs");
            chatModule.boot($);
          } catch (err) {
            console.log("💬 Chat disabled in OBJKT mode");
          }
        }
      };

      sim = module.sim || defaults.sim;
      paint = async ($) => {
        let painted = false;
        
        if (module.paint) {
          painted = module.paint($);
          if ($.system?.nopaint) {
            $.system.nopaint.needsPresent = true;
          }
        }

        // 🎨 OVERLAY SUPPORT: Call overlay function for preview rendering with painting coordinates
        if (module.overlay && $.system.nopaint?.brush?.dragBox) {
          // Transform painting coordinates to screen coordinates for direct screen rendering
          const originalDragBox = $.system.nopaint.brush.dragBox;
          const zoom = $.system.nopaint.zoomLevel;
          const tx = $.system.nopaint.translation.x;
          const ty = $.system.nopaint.translation.y;
          
          // Create screen-space dragBox
          const screenDragBox = {
            x: originalDragBox.x * zoom + tx,
            y: originalDragBox.y * zoom + ty,
            w: originalDragBox.w * zoom,
            h: originalDragBox.h * zoom
          };
          
          // Temporarily replace the dragBox with screen coordinates
          const originalBrush = $.system.nopaint.brush;
          $.system.nopaint.brush = { ...originalBrush, dragBox: screenDragBox };
          
          // Add color word for overlay function
          $.color = $.system.nopaint.color;
          
          // Add mark word for overlay function (preview dragBox)
          $.mark = $.system.nopaint.brush?.dragBox;
          
          // Render overlay directly to screen buffer 
          module.overlay($);
          
          // Restore original dragBox
          $.system.nopaint.brush = originalBrush;
        }

        // 📊 Always render nopaint performance HUD if enabled (regardless of module.paint result)
        if (system === "nopaint" && nopaintPerf) {
          nopaint_renderPerfHUD($);
          // Force continuous painting for nopaint system when perf HUD is enabled
          painted = true;
        }

        if (painted) {
          return painted;
        }

        // TODO: Pass in extra arguments here that flag the wipe.
        if (chatEnabled) {
          try {
            const chatModule = await import("../disks/chat.mjs");
            chatModule.paint($, { embedded: true }); // Render any chat interface necessary.
          } catch (err) {
            console.log("💬 Chat disabled in OBJKT mode");
          }
        }
      };
      beat = module.beat || defaults.beat;
      brush = module.brush;
      lift = module.lift;
      filter = module.filter;
      act = ($) => {
        nopaint_act($); // Inherit base functionality.
        
        // 🎯 IMMEDIATE BAKING: Fix race condition by triggering bake immediately after lift
        const np = $.system?.nopaint;
        if (np?.needsBake === true && bake) {
          // 📊 Trigger bake flash effect and beep sound
          nopaint_triggerBakeFlash();
          
          // 🔊 Microwave-style beep for bake completion (only for robo)
          if (currentPath?.includes('robo')) {
            $commonApi.sound.synth({
              tone: 800, // Higher pitched beep like a microwave
              duration: 0.1,
              attack: 0.01,
              decay: 0.5,
              volume: 0.3,
            });
          }
          
          // 🎨 ELEGANT BRUSH PATTERN: Call brush or lift function for final painting
          if (brush || lift) {
            const finalBrushApi = { ...$ };
            // Add top-level 'color' word that maps to system.nopaint.color
            finalBrushApi.color = $.system.nopaint.color;
            
            // Add top-level 'mark' word that maps to finalDragBox for brush
            finalBrushApi.mark = $.system.nopaint.finalDragBox;
            
            // 🎯 Use preserved coordinates since brush is null at bake time
            const preservedDragBox = $.system.nopaint.finalDragBox;
            const preservedStartDrag = $.system.nopaint.finalStartDrag;
            
            if (preservedDragBox) {
              // Create a pen object with the preserved coordinates
              finalBrushApi.pen = {
                dragBox: preservedDragBox,
                x: preservedStartDrag?.x || preservedDragBox.x,
                y: preservedStartDrag?.y || preservedDragBox.y
              };
            } else {
              finalBrushApi.pen = $.system.nopaint.brush; // Fallback to original (likely null)
            }
            
            finalBrushApi.lift = true; // 🎯 Single clean flag for final brush call
            
            $.page($.system.painting); // Set context to painting surface
            
            // Call lift function if it exists, otherwise fall back to brush
            if (lift) {
              lift(finalBrushApi); // Call lift for final painting
            } else if (brush) {
              brush(finalBrushApi); // Call brush for final painting (legacy)
            }
            
            $.page($.screen); // Reset context
          }
          
          $.page($.system.painting);
          bake($);
          
          // 🧹 CLEAR BUFFER: Prevent flickering by clearing buffer BEFORE presentation
          $.page($.system.nopaint.buffer).wipe(255, 255, 255, 0);
          
          $.page($.screen);
          np.present($);
          np.needsBake = false;
          
          // 🚀 Broadcast bake completion
          $commonApi.broadcastPaintingUpdateImmediate("baked", {
            source: "immediate_bake",
            piece: loadedModule?.meta?.()?.title || "unknown"
          });
        }
        
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
        $commonApi.system.fps = { doll, renderStats: graph.renderStats };
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
      // Reset scroll state when a piece loads
      graph.resetScrollState();
      // Reset mask state to prevent masks from persisting across pieces
      graph.unmask();
      
      boot = module.boot || defaults.boot;
      sim = module.sim || defaults.sim;
      paint = module.paint || defaults.paint;
      beat = module.beat || defaults.beat;
      act = module.act || defaults.act;
      leave = module.leave || defaults.leave;
      receive = module.receive || defaults.receive; // Handle messages from BIOS
      
      // 🎨 AUTO-DETECT BRUSH FUNCTIONS: If a piece exports a brush or lift function, automatically use nopaint system
      system = module.system || (module.brush || module.lift ? "nopaint" : null);
      
      // 🎨 AUTO-GENERATE BAKE: If using brush/lift but no explicit bake function, create a default one
      if ((module.brush || module.lift) && !module.bake) {
        bake = ({ paste, system, page }) => {
          paste(system.nopaint.buffer);
          page(system.nopaint.buffer).wipe(255, 255, 255, 0);
        };
      }

      // delete $commonApi.system.name; // No system in use.
    }

    preview = module.preview || defaults.preview; // Set preview method.
    icon = module.icon || defaults.icon; // Set preview method.

    // ♻️ Reset global state for this piece.
    paintCount = 0n;
    paintingAPIid = 0n;
    simCount = 0n;
    booted = false;
    // initialSim = true;
    activeVideo = null;
    videoSwitching = false;
    lastActiveVideo = null;
    keys(preloadPromises).forEach((key) => preloadPromises[key].reject(key));
    preloadPromises = {};
    noPaint = false;
    
    // 🎞️ Reset piece-level FPS control
    pieceFPS = null;
    lastPaintTime = 0;
    lastPaintOut = undefined;
    shouldSkipPaint = false;
    pieceFrameCount = 0;
    
    formsSent = {}; // Clear 3D list for GPU.
    currentPath = path;
    currentHost = host;
    currentSearch = search;
    // console.log("Set currentSearch to:", search);
    firstPreviewOrIcon = true;
    
  // Parse search parameters properly to check for nolabel
  hideLabel = false;
  if (parsed.search) {
    const searchParams = new URLSearchParams(parsed.search);
    hideLabel = searchParams.has("nolabel");
  }
  
  // Also hide label by default in OBJKT mode (like nolabel)
  if (getPackMode()) {
    hideLabel = true;
  }    currentColon = colon;
    currentParams = params;
    currentHash = hash;
    // sound = null;
    glazeEnabled = null;
    // soundClear = null;
    hourGlasses.length = 0;
    // labelBack = false; // Now resets after a jump label push. 25.03.22.21.36

    // Parse search parameters properly to check for preview and icon
    previewMode = false;
    iconMode = false;
    if (parsed.search) {
      const searchParams = new URLSearchParams(parsed.search);
      previewMode = searchParams.has("preview");
      iconMode = searchParams.has("icon");
    }

    // console.log("🔴 PREVIEW OR ICON:", PREVIEW_OR_ICON, "Preview mode:", previewMode, "Icon mode:", iconMode);
    // console.log("📑 Search:", parsed.search);
    // console.log("🖼️ ICON MODE:", iconMode);

    previewOrIconMode = previewMode || iconMode;
    paintings = {}; // Reset painting cache.
    // Reset KidLisp persistent paintings cache to avoid stale embedded content
    if (globalKidLispInstance?.persistentPaintings) {
      globalKidLispInstance.persistentPaintings.clear();
    }
    prefetches?.forEach((p) => prefetchPicture(p)); // Prefetch parsed media.
    graph.color2(null); // Remove any secondary color that was added from another piece.
    // 🐢 Reset turtle state.
    turtleAngle = 270;
    turtleDown = false;
    turtlePosition = { x: screen.width / 2, y: screen.height / 2 };

    //$api.fps = function (newFps) {
    // No longer reset global FPS when loading new pieces
    // send({ type: "fps-change", content: undefined });
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
      currentHUDTxt = slug; // Update hud if this is not an alias.
      currentHUDPlainTxt = stripColorCodes(slug);
      
      // Hide HUD label for RGB-only kidlisp pieces (e.g., "255 0 0")
      // since they're just background colors and don't need to be displayed
      if (sourceCode && lisp?.isValidRGBString && lisp.isValidRGBString(sourceCode.trim())) {
        currentHUDTxt = undefined;
        currentHUDPlainTxt = undefined;
      }
    }
    if (module.nohud || system === "prompt") {
      currentHUDTxt = undefined;
      currentHUDPlainTxt = undefined;
    }
    // Clear original code ID for non-$code pieces (unless this was a $code load)
    if (!slug?.startsWith("$")) {
      currentOriginalCodeId = undefined;
    }
    currentHUDOffset = undefined; // Always reset these to the defaults.
    currentHUDTextColor = undefined;
    currentHUDStatusColor = "red"; //undefined;
    currentHUDButton = undefined;
    currentHUDScrub = 0;
  currentHUDLeftPad = 0;
    // currentPromptButton = undefined;

    // Push last piece to a history list, skipping prompt and repeats.
    if (
      !fromHistory &&
      currentText &&
      currentText !== "prompt" &&
      currentText !== $commonApi.history[$commonApi.history.length - 1]
    ) {
      $commonApi.history.push(currentText);
    }

    currentText = slug;
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

  send({
    type: "disk-loaded",
    content: {
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
    },
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
  setPackMode(content.objktMode || false);    // Store OBJKT KidLisp cache in worker global scope
    if (content.objktKidlispCodes) {
      const globalScope = (function() {
        if (typeof globalThis !== 'undefined') return globalThis;
        if (typeof self !== 'undefined') return self;
        if (typeof global !== 'undefined') return global;
        return {};
      })();
      globalScope.objktKidlispCodes = content.objktKidlispCodes;
    }
    
    TV_MODE = content.resolution?.tv === true;
    
    // Parse highlight parameter
    const highlightParam = content.resolution?.highlight;
    if (highlightParam) {
      HIGHLIGHT_MODE = true;
      if (highlightParam !== true && typeof highlightParam === 'string') {
        // If it's a color string, parse it
        if (highlightParam.startsWith('#')) {
          // Convert hex to RGB
          const hex = highlightParam.slice(1);
          const r = parseInt(hex.slice(0, 2), 16);
          const g = parseInt(hex.slice(2, 4), 16);
          const b = parseInt(hex.slice(4, 6), 16);
          HIGHLIGHT_COLOR = `${r},${g},${b}`;
        } else if (highlightParam.includes(',')) {
          // Already RGB format
          HIGHLIGHT_COLOR = highlightParam;
        } else {
          // Named color or other format - use CSS color parser or default
          HIGHLIGHT_COLOR = highlightParam;
        }
      }
    } else {
      HIGHLIGHT_MODE = false;
    }

    microphone.permission = content.microphonePermission;

    $commonApi.canShare = SHARE_SUPPORTED;
    $commonApi.vscode = VSCODE; // Add vscode flag to the common api.
    $commonApi.net.lan = LAN_HOST;
    $commonApi.user = USER;
    $commonApi.net.iframe = content.iframe;
    $commonApi.net.sandboxed = content.sandboxed;
    $commonApi.net.tvMode = TV_MODE; // Add TV mode flag to common API

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
    } else if (getPackMode()) {
      // Skip chat connection in PACK mode - offline bundle
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
      // Log tab ID for debugging
      if (!$commonApi._tabId) {
        $commonApi._tabId = Math.random().toString(36).substr(2, 9);
      }
      // Broadcast to other tabs...
      $commonApi.broadcast("login:success");
    } else {
      // console.log("🔐 You are not logged in.");
      // Generate tab ID for debugging (anonymous user)
      if (!$commonApi._tabId) {
        $commonApi._tabId = Math.random().toString(36).substr(2, 9);
      }
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
    if (!cachedAPI) return; // Guard against undefined cachedAPI
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

  // Load the source code for a dropped `.mjs` file.
  if (type === "dropped:piece") {
    load(content, false, false, true);
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

  // Handle dropped ALS files - route to piece receive function if available
  if (type === "dropped:als") {
    console.log("🎵 DISK.MJS: Received dropped:als message:", content);
    
    // First try to call the piece's receive function if it exists
    if (typeof receive === "function") {
      console.log("🎵 DISK.MJS: Calling piece receive function");
      try {
        receive({ type, content });
      } catch (e) {
        console.warn("🎵 DISK.MJS: Piece receive function error:", e);
      }
    } else {
      console.log("🎵 DISK.MJS: No piece receive function found, triggering act event");
      // Fall back to act event pattern
      const $api = cachedAPI;
      if ($api) {
        const data = { 
          name: content.name, 
          xmlData: content.xmlData,
          type: "dropped:als"
        };
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
    }
    return;
  }

  // Handle dropped WAV files - route to piece receive function if available
  if (type === "dropped:wav") {
    console.log("🎵 DISK.MJS: Received dropped:wav message:", content);
    
    // First try to call the piece's receive function if it exists
    if (typeof receive === "function") {
      console.log("🎵 DISK.MJS: Calling piece receive function for WAV");
      try {
        receive({ type, content });
      } catch (e) {
        console.warn("🎵 DISK.MJS: Piece receive function error for WAV:", e);
      }
    } else {
      console.log("🎵 DISK.MJS: No piece receive function found for WAV, triggering act event");
      // Fall back to act event pattern
      const $api = cachedAPI;
      if ($api) {
        const data = { 
          name: content.name || content.originalName,
          id: content.id,
          size: content.size,
          type: "dropped:wav"
        };
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

  // Forward recorder export events to the current piece, but only when not in export mode
  // During export (when actEvents is undefined), let events pass through directly to main thread
  if (
    type === "recorder:export-status" ||
    type === "recorder:export-progress" ||
    type === "recorder:transcode-progress" ||
    type === "recorder:export-complete"
  ) {
    // If actEvents is available, route to the piece as normal
    if (typeof actEvents !== 'undefined' && actEvents && Array.isArray(actEvents)) {
      const event = {
        is: (eventType) => eventType === type,
        type: type,
        content: content,
        progress: type === "recorder:transcode-progress" ? content : undefined
      };
      
      actEvents.push(event);
      if (debug) console.log("📼 ✅ Used actEvents for export event:", type);
      return; // Don't let it pass through to main thread
    } else {
      // During export, actEvents is undefined - let the event pass through directly
      // to main thread without trying to route it through the piece system
      // (Silent mode - no debug logging to avoid console spam)
      // Don't return here - let it fall through to normal send() handling
    }
  }

  if (
    type === "recorder:rolling:started" ||
    type === "recorder:rolling:resumed"
  ) {
    $commonApi.rec.recording = true;
    $commonApi.rec.rollingCallback?.(content.time);
    return;
  }

  if (type === "recorder:rolling:ended") {
    $commonApi.rec.recording = false;
    $commonApi.rec.recorded = true; // Also cleared when a recording "slates".
    
    if ($commonApi.rec.cutCallback) {
      try {
        $commonApi.rec.cutCallback();
        // Clear the callback after execution to prevent duplicate calls
        $commonApi.rec.cutCallback = null;
      } catch (error) {
        console.error(`🎬 ❌ Error in cutCallback:`, error);
        console.error(`🎬 ❌ Stack trace:`, error.stack);
      }
    } else {
      console.warn(`🎬 ⚠️ No cutCallback available when rolling ended`);
      // Fallback: try to jump to video directly if no callback is set
      $commonApi.jump("video");
    }
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
    $commonApi.rec.playing = false;
    return;
  }

  if (type === "recorder:unpresented") {
    $commonApi.rec.presenting = false;
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

  if (type === "sfx:progress:report") {
    sfxProgressReceivers[content.id]?.(content); // Resolve the progress report.
    return;
  }

  if (type === "sfx:got-duration") {
    sfxDurationReceivers[content.id]?.(content.duration);
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

  if (type === "frequencies") {
    speaker.frequencies = content;
    // Extract beat data if it exists in the content
    if (content.beat) {
      speaker.beat = content.beat;
    }
    return;
  }

  // Handle Game Boy frame data
  if (type === "gameboy:frame-data") {
    // Store frame data and metadata in gameboy API object
    gameboy.frame = content.pixels;
    gameboy.romName = content.romName;
    gameboy.title = content.title; 
    gameboy.isGameBoyColor = content.isGameBoyColor;
    gameboy.isPlaying = true;
    
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
    $commonApi.load(content, true);
    return;
  }

  // 1d. Loading Bitmaps
  if (type === "loaded-bitmap-success") {
    if (debug) console.log("🖼️ Bitmap loaded:", content);
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

  // Handle QR code fullscreen toggle from hitbox taps
  if (type === "qr:toggle-fullscreen") {
    toggleQRFullscreen();
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
      if (screen) screen.pixels = pixels;
    }

    // 🌟 Global Keyboard Shortcuts (these could also be seen via `act`)
    content.keyboard.forEach((data) => {
      if (currentText && currentText.indexOf("botce") > -1) return; // No global keys on `botce`. 23.11.12.23.38
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

        if ((data.key === "$" || data.key === "Home") && !getPackMode()) {
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
          !getPackMode() && // Disable navigation keys in OBJKT mode
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
          // Stop merry pipeline for both Escape and Backspace
          let merryOriginalCommand = null;
          if (data.key === "Escape" || data.key === "Backspace") {
            const stopMerry = $commonApi.system.stopMerryPipeline;
            if (typeof stopMerry === "function") {
              // Capture original command before stopping (for backspace editing)
              if (data.key === "Backspace" && $commonApi.system.merry?.originalCommand) {
                merryOriginalCommand = $commonApi.system.merry.originalCommand;
              }
              // Reset leaving flag to allow merry stop to jump
              leaving = false;
              stopMerry({
                reason: data.key === "Escape" ? "escape" : "backspace",
                jumpAfter: false,
                jumpTarget: null,
                cutTape: true,
              });
            } else if ($commonApi.system.merry) {
              $commonApi.system.merry.running = false;
              delete $commonApi.system.merry;
            }
          }

          $commonApi.sound.synth({
            tone: data.key === "Backspace" ? 800 : 1200,
            beats: 0.1,
            attack: 0.01,
            decay: 0.5,
            volume: 0.15,
          });

          send({ type: "keyboard:unlock" });
          if (!labelBack || data.key === "Backspace" || data.key === "Escape") {
            let promptSlug = "prompt";
            if (data.key === "Backspace") {
              // Use merry original command if available, otherwise use current content
              const content = merryOriginalCommand || currentHUDPlainTxt || currentHUDTxt || currentText;
              // Only encode kidlisp content with kidlisp encoder
              if (lisp.isKidlispSource(content)) {
                const encodedContent = lisp.encodeKidlispForUrl(content);
                promptSlug += "~" + encodedContent;
              } else {
                // For regular piece names, convert tildes to spaces for display
                const spaceContent = content.replace(/~/g, " ");
                promptSlug += "~" + spaceContent;
              }
            }
            $commonApi.jump(promptSlug);
            send({ type: "keyboard:open" });
          } else {
            if ($commonApi.history.length > 0) {
              send({ type: "back-to-piece" });
              // $commonApi.jump(
              //   $commonApi.history[$commonApi.history.length - 1],
              // );
            } else {
              $commonApi.jump(promptSlug);
              send({ type: "keyboard:open" });
            }
          }
        }

        // [Tab] Toggle HUD label and QR overlay visibility with smooth animation
        if (data.key === "Tab") {
          const currentTime = performance.now();
          const timeSinceLastTab = currentTime - (hudAnimationState.lastTabTime || 0);
          const isDoubleTap = timeSinceLastTab < 300; // 300ms double-tap window
          
          hudAnimationState.lastTabTime = currentTime;
          toggleHUDVisibility(isDoubleTap);
        }

        // [Shift] Toggle QR code fullscreen mode for KidLisp pieces
        // if (data.key === "Shift") {
        //   console.log("⌨️ [Shift Key] Received! getPackMode():", getPackMode());
        // }
        if (data.key === "Shift" && !getPackMode()) {
          toggleQRFullscreen();
        }

        // [Ctrl + X]
        // Enter and exit fullscreen mode.
        if (data.key === "x" && data.ctrl && currentText !== "notepat" && !getPackMode()) {
          send({ type: "fullscreen-enable" });
        }
      }
    });

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

      // Make all available dragBoxes into `Box` instances.
      pointersValues.forEach((p) => {
        if (p.dragBox) p.dragBox = geo.Box.from(p.dragBox);
      });

      const pens = pointersValues.reduce((arr, value) => {
        arr[value.pointerNumber] = value;
        return arr;
      }, []);

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
      get time() {
        const currentTime = content.audioTime;
        // Add comprehensive logging for audio timing (throttled to avoid spam)
        // if (debug && Math.floor(currentTime * 100) % 100 === 0) { // Log every 10ms when rounded
        //   console.log(`🎵 AUDIO_TIME: ${currentTime.toFixed(6)}s (getter called from ${(new Error().stack.split('\n')[2] || 'unknown').trim()})`);
        // }
        return currentTime;
      },
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

        const trimmed = String(input).trim();
        if (trimmed === "") return null;

        if (!isNaN(parseFloat(trimmed)) && isFinite(trimmed)) return Number(trimmed);

        let octave = 4;
        let note = trimmed.toLowerCase(); // Downcase everything.

        // Handle special tokens that shouldn't resolve to a frequency
        if (note === "rest" || note === "pause" || note === "_" || note === "speech") return null;

        const prefixMatch = note.match(/^(-?\d+)([a-z#/+\-]+)$/);
        const suffixMatch = note.match(/^([a-z#/+\-]+)(-?\d+)$/);

        if (prefixMatch) {
          octave = parseInt(prefixMatch[1], 10);
          note = prefixMatch[2];
        } else if (suffixMatch) {
          note = suffixMatch[1];
          octave = parseInt(suffixMatch[2], 10);
        }

        // Remove trailing + or - markers and other notation artifacts
        note = note.replace(/[+\-]+$/g, "");

        // Replace notepat "s"/"f" suffixes with standard accidentals when appropriate
        note = note.replace(/^([a-g])s$/, "$1#").replace(/^([a-g])f$/, "$1b");

        const frequency = noteFrequencies[note]; // Look up freq for the note.
        if (!frequency) throw new Error("Note not found in the list");

        // Calculate the frequency for the given octave
        const finalFreq = frequency * Math.pow(2, octave);
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
    $sound.gameboy = gameboy;
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
            sfxProgressReceivers[id] = (progressData) => {
              // Enhanced logging for audio progress tracking
              if (debug && progressData?.progress !== undefined) {
                const timeElapsed = soundTime - playingSound.startedAt;
                // console.log(`🎵 AUDIO_PROGRESS: id=${id.substring(id.lastIndexOf('_') + 1)}, progress=${progressData.progress.toFixed(6)}, elapsed=${timeElapsed.toFixed(3)}s, started=${playingSound.startedAt.toFixed(3)}s, current=${soundTime.toFixed(3)}s`);
                
                // Detect potential timing issues - compare in consistent units
                if (progressData.progress > 0 && timeElapsed > 0 && progressData.duration) {
                  // Convert audio progress to actual time for comparison
                  const actualAudioTime = progressData.progress * progressData.duration;
                  const timeDrift = Math.abs(actualAudioTime - timeElapsed);
                  const driftPercent = (timeDrift / progressData.duration) * 100;
                  
                  if (driftPercent > 5) { // 5% drift threshold
                    console.warn(`🎵 SYNC_DRIFT: actual=${actualAudioTime.toFixed(3)}s, expected=${timeElapsed.toFixed(3)}s, drift=${timeDrift.toFixed(3)}s (${driftPercent.toFixed(1)}%)`);
                  }
                }
              }
              resolve(progressData);
            };
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

    $sound.getDuration = async function getDuration(id) {
      const prom = new Promise((resolve, reject) => {
        sfxDurationReceivers[id] = resolve;
        return { resolve, reject };
      });
      send({ type: "sfx:get-duration", content: { id } });
      return prom;
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
    } = {}) {
      const id = soundId;
      if (volume === undefined) volume = 1;
      if (duration === "🔁") duration = Infinity; // First emoji in the API. 24.07.03.02.26
      if (beats === undefined && duration !== undefined)
        beats = (duration * sound.bpm) / 60;

      tone = $sound.freq(tone);
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
        enableSustain: function () {
          send({
            type: "bubble:update",
            content: { id, properties: { sustain: true } },
          });
        },
        disableSustain: function () {
          send({
            type: "bubble:update",
            content: { id, properties: { sustain: false } },
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
      const $api = {};
      keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      keys($updateApi).forEach((key) => ($api[key] = $updateApi[key]));
      keys(painting.api).forEach((key) => ($api[key] = painting.api[key]));
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      // Set the robo API context so it can send events
      $api.robo.setAPI($api);

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
      const startTime = performance.now();
      let penEventCount = 0;
      
      content.pen?.events.forEach((data) => {
        penEventCount++;
        Object.assign(data, {
          device: data.device,
          hudButtonActive: currentHUDButtonActive, // Add global HUD button flag
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
          if (!TV_MODE) {
            currentHUDButton?.act(e, {
              down: () => {
                originalColor = currentHUDTextColor;
                currentHUDScrub = 0;
                currentHUDTextColor = [0, 255, 0];
                send({ type: "keyboard:enabled" }); // Enable keyboard flag.
                send({ type: "keyboard:unlock" });
                $api.needsPaint();

                // Set global flag to block other button interactions
                currentHUDButtonActive = true;
                // Track that this was a direct touch on the HUD button
                currentHUDButtonDirectTouch = currentHUDButton.box.contains(e);

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
                const fallbackShareWidth = tf.blockWidth * "share ".length;
                const shareWidth = Math.max(currentHUDShareWidth || 0, fallbackShareWidth);
                
                // If scrubbed to reveal "share", jump to share piece
                if (currentHUDScrub >= shareWidth) {
                  // Clear global HUD button flags
                  currentHUDButtonActive = false;
                  currentHUDButtonDirectTouch = false;
                  
                  $api.sound.synth({
                    tone: 1200,
                    beats: 0.1,
                    attack: 0.01,
                    decay: 0.5,
                    volume: 0.15,
                  });
                  
                  // Build the share URL with current piece as parameter
                  const content = currentHUDPlainTxt || currentHUDTxt;
                  jump(`share~${content}`);
                  $api.needsPaint();
                  masked = true;
                  currentHUDScrub = 0;
                  return;
                }
                
                // If scrubbed but not to max, cancel the push
                if (currentHUDScrub > 0 && currentHUDScrub < shareWidth) {
                  btn.actions.cancel?.();
                  return;
                }

                // Clear global HUD button flags
                currentHUDButtonActive = false;
                currentHUDButtonDirectTouch = false;

                $api.sound.synth({
                  tone: 1200,
                  beats: 0.1,
                  attack: 0.01,
                  decay: 0.5,
                  volume: 0.15,
                });
                if (!labelBack) {
                  // Only clear prompt text when leaving NON-kidlisp pieces by tapping HUD
                  // For inline kidlisp prompts, preserve the content so user can continue editing
                  const content = currentHUDPlainTxt || currentHUDTxt;
                  let promptSlug = "prompt";
                  if (content && lisp.isKidlispSource(content)) {
                    // Preserve kidlisp content when tapping HUD to return to prompt for editing
                    const encodedContent = lisp.encodeKidlispForUrl(content);
                    promptSlug += "~" + encodedContent;
                  } else if (content) {
                    // For regular piece names, clear the prompt by not passing content as params
                    // This allows the prompt to clear properly when leaving non-kidlisp pieces
                    promptSlug = "prompt";
                  }
                  jump(promptSlug);
                } else {
                  labelBack = false; // Reset `labelBack` after jumping.
                  if ($commonApi.history.length > 0) {
                    send({ type: "back-to-piece" });
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

                // Only allow scrubbing if this was a direct touch on the HUD button
                if (!currentHUDButtonDirectTouch) return;

                if (btn.over || currentHUDScrub > 0) {
                  currentHUDScrub += e.delta.x;
                }

                const fallbackShareWidth = (currentHUDLabelBlockWidth || DEFAULT_TYPEFACE_BLOCK_WIDTH) * "share ".length;
                const shareWidth = Math.max(currentHUDShareWidth || 0, fallbackShareWidth);

                if (currentHUDScrub >= 0) {
                  const fallbackWidth = hudAnimationState.labelWidth
                    ? Math.max(hudAnimationState.labelWidth - currentHUDScrub, 0)
                    : currentHUDLabelBlockWidth * (currentHUDPlainTxt?.length || currentHUDTxt.length);
                  const baseWidth = currentHUDLabelMeasuredWidth ?? fallbackWidth;
                  btn.box.w = baseWidth + currentHUDScrub;
                  // console.log(btn.b);
                }

                if (currentHUDScrub < 0) currentHUDScrub = 0;

                if (currentHUDScrub >= shareWidth) {
                  currentHUDScrub = shareWidth;
                  currentHUDTextColor = [255, 255, 0];
                } else if (currentHUDScrub > 0) {
                  currentHUDTextColor = [255, 0, 0];
                } else if (currentHUDScrub === 0) {
                  if (btn.over) {
                    currentHUDTextColor = [0, 255, 0];
                  } else {
                    currentHUDTextColor = [255, 0, 0];
                  }
                }

                $api.needsPaint();
              },
              cancel: () => {
                currentHUDTextColor = originalColor;

                // Clear global HUD button flags
                currentHUDButtonActive = false;
                currentHUDButtonDirectTouch = false;

                const fallbackShareWidth = tf.blockWidth * "share ".length;
                const shareWidth = Math.max(currentHUDShareWidth || 0, fallbackShareWidth);
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
                  // If this was originally loaded from $code, use that format for sharing instead of full source
                  if (currentOriginalCodeId && currentOriginalCodeId.startsWith("$")) {
                    $api.jump("share~" + currentOriginalCodeId);
                  } else {
                    // Use plain text version (without color codes) for sharing
                    const textToShare = currentHUDPlainTxt || currentHUDTxt;
                    $api.jump("share~" + lisp.encodeKidlispForUrl(textToShare));
                  }
                  return;
                }

                currentHUDScrub = 0;
                // TODO: This might break on pieces where the keyboard is already
                //       open.
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
              rollout: () => {
                // console.log("rolled out...");
                currentHUDTextColor = [200, 80, 80];
                send({ type: "keyboard:lock" });
              },
            });
          }

          if (!masked && !TV_MODE) act($api); // Run the act function for all pen events (skip in TV mode).
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });

      // *** 3D Pen Events ***
      content.pen3d?.events?.forEach((data) => {
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
          if (!TV_MODE) act($api); // Skip 3D pen events in TV mode
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });
      
      // if (penEventCount > 0) {
        // const processingTime = performance.now() - startTime;
        //if (processingTime > 5) { // Only log if processing took more than 5ms
          //console.log("🖋️ Pen event processing:", { avgPerEvent: (processingTime / penEventCount).toFixed(2) + "ms"
          //});
        //}
      // }

      // Ingest all keyboard input events by running act for each event.
      content.keyboard?.forEach((data) => {
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
          if (!TV_MODE) act($api); // Execute piece shortcut (skip in TV mode).
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });

      // Ingest all gamepad input events by running act for each event.
      content.gamepad?.forEach((data) => {
        Object.assign(data, {
          device: "gamepad",
          is: (e) => data.name.indexOf(e) === 0,
        });
        $api.event = data;
        try {
          if (!TV_MODE) act($api); // Execute piece shortcut (skip in TV mode).
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });

      // *** UI Cancel Interactions *** (For edge detection)
      if (content.type === "ui:cancel-interactions" && content.content) {
        const cancelData = {
          name: "ui:cancel-interactions",
          x: content.content.x,
          y: content.content.y,
          pointer: content.content.pointer,
          reason: content.content.reason,
          is: (e) => e === "ui:cancel-interactions",
        };
        
        $api.event = cancelData;
        try {
          if (!TV_MODE) act($api); // Execute cancel for all buttons (skip in TV mode)
        } catch (e) {
          console.warn("️ ✒ Act failure (UI cancel)...", e);
        }
      }

      // *** Act Alerts *** (Custom events defined in here.)
      // These do not run in the initial loader / preview piece.
      actAlerts.forEach((action) => {
        // Check if `name`'s not a string, and if not, attach arbitrary data.
        let name,
          extra = {};
        if (typeof action !== "string") {
          // Make extra be an object with every key on action other than 'name'.
          ({ name, ...extra } = action);
        } else {
          name = action;
        }

        const data = {
          name,
          is: (e) => e === name,
          of: (e) => name.startsWith(e),
          ...extra,
        };

        // TODO: All all fields from 'extra' into 'data'.

        $api.event = data;
        try {
          // In TV mode, only allow certain system events, not user input events
          const allowInTvMode = name.startsWith('speech:') || 
                               name.startsWith('dark-mode') || 
                               name.startsWith('light-mode') ||
                               name.startsWith('microphone-') ||
                               name.startsWith('aesthetic-parent:');
          
          if (!TV_MODE || allowInTvMode) {
            act($api);
          }
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });
      //if (actAlerts.length > 0) console.log(actAlerts, booted);
      actAlerts.length = 0; // Clear act alerts.
    }

    // 🖼 Paint
    if (content.needsRender) {
      const $api = {};
      keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      keys(painting.api).forEach((key) => ($api[key] = painting.api[key]));
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      // Set the robo API context so it can send events
      $api.robo.setAPI($api);

      // Object.assign($api, $commonApi);
      // Object.assign($api, painting.api);

      // Use piece-level frame counter that only increments when piece actually paints
      // This ensures frame-based calculations (like in kidlisp) work correctly with FPS limiting
      $api.paintCount = pieceFrameCount;

      $api.inFocus = content.inFocus;

      // Make a screen buffer or resize it automatically if it doesn't exist.

      if (
        !screen ||
        screen.width !== content.width ||
        screen.height !== content.height
      ) {
        const hasScreen = screen !== undefined;

        screen = {
          pixels:
            pixels || new Uint8ClampedArray(content.width * content.height * 4),
          width: content.width,
          height: content.height,
          load: function load(name) {
            if (store[name]?.pixels) {
              this.pixels = new Uint8ClampedArray(store[name].pixels);
              this.width = store[name].width;
              this.height = store[name].height;
              $commonApi.resize(this.width, this.height);
              
              // 🎨 Broadcast screen resize from load operation
              if ($commonApi.broadcastPaintingUpdate) {
                $commonApi.broadcastPaintingUpdate("resized", {
                  width: this.width,
                  height: this.height,
                  source: "screen_load"
                });
              }
              
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

        screen[hasScreen ? "resized" : "created"] = true; // Screen change type.

        // 🎨 Broadcast screen resize to other tabs
        if (hasScreen && $commonApi.broadcastPaintingUpdate) {
          $commonApi.broadcastPaintingUpdate("resized", {
            width: screen.width,
            height: screen.height,
            source: "screen_resize"
          });
        }

        // TODO: Add the depth buffer back here.
        // Reset the depth buffer.
        // TODO: I feel like this is causing a memory leak...
        graph.depthBuffer.length = screen.width * screen.height;
        graph.depthBuffer.fill(Number.MAX_VALUE);

        graph.writeBuffer.length = 0; //screen.width * screen.height;
        // graph.writeBuffer.fill(0);
      }

      // TODO: Disable the depth buffer for now... it doesn't need to be
      //       regenerated on every frame.
      // TODO: This only needs to run if 'form' is running in a piece. 25.03.20.19.27
      graph.depthBuffer.fill(Number.MAX_VALUE); // Clear depthbuffer.
      graph.writeBuffer.length = 0; //fill(0); // Clear writebuffer.

      $api.screen = screen;
      $api.screen.center = { x: screen.width / 2, y: screen.height / 2 };

      $api.fps = function (newFps) {
        // Use piece-level timing instead of changing global render loop
        if (newFps === undefined || newFps === null) {
          if (pieceFPS !== null) {
            pieceFPS = null; // Remove FPS limit only if it was previously set
          }
        } else {
          // Only initialize timing if FPS wasn't set before, or if it's a different value
          if (pieceFPS === null) {
            // First time setting FPS - initialize timing
            pieceFPS = newFps;
            lastPaintTime = 0; // This will trigger "first frame" logic on next paint
          } else if (pieceFPS !== newFps) {
            // FPS changed to a different value - reset timing
            pieceFPS = newFps;
            lastPaintTime = 0; // This will trigger "first frame" logic on next paint
          }
          // If pieceFPS === newFps, do nothing (cache logic)
        }
        // Don't send fps-change to global render loop anymore
        // send({ type: "fps-change", content: newFps });
      };

      $api.cursor = (code) => (cursorCode = code);

      graph.setBuffer(screen);

      // API Stops being modified here...
      /*if (!$activePaintApi)*/ $activePaintApi = $api;
      
      // Add paintings cache to API for KidLisp ready? function
      $activePaintApi.paintings = paintings;
      
      // 🎯 Initialize global KidLisp instance with the main API
      initializeGlobalKidLisp($api);

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
          // Reset zebra cache at the beginning of boot to ensure consistent state
          $api.num.resetZebraCache();
          
          if (system === "nopaint") nopaint_boot({ ...$api, params: $api.params, colon: $api.colon });
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
        (noPaint === false || scream || fairies.length > 0 || (system === "nopaint" && nopaintPerf)) &&
        booted
      ) {
        let paintOut;
        
        // Restore kidlisp's accumulated pan state from previous frame
        $api.loadpan();

        try {
          // 📓 Bake any painting from the nopaint system before anything else.
          if (system === "nopaint" && $api.system?.nopaint) {
            const np = $api.system.nopaint;
            // No Paint: baking

            if (
              (brush || filter) &&
              $api.pen?.drawing /*&& currentHUDButton.down === false*/
            ) {
              const brushFilterApi = { ...$api };
              // Add top-level 'color' word that maps to system.nopaint.color
              brushFilterApi.color = np.color;
              if (currentHUDButton.down === false) {
                brushFilterApi.pen = np.brush;
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

            // 🎯 BAKING MOVED TO ACT PHASE: Removed duplicate baking logic
            // Baking now happens immediately in act() to fix race conditions
            if (np.is("painting") || np.needsPresent) {
              np.present($api); // No Paint: prepaint
            }
          } // All: Paint
          
          // 🎞️ Piece-level FPS timing control
          const now = performance.now();
          let shouldPaint = true;
          
          if (pieceFPS !== null && pieceFPS > 0) {
            const targetFrameTime = 1000 / pieceFPS; // milliseconds per frame
            
            // If this is the first paint (lastPaintTime === 0), always allow it
            if (lastPaintTime === 0) {
              lastPaintTime = now;
              shouldPaint = true;
            } else {
              const timeSinceLastPaint = now - lastPaintTime;
              
              if (timeSinceLastPaint < targetFrameTime) {
                shouldPaint = false; // Skip this frame
              } else {
                lastPaintTime = now; // Update last paint time
                shouldPaint = true;
              }
            }
          }
          
          // Only call paint() if timing allows or no FPS limit is set
          if (shouldPaint) {
            paintOut = paint($api); // Returns `undefined`, `false`, or `DirtyBox`.
            // Increment piece frame counter only when we actually paint
            pieceFrameCount++;
            
            // TODO: Remove old embedded layer rendering - using simplified approach now
            // globalThis.renderKidlispProgrammaticLayers();
          } else {
            // When skipping paint, use undefined to ensure continuous rendering
            // This maintains the render loop while skipping the actual paint call
            paintOut = undefined;
          }
          
          // Store paintOut for next frame (only when we actually painted)
          if (shouldPaint) {
            lastPaintOut = paintOut;
          }

          // Save kidlisp's accumulated pan state for next frame
          $api.savepan();
          // Reset pan for system UI rendering
          $api.unpan();
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
          $api.system?.nopaint?.recording &&
          !hideLabel &&
          pieceHistoryIndex > -1 &&
          !loading
        ) {
          // ink("red").box(screen.width - 3, 1, 2);
        }

        // Show a notice if necessary.
        if (notice) {
          ink(noticeColor[0])
            //.pan(help.choose(-1, 0, 1), help.choose(-1, 0, 1))
            .write(
              notice,
              { center: "xy", size: 2 },
              // { center: "x", y: 32, size: 2 },
              noticeColor[1],
              $api.screen.width - 8,
              noticeOpts?.wrap === "char" ? false : true,
            );
          //.unpan();
        }

        layer(0);

        painting.paint(true);
        painted = true;
        paintCount = paintCount + 1n;

        // Check for frame-based recording completion (runs on every painted frame)
        if ($api.rec.tapeFrameMode && $api.rec.tapeFrameTarget > 0) {
          const currentFrame = Number(paintCount);
          const framesPassed = currentFrame - $api.rec.tapeFrameStart;
          
          $api.rec.tapeProgress = framesPassed / $api.rec.tapeFrameTarget;
          
          if (framesPassed >= $api.rec.tapeFrameTarget) {
            console.log(`🎬 Frame-based recording complete: ${framesPassed}/${$api.rec.tapeFrameTarget} frames`);
            
            // Clear failsafe since normal timer completed
            if ($api.rec.failsafeTimeout) {
              clearTimeout($api.rec.failsafeTimeout);
              $api.rec.failsafeTimeout = null;
            }
            
            // Reset frame-based recording state
            $api.rec.tapeProgress = 0;
            $api.rec.tapeFrameMode = false;
            $api.rec.tapeFrameStart = 0;
            $api.rec.tapeFrameTarget = 0;
            
            // Trigger cut and jump to video
            if (typeof $api.rec.cut === 'function') {
              $api.rec.cut(() => {
                $commonApi.jump("video");
              });
            } else {
              console.warn(`🎬 ⚠️ Cut function not available, manual jump to video`);
              $commonApi.jump("video");
            }
          }
        }

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
      let label;
      const piece = currentHUDTxt?.split("~")[0];
      const defo = 6; // Default offset


      
      if (
        !previewMode &&
        !iconMode &&
        !hideLabel &&
        piece !== undefined &&
        piece.length > 0
      ) {

        // Use plain text for width calculation to avoid counting color codes
        const textForWidthCalculation = currentHUDPlainTxt || currentHUDTxt;
        
        // Double-check: strip color codes directly if they're still present
        // Handle multiple color code formats: \colorname\, \255,255,255\, etc.
        const colorCodeRegex = /\\[^\\]*\\/g; // Matches \anything\ format
        let cleanText = textForWidthCalculation.replace(colorCodeRegex, '');
        
        // Also try removing RGB color codes like \255,255,255\ if any remain
        cleanText = cleanText.replace(/\\\d+,\d+,\d+(,\d+)?\\/g, '');
        
        // Remove any remaining single backslashes that might be color code remnants
        cleanText = cleanText.replace(/\\/g, '');
        
        // Analyze visible lines for accurate sizing
        const rawLines = cleanText.split('\n');
        const visibleLines = rawLines.map((line) => line.replace(/\s+$/, ''));
        const visibleLineCount = visibleLines.length;

        const defaultTypeface = tf;
        const matrixTypeface = typefaceCache.get("MatrixChunky8");

        const isKidlispPiece = detectKidLispPiece({
          currentPath,
          currentHUDTxt,
          currentText,
          cleanText,
        });

        // 🔤 Font Preloading: Ensure MatrixChunky8 starts loading early if available
        if (matrixTypeface && !matrixTypeface.__loadPromise) {
          ensureTypefaceLoaded(matrixTypeface);
        }

        const measureLineWidth = (line, typeface) => {
          if (!line || !line.length || !typeface) return 0;
          const advances = typeface.data?.advances;
          const defaultAdvance = typeface.blockWidth || DEFAULT_TYPEFACE_BLOCK_WIDTH;
          let width = 0;
          for (let i = 0; i < line.length; i++) {
            const ch = line[i];
            width += advances && advances[ch] !== undefined ? advances[ch] : defaultAdvance;
          }
          return width;
        };

        const buildLayout = (typeface) => {
          if (!typeface) return null;

          const fontIdentifier = typeface === defaultTypeface ? undefined : typeface.name;
          const blockWidth = typeface.blockWidth || DEFAULT_TYPEFACE_BLOCK_WIDTH;
          const blockHeight = typeface.blockHeight || DEFAULT_TYPEFACE_BLOCK_HEIGHT;
          const lineWidths = visibleLines.map((line) => measureLineWidth(line, typeface));
          const longestLineWidth = lineWidths.reduce((max, width) => Math.max(max, width), 0);
          // 🎨 Use screen width minus padding for KidLisp wrapping
          // Account for left margin (HUD_LABEL_TEXT_MARGIN) in measurement bounds
          const bounds = isKidlispPiece ? ($api.screen.width - 6 - HUD_LABEL_TEXT_MARGIN) : Math.max($api.screen.width * 4, 1000);

          const naturalBounds = $api.text.box(
            cleanText,
            { x: 0, y: 0 },
            bounds,
            1,
            true, // Enable wrapping for proper measurement
            fontIdentifier,
          );

          if (fontIdentifier === "MatrixChunky8") {
            for (const line of visibleLines) {
              for (let i = 0; i < line.length; i++) {
                typeface.glyphs?.[line[i]];
              }
            }
          }

          return {
            typeface,
            fontIdentifier,
            blockWidth,
            blockHeight,
            lineWidths,
            longestLineWidth,
            textBoxWidth: naturalBounds?.box?.width ?? 0,
            textBoxHeight: naturalBounds?.box?.height ?? 0,
            naturalBounds,
          };
        };

        const defaultLayout = buildLayout(defaultTypeface);
        let selectedLayout = defaultLayout;

        // Disabled: MatrixChunky8 breakpoint removed per user request
        // if (
        //   isKidlispPiece &&
        //   matrixTypeface &&
        //   defaultLayout &&
        //   defaultLayout.longestLineWidth > $api.screen.width
        // ) {
        //   const matrixLayout = buildLayout(matrixTypeface);
        //   if (matrixLayout) {
        //     selectedLayout = matrixLayout;
        //   }
        // }

        if (!selectedLayout && matrixTypeface) {
          selectedLayout = buildLayout(matrixTypeface);
        }

        if (!selectedLayout) {
          selectedLayout = defaultLayout;
        }

        const selectedTypeface = selectedLayout?.typeface || defaultTypeface;
        const selectedHudFont = selectedLayout?.fontIdentifier;
        const hudBlockWidth = selectedLayout?.blockWidth ?? DEFAULT_TYPEFACE_BLOCK_WIDTH;
        const hudBlockHeight = selectedLayout?.blockHeight ?? DEFAULT_TYPEFACE_BLOCK_HEIGHT;
        const textBounds = selectedLayout?.naturalBounds;
        const textBoxWidth = selectedLayout?.textBoxWidth ?? 0;
        const textBoxHeight = selectedLayout?.textBoxHeight ?? 0;
        const longestVisibleLineWidth = selectedLayout?.longestLineWidth ?? 0;
  const wrappedLineCount = textBounds?.lines?.length ?? visibleLineCount;
  const measuredTextHeight = textBoxHeight || (wrappedLineCount * hudBlockHeight);

        // 🔤 Font Loading Check: If MatrixChunky8 was selected but not loaded, skip rendering HUD
        // This prevents pop-in where font_1 renders briefly before MatrixChunky8 glyphs load
        let skipHudRender = false;
        if (selectedHudFont === "MatrixChunky8" && matrixTypeface) {
          // Check if actual glyph data exists (glyphs are loaded on-demand via Proxy)
          // Test a common character like '$' to verify glyphs are accessible
          // A real glyph will have properties like dwidth, advance, or resolution
          // The Proxy returns null for missing glyphs (typeof null === 'object')
          const dollarGlyph = matrixTypeface.glyphs && matrixTypeface.glyphs['$'];
          const hasGlyphs = dollarGlyph && dollarGlyph !== null && 
            (dollarGlyph.dwidth || dollarGlyph.advance !== undefined || dollarGlyph.resolution);
          
          console.log('[HUD] MatrixChunky8 check:', {
            hasGlyphsObject: !!matrixTypeface.glyphs,
            dollarGlyph: dollarGlyph ? 'exists' : 'missing',
            dollarGlyphIsNull: dollarGlyph === null,
            dollarGlyphHasProps: !!(dollarGlyph && (dollarGlyph.dwidth || dollarGlyph.advance !== undefined)),
            hasGlyphs
          });
          
          if (!hasGlyphs) {
            console.log('[HUD] Skipping HUD render - glyphs not ready');
            // Trigger loading if not started
            if (!matrixTypeface.__loadPromise) {
              console.log('[HUD] Starting font load...');
              ensureTypefaceLoaded(matrixTypeface);
            }
            
            // Wait for font to load
            if (matrixTypeface.__loadPromise) {
              matrixTypeface.__loadPromise.then(() => {
                console.log('[HUD] Font load promise resolved');
                matrixTypeface.__loaded = true;
                // Trigger repaint once loaded
                if (typeof window !== "undefined" && window.$activePaintApi?.needsPaint) {
                  window.$activePaintApi.needsPaint();
                }
              }).catch((err) => {
                console.warn("Failed to load MatrixChunky8:", err);
                matrixTypeface.__loaded = true; // Mark as loaded to prevent infinite waiting
              });
            }
            // Skip rendering this frame - wait for font to load
            skipHudRender = true;
          } else {
            console.log('[HUD] Rendering with MatrixChunky8 - glyphs ready');
          }
        }

        // Only proceed with HUD rendering if font is ready
        if (!skipHudRender) {

        const measuredShareWidth = measureLineWidth("share ", selectedTypeface);
        const fallbackShareWidth = hudBlockWidth * "share ".length;
        const effectiveShareWidth = measuredShareWidth > 0
          ? Math.max(measuredShareWidth, fallbackShareWidth)
          : fallbackShareWidth;
        currentHUDShareWidth = effectiveShareWidth;
        const shareWidth = effectiveShareWidth;
        currentHUDLeftPad = shareWidth;
        const scrubExtension = Math.max(0, currentHUDScrub);

  currentHUDLabelFontName = selectedHudFont;
  currentHUDLabelBlockWidth = hudBlockWidth;
  currentHUDLabelBlockHeight = hudBlockHeight;

        // Prefer visible line width for KidLisp pieces to avoid oversized buffers
        // When wrapping is enabled, use textBoxWidth (wrapped width) instead
        let w = isKidlispPiece ? textBoxWidth : textBoxWidth;
        if (!w || w <= 0) {
          w = textBoxWidth;
        }
        
        // Add extra padding to buffer width to prevent font_1 character cutoff
        const bufferWidthPadding = 6; // Extra pixels for wider glyphs
        w += bufferWidthPadding;
        
        // Final text dimensions: KidLisp width uses visible line metrics, height from text box
  let h = measuredTextHeight;
        
        if (piece === "video") w = screen.width;
        
        const baseLabelWidth = w + shareWidth;
        
        // Use natural text dimensions for buffer - preserve original layout
        let bufferW = baseLabelWidth + scrubExtension;
        let bufferH = h;
        
        // Ensure minimum buffer size for readability
        const minTextWidth = Math.max(isKidlispPiece ? textBoxWidth : textBoxWidth, 50);
        const minimumAllowedWidth = shareWidth + Math.max(minTextWidth, 0);
        const minBufferH = Math.max(hudBlockHeight, 20); // At least one line height
        
        if (bufferW < minimumAllowedWidth + scrubExtension) {
          bufferW = minimumAllowedWidth + scrubExtension;
        }
        if (bufferH < minBufferH) {
          bufferH = minBufferH;
        }
        
        // Fix height calculation for KidLisp pieces when wordWrap=false
        if (isKidlispPiece) {
          // When wordWrap=false, text.box doesn't calculate height correctly for multi-line text
          // Calculate proper height based on actual line count
          const properHeight = measuredTextHeight;
          if (properHeight > bufferH) {
            bufferH = properHeight;
          }
        }
        
        const hudDescenderPadding = Math.max(2, Math.round(hudBlockHeight * 0.2));
        bufferH += hudDescenderPadding;

        // DEBUG: Log buffer dimensions in copy-pasteable format (reduced frequency)
        if (false && isKidlispPiece && currentHUDTxt && pieceFrameCount % 120 === 0) {
          console.log(`HUD_BUFFER: bufferW=${bufferW} bufferH=${bufferH} textBoxW=${textBounds.box.width} textBoxH=${textBounds.box.height} longestLinePx=${longestVisibleLineWidth} lines=${visibleLineCount} fontHeight=${hudBlockHeight} expectedH=${visibleLineCount * hudBlockHeight}`);
          console.log(`HUD_TEXT: "${cleanText.replace(/\n/g, '\\n').substring(0, 150)}"`);
        }
        

        
        // Store actual dimensions for animation calculations
  currentHUDLabelMeasuredWidth = baseLabelWidth;
  hudAnimationState.labelWidth = bufferW;
        hudAnimationState.labelHeight = bufferH;
        h = bufferH;
        
        label = $api.painting(bufferW, bufferH, ($) => {
          // Ensure label renders with clean pan state
          $.unpan();
          // Ensure label renders without any active mask from the piece
          $.unmask();
          
          // Clean rendering - no debug elements
          


          let c;
          if (currentHUDTextColor) {
            c = num.shiftRGB(currentHUDTextColor, [255, 255, 255], 0.75);
          } else if (currentHUDStatusColor) {
            c = currentHUDStatusColor;
          } else {
            c = [255, 200, 240];
          }
          if (piece !== "video") {
            let text = currentHUDTxt;
            if (currentHUDTxt.split(" ")[1]?.indexOf("http") !== 0) {
              text = currentHUDTxt?.replaceAll("~", " ");
              // Handle command separators: replace § with spaces for display
              // but keep actual newlines as newlines
              text = text?.replaceAll("§", " ");
            }
            
            const baseX = currentHUDLeftPad;
            const hudTextX = baseX + HUD_LABEL_TEXT_MARGIN + currentHUDScrub;
            const hudTextY = 0;
            const typefaceNameForWrite = selectedHudFont;
            const hasColorCodes = textContainsColorCodes(text);
            const baseTextColor = currentHUDTextColor || "white";

            // 🎨 Character wrapping for KidLisp HUD prompts
            // Wrap based on screen width minus padding (6px total: 2px margin + 4px padding)
            // Subtract the left margin since text starts at x=HUD_LABEL_TEXT_MARGIN
            const wrapBounds = isKidlispPiece ? ($api.screen.width - 6 - HUD_LABEL_TEXT_MARGIN) : undefined;

            drawHudLabelText($, text, {
              x: hudTextX,
              y: hudTextY,
              typefaceName: typefaceNameForWrite,
              textColor: baseTextColor,
              preserveColors: hasColorCodes,
              bounds: wrapBounds,
              // wordWrap defaults to true when bounds is set, enabling character wrapping
            });

            if (currentHUDScrub > 0) {
              const shareWidth = currentHUDShareWidth || (currentHUDLabelBlockWidth * "share ".length);
              const shareTextX = HUD_LABEL_TEXT_MARGIN + currentHUDScrub;
              const shareTextY = 0;

              drawHudLabelText($, "share", {
                x: shareTextX,
                y: shareTextY,
                typefaceName: typefaceNameForWrite,
                textColor: c,
                preserveColors: false,
              });
            }
          } else {
            $.ink(0).line(1, 1, 1, h - 1);
            $.ink(c).line(0, 0, 0, h - 2);
          }
        });

        if (piece === "video") currentHUDOffset = { x: 0, y: 6 };
        if (!currentHUDOffset) currentHUDOffset = { x: defo, y: defo };

        // Apply animation effects to HUD label position
        if (hudAnimationState.animating) {
          const currentTime = performance.now();
          const elapsed = currentTime - hudAnimationState.startTime;
          const progress = Math.min(elapsed / hudAnimationState.duration, 1.0);
          
          // Easing function for smooth macOS-style animation (ease-out)
          const easeOut = 1 - Math.pow(1 - progress, 3);
          
          // Use actual calculated dimensions from when HUD was last drawn
          const hudWidth = hudAnimationState.labelWidth || 120;  // fallback to default
          const hudHeight = hudAnimationState.labelHeight || 40; // fallback to default
          const qrSize = hudAnimationState.qrSize || 80;
          
          // Calculate slide distances: ensure full content slides off-screen
          // Add extra padding (20px) to guarantee complete disappearance
          // For very large labels, cap the slide distance to keep animation smooth
          const maxSlideDistance = 400; // Maximum distance to slide for smooth animation
          const hudSlideX = Math.max(-(hudWidth + 20), -maxSlideDistance);  // Cap slide distance
          const hudSlideY = Math.max(-(hudHeight + 20), -maxSlideDistance); // Cap slide distance
          const qrSlideX = qrSize + 20;         // Slide right by full size + padding
          const qrSlideY = qrSize + 20;         // Slide down by full size + padding
          
          if (hudAnimationState.visible) {
            // Animating IN: fade from 0 to 1, slide from respective corners to position
            hudAnimationState.opacity = easeOut;
            // HUD slides in from top-left corner
            hudAnimationState.slideOffset = {
              x: (1 - easeOut) * hudSlideX, // Slide in from left using actual width
              y: (1 - easeOut) * hudSlideY  // Slide in from top using actual height
            };
            // QR slides in from bottom-right corner
            hudAnimationState.qrSlideOffset = {
              x: (1 - easeOut) * qrSlideX,  // Slide in from right using actual size
              y: (1 - easeOut) * qrSlideY   // Slide in from bottom using actual size
            };
          } else {
            // Animating OUT: fade from 1 to 0, slide to respective corners
            hudAnimationState.opacity = 1 - easeOut;
            // HUD slides out to top-left corner
            hudAnimationState.slideOffset = {
              x: easeOut * hudSlideX, // Slide out to left using actual width
              y: easeOut * hudSlideY  // Slide out to top using actual height
            };
            // QR slides out to bottom-right corner
            hudAnimationState.qrSlideOffset = {
              x: easeOut * qrSlideX,  // Slide out to right using actual size
              y: easeOut * qrSlideY   // Slide out to bottom using actual size
            };
          }
          
          // End animation when progress reaches 1.0
          if (progress >= 1.0) {
            hudAnimationState.animating = false;
            
            // Ensure final values are exact
            if (hudAnimationState.visible) {
              hudAnimationState.opacity = 1.0;
              hudAnimationState.slideOffset = { x: 0, y: 0 };
              hudAnimationState.qrSlideOffset = { x: 0, y: 0 };
            } else {
              hudAnimationState.opacity = 0.0;
              hudAnimationState.slideOffset = { x: hudSlideX, y: hudSlideY };
              hudAnimationState.qrSlideOffset = { x: qrSlideX, y: qrSlideY };
            }
          }
        }

        currentHUDButton =
          currentHUDButton ||
          new $api.ui.Button({
            x: 0,
            y: 0,
            w: currentHUDLabelMeasuredWidth, // FIXED: Don't add currentHUDOffset.x to button width
            h: h, // Use just the calculated height without extra y-offset
          });

        // Mark HUD button to bypass the global HUD active check (it checks itself)
        currentHUDButton.noEdgeDetection = true;
        // Prevent HUD button from being activated by dragging from other buttons
        currentHUDButton.noRolloverActivation = true;

        // $commonApi.hud.currentLabel = {
        //   text: currentHUDTxt,
        //   btn: currentHUDButton,
        // };
      }
        } // End of skipHudRender check - close the if (!skipHudRender) block

      // Return frame data back to the main thread.
      let sendData = { width: screen.width, height: screen.height };

      // Tack on the merry progress bar at the TOP if a pipeline is running
      if ($api.system.merry && $api.system.merry.running) {
        const merry = $api.system.merry;
        
        // Calculate progress based on elapsed time
        const now = Date.now();
        if (merry.startTime && merry.currentPieceStart) {
          const totalElapsed = (now - merry.startTime) / 1000; // in seconds
          const pieceElapsed = (now - merry.currentPieceStart) / 1000;
          merry.progress = Math.min(1, totalElapsed / merry.totalDuration);
          merry.pieceProgress = Math.min(1, pieceElapsed / merry.pipeline[merry.currentIndex].duration);
        }
        
        const mainScreenWidth = screen.width;
        
        // Create merry progress bar painting with SEGMENTED colored blocks per piece
        const merryProgressBarPainting = $api.painting(mainScreenWidth, 1, ($) => {
          $.unmask(); // Ensure progress bar renders without piece mask
          const animFrame = Number($api.paintCount || 0n);
          
          // Check for transition flash
          let flashIntensity = 0;
          if (merry.transitionFlash && merry.transitionFlash.active) {
            const elapsed = Date.now() - merry.transitionFlash.startTime;
            if (elapsed < merry.transitionFlash.duration) {
              // Ease out the flash (starts bright, fades quickly)
              const progress = elapsed / merry.transitionFlash.duration;
              flashIntensity = 1 - Math.pow(progress, 2); // Quadratic ease out
            } else {
              // Flash complete
              merry.transitionFlash.active = false;
              flashIntensity = 0;
            }
          }
          
          // Fill with black backdrop (or white during flash)
          if (flashIntensity > 0) {
            const bgBrightness = Math.floor(255 * flashIntensity);
            $.ink(bgBrightness, bgBrightness, bgBrightness, 255).box(0, 0, mainScreenWidth, 1);
          } else {
            $.ink(0, 0, 0, 255).box(0, 0, mainScreenWidth, 1);
          }
          
          // Define colors for each piece (cycle through palette)
          const pieceColors = [
            { r: 100, g: 255, b: 100 },  // Bright green
            { r: 100, g: 200, b: 255 },  // Cyan
            { r: 255, g: 200, b: 100 },  // Orange
            { r: 255, g: 100, b: 200 },  // Pink
            { r: 200, g: 100, b: 255 },  // Purple
            { r: 255, g: 255, b: 100 },  // Yellow
          ];
          
          // Calculate pixel width for each piece based on duration
          let xOffset = 0;
          merry.pipeline.forEach((piece, index) => {
            const pieceWidthRatio = piece.duration / merry.totalDuration;
            const pieceWidth = Math.floor(mainScreenWidth * pieceWidthRatio);
            
            // Determine if this piece is completed, current, or upcoming
            const isCompleted = index < merry.currentIndex;
            const isCurrent = index === merry.currentIndex;
            const isUpcoming = index > merry.currentIndex;
            
            // Get color for this piece
            const color = pieceColors[index % pieceColors.length];
            
            // Draw this piece's segment
            for (let x = xOffset; x < xOffset + pieceWidth && x < mainScreenWidth; x++) {
              let baseR, baseG, baseB, alpha;
              
              if (isCompleted) {
                // Completed pieces: Fade out progressively (darker as we move away from them)
                const completedAge = merry.currentIndex - index; // How many pieces ago
                const fadeFactor = Math.max(0.1, 1 - (completedAge * 0.2)); // Fade by 20% per step
                baseR = Math.floor(color.r * fadeFactor * 0.25); // Start at 25% brightness
                baseG = Math.floor(color.g * fadeFactor * 0.25);
                baseB = Math.floor(color.b * fadeFactor * 0.25);
                alpha = 255;
              } else if (isCurrent) {
                // Current piece: Show progress within this segment
                const pieceProgress = merry.pieceProgress || 0;
                const localX = x - xOffset;
                const progressPoint = Math.floor(pieceWidth * pieceProgress);
                
                if (localX <= progressPoint) {
                  // Filled part of current piece - bright with pulsing
                  const pulse = Math.sin(animFrame * 0.3) * 0.2 + 0.8;
                  baseR = Math.floor(color.r * pulse);
                  baseG = Math.floor(color.g * pulse);
                  baseB = Math.floor(color.b * pulse);
                  alpha = 255;
                  
                  // Leader pixel - extra bright white
                  if (localX === progressPoint) {
                    const leaderPulse = Math.sin(animFrame * 0.6) * 0.3 + 0.7;
                    baseR = Math.floor(255 * leaderPulse);
                    baseG = Math.floor(255 * leaderPulse);
                    baseB = Math.floor(255 * leaderPulse);
                  }
                } else {
                  // Unfilled part of current piece - dim preview
                  baseR = Math.floor(color.r * 0.3);
                  baseG = Math.floor(color.g * 0.3);
                  baseB = Math.floor(color.b * 0.3);
                  alpha = 255;
                }
              } else if (isUpcoming) {
                // Upcoming pieces: Warm up as turn approaches
                const stepsUntil = index - merry.currentIndex; // How many pieces away
                const nextIsThis = stepsUntil === 1;
                
                if (nextIsThis) {
                  // Next piece warms up based on current piece progress
                  const warmup = merry.pieceProgress || 0; // 0 to 1
                  const baseBrightness = 0.15; // Starting dim
                  const targetBrightness = 0.5; // Warm up to this
                  const brightness = baseBrightness + (targetBrightness - baseBrightness) * warmup;
                  
                  // Add warm orange/yellow glow as it heats up
                  const warmGlow = warmup * 0.3;
                  baseR = Math.floor(color.r * brightness + 255 * warmGlow);
                  baseG = Math.floor(color.g * brightness + 200 * warmGlow);
                  baseB = Math.floor(color.b * brightness + 100 * warmGlow * 0.5);
                  baseR = Math.min(255, baseR);
                  baseG = Math.min(255, baseG);
                  baseB = Math.min(255, baseB);
                  alpha = 255;
                } else {
                  // Further away pieces: Very dim, getting dimmer with distance
                  const distanceFade = Math.max(0.05, 1 - (stepsUntil - 1) * 0.15);
                  baseR = Math.floor(color.r * 0.15 * distanceFade);
                  baseG = Math.floor(color.g * 0.15 * distanceFade);
                  baseB = Math.floor(color.b * 0.15 * distanceFade);
                  alpha = 255;
                }
              }
              
              // Apply transition flash boost to current piece
              if (isCurrent && flashIntensity > 0) {
                const boost = 1 + flashIntensity * 0.8; // Up to 80% brighter
                baseR = Math.min(255, Math.floor(baseR * boost));
                baseG = Math.min(255, Math.floor(baseG * boost));
                baseB = Math.min(255, Math.floor(baseB * boost));
              }
              
              $.ink(baseR, baseG, baseB, alpha).box(x, 0, 1, 1);
            }
            
            xOffset += pieceWidth;
          });
        });
        
        if (merryProgressBarPainting && merryProgressBarPainting.pixels && merryProgressBarPainting.pixels.length > 0) {
          sendData.merryProgressBar = {
            x: 0,
            y: 0, // Position at TOP of screen (1px tall)
            img: {
              width: merryProgressBarPainting.width,
              height: merryProgressBarPainting.height,
              pixels: merryProgressBarPainting.pixels
            }
          };
        }
      }

      // Tack on the tape progress bar pixel buffer if necessary.
      if ($api.rec.tapeProgress || ($api.rec.recording && $api.rec.tapeTimerDuration)) {
        const progress = $api.rec.tapeProgress || 0;
        
        // Determine if we should show progress bar at all
        const isFrameBased = $api.rec.tapeFrameMode && $api.rec.tapeFrameTarget > 0;
        const frameCount = $api.rec.tapeFrameTarget || 1;
        
        // Hide progress bar for ALL frame-based recordings
        if (isFrameBased) {
          // Skip creating progress bar entirely for frame recordings
        } else {
          // IMPORTANT: Access the main screen buffer OUTSIDE the painting context
          // because inside painting(), $api.screen refers to the painting's own buffer, not the main screen
          const mainScreenPixels = screen.pixels; // This is the actual frame content
          const mainScreenWidth = screen.width;
          const mainScreenHeight = screen.height;
          
          const isShortRecording = isFrameBased ? frameCount <= 120 : ($api.rec.tapeTimerDuration || 0) <= 1;
          
          let currentProgressWidth;
          if (isFrameBased) {
            // For frame-based recording, create discrete segments
            const framesPassed = Math.floor(progress * frameCount);
            const segmentWidth = Math.max(1, Math.floor(mainScreenWidth / frameCount));
            currentProgressWidth = framesPassed * segmentWidth;
          } else {
            // For time-based recording, use smooth progress
            currentProgressWidth = Math.floor(mainScreenWidth * progress);
          }
          
          // Create tape progress bar painting with VHS-style red glow
          const tapeProgressBarPainting = $api.painting(mainScreenWidth, 2, ($) => {
            $.unmask(); // Ensure tape progress bar renders without piece mask
            // Animation frame for VHS effects - increased speed for more vibes
            const animFrame = Number($api.paintCount || 0n);
            
            // Helper function to sample pixel color from above the progress bar
            const sampleColorFromAbove = (x) => {
              // Skip sampling during early frames for performance (first ~5 frames)
              const frameCount = Number($api.paintCount || 0n);
              if (frameCount < 5 || !mainScreenPixels || mainScreenPixels.length === 0) {
                return { r: 0, g: 0, b: 0 }; // Return black during startup
              }
              
              // Sample from a few pixels above the progress bar position
              const sampleY = Math.max(0, screen.height - 10); // Sample 10px above progress bar
              const pixelIndex = (sampleY * screen.width + x) * 4;
              
              if (pixelIndex >= 0 && pixelIndex < mainScreenPixels.length - 3) {
                return {
                  r: mainScreenPixels[pixelIndex],
                  g: mainScreenPixels[pixelIndex + 1],
                  b: mainScreenPixels[pixelIndex + 2]
                };
              }
              // Fallback to black if can't sample
              return { r: 0, g: 0, b: 0 };
            };
            
            // Helper function to blend sampled color with VHS red
            const blendWithVHS = (sampledColor, vhsR, vhsG, vhsB, blendFactor = 0.3) => {
              return {
                r: Math.floor(sampledColor.r * blendFactor + vhsR * (1 - blendFactor)),
                g: Math.floor(sampledColor.g * blendFactor + vhsG * (1 - blendFactor)),
                b: Math.floor(sampledColor.b * blendFactor + vhsB * (1 - blendFactor))
              };
            };
          
          // Special color override for first and last frames
          const isFirstFrame = progress <= 0.01; // First 1% of progress
          const isLastFrame = progress >= 0.99;  // Last 1% of progress
          
          // Calculate smooth alpha fade - progress bar fades from 25% to 75% for longer clean content viewing
          // Skip fade animation for short recordings
          let progressBarAlpha = 1.0; // Default to fully visible
          
          if (!isShortRecording) {
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
          }
          
          // Fill entire bar with black backdrop using fade alpha
          $.ink(0, 0, 0, Math.floor(progressBarAlpha * 255)).box(0, 0, mainScreenWidth, 2);
          
          if (isFrameBased) {
            // Draw segmented progress for frame-based recording
            const segmentWidth = Math.max(1, Math.floor(mainScreenWidth / frameCount));
            const framesPassed = Math.floor(progress * frameCount);
            const currentProgressWidth = framesPassed * segmentWidth;
            
            // Draw VHS-style progress bar pixel by pixel (same as time-based but with discrete frame spacing and no leader pixel)
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
                  // NORMAL FRAMES - Enhanced color sampling from buffer above
                  const sampledColor = sampleColorFromAbove(x);
                  
                  // Sample additional colors for more variety
                  const sampledColor2 = sampleColorFromAbove(Math.max(0, x - 3));
                  const sampledColor3 = sampleColorFromAbove(Math.min(mainScreenWidth - 1, x + 3));
                  
                  // Mix multiple sampled colors for richer palette
                  const mixedR = Math.floor((sampledColor.r + sampledColor2.r + sampledColor3.r) / 3);
                  const mixedG = Math.floor((sampledColor.g + sampledColor2.g + sampledColor3.g) / 3);
                  const mixedB = Math.floor((sampledColor.b + sampledColor2.b + sampledColor3.b) / 3);
                  const mixedColor = { r: mixedR, g: mixedG, b: mixedB };
                  
                  // Base VHS red intensity - reduced to let more color through
                  let redIntensity = 200; // Reduced from 255 for more color mixing
                  
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
                  
                  // Create VHS red color
                  const vhsR = Math.max(180, Math.min(255, redIntensity)); // Reduced min from 200
                  const vhsG = Math.floor(vhsR * 0.05); // Very minimal green
                  const vhsB = Math.floor(vhsR * 0.02); // Very minimal blue
                  
                  // Blend mixed sampled color with VHS red (55% sampled, 45% VHS red for more color influence)
                  const blended = blendWithVHS(mixedColor, vhsR, vhsG, vhsB, 0.55);
                  baseR = blended.r;
                  baseG = blended.g;
                  baseB = blended.b;
                }
                
                // Use proper alpha blending for filled area (no special leader pixel treatment for frame-based)
                $.ink(baseR, baseG, baseB, Math.floor(progressBarAlpha * 255)).box(x, 0, 1, 2);
              } else {
                // UNFILLED AREA - Keep black background (already filled above)
                // No need to redraw black pixels, they're already set
              }
            }
          } else {
            // Draw VHS-style progress bar pixel by pixel (original time-based logic)
            for (let x = 0; x < mainScreenWidth; x++) {
              let baseR, baseG, baseB;
              
              // Leading edge pixel - bright glowing leader
              const isLeaderPixel = x === currentProgressWidth - 1 && currentProgressWidth > 0;
              
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
                  // NORMAL FRAMES - Enhanced color sampling from buffer above
                  const sampledColor = sampleColorFromAbove(x);
                  
                  // Sample additional colors for more variety
                  const sampledColor2 = sampleColorFromAbove(Math.max(0, x - 3));
                  const sampledColor3 = sampleColorFromAbove(Math.min(mainScreenWidth - 1, x + 3));
                  
                  // Mix multiple sampled colors for richer palette
                  const mixedR = Math.floor((sampledColor.r + sampledColor2.r + sampledColor3.r) / 3);
                  const mixedG = Math.floor((sampledColor.g + sampledColor2.g + sampledColor3.g) / 3);
                  const mixedB = Math.floor((sampledColor.b + sampledColor2.b + sampledColor3.b) / 3);
                  const mixedColor = { r: mixedR, g: mixedG, b: mixedB };
                  
                  // Base VHS red intensity - reduced to let more color through
                  let redIntensity = 200; // Reduced from 255 for more color mixing
                  
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
                  
                  // Create VHS red color
                  const vhsR = Math.max(180, Math.min(255, redIntensity)); // Reduced min from 200
                  const vhsG = Math.floor(vhsR * 0.05); // Very minimal green
                  const vhsB = Math.floor(vhsR * 0.02); // Very minimal blue
                  
                  // Blend mixed sampled color with VHS red (55% sampled, 45% VHS red for more color influence)
                  const blended = blendWithVHS(mixedColor, vhsR, vhsG, vhsB, 0.55);
                  baseR = blended.r;
                  baseG = blended.g;
                  baseB = blended.b;
                }
                
                // Special leader pixel treatment
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
                    
                    if (isInFadePeriod && !isShortRecording) {
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
                  if (progress >= 0.20 && progress <= 0.80 && !isShortRecording) {
                    // During fade period - beacon-like signal marker (strong but not full opacity)
                    finalAlpha = 0.8; // Bright beacon for progress indication
                  } else {
                    // Outside fade period - still visible beacon
                    finalAlpha = Math.min(progressBarAlpha, 0.9); // Strong beacon at 90% opacity
                  }
                }
                
                // Use proper alpha blending for filled area
                $.ink(baseR, baseG, baseB, Math.floor(finalAlpha * 255)).box(x, 0, 1, 2);
              } else {
                // UNFILLED AREA - Keep black background (already filled above)
                // No need to redraw black pixels, they're already set
              }
            }
          }
        });
        
        // Ensure the painting was created successfully before adding to sendData
        if (tapeProgressBarPainting && tapeProgressBarPainting.pixels && tapeProgressBarPainting.pixels.length > 0) {
          // Structure the data to match what bios.mjs expects (same as label format)
          sendData.tapeProgressBar = {
            x: 0,
            y: screen.height - 2, // Position at bottom of screen (2px tall)
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
        } // Close the else block for frame count >= 60
      }

      maybeLeave();

      // TODO: Write this up to the data in `painting`.
      sendData.TwoD = { code: twoDCommands };

      // Attach a label buffer if necessary.
      // Hide label when QR is in fullscreen mode
      if (label && !hudAnimationState.qrFullscreen) {
  const finalX = currentHUDOffset.x + hudAnimationState.slideOffset.x - currentHUDLeftPad;
        const finalY = currentHUDOffset.y + hudAnimationState.slideOffset.y;
        
        sendData.label = {
          x: finalX,
          y: finalY,
          opacity: hudAnimationState.opacity,
          img: (({ width, height, pixels }) => ({ width, height, pixels }))(
            label,
          ),
        };
        
        // DEBUG: Add hitbox visualization overlay
        if (globalThis.debugHudHitbox && currentHUDButton) {
          const hitboxWidth = currentHUDButton.box.w;
          const hitboxHeight = currentHUDButton.box.h;
          
          const hitboxOverlay = $api.painting(hitboxWidth, hitboxHeight, ($) => {
            $.unpan();
            $.unmask(); // Ensure hitbox overlay renders without piece mask
            // Draw a semi-transparent green border to show the hitbox
            $.ink(0, 255, 0, 128).box(0, 0, hitboxWidth, hitboxHeight, "outline");
            $.ink(0, 255, 0, 64).box(1, 1, hitboxWidth - 2, hitboxHeight - 2, "outline");
          });
          
          sendData.hitboxDebug = {
            x: currentHUDOffset.x + hudAnimationState.slideOffset.x - currentHUDLeftPad,
            y: currentHUDOffset.y + hudAnimationState.slideOffset.y,
            opacity: hudAnimationState.opacity,
            img: (({ width, height, pixels }) => ({ width, height, pixels }))(
              hitboxOverlay,
            ),
          };
        }
      }

      // 🔲 Generate QR code overlay for KidLisp pieces
      let qrOverlay;
      
      // Clear QR cache if caching is disabled to prevent memory buildup
      if (isQROverlayCacheDisabled() && qrOverlayCache.size > 0) {
        qrOverlayCache.clear();
      }
      
      // Detect if this is a KidLisp piece
      const sourceCode = currentText || currentHUDTxt; // Use plain currentText first, then fall back to HUD text
      
      // Use centralized KidLisp detection for QR code generation
      // But exclude proper .lisp files which should not show source HUD
      const isInlineKidlispPiece = (currentPath && lisp.isKidlispSource(currentPath) && !currentPath.endsWith('.lisp')) ||
                             currentPath === "(...)" ||
                             // Detect cached KidLisp pieces by $code format
                             (sourceCode && sourceCode.startsWith("$")) ||
                             (currentPath && currentPath.includes("/disks/$")) ||
                             // Use the centralized KidLisp detection that includes comma syntax
                             (sourceCode && lisp.isKidlispSource(sourceCode));
      

      if (isInlineKidlispPiece && sourceCode && !hideLabel && (hudAnimationState.visible || hudAnimationState.animating || hudAnimationState.qrFullscreen)) {
        // console.log("🎨 [QR Debug] Entering QR generation. qrFullscreen:", hudAnimationState.qrFullscreen, "sourceCode:", sourceCode?.substring(0, 50));
        try {
          // For $code pieces, the sourceCode is the code itself, so use it directly
          let cachedCode;
          if (sourceCode.startsWith("$")) {
            // For $code format, the sourceCode IS the cached code (without $)
            cachedCode = sourceCode.substring(1); // Remove the $ prefix
            // console.log("🔑 [QR Debug] Using $code format, cachedCode:", cachedCode);
          } else {
            // For regular KidLisp source, check if it has been cached
            cachedCode = getCachedCode(sourceCode);
            // console.log("🔍 [QR Debug] Checked cache, cachedCode:", cachedCode);
            
            // If not cached yet but QR fullscreen is active (shift was pressed),
            // trigger caching and request a repaint when complete
            if (!cachedCode && hudAnimationState.qrFullscreen) {
              // console.log("⏳ [QR Debug] Code not cached, triggering caching...");
              const kidlispInstance = getGlobalKidLisp();
              if (kidlispInstance) {
                // console.log("✅ [QR Debug] Got KidLisp instance, calling cacheKidlispSource");
                // Trigger caching asynchronously
                kidlispInstance.cacheKidlispSource(sourceCode, $commonApi).then(() => {
                  // Check if code was successfully cached
                  const newCachedCode = getCachedCode(sourceCode);
                  if (newCachedCode) {
                    //console.log(`🔄 Code cached after shift press: $${newCachedCode}`);
                    // Trigger repaint to show the QR code
                    if (typeof window !== "undefined" && window.$activePaintApi?.needsPaint) {
                      window.$activePaintApi.needsPaint();
                    }
                  }
                }).catch(err => {
                  console.warn("Failed to cache KidLisp code:", err);
                });
              }
            }
          }
          
          if (cachedCode) {
            // Send the cached code to main thread for tape naming
            send({ type: "kidlisp:cached-code", content: cachedCode });
            
            // Use cache key based on cached code to avoid regenerating the same QR
            const cacheKey = `qr_${cachedCode}`;
            
            // Get the font and ensure it's properly loaded before proceeding
            const font = typefaceCache.get("MatrixChunky8");
            
            // Check if ALL glyphs in the label text are loaded
            // The label will be "$" + cachedCode (e.g., "$wipe", "$line", etc.)
            // We need to verify every character's glyph is loaded to avoid pop-in
            const labelText = `$${cachedCode}`;
            let allGlyphsLoaded = false;
            
            if (font && font.glyphs) {
              allGlyphsLoaded = true;
              const missingGlyphs = [];
              
              for (const char of labelText) {
                const glyph = font.glyphs[char];
                // A real glyph has properties like dwidth, advance, or resolution
                // The Proxy returns null for missing glyphs
                const isRealGlyph = glyph && glyph !== null && 
                  (glyph.dwidth || glyph.advance !== undefined || glyph.resolution);
                
                if (!isRealGlyph) {
                  allGlyphsLoaded = false;
                  missingGlyphs.push(char);
                }
              }
              
              // if (!allGlyphsLoaded) {
              //   console.log('[QR] MatrixChunky8 glyphs pending:', {
              //     labelText,
              //     missingGlyphs: missingGlyphs.join('') || 'unknown'
              //   });
              // }
            }
            
            const shouldShowQR = allGlyphsLoaded;
            
            // If font exists but glyphs not loaded yet, trigger loading and request repaint
            if (font && !shouldShowQR) {
              if (!font.__loadPromise) {
                console.log('[QR] Starting font load...');
                ensureTypefaceLoaded(font);
              }
              if (font.__loadPromise) {
                font.__loadPromise.then(() => {
                  font.__loaded = true;
                  // Trigger repaint to show QR once font is ready
                  if (typeof window !== "undefined" && window.$activePaintApi?.needsPaint) {
                    window.$activePaintApi.needsPaint();
                  }
                }).catch((err) => {
                  console.warn("Failed to load MatrixChunky8 for QR:", err);
                  font.__loaded = true; // Mark as loaded to prevent infinite waiting
                });
              }
            }
            

            // Check if this QR overlay is already cached (unless caching is disabled or font not loaded)
            const isQRCacheDisabled = isQROverlayCacheDisabled();
            const hasQRCache = qrOverlayCache.has(cacheKey);
            
            // Declare variables for QR positioning and sizing (used in both cached and fresh QR paths)
            let overlayWidth, overlayHeight, startX, startY;
            

            if (!isQRCacheDisabled && shouldShowQR && hasQRCache) {

              const cachedQrData = qrOverlayCache.get(cacheKey);
              
              // Store QR dimensions in animation state for proper bounding box animations
              hudAnimationState.qrSize = Math.max(cachedQrData.width, cachedQrData.height);
              
              if (hudAnimationState.qrFullscreen) {
                // Fullscreen mode: integer scaling only for pixel-perfect QR
                const originalWidth = cachedQrData.width;
                const originalHeight = cachedQrData.height;
                
                // Calculate maximum integer scale that fits screen with padding
                const padding = Math.min(screen.width, screen.height) * 0.1; // 10% padding
                const maxWidth = screen.width - padding * 2;
                const maxHeight = screen.height - padding * 2;
                
                const maxScaleX = Math.floor(maxWidth / originalWidth);
                const maxScaleY = Math.floor(maxHeight / originalHeight);
                const scale = Math.min(maxScaleX, maxScaleY, 8); // Cap at 8x scale
                
                // Use integer scaling for pixel-perfect results
                overlayWidth = originalWidth * scale;
                overlayHeight = originalHeight * scale;
                
                // Calculate text dimensions for canvas sizing
                const codeText = `$${cachedCode}`;
                const fontSize = 16; // Fixed 2x scale (8px base font * 2 = 16px)
                const textPadding = 8;
                
                // Calculate actual text width using font advances for proper sizing
                const font = typefaceCache.get("MatrixChunky8");
                const advances = font?.data?.advances || {};
                let actualTextWidth = 0;
                for (const char of codeText) {
                  const charWidth = advances[char] || 4; // Default 4px per char if no advance data
                  actualTextWidth += charWidth;
                }
                // Scale up by 2x for our size: 2 scaling
                actualTextWidth *= 2;
                
                const textWidth = actualTextWidth + textPadding * 2; // Add padding
                const textHeight = fontSize + textPadding * 2;
                
                // Use full screen canvas to position QR code (centered) and text
                const canvasHeight = screen.height;
                const canvasWidth = screen.width;
                
                // Center QR on screen (ensure integer coordinates)
                const qrX = Math.floor((screen.width - overlayWidth) / 2);
                const qrY = Math.floor((screen.height - overlayHeight) / 2);
                
                // Position starts at top-left of screen since we're using full screen canvas
                startX = 0;
                startY = 0;
                
                // Create scaled QR overlay with styled code text
                qrOverlay = $api.painting(canvasWidth, canvasHeight, async ($) => {
                  $.unmask(); // Ensure QR overlay renders without piece mask
                  // Draw scaled QR with integer scaling (centered in canvas)
                  
                  for (let y = 0; y < originalHeight; y++) {
                    for (let x = 0; x < originalWidth; x++) {
                      const srcIndex = (y * originalWidth + x) * 4;
                      
                      if (srcIndex < cachedQrData.basePixels.length) {
                        const r = cachedQrData.basePixels[srcIndex];
                        const g = cachedQrData.basePixels[srcIndex + 1];
                        const b = cachedQrData.basePixels[srcIndex + 2];
                        
                        $.ink(r, g, b);
                        // Draw scale x scale pixel block
                        $.box(qrX + x * scale, qrY + y * scale, scale, scale);
                      }
                    }
                  }
                  
                  // Add styled code text positioned at top-left corner of screen
                  const textX = 10; // Small margin from left edge of screen
                  const textY = 10; // Small margin from top edge of screen
                  
                  // Draw black shadow
                  $.ink("black");
                  $.write(codeText, { x: textX + 1, y: textY + 1, size: 2 });
                  
                  // Draw white text on black background - try MatrixChunky8 first, fallback to default
                  $.ink("white");
                  const matrixFont = typefaceCache.get("MatrixChunky8");
                  if (matrixFont && matrixFont.glyphs) {
                    try {
                      $.write(codeText, { x: textX, y: textY, size: 2 }, undefined, undefined, false, "MatrixChunky8");
                    } catch (error) {
                      // Fallback to default font if MatrixChunky8 fails
                      $.write(codeText, { x: textX, y: textY, size: 2 });
                    }
                  } else {
                    // Use default font if MatrixChunky8 not available
                    $.write(codeText, { x: textX, y: textY, size: 2 });
                  }
                  
                  // Add "TAP TO CLOSE" instruction at bottom center of screen
                  const closeText = "TAP TO CLOSE";
                  const closeTextY = screen.height - 20; // 20px from bottom
                  const closeTextX = Math.floor(screen.width / 2) - 30; // Rough center
                  
                  // Draw black shadow for close text
                  $.ink("black");
                  $.write(closeText, { x: closeTextX + 1, y: closeTextY + 1, size: 1 });
                  
                  // Draw white close text
                  $.ink("white");
                  $.write(closeText, { x: closeTextX, y: closeTextY, size: 1 });
                });
              } else {
                // Normal mode: use original positioning
                const margin = 4;
                overlayWidth = cachedQrData.width;
                overlayHeight = cachedQrData.height;
                startX = screen.width - overlayWidth - margin;
                startY = screen.height - overlayHeight - margin;
                
                // Create fresh overlay for transfer from cached data
                qrOverlay = {
                  width: cachedQrData.width,
                  height: cachedQrData.height,
                  pixels: new Uint8ClampedArray(cachedQrData.basePixels) // Fresh copy for transfer
                };
              }
              
              // Add QR overlay to sendData with animation effects
              sendData.qrOverlay = {
                x: startX + (hudAnimationState.qrFullscreen ? 0 : hudAnimationState.qrSlideOffset.x),
                y: startY + (hudAnimationState.qrFullscreen ? 0 : hudAnimationState.qrSlideOffset.y),
                opacity: hudAnimationState.opacity,
                img: qrOverlay
              };
              
              // Add clickable hitbox for QR code
              if (hudAnimationState.qrFullscreen) {
                // Fullscreen QR: make entire QR tappable to go back
                send({
                  type: "button:hitbox:add",
                  content: {
                    label: "qr-fullscreen",
                    box: {
                      x: startX,
                      y: startY,
                      w: overlayWidth,
                      h: overlayHeight
                    },
                    message: "qr-fullscreen-tap"
                  }
                });
              } else {
                // Corner QR: make it tappable to go fullscreen
                send({
                  type: "button:hitbox:add",
                  content: {
                    label: "qr-corner",
                    box: {
                      x: startX + hudAnimationState.qrSlideOffset.x,
                      y: startY + hudAnimationState.qrSlideOffset.y,
                      w: overlayWidth,
                      h: overlayHeight
                    },
                    message: "qr-corner-tap"
                  }
                });
              }

            } else {

              // Always use prompt.ac for QR codes (even in dev/local)
              let url = "https://prompt.ac";
              
              // Use the cached nanoid code with $ prefix for a much shorter URL
              url += `/$${cachedCode}`;
              
              // Generate QR code with medium error correction for better scannability
              const cells = qr(url, { errorCorrectLevel: ErrorCorrectLevel.M }).modules;
              
              // Calculate size and position for bottom-right corner with 4px margin
              const margin = 4;
              const cellSize = 1; // 1 pixel per cell for smallest 1:1 size
              const qrSize = cells.length * cellSize;
              
              // Store QR dimensions in animation state for proper bounding box animations
              hudAnimationState.qrSize = qrSize;
            
              // Position in bottom-right corner
              let startX = screen.width - qrSize - margin - 1; // Account for shadow width
              const textHeight = 12; // Space for MatrixChunky8 8px font with shadow (8px + 4px padding)
              const totalHeight = qrSize + textHeight;
              let startY = screen.height - totalHeight - margin; // Move 1px closer to bottom for balanced margins
              
              // Create QR overlay using painting API with extra space for shadow
              const textAreaHeight = 9;
              const qrOffsetY = textAreaHeight; // QR starts right after text area (no gap)
              const canvasHeight = qrOffsetY + qrSize + 2; // Ensure room for bottom shadow
              const generatedQR = $api.painting(qrSize + 1, canvasHeight, async ($) => {
                $.unmask(); // Ensure QR renders without piece mask
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
                  const charWidth = advances[char] || 4;
                  actualTextWidth += charWidth;
                }
                
                // Text area configuration - be more generous with sizing
                const textPaddingLeft = 1; // Minimal left padding
                const textPaddingRight = 1; // Minimal right padding for safety
                const textAreaWidth = actualTextWidth + textPaddingLeft + textPaddingRight;
                const textAreaHeight = 10; // Increased height (was 9)
                
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
                  
                  if (getPackMode()) {
                    // In OBJKT mode, try MatrixChunky8 first, fall back to default if not available
                    let matrixFont = typefaceCache.get("MatrixChunky8");
                    if (matrixFont) {
                      
                      // Check if this is a proper Typeface instance with all methods
                      if (!matrixFont.getGlyph || typeof matrixFont.getGlyph !== 'function') {
                        // Create a new proper Typeface instance
                        const newMatrixFont = new Typeface("MatrixChunky8");
                        await newMatrixFont.load($commonApi.net.preload);
                        // Copy over existing glyphs data if any
                        if (matrixFont.glyphs) {
                          Object.assign(newMatrixFont.glyphs, matrixFont.glyphs);
                        }
                        matrixFont = newMatrixFont;
                        typefaceCache.set("MatrixChunky8", matrixFont);
                      }
                      
                      // Check if glyphs actually work by testing a specific character
                      const testGlyph = matrixFont.glyphs['$'];
                      // MatrixChunky8 uses BDF structure: {resolution, offset, baselineOffset, advance, commands, bbx}
                      const glyphsWorking = testGlyph && typeof testGlyph === 'object' && 
                                          (testGlyph.pixels || testGlyph.commands || testGlyph.resolution);
                      
                      if (!glyphsWorking) {
                        // Create simple fallback glyphs for common characters
                        const fallbackGlyph = {
                          resolution: [6, 8],
                          pixels: [
                            [0, 1, 1, 1, 1, 0],
                            [1, 0, 0, 0, 0, 1],
                            [1, 0, 1, 1, 0, 1],
                            [1, 0, 1, 1, 0, 1],
                            [1, 0, 0, 0, 0, 1],
                            [1, 0, 0, 0, 0, 1],
                            [0, 1, 1, 1, 1, 0],
                            [0, 0, 0, 0, 0, 0]
                          ]
                        };
                        
                        // DO NOT replace glyphs object - it's a Proxy that loads on-demand
                        // Just populate common characters if they're missing
                        if (matrixFont.glyphs) {
                          const commonChars = '$rozeabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
                          for (const char of commonChars) {
                            if (!matrixFont.glyphs[char]) {
                              matrixFont.glyphs[char] = fallbackGlyph;
                              matrixFont[char] = fallbackGlyph; // Also set directly on font object
                            }
                          }
                        }
                      }
                      
                      // Check if font has reasonable glyph data before rendering
                      const testChar = matrixFont.glyphs && (matrixFont.glyphs['$'] || matrixFont.glyphs['A'] || matrixFont.glyphs['a']);
                      const hasValidGlyphs = testChar && (testChar.pixels || testChar.commands || testChar.resolution);
                      if (hasValidGlyphs) {
                        $.write(codeToRender, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");
                      } else {
                        // Fallback to default font if MatrixChunky8 isn't ready
                        $.write(codeToRender, { x: textX, y: textY });
                      }
                    } else {
                      // Fallback to default font if MatrixChunky8 not available
                      $.write(codeToRender, { x: textX, y: textY });
                    }
                  } else {
                    // Use MatrixChunky8 font in normal mode - with simple fallback
                    const matrixFont = typefaceCache.get("MatrixChunky8");
                    if (matrixFont) {
                      try {
                        $.write(codeToRender, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");
                      } catch (error) {
                        // Fallback to default font if MatrixChunky8 fails
                        $.write(codeToRender, { x: textX, y: textY });
                      }
                    } else {
                      // Use default font if MatrixChunky8 not available
                      $.write(codeToRender, { x: textX, y: textY });
                    }
                  }
                } else {
                  // Shadow style: black shadow first, then white text
                  // Draw black shadow (1px offset) - no rotation for now
                  $.ink("black");
                  
                  if (getPackMode()) {
                    // In OBJKT mode, try MatrixChunky8 first, fall back to default if not available
                    const matrixFont = typefaceCache.get("MatrixChunky8");
                    if (matrixFont && matrixFont.glyphs) {
                      const testChar = matrixFont.glyphs['$'] || matrixFont.glyphs['A'] || matrixFont.glyphs['a'];
                      const hasValidGlyphs = testChar && (testChar.pixels || testChar.commands || testChar.resolution);
                      if (hasValidGlyphs) {
                        $.write(codeToRender, { x: textX + 1, y: textY + 1 }, undefined, undefined, false, "MatrixChunky8");
                      } else {
                        $.write(codeToRender, { x: textX + 1, y: textY + 1 });
                      }
                    } else {
                      $.write(codeToRender, { x: textX + 1, y: textY + 1 });
                    }
                  } else {
                    // Try MatrixChunky8 with simple fallback
                    const matrixFont = typefaceCache.get("MatrixChunky8");
                    if (matrixFont) {
                      try {
                        $.write(codeToRender, { x: textX + 1, y: textY + 1 }, undefined, undefined, false, "MatrixChunky8");
                      } catch (error) {
                        $.write(codeToRender, { x: textX + 1, y: textY + 1 });
                      }
                    } else {
                      $.write(codeToRender, { x: textX + 1, y: textY + 1 });
                    }
                  }
                  
                  // Draw white text on top - no rotation for now
                  $.ink("white");
                  if (getPackMode()) {
                    // In OBJKT mode, try MatrixChunky8 first, fall back to default if not available
                    const matrixFont = typefaceCache.get("MatrixChunky8");
                    if (matrixFont && matrixFont.glyphs) {
                      const testChar = matrixFont.glyphs['$'] || matrixFont.glyphs['A'] || matrixFont.glyphs['a'];
                      const hasValidGlyphs = testChar && (testChar.pixels || testChar.commands || testChar.resolution);
                      if (hasValidGlyphs) {
                        $.write(codeToRender, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");
                      } else {
                        $.write(codeToRender, { x: textX, y: textY });
                      }
                    } else {
                      $.write(codeToRender, { x: textX, y: textY });
                    }
                  } else {
                    // Try MatrixChunky8 with simple fallback  
                    const matrixFont = typefaceCache.get("MatrixChunky8");
                    if (matrixFont) {
                      try {
                        $.write(codeToRender, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");
                      } catch (error) {
                        $.write(codeToRender, { x: textX, y: textY });
                      }
                    } else {
                      $.write(codeToRender, { x: textX, y: textY });
                    }
                  }
                }
                
                // Draw gray shadow along the right side and bottom of the entire overlay
                $.ink("gray", 128);
                // Right shadow - offset 1px down from top like a drop shadow
                $.box(qrSize, 1, 1, qrOffsetY + qrSize);
                // Bottom shadow - offset 1px from left edge like a drop shadow
                $.box(1, qrOffsetY + qrSize, qrSize, 1);
                
                // Add a subtle white highlight to indicate the QR is clickable (corner only)
                // This creates a "button-like" appearance
                $.ink("white", 64); // Semi-transparent white
                // Top highlight line
                $.line(0, qrOffsetY, qrSize - 1, qrOffsetY);
                // Left highlight line
                $.line(0, qrOffsetY, 0, qrOffsetY + qrSize - 1);
              });
              
              // Don't cache QR overlay if font isn't fully loaded yet (text label needs to re-render)
              // Cache the base QR data (not the transferable pixels)
              const qrData = {
                width: generatedQR.width,
                height: generatedQR.height,
                basePixels: new Uint8ClampedArray(generatedQR.pixels) // Keep a safe copy for caching
              };
              
              // Store QR dimensions in animation state for proper bounding box animations
              hudAnimationState.qrSize = Math.max(qrData.width, qrData.height);
              
              if (hudAnimationState.qrFullscreen) {
                // Fullscreen mode: integer scaling only for pixel-perfect QR
                const originalQrSize = cells.length;
                
                // Calculate maximum integer scale that fits screen with padding
                const padding = Math.min(screen.width, screen.height) * 0.1; // 10% padding
                const maxSize = Math.min(screen.width, screen.height) - padding * 2;
                
                const maxCellSize = Math.floor(maxSize / originalQrSize);
                const cellSize = Math.max(2, Math.min(maxCellSize, 12)); // Integer cell size, 2-12px
                const finalQrSize = originalQrSize * cellSize;
                
                overlayWidth = finalQrSize;
                overlayHeight = finalQrSize;
                
                // Calculate text dimensions for canvas sizing
                const codeText = `$${cachedCode}`;
                const fontSize = 16; // Fixed 2x scale (8px base font * 2 = 16px)
                const textPadding = 8;
                
                // Calculate actual text width using font advances for proper sizing
                const font = typefaceCache.get("MatrixChunky8");
                const advances = font?.data?.advances || {};
                let actualTextWidth = 0;
                for (const char of codeText) {
                  const charWidth = advances[char] || 4; // Default 4px per char if no advance data
                  actualTextWidth += charWidth;
                }
                // Scale up by 2x for our size: 2 scaling
                actualTextWidth *= 2;
                
                const textWidth = actualTextWidth + textPadding * 2; // Add padding
                const textHeight = fontSize + textPadding * 2;
                
                // Use full screen canvas to position QR code (centered) and text (top-left)
                const canvasHeight = screen.height;
                const canvasWidth = screen.width;
                
                // Position QR code in center of screen
                const qrX = Math.floor((screen.width - finalQrSize) / 2);
                const qrY = Math.floor((screen.height - finalQrSize) / 2);
                
                // Position starts at top-left of screen since we're using full screen canvas
                startX = 0;
                startY = 0;
                
                // Generate fullscreen canvas with centered QR and top-left text
                const fullscreenQR = $api.painting(canvasWidth, canvasHeight, ($) => {
                  $.unmask(); // Ensure fullscreen QR renders without piece mask
                  // Draw QR code centered on screen
                  for (let y = 0; y < cells.length; y++) {
                    for (let x = 0; x < cells.length; x++) {
                      const isBlack = cells[y][x];
                      if (isBlack) {
                        $.ink("black");
                      } else {
                        $.ink("white");
                      }
                      $.box(qrX + x * cellSize, qrY + y * cellSize, cellSize, cellSize);
                    }
                  }
                  
                  // Draw text in absolute top-left corner of screen
                  const codeText = `$${cachedCode}`;
                  const textX = 10; // Small margin from left edge of screen
                  const textY = 10; // Small margin from top edge of screen
                  
                  // Draw black shadow offset by 1px
                  $.ink("black");
                  $.write(codeText, { x: textX + 1, y: textY + 1, size: 2 }); // 2x scale for shadow
                  
                  // Draw white text on top
                  $.ink("white");
                  $.write(codeText, { x: textX, y: textY, size: 2 }); // 2x scale for main text
                  
                  // Add "TAP TO CLOSE" instruction at bottom center of screen
                  const closeText = "TAP TO CLOSE";
                  const closeTextY = screen.height - 20; // 20px from bottom
                  const closeTextX = Math.floor(screen.width / 2) - 30; // Rough center (adjust as needed)
                  
                  // Draw black shadow for close text
                  $.ink("black");
                  $.write(closeText, { x: closeTextX + 1, y: closeTextY + 1, size: 1 });
                  
                  // Draw white close text
                  $.ink("white");
                  $.write(closeText, { x: closeTextX, y: closeTextY, size: 1 });
                });
                
                // Update qrData for fullscreen
                qrData.width = fullscreenQR.width;
                qrData.height = fullscreenQR.height;
                qrData.basePixels = new Uint8ClampedArray(fullscreenQR.pixels);
                
                qrOverlay = {
                  width: qrData.width,
                  height: qrData.height,
                  pixels: new Uint8ClampedArray(qrData.basePixels)
                };
              } else {
                // Normal mode: use original positioning and size
                overlayWidth = qrData.width;
                overlayHeight = qrData.height;
                startX = screen.width - overlayWidth - margin;
                startY = screen.height - overlayHeight - margin; // Removed +1 to prevent label shadow overlap
                
                // Create fresh overlay for transfer each time
                qrOverlay = {
                  width: qrData.width,
                  height: qrData.height,
                  pixels: new Uint8ClampedArray(qrData.basePixels) // Fresh copy for transfer
                };
              }
              
              // Cache the QR data for this piece (not the transferable version)
              // Cache if we have some characters loaded to stabilize the QR
              if (!isQRCacheDisabled && shouldShowQR && !hudAnimationState.qrFullscreen) {
                qrOverlayCache.set(cacheKey, qrData);
              }
              
              // Add QR overlay to sendData with exact position and animation effects
              sendData.qrOverlay = {
                x: startX + (hudAnimationState.qrFullscreen ? 0 : hudAnimationState.qrSlideOffset.x),
                y: startY + (hudAnimationState.qrFullscreen ? 0 : hudAnimationState.qrSlideOffset.y),
                opacity: hudAnimationState.opacity,
                img: qrOverlay
              };
              
              // Add clickable hitbox for QR code
              if (hudAnimationState.qrFullscreen) {
                // Fullscreen QR: make entire QR tappable to go back
                send({
                  type: "button:hitbox:add",
                  content: {
                    label: "qr-fullscreen",
                    box: {
                      x: startX,
                      y: startY,
                      w: overlayWidth || qrData.width,
                      h: overlayHeight || qrData.height
                    },
                    message: "qr-fullscreen-tap"
                  }
                });
              } else {
                // Corner QR: make it tappable to go fullscreen
                send({
                  type: "button:hitbox:add",
                  content: {
                    label: "qr-corner",
                    box: {
                      x: startX + hudAnimationState.qrSlideOffset.x,
                      y: startY + hudAnimationState.qrSlideOffset.y,
                      w: overlayWidth || qrData.width,
                      h: overlayHeight || qrData.height
                    },
                    message: "qr-corner-tap"
                  }
                });
              }

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

      if (croppedBox?.w > 0 && croppedBox?.h > 0) {
        transferredPixels = dirtyBox.crop(screen);
        sendData.pixels = transferredPixels;
        sendData.dirtyBox = croppedBox;
      } else if (painted === true) {
        // TODO: Toggling this causes a flicker in `line`... but helps prompt. 2022.01.29.13.21
        // Otherwise render everything if we drew anything!
        transferredPixels = screen.pixels;
        sendData.pixels = transferredPixels;
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

      // Note: transferredPixels will be undefined when sendData === {}.
      if (sendData.pixels) {
        // 🛡️ Create a copy for transfer to avoid detaching the main rendering buffer
        const pixelsCopy = new Uint8ClampedArray(sendData.pixels);
        sendData.pixels = pixelsCopy.buffer;
      } else {
        sendData.pixels = content.pixels;
      }

      if (sendData.pixels?.byteLength === 0) sendData.pixels = undefined;

      let transferredObjects = [sendData.pixels];

      if (sendData.label) {
        // 🛡️ Create a copy for transfer to avoid detaching the label buffer
        const labelPixelsCopy = new Uint8ClampedArray(sendData.label.img.pixels);
        transferredObjects.push(labelPixelsCopy.buffer);
      }

      if (sendData.qrOverlay) {
        // 🛡️ Create a copy for transfer to avoid detaching the QR overlay buffer
        const qrPixelsCopy = new Uint8ClampedArray(sendData.qrOverlay.img.pixels);
        transferredObjects.push(qrPixelsCopy.buffer);
      }

      if (sendData.qrCornerText) {
        // 🛡️ Create a copy for transfer to avoid detaching the QR corner text buffer
        const qrCornerPixelsCopy = new Uint8ClampedArray(sendData.qrCornerText.img.pixels);
        transferredObjects.push(qrCornerPixelsCopy.buffer);
      }

      if (sendData.tapeProgressBar) {
        // 🛡️ Create a copy for transfer to avoid detaching the tape progress bar buffer
        const tapePixelsCopy = new Uint8ClampedArray(sendData.tapeProgressBar.img.pixels);
        transferredObjects.push(tapePixelsCopy.buffer);
      }

      if (sendData.merryProgressBar) {
        // 🎄 Create a copy for transfer to avoid detaching the merry progress bar buffer
        const merryPixelsCopy = new Uint8ClampedArray(sendData.merryProgressBar.img.pixels);
        transferredObjects.push(merryPixelsCopy.buffer);
      }

      // console.log("TO:", transferredObjects);
      // console.log("Sent data:", sendData);

      sendData.sound = sound;

      send({ type: "render", content: sendData }, transferredObjects);

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
      // 🛡️ Create a copy for transfer to avoid detaching the pixels buffer
      const transferPixels = pixels ? new Uint8ClampedArray(pixels) : null;
      
      send(
        {
          type: "update",
          content: {
            didntRender: true,
            loading,
            pixels: transferPixels?.buffer,
            width: content.width,
            height: content.height,
            sound,
          },
        },
        [transferPixels?.buffer],
      );
    }

    // Wait 8 frames of the default piece before loading the initial piece.
    // And also make sure the session has been queried.
    // console.log(sessionStarted);
    if (
      paintCount > 8n &&
      (sessionStarted || PREVIEW_OR_ICON || $commonApi.net.sandboxed)
    ) {
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

// Debug utility for HUD hitbox visualization
globalThis.toggleHudHitboxDebug = () => {
  globalThis.debugHudHitbox = !globalThis.debugHudHitbox;
  $commonApi.needsPaint(); // Force repaint to show/hide the debug visualization
};

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
