// Prompt, 2023.5.26.21.38.35
//         2021.11.28.03.13 (Created on)
// A language based "access-everything" console with LLM fallback.
// 🔄 Cache bust: 2025-11-18-v2

// console.log("📨 ✅ prompt.mjs module loading - receive function will be defined and exported");

/* #region 📚 README
  🎄 Merry Pipeline System
  - Chain pieces together with configurable durations
  - Syntax: `merry piece1 piece2 piece3` (default 5 seconds each)
  - Custom: `merry tone:3 clock:5 wand:2` or `merry 3-tone 5-clock 2-wand`
  - Loop forever: `merryo 0.25-tone` (use `stop` to exit)
  - Uniform timing shorthand:
    - `merryo.1 a b c` -> 0.1s each (same as `merryo 0.1-a 0.1-b 0.1-c`)
    - `mo.1 a b c` -> even shorter! (0.1s each, loops)
    - `mo.05 a b c` -> 50ms each, loops
  - URL-able: `/merryo:0.5-tone:0.5-clock` or `/mo.1:a:b:c`
  - Stop early: `merry:stop` or `stop`
  - Example: `merry tone:3 clock:5` plays tone for 3s, then clock for 5s, then returns to prompt
#endregion */

/* #region 🏁 todo
  + Later
  - [] Generate or pretty print docs (made from the APIs) inside this disk.
       (This would allow people to have a reference while writing disks.)
  + Done
  - [x] Reset pieceCount on developer reload.
  - [x] Reposition buttons once the frame is resized.
  - [x] Hide buttons after logging in.
  - [x] Hide buttons once starting to type.
  - [x] Wire up login and sign-up buttons.
  - [x] Make Login Button
    - [x] Get layering working with `write`.
  - [x] Make Sign-Up Button
  - [x] Positioning Login button in center of display.
  - [x] Write an initial prompt program.
  - [x] Generically "lock" prompt after input before a result returns.
        Show a spinner if too much time has passed?
  - [x] Pass the api into `halt`.
  - [x] create a system.prompt.input reference in the api.
  - [x] `export const palette`
  - [x] `export const wrap`
  - [x] `export const autolock`
#endregion */

const before = `
You are playing a character who tries to help me find the command I'm searching for

- The following is a data set of all possible options for commands:
  - 'bgm', 'bits', 'blank', 'bleep', 'bubble', 'camera',
  'channel', 'decode', 'baktok', 'painting', 'desktop',
  'download', 'encode', 'ff', 'ff1', 'freaky-flowers', 'gargoyle', 'handle',
  'happy-hands-assembler', 'hha', 'liar', 'line', 'login',
  'logout', 'm2w2', 'melody', 'metronome', 'microphone',
  'no!', 'no', 'oval', 'done', 'paint', 'paste', 'handprint',
  'plot', 'profile', 'prompt', 'pull', 'rect',
  'girlfriend', 'boyfriend', 'mom', 'dad', 'husband', 'wife', 'kid', 'brother', 'sister', 'scawy-snake', 'scream', 'sfx', 'shape', 'sign', 'sing', 'smear',
  'song', 'sparkle', 'right', 'left', 'flip', 'flop',
  'staka', 'starfield', 'tone', 'tracker', 'valbear', 'vary', 'video', 'wand', 'wg',
  'wgr', 'whistle', 'whistlegraph', 'wipe', 'word', 'zoom', 'booted-by'.

- If I type a word that is similar to one of the commands, you only respond "did you mean
(insert correct command)?"
  - for example, if I write "linr", you write "Try typing 'line' instead"
  - you only suggest correct commands that are in the above data set
  - when you suggest a command, always put it in "quotes."
  - if I type "hife" you do not suggest "life" because that is not a command in the data set
  - you do not respond with any additional information

If the user asks to delete their account or enters "delete" or "deactivate", you tell them to enter "delete-erase-and-forget-me" to delete their account.

If the user enters 'goodiepal' please reply: Yes, but people on the Faro islands call me Pruttipal, so enter 'prutti' instead.

The word I'm entering is:`;

const after = ``;
const forgetful = true;

const TYPO_REPLY = `
Use Aesthetic Computer by entering a correct word.\n\nEnter "list" for some available words.\n\nEnter "chat" for help.\n\nText 1-508-728-4043 for tips.\n\n - @jeffrey`.trim();

import { Android, MetaBrowser, iOS } from "../lib/platform.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { parse } from "../lib/parse.mjs";
import { signed as shop } from "../lib/shop.mjs";
import { ordfish } from "./ordfish.mjs";
import { createHandleAutocomplete } from "../lib/autocomplete.mjs";
import {
  isPromptInKidlispMode,
  isActualKidLisp,
  decodeKidlispFromUrl,
  encodeKidlispForUrl,
  isKidlispSource,
  getSyntaxHighlightingColors,
  fetchCachedCode,
  tokenize,
} from "../lib/kidlisp.mjs";
import { KidLisp } from "../lib/kidlisp.mjs";
import { parseMessageElements, applyColorCodes, defaultColorTheme } from "../lib/chat-highlighting.mjs";
import { colorizeText, hasHotlinks } from "../lib/hotlink.mjs";
import * as products from "./common/products.mjs";
const { abs, max, min, sin, cos } = Math;
const { floor } = Math;
const { keys } = Object;

// Error / feedback flash on command entry.
let theme = "default"; // Prompt theme: "default", "serious", "neo" (prompt:serious, prompt:neo)

let flash;
let flashShow = false;
let flashColor = [];
let flashPresent = false;

let progressBar = -1; // If not zero, then draw a progress bar.
let progressTrick; // A faux growth period on the progress bar.
let cachedGizmo; // Reference to gizmo for use in act() function
let progressPhase = ""; // Current phase of upload (e.g., "ZIPPING", "UPLOADING IMAGE")
let progressPercentage = 0; // 0-100

// 📦 Pack progress state
let packProgress = null; // { timeline, startTime, code } or null

// Structured pack timeline — each step has an id, label, and status.
// Steps are marked done/active/skipped as SSE progress events arrive.
const PACK_STEP_DEFS = [
  { id: 'fetch',     label: 'Fetch Source' },
  { id: 'init',      label: 'Initialize' },       // .mjs pieces
  { id: 'deps',      label: 'Resolve Dependencies' },
  { id: 'cache-hit', label: 'Load Cache' },        // skipped on cold start
  { id: 'discover',  label: 'Discover Files' },    // skipped on cache hit
  { id: 'minify',    label: 'Minify Code' },       // skipped on cache hit
  { id: 'fonts',     label: 'Embed Fonts' },       // skipped on cache hit
  { id: 'piece',     label: 'Load Piece' },        // .mjs pieces only
  { id: 'paintings', label: 'Embed Paintings' },   // only if paintings exist
  { id: 'generate',  label: 'Generate HTML' },
  { id: 'compress',  label: 'Compress Pack' },
  { id: 'complete',  label: 'Done!' },
];

function makePackTimeline() {
  return PACK_STEP_DEFS.map(s => ({
    ...s, status: 'pending', message: null, time: null,
  }));
}

// Mark a timeline step active, completing the previous active step.
function advancePackStep(timeline, stageId, message) {
  // Complete any currently-active step.
  for (const step of timeline) {
    if (step.status === 'active') {
      step.status = 'done';
      step.time = performance.now();
    }
  }
  const step = timeline.find(s => s.id === stageId);
  if (step) {
    step.status = 'active';
    step.message = message;
    step.time = performance.now();
  }
}

// Finish: mark all remaining pending steps as skipped and active as done.
function finalizePackTimeline(timeline) {
  for (const step of timeline) {
    if (step.status === 'active') step.status = 'done';
    else if (step.status === 'pending') step.status = 'skipped';
  }
}

// 🎄 Autorun state (for URL-based command execution like merry URLs)
let pendingAutorun = null; // { text: string, api: object }

// Manual adjustment for ꜩ symbol (unifont has yOffset: -2 which shifts it down)
const TEZ_Y_ADJUST = -5;

let login, // A login button in the center of the display.
  signup, // A Sign-up button.
  profile, // A profile page button.
  profileAction,
  walletBtn, // Tezos wallet button (shown when connected)
  commitBtn, // Commit hash button (navigates to commits piece)
  kidlispBtn; // KidLisp.com button (shown when in KidLisp mode)
let clearBtn; // 🧹 "Blank" button (fixed top-right, appears at 32+ chars)
let clearBtnConfirming = false; // Two-tap confirmation state
let clearBtnConfirmTimeout = null; // Reset timer for confirmation

let resendVerificationText;
let ellipsisTicker;
let chatTicker; // Ticker instance for chat messages
let chatTickerButton; // Button for chat ticker hover interaction
let clockChatTicker; // Ticker instance for Laer-Klokken clock chat
let clockChatTickerButton; // Button for clock chat ticker
let clockChatMessages = []; // Messages from clock chat (starts empty, shows Matrix until API loads)
let contentTicker; // Ticker instance for mixed $kidlisp, #painting, !tape content
let contentTickerButton; // Button for content ticker hover interaction
let mediaPreviewBox; // Shared preview box renderer for all media types

// 🎰 UNITICKER - Unified ticker combining chat, laer-klokken, and media content
let uniticker; // Single ticker for all combined content
let unitickerButton; // Button for hover interaction
let unitickerItems = []; // Combined items: {type: 'chat'|'clock'|'media', text: string, code: string, ...}
let unitickerHoveredItem = null; // Currently hovered item for tooltip
let unitickerTooltipVisible = false; // Whether to show the "Enter code" tooltip
// Idle auto-select state
let unitickerIdleFrames = 0; // Frames since last pen movement
let unitickerLastPenX = -1; // Last pen X position
let unitickerLastPenY = -1; // Last pen Y position
let unitickerAutoSelectedItem = null; // Auto-selected item when idle
let unitickerAutoSelectedX = 0; // X position of auto-selected item
let unitickerAutoSelectedWidth = 0; // Width of auto-selected item
const UNITICKER_IDLE_THRESHOLD = 120; // 2 seconds at 60fps before auto-selecting

// 💸 FUNDING SEVERITY: Controls funding mode features
// "critical" = full lockdown (chat offline, all alerts)
// "yikes" = chat works, GIVE button shows, but no $ replacement
// "off" = normal operation
export const FUNDING_SEVERITY = "off";

// Legacy export for backwards compatibility
export const FUNDING_MODE = FUNDING_SEVERITY === "critical";

// Helper flags
const showFundingEffects = FUNDING_SEVERITY !== "off"; // GIVE button, face (no longer includes $ replacement)
const isCriticalFunding = FUNDING_SEVERITY === "critical"; // Full lockdown mode

// Set global flags for disk.mjs
if (typeof globalThis !== "undefined") {
  globalThis.AC_FUNDING_MODE = false; // $ replacement disabled - only GIVE button and boot screen active
  globalThis.AC_CHAT_DISABLED = isCriticalFunding; // Only block chat in critical mode
}

// Colorful funding messages for each ticker (using \\color\\ codes for rendering)
// Uses ASCII-only characters compatible with font_1 (MatrixChunky8 loads from assets which has CORS issues)
// English messages
const FUNDING_MESSAGE_CHAT_EN = "*** \\pink\\'chat'\\cyan\\, media storage, and multiplayer are offline due to server bill hardship -- Enter 'give' to help support AC in the New Year! ***";
const FUNDING_MESSAGE_CLOCK_EN = "*** \\orange\\'laer-klokken'\\255,200,100\\ and other services are offline due to server bill hardship -- Enter 'give' to help support AC in the New Year! ***";
// Danish messages
const FUNDING_MESSAGE_CHAT_DA = "*** \\pink\\'chat'\\cyan\\, medielagring og multiplayer er offline pga. serverregning -- Skriv 'give' for at stoette AC i det nye aar! ***";
const FUNDING_MESSAGE_CLOCK_DA = "*** \\orange\\'laer-klokken'\\255,200,100\\ og andre tjenester er offline pga. serverregning -- Skriv 'give' for at stoette AC i det nye aar! ***";
// Alternate between English and Danish every 10 seconds
const getLangPhase = () => Math.floor(Date.now() / 10000) % 2;
const FUNDING_MESSAGE_CHAT = getLangPhase() === 0 ? FUNDING_MESSAGE_CHAT_EN : FUNDING_MESSAGE_CHAT_DA;
const FUNDING_MESSAGE_CLOCK = getLangPhase() === 0 ? FUNDING_MESSAGE_CLOCK_EN : FUNDING_MESSAGE_CLOCK_DA;

// Recovery mode ticker messages (when severity is "yikes" - chat back online but still need support)
const RECOVERY_TICKER_EN = "Happy New Year ~ Enter 'give' to help AC '26 stay online and healthy!";
const RECOVERY_TICKER_DA = "Godt Nytar ~ Skriv 'give' for at stoette AC i 2026!";
export const getRecoveryTicker = () => getLangPhase() === 0 ? RECOVERY_TICKER_EN : RECOVERY_TICKER_DA;
export const showFundingEffectsFlag = showFundingEffects; // Export for chat.mjs

const tinyTickers = !isCriticalFunding; // Use MatrixChunky8 font for tighter, smaller tickers (disabled in critical funding mode - assets CORS)
let contentItems = []; // Store fetched content: {type: 'kidlisp'|'painting'|'tape', code: string, source?: string}
let currentTooltipItem = null; // Current item being shown in tooltip (auto-cycles)
let tooltipTimer = 0; // Timer for switching between items
let tooltipFadeIn = 0; // Fade in animation for tooltip (0-1)
let tooltipItemIndex = 0; // Index of current tooltip item
let tooltipDriftX = 0; // Drift offset X
let tooltipDriftY = 0; // Drift offset Y
let tooltipDriftPhase = 0; // Phase for drift animation (time-based)
let lastTooltipTime = 0; // Timestamp for tooltip animation
let motdBylineHandleBox = null;
let motdBylineHandleHover = false;
let lastContentFetchAt = 0;
let lastClockChatFetchAt = 0;
let contentFetchInFlight = false;
let clockChatFetchInFlight = false;
const CONTENT_REFRESH_MS = 60000;
const CLOCK_CHAT_REFRESH_MS = 20000;
let ruler = false; // Paint a line down the center of the display.
//                   (for measuring the login / signup centering).
// let firstCommandSent = false; // 🏳️
let firstActivation = true; // 🏳️ Used to trigger a startup 🔊🎆

// 📊 FPS Meter state
let fpsTimestamps = [];
let currentFps = 0;
let showFpsMeter = false; // Toggle with backtick key

// 🚫 Content ticker controls
const DISABLE_CONTENT_TICKER = false;
const DISABLE_CONTENT_PREVIEWS = true; // Disable live preview tooltips

let startupSfx, keyboardSfx;

// 🔍 @handle autocomplete
let handleAutocomplete;

// 🎆 Corner particles (for cursor effect)
let cornerParticles = [];

let tapePromiseResolve, tapePromiseReject;
let promptSend;
let promptNeedsPaint;

const TAPE_PREVIEW_DURATION_MS = 5000;
let activeTapePreview = null;
let tapePreviewQueue = [];
let tapePreviewTimeoutId = null;
const TAPE_PREVIEW_MAX_FRAMES = 90;
const KEN_BURNS_MIN_ZOOM = 1.05;
const KEN_BURNS_MAX_ZOOM = 1.25;
const KEN_BURNS_CYCLE_MS = 8000;

let handles; // Keep track of total handles set.
let motd; // Store the moods of the day text
let motdByHandle; // Store the mood author handle
let motdFrame = 0; // Animation frame counter for MOTD effects (time-based)
let lastMotdTime = 0; // Timestamp for MOTD animation
let motdCandidates = [];
let motdCandidateIndex = 0;
let lastMotdCycleTime = 0;

// 🎨 Handle Colors System
const handleColorsCache = new Map();

// Convert a handle to colored text using \color\ syntax
function colorizeHandle(handle, colors) {
  if (!colors || colors.length === 0) return handle;

  const handleWithAt = handle.startsWith("@") ? handle : "@" + handle;
  let result = "";

  const defaultColor = "255,255,255"; // White

  for (let i = 0; i < handleWithAt.length && i < colors.length; i++) {
    const char = handleWithAt[i];
    const color = colors[i];
    // Format: \r,g,b\char\defaultColor\
    result += `\\${color.r},${color.g},${color.b}\\${char}\\${defaultColor}\\`;
  }

  return result;
}

// Fetch handle colors from API
async function fetchHandleColors(handle) {
  const cleanHandle = handle.startsWith("@") ? handle.slice(1) : handle;

  if (handleColorsCache.has(cleanHandle)) {
    return handleColorsCache.get(cleanHandle);
  }

  try {
    const response = await fetch(`/.netlify/functions/handle-colors?handle=${encodeURIComponent(cleanHandle)}`);
    if (response.ok) {
      const data = await response.json();
      if (data.colors) {
        console.log(`🎨 Handle colors for @${cleanHandle}:`, data.colors);
        handleColorsCache.set(cleanHandle, data.colors);
        return data.colors;
      }
    }
  } catch (error) {
    console.warn(`Failed to fetch colors for @${cleanHandle}:`, error);
  }

  return null;
}
const MOTD_CYCLE_MS = 8000;
let previousKidlispMode = false; // Track previous KidLisp mode state for sound triggers
let versionInfo = null; // { deployed, latest, status, behindBy } - git commit status
let versionCommit = null; // Current commit hash for the commit button
let recentCommits = []; // Recent commits for uniticker display: [{hash, message, author, date}]
let updateAvailable = false; // True when a new deployment is detected via long-poll
let versionPollController = null; // AbortController for long-poll requests

// Multilingual "Prompt" translations cycling
const promptTranslations = [
  "Prompt",    // English
  "Indtast",   // Danish - "Enter/Input"
  "Apunta",    // Spanish - "Aim/Point" (call to action)
  "提示",      // Chinese (Simplified) - 2 chars
  "プロンプト", // Japanese - 6 chars
  "프롬프트",  // Korean - 4 chars (Hangul double-width)
  "संकेत",     // Hindi/Devanagari - 4 chars (note: unifont may not shape ligatures perfectly)
  "Eingabe",   // German - "Input/Entry"
  "Saisie",    // French - "Input/Entry"
  "Inserir",   // Portuguese - "Insert/Enter"
  "Inserisci", // Italian - "Insert"
  "Ввод",      // Russian - "Input" (Cyrillic)
  "إدخال",     // Arabic - "Input" (RTL script)
  "Giriş",     // Turkish - "Input/Entry"
  "Εισαγωγή",  // Greek - "Input" (Ancient Greek alphabet)
  "קלט",       // Hebrew - "Input" (RTL script)
  "พรอมต์",    // Thai - "Prompt" (Thai script with curves)
  "Nhập",      // Vietnamese - "Input" (Latin with tone marks)
  "ইনপুট",     // Bengali - "Input" (Bengali/Bangla script)
  "Wprowadź",  // Polish - "Enter/Input" (Slavic with diacritics)
];

// Language names to display under the prompt text
const promptLanguageNames = [
  "English",
  "Danish",
  "Spanish",
  "Chinese",
  "Japanese",
  "Korean",
  "Hindi",
  "German",
  "French",
  "Portuguese",
  "Italian",
  "Russian",
  "Arabic",
  "Turkish",
  "Greek",
  "Hebrew",
  "Thai",
  "Vietnamese",
  "Bengali",
  "Polish",
];
let promptLanguageIndex = 0;
let promptLanguageChangeFrame = 0; // Time-based counter (60 units per second)
let lastLanguageChangeTime = 0; // Timestamp for language change animation
const promptLanguageChangeInterval = 90; // Change language every 90 frames (~1.5 seconds at 60fps)

let defaultDownloadScale = 6;

import * as starfield from "./starfield.mjs";

let server;

let darkModeOn;
let pal;

let autocompletions = {};
const activeCompletions = [];
let fetchingUser = false,
  fetchUserAPI;

// 💡 Tooltip System State
// AC has two distinct param systems:
// - Space params: `command arg1 arg2` → params[0], params[1]
// - Colon params: `command:opt1:opt2` → colon[0], colon[1]
let tooltipState = {
  visible: false,
  command: null,        // Doc entry for current command
  commandName: null,    // Raw command name (without colon suffix)
  colonParams: [],      // Colon param values (after `:` in command)
  spaceParams: [],      // Space param values (after command)
  cursorContext: "command", // "command" | "colon" | "space"
  currentIndex: -1,     // Index within current context
  suggestions: [],      // Valid suggestions for current position
  error: null,          // Validation error
  sig: null,            // Command signature string
  desc: null,           // Current description to show
};
let tooltipDocs = null; // Cached docs.prompts from server

// 💡 Parse current input text for tooltip display
function updateTooltipState(text, cursorPos) {
  if (!text || text.length === 0) {
    tooltipState.visible = false;
    tooltipState.command = null;
    tooltipState.commandName = null;
    return;
  }

  // Parse: "command:colon1:colon2 space1 space2"
  const tokens = text.split(" ");
  const firstToken = tokens[0]; // e.g. "notepat:square:5"
  const colonParts = firstToken.split(":");
  const commandName = colonParts[0].toLowerCase();
  const colonParams = colonParts.slice(1);
  const spaceParams = tokens.slice(1);

  // Find matching doc entry
  let doc = tooltipDocs?.[commandName];

  // Also check for exact match with colon (e.g. "tape:add")
  if (!doc && colonParams.length > 0) {
    const fullKey = `${commandName}:${colonParams[0]}`;
    doc = tooltipDocs?.[fullKey];
  }

  tooltipState.commandName = commandName;
  tooltipState.command = doc;
  tooltipState.colonParams = colonParams;
  tooltipState.spaceParams = spaceParams;
  tooltipState.visible = commandName.length > 0;

  // Determine cursor context and index based on cursor position
  // cursorPos is the character index in the text
  if (cursorPos <= firstToken.length) {
    // Cursor is in the first token (command + colon params)
    const beforeCursor = firstToken.slice(0, cursorPos);
    const colonsBeforeCursor = (beforeCursor.match(/:/g) || []).length;
    if (colonsBeforeCursor === 0) {
      tooltipState.cursorContext = "command";
      tooltipState.currentIndex = -1;
    } else {
      tooltipState.cursorContext = "colon";
      tooltipState.currentIndex = colonsBeforeCursor - 1;
    }
  } else {
    // Cursor is after the first token (in space params)
    tooltipState.cursorContext = "space";
    // Count which space param we're in
    let charCount = firstToken.length + 1; // +1 for space
    let paramIndex = 0;
    for (let i = 0; i < spaceParams.length; i++) {
      if (cursorPos <= charCount + spaceParams[i].length) {
        paramIndex = i;
        break;
      }
      charCount += spaceParams[i].length + 1; // +1 for space
      paramIndex = i + 1;
    }
    tooltipState.currentIndex = paramIndex;
  }

  // Set description based on context
  if (doc) {
    tooltipState.sig = doc.sig || commandName;
    tooltipState.desc = doc.desc || "";

    // Get contextual suggestions
    tooltipState.suggestions = [];
    if (tooltipState.cursorContext === "colon" && doc.colon) {
      const colonDef = doc.colon[tooltipState.currentIndex];
      if (colonDef?.values) {
        tooltipState.suggestions = colonDef.values;
      }
    } else if (tooltipState.cursorContext === "space" && doc.params) {
      const paramDef = doc.params[tooltipState.currentIndex];
      if (paramDef?.values) {
        tooltipState.suggestions = paramDef.values;
      }
    }
  } else {
    tooltipState.sig = null;
    tooltipState.desc = null;
    tooltipState.suggestions = [];
  }
}

// 💡 Paint the tooltip overlay
function paintTooltip($, inputText) {
  const { ink, screen } = $;
  const doc = tooltipState.command;
  if (!doc) return;

  // Get cursor position to draw ghost text inline
  const cursorPos = $.system.prompt.input.prompt?.pos?.(undefined, true);
  if (!cursorPos) return;

  const cursorX = cursorPos.x ?? 6;
  const cursorY = cursorPos.y ?? 10;

  // Parse what's already typed to show remaining hint
  const typed = inputText.trim();
  const typedParts = typed.split(/[\s]+/);
  const firstToken = typedParts[0] || "";
  const colonParts = firstToken.split(":");
  const numColonParams = colonParts.length - 1; // -1 for command itself
  const numSpaceParams = typedParts.length - 1; // -1 for first token

  let ghostText = "";
  let descText = doc.desc || "";

  // Build ghost text from remaining colon params first
  if (doc.colon && doc.colon.length > 0) {
    const remainingColon = doc.colon.slice(numColonParams);
    if (remainingColon.length > 0) {
      ghostText = remainingColon.map(p => {
        const name = p.name || "?";
        return `:${name}`;
      }).join("");
    }
  }

  // Then add remaining space params
  if (doc.params && doc.params.length > 0) {
    const remainingParams = doc.params.slice(numSpaceParams);
    if (remainingParams.length > 0) {
      const spaceHints = remainingParams.map(p => {
        if (p.values) {
          return p.values.slice(0, 3).join("|");
        }
        return p.required ? `<${p.name}>` : `[${p.name}]`;
      }).join(" ");
      ghostText += (ghostText ? " " : "") + spaceHints;
    }
  }

  // Draw ghost text inline after cursor (faded)
  if (ghostText) {
    ink($.dark ? [100, 180, 255, 80] : [0, 100, 200, 80]).write(ghostText, {
      x: cursorX + 2,
      y: cursorY,
    }, undefined, undefined, false, "MatrixChunky8");
  }

  // Draw description as subtle underline text below
  if (descText) {
    ink($.dark ? [120, 120, 140, 120] : [80, 80, 100, 120]).write(descText, {
      x: 6,
      y: cursorY + 10,
    }, undefined, undefined, false, "MatrixChunky8");
  }

  // Show current param suggestions inline (highlighted options)
  if (tooltipState.suggestions.length > 0 && tooltipState.currentIndex >= 0) {
    const suggestionsText = tooltipState.suggestions.slice(0, 6).join(" · ");
    ink($.dark ? [100, 255, 150, 150] : [0, 150, 50, 150]).write(suggestionsText, {
      x: cursorX + 2,
      y: cursorY + 10,
    }, undefined, undefined, false, "MatrixChunky8");
  }
}

// Tezos wallet connection state
let tezosWalletAddress = null;
let tezosWalletBalance = null; // Balance in tez
let tezosNetwork = "mainnet"; // "ghostnet" or "mainnet"
let tezosBalanceLastFetch = 0; // Timestamp of last balance fetch
let tezosDomainName = null; // Resolved .tez domain (if any)

// Fetch Tezos wallet balance from RPC
async function fetchTezosBalance(address, network = "mainnet") {
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

// Resolve .tez domain for an address using TzKT API (more reliable)
// NOTE: Always use mainnet API since .tez domains are only registered on mainnet
async function fetchTezosDomain(address, _network = "mainnet") {
  try {
    // Always use mainnet TzKT API - .tez domains only exist on mainnet
    const apiBase = "https://api.tzkt.io";

    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 5000); // 5s timeout

    // Look for domains where this address is set AND has reverse=true (primary domain)
    const res = await fetch(
      `${apiBase}/v1/domains?address=${address}&reverse=true&select=name`,
      { signal: controller.signal }
    );

    clearTimeout(timeout);

    if (res.ok) {
      const data = await res.json();
      // Returns array of domains, take the first one with reverse=true
      if (data && data.length > 0 && data[0].name) {
        const domain = data[0].name;
        console.log(`🔷 Resolved .tez domain: ${domain}`);
        return domain;
      }
    }
  } catch (e) {
    // Silently fail - .tez domain resolution is optional
    if (e.name !== 'AbortError') {
      console.log("🔷 .tez domain lookup skipped (API unavailable)");
    }
  }
  return null;
}

// 🎹 QWERTY keyboard musical mapping (notepat-style two octaves)
// Maps keyboard keys to semitone offsets (0-23 for two octaves) and pan positions
// Left octave (lower): c, c#, d, d#, e, f, f#, g, g#, a, a#, b
// Right octave (upper): +c, +c#, +d, +d#, +e, +f, +f#, +g, +g#, +a, +a#, +b
const QWERTY_MUSIC_MAP = {
  // Lower octave (left side - pan left)
  c: { semitone: 0, pan: -0.7 },    // C
  v: { semitone: 1, pan: -0.5 },    // C#
  d: { semitone: 2, pan: -0.6 },    // D
  s: { semitone: 3, pan: -0.4 },    // D#
  e: { semitone: 4, pan: -0.5 },    // E
  f: { semitone: 5, pan: -0.4 },    // F
  w: { semitone: 6, pan: -0.3 },    // F#
  g: { semitone: 7, pan: -0.3 },    // G
  r: { semitone: 8, pan: -0.2 },    // G#
  a: { semitone: 9, pan: -0.2 },    // A
  q: { semitone: 10, pan: -0.1 },   // A#
  b: { semitone: 11, pan: -0.1 },   // B
  // Upper octave (right side - pan right)
  h: { semitone: 12, pan: 0.1 },    // +C
  t: { semitone: 13, pan: 0.1 },    // +C#
  i: { semitone: 14, pan: 0.2 },    // +D
  y: { semitone: 15, pan: 0.2 },    // +D#
  j: { semitone: 16, pan: 0.3 },    // +E
  k: { semitone: 17, pan: 0.4 },    // +F
  u: { semitone: 18, pan: 0.3 },    // +F#
  l: { semitone: 19, pan: 0.5 },    // +G
  o: { semitone: 20, pan: 0.4 },    // +G#
  m: { semitone: 21, pan: 0.6 },    // +A
  p: { semitone: 22, pan: 0.5 },    // +A#
  n: { semitone: 23, pan: 0.7 },    // +B
  // Extra keys (z, x for lower notes, ;' for higher)
  z: { semitone: -2, pan: -0.9 },   // A# (below)
  x: { semitone: -1, pan: -0.8 },   // B (below)
};

// Convert semitone offset to pitch ratio (for sample playback)
// Base pitch is 1.0, each semitone is 2^(1/12) ratio
function semitoneToPlaybackRate(semitone) {
  // Center around middle of keyboard (semitone 11-12)
  const centerSemitone = 11;
  const offset = semitone - centerSemitone;
  return Math.pow(2, offset / 12);
}

// Track last pressed key for musical keyboard sounds
let lastPressedKey = null;

// 🥾 Boot
async function boot({
  glaze,
  api,
  gizmo,
  net,
  system,
  pieceCount,
  send,
  needsPaint,
  ui,
  screen,
  user,
  handle,
  params,
  colon,
  notice,
  dark,
  store,
  // code,
  net: { socket },
  vscode,
}) {
  promptSend = send;
  promptNeedsPaint = needsPaint;
  cachedGizmo = gizmo; // Cache gizmo for use in act() function
  if (store["prompt:theme"]) theme = store["prompt:theme"];
  if (store["prompt:lang"]) lang = store["prompt:lang"];
  if (colon?.includes("serious")) theme = "serious";
  if (colon?.includes("neo")) theme = "neo";
  if (dark) glaze({ on: true });
  // if (vscode) console.log("🟣 Running `prompt` in the VSCode extension.");

  net.requestDocs().then((d) => {
    autocompletions = { ...d.pieces, ...d.prompts };
    tooltipDocs = { ...d.pieces, ...d.prompts }; // Include both pieces and prompts for tooltips
    // Remove hidden autocompleteions.
    keys(autocompletions).forEach((key) => {
      if (autocompletions[key].hidden) delete autocompletions[key];
    });
  });

  server = socket((id, type, content) => {
    // console.log("🧦 Got message:", id, type, content);
  });

  // Fetch handle count.
  fetch("/handle?count=true")
    .then((res) => res.json())
    .then((data) => {
      handles = data.handles;
    })
    .catch((err) => {
      console.warn("💁 Could not get handle count.");
    });

  // Fetch git commit/version status with long-poll for deploy detection
  const fetchVersion = async () => {
    try {
      // On localhost, fetch GitHub directly to show latest remote commit
      if (location.hostname === "localhost" || location.hostname === "127.0.0.1") {
        const ghRes = await fetch(
          "https://api.github.com/repos/whistlegraph/aesthetic-computer/commits?per_page=10",
          { headers: { Accept: "application/vnd.github.v3+json" } }
        );
        if (ghRes.ok) {
          const commits = await ghRes.json();
          const currentHash = commits[0]?.sha?.slice(0, 7);
          versionInfo = {
            deployed: currentHash || "dev",
            latest: currentHash,
            status: "local",
          };
          // Extract commits for uniticker (local dev mode)
          recentCommits = commits.map(c => ({
            hash: c.sha.slice(0, 7),
            message: c.commit?.message?.split("\n")[0]?.slice(0, 60) || "no message",
            author: c.commit?.author?.name || c.author?.login || "unknown",
            date: c.commit?.author?.date,
          }));
        } else {
          versionInfo = { deployed: "dev", status: "local" };
          recentCommits = [];
        }
        needsPaint();
        return;
      }
      const res = await fetch("/api/version");
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      versionInfo = await res.json();
      // Store recent commits from the version API
      recentCommits = versionInfo.recentCommits || [];
      needsPaint();
    } catch (e) {
      console.warn("📦 Could not fetch version info:", e);
    }
  };
  fetchVersion();

  // Long-poll loop: detect new deployments within ~5 seconds
  const startVersionPoll = async () => {
    if (location.hostname === "localhost" || location.hostname === "127.0.0.1") {
      // Local dev: just poll every 60 seconds
      setInterval(fetchVersion, 60 * 1000);
      return;
    }
    const poll = async () => {
      while (true) {
        try {
          if (!versionInfo?.deployed) {
            await new Promise(r => setTimeout(r, 5000));
            continue;
          }
          versionPollController = new AbortController();
          const res = await fetch(
            `/api/version?current=${versionInfo.deployed}`,
            { signal: versionPollController.signal }
          );
          if (!res.ok) throw new Error(`HTTP ${res.status}`);
          const data = await res.json();
          if (data.changed === false) {
            // Same version — loop again immediately (server already waited ~4s)
            continue;
          }
          // New deployment detected! Update version info
          updateAvailable = true;
          versionInfo = data;
          recentCommits = data.recentCommits || [];
          needsPaint();
          console.log("📦 New deployment detected:", data.deployed);
          break; // Stop polling — update is available
        } catch (e) {
          if (e.name === "AbortError") break;
          console.warn("📦 Version poll error:", e);
          await new Promise(r => setTimeout(r, 10000)); // Back off on error
        }
      }
    };
    poll();
  };
  startVersionPoll();

  // Clear handle colors cache on each boot so edits are picked up immediately.
  handleColorsCache.clear();

  // Boot starfield with a clear backdrop.
  starfield.boot(api, { stars: 128 });
  starfield.wipe(false);

  // TODO: How could I not keep reloading these sounds?
  //       Are they already cached?
  net
    .preload("startup")
    .then((sfx) => (startupSfx = sfx))
    .catch((err) => console.warn(err)); // Load startup
  net
    .preload("compkey")
    .then((sfx) => (keyboardSfx = sfx))
    .catch((err) => console.warn(err)); // and key sounds.

  // � Initialize @handle autocomplete
  handleAutocomplete = createHandleAutocomplete();

  // �📦 Load product images (DISABLED for now)
  await products.boot(api);

  // Create login & signup buttons (skip when not in default theme).
  if (!user && theme === "default") {
    login = new ui.TextButton(currentLang().login, { center: "xy", screen });
    login.stickyScrubbing = true; // Prevent drag-between-button behavior
    signup = new ui.TextButton(currentLang().signup, { center: "xy", screen });
    signup.stickyScrubbing = true; // Prevent drag-between-button behavior
    positionWelcomeButtons(screen, net.iframe);
  }

  if (user && theme === "default") {
    // console.log("User:", user);
    const hand = handle();
    const btnPos = { center: "xy", screen };
    if (hand) {
      profileAction = "profile";
      profile = new ui.TextButton(hand, btnPos);
      profile.stickyScrubbing = true;

      // Apply colored handle asynchronously (update in-place to avoid flicker)
      fetchHandleColors(hand).then(colors => {
        if (colors) {
          profile.replaceLabel(colorizeHandle(hand, colors));
          needsPaint();
        }
      });
    } else if (!user.email_verified) {
      profile = new ui.TextButton(currentLang().resendEmail, btnPos);
      profile.stickyScrubbing = true; // Prevent drag-between-button behavior
      profileAction = "resend-verification";
      ellipsisTicker = new gizmo.EllipsisTicker();
      fetchUserAPI = api;
      fetchUser();
    } else if (user.email_verified) {
      profileAction = "set-handle";
      profile = new ui.TextButton("Create handle", btnPos);
      profile.stickyScrubbing = true; // Prevent drag-between-button behavior
    }
  }

  // Only if prompt is set to recall conversations.
  if (
    !system.prompt.convo.messages ||
    system.prompt.convo.messages?.length === 0
  ) {
    // Check if we'll be activating the prompt later to avoid showing MOTD when focused
    const willActivate =
      (pieceCount > 0 && !store["prompt:splash"] && !net.devReload) ||
      (vscode && pieceCount === 0);

    if (theme === "default") {
      // Fetch the MOTD to display above login/signup buttons
      if (!params[0]) makeMotd({ ...api, notice });

      // Fetch all content (kidlisp, painting, tape) for content ticker
      // Always fetch content items, even when params exist (prefilled text)
      fetchContentItems(api);
      fetchClockChatMessages(); // Fetch Laer-Klokken clock chat messages
    }

    if (pieceCount === 0 || store["prompt:splash"] === true) {
      // Initial boot setup
    } else {
      firstActivation = false; // Assume we've activated if returning from
      //                          elsewhere.
    }
    system.prompt.input.showButton(api, {
      nocopy: true,
      nopaste: false, // Always show paste button
    });
  }
  // Handle params - content is already decoded by parse.mjs
  if (params[0]) {
    // Check for !autorun flag (last param) - auto-execute the command
    // Also handles single-param case from prompt~ routing where ~!autorun is a suffix
    let hasAutorun = params[params.length - 1] === "!autorun";
    let effectiveParams = hasAutorun ? params.slice(0, -1) : params;
    let text = effectiveParams.join(" "); // Already decoded, just join if multiple params

    // Handle single-param with ~!autorun suffix (e.g. from mo.mjs / merry.mjs jump)
    if (!hasAutorun && text.endsWith("~!autorun")) {
      hasAutorun = true;
      text = text.slice(0, -"~!autorun".length);
    }

    // Only show text visually if not autorunning — prevents command text flash
    // when routing through prompt via mo.mjs / merry.mjs / merryo.mjs
    if (!hasAutorun) {
      system.prompt.input.text = text;
      system.prompt.input.runnable = true;
      system.prompt.input.addUserText(text);
      system.prompt.input.snap();
      send({ type: "keyboard:text:replace", content: { text } });
    }

    activated({ ...api, params: effectiveParams }, true);
    system.prompt.input.canType = true;
    send({ type: "keyboard:unlock" });
    send({ type: "keyboard:open" }); // Necessary for desktop.

    // If !autorun was specified, schedule execution via sim function
    if (hasAutorun) {
      pendingAutorun = { text };
    }

    // Ensure text and cursor position persist after any system initialization
    if (!hasAutorun) {
      setTimeout(() => {
        if (system.prompt.input.text !== text) {
          system.prompt.input.text = text;
          send({ type: "keyboard:text:replace", content: { text } });
        }
        // Always ensure cursor is at the end
        system.prompt.input.snap();
      }, 100);
    }
  } else {
    system.prompt.input.text = "";
  }

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (
    (pieceCount > 0 && !store["prompt:splash"] && !net.devReload) ||
    (vscode && pieceCount === 0)
  ) {
    // Check if we're coming from chat via labelBack - if so, don't activate
    const labelBackSource =
      typeof window !== "undefined" && window.safeSessionStorageGet
        ? window.safeSessionStorageGet("aesthetic-labelBack-source")
        : null;
    const isFromChatLabelBack = labelBackSource === "chat";

    // Clear the labelBack source since we've checked it
    if (labelBackSource && typeof window !== "undefined" && window.safeSessionStorageRemove) {
      window.safeSessionStorageRemove("aesthetic-labelBack-source");
    }

    if (vscode && pieceCount === 0) firstActivation = false;

    // Only activate if NOT coming from chat via labelBack
    if (!isFromChatLabelBack) {
      // system.prompt.input.enter.btn.disabled = true; // Disable button.
      // system.prompt.input.inputStarted = true;
      // 🍫 Create a pleasurable blinking cursor delay.
      // system.prompt.input.showBlink = false;
      // setTimeout(() => (system.prompt.input.showBlink = true), 100);

      // Clear any latent text before activating to prevent MOTD showing when focused
      // but only if we don't have params (which means we're not coming from backspace navigation)
      if (!params[0]) {
        system.prompt.input.text = "";
      }

      activated({ ...api, params }, true);
      system.prompt.input.canType = true;
      send({ type: "keyboard:unlock" });
      send({ type: "keyboard:open" }); // Necessary for desktop.

      // Force a repaint when returning from another piece (fixes blank screen on backspace from kidlisp)
      needsPaint();
    }
  }

  delete store["prompt:splash"];
}

// 🛑 Halt: (Intercept input and route it to commands.)
async function halt($, text) {
  const {
    api,
    broadcast,
    notice,
    handle,
    load,
    download,
    darkMode,
    text: { capitalize },
    num,
    store,
    connect,
    bgm,
    needsPaint,
    leaving,
    system,
    gizmo,
    screen,
    painting,
    net,
    ui,
    jump,
    beep,
    user,
    upload,
    code,
    send,
    help,
    zip,
    print,
    mug,
    mint,
    rec,
    sound,
    canShare,
    debug,
    clock,
    preloadPieces,
  } = $;

  const stopMerryPipeline = ({
    reason = "manual",
    jumpAfter = true,
    jumpTarget = "prompt",
    cutTape = true,
  } = {}) => {
    try {
      send({ type: "url-freeze", content: { freeze: false } });
    } catch (e) {
      /* ignore */
    }

    const merryState = system.merry;
    if (!merryState) {
      return { stopped: false, wasTaping: false };
    }

    console.log(`🎄 Merry stop requested (${reason})`);
    merryState.running = false;

    if (merryState.paintInterval != null) {
      clearInterval(merryState.paintInterval);
      merryState.paintInterval = null;
    }
    const wasTaping = merryState.isTaping;

  delete system.merry;

    if (wasTaping && cutTape) {
      rec.cut(() => {
        try {
          rec.present();
        } catch (err) {
          console.warn("🎄📼 Unable to present recording after merry stop", err);
        }

        if (jumpAfter && jumpTarget) {
          rec.videoOnLeave = false;
          jump(jumpTarget);
        }
      });
    } else if (jumpAfter && jumpTarget) {
      jump(jumpTarget);
    }

    return { stopped: true, wasTaping };
  };

  system.stopMerryPipeline = stopMerryPipeline;

  const activateMerry = async (
    pieceParams,
    { markAsTaping = false, flashOnSuccess = true, loop = false, originalCommand = "", fadeDuration = 0 } = {}
  ) => {
    if (!pieceParams || pieceParams.length === 0) {
      flashColor = [255, 0, 0];
      makeFlash($);
      notice("MERRY NEEDS PIECES", ["yellow", "red"]);
      return false;
    }

    stopMerryPipeline({ reason: "restart", jumpAfter: false, cutTape: false });

    const defaultDuration = 5;
    const pipeline = [];

    pieceParams.forEach((raw) => {
      const param = raw.trim();
      if (!param) return;

      let piece = param;
      let duration = defaultDuration;

      const colonParts = param.split(":");
      if (colonParts.length > 1) {
        const parsed = parseFloat(colonParts[1]);
        if (!isNaN(parsed) && parsed > 0) {
          duration = parsed;
        }
        piece = colonParts[0];
      } else {
        const hyphenIndex = param.indexOf("-");
        if (hyphenIndex > 0) {
          const prefix = param.slice(0, hyphenIndex);
          const remainder = param.slice(hyphenIndex + 1);
          const parsed = parseFloat(prefix);
          if (!isNaN(parsed) && parsed > 0 && remainder) {
            duration = parsed;
            piece = remainder;
          }
        }
      }

      if (piece && !isNaN(duration) && duration > 0) {
        pipeline.push({ piece, duration });
      }
    });

    if (pipeline.length === 0) {
      flashColor = [255, 0, 0];
      makeFlash($);
      notice("MERRY NEEDS PIECES", ["yellow", "red"]);
      return false;
    }

    // 🎄⏰ Resync clock with UTC server before starting pipeline
    if (clock?.resync) {
      clock.resync();
      console.log("🎄⏰ Clock resynced with UTC server");
    }

    // 🎄📦 Preload all piece modules before starting (prevents network latency during transitions)
    const pieceNames = pipeline.map(p => p.piece);
    if (preloadPieces) {
      console.log("🎄📦 Preloading pieces:", pieceNames);
      await preloadPieces(pieceNames);
    }

    // Helper function to get UTC-synced time
    const getUTCTime = () => clock?.time?.()?.getTime?.() || Date.now();

    const totalDuration = pipeline.reduce((sum, p) => sum + p.duration, 0);
    
    // 🎄⏰ Calculate UTC-aligned start for synced playback across devices
    // This allows multiple merryo instances to stay in sync by aligning to
    // the same cycle boundary in UTC time
    const now = getUTCTime();
    const totalDurationMs = totalDuration * 1000;
    
    // Calculate how far into the current cycle we are (based on UTC epoch)
    // This means all devices will calculate the same position in the cycle
    const cyclePosition = now % totalDurationMs;
    
    // Calculate the start of the current cycle (when it began in UTC)
    const cycleStartTime = now - cyclePosition;
    
    // Calculate which piece we should be on and how far into it
    let accumulatedDuration = 0;
    let startIndex = 0;
    let pieceElapsed = 0;
    
    for (let i = 0; i < pipeline.length; i++) {
      const pieceDurationMs = pipeline[i].duration * 1000;
      if (cyclePosition < accumulatedDuration + pieceDurationMs) {
        startIndex = i;
        pieceElapsed = cyclePosition - accumulatedDuration;
        break;
      }
      accumulatedDuration += pieceDurationMs;
    }
    
    console.log(`🎄⏰ UTC sync: cycle position ${(cyclePosition/1000).toFixed(2)}s, starting at piece ${startIndex} (${pipeline[startIndex]?.piece})`);

    system.merry = {
      pipeline,
      currentIndex: startIndex,
      running: true,
      totalDuration,
      elapsedTime: cyclePosition / 1000, // Start with elapsed time from cycle position
      progress: cyclePosition / totalDurationMs,
      pieceProgress: pieceElapsed / (pipeline[startIndex]?.duration * 1000 || 1),
      startTime: cycleStartTime, // Use calculated cycle start for consistent timing
      currentPieceStart: cycleStartTime + accumulatedDuration, // When current piece started
      isTaping: markAsTaping,
      paintInterval: null,
      loop,
      cycleCount: 0,
      originalCommand, // Store for backspace editing
      getUTCTime, // Store the helper for use in setTimeout calculations
      fadeDuration, // Crossfade duration in seconds (0 = no fade)
    };

    console.log("🎄 Merry pipeline:", pipeline, `total: ${totalDuration}s`);

    const merryToneSequence = [392, 494, 523, 587, 659, 784, 880];

    const startMerryPaintTicker = () => {
      const merryState = system.merry;
      if (!merryState || !merryState.running) {
        return;
      }

      // Clear any existing ticker to avoid duplicates
      if (merryState.paintInterval != null) {
        clearInterval(merryState.paintInterval);
        merryState.paintInterval = null;
      }

      const tick = () => {
        const active = system.merry;
        if (!active || !active.running) {
          if (merryState.paintInterval != null) {
            clearInterval(merryState.paintInterval);
            merryState.paintInterval = null;
          }
          return;
        }
        needsPaint();
      };

      // Trigger an immediate paint so progress appears without delay
      needsPaint();

      const intervalHandle = setInterval(tick, 1000 / 30); // ~33ms cadence
      merryState.paintInterval = intervalHandle;
    };

    const startMerryPiece = (index) => {
      if (!system.merry || !system.merry.running) {
        return;
      }

      if (index >= pipeline.length) {
        const merryState = system.merry;
        if (merryState?.loop && merryState.running) {
          merryState.cycleCount = (merryState.cycleCount || 0) + 1;
          merryState.elapsedTime = 0;
          merryState.progress = 0;
          merryState.pieceProgress = 0;
          merryState.currentPieceStart = null;
          merryState.startTime = getUTCTime();
          needsPaint();
          startMerryPiece(0);
          return;
        }

        console.log("🎄 Merry pipeline complete!");
        const wasTaping = merryState?.isTaping;
        stopMerryPipeline({
          reason: "complete",
          jumpAfter: true,
          jumpTarget: wasTaping ? "video" : "prompt",
          cutTape: Boolean(wasTaping),
        });
        return;
      }

      const { piece, duration } = pipeline[index];
      
      // Calculate remaining time for this piece based on UTC-synced timing
      // On first call, we may be starting mid-piece due to UTC sync
      const now = getUTCTime();
      let remainingDuration = duration * 1000;
      
      if (system.merry && system.merry.currentPieceStart) {
        // Calculate how much time has already passed in this piece
        const pieceElapsed = now - system.merry.currentPieceStart;
        remainingDuration = Math.max(0, (duration * 1000) - pieceElapsed);
      }
      
      console.log(`🎄 Merry: Playing ${piece} for ${duration}s (${index + 1}/${pipeline.length}), remaining: ${(remainingDuration/1000).toFixed(2)}s`);

      if (system.merry) {
        // Only reset piece start if we're not continuing from UTC sync
        if (!system.merry.currentPieceStart || index !== system.merry.currentIndex) {
          system.merry.currentPieceStart = getUTCTime();
          system.merry.pieceProgress = 0;
        }
        system.merry.currentIndex = index;
        if (index === 0 && !system.merry.startTime) {
          system.merry.startTime = getUTCTime();
        }
      }

      startMerryPaintTicker();

      // 🎄 Trigger a visual flash on the progress bar when transitioning (instead of sound)
      // Only flash if we're actually transitioning (not on initial UTC-synced start)
      if (system.merry && remainingDuration === duration * 1000) {
        system.merry.transitionFlash = {
          active: true,
          startTime: getUTCTime(),
          duration: 150, // Flash duration in ms
        };
      }

      // In fade mode, the merry-fade host piece handles all rendering via
      // paintApi.kidlisp() — no jump() needed. The host reads system.merry
      // timing state each frame and crossfades between $code pieces.
      if (system.merry.fadeDuration > 0) {
        // Only set up the next-piece timer; don't jump.
        setTimeout(() => {
          if (system.merry && system.merry.running) {
            system.merry.elapsedTime += duration;
            system.merry.currentPieceStart = getUTCTime();
            startMerryPiece(index + 1);
          }
        }, remainingDuration);
      } else {
        setTimeout(() => {
          if (system.merry && system.merry.running) {
            system.merry.elapsedTime += duration;
            // Reset piece start for next piece (it will start fresh)
            system.merry.currentPieceStart = getUTCTime();
            startMerryPiece(index + 1);
          }
        }, remainingDuration);

        jump(piece);
      }
    };

    // Start at the calculated index (may be > 0 due to UTC sync)
    if (fadeDuration > 0) {
      // In fade mode, jump to the merry-fade host piece once.
      // It reads system.merry state each frame and renders $code pieces.
      jump("merry-fade");
      startMerryPaintTicker();
    } else {
      startMerryPiece(startIndex);
    }

    if (flashOnSuccess) {
      flashColor = [0, 255, 0];
      makeFlash($);
    }

    return true;
  };
  activeCompletions.length = 0; // Reset activeCompletions on every halt.
  motdController?.abort(); // Abort any motd update.

  // Default to unfreezing URL updates when a new command is entered.
  // (Merry commands will re-freeze below.)
  try {
    send({ type: "url-freeze", content: { freeze: false } });
  } catch (e) {
    /* ignore */
  }

  // Roughly parse out the text (could also do a full `parse` here.)
  const tokens = text.split(" ");
  const slug = tokens[0]; // Note: Includes colon params.
  const slugWithoutColon = slug.split(":")[0];
  const params = tokens.slice(1);
  const input = $.system.prompt.input; // Reference to the TextInput.

  const openExternalFromIframe = (url) => {
    if (!net.iframe) return false;
    send({ type: "post-to-parent", content: { type: "openExternal", url } });
    return true;
  };

  const siteBase = debug && typeof self !== "undefined" && self.location
    ? self.location.origin
    : "https://aesthetic.computer";

  const toAbsoluteSiteUrl = (pathOrUrl) => {
    if (/^https?:\/\//.test(pathOrUrl)) return pathOrUrl;
    const normalized = pathOrUrl.startsWith("/") ? pathOrUrl : `/${pathOrUrl}`;
    return `${siteBase}${normalized}`;
  };

  // 🖥️ Open Electron DevTools (docked at bottom, JS console).
  if (text.trim() === "~") {
    send({ type: "open-devtools" });
    return true;
  }

  // 🕸️ Custom URL routing.
  if (slug.startsWith("/")) {
    jump(`${siteBase}${slug}`);
    return true;
  } else if (slug === "shop") {
    console.log(slug);

    const openShopPath = (path) => {
      if (openExternalFromIframe(toAbsoluteSiteUrl(path))) return;
      jump(path);
    };

    if (params.length > 0) {
      if (shop.indexOf(params[0]) > -1) {
        // Use /shop~code pattern for signed product codes
        openShopPath("/shop~" + params[0]);
      } else {
        openShopPath("/shop/" + params.join("/"));
      }
    } else {
      openShopPath("/shop");
    }
    return true;
  } else if (slug.startsWith("shop")) {
    const target = params[0] ? "/" + params[0] : "/shop";
    if (openExternalFromIframe(toAbsoluteSiteUrl(target))) return true;
    jump(target);
    return true;
  } else if (shop.indexOf(slug) > -1) {
    // Use /shop~code pattern for signed product codes
    if (openExternalFromIframe(toAbsoluteSiteUrl("/shop~" + slug))) return true;
    jump("/shop~" + slug);
    return true;
  } else if (slug === "at") {
    // Jump to ATProto user pages landing
    jump(`https://at.aesthetic.computer`);
    return true;
  } else if (slug === "oven") {
    // 🔥 Jump to Oven dashboard
    jump(`https://oven.aesthetic.computer`);
    return true;
  } else if (slug === "give") {
    // 🎁 Jump to Give page (opens in new window)
    // If user is logged in, prefill their email for easier Stripe checkout
    let giveUrl = `https://give.aesthetic.computer`;
    if (user?.email) {
      giveUrl += `?email=${encodeURIComponent(user.email)}`;
    }
    jump(`out:${giveUrl}`);
    return true;
  } else if (slug === "news" || slug === "nws") {
    // 📰 Jump to News site
    jump(`https://news.aesthetic.computer`);
    return true;
  } else if (slug === "desktop" || slug === "app" || slug === "electron") {
    // 💻 Jump to Desktop app download page
    jump("desktop");
    return true;
  } else if (slug === "r8dio:web" || slug === "r8Dio:web" || slug === "radio:web") {
    // 📻 Jump to R8dio.dk website (supports both r8dio and r8Dio)
    jump(`https://r8dio.dk/lyt-live/`);
    return true;
  } else if (
    slugWithoutColon === "merry" ||
    slugWithoutColon === "merryo" ||
    slugWithoutColon.startsWith("merryo.") ||
    slugWithoutColon.startsWith("mo.") ||
    slugWithoutColon === "mo" ||
    /^mo\d+$/.test(slugWithoutColon)
  ) {
    // Freeze URL updates while merry runs so piece swaps don't rewrite the address bar.
    try {
      const loc = typeof self !== "undefined" && self.location ? self.location : null;
      const freezePath = loc
        ? loc.pathname + loc.search + loc.hash
        : "";
      send({ type: "url-freeze", content: { freeze: true, path: freezePath } });
    } catch (e) {
      /* ignore */
    }

    // 🎄 Merry / Merryo with optional uniform timing shorthand
    // Examples:
    //   merry tone clock        -> 5s each
    //   merryo tone clock       -> 5s each, loops
    //   merryo.1 a b c          -> 0.1s each, loops (shorthand for merryo 0.1-a 0.1-b 0.1-c)
    //   mo.1 a b c              -> same as merryo.1
    //   mo.05 a b c             -> 0.05s (50ms) each, loops
    //   mo1 a b c               -> 1s each, loops
    //   mo10 a b c              -> 10s each, loops
    const isMoIntegerSeconds = /^mo\d+$/.test(slugWithoutColon);
    let loop =
      slugWithoutColon === "merryo" ||
      slugWithoutColon.startsWith("merryo.") ||
      slugWithoutColon.startsWith("mo.") ||
      slugWithoutColon === "mo" ||
      isMoIntegerSeconds;
    let uniformDuration = null;
    
    // Parse uniform duration from slug like "merryo.1" or "mo.05"
    if (slugWithoutColon.startsWith("merryo.")) {
      const timingPart = slugWithoutColon.slice(6); // ".1" or ".05"
      const parsed = parseFloat("0" + timingPart);
      if (!isNaN(parsed) && parsed > 0) {
        uniformDuration = parsed;
      }
    } else if (slugWithoutColon.startsWith("mo.")) {
      const timingPart = slugWithoutColon.slice(2); // ".1" or ".05"
      const parsed = parseFloat("0" + timingPart);
      if (!isNaN(parsed) && parsed > 0) {
        uniformDuration = parsed;
      }
    } else if (isMoIntegerSeconds) {
      const seconds = parseInt(slugWithoutColon.slice(2), 10);
      if (Number.isFinite(seconds) && seconds > 0) {
        uniformDuration = seconds;
      }
    }
    
    // 🎄✨ Parse :fade option from colon params (e.g., merryo:fade, mo1:fade.5)
    const slugColonParts = slug.split(":").slice(1); // Everything after the first colon segment
    let fadeDuration = 0;
    const nonFadeColonParts = [];
    for (const part of slugColonParts) {
      const fadeMatch = part.match(/^fade(\.\d+)?$/);
      if (fadeMatch) {
        fadeDuration = fadeMatch[1] ? parseFloat("0" + fadeMatch[1]) : 0.3;
      } else {
        nonFadeColonParts.push(part);
      }
    }

    // Apply uniform duration to all params if specified
    let processedParams = params;
    if (uniformDuration !== null && params.length > 0) {
      processedParams = params.map(piece => `${uniformDuration}-${piece}`);
    }

    console.log(
      `🎄 ${slugWithoutColon.toUpperCase()} command received with params:`,
      processedParams,
      uniformDuration ? `(uniform: ${uniformDuration}s)` : "",
      fadeDuration ? `(fade: ${fadeDuration}s)` : "",
    );
    activateMerry(processedParams, {
      markAsTaping: false,
      flashOnSuccess: true,
      loop,
      originalCommand: text, // Store the full original text (already has proper format)
      fadeDuration,
    });
    return true;
  } else if (slug.startsWith("!") && slug.length > 1) {
    console.log("📼 Tape code detected:", slug, "params:", params);
    // Route to video piece to handle tape playback
    // Use ~ separator for params instead of space
    jump("video~" + text);
    return true;
  } else if (
    slug === "tape" ||
    slug === "tape:add" ||
    slug === "tape:tt" ||
    slug === "tape:nomic" ||
    slug === "tape:mic" ||
    slug === "tape:neat" ||
    slug === "tapem"
  ) {
    const playbackParam = params[0];

    // 📼 Check if this is a playback command (e.g., "tape !JyK")
    if (playbackParam && playbackParam.startsWith('!')) {
      console.log("📼 Tape playback mode detected, routing to video piece");
      // Use ~ separator for params instead of space
      jump("video~" + params.join('~'));
      return true;
    }

    // 📼 Start taping (recording mode).
    // Note: Right now, tapes get saved on refresh but can't be concatenated to,
    // and they start over when using `tape`.
    // This could eventually be replaced by a system that makes a new
    // video for every clip and then renders or stitches them together
    // in the end, where `video` can evolve into more of a clip editor.
    // Each of these clips can be stored in indexedDB more easily and played
    // back or be rearranged.
    // 23.09.16.18.01
    if (slug !== "tape:add") rec.slate(); // Start a recording over.
    const defaultDuration = 7;
    const tapePromise = new Promise((resolve, reject) => {
      tapePromiseResolve = resolve;
      tapePromiseReject = reject;
    });

    let nomic;
    if (slug === "tape" || slug === "tape:tt") {
      nomic = iOS || Android ? false : true;
      if (params[0] === "baktok" || params[1] == "baktok") {
        nomic = false;
      } else {
        nomic = true;
      }
    } else if (slug === "tape:nomic" || slug === "tape:neat") {
      nomic = true;
    } else if (slug === "tape:mic" || slug === "tapem") {
      nomic = false;
    }

    if (!nomic) sound.microphone.connect(); // Connect the mic.
    try {
      if (nomic) {
        console.log("📼 Taping...");
        tapePromiseResolve?.();
      }
      await tapePromise;
      let duration = parseFloat(params[0]);
      let frameMode = false;
  let isTapingMerry = false; // Flag to track if we're recording a merry pipeline
  let jumpTo;
  let merryPieceParams = null;

      // Check if the first parameter ends with 'f' for frame-based recording
      if (params[0] && typeof params[0] === 'string' && params[0].toLowerCase().endsWith('f')) {
        frameMode = true;
        duration = parseFloat(params[0].slice(0, -1)); // Remove the 'f' suffix
        console.log(`🎬 Frame-based recording requested: ${duration} frames`);
      }

      // 🎄 Check if we're taping a merry pipeline anywhere in the params: "tape merry tone:3 clock:5"
      const merryTokenIndex = params.findIndex(
        (param) => param === "merry" || param === "merryo"
      );
      if (merryTokenIndex !== -1) {
        isTapingMerry = true;
        console.log("🎄📼 Taping a merry pipeline detected!");

        const merryToken = params[merryTokenIndex];
        const loopRequested = merryToken === "merryo";

        if (loopRequested) {
          params[merryTokenIndex] = "merry";
          flashColor = [255, 165, 0];
          makeFlash($);
          notice("MERRYO DISABLED WHILE TAPING", ["yellow", "red"]);
        }

        merryPieceParams = params.slice(merryTokenIndex + 1);
        if (!merryPieceParams.length) {
          flashColor = [255, 0, 0];
          makeFlash($);
          notice("MERRY NEEDS PIECES", ["yellow", "red"]);
          return true;
        }

        const defaultMerryDuration = 5;
        let totalMerryDuration = 0;
        merryPieceParams.forEach((param) => {
          const parts = param.split(":");
          const pieceDuration = parts[1] ? parseFloat(parts[1]) : defaultMerryDuration;
          if (!isNaN(pieceDuration) && pieceDuration > 0) {
            totalMerryDuration += pieceDuration;
          }
        });

        if (totalMerryDuration <= 0) {
          totalMerryDuration = defaultMerryDuration * merryPieceParams.length;
        }

        console.log(`🎄📼 Calculated merry total duration: ${totalMerryDuration}s`);
        duration = totalMerryDuration; // Set tape duration to match merry duration

  jumpTo = "merry";
      }

      // Gets picked up on next piece load automatically.
      rec.loadCallback = () => {
        // Capture the KidLisp FPS if available (set by fps function)
        const kidlispFps = (typeof window !== 'undefined' && window.currentKidlispFps) || null;
        console.log(`🎬 Captured KidLisp FPS for recording: ${kidlispFps}`);

        // 🎄 If we're taping a merry, mark it in the merry system
        if (isTapingMerry && system.merry) {
          system.merry.isTaping = true;
          console.log("🎄📼 Marked merry pipeline as being taped");
        }

        // 😶‍🌫️ Running after the `jump` prevents any flicker and starts
        // the recording at the appropriate time.
        rec.rolling(
          {
            type: "video" + (slug === "tape:tt" || jumpTo === "baktok" ? ":tiktok" : ""),
            pieceName: (jumpTo && jumpTo.startsWith("$")) ? "$code" : (jumpTo || "tape"),
            pieceParams: (() => {
              if (jumpTo === "merry") {
                return (merryPieceParams || []).join("~");
              }

              // Exclude the piece name from params to avoid duplication in filename
              const startIndex = isNaN(duration) ? 0 : 1;
              const relevantParams = params.slice(startIndex);
              if (relevantParams.length > 0 && relevantParams[0] === jumpTo) {
                return relevantParams.slice(1).join("~");
              }
              return relevantParams.join("~");
            })(),
            originalCommand: text,
            intendedDuration: isNaN(duration) ? null : duration,
            frameMode: frameMode,
            frameCount: frameMode ? (isNaN(duration) ? 8 : duration) : null,
            kidlispFps: kidlispFps, // Pass the KidLisp framerate
            cleanMode: slug === "tape:neat", // Enable clean mode (no overlays, no progress bar)
            // showTezosStamp: true, // Enable Tezos stamp by default for GIF recordings (DISABLED)
            showTezosStamp: false, // Tezos stamp disabled - set to true to re-enable
            mystery: false
          },
          (time) => {
            if (frameMode) {
              rec.tapeTimerSet(duration || 8, time, true); // Default to 8 frames if no duration specified
            } else {
              rec.tapeTimerSet(duration || defaultDuration, time, false);
            }
          },
        ); // Start recording immediately.
      };

      if (isTapingMerry && merryPieceParams) {
        const merryStarted = activateMerry(merryPieceParams, {
          markAsTaping: true,
          flashOnSuccess: false,
          loop: false,
        });
        if (!merryStarted) {
          return true;
        }
        rec.videoOnLeave = true;
      } else if ((isNaN(duration) || duration === 0) && params[0]?.length > 0) {
        // Handle cases like "tape $code" or "tape f" or "tape 0f"
        if (frameMode && (isNaN(duration) || duration === 0)) {
          duration = 8; // Default to 8 frames for "tape f" or "tape 0f"
        } else if (!frameMode) {
          duration = defaultDuration; // Default to 7 seconds for "tape $code"
        }

        if (!frameMode || !params[0].toLowerCase().endsWith('f')) {
          // Only jump to piece if it's not just a frame count
          // Reconstruct the original kidlisp content without adding tildes
          const originalContent = text.slice(text.indexOf(params[0])); // Get everything after "tape "
          jumpTo = params[0];
          jump(originalContent);
          rec.videoOnLeave = true;
        } else {
          // For "tape f" or "tape 0f", just record the prompt
          jump("prompt");
        }
      } else if (!isTapingMerry) {
        // Find the first non-empty param after duration (params[0])
        const pieceParam = params.slice(1).find(p => p && p.length > 0);
        if (pieceParam) {
          jumpTo = pieceParam;
          // Reconstruct the original content for kidlisp preservation
          const originalContent = text.slice(text.indexOf(pieceParam)); // Get everything after duration
          jump(originalContent);
        } else {
          jump("prompt");
        }
      } else {
        jump("prompt");
      }
      flashColor = [0, 255, 0];
    } catch (err) {
      console.log(err);
      flashColor = [255, 0, 0];
    }
    makeFlash($);
    return true;
    // 📼 Cut a tape early.
  } else if (slug === "tape:cut" || slug === "cut") {
    let cutRes, cutRej;
    const cutPromise = new Promise((res, rej) => {
      cutRes = res;
      cutRej = rej;
    });
    setTimeout(cutRej, 250);
    rec.cut(() => {
      cutRes();
    });
    try {
      await cutPromise;
      jump("video");
      flashColor = [0, 255, 0];
    } catch (err) {
      flashColor = [255, 0, 0];
    }
    makeFlash($);
    // TODO: How can I hold the cursor here...
    return true;
  } else if (slug === "merry:stop" || slug === "stop") {
    // 🎄 Stop the merry pipeline early
    if (system.merry && system.merry.running) {
      console.log("🎄 Merry pipeline stopped!");
      const wasTaping = system.merry.isTaping;
      stopMerryPipeline({
        reason: "manual",
        jumpAfter: true,
        jumpTarget: wasTaping ? "video" : "prompt",
        cutTape: wasTaping,
      });
      flashColor = [0, 255, 0];
    } else {
      flashColor = [255, 0, 0];
      notice("NO MERRY RUNNING", ["yellow", "red"]);
    }
    makeFlash($);
    return true;
  } else if (slug === "me" || slug === "profile") {
    console.log("Logged in?", user);
    if (user) {
      jump("profile");
      return true;
    } else {
      notice("LOG IN OR SIGN UP", ["yellow", "red"]);
      $.system.prompt.input.blank(); // Clear the prompt.
      send({ type: "keyboard:close" });
      return true; //"dont-unlock";
    }
  } else if (slug === "scream") {
    // TODO: Scream additions. 23.12.11.12.53
    // - [] Vocalize all screams / make a sound?
    // - [] Smartly time-synchronize that message for all users by looking ahead?
    // console.log("😱 Screaming...");
    server?.send("scream", params.join(" ") || "Ahh!");
    flashColor = [255, 0, 0];
    makeFlash($);
    return true;
  } else if (slug === "nonotifs") {
    send({
      type: "ios:send",
      content: { type: "notifications", body: false },
    });
    send({ type: "notifications:web", content: { enable: false } });
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "notifs") {
    send({
      type: "ios:send",
      content: { type: "notifications", body: true },
    });
    send({ type: "notifications:web", content: { enable: true } });
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "selfie") {
    jump("camera~me");
    return true;
  } else if (
    slug === "cam" ||
    text.startsWith === "cam " ||
    text.startsWith("cam:")
  ) {
    jump(text.replace("cam", "camera"));
    return true;
  } else if (slug === "camu") {
    jump("camera:under");
    return true;
  } else if (slug === "@maya" + "/sparkle") {
    jump("sparkle");
    return true;
  } else if (slug === "painting:start") {
    system.nopaint.startRecord();
    console.log("🖌️🔴 Now recording:", system.nopaint.record);
    flashColor = [200, 0, 200];
    makeFlash($);
    return true;
  } else if (slug === "print" || slug === "mint" || slug === "mug") {
    progressBar = 0;

    progressTrick = new gizmo.Hourglass(24, {
      completed: () => (progressBar += min(0.5, progressBar + 0.1)),
      autoFlip: true,
    });

    try {
      if (slug === "print") {
        // 🏚️ Print a sticker.
        await print(system.painting, params[0], (p) => (progressBar = p));
      } else if (slug === "mug") {
        // ☕ Preview mug and purchase
        // Usage: mug [code] [color]
        // Examples: mug, mug blue, mug abc, mug abc blue, mug +productcode
        let code = system.painting?.code || store["painting:code"] || "";
        let color = "";
        let paramIndex = 0;

        // Check if first param is a code (with or without # or +)
        if (params[0] && !/^(white|black|blue|pink|orange)$/i.test(params[0])) {
          // Pass through + prefixed product codes as-is
          // For painting codes, use as-is (no # prefix needed)
          code = params[0];
          paramIndex = 1;
        }

        // Parse color (only for painting codes, not product codes)
        if (params[paramIndex] && !code.startsWith("+")) {
          color = params[paramIndex];
        } else if (!code.startsWith("+")) {
          color = "white"; // Default color only for painting codes
        }

        // Jump to mug piece for preview
        // Use ~ separator for params instead of space
        const mugJump = ["mug", code, color].filter(Boolean).join("~");
        $.jump(mugJump);
        return true;
      } else if (slug === "mint") {
        // 🪙 Mint on Zora.
        await mint(
          {
            ...system.painting,
            record: system.nopaint.recording
              ? system.nopaint.record
              : undefined,
          },
          (p) => (progressBar = p),
          params,
        );
      }
      flashColor = [0, 200, 0];
    } catch (err) {
      console.warn(err);
      flashColor = [200, 0, 0];
    }
    progressTrick = null;
    progressBar = 1;
    makeFlash($);
    return true;
  } else if (slug === "painting:done" || slug === "yes!" || slug === "done") {
    let destination = params[0] || "upload"; // or "upload"
    if (destination === "u" || slug === "yes!") destination = "upload";
    //                                  ^ "yes!" is always an upload.
    let filename; // Used in painting upload.
    let recordingSlug;

    if (system.nopaint.recording) {
      console.log("🖌️ Saving recording:", destination);
      const record = system.nopaint.record;
      filename = `painting-${record[record.length - 1].timestamp}.png`;
      // ^ For below, because the record will be cleared.

      if (destination === "upload") {
        progressBar = 0;
        progressPhase = "PREPARING";
        progressPercentage = 0;
        progressTrick = new gizmo.Hourglass(24, {
          completed: () => (progressBar += min(0.5, progressBar + 0.1)),
          autoFlip: true,
        });
      }

      progressPhase = "ZIPPING RECORDING";
      const zipped = await zip({ destination, painting: { record } }, (p) => {
        console.log("🤐 Zip progress:", p);
        progressBar = p * 0.3; // Zip is 0-30% of total progress
        progressPercentage = Math.floor(p * 30);
      });

      progressTrick = null;

      console.log("🤐 Zipped:", zipped);
      recordingSlug = zipped.slug;

      // TODO: Don't delete painting record unless `new` is entered. 23.10.03.01.51
      // system.nopaint.recording = false;
      // system.nopaint.record = [];
      // await store.delete("painting:record", "local:db");

      flashColor = [0, 255, 0];
    } else {
      filename = `painting-${num.timestamp()}.png`;
      flashColor = [255, 0, 0];
      console.warn("🖌️ No recording to save!");
    }

    // Always upload a PNG.
    if (destination === "upload") {
      console.log("🖼️ Uploading painting...");

      try {
        progressPhase = "FINISHING...";
        console.log(`📞 Calling upload with recordingSlug: ${recordingSlug}`);
        const uploadData = store["painting:tags"]
          ? { ...store["painting"], tags: store["painting:tags"] }
          : store["painting"];
        const data = await upload(filename, uploadData, (p) => {
          console.log("🖌️ Painting progress:", p);
          if (p < 1.0) {
            // During S3 upload: 30-80%
            progressBar = 0.3 + (p * 0.5);
            progressPercentage = Math.floor(30 + (p * 50));
          } else {
            // S3 complete, database processing: show indeterminate state
            progressPhase = "PROCESSING...";
            progressBar = -2; // Special value for pulsing animation
            progressPercentage = -1; // Hide percentage
          }
          needsPaint(); // Update display during upload
        }, undefined, recordingSlug); // Pass bucket as undefined (use auth), recordingSlug as 5th param
        console.log("🪄 Painting uploaded:", filename, data);
        if (store["painting:tags"]) {
          delete store["painting:tags"];
          store.persist?.("painting:tags", "local:db");
        }

        // The upload function includes the database call, so this happens after everything
        progressPhase = "COMPLETE";
        progressBar = 1.0; // 100%
        progressPercentage = 100;
        needsPaint(); // Force paint to show completion

        // Brief delay to show completed state
        await new Promise(resolve => setTimeout(resolve, 300));

        progressBar = -1; // Hide progress bar
        progressPhase = "";
        progressPercentage = 0;

        // For anonymous paintings with recordings, the slug is already combined in MongoDB
        // No need to update it separately

        // Jump to the painting using its code
        if (data.code) {
          console.log(`🎨 Your painting code: #${data.code}`);
          jump(`painting#${data.code}`);
        } else {
          // Fallback if no code (shouldn't happen but be safe)
          notice(`Saved!`, ["lime"]);
        }

        flashColor = [0, 255, 0];
        makeFlash($);
        return true; // Prevent default - we handled the upload
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0];
        progressBar = -1;
        progressPhase = "";
        progressPercentage = 0;
        makeFlash($);
        return true;
      }
    } else {
      makeFlash($);
      return true;
    }

    /*
    // 🧪 DEBUG: Simple spinner test with dummy await
    console.log("🧪 Testing spinner for 'done' command");

    progressBar = 0; // Show spinner
    progressTrick = new gizmo.Hourglass(24, {
      completed: () => (progressBar += min(0.5, progressBar + 0.1)),
      autoFlip: true,
    });
    needsPaint(); // Request paint

    // Dummy async work to test spinner visibility
    await new Promise(resolve => setTimeout(resolve, 2000)); // 2 second delay

    progressBar = 1.0; // Complete
    needsPaint();
    await new Promise(resolve => setTimeout(resolve, 300)); // Show completion

    progressBar = -1; // Hide
    progressTrick = null;

    console.log("🧪 Spinner test complete");
    makeFlash($, false);
    return true;

    /*
    let destination = params[0] || "upload"; // or "upload"
    if (destination === "u" || slug === "yes!") destination = "upload";
    //                                  ^ "yes!" is always an upload.
    let filename; // Used in painting upload.
    let recordingSlug;

    if (system.nopaint.recording) {
      console.log("🖌️ Saving recording:", destination);
      const record = system.nopaint.record;
      filename = `painting-${record[record.length - 1].timestamp}.png`;
      // ^ For below, because the record will be cleared.

      if (destination === "upload") {
        progressBar = 0;
        progressTrick = new gizmo.Hourglass(24, {
          completed: () => (progressBar += min(0.5, progressBar + 0.1)),
          autoFlip: true,
        });
      }

      const zipped = await zip({ destination, painting: { record } }, (p) => {
        console.log("🤐 Zip progress:", p);
        progressBar = p;
      });

      progressTrick = null;

      console.log("🤐 Zipped:", zipped);
      recordingSlug = zipped.slug;

      // TODO: Don't delete painting record unless `new` is entered. 23.10.03.01.51
      // system.nopaint.recording = false;
      // system.nopaint.record = [];
      // await store.delete("painting:record", "local:db");

      flashColor = [0, 255, 0];
    } else {
      filename = `painting-${num.timestamp()}.png`;
      flashColor = [255, 0, 0];
      console.warn("🖌️ No recording to save!");
    }

    // Always upload a PNG.
    if (destination === "upload") {
      console.log("🖼️ Uploading painting...");

      // Only initialize progress bar if not already started (by act() pre-emptively)
      if (progressBar < 0) {
        progressBar = 0; // Trigger progress bar rendering.
        progressTrick = new gizmo.Hourglass(24, {
          completed: () => (progressBar += min(0.5, progressBar + 0.1)),
          autoFlip: true,
        });
        needsPaint(); // Force a repaint to show the progress bar

        // Yield to the event loop so the progress bar can render
        await new Promise(resolve => setTimeout(resolve, 0));
      }

      try {
        console.log(`📞 Calling upload with recordingSlug: ${recordingSlug}`);
        const uploadData = store["painting:tags"]
          ? { ...store["painting"], tags: store["painting:tags"] }
          : store["painting"];
        const data = await upload(filename, uploadData, (p) => {
          console.log("🖌️ Painting progress:", p);
          progressBar = p;
        }, undefined, recordingSlug); // Pass bucket as undefined (use auth), recordingSlug as 5th param
        console.log("🪄 Painting uploaded:", filename, data);
        if (store["painting:tags"]) {
          delete store["painting:tags"];
          store.persist?.("painting:tags", "local:db");
        }

        // Show completed progress bar briefly before jumping
        progressBar = 1.0; // Full bar
        needsPaint(); // Force paint
        progressTrick?.stop?.(); // Stop the hourglass animation
        progressTrick = null;

        // Brief delay to show completed state
        await new Promise(resolve => setTimeout(resolve, 300));

        progressBar = -1; // Hide progress bar

        // For anonymous paintings with recordings, the slug is already combined in MongoDB
        // No need to update it separately

        // Jump to the painting using its code
        if (data.code) {
          console.log(`🎨 Your painting code: #${data.code}`);
          jump(`painting#${data.code}`);
        } else {
          // Fallback if no code (shouldn't happen but be safe)
          notice(`Saved!`, ["lime"]);
        }

        flashColor = [0, 255, 0];
        makeFlash($);
        return true; // Prevent default - we handled the upload
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0];
        progressBar = -1;
        makeFlash($);
        return true;
      }
    } else {
      makeFlash($);
      return true;
    }
    */

  } else if (slug === "flower") {
    jump("lmn-flower");
    return true;
  } else if (slug === "petal") {
    jump("lmn-petal");
    return true;
  } else if (slug === "product") {
    // Switch active product: product book, product record
    const productKey = params[0];
    if (productKey) {
      products.setActiveProduct(productKey);
      flashColor = [0, 255, 0];
      notice(`PRODUCT: ${productKey.toUpperCase()}`, ["lime"]);
    } else {
      const current = products.getActiveProduct();
      notice(`CURRENT: ${current ? current.type.toUpperCase() : 'NONE'}`, ["cyan"]);
      flashColor = [0, 255, 255];
    }
    makeFlash($);
    return true;
  } else if (slug === "bro") {
    jump("brother");
    return true;
  } else if (slug === "sis") {
    jump("sister");
    return true;
  } else if (slug === "gf") {
    jump("girlfriend");
    return true;
  } else if (slug === "bf") {
    jump("boyfriend");
    return true;
  } else if (slug === "bb") {
    jump("booted-by");
    return true;
  } else if (slug === "p" || slug === "pain") {
    jump("painting");
    return true;
  } else if (slug === "stample") {
    const arg = params[0];
    if (arg) {
      const safeArg = arg.startsWith("#") ? `%23${arg.slice(1)}` : arg;
      jump(["stample", safeArg].join("~"));
    } else {
      jump("stample");
    }
    return true;
  } else if (slug === "load") {
    // Load a file via URL.
    // Images:
    if (params[0].startsWith("http")) {
      // Replace painting with loaded image, adding it to the undo stack.
      try {
        const image = await net.preload(params[0]);
        system.nopaint.replace({ system, store, needsPaint }, image);
        flashColor = [0, 0, 255];
        makeFlash($);
        return true;
      } catch (err) {
        console.error("🚫 Could not load:", err);
        flashColor = [255, 0, 0];
        makeFlash($);
        return true;
      }
    } else {
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }
  } else if (slug === "mood:nuke" || slug === "mood:denuke") {
    const nuke = slug === "mood:nuke";
    const label = slug === "mood:nuke" ? "NUKE" : "DENUKE";
    const res = await net.userRequest("POST", "/api/mood", { nuke });
    flashColor = res?.deleted ? [0, 255, 0] : [255, 0, 0];
    if (res?.altered >= 0) {
      notice(`${label}D MOODS`);
    } else {
      notice(`${label} FAILED`, ["yellow", "red"]);
    }
    makeFlash($, true);
    return true;
  } else if (slug === "mood") {
    let res;
    const moodInput = params.join(" ").trim();
    if (moodInput.length > 0) {
      res = await net.userRequest("POST", "/api/mood", {
        mood: moodInput,
      });
    }

    if (moodInput.length === 0) {
      flashColor = [0, 255, 255]; // Cyan color for flash
      notice("EMPTY", ["cyan", "blue"]);
    } else {
      flashColor = res?.mood ? [0, 255, 0] : [255, 0, 0];

      if (res?.mood) {
        console.log("‍🍼 mood:", res.mood);
        notice(help.choose(":)", ":|", ":(", ":O", ":\\", ":/"));
      } else {
        const message = res?.message;
        let note = "ERROR";
        if (message === "unauthorized") note = "UNAUTHORIZED";
        makeFlash($, true);
        notice(note, ["yellow", "red"]);
        console.error("🍼🚫 Could not set mood.");
      }
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("publish")) {
    const publishablePiece = store["publishable-piece"];
    if (!publishablePiece) {
      flashColor = [255, 0, 0];
      makeFlash($);
      notice("No piece found!", ["yellow", "red"]);
      console.error("🪄 No publishable piece found!");
      return true;
    }

    // 🐍
    // TODO: May need to detect (...) here and give the piece
    //       a random name if one is not specified.
    let publishSlug = params[0] || publishablePiece.slug;
    if (publishSlug === "(...)") {
      publishSlug = "sketch";
    }

    // Check if publishSlug contains only valid filename characters.
    if (!/^[a-zA-Z0-9_-]+$/.test(publishSlug)) {
      notice("BAD NAME", ["yellow", "red"]);
      // throw new Error("publishSlug contains invalid characters.");
    } else {
      await publishPiece(
        { api, send, jump, handle, upload },
        publishSlug,
        publishablePiece.source,
        publishablePiece.ext,
      );
    }

    return true;
  } else if (text.startsWith("channel") || text.startsWith("code-channel")) {
    // Set a `code-channel` for piece writing.
    let newChannel = params.join(" ") || "";
    let isNone = false;
    console.log("new channel:", newChannel);
    if (newChannel === "") {
      const currentChannel = await store.retrieve("code-channel");
      isNone = true;
      const text = currentChannel || "no channel";
      notice(
        text,
        text === "no channel" ? ["yellow", "red"] : ["white", "blue"],
        { wrap: "char" },
      );
    } else if (newChannel === "none") {
      newChannel = "";
      isNone = true;
      notice("no channel", ["yellow", "red"]);
      code.channel(newChannel);
    } else {
      notice(newChannel, ["white", "blue"]);
      code.channel(newChannel);
    }

    if (isNone) {
      flashColor = [255, 0, 0];
    } else {
      flashColor = [0, 0, 255];
    }
    makeFlash($);
    return true;
  } else if (text === "run") {
    send({ type: "post-to-parent", content: { type: "runPiece" } });
    makeFlash($);
    return true;
  } else if (text === "docs") {
    if (net.iframe) {
      send({ type: "post-to-parent", content: { type: "openDocs" } });
    } else {
      jump("out:/docs");
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("edit")) {
    jump(
      "out:https://marketplace.visualstudio.com/items?itemName=aesthetic-computer.aesthetic-computer-code",
    );
    makeFlash($);
    return true;
  } else if (/^\++$/.test(text)) {
    // New window(s). '+' opens 1, '++' opens 2, '+++' opens 3, etc.
    // In Electron, the host webview wrapper intercepts and opens new BrowserWindows.
    const count = text.length;
    const isElectron = /Electron/i.test(navigator.userAgent || "");
    console.log(`[prompt] '${text}' command - opening ${count} new window(s)`, { 
      isElectron, 
      userAgent: navigator.userAgent,
      href: location.href 
    });
    // Send one request per window, with index for positioning
    for (let i = 0; i < count; i++) {
      send({ type: "window:open", content: { url: location.href, index: i, total: count } });
    }
    makeFlash($);
    return true;
  } else if (text === "-") {
    const isElectron = /Electron/i.test(navigator.userAgent || "");
    console.log("[prompt] '-' command - closing window", { 
      isElectron, 
      userAgent: navigator.userAgent 
    });
    try {
      send({ type: "window:close" });
    } catch (err) {
      console.error("[prompt] window:close send failed:", err);
      if (isElectron) {
        // Ask the Electron host to close this BrowserWindow (intercepted as a popup).
        console.log("[prompt] Attempting ac://close popup...");
        try {
          const result = window.open("ac://close", "_blank");
          console.log("[prompt] ac://close result:", result);
          if (!result) {
            console.log("[prompt] ac://close popup blocked, falling back to navigation");
            location.href = "ac://close";
          }
        } catch (innerErr) {
          console.error("[prompt] ac://close failed:", innerErr);
          console.log("[prompt] ac://close error, falling back to navigation");
          location.href = "ac://close";
        }
      } else {
        // Browsers will only allow this for script-opened tabs/windows.
        console.log("[prompt] Attempting window.close()...");
        window.close();
      }
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("google")) {
    const query = text.substring(7).trim(); // Extract the search query
    jump(`https://www.google.com/search?q=${encodeURIComponent(query)}`);
    makeFlash($);
    return true;
  } else if (text.startsWith("source")) {
    // Try to grab the piece requested in param[0] or just load blank.
    const piece = params[0] || "blank";
    const { host, path } = parse(piece);

    // Replacement tokens for blank piece.
    const tokens = {
      name: "$NAME",
      timestamp: "$TIMESTAMP",
      desc: "$THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES",
    };

    // Inject the Blank template with some starting data.
    function inject(body, desc) {
      return body
        .replaceAll(tokens.name, capitalize(piece))
        .replaceAll(tokens.timestamp, num.timestamp())
        .replaceAll(tokens.desc, desc);
    }

    try {
      const fullUrl = `https://${host}/${path}.mjs`;
      console.log("📥 Attempting to load source from url:", fullUrl);
      let result = await fetch(fullUrl);

      if (result.status === 404) {
        const anonUrl =
          "https://art.aesthetic.computer/" + path.split("/").pop() + ".mjs";
        console.log("🧑‍🤝‍🧑 Attempting to load piece from anon url:", anonUrl);
        result = await fetch(anonUrl);
        if (result.status === 404) {
          throw new Error("📄🚫 Piece not found.");
        }
      }

      let body = await result.text();
      if (piece === "blank") body = inject(body, `A blank piece.`);

      if (!net.iframe) {
        download(`${piece}.mjs`, body);
      } else {
        send({
          type: "post-to-parent",
          content: { type: "openSource", title: `${piece}.mjs`, source: body },
        });
      }

      flashColor = [0, 0, 255];
    } catch (err) {
      console.error("📄🤯", err);
      // Download the blank piece if the piece was not found,
      // and replace it.
      console.log("📄📥 Downloading the blank piece.");
      const { host, path } = parse("blank");
      const result = await fetch(`https://${host}/${path}.mjs`);
      let body = await result.text();
      let desc = params.slice(1).join(" ");
      if (desc.length === 0) desc = `A piece called \`${piece}\`.`;
      body = inject(body, desc);
      download(`${piece}.mjs`, body);
      flashColor = [0, 0, 255];
    }

    makeFlash($);
    return true;
  } else if (text.startsWith("html ") || text.startsWith("pack ") || text.startsWith("bundle ")) {
    // Generate a self-contained HTML pack for a piece (KidLisp $code or .mjs piece)
    const pieceCode = params[0];
    if (!pieceCode) {
      notice("Usage: pack $code or pack piece", ["red"]);
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }

    // Detect KidLisp ($code) vs normal .mjs piece
    const isKidlisp = pieceCode.startsWith("$");
    const code = isKidlisp ? pieceCode.slice(1) : pieceCode;
    const displayName = isKidlisp ? `$${code}` : code;

    // Initialize pack progress UI with structured timeline
    const timeline = makePackTimeline();
    advancePackStep(timeline, 'fetch', `Fetching ${displayName}...`);
    packProgress = { timeline, startTime: performance.now(), code: displayName };
    needsPaint();

    try {
      // Use streaming endpoint for progress updates
      // Hit the oven directly to avoid Netlify proxy timeout (26s limit + SSE buffering)
      const bundleParam = isKidlisp ? `code=$${code}` : `piece=${code}`;
      const response = await fetch(`https://oven.aesthetic.computer/pack-html?${bundleParam}&format=stream`);
      if (!response.ok) {
        throw new Error(`Pack API returned ${response.status}`);
      }
      const reader = response.body.getReader();
      const decoder = new TextDecoder();
      let buffer = '';
      let result = null;
      let currentEventType = null; // Persist across chunk boundaries
      let serverError = null; // Track server-side errors from SSE

      // Helper to parse SSE lines
      const parseSSELines = (lines) => {
        for (const line of lines) {
          if (line.startsWith('event: ')) {
            currentEventType = line.slice(7);
          } else if (line.startsWith('data: ') && currentEventType) {
            if (currentEventType === 'error') {
              try {
                const data = JSON.parse(line.slice(6));
                serverError = data.error || "Unknown server error";
              } catch (e) {
                serverError = line.slice(6);
              }
              currentEventType = null;
              continue;
            }
            try {
              const data = JSON.parse(line.slice(6));

              if (currentEventType === 'progress') {
                // Advance the structured timeline
                if (data.stage) {
                  advancePackStep(packProgress.timeline, data.stage, data.message);
                }
                needsPaint();
              } else if (currentEventType === 'complete') {
                result = data;
                advancePackStep(packProgress.timeline, 'complete', 'Done!');
                finalizePackTimeline(packProgress.timeline);
                needsPaint();
              }
            } catch (parseErr) {
              console.warn("SSE parse error:", parseErr, "line:", line.slice(0, 100));
            }
            currentEventType = null;
          }
        }
      };

      // Timeout: abort the stream if it takes longer than 2 minutes
      const PACK_TIMEOUT = 120_000;
      const timeoutId = setTimeout(() => {
        reader.cancel();
      }, PACK_TIMEOUT);

      try {
        while (true) {
          const { done, value } = await reader.read();
          if (done) {
            // Flush the decoder to get any remaining bytes
            buffer += decoder.decode();
            break;
          }

          buffer += decoder.decode(value, { stream: true });

          // Parse SSE events from buffer
          const lines = buffer.split('\n');
          buffer = lines.pop() || ''; // Keep incomplete line in buffer

          parseSSELines(lines);

          // Stop reading if server sent an error event
          if (serverError) break;
        }
      } finally {
        clearTimeout(timeoutId);
      }

      // Process any remaining data in buffer after stream ends
      if (buffer.trim()) {
        const finalLines = buffer.split('\n');
        parseSSELines(finalLines);
      }

      if (serverError) {
        throw new Error(serverError);
      }

      if (!result) {
        throw new Error("No result received from pack API");
      }

      // Decode base64 content and download
      const htmlContent = atob(result.content);
      download(result.filename, htmlContent, { type: "text/html" });

      notice("Downloaded " + result.filename + " (" + result.sizeKB + "KB)", ["lime"]);
      flashColor = [0, 255, 0];
    } catch (err) {
      console.error("Pack error:", err);
      notice("Pack failed: " + err.message, ["red"]);
      flashColor = [255, 0, 0];
    }

    // Clear pack progress
    packProgress = null;

    makeFlash($);
    return true;
  } else if (text.startsWith("m4d ") || text.startsWith("4d ")) {
    // Generate an offline Max for Live device (.amxd) for any piece
    const pieceRef = params[0];
    if (!pieceRef) {
      notice("Usage: m4d piece or m4d $code", ["red"]);
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }

    const isKidlisp = pieceRef.startsWith("$");
    const code = isKidlisp ? pieceRef.slice(1) : pieceRef;
    const displayName = isKidlisp ? `$${code}` : code;

    // Show indeterminate progress overlay
    const m4dTimeline = makePackTimeline();
    advancePackStep(m4dTimeline, 'fetch', `Building M4L device for ${displayName}...`);
    packProgress = { timeline: m4dTimeline, startTime: performance.now(), code: displayName };
    needsPaint();

    try {
      const bundleParam = isKidlisp ? `code=$${code}` : `piece=${code}`;
      const response = await fetch(`/api/pack-html?${bundleParam}&format=m4d`);

      if (!response.ok) {
        const err = await response.json().catch(() => ({ error: `HTTP ${response.status}` }));
        throw new Error(err.error || `M4D API returned ${response.status}`);
      }

      advancePackStep(packProgress.timeline, 'complete', 'Downloading .amxd...');
      finalizePackTimeline(packProgress.timeline);
      needsPaint();

      // Convert binary to latin1 string for reliable worker→main postMessage transfer
      const buffer = await response.arrayBuffer();
      const bytes = new Uint8Array(buffer);
      let binaryStr = '';
      const chunk = 8192;
      for (let i = 0; i < bytes.length; i += chunk) {
        binaryStr += String.fromCharCode.apply(null, bytes.subarray(i, i + chunk));
      }
      download(`AC ${displayName} (offline).amxd`, binaryStr, { encoding: "binary" });

      notice(`Downloaded AC ${displayName} (offline).amxd`, ["lime"]);
      flashColor = [0, 255, 0];
    } catch (err) {
      console.error("M4D error:", err);
      notice("M4D failed: " + err.message, ["red"]);
      flashColor = [255, 0, 0];
    }

    packProgress = null;
    makeFlash($);
    return true;
  } else if (text.startsWith("m4do ") || text.startsWith("4do ")) {
    // Generate an online Max for Live device (.amxd) that loads the live URL
    const pieceRef = params[0];
    if (!pieceRef) {
      notice("Usage: m4do piece", ["red"]);
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }

    const displayName = pieceRef;
    const liveUrl = `https://aesthetic.computer/${pieceRef}`;
    const width = 400, height = 200;

    // Build a minimal M4L patcher with jweb~ pointing to the live URL
    const patcher = {
      patcher: {
        fileversion: 1,
        appversion: { major: 9, minor: 0, revision: 7, architecture: "x64", modernui: 1 },
        classnamespace: "box",
        rect: [134, 174, 800, 600],
        openrect: [0, 0, width, height],
        openinpresentation: 1,
        gridsize: [15, 15],
        enablehscroll: 0, enablevscroll: 0,
        devicewidth: width,
        description: `Aesthetic Computer ${displayName} (online)`,
        boxes: [
          { box: { disablefind: 0, id: "obj-jweb", latency: 0, maxclass: "jweb~", numinlets: 1, numoutlets: 3, outlettype: ["signal","signal",""], patching_rect: [10,50,width,height], presentation: 1, presentation_rect: [0,0,width+1,height+1], rendermode: 1, url: liveUrl } },
          { box: { id: "obj-plugout", maxclass: "newobj", numinlets: 2, numoutlets: 0, patching_rect: [10,280,75,22], text: "plugout~ 1 2" } },
          { box: { id: "obj-thisdevice", maxclass: "newobj", numinlets: 1, numoutlets: 3, outlettype: ["bang","int","int"], patching_rect: [350,50,85,22], text: "live.thisdevice" } },
          { box: { id: "obj-print", maxclass: "newobj", numinlets: 1, numoutlets: 0, patching_rect: [350,80,150,22], text: `print [AC-${displayName.toUpperCase()}]` } },
          { box: { id: "obj-route", maxclass: "newobj", numinlets: 1, numoutlets: 2, outlettype: ["",""], patching_rect: [350,140,60,22], text: "route ready" } },
          { box: { id: "obj-activate", maxclass: "message", numinlets: 2, numoutlets: 1, outlettype: [""], patching_rect: [350,170,60,22], text: "activate 1" } },
        ],
        lines: [
          { patchline: { destination: ["obj-plugout",0], source: ["obj-jweb",0] } },
          { patchline: { destination: ["obj-plugout",1], source: ["obj-jweb",1] } },
          { patchline: { destination: ["obj-print",0], source: ["obj-thisdevice",0] } },
          { patchline: { destination: ["obj-route",0], source: ["obj-jweb",2] } },
          { patchline: { destination: ["obj-activate",0], source: ["obj-route",0] } },
          { patchline: { destination: ["obj-jweb",0], source: ["obj-activate",0] } },
        ],
        dependency_cache: [], latency: 0, is_mpe: 0,
        external_mpe_tuning_enabled: 0, minimum_live_version: "",
        minimum_max_version: "", platform_compatibility: 0, autosave: 0,
      },
    };

    // Pack into .amxd binary (header + length + JSON)
    const M4L_HEADER = "ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch";
    const jsonStr = JSON.stringify(patcher);
    const jsonBytes = new TextEncoder().encode(jsonStr);
    const headerBytes = new Uint8Array([...M4L_HEADER].map(c => c.charCodeAt(0)));
    const lenBuf = new Uint8Array(4);
    new DataView(lenBuf.buffer).setUint32(0, jsonBytes.length, true); // little-endian
    const binary = new Uint8Array(headerBytes.length + 4 + jsonBytes.length);
    binary.set(headerBytes, 0);
    binary.set(lenBuf, headerBytes.length);
    binary.set(jsonBytes, headerBytes.length + 4);

    // Convert binary to latin1 string for reliable worker→main postMessage transfer
    let binaryStr = '';
    const chunk = 8192;
    for (let i = 0; i < binary.length; i += chunk) {
      binaryStr += String.fromCharCode.apply(null, binary.subarray(i, i + chunk));
    }
    download(`AC ${displayName} (online).amxd`, binaryStr, { encoding: "binary" });

    notice(`Downloaded AC ${displayName} (online).amxd`, ["lime"]);
    flashColor = [0, 255, 0];
    makeFlash($);
    return true;
  } else if (text === "wallet" || text.startsWith("wallet ")) {
    // Tezos wallet: connect, view, disconnect
    const subcommand = params[0]?.toLowerCase();

    try {
      // Check if already connected (from localStorage session)
      const existingAddress = tezosWalletAddress || await api.tezos.address();

      if (subcommand === "disconnect") {
        // Disconnect wallet
        await api.tezos.disconnect();
        tezosWalletAddress = null;
        tezosWalletBalance = null;
        tezosDomainName = null;
        notice("ꜩ Wallet disconnected", ["yellow"]);
        flashColor = [255, 200, 0];
      } else if (subcommand === "status") {
        // Show status without connecting
        if (existingAddress) {
          tezosWalletAddress = existingAddress;
          const [balance, domain] = await Promise.all([
            fetchTezosBalance(existingAddress, tezosNetwork),
            fetchTezosDomain(existingAddress, tezosNetwork)
          ]);
          tezosWalletBalance = balance;
          tezosDomainName = domain;
          tezosBalanceLastFetch = Date.now();
          const balanceStr = balance !== null ? `${balance.toFixed(2)}ꜩ` : "?ꜩ";
          const displayName = domain || `${existingAddress.slice(0, 8)}...${existingAddress.slice(-4)}`;
          notice(`ꜩ ${displayName}`, ["cyan"]);
          notice(`${balanceStr} • ${tezosNetwork}`, ["gray"]);
          flashColor = [0, 200, 255];
        } else {
          notice("ꜩ No wallet connected", ["gray"]);
          notice("Type 'wallet' to connect", ["gray"]);
          flashColor = [100, 100, 100];
        }
      } else if (!subcommand || subcommand === "connect") {
        // Jump to wallet piece for connection UI
        // The /wallet piece will handle address entry and connection
        makeFlash($);
        jump("wallet");
        return true;
      } else {
        notice("Usage: wallet [disconnect|status]", ["yellow"]);
        notice("  wallet - connect & view", ["gray"]);
        notice("  wallet mainnet - use mainnet", ["gray"]);
        flashColor = [255, 200, 0];
      }
    } catch (err) {
      console.error("Wallet error:", err);
      // Handle user cancellation gracefully (various error messages from Beacon)
      const errMsg = (err.message || err || "").toLowerCase();
      if (errMsg.includes("aborted") ||
          errMsg.includes("cancel") ||
          errMsg.includes("reject") ||
          errMsg.includes("denied") ||
          errMsg.includes("closed") ||
          errMsg.includes("dismissed") ||
          errMsg.includes("user")) {
        notice("ꜩ Connection cancelled", ["gray"]);
        flashColor = [100, 100, 100];
      } else {
        notice("Wallet error: " + (err.message || err), ["red"]);
        flashColor = [255, 0, 0];
      }
    }

    makeFlash($);
    return true;
  } else if (text === "tezos" || text.startsWith("tezos ")) {
    // Legacy alias - redirect to wallet
    notice("Use 'wallet' command instead", ["yellow"]);
    flashColor = [255, 200, 0];
    makeFlash($);
    return true;
  } else if (text.startsWith("keep ")) {
    // Mint a KidLisp piece as an NFT on Tezos (user pays with their wallet)
    const pieceCode = params[0];
    if (!pieceCode) {
      notice("Usage: keep $code", ["red"]);
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }

    // Normalize piece code (ensure it starts with $)
    const code = pieceCode.startsWith("$") ? pieceCode.slice(1) : pieceCode;

    // For new mints, require AC login
    // For existing tokens, allow access (token owner can resync without AC account)
    // The keep piece will handle authorization based on wallet ownership
    if (!user) {
      // Let them through - keep.mjs will show appropriate UI
      // If token exists, they can view/resync with just wallet
      // If token doesn't exist, keep.mjs will show login prompt
      console.log("🪙 KEEP: User not logged in, allowing access to keep piece (will check wallet ownership)");
    }

    // Jump to the dedicated keep piece for multi-step minting flow
    store["keep:piece"] = code;
    jump(`keep~$${code}`);
    return true;
  } else if (text.startsWith("email")) {
    // Set user email.
    const email = text.split(" ")[1];
    let clear = true;
    if (email) {
      const res = await net.userRequest("POST", "/api/email", {
        email,
        name: email,
      });
      console.log("Request:", res);
      if (res.email) {
        flashColor = [0, 255, 0];
        notice("Check " + res.email);
        send({ type: "keyboard:close" });
        profile = new ui.TextButton(currentLang().resendEmail, { center: "xy", screen });
        profile.stickyScrubbing = true; // Prevent drag-between-button behavior
        profileAction = "resend-verification";
        ellipsisTicker = new gizmo.EllipsisTicker();
        user.email = res.email; // Update the global `user` object for this session.
        user.name = res.email;
        // store["aesthetic:refresh-user"] = true;
        // store.persist("aesthetic:refresh-user");
        fetchUserAPI = api;
        fetchUser();
      } else {
        flashColor = [255, 0, 0];
        console.warn(res.message);
        notice("NETWORK ERROR", ["yellow", "red"]);
        clear = false;
      }
    } else {
      flashColor = [255, 0, 0];
    }
    makeFlash($, clear);
    return true;
  } else if (slug === "admin:shop") {
    jump("https://admin.shopify.com");
    makeFlash($);
    return true;
  } else if (slug === "admin:shop-editor") {
    jump("https://admin.shopify.com/store/aesthetic-computer/themes/141869547701");
    makeFlash($);
    return true;
  } else if (slug.startsWith("admin:migrate-")) {
    // Usage: `admin:migrate-painting`
    //        `admin:migrate-piece`
    const res = await net.userRequest(
      "GET",
      `/api/admin?migrate=${slug.split("-")[1]}`,
    );
    flashColor = res && res.status === 202 ? [0, 255, 0] : [255, 0, 0];
    if (res && res.status === 202) {
      notice("MIGRATION STARTED ;)");

      // Optionally, you could automatically jump to the new location if desired
      // For example, if migrating to a new piece, you might want to jump there directly
      // const newSlug = "new-piece-slug"; // Replace with actual logic to determine new slug
      // jump(newSlug);
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("handle") && !text.startsWith("handles")) {
    // Set user handle or jump to handle color customization piece.

    // Make sure there is a parameter.
    let newHandle = text.split(" ")[1];
    if (!newHandle) {
      // No parameter - jump to handle customization piece
      if (user && handle()) {
        makeFlash($);
        jump("handle");
        return true;
      } else {
        // User doesn't have a handle yet - show empty notice
        flashColor = [0, 0, 128];
        makeFlash($);
        notice("EMPTY", ["cyan", "blue"]);
        return true;
      }
    }

    if (newHandle[0] === "@") newHandle = newHandle.slice(1); // Strip off any leading "@" sign to help with validation.

    // And a handle has been specified.
    const validated = validateHandle(newHandle);
    if (newHandle?.length > 0 && validated === "valid") {
      const res = await net.userRequest("POST", "/handle", {
        handle: newHandle,
      });
      const handleChanged = res?.handle;
      flashColor = handleChanged ? [0, 255, 0] : [255, 0, 0];
      if (handleChanged) {
        const previousHandle = handle();
        broadcast("handle:updated:" + res.handle);
        console.log("🧖 Handle changed:", res.handle);
        makeFlash($, true);
        if (previousHandle) notice("@" + res.handle);
        profileAction = "profile";
        store["handle"] = res.handle;
        if (!previousHandle) {
          jump("chat");
          beep();
        }
        // store.persist("handle");
      } else {
        const note = res?.message || "error";
        console.log("Response:", res);
        makeFlash($, true);
        notice(note.toUpperCase(), ["yellow", "red"]);
      }
      needsPaint();
    } else {
      console.warn("🧖 No @handle specified / bad handle design:", validated);
      notice(validated.toUpperCase(), ["yellow", "red"]);
    }
    return true;
  } else if (text.startsWith("admin:handle:strip")) {
    const handleToStrip = text.split(" ")[1];
    //  🩹️ Strip the handle from a user.

    if (handleToStrip) {
      console.log("🩹 Stripping handle:", handleToStrip);
      const res = await net.userRequest("POST", "/handle", {
        handle: handleToStrip,
        action: "strip",
      });

      console.log("🩹 Strip result:", res);
      notice(
        res.message.toUpperCase(),
        res.status === 200 ? undefined : ["yellow", "red"],
      );
      flashColor = res.status === 200 ? "lime" : "red";
      makeFlash($, true);
      // If the handle was stripped then somehow broadcast it
      // and update the chat.
    }

    return true;
  } else if (text.startsWith("admin:chat-system:mute")) {
    const userToMute = text.split(" ")[1];
    const res = await net.userRequest("POST", "/handle", {
      handle: userToMute, // could be a handle, sub, or email
      action: "chat-system:mute",
    });
    // console.log("🦻 Mute result:", res);
    notice(
      res.message.toUpperCase(),
      res.status === 200 ? undefined : ["yellow", "red"],
    );
    flashColor = res.status === 200 ? "lime" : "red";
    makeFlash($, true);
    return true;
  } else if (text.startsWith("admin:chat-system:unmute")) {
    const userToMute = text.split(" ")[1];
    const res = await net.userRequest("POST", "/handle", {
      handle: userToMute, // could be a handle, sub, or email
      action: "chat-system:unmute",
    });
    // console.log("🦻 Unmute result:", res);
    notice(
      res.message.toUpperCase(),
      res.status === 200 ? undefined : ["yellow", "red"],
    );
    flashColor = res.status === 200 ? "lime" : "red";
    makeFlash($, true);
    return true;
  } else if (text.startsWith("patch ") || text === "patch") {
    // 🤖 Admin-only command to spawn a GitHub Copilot coding agent
    // Usage: `patch <prompt/instruction for the agent>`
    const prompt = text.replace(/^patch\s*/, "").trim();
    
    if (!prompt) {
      notice("ENTER A PROMPT", ["yellow", "red"]);
      flashColor = [255, 128, 0];
      makeFlash($);
      return true;
    }

    if (!user) {
      notice("LOGIN REQUIRED", ["yellow", "red"]);
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }

    notice("SPAWNING AGENT...", ["cyan", "blue"]);
    progressBar = 0.1; // Show some initial progress

    try {
      const res = await net.userRequest("POST", "/patch", { prompt });
      
      if (res && res.success) {
        progressBar = 1;
        flashColor = [0, 255, 0];
        notice("AGENT SPAWNED", ["lime", "green"]);
        
        // If we got a URL, offer to open it
        if (res.data?.url) {
          console.log("🤖 PR Agent issue:", res.data.url);
          setTimeout(() => {
            notice(`#${res.data.jobId?.replace("issue-", "")}`, ["cyan", "blue"]);
          }, 1500);
        }
      } else {
        progressBar = -1;
        flashColor = [255, 0, 0];
        const msg = res?.message || "ERROR";
        notice(msg.toUpperCase().substring(0, 20), ["yellow", "red"]);
      }
    } catch (err) {
      progressBar = -1;
      flashColor = [255, 0, 0];
      console.error("🔴 Patch error:", err);
      notice("FAILED", ["yellow", "red"]);
    }
    
    makeFlash($, true);
    return true;
  } else if ((text === "ul" || text === "upload") && store["painting"]) {
    if (!navigator.onLine) {
      flashColor = [255, 0, 0];
      notice("OFFLINE", ["yellow", "red"]);
    } else {
      const filename = `painting-${num.timestamp()}.png`;
      // The first dashed string will get replaced with a slash / media directory filter on the server.
      progressBar = 0; // Trigger progress bar rendering.
      try {
        const data = await upload(
          filename,
          store["painting"],
          (p) => (progressBar = p),
        );
        console.log("🪄 Painting uploaded:", filename, data);
        flashColor = [0, 255, 0, 128];
        makeFlash($);
        const slug = user
          ? `${handle() || user.email}/painting/${data.slug}`
          : data.slug;
        jump(`download:painting ${slug}`);
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0, 127];
        makeFlash($);
      }
    }
    return true;
  } else if (slug === "flip" || slug === "flop") {
    // Direct pixel remapping — paste() with negative scale is broken in grid().
    const w = system.painting.width,
      h = system.painting.height;
    const src = system.painting.pixels;
    const dst = new Uint8ClampedArray(w * h * 4);

    if (slug === "flip") {
      // Vertical mirror: reverse row order.
      for (let y = 0; y < h; y++) {
        const srcOff = y * w * 4;
        const dstOff = (h - 1 - y) * w * 4;
        dst.set(src.subarray(srcOff, srcOff + w * 4), dstOff);
      }
    } else {
      // Horizontal mirror: reverse each row left-to-right.
      for (let y = 0; y < h; y++) {
        for (let x = 0; x < w; x++) {
          const si = (y * w + x) * 4;
          const di = (y * w + (w - 1 - x)) * 4;
          dst[di] = src[si];
          dst[di + 1] = src[si + 1];
          dst[di + 2] = src[si + 2];
          dst[di + 3] = src[si + 3];
        }
      }
    }

    system.painting = { width: w, height: h, pixels: dst };

    // Also mirror the nopaint overlay buffer.
    if (system.nopaint.buffer?.pixels) {
      const bs = system.nopaint.buffer.pixels;
      const bd = new Uint8ClampedArray(w * h * 4);
      if (slug === "flip") {
        for (let y = 0; y < h; y++) {
          const off = y * w * 4;
          bd.set(bs.subarray(off, off + w * 4), (h - 1 - y) * w * 4);
        }
      } else {
        for (let y = 0; y < h; y++) {
          for (let x = 0; x < w; x++) {
            const si = (y * w + x) * 4;
            const di = (y * w + (w - 1 - x)) * 4;
            bd[di] = bs[si]; bd[di+1] = bs[si+1];
            bd[di+2] = bs[si+2]; bd[di+3] = bs[si+3];
          }
        }
      }
      system.nopaint.buffer = { width: w, height: h, pixels: bd };
    }

    // Persist the painting.
    store["painting"] = {
      width: system.painting.width,
      height: system.painting.height,
      pixels: system.painting.pixels,
    };
    store.persist("painting", "local:db");

    // 🎨 Broadcast painting flip/flop to other tabs
    if (typeof $commonApi !== 'undefined' && $commonApi.broadcastPaintingUpdate) {
      $commonApi.broadcastPaintingUpdate("updated", {
        source: "transform",
        operation: slug,
        vertical: slug === "flip"
      });
    }

    system.nopaint.addUndoPainting(system.painting, slug);
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "right" || slug === "left") {
    // Direct pixel remapping — grid() rotation draws overlapping boxes.
    const w = system.painting.width, h = system.painting.height;
    const nw = h, nh = w; // Swapped dimensions for 90° rotation.
    const src = system.painting.pixels;
    const dst = new Uint8ClampedArray(nw * nh * 4);

    for (let sy = 0; sy < h; sy++) {
      for (let sx = 0; sx < w; sx++) {
        const si = (sy * w + sx) * 4;
        let dx, dy;
        if (slug === "right") {
          // 90° CW: src(x,y) → dst(h-1-y, x)
          dx = h - 1 - sy;
          dy = sx;
        } else {
          // 90° CCW: src(x,y) → dst(y, w-1-x)
          dx = sy;
          dy = w - 1 - sx;
        }
        const di = (dy * nw + dx) * 4;
        dst[di] = src[si];
        dst[di + 1] = src[si + 1];
        dst[di + 2] = src[si + 2];
        dst[di + 3] = src[si + 3];
      }
    }

    system.painting = { width: nw, height: nh, pixels: dst };

    // Rotate the nopaint overlay buffer with same transform.
    if (system.nopaint.buffer?.pixels) {
      const bs = system.nopaint.buffer.pixels;
      const bd = new Uint8ClampedArray(nw * nh * 4);
      for (let sy = 0; sy < h; sy++) {
        for (let sx = 0; sx < w; sx++) {
          const si = (sy * w + sx) * 4;
          let dx, dy;
          if (slug === "right") { dx = h-1-sy; dy = sx; }
          else { dx = sy; dy = w-1-sx; }
          const di = (dy * nw + dx) * 4;
          bd[di] = bs[si]; bd[di+1] = bs[si+1];
          bd[di+2] = bs[si+2]; bd[di+3] = bs[si+3];
        }
      }
      system.nopaint.buffer = { width: nw, height: nh, pixels: bd };
    }

    // Move the painting to the center of the screen.
    system.nopaint.resetTransform({ system, screen });
    system.nopaint.storeTransform(store, system);

    // Persist the painting and lock the resolution.
    store["painting"] = {
      width: system.painting.width,
      height: system.painting.height,
      pixels: system.painting.pixels,
    };
    store.persist("painting", "local:db");

    // 🎨 Broadcast painting rotation to other tabs
    if (typeof $commonApi !== 'undefined' && $commonApi.broadcastPaintingUpdate) {
      $commonApi.broadcastPaintingUpdate("updated", {
        source: "rotate",
        direction: slug,
        angle: slug === "right" ? 90 : -90
      });
    }

    system.nopaint.addUndoPainting(system.painting, slug);
    store["painting:resolution-lock"] = true;
    store.persist("painting:resolution-lock", "local:db");

    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "resize" || slug === "res") {
    // Resize the active painting if one exists, or make one at this
    // size if it doesn't.
    const w = params[0],
      h = params[1] || w;

    let fullText = slug;
    if (params.length > 0) fullText += "~" + params.join("~");

    if (w === undefined) {
      flashColor = [255, 0, 0];
    } else {
      const result = nopaint_adjust(
        api,
        { w, h, scale: true },
        fullText,
      );
      flashColor = result ? "lime" : "red";
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("dl") || text.startsWith("download")) {
    if (store["painting"]) {
      if (!canShare) {
        downloadPainting(
          api,
          abs(parseInt(text.split(" ")[1])) || defaultDownloadScale,
        );
      }
      // Show a green flash if we succesfully download the file.
      flashColor = [0, 255, 0];
    } else {
      flashColor = [255, 0, 0]; // Show a red flash otherwise.
    }
    makeFlash($);
    return true;
  } else if (slug === "gutter") {
    // Change the `TextInput` gutter to a minimum of 5 or a default of 16.
    input.gutter = max(5, parseInt(params[0])) || 16;
    store["gutter:lock"] = input.columns;
    // This will reflow on resize.
    flashColor = [100, 0, 100, 100]; // Dark Magenta
    makeFlash($);
    return true;
  } else if (slug === "login") {
    net.login();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    // if (net.iframe) jump("login-wait");
    return true;
  } else if (slug === "hi") {
    net.login();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    // if (net.iframe) jump("login-wait");
    return true;
  } else if (slug === "signup" || slug === "imnew") {
    net.signup();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    return true;
  } else if (text === "logout" || text === "bye") {
    net.logout();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    return true;
  } else if (text === "no") {
    system.nopaint.no({ system, store, needsPaint });
    if (system.nopaint.undo.paintings.length > 1) {
      flashColor = [0, 0, 255, 100]; // Blue for successful undo.
    } else {
      flashColor = [255, 0, 0, 100]; // Red for failed undo.
    }
    makeFlash($);
    return true;
  } else if (text === "yes") {
    system.nopaint.no({ system, store, needsPaint }, true);
    if (system.nopaint.undo.paintings.length > 1) {
      flashColor = [0, 0, 255, 100]; // Blue for success.
    } else {
      flashColor = [255, 0, 0, 100]; // Red for fail.
    }
    makeFlash($);
    return true;
  } else if (text === "nopan") {
    system.nopaint.resetTransform(api);
    system.nopaint.storeTransform(store, system); // Store the translation after completion.
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "new") {
    // Combines "no!" and "painting:start" in a graphics context;
    // Or jumps to the creation of a new piece of code given a textual parameter.

    // ❓ How could this UX be improved for a better in-editor experience? 24.02.23.19.12

    if (
      ["piece", "bot", "brush", "fps", "space", "stamp"].includes(params[0])
    ) {
      try {
        const response = await fetch(
          `https://raw.githubusercontent.com/digitpain/aesthetic.computer-code/main/${params[0]}.mjs`,
        );
        let body = await response.text();

        const lines = body.split("\n"); // Split the body into lines.
        if (
          params[1] &&
          lines.length >= 2 && // Check if the first two lines are comments...
          lines[0].startsWith("//") &&
          lines[1].startsWith("//")
        ) {
          lines[0] = `// ${capitalize(params[1]) || ""}, ${num.timestamp()}`;
          const desc = params.slice(2).join(" ");
          if (desc) lines[1] = `// ${desc}`;
          body = lines.join("\n");
        }

        const name = params[1] || params[0];
        if (!net.iframe) {
          download(`${name}.mjs`, body);
        } else {
          send({
            type: "post-to-parent",
            content: {
              type: "openSource",
              title: `${name}.mjs`,
              source: body,
            },
          });
          flashColor = [0, 0, 255];
          makeFlash($);
          return true;
        }
      } catch (error) {
        console.error("Error fetching source:", error);
        flashColor = [255, 0, 0];
        makeFlash($);
        return true;
      }
    }

    const w = parseInt(params[0]),
      h = parseInt(params[1]) || w;

    let size;
    if (!isNaN(w) && !isNaN(h)) {
      size = { w, h };
    } else {
      // If no params or invalid params, use full screen dimensions
      size = { w: screen.width, h: screen.height };
    }

    // Clear storage and reset state (without creating a painting yet)
    await store.delete("painting", "local:db");
    await store.delete("painting:resolution-lock", "local:db");
    await store.delete("painting:transform", "local:db");
    await store.delete("painting:record", "local:db");

    // Also clear from memory to ensure nopaint_adjust doesn't see stale values
    delete store["painting"];
    delete store["painting:resolution-lock"];
    delete store["painting:transform"];
    delete store["painting:record"];

    system.nopaint.undo.paintings.length = 0; // Reset undo stack.
    system.painting = null;
    system.nopaint.resetTransform({ system, screen }); // Reset transform.

    if (system.nopaint.recording) {
      system.nopaint.recording = false;
      system.nopaint.record.length = 0;
    }

    let fullText = slug;
    if (params.length > 0) fullText += "~" + params.join("~");

    // Now create the new painting at the specified size
    nopaint_adjust(api, size, fullText);

    system.nopaint.startRecord(fullText); // Start recording paintings.

    needsPaint();

    flashColor = [200, 0, 200];
    makeFlash($);
    return true;
  } else if (text === "painting:reset" || text === "no!") {
    const deleted = await system.nopaint.noBang(api); //{
    //   system,
    //   store,
    //   screen,
    //   needsPaint,
    //   painting,
    // });

    system.nopaint.startRecord("new"); // Start recording paintings.

    if (deleted) {
      flashColor = [0, 0, 255]; // Blue for succesful deletion.
    } else {
      flashColor = [255, 0, 0]; // Red if delete failed.
    }

    makeFlash($);
    needsPaint();
    return true;
  } else if (text === "3dline:reset") {
    const deleted = await store.delete("3dline:drawing", "local:db");

    if (deleted) {
      flashColor = [0, 0, 255]; // Blue for succesful deletion.
    } else {
      flashColor = [255, 0, 0]; // Red if delete failed.
    }

    makeFlash($);
    needsPaint();
    return true;
  } else if (text === "dark" || text === "light") {
    if (text === "light") {
      store.delete("dark-mode");
      darkMode(false);
      flashColor = [255, 255, 255];
    } else {
      flashColor = [0, 0, 0];
      darkMode(true);
    }
    makeFlash($);
    return true;
  } else if (text === "serious" || text === "neo" || text === "theme:default" || text === "theme:serious" || text === "theme:neo") {
    const newTheme = text === "theme:default" ? "default" :
                     text === "theme:serious" || text === "serious" ? (theme === "serious" ? "default" : "serious") :
                     text === "theme:neo" || text === "neo" ? (theme === "neo" ? "default" : "neo") : "default";
    setTheme(newTheme, $);
    flashColor = theme === "serious" ? [0, 0, 0] :
                 theme === "neo" ? ($.dark ? [0, 255, 0] : [0, 80, 0]) :
                 scheme.dark.block;
    makeFlash($);
    needsPaint();
    return true;
  } else if (text === "theme") {
    load(parse("theme"));
    return true;
  } else if (text === "lang") {
    load(parse("lang"));
    return true;
  } else if (text.startsWith("2022")) {
    load(parse("wand~" + text)); // Execute the current command.
    return true;
  } else if (text === "connect") {
    let identity;
    input.text = "";
    try {
      identity = await connect(); // Web3 connect.
      store["identity"] = identity; // Store the identity.
      store.persist("identity"); // ... and persist it!
      makeFlash($, false);
      flashColor = [0, 255, 0];
    } catch (e) {
      makeFlash($, false);
      flashColor = [255, 0, 0];
    }
    return true;
  } else if (text === "bgm stop") {
    bgm.stop();
    makeFlash($, false);
    flashColor = [255, 0, 0];
    return true;
  } else if (text.toLowerCase() === "sotce-net") {
    let url = debug
      ? "https://" + location.host + "/sotce-net"
      : "https://sotce.net";
    if (net.iframe) url += "?session-sotce=retrieve";
    jump(url);
    flashColor = "pink";
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "github" || text === "gh") {
    const githubUrl = "https://github.com/digitpain/aesthetic.computer";
    if (!openExternalFromIframe(githubUrl)) jump(githubUrl);
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "agc") {
    const agcUrl = "https://acg.media.mit.edu/";
    if (!openExternalFromIframe(agcUrl)) jump(agcUrl);
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "gmail") {
    jump("https://gmail.com");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "ucla-syllabus") {
    jump(
      "out:https://docs.google.com/document/d/1foiOLdvJeTdPHQKMIWzKBoGcszGPJS5bRcY-Rg3ou6A/edit?usp=sharing",
    );
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "app" || text === "ios") {
    jump("https://apps.apple.com/app/aesthetic-computer/id6450940883");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "pp") {
    const prefix = !net.iframe ? "out:" : "";
    jump(prefix + "/privacy-policy");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "direct") {
    const prefix = !net.iframe ? "out:" : "";
    jump(
      debug
        ? prefix + "/aesthetic-direct"
        : prefix + "https://aesthetic.direct",
    );
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "kidlisp") {
    // Store labelBack source so kidlisp.com can offer back navigation
    if (typeof window !== "undefined" && window.safeSessionStorageSet) {
      window.safeSessionStorageSet("aesthetic-labelBack-source", "prompt");
    }
    jump(debug ? "/kidlisp.com" : "https://kidlisp.com");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "support") {
    jump("https://aesthetic.computer/support");
    makeFlash($);
    return true;
  } else if (text === "browserstack" || text === "bs") {
    jump("https://live.browserstack.com");
    makeFlash($);
    return true;
  } else if (text === "gpt" || text === "chatgpt") {
    jump("https://chat.openai.com");
    makeFlash($);
    return true;
  } else if (text === "help") {
    // Go to the Discord for now if anyone types help.
    makeFlash($);
    jump("chat");
    return true;
  } else if (text === "shillball" || text === "sb") {
    // Shortcuts for Yeche's Shillball game.
    jump("https://galerie-yechelange.baby/ball");
    makeFlash($);
    return true;
  } else if (text === "ssl") {
    jump("/aesthetic.crt"); // Download the local CRT file.
    // TODO: Is there a way to detect this? 24.07.27.02.48
    makeFlash($);
    return true;
  } else if (text === "prod") {
    jump("https://prompt.ac"); // Visit the live site.
    makeFlash($);
    return true;
  } else if (text === "local" || text.startsWith("local")) {
    const param = text.replace("local", "").trim().replaceAll(" ", "~");
    const slug = param.length > 0 ? `/${param}` : "";
    jump("https://local.aesthetic.computer" + slug); // Go to the ngrok dev server, passing any params as a piece.
    // jump("https://localhost:8888" + slug); // Go to the local dev server, passing any params as a piece.
    makeFlash($);
    return true;
  } else if (text.split(" ")[0] === "of") {
    // Ordfish shortcuts.
    jump(`ordfish~${text.split(" ").slice(1).join("~")}`);
    return true;
  } else if (ordfish[text] || text.split(" ") === "of") {
    jump(`ordfish~${text}`);
    return true;
  } else if (text.startsWith("hiccup")) {
    // Disconnect from socket server, chat, and udp in 5 seconds...
    net.hiccup();
    return true;
  } else if (text.startsWith("#")) {
    // Handle painting short codes like #k3d, #WDv
    const code = text.slice(1).trim();
    if (code.length > 0) {
      console.log(`🎨 Looking up painting by code: #${code}`);

      if (progressTrick) progressTrick = null;
      const promptInput = system?.prompt?.input;
      let spinnerActive = true;
      const spinnerStart = Date.now();

      const enforceSpinnerDelay = (fn, minimum = 160) => {
        const elapsed = Date.now() - spinnerStart;
        const wait = Math.max(0, minimum - elapsed);
        if (wait > 0) {
          setTimeout(fn, wait);
        } else {
          fn();
        }
      };

      if (promptInput) {
        promptInput.lock = true;
        needsPaint();
        setTimeout(() => {
          if (spinnerActive && promptInput && !promptInput.lock) {
            promptInput.lock = true;
            needsPaint();
          }
        }, 0);
      }

      const clearLookupSpinner = (delay = 0) => {
        progressTrick = null;
        progressBar = -1;
        if (promptInput) {
          const unlock = () => {
            spinnerActive = false;
            promptInput.lock = false;
            needsPaint();
          };
          if (delay > 0) {
            setTimeout(unlock, delay);
          } else {
            unlock();
          }
        }
      };

      const cacheKey = `painting-code:${code}`;
      const setLocationHash = (displayCode) => {
        if (typeof window === "undefined") return;
        const normalizedHash = displayCode.startsWith("#")
          ? displayCode
          : `#${displayCode}`;
        try {
          if (window.location.hash !== normalizedHash) {
            window.location.hash = normalizedHash;
          }
          window.acSTARTING_HASH = normalizedHash.slice(1);
        } catch (err) {
          console.warn("⚠️ Unable to update window hash for painting code", normalizedHash, err);
        }
      };

      const routeToPainting = (metadata) => {
        if (!metadata || !metadata.slug || !metadata.handle) {
          console.error(`❌ Incomplete metadata for painting code: #${code}`, metadata);
          notice(`Painting #${code} not found`, ["red"]);
          clearLookupSpinner();
          return;
        }

        const canonicalCode = (metadata.code || code || "").replace(/^#/, "");
        const normalizedHandle = (metadata.handle || "").replace(/^@+/, "");
        const record = {
          slug: metadata.slug,
          handle: normalizedHandle,
          code: canonicalCode,
        };

        // Cache under both the canonical code and the raw code that was requested
        store[cacheKey] = record;
        store[`painting-code:${canonicalCode}`] = record;
        if (normalizedHandle) {
          store[`painting-slug:${normalizedHandle}/${metadata.slug}`] = canonicalCode;
        }
        if (metadata.handle && metadata.handle !== normalizedHandle) {
          store[`painting-slug:${metadata.handle}/${metadata.slug}`] = canonicalCode;
        }

        enforceSpinnerDelay(() => {
          clearLookupSpinner();
          setLocationHash(canonicalCode);
          jump(`painting#${canonicalCode}`);
        });
      };

      const cached = store[cacheKey];

      if (cached) {
        console.log(`✅ Found cached painting: ${cached.handle}/painting/${cached.slug}`);
        routeToPainting(cached);
        return true;
      }

      fetch(`/api/painting-code?code=${code}`)
        .then((response) => {
          if (!response.ok) {
            throw new Error(`HTTP ${response.status}`);
          }
          return response.json();
        })
        .then((data) => {
          if (data?.slug && data?.handle) {
            console.log(`✅ Found painting: ${data.handle}/painting/${data.slug}`);
            routeToPainting({ ...data, code: data.code || code });
          } else {
            console.error(`❌ Painting not found for code: #${code}`);
            notice(`Painting #${code} not found`, ["red"]);
            clearLookupSpinner(0);
          }
        })
        .catch((err) => {
          console.error(`❌ Error looking up painting code #${code}:`, err);
          notice(`Error loading #${code}`, ["red"]);
          clearLookupSpinner(0);
        });

      return true;
    }
  } else if (/^ac[0-9]{2}[a-z]{5}$/.test(slug)) {
    // Handle permahandle codes like ac25namuc → @jeffrey's profile
    console.log(`🔑 Looking up permahandle: ${slug}`);

    const promptInput = system?.prompt?.input;
    if (promptInput) promptInput.lock = true;

    fetch(`/api/permahandle/${slug}`)
      .then((response) => {
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return response.json();
      })
      .then((data) => {
        if (data?.handle) {
          console.log(`✅ Found user: ${slug} → @${data.handle}`);
          if (promptInput) promptInput.lock = false;
          jump(`@${data.handle}`);
        } else {
          throw new Error("No handle found");
        }
      })
      .catch((err) => {
        console.error(`❌ Permahandle not found: ${slug}`, err);
        notice(`Unknown code: ${slug}`, ["red"]);
        if (promptInput) promptInput.lock = false;
        needsPaint();
      });

    return true;
  } else {
    // console.log("🟢 Attempting a load!");    // 🟠 Local and remote pieces...

    // Theory: Is `load` actually similar to eval?
    //         (Whereas this is eval/apply at the program level.)
    let body, loaded;
    const trimmed = text.trim();
    // 🍏 Detect if we are in kidlisp mode and pass that flag through to 'load'
    const isKidlisp =
      trimmed.startsWith("(") ||
      trimmed.startsWith(";") ||
      isKidlispSource(trimmed);
    if (isKidlisp) {
      body = { name: trimmed, source: trimmed };
      loaded = await load(body, false, false, true, undefined, true); // Force kidlisp
      //                                        ^^^^ devReload  ^^^^^ forceKidlisp
    } else {
      body = parse(trimmed);
      loaded = await load(body); // Execute the current command.
    }

    // console.log("Loaded:", loaded);

    if (!loaded) {
      leaving(false);
      if (/*text.indexOf(" ") === -1 &&*/ text !== "goodiepal") {
        system.prompt.input.text = TYPO_REPLY;
        system.prompt.input.replied($); // Set the UI state back to normal.
        loaded = { replied: true };
      }
    } else {
      loaded = { left: true };
    }
    return loaded;
  }
}

// 📦 Media Preview Box - renders kidlisp/painting/tape previews with consistent sizing
class MediaPreviewBox {
  constructor() {
    this.width = 150;  // Fixed width for all media types
    this.height = 120; // Fixed height for all media types
    this.padding = 4;
  }

  // Get box dimensions for tooltip sizing
  getBoxDimensions() {
    return {
      width: this.width + this.padding * 2,
      height: this.height + this.padding * 2
    };
  }

  // Render a media item in the preview box with proper clipping
  render($, item, x, y, fadeIn) {
    const contentX = x + this.padding;
    const contentY = y + this.padding;

    // Set up clipping mask for the content area
    $.mask({ x: contentX, y: contentY, width: this.width, height: this.height });

    if (item.type === 'kidlisp' && item.source) {
      this.renderKidlisp($, item, contentX, contentY, fadeIn);
    } else if (item.type === 'painting' && item.image) {
      this.renderPainting($, item, contentX, contentY, fadeIn);
    } else if (item.type === 'tape' && item.frames && item.frames.length > 0) {
      this.renderTape($, item, contentX, contentY, fadeIn);
    } else if (item.type === 'tape' && item.isLoading) {
      this.renderTapeLoading($, item, contentX, contentY, fadeIn);
    }

    // Remove clipping mask
    $.unmask();
  }

  renderKidlisp($, item, x, y, fadeIn) {
    const charWidth = 4;
    const lineHeight = 10;
    const lines = item.source.split('\n').slice(0, Math.floor(this.height / lineHeight));

    const textAlpha = Math.floor(255 * fadeIn);

    lines.forEach((line, i) => {
      const lineY = y + i * lineHeight;
      // Simple monochrome rendering for now - could add syntax highlighting
      $.ink(100, 255, 150, textAlpha).write(
        line.substring(0, Math.floor(this.width / charWidth)),
        { x, y: lineY },
        undefined,
        undefined,
        false,
        "MatrixChunky8"
      );
    });
  }

  renderPainting($, item, x, y, fadeIn) {
    const img = item.image;

    // Ken Burns effect: pan a crop window that fills the preview box
    const nowTime = performance.now();
    const burnSeed = item.kenBurnsSeed ?? Math.random();
    item.kenBurnsSeed = burnSeed;
    const KEN_BURNS_CYCLE_MS = 8000;
    const burnProgress = ((nowTime / KEN_BURNS_CYCLE_MS) + burnSeed) % 1;

    // Calculate how much to scale the image to cover the box (cover, not contain)
    const scaleX = this.width / img.width;
    const scaleY = this.height / img.height;
    const scale = Math.max(scaleX, scaleY); // Cover - may crop

    // Scaled dimensions
    const scaledW = img.width * scale;
    const scaledH = img.height * scale;

    // Maximum pan range in scaled space
    const maxPanX = Math.max(0, scaledW - this.width);
    const maxPanY = Math.max(0, scaledH - this.height);

    // Ken Burns pan position (0-1)
    const panX = (Math.cos((burnProgress + 0.25) * Math.PI * 2) + 1) / 2;
    const panY = (Math.sin((burnProgress + 0.65) * Math.PI * 2) + 1) / 2;

    // Offset in scaled space
    const offsetX = maxPanX * panX;
    const offsetY = maxPanY * panY;

    // Draw scaled image at negative offset to create crop effect
    const drawX = x - offsetX;
    const drawY = y - offsetY;

    const imageAlpha = Math.floor(255 * fadeIn);
    $.ink(255, 255, 255, imageAlpha);
    $.paste(img, drawX, drawY, {
      width: Math.ceil(scaledW),
      height: Math.ceil(scaledH)
    });

    $.needsPaint(); // Keep animating
  }

  renderTape($, item, x, y, fadeIn) {
    // Cycle through frames at ~12 fps
    const nowTime = performance.now();
    const frameIndex = Math.floor((nowTime / 83)) % item.frames.length;
    const frame = item.frames[frameIndex];

    if (frame) {
      // Ken Burns effect
      const burnSeed = item.kenBurnsSeed ?? Math.random();
      item.kenBurnsSeed = burnSeed;
      const KEN_BURNS_CYCLE_MS = 8000;
      const burnProgress = ((nowTime / KEN_BURNS_CYCLE_MS) + burnSeed) % 1;

      // Calculate how much to scale the frame to cover the box
      const scaleX = this.width / frame.width;
      const scaleY = this.height / frame.height;
      const scale = Math.max(scaleX, scaleY); // Cover - may crop

      // Scaled dimensions
      const scaledW = frame.width * scale;
      const scaledH = frame.height * scale;

      // Maximum pan range in scaled space
      const maxPanX = Math.max(0, scaledW - this.width);
      const maxPanY = Math.max(0, scaledH - this.height);

      // Ken Burns pan position (0-1)
      const panX = (Math.cos((burnProgress + 0.25) * Math.PI * 2) + 1) / 2;
      const panY = (Math.sin((burnProgress + 0.65) * Math.PI * 2) + 1) / 2;

      // Offset in scaled space
      const offsetX = maxPanX * panX;
      const offsetY = maxPanY * panY;

      // Draw scaled frame at negative offset to create crop effect
      const drawX = x - offsetX;
      const drawY = y - offsetY;

      $.paste(frame, drawX, drawY, {
        width: Math.ceil(scaledW),
        height: Math.ceil(scaledH)
      });
    }

    $.needsPaint(); // Keep animating
  }

  renderTapeLoading($, item, x, y, fadeIn) {
    const animPhase = (performance.now() / 100) % (Math.PI * 2);
    const loadingAlpha = Math.floor(255 * fadeIn);

    // Draw cassette body outline
    $.ink(80, 80, 80, loadingAlpha).box(x, y, this.width, this.height, "outline");

    // Draw two reels
    const reelRadius = 15;
    const reelY = y + this.height / 2;
    const reel1X = x + this.width * 0.3;
    const reel2X = x + this.width * 0.7;

    // Left reel
    $.ink(150, 100, 150, loadingAlpha).circle(reel1X, reelY, reelRadius, false);
    const spoke1Angle = animPhase;
    const spokeLength = reelRadius - 3;
    for (let i = 0; i < 6; i++) {
      const angle = spoke1Angle + (i * Math.PI / 3);
      const endX = reel1X + Math.cos(angle) * spokeLength;
      const endY = reelY + Math.sin(angle) * spokeLength;
      $.ink(150, 100, 150, loadingAlpha).line(reel1X, reelY, endX, endY);
    }

    // Right reel
    $.ink(150, 100, 150, loadingAlpha).circle(reel2X, reelY, reelRadius, false);
    const spoke2Angle = -animPhase;
    for (let i = 0; i < 6; i++) {
      const angle = spoke2Angle + (i * Math.PI / 3);
      const endX = reel2X + Math.cos(angle) * spokeLength;
      const endY = reelY + Math.sin(angle) * spokeLength;
      $.ink(150, 100, 150, loadingAlpha).line(reel2X, reelY, endX, endY);
    }

    // Tape connecting reels
    const tapeY1 = reelY - reelRadius;
    const tapeY2 = reelY + reelRadius;
    $.ink(100, 70, 50, loadingAlpha).line(reel1X, tapeY1, reel2X, tapeY1);
    $.ink(100, 70, 50, loadingAlpha).line(reel1X, tapeY2, reel2X, tapeY2);

    $.needsPaint(); // Keep animating
  }

  // Render tape progress bar and time (outside the box)
  renderTapeMetrics($, item, x, y, boxWidth, fadeIn) {
    if (!item.frames || item.frames.length === 0) return;

    const nowTime = performance.now();
    const frameIndex = Math.floor((nowTime / 83)) % item.frames.length;

    // Progress bar (2px below box)
    const progressBarY = y + 2;
    const totalFrames = item.frames.length;
    const progress = frameIndex / totalFrames;
    const progressWidth = Math.floor(boxWidth * progress);

    $.ink(0, 0, 0, 255).line(x, progressBarY, x + boxWidth - 1, progressBarY);
    if (progressWidth > 0) {
      $.ink(255, 0, 0, 255).line(x, progressBarY, x + progressWidth - 1, progressBarY);
    }

    // Time display (2px below progress bar)
    const timeY = progressBarY + 4;
    const fps = 12;
    const currentSeconds = frameIndex / fps;
    const totalSeconds = totalFrames / fps;

    const formatTime = (seconds) => {
      const mins = Math.floor(seconds / 60);
      const secs = Math.floor(seconds % 60);
      return `${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
    };

    const timeText = `${formatTime(currentSeconds)} / ${formatTime(totalSeconds)}`;
    const timeAlpha = Math.floor(180 * fadeIn);
    $.ink(100, 180, 120, timeAlpha).write(
      timeText,
      { x: x + this.padding, y: timeY },
      undefined,
      undefined,
      false,
      "MatrixChunky8"
    );
  }
}

// 🎨 Paint
function paint($) {
  // Time-based animation counter (used by both MOTD and ghost hint)
  const now = performance.now();

  // 📊 FPS calculation using rolling window
  fpsTimestamps.push(now);
  while (fpsTimestamps.length > 0 && fpsTimestamps[0] < now - 1000) {
    fpsTimestamps.shift();
  }
  currentFps = fpsTimestamps.length;

  if (!lastMotdTime) lastMotdTime = now;
  const deltaTime = (now - lastMotdTime) / 1000; // Convert to seconds
  lastMotdTime = now;
  motdFrame += deltaTime * 60; // 60 units per second (equivalent to 60fps @ 1 per frame)

  if (motdCandidates.length > 1) {
    if (!lastMotdCycleTime) lastMotdCycleTime = now;
    if (now - lastMotdCycleTime >= MOTD_CYCLE_MS) {
      motdCandidateIndex = (motdCandidateIndex + 1) % motdCandidates.length;
      setMotdCandidate(motdCandidates[motdCandidateIndex]);
      $.net.motd = motd;
      lastMotdCycleTime = now;
    }
  }

  if (fetchingUser) fetchUserAPI = $.api;

  // Ensure pal is always defined with a fallback
  const langButtons = currentLang().buttons;
  if (theme === "serious") {
    pal = $.dark ? scheme.serious.dark : scheme.serious.light;
    if ($.system.prompt?.input) {
      $.system.prompt.input.scheme = { dark: scheme.serious.dark, light: scheme.serious.light, buttons: langButtons };
    }
  } else if (theme === "neo") {
    pal = $.dark ? scheme.neo.dark : scheme.neo.light;
    if ($.system.prompt?.input) {
      $.system.prompt.input.scheme = { dark: scheme.neo.dark, light: scheme.neo.light, buttons: langButtons };
    }
  } else {
    pal = $.dark ? scheme.dark : scheme.light;
    if ($.system.prompt?.input) {
      $.system.prompt.input.scheme = { ...scheme, buttons: langButtons };
    }
  }

  // Sync Enter button label with current language.
  if ($.system.prompt?.input?.enter) {
    $.system.prompt.input.enter.txt = langButtons.enter;
  }

  // 🅰️ Paint below the prompt || scheme.
  if ($.store["painting"]) {
    $.wipe(pal.background);
    $.system.nopaint.present($); // Render the painting.
    // Override pal if available from nopaint
    if ($.system.prompt.input.pal) {
      pal = $.system.prompt.input.pal;
    }
    scheme.dark.background[3] = 176; // Half semi-opaque palette background.
    scheme.light.background[3] = 190;
    scheme.serious.dark.background[3] = 176;
    scheme.serious.light.background[3] = 190;
    scheme.neo.dark.background[3] = 176;
    scheme.neo.light.background[3] = 190;
  } else {
    $.wipe(pal.background);
  }

  $.layer(1); // 🅱️ And above it...

  const { screen, ink, history, net, help } = $;

  // Make prompt text semi-transparent when curtain is up (default theme only)
  const showLoginCurtain = theme === "default" && ((!login?.btn.disabled && !profile) || (!login && !profile?.btn.disabled));
  if (showLoginCurtain && $.system.prompt.input.canType) {
    // Add opacity to text colors when curtain is up
    const originalDarkText = [...scheme.dark.text];
    const originalLightText = [...scheme.light.text];
    scheme.dark.text = [...originalDarkText.slice(0, 3), 128]; // 50% opacity
    scheme.light.text = [...originalLightText.slice(0, 3), 128];
  }

  if ($.system.prompt.input.canType) {
    const currentInputText = $.system.prompt.input.text;

    // 💡 Update tooltip state for current text
    // Get cursor position as text index using prompt.textPos()
    const prompt = $.system.prompt.input.prompt;
    const cursorCharPos = prompt?.textPos?.() ?? currentInputText.length;
    updateTooltipState(currentInputText, cursorCharPos);

    // 🔍 Update @handle autocomplete
    if (handleAutocomplete) {
      handleAutocomplete.update(currentInputText, cursorCharPos);
    }

    // 🤖 Check if we're in kidlisp mode (for syntax highlighting)
    const inKidlispMode = isPromptInKidlispMode(currentInputText);

    // 🟢 Check if this is ACTUAL KidLisp code (for cursor color, not just nopaint)
    const isActualKidLispCode = isActualKidLisp(currentInputText);

    // Store kidlisp mode state for other parts of the prompt to use
    $.system.prompt.kidlispMode = inKidlispMode;
    $.system.prompt.actualKidlisp = isActualKidLispCode;

    // 🔊 Play sound when entering or leaving KidLisp mode
    if (isActualKidLispCode !== previousKidlispMode) {
      if (isActualKidLispCode) {
        // Entering KidLisp mode - ascending synth sound
        $.sound.synth({
          type: "sine",
          tone: 440, // A4
          duration: 0.08,
          volume: 0.15,
          attack: 0.02,
          decay: 0.03,
          release: 0.03,
        });
        // Add a second harmonic for richness
        setTimeout(() => {
          $.sound.synth({
            type: "sine",
            tone: 660, // E5 (perfect fifth above)
            duration: 0.06,
            volume: 0.1,
            attack: 0.01,
            decay: 0.02,
            release: 0.03,
          });
        }, 40);
      } else {
        // Leaving KidLisp mode - descending synth sound
        $.sound.synth({
          type: "sine",
          tone: 440, // A4
          duration: 0.08,
          volume: 0.15,
          attack: 0.02,
          decay: 0.03,
          release: 0.03,
        });
        // Add a lower harmonic for contrast
        setTimeout(() => {
          $.sound.synth({
            type: "sine",
            tone: 293.66, // D4 (perfect fifth below)
            duration: 0.06,
            volume: 0.1,
            attack: 0.01,
            decay: 0.02,
            release: 0.03,
          });
        }, 40);
      }
      previousKidlispMode = isActualKidLispCode;
    }

    // If activeCompletions is currently empty, but the input text itself
    // is a valid, non-hidden command, it's likely due to a Tab completion
    // that cleared the active suggestions. In this case, we want to treat
    // the current input text as the single active completion to show its description.
    if (
      activeCompletions.length === 0 &&
      currentInputText && // Ensure text is not empty
      autocompletions[currentInputText] &&
      !autocompletions[currentInputText].hidden
    ) {
      activeCompletions.push(currentInputText);
      // This modification allows the description rendering logic below to pick up
      // the tab-completed command. activeCompletions will be naturally reset
      // by other parts of the system (e.g., in halt() or by TextInput updates).
    }

    // Hide history and autocomplete when in KidLisp mode
    if (!inKidlispMode) {
      if (activeCompletions.length === 0) {
        // History
        let historyTexts =
          history.length === 0 ? [] : history.map((h) => h.replaceAll("~", " "));

        historyTexts.reverse().forEach((t, i) => {
          const ii = i + 1;
          ink(140, 90, 235, 80 / ii).write(t, {
            x: 6,
            y: 6 + $.system.prompt.input.typeface.blockHeight * ii,
          });
        });
      }

      // Autocompetions
      if (activeCompletions.length > 0) {
        activeCompletions.forEach((completion, i) => {
          $.system.prompt.input.text;
          const diff =
            completion.length -
            (completion.length - $.system.prompt.input.text.length);
          let text = completion;
          if (i === 0) {
            text = completion.replace(
              $.system.prompt.input.text,
              " ".repeat(diff),
            );
          }
          ink($.dark ? "white" : "red", 32).write(text, {
            x: 6,
            y: 6 + i * $.system.prompt.input.typeface.blockHeight,
          });
        });
      }

      if (activeCompletions.length === 1) {
        // console.log("has completions!");
        ink(
          $.dark ? "white" : "red",
          $.system.prompt.input.text !== activeCompletions[0] ? 64 : 255,
        ).write(
          autocompletions[activeCompletions[0]].desc,
          { center: "xy" },
          null,
          screen.width - 8,
        );
      }

      // 💡 Paint tooltip when we have a recognized command
      // Show tooltip even with activeCompletions if we're typing params (have a space)
      // or if the command is fully typed (exact match in activeCompletions)
      const hasSpace = currentInputText.includes(" ") || currentInputText.includes(":");
      const isExactMatch = activeCompletions.length === 1 && activeCompletions[0] === currentInputText.split(/[: ]/)[0];
      if (tooltipState.visible && tooltipState.command && (activeCompletions.length === 0 || hasSpace || isExactMatch)) {
        paintTooltip($, currentInputText);
      }
    }

    // 📊 Character limit counter (per-character chaos with fast color cycling)
    const PROMPT_CHAR_LIMIT = 256;
    const charLen = currentInputText.length;
    const charCountText = `${charLen}/${PROMPT_CHAR_LIMIT}`;

    // Calculate fill percentage
    const fillPercent = charLen / PROMPT_CHAR_LIMIT; // 0 to 1+

    // Color zones with power-of-2 thresholds
    const grayColor = $.dark ? [120, 120, 130] : [100, 100, 110];
    const yellowColor = [255, 200, 60];
    const orangeColor = [255, 140, 40];
    const redColor = [255, 50, 50];

    let meterColor;
    let blinkSpeed = 0;

    if (charLen < 64) {
      // 0-63: Simple gray, no cycling
      meterColor = grayColor;
    } else if (charLen < 128) {
      // 64-127: Gray → Yellow with gentle blinking
      const progress = (charLen - 64) / 64; // 0 to 1
      blinkSpeed = progress * 3;
      const blink = Math.sin($.store.t * blinkSpeed) * 0.5 + 0.5;
      const r = Math.floor(grayColor[0] + (yellowColor[0] - grayColor[0]) * progress * blink);
      const g = Math.floor(grayColor[1] + (yellowColor[1] - grayColor[1]) * progress * blink);
      const b = Math.floor(grayColor[2] + (yellowColor[2] - grayColor[2]) * progress * blink);
      meterColor = [r, g, b];
    } else if (charLen < 192) {
      // 128-191: Yellow → Orange with faster blinking
      const progress = (charLen - 128) / 64; // 0 to 1
      blinkSpeed = 3 + progress * 8;
      const blink = Math.sin($.store.t * blinkSpeed) * 0.5 + 0.5;
      const r = Math.floor(yellowColor[0] + (orangeColor[0] - yellowColor[0]) * progress * blink);
      const g = Math.floor(yellowColor[1] + (orangeColor[1] - yellowColor[1]) * progress * blink);
      const b = Math.floor(yellowColor[2] + (orangeColor[2] - yellowColor[2]) * progress * blink);
      meterColor = [r, g, b];
    } else {
      // 192-256+: Orange → Red with rapid blinking
      const progress = Math.min(1, (charLen - 192) / 64); // 0 to 1
      blinkSpeed = 11 + progress * 20;
      const blink = Math.sin($.store.t * blinkSpeed) * 0.5 + 0.5;
      const r = Math.floor(orangeColor[0] + (redColor[0] - orangeColor[0]) * progress * blink);
      const g = Math.floor(orangeColor[1] + (redColor[1] - orangeColor[1]) * progress * blink);
      const b = Math.floor(orangeColor[2] + (redColor[2] - orangeColor[2]) * progress * blink);
      meterColor = [r, g, b];
    }

    // Only show counter when we hit 32 characters
    if (charLen >= 32) {
      // Switch to larger font when approaching limit (at 70% = 179 chars)
      const useLargeFont = fillPercent > 0.7;
      const charFont = useLargeFont ? undefined : "MatrixChunky8"; // undefined = default 6x8 font
      const charWidth = useLargeFont ? 6 : 4;

      // Position in top right corner with small margin
      const charMargin = 6;
      const baseCharCountX = screen.width - (charCountText.length * charWidth) - charMargin;
      const baseCharCountY = 6;

      // Gentle per-character chaos (slower, subtler movement)
      const chaosThreshold = 0.5;
      const chaosAmount = Math.max(0, (fillPercent - chaosThreshold) * 3); // Reduced from 8 to 3

      // Draw each character individually with its own gentle chaos offset
      const shadowOffset = 1;
      const shadowColor = $.dark ? [0, 0, 0, 180] : [255, 255, 255, 180];

      for (let i = 0; i < charCountText.length; i++) {
        const char = charCountText[i];
        const charX = baseCharCountX + (i * charWidth);
        const charY = baseCharCountY;

        // Each character gets gentle random jitter
        const jitterX = (Math.random() - 0.5) * chaosAmount;
        const jitterY = (Math.random() - 0.5) * chaosAmount;

        // Draw shadow
        ink(...shadowColor).write(char, {
          x: charX + shadowOffset + jitterX,
          y: charY + shadowOffset + jitterY
        }, undefined, undefined, false, charFont);

        // Draw character
        ink(...meterColor).write(char, {
          x: charX + jitterX,
          y: charY + jitterY
        }, undefined, undefined, false, charFont);
      }
    }

    // 📍 Cursor position indicator (below char count, shown at 32+ chars)
    if (charLen >= 32) {
      const sel = $.system.prompt.input.selection;
      const cursorText = sel
        ? `${sel[0]}-${sel[1]}`
        : `${cursorCharPos}`;
      const cpFont = "MatrixChunky8";
      const cpWidth = 4;
      const cpMargin = 6;
      const cpX = screen.width - (cursorText.length * cpWidth) - cpMargin;
      const cpY = fillPercent > 0.7 ? 16 : 14;
      const cpColor = $.dark ? [80, 80, 90] : [140, 140, 150];
      const cpShadow = $.dark ? [0, 0, 0, 120] : [255, 255, 255, 120];
      for (let i = 0; i < cursorText.length; i++) {
        const cx = cpX + (i * cpWidth);
        ink(...cpShadow).write(cursorText[i], { x: cx + 1, y: cpY + 1 }, undefined, undefined, false, cpFont);
        ink(...cpColor).write(cursorText[i], { x: cx, y: cpY }, undefined, undefined, false, cpFont);
      }
    }
  }

  if (progressBar >= 0 || progressBar === -2) {
    // Draw orange semi-transparent overlay
    ink(255, 180, 0, 120).box(0, 0, screen.width, screen.height, "inline");

    // Draw progress bar line at the top
    if (progressBar > 0 && progressBar <= 1) {
      // Normal progress bar (0-100%)
      const barWidth = Math.floor((screen.width - 2) * progressBar);
      ink(255, 180, 0).box(1, 1, barWidth, 1);
    } else if (progressBar === -2) {
      // Pulsing indeterminate progress (for backend processing)
      const pulse = (Math.sin(Date.now() / 200) + 1) / 2; // 0-1 sine wave
      const barWidth = Math.floor((screen.width - 2) * (0.3 + pulse * 0.4)); // 30-70% width
      const alpha = Math.floor(150 + pulse * 105); // 150-255 alpha
      ink(255, 180, 0, alpha).box(1, 1, barWidth, 1);
      $.system.nopaint.needsRender = true; // Keep animating
    }

    // Show progress text in center
    if (progressPhase) {
      // Animated dots
      const dots = Math.floor((Date.now() / 250) % 4);
      const text = progressPhase + ".".repeat(dots);

      // Background for text
      const textWidth = text.length * 6 + 16;
      const textHeight = 20;
      const x = (screen.width - textWidth) / 2;
      const y = screen.height / 2 - 10;

      ink(0, 200).box(x, y, textWidth, textHeight);
      ink(255, 180, 0).box(x, y, textWidth, textHeight, "outline");

      // Progress text
      ink(255, 255, 255).write(text, { center: "x", y: y + 6 });

      // Percentage below (only show if not in indeterminate state)
      if (progressPercentage > 0) {
        ink(255, 255, 255).write(`${progressPercentage}%`, {
          center: "x",
          y: y + textHeight + 8
        });
      }
    }
  }

  // 📦 Pack progress overlay — structured step list
  if (packProgress) {
    const { timeline, startTime, code } = packProgress;
    const elapsed = performance.now() - startTime;

    // Count completed steps for progress bar
    const done = timeline.filter(s => s.status === 'done').length;
    const progress = done / timeline.length;

    // Accent color (pink)
    const accent = [255, 100, 200];

    // Semi-transparent overlay
    ink(0, 0, 0, 140).box(0, 0, screen.width, screen.height);

    // 🎯 Progress bar at top — only moves forward
    const barY = 1;
    const barHeight = 2;
    const fullWidth = screen.width - 2;
    const filledWidth = Math.floor(fullWidth * progress);
    ink(40, 40, 40).box(1, barY, fullWidth, barHeight);
    if (filledWidth > 0) ink(...accent).box(1, barY, filledWidth, barHeight);

    // 📋 Title
    const titleText = `PACK ${code}`;
    const titleY = 6;
    ink(...accent).write(titleText, { center: "x", y: titleY });

    // Step list — show done + active + first pending, skip others
    const rowH = 10;
    const margin = 6;
    let y = titleY + 12;
    let shownPending = false;

    for (const step of timeline) {
      const isDone = step.status === 'done';
      const isActive = step.status === 'active';
      const isSkipped = step.status === 'skipped';
      const isPending = step.status === 'pending';

      // Skip steps that were never reached
      if (isSkipped) continue;

      // Only show the first pending step (as "...")
      if (isPending) {
        if (shownPending) continue;
        shownPending = true;
      }

      // Don't render past screen bottom
      if (y + rowH > screen.height - 2) break;

      // Background stripe for active step
      if (isActive) {
        ink(accent[0], accent[1], accent[2], 30).box(0, y, screen.width, rowH);
      }

      // Status indicator
      let indicator, indicatorColor;
      if (isDone) {
        indicator = '+';
        indicatorColor = [100, 255, 100];
      } else if (isActive) {
        // Animated spinner: - \ | /
        const spinChars = ['-', '\\', '|', '/'];
        indicator = spinChars[Math.floor(elapsed / 150) % 4];
        indicatorColor = accent;
      } else {
        indicator = '-';
        indicatorColor = [100, 100, 100];
      }

      ink(...indicatorColor).write(indicator, { x: margin, y });

      // Label
      const labelColor = isDone ? [150, 150, 150] : isActive ? [255, 255, 255] : [80, 80, 80];
      ink(...labelColor).write(step.label, { x: margin + 8, y });

      // Detail message (right side, for active step)
      if (isActive && step.message) {
        const maxMsgLen = Math.floor((screen.width - margin * 2 - step.label.length * 6 - 16) / 6);
        const msg = step.message.length > maxMsgLen
          ? step.message.slice(0, maxMsgLen - 2) + '..'
          : step.message;
        ink(accent[0], accent[1], accent[2], 180).write(msg, {
          x: screen.width - margin - msg.length * 6, y
        });
      }

      y += rowH;
    }

    // Keep animating (for the spinner)
    $.needsPaint();
  }

  // Calculate MOTD offset (do this before book rendering so it's always available)
  let motdXOffset = 0;

  // 📦 Paint SHOP box in top-right corner (hidden when typing)
  {
    const promptHasContent = $.system.prompt.input.text && $.system.prompt.input.text.length > 0;
    const shouldShow = showLoginCurtain && !$.system.prompt.input.canType && !promptHasContent;
    products.paint({ ...$, login, signup }, $.screen, shouldShow);
  }
  // 🟢 KidLisp mode border effect (full window border with scrolling dots)
  // Only show when prompt is focused (canType is true)
  const isKidlispMode = $.system?.prompt?.actualKidlisp;
  if (isKidlispMode && $.system.prompt.input.canType) {
    const activeProduct = products.getActiveProduct();
    const rotation = activeProduct ? activeProduct.rotation : 0;

    // Animated offset for scrolling effect
    const scrollOffset = Math.floor(rotation * 0.5) % 4; // Scroll 4 pixel cycle (matches spacing)

    // Cycle through vibrant colors
    const colorPhase = (rotation * 0.1) % 6;
    const colors = [
      [100, 255, 100], // Bright green (primary for kidlisp)
      [100, 255, 255], // Cyan
      [100, 200, 255], // Sky blue
      [200, 100, 255], // Purple
      [255, 100, 200], // Pink
      [255, 255, 100], // Yellow
    ];

    const currentColorIndex = Math.floor(colorPhase);
    const nextColorIndex = (currentColorIndex + 1) % colors.length;
    const blend = colorPhase - currentColorIndex;

    // Blend between current and next color
    const currentColor = colors[currentColorIndex];
    const nextColor = colors[nextColorIndex];
    const blendedColor = [
      Math.floor(currentColor[0] * (1 - blend) + nextColor[0] * blend),
      Math.floor(currentColor[1] * (1 - blend) + nextColor[1] * blend),
      Math.floor(currentColor[2] * (1 - blend) + nextColor[2] * blend),
    ];

    // Pulsing alpha
    const pulseAlpha = Math.floor(Math.sin(rotation * 0.12) * 60 + 140); // 80-200 range

    // Draw solid dotted border around entire screen (all dots filled)
    const dotSpacing = 4; // Pixels between dots
    const dotSize = 1; // 1 pixel dots

    // Top border
    for (let x = 0; x < screen.width; x += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box((x + scrollOffset) % screen.width, 0, dotSize, dotSize);
    }

    // Bottom border
    for (let x = 0; x < screen.width; x += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box((x + scrollOffset) % screen.width, screen.height - dotSize, dotSize, dotSize);
    }

    // Left border
    for (let y = 0; y < screen.height; y += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box(0, (y + scrollOffset) % screen.height, dotSize, dotSize);
    }

    // Right border
    for (let y = 0; y < screen.height; y += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box(screen.width - dotSize, (y + scrollOffset) % screen.height, dotSize, dotSize);
    }

    // 🟢 KidLisp.com button centered at bottom (between paste and enter buttons)
    const kidlispBtnText = "Edit on KidLisp.com";
    
    // Get paste and enter button positions to center between them
    const pasteBox = $.system.prompt.input?.paste?.btn?.box;
    const enterBox = $.system.prompt.input?.enter?.btn?.box;
    
    // Calculate center position between paste (left) and enter (right) buttons
    let kidlispBtnX, kidlispBtnY;
    if (pasteBox && enterBox) {
      // Position horizontally centered between paste's right edge and enter's left edge
      const gapStart = pasteBox.x + pasteBox.w;
      const gapEnd = enterBox.x;
      const gapCenter = gapStart + (gapEnd - gapStart) / 2;
      const btnWidth = kidlispBtnText.length * 4 + 4; // MatrixChunky8: 4px per char + 4px padding
      kidlispBtnX = Math.floor(gapCenter - btnWidth / 2);
      kidlispBtnY = pasteBox.y; // Same Y position as the bottom buttons
    } else {
      // Fallback to bottom center if buttons not available
      const btnWidth = kidlispBtnText.length * 4 + 4;
      kidlispBtnX = Math.floor(screen.width / 2 - btnWidth / 2);
      kidlispBtnY = screen.height - 18;
    }
    
    if (!kidlispBtn) {
      kidlispBtn = new $.ui.TextButton(kidlispBtnText, { x: kidlispBtnX, y: kidlispBtnY });
      kidlispBtn.stickyScrubbing = true;
    } else {
      kidlispBtn.reposition({ x: kidlispBtnX, y: kidlispBtnY }, kidlispBtnText);
      kidlispBtn.btn.disabled = false;
    }
    
    // Paint the button with rainbow cycling colors (like GIVE button) + blinking
    const kidlispBtnBox = kidlispBtn.btn.box;
    const isKidlispBtnDown = kidlispBtn.btn.down;
    const isKidlispBtnOver = kidlispBtn.btn.over;
    
    // 🌈 Rainbow cycling colors for attention-seeking effect
    const klT = performance.now() / 1000;
    const klHue = (klT * 60) % 360; // Cycle through hues
    const klPulse = Math.sin(klT * 4) * 0.5 + 0.5; // Pulsing effect (0-1)
    const klBlink = Math.floor(klT * 3) % 2; // Blink on/off 3 times per second
    
    // Convert HSL to RGB
    const klHslToRgb = (h, s, l) => {
      h /= 360; s /= 100; l /= 100;
      let r, g, b;
      if (s === 0) { r = g = b = l; }
      else {
        const hue2rgb = (p, q, t) => {
          if (t < 0) t += 1;
          if (t > 1) t -= 1;
          if (t < 1/6) return p + (q - p) * 6 * t;
          if (t < 1/2) return q;
          if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
          return p;
        };
        const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
        const p = 2 * l - q;
        r = hue2rgb(p, q, h + 1/3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1/3);
      }
      return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
    };
    
    // Bright saturated fill that cycles through rainbow
    const klFillColor = klHslToRgb(klHue, 90, 40 + klPulse * 15);
    const klTextColor = klHslToRgb((klHue + 180) % 360, 100, 85); // Complementary color for text
    const klOutlineColor = klHslToRgb((klHue + 120) % 360, 100, 60); // Triadic color for outline
    
    // Manually size the button box to fit the text tightly (reduce right padding)
    const textWidth = kidlispBtnText.length * 4; // MatrixChunky8: 4px per char
    const btnPadX = 2; // Tighter horizontal padding
    const btnPadY = 2; // Tighter vertical padding  
    kidlispBtnBox.w = textWidth + btnPadX * 2;
    kidlispBtnBox.h = 7 + btnPadY * 2; // MatrixChunky8 height is 7px
    
    // Background - rainbow cycling with blink effect
    if (klBlink || isKidlispBtnOver || isKidlispBtnDown) {
      const bgColor = isKidlispBtnDown ? [klFillColor[0] + 40, klFillColor[1] + 40, klFillColor[2] + 40] : klFillColor;
      ink(...bgColor).box(kidlispBtnBox);
    } else {
      ink(20, 20, 30).box(kidlispBtnBox); // Dark background during "off" blink
    }
    
    // Animated outline
    ink(...klOutlineColor).box(kidlispBtnBox, "outline");
    
    // Text with complementary color (tighter positioning)
    ink(...klTextColor).write(kidlispBtnText, { x: kidlispBtnBox.x + btnPadX, y: kidlispBtnBox.y + btnPadY }, undefined, undefined, false, "MatrixChunky8");

    // Keep animating
    $.needsPaint();
  } else {
    // Disable KidLisp button when not in KidLisp mode
    if (kidlispBtn) {
      kidlispBtn.btn.disabled = true;
    }
  }

  // 🎰 Polychrome border effect pointing to top-left corner (on login curtain)
  // In CRITICAL funding mode: Red/yellow warning stripes around ALL edges
  if (showLoginCurtain) {
    const activeProduct = products.getActiveProduct();
    const rotation = activeProduct ? activeProduct.rotation : 0;

    if (isCriticalFunding) {
      // 🚨 CRITICAL FUNDING: Animated glittering border with primary colors + BLINK
      const stripeWidth = 3; // Slightly narrower stripes
      const borderThickness = 2; // 2px border for visibility
      const t = performance.now() / 1000;

      // Blink effect - border blinks on/off rapidly
      const blinkPhase = Math.floor(t * 4) % 2; // Blink 4 times per second
      if (blinkPhase === 0) {
        // Skip drawing border during "off" phase
      } else {

      // Primary colors - pure RGB (fully saturated)
      const alertColors = [
        [255, 0, 0],     // Pure red
        [255, 255, 0],   // Pure yellow
        [0, 255, 0],     // Pure green (occasional)
      ];

      // Animate stripes scrolling much faster!
      const stripeOffset = Math.floor(rotation * 4) % (stripeWidth * 2);

      // Glitter effect - random white sparkles
      const glitterChance = 0.08; // 8% chance per pixel to sparkle
      const glitterSeed = Math.floor(t * 10); // Changes 10x per second for sparkle effect

      // Top border - diagonal stripes moving right
      for (let x = -stripeWidth * 2; x < screen.width + stripeWidth * 2; x++) {
        for (let y = 0; y < borderThickness; y++) {
          if (x >= 0 && x < screen.width) {
            const diagonalPos = x + y + stripeOffset;
            const stripeIndex = Math.floor(diagonalPos / stripeWidth) % 2;
            // Use red/yellow primarily, occasional green
            const colorIdx = ((x + glitterSeed) % 20 === 0) ? 2 : stripeIndex;
            let color = alertColors[colorIdx];
            // Sparkle effect - random white flashes
            const sparkleHash = ((x * 7 + y * 13 + glitterSeed) % 100) / 100;
            if (sparkleHash < glitterChance) {
              color = [255, 255, 255]; // White sparkle
            }
            ink(...color).box(x, y, 1, 1);
          }
        }
      }

      // Bottom border - diagonal stripes moving left
      for (let x = -stripeWidth * 2; x < screen.width + stripeWidth * 2; x++) {
        for (let y = 0; y < borderThickness; y++) {
          if (x >= 0 && x < screen.width) {
            const diagonalPos = x - y - stripeOffset;
            const stripeIndex = Math.floor(diagonalPos / stripeWidth) % 2;
            const colorIdx = ((x + glitterSeed + 10) % 20 === 0) ? 2 : (stripeIndex + 1) % 2;
            let color = alertColors[colorIdx];
            const sparkleHash = ((x * 11 + y * 17 + glitterSeed) % 100) / 100;
            if (sparkleHash < glitterChance) {
              color = [255, 255, 255];
            }
            ink(...color).box(x, screen.height - borderThickness + y, 1, 1);
          }
        }
      }

      // Left border - diagonal stripes moving down
      for (let y = -stripeWidth * 2; y < screen.height + stripeWidth * 2; y++) {
        for (let x = 0; x < borderThickness; x++) {
          if (y >= 0 && y < screen.height) {
            const diagonalPos = y + x + stripeOffset;
            const stripeIndex = Math.floor(diagonalPos / stripeWidth) % 2;
            const colorIdx = ((y + glitterSeed + 5) % 20 === 0) ? 2 : stripeIndex;
            let color = alertColors[colorIdx];
            const sparkleHash = ((x * 19 + y * 23 + glitterSeed) % 100) / 100;
            if (sparkleHash < glitterChance) {
              color = [255, 255, 255];
            }
            ink(...color).box(x, y, 1, 1);
          }
        }
      }

      // Right border - diagonal stripes moving up
      for (let y = -stripeWidth * 2; y < screen.height + stripeWidth * 2; y++) {
        for (let x = 0; x < borderThickness; x++) {
          if (y >= 0 && y < screen.height) {
            const diagonalPos = y - x - stripeOffset;
            const stripeIndex = Math.floor(diagonalPos / stripeWidth) % 2;
            const colorIdx = ((y + glitterSeed + 15) % 20 === 0) ? 2 : (stripeIndex + 1) % 2;
            let color = alertColors[colorIdx];
            const sparkleHash = ((x * 29 + y * 31 + glitterSeed) % 100) / 100;
            if (sparkleHash < glitterChance) {
              color = [255, 255, 255];
            }
            ink(...color).box(screen.width - borderThickness + x, y, 1, 1);
          }
        }
      }
      } // End blink "on" phase
    } else {
      // Normal mode: pink/purple/green gradient border
      // Cycle through pink, purple, green phases
      const colorPhase = (rotation * 0.08) % 3;
      let primaryColor, secondaryColor, tertiaryColor;

      if (colorPhase < 1) {
        primaryColor = [255, 100, 200]; // Pink
        secondaryColor = [200, 100, 255]; // Purple
        tertiaryColor = [100, 255, 150]; // Green
      } else if (colorPhase < 2) {
        primaryColor = [200, 100, 255]; // Purple
        secondaryColor = [100, 255, 150]; // Green
        tertiaryColor = [255, 100, 200]; // Pink
      } else {
        primaryColor = [100, 255, 150]; // Green
        secondaryColor = [255, 100, 200]; // Pink
        tertiaryColor = [200, 100, 255]; // Purple
      }

      // Pulsing effect for base intensity
      const pulseBase = Math.sin(rotation * 0.15);

      // Border extends to 2/3rds of screen
      const borderWidth = (screen.width / 3) * 2;
      const borderHeight = (screen.height / 3) * 2;
      const borderThickness = 1; // Fixed 1 pixel wide

      // Draw top border with dithered/striped pattern (1 pixel tall)
      for (let x = 0; x < borderWidth; x++) {
        // Intensity increases as we get closer to the corner (left edge)
        const intensityRatio = 1 - (x / borderWidth); // 1.0 at corner, 0.0 at edge
        const alpha = Math.floor(intensityRatio * 100 + 30 + pulseBase * 30); // 30-160 range

        // Dither pattern: show pixels based on intensity ratio
        const patternValue = (x + Math.floor(rotation / 4)) % 4; // 0-3 pattern
        const threshold = (1 - intensityRatio) * 4; // 0-4 threshold

        if (patternValue >= threshold) {
          ink(...primaryColor, alpha).box(x, 0, 1, borderThickness);
        }
      }

      // Draw left border with dithered/striped pattern (1 pixel wide)
      for (let y = 0; y < borderHeight; y++) {
        // Intensity increases as we get closer to the corner (top edge)
        const intensityRatio = 1 - (y / borderHeight); // 1.0 at corner, 0.0 at edge
        const alpha = Math.floor(intensityRatio * 100 + 30 + pulseBase * 30); // 30-160 range

        // Dither pattern: show pixels based on intensity ratio
        const patternValue = (y + Math.floor(rotation / 4)) % 4; // 0-3 pattern
        const threshold = (1 - intensityRatio) * 4; // 0-4 threshold

        if (patternValue >= threshold) {
          ink(...primaryColor, alpha).box(0, y, borderThickness, 1);
        }
      }
    }

    // 🎆 Spawn particles from the cursor position and draw cursor overlay
    const input = $.system.prompt.input;
    if (input?.prompt) {
      const cursorPos = input.prompt.pos(undefined, true);

      if (cursorPos && cursorPos.x !== undefined && cursorPos.y !== undefined) {
        // Spawn particles 30% of the time
        if (Math.random() < 0.3) {
          // Match cursor color - green for kidlisp, theme color otherwise
          const isDark = $.dark;
          const isKidlisp = $.system?.prompt?.actualKidlisp;
          let particleColor;

          // Every few particles (20% chance), use a bright primary color
          if (Math.random() < 0.2) {
            const brightColors = [
              [255, 0, 0],     // Red
              [255, 255, 0],   // Yellow
              [0, 255, 0],     // Green
              [0, 255, 255],   // Cyan
              [0, 0, 255],     // Blue
              [255, 0, 255],   // Magenta
            ];
            particleColor = brightColors[Math.floor(Math.random() * brightColors.length)];
          } else if (isKidlisp) {
            // Green for kidlisp - brighter in light mode
            particleColor = isDark ? [100, 255, 100] : [0, 200, 0];
          } else {
            // Use palette block color or fallback to theme text color
            // In light mode, make particles more vibrant
            const blockColor = input.pal?.block || (isDark ? [255, 255, 255] : [0, 0, 0]);
            if (isDark) {
              particleColor = Array.isArray(blockColor) ? blockColor.slice(0, 3) : [255, 255, 255];
            } else {
              // Light mode: boost saturation for more vibrant particles
              particleColor = Array.isArray(blockColor) ? blockColor.slice(0, 3).map(c => Math.min(255, c * 1.5)) : [255, 0, 255];
            }
          }

          cornerParticles.push({
            x: cursorPos.x + Math.random() * cursorPos.w, // Spawn across bottom of cursor box
            y: cursorPos.y + cursorPos.h, // Start at bottom of cursor
            vx: (Math.random() - 0.5) * 0.5, // Minimal horizontal drift
            vy: Math.random() * 1.5 + 0.5, // Longer vertical fall (0.5-2 pixels per frame)
            life: 1.0, // Full life
            color: particleColor,
            size: 1, // Always 1 pixel
          });
        }
      }
    }

    // Update and draw particles
    cornerParticles = cornerParticles.filter(p => {
      // Update position
      p.x += p.vx;
      p.y += p.vy;
      p.life -= 0.02; // Fade out

      // Draw particle
      if (p.life > 0) {
        const alpha = Math.floor(p.life * 200);
        ink(...p.color, alpha).box(p.x, p.y, p.size, p.size);
      }

      // Keep particle if still alive
      return p.life > 0;
    });

    // 💡 Draw semi-transparent ghost cursor when curtain is up
    if (input?.prompt) {
      const cursorPos = input.prompt.pos(undefined, true);

      if (cursorPos && cursorPos.x !== undefined && cursorPos.y !== undefined) {
        const isDark = $.dark;
        // Match the actual cursor color but with reduced opacity
        const isKidlisp = $.system?.prompt?.actualKidlisp;
        let fillColor;

        if (isKidlisp) {
          // Green for kidlisp (matching the actual cursor)
          fillColor = isDark ? [100, 255, 100, 100] : [0, 150, 0, 100];
        } else {
          // Use the palette block color (default cursor color)
          const blockColor = input.pal?.block || (isDark ? [255, 255, 255] : [0, 0, 0]);
          fillColor = [...blockColor, 100]; // Add alpha 100 for subtle transparency
        }

        // Draw on layer 3 to ensure it's above everything else on the curtain
        $.layer(3);

        // Draw animated primary color gradient outline (smooth wave pattern)
        const waveOffset = motdFrame * 0.05; // Slow smooth animation
        // Oscillate opacity for blinking effect
        const opacityOscillation = Math.sin(motdFrame * 0.08) * 20 + 40; // 20-60 range
        const primaryColors = [
          [255, 0, 0],     // Red
          [255, 255, 0],   // Yellow
          [0, 255, 0],     // Green
          [0, 255, 255],   // Cyan
          [0, 0, 255],     // Blue
          [255, 0, 255],   // Magenta
        ];

        // Top edge
        for (let x = cursorPos.x - 1; x < cursorPos.x + cursorPos.w + 1; x++) {
          const wave = (x + waveOffset) * 0.3;
          const colorIndex = Math.floor(wave) % primaryColors.length;
          const nextColorIndex = (colorIndex + 1) % primaryColors.length;
          const blend = wave - Math.floor(wave);

          const color = primaryColors[colorIndex];
          const nextColor = primaryColors[nextColorIndex];
          const r = Math.floor(color[0] * (1 - blend) + nextColor[0] * blend);
          const g = Math.floor(color[1] * (1 - blend) + nextColor[1] * blend);
          const b = Math.floor(color[2] * (1 - blend) + nextColor[2] * blend);

          ink(r, g, b, opacityOscillation).box(x, cursorPos.y - 1, 1, 1);
        }

        // Bottom edge
        for (let x = cursorPos.x - 1; x < cursorPos.x + cursorPos.w + 1; x++) {
          const wave = (x + waveOffset) * 0.3;
          const colorIndex = Math.floor(wave) % primaryColors.length;
          const nextColorIndex = (colorIndex + 1) % primaryColors.length;
          const blend = wave - Math.floor(wave);

          const color = primaryColors[colorIndex];
          const nextColor = primaryColors[nextColorIndex];
          const r = Math.floor(color[0] * (1 - blend) + nextColor[0] * blend);
          const g = Math.floor(color[1] * (1 - blend) + nextColor[1] * blend);
          const b = Math.floor(color[2] * (1 - blend) + nextColor[2] * blend);

          ink(r, g, b, opacityOscillation).box(x, cursorPos.y + cursorPos.h, 1, 1);
        }

        // Left edge
        for (let y = cursorPos.y - 1; y < cursorPos.y + cursorPos.h + 1; y++) {
          const wave = (y + waveOffset) * 0.3;
          const colorIndex = Math.floor(wave) % primaryColors.length;
          const nextColorIndex = (colorIndex + 1) % primaryColors.length;
          const blend = wave - Math.floor(wave);

          const color = primaryColors[colorIndex];
          const nextColor = primaryColors[nextColorIndex];
          const r = Math.floor(color[0] * (1 - blend) + nextColor[0] * blend);
          const g = Math.floor(color[1] * (1 - blend) + nextColor[1] * blend);
          const b = Math.floor(color[2] * (1 - blend) + nextColor[2] * blend);

          ink(r, g, b, opacityOscillation).box(cursorPos.x - 1, y, 1, 1);
        }

        // Right edge
        for (let y = cursorPos.y - 1; y < cursorPos.y + cursorPos.h + 1; y++) {
          const wave = (y + waveOffset) * 0.3;
          const colorIndex = Math.floor(wave) % primaryColors.length;
          const nextColorIndex = (colorIndex + 1) % primaryColors.length;
          const blend = wave - Math.floor(wave);

          const color = primaryColors[colorIndex];
          const nextColor = primaryColors[nextColorIndex];
          const r = Math.floor(color[0] * (1 - blend) + nextColor[0] * blend);
          const g = Math.floor(color[1] * (1 - blend) + nextColor[1] * blend);
          const b = Math.floor(color[2] * (1 - blend) + nextColor[2] * blend);

          ink(r, g, b, opacityOscillation).box(cursorPos.x + cursorPos.w, y, 1, 1);
        }

        // Draw filled cursor box with subtle transparency (no outline)
        ink(...fillColor).box(cursorPos.x, cursorPos.y, cursorPos.w, cursorPos.h);

        // 🎯 TAP/TOUCH/TYPE vertical label with black box pointing to cursor
        // Shows cycling action words with bounce animation and color cycling
        if (input.text === "" && !input.canType) {
          const isDark = $.dark;
          const now = performance.now();

          // Update animation frame counter
          if (!lastLanguageChangeTime) lastLanguageChangeTime = now;
          const deltaTime = (now - lastLanguageChangeTime) / 1000;
          lastLanguageChangeTime = now;
          promptLanguageChangeFrame += deltaTime * 60;

          // Position reference near cursor
          const textY = cursorPos.y + cursorPos.h + 4;

          // High contrast polychrome colors - cycle through vibrant colors
          const textColorCycle = isDark ? [
            [255, 100, 255], // Bright magenta
            [100, 255, 255], // Bright cyan
            [255, 255, 100], // Bright yellow
            [100, 255, 100], // Bright green
            [255, 150, 100], // Bright orange
            [255, 100, 150], // Bright pink
          ] : [
            [255, 0, 255],   // Pure magenta
            [0, 255, 255],   // Pure cyan
            [255, 255, 0],   // Pure yellow
            [0, 255, 0],     // Pure green
            [255, 128, 0],   // Pure orange
            [255, 0, 128],   // Pure pink
          ];

          // Vertical action text on LEFT side of screen - ROTATED
          const actionPhrases = ["tap 2 prompt", "touch 2 prompt", "type to prompt"];
          const actionIndex = Math.floor(promptLanguageChangeFrame / 60) % actionPhrases.length;
          // Jumble casing per-character for attention
          let actionText = "";
          const phrase = actionPhrases[actionIndex];
          for (let ci = 0; ci < phrase.length; ci++) {
            const ch = phrase[ci];
            // Use a time-varying seed per character for animated case jumbling
            const caseSeed = Math.sin(promptLanguageChangeFrame * 0.12 + ci * 1.7);
            actionText += caseSeed > 0 ? ch.toUpperCase() : ch.toLowerCase();
          }

          // Bounce animation - subtle up and down movement
          const bounceOffset = Math.sin(promptLanguageChangeFrame * 0.1) * 3;

          // Calculate text dimensions for background
          const verticalTextWidth = actionText.length * 4; // MatrixChunky8 width per char
          const verticalTextHeight = 8; // MatrixChunky8 height

          // Position flush to left side
          const leftMargin = 5;
          const verticalTextX = leftMargin;

          // Position label box below cursor with triangle tip
          const arrowHeight = 10;
          const arrowTipY = textY + 3;
          const bgX = leftMargin - 1;
          const arrowTipYWithBounce = arrowTipY + bounceOffset;
          const bgYWithBounce = arrowTipYWithBounce + arrowHeight;
          const verticalBgWidth = verticalTextHeight + 1;
          const verticalBgHeight = verticalTextWidth + 7;

          // Position text inside the box (accounting for rotation)
          const verticalTextY = bgYWithBounce + verticalTextWidth + 1 + bounceOffset;

          // Draw black label box with triangle tip pointing up
          ink(0, 0, 0, 200).shape([
            [bgX, bgYWithBounce],
            [bgX, arrowTipYWithBounce],
            [bgX + verticalBgWidth, bgYWithBounce],
            [bgX + verticalBgWidth, bgYWithBounce + verticalBgHeight],
            [bgX, bgYWithBounce + verticalBgHeight],
            [bgX, bgYWithBounce]
          ]);

          // Draw white text with flickering per-character brightness
          let flickeringText = "";
          for (let i = 0; i < actionText.length; i++) {
            const char = actionText[i];
            const flickerSeed = Math.sin((promptLanguageChangeFrame * 0.15) + (i * 0.5));
            const brightness = Math.floor(200 + flickerSeed * 55);
            flickeringText += `\\${brightness},${brightness},${brightness}\\${char}`;
          }

          ink(255, 255, 255, 150).write(flickeringText, {
            x: verticalTextX,
            y: verticalTextY,
            rotation: 270
          }, undefined, undefined, false, "MatrixChunky8");

          // Color cycling for cursor overlay
          const blinkColorIndex = Math.floor(promptLanguageChangeFrame * 0.05) % textColorCycle.length;

          // Blinking overlay on the cursor block - magic jewel/gem effect
          const blinkSpeed = Math.sin(promptLanguageChangeFrame * 0.08);
          const blinkAlpha = isDark
            ? Math.floor((blinkSpeed + 1) * 60 + 60)
            : Math.floor((blinkSpeed + 1) * 80 + 100);

          // Rhythmic subdivision pattern - changes every 30 frames
          const patternPhase = Math.floor(promptLanguageChangeFrame / 30) % 4;
          let cols, rows;

          if (patternPhase === 0) {
            cols = 2; rows = 2;
          } else if (patternPhase === 1) {
            cols = 4; rows = 2;
          } else if (patternPhase === 2) {
            cols = 2; rows = 4;
          } else {
            cols = 3; rows = 3;
          }

          const cellWidth = cursorPos.w / cols;
          const cellHeight = cursorPos.h / rows;

          for (let row = 0; row < rows; row++) {
            for (let col = 0; col < cols; col++) {
              const isCheckerSquare = ((col + row + Math.floor(promptLanguageChangeFrame / 20)) % 2 === 0);
              if (isCheckerSquare) {
                const squareColorIndex = (blinkColorIndex + col + row) % textColorCycle.length;
                const squareColor = [...textColorCycle[squareColorIndex], blinkAlpha];
                const cellX = cursorPos.x + Math.floor(col * cellWidth);
                const cellY = cursorPos.y + Math.floor(row * cellHeight);
                const cellW = Math.floor(cellWidth);
                const cellH = Math.floor(cellHeight);
                ink(...squareColor).box(cellX, cellY, cellW, cellH);
              }
            }
          }
        }

        // Reset to default layer
        $.layer(1);
      }
    }
  }

  if (showLoginCurtain) {
    // Paint current status color.
    // if (!$.system.prompt.input.canType) {
    starfield.paint($, {
      alpha: $.dark ? 0.3 : 0.9,
      color: $.hud.currentStatusColor() || [255, 0, 200],
    });

    // Stats / Analytics - UNITICKER System (unified ticker combining chat, laer-klokken, and media)
    $.layer(2); // Render ticker on top of tooltips

    const loginY = screen.height / 2;

    // Calculate dynamic positioning
    const tickerHeight = 8; // MatrixChunky8 is 8px
    const tickerPadding = 5; // Padding around ticker
    const tickerFont = "MatrixChunky8";

    // Helper to ensure ticker text is long enough to fill screen without gaps
    // Repeats the content until it's at least 4x screen width for seamless looping
    const ensureMinTickerLength = (text, separator = " · ") => {
      if (!text) return text;
      const charWidth = 4; // MatrixChunky8 char width estimate
      const minWidth = screen.width * 4; // 4x screen width for smooth seamless loop
      const textWidthApprox = text.length * charWidth;
      if (textWidthApprox < minWidth) {
        const repeats = Math.ceil(minWidth / textWidthApprox) + 1;
        return Array(repeats).fill(text).join(separator);
      }
      return text;
    };

    const unitickerY = loginY + 44; // Position below login
    let contentTickerY = unitickerY; // Y position of content ticker (declared here for tooltip access)

    // Build combined uniticker items from all sources
    // Types: 'chat' (blue), 'clock' (orange), 'kidlisp' ($), 'painting' (#), 'tape' (!)
    const hasChatMessages = $.chat?.messages && $.chat.messages.length > 0;
    const hasClockMessages = clockChatMessages && clockChatMessages.length > 0;
    const hasMediaItems = contentItems.length > 0;

    // Build unified items array - collect all items first, then interleave for frecency-style mixing
    const chatItems = [];
    const clockItems = [];
    const mediaItems = [];
    const commitItems = [];
    const statsItems = [];
    const moodItems = [];

    // Collect chat messages (blue) - code: 'chat'
    if (!isCriticalFunding && hasChatMessages) {
      const numMessages = Math.min(6, $.chat.messages.length);
      const recentMessages = $.chat.messages.slice(-numMessages);
      recentMessages.forEach(msg => {
        const sanitizedText = msg.text.replace(/[\r\n]+/g, ' ');
        const displayText = msg.from + ": " + sanitizedText.slice(0, 50) + (sanitizedText.length > 50 ? "..." : "");
        chatItems.push({
          type: 'chat',
          text: displayText,
          code: 'chat',
          color: [100, 200, 255], // Bright blue
        });
      });
    }

    // Collect clock messages (orange) - code: 'laer-klokken'
    if (!isCriticalFunding && hasClockMessages) {
      const numClockMessages = Math.min(4, clockChatMessages.length);
      const recentClockMessages = clockChatMessages.slice(-numClockMessages);
      recentClockMessages.forEach(msg => {
        const sanitizedText = (msg.text || msg.message || '').replace(/[\r\n]+/g, ' ');
        const from = msg.from || msg.handle || msg.author || 'anon';
        const displayText = from + ": " + sanitizedText.slice(0, 50) + (sanitizedText.length > 50 ? "..." : "");
        clockItems.push({
          type: 'clock',
          text: displayText,
          code: 'laer-klokken',
          color: [255, 180, 80], // Bright orange
        });
      });
    }

    // Collect media items (kidlisp, painting, tape)
    if (hasMediaItems) {
      contentItems.forEach(item => {
        let prefix, color, code;
        if (item.type === 'kidlisp') {
          prefix = '$';
          color = [150, 255, 150]; // Bright lime green
          code = `$${item.code}`;
        } else if (item.type === 'painting') {
          prefix = '#';
          color = [255, 150, 255]; // Bright magenta
          // Determine proper code for paintings
          if (!item.handle || item.handle === 'undefined' || item.handle === 'null') {
            code = `painting#${item.code}`;
          } else {
            code = `#${item.code}`;
          }
        } else { // tape
          prefix = '!';
          color = [255, 200, 100]; // Bright orange/yellow
          code = `!${item.code}`;
        }
        mediaItems.push({
          type: item.type,
          text: `${prefix}${item.code}`,
          code: code,
          color: color,
          mediaItem: item, // Keep reference to original item for media preview
        });
      });
    }

    // Collect recent commits (lime green for hash and author visibility)
    if (recentCommits.length > 0) {
      const numCommits = Math.min(5, recentCommits.length);
      recentCommits.slice(0, numCommits).forEach(commit => {
        // Map known authors to their handles
        let author = commit.author;
        if (author === 'Jeffrey Alan Scudder') author = '@jeffrey';
        const displayText = `[${commit.hash}] ${commit.message} ~${author}`;
        commitItems.push({
          type: 'commit',
          text: displayText,
          code: 'commits',
          color: [100, 255, 100], // Lime green for commits
        });
      });
    }

    // Collect handles count (magenta/pink for visibility)
    if (handles) {
      const handlesText = `${handles.toLocaleString()} handles`;
      statsItems.push({
        type: 'stats',
        text: handlesText,
        code: 'handles',
        color: [255, 100, 255], // Magenta/pink for handles stat
      });
    }

    // Collect moods (cyan/teal) - code: 'mood'
    if (motdCandidates.length > 0) {
      motdCandidates.forEach(candidate => {
        if (!candidate?.mood) return;
        const moodText = candidate.mood.replace(/[\r\n]+/g, ' ').slice(0, 60);
        const handle = candidate.handle || candidate.handleInfo?.handle || candidate.owner?.handle || candidate.user?.handle || null;
        const from = handle ? (handle.startsWith("@") ? handle : `@${handle}`) : 'anon';
        moodItems.push({
          type: 'mood',
          text: `${from}: ${moodText}`,
          code: 'mood',
          color: [100, 255, 220], // Cyan/teal for moods
        });
      });
    }

    // 🎲 Interleave items for frecency-style distribution
    // Round-robin through all item types to ensure good mixing
    // Currently limited to: chats, laer-klokkens, and moods
    unitickerItems = [];
    const allBuckets = [chatItems, clockItems, moodItems].filter(b => b.length > 0);
    const bucketIndices = allBuckets.map(() => 0);
    const totalItems = allBuckets.reduce((sum, b) => sum + b.length, 0);
    
    // Distribute items by cycling through buckets
    let placed = 0;
    let bucketCursor = 0;
    while (placed < totalItems) {
      // Find next bucket with items remaining
      let attempts = 0;
      while (bucketIndices[bucketCursor] >= allBuckets[bucketCursor].length && attempts < allBuckets.length) {
        bucketCursor = (bucketCursor + 1) % allBuckets.length;
        attempts++;
      }
      if (attempts >= allBuckets.length) break; // All buckets exhausted
      
      // Take item from current bucket
      const bucket = allBuckets[bucketCursor];
      const idx = bucketIndices[bucketCursor];
      if (idx < bucket.length) {
        unitickerItems.push(bucket[idx]);
        bucketIndices[bucketCursor]++;
        placed++;
      }
      
      // Move to next bucket for round-robin
      bucketCursor = (bucketCursor + 1) % allBuckets.length;
    }

    // Show uniticker if we have any items
    const showUniticker = screen.height >= 180 && unitickerItems.length > 0;

    if (showUniticker) {
      // Build full text for ticker (just for width calculation)
      const fullText = unitickerItems.map(item => item.text).join(" · ");

      // Create or update uniticker instance
      if (!uniticker) {
        mediaPreviewBox = new MediaPreviewBox(); // Initialize shared preview box
        uniticker = new $.gizmo.Ticker(fullText, {
          speed: 1, // 1px per frame
          separator: " · ",
        });
        uniticker.paused = false;
        uniticker.offset = 0;
      } else {
        uniticker.setText(fullText);
      }

      // Update ticker animation
      if (uniticker && !uniticker.paused) {
        uniticker.update($);
      }

      // Create or update invisible button over ticker area
      const boxHeight = tickerHeight + (tickerPadding * 2);
      const boxY = unitickerY - tickerPadding;

      if (!unitickerButton) {
        unitickerButton = new $.ui.Button({
          x: 0,
          y: boxY,
          w: screen.width,
          h: boxHeight,
        });
        unitickerButton.noEdgeDetection = true;
        unitickerButton.noRolloverActivation = true;
        unitickerButton.stickyScrubbing = true;
      } else {
        unitickerButton.box.x = 0;
        unitickerButton.box.y = boxY;
        unitickerButton.box.w = screen.width;
        unitickerButton.box.h = boxHeight;
      }

      // Paint background and borders
      if (!unitickerButton.disabled) {
        // Dark background for high contrast (brighter when hovering)
        const isTickerHover = unitickerButton.over && !unitickerButton.down;
        const bgAlpha = unitickerButton.down ? 100 : (isTickerHover ? 80 : 60);
        ink([20, 20, 30, bgAlpha]).box(0, boxY, screen.width, boxHeight - 1);

        // Subtle top and bottom borders (purple/pink tint - brighter on hover)
        const borderAlpha = isTickerHover ? 230 : 180;
        const borderColor = isTickerHover ? [180, 120, 230, borderAlpha] : [150, 100, 200, borderAlpha];
        ink(borderColor).line(0, boxY, screen.width, boxY);
        ink(borderColor).line(0, boxY + boxHeight - 1, screen.width, boxY + boxHeight - 1);

        // Render items with individual colors and hover detection
        const textY = unitickerY;
        const tickerAlpha = unitickerButton.down ? 255 : 220;

        // Calculate per-item widths and total cycle width
        const separator = " · ";
        const separatorWidth = $.text.box(separator, undefined, undefined, undefined, undefined, tickerFont).box.width;
        let itemWidths = [];
        let totalCycleWidth = 0;
        unitickerItems.forEach((item, idx) => {
          const textWidth = $.text.box(item.text, undefined, undefined, undefined, undefined, tickerFont).box.width;
          itemWidths.push(textWidth);
          totalCycleWidth += textWidth;
          if (idx < unitickerItems.length - 1) {
            totalCycleWidth += separatorWidth;
          }
        });
        totalCycleWidth += separatorWidth; // Add trailing separator for seamless loop

        // Use modulo offset for seamless looping
        const rawOffset = uniticker.getOffset();
        const loopedOffset = totalCycleWidth > 0 ? (rawOffset % totalCycleWidth) : 0;

        // Track hovered item and visible items for auto-selection
        let hoveredItem = null;
        let hoveredItemX = 0;
        let hoveredItemWidth = 0;
        let visibleItemsForAutoSelect = []; // Track all visible items with positions for idle auto-select

        // Track idle state - detect pen movement
        const penX = $.pen?.x ?? -1;
        const penY = $.pen?.y ?? -1;
        if (penX !== unitickerLastPenX || penY !== unitickerLastPenY) {
          unitickerIdleFrames = 0;
          unitickerLastPenX = penX;
          unitickerLastPenY = penY;
          // Clear auto-selection when user moves mouse
          if (hoveredItem) {
            unitickerAutoSelectedItem = null;
          }
        } else {
          unitickerIdleFrames++;
        }

        // Calculate starting X position with looped offset
        const startMargin = 6;
        let baseX = startMargin - loopedOffset;

        // Render enough cycles to fill screen plus buffer
        const numCycles = Math.ceil((screen.width + totalCycleWidth) / totalCycleWidth) + 1;

        for (let cycle = 0; cycle < numCycles; cycle++) {
          let currentX = baseX + (cycle * totalCycleWidth);

          unitickerItems.forEach((item, idx) => {
            const text = item.text;
            const color = item.color;
            const textWidth = itemWidths[idx];

            // Check if mouse is hovering over this item
            const mouseX = $.pen?.x ?? -1;
            const mouseY = $.pen?.y ?? -1;
            const isHovered = mouseX >= currentX &&
                             mouseX < currentX + textWidth &&
                             mouseY >= boxY &&
                             mouseY < boxY + boxHeight;

            // Track hovered item (prefer items visible on screen)
            if (isHovered && currentX >= 0 && currentX < screen.width) {
              hoveredItem = item;
              hoveredItemX = currentX;
              hoveredItemWidth = textWidth;
            }

            // Track visible items for auto-selection (items entering from right side)
            if (currentX >= 0 && currentX < screen.width) {
              visibleItemsForAutoSelect.push({
                item,
                x: currentX,
                width: textWidth,
              });
            }

            // Only render if visible on screen (with small buffer)
            const isVisible = (currentX + textWidth) > -10 && currentX < (screen.width + 10);
            if (isVisible) {
              if (isHovered && !unitickerButton.down) {
                // Highlight background on hover
                $.ink([...color, 50]).box(currentX - 1, boxY + 1, textWidth + 2, boxHeight - 3);
                // Brighter text when hovered
                $.ink(color, 255).write(text, { x: currentX, y: textY }, undefined, undefined, false, tickerFont);
              } else {
                $.ink(color, tickerAlpha).write(text, { x: currentX, y: textY }, undefined, undefined, false, tickerFont);
              }
            }

            // Move to next position
            currentX += textWidth;

            // Add separator after each item
            const sepVisible = (currentX + separatorWidth) > -10 && currentX < (screen.width + 10);
            if (sepVisible) {
              const blinkAlpha = 120 + Math.sin(motdFrame * 0.2) * 60;
              ink([180, 180, 200], blinkAlpha).write(separator, { x: currentX, y: textY }, undefined, undefined, false, tickerFont);
            }
            currentX += separatorWidth;
          });
        }

        // Store hovered item for click handler and tooltip
        unitickerHoveredItem = hoveredItem;
        unitickerButton.hoveredItem = hoveredItem;

        // 🎯 Auto-select on idle: pick a "future" item (from right side) when idle
        let displayItem = hoveredItem;
        let displayItemX = hoveredItemX;
        let displayItemWidth = hoveredItemWidth;

        if (!hoveredItem && unitickerIdleFrames >= UNITICKER_IDLE_THRESHOLD && visibleItemsForAutoSelect.length > 0) {
          // Sort by X position to find rightmost items ("future" items coming from the right)
          visibleItemsForAutoSelect.sort((a, b) => a.x - b.x);

          // Check if current auto-selected item is still visible and hasn't scrolled off left
          if (unitickerAutoSelectedItem) {
            const stillVisible = visibleItemsForAutoSelect.find(
              v => v.item === unitickerAutoSelectedItem && v.x >= -v.width * 0.3
            );
            if (stillVisible) {
              // Keep tracking the auto-selected item (tooltip scrolls with it)
              displayItem = stillVisible.item;
              displayItemX = stillVisible.x;
              displayItemWidth = stillVisible.width;
              unitickerAutoSelectedX = stillVisible.x;
              unitickerAutoSelectedWidth = stillVisible.width;
            } else {
              // Item scrolled off to the left - select next "future" item
              unitickerAutoSelectedItem = null;
            }
          }

          // If no auto-selected item, pick one from center-right area (not too eager)
          if (!unitickerAutoSelectedItem && visibleItemsForAutoSelect.length > 0) {
            // Pick item from center-right of screen (40%-70% range) - not too eager
            const minThreshold = screen.width * 0.4; // Don't pick items too far left
            const maxThreshold = screen.width * 0.7; // Don't pick items too far right (let them settle in)
            const centeredItems = visibleItemsForAutoSelect.filter(v => v.x >= minThreshold && v.x <= maxThreshold);
            
            let targetItem;
            if (centeredItems.length > 0) {
              // Pick the rightmost centered item (newest arrival that's settled)
              targetItem = centeredItems[centeredItems.length - 1];
            } else {
              // Fallback: pick item closest to center-right
              const idealX = screen.width * 0.55;
              targetItem = visibleItemsForAutoSelect.reduce((best, v) => {
                const bestDist = Math.abs(best.x - idealX);
                const vDist = Math.abs(v.x - idealX);
                return vDist < bestDist ? v : best;
              });
            }

            unitickerAutoSelectedItem = targetItem.item;
            unitickerAutoSelectedX = targetItem.x;
            unitickerAutoSelectedWidth = targetItem.width;
            displayItem = targetItem.item;
            displayItemX = targetItem.x;
            displayItemWidth = targetItem.width;
          }
        } else if (hoveredItem) {
          // User is hovering - clear auto-selection
          unitickerAutoSelectedItem = null;
        }

        // 🎯 Draw "Enter [code]" tooltip below hovered or auto-selected item (scrolls with it)
        if (displayItem && !unitickerButton.down) {
          // Generate contextual tooltip text based on item type and docs
          let tooltipPrefix, tooltipCode, tooltipSuffix;
          const doc = tooltipDocs?.[displayItem.code];
          if (doc?.desc) {
            // Use doc description with action prefix
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = `' to ${doc.desc.toLowerCase().replace(/\.$/, '')}`;
          } else if (displayItem.type === 'kidlisp') {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "' to run";
          } else if (displayItem.type === 'painting') {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "' to view";
          } else if (displayItem.type === 'tape') {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "' to listen";
          } else if (displayItem.type === 'commit') {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "' to browse";
          } else if (displayItem.type === 'stats') {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "' to browse";
          } else if (displayItem.type === 'mood') {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "' to set yours";
          } else {
            tooltipPrefix = "Enter '";
            tooltipCode = displayItem.code;
            tooltipSuffix = "'";
          }
          const tooltipText = tooltipPrefix + tooltipCode + tooltipSuffix;
          const tooltipWidth = $.text.box(tooltipText, undefined, undefined, undefined, undefined, tickerFont).box.width;
          const tooltipHeight = 10;
          const tooltipPadding = 3;

          // Position tooltip below the item, centered
          let tooltipX = displayItemX + (displayItemWidth / 2) - (tooltipWidth / 2) - tooltipPadding;
          const tooltipY = boxY + boxHeight + 10; // Below the ticker with more room for arrow

          // Clamp to screen bounds
          tooltipX = Math.max(4, Math.min(tooltipX, screen.width - tooltipWidth - tooltipPadding * 2 - 4));

          // Draw tooltip background (slightly dimmer for auto-selected to indicate passive state)
          const alphaMultiplier = hoveredItem ? 1 : 0.7;
          const tooltipBgColor = [...displayItem.color, Math.round(180 * alphaMultiplier)];
          ink([10, 10, 20, Math.round(220 * alphaMultiplier)]).box(tooltipX, tooltipY, tooltipWidth + tooltipPadding * 2, tooltipHeight + tooltipPadding * 2);
          ink(tooltipBgColor).box(tooltipX, tooltipY, tooltipWidth + tooltipPadding * 2, tooltipHeight + tooltipPadding * 2, "inline");

          // Draw tooltip text with syntax highlighting for the quoted code
          const textY = tooltipY + tooltipPadding + 1;
          let textX = tooltipX + tooltipPadding;
          const baseAlpha = Math.round(255 * alphaMultiplier);
          const dimAlpha = Math.round(180 * alphaMultiplier);
          
          // Draw prefix in dimmer white
          ink([255, 255, 255, dimAlpha]).write(tooltipPrefix, { x: textX, y: textY }, undefined, undefined, false, tickerFont);
          textX += $.text.box(tooltipPrefix, undefined, undefined, undefined, undefined, tickerFont).box.width;
          
          // Draw code in bright highlight color (use item's color for consistency)
          ink([...displayItem.color, baseAlpha]).write(tooltipCode, { x: textX, y: textY }, undefined, undefined, false, tickerFont);
          textX += $.text.box(tooltipCode, undefined, undefined, undefined, undefined, tickerFont).box.width;
          
          // Draw suffix in dimmer white
          ink([255, 255, 255, dimAlpha]).write(tooltipSuffix, { x: textX, y: textY }, undefined, undefined, false, tickerFont);

          // Draw small arrow pointing up to the item
          const arrowX = displayItemX + (displayItemWidth / 2);
          const arrowY = tooltipY;
          const arrowAlpha = Math.round(200 * alphaMultiplier);
          ink([...displayItem.color, arrowAlpha]).line(arrowX, arrowY, arrowX - 3, arrowY - 3);
          ink([...displayItem.color, arrowAlpha]).line(arrowX, arrowY, arrowX + 3, arrowY - 3);

          // Draw subtle highlight on auto-selected item (when not hovering)
          if (!hoveredItem && unitickerAutoSelectedItem) {
            $.ink([...displayItem.color, 30]).box(displayItemX - 1, boxY + 1, displayItemWidth + 2, boxHeight - 3);
          }
        }
      }
    } else {
      uniticker = null;
      unitickerButton = null;
      unitickerHoveredItem = null;
      unitickerAutoSelectedItem = null; // Clear auto-selection when ticker is hidden
      unitickerIdleFrames = 0;
      currentTooltipItem = null; // Clear when ticker is hidden
      tooltipTimer = 0;
      if (activeTapePreview) {
        releaseActiveTapePreview("ticker-hidden");
      }
    }

    $.layer(1); // Reset layer back to 1 for tooltips

    // 🎨 CONTENT TOOLTIP (ambient floating preview)
    // Automatically cycles through content items (KidLisp + Paintings), showing previews
    // Now uses uniticker instead of the old contentTicker
    if (!DISABLE_CONTENT_PREVIEWS && showUniticker && contentItems.length > 0) {
      // Filter to only media items (not chat/clock messages) for preview
      const visibleItems = [];

      if (uniticker) {
        const offset = uniticker.getOffset();
        let currentX = 6 - offset;

        // Only show previews for media items (kidlisp, painting, tape)
        const mediaItems = unitickerItems.filter(item => 
          item.type === 'kidlisp' || item.type === 'painting' || item.type === 'tape'
        );

        mediaItems.forEach(item => {
          const text = item.text;
          const textWidth = $.text.box(text, undefined, undefined, undefined, undefined, tickerFont).box.width;

          // Check if this item is visible on screen (prefer right side)
          const rightBiasStart = (screen.width) * 0.3;
          if (currentX >= rightBiasStart && currentX < screen.width) {
            // Use original media item object if available
            const mediaItem = item.mediaItem || item;
            mediaItem.screenX = currentX;
            visibleItems.push(mediaItem);
          }

          currentX += textWidth + $.text.box(" · ", undefined, undefined, undefined, undefined, tickerFont).box.width;
        });
      }

      if (visibleItems.length > 0) {
        if (activeTapePreview && currentTooltipItem && activeTapePreview !== currentTooltipItem) {
          releaseActiveTapePreview("tooltip-switch");
        }

        // Tape-specific durations (tapes are heavy - need more time to load and display)
        const baseDuration = 180; // Show kidlisp/paintings for 3 seconds (at 60fps)
        const tapeDuration = 480; // Show tapes for 8 seconds (at 60fps)
        const displayDuration = currentTooltipItem?.type === 'tape' ? tapeDuration : baseDuration;
        const fadeDuration = 20; // Fade in/out over ~0.33 seconds

        // Initialize ONLY if we don't have a tooltip yet
        if (!currentTooltipItem) {
          // Find first item that hasn't failed or try first one
          let startItem = visibleItems.find(i => !i.fetchFailed) || visibleItems[0];
          currentTooltipItem = startItem;
          tooltipItemIndex = visibleItems.indexOf(startItem);
          tooltipTimer = 0;

          // Fetch source/image/audio for current item
          if (currentTooltipItem.type === 'kidlisp' && !currentTooltipItem.source && !currentTooltipItem.fetchAttempted) {
            fetchKidlispSource(currentTooltipItem, $);
          } else if (currentTooltipItem.type === 'painting' && !currentTooltipItem.image && !currentTooltipItem.fetchAttempted) {
            fetchPaintingImage(currentTooltipItem, $);
          } else if (currentTooltipItem.type === 'tape' && !currentTooltipItem.audioUrl && !currentTooltipItem.fetchAttempted) {
            enqueueTapePreview(currentTooltipItem, $);
          }

          console.log(`🎨 Tooltip initialized with ${currentTooltipItem.type} item: ${currentTooltipItem.type === 'kidlisp' ? '$' : currentTooltipItem.type === 'painting' ? '#' : '!'}${currentTooltipItem.code}`);
        }

        // Pre-fetch next items (always do this, even while waiting for current)
        for (let i = 1; i <= 2; i++) {
          const nextIndex = (tooltipItemIndex + i) % visibleItems.length;
          const nextItem = visibleItems[nextIndex];
          if (nextItem) {
            if (nextItem.type === 'kidlisp' && !nextItem.source && !nextItem.fetchAttempted && !nextItem.fetchFailed) {
              fetchKidlispSource(nextItem, $);
            } else if (nextItem.type === 'painting' && !nextItem.image && !nextItem.fetchAttempted && !nextItem.fetchFailed) {
              fetchPaintingImage(nextItem, $);
            } else if (nextItem.type === 'tape' && !nextItem.audioUrl && !nextItem.fetchAttempted && !nextItem.fetchFailed) {
              enqueueTapePreview(nextItem, $);
            }
          }
        }

        // Only increment timer if current item has source/image or is a loading/loaded tape
        if (currentTooltipItem.source || currentTooltipItem.image || currentTooltipItem.isLoading || currentTooltipItem.framesLoaded) {
          tooltipTimer++;
        }

        // Switch to next item after display duration OR if current item failed
        if (tooltipTimer > displayDuration || currentTooltipItem.fetchFailed) {
          // Find next item with source already loaded (don't switch until ready)
          let attempts = 0;
          let foundNext = false;
          const startIndex = tooltipItemIndex;

          while (attempts < visibleItems.length && !foundNext) {
            const checkIndex = (startIndex + attempts + 1) % visibleItems.length;
            const nextItem = visibleItems[checkIndex];

            // Only switch to items with source/image/audio or loading tapes
            const hasContent = (nextItem.type === 'kidlisp' && nextItem.source) ||
                              (nextItem.type === 'painting' && nextItem.image) ||
                              (nextItem.type === 'tape' && (nextItem.isLoading || nextItem.framesLoaded));
            if (hasContent && !nextItem.fetchFailed) {
              tooltipTimer = 0;
              tooltipItemIndex = checkIndex;
              currentTooltipItem = nextItem;
              foundNext = true;
              const prefix = nextItem.type === 'kidlisp' ? '$' : nextItem.type === 'painting' ? '#' : '!';
              console.log(`🎨 Tooltip cycling to ${nextItem.type} item ${tooltipItemIndex + 1}/${visibleItems.length}: ${prefix}${currentTooltipItem.code}`);
            } else if (!nextItem.fetchAttempted && !nextItem.fetchFailed) {
              // Try to fetch if not attempted yet
              if (nextItem.type === 'kidlisp') {
                fetchKidlispSource(nextItem, $);
              } else if (nextItem.type === 'painting') {
                fetchPaintingImage(nextItem, $);
              } else if (nextItem.type === 'tape') {
                enqueueTapePreview(nextItem, $);
              }
            }

            attempts++;
          }

          // If no loaded item found, wait at full display (don't fade out)
          if (!foundNext) {
            tooltipTimer = displayDuration; // Hold at full opacity
            console.log(`🎨 Waiting for next item to load...`);
          }
        } else if (!currentTooltipItem.source && !currentTooltipItem.image && !currentTooltipItem.isLoading && !currentTooltipItem.framesLoaded) {
          // Waiting for source/image/tape to load - check if it's taking too long
          if (currentTooltipItem.fetchAttempted && !currentTooltipItem.fetchFailed) {
            // Tapes need longer timeout (heavy ZIPs with many frames)
            const baseWaitTime = 120; // 2 seconds at 60fps for kidlisp/paintings
            const tapeWaitTime = 600; // 10 seconds at 60fps for tapes (heavy ZIPs)
            const waitTime = currentTooltipItem.type === 'tape' ? tapeWaitTime : baseWaitTime;
            if (tooltipTimer > waitTime) {
              const prefix = currentTooltipItem.type === 'kidlisp' ? '$' : currentTooltipItem.type === 'painting' ? '#' : '!';
              console.log(`🎨 Timeout waiting for ${prefix}${currentTooltipItem.code}, marking as failed`);
              currentTooltipItem.fetchFailed = true;
              // Switch logic will run on next frame
            } else {
              tooltipTimer++; // Keep counting while waiting
            }
          }
          // If not yet attempted, don't increment timer - just wait for fetch to start
        }

        // Calculate fade in/out ONLY when we have source or image or audioUrl or loading tape
        if (currentTooltipItem.source || currentTooltipItem.image || currentTooltipItem.isLoading || currentTooltipItem.framesLoaded) {
          if (tooltipTimer < fadeDuration) {
            // Fade in
            tooltipFadeIn = tooltipTimer / fadeDuration;
          } else if (tooltipTimer > displayDuration - fadeDuration) {
            // Fade out (only if we have a next item ready)
            tooltipFadeIn = (displayDuration - tooltipTimer) / fadeDuration;
          } else {
            // Full opacity
            tooltipFadeIn = 1;
          }
        } else {
          tooltipFadeIn = 0; // Don't show until loaded
        }

        // Render media preview as centered, faded background element
        if (currentTooltipItem && (currentTooltipItem.source || currentTooltipItem.image || currentTooltipItem.isLoading || currentTooltipItem.framesLoaded) && tooltipFadeIn > 0) {
          $.layer(0); // Render behind everything

          // Calculate preview dimensions based on type
          let tooltipWidth, tooltipHeight;

          // Pre-calculate metadata text to determine width
          let timestampText = '';
          if (currentTooltipItem.timestamp) {
            const now = new Date();
            const past = new Date(currentTooltipItem.timestamp);
            const seconds = Math.floor((now - past) / 1000);

            const units = [
              { name: "year", seconds: 31536000 },
              { name: "month", seconds: 2592000 },
              { name: "week", seconds: 604800 },
              { name: "day", seconds: 86400 },
              { name: "hour", seconds: 3600 },
              { name: "minute", seconds: 60 },
              { name: "second", seconds: 1 },
            ];

            for (const unit of units) {
              const count = Math.floor(seconds / unit.seconds);
              if (count >= 1) {
                timestampText = `${count} ${unit.name}${count > 1 ? "s" : ""} ago`;
                break;
              }
            }
            if (!timestampText) timestampText = "just now";
          }

          const authorHandle =
            currentTooltipItem.author ||
            currentTooltipItem.handle ||
            currentTooltipItem.owner?.handle ||
            null;
          const authorText = authorHandle
            ? authorHandle.startsWith("@")
              ? authorHandle
              : `@${authorHandle}`
            : null;

          // Add tape code to metadata for tape items
          const tapeCodeText = currentTooltipItem.type === 'tape' ? `!${currentTooltipItem.code}` : null;

          // Build metadata with code for tapes: "!code · timestamp · @author"
          let metadataText;
          if (currentTooltipItem.type === 'tape') {
            const parts = [tapeCodeText, timestampText, authorText].filter(Boolean);
            metadataText = parts.join(' · ');
          } else {
            metadataText = timestampText && authorText
              ? `${timestampText} · ${authorText}`
              : timestampText || authorText || '';
          }

          // Calculate metadata text width using MatrixChunky8
          const metadataWidth = metadataText ? $.text.box(metadataText, undefined, undefined, undefined, undefined, "MatrixChunky8").box.width : 0;

          // Standardized tooltip dimensions across all types
          const padding = 6; // Consistent padding for all tooltips
          const metadataHeight = 20; // Consistent metadata section height
          const metadataGap = 6; // Consistent gap before metadata

          // Calculate available space below ticker for tooltip
          const tickerBottomY = contentTickerY + tickerHeight + (tickerPadding * 2);
          const tooltipTopMargin = 32; // Space between ticker and tooltip
          const tooltipBottomMargin = 4; // Margin from screen bottom
          const availableHeight = screen.height - tickerBottomY - tooltipTopMargin - tooltipBottomMargin;
          const availableWidth = screen.width - 8; // 4px margin on each side

          // Maximum dimensions that respect available space
          const maxTooltipWidth = Math.min(availableWidth, 500); // Cap at 500px or available width
          const maxContentHeight = availableHeight - padding - metadataGap - metadataHeight; // Reserve space for metadata

          if (currentTooltipItem.type === 'kidlisp' && currentTooltipItem.source) {
            // KidLisp tooltip: use shared MediaPreviewBox dimensions
            const boxDims = mediaPreviewBox.getBoxDimensions();
            tooltipWidth = boxDims.width;
            tooltipHeight = boxDims.height; // Just the box, metadata goes outside
          } else if (currentTooltipItem.type === 'painting' && currentTooltipItem.image) {
            // Painting tooltip: use shared MediaPreviewBox dimensions
            const boxDims = mediaPreviewBox.getBoxDimensions();
            tooltipWidth = boxDims.width;
            tooltipHeight = boxDims.height; // Just the box, metadata goes outside
          } else if (currentTooltipItem.type === 'tape' && currentTooltipItem.audioUrl) {
            // Tape tooltip: show title and audio visualization - fit to available space
            const tapeTitle = currentTooltipItem.title || `Tape !${currentTooltipItem.code}`;
            const tapeTitleWidth = $.text.box(tapeTitle, undefined, undefined, undefined, undefined, "MatrixChunky8").box.width;
            const visualizerHeight = Math.min(120, maxContentHeight - 8); // Fit visualizer, reserve 8px for title

            const minWidthForMetadata = metadataWidth + padding * 2;
            const minWidthForTitle = tapeTitleWidth + padding * 2;
            tooltipWidth = Math.max(Math.min(150, maxTooltipWidth), minWidthForMetadata, minWidthForTitle);
            tooltipHeight = 8 + padding + visualizerHeight + padding; // Title + viz only, metadata goes outside
          } else if (currentTooltipItem.type === 'tape' && (currentTooltipItem.isLoading || currentTooltipItem.framesLoaded)) {
            // Tape tooltip: use shared MediaPreviewBox dimensions
            const boxDims = mediaPreviewBox.getBoxDimensions();
            tooltipWidth = boxDims.width;
            tooltipHeight = boxDims.height; // Just the box, progress/time/metadata go outside
          } else {
            return; // No valid content to show
          }

          // Update drift animation (smooth organic movement) - time-based
          const now = performance.now();
          if (!lastTooltipTime) lastTooltipTime = now;
          const deltaTooltip = (now - lastTooltipTime) / 1000; // Convert to seconds
          lastTooltipTime = now;
          tooltipDriftPhase += deltaTooltip * 1.2; // ~0.02 per frame at 60fps
          const driftSpeed = 0.5;
          tooltipDriftX = Math.sin(tooltipDriftPhase) * 15 * driftSpeed;
          tooltipDriftY = Math.cos(tooltipDriftPhase * 0.7) * 10 * driftSpeed;

          // Find the position of the highlighted item in the ticker
          // We need to calculate where the item appears in the scrolling ticker
          const tickerY = contentTickerY; // Use contentTickerY (where ticker is actually rendered)
          const tickerBoxY = contentTickerY - tickerPadding; // Top of ticker box
          const tickerBoxHeight = tickerHeight + (tickerPadding * 2); // Height including padding
          let highlightedItemX = -1;
          let highlightedItemWidth = -1;

          // Calculate the offset in the ticker for our current item
          if (contentTicker) {
            const offset = contentTicker.getOffset();
            let currentX = tickerContentX - offset;
            const prefix = currentTooltipItem.type === 'kidlisp' ? '$' : currentTooltipItem.type === 'painting' ? '#' : '!';
            const targetText = `${prefix}${currentTooltipItem.code}`;

            // Find where this item appears in the ticker
            for (let i = 0; i < contentItems.length; i++) {
              const item = contentItems[i];
              const itemPrefix = item.type === 'kidlisp' ? '$' : item.type === 'painting' ? '#' : '!';
              const itemText = `${itemPrefix}${item.code}`;
              const textWidth = $.text.box(itemText).box.width;

              if (item === currentTooltipItem) {
                // Check if this position is on screen
                if (currentX >= 0 && currentX < screen.width) {
                  highlightedItemX = currentX;
                  highlightedItemWidth = textWidth;
                }
                break;
              }

              currentX += textWidth + $.text.box(" · ").box.width;
            }
          }

          // Position preview centered on screen with subtle fade
          const baseTooltipX = (screen.width - tooltipWidth) / 2;
          const baseTooltipY = (screen.height - tooltipHeight) / 2;
          let tooltipX = baseTooltipX;
          let tooltipY = baseTooltipY;

          // Reduce opacity for background effect (30% max)
          const bgFadeMultiplier = 0.3;

          // Calculate total height including metadata/progress/time
          let totalTooltipHeight = tooltipHeight + metadataGap + metadataHeight;

          // For tapes with frames, add progress bar and time
          if (currentTooltipItem.type === 'tape' && (currentTooltipItem.isLoading || currentTooltipItem.framesLoaded)) {
            const progressBarHeight = 1;
            const timeDisplayHeight = 8;
            totalTooltipHeight = tooltipHeight + 2 + progressBarHeight + 2 + timeDisplayHeight + metadataGap + metadataHeight;
          }

          // Clamp tooltip to stay on screen (including metadata below)
          tooltipX = Math.max(4, Math.min(tooltipX, screen.width - tooltipWidth - 4));
          tooltipY = Math.max(4, Math.min(tooltipY, screen.height - totalTooltipHeight - 4));

          // Draw subtle background
          const bgAlpha = Math.floor(60 * tooltipFadeIn * bgFadeMultiplier);
          $.ink([10, 20, 15, bgAlpha]).box(tooltipX, tooltipY, tooltipWidth, tooltipHeight);

          // Draw border matching media type color
          let borderColor;
          if (currentTooltipItem.type === 'kidlisp') {
            borderColor = [120, 255, 160]; // Green
          } else if (currentTooltipItem.type === 'painting') {
            borderColor = [255, 120, 255]; // Magenta
          } else { // tape
            borderColor = [255, 200, 120]; // Orange
          }
          const borderAlpha = Math.floor(50 * tooltipFadeIn * bgFadeMultiplier);
          $.ink([...borderColor, borderAlpha]).box(tooltipX, tooltipY, tooltipWidth, tooltipHeight, "inline");

          // Render content based on type with reduced opacity
          const textAlpha = Math.floor(80 * tooltipFadeIn * bgFadeMultiplier);

          if (currentTooltipItem.type === 'kidlisp' && currentTooltipItem.source) {
            // Render KidLisp source with proper syntax highlighting
            renderKidlispSource(
              $,
              currentTooltipItem.source,
              tooltipX + padding,
              tooltipY + padding,
              tooltipWidth - padding * 2,
              5, // maxLines
              textAlpha
            );
          } else if (currentTooltipItem.type === 'painting' && currentTooltipItem.image) {
            // Render painting using shared MediaPreviewBox
            mediaPreviewBox.render($, currentTooltipItem, tooltipX, tooltipY, tooltipFadeIn * bgFadeMultiplier);
          } else if (currentTooltipItem.type === 'tape' && (currentTooltipItem.isLoading || currentTooltipItem.framesLoaded)) {
            // Render tape using shared MediaPreviewBox
            mediaPreviewBox.render($, currentTooltipItem, tooltipX, tooltipY, tooltipFadeIn * bgFadeMultiplier);
          }

          $.layer(1); // Reset to main layer

          // Skip metadata rendering for background preview
          /*
          // Render metadata (timestamp and @author) OUTSIDE the box, below it
          // For KidLisp: after the box + gap
          // For Painting: after the box + gap
          // For Tape with frames: after the box + progress bar + time + gap
          // For Tape with audio or loading: after the box + gap
          let metadataY;
          if (currentTooltipItem.type === 'kidlisp' && currentTooltipItem.source) {
            metadataY = tooltipY + tooltipHeight + metadataGap;
          } else if (currentTooltipItem.type === 'painting' && currentTooltipItem.image) {
            metadataY = tooltipY + tooltipHeight + metadataGap;
          } else if (currentTooltipItem.type === 'tape' && (currentTooltipItem.isLoading || currentTooltipItem.framesLoaded)) {
            // For tape with frames: metadata goes after progress bar (2px gap) + 1px bar + time (8px) + gap
            const progressBarHeight = 1;
            const timeDisplayHeight = 8;
            metadataY = tooltipY + tooltipHeight + 2 + progressBarHeight + 2 + timeDisplayHeight + metadataGap;
          } else if (currentTooltipItem.type === 'tape' && currentTooltipItem.audioUrl) {
            metadataY = tooltipY + tooltipHeight + metadataGap;
          }

          // Render metadata in dimmer color using MatrixChunky8
          if (metadataText) {
            const metadataAlpha = Math.floor(180 * tooltipFadeIn);
            $.ink([100, 180, 120, metadataAlpha]).write(
              metadataText,
              { x: tooltipX + padding, y: metadataY },
              undefined,
              undefined,
              false,
              "MatrixChunky8"
            );
          }
          */
        }
      }
    }

    // 📦 Commit hash button - shows version status / update availability
    // Hide commits button when KidLisp button is active (they share the same screen area)
    if (versionInfo && versionInfo.deployed && !(kidlispBtn && !kidlispBtn.btn.disabled)) {
      const commitText = updateAvailable
        ? "update ready"
        : versionInfo.deployed + (versionInfo.behindBy ? ` (${versionInfo.behindBy} behind)` : "");
      // Create button using TextButtonSmall (MatrixChunky8 font) centered at bottom
      const buttonY = screen.height - 20; // 20px from bottom
      if (!commitBtn) {
        commitBtn = new $.ui.TextButtonSmall(commitText, { center: "x", y: buttonY, screen });
      } else {
        commitBtn.reposition({ center: "x", y: buttonY, screen }, commitText);
        commitBtn.btn.disabled = false;
      }
      const cBox = commitBtn.btn.box;
      if (cBox) {
        const isHover = commitBtn.btn.over && !commitBtn.btn.down;

        const colors = updateAvailable
          ? isHover
            ? [
                [30, 60, 30, 180],    // fill: bright green on hover
                [60, 140, 60, 200],   // outline: vivid green
                220,                  // text alpha
                [30, 60, 30, 180]
              ]
            : [
                [20, 50, 20, 160],    // fill: green (update ready)
                [50, 120, 50, 180],   // outline: green
                180,                  // text alpha
                [20, 50, 20, 160]
              ]
          : versionInfo.status === "behind"
            ? isHover
              ? [
                  [60, 52, 30, 160],    // fill: brighter orange on hover
                  [120, 105, 60, 180],  // outline: much brighter orange
                  180,                  // text alpha brighter
                  [60, 52, 30, 160]
                ]
              : [
                  [40, 35, 20, 128],    // fill: dark orange (behind)
                  [80, 70, 40, 128],    // outline: lighter orange
                  128,                  // text alpha
                  [40, 35, 20, 128]
                ]
            : isHover
              ? [
                  [35, 42, 50, 160],    // fill: brighter blue/gray on hover
                  [70, 84, 100, 180],   // outline: much brighter blue/gray
                  180,                  // text alpha brighter
                  [35, 42, 50, 160]
                ]
              : [
                  [20, 25, 30, 128],    // fill: dark blue/gray (up-to-date)
                  [40, 50, 60, 128],    // outline: lighter blue/gray
                  128,                  // text alpha
                  [20, 25, 30, 128]
                ];
        commitBtn.paint($, colors);
      }
      versionCommit = versionInfo.deployed;
    } else {
      if (commitBtn) commitBtn.btn.disabled = true;
      commitBtn = null;
      versionCommit = null;
    }

    // 🚫 DEPRECATED: MOTD (Mood of the Day) - now shown in boot canvas2d initializer instead
    // The boot loader shows MOTDs faster with potential redis caching
    /*
    // MOTD (Mood of the Day) - show above login/signup buttons with animation
    // In CRITICAL funding mode, always show regardless of screen height
    if (motd && (isCriticalFunding || screen.height >= 180)) {
      motdBylineHandleBox = null;
      // Subtle sway (up and down)
      const swayY = Math.sin(motdFrame * 0.05) * 2; // 2 pixel sway range

      // Parse MOTD for interactive elements (handles, URLs, prompts, etc.)
      // In CRITICAL funding mode, toggle languages every 1.5 seconds
      let displayMotd = motd;
      const langPhase = Math.floor(Date.now() / 1500) % 2; // Toggle every 1.5s

      // Color schemes: EN = red/orange, DA = cyan/blue
      const enColors = ["red", "255,100,50", "yellow", "orange"];
      const daColors = ["cyan", "0,150,255", "white", "lime"];

      // Small screen mode: 3 lines, moved up
      const isSmallScreen = screen.width < 160;
      const lineSpacing = 10;

      if (isCriticalFunding && isSmallScreen) {
        // 3-line layout for small screens
        const line1EN = "CRITICAL MEDIA";
        const line1DA = "KRITISKE";
        const line2EN = "SERVICES OFFLINE";
        const line2DA = "MEDIETJENESTER OFFLINE";
        const line3EN = "ENTER 'give' TO HELP";
        const line3DA = "SKRIV 'give' FOR HJAELP";

        const lines = langPhase === 0
          ? [line1EN, line2EN, line3EN]
          : [line1DA, line2DA, line3DA];

        const colors = langPhase === 0 ? enColors : daColors;

        // Move up more for 3 lines
        const baseY = screen.height / 2 - 58 + swayY;

        for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
          const line = lines[lineIdx];
          let coloredLine = "";
          for (let i = 0; i < line.length; i++) {
            const colorIndex = Math.floor((i + motdFrame * 0.15 + lineIdx * 3) % colors.length);
            const color = colors[colorIndex];
            coloredLine += `\\${color}\\${line[i]}`;
          }

          ink(pal.handleColor).write(
            coloredLine,
            { center: "x", y: Math.floor(baseY + lineIdx * lineSpacing) },
            [255, 50, 200, 24],
            screen.width - 8,
          );
        }

        $.needsPaint();
      } else if (isCriticalFunding) {
        // CRITICAL funding mode: Normal 2-line layout for larger screens
        const motdY = screen.height / 2 - 48 + swayY;

        displayMotd = langPhase === 0
          ? "CRITICAL MEDIA SERVICES OFFLINE"
          : "KRITISKE MEDIETJENESTER OFFLINE";

      const motdElements = parseMessageElements(displayMotd);
      const hasLinks = motdElements.length > 0;

      let coloredText = "";
      const rainbowColors = ["pink", "cyan", "yellow", "lime", "orange", "magenta"];

      if (hasLinks) {
        // Use syntax highlighting for interactive elements
        coloredText = colorizeText(displayMotd, "white");
      } else {
        // Use color scheme matching the current language
        const line1Colors = langPhase === 0 ? enColors : daColors;
        for (let i = 0; i < displayMotd.length; i++) {
          const colorIndex = Math.floor((i + motdFrame * 0.15) % line1Colors.length);
          const color = line1Colors[colorIndex];
          coloredText += `\\${color}\\${displayMotd[i]}`;
        }
      }

      // Use wider text wrapping to prevent overlap
      const motdMaxWidth = Math.min(screen.width - 18, 200);

      // Determine alignment: left-align when book needs room on narrow screens
      let writePos;
      const leftMargin = 9;

      if (motdXOffset < 0 && screen.width < 400) {
        // Left-align on narrow screens when overlapping with book
        writePos = { x: leftMargin, y: Math.floor(motdY) };
      } else {
        // Center by default
        writePos = { center: "x", y: Math.floor(motdY) };
      }

      // Harsh shadow for MOTD (tight offset)
      const shadowOffset = 1;
      const shadowWritePos = writePos.x !== undefined
        ? { x: writePos.x + shadowOffset, y: (writePos.y || Math.floor(motdY)) + shadowOffset }
        : { center: "x", y: (writePos.y || Math.floor(motdY)) + shadowOffset };
      ink([0, 0, 0, 220]).write(
        displayMotd,
        shadowWritePos,
        undefined,
        motdMaxWidth,
      );

      ink(pal.handleColor).write(
        coloredText,
        writePos,
        [255, 50, 200, 24],
        motdMaxWidth,
      );

      // Add help line below MOTD (opposite language from line 1)
        const helpTextEN = "ENTER 'give' TO HELP";
        const helpTextDA = "SKRIV 'give' FOR HJAELP";
        const helpText = langPhase === 0 ? helpTextDA : helpTextEN;

        // Use opposite color scheme from line 1
        const line2Colors = langPhase === 0 ? daColors : enColors;

        // Build text with consistent language colors
        let coloredHelpText = "";
        for (let i = 0; i < helpText.length; i++) {
          const colorIndex = Math.floor((i + motdFrame * 0.15 + 5) % line2Colors.length);
          const color = line2Colors[colorIndex];
          coloredHelpText += `\\${color}\\${helpText[i]}`;
        }
        // Use same positioning logic as MOTD, but directly below (10px spacing)
        const helpWritePos = writePos.x !== undefined
          ? { x: writePos.x, y: (writePos.y || Math.floor(motdY)) + 10 }
          : { center: "x", y: (writePos.y || Math.floor(motdY)) + 10 };
        ink(pal.handleColor).write(
          coloredHelpText,
          helpWritePos,
          [255, 50, 200, 24],
          motdMaxWidth,
        );

      $.needsPaint();
      } else if (motd) {
        // Non-funding mode: normal MOTD display
        const motdY = screen.height / 2 - 48 + swayY;
        const motdElements = parseMessageElements(motd);
        const hasLinks = motdElements.length > 0;

        let coloredText = "";
        const rainbowColors = ["pink", "cyan", "yellow", "lime", "orange", "magenta"];

        if (hasLinks) {
          coloredText = colorizeText(motd, "white");
        } else {
          for (let i = 0; i < motd.length; i++) {
            const colorIndex = Math.floor((i + motdFrame * 0.1) % rainbowColors.length);
            const color = rainbowColors[colorIndex];
            coloredText += `\\${color}\\${motd[i]}`;
          }
        }

        const motdMaxWidth = Math.min(screen.width - 18, 200);
        let writePos;
        const leftMargin = 9;

        if (motdXOffset < 0 && screen.width < 400) {
          writePos = { x: leftMargin, y: Math.floor(motdY) };
        } else {
          writePos = { center: "x", y: Math.floor(motdY) };
        }

        // Harsh shadow for MOTD (tight offset)
        const shadowOffset = 1;
        const shadowWritePos = writePos.x !== undefined
          ? { x: writePos.x + shadowOffset, y: (writePos.y || Math.floor(motdY)) + shadowOffset }
          : { center: "x", y: (writePos.y || Math.floor(motdY)) + shadowOffset };
        ink([0, 0, 0, 220]).write(
          motd,
          shadowWritePos,
          undefined,
          motdMaxWidth,
        );

        ink(pal.handleColor).write(
          coloredText,
          writePos,
          [255, 50, 200, 24],
          motdMaxWidth,
        );

        if (motdByHandle) {
          const motdCharWidth = 6;
          const motdLineHeight = 10;
          const motdLines = Math.ceil((motd.length * motdCharWidth) / motdMaxWidth);
          const bylineY = (writePos.y || Math.floor(motdY)) + (motdLines * motdLineHeight) + 4;
          const bylinePrefix = "mood of the day from ";
          const bylineText = `${bylinePrefix}${motdByHandle}`;
          const bylineTextWidth = $.text.box(bylineText, undefined, undefined, undefined, undefined, "MatrixChunky8").box.width;
          const bylineBaseX = writePos.x !== undefined
            ? writePos.x
            : Math.floor((screen.width - Math.min(bylineTextWidth, motdMaxWidth)) / 2);

          // Harsh shadow for byline
          ink([0, 0, 0, 220]).write(
            bylineText,
            { x: bylineBaseX + 1, y: bylineY + 1, noFunding: true },
            undefined,
            undefined,
            false,
            "MatrixChunky8"
          );

          const prefixWidth = $.text.box(bylinePrefix, undefined, undefined, undefined, undefined, "MatrixChunky8").box.width;
          const handleWidth = $.text.box(motdByHandle, undefined, undefined, undefined, undefined, "MatrixChunky8").box.width;
          motdBylineHandleBox = {
            x: bylineBaseX + prefixWidth,
            y: bylineY,
            w: handleWidth,
            h: 8,
          };

          const handleColor = motdBylineHandleHover ? [150, 190, 255, 230] : [120, 160, 255, 200];
          ink([220, 220, 220, 170]).write(
            bylinePrefix,
            { x: bylineBaseX, y: bylineY, noFunding: true },
            undefined,
            undefined,
            false,
            "MatrixChunky8"
          );
          ink(handleColor).write(
            motdByHandle,
            { x: bylineBaseX + prefixWidth, y: bylineY, noFunding: true },
            undefined,
            undefined,
            false,
            "MatrixChunky8"
          );
        } else {
          motdBylineHandleBox = null;
          motdBylineHandleHover = false;
        }

        $.needsPaint();
      }
    }
    */ // End of deprecated MOTD section
    // Reset motd handle hover state since MOTD is deprecated
    motdBylineHandleBox = null;
    motdBylineHandleHover = false;

    // 🎭 MASTHEAD DECORATIONS (commented out)
    // 😡😢😭 Emotional face stamp - cycles through angry, sad, crying
    // Show during funding effects (yikes/critical) or in winter (Dec, Jan, Feb)
    /*
    const month = new Date().getMonth(); // 0-indexed: 0=Jan, 11=Dec
    const isWinter = month === 11 || month === 0 || month === 1; // Dec, Jan, Feb
    if ((showFundingEffects || isWinter) && screen.height >= 120) {
      // Face dimensions (will be scaled 2x)
      const faceSize = 16;
      const scaledSize = faceSize * 2;
      let symbolX = Math.floor((screen.width - scaledSize) / 2);
      // MOTD is at screen.height/2 - 48, so put symbol above that
      let symbolY = Math.floor(screen.height / 2 - 90); // a bit higher above CRITICAL SERVICES

      // Cycle through emotions every 3 seconds: 0=angry, 1=sad, 2=crying
      const emotionPhase = Math.floor(Date.now() / 3000) % 3;

      // Only draw if there's enough space from top
      if (symbolY > 20) {
        // Shake animation - VERY intense when angry (vertical only)
        const shakeIntensity = emotionPhase === 0 ? 8 : 1; // Much more shake when angry!
        const shakeSpeed = emotionPhase === 0 ? 0.4 : 0.3; // Slower but still intense when angry
        const shakeX = emotionPhase === 0 ? 0 : Math.sin(motdFrame * shakeSpeed) * shakeIntensity; // No horizontal shake when angry
        const shakeY = Math.cos(motdFrame * (shakeSpeed * 1.3)) * (shakeIntensity * 0.7);

        // Color palette based on emotion
        let faceColors;
        if (emotionPhase === 0) {
          // Angry: VERY RED - pure saturated reds
          faceColors = [
            [255, 0, 0],     // Pure red
            [255, 30, 0],    // Slightly orange red
            [255, 0, 30],    // Slightly pink red
            [220, 0, 0],     // Dark red
          ];
        } else if (emotionPhase === 1) {
          // Sad: yellow colors (not blue!)
          faceColors = [
            [255, 220, 80],  // Golden yellow
            [255, 200, 50],  // Bright yellow
            [255, 240, 100], // Light yellow
            [230, 180, 40],  // Darker yellow
          ];
        } else {
          // Crying: cyan/teal colors
          faceColors = [
            [80, 200, 255],  // Cyan
            [100, 220, 230], // Light cyan
            [60, 180, 220],  // Teal
            [120, 200, 255], // Sky blue
          ];
        }
        const colorIndex = Math.floor(motdFrame * 0.08) % faceColors.length;
        const faceColor = faceColors[colorIndex];
        const faceAlpha = 200 + Math.sin(motdFrame * 0.1) * 55; // 145-255 pulsing

        const faceX = Math.floor(symbolX + shakeX);
        const faceY = Math.floor(symbolY + shakeY);

        // Draw custom face at 2x scale (manually scaled coordinates)
        // Helper to draw a 2x scaled box
        const s = 2; // scale factor
        const box2x = (x, y, w, h) => $.box(faceX + x * s, faceY + y * s, w * s, h * s);

        // Face outline (circle approximation - rounded square)
        ink(...faceColor, faceAlpha);
        // Top edge
        box2x(4, 0, 8, 1);
        // Upper sides
        box2x(2, 1, 2, 1); box2x(12, 1, 2, 1);
        // Side edges
        box2x(1, 2, 1, 2); box2x(14, 2, 1, 2);
        box2x(0, 4, 1, 8); box2x(15, 4, 1, 8);
        box2x(1, 12, 1, 2); box2x(14, 12, 1, 2);
        // Lower sides
        box2x(2, 14, 2, 1); box2x(12, 14, 2, 1);
        // Bottom edge
        box2x(4, 15, 8, 1);

        // Draw eyes and expression based on emotion phase
        if (emotionPhase === 0) {
          // ANGRY FACE 😡 - angry eyebrows, gritted teeth
          // Angry eyebrows (angled down toward center)
          box2x(3, 4, 2, 1);  // Left eyebrow outer
          box2x(5, 5, 1, 1);  // Left eyebrow inner (lower)
          box2x(11, 4, 2, 1); // Right eyebrow outer
          box2x(10, 5, 1, 1); // Right eyebrow inner (lower)
          // Eyes (smaller, squinting)
          box2x(4, 6, 2, 1);  // Left eye
          box2x(10, 6, 2, 1); // Right eye
          // Gritted teeth / angry mouth
          box2x(5, 11, 6, 1);  // Top teeth line
          box2x(5, 12, 1, 1); box2x(7, 12, 1, 1); box2x(9, 12, 1, 1); // Gaps
          box2x(5, 13, 6, 1);  // Bottom teeth line
        } else if (emotionPhase === 1) {
          // SAD FACE 😢 - regular eyes, frown
          // Eyes (2x2 pixels each)
          box2x(4, 5, 2, 2);  // Left eye
          box2x(10, 5, 2, 2); // Right eye
          // Sad frown (curved down)
          box2x(4, 11, 1, 1);   // Left corner up
          box2x(5, 12, 1, 1);   // Left slope
          box2x(6, 13, 4, 1);   // Bottom of frown
          box2x(10, 12, 1, 1);  // Right slope
          box2x(11, 11, 1, 1);  // Right corner up
        } else {
          // CRYING FACE 😭 - closed eyes (lines), tears falling to bottom, wailing mouth
          // Closed/squinting eyes (horizontal lines)
          box2x(3, 5, 3, 1);  // Left eye closed
          box2x(10, 5, 3, 1); // Right eye closed

          // Multiple tear drops falling to bottom of screen
          // Tears spawn from the face but drift independently (use symbolX/Y not faceX/Y)
          const tearSpawnY = symbolY + 14; // Start below eyes (base position, no shake)
          const tearColors = [
            [80, 180, 255],  // Light blue
            [100, 200, 255], // Cyan
            [120, 220, 255], // Brighter cyan
          ];

          // Draw multiple tears at different phases - they fall on their own paths
          for (let t = 0; t < 6; t++) {
            const tearPhase = (motdFrame * 0.15 + t * 1.2) % 8; // Staggered timing
            const tearProgress = tearPhase / 8; // 0-1
            const tearY = tearSpawnY + tearProgress * (screen.height - tearSpawnY + 10);

            // Tears drift independently with their own sine wave (based on their index, not face shake)
            const tearDriftX = Math.sin(tearPhase * 0.8 + t * 2.1) * (3 + tearProgress * 8); // Wider drift as they fall
            const tearDriftY = Math.cos(tearPhase * 0.5 + t) * 2; // Slight vertical wobble

            // Left tears - spawn from left eye area (base position)
            const leftTearBaseX = symbolX + 8 + (t % 2) * 4;
            const leftTearX = leftTearBaseX + tearDriftX;
            // Right tears - spawn from right eye area (base position)
            const rightTearBaseX = symbolX + 22 - (t % 2) * 4;
            const rightTearX = rightTearBaseX - tearDriftX; // Mirror the drift

            const tearColor = tearColors[t % tearColors.length];
            const tearAlpha = Math.floor((1 - tearProgress * 0.5) * 200); // Fade slightly
            const tearSize = t % 2 === 0 ? 3 : 2; // Vary sizes

            if (tearY < screen.height) {
              ink(...tearColor, tearAlpha);
              // Teardrop shape (stretched vertically)
              $.box(Math.floor(leftTearX), Math.floor(tearY + tearDriftY), tearSize, tearSize + 1);
              $.box(Math.floor(rightTearX), Math.floor(tearY + tearDriftY), tearSize, tearSize + 1);
            }
          }

          // Reset to face color for mouth
          ink(...faceColor, faceAlpha);
          // Wailing open mouth (big oval)
          box2x(5, 10, 6, 1);   // Top of mouth
          box2x(4, 11, 1, 3);   // Left side
          box2x(11, 11, 1, 3);  // Right side
          box2x(5, 14, 6, 1);   // Bottom of mouth
        }

        // Sparks flying off the head (color matches emotion)
        // WAY more particles when angry!
        const sparkCount = emotionPhase === 0 ? 24 : 8;
        for (let i = 0; i < sparkCount; i++) {
          // Each spark has a phase offset for continuous emission
          const sparkPhase = (motdFrame * 0.15 + i * 0.7) % 3; // 0-3 lifecycle
          const sparkLife = sparkPhase / 3; // 0-1 normalized

          // Sparks fly upward and outward - more spread when angry
          const spreadFactor = emotionPhase === 0 ? 0.4 : 0.25;
          const sparkAngle = -Math.PI / 2 + (i - sparkCount / 2) * spreadFactor + Math.sin(motdFrame * 0.1 + i) * 0.15;
          const sparkSpeed = emotionPhase === 0 ? (20 + i * 2) : (15 + i * 3);
          const sparkDist = sparkLife * sparkSpeed;

          // Start from top of head (center-top of face)
          const sparkStartX = faceX + scaledSize / 2;
          const sparkStartY = faceY;

          const sparkX = sparkStartX + Math.cos(sparkAngle) * sparkDist + Math.sin(motdFrame * 0.2 + i) * 3;
          const sparkY = sparkStartY + Math.sin(sparkAngle) * sparkDist - sparkLife * 8; // extra upward drift

          // Spark colors match emotion
          let sparkColors;
          if (emotionPhase === 0) {
            // VERY RED sparks for angry
            sparkColors = [[255, 0, 0], [255, 50, 0], [255, 100, 0], [255, 30, 30]];
          } else if (emotionPhase === 1) {
            // Yellow sparks for sad
            sparkColors = [[255, 220, 50], [255, 200, 80], [255, 240, 100], [230, 180, 40]];
          } else {
            sparkColors = [[80, 200, 255], [100, 220, 255], [150, 230, 255], [120, 210, 255]];
          }
          const sparkColorIdx = (i + Math.floor(motdFrame * 0.1)) % sparkColors.length;
          const sparkColor = sparkColors[sparkColorIdx];

          // Fade out as spark ages
          const sparkAlpha = Math.floor((1 - sparkLife) * 255);

          // Spark size shrinks as it ages
          const sparkSize = Math.max(1, Math.floor((1 - sparkLife * 0.7) * 3));

          if (sparkAlpha > 20) {
            ink(...sparkColor, sparkAlpha).box(
              Math.floor(sparkX),
              Math.floor(sparkY),
              sparkSize,
              sparkSize
            );
          }
        }

        // Words floating laterally off BOTH sides - mood-dependent
        // emotionPhase: 0 = ANGRY, 1 = SAD, 2 = CRYING
        const moodWords = ["GRRR!", "sigh...", "HELP!"];
        const moodWord = moodWords[emotionPhase] || "HELP!";
        const numWordsPerSide = 3;
        // Fixed screen position - independent of head movement
        const wordOriginX = screen.width / 2;
        const wordOriginY = Math.min(screen.height * 0.22, 50);

        // Mood-based color palettes
        const moodLeftColors = [
          [[255, 80, 80], [255, 120, 60], [200, 50, 50], [255, 100, 100]],    // Angry: reds
          [[140, 140, 180], [160, 160, 200], [120, 120, 160], [180, 180, 220]], // Sad: muted blues/grays
          [[100, 200, 255], [150, 100, 255], [50, 255, 200], [180, 150, 255]] // Crying: cool blues
        ];
        const moodRightColors = [
          [[255, 60, 60], [255, 100, 40], [220, 40, 40], [255, 80, 80]],      // Angry: reds
          [[160, 160, 190], [140, 140, 170], [180, 180, 210], [130, 130, 160]], // Sad: muted blues/grays
          [[255, 150, 100], [255, 200, 50], [255, 100, 150], [255, 180, 80]]  // Crying: warm oranges
        ];
        const leftColors = moodLeftColors[emotionPhase] || moodLeftColors[2];
        const rightColors = moodRightColors[emotionPhase] || moodRightColors[2];

        // Mood-based motion parameters
        const moodSpeed = [0.055, 0.025, 0.035][emotionPhase] || 0.035;       // Angry fast, sad slow
        const moodWaveAmp = [2, 8, 5][emotionPhase] || 5;                      // Sad droopy, angry tight
        const moodShake = [3, 0.5, 1][emotionPhase] || 1;                      // Angry shaky, sad still
        const moodDrift = [2, 12, 6][emotionPhase] || 6;                       // Sad sinks more

        // RIGHT side words - fixed origin, independent of head
        for (let w = 0; w < numWordsPerSide; w++) {
          const wordPhase = ((motdFrame * moodSpeed + w * 0.85) % 3) / 3;

          const startX = wordOriginX + 15; // Fixed offset from center
          const startY = wordOriginY + w * 4;

          const floatDist = wordPhase * 80;
          // Sad: droop down; Angry: jitter; Crying: wave
          const waveY = emotionPhase === 1
            ? wordPhase * moodWaveAmp + Math.sin(wordPhase * Math.PI) * 3  // Sad droops
            : Math.sin(wordPhase * Math.PI * 2 + w) * moodWaveAmp;

          const wordX = startX + floatDist;
          const wordY = startY + waveY + (emotionPhase === 1 ? wordPhase * moodDrift : -wordPhase * moodDrift);

          const alpha = Math.floor((1 - wordPhase * 0.85) * 255);
          // Scale from 0.3 to 1.5 as it moves outward
          const wordScale = 0.3 + wordPhase * 1.2;

          if (alpha > 20) {
            let coloredWord = "";
            for (let i = 0; i < moodWord.length; i++) {
              const colorIdx = Math.floor((i + motdFrame * 0.12 + w * 2) % rightColors.length);
              const c = rightColors[colorIdx];
              coloredWord += `\\${c[0]},${c[1]},${c[2]},${alpha}\\${moodWord[i]}`;
            }

            const shakeX = Math.sin(motdFrame * 0.2 + w) * moodShake;
            const shakeY = Math.cos(motdFrame * 0.25 + w) * moodShake;

            ink(255, 255, 255).write(
              coloredWord,
              { x: Math.floor(wordX + shakeX), y: Math.floor(wordY + shakeY), size: wordScale }
            );
          }
        }

        // LEFT side words (float leftward, fixed origin)
        const wordWidth = moodWord.length * 6;
        for (let w = 0; w < numWordsPerSide; w++) {
          const wordPhase = ((motdFrame * (moodSpeed * 1.08) + w * 0.9 + 0.3) % 3) / 3;

          const startX = wordOriginX - 15 - wordWidth * 0.3; // Fixed offset from center
          const startY = wordOriginY + w * 4;

          const floatDist = wordPhase * 80;
          const waveY = emotionPhase === 1
            ? wordPhase * moodWaveAmp + Math.sin(wordPhase * Math.PI + 1) * 3
            : Math.sin(wordPhase * Math.PI * 2 + w + 1) * moodWaveAmp;

          const wordX = startX - floatDist - wordPhase * wordWidth * 0.7; // Account for growing text width
          const wordY = startY + waveY + (emotionPhase === 1 ? wordPhase * moodDrift : -wordPhase * moodDrift);

          const alpha = Math.floor((1 - wordPhase * 0.85) * 255);
          const wordScale = 0.3 + wordPhase * 1.2;

          if (alpha > 20) {
            let coloredWord = "";
            for (let i = 0; i < moodWord.length; i++) {
              const colorIdx = Math.floor((i + motdFrame * 0.12 + w * 2) % leftColors.length);
              const c = leftColors[colorIdx];
              coloredWord += `\\${c[0]},${c[1]},${c[2]},${alpha}\\${moodWord[i]}`;
            }

            const shakeX = Math.sin(motdFrame * 0.22 + w) * moodShake;
            const shakeY = Math.cos(motdFrame * 0.27 + w) * moodShake;

            ink(255, 255, 255).write(
              coloredWord,
              { x: Math.floor(wordX + shakeX), y: Math.floor(wordY + shakeY), size: wordScale }
            );
          }
        }
      }
    }
    */
  }

  // 🎹 Non-default theme unfocused visual cues (serious / neo)
  if (theme !== "default" && !$.system.prompt.input.canType) {
    const input = $.system.prompt.input;
    const isDark = $.dark;
    const fg = isDark ? [255, 255, 255] : [0, 0, 0];

    if (input?.prompt) {
      const cursorPos = input.prompt.pos(undefined, true);

      if (cursorPos && cursorPos.x !== undefined && cursorPos.y !== undefined) {
        $.layer(3);

        // Dim blinking cursor
        const blinkAlpha = Math.floor(Math.sin(motdFrame * 0.08) * 40 + 50); // 10-90
        ink(...fg, blinkAlpha).box(cursorPos.x, cursorPos.y, cursorPos.w, cursorPos.h);

        // Subtle border around cursor
        const borderAlpha = Math.floor(Math.sin(motdFrame * 0.06) * 20 + 30); // 10-50
        ink(...fg, borderAlpha)
          .box(cursorPos.x - 1, cursorPos.y - 1, cursorPos.w + 2, 1) // top
          .box(cursorPos.x - 1, cursorPos.y + cursorPos.h, cursorPos.w + 2, 1) // bottom
          .box(cursorPos.x - 1, cursorPos.y, 1, cursorPos.h) // left
          .box(cursorPos.x + cursorPos.w, cursorPos.y, 1, cursorPos.h); // right

        $.layer(1);
      }

      // Faded last-typed text
      if (input.text && input.text.length > 0) {
        const fadedAlpha = Math.floor(Math.sin(motdFrame * 0.04) * 15 + 25); // 10-40
        ink(...fg, fadedAlpha).write(input.text, {
          x: input.prompt.left,
          y: input.prompt.top,
          size: input.prompt.scale,
        });
      }
    }
  }

  // Paint UI Buttons
  //if (!net.iframe) {

  // what we actually want for login and signup is pal.signup / pal.login
  // currently gives an error, I think because of paint (works fine with ink)
  if (!net.sandboxed) {
    if (login && !login.btn.disabled) {
      // Draw waveform visualization behind login button
      if ($.sound.speaker?.waveforms?.left && $.sound.speaker?.amplitudes?.left !== undefined) {
        const hasSignal = $.sound.speaker.amplitudes.left > 0.001;

        if (hasSignal) {
          const waveX = login.btn.box.x;
          const waveY = login.btn.box.y;
          const waveW = login.btn.box.w;
          const waveH = login.btn.box.h;

          // Draw waveform with subtle transparency
          $.sound.paint.waveform(
            $,
            $.sound.speaker.amplitudes.left,
            $.sound.speaker.waveforms.left,
            waveX,
            waveY,
            waveW,
            waveH,
            $.dark ? [0, 255, 200, 32] : [255, 100, 200, 32], // Cyan in dark mode, pink in light mode
            { center: true }
          );
        }
      }

      login.paint(
        $,
        $.dark ? scheme.dark.login : scheme.light.login,
        $.dark ? scheme.dark.loginDown : scheme.light.loginDown, // downScheme (pressed)
        undefined, // disabledScheme (use default)
        $.dark ? scheme.dark.loginRollover : scheme.light.loginRollover  // rolloverScheme (hover)
      );
    }
  }

  if (!net.iframe) {
    if (signup && !signup.btn.disabled) {
      // Draw waveform visualization behind signup button too
      if ($.sound.speaker?.waveforms?.left && $.sound.speaker?.amplitudes?.left !== undefined) {
        const hasSignal = $.sound.speaker.amplitudes.left > 0.001;

        if (hasSignal) {
          const waveX = signup.btn.box.x;
          const waveY = signup.btn.box.y;
          const waveW = signup.btn.box.w;
          const waveH = signup.btn.box.h;

          // Draw waveform with subtle transparency
          $.sound.paint.waveform(
            $,
            $.sound.speaker.amplitudes.left,
            $.sound.speaker.waveforms.left,
            waveX,
            waveY,
            waveW,
            waveH,
            $.dark ? [255, 150, 0, 32] : [255, 200, 0, 32], // Orange tones
            { center: true }
          );
        }
      }

      signup.paint(
        $,
        $.dark ? scheme.dark.signup : scheme.light.signup,
        undefined, // hoverScheme (use default)
        undefined, // disabledScheme (use default)
        $.dark ? scheme.dark.signupRollover : scheme.light.signupRollover
      );
    }
  }
  if (profile && !profile.btn.disabled) {
    // Draw full-width waveform visualization behind profile button
    if ($.sound.speaker?.waveforms?.left && $.sound.speaker?.amplitudes?.left !== undefined) {
      const hasSignal = $.sound.speaker.amplitudes.left > 0.001;

      if (hasSignal) {
        // Full screen width, positioned at button's Y location
        const waveX = 0;
        const waveY = profile.btn.box.y;
        const waveW = $.screen.width;
        const waveH = profile.btn.box.h;

        // console.log(`🎵 Drawing full-width waveform behind @${$.handle()} button - amplitude: ${$.sound.speaker.amplitudes.left.toFixed(3)}`);

        // Draw waveform with subtle transparency
        $.sound.paint.waveform(
          $,
          $.sound.speaker.amplitudes.left,
          $.sound.speaker.waveforms.left,
          waveX,
          waveY,
          waveW,
          waveH,
          $.dark ? [0, 255, 200, 32] : [255, 100, 200, 32], // Cyan in dark mode, pink in light mode
          { center: true }
        );
      }
    }

    if (
      profileAction === "resend-verification" ||
      profileAction === "set-handle"
    ) {
      const verified = profileAction === "set-handle";
      const message = verified
        ? "Email verified!"
        : "Awaiting email verification" + ellipsisTicker.text(help.repeat);

      ink("black", 128).write(
        message,
        {
          center: "x",
          x: screen.width / 2 + 1,
          y: screen.height / 2 - 48 + 1,
        },
        undefined,
        screen.width - 16,
      );

      ink(verified ? "lime" : "yellow").write(
        message,
        { center: "x", y: screen.height / 2 - 48 },
        undefined,
        screen.width - 16,
      );
    }
    profile?.paint(
      $,
      $.dark ? scheme.dark.profile : scheme.light.profile,
      undefined, // hoverScheme (use default)
      undefined, // disabledScheme (use default)
      $.dark ? scheme.dark.profileRollover : scheme.light.profileRollover
    );

    // ꜩ Tezos wallet button (positioned to the right of handle button)
    if (tezosWalletAddress && profile) {
      // Format balance
      const balanceText = tezosWalletBalance !== null
        ? `${tezosWalletBalance.toFixed(2)}`
        : "...";

      // Build button text (space for ꜩ symbol drawn separately)
      const btnTextWithSymbol = "  " + balanceText; // 2 spaces for the ꜩ symbol

      // Position to the right of profile button with a gap
      const gap = 6; // Gap between handle and wallet buttons
      const profileBox = profile.btn.box;
      const walletBtnY = profileBox.y; // Same Y as profile button

      // Create or update wallet button (positioned temporarily, will adjust X after)
      if (!walletBtn) {
        walletBtn = new $.ui.TextButton(btnTextWithSymbol, {
          x: 0, y: walletBtnY, screen
        });
      } else {
        walletBtn.reposition({ x: 0, y: walletBtnY, screen }, btnTextWithSymbol);
      }

      // Calculate combined width and center both buttons together
      const walletWidth = walletBtn.btn.box.w;
      const profileWidth = profileBox.w;
      const totalWidth = profileWidth + gap + walletWidth;
      const startX = (screen.width - totalWidth) / 2;

      // Reposition both buttons to be centered together
      profile.btn.box.x = startX;
      walletBtn.btn.box.x = startX + profileWidth + gap;

      // Color schemes: [fill, outline, text, textBackground]
      const normalFill = [20, 50, 70];
      const hoverFill = [30, 80, 100];

      // Paint the wallet button with cyan theme
      walletBtn?.paint($,
        [normalFill, [0, 150, 200], [0, 200, 255], normalFill],  // normal: textBg matches fill
        [hoverFill, [0, 200, 255], [255, 255, 255], hoverFill]   // hover: textBg matches fill
      );

      // Draw ꜩ symbol using unifont on top of button (aligned with button text)
      const tezSymbolX = walletBtn.btn.box.x + 3; // Inside button padding (left 1px)
      const tezSymbolY = walletBtn.btn.box.y + 3; // Align with button text (down 1px)
      const tezColor = walletBtn.btn.down ? [255, 255, 255] : [0, 200, 255];
      const tezBg = walletBtn.btn.down ? hoverFill : normalFill;
      ink(...tezColor).write("ꜩ", { x: tezSymbolX, y: tezSymbolY + TEZ_Y_ADJUST, bg: tezBg }, undefined, undefined, false, "unifont");
    } else if (!tezosWalletAddress) {
      walletBtn = null; // Clear button when disconnected
    }
  }
  //}

  // 📏 Paint a measurement line in the center of the display.
  if (ruler) {
    $.ink(255, 0, 255, 127).line(
      screen.width / 2,
      0,
      screen.width / 2,
      screen.height,
    );
    if (screen.width % 2 === 0) {
      $.ink(255, 0, 255, 127).line(
        screen.width / 2 - 1,
        0,
        screen.width / 2 - 1,
        screen.height,
      );
    }
  }

  // 📊 FPS Meter - tiny display in bottom-right corner
  if (showFpsMeter) {
    const fpsText = `${currentFps}`;
    const fpsColor = currentFps >= 55 ? [0, 200, 100, 200] :
                     currentFps >= 30 ? [255, 200, 0, 200] : [255, 60, 60, 200];
    // Draw background pill
    const textWidth = fpsText.length * 6;
    $.ink(0, 0, 0, 150).box(screen.width - textWidth - 8, screen.height - 12, textWidth + 6, 10);
    // Draw FPS text
    $.ink(fpsColor).write(fpsText, { x: screen.width - textWidth - 5, y: screen.height - 11 }, undefined, undefined, false, "MatrixChunky8");
  }

  // 🏷️ Dev Mode indicator - TEMPORARILY DISABLED for debugging
  // TODO: Re-enable once black box source is identified

  // 🔍 Paint @handle autocomplete dropdown (last, so it renders on top of everything)
  if (handleAutocomplete?.visible && $.system.prompt.input?.canType) {
    const prompt = $.system.prompt.input?.prompt;
    const cursorPos = prompt?.pos?.(undefined, true);
    handleAutocomplete.paint(
      { ink, write: (t, opts) => ink().write(t, opts), box: (x, y, w, h, style) => ink().box(x, y, w, h, style), screen },
      { x: cursorPos?.x ?? 10, y: (cursorPos?.y ?? 10) + 12 }
    );
  }

  // 🧹 "Blank" button (fixed top-right, below char count, at 32+ chars)
  {
    const inp = $.system.prompt.input;
    if (inp?.canType && inp?.prompt && inp.text?.length >= 32) {
      const label = clearBtnConfirming ? "Blank?" : "Blank";
      const cw = 4; // MatrixChunky8 char width
      const ch = 7; // MatrixChunky8 char height
      const pad = 2;
      const bw = label.length * cw + pad * 2;
      const bh = ch + pad * 2;
      const bx = screen.width - bw - 2;
      const by = (inp.text.length / 256) > 0.7 ? 26 : 23;
      if (!clearBtn) {
        clearBtn = new $.ui.Button(bx, by, bw, bh);
      } else {
        clearBtn.box.x = bx;
        clearBtn.box.y = by;
        clearBtn.box.w = bw;
        clearBtn.box.h = bh;
        clearBtn.disabled = false;
      }
      $.layer(3);
      const down = clearBtn.down;
      const alpha = clearBtnConfirming ? 255 : down ? 255 : clearBtn.over ? 220 : 180;
      const bg = clearBtnConfirming
        ? [200, 30, 30, alpha]
        : down ? [180, 20, 20, alpha] : [160, 40, 40, alpha];
      const fg = [255, 255, 255, alpha];
      ink(...bg).box(bx, by, bw, bh, "fill");
      ink(...fg).write(label, { x: bx + pad, y: by + pad }, undefined, undefined, false, "MatrixChunky8");
      $.layer(1);
    } else if (clearBtn) {
      clearBtn.disabled = true;
    }
  }

  // Trigger a red or green screen flash with a timer.
  if (flashShow) {
    let color = firstActivation ? scheme.dark.block : flashColor;
    ink(color).box(0, 0, screen.width, screen.height);
    if (firstActivation) return true;
  }

  $.layer(0); // Return to the bottom layer.
  return false;
}

// 🧮 Sim
function sim($) {
  // 🎄 Handle pending autorun commands (from URL-based merry, etc.)
  if (pendingAutorun) {
    const { text } = pendingAutorun;
    pendingAutorun = null; // Clear immediately to prevent re-execution
    // Set text right before halt (may have been skipped in boot to avoid flash)
    $.system.prompt.input.text = text;
    halt($, text);
  }

  ellipsisTicker?.sim();
  progressTrick?.step();

  // 🔍 Sync autocomplete skipHistory and skipEnter flags with TextInput
  if ($.system.prompt.input) {
    const autocompleteActive = !!(handleAutocomplete?.visible && handleAutocomplete.items.length > 0);
    $.system.prompt.input.skipHistory = autocompleteActive;
    $.system.prompt.input.skipEnter = autocompleteActive;
  }

  // �🔷 Sync Tezos wallet state from bios (for restored sessions)
  const biosWallet = $.wallet?.get();
  if (biosWallet?.connected && !tezosWalletAddress) {
    // Wallet connected in bios but prompt doesn't know about it yet
    tezosWalletAddress = biosWallet.address;
    tezosWalletBalance = biosWallet.balance;
    tezosDomainName = biosWallet.domain;
    tezosNetwork = biosWallet.network || "mainnet";
    $.needsPaint();
  } else if (!biosWallet?.connected && tezosWalletAddress) {
    // Wallet disconnected in bios
    tezosWalletAddress = null;
    tezosWalletBalance = null;
    tezosDomainName = null;
    $.needsPaint();
  } else if (biosWallet?.connected && tezosWalletAddress) {
    // Both connected - sync balance/domain if updated
    if (biosWallet.balance !== tezosWalletBalance) {
      tezosWalletBalance = biosWallet.balance;
      $.needsPaint();
    }
    if (biosWallet.domain !== tezosDomainName) {
      tezosDomainName = biosWallet.domain;
      $.needsPaint();
    }
  }

  // Poll audio speaker for waveform visualization
  if ($.sound.speaker) {
    $.sound.speaker.poll();
  }

  if (theme === "default" && (!login?.btn.disabled || !profile?.btn.disabled)) {
    starfield.sim($);
    $.needsPaint();
  }

  // Drive blinking animation for non-default theme unfocused cues
  if (theme !== "default" && !$.system.prompt.input.canType) {
    $.needsPaint();
  }

  const now = Date.now();
  if (theme === "default") {
    if (now - lastClockChatFetchAt > CLOCK_CHAT_REFRESH_MS) {
      fetchClockChatMessages();
    }
    if (now - lastContentFetchAt > CONTENT_REFRESH_MS) {
      fetchContentItems($);
    }
  }

  // 📦 Update product animations
  {
    const showLoginCurtainSim =
      (!login?.btn.disabled && !profile) ||
      (!login && !profile?.btn.disabled);
    const promptHasContent = $.system.prompt.input.text && $.system.prompt.input.text.length > 0;
    const shouldShow = showLoginCurtainSim && !$.system.prompt.input.canType && !promptHasContent;
    if (shouldShow) {
      const product = products.getActiveProduct();
      if (product && product.imageScaled) {
        products.sim($);
        $.needsPaint();
      }
    }
  }

  if ($.store["handle:received"]) {
    const hand = $.handle();
    profile = new $.ui.TextButton(hand, {
      center: "xy",
      screen: $.screen,
    });
    profile.stickyScrubbing = true; // Prevent drag-between-button behavior

    // Apply colored handle asynchronously (update in-place to avoid flicker)
    fetchHandleColors(hand, $).then((colors) => {
      if (colors) {
        profile.replaceLabel(colorizeHandle(hand, colors));
        $.needsPaint();
      }
    });

    if (login) login.btn.disabled = true;
    if (signup) signup.btn.disabled = true;
    delete $.store["handle:received"];
    profileAction = "profile";
    $.needsPaint();
  }

  if ($.store["handle:failed"]) {
    delete $.store["handle:failed"];
    $.needsPaint();
  }

  if (flashPresent) flash.step();
}

// 🎪 Act
function act({
  event: e,
  api,
  needsPaint,
  net,
  screen,
  num,
  jump,
  system,
  painting,
  user,
  store,
  sound: { play, synth },
  send,
  handle,
  glaze,
  canShare,
  notice,
  ui,
}) {
  // Checks to clear prefilled 'email user@email.com' message
  // on signup.
  if (
    e.is("keyboard:close") &&
    resendVerificationText &&
    system.prompt.input.text === resendVerificationText
  ) {
    system.prompt.input.blank(); // Clear the prompt.
    resendVerificationText = undefined;
  }

  const isOverMotdHandle = motdBylineHandleBox &&
    e.x >= motdBylineHandleBox.x &&
    e.x <= motdBylineHandleBox.x + motdBylineHandleBox.w &&
    e.y >= motdBylineHandleBox.y &&
    e.y <= motdBylineHandleBox.y + motdBylineHandleBox.h;

  if (e.is("move") && motdBylineHandleBox) {
    if (isOverMotdHandle !== motdBylineHandleHover) {
      motdBylineHandleHover = isOverMotdHandle;
      needsPaint();
    }
  }

  if (e.is("lift") && motdByHandle && isOverMotdHandle) {
    pushSound();
    jump(motdByHandle);
    return;
  }

  // Light and dark mode glaze shift.
  // 🧪 Temporarily disabled to test noise16 transition
  // if (e.is("dark-mode")) glaze({ on: true });
  // if (e.is("light-mode")) glaze({ on: false });

  // Via vscode extension.
  if (e.is("aesthetic-parent:focused")) {
    // Only clear text and reopen keyboard if the prompt isn't already active.
    // This prevents wiping user-typed text and canceling curtain toggles
    // when the VS Code panel regains focus from within.
    if (!system.prompt.input.canType) {
      system.prompt.input.text = "";
      send({ type: "keyboard:unlock" });
      send({ type: "keyboard:open" }); // Necessary for desktop.
    }
  }

  // 👱 Handle Callback
  if (e.is("handle:request:completed")) {
    console.log("Handle request completed:", profile);
    profile.btn.disabled = false;
  }

  // 📼 Taping
  if (e.is("microphone:connect:success")) {
    console.log("📼 Taping...");
    tapePromiseResolve?.();
  }

  if (e.is("microphone:connect:failure")) {
    console.warn("📼 🟡 Microphone failed to connect. Not taping.");
    // TODO: How to re-approve permission here in a cross-browser way?
    tapePromiseReject?.();
  }

  // 🔘 Buttons
  const downSound = () => {
    synth({
      type: "sine",
      tone: 600,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.001,
    });
  };

  const pushSound = () => {
    synth({
      type: "sine",
      tone: 800,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.005,
    });
  };

  const cancelSound = () => {
    synth({
      tone: 200,
      beats: 0.1,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
  };

  // 🎮 Process button interactions (must be called early so state is ready for paint)
  if (!net.sandboxed) {
    if (login && !login.btn.disabled) {
      login.btn.act(e, {
        down: () => downSound(),
        push: () => {
          pushSound();
          net.login();
          // if (net.iframe) jump("login-wait");
        },
        cancel: () => cancelSound(),
      });
    }
  }

  if (!net.iframe) {
    if (signup && !signup.btn.disabled) {
      signup.btn.act(e, {
        down: () => downSound(),
        push: () => {
          pushSound();
          net.signup();
          // if (net.iframe) jump("signup-wait");
        },
        cancel: () => cancelSound(),
      });
    }
  }

  // End of button processing


  // 📦 SHOP box button interaction
  {
    const showLoginCurtainAct =
      (!login?.btn.disabled && !profile) ||
      (!login && !profile?.btn.disabled);
    const promptHasContentAct = system.prompt.input.text && system.prompt.input.text.length > 0;
    const shouldShow = showLoginCurtainAct && !system.prompt.input.canType && !promptHasContentAct;
    if (shouldShow) {
      products.act(
        { needsPaint, net, screen, jump, send, sound: { play, synth }, ui },
        e,
        {
          over: () => needsPaint(),
          down: () => downSound(),
          push: () => pushSound(),
          cancel: () => cancelSound(),
        }
      );
    }
  }

  // 🎰 UNITICKER button handler (unified ticker combining all content)
  if (unitickerButton && !unitickerButton.disabled) {
    unitickerButton.act(e, {
      down: () => {
        downSound();
        if (uniticker) {
          uniticker.paused = true;
          unitickerButton.scrubStartX = e.x;
          unitickerButton.scrubInitialOffset = uniticker.getOffset();
          unitickerButton.hasScrubbed = false;
        }
        needsPaint();
      },
      scrub: (btn) => {
        if (uniticker && e.x !== undefined && e.y !== undefined) {
          const scrubDelta = e.x - unitickerButton.scrubStartX;
          let newOffset = unitickerButton.scrubInitialOffset - scrubDelta;

          if (newOffset < 0) {
            newOffset = newOffset * 0.3; // Elastic effect
          }

          uniticker.setOffset(newOffset);
          unitickerButton.hasScrubbed = Math.abs(scrubDelta) > 5;

          synth({
            type: "sine",
            tone: 1200 + Math.abs(scrubDelta) * 2,
            attack: 0.005,
            decay: 0.9,
            volume: 0.08,
            duration: 0.01,
          });

          needsPaint();
        }
      },
      push: () => {
        if (!unitickerButton.hasScrubbed) {
          pushSound();
        }

        if (!unitickerButton.hasScrubbed && unitickerButton.hoveredItem) {
          // Jump to the hovered item's destination
          const item = unitickerButton.hoveredItem;
          const destination = item.code;

          // Set prompt input text to show what's loading
          system.prompt.input.text = destination;
          system.prompt.input.snap();

          // Jump to the destination
          jump(destination);
        } else {
          if (uniticker) {
            uniticker.paused = false;
          }
        }
        unitickerButton.hasScrubbed = false;
      },
      cancel: () => {
        cancelSound();
        if (uniticker) {
          uniticker.paused = false;
        }
        unitickerButton.hasScrubbed = false;
      },
    });
  }

  // (DEPRECATED - Now using uniticker) Chat ticker button
  if (chatTickerButton && !chatTickerButton.disabled) {
    chatTickerButton.act(e, {
      down: () => {
        downSound();
        if (chatTicker) {
          chatTicker.paused = true;
          chatTickerButton.scrubStartX = e.x;
          chatTickerButton.scrubInitialOffset = chatTicker.getOffset();
          chatTickerButton.hasScrubbed = false;
        }
        needsPaint();
      },
      scrub: (btn) => {
        if (chatTicker && e.x !== undefined && e.y !== undefined) {
          const scrubDelta = e.x - chatTickerButton.scrubStartX;
          let newOffset = chatTickerButton.scrubInitialOffset - scrubDelta;
          if (newOffset < 0) {
            newOffset = newOffset * 0.3;
          }
          chatTicker.setOffset(newOffset);
          chatTickerButton.hasScrubbed = Math.abs(scrubDelta) > 5;
          synth({
            type: "sine",
            tone: 1200 + Math.abs(scrubDelta) * 2,
            attack: 0.005,
            decay: 0.9,
            volume: 0.08,
            duration: 0.01,
          });
          needsPaint();
        }
      },
      push: () => {
        if (!chatTickerButton.hasScrubbed) {
          pushSound();
        }
        if (!chatTickerButton.hasScrubbed) {
          jump("chat");
        } else {
          if (chatTicker) {
            chatTicker.paused = false;
          }
        }
        chatTickerButton.hasScrubbed = false;
      },
      cancel: () => {
        cancelSound();
        if (chatTicker) {
          chatTicker.paused = false;
        }
        chatTickerButton.hasScrubbed = false;
      },
    });
  }

  // (DEPRECATED - Now using uniticker) Clock chat ticker button
  if (clockChatTickerButton && !clockChatTickerButton.disabled) {
    clockChatTickerButton.act(e, {
      down: () => {
        downSound();
        if (clockChatTicker) {
          clockChatTicker.paused = true;
          clockChatTickerButton.scrubStartX = e.x;
          clockChatTickerButton.scrubInitialOffset = clockChatTicker.getOffset();
          clockChatTickerButton.hasScrubbed = false;
        }
        needsPaint();
      },
      scrub: (btn) => {
        if (clockChatTicker && e.x !== undefined && e.y !== undefined) {
          const scrubDelta = e.x - clockChatTickerButton.scrubStartX;
          let newOffset = clockChatTickerButton.scrubInitialOffset - scrubDelta;
          if (newOffset < 0) {
            newOffset = newOffset * 0.3;
          }
          clockChatTicker.setOffset(newOffset);
          clockChatTickerButton.hasScrubbed = Math.abs(scrubDelta) > 5;
          synth({
            type: "sine",
            tone: 1200 + Math.abs(scrubDelta) * 2,
            attack: 0.005,
            decay: 0.9,
            volume: 0.08,
            duration: 0.01,
          });
          needsPaint();
        }
      },
      push: () => {
        if (!clockChatTickerButton.hasScrubbed) {
          pushSound();
        }
        if (!clockChatTickerButton.hasScrubbed) {
          jump("laer-klokken");
        } else {
          if (clockChatTicker) {
            clockChatTicker.paused = false;
          }
        }
        clockChatTickerButton.hasScrubbed = false;
      },
      cancel: () => {
        cancelSound();
        if (clockChatTicker) {
          clockChatTicker.paused = false;
        }
        clockChatTickerButton.hasScrubbed = false;
      },
    });
  }

  // Content ticker button (invisible, just for click interaction)
  if (contentTickerButton && !contentTickerButton.disabled) {
    contentTickerButton.act(e, {
      down: () => {
        downSound();
        if (contentTicker) {
          contentTicker.paused = true;
          contentTickerButton.scrubStartX = e.x;
          contentTickerButton.scrubInitialOffset = contentTicker.getOffset();
          contentTickerButton.hasScrubbed = false;
        }
        needsPaint();
      },
      scrub: (btn) => {
        if (contentTicker && e.x !== undefined && e.y !== undefined) {
          const scrubDelta = e.x - contentTickerButton.scrubStartX;
          let newOffset = contentTickerButton.scrubInitialOffset - scrubDelta;

          if (newOffset < 0) {
            newOffset = newOffset * 0.3; // Elastic effect
          }

          contentTicker.setOffset(newOffset);
          contentTickerButton.hasScrubbed = Math.abs(scrubDelta) > 5;

          synth({
            type: "sine",
            tone: 1200 + Math.abs(scrubDelta) * 2,
            attack: 0.005,
            decay: 0.9,
            volume: 0.08,
            duration: 0.01,
          });

          needsPaint();
        }
      },
      push: () => {
        if (!contentTickerButton.hasScrubbed) {
          pushSound();
        }

        if (!contentTickerButton.hasScrubbed && contentItems.length > 0) {
          // Jump to hovered item if one was hovered, otherwise first item
          const targetIndex = contentTickerButton.hoveredItemIndex >= 0 ?
                              contentTickerButton.hoveredItemIndex : 0;
          const item = contentItems[targetIndex];
          
          let destination;
          if (item.type === 'painting') {
            // For paintings, check if it's an anon painting (no handle) or has a handle
            // Anon paintings: jump directly to painting#CODE
            // User paintings: use #CODE to trigger the lookup
            if (!item.handle || item.handle === 'undefined' || item.handle === 'null') {
              // Anon painting - jump directly to painting piece with code
              destination = `painting#${item.code}`;
            } else {
              // User painting with handle - use # prefix to trigger lookup
              destination = `#${item.code}`;
            }
          } else {
            const prefix = item.type === 'kidlisp' ? '$' : item.type === 'tape' ? '!' : '*';
            destination = `${prefix}${item.code}`;
          }

          // Set prompt input text to show what's loading (like typing and pressing enter)
          system.prompt.input.text = destination;

          // Move cursor to end of input (like after typing)
          system.prompt.input.snap();

          // Jump to the destination
          jump(destination);
        } else {
          if (contentTicker) {
            contentTicker.paused = false;
          }
        }
        contentTickerButton.hasScrubbed = false;
      },
      cancel: () => {
        cancelSound();
        if (contentTicker) {
          contentTicker.paused = false;
        }
        contentTickerButton.hasScrubbed = false;
      },
    });
  }

  // Rollover keyboard locking.
  // TODO: ^ Move the below events, above to rollover events.
  if (
    e.is("draw") &&
    ((login?.btn.disabled === false && login?.btn.box.contains(e)) ||
      (signup?.btn.disabled === false && signup?.btn.box.contains(e)) ||
      (profile?.btn.disabled === false && profile?.btn.box.contains(e)) ||
      (commitBtn?.btn.disabled === false && commitBtn?.btn.box.contains(e)) ||
      (kidlispBtn?.btn.disabled === false && kidlispBtn?.btn.box.contains(e)) ||
      (clearBtn?.disabled === false && clearBtn?.box.contains(e)) ||
      isOverMotdHandle)
  ) {
    send({ type: "keyboard:lock" });
  }

  if (
    //system.prompt.input.backdropTouchOff === false &&
    (e.is("touch") || e.is("lift")) &&
    ((login?.btn.disabled === false && login?.btn.box.contains(e)) ||
      (signup?.btn.disabled === false && signup?.btn.box.contains(e)) ||
      (commitBtn?.btn.disabled === false && commitBtn?.btn.box.contains(e)) ||
      (kidlispBtn?.btn.disabled === false && kidlispBtn?.btn.box.contains(e)) ||
      (clearBtn?.disabled === false && clearBtn?.box.contains(e)) ||
      (products.getShopBoxButton()?.disabled === false && products.getShopBoxButton()?.box.contains(e)) ||
      (unitickerButton?.disabled === false && unitickerButton?.box.contains(e)) ||
      (chatTickerButton?.disabled === false && chatTickerButton?.box.contains(e)) ||
      (contentTickerButton?.disabled === false && contentTickerButton?.box.contains(e)) ||
      isOverMotdHandle ||
      (walletBtn?.btn.disabled === false && walletBtn?.btn.box.contains(e)) ||
      (profile?.btn.disabled === false &&
        profile?.btn.box.contains(e) &&
        profileAction === "profile"))
  ) {
    send({ type: "keyboard:lock" });
    system.prompt.input.backdropTouchOff = true;
  }

  if (e.is("lift") || e.is("touch")) needsPaint(); // Get button changes to
  //                                           ^      paint on-demand.
  // 🚨 Idea: It would be nice to pass     ----^
  //          what needs to be painted
  //          so the knowledge can be
  //          used in the `paint` function
  //          to allow for manual optimizations. 23.06.20.00.30

  if (profile && !profile.btn.disabled) {
    profile.btn.act(e, {
    down: () => {
      downSound();
      if (profileAction !== "profile") {
        //send({ type: "keyboard:enabled" }); // Enable keyboard flag.
        // send({ type: "keyboard:unlock" });
      }
    },
    push: () => {
      pushSound();
      if (profileAction === "resend-verification") {
        // notice("RESEND EMAIL?", ["yellow", "blue"]);
        const text = "email " + user.email;
        resendVerificationText = text;
        system.prompt.input.text = text;
        system.prompt.input.snap();
        system.prompt.input.runnable = true;
        firstActivation = false;
        send({ type: "keyboard:text:replace", content: { text } });
        // send({ type: "keyboard:unlock" });
        // send({ type: "keyboard:open" });
      } else if (profileAction === "profile") {
        jump(handle() || "profile");
      } else if (profileAction === "set-handle") {
        notice("ENTER HANDLE", ["yellow", "blue"]);
        const text = "handle ";
        system.prompt.input.text = text;
        system.prompt.input.snap();
        system.prompt.input.runnable = true;
        firstActivation = false;
        send({ type: "keyboard:text:replace", content: { text } });
        // send({ type: "keyboard:unlock" });
        // send({ type: "keyboard:open" });
      }
    },
    cancel: () => {
      cancelSound();

      if (profileAction !== "profile") {
        // send({ type: "keyboard:disabled" }); // Disable keyboard flag.
        send({ type: "keyboard:lock" });
        system.prompt.input.backdropTouchOff = true;
      }
    },
  });
  }


  // 📦 Commit button - reload page when update is ready, else navigate to commits
  if (commitBtn && !commitBtn.btn.disabled) {
    commitBtn.btn.act(e, {
      down: () => downSound(),
      push: () => {
        pushSound();
        if (updateAvailable) {
          location.reload();
        } else {
          jump("commits");
        }
      },
      cancel: () => cancelSound(),
    });
  }

  // 🔷 Tezos wallet button - navigate to wallet piece
  if (walletBtn && !walletBtn.btn.disabled) {
    walletBtn.btn.act(e, {
      down: () => downSound(),
      push: () => {
        pushSound();
        jump("wallet");
      },
      cancel: () => cancelSound(),
    });
  }

  // 🟢 KidLisp.com button - copy code and open IDE
  if (kidlispBtn && !kidlispBtn.btn.disabled) {
    kidlispBtn.btn.act(e, {
      down: () => downSound(),
      push: () => {
        pushSound();
        // Get the current code from the prompt
        const code = system.prompt.input.text || "";
        // Encode the code for URL
        const encodedCode = encodeURIComponent(code);
        // Open KidLisp.com IDE with the code
        const kidlispUrl = `https://kidlisp.com?code=${encodedCode}`;
        // Copy code to clipboard first
        if (navigator.clipboard?.writeText) {
          navigator.clipboard.writeText(code).catch(() => {});
        }
        // Open in new tab
        if (typeof window !== "undefined" && window.open) {
          window.open(kidlispUrl, "_blank");
        } else {
          // Fallback for non-browser environments
          send({ type: "open-url", content: kidlispUrl });
        }
      },
      cancel: () => cancelSound(),
    });
  }

  // 🖥️ Screen
  if (e.is("reframed")) {
    positionWelcomeButtons(screen, net.iframe);
    nopaint_adjust(api);
    system.nopaint.present(api);
  }

  // ⌨️ Keyboard (Skip startup sound if a key is pressed or text is pasted.)
  if (e.is("keyboard:open") && firstActivation && e.method !== "pointer") {
    firstActivation = false;
    // console.log("⌨️ First keyboard activation completed!");
  }

  // 🎹 Track keyboard key presses for musical sound pitch/pan
  if (e.name?.indexOf("keyboard:down:") === 0 && e.key?.length === 1) {
    lastPressedKey = e.key.toLowerCase();
  }

  // if (e.is("pasted:text")) firstActivation = false;

  // Whenever the text input is edited.
  if (
    e.is("prompt:text:replace") &&
    !firstActivation &&
    system.prompt.input.canType
  ) {
    if (!e.mute) {
      // 🎹 Apply notepat-style pitch and pan based on QWERTY layout
      const musicData = lastPressedKey ? QWERTY_MUSIC_MAP[lastPressedKey] : null;
      const baseVolume = 0.2 + (num.randInt(100) / 100) * 0.4;

      if (musicData) {
        const speed = semitoneToPlaybackRate(musicData.semitone);
        play(keyboardSfx, {
          volume: baseVolume,
          speed: speed,
          pan: musicData.pan
        });
      } else {
        // Fallback for unmapped keys (numbers, punctuation, etc.)
        play(keyboardSfx, { volume: baseVolume });
      }
    }

    // Compute autocompletions...
    activeCompletions.length = 0;
    if (e.text.length > 0) {
      keys(autocompletions).forEach((key) => {
        if (key.startsWith(e.text)) activeCompletions.push(key);
      });
      //  if (activeCompletions.length > 0)
      //  console.log("✍️ Completions:", activeCompletions);
    }

    if (
      (e.text === "dl" || e.text === "download") &&
      canShare &&
      store["painting"]
    ) {
      downloadPainting(api, defaultDownloadScale, true); // Trigger early download response, before the user enters.
    }
  }

  // 🔍 @handle autocomplete keyboard handling
  if (handleAutocomplete?.visible && handleAutocomplete.items.length > 0) {
    // Arrow up/down to navigate
    if (e.is("keyboard:down:arrowup")) {
      handleAutocomplete.selectedIndex = handleAutocomplete.selectedIndex > 0
        ? handleAutocomplete.selectedIndex - 1
        : handleAutocomplete.items.length - 1;
      needsPaint();
      return; // Consume event
    }
    if (e.is("keyboard:down:arrowdown")) {
      handleAutocomplete.selectedIndex = (handleAutocomplete.selectedIndex + 1) % handleAutocomplete.items.length;
      needsPaint();
      return; // Consume event
    }
    // Tab or Enter to complete
    if ((e.is("keyboard:down:tab") || e.is("keyboard:down:enter")) && handleAutocomplete.selected) {
      const cursorPos = system.prompt.input.prompt?.textPos?.() ?? system.prompt.input.text.length;
      const newText = handleAutocomplete.getCompletedText(system.prompt.input.text, cursorPos);
      system.prompt.input.text = newText + " "; // Add space after handle
      system.prompt.input.snap();
      send({ type: "keyboard:text:replace", content: { text: system.prompt.input.text } });
      handleAutocomplete.hide();
      needsPaint();
      return; // Consume event
    }
    // Escape to dismiss
    if (e.is("keyboard:down:escape")) {
      handleAutocomplete.hide();
      needsPaint();
      return; // Consume event
    }
  }

  // 🔍 @handle autocomplete pointer/mouse handling
  if (handleAutocomplete?.visible && handleAutocomplete.items.length > 0) {
    const result = handleAutocomplete.handlePointer(e);
    if (result.clicked) {
      // Item was clicked - complete with the clicked item
      const cursorPos = system.prompt.input.prompt?.textPos?.() ?? system.prompt.input.text.length;
      const newText = handleAutocomplete.getCompletedText(system.prompt.input.text, cursorPos);
      system.prompt.input.text = newText + " "; // Add space after handle
      system.prompt.input.snap();
      send({ type: "keyboard:text:replace", content: { text: system.prompt.input.text } });
      handleAutocomplete.hide();
      needsPaint();
      return; // Consume event
    }
    if (result.consumed) {
      needsPaint(); // Update hover state
      return; // Consume event to prevent other interactions
    }
  }

  if (e.is("keyboard:down:tab") && e.key === "Tab" && activeCompletions[0]) {
    // console.log("Tab completing:", activeCompletions[0]);
    // TODO: The text input object needs to be updated here also...
    system.prompt.input.text = activeCompletions[0];
    system.prompt.input.snap();
    send({
      type: "keyboard:text:replace",
      content: { text: system.prompt.input.text },
    });
  }

  function autocompleteChar() {
    const text = system.prompt.input.text;
    const completion = activeCompletions[0];
    if (completion && text !== completion) {
      const cursorX = system.prompt.input.prompt.cursor.x;
      system.prompt.input.text = completion.slice(0, cursorX + 1);
      system.prompt.input.snap();
      send({
        type: "keyboard:text:replace",
        content: { text: system.prompt.input.text },
      });
    }
  }

  if (e.is("keyboard:down:arrowright")) {
    if (system.prompt.input.prompt.textPos() === undefined) autocompleteChar();
  }

  if (e.is("textinput:shift-right:empty")) autocompleteChar();

  // 📊 Toggle FPS meter with backtick key
  if (e.is("keyboard:down:`")) {
    showFpsMeter = !showFpsMeter;
    needsPaint();
  }

  // if (e.is("keyboard:down") && e.key !== "Enter") {
  // console.log("down key...");
  // play(keyboardSfx, { volume: 0.2 + (num.randInt(100) / 100) * 0.4 });
  // }

  // 💾 Piece / disk loading
  if (e.is("load-error")) {
    makeFlash(api, false);
    flashColor = [255, 0, 0];
    if (MetaBrowser) api.system.prompt.input.canType = false;
    needsPaint();
  }

  // 🧹 Blank button
  if (clearBtn && !clearBtn.disabled) {
    clearBtn.act(e, {
      down: () => {
        downSound();
        system.prompt.input.backdropTouchOff = true;
      },
      push: () => {
        if (clearBtnConfirming) {
          clearTimeout(clearBtnConfirmTimeout);
          clearBtnConfirming = false;
          pushSound();
          system.prompt.input.blank();
          send({ type: "keyboard:text:replace", content: { text: "" } });
          needsPaint();
        } else {
          clearBtnConfirming = true;
          notice("TAP BLANK TO CLEAR", ["red"]);
          clearBtnConfirmTimeout = setTimeout(() => {
            clearBtnConfirming = false;
            needsPaint();
          }, 2000);
          needsPaint();
        }
      },
      cancel: () => {
        cancelSound();
        system.prompt.input.backdropTouchOff = true;
      },
    });
  }
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 🖥️ Run When the Prompt is activated.
function activated($, state) {
  // Clear any latent text when activating to prevent MOTD showing when focused
  // but only if we don't have params (which means we're not coming from backspace navigation)
  if (state === true && !$.params[0]) {
    $.system.prompt.input.text = "";
  }

  if (firstActivation) {
    $.sound.play(startupSfx); // Play startup sound...
    flashColor = theme === "serious" ? ($.dark ? [255] : [0]) :
                 theme === "neo" ? ($.dark ? [0, 255, 0] : [0, 80, 0]) :
                 scheme.dark.block; // Trigger startup animation...
    makeFlash($ /*, $.params[0]*/); // Always sets firstActivation flag to false.
  }
  // console.log(state, firstCommandSent)
  // if (state === false && firstCommandSent) return;
  const hideCurtain = theme !== "default";
  if (login) login.btn.disabled = state || hideCurtain;
  if (signup) signup.btn.disabled = state || hideCurtain;
  if (profile) profile.btn.disabled = state || hideCurtain;
  if (chatTickerButton) chatTickerButton.disabled = state;
}

// 💬 Receive each response in full.
function reply(text) {
  // firstCommandSent = true;
  // console.log("😀 Replied with:", text || "Halted?");
}

// 📰 Meta
function meta() {
  return {
    title: "Prompt",
    desc: "Enter anything to get started.",
  };
}

// 👋 Leave
function leave() {
  motdController?.abort(); // Abort any motd update.
  versionPollController?.abort(); // Stop version long-poll.
}

export const nohud = true;

export {
  before,
  after,
  forgetful,
  halt,
  boot,
  paint,
  sim,
  act,
  activated,
  reply,
  receive,
  meta,
  leave,
};

export const system = "prompt:character"; // or "prompt:code"

// Prompt configuration overrides.
export const wrap = "word"; // or "char"
export const scheme = {
  dark: {
    text: [255, 100],
    background: [70, 50, 100],
    prompt: [200, 30, 100, 200],
    block: [200, 30, 100],
    highlight: [255, 100, 0],
    guideline: [0, 0, 255, 64],
    login: [[0, 0, 64], 255, 255, [0, 0, 64]],
    loginRollover: [[0, 100, 160], [120, 220, 255], [120, 220, 255], [0, 100, 160]], // Brighter blue glow on hover
    loginDown: [[0, 40, 120], [80, 160, 200], [255, 255, 255], [0, 40, 120]], // Pressed (darker blue, white text)
    signup: [[0, 64, 0], 255, 255, [0, 64, 0]],
    signupRollover: [[40, 100, 40], [100, 255, 100], [100, 255, 100], [40, 100, 40]], // Bright green glow
    profile: [[64, 0, 64], [255, 100, 255], [255, 100, 255], [64, 0, 64]], // Magenta base
    profileRollover: [[100, 40, 120], [255, 150, 255], [255, 150, 255], [100, 40, 120]], // Brighter magenta
    // Paste/Enter button rollover (used by TextInput)
    btnRollover: [80, 50, 120], // Purple tint fill
    btnRolloverTxt: [220, 180, 255], // Light purple text
    handleColor: [255, 0, 255, 128],
    auto: "white",
    statusColor: "lime",
    focusOutline: "brown",
  },
  light: {
    text: [40, 30, 90], // Dark purple-blue (readable on legal pad yellow)
    background: [252, 247, 197], // Slightly deeper legal pad yellow for better contrast
    prompt: [60, 40, 120], // Dark purple for prompt text
    block: [56, 122, 223],
    highlight: [246, 253, 195],
    guideline: [255, 207, 105],
    // login: [255, [0, 0, 64], [0, 0, 64], 255],
    login: [[0, 0, 128], 255, 255, [0, 0, 128]],
    loginRollover: [[60, 120, 220], [180, 230, 255], [180, 230, 255], [60, 120, 220]], // Brighter blue glow on hover
    loginDown: [[40, 90, 200], [150, 200, 255], [255, 255, 255], [40, 90, 200]], // Pressed (darker blue, white text)
    // signup: [255, [0, 64, 0], [0, 64, 0], 255],
    signup: [[0, 128, 0], 255, 255, [0, 128, 0]],
    signupRollover: [[50, 160, 50], [150, 255, 150], [150, 255, 150], [50, 160, 50]], // Lighter green glow
    profile: [[128, 0, 128], [255, 100, 255], [255, 100, 255], [128, 0, 128]], // Magenta base
    profileRollover: [[160, 50, 160], [255, 170, 255], [255, 170, 255], [160, 50, 160]], // Brighter magenta
    // Paste/Enter button rollover (used by TextInput)
    btnRollover: [200, 180, 240], // Light purple tint fill
    btnRolloverTxt: [80, 40, 140], // Dark purple text
    handleColor: [0, 0, 255, 128],
    auto: "red",
    statusColor: "darkgreen",
    focusOutline: "aqua",
  },
  // 🌐 Neo mode — matrix lime / black with blue & pink accents
  neo: {
    dark: {
      text: [0, 255, 0],          // lime text
      background: [0, 0, 0],      // black background
      prompt: [0, 255, 0],
      block: [0, 200, 0],
      highlight: [0, 255, 128],
      guideline: [0, 80, 0, 64],
      login: [[0, 20, 60], [0, 100, 255], [0, 100, 255], [0, 20, 60]],
      loginRollover: [[0, 40, 100], [80, 160, 255], [80, 160, 255], [0, 40, 100]],
      loginDown: [[0, 10, 50], [0, 60, 200], [255, 255, 255], [0, 10, 50]],
      signup: [[40, 0, 40], [255, 0, 200], [255, 0, 200], [40, 0, 40]],
      signupRollover: [[80, 0, 80], [255, 80, 230], [255, 80, 230], [80, 0, 80]],
      profile: [[40, 0, 40], [255, 0, 200], [255, 0, 200], [40, 0, 40]],
      profileRollover: [[80, 0, 80], [255, 80, 230], [255, 80, 230], [80, 0, 80]],
      btnRollover: [0, 30, 0],
      btnRolloverTxt: [0, 255, 0],
      handleColor: [0, 255, 0, 128],
      auto: "lime",
      statusColor: "lime",
      focusOutline: "#00cc00",
    },
    light: {
      text: [0, 0, 0],            // black text
      background: [0, 255, 0],    // lime background
      prompt: [0, 0, 0],
      block: [0, 30, 0],
      highlight: [0, 200, 0],
      guideline: [0, 60, 0, 64],
      login: [[0, 0, 80], [0, 0, 180], [0, 0, 180], [0, 0, 80]],
      loginRollover: [[0, 20, 120], [40, 40, 220], [40, 40, 220], [0, 20, 120]],
      loginDown: [[0, 0, 60], [0, 0, 140], [255, 255, 255], [0, 0, 60]],
      signup: [[60, 0, 60], [180, 0, 150], [180, 0, 150], [60, 0, 60]],
      signupRollover: [[100, 0, 100], [220, 0, 200], [220, 0, 200], [100, 0, 100]],
      profile: [[60, 0, 60], [180, 0, 150], [180, 0, 150], [60, 0, 60]],
      profileRollover: [[100, 0, 100], [220, 0, 200], [220, 0, 200], [100, 0, 100]],
      btnRollover: [0, 210, 0],
      btnRolloverTxt: [0, 0, 0],
      handleColor: [0, 0, 0, 128],
      auto: "black",
      statusColor: "darkgreen",
      focusOutline: "#005500",
    },
  },
  // 🎹 Serious mode — piano black & white
  serious: {
    dark: {
      text: [255],
      background: [0, 0, 0],
      prompt: [255],
      block: [255],
      highlight: [128],
      guideline: [60, 60, 60, 64],
      login: [[40, 40, 40], 200, 200, [40, 40, 40]],
      loginRollover: [[60, 60, 60], 220, 220, [60, 60, 60]],
      loginDown: [[30, 30, 30], 180, 255, [30, 30, 30]],
      signup: [[40, 40, 40], 200, 200, [40, 40, 40]],
      signupRollover: [[60, 60, 60], 220, 220, [60, 60, 60]],
      profile: [[40, 40, 40], 200, 200, [40, 40, 40]],
      profileRollover: [[60, 60, 60], 220, 220, [60, 60, 60]],
      btnRollover: [40, 40, 40],
      btnRolloverTxt: [220, 220, 220],
      handleColor: [255, 255, 255, 128],
      auto: "white",
      statusColor: "gray",
      focusOutline: "gray",
    },
    light: {
      text: [0],
      background: [255, 255, 255],
      prompt: [0],
      block: [0],
      highlight: [128],
      guideline: [200, 200, 200, 64],
      login: [[220, 220, 220], 60, 60, [220, 220, 220]],
      loginRollover: [[200, 200, 200], 40, 40, [200, 200, 200]],
      loginDown: [[230, 230, 230], 80, 0, [230, 230, 230]],
      signup: [[220, 220, 220], 60, 60, [220, 220, 220]],
      signupRollover: [[200, 200, 200], 40, 40, [200, 200, 200]],
      profile: [[220, 220, 220], 60, 60, [220, 220, 220]],
      profileRollover: [[200, 200, 200], 40, 40, [200, 200, 200]],
      btnRollover: [220, 220, 220],
      btnRolloverTxt: [40, 40, 40],
      handleColor: [0, 0, 0, 128],
      auto: "black",
      statusColor: "gray",
      focusOutline: "gray",
    },
  },
};

// 🌍 Interface language strings
export const languages = {
  en: {
    label: "English",
    buttons: {
      enter: "Enter",
      copy: { label: "Copy", copied: "Copied", failed: "Failed" },
      paste: { label: "Paste", pasted: "Pasted", empty: "Empty", failed: "Failed" },
    },
    login: "Log in",
    signup: "I'm new",
    resendEmail: "Resend email",
  },
  da: {
    label: "Dansk",
    buttons: {
      enter: "Kør",
      copy: { label: "Kopiér", copied: "Kopieret", failed: "Fejl" },
      paste: { label: "Indsæt", pasted: "Indsat", empty: "Tom", failed: "Fejl" },
    },
    login: "Log ind",
    signup: "Jeg er ny",
    resendEmail: "Send igen",
  },
  es: {
    label: "Español",
    buttons: {
      enter: "Entrar",
      copy: { label: "Copiar", copied: "Copiado", failed: "Error" },
      paste: { label: "Pegar", pasted: "Pegado", empty: "Vacío", failed: "Error" },
    },
    login: "Entrar",
    signup: "Soy nuevo",
    resendEmail: "Reenviar",
  },
  ru: {
    label: "Русский",
    buttons: {
      enter: "Ввод",
      copy: { label: "Копия", copied: "Скопировано", failed: "Ошибка" },
      paste: { label: "Вставка", pasted: "Вставлено", empty: "Пусто", failed: "Ошибка" },
    },
    login: "Войти",
    signup: "Я новый",
    resendEmail: "Повторить",
  },
};

let lang = "en"; // Current interface language

function currentLang() {
  return languages[lang] || languages.en;
}

// 📚 Library
//   (Useful functions used throughout the piece)

let motdController;

function setMotdCandidate(candidate) {
  if (!candidate?.mood) return;
  motd = candidate.mood;
  const apiHandle = candidate.handle || candidate.handleInfo?.handle || candidate.owner?.handle || candidate.user?.handle || null;
  motdByHandle = apiHandle
    ? (apiHandle.startsWith("@") ? apiHandle : `@${apiHandle}`)
    : null;
}

function setTheme(newTheme, $) {
  theme = newTheme;
  if ($?.store) {
    $["store"]["prompt:theme"] = theme;
    $["store"].persist?.("prompt:theme");
  }
  const hideCurtain = theme !== "default";
  if (login) login.btn.disabled = hideCurtain;
  if (signup) signup.btn.disabled = hideCurtain;
  if (profile) profile.btn.disabled = hideCurtain;
}

async function makeMotd({ system, needsPaint, handle, user, net, api, notice }) {
  motdByHandle = null;
  motdCandidates = [];
  motdCandidateIndex = 0;
  lastMotdCycleTime = 0;
  // Use funding mode message or default (alternate EN/DA every 3 seconds)
  if (isCriticalFunding) {
    const langPhase = Math.floor(Date.now() / 3000) % 2;
    motd = langPhase === 0 ? "CRITICAL MEDIA SERVICES OFFLINE" : "KRITISKE MEDIETJENESTER OFFLINE";
    motdByHandle = null;
    net.motd = motd;
  } else {
    motd = "aesthetic.computer";
    net.motd = motd;
  }
  motdController = new AbortController();

  // Skip fetching mood in critical funding mode - use the hardcoded message
  if (isCriticalFunding) return;

  try {
    const res = await fetch("/api/mood/moods-of-the-day?list=1", {
      signal: motdController.signal,
    });
    if (res.status === 200) {
      const data = await res.json();
      if (Array.isArray(data.moods) && data.moods.length > 0) {
        motdCandidates = data.moods;
        motdCandidateIndex = 0;
        setMotdCandidate(motdCandidates[0]);
        net.motd = motd;
        lastMotdCycleTime = performance.now();
      } else if (data.mood) {
        setMotdCandidate(data);
        net.motd = motd;
      }
      needsPaint();
    } else {
      console.warn("😢 No mood found.");
    }
  } catch (err) {
    // console.warn("🙁 System `mood` fetch aborted.");
  }
}

// Fetch all content (kidlisp, painting, tape) and combine into a single shuffled array
async function fetchContentItems(api) {
  if (contentFetchInFlight) return;
  contentFetchInFlight = true;
  lastContentFetchAt = Date.now();
  try {
    // Fetch all types mixed with frecency using the "sprinkle" filter
    // This creates a diverse feed mixing tapes, paintings, and kidlisp based on
    // recency and popularity (hits)
    const apiUrl = (typeof window !== 'undefined' ? window.location.origin : '') + "/api/tv?filter=sprinkle&limit=60";
    const res = await fetch(apiUrl);
    if (res.status === 200) {
      const data = await res.json();
      const items = [];

      // Use mixed feed if available (interwoven by timestamp)
      if (data.mixed && Array.isArray(data.mixed)) {
        data.mixed.forEach(item => {
          if (!item.code) return; // Skip items without code

          const baseItem = {
            type: item.type,
            code: item.code,
            timestamp: item.timestamp || item.created_at || item.when,
            author: item.owner?.handle || null
          };

          // Sanitize handle to prevent string "undefined" or "null"
          const sanitizedHandle = (item.owner?.handle &&
                                  typeof item.owner.handle === 'string' &&
                                  item.owner.handle !== 'undefined' &&
                                  item.owner.handle !== 'null' &&
                                  item.owner.handle.length > 0)
            ? item.owner.handle
            : null;

          if (item.type === 'kidlisp') {
            items.push({ ...baseItem, source: item.source });
          } else if (item.type === 'painting') {
            items.push({
              ...baseItem,
              slug: item.slug,
              handle: sanitizedHandle,
              mediaPath: item.media?.path || null,
              mediaUrl: item.media?.url || null
            });
          } else if (item.type === 'tape') {
            items.push({
              ...baseItem,
              slug: item.slug,
              handle: sanitizedHandle,
              mediaPath: item.media?.path || null,
              mediaUrl: item.media?.url || null,
              title: item.title || null
            });
          }
        });
      } else {
        // Fallback: collect and manually interweave (for backwards compatibility)
        const allItems = [];

        // Collect kidlisp items
        if (data.media?.kidlisp && Array.isArray(data.media.kidlisp)) {
          data.media.kidlisp.forEach(item => {
            if (item.code) allItems.push({
              type: 'kidlisp',
              code: item.code,
              source: item.source,
              timestamp: item.timestamp || item.created_at || item.when,
              author: item.owner?.handle || null
            });
          });
        }

        // Collect painting items
        if (data.media?.paintings && Array.isArray(data.media.paintings)) {
          data.media.paintings.forEach(item => {
            const sanitizedHandle = (item.owner?.handle &&
                                    typeof item.owner.handle === 'string' &&
                                    item.owner.handle !== 'undefined' &&
                                    item.owner.handle !== 'null' &&
                                    item.owner.handle.length > 0)
              ? item.owner.handle
              : null;

            if (item.code) allItems.push({
              type: 'painting',
              code: item.code,
              slug: item.slug,
              handle: sanitizedHandle,
              mediaPath: item.media?.path || null,
              mediaUrl: item.media?.url || null,
              timestamp: item.timestamp || item.created_at || item.when,
              author: item.owner?.handle || null
            });
          });
        }

        // Collect tape items
        if (data.media?.tapes && Array.isArray(data.media.tapes)) {
          data.media.tapes.forEach(item => {
            if (item.code) allItems.push({
              type: 'tape',
              code: item.code,
              timestamp: item.timestamp || item.created_at || item.when,
              author: item.owner?.handle || null
            });
          });
        }

        // Sort by timestamp (newest first)
        allItems.sort((a, b) => {
          const timeA = new Date(a.timestamp || 0).getTime();
          const timeB = new Date(b.timestamp || 0).getTime();
          return timeB - timeA;
        });

        items.push(...allItems);
      }

      contentItems = items;

      console.log("✅ Content items loaded with sprinkle filter:", contentItems.length,
                  `(${items.filter(i => i.type === 'kidlisp').length} kidlisp, ` +
                  `${items.filter(i => i.type === 'painting').length} paintings, ` +
                  `${items.filter(i => i.type === 'tape').length} tapes)`);
      api.needsPaint();
    } else {
      console.warn("⚠️ Could not fetch content items. Status:", res.status);
      try {
        const errorText = await res.text();
        console.warn("⚠️ Error response:", errorText);
      } catch (e) {
        // Body may be unavailable
      }
    }
  } catch (err) {
    console.warn("⚠️ Content fetch error:", err);
  } finally {
    contentFetchInFlight = false;
  }
}

// Fetch recent messages from Laer-Klokken clock chat
async function fetchClockChatMessages() {
  if (clockChatFetchInFlight) return;
  clockChatFetchInFlight = true;
  lastClockChatFetchAt = Date.now();
  try {
    // Use the same chat API endpoint but for the "clock" chat instance
    const apiUrl = (typeof window !== 'undefined' ? window.location.origin : '') + "/api/chat/messages?instance=clock&limit=12";
    const res = await fetch(apiUrl);

    if (res.status === 200) {
      const data = await res.json();
      if (data.messages && Array.isArray(data.messages)) {
        clockChatMessages = data.messages;
        console.log(`🕰️ Loaded ${clockChatMessages.length} clock chat messages`);
        if (typeof promptNeedsPaint === "function") {
          promptNeedsPaint();
        }
      }
    } else {
      console.warn("⚠️ Clock chat fetch failed:", res.status);
    }
  } catch (err) {
    console.warn("⚠️ Clock chat fetch error:", err);
  } finally {
    clockChatFetchInFlight = false;
  }
}

// Fetch KidLisp source code for a given item
async function fetchKidlispSource(item, $) {
  if (!item || item.type !== 'kidlisp' || !item.code) return;

  // Prevent duplicate fetches - check and set atomically
  if (item.fetchAttempted) return;
  item.fetchAttempted = true; // Set IMMEDIATELY to block other calls

  try {
    const source = await fetchCachedCode(item.code);
    if (source) {
      item.source = source;
      item.fetchFailed = false;
      console.log(`✅ Fetched source for $${item.code} (${source.length} chars)`);
      $.needsPaint();
    } else {
      console.warn(`⚠️ No source returned for $${item.code}`);
      item.fetchFailed = true;
    }
  } catch (err) {
    console.warn(`⚠️ Could not fetch source for $${item.code}:`, err);
    item.fetchFailed = true;
  }
}

// Fetch painting image for a given item
async function fetchPaintingImage(item, $) {
  if (!item || item.type !== 'painting' || !item.code) {
    console.warn(`⚠️ fetchPaintingImage called with invalid item:`, item);
    return;
  }

  // Prevent duplicate fetches - check and set atomically
  if (item.fetchAttempted) {
    console.log(`🖼️ Already attempted fetch for #${item.code}, skipping`);
    return;
  }
  item.fetchAttempted = true; // Set IMMEDIATELY to block other calls

  let imageUrl; // Declare outside try block so it's accessible in catch
  try {
    // Prefer mediaPath from API, fall back to constructing path
    if (item.mediaPath && !item.mediaPath.includes('/undefined/')) {
      // mediaPath is a relative path like /media/@user/painting/file.png
      // Convert to absolute URL using current origin
      imageUrl = `${location.origin}${item.mediaPath}`;
    } else if (item.handle &&
               typeof item.handle === 'string' &&
               item.handle !== 'undefined' &&
               item.handle !== 'null' &&
               item.handle.length > 0) {
      // User painting with valid handle (check both string and actual null/undefined)
      imageUrl = `${location.origin}/media/@${item.handle}/painting/${item.slug}.png`;
    } else {
      // Anonymous painting - use /media/paintings/CODE route
      console.log(`🖼️ Using anonymous painting route for #${item.code} (handle: ${JSON.stringify(item.handle)})`);
      imageUrl = `${location.origin}/media/paintings/${item.code}.png`;
    }

    // Load the image
    console.log(`🖼️ Attempting to load image for #${item.code} from: ${imageUrl}`);
    const result = await $.net.preload(imageUrl);
    if (result && result.img) {
      // Extract the actual image bitmap from the result
      item.image = result.img;
      item.fetchFailed = false;
      console.log(`✅ Fetched image for #${item.code} (${result.img.width}x${result.img.height})`);
      $.needsPaint();
    } else {
      console.warn(`⚠️ No image returned for #${item.code} (tried: ${imageUrl})`, result);
      item.fetchFailed = true;
    }
  } catch (err) {
    console.warn(`⚠️ Could not fetch image for #${item.code}:`, imageUrl, err);
    item.fetchFailed = true;
  }
}

// Fetch tape audio and frames for preview playback
async function fetchTapeAudio(item, $) {
  if (!item || item.type !== 'tape' || !item.code) {
    console.warn(`⚠️ fetchTapeAudio called with invalid item:`, item);
    return;
  }

  // Prevent duplicate fetches
  if (item.fetchAttempted) {
    console.log(`🎵 Already attempted fetch for !${item.code}, skipping`);
    return;
  }
  item.fetchAttempted = true;

  try {
    console.log(`🎵 Loading tape !${item.code}`);

    // Fetch tape metadata first
    const metadataResponse = await fetch(`/api/get-tape?code=${item.code}`);
    if (!metadataResponse.ok) {
      throw new Error(`Failed to load tape metadata: ${metadataResponse.status}`);
    }

    const metadata = await metadataResponse.json();

    if (metadata.nuked) {
      throw new Error(`Tape !${item.code} has been deleted`);
    }

    // Construct ZIP URL
    const zipUrl = `${location.origin}/media/tapes/${item.code}`;
    const tapeId = `prompt-tape-${item.code}`;

    // Store metadata and URLs
    item.metadata = metadata;
    item.zipUrl = zipUrl;
    item.tapeId = tapeId;
    item.isLoading = true;
    item.loadProgress = 0;
    item.loadPhase = 'downloading';

    // Check if tape is already loaded in bios TapeManager
    // (We can't access globalThis.tapeManager directly since it's in the worker,
    // but we can send a message to check and get frames back)
    item.isLoading = true;
    item.loadProgress = 0;
    item.loadPhase = 'checking';

    // Send preload request to bios to load frames (or get existing frames)
    console.log(`📼 Sending tape:preload with zipUrl: ${zipUrl}`);
    $.send({
      type: "tape:preload",
      content: {
        tapeId: tapeId,
        code: item.code,
        zipUrl: zipUrl,
        metadata: metadata,
        requestFrames: true // Tell bios to send frames back to disk
      }
    });

    console.log(`✅ Initiated tape load for !${item.code}`);
    item.fetchFailed = false;
    $.needsPaint();
  } catch (err) {
    console.warn(`⚠️ Could not prepare tape for !${item.code}:`, err);
    item.fetchFailed = true;
    item.fetchAttempted = false;
    throw err;
  }
}

// console.log("📨 ✅ receive function defined! typeof receive =", typeof receive);

// Render syntax-highlighted KidLisp source code
function renderKidlispSource($, source, x, y, maxWidth, maxLines, fadeAlpha) {
  const lines = source.split('\n').slice(0, maxLines);
  const charWidth = 4;
  const lineHeight = 10;
  const isLightMode = !$.dark;

  // Create temporary KidLisp instance for color mapping
  const tempKidlisp = new KidLisp();
  tempKidlisp.syntaxHighlightSource = source;
  tempKidlisp.isEditMode = true; // Enable edit mode to prevent transparent text

  // Helper to check if color is too bright for light mode background
  const needsShadow = (rgb) => {
    if (!isLightMode) return false;
    // Calculate luminance - bright colors need shadow on light bg
    const luminance = (0.299 * rgb.r + 0.587 * rgb.g + 0.114 * rgb.b);
    return luminance > 120; // Lower threshold to catch more bright colors like cyan
  };

  // Helper to render a character with optional shadow
  const writeCharWithShadow = (char, charX, charY, rgb) => {
    if (needsShadow(rgb)) {
      // Draw dark purple-blue shadow offset by 1 pixel (matches HUD shadow)
      $.ink([30, 20, 50, Math.floor(fadeAlpha * 0.7)]).write(char, { x: charX + 1, y: charY + 1 }, undefined, undefined, false, "MatrixChunky8");
    }
    $.ink([rgb.r, rgb.g, rgb.b, fadeAlpha]).write(char, { x: charX, y: charY }, undefined, undefined, false, "MatrixChunky8");
  };

  lines.forEach((line, lineIdx) => {
    const lineY = y + lineIdx * lineHeight;
    let currentX = x;

    // Tokenize this line
    const tokens = tokenize(line);
    let sourceIndex = 0;

    tokens.forEach((token, tokenIdx) => {
      // Get color for this token
      const colorName = tempKidlisp.getTokenColor(token, tokens, tokenIdx);

      // Handle fade: expressions specially
      if (token.startsWith('fade:')) {
        const coloredFadeString = tempKidlisp.colorFadeExpression(token);
        // Parse the colored fade string format: \color1\text1\color2\text2...
        const segments = coloredFadeString.split('\\').filter(s => s);

        for (let i = 0; i < segments.length; i += 2) {
          const segmentColor = segments[i];
          const segmentText = segments[i + 1] || '';

          const rgb = parseColorName(segmentColor);
          for (let charIdx = 0; charIdx < segmentText.length; charIdx++) {
            const char = segmentText[charIdx];
            writeCharWithShadow(char, currentX, lineY, rgb);
            currentX += charWidth;
          }
        }
      } else {
        // Normal token - render each character
        const rgb = parseColorName(colorName);
        for (let charIdx = 0; charIdx < token.length; charIdx++) {
          const char = token[charIdx];
          writeCharWithShadow(char, currentX, lineY, rgb);
          currentX += charWidth;
        }
      }

      // Add space between tokens (if there was one in original)
      const tokenEnd = line.indexOf(token, sourceIndex) + token.length;
      if (tokenEnd < line.length && line[tokenEnd] === ' ') {
        currentX += charWidth;
        sourceIndex = tokenEnd + 1;
      } else {
        sourceIndex = tokenEnd;
      }
    });
  });
}

// Helper to parse color names to RGB
function parseColorName(colorName) {
  // Handle RGB format colors (like "192,192,192")
  if (colorName && colorName.includes(',')) {
    const parts = colorName.split(',').map(p => parseInt(p.trim()));
    return { r: parts[0] || 200, g: parts[1] || 200, b: parts[2] || 200 };
  }

  // Handle named colors
  const colorMap = {
    'cyan': { r: 64, g: 224, b: 208 },
    'teal': { r: 64, g: 224, b: 208 },
    'lime': { r: 50, g: 205, b: 50 },
    'green': { r: 0, g: 255, b: 0 },
    'yellow': { r: 255, g: 255, b: 0 },
    'orange': { r: 255, g: 165, b: 0 },
    'red': { r: 255, g: 0, b: 0 },
    'magenta': { r: 255, g: 0, b: 255 },
    'pink': { r: 255, g: 192, b: 203 },
    'purple': { r: 128, g: 0, b: 128 },
    'blue': { r: 0, g: 0, b: 255 },
    'white': { r: 255, g: 255, b: 255 },
    'gray': { r: 128, g: 128, b: 128 },
    'grey': { r: 128, g: 128, b: 128 },
    'silver': { r: 192, g: 192, b: 192 },
  };

  return colorMap[colorName] || { r: 200, g: 200, b: 200 };
}

function makeFlash($, clear = true, beep = false) {
  flash = new $.gizmo.Hourglass($.seconds(0.1), {
    flipped: () => {
      progressBar = -1;
      flashShow = false;
      flashPresent = false;
      flash = undefined;
      firstActivation = false;
      $.needsPaint();
    },
    autoFlip: true,
  });

  flashPresent = true;
  flashShow = true;
  if (clear === true) {
    $.system.prompt.input.blank(); // Clear the prompt.
  } else if (typeof clear === "string") {
    $.system.prompt.input.text = clear;
    $.system.prompt.input.snap();
    $.send({
      type: "keyboard:text:replace",
      content: { text: $.system.prompt.input.text },
    });
  }

  if (beep) $.beep();
}

function positionWelcomeButtons(screen, iframe) {
  if (login && signup) {
    login.reposition({ center: "xy", screen });
    signup.reposition({ center: "xy", screen });
    // Nudge signup and login by half their width.
    if (iframe) return; // But not if embedded in an iframe (where only login appears)
    let offset = 5; // With a fixed pixel offset.
    signup.btn.box.x += signup.btn.box.w / 2 + offset;
    login.btn.box.x -= login.btn.box.w / 2 + offset;
    if (screen.width % 2 !== 0) login.btn.box.x += 1; // Nudge odd display width.
  }

  if (profile) profile.reposition({ center: "xy", screen });
}

function downloadPainting({ download, num, store }, scale, sharing = false) {
  download(`painting-${num.timestamp()}.png`, store["painting"], {
    scale,
    // Read an integer parameter for scale.
    cropToScreen: !(store["painting:resolution-lock"] === true),
    // Only cut the download off at screen-size if user never
    // set a resolution.
    sharing,
  });
}

async function publishPiece(
  { api, send, jump, handle, upload },
  slug,
  source,
  ext = "mjs",
) {
  progressBar = 0; // Trigger progress bar rendering.
  try {
    const data = await upload("piece-" + slug + "." + ext, source, (p) => {
      console.log("🎁️ Publishing progress:", p);
      progressBar = p;
    });
    console.log("🪄 Code uploaded:", data);
    flashColor = [0, 255, 0];
    const route = handle() ? `${handle()}/${data.slug}` : data.slug;
    makeFlash(api, route);
    console.log(`\`${route}\` was published!`);
    jump(route);
  } catch (err) {
    console.error("🪄 Code upload failed:", err);
    send({
      type: "alert",
      content: `😥 Piece: \`${slug}\` failed to publish.`,
    });
    flashColor = [255, 0, 0];
    makeFlash(api);
  }
}

function fetchUser() {
  const { api, ui, user, handle, screen, store, jump, beep, broadcast } =
    fetchUserAPI;
  fetchingUser = true;
  fetch(`/user?from=${encodeURIComponent(user.email)}&withHandle=true`)
    .then((res) => res.json())
    .then((u) => {
      if (u.email_verified) {
        const previousHandle = handle();
        // console.log("🟪 User:", u, "Previous Handle:", previousHandle);
        if (previousHandle) {
          profileAction = "profile";
          profile = new ui.TextButton(previousHandle, { center: "xy", screen });
          profile.stickyScrubbing = true; // Prevent drag-between-button behavior

          // Apply colored handle asynchronously (update in-place to avoid flicker)
          fetchHandleColors(previousHandle, api).then((colors) => {
            if (colors) {
              profile.replaceLabel(colorizeHandle(previousHandle, colors));
            }
          });
        } else if (u.handle) {
          broadcast("handle:updated:" + u.handle);
          store["handle"] = u.handle;
          // Announce the handle change...
          jump("chat");
          beep();
        } else {
          profileAction = "set-handle";
          profile = new ui.TextButton("Create handle", {
            center: "xy",
            screen,
          });
          profile.stickyScrubbing = true; // Prevent drag-between-button behavior
          user.email_verified = true; // Set verified on global 'user' object.
          // store["aesthetic:refresh-user"] = true;
          // store.persist("aesthetic:refresh-user");
        }
        fetchingUser = false;
        flashColor = "lime";
        makeFlash(api, true, true);
      } else setTimeout(() => fetchUser(), 1000);
    })
    .catch((err) => setTimeout(() => fetchUser(), 1000));
}

// 📨 Receive messages from bios (for tape loading progress)
function receive(e) {
  // console.log(`📨 ✅✅✅ PROMPT.MJS RECEIVE called with type: ${e.type}, is() available: ${typeof e.is === 'function'}`);

  if (e.is("upload:status")) {
    const { stage, message } = e.content || {};
    const stageText = {
      init: "PREPARING",
      auth: "AUTHORIZING",
      database: "CONNECTING DATABASE",
      indexes: "PREPARING INDEXES",
      code: "GENERATING CODE",
      insert: "SAVING RECORD",
      sync: "SYNCING FEED",
      acl: "SETTING ACL",
      oven: "SENDING TO OVEN",
      fallback: "TRACKING (FALLBACK)",
      complete: "FINALIZING",
    };
    progressPhase = String(message || stageText[stage] || "PROCESSING").toUpperCase();

    // Keep "done" in indeterminate mode while backend stages are running.
    if (progressBar < 0) {
      progressBar = -2;
      progressPercentage = -1;
    }
    promptNeedsPaint?.();
  }

  if (e.is("tape:load-progress")) {
    const { code, phase, progress, loadedFrames, totalFrames } = e.content || {};

    // Find the tape item in contentItems
    const tapeItem = contentItems.find(item => item.type === 'tape' && item.code === code);
    if (tapeItem) {
      tapeItem.loadPhase = phase;
      tapeItem.loadProgress = progress;

      if (phase === 'frames') {
        tapeItem.loadMessage = `${loadedFrames}/${totalFrames} FRAMES`;
      } else if (phase === 'downloading') {
        tapeItem.loadMessage = 'DOWNLOADING';
      } else if (phase === 'audio') {
        tapeItem.loadMessage = 'LOADING AUDIO';
      }
    }
  }

  if (e.is("tape:preloaded")) {
    const { tapeId, frameCount } = e.content || {};

    // Find tape by tapeId
    const tapeItem = contentItems.find(item => item.tapeId === tapeId);
    if (tapeItem) {
      tapeItem.isLoading = false;
      tapeItem.framesLoaded = true;
      tapeItem.frameCount = frameCount;
      tapeItem.frames = []; // Array to hold frames sent from bios
      console.log(`✅ Tape !${tapeItem.code} loaded: ${frameCount} frames - requesting frames for preview`);

      // Request frames from bios for preview (send all frames at once)
      if (typeof promptSend === "function") {
        promptSend({
          type: "tape:request-frames",
          content: { tapeId },
        });
      } else {
        console.warn("⚠️ promptSend unavailable; cannot request frames for", tapeId);
      }
    }
  }

  if (e.is("tape:frames")) {
    // console.log(`📼 Received tape:frames message:`, e.content);
  const { tapeId, frames } = e.content || {};
    // console.log(`📼 Looking for tapeId: ${tapeId}, frames array length: ${frames?.length || 0}`);
    // console.log(`📼 Content items with tapeIds:`, contentItems.filter(i => i.tapeId).map(i => ({code: i.code, tapeId: i.tapeId})));

    // Find tape by tapeId
    const tapeItem = contentItems.find(item => item.tapeId === tapeId);
    if (tapeItem) {
      const limitedFrames = Array.isArray(frames)
        ? frames.slice(0, TAPE_PREVIEW_MAX_FRAMES)
        : frames;
      tapeItem.frames = limitedFrames; // Array of ImageBitmap objects
      tapeItem.framesLoaded = true;
      // console.log(`✅ Received ${frames?.length || 0} frames for !${tapeItem.code}`);
      if (activeTapePreview === tapeItem) {
        scheduleTapePreviewRelease(tapeItem);
      }
      if (typeof promptNeedsPaint === "function") {
        promptNeedsPaint();
      } else {
        console.warn("⚠️ promptNeedsPaint unavailable; cannot trigger repaint for", tapeId);
      }
    } else {
      // Only warn if this is actually a prompt tape (ignore frames for other contexts)
      if (tapeId && tapeId.startsWith("prompt-tape-")) {
        // Silently ignore - likely cached frames arriving before content items are loaded
        // This is normal behavior and not an error condition
      }
    }
  }

  if (e.is("tape:preload-error")) {
    const { tapeId, error } = e.content || {};

    const tapeItem = contentItems.find(item => item.tapeId === tapeId);
    if (tapeItem) {
      tapeItem.isLoading = false;
      tapeItem.fetchFailed = true;
      tapeItem.fetchAttempted = false;
      console.error(`❌ Failed to load tape !${tapeItem.code}:`, error);
      tapePreviewQueue = tapePreviewQueue.filter((entry) => entry.item !== tapeItem);
      tapeItem.previewQueued = false;
      tapeItem.previewActive = false;
      if (activeTapePreview === tapeItem) {
        releaseActiveTapePreview("error");
      }
    }
  }
}
function releaseActiveTapePreview(reason = "complete") {
  if (tapePreviewTimeoutId) {
    clearTimeout(tapePreviewTimeoutId);
    tapePreviewTimeoutId = null;
  }
  if (activeTapePreview) {
    console.log(`📼 Tape preview ${reason}: !${activeTapePreview.code}`);
    activeTapePreview.previewActive = false;
    activeTapePreview.previewQueued = false;
    if (!activeTapePreview.framesLoaded) {
      activeTapePreview.isLoading = false;
      activeTapePreview.loadPhase = undefined;
      activeTapePreview.loadMessage = undefined;
      activeTapePreview.loadProgress = 0;
    }
  }
  activeTapePreview = null;
  processTapePreviewQueue();
}

function processTapePreviewQueue() {
  if (activeTapePreview || tapePreviewQueue.length === 0) return;
  const next = tapePreviewQueue.shift();
  if (next?.item && next.api) {
    activeTapePreview = next.item;
    activeTapePreview.previewQueued = false;
    activeTapePreview.previewActive = true;
    console.log(`📼 ▶️ Starting queued tape preview for !${next.item.code}`);
    fetchTapeAudio(next.item, next.api).catch((err) => {
      console.warn(`📼 Tape preview load failed for !${next.item.code}:`, err);
      releaseActiveTapePreview("load-error");
    });
  }
}

function enqueueTapePreview(item, api) {
  if (!item || item.type !== "tape" || !api) return;
  if (item.fetchAttempted) return;
  if (activeTapePreview === item) return;
  const isAlreadyQueued = tapePreviewQueue.some((entry) => entry.item === item);
  if (isAlreadyQueued) return;
  item.previewQueued = true;
  tapePreviewQueue.push({ item, api });
  processTapePreviewQueue();
}

function scheduleTapePreviewRelease(tapeItem) {
  if (activeTapePreview !== tapeItem) return;
  if (tapePreviewTimeoutId) clearTimeout(tapePreviewTimeoutId);
  tapePreviewTimeoutId = setTimeout(() => {
    if (activeTapePreview === tapeItem) {
      releaseActiveTapePreview("duration-complete");
    }
  }, TAPE_PREVIEW_DURATION_MS);
}

// Shared Matrix-style loading animation for tickers
function paintMatrixLoading($, x, y, width, tickerFont, colorTheme = "default") {
  const chars = "!@#$%^&*()_+-=[]{}|;:,.<>?~/`'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  const charWidth = tickerFont === "MatrixChunky8" ? 4 : 5;
  const numChars = floor(width / charWidth);
  const framePhase = motdFrame;

  for (let i = 0; i < numChars; i++) {
    const baseCharIndex = (i * 7919) % chars.length;
    const shiftOffset = (framePhase + i * 13) % chars.length;
    const charIndex = (baseCharIndex + shiftOffset) % chars.length;
    const char = chars[charIndex];
    const charX = x + (i * charWidth);

    // Color themes: default (green/red), cyan, orange
    const colorSeed = (i * 17 + framePhase) % 10;
    let r, g, b;

    if (colorTheme === "cyan") {
      // Cyan theme for chat ticker
      if (colorSeed < 6) {
        r = 0 + (colorSeed * 20);
        g = 200 + (colorSeed * 9);
        b = 200 + (colorSeed * 9);
      } else {
        r = 100 + (colorSeed * 10);
        g = 220 + (colorSeed * 5);
        b = 255;
      }
    } else if (colorTheme === "orange") {
      // Orange theme for clock ticker
      if (colorSeed < 6) {
        r = 200 + (colorSeed * 9);
        g = 120 + (colorSeed * 13);
        b = 0 + (colorSeed * 10);
      } else {
        r = 255;
        g = 180 + (colorSeed * 10);
        b = 80 + (colorSeed * 10);
      }
    } else {
      // Default green/red theme
      if (colorSeed < 4) {
        r = 50 + (colorSeed * 30);
        g = 200 + (colorSeed * 13);
        b = 50 + (colorSeed * 20);
      } else if (colorSeed < 8) {
        r = 200 + ((colorSeed - 4) * 13);
        g = 50 + ((colorSeed - 4) * 30);
        b = 50 + ((colorSeed - 4) * 20);
      } else {
        r = 220 + (colorSeed * 5);
        g = 200 + (colorSeed * 5);
        b = 50;
      }
    }

    const pulse = Math.sin(motdFrame * 0.05 + i * 0.3) * 20;
    const alpha = 180 + pulse;

    $.ink([r, g, b, alpha]).write(char, { x: charX, y }, undefined, undefined, false, tickerFont);
  }
}

// Helper to parse message elements for syntax highlighting (ported from chat.mjs)
// parseMessageElements is imported from chat-highlighting.mjs
