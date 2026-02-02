// Chat, 2024.3.23.16.35.00.043
// Chat with other handles.

/* #region 📚 README
#endregion */

/* #region 🏁 TODO
  - [] Add custom sound to iOS notifications.
  + Future
  + Done
  - [x] Make a 'renderableMessages' object model with action support. 
  - [x] Search this file for `ChatToDisk` references and do them.
  - [x] Add custom message received sound.
  - [x] Include this file as an interface module inside of disk, so it can 
       conditionally be run inside of any piece so chat can be everywhere!
  - [x] Play a sound when chats come in by others.
  - [x] Move connection so that updates appear in every piece.
  - [x] Words longer than the width should be character-wrapped in word wrap
         mode of `write`.
  - [x] Don't push messages to the database on local / have an option for that.
  - [x] Add scrolling.
    - [x] Only calculate the height of each existing message one time.
    - [x] Add a better reframing on scroll.
    - [x] Add colored overlays for hiding the fold.
    - [x] Prevent scrolling past top.
    - [x] Don't render lines that are under the bottomMargin.
    - [x] Can't scroll past newest message.
  - [x] Line breaks.
  - [x] Live chatter updates.
  - [x] Show better connectivity.
#endregion */

const { max, floor, ceil } = Math;

import { isKidlispSource, tokenize, KidLisp } from "../lib/kidlisp.mjs";
import { parseMessageElements as parseMessageElementsShared } from "../lib/chat-highlighting.mjs";
import { getCommandDescription, isPromptOnlyCommand } from "../lib/prompt-commands.mjs";
import { FUNDING_MODE } from "./prompt.mjs";
import { createHandleAutocomplete } from "../lib/autocomplete.mjs";
import { iOS } from "../lib/platform.mjs";

// 🔤 Chat Font System
// Available fonts for chat messages - each user can pick their preferred font
export const CHAT_FONTS = {
  "font_1": { 
    name: "Default",
    typeface: null, // null means use the default typeface
    rowHeight: null, // null means use typeface.blockHeight + 1
    timestampGap: 4,
    sample: "Aa Bb Cc",
  },
  "matrix": {
    name: "Matrix",
    typeface: "MatrixChunky8",
    rowHeight: 9, // 8px font + 1px spacing
    timestampGap: 2,
    sample: "▄▀ ██ ░▒",
  },
  "unifont": {
    name: "Unifont",
    typeface: "unifont", 
    rowHeight: 17, // 16px font + 1px spacing
    timestampGap: 6,
    sample: "你好 Привет",
  },
};

// Get row height for a specific font
function getFontRowHeight(fontId, defaultRowHeight) {
  const fontConfig = CHAT_FONTS[fontId];
  if (!fontConfig) return defaultRowHeight;
  return fontConfig.rowHeight ?? defaultRowHeight;
}

// Get typeface name for a specific font
function getFontTypeface(fontId) {
  const fontConfig = CHAT_FONTS[fontId];
  if (!fontConfig) return null;
  return fontConfig.typeface;
}



function getFontTimestampGap(fontId) {
  const fontConfig = CHAT_FONTS[fontId];
  if (!fontConfig) return 4;
  return fontConfig.timestampGap ?? 4;
}

function stripInlineColorCodes(s) {
  // Removes inline color markup of the form: \color\text\reset\
  // where color/reset are any strings between backslashes.
  if (!s || typeof s !== "string") return "";
  let out = "";
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch !== "\\") {
      out += ch;
      continue;
    }
    // Skip until the next backslash (end of spec)
    const next = s.indexOf("\\", i + 1);
    if (next === -1) break;
    i = next;
  }
  return out;
}

let input, inputBtn, handleBtn, token;
let messagesNeedLayout = true;
let tapState = null;
let inputTypefaceName; // Store the typeface name for text input
let userSelectedFont = "font_1"; // Current user's selected chat font
let fontPickerOpen = false; // Is the font picker UI open?
let fontPickerBtnBounds = null; // Bounds for font button click detection
let fontPickerPanelBounds = null; // Bounds for font picker panel
let fontPickerItemBounds = []; // Bounds for each font option

// 📜 Scroll/drag tracking to prevent link taps after scrolling
let isDragging = false; // True when user is actively dragging/scrolling
let dragStartPos = null; // Position where drag started
let dragDistanceThreshold = 5; // Pixels moved before considered a scroll
let hoveredMessageIndex = null; // Index of message currently being hovered

let rowHeight;
const lineGap = 1,
  topMargin = 31,
  leftMargin = 6;

// Dynamic bottom margin based on selected font
function getBottomMargin(fontConfig, defaultBlockHeight) {
  const rowHeight = fontConfig?.rowHeight ?? (defaultBlockHeight + 1);
  return Math.max(33, rowHeight + 20); // At least 33px, or font height + padding
}

// Get the preview frame height when typing (fixed height regardless of font)
function getPreviewFrameHeight() {
  // Fixed height so preview doesn't grow past screen for larger fonts
  return 90; // Enough for ~6 lines at standard font + buttons
}

let messageSfx;
let ellipsisTicker;

let scroll = 0,
  totalScrollHeight,
  chatHeight;

// 🎨 Painting preview system
let paintingPreviewCache = new Map(); // Store loaded painting previews
let paintingLoadQueue = new Set(); // Track which paintings are being loaded
let paintingLoadProgress = new Map(); // Track loading progress (0-1) for each painting
let paintingAnimations = new Map(); // Track Ken Burns animation state for each painting
let modalPainting = null; // Track fullscreen modal painting { painting, code, metadata }
let modalJustOpened = false; // Prevent closing modal on the same click that opened it
let draftMessage = ""; // Store draft message text persistently

// 📺 YouTube preview system
let youtubePreviewCache = new Map(); // Store loaded YouTube thumbnails
let youtubeLoadQueue = new Set(); // Track which videos are being loaded
let youtubeModalOpen = false; // Track if YouTube modal is open
let youtubeModalVideoId = null; // Current video in modal
let domApi = null; // Store dom API reference for modal
let netPreload = null; // Store net.preload reference for YouTube loading
let globalYoutubeThumbCache = null;

if (typeof globalThis !== "undefined") {
  if (!globalThis.__acYoutubeThumbCache) {
    globalThis.__acYoutubeThumbCache = new Map();
  }
  globalYoutubeThumbCache = globalThis.__acYoutubeThumbCache;
}

// 🔗 Open Graph link preview system
let ogPreviewCache = new Map(); // Store loaded OG metadata { url, title, image, imageData }
let ogLoadQueue = new Set(); // Track which URLs are being loaded
let globalOgPreviewCache = null;

const LINK_PREVIEW_WIDTH = 120;
const LINK_PREVIEW_HEIGHT = 68;

if (typeof globalThis !== "undefined") {
  if (!globalThis.__acOgPreviewCache) {
    globalThis.__acOgPreviewCache = new Map();
  }
  globalOgPreviewCache = globalThis.__acOgPreviewCache;
}

//  Link confirmation modal system
// { type: "prompt"|"url"|"handle"|"kidlisp"|"painting", text: string, action: function }
let linkConfirmModal = null;

// 📋 Message copy modal system
// { message: string, from: string, copied: boolean, error: boolean }
let messageCopyModal = null;

// 📰 News ticker system
let newsHeadlines = []; // Array of { title, code, user } from news.aesthetic.computer
let newsTickerText = "Report a story"; // Default/fallback text for headlines row
let newsActivityText = ""; // Second row: recent activity/comments
let newsTickerBounds = null; // { x, y, w, h } for click detection
let newsTickerHovered = false; // Hover state for visual feedback
let newsFetchPromise = null; // Track fetch to avoid duplicate requests

// � R8dio mini-player system (for laer-klokken)
const R8DIO_STREAM_URL = "https://s3.radio.co/s7cd1ffe2f/listen";
const R8DIO_STREAM_ID = "chat-r8dio-stream";
const R8DIO_METADATA_URL = "https://public.radio.co/stations/s7cd1ffe2f/status";
let r8dioEnabled = false; // Whether r8dio player is shown
let r8dioPlaying = false;
let r8dioLoading = false;
let r8dioError = null;
let r8dioVolume = 0.5;
let r8dioTrack = ""; // Current track/program title
let r8dioLastMetadataFetch = 0;
let r8dioFrequencyData = [];
let r8dioWaveformData = [];
let r8dioBars = []; // Visualization bars
let r8dioNoAnalyserCount = 0;
let r8dioAnimPhase = 0;
let r8dioPlayerBounds = null; // { x, y, w, h } for click detection
let r8dioPlayBtnBounds = null; // Play/pause button bounds
let r8dioVolSliderBounds = null; // Volume slider bounds
let r8dioHovered = false; // Hover over player area
let r8dioPlayHovered = false; // Hover over play button
let r8dioVolDragging = false; // Dragging volume
const R8DIO_BAR_COUNT = 16; // Fewer bars for compact display
const R8DIO_METADATA_INTERVAL = 15000;

//  @handle autocomplete
let handleAutocomplete = null;

function buildScaledPreview(imageData, targetW, targetH, api) {
  if (!imageData || !api?.painting) return null;
  const imgWidth = imageData.width || imageData.w || 0;
  const imgHeight = imageData.height || imageData.h || 0;
  if (!imgWidth || !imgHeight) return null;
  const scaleX = targetW / imgWidth;
  const scaleY = targetH / imgHeight;
  const scaleFactor = Math.min(scaleX, scaleY);
  if (!Number.isFinite(scaleFactor) || scaleFactor <= 0) return null;
  const scaledW = imgWidth * scaleFactor;
  const scaledH = imgHeight * scaleFactor;
  const offsetX = (targetW - scaledW) / 2;
  const offsetY = (targetH - scaledH) / 2;
  return api.painting(targetW, targetH, (p) => {
    p.paste(imageData, floor(offsetX), floor(offsetY), scaleFactor);
  });
}

async function boot(
  {
    api,
    ui,
    send,
    handle,
    gizmo,
    notice,
    authorize,
    user,
    screen,
    chat,
    sound,
    net,
    store,
    typeface,
    params,
    hud,
    dom,
  },
  otherChat,
  options,
) {
  // Store dom API reference for YouTube modal
  domApi = dom;
  
  // Store net.preload reference for YouTube thumbnail loading in paint()
  netPreload = net.preload;
  
  // Set clean label without params
  if (params && params.length > 0) {
    hud.label("chat");
  }
  
  rowHeight = typeface.blockHeight + 1;

  const client = otherChat || chat;
  
  // Store typeface name from options if provided
  if (options?.typeface) {
    inputTypefaceName = options.typeface;
  }

  // console.log("💬 Chat booting...");
  scroll = store["chat:scroll"] || 0; // Memoize scroll.
  
  // 🔤 Load saved font preference
  userSelectedFont = store["chat:font"] || "font_1";
  if (!CHAT_FONTS[userSelectedFont]) {
    userSelectedFont = "font_1"; // Fallback to default if invalid
  }
  
  // 📝 Prefill message from URL params (e.g., chat~hey~@jeffrey~message)
  if (params && params.length > 0) {
    draftMessage = params.join(" ");
  }
  
  // Calculate dynamic bottom margin based on selected font
  const selectedFontConfig = CHAT_FONTS[userSelectedFont];
  const bottomMargin = getBottomMargin(selectedFontConfig, typeface.blockHeight);

  // TODO: Now make it so that you see the last chat message
  //       as a button under the piece name.

  // 🥅 Preload messageReceived sound.
  net
    .preload("chat_1")
    .then((sfx) => (messageSfx = sfx))
    .catch((err) => console.warn("Could not preload:", err)); // and key sounds.

  // � Load MatrixChunky8 font for timestamps
  if (api.Typeface) {
    const matrixFont = new api.Typeface("MatrixChunky8");
    await matrixFont.load(net.preload);
  }

  // �🗨️ Chat Networking

  // Get the user token for sending authorized messages.
  try {
    token = await authorize();
    // console.log("🔐 Authorized token:", token);
  } catch (err) {
    // console.error("🟥 Unauthorized.");
  }

  // 🟢 Connected...
  // chat.connected(() => {
  //   console.log("💬 Connected... computing layout!");
  // });

  // 🤖 Runs on every message...
  client.receiver = (id, type, content, extra) => {
    if (type === "connected") {
      messagesNeedLayout = true;
      return;
    }

    if (type === "muted") {
      notice("MUTED", ["red", "yellow"]);
      return;
    }

    if (type === "too-long") {
      notice("TOO LONG", ["red", "yellow"]);
      return;
    }

    if (type === "unauthorized") {
      notice("Unauthorized", ["red", "yellow"]);
      return;
    }

    if (type === "message") {
      const msg = content; // Pre-transformed and stored.
      sound.play(messageSfx);
      // delete store["chat:scroll"]; // Reset scroll on new message?
      // return;
    }

    if (extra?.layoutChanged) messagesNeedLayout = true;
    // console.log("🌠 Message received:", id, type, content);
  };

  // chat.disconnect = () => {}; // This is also part of the API.

  // ✍️️️ Text Input
  input = new ui.TextInput(
    api,
    "...", // This empty call-to-action prompt will not be shown.
    async (text) => {
      const currentHandle = handle();

      if (!currentHandle) {
        notice("NO HANDLE", ["red", "yellow"]);
      } else {
        text = text.replace(/\s+$/, ""); // Trim trailing whitespace.
        // Send the chat message with user's selected font
        client.server.send(`chat:message`, { 
          text, 
          token, 
          sub: user.sub,
          font: userSelectedFont, // 🔤 Include selected font
        });
        notice("SENT");
      }

      // Clear text, hide cursor block, and close keyboard after sending message.
      input.text = "";
      draftMessage = ""; // Clear draft
      input.showBlink = false;
      input.mute = true;
      if (handleAutocomplete) handleAutocomplete.hide(); // 🔍 Clear autocomplete state
      console.log("⌨️🔴 [chat.mjs] sending keyboard:close - reason: message sent");
      send({ type: "keyboard:close" });
    },
    {
      // autolock: false,
      // wrap,
      // 🔤 Use saved font preference (or fallback to inputTypefaceName or font_1)
      font: CHAT_FONTS[userSelectedFont]?.typeface || inputTypefaceName || "font_1",
      scheme: {
        text: 255,
        background: [0, 180],
        block: 255,
        highlight: 0,
        guideline: [255, 128],
      },
      // copied,
      // activated,
      // didReset: () => {
      // messageComplete = true;
      // },
      // gutterMax,
      // lineSpacing,
      hideGutter: false,
      closeOnEmptyEnter: true,
    },
  );

  // Set prefilled text if draftMessage was set from params
  if (draftMessage) {
    input.text = draftMessage;
    input.addUserText(draftMessage); // Also set lastUserText so it persists on activation
    input.snap(); // Move cursor to end of prefilled text
  }

  const currentHandle = handle();
  const handleText = currentHandle || "Log in";
  handleBtn = new ui.TextButton(handleText, { 
    left: 0, 
    bottom: 0,
    screen 
  });
  
  // Simple button for "Enter message"
  inputBtn = new ui.Button(
    0,
    screen.height - bottomMargin + 2,
    screen.width / 2,
    bottomMargin - 2,
  );

  ellipsisTicker = new gizmo.EllipsisTicker();

  // 🔍 Initialize @handle autocomplete
  handleAutocomplete = createHandleAutocomplete();

  // 📰 Fetch news headlines from news.aesthetic.computer
  fetchNewsHeadlines();

  send({ type: "keyboard:soft-lock" });
}

// 📰 Fetch news headlines from the API
async function fetchNewsHeadlines() {
  if (newsFetchPromise) return newsFetchPromise; // Avoid duplicate fetches
  
  newsFetchPromise = (async () => {
    try {
      // Fetch all posts with recent comments
      const response = await fetch("https://news.aesthetic.computer/api/news/posts?limit=10&sort=new&includeRecentComments=2");
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const data = await response.json();
      
      if (data.posts && data.posts.length > 0) {
        // Store all headlines with comments
        newsHeadlines = data.posts.map(post => ({
          title: post.title,
          code: post.code,
          user: post.user,
          commentCount: post.commentCount ?? 0,
          score: post.score ?? 0,
          recentComments: post.recentComments || [],
        }));
        
        // Row 1: ALL Headlines with dash separator (MatrixChunky8 compatible)
        newsTickerText = newsHeadlines.map(h => h.title).join(" - ");
        
        // Row 2: Recent comment snippets from all stories
        const commentSnippets = newsHeadlines.flatMap(h => 
          (h.recentComments || []).map(c => {
            const handle = c.handle || 'anon';
            const text = (c.text || '').slice(0, 40);
            return `${handle}: "${text}${c.text?.length > 40 ? '…' : ''}"`;
          })
        );
        
        if (commentSnippets.length > 0) {
          newsActivityText = commentSnippets.slice(0, 6).join(" · ");
        } else {
          newsActivityText = `${newsHeadlines.length} stories · Submit yours!`;
        }
      } else {
        newsHeadlines = [];
        newsTickerText = "No stories yet — be the first to report!";
        newsActivityText = "Submit a story at news.aesthetic.computer";
      }
    } catch (err) {
      console.warn("📰 Failed to fetch news:", err.message);
      newsHeadlines = [];
      newsTickerText = "Report a story";
      newsActivityText = "news.aesthetic.computer";
    }
    newsFetchPromise = null;
  })();
  
  return newsFetchPromise;
}

function paint(
  {
    api,
    ink,
    wipe,
    text,
    screen,
    leaving,
    chat,
    mask,
    unmask,
    geo: { Box },
    handle,
    help,
    hud,
    typeface,
    pen,
    num,
    piece,
    store,
    paste,
    needsPaint,
  },
  options,
) {
  const client = options?.otherChat || chat;
  
  // Calculate dynamic bottom margin based on selected font
  const selectedFontConfig = CHAT_FONTS[userSelectedFont] || CHAT_FONTS["font_1"];
  const bottomMargin = getBottomMargin(selectedFontConfig, typeface.blockHeight);
  
  // User-selected font is only for NEW messages being typed, not for rendering existing messages
  const typefaceName = selectedFontConfig.typeface;
  const currentRowHeight = selectedFontConfig.rowHeight ?? (typeface.blockHeight + 1);
  
  // Default theme
  const defaultTheme = {
    background: [100, 100, 145],
    lines: [90, 200, 150, 48],
    scrollbar: "pink",
    messageText: [255, 255, 255], // Changed from "white" to explicit RGB
    messageBox: [255, 32], // white with alpha for hover
    log: "cyan", // System log messages (hi @newuser, etc)
    logHover: "yellow",
    handle: "pink",
    handleHover: "yellow",
    url: "cyan",
    urlHover: "yellow",
    prompt: "lime",
    promptContent: "cyan",
    promptHover: "yellow",
    promptContentHover: "yellow",
    painting: "orange",
    paintingHover: "yellow",
    kidlisp: "magenta",
    kidlispHover: "yellow",
    clock: [255, 200, 50], // Gold/yellow for *clock links
    clockHover: "yellow",
    r8dio: [255, 0, 255], // Magenta for r8dio radio links
    r8dioHover: "yellow",
    youtube: [255, 80, 80], // Red for YouTube links
    youtubeHover: [255, 50, 50],
    timestamp: [100 / 1.3, 100 / 1.3, 145 / 1.3],
    timestampHover: "yellow",
  };
  
  // Merge custom theme with defaults
  const theme = { ...defaultTheme, ...options?.theme };
  
  if (!options?.embedded) {
    if (Array.isArray(theme.background)) {
      wipe(...theme.background);
    } else {
      wipe(theme.background);
    }
  }

  // Interface
  {
    // Back to prompt arrow removed
  }

  // Use theme line color
  if (Array.isArray(theme.lines)) {
    ink(...theme.lines)
      .line(0, topMargin, screen.width, topMargin)
      .line(
        0,
        screen.height - bottomMargin + 2,
        screen.width,
        screen.height - bottomMargin + 2,
      );
  } else {
    ink(theme.lines)
      .line(0, topMargin, screen.width, topMargin)
      .line(
        0,
        screen.height - bottomMargin + 2,
        screen.width,
        screen.height - bottomMargin + 2,
      );
  }

  if (client.connecting) {
    if (FUNDING_MODE) {
      // Detect if this is laer-klokken (clock chat) or main chat
      const isClockChat = piece === "laer-klokken";
      
      // Danish for laer-klokken, English for main chat
      const offlineMsg = isClockChat ? [
        "Forbinder" + (ellipsisTicker?.text(help.repeat) || "..."),
        "",
        "Chats er offline pga.",
        "en stor serverregning.",
        "",
        "Skriv 'give' for at stoette AC",
        "og vi er tilbage snart!",
      ] : [
        "Connecting" + (ellipsisTicker?.text(help.repeat) || "..."),
        "",
        "Chats are offline due to",
        "a large end-of-year server bill.",
        "",
        "Enter 'give' to help support AC",
        "and services will return ASAP!",
      ];
      
      const lineHeight = 12;
      const totalLines = offlineMsg.length;
      const halfBlock = Math.floor((totalLines * lineHeight) / 2);
      const shakeFrame = help?.repeat ?? 0;
      
      // Debug logging - log immediately then throttle
      if (!globalThis._chatFundingDebugLogged) {
        globalThis._chatFundingDebugLogged = true;
        console.log("📝 Chat funding message debug:", {
          screenHeight: screen.height,
          screenWidth: screen.width,
          totalLines,
          lineHeight,
          halfBlock,
          centerY: Math.floor(screen.height / 2),
          helpRepeat: help?.repeat,
          shakeFrame
        });
      }
      
      for (let i = 0; i < offlineMsg.length; i++) {
        const line = offlineMsg[i];
        // High contrast colors that cycle
        let color = [200, 200, 200]; // Default fallback
        if (i === 0) {
          const titleColors = [[255, 100, 150], [255, 150, 100], [255, 200, 100]];
          color = titleColors[Math.floor(shakeFrame * 0.15) % titleColors.length] || color;
        } else if (i === 5) {
          const giveColors = [[100, 255, 100], [150, 255, 50], [200, 255, 100]];
          color = giveColors[Math.floor(shakeFrame * 0.2) % giveColors.length] || color;
        } else if (line !== "") {
          const textColors = [[255, 220, 150], [200, 220, 255], [255, 200, 200]];
          color = textColors[(i + Math.floor(shakeFrame * 0.1)) % textColors.length] || color;
        } else {
          color = [100, 100, 100];
        }
        
        // Calculate Y: center of screen minus half the block, plus line offset
        const centerY = Math.floor(screen.height / 2);
        const startY = centerY - halfBlock;
        const finalY = startY + (i * lineHeight);
        
        ink(color[0], color[1], color[2]).write(line, { center: "x", y: finalY }, undefined, undefined, false, typefaceName);
      }
    } else {
      // Normal connecting message
      ink("pink").write("Connecting" + ellipsisTicker?.text(help.repeat), {
        center: "xy",
      }, undefined, undefined, false, typefaceName);
    }
    return;
  }

  // Messages
  // Start from the bottom of the screen
  // if (!client.connecting) {
  // Calculate effective margins based on whether input preview is showing
  // The preview appears at the TOP (starting at topMargin), so we adjust the top edge of the mask
  let effectiveTopMargin = topMargin;
  if (input.canType) {
    // When typing, the preview frame covers the top area
    const previewFrameHeight = getPreviewFrameHeight();
    effectiveTopMargin = topMargin + previewFrameHeight;
  }

  if (messagesNeedLayout) {
    totalScrollHeight = computeMessagesHeight(api, client, typefaceName, currentRowHeight);
    // Always layout with regular bottomMargin so messages don't jump
    computeMessagesLayout(api, client, typefaceName, currentRowHeight, bottomMargin);
    chatHeight = computeScrollbarHeight(api, bottomMargin);
    messagesNeedLayout = false;
  }
  
  // Mask off the area of renderable messages
  // When preview is open, start mask below the preview frame (effectiveTopMargin)
  mask({
    x: 0,
    y: effectiveTopMargin,
    width: screen.width,
    height: screen.height - effectiveTopMargin - bottomMargin,
  });

  let lastAgo; // Keep track of last rendered timestamp.

  // Iterate through the messages array backwards, calculating their
  // height and painting them if they are within the boundaries.

  // console.log(client.messages.length);

  for (let i = client.messages.length - 1; i >= 0; i--) {
    const message = client.messages[i];

    if (!message.tb || !message.layout) {
      continue; // If `tb` is not defined then kill this. 👾
      // console.log("No message layout found for:", message);
    }

    // 🔤 Use per-message font (as sender chose) for rendering
    // computedTypefaceName of null/undefined means "system default typeface" (not user's selection)
    const msgTypefaceName = message.computedTypefaceName; // null = system default
    const msgRowHeight = message.computedRowHeight || (typeface.blockHeight + 1);
    const msgFontId = message.computedFontId || "font_1";

    const x = leftMargin;
    const tb = message.tb; // Precomputed in `computeScrollbar` for each message.

    const layout = message.layout;
    const y = layout.y;

    // 🪧 Paint the message
    
    // 🎯 Draw hover background if this message is being hovered
    if (hoveredMessageIndex === i) {
      ink(255, 255, 255, 15).box(
        layout.x - 2, 
        layout.y - 1, 
        layout.width + 4, 
        layout.height + 2
      );
    }
    
    // Render each line of the message separately for proper multi-line display
    // Parse elements once for this message
    const parsedElements = parseMessageElements(message.fullMessage);
    const hoveredElements = layout.hoveredElements || new Set();
    
    let charPos = 0; // Track position in the full message
    let lastLineRenderedWidthForThisMessage = 0; // Track actual rendered width of last line for THIS message
    
    // First pass: draw background box for entire kidlisp code block
    const hasKidlisp = parsedElements.some(el => el.type === "kidlisp-token");
    if (hasKidlisp) {
      // Find the full range of kidlisp content (from first to last token)
      const kidlispTokens = parsedElements.filter(el => el.type === "kidlisp-token");
      const firstToken = kidlispTokens[0];
      const lastToken = kidlispTokens[kidlispTokens.length - 1];
      
      // Draw background for each line that contains kidlisp
      let tempCharPos = 0;
      for (let lineIdx = 0; lineIdx < tb.lines.length; lineIdx++) {
        const line = tb.lines[lineIdx];
        const lineY = y + lineIdx * msgRowHeight;
        const lineStart = tempCharPos;
        const lineEnd = tempCharPos + line.length;
        
        // Check if this line contains any kidlisp content
        if (lineEnd > firstToken.start && lineStart < lastToken.end) {
          // Calculate the start and end of kidlisp content in this line
          const kidlispStartInLine = Math.max(0, firstToken.start - lineStart);
          const kidlispEndInLine = Math.min(line.length, lastToken.end - lineStart);
          
          const startX = text.width(line.substring(0, kidlispStartInLine), msgTypefaceName);
          const kidlispText = line.substring(kidlispStartInLine, kidlispEndInLine);
          const kidlispWidth = text.width(kidlispText, msgTypefaceName);
          
          // Draw dark background for entire kidlisp section (including spaces)
          ink(0, 0, 0, 80).box(x + startX, lineY, kidlispWidth, msgRowHeight);
        }
        
        tempCharPos += line.length;
      }
    }
    
    // Second pass: render text with syntax colors
    charPos = 0;
    for (let lineIdx = 0; lineIdx < tb.lines.length; lineIdx++) {
      const line = tb.lines[lineIdx];
      const lineY = y + lineIdx * msgRowHeight;
      const lineStart = charPos;
      const lineEnd = charPos + line.length;
      
      // Build color-coded version of this line
      let colorCodedLine = line;
      
      // Find elements that overlap with this line and apply colors (in reverse order)
      const lineElements = parsedElements
        .filter(el => el.start < lineEnd && el.end > lineStart)
        .sort((a, b) => b.start - a.start);
      
      for (const element of lineElements) {
        // Calculate element's position within this line
        const elemStartInLine = Math.max(0, element.start - lineStart);
        const elemEndInLine = Math.min(line.length, element.end - lineStart);
        
        if (elemStartInLine < elemEndInLine) {
          const elementText = line.substring(elemStartInLine, elemEndInLine);
          
          // Check if hovered
          const isHovered = Array.from(hoveredElements).some(hoveredEl => 
            hoveredEl.start === element.start && 
            hoveredEl.end === element.end && 
            hoveredEl.type === element.type
          );
          
          // Get color based on type and hover state
          let color;
          if (element.type === "kidlisp-token") {
            // Use yellow on hover, otherwise use the token's specific syntax color
            color = isHovered ? theme.kidlispHover : element.color;
          } else if (element.type === "handle") {
            color = isHovered ? theme.handleHover : theme.handle;
          } else if (element.type === "url") {
            color = isHovered ? theme.urlHover : theme.url;
          } else if (element.type === "prompt") {
            color = isHovered ? theme.promptHover : theme.prompt;
          } else if (element.type === "prompt-content") {
            color = isHovered ? theme.promptContentHover : theme.promptContent;
          } else if (element.type === "painting") {
            color = isHovered ? theme.paintingHover : theme.painting;
          } else if (element.type === "kidlisp") {
            color = isHovered ? theme.kidlispHover : theme.kidlisp;
          } else if (element.type === "clock") {
            color = isHovered ? theme.clockHover : theme.clock;
          } else if (element.type === "r8dio") {
            color = isHovered ? theme.r8dioHover : theme.r8dio;
          } else if (element.type === "log") {
            color = isHovered ? theme.logHover : theme.log;
          } else if (element.type === "youtube") {
            color = isHovered ? theme.youtubeHover : theme.youtube;
          }
          
          if (color) {
            const colorStr = Array.isArray(color) ? color.join(',') : color;
            const textColorStr = Array.isArray(theme.messageText) ? theme.messageText.join(',') : theme.messageText;
            const colorCodedText = `\\${colorStr}\\${elementText}\\${textColorStr}\\`;
            
            colorCodedLine = 
              colorCodedLine.substring(0, elemStartInLine) + 
              colorCodedText + 
              colorCodedLine.substring(elemEndInLine);
          }
        }
      }
      
      // Render this line with color codes
      // Note: For unifont, we don't want a background box per character
      ink(theme.messageText).write(
        colorCodedLine,
        { x, y: lineY },
        false, // bg - no background
        undefined, // bounds - no width limit (line is already wrapped)
        false, // wordWrap - off since line is pre-wrapped
        msgTypefaceName // typeface - use per-message font
      );
      
      // Track width of last line (visual width, ignoring inline color codes)
      if (lineIdx === tb.lines.length - 1) {
        lastLineRenderedWidthForThisMessage = text.width(
          stripInlineColorCodes(colorCodedLine),
          msgTypefaceName
        );
      }
      
      charPos += line.length; // Don't add +1 - text.box wraps without adding spaces
    }

    const ago = timeAgo(message.when);
    let overTimestamp = false;

    // Show all timestamps (not just unique ones), with fading for older messages
    const tsColor = layout.timestamp.over ? theme.timestampHover : theme.timestamp;
    
    // Use MatrixChunky8 for compact timestamps tacked onto the end of messages
    const timestampWidth = text.width(ago, "MatrixChunky8");
    const timestampGap = getFontTimestampGap(msgFontId);
    
    // Position timestamp right after the last *visually rendered* line of the message
    layout.timestamp.x = lastLineRenderedWidthForThisMessage + timestampGap;
    const timestampX = x + layout.timestamp.x;
    
    // Bottom-align timestamp with the message text
    // Timestamp is 8px tall (MatrixChunky8), so offset it down within the row
    const timestampHeight = 8; // MatrixChunky8 glyph height
    const timestampY = layout.timestamp.y + (msgRowHeight - timestampHeight);
    
    // Calculate fade based on message index (newer = more opaque, older = more faded)
    // Drastically reduce opacity for older timestamps
    const messageIndex = client.messages.length - 1 - i;
    const fadeAlpha = Math.max(25, 200 - (messageIndex * 25)); // Min 25, decrease by 25 per message (more dramatic fade)
    
    if (Array.isArray(tsColor)) {
      ink(...tsColor, fadeAlpha).write(ago, { x: timestampX, y: timestampY }, undefined, undefined, false, "MatrixChunky8");
    } else {
      ink(tsColor, fadeAlpha).write(ago, { x: timestampX, y: timestampY }, undefined, undefined, false, "MatrixChunky8");
    }

    lastAgo = ago;

    let nameColor = message.from === "log" ? "cyan" : "pink";

    // 🖋️ Check if the pen is inside the user of this message.
    /*
    if (pen && !input.canType) {
      if (
        pen.x > x &&
        pen.x < x + text.width(message.from) &&
        pen.y > y &&
        pen.y < y + rowHeight
      ) {
        nameColor = pen.drawing ? "yellow" : [200, 255, 200];
      }
    }
    */

    // Message byline.
    /*
    ink(nameColor).write(message.from, {
      x,
      y,
    });
    */

    /*
    y -= rowHeight; // Move up one line for the next message.
    if (y < topMargin - rowHeight) break; // Break if y is below top line.
    */
  }

  // 🎨 Render painting previews and trigger loading
  let hasAnimatingPaintings = false;
  
  for (let i = client.messages.length - 1; i >= 0; i--) {
    const message = client.messages[i];
    if (!message.layout?.paintingCodes) continue;
    
    // Preview position: 4px gap below message text, then 64px image, then 6px padding = 74px total
    const previewY = message.layout.y + message.layout.height + 4; // 4px gap above image
    const previewHeight = 64; // Actual image size
    const previewTotalHeight = 74; // Total reserved space (4px gap + 64px + 6px bottom padding)
    
    // Only process if the PREVIEW is visible (not just the message)
    if (previewY + previewTotalHeight < effectiveTopMargin) continue; // Preview is above visible area
    if (previewY > screen.height - bottomMargin) continue; // Preview is below visible area
    
    let previewX = message.layout.x; // Start from left margin
    
    for (let codeIdx = 0; codeIdx < message.layout.paintingCodes.length; codeIdx++) {
      const code = message.layout.paintingCodes[codeIdx];
      const cached = paintingPreviewCache.get(code);
      
      if (!cached && !paintingLoadQueue.has(code)) {
        // Trigger async loading
        loadPaintingPreview(code, api.get, store).then(result => {
          if (result) {
            help.repeat(); // Trigger repaint when preview loads
          }
        });
      }
      
      if (cached) {
        hasAnimatingPaintings = true; // Mark that we have animations
        const { painting } = cached;
        
        // Fixed preview size (Ken Burns style)
        const previewSize = 64;
        
        // Initialize animation state for this painting if it doesn't exist
        // Use message index + code to create stable key that doesn't change on scroll
        const animKey = `${i}-${code}`;
        if (!paintingAnimations.has(animKey)) {
          // Random starting position for Ken Burns effect
          paintingAnimations.set(animKey, {
            seed: Math.random(),
            startTime: performance.now(),
          });
        }
        
        const anim = paintingAnimations.get(animKey);
        
        // Ken Burns effect: pan a crop window that fills the preview box
        const KEN_BURNS_CYCLE_MS = 8000;
        const nowTime = performance.now();
        const burnProgress = ((nowTime / KEN_BURNS_CYCLE_MS) + anim.seed) % 1;
        
        // Get actual image dimensions
        const imgWidth = painting.width || painting.w;
        const imgHeight = painting.height || painting.h;
        
        // NO SCALING - Use 1:1 pixel ratio
        // Calculate maximum pan range (how much we can move within the source image)
        const maxPanX = Math.max(0, imgWidth - previewSize);
        const maxPanY = Math.max(0, imgHeight - previewSize);
        
        // Ken Burns pan position (0-1) - smooth circular motion
        const panX = (Math.cos((burnProgress + 0.25) * Math.PI * 2) + 1) / 2;
        const panY = (Math.sin((burnProgress + 0.65) * Math.PI * 2) + 1) / 2;
        
        // Calculate crop position in source pixels (for Ken Burns animation)
        const kenBurnsCropX = floor(Math.max(0, Math.min(maxPanX * panX, imgWidth - previewSize)));
        const kenBurnsCropY = floor(Math.max(0, Math.min(maxPanY * panY, imgHeight - previewSize)));
        const kenBurnsCropW = Math.min(previewSize, imgWidth - kenBurnsCropX);
        const kenBurnsCropH = Math.min(previewSize, imgHeight - kenBurnsCropY);
        
        // Check if this preview is being hovered
        const isHovered = message.layout.hoveredPainting === code && 
                          message.layout.hoveredPaintingIndex === codeIdx;
        
        // Calculate visible bounds (what portion of the preview is within the mask area)
        const maskTop = effectiveTopMargin;
        const maskBottom = screen.height - bottomMargin;
        
        // Check if preview is at least partially visible
        const isPartiallyVisible = previewY + previewSize > maskTop && 
                                   previewY < maskBottom &&
                                   previewX + previewSize > 0 &&
                                   previewX < screen.width;
        
        if (isPartiallyVisible) {
          // Render at natural position with Ken Burns crop - mask will clip outside visible area
          if (kenBurnsCropW > 0 && kenBurnsCropH > 0) {
            paste(
              painting,
              floor(previewX),
              floor(previewY),
              { 
                crop: {
                  x: kenBurnsCropX,
                  y: kenBurnsCropY,
                  w: kenBurnsCropW,
                  h: kenBurnsCropH
                }
              }
            );
          }
        }
        
        // Draw border only for the visible portion
        const borderAlpha = isHovered ? 255 : 150;
        const borderTop = Math.max(previewY, effectiveTopMargin);
        const borderBottom = Math.min(previewY + previewSize, screen.height - bottomMargin);
        const borderLeft = Math.max(previewX, 0);
        const borderRight = Math.min(previewX + previewSize, screen.width);
        if (borderBottom > borderTop && borderRight > borderLeft) {
          ink(100, 100, 120, borderAlpha).box(borderLeft, borderTop, borderRight - borderLeft, borderBottom - borderTop, "outline");
        }
        
        // Reserve space for the painting preview
        previewX += previewSize + 4;
      } else {
        // Show loading indicator only when NOT cached
        // Get loading progress (0-1)
        const progress = paintingLoadProgress.get(code) || 0;
        
        // Loading indicator dimensions
        const loadingW = 64;
        const loadingH = 64;
        
        // Pulsing animation based on time
        const pulse = (Math.sin(help.repeat * 0.1) + 1) / 2; // 0 to 1
        const bgAlpha = Math.floor(140 + pulse * 60); // 140-200
        
        // Draw background box with pulsing alpha
        ink(50, 50, 80, bgAlpha).box(
          previewX, 
          previewY, 
          loadingW, 
          loadingH
        );
        
        // Draw progress bar
        const progressBarH = 4;
        const progressBarY = previewY + loadingH - 10;
        const progressBarW = loadingW - 8;
        const progressBarX = previewX + 4;
        
        // Progress bar background
        ink(40, 40, 60, 200).box(progressBarX, progressBarY, progressBarW, progressBarH);
        
        // Progress bar fill
        const fillW = Math.floor(progressBarW * progress);
        if (fillW > 0) {
          ink(100, 150, 200, 255).box(progressBarX, progressBarY, fillW, progressBarH);
        }
        
        // Draw border with pulsing color
        const borderBrightness = Math.floor(80 + pulse * 60);
        ink(borderBrightness, borderBrightness, borderBrightness + 40, 200).box(
          previewX, 
          previewY, 
          loadingW, 
          loadingH, 
          "outline"
        );
        
        // Spinning dots in center
        const centerX = previewX + loadingW / 2;
        const centerY = previewY + loadingH / 2 - 4;
        const dotRadius = 12;
        const numDots = 8;
        
        for (let di = 0; di < numDots; di++) {
          const angle = (help.repeat * 0.05 + (di / numDots) * Math.PI * 2);
          const dotX = Math.floor(centerX + Math.cos(angle) * dotRadius);
          const dotY = Math.floor(centerY + Math.sin(angle) * dotRadius);
          
          // Fade dots based on position in circle
          const dotAlpha = Math.floor(100 + ((di / numDots) * 155));
          ink(150, 150, 180, dotAlpha).box(dotX - 1, dotY - 1, 2, 2);
        }
        
        // Reserve space for loading preview
        previewX += loadingW + 4;
      }
    }
  }
  
  // 📺 Render YouTube previews and trigger loading
  let hasAnimatingYoutube = false;
  
  for (let i = client.messages.length - 1; i >= 0; i--) {
    const message = client.messages[i];
    if (!message.layout?.youtubeVideoIds) continue;
    
    // YouTube previews appear after painting previews (if any)
    const paintingOffset = message.layout.paintingCodes ? 74 : 0;
    const previewY = message.layout.y + message.layout.height + 4 + paintingOffset;
    const previewHeight = 68; // Actual thumbnail size
    const previewTotalHeight = 74; // Total reserved space
    
    // Only process if visible
    if (previewY + previewTotalHeight < effectiveTopMargin) continue;
    if (previewY > screen.height - bottomMargin) continue;
    
    let previewX = message.layout.x;
    
    for (let vidIdx = 0; vidIdx < message.layout.youtubeVideoIds.length; vidIdx++) {
      const videoId = message.layout.youtubeVideoIds[vidIdx];
      const cached = youtubePreviewCache.get(videoId);
      
      if (!cached && !youtubeLoadQueue.has(videoId)) {
        // Trigger async loading
        loadYoutubePreview(videoId, netPreload).then(result => {
          if (result) {
            help.repeat(); // Trigger repaint when preview loads
          }
        });
      }
      
      if (cached) {
        hasAnimatingYoutube = true;
        const { thumbnail } = cached;
        
        // YouTube is 16:9 - use standard preview size
        const previewW = LINK_PREVIEW_WIDTH;
        const previewH = LINK_PREVIEW_HEIGHT;
        
        const imgWidth = thumbnail.width || thumbnail.w || 320;
        const imgHeight = thumbnail.height || thumbnail.h || 180;
        
        const isHovered = message.layout.hoveredYoutube === videoId;
        
        // Check if preview is at least partially visible
        const visibleTop = Math.max(previewY, effectiveTopMargin);
        const visibleBottom = Math.min(previewY + previewH, screen.height - bottomMargin);
        const visibleHeight = visibleBottom - visibleTop;
        
        if (visibleHeight > 0 && imgWidth > 0 && imgHeight > 0) {
          if (!Object.hasOwn(cached, "previewPainting")) {
            cached.previewPainting = buildScaledPreview(
              thumbnail,
              previewW,
              previewH,
              api,
            );
          }
          if (cached.previewPainting) {
            // Render at natural position - mask will clip what's outside visible area
            paste(cached.previewPainting, floor(previewX), floor(previewY));
          } else {
            // Fallback if preview cache fails
            const scaleX = previewW / imgWidth;
            const scaleY = previewH / imgHeight;
            const scaleFactor = Math.min(scaleX, scaleY);
            
            if (Number.isFinite(scaleFactor) && scaleFactor > 0) {
              paste(thumbnail, floor(previewX), floor(previewY), scaleFactor);
            }
          }
          
          // Red YouTube-style border - only draw when visible
          if (isHovered) {
            ink(255, 50, 50).box(previewX, floor(previewY), previewW, previewH, "outline");
          } else {
            const blinkAlpha = Math.floor(100 + (Math.sin(help.repeat * 0.15) + 1) * 50);
            ink(255, 80, 80, blinkAlpha).box(previewX, floor(previewY), previewW, previewH, "outline");
          }
        }
        
        previewX += previewW + 4;
      } else {
        // Loading indicator for YouTube (same 16:9 size)
        const loadingW = 120;
        const loadingH = 68;
        const pulse = (Math.sin(help.repeat * 0.1) + 1) / 2;
        const bgAlpha = Math.floor(60 + pulse * 30);
        
        ink(40, 15, 15, bgAlpha).box(previewX, previewY, loadingW, loadingH);
        ink(255, 80, 80, 100).box(previewX, previewY, loadingW, loadingH, "outline");
        
        // YouTube play icon loading pulse
        const centerX = previewX + loadingW / 2;
        const centerY = previewY + loadingH / 2;
        const pulseSize = 10 + pulse * 4;
        ink(255, 0, 0, 150 + pulse * 50).box(floor(centerX - pulseSize/2), floor(centerY - pulseSize/2), floor(pulseSize), floor(pulseSize * 0.75));
        
        previewX += loadingW + 4;
      }
    }
  }
  
  // 🔗 Render OG link previews and trigger loading
  let hasAnimatingOg = false;
  
  for (let i = client.messages.length - 1; i >= 0; i--) {
    const message = client.messages[i];
    if (!message.layout?.ogUrls) continue;
    
    // OG previews appear after painting and YouTube previews
    const paintingOffset = message.layout.paintingCodes ? 74 : 0;
    const youtubeOffset = message.layout.youtubeVideoIds ? 74 : 0;
    const previewY = message.layout.y + message.layout.height + 4 + paintingOffset + youtubeOffset;
    const previewHeight = 68; // Actual preview size
    const previewTotalHeight = 84; // Total reserved space (includes 10px caption)
    
    // Only process if visible
    if (previewY + previewTotalHeight < effectiveTopMargin) continue;
    if (previewY > screen.height - bottomMargin) continue;
    
    let previewX = message.layout.x;
    
    for (let urlIdx = 0; urlIdx < message.layout.ogUrls.length; urlIdx++) {
      const url = message.layout.ogUrls[urlIdx];
      const cached = ogPreviewCache.get(url);
      
      if (!cached && !ogLoadQueue.has(url)) {
        // Trigger async loading
        loadOgPreview(url, netPreload).then(result => {
          if (result && result.imageData) {
            messagesNeedLayout = true; // Relayout to add space for preview
            help.repeat(); // Trigger repaint when preview loads
          }
        });
      }
      
      if (cached && !cached.failed) {
        const { imageData, faviconData, title, siteName } = cached;
        const hasImage = imageData || faviconData;
        
        if (hasImage) {
          hasAnimatingOg = true;
          
          // Use OG image if available, otherwise use favicon
          const displayImage = imageData || faviconData;
          const isFavicon = !imageData && faviconData;
          
          // OG previews use standard preview size, favicons are smaller
          const previewW = isFavicon ? 32 : LINK_PREVIEW_WIDTH;
          const previewH = isFavicon ? 32 : LINK_PREVIEW_HEIGHT;
        
          const imgWidth = displayImage.width || displayImage.w || (isFavicon ? 32 : 320);
          const imgHeight = displayImage.height || displayImage.h || (isFavicon ? 32 : 180);
        
          const isHovered = message.layout.hoveredOgUrl === url;
        
          const visibleLeft = Math.max(previewX, 0);
          const visibleRight = Math.min(previewX + previewW, screen.width);
          const visibleTop = Math.max(previewY, effectiveTopMargin);
          const visibleBottom = Math.min(previewY + previewH, screen.height - bottomMargin);
          const visibleWidth = visibleRight - visibleLeft;
          const visibleHeight = visibleBottom - visibleTop;
        
          if (visibleHeight > 0 && visibleWidth > 0 && imgWidth > 0 && imgHeight > 0) {
            // For favicons, just paste directly (they're small)
            if (isFavicon) {
              // Draw a background box for favicon
              ink(40, 40, 50, 200).box(previewX - 2, previewY - 2, previewW + 4, previewH + 4);
              paste(displayImage, floor(previewX), floor(previewY));
            } else {
              if (!Object.hasOwn(cached, "previewPainting")) {
                cached.previewPainting = buildScaledPreview(
                  displayImage,
                  previewW,
                  previewH,
                  api,
                );
              }
              if (cached.previewPainting) {
                // Render at natural position - mask will clip outside visible area
                paste(cached.previewPainting, floor(previewX), floor(previewY));
              } else {
                // Fallback if preview cache fails
                const scaleX = previewW / imgWidth;
                const scaleY = previewH / imgHeight;
                const scaleFactor = Math.min(scaleX, scaleY);
            
                if (Number.isFinite(scaleFactor) && scaleFactor > 0) {
                  // Center the image if it doesn't fill the space
                  const scaledW = imgWidth * scaleFactor;
                  const scaledH = imgHeight * scaleFactor;
                  const offsetX = (previewW - scaledW) / 2;
                  const offsetY = (previewH - scaledH) / 2;
              
                  paste(displayImage, floor(previewX + offsetX), floor(previewY + offsetY), scaleFactor);
                }
              }
            }
          }
        
          // Border color - cyan for OG links, slightly different for favicons
          const borderColor = isFavicon ? [100, 200, 200] : [0, 200, 200];
          const hoverBorderColor = isFavicon ? [150, 255, 255] : [0, 255, 255];
          
          // Draw border at natural position - mask will clip outside visible area
          if (isHovered) {
            ink(...hoverBorderColor).box(previewX, floor(previewY), previewW, previewH, "outline");
          } else {
            const blinkAlpha = Math.floor(100 + (Math.sin(help.repeat * 0.15) + 1) * 50);
            ink(...borderColor, blinkAlpha).box(previewX, floor(previewY), previewW, previewH, "outline");
          }
        
          // Draw title/site name below the preview
          const captionY = previewY + previewH + 2;
          if (captionY < screen.height - bottomMargin - 8) {
            const captionText = title || siteName || new URL(url).hostname;
            const maxLen = isFavicon ? 15 : 20;
            const truncatedCaption = captionText.length > maxLen ? captionText.slice(0, maxLen - 1) + "…" : captionText;
            ink(150, 150, 180).write(truncatedCaption, { x: previewX, y: captionY }, undefined, undefined, false, "MatrixChunky8");
          }
        
          previewX += previewW + 4;
        }
      }
    }
  }
  
  // Request continuous painting only if we have animating paintings, YouTube, or OG previews
  if (hasAnimatingPaintings || hasAnimatingYoutube || hasAnimatingOg) {
    needsPaint();
  }

  unmask();

  // 📜 Scroll bar.

  ink("gray").box(0, topMargin + 1, 3, chatHeight - 1); // Backdrop.

  const segHeight = max(
    1,
    floor((chatHeight / totalScrollHeight) * chatHeight) - 1,
  );

  const boxY =
    ceil(
      chatHeight +
        topMargin -
        segHeight -
        (scroll / totalScrollHeight) * chatHeight,
    ) || 0;

  // Use theme scrollbar color
  if (Array.isArray(theme.scrollbar)) {
    ink(...theme.scrollbar).box(0, boxY, 3, segHeight);
  } else {
    ink(theme.scrollbar).box(0, boxY, 3, segHeight);
  }
  // }

  const currentHandle = handle();
  
  // Update button text if handle changed
  if (currentHandle && handleBtn.text !== currentHandle) {
    handleBtn.text = currentHandle;
  } else if (!currentHandle && handleBtn.text !== "Log in") {
    handleBtn.text = "Log in";
  }
  
  // Calculate panel height based on selected font (use already-declared selectedFontConfig)
  const selectedRowHeight = selectedFontConfig.rowHeight ?? (typeface.blockHeight + 1);
  const panelHeight = Math.max(32, selectedRowHeight + 20); // At least 32px, or font height + padding
  
  // Get font-specific dimensions for handle button
  const selectedTypeface = selectedFontConfig.typeface || typefaceName;
  const btnGap = 4;
  const btnTextWidth = text.width(handleBtn.text, selectedTypeface);
  const btnW = btnTextWidth + btnGap * 2;
  const btnH = selectedRowHeight + btnGap * 2;
  
  // Position handle button at bottom
  const handleBtnX = leftMargin + 1;
  const handleBtnY = screen.height - 7 - btnH;
  
  // Update handleBtn box for click detection
  handleBtn.btn.box = new Box(handleBtnX, handleBtnY, btnW, btnH);
  
  // Draw panel background behind both buttons
  ink(20, 20, 30, 255).box(
    0,
    screen.height - panelHeight,
    screen.width,
    panelHeight
  );
  
  // Use blue color scheme for Log in, pink/magenta for handle (like prompt.mjs)
  const loginBgColor = currentHandle ? [128, 0, 128] : [0, 0, 128];
  const loginBorderColor = 255;
  const loginTextColor = 255;
  
  // Draw handle button manually with selected font
  const btnScheme = handleBtn.btn.down 
    ? [255, 0, 0, 255]  // Hover/pressed
    : [loginBgColor, loginBorderColor, loginTextColor, loginBgColor];
  
  ink(btnScheme[0]).box(handleBtn.btn.box, "fill");
  ink(btnScheme[1]).box(handleBtn.btn.box, "outline");
  ink(btnScheme[2]).write(handleBtn.text, { 
    x: handleBtnX + btnGap, 
    y: handleBtnY + btnGap 
  }, btnScheme[3], undefined, false, selectedTypeface);

  // Draw "Enter message" button with visible border - 3px taller than handle button
  const gapWidth = 1; // 1px gap between buttons
  
  const inputBtnX = handleBtn.btn.box.x + handleBtn.btn.box.w + gapWidth;
  const inputBtnWidth = screen.width - inputBtnX - 5; // 1px less width on right side
  const inputBtnHeight = handleBtn.btn.box.h + 2; // 2px taller than handle button (was +3)
  
  inputBtn.box = new Box(
    inputBtnX,
    handleBtn.btn.box.y - 1, // Position matches handle button
    inputBtnWidth,
    inputBtnHeight,
  );

  // Draw border for Enter message button (no left border)
  const borderColor = inputBtn.down ? "yellow" : (inputBtn.over ? "white" : "gray");
  // Top border
  ink(borderColor).line(
    inputBtn.box.x,
    inputBtn.box.y,
    inputBtn.box.x + inputBtn.box.w - 1,
    inputBtn.box.y
  );
  // Bottom border
  ink(borderColor).line(
    inputBtn.box.x,
    inputBtn.box.y + inputBtn.box.h - 1,
    inputBtn.box.x + inputBtn.box.w - 1,
    inputBtn.box.y + inputBtn.box.h - 1
  );
  // Right border
  ink(borderColor).line(
    inputBtn.box.x + inputBtn.box.w - 1,
    inputBtn.box.y,
    inputBtn.box.x + inputBtn.box.w - 1,
    inputBtn.box.y + inputBtn.box.h - 1
  );

  // Draw background for Enter message button (always visible, darker when hovering)
  const draftText = draftMessage;
  const hasDraft = draftText.length > 0;
  
  if (inputBtn.down) {
    ink("yellow", 200).box(
      inputBtn.box.x,
      inputBtn.box.y + 1,
      inputBtn.box.w - 1,
      inputBtn.box.h - 2
    );
  } else if (inputBtn.over) {
    ink(220, 100).box(
      inputBtn.box.x,
      inputBtn.box.y + 1,
      inputBtn.box.w - 1,
      inputBtn.box.h - 2
    );
  } else if (hasDraft) {
    // Highlight background when there's a draft
    ink(80, 100, 80, 100).box(
      inputBtn.box.x,
      inputBtn.box.y + 1,
      inputBtn.box.w - 1,
      inputBtn.box.h - 2
    );
  } else {
    // Default subtle background
    ink(60, 60, 80, 80).box(
      inputBtn.box.x,
      inputBtn.box.y + 1,
      inputBtn.box.w - 1,
      inputBtn.box.h - 2
    );
  }

  // Show draft text if there is any, otherwise show "Enter message..."
  const enterMsg = hasDraft ? draftText : ("Enter message" + ellipsisTicker.text(help.repeat));
  const textColor = inputBtn.down ? "black" : (hasDraft ? "lime" : (inputBtn.over ? "white" : 200));
  
  // Use the selected font for the bottom bar text (selectedTypeface already defined above)
  ink(textColor).write(
    enterMsg,
    {
      left: inputBtnX + 6,
      bottom: 9,
    },
    false, undefined, false, selectedTypeface
  );

  // Presence display - shows "here" (viewing chat) and "online" (connected anywhere)
  if (!client.connecting) {
    const chatterCount = client?.chatterCount ?? 0;
    const onlineHandles = client?.onlineHandles || [];
    const hereHandles = client?.hereHandles || [];
    
    // Build presence text - list ALL handles instead of cycling through one
    let presenceText;
    const hereCount = hereHandles.length;
    const onlineCount = onlineHandles.length;
    
    if (hereCount > 0) {
      // List ALL "here" handles joined by spaces
      const allHereHandles = hereHandles.join(" ");
      presenceText = `${hereCount} here ${allHereHandles}`;
      if (onlineCount > hereCount) {
        presenceText += ` · ${onlineCount} online`;
      }
    } else if (onlineCount > 0) {
      // List ALL online handles joined by spaces
      const allOnlineHandles = onlineHandles.join(" ");
      presenceText = `${onlineCount} online ${allOnlineHandles}`;
    } else {
      // Fallback to connection count
      presenceText = chatterCount + " online";
    }
    
    // Get HUD label dimensions to position below it
    const hudLabel = hud?.currentLabel?.();
    const hudLabelBox = hudLabel?.btn?.box;
    const hudLabelHeight = hudLabelBox?.h ?? 10;
    
    // QR code size from HUD label (if present, e.g., in laer-klokken)
    // Only use qrSize if it's from actual QR cells (not animation state fallback)
    // QR cells length is typically 21-29 for URLs, hudAnimationState.qrSize is 80
    const rawQrSize = hudLabel?.qrSize ?? 0;
    const qrSize = rawQrSize > 0 && rawQrSize < 50 ? rawQrSize : 0; // Filter out animation default (80)
    const qrTotalWidth = qrSize > 0 ? qrSize + 6 : 0; // QR + 2px border + 4px gap
    
    // Position presence text under the HUD label, shifted right by QR width
    // HUD label starts at x=6 (default offset)
    const presenceX = 6 + qrTotalWidth;
    const presenceY = hudLabelHeight + 4; // Just below the HUD label
    
    const onlineFgColor = theme?.timestamp || 160;
    const tickerLeftEdge = screen.width - 230; // Reserve space for News/r8Dio
    let displayPresenceText = presenceText;
    let presenceWidth = text.width(displayPresenceText, "MatrixChunky8");

    // If it would overlap the ticker area, show truncated list with ellipsis
    if (presenceX + presenceWidth > tickerLeftEdge) {
      // Try progressively shorter versions
      if (hereCount > 0) {
        // First try showing just first 3 handles
        const limitedHandles = hereHandles.slice(0, 3).join(" ");
        const hasMore = hereHandles.length > 3;
        displayPresenceText = `${hereCount} here ${limitedHandles}${hasMore ? "…" : ""}`;
        presenceWidth = text.width(displayPresenceText, "MatrixChunky8");
        
        // If still too wide, just show count
        if (presenceX + presenceWidth > tickerLeftEdge) {
          displayPresenceText = `${hereCount} here`;
          presenceWidth = text.width(displayPresenceText, "MatrixChunky8");
        }
      } else if (onlineCount > 0) {
        // First try showing just first 3 handles
        const limitedHandles = onlineHandles.slice(0, 3).join(" ");
        const hasMore = onlineHandles.length > 3;
        displayPresenceText = `${onlineCount} online ${limitedHandles}${hasMore ? "…" : ""}`;
        presenceWidth = text.width(displayPresenceText, "MatrixChunky8");
        
        // If still too wide, just show count
        if (presenceX + presenceWidth > tickerLeftEdge) {
          displayPresenceText = `${onlineCount} online`;
          presenceWidth = text.width(displayPresenceText, "MatrixChunky8");
        }
      } else {
        displayPresenceText = chatterCount + " online";
      }
    }

    // Final guard: if still too wide, drop to just the count
    if (presenceX + presenceWidth > tickerLeftEdge) {
      displayPresenceText = String(hereCount > 0 ? hereCount : (onlineCount > 0 ? onlineCount : chatterCount));
    }

    ink(onlineFgColor).write(displayPresenceText, {
      x: presenceX,
      top: presenceY,
    }, false, undefined, false, "MatrixChunky8");
  }

  if (input.canType && !leaving()) {
    // Update draft message while typing
    if (input.text !== "...") {
      draftMessage = input.text;
    }
    
    // 🔍 Update @handle autocomplete
    if (handleAutocomplete && input.text !== "...") {
      const cursorPos = input.prompt?.textPos?.() ?? input.text.length;
      handleAutocomplete.update(input.text, cursorPos);
    }
    
    // Fixed preview frame height (doesn't change with font)
    const previewFrameHeight = getPreviewFrameHeight();
    
    const previewFrame = {
      x: 0,
      y: topMargin,
      width: screen.width,
      height: previewFrameHeight,
    };
    
    input.paint(api, false, previewFrame);

    // Character limit and font picker - centered at bottom of preview frame
    const len = 128;
    const charCountText = `${input.text.length}/${len}`;
    const charCountWidth = text.width(charCountText); // Normal font
    
    // Font button dimensions (normal size)
    const fontBtnW = 22;
    const fontBtnH = 12;
    const fontBtnLabel = "Aa";
    const fontBtnLabelWidth = text.width(fontBtnLabel);
    
    // Total width of "Aa  0/128" centered
    const gap = 8; // Gap between Aa and count
    const totalWidth = fontBtnW + gap + charCountWidth;
    const centerX = Math.floor(screen.width / 2);
    const startX = centerX - Math.floor(totalWidth / 2);
    
    // Position at bottom of preview frame (same line as paste/enter)
    const bottomY = topMargin + previewFrame.height - 18;
    
    // Draw font button "Aa"
    const fontBtnX = startX;
    const fontBtnY = bottomY;
    const fontBtnOver = pen && pen.x >= fontBtnX && pen.x < fontBtnX + fontBtnW &&
                        pen.y >= fontBtnY && pen.y < fontBtnY + fontBtnH;
    
    ink(fontPickerOpen ? [80, 60, 120] : (fontBtnOver ? [60, 60, 80] : [40, 40, 60])).box(fontBtnX, fontBtnY, fontBtnW, fontBtnH);
    ink(fontPickerOpen ? [100, 80, 140] : [70, 70, 90]).box(fontBtnX, fontBtnY, fontBtnW, fontBtnH, "outline");
    ink(fontBtnOver || fontPickerOpen ? "yellow" : "white").write(fontBtnLabel, { 
      x: fontBtnX + Math.floor((fontBtnW - fontBtnLabelWidth) / 2), 
      y: fontBtnY + 2 
    });
    
    // Store button bounds for click detection
    fontPickerBtnBounds = { x: fontBtnX, y: fontBtnY, w: fontBtnW, h: fontBtnH };
    
    // Draw character count (normal font, to the right of Aa)
    const charCountX = fontBtnX + fontBtnW + gap;
    const charCountY = bottomY + 2; // Vertically align with button text
    ink(input.text.length > len ? "red" : "gray").write(charCountText, { x: charCountX, y: charCountY });
    
    // 🔤 Font picker panel (when open) - drops UP from button
    if (fontPickerOpen) {
      const fontIds = Object.keys(CHAT_FONTS);
      const itemHeight = 20;
      const panelW = 100;
      const panelH = fontIds.length * itemHeight + 8;
      const panelX = fontBtnX;
      const panelY = fontBtnY - panelH - 2; // ABOVE the button (drop-up)
      
      // Panel background
      ink(30, 25, 45, 240).box(panelX, panelY, panelW, panelH);
      ink(80, 70, 110).box(panelX, panelY, panelW, panelH, "outline");
      
      // Store panel bounds for click detection
      fontPickerPanelBounds = { x: panelX, y: panelY, w: panelW, h: panelH };
      fontPickerItemBounds = [];
      
      fontIds.forEach((fontId, i) => {
        const fontConfig = CHAT_FONTS[fontId];
        const itemX = panelX + 4;
        const itemY = panelY + 4 + i * itemHeight;
        const itemW = panelW - 8;
        const itemH = itemHeight - 2;
        
        // Store bounds for this item
        fontPickerItemBounds.push({ x: itemX, y: itemY, w: itemW, h: itemH, fontId });
        
        // Check hover
        const isHovered = pen && pen.x >= itemX && pen.x < itemX + itemW &&
                          pen.y >= itemY && pen.y < itemY + itemH;
        const isSelected = fontId === userSelectedFont;
        
        // Item background
        if (isSelected) {
          ink(80, 60, 120).box(itemX, itemY, itemW, itemH);
        } else if (isHovered) {
          ink(50, 45, 70).box(itemX, itemY, itemW, itemH);
        }
        
        // Font sample text - render in that font
        const sampleTypeface = fontConfig.typeface;
        const sampleText = fontConfig.sample;
        const textColor = isSelected ? "yellow" : (isHovered ? "white" : [180, 180, 200]);
        ink(textColor).write(sampleText, { x: itemX + 2, y: itemY + 3 }, false, undefined, false, sampleTypeface);
        
        // Checkmark for selected
        if (isSelected) {
          ink("lime").write("✓", { x: itemX + itemW - 10, y: itemY + 3 }, false, undefined, false, "MatrixChunky8");
        }
      });
    } else {
      fontPickerPanelBounds = null;
      fontPickerItemBounds = [];
    }
  }
  
  // 🖼️ Render fullscreen painting modal (overlay over everything)
  if (modalPainting) {
    const { painting, code, metadata } = modalPainting;
    
    // Safety check - close modal if painting data is missing
    if (!painting) {
      modalPainting = null;
      return;
    }
    
    // Semi-transparent black backdrop
    ink(0, 0, 0, 220).box(0, 0, screen.width, screen.height);
    
    // Get actual dimensions (some paintings use .w/.h instead of .width/.height)
    const paintingW = painting.width || painting.w || 64;
    const paintingH = painting.height || painting.h || 64;
    
    // Scale painting to fit screen while maintaining aspect ratio
    const maxW = screen.width - 20;
    const maxH = screen.height - 40; // Leave room for title
    
    const scaleW = maxW / paintingW;
    const scaleH = maxH / paintingH;
    const scale = Math.min(scaleW, scaleH, 1); // Don't scale up past 1:1
    
    const displayW = Math.floor(paintingW * scale);
    const displayH = Math.floor(paintingH * scale);
    const x = Math.floor((screen.width - displayW) / 2);
    const y = Math.floor((screen.height - displayH) / 2) - 8;
    
    // Draw painting (scaled or 1:1)
    if (scale >= 1) {
      paste(painting, x, y);
    } else {
      paste(painting, x, y, { width: displayW, height: displayH });
    }
    
    // Draw border
    ink(255, 255, 255).box(x - 1, y - 1, displayW + 2, displayH + 2, "outline");
    
    // Draw title/code below painting
    const titleText = metadata?.handle ? `#${code} · @${metadata.handle}` : `#${code}`;
    ink(180, 180, 180).write(titleText, { center: "x", y: y + displayH + 6 }, undefined, undefined, false, "MatrixChunky8");
    
    // Store bounds for click detection (tap anywhere to close)
    modalPainting.bounds = { x, y, w: displayW, h: displayH };
    
    needsPaint(); // Keep modal visible
  }
  
  // 🔗 Render link confirmation modal (overlay over everything)
  if (linkConfirmModal) {
    const { type, text: linkText, displayText, description } = linkConfirmModal;
    
    // Semi-transparent black backdrop with slight purple tint
    ink(20, 15, 30, 230).box(0, 0, screen.width, screen.height);
    
    // Calculate modal dimensions based on content
    const hasDescription = description && description.length > 0;
    const modalW = Math.min(screen.width - 24, 180);
    const modalH = hasDescription ? 72 : 58;
    const modalX = Math.floor((screen.width - modalW) / 2);
    const modalY = Math.floor((screen.height - modalH) / 2);
    
    // Draw modal background with gradient-like effect
    ink(35, 30, 50).box(modalX, modalY, modalW, modalH); // Dark base
    ink(45, 40, 65).box(modalX + 1, modalY + 1, modalW - 2, modalH - 2); // Slightly lighter inner
    ink(80, 70, 110).box(modalX, modalY, modalW, modalH, "outline"); // Border
    ink(100, 90, 140).box(modalX + 1, modalY, modalW - 2, 1); // Top highlight
    
    // Draw action text based on type (using default font for header)
    let actionLabel;
    let actionColor;
    if (type === "url") {
      actionLabel = "Open URL?";
      actionColor = [120, 200, 255]; // Cyan for URLs
    } else if (type === "handle") {
      actionLabel = "Go to profile?";
      actionColor = [255, 150, 200]; // Pink for handles
    } else if (type === "painting") {
      actionLabel = "View painting?";
      actionColor = [255, 180, 100]; // Orange for paintings
    } else if (type === "kidlisp") {
      actionLabel = "Run code?";
      actionColor = [200, 150, 255]; // Purple for kidlisp
    } else {
      actionLabel = "Run?";
      actionColor = [150, 255, 150]; // Green for commands
    }
    
    ink(...actionColor).write(actionLabel, { x: modalX + 6, y: modalY + 4 }, undefined, undefined, false, "MatrixChunky8");
    
    // Show the link/command text (truncated if needed)
    const maxDisplayLen = 22;
    const showText = displayText || linkText;
    const truncatedText = showText.length > maxDisplayLen 
      ? showText.slice(0, maxDisplayLen - 1) + "…" 
      : showText;
    ink(255, 255, 255).write(truncatedText, { x: modalX + 6, y: modalY + 16 });
    
    // Show command description if available (using MatrixChunky8 for compact look)
    if (hasDescription) {
      const maxDescLen = 28;
      const truncatedDesc = description.length > maxDescLen
        ? description.slice(0, maxDescLen - 1) + "…"
        : description;
      ink(140, 180, 140).write(truncatedDesc, { x: modalX + 6, y: modalY + 32 }, undefined, undefined, false, "MatrixChunky8");
    }
    
    // Draw Yes/No buttons (compact)
    const btnW = 36;
    const btnH = 14;
    const btnY = modalY + modalH - btnH - 6;
    const btnGap = 12;
    const totalBtnW = btnW * 2 + btnGap;
    const btnStartX = modalX + Math.floor((modalW - totalBtnW) / 2);
    
    // Store button positions for hit detection
    linkConfirmModal.yesBtn = { x: btnStartX, y: btnY, w: btnW, h: btnH };
    linkConfirmModal.noBtn = { x: btnStartX + btnW + btnGap, y: btnY, w: btnW, h: btnH };
    
    // Yes button - green theme
    const yesHover = linkConfirmModal.hoverYes;
    ink(yesHover ? [60, 140, 60] : [40, 90, 40]).box(btnStartX, btnY, btnW, btnH);
    ink(yesHover ? [100, 200, 100] : [70, 130, 70]).box(btnStartX, btnY, btnW, btnH, "outline");
    ink(yesHover ? [180, 255, 180] : [150, 220, 150]).write("yes", { x: btnStartX + 8, y: btnY + 3 }, undefined, undefined, false, "MatrixChunky8");
    
    // No button - red theme  
    const noHover = linkConfirmModal.hoverNo;
    ink(noHover ? [140, 50, 50] : [90, 35, 35]).box(btnStartX + btnW + btnGap, btnY, btnW, btnH);
    ink(noHover ? [200, 90, 90] : [130, 60, 60]).box(btnStartX + btnW + btnGap, btnY, btnW, btnH, "outline");
    ink(noHover ? [255, 180, 180] : [220, 150, 150]).write("no", { x: btnStartX + btnW + btnGap + 11, y: btnY + 3 }, undefined, undefined, false, "MatrixChunky8");
    
    needsPaint(); // Keep modal animating
  }
  
  // � Message copy modal (for copying plain message text)
  if (messageCopyModal) {
    const { message: msgText, from, copied, error } = messageCopyModal;
    
    // Semi-transparent black backdrop
    ink(20, 15, 30, 230).box(0, 0, screen.width, screen.height);
    
    // Calculate modal dimensions based on content
    const modalW = Math.min(screen.width - 24, 200);
    const modalH = 90;
    const modalX = Math.floor((screen.width - modalW) / 2);
    const modalY = Math.floor((screen.height - modalH) / 2);
    
    // Draw modal background
    ink(35, 30, 50).box(modalX, modalY, modalW, modalH);
    ink(45, 40, 65).box(modalX + 1, modalY + 1, modalW - 2, modalH - 2);
    ink(80, 70, 110).box(modalX, modalY, modalW, modalH, "outline");
    ink(100, 90, 140).box(modalX + 1, modalY, modalW - 2, 1);
    
    // Header: "Message from @handle"
    const truncatedFrom = from.length > 15 ? from.slice(0, 14) + "…" : from;
    ink(180, 180, 200).write("from " + truncatedFrom, { x: modalX + 6, y: modalY + 4 }, undefined, undefined, false, "MatrixChunky8");
    
    // Message preview (truncated)
    const maxMsgLen = 28;
    const truncatedMsg = msgText.length > maxMsgLen
      ? msgText.slice(0, maxMsgLen - 1) + "…"
      : msgText;
    ink(255, 255, 255).write(truncatedMsg, { x: modalX + 6, y: modalY + 18 });
    
    // Status text
    if (copied) {
      ink(150, 255, 150).write("Copied!", { x: modalX + 6, y: modalY + 36 }, undefined, undefined, false, "MatrixChunky8");
    } else if (error) {
      ink(255, 150, 150).write("Failed to copy", { x: modalX + 6, y: modalY + 36 }, undefined, undefined, false, "MatrixChunky8");
    }
    
    // Buttons
    const btnW = 50;
    const btnH = 14;
    const btnY = modalY + modalH - btnH - 8;
    const btnGap = 12;
    const totalBtnW = btnW * 2 + btnGap;
    const btnStartX = modalX + Math.floor((modalW - totalBtnW) / 2);
    
    // Store button positions for hit detection
    messageCopyModal.copyBtn = { x: btnStartX, y: btnY, w: btnW, h: btnH };
    messageCopyModal.closeBtn = { x: btnStartX + btnW + btnGap, y: btnY, w: btnW, h: btnH };
    
    // Copy button - cyan theme
    const copyHover = messageCopyModal.hoverCopy;
    ink(copyHover ? [40, 100, 130] : [30, 70, 90]).box(btnStartX, btnY, btnW, btnH);
    ink(copyHover ? [80, 180, 220] : [60, 130, 160]).box(btnStartX, btnY, btnW, btnH, "outline");
    ink(copyHover ? [180, 240, 255] : [150, 210, 230]).write("copy", { x: btnStartX + 10, y: btnY + 3 }, undefined, undefined, false, "MatrixChunky8");
    
    // Close button - gray theme
    const closeHover = messageCopyModal.hoverClose;
    ink(closeHover ? [80, 75, 90] : [55, 50, 65]).box(btnStartX + btnW + btnGap, btnY, btnW, btnH);
    ink(closeHover ? [120, 110, 140] : [90, 80, 110]).box(btnStartX + btnW + btnGap, btnY, btnW, btnH, "outline");
    ink(closeHover ? [200, 200, 210] : [170, 170, 180]).write("close", { x: btnStartX + btnW + btnGap + 7, y: btnY + 3 }, undefined, undefined, false, "MatrixChunky8");
    
    needsPaint();
  }
  
  //  News ticker (top right, visible by default) - hide when modal is open
  // Use options.showNews to control visibility (defaults to true)
  const showNews = options?.showNews !== false;
  if (showNews && !client.connecting && !modalPainting && !messageCopyModal) {
    paintNewsTicker({ ink, screen, text, hud }, theme);
    needsPaint();
  }
  
  // 📻 R8dio mini-player (laer-klokken only)
  if (options?.r8dioPlayer && !client.connecting && !modalPainting && !messageCopyModal) {
    r8dioEnabled = true;
    paintR8dioPlayer({ ink, screen, help, hud }, theme);
    needsPaint();
  } else {
    r8dioEnabled = false;
  }

  // 🔍 Paint @handle autocomplete dropdown (last, so it renders on top of everything)
  if (handleAutocomplete?.visible && input?.canType) {
    const cursorPos = input.prompt?.pos?.(undefined, true);
    // Position below the text input area
    const dropdownX = (cursorPos?.x ?? leftMargin) + leftMargin;
    const dropdownY = (cursorPos?.y ?? topMargin) + topMargin + 14; // Below cursor line
    handleAutocomplete.paint(
      { ink, write: (t, opts) => ink().write(t, opts), box: (x, y, w, h, style) => ink().box(x, y, w, h, style), screen },
      { x: dropdownX, y: dropdownY }
    );
  }
}

function act(
  {
    api,
    chat,
    pen,
    event: e,
    hud,
    piece,
    send,
    handle,
    store,
    beep,
    text,
    jump,
    typeface,
  },
  otherChat,
  options,
) {
  const client = otherChat || chat;
  const typefaceName = options?.typeface;
  
  // Calculate rowHeight based on the typeface being used
  const currentRowHeight = typefaceName === "unifont" ? 17 : (typeface.blockHeight + 1);
  
  // Calculate dynamic bottom margin based on selected font
  const selectedFontConfig = CHAT_FONTS[userSelectedFont] || CHAT_FONTS["font_1"];
  const bottomMargin = getBottomMargin(selectedFontConfig, typeface.blockHeight);

  // 📺 YouTube modal intercepts all events (prevent click-through)
  if (youtubeModalOpen) {
    const modalState = typeof globalThis !== "undefined" ? globalThis.acYoutubeModalState : null;
    const overlayExists = typeof document !== "undefined"
      ? document.getElementById("youtube-modal-overlay")
      : null;
    const modalClosed = (modalState && modalState.open === false) || overlayExists === null;

    if (modalClosed) {
      youtubeModalOpen = false;
      youtubeModalVideoId = null;
    } else {
      if (e.is("keyboard:down:escape")) {
        const closer = typeof globalThis !== "undefined" ? globalThis.acCloseYoutubeModal : null;
        if (typeof closer === "function") closer();
        closeYoutubeModal();
      }
      return;
    }
  }
  
  // 📰 News ticker interaction (hover + click)
  if (newsTickerBounds && pen && !linkConfirmModal && !messageCopyModal && !modalPainting) {
    const inBounds = pen.x >= newsTickerBounds.x && pen.x < newsTickerBounds.x + newsTickerBounds.w &&
                     pen.y >= newsTickerBounds.y && pen.y < newsTickerBounds.y + newsTickerBounds.h;
    
    // Update hover state
    if (e.is("move") || e.is("draw")) {
      newsTickerHovered = inBounds;
    }
    
    // Click to open the latest headline's comment page (touch only, not lift, to avoid scroll conflicts)
    if (e.is("touch") && inBounds) {
      beep();
      const latest = newsHeadlines[0];
      const target = latest?.code
        ? `out:https://news.aesthetic.computer/item/${latest.code}`
        : "out:https://news.aesthetic.computer";
      jump(target);
    }
  }
  
  // 📻 R8dio mini-player interaction
  if (r8dioEnabled && pen && !linkConfirmModal && !messageCopyModal && !modalPainting) {
    // Update hover states
    if (e.is("move") || e.is("draw")) {
      // Player area hover
      if (r8dioPlayerBounds) {
        r8dioHovered = pen.x >= r8dioPlayerBounds.x && pen.x < r8dioPlayerBounds.x + r8dioPlayerBounds.w &&
                       pen.y >= r8dioPlayerBounds.y && pen.y < r8dioPlayerBounds.y + r8dioPlayerBounds.h;
      }
      // Play button hover (entire bar is clickable, but button gets special hover)
      if (r8dioPlayBtnBounds) {
        r8dioPlayHovered = pen.x >= r8dioPlayBtnBounds.x && pen.x < r8dioPlayBtnBounds.x + r8dioPlayBtnBounds.w &&
                           pen.y >= r8dioPlayBtnBounds.y && pen.y < r8dioPlayBtnBounds.y + r8dioPlayBtnBounds.h;
      }
    }
    
    // Touch anywhere on bar toggles playback
    if (e.is("touch") && r8dioPlayerBounds) {
      const inBar = pen.x >= r8dioPlayerBounds.x && pen.x < r8dioPlayerBounds.x + r8dioPlayerBounds.w &&
                    pen.y >= r8dioPlayerBounds.y && pen.y < r8dioPlayerBounds.y + r8dioPlayerBounds.h;
      if (inBar) {
        beep();
        toggleR8dioPlayback(send);
      }
    }
  }
  
  // 🔗 Link confirmation modal intercepts all events
  if (linkConfirmModal) {
    const { yesBtn, noBtn, action } = linkConfirmModal;
    
    // Handle hover states
    if (e.is("move") || e.is("draw")) {
      if (yesBtn && noBtn) {
        linkConfirmModal.hoverYes = pen.x >= yesBtn.x && pen.x < yesBtn.x + yesBtn.w &&
                                     pen.y >= yesBtn.y && pen.y < yesBtn.y + yesBtn.h;
        linkConfirmModal.hoverNo = pen.x >= noBtn.x && pen.x < noBtn.x + noBtn.w &&
                                    pen.y >= noBtn.y && pen.y < noBtn.y + noBtn.h;
      }
    }
    
    // Handle clicks
    if (e.is("lift") || e.is("touch")) {
      if (yesBtn && noBtn) {
        const clickedYes = pen.x >= yesBtn.x && pen.x < yesBtn.x + yesBtn.w &&
                           pen.y >= yesBtn.y && pen.y < yesBtn.y + yesBtn.h;
        const clickedNo = pen.x >= noBtn.x && pen.x < noBtn.x + noBtn.w &&
                          pen.y >= noBtn.y && pen.y < noBtn.y + noBtn.h;
        
        if (clickedYes) {
          beep();
          hud.label(piece); // Set back label to current piece
          if (action) action(); // Execute the stored action
          linkConfirmModal = null;
        } else if (clickedNo) {
          beep();
          linkConfirmModal = null; // Just close
        }
        // Clicking outside buttons also closes the modal
        else {
          beep();
          linkConfirmModal = null;
        }
      }
    }
    
    // Escape key closes modal
    if (e.is("keyboard:down:escape")) {
      beep();
      linkConfirmModal = null;
    }
    
    return; // Block all other interactions when modal is open
  }
  
  // � Message copy modal intercepts all events
  if (messageCopyModal) {
    const { copyBtn, closeBtn, message: msgText } = messageCopyModal;
    
    // Handle hover states
    if (e.is("move") || e.is("draw")) {
      if (copyBtn && closeBtn) {
        messageCopyModal.hoverCopy = pen.x >= copyBtn.x && pen.x < copyBtn.x + copyBtn.w &&
                                      pen.y >= copyBtn.y && pen.y < copyBtn.y + copyBtn.h;
        messageCopyModal.hoverClose = pen.x >= closeBtn.x && pen.x < closeBtn.x + closeBtn.w &&
                                       pen.y >= closeBtn.y && pen.y < closeBtn.y + closeBtn.h;
      }
    }
    
    // Handle clicks
    if (e.is("lift") || e.is("touch")) {
      if (copyBtn && closeBtn) {
        const clickedCopy = pen.x >= copyBtn.x && pen.x < copyBtn.x + copyBtn.w &&
                            pen.y >= copyBtn.y && pen.y < copyBtn.y + copyBtn.h;
        const clickedClose = pen.x >= closeBtn.x && pen.x < closeBtn.x + closeBtn.w &&
                             pen.y >= closeBtn.y && pen.y < closeBtn.y + closeBtn.h;
        
        if (clickedCopy) {
          beep();
          // Use the direct clipboard API in bios
          send({ type: "copy", content: msgText });
          messageCopyModal.copied = true;
          messageCopyModal.error = false;
          // Auto-close after showing "Copied!" for a moment
          setTimeout(() => {
            if (messageCopyModal) messageCopyModal = null;
          }, 800);
        } else if (clickedClose) {
          beep();
          messageCopyModal = null;
        }
        // Clicking outside buttons also closes the modal
        else {
          beep();
          messageCopyModal = null;
        }
      }
    }
    
    // Handle clipboard result events
    if (e.is("copy:copied")) {
      messageCopyModal.copied = true;
      messageCopyModal.error = false;
    }
    if (e.is("copy:failed")) {
      messageCopyModal.copied = false;
      messageCopyModal.error = true;
    }
    
    // Escape key closes modal
    if (e.is("keyboard:down:escape")) {
      beep();
      messageCopyModal = null;
    }
    
    return; // Block all other interactions when modal is open
  }
  
  // �🖼️ Modal painting - tap anywhere or press Escape to close
  if (modalPainting) {
    // Skip the lift event from the click that opened the modal
    if (modalJustOpened && e.is("lift")) {
      modalJustOpened = false;
      return;
    }
    
    // Close on new touch or lift (after first lift is consumed)
    if (e.is("touch") || e.is("lift")) {
      beep();
      modalPainting = null;
      return;
    }
    
    if (e.is("keyboard:down:escape")) {
      beep();
      modalPainting = null;
    }
    
    return; // Block all other interactions when modal is open
  }
  
  // 🔤 Font picker interaction
  if (input.canType) {
    // Check if touch/lift is over interactive elements (don't soft-lock these!)
    const isOverEnterBtn = input.enter?.btn?.box?.contains(e);
    const isOverFontPickerBtn = fontPickerBtnBounds && e.x >= fontPickerBtnBounds.x && e.x < fontPickerBtnBounds.x + fontPickerBtnBounds.w &&
                                e.y >= fontPickerBtnBounds.y && e.y < fontPickerBtnBounds.y + fontPickerBtnBounds.h;
    const isOverCopyBtn = input.copy?.btn?.box?.contains(e);
    const isOverPasteBtn = input.paste?.btn?.box?.contains(e);
    const isOverInteractiveElement = isOverEnterBtn || isOverFontPickerBtn || isOverCopyBtn || isOverPasteBtn;
    
    // 🔧 FIX: Soft-lock keyboard during content area touch/draw to prevent bios pointerup blur
    // BUT don't soft-lock if touching interactive buttons!
    const isInContentArea = e.y < api.screen.height - bottomMargin;
    console.log("📍 [chat act] canType=true | e:", e.name, "| y:", e.y, "| screenH:", api.screen.height, "| bottomMargin:", bottomMargin, "| isInContentArea:", isInContentArea, "| isOverInteractive:", isOverInteractiveElement);
    if (isInContentArea && !isOverInteractiveElement) {
      if (e.is("touch") || e.is("draw")) {
        console.log("🔒 [chat act] SOFT-LOCKING keyboard (content area touch/draw)");
        send({ type: "keyboard:soft-lock" });
      }
      if (e.is("lift")) {
        console.log("🔓 [chat act] SOFT-UNLOCKING keyboard (content area lift)");
        send({ type: "keyboard:soft-unlock" });
      }
    }
    
    // Handle font picker button click (use e.x/e.y directly, pen may be undefined)
    if ((e.is("touch") || e.is("lift")) && fontPickerBtnBounds && e.x !== undefined) {
      const inBtn = e.x >= fontPickerBtnBounds.x && e.x < fontPickerBtnBounds.x + fontPickerBtnBounds.w &&
                    e.y >= fontPickerBtnBounds.y && e.y < fontPickerBtnBounds.y + fontPickerBtnBounds.h;
      console.log("🔤📍 [fontPicker btn] event:", e.name, "e:", e.x, e.y, "bounds:", fontPickerBtnBounds, "inBtn:", inBtn);
      if (inBtn && e.is("lift")) {
        console.log("🔤✅ [fontPicker btn] TOGGLING fontPickerOpen");
        beep();
        fontPickerOpen = !fontPickerOpen;
        return;
      }
    }
    
    // Handle font picker panel clicks
    if (fontPickerOpen && (e.is("touch") || e.is("lift")) && e.x !== undefined) {
      // Check if click is in an item
      for (const item of fontPickerItemBounds) {
        const inItem = e.x >= item.x && e.x < item.x + item.w &&
                       e.y >= item.y && e.y < item.y + item.h;
        if (inItem && e.is("lift")) {
          beep();
          userSelectedFont = item.fontId;
          fontPickerOpen = false;
          // Save preference locally
          store["chat:font"] = userSelectedFont;
          // 🔤 Update the text input to use the new font
          const fontConfig = CHAT_FONTS[userSelectedFont];
          if (fontConfig && input) {
            input.setFont(fontConfig.typeface || "font_1");
          }
          // Trigger message re-layout with new font
          messagesNeedLayout = true;
          return;
        }
      }
      
      // Check if click is outside panel (close it)
      if (fontPickerPanelBounds && e.is("lift")) {
        const inPanel = e.x >= fontPickerPanelBounds.x && e.x < fontPickerPanelBounds.x + fontPickerPanelBounds.w &&
                        e.y >= fontPickerPanelBounds.y && e.y < fontPickerPanelBounds.y + fontPickerPanelBounds.h;
        const inBtn = fontPickerBtnBounds && e.x >= fontPickerBtnBounds.x && e.x < fontPickerBtnBounds.x + fontPickerBtnBounds.w &&
                      e.y >= fontPickerBtnBounds.y && e.y < fontPickerBtnBounds.y + fontPickerBtnBounds.h;
        if (!inPanel && !inBtn) {
          fontPickerOpen = false;
          return;
        }
      }
    }
    
    // Escape closes font picker
    if (fontPickerOpen && e.is("keyboard:down:escape")) {
      beep();
      fontPickerOpen = false;
      return;
    }
  }
  
  // if (e.is("viewport-height:changed")) {
  // console.log("✨ New keyboard cutoff would be:", e.y, "?");
  // notice(e.y);
  // }

  if (e.is("reframed")) {
    const lastScrollHeight = totalScrollHeight;
    const lastScroll = scroll;

    totalScrollHeight = computeMessagesHeight(api, client, typefaceName, currentRowHeight);
    computeMessagesLayout(api, client, typefaceName, currentRowHeight, bottomMargin);
    chatHeight = computeScrollbarHeight(api, bottomMargin);

    scroll = (lastScroll / lastScrollHeight) * totalScrollHeight;
    store["chat:scroll"] = scroll;
    boundScroll();
  }

  // TODO: ChatToDisk: The chat module will need some kind of "focus" in order
  //                   to enable scroll and such.. maybe.

  if (!input.canType) {
    // 👇 Message Picking
    
    // 🔴 Store isDragging state BEFORE we reset it, so lift handlers can use it
    const wasScrollingOnLift = isDragging;

    if (e.is("lift")) {
      tapState = null;
      // Reset drag tracking on lift (AFTER storing the state above)
      isDragging = false;
      dragStartPos = null;
    }

    // 📜 Track drag start for scroll detection
    if (e.is("touch")) {
      dragStartPos = { x: e.x, y: e.y };
      isDragging = false; // Reset - will become true if we move enough
    }

    // 👈 Tapping
    if (e.is("touch")) {
      // Skip message interaction if clicking in top UI area (News, r8dio, online count)
      if (e.y < topMargin) {
        // Let the top UI buttons handle this click
        return;
      }
      
      // Detect if we are inside a message or not.
      for (let i = client.messages.length - 1; i >= 0; i--) {
        const message = client.messages[i];
        if (!message.tb || !message.layout) {
          continue; // If `tb` is not defined then kill this. 👾
          // console.log("No message layout found for:", message);
        }
        
        // Calculate the full clickable area including painting previews
        const hasPreview = message.layout.paintingCodes?.length > 0;
        const previewAreaHeight = hasPreview ? 68 : 0; // 64px preview + 4px gap
        const fullHeight = message.layout.height + previewAreaHeight;
        
        if (
          e.x > message.layout.x &&
          e.x < message.layout.x + message.layout.width &&
          e.y > message.layout.y &&
          e.y < message.layout.y + fullHeight
        ) {
          message.layout.inBox = true;

          // 📆 `timestamp` hover and activate.
          const timestamp = message.layout.timestamp;
          const startX = message.layout.x + timestamp.x;
          if (
            e.x > startX &&
            e.x < startX + timestamp.width &&
            e.y > timestamp.y &&
            e.y < timestamp.y + timestamp.height
          ) {
            timestamp.over = true;
            break;
          }

          // Check for hover on interactive elements
          const parsedElements = parseMessageElements(message.fullMessage);
          const relativeX = e.x - message.layout.x;
          const relativeY = e.y - message.layout.y;
          
          // Reset hover states
          if (!message.layout.hoveredElements) {
            message.layout.hoveredElements = new Set();
          }
          message.layout.hoveredElements.clear();
          
          // Check each interactive element for hover
          for (const element of parsedElements) {
            // Use per-message font settings
            const msgRowHeight = message.computedRowHeight || (typeface.blockHeight + 1);
            const msgTypefaceName = message.computedTypefaceName;
            const elementPosition = calculateElementPosition(
              element, 
              message.fullMessage, 
              message.tb.lines, 
              text,
              msgRowHeight,
              msgTypefaceName
            );
            
            if (elementPosition && isClickInsideElement(relativeX, relativeY, elementPosition)) {
              message.layout.hoveredElements.add(element);
              break; // Only hover one element at a time
            }
          }
          
          // 🎨 Check for hover on painting previews
          if (message.layout.paintingCodes) {
            const previewY = message.layout.y + message.layout.height + 4;
            let previewX = message.layout.x;
            const previewSize = 64; // Fixed size for Ken Burns previews
            
            message.layout.hoveredPainting = null;
            message.layout.hoveredPaintingIndex = null;
            
            for (let codeIdx = 0; codeIdx < message.layout.paintingCodes.length; codeIdx++) {
              const code = message.layout.paintingCodes[codeIdx];
              const cached = paintingPreviewCache.get(code);
              if (cached) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewSize &&
                  e.y >= previewY &&
                  e.y < previewY + previewSize
                ) {
                  message.layout.hoveredPainting = code;
                  message.layout.hoveredPaintingIndex = codeIdx;
                  break;
                }
                
                previewX += previewSize + 2; // Move to next preview with 2px gap
              } else {
                previewX += 64; // Reserve space for loading preview
              }
            }
          }
          
          // 📺 Check for hover on YouTube previews
          if (message.layout.youtubeVideoIds) {
            const paintingOffset = message.layout.paintingCodes ? 74 : 0;
            const previewY = message.layout.y + message.layout.height + 4 + paintingOffset;
            let previewX = message.layout.x;
            const previewW = 120;
            const previewH = 68;
            
            message.layout.hoveredYoutube = null;
            
            for (let vidIdx = 0; vidIdx < message.layout.youtubeVideoIds.length; vidIdx++) {
              const videoId = message.layout.youtubeVideoIds[vidIdx];
              const cached = youtubePreviewCache.get(videoId);
              if (cached) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewW &&
                  e.y >= previewY &&
                  e.y < previewY + previewH
                ) {
                  message.layout.hoveredYoutube = videoId;
                  break;
                }
                previewX += previewW + 4;
              } else {
                previewX += 120 + 4;
              }
            }
          }
          
          // 🔗 Check for hover on OG link previews
          if (message.layout.ogUrls) {
            const paintingOffset = message.layout.paintingCodes ? 74 : 0;
            const youtubeOffset = message.layout.youtubeVideoIds ? 74 : 0;
            const previewY = message.layout.y + message.layout.height + 4 + paintingOffset + youtubeOffset;
            let previewX = message.layout.x;
            const previewW = 120;
            const previewH = 68;
            
            message.layout.hoveredOgUrl = null;
            
            for (let urlIdx = 0; urlIdx < message.layout.ogUrls.length; urlIdx++) {
              const url = message.layout.ogUrls[urlIdx];
              const cached = ogPreviewCache.get(url);
              if (cached && cached.imageData && !cached.failed) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewW &&
                  e.y >= previewY &&
                  e.y < previewY + previewH
                ) {
                  message.layout.hoveredOgUrl = url;
                  break;
                }
                previewX += previewW + 4;
              }
            }
          }

          // Store the clicked message for potential interaction
          message.clicked = true;

          // TODO: ⏰ Add timestamp hover and activate per message.
          //            - With 'delete' action.
        }
      }
    }

    if (e.is("lift")) {
      for (let i = client.messages.length - 1; i >= 0; i--) {
        const message = client.messages[i];
        if (!message.tb || !message.layout) {
          continue; // If `tb` is not defined then kill this. 👾
        }
        message.layout.inBox = false;
        
        // Handle clicks on interactive elements with proper hit detection
        // 🚫 Skip link activation if user was scrolling/dragging
        if (message.clicked) {
          // Always reset clicked state
          message.clicked = false;
          
          // But only process interaction if not scrolling
          if (wasScrollingOnLift) {
            continue; // User was scrolling, skip link activation
          }
          
          // Track if any interactive element was clicked
          let clickedInteractiveElement = false;
          
          // Parse the original message to find interactive elements
          const parsedElements = parseMessageElements(message.fullMessage);
          
          // Calculate click position relative to message
          const relativeX = e.x - message.layout.x;
          const relativeY = e.y - message.layout.y;
          
          // Check each interactive element for hit detection
          for (const element of parsedElements) {
            // Calculate the position of this element in the rendered text
            // Use per-message font settings
            const msgRowHeight = message.computedRowHeight || (typeface.blockHeight + 1);
            const msgTypefaceName = message.computedTypefaceName;
            const elementPosition = calculateElementPosition(
              element, 
              message.fullMessage, 
              message.tb.lines, 
              text,
              msgRowHeight,
              msgTypefaceName
            );
            
            if (elementPosition && isClickInsideElement(relativeX, relativeY, elementPosition)) {
              clickedInteractiveElement = true;
              if (element.type === "handle") {
                beep();
                // Show confirmation modal for handle navigation
                linkConfirmModal = {
                  type: "handle",
                  text: element.text,
                  displayText: element.text,
                  description: "View this user's profile",
                  action: () => jump(element.text)
                };
                break;
              } else if (element.type === "prompt") {
                // Skip individual quote marks - only handle full prompts
                if (element.text === "'") {
                  clickedInteractiveElement = false;
                  continue;
                }
                
                beep();
                const innerPrompt = element.text.slice(1, -1); // Unquote prompt text.
                let jumpTarget;
                if (innerPrompt.startsWith(">")) {
                  jumpTarget = "prompt~" + innerPrompt.slice(1);
                } else if (innerPrompt.startsWith("#")) {
                  jumpTarget = "painting#" + innerPrompt.slice(1);
                } else {
                  jumpTarget = "prompt~" + innerPrompt;
                }
                // Get command description from registry
                const cmdDescription = getCommandDescription(innerPrompt);
                // Show confirmation modal
                linkConfirmModal = {
                  type: "prompt",
                  text: jumpTarget,
                  displayText: innerPrompt,
                  description: cmdDescription,
                  action: () => jump(jumpTarget)
                };
                break;
              } else if (element.type === "prompt-content") {
                // Click on content inside quotes - treat as prompt
                beep();
                const innerPrompt = element.text;
                let jumpTarget;
                if (innerPrompt.startsWith(">")) {
                  jumpTarget = "prompt~" + innerPrompt.slice(1);
                } else if (innerPrompt.startsWith("#")) {
                  jumpTarget = "painting#" + innerPrompt.slice(1);
                } else {
                  jumpTarget = "prompt~" + innerPrompt;
                }
                // Get command description from registry
                const cmdDescription = getCommandDescription(innerPrompt);
                // Show confirmation modal
                linkConfirmModal = {
                  type: "prompt",
                  text: jumpTarget,
                  displayText: innerPrompt,
                  description: cmdDescription,
                  action: () => jump(jumpTarget)
                };
                break;
              } else if (element.type === "kidlisp-token") {
                // Find the full kidlisp code block (all tokens between the quotes)
                const kidlispTokens = parsedElements.filter(el => el.type === "kidlisp-token");
                if (kidlispTokens.length > 0) {
                  // Get the full range from first to last token
                  const firstToken = kidlispTokens[0];
                  const lastToken = kidlispTokens[kidlispTokens.length - 1];
                  const fullKidlispCode = message.fullMessage.substring(firstToken.start, lastToken.end);
                  
                  beep();
                  // Show confirmation modal for kidlisp
                  linkConfirmModal = {
                    type: "kidlisp",
                    text: fullKidlispCode,
                    displayText: fullKidlispCode.slice(0, 30),
                    description: "Execute KidLisp code",
                    action: () => jump(fullKidlispCode)
                  };
                }
                break;
              } else if (element.type === "url") {
                beep();
                // Show confirmation modal for external URL
                linkConfirmModal = {
                  type: "url",
                  text: element.text,
                  displayText: element.text,
                  description: "Open in browser",
                  action: () => jump("out:" + element.text)
                };
                break;
              } else if (element.type === "painting") {
                beep();
                // Show confirmation modal for painting
                linkConfirmModal = {
                  type: "painting",
                  text: element.text,
                  displayText: element.text,
                  description: "View this painting",
                  action: () => jump("painting" + element.text)
                };
                break;
              } else if (element.type === "kidlisp") {
                beep();
                // Show confirmation modal for kidlisp reference
                linkConfirmModal = {
                  type: "kidlisp",
                  text: element.text,
                  displayText: element.text,
                  description: "Open KidLisp piece",
                  action: () => jump(element.text)
                };
                break;
              } else if (element.type === "clock") {
                beep();
                // Show confirmation modal for clock reference
                linkConfirmModal = {
                  type: "clock",
                  text: element.text,
                  displayText: element.text,
                  description: "Open clock piece",
                  action: () => jump(element.text)
                };
                break;
              } else if (element.type === "r8dio") {
                beep();
                // Show confirmation modal for r8dio radio
                linkConfirmModal = {
                  type: "r8dio",
                  text: "r8dio",
                  displayText: "r8Dio",
                  description: "Listen to Danish talk radio",
                  action: () => jump("r8dio")
                };
                break;
              } else if (element.type === "youtube") {
                beep();
                // Open YouTube modal directly (no confirmation needed)
                openYoutubeModal(element.videoId);
                clickedInteractiveElement = true;
                break;
              }
            }
          }
          
          // 🎨 Check if click was on a painting preview
          if (message.layout.paintingCodes) {
            const previewY = message.layout.y + message.layout.height + 4;
            let previewX = message.layout.x;
            const previewSize = 64; // Fixed size for Ken Burns previews
            
            for (const code of message.layout.paintingCodes) {
              const cached = paintingPreviewCache.get(code);
              if (cached) {
                // Check if click is inside this preview
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewSize &&
                  e.y >= previewY &&
                  e.y < previewY + previewSize
                ) {
                  clickedInteractiveElement = true;
                  beep();
                  // Open painting in fullscreen modal
                  modalPainting = {
                    painting: cached.painting,
                    code: cached.code,
                    metadata: cached.metadata
                  };
                  modalJustOpened = true; // Prevent closing on same click
                  break;
                }
                
                previewX += previewSize + 2; // Move to next preview with 2px gap
              } else {
                previewX += 64; // Reserve space for loading preview (no gap)
              }
            }
          }
          
          // 📺 Check if click was on a YouTube preview
          if (message.layout.youtubeVideoIds && !clickedInteractiveElement) {
            const paintingOffset = message.layout.paintingCodes ? 74 : 0;
            const previewY = message.layout.y + message.layout.height + 4 + paintingOffset;
            let previewX = message.layout.x;
            const previewW = 120;
            const previewH = 68;
            
            for (const videoId of message.layout.youtubeVideoIds) {
              const cached = youtubePreviewCache.get(videoId);
              if (cached) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewW &&
                  e.y >= previewY &&
                  e.y < previewY + previewH
                ) {
                  clickedInteractiveElement = true;
                  beep();
                  // Open YouTube modal
                  openYoutubeModal(videoId);
                  break;
                }
                previewX += previewW + 4;
              } else {
                previewX += 120 + 4;
              }
            }
          }
          
          // 🔗 Check if click was on an OG link preview
          if (message.layout.ogUrls && !clickedInteractiveElement) {
            const paintingOffset = message.layout.paintingCodes ? 74 : 0;
            const youtubeOffset = message.layout.youtubeVideoIds ? 74 : 0;
            const previewY = message.layout.y + message.layout.height + 4 + paintingOffset + youtubeOffset;
            let previewX = message.layout.x;
            const previewW = 120;
            const previewH = 68;
            
            for (const url of message.layout.ogUrls) {
              const cached = ogPreviewCache.get(url);
              if (cached && cached.imageData && !cached.failed) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewW &&
                  e.y >= previewY &&
                  e.y < previewY + previewH
                ) {
                  clickedInteractiveElement = true;
                  beep();
                  // Show confirmation modal for external URL (same as text link)
                  const ogTitle = cached.title || url;
                  linkConfirmModal = {
                    type: "url",
                    text: url,
                    displayText: ogTitle.length > 40 ? ogTitle.slice(0, 38) + "…" : ogTitle,
                    description: cached.siteName || "Open in browser",
                    action: () => jump("out:" + url)
                  };
                  break;
                }
                previewX += previewW + 4;
              }
            }
          }
          
          // 📋 If click was on plain message text (not interactive elements), show copy modal
          // Only show if this was a tap, not a drag/scroll (scroll already handled above with continue)
          if (!clickedInteractiveElement && !linkConfirmModal && !modalPainting) {
            // Check if click was within the message text bounds
            const messageTextHeight = message.layout.height;
            if (relativeX >= 0 && relativeY >= 0 && relativeY < messageTextHeight) {
              beep();
              messageCopyModal = {
                message: message.fullMessage,
                from: message.from,
                when: message.when,
                copied: false,
                error: false
              };
            }
          }
        }

        // Timestamps (Moderation Menu)
        const timestamp = message.layout.timestamp;
        if (timestamp.over) {
          timestamp.over = false;
        }
        
        // Clear hover states on lift
        if (message.layout.hoveredElements) {
          message.layout.hoveredElements.clear();
        }
      }
    }

    // 📜 Scrolling
    if (e.is("draw")) {
      // Track if user has scrolled/dragged beyond threshold
      if (dragStartPos && !isDragging) {
        const dx = Math.abs(e.x - dragStartPos.x);
        const dy = Math.abs(e.y - dragStartPos.y);
        if (dx > dragDistanceThreshold || dy > dragDistanceThreshold) {
          isDragging = true; // Mark as dragging - will prevent link activation on lift
        }
      }
      
      scroll += e.delta.y;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    if (e.is("keyboard:down:arrowdown")) {
      scroll -= 10;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    if (e.is("keyboard:down:arrowup")) {
      scroll += 10;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    if (e.is("scroll")) {
      scroll -= e.y;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    // Track hover state for button visual feedback
    if (e.is("move")) {
      handleBtn.over = handleBtn.btn.box.contains(e);
      inputBtn.over = inputBtn.box.contains(e);
      
      let hoveredAnyElement = false;
      hoveredMessageIndex = null; // Reset hovered message tracking
      
      // Skip message hover detection if in top UI area
      if (e.y < topMargin) {
        // Clear any existing hover states
        for (let i = 0; i < client.messages.length; i++) {
          const message = client.messages[i];
          if (message.layout?.hoveredElements) {
            message.layout.hoveredElements.clear();
          }
        }
        return;
      }
      
      // Also track hover on message elements (handles, links, etc.)
      for (let i = client.messages.length - 1; i >= 0; i--) {
        const message = client.messages[i];
        if (!message.tb || !message.layout) {
          continue;
        }
        
        // Check if hovering over this message
        if (
          e.x > message.layout.x &&
          e.x < message.layout.x + message.layout.width &&
          e.y > message.layout.y &&
          e.y < message.layout.y + message.layout.height
        ) {
          // 🎯 Track which message is hovered for background highlight
          hoveredMessageIndex = i;
          
          // Check for hover on interactive elements
          const parsedElements = parseMessageElements(message.fullMessage);
          const relativeX = e.x - message.layout.x;
          const relativeY = e.y - message.layout.y;
          
          // Reset hover states
          if (!message.layout.hoveredElements) {
            message.layout.hoveredElements = new Set();
          }
          message.layout.hoveredElements.clear();
          
          // Check each interactive element for hover
          for (const element of parsedElements) {
            // Use per-message font settings
            const msgRowHeight = message.computedRowHeight || (typeface.blockHeight + 1);
            const msgTypefaceName = message.computedTypefaceName;
            const elementPosition = calculateElementPosition(
              element, 
              message.fullMessage, 
              message.tb.lines, 
              text,
              msgRowHeight,
              msgTypefaceName
            );
            
            if (elementPosition && isClickInsideElement(relativeX, relativeY, elementPosition, text, msgTypefaceName)) {
              message.layout.hoveredElements.add(element);
              hoveredAnyElement = true;
              break; // Only hover one element at a time
            }
          }
          
          // Check timestamp hover
          const timestamp = message.layout.timestamp;
          const startX = message.layout.x + timestamp.x;
          if (
            e.x > startX &&
            e.x < startX + timestamp.width &&
            e.y > timestamp.y &&
            e.y < timestamp.y + timestamp.height
          ) {
            timestamp.over = true;
          } else {
            timestamp.over = false;
          }
          
          // 🎨 Check for hover on painting previews
          if (message.layout.paintingCodes) {
            const previewY = message.layout.y + message.layout.height + 4;
            let previewX = message.layout.x;
            message.layout.hoveredPainting = null;
            message.layout.hoveredPaintingIndex = null;
            
            for (let codeIdx = 0; codeIdx < message.layout.paintingCodes.length; codeIdx++) {
              const code = message.layout.paintingCodes[codeIdx];
              const cached = paintingPreviewCache.get(code);
              const previewSize = 64; // Fixed size for Ken Burns previews
              
              if (cached) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewSize &&
                  e.y >= previewY &&
                  e.y < previewY + previewSize
                ) {
                  message.layout.hoveredPainting = code;
                  message.layout.hoveredPaintingIndex = codeIdx;
                  hoveredAnyElement = true;
                  break;
                }
                
                previewX += previewSize + 2; // Move to next preview with 2px gap
              } else {
                previewX += 64; // Reserve space for loading preview
              }
            }
          }
          
          // 📺 Check for hover on YouTube previews
          if (message.layout.youtubeVideoIds) {
            const paintingOffset = message.layout.paintingCodes ? 74 : 0;
            const previewY = message.layout.y + message.layout.height + 4 + paintingOffset;
            let previewX = message.layout.x;
            const previewW = 120;
            const previewH = 68;
            
            message.layout.hoveredYoutube = null;
            
            for (let vidIdx = 0; vidIdx < message.layout.youtubeVideoIds.length; vidIdx++) {
              const videoId = message.layout.youtubeVideoIds[vidIdx];
              const cached = youtubePreviewCache.get(videoId);
              if (cached) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewW &&
                  e.y >= previewY &&
                  e.y < previewY + previewH
                ) {
                  message.layout.hoveredYoutube = videoId;
                  hoveredAnyElement = true;
                  break;
                }
                previewX += previewW + 4;
              } else {
                previewX += 120 + 4;
              }
            }
          }
          
          // 🔗 Check for hover on OG link previews
          if (message.layout.ogUrls) {
            const paintingOffset = message.layout.paintingCodes ? 74 : 0;
            const youtubeOffset = message.layout.youtubeVideoIds ? 74 : 0;
            const previewY = message.layout.y + message.layout.height + 4 + paintingOffset + youtubeOffset;
            let previewX = message.layout.x;
            const previewW = 120;
            const previewH = 68;
            
            message.layout.hoveredOgUrl = null;
            
            for (let urlIdx = 0; urlIdx < message.layout.ogUrls.length; urlIdx++) {
              const url = message.layout.ogUrls[urlIdx];
              const cached = ogPreviewCache.get(url);
              if (cached && cached.imageData && !cached.failed) {
                if (
                  e.x >= previewX &&
                  e.x < previewX + previewW &&
                  e.y >= previewY &&
                  e.y < previewY + previewH
                ) {
                  message.layout.hoveredOgUrl = url;
                  hoveredAnyElement = true;
                  break;
                }
                previewX += previewW + 4;
              }
            }
          }
        } else {
          // Clear hover states when not over message
          if (message.layout.hoveredElements) {
            message.layout.hoveredElements.clear();
          }
          if (message.layout.timestamp) {
            message.layout.timestamp.over = false;
          }
        }
      }
      
      // Change cursor to pointer when hovering over interactive elements
      if (hoveredAnyElement || handleBtn.over || inputBtn.over) {
        send({ type: "cursor", cursor: "pointer" });
      } else {
        send({ type: "cursor", cursor: "default" });
      }
    }

    // 🖥️ Keyboard / Text Input
    inputBtn.act(e, {
      down: () => {
        send({ type: "keyboard:enabled" }); // In case corner was pressed,
        //                                     which can disable the keyboard.
        send({ type: "keyboard:soft-unlock" });
      },
      push: () => {
        send({ type: "keyboard:soft-lock" });
      },
      cancel: () => {
        send({ type: "keyboard:soft-lock" });
      },
      rollout: () => {
        send({ type: "keyboard:soft-lock" });
      },
      rollover: () => {
        if (inputBtn.down) send({ type: "keyboard:soft-unlock" });
      },
    });

    handleBtn.act(e, () => {
      const hand = handle();
      if (!hand) store["prompt:splash"] = true;
      jump(hand || "prompt");
    });

    if (
      !input.canType &&
      e.is("keyboard:down:enter") // ||
      // e.is("keyboard:down:escape") ||
      // e.is("keyboard:down:`")
    ) {
      send({ type: "keyboard:open" });
    }

    if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) jump("prompt");

    // Backspace back to `prompt`.
    if (e.is("keyboard:down:backspace")) {
      jump(`prompt~${hud.currentLabel().plainText || piece}`)(() => {
        send({ type: "keyboard:open" });
      });
    }
  }

  // 🔍 @handle autocomplete keyboard handling (when typing)
  if (input.canType && handleAutocomplete?.visible && handleAutocomplete.items.length > 0) {
    if (e.is("keyboard:down:arrowup")) {
      handleAutocomplete.selectedIndex = handleAutocomplete.selectedIndex > 0
        ? handleAutocomplete.selectedIndex - 1
        : handleAutocomplete.items.length - 1;
      return; // Consume event
    }
    if (e.is("keyboard:down:arrowdown")) {
      handleAutocomplete.selectedIndex = (handleAutocomplete.selectedIndex + 1) % handleAutocomplete.items.length;
      return; // Consume event
    }
    if ((e.is("keyboard:down:tab") || e.is("keyboard:down:enter")) && handleAutocomplete.selected) {
      const cursorPos = input.prompt?.textPos?.() ?? input.text.length;
      const newText = handleAutocomplete.getCompletedText(input.text, cursorPos);
      input.text = newText + " "; // Add space after handle
      input.snap();
      send({ type: "keyboard:text:replace", content: { text: input.text } });
      handleAutocomplete.hide();
      return; // Consume event
    }
    if (e.is("keyboard:down:escape")) {
      handleAutocomplete.hide();
      return; // Consume event
    }
  }

  // 🔍 @handle autocomplete pointer/mouse handling
  if (input.canType && handleAutocomplete?.visible && handleAutocomplete.items.length > 0) {
    const result = handleAutocomplete.handlePointer(e);
    if (result.clicked) {
      // Item was clicked - complete with the clicked item
      const cursorPos = input.prompt?.textPos?.() ?? input.text.length;
      const newText = handleAutocomplete.getCompletedText(input.text, cursorPos);
      input.text = newText + " "; // Add space after handle
      input.snap();
      send({ type: "keyboard:text:replace", content: { text: input.text } });
      handleAutocomplete.hide();
      return; // Consume event
    }
    if (result.consumed) {
      return; // Consume event to prevent other interactions
    }
  }

  if (
    input.canType &&
    (e.is("keyboard:down:`") ||
      e.is("keyboard:down:escape") ||
      (input.text.trim().length === 0 &&
        e.is("keyboard:down:enter") &&
        !e.shift))
  ) {
    console.log("⌨️🔴 [chat.mjs] sending keyboard:close - reason: backtick/escape/empty-enter");
    send({ type: "keyboard:close" });
  }

  if (input.canType && e.is("lift") && !input.shifting && !input.paste.down) {
    // Don't close if the Enter button is currently pressed down (user is activating it)
    // Check btn.down instead of box.contains because box position may not be updated until paint()
    const enterButtonIsDown = input.enter && !input.enter.btn.disabled && input.enter.btn.down;
    
    // 🔧 FIX: Don't close keyboard when touching the preview/message area (above bottom panel)
    // This prevents iOS keyboard from getting into a disconnected state
    const isInBottomPanel = e.y >= api.screen.height - bottomMargin;
    
    console.log("⌨️📍 [chat.mjs lift] e.y:", e.y, "screenH:", api.screen.height, "bottomMargin:", bottomMargin, "isInBottomPanel:", isInBottomPanel, "enterBtnDown:", enterButtonIsDown);
    
    if (!enterButtonIsDown && isInBottomPanel) {
      console.log("⌨️🔴 [chat.mjs] sending keyboard:close - reason: lift in bottom panel");
      send({ type: "keyboard:close" });
    }
  }

  // Trigger re-layout when keyboard opens/closes (changes visible message area)
  if (e.is("keyboard:open") || e.is("keyboard:close")) {
    messagesNeedLayout = true;
    // 🔍 Hide autocomplete when keyboard closes to prevent stale state
    if (e.is("keyboard:close") && handleAutocomplete) {
      handleAutocomplete.hide();
    }
  }

  // Debug: Check the condition for calling input.act
  const shouldCallInputAct = (
    e.is("keyboard:open") ||
    e.is("keyboard:close") ||
    (input.canType && !e.is("keyboard:down:escape")) ||
    // Also handle touch/lift events when input is not active but user interacts with the input button area
    (!input.canType && (e.is("touch") || e.is("lift")) && inputBtn && inputBtn.btn.box.contains(e))
  );
  
  if (shouldCallInputAct) {
    input.act(api);
  }
}

function sim({ api, num, send, net }) {
  if (input) {
    // 🔍 Sync autocomplete skipHistory and skipEnter flags with TextInput
    // Only skip Enter when autocomplete is truly visible with selectable items AND keyboard is open
    const autocompleteActive = !!(
      input.canType && 
      handleAutocomplete?.visible && 
      handleAutocomplete.items.length > 0 && 
      handleAutocomplete.selected
    );
    input.skipHistory = autocompleteActive;
    input.skipEnter = autocompleteActive;
    input.sim(api); // 💬 Chat
  }
  ellipsisTicker?.update(api.clock?.time());
  
  // 📻 R8dio player simulation
  if (r8dioEnabled) {
    r8dioAnimPhase += 0.05;
    
    // Request frequency/waveform data when playing
    if (r8dioPlaying && send) {
      send({ type: "stream:frequencies", content: { id: R8DIO_STREAM_ID } });
      if (r8dioNoAnalyserCount >= 10) {
        send({ type: "stream:waveform", content: { id: R8DIO_STREAM_ID } });
      }
    }
    
    // Fetch metadata periodically
    const now = Date.now();
    if (r8dioPlaying && now - r8dioLastMetadataFetch > R8DIO_METADATA_INTERVAL) {
      fetchR8dioMetadata(net);
    }
    
    // Update bars
    const lerp = num?.lerp || ((a, b, t) => a + (b - a) * t);
    for (let i = 0; i < R8DIO_BAR_COUNT; i++) {
      if (!r8dioBars[i]) r8dioBars[i] = { height: 0, targetHeight: 0 };
      
      if (r8dioPlaying && r8dioFrequencyData.length > 0) {
        const dataIndex = Math.floor((i / R8DIO_BAR_COUNT) * r8dioFrequencyData.length);
        r8dioBars[i].targetHeight = (r8dioFrequencyData[dataIndex] || 0) / 255;
        r8dioNoAnalyserCount = 0;
      } else if (r8dioPlaying && r8dioWaveformData.length > 0) {
        const dataIndex = Math.floor((i / R8DIO_BAR_COUNT) * r8dioWaveformData.length);
        const sample = r8dioWaveformData[dataIndex] || 128;
        r8dioBars[i].targetHeight = Math.abs(sample - 128) / 128;
      } else if (r8dioPlaying) {
        // Fake animation while playing without analyser data
        const wave = Math.sin(r8dioAnimPhase + i * 0.3) * 0.3 + 0.4;
        const noise = Math.random() * 0.2;
        r8dioBars[i].targetHeight = wave + noise;
      } else {
        r8dioBars[i].targetHeight = 0;
      }
      
      r8dioBars[i].height = lerp(r8dioBars[i].height, r8dioBars[i].targetHeight, 0.15);
    }
  }
}

// 📻 Handle BIOS messages for r8dio streaming
function receive({ type, content }) {
  if (!r8dioEnabled) return;
  
  if (type === "stream:playing" && content.id === R8DIO_STREAM_ID) {
    r8dioPlaying = true;
    r8dioLoading = false;
    r8dioError = null;
  }
  
  if (type === "stream:paused" && content.id === R8DIO_STREAM_ID) {
    r8dioPlaying = false;
  }
  
  if (type === "stream:stopped" && content.id === R8DIO_STREAM_ID) {
    r8dioPlaying = false;
    r8dioLoading = false;
  }
  
  if (type === "stream:error" && content.id === R8DIO_STREAM_ID) {
    r8dioPlaying = false;
    r8dioLoading = false;
    r8dioError = content.error;
  }
  
  if (type === "stream:frequencies-data" && content.id === R8DIO_STREAM_ID) {
    const data = content.data || [];
    if (data.length > 0 && data.some(v => v > 0)) {
      r8dioFrequencyData = data;
      r8dioNoAnalyserCount = 0;
    } else {
      r8dioNoAnalyserCount++;
      r8dioFrequencyData = [];
    }
  }
  
  if (type === "stream:waveform-data" && content.id === R8DIO_STREAM_ID) {
    r8dioWaveformData = content.data || [];
  }
}

// function leave() {
// chat?.kill();
// }

export { boot, paint, act, sim, receive };

// 📚 Library
//   (Useful functions used throughout the piece)

// 📻 Fetch r8dio track metadata
async function fetchR8dioMetadata(net) {
  r8dioLastMetadataFetch = Date.now();
  try {
    const response = await fetch(R8DIO_METADATA_URL);
    if (response.ok) {
      const data = await response.json();
      if (data.current_track && data.current_track.title) {
        r8dioTrack = data.current_track.title;
      }
    }
  } catch (err) {
    // Silently fail - metadata is optional
    console.log("📻 Could not fetch r8dio metadata:", err.message);
  }
}

// 📻 R8dio playback control
function toggleR8dioPlayback(send) {
  if (r8dioLoading) return;
  
  if (r8dioPlaying) {
    // Pause
    send({ type: "stream:pause", content: { id: R8DIO_STREAM_ID } });
  } else {
    // Play
    r8dioLoading = true;
    r8dioError = null;
    send({ 
      type: "stream:play", 
      content: { 
        id: R8DIO_STREAM_ID, 
        url: R8DIO_STREAM_URL, 
        volume: r8dioVolume 
      } 
    });
  }
}

// 📻 R8dio volume control
function setR8dioVolume(vol, send) {
  r8dioVolume = Math.max(0, Math.min(1, vol));
  if (r8dioPlaying && send) {
    send({ type: "stream:volume", content: { id: R8DIO_STREAM_ID, volume: r8dioVolume } });
  }
}

function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
  }
}

// 🎨 Load painting preview by code
async function loadPaintingPreview(code, get, store) {
  // Check cache first
  if (paintingPreviewCache.has(code)) {
    return paintingPreviewCache.get(code);
  }
  
  // Check if already loading
  if (paintingLoadQueue.has(code)) {
    return null; // Still loading
  }
  
  paintingLoadQueue.add(code);
  paintingLoadProgress.set(code, 0); // Start at 0%
  
  try {
    // Resolve metadata (same approach as painting.mjs)
    const normalized = code.replace(/^#/, '');
    const cached = store[`painting-code:${normalized}`];
    
    paintingLoadProgress.set(code, 0.2); // 20% - starting metadata fetch
    
    let metadata;
    if (cached?.slug && cached?.handle) {
      metadata = cached;
      paintingLoadProgress.set(code, 0.4); // 40% - metadata from cache
    } else {
      const response = await fetch(`/api/painting-code?code=${normalized}`);
      if (!response.ok) {
        paintingLoadQueue.delete(code);
        paintingLoadProgress.delete(code);
        return null;
      }
      metadata = await response.json();
      paintingLoadProgress.set(code, 0.4); // 40% - metadata fetched
      
      // Cache the metadata
      if (metadata?.slug && metadata?.handle) {
        store[`painting-code:${normalized}`] = metadata;
      }
    }
    
    if (!metadata?.slug || !metadata?.handle) {
      paintingLoadQueue.delete(code);
      paintingLoadProgress.delete(code);
      return null;
    }
    
    paintingLoadProgress.set(code, 0.6); // 60% - starting image load
    
    // Load the painting
    const got = await get.painting(metadata.slug).by(metadata.handle);
    const painting = got.img;
    
    paintingLoadProgress.set(code, 0.9); // 90% - image loaded
    
    // Cache it
    const result = { painting, metadata, code: normalized };
    paintingPreviewCache.set(code, result);
    paintingLoadQueue.delete(code);
    paintingLoadProgress.delete(code);
    
    return result;
  } catch (err) {
    console.warn(`Failed to load painting preview #${code}:`, err);
    paintingLoadQueue.delete(code);
    paintingLoadProgress.delete(code);
    return null;
  }
}

// � Load Open Graph preview for a URL
async function loadOgPreview(url, preload) {
  // Check cache first
  if (ogPreviewCache.has(url)) {
    return ogPreviewCache.get(url);
  }
  if (globalOgPreviewCache && globalOgPreviewCache.has(url)) {
    const cached = globalOgPreviewCache.get(url);
    ogPreviewCache.set(url, cached);
    return cached;
  }
  
  // Check if already loading
  if (ogLoadQueue.has(url)) {
    return null; // Still loading
  }
  
  ogLoadQueue.add(url);
  
  try {
    // Fetch OG metadata from our serverless function
    // In local dev, Netlify Dev can't make outbound HTTP from functions,
    // so use the production endpoint instead
    const isLocal = typeof window !== "undefined" && 
      (window.location.hostname === "localhost" || window.location.hostname === "127.0.0.1");
    const apiBase = isLocal ? "https://aesthetic.computer" : "";
    const apiUrl = `${apiBase}/api/og-preview?url=${encodeURIComponent(url)}`;
    const response = await fetch(apiUrl);
    
    if (!response.ok) {
      throw new Error(`Failed to fetch OG data: ${response.status}`);
    }
    
    const data = await response.json();
    
    // If there's an og:image, load it
    let imageData = null;
    let faviconData = null;
    
    if (data.image) {
      try {
        const loaded = await preload(data.image);
        imageData = loaded?.img || loaded;
      } catch (imgErr) {
        console.warn(`Failed to load OG image for ${url}:`, imgErr);
      }
    }
    
    // If no OG image or it failed, try to load favicon as fallback
    if (!imageData && data.favicon) {
      try {
        const loaded = await preload(data.favicon);
        faviconData = loaded?.img || loaded;
      } catch (favErr) {
        console.warn(`Failed to load favicon for ${url}:`, favErr);
      }
    }
    
    // If still no image, try common favicon paths
    if (!imageData && !faviconData) {
      try {
        const urlObj = new URL(url);
        const defaultFavicon = `${urlObj.origin}/favicon.ico`;
        const loaded = await preload(defaultFavicon);
        faviconData = loaded?.img || loaded;
      } catch (defaultFavErr) {
        // Ignore - no favicon available
      }
    }
    
    const result = {
      url,
      title: data.title,
      description: data.description,
      image: data.image,
      imageData,
      faviconData, // Fallback favicon
      siteName: data.siteName,
      favicon: data.favicon,
    };
    
    ogPreviewCache.set(url, result);
    if (globalOgPreviewCache) {
      globalOgPreviewCache.set(url, result);
    }
    ogLoadQueue.delete(url);
    return result;
  } catch (err) {
    console.warn(`Failed to load OG preview for ${url}:`, err);
    
    // Try to load just the favicon as a fallback
    let faviconData = null;
    try {
      const urlObj = new URL(url);
      const defaultFavicon = `${urlObj.origin}/favicon.ico`;
      const loaded = await preload(defaultFavicon);
      faviconData = loaded?.img || loaded;
    } catch (favErr) {
      // No favicon either
    }
    
    // Cache result with favicon fallback
    const result = { 
      url, 
      title: null, 
      image: null, 
      imageData: null, 
      faviconData, // May have favicon even if OG failed
      failed: !faviconData // Only mark as fully failed if no favicon either
    };
    ogPreviewCache.set(url, result);
    ogLoadQueue.delete(url);
    return result;
  }
}

// �📺 Load YouTube thumbnail by video ID
async function loadYoutubePreview(videoId, preload) {
  // Check cache first
  if (youtubePreviewCache.has(videoId)) {
    return youtubePreviewCache.get(videoId);
  }
  if (globalYoutubeThumbCache && globalYoutubeThumbCache.has(videoId)) {
    const cached = globalYoutubeThumbCache.get(videoId);
    youtubePreviewCache.set(videoId, cached);
    return cached;
  }
  
  // Check if already loading
  if (youtubeLoadQueue.has(videoId)) {
    return null; // Still loading
  }
  
  youtubeLoadQueue.add(videoId);
  
  try {
    // YouTube thumbnails are publicly available without API key
    // mqdefault is 320x180 (16:9 aspect ratio)
    const thumbnailUrl = `https://i.ytimg.com/vi/${videoId}/mqdefault.jpg`;
    
    // Load the thumbnail image using net.preload
    const loaded = await preload(thumbnailUrl);
    const thumbnail = loaded?.img || loaded;
    
    if (thumbnail) {
      const result = { thumbnail, videoId };
      youtubePreviewCache.set(videoId, result);
      if (globalYoutubeThumbCache) {
        globalYoutubeThumbCache.set(videoId, result);
      }
      youtubeLoadQueue.delete(videoId);
      return result;
    }
    
    youtubeLoadQueue.delete(videoId);
    return null;
  } catch (err) {
    console.warn(`Failed to load YouTube thumbnail ${videoId}:`, err);
    youtubeLoadQueue.delete(videoId);
    return null;
  }
}

// 📺 Open YouTube modal with embed iframe
// On iOS, we skip the embed modal and open YouTube directly 
// because the iframe embed has audio issues and tap-outside-to-close doesn't work reliably
// (All iOS browsers use WebKit and have the same iframe restrictions)
function openYoutubeModal(videoId) {
  if (!domApi) return;
  
  // On iOS, open YouTube directly instead of embed (audio doesn't work, touch handling issues)
  if (iOS) {
    window.open(`https://www.youtube.com/watch?v=${videoId}`, "_blank");
    return;
  }
  
  if (youtubeModalOpen) {
    const overlay = typeof document !== "undefined"
      ? document.getElementById("youtube-modal-overlay")
      : null;
    if (overlay) return;
    youtubeModalOpen = false;
    youtubeModalVideoId = null;
  }
  
  youtubeModalOpen = true;
  youtubeModalVideoId = videoId;
  if (typeof globalThis !== "undefined") {
    globalThis.acYoutubeModalState = { open: true, videoId };
    globalThis.acYoutubeModalClose = () => {
      youtubeModalOpen = false;
      youtubeModalVideoId = null;
      if (globalThis.acYoutubeModalState) {
        globalThis.acYoutubeModalState.open = false;
        globalThis.acYoutubeModalState.videoId = null;
      }
    };
  }
  
  domApi.html`
    <style>
      #youtube-modal-overlay {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: rgba(0, 0, 0, 0.75);
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        animation: ytFadeIn 0.2s ease-out;
        cursor: pointer;
        -webkit-tap-highlight-color: transparent;
        touch-action: none;
      }
      @keyframes ytFadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      #youtube-modal-content {
        width: min(90vw, 720px);
        max-width: calc(100vw - 32px);
        aspect-ratio: 16/9;
        background: #000;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
        cursor: default;
        position: relative;
      }
      #youtube-modal-content iframe {
        width: 100%;
        height: 100%;
        border: none;
        pointer-events: auto;
      }
      #youtube-modal-close {
        position: absolute;
        top: -40px;
        right: 0;
        width: 36px;
        height: 36px;
        background: rgba(255, 255, 255, 0.9);
        border: none;
        border-radius: 50%;
        cursor: pointer;
        font-size: 24px;
        line-height: 36px;
        text-align: center;
        color: #333;
        z-index: 10000;
        transition: background 0.15s, transform 0.15s;
      }
      #youtube-modal-close:hover {
        background: #fff;
        transform: scale(1.1);
      }
      #youtube-modal-close:active {
        transform: scale(0.95);
      }
      #youtube-modal-tap-hint {
        color: rgba(255, 255, 255, 0.6);
        font-size: 14px;
        margin-top: 16px;
        font-family: system-ui, sans-serif;
      }
    </style>
    <div id="youtube-modal-overlay" onclick="if(event.target.id === 'youtube-modal-overlay' || event.target.id === 'youtube-modal-tap-hint') { window.acCloseYoutubeModal && window.acCloseYoutubeModal(); }">
      <div id="youtube-modal-content">
        <button id="youtube-modal-close" onclick="window.acCloseYoutubeModal && window.acCloseYoutubeModal();">&times;</button>
        <iframe 
          src="https://www.youtube.com/embed/${videoId}?autoplay=1&rel=0"
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
          allowfullscreen>
        </iframe>
      </div>
      <div id="youtube-modal-tap-hint">tap outside to close</div>
    </div>
    <script>
      window.acCloseYoutubeModal = function() {
        const overlay = document.getElementById('youtube-modal-overlay');
        if (overlay) overlay.remove();
        if (window.acYoutubeModalClose) window.acYoutubeModalClose();
        if (window.acYoutubeModalState) {
          window.acYoutubeModalState.open = false;
          window.acYoutubeModalState.videoId = null;
        }
      };
      document.addEventListener('keydown', function ytEscHandler(e) {
        if (e.key === 'Escape') {
          window.acCloseYoutubeModal();
          document.removeEventListener('keydown', ytEscHandler);
        }
      });
    </script>
  `;
}

// 📺 Close YouTube modal
function closeYoutubeModal() {
  youtubeModalOpen = false;
  youtubeModalVideoId = null;
  // The DOM cleanup happens via the onclick/escape handlers in the HTML
}

function computeMessagesHeight({ text, screen, typeface }, chat, defaultTypefaceName, defaultRowHeight) {
  let height = 0;
  
  // System default row height (for fonts with rowHeight: null) - always use typeface.blockHeight
  const systemDefaultRowHeight = typeface.blockHeight + 1;
  
  // Iterate through the messages array.
  for (let i = 0; i < chat.messages.length; i += 1) {
    const message = chat.messages[i];
    
    // 🔤 Use per-message font (as sender chose) - fallback to font_1 for old messages
    const msgFontId = message.font || "font_1";
    const msgFontConfig = CHAT_FONTS[msgFontId] || CHAT_FONTS["font_1"];
    // null in config means "use system default" - NOT the user's current selection!
    const msgTypefaceName = msgFontConfig.typeface; // null = system default typeface
    const msgRowHeight = msgFontConfig.rowHeight !== null ? msgFontConfig.rowHeight : systemDefaultRowHeight;
    
    // Store computed font info on the message for use in paint
    message.computedTypefaceName = msgTypefaceName;
    message.computedRowHeight = msgRowHeight;
    message.computedFontId = msgFontId;
    
    // Add count multiplier if message was repeated
    const countSuffix = message.count > 1 ? ` x${message.count}` : "";
    const fullMessage = message.from + " " + message.text + countSuffix;
    const tb = text.box(
      fullMessage,
      { x: leftMargin, y: 0 },
      screen.width - leftMargin,
      1,
      true,
      msgTypefaceName
    );
    message.tb = tb;
    message.fullMessage = fullMessage;
    // Add height for all lines in the message
    // Each line is msgRowHeight tall (per-message font height)
    // Plus add lineGap between lines within the message and after the message
    height += tb.lines.length * msgRowHeight + lineGap;
    
    // 🎨 Add space for painting previews (if any)
    const paintingElements = parseMessageElements(fullMessage).filter(el => el.type === "painting");
    if (paintingElements.length > 0) {
      // 4px gap above + 64px image + 4px padding + 2px gap below = 74px
      const previewHeight = 74;
      height += previewHeight;
    }
    
    // 📺 Add space for YouTube previews (if any)
    const youtubeElements = parseMessageElements(fullMessage).filter(el => el.type === "youtube");
    if (youtubeElements.length > 0) {
      // 4px gap above + 64px image + 4px padding + 2px gap below = 74px
      const previewHeight = 74;
      height += previewHeight;
    }
    
    // 🔗 Add space for OG link previews (non-YouTube URLs with images)
    const urlElements = parseMessageElements(fullMessage).filter(el => el.type === "url" && !el.sensitive);
    // Only add space for URLs that have loaded OG data with images
    const urlsWithPreviews = urlElements.filter(el => {
      const cached = ogPreviewCache.get(el.text) || (globalOgPreviewCache && globalOgPreviewCache.get(el.text));
      return cached && cached.imageData && !cached.failed;
    });
    if (urlsWithPreviews.length > 0) {
      // 4px gap above + 64px image + 4px padding + 2px gap below = 74px
      const previewHeight = 74;
      height += previewHeight;
    }
  }
  return height;
}

// Build a display graph for the messages.
// (From the bottom to the top.)
function computeMessagesLayout({ screen, text, typeface }, chat, defaultTypefaceName, defaultRowHeight, bottomMargin) {
  // System default row height for fallback
  const systemDefaultRowHeight = typeface.blockHeight + 1;
  
  // Start from bottom, but use the first message's row height for initial positioning
  const lastMsg = chat.messages[chat.messages.length - 1];
  const lastMsgRowHeight = lastMsg ? (lastMsg.computedRowHeight ?? systemDefaultRowHeight) : systemDefaultRowHeight;
  let y = screen.height - lastMsgRowHeight - bottomMargin + scroll;

  // Delete all layouts.
  for (let i = chat.messages.length - 1; i >= 0; i--) {
    const msg = chat.messages[i];
    msg.lastLayout = msg.layout;
    delete msg.layout;
  }

  for (let i = chat.messages.length - 1; i >= 0; i--) {
    const msg = chat.messages[i];
    
    // 🔤 Use per-message font settings (computed in computeMessagesHeight)
    // null typeface means "system default", not user's current selection
    const msgTypefaceName = msg.computedTypefaceName; // null = system default
    const msgRowHeight = msg.computedRowHeight || systemDefaultRowHeight;
    const msgFontId = msg.computedFontId || "font_1";
    const msgTimestampGap = getFontTimestampGap(msgFontId);
    
    // Parse elements first to know if we have painting previews
    const parsedElements = parseMessageElements(msg.fullMessage);
    const paintingElements = parsedElements.filter(el => el.type === "painting");
    const paintingCodes = paintingElements.map(el => el.text.replace(/^#/, ''));
    
    // 📺 YouTube video IDs
    const youtubeElements = parsedElements.filter(el => el.type === "youtube");
    const youtubeVideoIds = youtubeElements.map(el => el.videoId);
    
    // 🔗 OG preview URLs (non-YouTube, non-sensitive URLs)
    const urlElements = parsedElements.filter(el => el.type === "url" && !el.sensitive);
    const ogUrls = urlElements.map(el => el.text);
    // Check which URLs have loaded OG data with images OR favicon fallbacks
    const ogUrlsWithPreviews = ogUrls.filter(url => {
      const cached = ogPreviewCache.get(url) || (globalOgPreviewCache && globalOgPreviewCache.get(url));
      return cached && (cached.imageData || cached.faviconData) && !cached.failed;
    });
    
    // 🎨 Move up for painting previews FIRST (before positioning this message)
    if (paintingCodes.length > 0) {
      // 4px gap above + 64px image + 4px padding + 2px gap below = 74px
      const previewHeight = 74;
      y -= previewHeight;
    }
    
    // 📺 Move up for YouTube previews
    if (youtubeVideoIds.length > 0) {
      const previewHeight = 74;
      y -= previewHeight;
    }
    
    // 🔗 Move up for OG link previews (including favicon fallbacks)
    // Include extra space for title caption (10px)
    if (ogUrlsWithPreviews.length > 0) {
      const previewHeight = 84; // 74px + 10px for caption
      y -= previewHeight;
    }

    y -= msgRowHeight * (msg.tb.lines.length - 1) + lineGap;
    if (y > screen.height - bottomMargin) {
      y -= msgRowHeight;
      continue;
    }
    
    // Create a modern color-coded message using \color\ syntax like KidLisp
    let colorCodedMessage = msg.fullMessage;
    let msgColor = "white";
    let inBox = msg.lastLayout?.inBox || false;

    // Sort elements by start position (reverse order for safe replacement)
    parsedElements.sort((a, b) => b.start - a.start);
    
    // Replace each element with color-coded version
    for (const element of parsedElements) {
      const elementText = msg.fullMessage.substring(element.start, element.end);
      let colorCodedText = "";
      
      if (element.type === "log") {
        // System log messages get cyan color with a distinctive look
        colorCodedText = `\\cyan\\${elementText}\\white\\`;
      } else if (element.type === "handle") {
        colorCodedText = `\\pink\\${elementText}\\white\\`;
      } else if (element.type === "url") {
        colorCodedText = `\\cyan\\${elementText}\\white\\`;
      } else if (element.type === "prompt") {
        colorCodedText = `\\yellow\\${elementText}\\white\\`;
      }
      
      // Replace the original text with the color-coded version
      colorCodedMessage = 
        colorCodedMessage.substring(0, element.start) + 
        colorCodedText + 
        colorCodedMessage.substring(element.end);
    }

    // Create layout using the color-coded message with per-message font
    const lastLineWidth = text.width(msg.tb.lines[msg.tb.lines.length - 1], msgTypefaceName);
    const timestamp = {
      x: lastLineWidth + msgTimestampGap,
      y: y + (msg.tb.lines.length - 1) * msgRowHeight,
      height: 8, // MatrixChunky8 height
      width: text.width(timeAgo(msg.when), "MatrixChunky8"),
    };
    let timestampColor = [100 / 1.3, 100 / 1.3, 145 / 1.3];
    
    // Calculate box dimensions based on per-message rowHeight
    const boxHeight = msg.tb.lines.length * msgRowHeight;
    
    // paintingElements and paintingCodes already extracted at the top of the loop
    
    msg.layout = {
      x: leftMargin,
      y,
      width: msg.tb.box.width,
      height: boxHeight, // Use our calculated height instead of tb.box.height
      timestamp,
      timestampColor,
      msgColor,
      inBox,
      handles: [], // No longer needed with color codes
      prompts: [], // No longer needed with color codes  
      urls: [], // No longer needed with color codes
      paintableMessage: colorCodedMessage,
      paintingCodes: paintingCodes.length > 0 ? paintingCodes : undefined, // Store painting codes
      paintingPreviews: paintingCodes.length > 0 ? [] : undefined, // Will store loaded previews
      youtubeVideoIds: youtubeVideoIds.length > 0 ? youtubeVideoIds : undefined, // 📺 Store YouTube video IDs
      ogUrls: ogUrls.length > 0 ? ogUrls : undefined, // 🔗 Store OG preview URLs
    };

    delete msg.lastLayout;

    // Move up for the next message using THE NEXT MESSAGE'S row height (not current)
    // This ensures proper spacing when fonts have different heights
    const nextMsg = chat.messages[i - 1];
    const nextMsgRowHeight = nextMsg ? (nextMsg.computedRowHeight ?? defaultRowHeight) : defaultRowHeight;
    y -= nextMsgRowHeight; // Use next message's height for stepping
    
    // Check against next message's row height for break condition
    if (y < topMargin - nextMsgRowHeight) {
      break; // Break if y is below top line.
    }
  }
}

function computeScrollbarHeight(api, bottomMargin) {
  return api.screen.height - bottomMargin - topMargin + 2;
}

function timeAgo(timestamp) {
  const now = new Date();
  const past = new Date(timestamp);
  const seconds = floor((now - past) / 1000);

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
    const count = floor(seconds / unit.seconds);
    if (count >= 1) {
      return `${count} ${unit.name}${count > 1 ? "s" : ""} ago`;
    }
  }
  return "just now";
}

// Build a mapping of word positions in the text.box output to their positions in the original message
function buildWordPositionMap(originalMessage, textBoxLines) {
  const wordMap = new Map();
  
  // Reconstruct the full text as it appears in the text box
  let reconstructedText = "";
  const wordPositions = [];
  
  for (let rowIndex = 0; rowIndex < textBoxLines.length; rowIndex++) {
    for (let wordIndex = 0; wordIndex < textBoxLines[rowIndex].length; wordIndex++) {
      const word = textBoxLines[rowIndex][wordIndex];
      const startPos = reconstructedText.length;
      
      wordPositions.push({
        word,
        rowIndex,
        wordIndex,
        key: `${rowIndex}-${wordIndex}`,
        reconstructedStart: startPos,
        reconstructedEnd: startPos + word.length
      });
      
      reconstructedText += word;
      
      // Add space between words (except for the last word in a row)
      if (wordIndex < textBoxLines[rowIndex].length - 1) {
        reconstructedText += " ";
      }
    }
    
    // Add space between rows (except for the last row)
    if (rowIndex < textBoxLines.length - 1) {
      reconstructedText += " ";
    }
  }
    // Debug logging (can be removed once mapping is confirmed stable)
  // console.log("🔨 Building word position map:");
  // console.log("  Original message:", originalMessage);
  // console.log("  Reconstructed text:", reconstructedText);
  // console.log("  Word positions:", wordPositions);
  
  // Now find the mapping between reconstructed text and original message
  // Handle cases where text wrapping might have changed the structure
  let originalIndex = 0;
  let reconstructedIndex = 0;
  
  for (const wordPos of wordPositions) {
    // Find where this reconstructed word appears in the original message
    let found = false;
    
    // Look for the word starting from our current position in the original
    for (let searchStart = originalIndex; searchStart <= originalMessage.length - wordPos.word.length; searchStart++) {
      const candidate = originalMessage.substr(searchStart, wordPos.word.length);
      
      if (candidate === wordPos.word) {
        // Found a match!
        wordMap.set(wordPos.key, {
          startIndex: searchStart,
          endIndex: searchStart + wordPos.word.length
        });
        
        // Debug mapping (can be removed once confirmed stable)
        // console.log(`  Mapped "${wordPos.word}" (${wordPos.key}) to ${searchStart}-${searchStart + wordPos.word.length}`);
        
        // Update our position in the original message
        originalIndex = searchStart + wordPos.word.length;
        found = true;
        break;
      }
    }
      // Debug warning (can be removed once confirmed stable)
    // if (!found) {
    //   console.log(`  Warning: Could not map word "${wordPos.word}" (${wordPos.key})`);
    // }
  }
    // Debug final result (can be removed once confirmed stable)
  // console.log("  Final word map:", Array.from(wordMap.entries()));
  return wordMap;
}

// Use shared parsing function from chat-highlighting.mjs
const parseMessageElements = parseMessageElementsShared;

// Calculate the rendered position of an interactive element in the text layout
function calculateElementPosition(element, fullMessage, textLines, text, currentRowHeight, typefaceName) {
  // Find which line(s) and character positions this element spans
  let charCount = 0;
  const linePositions = []; // Track all lines this element spans
  
  for (let lineIndex = 0; lineIndex < textLines.length; lineIndex++) {
    const lineText = textLines[lineIndex];
    const lineStart = charCount;
    const lineEnd = charCount + lineText.length;
    
    // Check if element overlaps with this line
    if (element.start < lineEnd && element.end > lineStart) {
      const startInLine = Math.max(0, element.start - lineStart);
      const endInLine = Math.min(element.end - lineStart, lineText.length);
      
      // Calculate pixel position for this line segment
      const startX = text.width(lineText.substring(0, startInLine), typefaceName);
      const elementTextInLine = lineText.substring(startInLine, endInLine);
      const width = text.width(elementTextInLine, typefaceName);
      
      linePositions.push({
        x: startX,
        y: lineIndex * currentRowHeight,
        width: width,
        height: currentRowHeight,
        lineIndex: lineIndex
      });
    }
    
    // Don't add +1 for space - text.box wraps without adding spaces
    charCount += lineText.length;
  }
  
  // Return array of line positions for multi-line elements, or single position
  return linePositions.length > 0 ? linePositions : null;
}

// Check if a click position is inside an element's bounds (handles multi-line elements)
function isClickInsideElement(clickX, clickY, elementPositions, text, typefaceName) {
  if (!elementPositions) return false;
  
  // Handle array of positions for multi-line elements
  const positions = Array.isArray(elementPositions) ? elementPositions : [elementPositions];
  
  // Get average character width for tolerance
  const charWidth = text ? text.width("M", typefaceName) : 8;
  
  return positions.some(pos => 
    clickX >= pos.x &&
    clickX <= pos.x + pos.width + charWidth && // Add one character width tolerance
    clickY >= pos.y &&
    clickY < pos.y + pos.height
  );
}

// Generate a color-coded message with dynamic hover states
function generateDynamicColorMessage(message, theme) {
  let colorCodedMessage = message.fullMessage;
  const parsedElements = parseMessageElements(message.fullMessage);
  const hoveredElements = message.layout.hoveredElements || new Set();
  
  // Sort elements by start position (reverse order for safe replacement)
  parsedElements.sort((a, b) => b.start - a.start);
  
  // Helper to get color string for ink() calls
  const getColorString = (color) => {
    if (Array.isArray(color)) {
      return color.map(c => c.toString()).join(',');
    }
    return color;
  };
  
  // Replace each element with color-coded version
  for (const element of parsedElements) {
    const elementText = message.fullMessage.substring(element.start, element.end);
    let colorCodedText = "";
    
    // Check if this element is being hovered
    const isHovered = Array.from(hoveredElements).some(hoveredEl => 
      hoveredEl.start === element.start && 
      hoveredEl.end === element.end && 
      hoveredEl.type === element.type
    );
    
    if (element.type === "log") {
      const color = isHovered ? theme.logHover : theme.log;
      colorCodedText = `\\${getColorString(color)}\\${elementText}\\${getColorString(theme.messageText)}\\`;
    } else if (element.type === "handle") {
      const color = isHovered ? theme.handleHover : theme.handle;
      colorCodedText = `\\${getColorString(color)}\\${elementText}\\${getColorString(theme.messageText)}\\`;
    } else if (element.type === "url") {
      const color = isHovered ? theme.urlHover : theme.url;
      colorCodedText = `\\${getColorString(color)}\\${elementText}\\${getColorString(theme.messageText)}\\`;
    } else if (element.type === "prompt") {
      const color = isHovered ? theme.promptHover : theme.prompt;
      colorCodedText = `\\${getColorString(color)}\\${elementText}\\${getColorString(theme.messageText)}\\`;
    } else if (element.type === "painting") {
      const color = isHovered ? theme.paintingHover : theme.painting;
      colorCodedText = `\\${getColorString(color)}\\${elementText}\\${getColorString(theme.messageText)}\\`;
    } else if (element.type === "r8dio") {
      const color = isHovered ? theme.r8dioHover : theme.r8dio;
      colorCodedText = `\\${getColorString(color)}\\${elementText}\\${getColorString(theme.messageText)}\\`;
    }
    
    // Replace the original text with the color-coded version
    colorCodedMessage = 
      colorCodedMessage.substring(0, element.start) + 
      colorCodedText + 
      colorCodedMessage.substring(element.end);
  }
  
  return colorCodedMessage;
}
// 📰 News ticker with TWO ROWS: headlines on top, activity/stats below
// Now fetches from news.aesthetic.computer API (see fetchNewsHeadlines)

function paintNewsTicker($, theme) {
  const { ink, screen, text, hud } = $;
  const tickerCharWidth = 4; // MatrixChunky8 char width
  const tickerHeight = 8;
  const rowSpacing = 2; // Gap between rows
  const rightMargin = 0; // Flush right, no margin
  
  // "News" prefix styling - uniform width with r8Dio label
  const newsPrefix = "News";
  const uniformLabelWidth = 28; // Fixed width to match both labels
  
  // Ticker dimensions - TWO ROWS
  const tickerMaxWidth = 180;
  const tickerRight = screen.width - rightMargin;
  const tickerY = 2; // Top row Y position
  const row2Y = tickerY + tickerHeight + rowSpacing; // Second row Y
  const totalTickerHeight = (tickerHeight * 2) + rowSpacing + 4; // Both rows + padding
  
  // Use dynamic news text (fetched from API or fallback)
  const displayText = newsTickerText || "Report a story";
  const activityText = newsActivityText || "news.aesthetic.computer";
  
  // Seamless loop with separator (always scroll, even for fallback text)
  const hasNews = newsHeadlines.length > 0;
  const separator = "   -   ";
  const loopText = displayText + separator;
  const loopWidth = loopText.length * tickerCharWidth;
  
  // Activity row loop (scrolls same direction as headlines, but slower)
  const activitySeparator = "   ·   ";
  const activityLoopText = activityText + activitySeparator;
  const activityLoopWidth = activityLoopText.length * tickerCharWidth;
  
  // Scroll animation (always scroll, slower for fallback text)
  const scrollSpeed = hasNews ? 0.5 : 0.25;
  const scrollOffset = (performance.now() * scrollSpeed / 16) % loopWidth;
  // Activity scrolls same direction as headlines, but slower
  const activityScrollSpeed = 0.25;
  const activityScrollOffset = (performance.now() * activityScrollSpeed / 16) % activityLoopWidth;
  
  // Calculate HUD label right edge to avoid overlap
  // HUD label starts at x=6 and has width from hud.currentLabel()
  const hudLabelOffset = 6; // Default HUD x offset
  const hudLabelWidth = hud?.currentLabel?.()?.btn?.box?.w || 0;
  const hudLabelRight = hudLabelOffset + hudLabelWidth;
  const minGapAfterHud = 10; // Minimum spacing between HUD label and News ticker
  
  // Position calculations - ensure News ticker starts after HUD label
  const scrollAreaRight = tickerRight;
  const idealScrollAreaLeft = scrollAreaRight - tickerMaxWidth;
  const idealNewsBgX = idealScrollAreaLeft - uniformLabelWidth;
  
  // Push News ticker to the right if it would overlap the HUD label
  const newsBgX = Math.max(
    hudLabelRight + minGapAfterHud, // Don't overlap HUD label
    idealNewsBgX // Original position
  );
  
  // Recalculate scroll area left edge based on actual News ticker position
  const scrollAreaLeft = newsBgX + uniformLabelWidth;
  
  // Colors from theme
  const handleColor = theme?.handle ? 
    (Array.isArray(theme.handle) ? theme.handle : [255, 150, 200]) : [255, 150, 200];
  const textColor = theme?.messageText ? 
    (Array.isArray(theme.messageText) ? theme.messageText : [200, 200, 200]) : [200, 200, 200];
  const dimTextColor = [150, 150, 160]; // Dimmer for activity row
  
  // "News" label - magenta background with white text (like aesthetic.news banner)
  const newsBgColor = newsTickerHovered ? [200, 50, 150] : [180, 40, 130]; // Bright magenta
  const newsFgColor = newsTickerHovered ? [255, 255, 255] : [255, 255, 255]; // White text
  
  // Scrolling area - darker magenta-tinted background (brighter on hover)
  const scrollBgColor = newsTickerHovered ? [50, 25, 45] : [35, 18, 32];
  
  // Calculate actual scrolling area width based on position
  const actualTickerWidth = scrollAreaRight - scrollAreaLeft;
  
  // Store bounds for click detection (entire ticker area including "News" label, both rows)
  const totalWidth = uniformLabelWidth + actualTickerWidth + 1;
  newsTickerBounds = {
    x: newsBgX,
    y: tickerY - 2,
    w: totalWidth,
    h: totalTickerHeight,
  };
  
  // Draw "News" label background - spans both rows
  ink(...newsBgColor, 230).box(newsBgX, tickerY - 2, uniformLabelWidth + 1, totalTickerHeight);
  // Center "News" text vertically in label area
  const newsTextY = tickerY + Math.floor((totalTickerHeight - tickerHeight - 4) / 2);
  const newsTextX = newsBgX + Math.floor((uniformLabelWidth - newsPrefix.length * tickerCharWidth) / 2);
  ink(...newsFgColor).write(newsPrefix, { x: newsTextX, y: newsTextY }, undefined, undefined, false, "MatrixChunky8");
  
  // Draw scrolling ticker background for both rows (use actual width)
  ink(...scrollBgColor, 200).box(scrollAreaLeft, tickerY - 2, actualTickerWidth + 1, totalTickerHeight);
  
  // Draw subtle separator line between rows
  ink(80, 50, 70, 150).box(scrollAreaLeft, row2Y - 1, actualTickerWidth, 1);
  
  // Draw hover underline indicator (shows it's clickable)
  if (newsTickerHovered) {
    ink(255, 255, 255, 180).box(newsBgX, tickerY + totalTickerHeight - 1, totalWidth, 1);
  }
  
  // Parse text for @handles to highlight (row 1)
  const handleRegex = /@[\w]+/g;
  const handles = [];
  let match;
  while ((match = handleRegex.exec(loopText)) !== null) {
    handles.push({ start: match.index, end: match.index + match[0].length, text: match[0] });
  }
  
  // ROW 1: Draw seamless looping HEADLINES with handle highlighting
  for (let copy = 0; copy < 3; copy++) {
    const baseX = scrollAreaLeft - scrollOffset + (copy * loopWidth);
    
    for (let i = 0; i < loopText.length; i++) {
      const charX = baseX + i * tickerCharWidth;
      
      // Manual clip: only draw if within scroll area bounds
      if (charX >= scrollAreaLeft && charX + tickerCharWidth <= scrollAreaRight) {
        // Check if this character is part of a handle
        const isHandle = handles.some(h => i >= h.start && i < h.end);
        const charColor = isHandle ? handleColor : textColor;
        
        ink(...charColor).write(loopText[i], { x: Math.round(charX), y: tickerY }, undefined, undefined, false, "MatrixChunky8");
      }
    }
  }
  
  // ROW 2: Draw ACTIVITY text (scrolls same direction as row 1, but slower)
  for (let copy = 0; copy < 3; copy++) {
    // Scroll left-to-right (same as row 1)
    const baseX = scrollAreaLeft - activityScrollOffset + (copy * activityLoopWidth);
    
    for (let i = 0; i < activityLoopText.length; i++) {
      const charX = baseX + i * tickerCharWidth;
      
      // Manual clip: only draw if within scroll area bounds
      if (charX >= scrollAreaLeft && charX + tickerCharWidth <= scrollAreaRight) {
        ink(...dimTextColor).write(activityLoopText[i], { x: Math.round(charX), y: row2Y }, undefined, undefined, false, "MatrixChunky8");
      }
    }
  }
}

// 📻 R8dio mini-player bar for laer-klokken (styled like News ticker)
function paintR8dioPlayer($, theme) {
  const { ink, screen, help, hud } = $;
  
  // Initialize bars if needed
  if (r8dioBars.length === 0) {
    for (let i = 0; i < R8DIO_BAR_COUNT; i++) {
      r8dioBars.push({ height: 0, targetHeight: 0 });
    }
  }
  
  const tickerCharWidth = 4; // MatrixChunky8 char width
  const tickerHeight = 8;
  const tickerPadding = 3;
  const rightMargin = 0; // Flush right, no margin
  
  // "r8Dio" prefix styling - uniform width with News label
  const r8dioPrefix = "r8Dio";
  const uniformLabelWidth = 28; // Fixed width to match both labels
  
  // Bar dimensions - match news ticker width
  const tickerMaxWidth = 180;
  const tickerRight = screen.width - rightMargin;
  const tickerY = 14; // Right below news ticker (at y=2, ~12px tall)
  
  // Calculate HUD label right edge to avoid overlap (same as news ticker)
  const hudLabelOffset = 6;
  const hudLabelWidth = hud?.currentLabel?.()?.btn?.box?.w || 0;
  const hudLabelRight = hudLabelOffset + hudLabelWidth;
  const minGapAfterHud = 10;
  
  // Position calculations
  const scrollAreaRight = tickerRight;
  const idealScrollAreaLeft = scrollAreaRight - tickerMaxWidth;
  const idealR8dioBgX = idealScrollAreaLeft - uniformLabelWidth;
  
  const r8dioBgX = Math.max(hudLabelRight + minGapAfterHud, idealR8dioBgX);
  const contentAreaLeft = r8dioBgX + uniformLabelWidth;
  const actualContentWidth = scrollAreaRight - contentAreaLeft;
  const totalWidth = uniformLabelWidth + actualContentWidth + 1;
  
  // Store bounds for click detection (entire bar)
  r8dioPlayerBounds = { x: r8dioBgX, y: tickerY - 2, w: totalWidth, h: tickerHeight + 4 };
  
  // "r8Dio" label background - dark with orange text (radio style)
  const labelBgColor = r8dioHovered ? [50, 35, 25] : [35, 25, 18]; // Dark warm brown
  const labelFgColor = r8dioHovered ? [255, 180, 80] : [255, 150, 50]; // Orange
  
  ink(...labelBgColor, 230).box(r8dioBgX, tickerY - 2, uniformLabelWidth + 1, tickerHeight + 4);
  
  // Draw "r8Dio" in orange, centered in label area
  const labelTextWidth = r8dioPrefix.length * tickerCharWidth;
  const labelX = r8dioBgX + Math.floor((uniformLabelWidth - labelTextWidth) / 2);
  ink(...labelFgColor).write("r", { x: labelX, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  ink(...labelFgColor).write("8D", { x: labelX + 4, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  ink(...labelFgColor).write("io", { x: labelX + 12, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  
  // Content area background - dark (brighter on hover)
  const contentBgColor = r8dioHovered ? [40, 30, 25] : [28, 22, 18]; // Dark warm
  ink(...contentBgColor, 200).box(contentAreaLeft, tickerY - 2, actualContentWidth + 1, tickerHeight + 4);
  
  // Separator line between News and r8Dio (subtle)
  ink(80, 60, 50, 150).box(r8dioBgX, tickerY - 3, totalWidth, 1);
  
  // Hover underline indicator (orange)
  if (r8dioHovered) {
    ink(255, 150, 50, 180).box(r8dioBgX, tickerY + tickerHeight + 1, totalWidth, 1);
  }
  
  // Play/Pause button right after label (not far right)
  const btnSize = 10;
  const btnX = contentAreaLeft + 2;
  const btnY = tickerY - 1;
  
  r8dioPlayBtnBounds = { x: btnX - 2, y: btnY - 2, w: btnSize + 4, h: btnSize + 4 };
  
  // Button background (orange tint)
  const btnBg = r8dioPlayHovered ? [80, 55, 35] : [55, 40, 25];
  ink(...btnBg).box(btnX, btnY, btnSize, btnSize);
  ink(r8dioPlayHovered ? [140, 100, 60] : [100, 70, 45]).box(btnX, btnY, btnSize, btnSize, "outline");
  
  // Play/Pause/Loading icon (orange)
  const iconColor = r8dioPlayHovered ? [255, 200, 120] : [255, 160, 80];
  const iconCenterX = btnX + btnSize / 2;
  const iconCenterY = btnY + btnSize / 2;
  
  if (r8dioLoading) {
    // Simple loading indicator (blinking dot)
    const phase = Math.floor((help?.repeat || 0) / 15) % 2;
    ink(255, 200, 100, phase ? 255 : 100).box(iconCenterX - 1, iconCenterY - 1, 3, 3);
  } else if (r8dioPlaying) {
    // Pause icon (two small bars)
    ink(...iconColor).box(iconCenterX - 3, iconCenterY - 3, 2, 6);
    ink(...iconColor).box(iconCenterX + 1, iconCenterY - 3, 2, 6);
  } else {
    // Play icon (small triangle)
    ink(...iconColor).box(iconCenterX - 2, iconCenterY - 3, 2, 6);
    ink(...iconColor).box(iconCenterX, iconCenterY - 2, 2, 4);
    ink(...iconColor).box(iconCenterX + 2, iconCenterY - 1, 1, 2);
  }
  
  // Content: mini visualizer bars + status text (after play button)
  const barAreaX = btnX + btnSize + 4;
  const barAreaWidth = Math.min(60, actualContentWidth - btnSize - 12);
  const barWidth = Math.max(1, Math.floor(barAreaWidth / R8DIO_BAR_COUNT) - 1);
  const maxBarHeight = tickerHeight;
  
  // Draw mini visualizer bars
  for (let i = 0; i < R8DIO_BAR_COUNT; i++) {
    const bar = r8dioBars[i];
    const x = barAreaX + i * (barWidth + 1);
    const height = Math.max(1, Math.floor(bar.height * maxBarHeight));
    
    if (r8dioPlaying || bar.height > 0.05) {
      // Orange gradient for visualizer
      const t = bar.height;
      const r = Math.floor(200 + t * 55);
      const g = Math.floor(100 + t * 80);
      const b = Math.floor(30 + t * 40);
      ink(r, g, b).box(x, tickerY + maxBarHeight - height, barWidth, height);
    } else {
      // Idle state: dim orange line
      ink(80, 50, 30).box(x, tickerY + maxBarHeight - 1, barWidth, 1);
    }
  }
  
  // Status text after visualizer
  const statusX = barAreaX + barAreaWidth + 4;
  const statusTextColor = theme?.messageText || [200, 200, 200];
  const statusEndX = scrollAreaRight - 2;
  
  if (r8dioError) {
    ink(255, 100, 100).write("err", { x: statusX, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  } else if (r8dioLoading) {
    ink(255, 180, 80).write("...", { x: statusX, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  } else if (r8dioPlaying) {
    // Show truncated track or "live" (orange text)
    const maxLen = Math.floor((statusEndX - statusX) / tickerCharWidth);
    const text = r8dioTrack 
      ? (r8dioTrack.length > maxLen ? r8dioTrack.substring(0, maxLen - 1) + "…" : r8dioTrack)
      : "live";
    ink(255, 180, 80).write(text, { x: statusX, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  } else {
    // Blinking "Listen Now" with > < arrows
    const blink = Math.floor((help?.repeat || 0) / 20) % 2;
    const arrowColor = blink ? [255, 180, 80] : [120, 90, 60]; // Orange blink
    const textColor = [180, 140, 100];
    ink(...arrowColor).write(">", { x: statusX, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
    ink(...textColor).write("Listen Now", { x: statusX + 6, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
    ink(...arrowColor).write("<", { x: statusX + 46, y: tickerY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // Volume slider removed for slim design - could add keyboard shortcuts later
  r8dioVolSliderBounds = null;
}