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

let input, inputBtn, handleBtn, token;
let messagesNeedLayout = true;
let tapState = null;
let inputTypefaceName; // Store the typeface name for text input

let rowHeight;
const lineGap = 1,
  topMargin = 22,
  bottomMargin = 33,
  leftMargin = 6;

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
let draftMessage = ""; // Store draft message text persistently

// 🔗 Link confirmation modal system
// { type: "prompt"|"url"|"handle"|"kidlisp"|"painting", text: string, action: function }
let linkConfirmModal = null;

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
  },
  otherChat,
  options,
) {
  rowHeight = typeface.blockHeight + 1;

  const client = otherChat || chat;
  
  // Store typeface name from options if provided
  if (options?.typeface) {
    inputTypefaceName = options.typeface;
  }

  // console.log("💬 Chat booting...");
  scroll = store["chat:scroll"] || 0; // Memoize scroll.

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
        // Send the chat message.
        client.server.send(`chat:message`, { text, token, sub: user.sub });
        notice("SENT");
      }

      // Clear text, hide cursor block, and close keyboard after sending message.
      input.text = "";
      draftMessage = ""; // Clear draft
      input.showBlink = false;
      input.mute = true;
      send({ type: "keyboard:close" });
    },
    {
      // autolock: false,
      // wrap,
      font: inputTypefaceName || "font_1",
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

  send({ type: "keyboard:soft-lock" });
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
  
  // Extract typeface name option (defaults to undefined for normal typeface)
  const typefaceName = options?.typeface;
  
  // Calculate rowHeight based on the typeface being used
  // For unifont, use 16 (standard unifont height at scale 1)
  // For default typeface, use the blockHeight + 1
  const currentRowHeight = typefaceName === "unifont" ? 17 : (typeface.blockHeight + 1);
  
  // Default theme
  const defaultTheme = {
    background: [100, 100, 145],
    lines: [90, 200, 150, 48],
    scrollbar: "pink",
    messageText: [255, 255, 255], // Changed from "white" to explicit RGB
    messageBox: [255, 32], // white with alpha for hover
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
    r8dio: [255, 0, 255], // Magenta for r8dio radio links
    r8dioHover: "yellow",
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
    const x = 42;
    const y = 10;
    const gap = 8;

    if (screen.width > 200 && piece === "chat") {
      ink(50, 50, 100)
        .write("back to prompt", { x: x + gap, y: 6 })
        .line(x, y, x - gap, y) // Base of arrow.
        .line(x - gap, y, x - gap + 3, y - 3) // Top of arrow.
        .line(x - gap, y, x - gap + 3, y + 3); // Bottom of arrow.
    }
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
      // Show funding alert when in funding mode
      const offlineMsg = [
        "Connecting" + ellipsisTicker?.text(help.repeat),
        "",
        "Chats are offline due to",
        "a large end-of-year server bill.",
        "",
        "Enter 'give' to help support AC",
        "and services will return ASAP!",
      ];
      const lineHeight = 12;
      const startY = Math.floor(screen.height / 2 - (offlineMsg.length * lineHeight) / 2);
      offlineMsg.forEach((line, i) => {
        const color = i === 0 ? "pink" : (i === 5 ? "lime" : "gray");
        ink(color).write(line, { center: "x", y: startY + i * lineHeight }, undefined, undefined, false, typefaceName);
      });
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
  if (messagesNeedLayout) {
    totalScrollHeight = computeMessagesHeight(api, client, typefaceName, currentRowHeight);
    computeMessagesLayout(api, client, typefaceName, currentRowHeight);
    chatHeight = computeScrollbarHeight(api, client);
    messagesNeedLayout = false;
  }
  // Mask off the area of renderable messages.
  mask({
    x: 0,
    y: topMargin,
    width: screen.width,
    height: screen.height - topMargin - bottomMargin,
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

    const x = leftMargin;
    const tb = message.tb; // Precomputed in `computeScrollbar` for each message.

    const layout = message.layout;
    const y = layout.y;

    // 🪧 Paint the message (no background box for cleaner appearance)
    
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
        const lineY = y + lineIdx * currentRowHeight;
        const lineStart = tempCharPos;
        const lineEnd = tempCharPos + line.length;
        
        // Check if this line contains any kidlisp content
        if (lineEnd > firstToken.start && lineStart < lastToken.end) {
          // Calculate the start and end of kidlisp content in this line
          const kidlispStartInLine = Math.max(0, firstToken.start - lineStart);
          const kidlispEndInLine = Math.min(line.length, lastToken.end - lineStart);
          
          const startX = text.width(line.substring(0, kidlispStartInLine), typefaceName);
          const kidlispText = line.substring(kidlispStartInLine, kidlispEndInLine);
          const kidlispWidth = text.width(kidlispText, typefaceName);
          
          // Draw dark background for entire kidlisp section (including spaces)
          ink(0, 0, 0, 80).box(x + startX, lineY, kidlispWidth, currentRowHeight);
        }
        
        tempCharPos += line.length;
      }
    }
    
    // Second pass: render text with syntax colors
    charPos = 0;
    for (let lineIdx = 0; lineIdx < tb.lines.length; lineIdx++) {
      const line = tb.lines[lineIdx];
      const lineY = y + lineIdx * currentRowHeight;
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
          } else if (element.type === "r8dio") {
            color = isHovered ? theme.r8dioHover : theme.r8dio;
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
        typefaceName // typeface
      );
      
      // Track width of last line (plain text, not color-coded)
      if (lineIdx === tb.lines.length - 1) {
        lastLineRenderedWidthForThisMessage = text.width(line, typefaceName);
      }
      
      charPos += line.length; // Don't add +1 - text.box wraps without adding spaces
    }

    const ago = timeAgo(message.when);
    let overTimestamp = false;

    // Show all timestamps (not just unique ones), with fading for older messages
    const tsColor = layout.timestamp.over ? theme.timestampHover : theme.timestamp;
    
    // Use MatrixChunky8 for compact timestamps tacked onto the end of messages
    const timestampWidth = text.width(ago, "MatrixChunky8");
    const timestampGap = 4; // Small gap between message and timestamp
    
    // Position timestamp right after the last line of the message
    const lastLineText = message.tb.lines[message.tb.lines.length - 1];
    const lastLineWidth = text.width(lastLineText, typefaceName);
    const timestampX = x + lastLineWidth + timestampGap;
    
    // Bottom-align timestamp with the message text by offsetting from the baseline
    // The timestamp should sit on the same baseline as the last line of text
    const timestampY = layout.timestamp.y + currentRowHeight - 10; // Align to bottom of text line (raised 2px)
    
    // Calculate fade based on message index (newer = more opaque, older = more faded)
    const messageIndex = client.messages.length - 1 - i;
    const fadeAlpha = Math.max(80, 255 - (messageIndex * 8)); // Min 80, decrease by 8 per message
    
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
    
    const previewY = message.layout.y + message.layout.height + 4; // 4px gap above image
    const previewHeight = 68; // 64px + 4px padding (top/bottom)
    
    // Only process if the PREVIEW is visible (not just the message)
    if (previewY + previewHeight < topMargin) continue; // Preview is above visible area
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
        
        // Calculate crop position in source pixels
        const cropX = Math.max(0, Math.min(maxPanX * panX, imgWidth - previewSize));
        const cropY = Math.max(0, Math.min(maxPanY * panY, imgHeight - previewSize));
        
        // Ensure crop dimensions don't exceed image bounds
        const cropW = Math.min(previewSize, imgWidth - cropX);
        const cropH = Math.min(previewSize, imgHeight - cropY);
        
        // Check if this preview is being hovered
        const isHovered = message.layout.hoveredPainting === code && 
                          message.layout.hoveredPaintingIndex === codeIdx;
        
        // Paste with crop at 1:1 scale (no width/height scale)
        paste(
          painting,
          floor(previewX),
          floor(previewY),
          { 
            crop: {
              x: floor(cropX),
              y: floor(cropY),
              w: floor(cropW),
              h: floor(cropH)
            }
          }
        );
        
        // Only draw border on hover (after unmask so border isn't clipped)
        if (isHovered) {
          const borderColor = theme.paintingHover;
          if (Array.isArray(borderColor)) {
            ink(...borderColor).box(
              previewX, 
              previewY, 
              previewSize, 
              previewSize, 
              "outline"
            );
          } else {
            ink(borderColor).box(
              previewX, 
              previewY, 
              previewSize, 
              previewSize, 
              "outline"
            );
          }
        }
        
        // Move X position for next preview (add 2px gap between paintings)
        previewX += previewSize + 2;
      } else {
        // Show loading indicator with background and animation
        const loadingW = 64; // Max preview size (no padding)
        const loadingH = 64;
        
        // Get loading progress (0-1)
        const progress = paintingLoadProgress.get(code) || 0;
        
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
        
        for (let i = 0; i < numDots; i++) {
          const angle = (help.repeat * 0.05 + (i / numDots) * Math.PI * 2);
          const dotX = Math.floor(centerX + Math.cos(angle) * dotRadius);
          const dotY = Math.floor(centerY + Math.sin(angle) * dotRadius);
          
          // Fade dots based on position in circle
          const dotAlpha = Math.floor(100 + ((i / numDots) * 155));
          ink(150, 150, 180, dotAlpha).box(dotX - 1, dotY - 1, 2, 2);
        }
        
        // Reserve space for loading preview (no gap)
        previewX += 64;
      }
    }
  }
  
  // Request continuous painting only if we have animating paintings
  if (hasAnimatingPaintings) {
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
  
  // Position and paint login/handle button with prompt.mjs styling
  // Place it at the very bottom, below the chat area
  handleBtn.reposition({ left: leftMargin + 1, bottom: 7, screen });
  
  // Draw panel background behind both buttons
  const panelHeight = 32; // Height of the button panel area
  ink(20, 20, 30, 255).box(
    0,
    screen.height - panelHeight,
    screen.width,
    panelHeight
  );
  
  // Use blue color scheme for Log in, pink/magenta for handle (like prompt.mjs)
  const loginColors = currentHandle 
    ? [[128, 0, 128], 255, 255, [128, 0, 128]] // Magenta for handle
    : [[0, 0, 128], 255, 255, [0, 0, 128]];     // Blue for "Log in"
    
  handleBtn.paint(
    { ink, wipe, paste, screen, text, typeface }, 
    loginColors
  );

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
  
  ink(textColor).write(
    enterMsg,
    {
      left: inputBtnX + 6,
      bottom: 9, // Moved up 1px
    },
    false, undefined, false, typefaceName
  );

  if (!input.canType) {
    ink(160).write("Online: " + (client?.chatterCount ?? 0), {
      right: leftMargin,
      top: 6,
    }); // Use default font for online count
  }

  if (input.canType && !leaving()) {
    // Update draft message while typing
    if (input.text !== "...") {
      draftMessage = input.text;
    }
    
    input.paint(api, false, {
      x: 0,
      y: topMargin,
      width: screen.width,
      height: screen.height / 2, // - 18
    });

    // Character limit.
    const len = 128;
    ink(input.text.length > len ? "red" : "gray").write(
      `${input.text.length}/${len}`,
      { right: 6, top: 6 }
    ); // Use default font for character count
  }
  
  // 🖼️ Render fullscreen painting modal (overlay over everything)
  if (modalPainting) {
    const { painting, code, metadata } = modalPainting;
    
    // Semi-transparent black backdrop
    ink(0, 0, 0, 200).box(0, 0, screen.width, screen.height);
    
    // Calculate centered position and scale to fit screen while maintaining aspect ratio
    const maxW = screen.width - 40; // 20px padding on each side
    const maxH = screen.height - 40;
    
    const scaleW = maxW / painting.width;
    const scaleH = maxH / painting.height;
    const scale = Math.min(scaleW, scaleH, 1); // Don't scale up, max 1:1
    
    const displayW = Math.floor(painting.width * scale);
    const displayH = Math.floor(painting.height * scale);
    
    const x = Math.floor((screen.width - displayW) / 2);
    const y = Math.floor((screen.height - displayH) / 2);
    
    // Draw painting
    if (scale === 1) {
      // 1:1 pixel perfect
      paste(painting, x, y);
    } else {
      // Scaled to fit
      paste(painting, x, y, { width: displayW, height: displayH });
    }
    
    // Draw border
    ink(255, 255, 255).box(x - 1, y - 1, displayW + 2, displayH + 2, "outline");
    
    // Draw title/code at bottom
    const titleText = metadata?.slug ? `#${code} - ${metadata.handle}` : `#${code}`;
    ink(255, 255, 255).write(titleText, { center: "x", bottom: 10 });
    
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
  
  // 🖼️ Modal painting intercepts all events
  if (modalPainting) {
    if (e.is("lift") || e.is("touch")) {
      beep();
      modalPainting = null; // Close modal on any tap/click
    }
    return; // Block all other interactions when modal is open
  }
  
  // if (e.is("viewport-height:changed")) {
  // console.log("✨ New keyboard cutoff would be:", e.y, "?");
  // notice(e.y);
  // }

  if (e.is("reframed")) {
    const lastScrollHeight = totalScrollHeight;
    const lastScroll = scroll;

    totalScrollHeight = computeMessagesHeight(api, client, typefaceName, currentRowHeight);
    computeMessagesLayout(api, client, typefaceName, currentRowHeight);
    chatHeight = computeScrollbarHeight(api);

    scroll = (lastScroll / lastScrollHeight) * totalScrollHeight;
    store["chat:scroll"] = scroll;
    boundScroll();
  }

  // TODO: ChatToDisk: The chat module will need some kind of "focus" in order
  //                   to enable scroll and such.. maybe.

  if (!input.canType) {
    // 👇 Message Picking

    if (e.is("lift")) {
      tapState = null;
    }

    // TODO: Scrolling should cancel out the tapping.

    // 👈 Tapping
    if (e.is("touch")) {
      // Detect if we are inside a message or not.
      for (let i = client.messages.length - 1; i >= 0; i--) {
        const message = client.messages[i];
        if (!message.tb || !message.layout) {
          continue; // If `tb` is not defined then kill this. 👾
          // console.log("No message layout found for:", message);
        }
        if (
          e.x > message.layout.x &&
          e.x < message.layout.x + message.layout.width &&
          e.y > message.layout.y &&
          e.y < message.layout.y + message.layout.height
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
            const elementPosition = calculateElementPosition(
              element, 
              message.fullMessage, 
              message.tb.lines, 
              text,
              currentRowHeight,
              typefaceName
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
        if (message.clicked) {
          message.clicked = false;
          
          // Parse the original message to find interactive elements
          const parsedElements = parseMessageElements(message.fullMessage);
          
          // Calculate click position relative to message
          const relativeX = e.x - message.layout.x;
          const relativeY = e.y - message.layout.y;
          
          // Check each interactive element for hit detection
          for (const element of parsedElements) {
            // Calculate the position of this element in the rendered text
            const elementPosition = calculateElementPosition(
              element, 
              message.fullMessage, 
              message.tb.lines, 
              text,
              currentRowHeight,
              typefaceName
            );
            
            if (elementPosition && isClickInsideElement(relativeX, relativeY, elementPosition)) {
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
                if (element.text === "'") continue;
                
                beep();
                const innerPrompt = element.text.slice(1, -1); // Unquote prompt text.
                let jumpTarget;
                if (innerPrompt.startsWith(">")) {
                  jumpTarget = "prompt " + innerPrompt.slice(1);
                } else if (innerPrompt.startsWith("#")) {
                  jumpTarget = "painting#" + innerPrompt.slice(1);
                } else {
                  jumpTarget = "prompt " + innerPrompt;
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
                  jumpTarget = "prompt " + innerPrompt.slice(1);
                } else if (innerPrompt.startsWith("#")) {
                  jumpTarget = "painting#" + innerPrompt.slice(1);
                } else {
                  jumpTarget = "prompt " + innerPrompt;
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
                  beep();
                  // Open painting in fullscreen modal
                  modalPainting = {
                    painting: cached.painting,
                    code: cached.code,
                    metadata: cached.metadata
                  };
                  break;
                }
                
                previewX += previewSize + 2; // Move to next preview with 2px gap
              } else {
                previewX += 64; // Reserve space for loading preview (no gap)
              }
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
            const elementPosition = calculateElementPosition(
              element, 
              message.fullMessage, 
              message.tb.lines, 
              text,
              currentRowHeight,
              typefaceName
            );
            
            if (elementPosition && isClickInsideElement(relativeX, relativeY, elementPosition, text, typefaceName)) {
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

  if (
    input.canType &&
    (e.is("keyboard:down:`") ||
      e.is("keyboard:down:escape") ||
      (input.text.trim().length === 0 &&
        e.is("keyboard:down:enter") &&
        !e.shift))
  ) {
    send({ type: "keyboard:close" });
  }

  if (input.canType && e.is("lift") && !input.shifting && !input.paste.down) {
    // Don't close if lifting over the Enter button OR if Enter button was just pressed
    const isOverEnterButton = input.enter && !input.enter.btn.disabled && input.enter.btn.box.contains(e);
    const enterButtonWasDown = input.enter && input.enter.btn._justProcessed; // Check if Enter was just pressed
    // console.log("🗨️ Chat lift handler", {
    //   canType: input.canType,
    //   isOverEnterButton,
    //   enterButtonWasDown,
    //   willSendClose: !isOverEnterButton && !enterButtonWasDown
    // });
    if (!isOverEnterButton && !enterButtonWasDown) {
      // console.log("🗨️ Chat sending keyboard:close");
      send({ type: "keyboard:close" });
    } else {
      // console.log("🗨️ Chat NOT sending keyboard:close - button handling it");
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
  
  if (!input.canType && (e.is("touch") || e.is("lift"))) {
    const containsInputBtn = inputBtn?.btn?.box?.contains(e);
    const containsEnterBtn = input.enter?.btn?.box?.contains(e);
    // console.log("🗨️ Chat checking input.act condition", {
    //   eventType: e.name,
    //   canType: input.canType,
    //   containsInputBtn,
    //   containsEnterBtn,
    //   eventCoords: { x: e.x, y: e.y },
    //   inputBtnBox: inputBtn?.btn?.box ? {
    //     x: inputBtn.btn.box.x,
    //     y: inputBtn.btn.box.y,
    //     width: inputBtn.btn.box.width,
    //     height: inputBtn.btn.box.height
    //   } : null,
    //   shouldCall: shouldCallInputAct
    // });
  }

  if (shouldCallInputAct) {
    input.act(api);
  }
}

function sim({ api }) {
  if (input) input.sim(api); // 💬 Chat
  ellipsisTicker?.update(api.clock?.time());
}

// function leave() {
// chat?.kill();
// }

export { boot, paint, act, sim };

// 📚 Library
//   (Useful functions used throughout the piece)

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

function computeMessagesHeight({ text, screen }, chat, typefaceName, currentRowHeight) {
  let height = 0;
  // Iterate through the messages array.
  for (let i = 0; i < chat.messages.length; i += 1) {
    const message = chat.messages[i];
    // Add count multiplier if message was repeated
    const countSuffix = message.count > 1 ? ` x${message.count}` : "";
    const fullMessage = message.from + " " + message.text + countSuffix;
    const tb = text.box(
      fullMessage,
      { x: leftMargin, y: 0 },
      screen.width - leftMargin,
      1,
      true,
      typefaceName
    );
    message.tb = tb;
    message.fullMessage = fullMessage;
    // Add height for all lines in the message
    // Each line is currentRowHeight tall
    // Plus add lineGap between lines within the message and after the message
    height += tb.lines.length * currentRowHeight + lineGap;
    
    // 🎨 Add space for painting previews (if any)
    const paintingElements = parseMessageElements(fullMessage).filter(el => el.type === "painting");
    if (paintingElements.length > 0) {
      // 4px gap above + 64px image + 4px padding + 2px gap below = 74px
      const previewHeight = 74;
      height += previewHeight;
    }
  }
  return height;
}

// Build a display graph for the messages.
// (From the bottom to the top.)
function computeMessagesLayout({ screen, text }, chat, typefaceName, currentRowHeight) {
  let y = screen.height - currentRowHeight - bottomMargin + scroll;

  // Delete all layouts.
  for (let i = chat.messages.length - 1; i >= 0; i--) {
    const msg = chat.messages[i];
    msg.lastLayout = msg.layout;
    delete msg.layout;
  }

  for (let i = chat.messages.length - 1; i >= 0; i--) {
    const msg = chat.messages[i];
    
    // Parse elements first to know if we have painting previews
    const parsedElements = parseMessageElements(msg.fullMessage);
    const paintingElements = parsedElements.filter(el => el.type === "painting");
    const paintingCodes = paintingElements.map(el => el.text.replace(/^#/, ''));
    
    // 🎨 Move up for painting previews FIRST (before positioning this message)
    if (paintingCodes.length > 0) {
      // 4px gap above + 64px image + 4px padding + 2px gap below = 74px
      const previewHeight = 74;
      y -= previewHeight;
    }

    y -= currentRowHeight * (msg.tb.lines.length - 1) + lineGap;    if (y > screen.height - bottomMargin) {
      y -= currentRowHeight;
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
      
      if (element.type === "handle") {
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

    // Create layout using the color-coded message
    const lastLineWidth = text.width(msg.tb.lines[msg.tb.lines.length - 1], typefaceName);
    const timestampGap = 4; // Small gap for MatrixChunky8 timestamps
    const timestamp = {
      x: lastLineWidth + timestampGap,
      y: y + (msg.tb.lines.length - 1) * currentRowHeight,
      height: 8, // MatrixChunky8 height
      width: text.width(timeAgo(msg.when), "MatrixChunky8"),
    };
    let timestampColor = [100 / 1.3, 100 / 1.3, 145 / 1.3];
    
    // Calculate box dimensions based on our rowHeight
    const boxHeight = msg.tb.lines.length * currentRowHeight;
    
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
    };

    delete msg.lastLayout;

    y -= currentRowHeight; // Move up one line for the next message.
    
    if (y < topMargin - currentRowHeight) {
      break; // Break if y is below top line.
    }
  }
}

function computeScrollbarHeight(api) {
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
    
    if (element.type === "handle") {
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
