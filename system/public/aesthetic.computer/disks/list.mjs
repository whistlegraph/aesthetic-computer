// List, 2024.1.30.13.18.29.955
// A comprehensive directory of pieces and prompt commands.
// Redesigned 2025.12.31 with categories, tabs, and responsive layout.
// Updated 2026.01.28 with Brushes category and multi-font support.

const { keys, entries } = Object;
const { min, max, floor } = Math;

// 🔤 Font configurations for different display densities
const COMPACT_FONT = "MatrixChunky8"; // 4px char width, great for metadata
const COMPACT_CHAR_WIDTH = 4;
const COMPACT_ROW_HEIGHT = 9;

// 🎨 Color schemes for dark/light mode
const scheme = {
  dark: {
    background: [16, 16, 24],
    headerBg: [24, 24, 36],
    categoryHeader: [140, 160, 220],
    categoryHeaderBg: [32, 32, 48],
    categoryCount: [80, 80, 100],
    pieceName: [100, 220, 150],
    commandName: [255, 200, 100],
    description: [100, 100, 120],
    tabActive: [100, 200, 255],
    tabInactive: [70, 70, 90],
    tabBg: [28, 28, 40],
    scrollbar: [60, 60, 80],
    scrollbarThumb: [100, 120, 180],
    highlight: [255, 255, 100],
    chevron: [80, 100, 140],
    dateText: [80, 100, 140],     // muted date color
    brushCategory: [255, 180, 100], // 🖌️ warm gold for brushes category
    // Per-tab colors
    tabPopular: [255, 120, 80],    // 🔥 orange/red
    tabAll: [100, 180, 255],       // blue
    tabPieces: [100, 220, 150],    // green (matches pieceName)
    tabCommands: [255, 200, 100],  // yellow (matches commandName)
    tabUser: [200, 140, 255],      // @ purple for user pieces
  },
  light: {
    background: [240, 240, 245],
    headerBg: [230, 230, 238],
    categoryHeader: [60, 80, 140],
    categoryHeaderBg: [220, 220, 230],
    categoryCount: [140, 140, 160],
    pieceName: [20, 120, 60],
    commandName: [160, 100, 20],
    description: [100, 100, 110],
    tabActive: [40, 100, 180],
    tabInactive: [140, 140, 160],
    tabBg: [225, 225, 235],
    scrollbar: [200, 200, 210],
    scrollbarThumb: [140, 150, 180],
    highlight: [255, 220, 80],
    chevron: [120, 140, 180],
    dateText: [130, 140, 160],    // muted date color
    brushCategory: [200, 120, 40], // 🖌️ warm gold for brushes category
    // Per-tab colors
    tabPopular: [220, 80, 40],     // 🔥 orange/red
    tabAll: [40, 100, 180],        // blue
    tabPieces: [20, 120, 60],      // green (matches pieceName)
    tabCommands: [160, 100, 20],   // yellow (matches commandName)
    tabUser: [140, 80, 200],       // @ purple for user pieces
  },
};

// 📁 Category definitions for pieces
const PIECE_CATEGORIES = {
  "🖌️ Brushes": ["line", "rect", "oval", "box", "circle", "shape", "poly", "smear", "blur", "spray", "fill", "wipe", "marker", "sparkle", "pull", "bits", "vary"],
  "🎨 Creative": ["colors", "nopaint", "painting", "plot", "drawings", "doodle", "crop", "download", "pixel", "handprint", "icon", "signature", "brush"],
  "🎵 Audio": ["clock", "tone", "bleep", "chord", "synth", "bubble", "bgm", "colplay", "metronome", "notepat", "sing", "whistlegraph"],
  "📹 Media": ["tape", "video", "camera", "selfie", "cam", "camu", "recorder", "mic", "snap", "cap"],
  "💬 Social": ["chat", "mood", "moods", "field", "scream", "handle", "profile", "me"],
  "🤖 AI Chat": ["sotce", "bf", "gf", "bro", "sis", "dad", "mom", "angel", "gargoyle", "boyfriend", "girlfriend", "brother", "sister"],
  "🎮 Games": ["pong", "snake", "balls", "brick-breaker", "gostop", "spin", "wand", "wiggle"],
  "💼 System": ["prompt", "list", "about", "help", "docs", "api", "debug", "404", "blank"],
  "💰 Wallet": ["wallet", "tezos", "keep", "kept", "mint", "ff", "freaky-flowers"],
  "🌍 World": ["world", "fly", "field", "handtime", "vr"],
  "🔧 Utilities": ["encode", "decode", "baktok", "qr", "resize", "preview"],
};

// 📁 Category definitions for commands (prompts)
const COMMAND_CATEGORIES = {
  "📼 Recording": ["tape", "cut", "tapem"],
  "💬 Social": ["mood", "scream", "nonotifs", "notifs", "channel"],
  "📷 Camera": ["selfie", "cam", "camu"],
  "🎨 Painting": ["print", "mint", "done", "yes!", "p", "pain", "painting:start", "painting:done"],
  "💰 Wallet": ["tezos", "keep"],
  "👤 Account": ["me", "handle", "delete-erase-and-forget-me"],
  "🤖 Companions": ["bro", "sis", "gf", "bf", "bb"],
  "🔗 Links": ["github", "gh", "gmail", "agc", "ucla-syllabus"],
};

// State
let docs = null;
let pieces = {};
let commands = {};
let userPieces = {};     // User-created pieces from MongoDB
let hitStats = {};       // { pieceName: hitCount }
let pieceDates = {};     // { pieceName: { created: Date, author: string } }
let currentTab = "all"; // "all" | "pieces" | "commands" | "popular" | "user"
let expandedCategories = new Set();
let scroll = 0;
let selectedItem = null;
let anyDown = false;
let layoutMode = "large"; // "tiny" | "small" | "medium" | "large"

// 🔗 Confirmation modal for navigation
// { name: string, type: "piece"|"command", desc: string, yesBtn, noBtn, hoverYes, hoverNo }
let confirmModal = null;

// UI elements
let tabButtons = [];
let itemButtons = [];
let categoryButtons = [];

// Cached data
let allItems = [];      // Flat list for "all" tab
let pieceItems = [];    // Categorized pieces
let commandItems = [];  // Categorized commands
let popularItems = [];  // Popular pieces sorted by hits
let userItems = [];     // User-created pieces
let visibleItems = [];  // Currently visible items based on tab + expanded categories

// Layout constants (adjusted per mode)
let ROW_HEIGHT = 14;
let HEADER_HEIGHT = 18;
let TAB_HEIGHT = 14;
let LEFT_MARGIN = 6;
let TAB_OFFSET = 32;     // Offset tabs from left to avoid HUD label overlap
let CONTENT_TOP = 36;

// 🥾 Boot
async function boot({ ui, net, store, params, screen }) {
  // Determine layout mode
  updateLayoutMode(screen);
  
  // Fetch docs, hit stats, and user pieces in parallel
  if (!params[0]) {
    const [docsResult, hitsResult, userResult, datesResult] = await Promise.all([
      net.requestDocs(),
      fetch("/api/piece-hit?top=100").then(r => r.json()).catch(() => ({ pieces: [] })),
      fetch("/api/kidlisp-list?limit=200").then(r => r.json()).catch(() => ({ pieces: [] })),
      fetch("/api/piece-dates").then(r => r.json()).catch(() => ({ dates: {} })),
    ]);
    
    docs = docsResult;
    
    // Build hit stats map
    hitStats = {};
    if (hitsResult?.pieces) {
      hitsResult.pieces.forEach(p => {
        hitStats[p.piece] = p.hits;
      });
    }
    
    // Build user pieces map
    userPieces = {};
    if (userResult?.pieces) {
      userResult.pieces.forEach(p => {
        const key = p.handle ? `@${p.handle}/${p.code}` : p.code;
        userPieces[key] = { 
          desc: p.title || "", 
          code: p.code,
          handle: p.handle,
          when: p.when,
        };
      });
    }
    
    // Build piece dates map
    pieceDates = datesResult?.dates || {};
    
    if (docs) {
      // Separate pieces and commands, filter hidden
      pieces = {};
      commands = {};
      
      if (docs.pieces) {
        keys(docs.pieces).forEach(key => {
          if (!docs.pieces[key].hidden) pieces[key] = docs.pieces[key];
        });
      }
      if (docs.prompts) {
        keys(docs.prompts).forEach(key => {
          if (!docs.prompts[key].hidden) commands[key] = docs.prompts[key];
        });
      }
      
      // Build categorized lists
      buildCategorizedLists(ui);
      
      // Build flat "all" list
      buildAllList(ui);
      
      // Build popular list
      buildPopularList(ui);
      
      // Build user list
      buildUserList(ui);
      
      // Restore state
      currentTab = (await store.retrieve("list:tab")) || "all";
      scroll = (await store.retrieve("list:scroll:" + currentTab)) || 0;
      const savedExpanded = await store.retrieve("list:expanded");
      if (savedExpanded) expandedCategories = new Set(savedExpanded);
      
      // Build visible items
      rebuildVisibleItems(ui);
    }
  } else {
    // Handle-specific pieces (legacy behavior)
    try {
      const response = await fetch(`/media/${params[0]}/pieces`);
      const json = await response.json();
      pieces = json.files.reduce((acc, url) => {
        const parsedUrl = new URL(url);
        const pathSegments = parsedUrl.pathname.split("/").filter(Boolean);
        const key = `${pathSegments[1]}/${pathSegments[3].replace(".mjs", "")}`;
        acc[key] = { desc: "" };
        return acc;
      }, {});
      buildAllList(ui);
      rebuildVisibleItems(ui);
    } catch (err) {
      console.error("Error fetching pieces:", err);
    }
  }
}

// Build categorized piece list
function buildCategorizedLists(ui) {
  pieceItems = [];
  commandItems = [];
  
  // Track which pieces/commands are categorized
  const categorizedPieces = new Set();
  const categorizedCommands = new Set();
  
  // Build piece categories
  entries(PIECE_CATEGORIES).forEach(([category, names]) => {
    const items = names.filter(name => pieces[name]).map(name => ({
      name,
      data: pieces[name],
      type: "piece",
    }));
    if (items.length > 0) {
      pieceItems.push({ type: "category", name: category, count: items.length });
      items.forEach(item => {
        pieceItems.push(item);
        categorizedPieces.add(item.name);
      });
    }
  });
  
  // Add uncategorized pieces
  const uncategorizedPieces = keys(pieces).filter(k => !categorizedPieces.has(k)).sort();
  if (uncategorizedPieces.length > 0) {
    pieceItems.push({ type: "category", name: "📦 Other", count: uncategorizedPieces.length });
    uncategorizedPieces.forEach(name => {
      pieceItems.push({ name, data: pieces[name], type: "piece" });
    });
  }
  
  // Build command categories
  entries(COMMAND_CATEGORIES).forEach(([category, names]) => {
    const items = names.filter(name => commands[name]).map(name => ({
      name,
      data: commands[name],
      type: "command",
    }));
    if (items.length > 0) {
      commandItems.push({ type: "category", name: category, count: items.length });
      items.forEach(item => {
        commandItems.push(item);
        categorizedCommands.add(item.name);
      });
    }
  });
  
  // Add uncategorized commands
  const uncategorizedCommands = keys(commands).filter(k => !categorizedCommands.has(k)).sort();
  if (uncategorizedCommands.length > 0) {
    commandItems.push({ type: "category", name: "📋 Other", count: uncategorizedCommands.length });
    uncategorizedCommands.forEach(name => {
      commandItems.push({ name, data: commands[name], type: "command" });
    });
  }
}

// Build flat alphabetical list for "all" tab
function buildAllList(ui) {
  const combined = {
    ...Object.fromEntries(keys(pieces).map(k => [k, { ...pieces[k], _type: "piece" }])),
    ...Object.fromEntries(keys(commands).map(k => [k, { ...commands[k], _type: "command" }])),
  };
  
  allItems = keys(combined).sort().map(name => ({
    name,
    data: combined[name],
    type: combined[name]._type,
  }));
}

// Build popular list sorted by hit count (Top 100 style)
// Includes both documented pieces AND tracked pieces from hit stats
function buildPopularList(ui) {
  // Start with documented pieces and commands
  const combined = {
    ...Object.fromEntries(keys(pieces).map(k => [k, { ...pieces[k], _type: "piece" }])),
    ...Object.fromEntries(keys(commands).map(k => [k, { ...commands[k], _type: "command" }])),
  };
  
  // Also add any pieces from hit stats that aren't in docs (like KidLisp $xxx pieces)
  keys(hitStats).forEach(name => {
    if (!combined[name] && !name.startsWith("api/")) { // Skip API endpoints
      combined[name] = { desc: "", _type: "piece" };
    }
  });
  
  // Sort by hits (descending), then alphabetically for ties
  // Add rank number for Top 100 style display
  popularItems = keys(combined)
    .map(name => ({
      name,
      data: combined[name],
      type: combined[name]._type,
      hits: hitStats[name] || 0,
    }))
    .filter(item => item.hits > 0) // Only show items with actual hits
    .sort((a, b) => {
      if (b.hits !== a.hits) return b.hits - a.hits;
      return a.name.localeCompare(b.name);
    })
    .slice(0, 100) // Top 100 only
    .map((item, index) => ({ ...item, rank: index + 1 })); // Add rank
}

// Build user pieces list sorted by creation date
function buildUserList(ui) {
  userItems = keys(userPieces)
    .map(name => ({
      name,
      data: userPieces[name],
      type: "user",
      when: userPieces[name].when ? new Date(userPieces[name].when) : null,
    }))
    .sort((a, b) => {
      // Sort by date descending (newest first), null dates last
      if (!a.when && !b.when) return a.name.localeCompare(b.name);
      if (!a.when) return 1;
      if (!b.when) return -1;
      return b.when - a.when;
    });
}

// Rebuild visible items based on current tab and expanded categories
function rebuildVisibleItems(ui) {
  visibleItems = [];
  itemButtons = [];
  categoryButtons = [];
  
  let sourceItems;
  if (currentTab === "all" || layoutMode === "tiny") {
    // Flat list, no categories
    sourceItems = allItems;
    visibleItems = sourceItems.map((item, i) => ({ ...item, y: CONTENT_TOP + i * ROW_HEIGHT }));
  } else if (currentTab === "popular") {
    // Popular list sorted by hits (flat, no categories)
    sourceItems = popularItems;
    visibleItems = sourceItems.map((item, i) => ({ ...item, y: CONTENT_TOP + i * ROW_HEIGHT }));
  } else if (currentTab === "user") {
    // User pieces sorted by date (flat, no categories)
    sourceItems = userItems;
    visibleItems = sourceItems.map((item, i) => ({ ...item, y: CONTENT_TOP + i * ROW_HEIGHT }));
  } else {
    // Categorized list (pieces or commands)
    sourceItems = currentTab === "pieces" ? pieceItems : commandItems;
    let y = CONTENT_TOP;
    let currentCategory = null;
    let skipUntilNextCategory = false;
    
    sourceItems.forEach((item, i) => {
      if (item.type === "category") {
        currentCategory = item.name;
        skipUntilNextCategory = !expandedCategories.has(currentCategory);
        visibleItems.push({ ...item, y, expanded: !skipUntilNextCategory });
        categoryButtons.push({
          name: currentCategory,
          y,
          height: ROW_HEIGHT,
        });
        y += ROW_HEIGHT;
      } else if (!skipUntilNextCategory) {
        visibleItems.push({ ...item, y });
        y += ROW_HEIGHT;
      }
    });
  }
  
  // Create buttons for items
  visibleItems.forEach((item, i) => {
    if (item.type !== "category") {
      const nameWidth = item.name.length * 6;
      itemButtons.push({
        index: i,
        name: item.name,
        box: { x: LEFT_MARGIN, y: item.y, w: nameWidth, h: ROW_HEIGHT },
        down: false,
      });
    }
  });
}

// Update layout mode based on screen size
function updateLayoutMode(screen) {
  const w = screen.width;
  if (w < 128) layoutMode = "tiny";
  else if (w < 192) layoutMode = "small";
  else if (w < 320) layoutMode = "medium";
  else layoutMode = "large";
  
  // Adjust constants based on mode
  ROW_HEIGHT = { tiny: 10, small: 12, medium: 13, large: 14 }[layoutMode];
  HEADER_HEIGHT = layoutMode === "tiny" ? 12 : 18;
  TAB_HEIGHT = layoutMode === "tiny" ? 0 : (layoutMode === "small" ? 12 : 14);
  CONTENT_TOP = HEADER_HEIGHT + TAB_HEIGHT + 4;
}

// Get max description length based on mode and screen width
function getMaxDescLength(screen) {
  if (layoutMode === "tiny") return 0;
  const availableWidth = screen.width - LEFT_MARGIN - 60; // Space after name
  return max(0, floor(availableWidth / 6));
}

// Truncate text to fit
function truncate(text, maxLen) {
  if (!text || maxLen <= 0) return "";
  if (text.length <= maxLen) return text;
  return text.slice(0, maxLen - 2) + "..";
}

// Format date to short string (e.g., "2024-12-31" or "6d ago")
function formatShortDate(date) {
  if (!date) return "";
  const d = new Date(date);
  const now = new Date();
  const diffMs = now - d;
  const diffDays = Math.floor(diffMs / (1000 * 60 * 60 * 24));
  
  // If within last 30 days, show relative
  if (diffDays === 0) return "today";
  if (diffDays === 1) return "1d ago";
  if (diffDays < 30) return `${diffDays}d ago`;
  
  // Otherwise show date
  const year = d.getFullYear();
  const month = String(d.getMonth() + 1).padStart(2, '0');
  const day = String(d.getDate()).padStart(2, '0');
  return `${year}-${month}-${day}`;
}

// 🎨 Paint
function paint({ wipe, ink, screen, dark, paintCount }) {
  const pal = dark ? scheme.dark : scheme.light;
  updateLayoutMode(screen);
  
  wipe(pal.background);
  
  // Loading state
  if (!docs && keys(pieces).length === 0) {
    if (paintCount > 8n) {
      ink(pal.pieceName).write("Fetching...", { center: "xy" });
    }
    return;
  }
  
  // Empty state
  if (visibleItems.length === 0) {
    ink(pal.description).write("No items", { center: "xy" });
    return;
  }
  
  // Content area - draw FIRST so header/tabs mask it
  const maxDescLen = getMaxDescLength(screen);
  const contentBottom = screen.height - 2;
  
  visibleItems.forEach((item, i) => {
    const y = item.y + scroll;
    
    // Skip if off-screen (including header/tab area)
    if (y < CONTENT_TOP || y > contentBottom) return;
    
    if (item.type === "category") {
      // Category header
      if (layoutMode !== "tiny") {
        ink(pal.categoryHeaderBg).box(0, y, screen.width, ROW_HEIGHT);
        
        // Chevron
        const chevron = item.expanded ? "▼" : "▸";
        ink(pal.chevron).write(chevron, { x: LEFT_MARGIN, y: y + 1 });
        
        // Category name (special color for Brushes)
        const isBrushes = item.name.includes("Brushes");
        const catColor = isBrushes ? pal.brushCategory : pal.categoryHeader;
        ink(catColor).write(item.name, { x: LEFT_MARGIN + 10, y: y + 1 });
        
        // Count (using compact font, right-aligned)
        const countStr = `${item.count}`;
        const countWidth = countStr.length * COMPACT_CHAR_WIDTH;
        ink(pal.categoryCount).write(countStr, { 
          x: screen.width - countWidth - 6, 
          y: y + 2 
        }, undefined, undefined, false, COMPACT_FONT);
      }
    } else {
      // Item row
      const btn = itemButtons.find(b => b.name === item.name);
      const isHighlighted = btn?.down || selectedItem === item.name;
      
      // Highlight background
      if (isHighlighted) {
        ink(pal.highlight, 60).box(0, y, screen.width, ROW_HEIGHT);
      }
      
      // Item name (colored by type)
      let nameColor;
      if (item.type === "user") nameColor = pal.tabUser;
      else if (item.type === "piece") nameColor = pal.pieceName;
      else nameColor = pal.commandName;
      
      // In Top 100 tab, show rank number prefix
      let nameX = LEFT_MARGIN;
      if (currentTab === "popular" && item.rank && layoutMode !== "tiny") {
        // Draw rank number (e.g., #1, #12, #100)
        const rankText = `#${item.rank}`;
        const rankWidth = rankText.length * COMPACT_CHAR_WIDTH;
        ink(pal.tabPopular).write(rankText, { x: LEFT_MARGIN, y: y + 2 }, undefined, undefined, false, COMPACT_FONT);
        nameX = LEFT_MARGIN + rankWidth + 4; // Offset name after rank
      }
      
      const indent = layoutMode === "tiny" ? 0 : (currentTab === "all" || currentTab === "user" ? 0 : (currentTab === "popular" ? 0 : 8));
      ink(isHighlighted ? pal.highlight : nameColor).write(item.name, { 
        x: nameX + indent, 
        y: y + 1 
      });
      
      // Show hit count in Top 100 tab (use compact font for metadata, right-aligned)
      if (currentTab === "popular" && item.hits > 0 && layoutMode !== "tiny") {
        const hitText = item.hits >= 1000 ? `${(item.hits / 1000).toFixed(1)}k` : `${item.hits}`;
        // Position at right edge with compact font
        const hitWidth = hitText.length * COMPACT_CHAR_WIDTH;
        const hitX = screen.width - hitWidth - 6;
        ink(pal.categoryCount).write(hitText, { x: hitX, y: y + 2 }, undefined, undefined, false, COMPACT_FONT);
      }
      // Show date in user tab (use compact font for metadata)
      else if (currentTab === "user" && item.when && layoutMode !== "tiny") {
        const dateStr = formatShortDate(item.when);
        // Position at right edge with compact font
        const dateWidth = dateStr.length * COMPACT_CHAR_WIDTH;
        const dateX = screen.width - dateWidth - 6;
        ink(pal.dateText).write(dateStr, { x: dateX, y: y + 2 }, undefined, undefined, false, COMPACT_FONT);
      }
      // Description (if space allows and not in popular/user tab)
      else if (maxDescLen > 0 && item.data?.desc && currentTab !== "popular" && currentTab !== "user") {
        const descX = LEFT_MARGIN + item.name.length * 6 + 8 + indent;
        const remainingSpace = floor((screen.width - descX - 4) / 6);
        const desc = truncate(item.data.desc, min(maxDescLen, remainingSpace));
        ink(pal.description).write(desc, { x: descX, y: y + 1 });
      }
    }
  });
  
  // Scrollbar (if needed)
  const totalHeight = visibleItems.length * ROW_HEIGHT;
  const viewHeight = screen.height - CONTENT_TOP;
  if (totalHeight > viewHeight) {
    const scrollbarHeight = max(10, (viewHeight / totalHeight) * viewHeight);
    const scrollbarY = CONTENT_TOP + (-scroll / totalHeight) * viewHeight;
    
    ink(pal.scrollbar).box(screen.width - 3, CONTENT_TOP, 2, viewHeight);
    ink(pal.scrollbarThumb).box(screen.width - 3, scrollbarY, 2, scrollbarHeight);
  }
  
  // === HEADER & TABS (drawn LAST to mask scrolling content) ===
  
  // Header background (masks content)
  ink(pal.headerBg).box(0, 0, screen.width, HEADER_HEIGHT);
  
  // Item count in header
  const countText = `${visibleItems.filter(i => i.type !== "category").length}`;
  if (layoutMode !== "tiny") {
    ink(pal.categoryCount).write(countText, { x: screen.width - countText.length * 6 - 4, y: 4 });
  }
  
  // Tabs (not in tiny mode)
  if (layoutMode !== "tiny") {
    const tabY = HEADER_HEIGHT + 1;
    
    // Tab bar background (masks content)
    ink(pal.tabBg).box(0, tabY - 1, screen.width, TAB_HEIGHT + 2);
    
    // Tab color map
    const tabColors = {
      popular: pal.tabPopular,
      all: pal.tabAll,
      pieces: pal.tabPieces,
      commands: pal.tabCommands,
      user: pal.tabUser,
    };
    
    const tabs = layoutMode === "small" 
      ? [["popular", "Top"], ["all", "All"], ["pieces", "📦"], ["commands", "🎯"], ["user", "@"]]
      : [["popular", "Top 100"], ["all", "All"], ["pieces", "Pieces"], ["commands", "Cmds"], ["user", "@User"]];
    
    // Start tabs offset from left to avoid HUD label overlap
    let tabX = TAB_OFFSET;
    tabButtons = []; // Reset
    tabs.forEach(([id, label]) => {
      const isActive = currentTab === id;
      const tabWidth = label.length * 6 + 8;
      const tabColor = tabColors[id] || pal.tabActive;
      
      if (isActive) {
        // Active tab: colored background, dark text
        ink(tabColor).box(tabX - 2, tabY, tabWidth, TAB_HEIGHT - 2);
        ink(pal.background).write(label, { x: tabX + 2, y: tabY + 2 });
      } else {
        // Inactive tab: colored text, no background
        ink(tabColor, 180).write(label, { x: tabX + 2, y: tabY + 2 });
      }
      
      tabButtons.push({ id, x: tabX - 2, y: tabY, w: tabWidth, h: TAB_HEIGHT });
      tabX += tabWidth + 6;
    });
  }
  
  // === CONFIRMATION MODAL (overlay over everything) ===
  if (confirmModal) {
    const { name, type, desc } = confirmModal;
    
    // Semi-transparent dark backdrop
    ink(12, 12, 20, 220).box(0, 0, screen.width, screen.height);
    
    // Calculate modal dimensions
    const hasDesc = desc && desc.length > 0;
    const modalW = min(screen.width - 16, 160);
    const modalH = hasDesc ? 68 : 54;
    const modalX = floor((screen.width - modalW) / 2);
    const modalY = floor((screen.height - modalH) / 2);
    
    // Modal background
    ink(pal.headerBg).box(modalX, modalY, modalW, modalH);
    ink(pal.categoryHeader).box(modalX, modalY, modalW, modalH, "outline");
    ink(pal.categoryHeader, 120).box(modalX + 1, modalY, modalW - 2, 1); // Top highlight
    
    // Action label
    const actionLabel = type === "user" ? "Open user piece?" : (type === "piece" ? "Open piece?" : "Run command?");
    let labelColor;
    if (type === "user") labelColor = pal.tabUser;
    else if (type === "piece") labelColor = pal.pieceName;
    else labelColor = pal.commandName;
    ink(labelColor).write(actionLabel, { x: modalX + 6, y: modalY + 4 });
    
    // Name
    const maxNameLen = floor((modalW - 12) / 6);
    const truncName = name.length > maxNameLen ? name.slice(0, maxNameLen - 1) + "…" : name;
    ink(255, 255, 255).write(truncName, { x: modalX + 6, y: modalY + 16 });
    
    // Description (if available)
    if (hasDesc) {
      const maxDescLen = floor((modalW - 12) / 6);
      const truncDesc = desc.length > maxDescLen ? desc.slice(0, maxDescLen - 1) + "…" : desc;
      ink(pal.description).write(truncDesc, { x: modalX + 6, y: modalY + 28 });
    }
    
    // Buttons
    const btnW = 32;
    const btnH = 14;
    const btnY = modalY + modalH - btnH - 6;
    const btnGap = 10;
    const totalBtnW = btnW * 2 + btnGap;
    const btnStartX = modalX + floor((modalW - totalBtnW) / 2);
    
    // Store button positions for hit detection
    confirmModal.yesBtn = { x: btnStartX, y: btnY, w: btnW, h: btnH };
    confirmModal.noBtn = { x: btnStartX + btnW + btnGap, y: btnY, w: btnW, h: btnH };
    
    // Yes button - green theme
    const yesHover = confirmModal.hoverYes;
    ink(yesHover ? [60, 140, 60] : [40, 90, 40]).box(btnStartX, btnY, btnW, btnH);
    ink(yesHover ? [100, 200, 100] : [70, 130, 70]).box(btnStartX, btnY, btnW, btnH, "outline");
    ink(yesHover ? [180, 255, 180] : [150, 220, 150]).write("yes", { x: btnStartX + 7, y: btnY + 3 });
    
    // No button - red theme
    const noHover = confirmModal.hoverNo;
    ink(noHover ? [140, 50, 50] : [90, 35, 35]).box(btnStartX + btnW + btnGap, btnY, btnW, btnH);
    ink(noHover ? [200, 90, 90] : [130, 60, 60]).box(btnStartX + btnW + btnGap, btnY, btnW, btnH, "outline");
    ink(noHover ? [255, 180, 180] : [220, 150, 150]).write("no", { x: btnStartX + btnW + btnGap + 9, y: btnY + 3 });
  }
}

// 🎪 Act
function act({ event: e, screen, hud, piece, jump, needsPaint, geo, beep }) {
  updateLayoutMode(screen);
  
  // === CONFIRMATION MODAL intercepts all events ===
  if (confirmModal) {
    const { yesBtn, noBtn, name } = confirmModal;
    
    // Handle hover states
    if (e.is("move") || e.is("draw")) {
      if (yesBtn && noBtn) {
        confirmModal.hoverYes = e.x >= yesBtn.x && e.x < yesBtn.x + yesBtn.w &&
                                 e.y >= yesBtn.y && e.y < yesBtn.y + yesBtn.h;
        confirmModal.hoverNo = e.x >= noBtn.x && e.x < noBtn.x + noBtn.w &&
                                e.y >= noBtn.y && e.y < noBtn.y + noBtn.h;
        needsPaint();
      }
    }
    
    // Handle clicks
    if (e.is("lift") || e.is("touch")) {
      if (yesBtn && noBtn) {
        const clickedYes = e.x >= yesBtn.x && e.x < yesBtn.x + yesBtn.w &&
                           e.y >= yesBtn.y && e.y < yesBtn.y + yesBtn.h;
        const clickedNo = e.x >= noBtn.x && e.x < noBtn.x + noBtn.w &&
                          e.y >= noBtn.y && e.y < noBtn.y + noBtn.h;
        
        if (clickedYes) {
          beep?.();
          hud.label(piece);
          jump("prompt~" + name);
          confirmModal = null;
        } else if (clickedNo || true) {
          // Clicking No or anywhere else closes modal
          beep?.();
          confirmModal = null;
          hud.label(piece);
          needsPaint();
        }
      }
    }
    
    // Escape key closes modal
    if (e.is("keyboard:down:escape")) {
      beep?.();
      confirmModal = null;
      hud.label(piece);
      needsPaint();
    }
    
    return; // Block all other interactions when modal is open
  }
  
  // Scroll handling
  if (e.is("scroll")) {
    scroll -= e.y;
    clampScroll(screen);
    needsPaint();
    return;
  }
  
  // Drag scrolling
  if (!anyDown && e.is("draw:1")) {
    scroll += e.delta.y;
    clampScroll(screen);
    needsPaint();
  }
  
  // Tab clicks
  if (e.is("touch") && layoutMode !== "tiny") {
    tabButtons.forEach(tab => {
      if (e.x >= tab.x && e.x <= tab.x + tab.w && 
          e.y >= tab.y && e.y <= tab.y + tab.h) {
        if (currentTab !== tab.id) {
          currentTab = tab.id;
          scroll = 0;
          rebuildVisibleItems();
          needsPaint();
        }
      }
    });
  }
  
  // Category expand/collapse
  if (e.is("touch") && layoutMode !== "tiny" && currentTab !== "all") {
    categoryButtons.forEach(cat => {
      const y = cat.y + scroll;
      if (e.x >= 0 && e.x <= screen.width && 
          e.y >= y && e.y <= y + cat.height) {
        if (expandedCategories.has(cat.name)) {
          expandedCategories.delete(cat.name);
        } else {
          expandedCategories.add(cat.name);
        }
        rebuildVisibleItems();
        needsPaint();
      }
    });
  }
  
  // Item interactions
  itemButtons.forEach((btn, i) => {
    const y = btn.box.y + scroll;
    // Only respond to clicks on the name itself, not the full row
    const inBounds = e.x >= btn.box.x && e.x <= btn.box.x + btn.box.w &&
                     e.y >= y && e.y <= y + btn.box.h;
    
    if (e.is("touch") && inBounds) {
      btn.down = true;
      anyDown = true;
      selectedItem = btn.name;
      hud.label(btn.name, "white");
      needsPaint();
    }
    
    if (e.is("lift") && btn.down) {
      btn.down = false;
      anyDown = false;
      // Show confirmation modal instead of jumping directly
      const item = visibleItems.find(i => i.name === btn.name);
      confirmModal = {
        name: btn.name,
        type: item?.type || "piece",
        desc: item?.data?.desc || "",
        yesBtn: null,
        noBtn: null,
        hoverYes: false,
        hoverNo: false,
      };
      beep?.();
      needsPaint();
    }
    
    if (e.is("draw:1") && anyDown && inBounds && !btn.down) {
      // Rollover while dragging
      itemButtons.forEach(b => b.down = false);
      btn.down = true;
      selectedItem = btn.name;
      hud.label(btn.name, "white");
      needsPaint();
    }
  });
  
  // Cancel on lift outside
  if (e.is("lift")) {
    anyDown = false;
    itemButtons.forEach(b => b.down = false);
    hud.label(piece);
    needsPaint();
  }
}

// Clamp scroll to valid range
function clampScroll(screen) {
  const totalHeight = visibleItems.length * ROW_HEIGHT;
  const viewHeight = screen.height - CONTENT_TOP;
  const maxScroll = 0;
  const minScroll = -max(0, totalHeight - viewHeight);
  scroll = max(minScroll, min(maxScroll, scroll));
}

// 👋 Leave
function leave({ store }) {
  store["list:tab"] = currentTab;
  store["list:scroll:" + currentTab] = scroll;
  store["list:expanded"] = Array.from(expandedCategories);
  store.persist("list:tab", "list:scroll:" + currentTab, "list:expanded");
}

// 📰 Meta
function meta() {
  return {
    title: "List",
    desc: "A directory of all system pieces and prompt commands.",
  };
}

export { boot, paint, act, leave, meta };
