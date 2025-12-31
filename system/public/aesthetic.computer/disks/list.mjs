// List, 2024.1.30.13.18.29.955
// A comprehensive directory of pieces and prompt commands.
// Redesigned 2025.12.31 with categories, tabs, and responsive layout.

const { keys, entries } = Object;
const { min, max, floor } = Math;

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
  },
};

// 📁 Category definitions for pieces
const PIECE_CATEGORIES = {
  "🎨 Creative": ["box", "line", "circle", "colors", "fill", "brush", "crop", "download", "pixel", "shape", "poly", "oval", "blur", "nopaint", "painting", "plot", "drawings", "doodle", "handprint"],
  "🎵 Audio": ["clock", "tone", "bleep", "chord", "synth", "bubble", "bgm", "colplay", "metronome", "notepat", "sing", "whistlegraph"],
  "📹 Media": ["tape", "video", "camera", "selfie", "cam", "camu", "recorder", "mic"],
  "💬 Social": ["chat", "mood", "moods", "field", "scream", "handle", "profile", "me"],
  "🤖 AI Chat": ["sotce", "bf", "gf", "bro", "sis", "dad", "mom", "angel", "gargoyle", "boyfriend", "girlfriend", "brother", "sister"],
  "🎮 Games": ["pong", "snake", "balls", "brick-breaker", "gostop", "spin", "wand", "wiggle"],
  "💼 System": ["prompt", "list", "about", "help", "docs", "api", "debug", "404", "blank"],
  "💰 Wallet": ["wallet", "tezos", "keep", "kept", "mint", "ff", "freaky-flowers"],
  "🌍 World": ["world", "fly", "field", "handtime", "vr"],
  "🔧 Utilities": ["encode", "decode", "baktok", "qr", "resize", "icon", "preview"],
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
};

// State
let docs = null;
let pieces = {};
let commands = {};
let hitStats = {};       // { pieceName: hitCount }
let currentTab = "all"; // "all" | "pieces" | "commands" | "popular"
let expandedCategories = new Set();
let scroll = 0;
let selectedItem = null;
let anyDown = false;
let layoutMode = "large"; // "tiny" | "small" | "medium" | "large"

// UI elements
let tabButtons = [];
let itemButtons = [];
let categoryButtons = [];

// Cached data
let allItems = [];      // Flat list for "all" tab
let pieceItems = [];    // Categorized pieces
let commandItems = [];  // Categorized commands
let popularItems = [];  // Popular pieces sorted by hits
let visibleItems = [];  // Currently visible items based on tab + expanded categories

// Layout constants (adjusted per mode)
let ROW_HEIGHT = 14;
let HEADER_HEIGHT = 18;
let TAB_HEIGHT = 14;
let LEFT_MARGIN = 6;
let CONTENT_TOP = 36;

// 🥾 Boot
async function boot({ ui, net, store, params, screen }) {
  // Determine layout mode
  updateLayoutMode(screen);
  
  // Fetch docs and hit stats in parallel
  if (!params[0]) {
    const [docsResult, hitsResult] = await Promise.all([
      net.requestDocs(),
      fetch("/api/piece-hit?top=100").then(r => r.json()).catch(() => ({ pieces: [] })),
    ]);
    
    docs = docsResult;
    
    // Build hit stats map
    hitStats = {};
    if (hitsResult?.pieces) {
      hitsResult.pieces.forEach(p => {
        hitStats[p.piece] = p.hits;
      });
    }
    
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

// Build popular list sorted by hit count
function buildPopularList(ui) {
  // Combine pieces and commands with their hit counts
  const combined = {
    ...Object.fromEntries(keys(pieces).map(k => [k, { ...pieces[k], _type: "piece" }])),
    ...Object.fromEntries(keys(commands).map(k => [k, { ...commands[k], _type: "command" }])),
  };
  
  // Sort by hits (descending), then alphabetically for ties
  popularItems = keys(combined)
    .map(name => ({
      name,
      data: combined[name],
      type: combined[name]._type,
      hits: hitStats[name] || 0,
    }))
    .sort((a, b) => {
      if (b.hits !== a.hits) return b.hits - a.hits;
      return a.name.localeCompare(b.name);
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
  
  // Header background
  ink(pal.headerBg).box(0, 0, screen.width, HEADER_HEIGHT);
  
  // Item count
  const countText = `${visibleItems.filter(i => i.type !== "category").length}`;
  if (layoutMode !== "tiny") {
    ink(pal.categoryCount).write(countText, { x: screen.width - countText.length * 6 - 4, y: 4 });
  }
  
  // Tabs (not in tiny mode)
  if (layoutMode !== "tiny") {
    const tabY = HEADER_HEIGHT + 1;
    ink(pal.tabBg).box(0, tabY - 1, screen.width, TAB_HEIGHT + 2);
    
    const tabs = layoutMode === "small" 
      ? [["popular", "🔥"], ["all", "All"], ["pieces", "📦"], ["commands", "🎯"]]
      : [["popular", "🔥 Hot"], ["all", "All"], ["pieces", "Pieces"], ["commands", "Commands"]];
    
    let tabX = LEFT_MARGIN;
    tabButtons = []; // Reset
    tabs.forEach(([id, label]) => {
      const isActive = currentTab === id;
      const tabWidth = label.length * 6 + 8;
      
      if (isActive) {
        ink(pal.tabActive).box(tabX - 2, tabY, tabWidth, TAB_HEIGHT - 2);
        ink(pal.background).write(label, { x: tabX + 2, y: tabY + 2 });
      } else {
        ink(pal.tabInactive).write(label, { x: tabX + 2, y: tabY + 2 });
      }
      
      tabButtons.push({ id, x: tabX - 2, y: tabY, w: tabWidth, h: TAB_HEIGHT });
      tabX += tabWidth + 6;
    });
  }
  
  // Content area
  const maxDescLen = getMaxDescLength(screen);
  const contentBottom = screen.height - 2;
  
  visibleItems.forEach((item, i) => {
    const y = item.y + scroll;
    
    // Skip if off-screen
    if (y < CONTENT_TOP - ROW_HEIGHT || y > contentBottom) return;
    
    if (item.type === "category") {
      // Category header
      if (layoutMode !== "tiny") {
        ink(pal.categoryHeaderBg).box(0, y, screen.width, ROW_HEIGHT);
        
        // Chevron
        const chevron = item.expanded ? "▼" : "▸";
        ink(pal.chevron).write(chevron, { x: LEFT_MARGIN, y: y + 1 });
        
        // Category name
        ink(pal.categoryHeader).write(item.name, { x: LEFT_MARGIN + 10, y: y + 1 });
        
        // Count
        const countStr = `(${item.count})`;
        ink(pal.categoryCount).write(countStr, { 
          x: LEFT_MARGIN + 10 + item.name.length * 6 + 6, 
          y: y + 1 
        });
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
      const nameColor = item.type === "piece" ? pal.pieceName : pal.commandName;
      const indent = layoutMode === "tiny" ? 0 : (currentTab === "all" || currentTab === "popular" ? 0 : 8);
      ink(isHighlighted ? pal.highlight : nameColor).write(item.name, { 
        x: LEFT_MARGIN + indent, 
        y: y + 1 
      });
      
      // Show hit count in popular tab
      if (currentTab === "popular" && item.hits > 0 && layoutMode !== "tiny") {
        const hitText = item.hits >= 1000 ? `${(item.hits / 1000).toFixed(1)}k` : `${item.hits}`;
        const hitX = LEFT_MARGIN + indent + item.name.length * 6 + 6;
        ink(pal.categoryCount).write(hitText, { x: hitX, y: y + 1 });
      }
      // Description (if space allows and not in popular tab)
      else if (maxDescLen > 0 && item.data?.desc && currentTab !== "popular") {
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
}

// 🎪 Act
function act({ event: e, screen, hud, piece, jump, needsPaint, geo }) {
  updateLayoutMode(screen);
  
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
    const inBounds = e.x >= btn.box.x && e.x <= screen.width &&
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
      // Jump to the piece/command
      jump("prompt~" + btn.name);
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
