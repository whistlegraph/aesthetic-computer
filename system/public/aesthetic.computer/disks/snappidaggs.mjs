// Snappidaggs, 2026.1.06
// Browse the complete snappidagg archive - scrollable index.
// Click any entry to view it, or use keyboard to navigate.

/* 📝 Notes
  - Three tiers of snappidaggs:
    - er: numbered 00-99 plus special codes (a1-a6, l0-l9, p0-p5) - 120 total
    - flokrae: letters a-y plus ae, oe, h - 26 total  
    - garundz: special numbers - 12 total
  - Total: 158 snappidaggs
*/

const ASSET_BASE = "https://assets.aesthetic.computer/snappidagg";

let index = null;
let entries = [];
let buttons = [];
let scroll = 0;
let loading = true;
let error = null;
let thumbnails = {}; // Cache loaded thumbnails
let loadingThumbs = new Set();
let loadQueue = []; // Queue of thumbnails to load
let activeLoads = 0; // Currently loading count
const MAX_CONCURRENT_LOADS = 3; // Only load 3 at a time
let lastScrollTime = 0;
let visibleKeys = new Set(); // Track what's currently visible
let netRef = null; // Store net reference for async loading
let storeRef = null; // Store reference for persistence
let visited = new Set(); // Track visited snappidaggs

// Layout constants
const LEFT_MARGIN = 8;
const TOP_MARGIN = 24;
const ROW_HEIGHT = 40;
const THUMB_SIZE = 32;

async function boot({ ui, store, net }) {
  netRef = net; // Store for async loading
  storeRef = store; // Store for persistence
  
  // Restore scroll position
  scroll = (await store.retrieve("snappidaggs:scroll")) || 0;
  
  // Restore visited set
  const visitedArray = (await store.retrieve("snappidaggs:visited")) || [];
  visited = new Set(visitedArray);
  
  // Load the index
  try {
    const res = await fetch(`${ASSET_BASE}/index.json`);
    index = await res.json();
    buildEntries(ui);
    loading = false;
  } catch (e) {
    error = "Failed to load snappidagg index";
    loading = false;
  }
}

function buildEntries(ui) {
  entries = [];
  
  // Add header
  entries.push({
    type: "header",
    name: "SNAPPIDAGG ARCHIVE",
    description: "Goodiepal's visual collection",
  });
  
  // Add 'er tier (main snappidaggs)
  entries.push({
    type: "divider",
    name: `SNAPPIDAGG'ER (${index.er.length})`,
  });
  
  index.er.forEach((id) => {
    entries.push({
      type: "snappidagg",
      tier: "er",
      id: id,
      name: `snappidagg ${id}`,
      command: `snappidagg ${id}`,
      button: new ui.Button(0, 0, 200, ROW_HEIGHT - 2),
    });
  });
  
  // Add flokrae tier
  entries.push({
    type: "divider",
    name: `SNAPPIDAGG FLÖKRÆ (${index.flokrae.length})`,
  });
  
  index.flokrae.forEach((id) => {
    entries.push({
      type: "snappidagg",
      tier: "flokrae",
      id: id,
      name: `snappidagg flökræ ${id.toUpperCase()}`,
      command: `snappidagg flokrae:${id}`,
      button: new ui.Button(0, 0, 200, ROW_HEIGHT - 2),
    });
  });
  
  // Add garundz tier
  entries.push({
    type: "divider",
    name: `SNAPPIDAGG GARUNDZ (${index.garundz.length})`,
  });
  
  index.garundz.forEach((id) => {
    entries.push({
      type: "snappidagg",
      tier: "garundz",
      id: id,
      name: `snappidagg garundz ${id}`,
      command: `snappidagg garundz:${id}`,
      button: new ui.Button(0, 0, 200, ROW_HEIGHT - 2),
    });
  });
  
  // Add footer
  entries.push({
    type: "footer",
    name: "",
  });
  
  // Collect buttons
  buttons = entries.filter(e => e.button).map(e => e.button);
}

function paint({ wipe, ink, screen, paste, net }) {
  wipe([16, 12, 20]);
  
  if (loading) {
    ink("gray").write("Loading snappidagg index...", { center: "xy" });
    return;
  }
  
  if (error) {
    ink("red").write(error, { center: "xy" });
    return;
  }
  
  // Track visible thumbnails this frame
  const frameVisibleKeys = new Set();
  
  // Draw entries
  let visibleCount = 0;
  
  entries.forEach((entry, i) => {
    const y = scroll + TOP_MARGIN + ROW_HEIGHT * i;
    
    // Only draw if visible
    if (y < -ROW_HEIGHT || y > screen.height + ROW_HEIGHT) return;
    
    visibleCount++;
    
    // Alternating row backgrounds for snappidagg entries
    if (entry.type === "snappidagg") {
      const rowColor = i % 2 === 0 ? [24, 20, 28] : [32, 28, 36];
      ink(rowColor).box(0, y, screen.width, ROW_HEIGHT);
    }
    
    if (entry.type === "header") {
      // Header styling
      ink("magenta").write(entry.name, {
        x: LEFT_MARGIN,
        y: y + 4,
        size: 1,
      });
      ink("gray").write(entry.description, {
        x: LEFT_MARGIN,
        y: y + 14,
      });
      return;
    }
    
    if (entry.type === "divider") {
      // Section divider
      ink([60, 50, 70]).box(LEFT_MARGIN, y + 10, screen.width - LEFT_MARGIN * 2, 1);
      ink("yellow").write(entry.name, {
        x: LEFT_MARGIN,
        y: y + 14,
      });
      return;
    }
    
    if (entry.type === "footer") {
      return;
    }
    
    // Snappidagg entry
    if (entry.button) {
      // Update button position based on scroll
      entry.button.box.x = LEFT_MARGIN;
      entry.button.box.y = y + 2;
      entry.button.box.w = screen.width - LEFT_MARGIN * 2;
      
      entry.button.paint((b) => {
        // Thumbnail placeholder or loaded image
        const thumbKey = `${entry.tier}/${entry.id}`;
        frameVisibleKeys.add(thumbKey);
        
        if (thumbnails[thumbKey]) {
          // Draw loaded thumbnail
          paste(thumbnails[thumbKey], LEFT_MARGIN, y + 4, { 
            width: THUMB_SIZE, 
            height: THUMB_SIZE 
          });
        } else {
          // Draw placeholder
          ink([40, 36, 44]).box(LEFT_MARGIN, y + 4, THUMB_SIZE, THUMB_SIZE);
          
          // Queue for loading if not already queued or loading
          if (!loadingThumbs.has(thumbKey) && !loadQueue.includes(thumbKey)) {
            loadQueue.push(thumbKey);
          }
        }
        
        // Border around thumbnail
        ink(b.down ? "yellow" : "gray").box(
          LEFT_MARGIN - 1, 
          y + 3, 
          THUMB_SIZE + 2, 
          THUMB_SIZE + 2, 
          "outline"
        );
        
        // Entry name - just show the ID
        const textColor = b.down ? "yellow" : "white";
        const displayName = entry.tier === "er" 
          ? entry.id 
          : `${entry.tier}:${entry.id}`;
        
        // Check if visited
        const isVisited = visited.has(`${entry.tier}/${entry.id}`);
        
        ink(textColor).write(displayName, {
          x: LEFT_MARGIN + THUMB_SIZE + 8,
          y: y + 12,
        });
        
        // Show checkmark if visited
        if (isVisited) {
          ink("lime").write("✓", {
            x: LEFT_MARGIN + THUMB_SIZE + 8 + displayName.length * 6 + 4,
            y: y + 12,
          });
        }
      });
    }
  });
  
  // Update visible keys and prioritize queue
  visibleKeys = frameVisibleKeys;
  
  // Scroll indicator
  const totalHeight = entries.length * ROW_HEIGHT;
  const viewRatio = screen.height / totalHeight;
  const scrollRatio = -scroll / (totalHeight - screen.height);
  
  if (viewRatio < 1) {
    const barHeight = Math.max(20, screen.height * viewRatio);
    const barY = scrollRatio * (screen.height - barHeight);
    
    ink([60, 50, 70]).box(screen.width - 4, 0, 4, screen.height);
    ink("magenta").box(screen.width - 3, barY, 2, barHeight);
  }
  
  // Top info bar
  ink([0, 0, 0, 200]).box(0, 0, screen.width, 18);
  ink("white").write("SNAPPIDAGGS", { x: 4, y: 4 });
  
  const total = index ? index.er.length + index.flokrae.length + index.garundz.length : 0;
  const visitedCount = visited.size;
  ink("gray").write(`${visitedCount}/${total}`, { x: 80, y: 4 });
  if (visitedCount > 0) {
    ink("lime").write("✓", { x: 80 + `${visitedCount}/${total}`.length * 6 + 2, y: 4 });
  }
  ink("cyan").write("↑↓ scroll | r random", { 
    x: screen.width - 120, 
    y: 4 
  });
}

// Process load queue - called from sim()
function processLoadQueue() {
  if (!netRef || activeLoads >= MAX_CONCURRENT_LOADS || loadQueue.length === 0) return;
  
  // Prioritize visible items - move them to front of queue
  loadQueue.sort((a, b) => {
    const aVisible = visibleKeys.has(a);
    const bVisible = visibleKeys.has(b);
    if (aVisible && !bVisible) return -1;
    if (!aVisible && bVisible) return 1;
    return 0;
  });
  
  // Remove items no longer visible (and not already loading)
  loadQueue = loadQueue.filter(key => visibleKeys.has(key) || loadingThumbs.has(key));
  
  // Start loading next items up to max concurrent
  while (activeLoads < MAX_CONCURRENT_LOADS && loadQueue.length > 0) {
    const key = loadQueue.shift();
    if (!key || loadingThumbs.has(key) || thumbnails[key] !== undefined) continue;
    
    const [tier, id] = key.split('/');
    loadingThumbs.add(key);
    activeLoads++;
    loadThumbnail(tier, id, key);
  }
}

async function loadThumbnail(tier, id, key) {
  try {
    // Use thumbnail version (64px wide, ~3-4KB each instead of multi-MB originals)
    const url = `${ASSET_BASE}/thumbs/${tier}/${id}.jpg`;
    const result = await netRef.preload(url);
    thumbnails[key] = result.img || result;
  } catch (e) {
    // Failed to load, mark as attempted
    thumbnails[key] = null;
  }
  loadingThumbs.delete(key);
  activeLoads--;
}

function act({ event: e, screen, store, jump }) {
  if (loading || error) return;
  
  // Scroll with keys
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    scroll -= ROW_HEIGHT;
    clampScroll(screen);
    store.persist("snappidaggs:scroll", scroll);
  }
  
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    scroll += ROW_HEIGHT;
    clampScroll(screen);
    store.persist("snappidaggs:scroll", scroll);
  }
  
  if (e.is("keyboard:down:pagedown")) {
    scroll -= screen.height - ROW_HEIGHT * 2;
    clampScroll(screen);
    store.persist("snappidaggs:scroll", scroll);
  }
  
  if (e.is("keyboard:down:pageup")) {
    scroll += screen.height - ROW_HEIGHT * 2;
    clampScroll(screen);
    store.persist("snappidaggs:scroll", scroll);
  }
  
  if (e.is("keyboard:down:home")) {
    scroll = 0;
    store.persist("snappidaggs:scroll", scroll);
  }
  
  if (e.is("keyboard:down:end")) {
    const totalHeight = entries.length * ROW_HEIGHT;
    scroll = -(totalHeight - screen.height);
    store.persist("snappidaggs:scroll", scroll);
  }
  
  // Random snappidagg
  if (e.is("keyboard:down:r")) {
    jump("snappidagg");
  }
  
  // Mouse wheel scroll
  if (e.is("scroll")) {
    scroll -= e.y;
    clampScroll(screen);
    store.persist("snappidaggs:scroll", scroll);
  }
  
  // Button clicks
  entries.forEach((entry) => {
    if (entry.button) {
      entry.button.act(e, {
        push: () => {
          // Mark as visited
          const key = `${entry.tier}/${entry.id}`;
          visited.add(key);
          store.persist("snappidaggs:visited", Array.from(visited));
          jump(entry.command);
        },
      });
    }
  });
}

function clampScroll(screen) {
  const totalHeight = entries.length * ROW_HEIGHT + TOP_MARGIN;
  const minScroll = -(totalHeight - screen.height + TOP_MARGIN);
  scroll = Math.min(0, Math.max(minScroll, scroll));
}

function sim() {
  // Process thumbnail load queue each frame
  processLoadQueue();
}

function meta() {
  return {
    title: "Snappidaggs",
    desc: "Browse Goodiepal's snappidagg archive.",
  };
}

export { boot, paint, act, sim, meta };
