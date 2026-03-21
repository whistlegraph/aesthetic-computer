// gameboy-lab - GameBoy ROM Testing Lab
// Lists all available GB/GBC ROMs and allows loading them for testing

const ROMS = [
  { name: "boxtest.gb", type: "gb", desc: "KidLisp box drawing test" },
  { name: "circle-anim.gb", type: "gb", desc: "Animated circle demo" },
  { name: "demo-graphics.gbc", type: "gbc", desc: "Graphics demo" },
  { name: "direct_test.gb", type: "gb", desc: "Direct test ROM" },
  { name: "galaxy.gb", type: "gb", desc: "Galaxy graphics demo" },
  { name: "hello-test.gbc", type: "gbc", desc: "Hello world test" },
  { name: "hello.gb", type: "gb", desc: "Hello world" },
  { name: "joytest.gb", type: "gb", desc: "Joypad input test" },
  { name: "lines.gb", type: "gb", desc: "KidLisp line drawing" },
  { name: "linetest.gb", type: "gb", desc: "Line drawing test" },
  { name: "melody.gb", type: "gb", desc: "Music/melody demo" },
  { name: "melody.gbc", type: "gbc", desc: "Music/melody demo (color)" },
  { name: "melody_sprites.gbc", type: "gbc", desc: "Melody with sprites" },
  { name: "minimal.gb", type: "gb", desc: "Minimal test ROM" },
  { name: "pagetest.gb", type: "gb", desc: "Page test" },
  { name: "scrub.gbc", type: "gbc", desc: "Scrub test" },
  { name: "sgb_sfx.gb", type: "gb", desc: "Super GB sound effects" },
  { name: "shapes.gb", type: "gb", desc: "KidLisp shapes demo" },
  { name: "sound.gb", type: "gb", desc: "Sound test" },
  { name: "test-simple.gb", type: "gb", desc: "Simple test ROM" },
  { name: "test-text.gbc", type: "gbc", desc: "Text rendering test" },
  { name: "turtle-circle-simple.gb", type: "gb", desc: "Turtle graphics circle" },
  { name: "ulysses-dmg.gb", type: "gb", desc: "Ulysses eBook (DMG)" },
  { name: "ulysses.gb", type: "gb", desc: "Ulysses eBook" },
  { name: "ulysses.gbc", type: "gbc", desc: "Ulysses eBook (color)" },
  { name: "wav_sample.gb", type: "gb", desc: "Wave sample demo" },
  { name: "wave-editor.gbc", type: "gbc", desc: "Wave editor" },
  { name: "wave-test.gbc", type: "gbc", desc: "Wave test" },
];

let selectedIndex = 0;
let scrollOffset = 0;
let scroll = 0;
let loading = false;
let loadedRom = null;

// Layout constants
const TOP_MARGIN = 30;
const ROW_HEIGHT = 22;
const LEFT_MARGIN = 6;

// 🥾 Boot
export function boot({ wipe }) {
  wipe("black");
}

// 🎨 Paint
export function paint({
  wipe,
  ink,
  write,
  screen,
  hud,
  pen
}) {
  // Dark background like list.mjs
  wipe(16, 16, 24);

  // Hide default label
  hud.label();

  // Header background (masks scrolling content)
  ink(24, 24, 36).box(0, 0, screen.width, 24);

  // Title
  ink("white").write("🎮 GameBoy ROM Lab", { x: LEFT_MARGIN, y: 4 });

  // ROM count using MatrixChunky8
  const countText = `${ROMS.length}`;
  ink(80, 80, 100).write(countText, {
    x: screen.width - countText.length * 4 - 4,
    y: 6
  }, undefined, undefined, false, "MatrixChunky8");

  // Instructions bar at bottom using MatrixChunky8
  const instructions = loading
    ? "Loading..."
    : "↑↓ Nav • Enter Load • Esc Clear";
  ink(100, 100, 120).write(instructions, {
    x: LEFT_MARGIN,
    y: screen.height - 9
  }, undefined, undefined, false, "MatrixChunky8");

  // Calculate visible range
  const contentTop = TOP_MARGIN;
  const viewHeight = screen.height - contentTop - 12;

  // Draw ROM list with alternating backgrounds
  ROMS.forEach((rom, index) => {
    const y = scroll + contentTop + (index * ROW_HEIGHT);

    // Skip if off-screen
    if (y < TOP_MARGIN - ROW_HEIGHT || y > screen.height) return;

    const isSelected = index === selectedIndex;

    // Alternating row background
    const rowColor = index % 2 === 0 ? [16, 16, 24] : [20, 20, 28];
    ink(...rowColor).box(0, y, screen.width - 4, ROW_HEIGHT);

    // Selection highlight
    if (isSelected) {
      ink(255, 255, 100, 60).box(0, y, screen.width - 4, ROW_HEIGHT);
    }

    // ROM type indicator (colored dot)
    const dotColor = rom.type === "gbc" ? [255, 200, 100] : [100, 220, 150]; // Yellow for GBC, Green for GB
    ink(...dotColor).box(LEFT_MARGIN + 2, y + 4, 6, 6, "*");

    // ROM name - color coded by type
    const nameColor = isSelected
      ? [255, 255, 100]  // Bright yellow when selected
      : (rom.type === "gbc" ? [255, 200, 100] : [100, 220, 150]); // Yellow for GBC, Green for GB
    ink(...nameColor).write(rom.name, { x: LEFT_MARGIN + 12, y: y + 2 });

    // Description using MatrixChunky8
    const descColor = isSelected ? [180, 180, 200] : [100, 100, 120];
    ink(...descColor).write(rom.desc, {
      x: LEFT_MARGIN + 12,
      y: y + 11
    }, undefined, undefined, false, "MatrixChunky8");
  });

  // Scrollbar (like list.mjs)
  const totalHeight = ROMS.length * ROW_HEIGHT;
  const viewHeight2 = screen.height - TOP_MARGIN - 12;
  if (totalHeight > viewHeight2) {
    const scrollbarHeight = Math.max(10, (viewHeight2 / totalHeight) * viewHeight2);
    const maxScroll = Math.max(0, totalHeight - viewHeight2);
    const scrollRatio = maxScroll > 0 ? Math.abs(scroll) / maxScroll : 0;
    const scrollbarY = TOP_MARGIN + (viewHeight2 - scrollbarHeight) * scrollRatio;

    ink(60, 60, 80).box(screen.width - 3, TOP_MARGIN, 2, viewHeight2);
    ink(100, 120, 180).box(screen.width - 3, scrollbarY, 2, scrollbarHeight);
  }

  // Show loaded ROM indicator in header
  if (loadedRom) {
    ink(100, 220, 150).write(`✓ ${loadedRom}`, {
      x: LEFT_MARGIN,
      y: 15
    }, undefined, undefined, false, "MatrixChunky8");
  }

  return false; // Don't paint every frame
}

// ⌨️ Act - Handle keyboard and mouse input
export function act({ event: e, jump, debug, send, screen, needsPaint }) {
  if (loading) return;

  // Scroll handling (like list.mjs)
  if (e.is("scroll")) {
    scroll -= e.y;
    clampScroll(screen);
    needsPaint();
    return;
  }

  // Drag scrolling
  if (e.is("draw:1")) {
    scroll += e.delta.y;
    clampScroll(screen);
    needsPaint();
    return;
  }

  // Arrow key navigation
  if (e.is("keyboard:down:ArrowDown") || e.is("keyboard:down:down")) {
    selectedIndex = Math.min(selectedIndex + 1, ROMS.length - 1);
    autoScroll(screen);
    needsPaint();
    return;
  }

  if (e.is("keyboard:down:ArrowUp") || e.is("keyboard:down:up")) {
    selectedIndex = Math.max(selectedIndex - 1, 0);
    autoScroll(screen);
    needsPaint();
    return;
  }

  // Load ROM with Enter
  if (e.is("keyboard:down:Enter") || e.is("keyboard:down:return")) {
    loadROM(selectedIndex, { debug, send, jump });
    needsPaint();
    return;
  }

  // Clear with Escape
  if (e.is("keyboard:down:Escape")) {
    loadedRom = null;
    needsPaint();
    return;
  }

  // Touch/click to select and load
  if (e.is("lift")) {
    const clickY = e.y;

    // Calculate which ROM was clicked based on scroll position
    const clickedIndex = Math.floor((clickY - scroll - TOP_MARGIN) / ROW_HEIGHT);

    if (clickedIndex >= 0 && clickedIndex < ROMS.length) {
      if (clickedIndex === selectedIndex) {
        // Double-click/same selection = load
        loadROM(selectedIndex, { debug, send, jump });
      } else {
        // First click = select
        selectedIndex = clickedIndex;
      }
      needsPaint();
      return;
    }
  }
}

// Clamp scroll to valid range
function clampScroll(screen) {
  const totalHeight = ROMS.length * ROW_HEIGHT;
  const viewHeight = screen.height - TOP_MARGIN - 12;
  const maxScroll = 0;
  const minScroll = -Math.max(0, totalHeight - viewHeight);
  scroll = Math.max(minScroll, Math.min(maxScroll, scroll));
}

// Auto-scroll to keep selected item visible
function autoScroll(screen) {
  const itemY = scroll + TOP_MARGIN + (selectedIndex * ROW_HEIGHT);
  const viewTop = TOP_MARGIN;
  const viewBottom = screen.height - 12;

  // Scroll down if item is below viewport
  if (itemY + ROW_HEIGHT > viewBottom) {
    scroll -= (itemY + ROW_HEIGHT - viewBottom);
  }
  // Scroll up if item is above viewport
  else if (itemY < viewTop) {
    scroll += (viewTop - itemY);
  }

  clampScroll(screen);
}

// Load a ROM
async function loadROM(index, { debug, send, jump }) {
  const rom = ROMS[index];
  if (!rom) return;

  loading = true;
  console.log(`🎮 Loading ROM: ${rom.name}`);

  try {
    // Construct path based on environment
    const basePath = debug
      ? "/assets/gameboy"
      : "https://assets.aesthetic.computer/gameboy";

    const romUrl = `${basePath}/${rom.name}`;

    // Fetch the ROM file
    const response = await fetch(romUrl);
    if (!response.ok) {
      throw new Error(`Failed to load ROM: ${response.status}`);
    }

    const romData = await response.arrayBuffer();
    console.log(`🎮 ROM loaded: ${romData.byteLength} bytes`);

    // Detect if it's a GBC ROM
    const isGameBoyColor = rom.type === "gbc";

    // Create romData object matching bios.mjs format
    const romDataObj = {
      name: rom.name.replace(/\.(gb|gbc)$/, ""),
      originalName: rom.name,
      romData: romData,
      isGameBoyColor: isGameBoyColor,
    };

    // Send load command to bios
    send({
      type: "gameboy:load-rom",
      content: romDataObj
    });

    loadedRom = rom.name;
    console.log(`🎮 ROM "${rom.name}" sent to BIOS`);

  } catch (error) {
    console.error(`🎮 Failed to load ROM "${rom.name}":`, error);
    loadedRom = null;
  } finally {
    loading = false;
  }
}

export function meta() {
  return {
    title: "GameBoy ROM Lab",
    desc: "Browse and test GameBoy ROMs"
  };
}
