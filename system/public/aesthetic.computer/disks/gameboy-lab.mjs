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
let loading = false;
let loadedRom = null;

const ITEMS_PER_PAGE = 12;

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
  wipe(10, 10, 20); // Dark blue-gray background

  // Hide default label
  hud.label();

  // Title
  ink("white").write("🎮 GameBoy ROM Lab", { x: 8, y: 8, size: 1 });
  ink(100, 100, 100).write(`${ROMS.length} ROMs available`, { x: 8, y: 18, size: 0.75 });

  // Instructions
  const instructions = loading
    ? "Loading ROM..."
    : "↑↓ Navigate • Enter/Click to Load • Esc to Clear";
  ink(150, 150, 150).write(instructions, { x: 8, y: screen.height - 12, size: 0.75 });

  // Calculate visible range
  const startY = 35;
  const lineHeight = 14;
  const visibleStart = scrollOffset;
  const visibleEnd = Math.min(scrollOffset + ITEMS_PER_PAGE, ROMS.length);

  // Draw ROM list
  ROMS.slice(visibleStart, visibleEnd).forEach((rom, i) => {
    const index = visibleStart + i;
    const y = startY + (i * lineHeight);
    const isSelected = index === selectedIndex;

    // Selection background
    if (isSelected) {
      ink(50, 50, 80).box(4, y - 2, screen.width - 8, lineHeight - 2, "*");
    }

    // ROM type indicator (colored dot)
    const dotColor = rom.type === "gbc" ? [255, 255, 0] : [0, 255, 0]; // Yellow for GBC, Green for GB
    ink(...dotColor).box(8, y + 2, 4, 4, "*");

    // ROM name
    const nameColor = isSelected ? [255, 255, 255] : [200, 200, 200];
    ink(...nameColor).write(rom.name, { x: 16, y, size: 0.85 });

    // Description
    const descColor = isSelected ? [180, 180, 255] : [120, 120, 140];
    ink(...descColor).write(rom.desc, { x: 16, y: y + 7, size: 0.65 });
  });

  // Scroll indicators
  if (scrollOffset > 0) {
    ink("white").write("▲", { x: screen.width - 12, y: startY - 5, center: "x" });
  }
  if (visibleEnd < ROMS.length) {
    ink("white").write("▼", { x: screen.width - 12, y: startY + (ITEMS_PER_PAGE * lineHeight) - 5, center: "x" });
  }

  // Show loaded ROM indicator
  if (loadedRom) {
    ink(0, 255, 0).write(`✓ Loaded: ${loadedRom}`, { x: 8, y: 28, size: 0.75 });
  }

  return false; // Don't paint every frame
}

// ⌨️ Act - Handle keyboard input
export function act({ event: e, jump, debug, send }) {
  if (loading) return;

  // Arrow key navigation
  if (e.is("keyboard:down:ArrowDown") || e.is("keyboard:down:down")) {
    selectedIndex = Math.min(selectedIndex + 1, ROMS.length - 1);

    // Auto-scroll down
    if (selectedIndex >= scrollOffset + ITEMS_PER_PAGE) {
      scrollOffset = Math.min(scrollOffset + 1, ROMS.length - ITEMS_PER_PAGE);
    }

    return { needsPaint: true };
  }

  if (e.is("keyboard:down:ArrowUp") || e.is("keyboard:down:up")) {
    selectedIndex = Math.max(selectedIndex - 1, 0);

    // Auto-scroll up
    if (selectedIndex < scrollOffset) {
      scrollOffset = Math.max(scrollOffset - 1, 0);
    }

    return { needsPaint: true };
  }

  // Load ROM with Enter
  if (e.is("keyboard:down:Enter") || e.is("keyboard:down:return")) {
    loadROM(selectedIndex, { debug, send, jump });
    return { needsPaint: true };
  }

  // Clear with Escape
  if (e.is("keyboard:down:Escape")) {
    loadedRom = null;
    return { needsPaint: true };
  }

  // Touch/click to select and load
  if (e.is("lift")) {
    const startY = 35;
    const lineHeight = 14;
    const clickY = e.y;

    const clickedIndex = Math.floor((clickY - startY) / lineHeight) + scrollOffset;

    if (clickedIndex >= 0 && clickedIndex < ROMS.length) {
      if (clickedIndex === selectedIndex) {
        // Double-click/same selection = load
        loadROM(selectedIndex, { debug, send, jump });
      } else {
        // First click = select
        selectedIndex = clickedIndex;
      }
      return { needsPaint: true };
    }
  }
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
