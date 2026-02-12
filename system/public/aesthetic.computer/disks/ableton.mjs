// Ableton - AC Max for Live Plugin Manager, 2026.02.12
// Browse and download Max for Live devices

/* #region 📚 README
#endregion */

/* #region 🏁 TODO
  + Done
  - [] Add search/filter functionality
  - [] Add category tabs (Instruments, Effects, MIDI)
  - [] Show version history
  - [] Add installation instructions
  - [] Add user ratings/reviews
#endregion */

let plugins = [];
let loading = true;
let error = null;
let downloadButtons = [];
let hoveredButton = null;
let selectedCategory = null; // null = all, "instrument", "effect", "midi"

const { floor } = Math;

export const boot = () => {
  // Fetch plugins from API
  fetch("/m4l-plugins")
    .then((res) => {
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return res.json();
    })
    .then((data) => {
      plugins = Array.isArray(data) ? data : [];
      loading = false;
      console.log(`📦 Loaded ${plugins.length} plugins`);
    })
    .catch((err) => {
      console.error("Failed to load plugins:", err);
      error = "Failed to load plugins. Please refresh.";
      loading = false;
    });
};

function paint({ wipe, ink, screen, write, line, box, pen, num }) {
  wipe(20, 20, 25); // Dark background

  const { width, height } = screen;
  const margin = 20;
  const headerHeight = 60;

  // Header
  ink(255, 255, 255).write("AC Max for Live Plugins", { x: margin, y: margin + 10, size: 2 });
  ink(150, 150, 150).write(
    "Download devices for Ableton Live",
    { x: margin, y: margin + 30, size: 1 }
  );

  // Separator line
  ink(60, 60, 70).line(margin, headerHeight, width - margin, headerHeight);

  // Loading state
  if (loading) {
    ink(200, 200, 200).write("Loading plugins...", {
      x: width / 2,
      y: height / 2,
      center: "xy",
    });
    return;
  }

  // Error state
  if (error) {
    ink(255, 100, 100).write(error, {
      x: width / 2,
      y: height / 2,
      center: "xy",
    });
    return;
  }

  // No plugins state
  if (plugins.length === 0) {
    ink(150, 150, 150).write("No plugins available", {
      x: width / 2,
      y: height / 2,
      center: "xy",
    });
    return;
  }

  // Plugin list
  downloadButtons = []; // Reset button tracking
  let y = headerHeight + 30;
  const cardHeight = 120;
  const cardMargin = 15;

  for (const plugin of plugins) {
    // Skip if category filter active
    if (selectedCategory && plugin.device.category !== selectedCategory) {
      continue;
    }

    // Card background
    const cardY = y;
    const isHovered = hoveredButton && hoveredButton.plugin.code === plugin.code;

    if (isHovered) {
      ink(40, 40, 50);
    } else {
      ink(30, 30, 35);
    }
    box(margin, cardY, width - margin * 2, cardHeight);

    // Device icon and name
    ink(255, 255, 255).write(
      `${plugin.device.icon} ${plugin.device.displayName}`,
      { x: margin + 15, y: cardY + 15, size: 1.5 }
    );

    // Version
    ink(150, 200, 255).write(
      `v${plugin.version.string}`,
      { x: margin + 15, y: cardY + 35, size: 1 }
    );

    // Description
    ink(180, 180, 180).write(
      plugin.metadata.description || "No description",
      { x: margin + 15, y: cardY + 55, size: 0.9, maxWidth: width - 200 }
    );

    // Dimensions
    ink(120, 120, 130).write(
      `${plugin.metadata.width}x${plugin.metadata.height}`,
      { x: margin + 15, y: cardY + 75, size: 0.8 }
    );

    // Stats
    const downloads = plugin.stats?.downloads || 0;
    ink(100, 150, 100).write(
      `↓ ${downloads} downloads`,
      { x: margin + 15, y: cardY + 90, size: 0.8 }
    );

    // Download button
    const btnWidth = 140;
    const btnHeight = 30;
    const btnX = width - margin - btnWidth - 15;
    const btnY = cardY + (cardHeight - btnHeight) / 2;

    // Button background
    const btnHovered =
      pen &&
      pen.x >= btnX &&
      pen.x < btnX + btnWidth &&
      pen.y >= btnY &&
      pen.y < btnY + btnHeight;

    if (btnHovered) {
      ink(80, 150, 255);
    } else {
      ink(60, 120, 200);
    }
    box(btnX, btnY, btnWidth, btnHeight);

    // Button text
    ink(255, 255, 255).write("Download .amxd", {
      x: btnX + btnWidth / 2,
      y: btnY + btnHeight / 2 + 3,
      center: "xy",
      size: 0.9
    });

    // Store button bounds
    downloadButtons.push({
      plugin,
      bounds: { x: btnX, y: btnY, w: btnWidth, h: btnHeight }
    });

    y += cardHeight + cardMargin;
  }

  // Footer instructions
  const footerY = height - 40;
  ink(80, 80, 90).line(margin, footerY, width - margin, footerY);
  ink(120, 120, 130).write(
    "Click Download to save .amxd file • Drag into Ableton Live to install",
    { x: width / 2, y: footerY + 15, center: "x", size: 0.8 }
  );
}

function act({ event: e, pen, sound, jump }) {
  if (!plugins || plugins.length === 0) return;

  // Track hover state
  if (e.is("move") || e.is("draw")) {
    hoveredButton = null;

    for (const btn of downloadButtons) {
      const { x, y, w, h } = btn.bounds;
      if (
        pen &&
        pen.x >= x &&
        pen.x < x + w &&
        pen.y >= y &&
        pen.y < y + h
      ) {
        hoveredButton = btn;
        break;
      }
    }
  }

  // Handle download click
  if (e.is("touch") && hoveredButton) {
    const plugin = hoveredButton.plugin;

    // Play feedback sound
    sound.synth({
      type: "sine",
      tone: 440,
      beats: 0.1,
      volume: 0.3
    });

    // Track download via API
    fetch(`/m4l-plugins/${plugin.code}/download`, { method: 'POST' })
      .then(res => res.json())
      .then(data => {
        console.log(`📥 Downloading: ${plugin.m4l.fileName}`);

        // Trigger download
        window.open(data.downloadUrl, '_blank');
      })
      .catch(err => {
        console.error("Download tracking failed:", err);
        // Still try to download even if tracking fails
        window.open(plugin.m4l.downloadUrl, '_blank');
      });
  }
}

export { paint, act };
