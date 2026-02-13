// Handle, 2026.02.13.09.00
// Customize your @handle colors - per-character RGB customization with dynamic shadows.

/* #region 🏁 TODO
  - [ ] Add color presets/themes
  - [ ] Add copy/share color scheme feature
#endregion */

const { floor, max, min } = Math;

let handle; // User's handle
let colors = []; // RGB colors for @ sign + each character [{r, g, b}, ...]
let defaultColors = []; // Store defaults for reset
let selectedCharIndex = 0; // Currently selected character for editing
let saveBtn, resetBtn; // Buttons
let loading = true;
let saveStatus = null; // "saving", "saved", "error"
let lastSaveTime = 0;
let hoveredCharIndex = -1; // Track hovered character
let hoveredSlider = null; // Track hovered slider ("r", "g", "b", or null)

// Slider config - responsive and centered
const sliderConfig = {
  width: 220,
  height: 18,
  gap: 32,
};

function boot({ get, net, store }) {
  // Load user's handle
  handle = store["handle"];

  if (!handle) {
    // No handle set - jump to prompt to set one
    return {
      jump: "prompt",
    };
  }

  // Initialize default colors (white for all characters) or load from API
  const handleWithAt = "@" + handle;

  // Fetch existing color data from server
  console.log(`🎨 Loading colors for handle: ${handle}`);
  fetch(`/.netlify/functions/handle-colors?handle=${encodeURIComponent(handle)}`)
    .then((response) => response.json())
    .then((data) => {
      console.log(`🎨 Received color data:`, data);
      if (data.colors && data.colors.length === handleWithAt.length) {
        // Load existing colors
        colors = data.colors.map((c) => ({
          r: c.r || 255,
          g: c.g || 255,
          b: c.b || 255,
        }));
        console.log(`🎨 Loaded ${colors.length} custom colors`);
      } else {
        // Initialize with default white colors
        colors = handleWithAt.split("").map(() => ({ r: 255, g: 255, b: 255 }));
        console.log(`🎨 Using default white colors (${colors.length} chars)`);
      }
      // Store defaults for reset functionality
      defaultColors = colors.map((c) => ({ ...c }));
      loading = false;
    })
    .catch((err) => {
      console.warn("⚠️ Failed to load handle colors, using defaults:", err);
      // Default to white for all characters
      colors = handleWithAt.split("").map(() => ({ r: 255, g: 255, b: 255 }));
      defaultColors = colors.map((c) => ({ ...c }));
      loading = false;
    });
}

function paint({ ink, wipe, write, screen, ui, help }) {
  wipe(15, 15, 20); // Darker background

  if (loading) {
    ink(255, 255, 255).write("Loading...", { center: "xy", screen });
    return;
  }

  const handleWithAt = "@" + handle;

  // 🎨 Draw title
  ink(150, 180, 255).write("Customize Your Handle", { center: "x", y: 12, screen });

  // 🌈 LARGE Unifont handle at top (main preview)
  const unifontY = 40;
  const unifontCharWidth = 8; // Unifont is 8x16
  const unifontCharHeight = 16;
  const unifontHandleWidth = handleWithAt.length * unifontCharWidth;
  const unifontStartX = floor((screen.width - unifontHandleWidth) / 2);

  handleWithAt.split("").forEach((char, i) => {
    const x = unifontStartX + i * unifontCharWidth;
    const color = colors[i];

    // Draw shadow (darker, offset)
    const shadowR = floor(color.r * 0.25);
    const shadowG = floor(color.g * 0.25);
    const shadowB = floor(color.b * 0.25);
    ink(shadowR, shadowG, shadowB, 200).write(char, { x: x + 2, y: unifontY + 2 }, undefined, undefined, false, "unifont");

    // Draw character with color
    ink(color.r, color.g, color.b).write(char, { x, y: unifontY }, undefined, undefined, false, "unifont");

    // Highlight selected or hovered character
    if (i === selectedCharIndex) {
      ink(255, 255, 0, 120).box(x - 1, unifontY - 1, unifontCharWidth + 2, unifontCharHeight + 2, "outline");
    } else if (i === hoveredCharIndex) {
      ink(255, 255, 255, 60).box(x - 1, unifontY - 1, unifontCharWidth + 2, unifontCharHeight + 2, "outline");
    }
  });

  // Draw smaller normal font version below
  const normalY = unifontY + 30;
  const normalCharWidth = 6;
  const normalCharHeight = 12;
  const normalHandleWidth = handleWithAt.length * normalCharWidth;
  const normalStartX = floor((screen.width - normalHandleWidth) / 2);

  handleWithAt.split("").forEach((char, i) => {
    const x = normalStartX + i * normalCharWidth;
    const color = colors[i];

    // Draw shadow
    const shadowR = floor(color.r * 0.25);
    const shadowG = floor(color.g * 0.25);
    const shadowB = floor(color.b * 0.25);
    ink(shadowR, shadowG, shadowB, 180).write(char, { x: x + 1, y: normalY + 1 });

    // Draw character
    ink(color.r, color.g, color.b).write(char, { x, y: normalY });

    // Highlight selected or hovered
    if (i === selectedCharIndex) {
      ink(255, 255, 0, 80).box(x - 1, normalY - 1, normalCharWidth + 2, normalCharHeight + 2, "outline");
    } else if (i === hoveredCharIndex) {
      ink(255, 255, 255, 40).box(x - 1, normalY - 1, normalCharWidth + 2, normalCharHeight + 2, "outline");
    }
  });

  // Draw MatrixChunky8 version below (with proper spacing)
  const chunkyY = normalY + 25;
  // MatrixChunky8 glyphs are 4px wide with variable spacing - calculate actual width
  let chunkyTotalWidth = 0;
  const charWidths = handleWithAt.split("").map((char) => {
    // Most chars in MatrixChunky8 are 4-5px, @ is typically wider
    return char === "@" ? 6 : 5;
  });
  chunkyTotalWidth = charWidths.reduce((sum, w) => sum + w, 0);

  const chunkyStartX = floor((screen.width - chunkyTotalWidth) / 2);
  const chunkyCharHeight = 8;

  let currentX = chunkyStartX;
  handleWithAt.split("").forEach((char, i) => {
    const charWidth = charWidths[i];
    const color = colors[i];

    // Draw shadow
    const shadowR = floor(color.r * 0.25);
    const shadowG = floor(color.g * 0.25);
    const shadowB = floor(color.b * 0.25);
    ink(shadowR, shadowG, shadowB, 180)
      .write(char, { x: currentX + 1, y: chunkyY + 1 }, undefined, undefined, false, "MatrixChunky8");

    // Draw character
    ink(color.r, color.g, color.b)
      .write(char, { x: currentX, y: chunkyY }, undefined, undefined, false, "MatrixChunky8");

    // Highlight selected or hovered
    if (i === selectedCharIndex) {
      ink(255, 255, 0, 80).box(currentX - 1, chunkyY - 1, charWidth + 2, chunkyCharHeight + 2, "outline");
    } else if (i === hoveredCharIndex) {
      ink(255, 255, 255, 40).box(currentX - 1, chunkyY - 1, charWidth + 2, chunkyCharHeight + 2, "outline");
    }

    currentX += charWidth;
  });

  // 📝 Selected character info
  const selectedChar = handleWithAt[selectedCharIndex];
  ink(180, 200, 255).write(
    `Editing: '${selectedChar}' (${selectedCharIndex + 1}/${handleWithAt.length})`,
    { center: "x", y: chunkyY + 25, screen },
    undefined,
    undefined,
    false,
    "MatrixChunky8"
  );

  // 🎚️ RGB Sliders - centered at bottom of screen with padding
  // Reserve 50px at bottom for buttons/status, position sliders above that
  const sliderStartY = screen.height - 216; // Position from bottom: 3 sliders + gaps + swatch + padding
  const sliderX = floor((screen.width - sliderConfig.width) / 2);
  const currentColor = colors[selectedCharIndex];

  // R slider
  drawSlider(
    ink,
    write,
    "R",
    sliderX,
    sliderStartY,
    sliderConfig.width,
    sliderConfig.height,
    currentColor.r,
    [255, 0, 0],
    hoveredSlider === "r"
  );

  // G slider
  drawSlider(
    ink,
    write,
    "G",
    sliderX,
    sliderStartY + sliderConfig.gap,
    sliderConfig.width,
    sliderConfig.height,
    currentColor.g,
    [0, 255, 0],
    hoveredSlider === "g"
  );

  // B slider
  drawSlider(
    ink,
    write,
    "B",
    sliderX,
    sliderStartY + sliderConfig.gap * 2,
    sliderConfig.width,
    sliderConfig.height,
    currentColor.b,
    [0, 0, 255],
    hoveredSlider === "b"
  );

  // Small color preview swatch (below sliders)
  const swatchSize = 40;
  const swatchX = floor(screen.width / 2 - swatchSize / 2);
  const swatchY = sliderStartY + sliderConfig.gap * 3 + 5;

  ink(currentColor.r, currentColor.g, currentColor.b).box(
    swatchX,
    swatchY,
    swatchSize,
    swatchSize,
    "fill"
  );
  ink(255, 255, 255, 150).box(
    swatchX,
    swatchY,
    swatchSize,
    swatchSize,
    "outline"
  );

  // 💾 Reset and Save buttons (bottom left and right like prompt curtain)
  if (!resetBtn) {
    resetBtn = new ui.TextButton("Reset", {
      x: 8,
      bottom: 8,
      screen,
    });
  } else {
    resetBtn.reposition({ x: 8, bottom: 8, screen });
  }

  if (!saveBtn) {
    saveBtn = new ui.TextButton("Save", {
      right: 8,
      bottom: 8,
      screen,
    });
  } else {
    saveBtn.reposition({ right: 8, bottom: 8, screen });
  }

  // Reset button color (cyan/teal)
  const resetColor = [[0, 100, 100], [0, 180, 180], 255];
  resetBtn.paint({ ink, box: (b) => ink().box(b) }, resetColor);

  // Save button color (green/yellow/red based on status)
  let saveBtnColor = [[0, 100, 0], [0, 180, 0], 255]; // Green
  if (saveStatus === "saving") {
    saveBtnColor = [[100, 100, 0], [180, 180, 0], 255]; // Yellow
  } else if (saveStatus === "saved") {
    saveBtnColor = [[0, 150, 0], [0, 255, 0], 255]; // Bright green
  } else if (saveStatus === "error") {
    saveBtnColor = [[150, 0, 0], [255, 0, 0], 255]; // Red
  }

  saveBtn.paint({ ink, box: (b) => ink().box(b) }, saveBtnColor);

  // Save status message
  if (saveStatus && Date.now() - lastSaveTime < 2500) {
    const statusText = {
      saving: "Saving...",
      saved: "Saved!",
      error: "Failed to save",
    }[saveStatus] || "";
    const statusColor = {
      saving: [255, 255, 0],
      saved: [0, 255, 0],
      error: [255, 0, 0],
    }[saveStatus] || [255, 255, 255];

    ink(statusColor).write(statusText, {
      center: "x",
      y: screen.height - 25,
      screen,
    }, undefined, undefined, false, "MatrixChunky8");
  }
}

function drawSlider(ink, write, label, x, y, width, height, value, color, hovered) {
  // Label
  ink(220, 220, 220).write(`${label}:`, {
    x: x - 18,
    y: y + floor(height / 2) - 4,
  }, undefined, undefined, false, "MatrixChunky8");

  // Track background with hover effect
  const bgBrightness = hovered ? 50 : 40;
  const outlineBrightness = hovered ? 100 : 80;
  ink(bgBrightness, bgBrightness, bgBrightness + 10).box(x, y, width, height, "fill");
  ink(outlineBrightness, outlineBrightness, outlineBrightness + 10).box(x, y, width, height, "outline");

  // Fill (shows current value)
  const fillWidth = floor((value / 255) * width);
  const fillAlpha = hovered ? 220 : 180;
  ink(color[0], color[1], color[2], fillAlpha).box(x + 1, y + 1, fillWidth - 1, height - 2, "fill");

  // Handle (draggable indicator) with hover effect
  const handleX = x + fillWidth - 3;
  const handleColor = hovered ? [255, 255, 255] : color;
  ink(...handleColor).box(handleX, y - 3, 6, height + 6, "fill");
  ink(255, 255, 255).box(handleX, y - 3, 6, height + 6, "outline");

  // Value display
  ink(255, 255, 255).write(value.toString().padStart(3, " "), {
    x: x + width + 10,
    y: y + floor(height / 2) - 4,
  }, undefined, undefined, false, "MatrixChunky8");
}

function act({ event: e, screen, net, help, jump, sound }) {
  if (loading) return;

  const handleWithAt = "@" + handle;

  // Calculate slider position for hover detection (matches paint calculation)
  const sliderStartY = screen.height - 216; // Position from bottom: 3 sliders + gaps + swatch + padding
  const sliderX = floor((screen.width - sliderConfig.width) / 2);

  // Update hover state for sliders
  hoveredSlider = null;
  const sliders = [
    { key: "r", y: sliderStartY },
    { key: "g", y: sliderStartY + sliderConfig.gap },
    { key: "b", y: sliderStartY + sliderConfig.gap * 2 },
  ];

  for (const slider of sliders) {
    if (
      e.y >= slider.y &&
      e.y < slider.y + sliderConfig.height &&
      e.x >= sliderX &&
      e.x < sliderX + sliderConfig.width
    ) {
      hoveredSlider = slider.key;
      help.repeat();
      break;
    }
  }

  // Update hover state for unifont preview
  const unifontY = 40;
  const unifontCharWidth = 8;
  const unifontCharHeight = 16;
  const unifontHandleWidth = handleWithAt.length * unifontCharWidth;
  const unifontStartX = floor((screen.width - unifontHandleWidth) / 2);

  // Update hover state for normal font preview
  const normalY = unifontY + 30;
  const normalCharWidth = 6;
  const normalCharHeight = 12;
  const normalHandleWidth = handleWithAt.length * normalCharWidth;
  const normalStartX = floor((screen.width - normalHandleWidth) / 2);

  // Update hover state for MatrixChunky8 preview (with proper spacing)
  const chunkyY = normalY + 25;
  const charWidths = handleWithAt.split("").map((char) => char === "@" ? 6 : 5);
  const chunkyStartX = floor((screen.width - charWidths.reduce((sum, w) => sum + w, 0)) / 2);
  const chunkyCharHeight = 8;

  hoveredCharIndex = -1;

  // Check hover on unifont
  if (e.y >= unifontY && e.y < unifontY + unifontCharHeight) {
    for (let i = 0; i < handleWithAt.length; i++) {
      const x = unifontStartX + i * unifontCharWidth;
      if (e.x >= x && e.x < x + unifontCharWidth) {
        hoveredCharIndex = i;
        help.repeat();
        break;
      }
    }
  }

  // Check hover on normal font
  if (e.y >= normalY && e.y < normalY + normalCharHeight) {
    for (let i = 0; i < handleWithAt.length; i++) {
      const x = normalStartX + i * normalCharWidth;
      if (e.x >= x && e.x < x + normalCharWidth) {
        hoveredCharIndex = i;
        help.repeat();
        break;
      }
    }
  }

  // Check hover on chunky font
  if (e.y >= chunkyY && e.y < chunkyY + chunkyCharHeight) {
    let currentX = chunkyStartX;
    for (let i = 0; i < handleWithAt.length; i++) {
      const charWidth = charWidths[i];
      if (e.x >= currentX && e.x < currentX + charWidth) {
        hoveredCharIndex = i;
        help.repeat();
        break;
      }
      currentX += charWidth;
    }
  }

  // Handle character selection (click on any preview)
  if (e.is("lift") || e.is("touch")) {
    // Check click on unifont
    if (e.y >= unifontY && e.y < unifontY + unifontCharHeight) {
      for (let i = 0; i < handleWithAt.length; i++) {
        const x = unifontStartX + i * unifontCharWidth;
        if (e.x >= x && e.x < x + unifontCharWidth) {
          selectedCharIndex = i;
          sound.synth({
            type: "triangle",
            tone: 400 + i * 50,
            duration: 0.05,
            volume: 0.3,
          });
          help.repeat();
          return;
        }
      }
    }

    // Check click on normal font
    if (e.y >= normalY && e.y < normalY + normalCharHeight) {
      for (let i = 0; i < handleWithAt.length; i++) {
        const x = normalStartX + i * normalCharWidth;
        if (e.x >= x && e.x < x + normalCharWidth) {
          selectedCharIndex = i;
          sound.synth({
            type: "triangle",
            tone: 400 + i * 50,
            duration: 0.05,
            volume: 0.3,
          });
          help.repeat();
          return;
        }
      }
    }

    // Check click on chunky font
    if (e.y >= chunkyY && e.y < chunkyY + chunkyCharHeight) {
      let currentX = chunkyStartX;
      for (let i = 0; i < handleWithAt.length; i++) {
        const charWidth = charWidths[i];
        if (e.x >= currentX && e.x < currentX + charWidth) {
          selectedCharIndex = i;
          sound.synth({
            type: "triangle",
            tone: 400 + i * 50,
            duration: 0.05,
            volume: 0.3,
          });
          help.repeat();
          return;
        }
        currentX += charWidth;
      }
    }
  }

  // Handle Reset button with proper act callback
  resetBtn.act(e, () => {
    colors = defaultColors.map((c) => ({ ...c }));
    sound.synth({
      type: "square",
      tone: 300,
      duration: 0.08,
      volume: 0.3,
    });
    help.repeat();
  });

  // Handle Save button with proper act callback
  saveBtn.act(e, () => {
    saveColors(net, help, sound);
  });

  // Handle slider dragging
  if (e.is("draw") || e.is("lift") || e.is("touch")) {
    const currentColor = colors[selectedCharIndex];

    for (const slider of sliders) {
      if (
        e.y >= slider.y &&
        e.y < slider.y + sliderConfig.height &&
        e.x >= sliderX &&
        e.x < sliderX + sliderConfig.width
      ) {
        const relX = e.x - sliderX;
        const newValue = floor((relX / sliderConfig.width) * 255);
        currentColor[slider.key] = max(0, min(255, newValue));
        help.repeat();
        return;
      }
    }
  }
}

async function saveColors(net, help, sound) {
  saveStatus = "saving";
  lastSaveTime = Date.now();
  help.repeat();

  console.log(`🎨 Saving ${colors.length} colors for handle: ${handle}`);

  try {
    const payload = {
      handle,
      colors: colors.map((c) => ({ r: c.r, g: c.g, b: c.b })),
    };
    console.log(`🎨 Save payload:`, payload);

    const response = await net.userRequest("POST", "/.netlify/functions/handle-colors", payload);

    console.log(`🎨 Save response:`, response);

    if (response.status === 200) {
      saveStatus = "saved";
      // Update defaults to current colors after successful save
      defaultColors = colors.map((c) => ({ ...c }));
      console.log(`✅ Colors saved successfully`);
      sound.synth({
        type: "sine",
        tone: 600,
        duration: 0.1,
        volume: 0.3,
      });
    } else {
      saveStatus = "error";
      console.error("❌ Failed to save handle colors. Status:", response.status, "Response:", response);
    }
  } catch (err) {
    saveStatus = "error";
    console.error("❌ Error saving handle colors:", err);
  }

  lastSaveTime = Date.now();
  help.repeat();

  // Clear status after 2.5 seconds
  setTimeout(() => {
    if (Date.now() - lastSaveTime >= 2500) {
      saveStatus = null;
      help.repeat();
    }
  }, 2500);
}

export { boot, paint, act };

function meta() {
  return {
    title: "Handle",
    desc: "Customize your @handle colors.",
  };
}

export { meta };
