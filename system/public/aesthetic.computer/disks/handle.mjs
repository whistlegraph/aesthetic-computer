// Handle, 2026.02.13.03.00
// Customize your @handle colors - per-character RGB customization with dynamic shadows.

/* #region 🏁 TODO
  - [ ] Add "Reset to default" button
  - [ ] Add color presets/themes
  - [ ] Add copy/share color scheme feature
#endregion */

const { floor, max, min } = Math;

let handle; // User's handle
let colors = []; // RGB colors for @ sign + each character [{r, g, b}, ...]
let selectedCharIndex = 0; // Currently selected character for editing
let saveBtn; // Save button
let loading = true;
let saveStatus = null; // "saving", "saved", "error"
let lastSaveTime = 0;
let hoveredCharIndex = -1; // Track hovered character

// Slider config - larger and more responsive
const sliderConfig = {
  x: 30,
  y: 180,
  width: 250,
  height: 20,
  gap: 35,
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
      loading = false;
    })
    .catch((err) => {
      console.warn("⚠️ Failed to load handle colors, using defaults:", err);
      // Default to white for all characters
      colors = handleWithAt.split("").map(() => ({ r: 255, g: 255, b: 255 }));
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

  // 🎨 Draw title with gradient effect
  ink(150, 180, 255).write("Customize Your Handle", { x: 20, y: 15 });
  ink(100, 140, 200).write("Click any character to edit its color", {
    x: 20,
    y: 28,
  }, undefined, undefined, false, "MatrixChunky8");

  // 🌈 Draw handle preview LARGE - with both normal and MatrixChunky8
  const previewY = 55;
  const normalCharWidth = 6; // Normal font width
  const normalCharHeight = 12; // Normal font height
  const handleWidth = handleWithAt.length * normalCharWidth;
  const previewStartX = floor((screen.width - handleWidth) / 2);

  // Draw normal font version
  handleWithAt.split("").forEach((char, i) => {
    const x = previewStartX + i * normalCharWidth;
    const color = colors[i];

    // Draw shadow (darker, offset)
    const shadowR = floor(color.r * 0.25);
    const shadowG = floor(color.g * 0.25);
    const shadowB = floor(color.b * 0.25);
    ink(shadowR, shadowG, shadowB, 200).write(char, { x: x + 2, y: previewY + 2 });

    // Draw character with color
    ink(color.r, color.g, color.b).write(char, { x, y: previewY });

    // Highlight selected or hovered character
    if (i === selectedCharIndex) {
      ink(255, 255, 0, 120).box(x - 1, previewY - 1, normalCharWidth + 2, normalCharHeight + 2);
    } else if (i === hoveredCharIndex) {
      ink(255, 255, 255, 60).box(x - 1, previewY - 1, normalCharWidth + 2, normalCharHeight + 2);
    }
  });

  // Draw MatrixChunky8 version below (larger, pixelated)
  const chunkyY = previewY + 25;
  const chunkyCharWidth = 8;
  const chunkyCharHeight = 14;
  const chunkyHandleWidth = handleWithAt.length * chunkyCharWidth;
  const chunkyStartX = floor((screen.width - chunkyHandleWidth) / 2);

  handleWithAt.split("").forEach((char, i) => {
    const x = chunkyStartX + i * chunkyCharWidth;
    const color = colors[i];

    // Draw shadow
    const shadowR = floor(color.r * 0.25);
    const shadowG = floor(color.g * 0.25);
    const shadowB = floor(color.b * 0.25);
    ink(shadowR, shadowG, shadowB, 200)
      .write(char, { x: x + 2, y: chunkyY + 2 }, undefined, undefined, false, "MatrixChunky8");

    // Draw character
    ink(color.r, color.g, color.b)
      .write(char, { x, y: chunkyY }, undefined, undefined, false, "MatrixChunky8");

    // Highlight selected or hovered
    if (i === selectedCharIndex) {
      ink(255, 255, 0, 100).box(x - 2, chunkyY - 2, chunkyCharWidth + 4, chunkyCharHeight + 4, "outline");
    } else if (i === hoveredCharIndex) {
      ink(255, 255, 255, 50).box(x - 2, chunkyY - 2, chunkyCharWidth + 4, chunkyCharHeight + 4, "outline");
    }
  });

  // 📝 Instructions
  const selectedChar = handleWithAt[selectedCharIndex];
  ink(180, 200, 255).write(
    `Editing: '${selectedChar}' (${selectedCharIndex + 1}/${handleWithAt.length})`,
    { x: sliderConfig.x, y: sliderConfig.y - 25 },
    undefined,
    undefined,
    false,
    "MatrixChunky8"
  );

  // 🎚️ RGB Sliders for selected character - BIGGER
  const currentColor = colors[selectedCharIndex];

  // R slider
  drawSlider(
    ink,
    write,
    "R",
    sliderConfig.x,
    sliderConfig.y,
    sliderConfig.width,
    sliderConfig.height,
    currentColor.r,
    [255, 0, 0]
  );

  // G slider
  drawSlider(
    ink,
    write,
    "G",
    sliderConfig.x,
    sliderConfig.y + sliderConfig.gap,
    sliderConfig.width,
    sliderConfig.height,
    currentColor.g,
    [0, 255, 0]
  );

  // B slider
  drawSlider(
    ink,
    write,
    "B",
    sliderConfig.x,
    sliderConfig.y + sliderConfig.gap * 2,
    sliderConfig.width,
    sliderConfig.height,
    currentColor.b,
    [0, 0, 255]
  );

  // RGB preview box
  const previewBoxX = sliderConfig.x + sliderConfig.width + 30;
  const previewBoxY = sliderConfig.y;
  const previewBoxSize = sliderConfig.height * 3 + sliderConfig.gap * 2;
  ink(currentColor.r, currentColor.g, currentColor.b).box(
    previewBoxX,
    previewBoxY,
    previewBoxSize,
    previewBoxSize,
    "fill"
  );
  ink(255, 255, 255, 150).box(
    previewBoxX,
    previewBoxY,
    previewBoxSize,
    previewBoxSize,
    "outline"
  );

  // 💾 Save button
  if (!saveBtn) {
    saveBtn = new ui.TextButton("Save Colors", {
      center: "x",
      y: screen.height - 50,
      screen,
    });
  } else {
    saveBtn.reposition({ center: "x", y: screen.height - 50, screen });
  }

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
      y: screen.height - 30,
      screen,
    }, undefined, undefined, false, "MatrixChunky8");
  }
}

function drawSlider(ink, write, label, x, y, width, height, value, color) {
  // Label
  ink(220, 220, 220).write(`${label}:`, {
    x: x - 20,
    y: y + floor(height / 2) - 6,
  }, undefined, undefined, false, "MatrixChunky8");

  // Track background
  ink(40, 40, 50).box(x, y, width, height, "fill");
  ink(80, 80, 90).box(x, y, width, height, "outline");

  // Fill (shows current value)
  const fillWidth = floor((value / 255) * width);
  ink(color[0], color[1], color[2], 180).box(x + 1, y + 1, fillWidth - 1, height - 2, "fill");

  // Handle (draggable indicator)
  const handleX = x + fillWidth - 3;
  ink(color).box(handleX, y - 3, 6, height + 6, "fill");
  ink(255, 255, 255).box(handleX, y - 3, 6, height + 6, "outline");

  // Value display
  ink(255, 255, 255).write(value.toString().padStart(3, " "), {
    x: x + width + 15,
    y: y + floor(height / 2) - 6,
  }, undefined, undefined, false, "MatrixChunky8");
}

function act({ event: e, screen, net, help, jump, sound }) {
  if (loading) return;

  const handleWithAt = "@" + handle;

  // Update hover state for normal font preview
  const previewY = 55;
  const normalCharWidth = 6;
  const normalCharHeight = 12;
  const handleWidth = handleWithAt.length * normalCharWidth;
  const previewStartX = floor((screen.width - handleWidth) / 2);

  // Update hover state for MatrixChunky8 preview
  const chunkyY = previewY + 25;
  const chunkyCharWidth = 8;
  const chunkyCharHeight = 14;
  const chunkyHandleWidth = handleWithAt.length * chunkyCharWidth;
  const chunkyStartX = floor((screen.width - chunkyHandleWidth) / 2);

  hoveredCharIndex = -1;

  // Check hover on normal font
  if (e.y >= previewY && e.y < previewY + normalCharHeight) {
    for (let i = 0; i < handleWithAt.length; i++) {
      const x = previewStartX + i * normalCharWidth;
      if (e.x >= x && e.x < x + normalCharWidth) {
        hoveredCharIndex = i;
        help.repeat();
        break;
      }
    }
  }

  // Check hover on chunky font
  if (e.y >= chunkyY && e.y < chunkyY + chunkyCharHeight) {
    for (let i = 0; i < handleWithAt.length; i++) {
      const x = chunkyStartX + i * chunkyCharWidth;
      if (e.x >= x && e.x < x + chunkyCharWidth) {
        hoveredCharIndex = i;
        help.repeat();
        break;
      }
    }
  }

  // Handle character selection (click on preview)
  if (e.is("lift") || e.is("touch")) {
    // Check click on normal font
    if (e.y >= previewY && e.y < previewY + normalCharHeight) {
      for (let i = 0; i < handleWithAt.length; i++) {
        const x = previewStartX + i * normalCharWidth;
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
      for (let i = 0; i < handleWithAt.length; i++) {
        const x = chunkyStartX + i * chunkyCharWidth;
        if (e.x >= x && e.x < x + chunkyCharWidth) {
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

    // Check save button (only on lift/touch, not draw)
    if (saveBtn && saveBtn.act) {
      saveColors(net, help, sound);
      return;
    }
  }

  // Handle slider dragging
  if (e.is("draw") || e.is("lift") || e.is("touch")) {
    const currentColor = colors[selectedCharIndex];
    const sliders = [
      { key: "r", y: sliderConfig.y },
      { key: "g", y: sliderConfig.y + sliderConfig.gap },
      { key: "b", y: sliderConfig.y + sliderConfig.gap * 2 },
    ];

    for (const slider of sliders) {
      if (
        e.y >= slider.y &&
        e.y < slider.y + sliderConfig.height &&
        e.x >= sliderConfig.x &&
        e.x < sliderConfig.x + sliderConfig.width
      ) {
        const relX = e.x - sliderConfig.x;
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

    if (response.ok || response.status === 200) {
      saveStatus = "saved";
      console.log(`✅ Colors saved successfully`);
      sound.synth({
        type: "sine",
        tone: 600,
        duration: 0.1,
        volume: 0.3,
      });
    } else {
      saveStatus = "error";
      console.error("❌ Failed to save handle colors:", response);
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

function meta() {
  return {
    title: "Handle Color Customization",
    desc: "Customize the colors of your @handle - per-character RGB control.",
  };
}

export { boot, paint, act, meta };
