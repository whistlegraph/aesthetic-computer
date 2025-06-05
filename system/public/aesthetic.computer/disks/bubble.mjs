// 🧋 Bubble 2022.02.04.17.46 [Sage: @mxsage + Jeffrey]
// Create a visual bubble when tapped or clicked. Bubbles float up off the
// screen and the pitch or size of the bubble is related to where you tap.

/* TODO
  - [x] Clean up interface.
  - [x] Remove keyboard shortcuts.
  - [x] Pause simulation during touch hold.
*/


// Enhanced physical modeling with multi-parameter touch control:
// - Touch down: Start bubble sound with sustain
// - Hold & drag: Real-time parameter updates while sound continues
// - Release: Stop sound with natural fade out
// - X position controls stereo pan and wave harmonics
// - Y position controls surface tension (pitch) and viscosity (damping)
// - Distance from center controls bubble size (fundamental frequency)
// - Touch velocity affects attack time and amplitude modulation
// - Keyboard controls: arrow keys for global params, Q/W/E for presets

let bub, progress;
let originalRadius = 50;
let radius = originalRadius;

// Physical modeling state
let activeSounds = new Map(); // Track active bubble sounds for real-time updates
let globalAttack = 0.01;
let globalDecay = 0.8;
let currentBubbleId = null; // Track the current active bubble for updates

// Global state
let showHelp = false;

// Touch parameter state
let currentParams = {
  radius: 50,
  soundRadius: 0.01,
  rise: 1.0,
  volume: 0.5,
  pan: 0,
};

// Visual feedback
let parameterDisplay = {
  fadeAlpha: 0,
};

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ resize, screen, cursor, glaze, density }) {
  // cursor("none");
  // TODO: Runs only once!
  // resize(50, 20);

  originalRadius = screen.width / 3;
  radius = originalRadius;
  glaze({ on: true, type: "prompt" });
}

// 💗 Beat (Runs once per bpm)
function beat({ sound: { bpm, bubble, time } }) {
  bpm(2000);

  // The beat function now primarily handles timing
  // Individual bubble sounds are managed in real-time through the activeSounds Map
  // This allows for more responsive sound control without periodic beat interruptions
}

// 🧮 Sim(ulate)
function sim({ sound: { time, bubble }, num, screen, pointer }) {
  // Only update parameters from pointer if not currently holding down touch
  // This prevents simulation from interfering with sustained bubble sounds
  if (pointer && !currentBubbleId) {
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    const maxDimension = Math.max(screen.width / 2, screen.height / 2);

    // Calculate distance from center for bubble size (0 to 1, where 1 = edge)
    let distanceFromCenter = num.dist(pointer.x, pointer.y, centerX, centerY);
    const normalizedDistance = num.clamp(
      distanceFromCenter / maxDimension,
      0.01,
      1.0,
    );

    // Map X position to pan (-1 to 1)
    const pan = num.map(pointer.x, 0, screen.width, -1, 1);

    // Map Y position to buoyancy/rise (inverted: top = high buoyancy, bottom = low buoyancy)
    const rise = num.map(pointer.y, 0, screen.height, 4.0, 0.2);

    // Map radial distance to bubble radius (center = small tight bubbles, edge = large loose bubbles)
    const bubbleRadius = num.map(normalizedDistance, 0, 1, 3, 25);

    // Calculate volume based on distance from edge (edge = max volume for more interaction)
    const edgeDistance = Math.min(
      pointer.x,
      pointer.y,
      screen.width - pointer.x,
      screen.height - pointer.y,
    );
    const normalizedEdgeDistance =
      (edgeDistance / Math.min(screen.width, screen.height)) * 2;
    const volume = num.clamp(normalizedEdgeDistance + 0.3, 0.3, 1.0);

    // Update current parameters for real-time visualization
    currentParams = {
      radius: normalizedDistance * maxDimension * 0.8 + 20, // Visual radius (minimum 20px)
      soundRadius: bubbleRadius, // Audio radius parameter
      rise: rise,
      volume: volume,
      pan: pan,
    };

    // Update visual radius more responsively during hover
    radius = num.lerp(radius, currentParams.radius, 0.15);

    // Keep parameter display visible when mouse is moving
    if (parameterDisplay.fadeAlpha < 0.6) {
      parameterDisplay.fadeAlpha = 0.6;
    }
  }  
  // Fade out parameter display when mouse is not active
  if (parameterDisplay.fadeAlpha > 0) {
    parameterDisplay.fadeAlpha = num.lerp(parameterDisplay.fadeAlpha, 0, 0.02);
  }
  
  // Keep visual radius stable during touch interactions
  if (!currentBubbleId) {
    // Only animate radius when not touching
    radius = num.lerp(radius, originalRadius, 0.05);
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ ink, plot, wipe, screen, num, write }) {
  const x = (progress || 0) * screen.width;
  wipe(128);

  // Main bubble visualization
  ink(0, 0, 255).circle(screen.width / 2, screen.height / 2, radius);

  ink(255).circle(
    screen.width / 2 - radius / 3,
    screen.height / 2 - radius / 3,
    radius / 4,
  ); // Parameter visualization
  if (parameterDisplay.fadeAlpha > 0.1) {
    const alpha = Math.floor(parameterDisplay.fadeAlpha * 255);

    // Visual indicators for each parameter
    // Pan indicator (horizontal line)
    const panX = screen.width / 2 + (currentParams.pan * screen.width) / 3;
    ink(255, 100, 100, alpha).line(panX - 20, 120, panX + 20, 120);
    write(`Pan: ${currentParams.pan.toFixed(2)}`, panX - 30, 135, [
      255,
      100,
      100,
      alpha,
    ]); // Rise indicator (vertical line)
    const riseY = screen.height - (currentParams.rise * 50 + 100);
    ink(100, 255, 100, alpha).line(30, riseY - 20, 30, riseY + 20);
    write(`Buoyancy: ${currentParams.rise.toFixed(2)}`, 5, riseY + 25, [
      100,
      255,
      100,
      alpha,
    ]); // Radius indicator (circle outline)
    ink(100, 100, 255, alpha).circle(
      screen.width - 60,
      60,
      Math.max(currentParams.radius / 4, 5),
      "outline",
    );
    write(
      `Size: ${(currentParams.soundRadius || 0).toFixed(1)}`,
      screen.width - 90,
      90,
      [100, 100, 255, alpha],
    );

    // Volume indicator (bar)
    const volumeWidth = currentParams.volume * 80;
    ink(255, 255, 100, alpha).box(
      screen.width - 100,
      screen.height - 50,
      volumeWidth,
      10,
    );
    write(
      `Vol: ${currentParams.volume.toFixed(2)}`,
      screen.width - 100,
      screen.height - 30,
      [255, 255, 100, alpha],
    );

    // Attack/Decay indicators
    const attackWidth = globalAttack * 1000; // Scale for visibility
    const decayWidth = globalDecay * 80;
    ink(255, 150, 150, alpha).box(
      screen.width - 100,
      screen.height - 80,
      Math.min(attackWidth, 80),
      5,
    );
    ink(150, 150, 255, alpha).box(
      screen.width - 100,
      screen.height - 70,
      decayWidth,
      5,
    );
    write(
      `A:${globalAttack.toFixed(3)} D:${globalDecay.toFixed(2)}`,
      screen.width - 100,
      screen.height - 60,
      [200, 200, 200, alpha],
    );
  }
  // Touch position indicator
  if (parameterDisplay.fadeAlpha > 0.3) {
    const alpha = Math.floor(parameterDisplay.fadeAlpha * 128);
    ink(255, 255, 255, alpha).circle(
      screen.width / 2 + (currentParams.pan * screen.width) / 3,
      screen.height - (currentParams.rise * 50 + 50),
      5,
    );
  }

  // Help display
  if (showHelp) {
    const helpAlpha = 200;
    ink(0, 0, 0, helpAlpha).box(20, 20, screen.width - 40, screen.height - 40);
    ink(255, 255, 255).box(
      22,
      22,
      screen.width - 44,
      screen.height - 44,
      "outline",
    );
    const helpText = [
      "🧋 BUBBLE CONTROLS",
      "",
      "TOUCH/MOUSE:",
      "• Touch down: Start bubble sound",
      "• Hold & drag: Sustain with real-time control",
      "• Release: Stop sound",
      "• Move (no touch): Preview parameters",
      "",
      "SOUND PARAMETERS:",
      "• X position: Pan (left/right)",
      "• Y position: Buoyancy (up=high, down=low)",
      "• Distance from center: Bubble size",
      "• Distance from edge: Volume",
      "",
      "KEYBOARD:",
      "• ↑↓: Adjust attack time",
      "• ←→: Adjust decay time",
      "• Q: Soft bubble preset",
      "• W: Sharp bubble preset",
      "• E: Experimental preset",
      "• R: Reset to defaults",
      "• T: Test sound at center",
      "• H: Toggle this help",
      "",
      "Press H to close",
    ];

    let y = 40;
    helpText.forEach((line) => {
      if (line.startsWith("🧋")) {
        ink(100, 200, 255).write(line, 40, y);
      } else if (line.endsWith(":")) {
        ink(255, 200, 100).write(line, 40, y);
      } else {
        ink(255, 255, 255).write(line, 40, y);
      }
      y += 16;
    });
  }
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, num, screen, sound: { bubble } }) {
  if (e.is("touch")) {
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    const maxDimension = Math.max(screen.width / 2, screen.height / 2);

    // Calculate distance from center for bubble size
    let distanceFromCenter = num.dist(e.x, e.y, centerX, centerY);
    const normalizedDistance = num.clamp(
      distanceFromCenter / maxDimension,
      0.01,
      1.0,
    );

    // Map X position to pan (-1 to 1)
    const pan = num.map(e.x, 0, screen.width, -1, 1);

    // Map Y position to buoyancy/rise (inverted: top = high buoyancy, bottom = low buoyancy)
    const rise = num.map(e.y, 0, screen.height, 4.0, 0.2);

    // Map radial distance to bubble radius (center = small tight bubbles, edge = large loose bubbles)
    const bubbleRadius = num.map(normalizedDistance, 0, 1, 3, 25);

    // Calculate volume based on distance from edge (edge = max volume for more interaction)
    const edgeDistance = Math.min(
      e.x,
      e.y,
      screen.width - e.x,
      screen.height - e.y,
    );
    const normalizedEdgeDistance =
      (edgeDistance / Math.min(screen.width, screen.height)) * 2;
    const volume = num.clamp(normalizedEdgeDistance + 0.3, 0.3, 1.0);

    // Store current parameters for visualization
    currentParams = {
      radius: normalizedDistance * maxDimension * 0.8 + 20, // Visual radius (minimum 20px)
      soundRadius: bubbleRadius,
      rise: rise,
      volume: volume,
      pan: pan,
    };

    // Update visual radius
    radius = currentParams.radius;
    // Show parameter display
    parameterDisplay.fadeAlpha = 1.0;

    // Create new bubble with ID for tracking
    currentBubbleId = Date.now(); // Simple ID generation
    // Create bubble with mapped parameters and ID
    const bubbleInstance = bubble({
      radius: currentParams.soundRadius,
      rise: rise,
      volume: volume,
      pan: pan,
    });

    // Enable sustain mode for continuous sound during drag
    if (bubbleInstance && bubbleInstance.enableSustain) {
      bubbleInstance.enableSustain();
    }

    // Track the bubble instance
    activeSounds.set(currentBubbleId, bubbleInstance);
  }
  if (e.is("draw")) {
    // Real-time parameter updates while dragging
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    const maxDimension = Math.max(screen.width / 2, screen.height / 2);

    // Update parameters based on current touch position
    let distanceFromCenter = num.dist(e.x, e.y, centerX, centerY);
    const normalizedDistance = num.clamp(
      distanceFromCenter / maxDimension,
      0.01,
      1.0,
    );

    // Map X position to pan (-1 to 1)
    const pan = num.map(e.x, 0, screen.width, -1, 1);

    // Map Y position to buoyancy/rise (inverted: top = high buoyancy, bottom = low buoyancy)
    const rise = num.map(e.y, 0, screen.height, 4.0, 0.2);

    // Map radial distance to bubble radius (center = small tight bubbles, edge = large loose bubbles)
    const bubbleRadius = num.map(normalizedDistance, 0, 1, 3, 25);

    // Calculate volume based on distance from edge (edge = max volume for more interaction)
    const edgeDistance = Math.min(
      e.x,
      e.y,
      screen.width - e.x,
      screen.height - e.y,
    );
    const normalizedEdgeDistance =
      (edgeDistance / Math.min(screen.width, screen.height)) * 2;
    const volume = num.clamp(normalizedEdgeDistance + 0.3, 0.3, 1.0);

    // Update current parameters for visualization
    currentParams = {
      radius: normalizedDistance * maxDimension * 0.8 + 20, // Visual radius (minimum 20px)
      soundRadius: bubbleRadius,
      rise: rise,
      volume: volume,
      pan: pan,
    };

    // Update visual radius immediately for responsive feedback
    radius = currentParams.radius;
    parameterDisplay.fadeAlpha = 1.0;

    // Update existing bubble instead of creating new ones
    if (currentBubbleId && activeSounds.has(currentBubbleId)) {
      const bubbleInstance = activeSounds.get(currentBubbleId);

      // Update the bubble parameters in real-time
      bubbleInstance.update({
        radius: currentParams.soundRadius,
        rise: currentParams.rise,
        volume: currentParams.volume,
        pan: currentParams.pan,
        duration: 0.05, // Faster transition for more responsive feel
      });
    }
  }
  if (e.is("lift")) {
    // Clean up active bubble on lift
    if (currentBubbleId && activeSounds.has(currentBubbleId)) {
      const bubbleInstance = activeSounds.get(currentBubbleId); // Disable sustain mode to allow natural fade out
      if (bubbleInstance.disableSustain) {
        bubbleInstance.disableSustain();
      }
      activeSounds.delete(currentBubbleId);
    }

    currentBubbleId = null;
    radius = originalRadius;
    // Keep parameter display visible for a moment after lift
    parameterDisplay.fadeAlpha = 0.7;
  }
  if (e.is("move")) {
    // Provide visual feedback even without clicking
    const centerX = screen.width / 2;
    const centerY = screen.height / 2;
    const maxDimension = Math.max(screen.width / 2, screen.height / 2);

    let distanceFromCenter = num.dist(e.x, e.y, centerX, centerY);
    const normalizedDistance = num.clamp(
      distanceFromCenter / maxDimension,
      0.01,
      1.0,
    );

    // Map X position to pan (-1 to 1)
    const pan = num.map(e.x, 0, screen.width, -1, 1);

    // Map Y position to buoyancy/rise (inverted: top = high buoyancy, bottom = low buoyancy)
    const rise = num.map(e.y, 0, screen.height, 4.0, 0.2);

    // Map radial distance to bubble radius (center = small tight bubbles, edge = large loose bubbles)
    const bubbleRadius = num.map(normalizedDistance, 0, 1, 3, 25);

    // Calculate volume based on distance from edge (edge = max volume for more interaction)
    const edgeDistance = Math.min(
      e.x,
      e.y,
      screen.width - e.x,
      screen.height - e.y,
    );
    const normalizedEdgeDistance =
      (edgeDistance / Math.min(screen.width, screen.height)) * 2;
    const volume = num.clamp(normalizedEdgeDistance + 0.3, 0.3, 1.0);

    // Update current parameters for preview visualization
    currentParams = {
      radius: normalizedDistance * maxDimension * 0.8 + 20, // Visual radius (minimum 20px)
      soundRadius: bubbleRadius, // Audio parameter
      rise: rise,
      volume: volume,
      pan: pan,
    };

    // Update visual bubble radius more responsively during mouse movement
    radius = num.lerp(radius, currentParams.radius, 0.2);

    // Show parameter display on hover with stronger presence
    if (parameterDisplay.fadeAlpha < 0.4) {
      parameterDisplay.fadeAlpha = 0.4;
    }
  } // Keyboard controls
  if (e.is("key")) {
    let parameterChanged = true;

    // Global parameter adjustments (arrow keys)
    if (e.key === "ArrowUp") {
      globalAttack = num.clamp(globalAttack + 0.01, 0.001, 1);
    } else if (e.key === "ArrowDown") {
      globalAttack = num.clamp(globalAttack - 0.01, 0.001, 1);
    } else if (e.key === "ArrowRight") {
      globalDecay = num.clamp(globalDecay + 0.05, 0.1, 5);
    } else if (e.key === "ArrowLeft") {
      globalDecay = num.clamp(globalDecay - 0.05, 0.1, 5);
    }

    // Quick parameter presets (Q, W, E, R, T)
    else if (e.key === "q") {
      // Soft bubble preset
      globalAttack = 0.05;
      globalDecay = 2.0;
    } else if (e.key === "w") {
      // Sharp bubble preset
      globalAttack = 0.001;
      globalDecay = 0.5;
    } else if (e.key === "e") {
      // Experimental preset
      globalAttack = 0.02;
      globalDecay = 1.5;
    } else if (e.key === "r") {
      // Reset to defaults
      globalAttack = 0.01;
      globalDecay = 0.8;
    } else if (e.key === "h") {
      // Toggle help display
      showHelp = !showHelp;
    } else if (e.key === "t") {
      // Test sound at center
      const testBubbleId = Date.now();
      const testBubble = bubble({
        radius: 10,
        rise: 2,
        volume: 0.5,
        pan: 0,
      });

      // Track the test bubble briefly
      activeSounds.set(testBubbleId, testBubble);

      // Clean up test bubble after a delay
      setTimeout(() => {
        activeSounds.delete(testBubbleId);
      }, 3000);
    } else {
      parameterChanged = false;
    }

    if (parameterChanged) {
      parameterDisplay.fadeAlpha = 1.0;
    }
  }
}

// 📚 Library (Useful classes & functions used throughout the piece)
// ...

export { boot, sim, paint, act, beat };
