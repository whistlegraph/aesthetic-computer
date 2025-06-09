// Make, 25.06.05.00.01
// Make a piece by typing.

/* #region 📓 TODO
  + Now
  - [] Clean up code.
  - [] Fail if prompt is empty / provide a sane default.
  + Later
  - [] Add sound based on animation?
  - [] Add time-based animation parameters.
  - [] Condense the syntax and adjust the prompt.
  - [] Make another prompt based tool or a conversation tool.
       (So this can be abstracted.)
  - [] How can I somehow make a character editor?
  - [] And have two characters talk to one another in turns, adding a human?
  - [] ASCII graphics with pixel manipulation.
  + Done
  - [x] Use Claude 4 Sonnet model.
  - [x] Direct pixel buffer manipulation approach.
  - [x] Animation-focused prompting.
#endregion */

import { Conversation } from "../lib/ask.mjs";

let conversation,
  executeCode,
  fullCode = "PROCESSING...",
  currentCode = "",
  abort,
  frameCount = 0,
  lastValidCode = "",
  chunkCount = 0,
  scrollOffset = 0, // For scrolling through long code
  currentEvalLine = 0, // Track which line is currently being evaluated
  streaming = false, // Track if we're actively streaming
  completionTime = 0; // Track when streaming completed

const DEBUG_MODE = false; // Set to true for detailed logging
const TEXT_SCALE = 1.0; // Normal text scale for better readability
const LINE_HEIGHT = 12; // Normal line height for scale 1.0
const RENDER_SCALE = 0.5; // Render at 50% resolution for performance (0.25 for quarter resolution)

// 🥾 Boot (Runs once before first paint and sim)
async function boot({ params, store, slug }) {
  const program = {
    before: `You generate JavaScript for interactive pixel animation. Available:
    - screen.pixels: Uint8ClampedArray (RGBA, 4 bytes/pixel)  
    - screen.width, screen.height: numbers
    - frameCount: animation counter
    - pen: {x, y, drawing} - mouse/touch position and button state (scaled to buffer)

    Set pixel (x,y) to color (r,g,b,a):
    const i = (y * screen.width + x) * 4;
    screen.pixels[i] = r; screen.pixels[i+1] = g; screen.pixels[i+2] = b; screen.pixels[i+3] = 255;

    If the prompt does not specify mouse interaction or a game character, use pen.x, pen.y, and pen.drawing to make the output interactive in a creative and context-sensitive way. For example, let the mouse position determine a threshold, direction, or color split (e.g., for 'half red half blue', use the mouse to control the dividing line or orientation). Use pen.drawing to explore different modes, parameters, or visual states - it can toggle effects, change speeds, reverse directions, switch color palettes, modify intensity, or transform the entire visual behavior. Do not default to drawing a cursor or highlight. If the prompt describes a game or interactive object (like a paddle, player, or ball), use pen for control, but do not add an extra cursor. If the prompt describes a game or interactive object, implement actual game mechanics and rules (e.g., collision, scoring, movement, physics) rather than just animating objects.`,
    // user input
    after: `

OUTPUT: Pure JavaScript only. No markdown, comments, or explanations. Compress all whitespace - use minimal spaces, no unnecessary line breaks, compact formatting.

TECHNICAL:
- Always set alpha = 255 (opaque)
- fill all pixels in the backbuffer unless specified otherwise
- Use frameCount for animation
- Use pen.x, pen.y for mouse position (clamped to screen bounds, scaled to buffer)
- Use pen.drawing (boolean) for mouse button/touch state (defaults to false)
- Stay in bounds: 0 to screen.width-1, screen.height-1
- Optimize: cache calculations, avoid expensive operations
- No 'line' function exists - implement Bresenham's line algorithm if line drawing is needed
- Only screen.pixels manipulation is available for drawing

STYLE: 
- Terse code, short variables (x,y,i,j,r,g,b,w,h,t)
- Compress whitespace: no unnecessary spaces, minimal line breaks, compact formatting
- Minimal code for games and interactive sketches
- Mathematical beauty over logic
- Fullscreen effects that fill every pixel
- Interactive elements that respond to mouse/touch

VISUALS:
- Unless specified use Vibrant, dynamic color 
- Mouse interaction: trails, ripples, brushes, attractors, repulsors
- Multiple objects should generally be simulated separately and not animated
  in groups with historical cos and sin tricks.
- Always be mindful of the aspect ratio of the screen and try not to
  stretch stuff or take that into account.    INTERACTIVITY (unless specified otherwise):
    - Always include some mouse/touch response, (could be decorative if unspecified)
    - Use pen.x, pen.y for continuous parameters (position, thresholds, color splits, intensity)
    - Use pen.drawing as a creative toggle/mode switch: change visual states, reverse effects, switch algorithms, modify speeds, toggle color palettes, or transform behavior entirely
    - When pen.drawing is toggled, you can also adjust how pen.x and pen.y are interpreted or used to better fit the new mode or behavior (e.g., different coordinate systems, scaling factors, or interaction patterns)
    - If the prompt describes a game or interactive object (like a paddle, player, or ball), use pen for control, but do not add an extra cursor
    - If the prompt describes a game or interactive object, implement actual game mechanics and rules (e.g., collision, scoring, movement, physics) rather than just animating objects
    - Examples: draw at cursor, affect nearby pixels, create ripples, bend waves, pen.drawing for mode changes
    - Don't ever include the \`\`\`javascript\`\`\` prefix in your output.

Generate executable JavaScript:`,
  };

  conversation = new Conversation(store, slug);
  await conversation.retrieve();

  abort = conversation.ask(
    {
      prompt:
        params.join(" ") ||
        "swirling colorful plasma effect that follows the mouse",
      program,
      hint: "code:claude-sonnet-4-20250514", // Use Claude Sonnet 4
    },
    function and(msg) {
      if (fullCode === "PROCESSING...") {
        fullCode = ``; // Clear any waiting message.
        streaming = true; // Mark that we're actively streaming
      }
      fullCode += msg;
      currentCode += msg;
      chunkCount++;

      // Try to evaluate code more frequently for better streaming feedback (was every 5, now every 3)
      if (chunkCount % 3 === 0) {
        tryExecuteCurrentCode();
      }
    },
    function done() {
      // Mark streaming as complete
      streaming = false;
      completionTime = frameCount;

      if (fullCode && fullCode.trim()) {
        tryExecuteCurrentCode(true); // Final attempt with full cleanup
      }
    },
    function fail() {
      fullCode = "NETWORK FAILURE";
    },
  );
}

// 🎨 Paint (Executes every display frame)
function paint({
  wipe,
  ink,
  screen,
  write,
  painting,
  paste,
  pen,
  text,
  hud: { currentLabel },
}) {
  frameCount++; // Increment frame counter for animations

  // Create a custom screen buffer for the animation
  let animationBuffer = null;

  // Execute the animation code in a custom buffer (if available)
  if (executeCode) {
    try {
      // Create custom buffer at reduced resolution for performance
      const bufferWidth = Math.ceil(screen.width * RENDER_SCALE);
      const bufferHeight = Math.ceil(screen.height * RENDER_SCALE);

      animationBuffer = painting(
        bufferWidth,
        bufferHeight,
        ({ wipe, screen: bufferScreen }) => {
          // Execute the AI-generated animation code on the custom buffer
          // Ensure the buffer screen has the expected interface
          const screenInterface = {
            pixels: bufferScreen.pixels,
            width: bufferScreen.width,
            height: bufferScreen.height,
          };
          // Scale pen coordinates to match the buffer resolution

          // console.log(pen.button, pen.drawing);

          const scaledPen = pen
            ? {
                x: Math.floor(pen.x * RENDER_SCALE),
                y: Math.floor(pen.y * RENDER_SCALE),
                drawing: pen.drawing || false,
              }
            : { x: 0, y: 0, drawing: false };
          executeCode(screenInterface, frameCount, scaledPen);
        },
      );
    } catch (err) {
      if (DEBUG_MODE) console.log("❌ Animation execution error:", err);

      // Create error buffer with black background at reduced resolution
      const bufferWidth = Math.floor(screen.width * RENDER_SCALE);
      const bufferHeight = Math.floor(screen.height * RENDER_SCALE);

      // 🟠 TODO: Make this animation buffer memoized. 25.06.09.07.53
      animationBuffer = painting(bufferWidth, bufferHeight, ({ wipe }) => {
        // wipe("black");
      });

      // If we have a fallback, try to use it
      if (lastValidCode && lastValidCode !== currentCode) {
        try {
          executeCode = new Function(
            "screen",
            "frameCount",
            "pen",
            lastValidCode,
          );
          if (DEBUG_MODE)
            console.log("🔄 Attempting to recover with last valid code");
        } catch (fallbackErr) {
          if (DEBUG_MODE)
            console.log("❌ Recovery failed, disabling execution");
          executeCode = null; // Disable execution to prevent crash loop
        }
      } else {
        executeCode = null; // Disable execution to prevent crash loop
      }
    }
  }

  wipe("purple");

  // If we have an animation buffer, paste it to the main screen (stretched to fit)
  if (animationBuffer) {
    // Use transform object to specify exact target dimensions
    paste(animationBuffer, 0, 0, {
      scale: 1 / RENDER_SCALE, // Scale to upscale the reduced resolution buffer back to full size
    });
  } else {
    // No animation yet, show purple background
    wipe("purple");
  }

  // Show status while loading or if no valid code to execute
  if (
    fullCode &&
    (!executeCode ||
      fullCode.startsWith("COMPILATION ERROR:") ||
      fullCode === "NETWORK FAILURE")
  ) {
    if (
      fullCode.startsWith("COMPILATION ERROR:") ||
      fullCode === "NETWORK FAILURE"
    ) {
      // Show error message
      ink("red").write(fullCode, { x: 20, y: 50 });
    } else if (fullCode !== "PROCESSING...") {
      // Show the generated code while it's being received (overlay on animation if present)
      if (!executeCode) {
        // wipe("black"); // Only wipe if no animation is running
      }
      // Display scrolling code buffer, passing hud for correct vertical offset
      displayScrollingCodeBuffer(
        screen,
        ink,
        fullCode,
        false,
        { currentLabel },
        text,
      );
    }
    // Note: No center message for "PROCESSING..." - rely on bottom corner indicator
  } else if (
    executeCode &&
    fullCode &&
    !fullCode.startsWith("COMPILATION ERROR:") &&
    fullCode !== "NETWORK FAILURE" &&
    fullCode !== "PROCESSING..."
  ) {
    // Always show code preview on top of animation (if code exists and is executing)
    // Display scrolling code buffer with live evaluation highlighting, passing hud for correct vertical offset
    displayScrollingCodeBuffer(
      screen,
      ink,
      fullCode,
      true,
      { currentLabel },
      text,
    );
  }
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave() {
  abort?.(); // Cancel any existing `ask` which halts the server.
}

export { boot, paint, leave };

// 📚 Library (Useful functions used throughout the piece)

// Display the entire source code as it grows with scrolling and character wrapping
function displayScrollingCodeBuffer(
  screen,
  ink,
  code,
  showEvaluation = false,
  hud,
  text,
) {
  if (!code || !code.trim()) return;

  // Determine vertical offset based on hud.currentLabel
  let codeStartY = hud.currentLabel.btn.box.h + 11;

  // Only show code overlay while streaming
  if (!streaming) return;

  // Calculate available space for code display
  const availableHeight = screen.height - codeStartY - 20; // Leave space for status indicators

  // Create semi-transparent background for the available code area

  // Display the entire code with character wrapping
  const textAlpha = 200;
  const bounds = screen.width - 12; // Leave margin for character wrapping

  // Determine how much text we can fit and truncate from the beginning if needed
  let displayCode = code.trim();

  // Check if we need to truncate the beginning of the text
  let textBounds = text.box(
    displayCode,
    { x: 8, y: codeStartY, size: TEXT_SCALE },
    bounds,
  );

  if (textBounds.box.height > availableHeight) {
    // Split into lines and remove from the beginning until it fits
    const lines = displayCode.split("\n");
    let fittingLines = lines;

    while (fittingLines.length > 0) {
      const testText = fittingLines.join("\n");
      const testBounds = text.box(
        testText,
        { x: 8, y: codeStartY, size: TEXT_SCALE },
        bounds,
      );

      if (testBounds.box.height <= availableHeight) {
        displayCode = testText;
        textBounds = testBounds;
        break;
      }

      // Remove the first line and try again
      fittingLines = fittingLines.slice(1);
    }
  }

  // Draw background for the code area
  //const codeBg = { r: 0, g: 0, b: 0, a: Math.round(180 * fadeAlpha) };
  //drawCodeBackground(screen, 5, codeStartY - 5, screen.width - 10, availableHeight, codeBg);

  ink(0).box(textBounds.box);

  // Render the text at the fixed position
  ink(180, 180, 180, textAlpha).write(
    displayCode,
    { x: 8, y: codeStartY, size: TEXT_SCALE },
    undefined,
    bounds,
  );

  // Show status indicators
  // Show streaming indicator (only when actively streaming)
  if (streaming) {
    const dots = ".".repeat((Math.floor(frameCount / 10) % 3) + 1);
    ink(0, 255, 0, 255).write(`Making${dots}`, {
      x: 10,
      y: screen.height - 15,
      size: 1,
    });
  }
}

function tryExecuteCurrentCode(isFinal = false) {
  if (!currentCode || currentCode.trim().length < 10) return;

  try {
    // Clean up the code - remove any markdown formatting
    let cleanCode = currentCode.trim();

    // Remove all types of markdown code blocks - enhanced filtering
    cleanCode = cleanCode
      .replace(/^```[a-zA-Z]*\n?/gm, "")
      .replace(/\n?```$/gm, "");

    // Remove any remaining ``` markers that might appear inline
    cleanCode = cleanCode.replace(/```/g, "");

    // Remove specific javascript block markers that might appear early
    cleanCode = cleanCode.replace(/^javascript\s*\n/gm, "");

    // Don't try to execute if it has obvious incomplete syntax
    if (!isFinal && hasIncompleteStructures(cleanCode)) {
      return;
    }

    // Update current evaluation line for highlighting
    const codeLines = cleanCode.split("\n");
    currentEvalLine = codeLines.length - 1; // Highlight the last (most recent) line

    // Try to compile the code - now with pen parameter
    const testFunction = new Function("screen", "frameCount", "pen", cleanCode);

    // If compilation succeeds, update our execute function
    executeCode = testFunction;
    lastValidCode = cleanCode;

    if (isFinal) {
      currentEvalLine = -1; // Clear highlighting when done
      streaming = false; // Ensure streaming is marked as complete
      if (completionTime === 0) completionTime = frameCount; // Set completion time if not already set
    }
  } catch (err) {
    // Silently ignore compilation errors during streaming
    // unless it's the final attempt
    if (isFinal) {
      currentEvalLine = -1; // Clear highlighting on error
      streaming = false; // Mark as complete even on error
      if (completionTime === 0) completionTime = frameCount;

      // Try to fall back to the last valid code
      if (lastValidCode) {
        try {
          executeCode = new Function(
            "screen",
            "frameCount",
            "pen",
            lastValidCode,
          );
        } catch (fallbackErr) {
          fullCode = "COMPILATION ERROR: " + err.message;
        }
      } else {
        fullCode = "COMPILATION ERROR: " + err.message;
      }
    }
    // During streaming, just ignore errors and continue
  }
}

function hasIncompleteStructures(code) {
  // During streaming, be more permissive - only block obviously broken syntax
  // Check for severely unmatched braces or parens (allow some imbalance during streaming)
  const openBraces = (code.match(/\{/g) || []).length;
  const closeBraces = (code.match(/\}/g) || []).length;
  const openParens = (code.match(/\(/g) || []).length;
  const closeParens = (code.match(/\)/g) || []).length;

  // Allow moderate imbalance during streaming (was 2, now 5)
  if (
    Math.abs(openBraces - closeBraces) > 5 ||
    Math.abs(openParens - closeParens) > 5
  ) {
    return true;
  }

  // Only check for the most obviously incomplete statements
  // Remove overly strict checks that prevent streaming execution
  const trimmedCode = code.trim();

  // Check for incomplete function declarations only if they're at the very end
  if (trimmedCode.endsWith("function ") || trimmedCode.endsWith("function("))
    return true;

  // Allow partial for/if statements - they might be building up
  // if (code.includes('for (') && !code.includes(') {')) return true;
  // if (code.includes('if (') && !code.includes(') {')) return true;

  return false;
}
