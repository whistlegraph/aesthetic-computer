// Make, 25.06.05.00.01
// Ask Claude 4 Sonnet to produce JavaScript code that directly manipulates screen pixels for animated graphics!

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
    before: `You are generating JavaScript code for pixel buffer animation. You have access to:
- screen.pixels: Uint8ClampedArray (RGBA format, 4 bytes per pixel)
- screen.width: number (canvas width in pixels) 
- screen.height: number (canvas height in pixels)
- frameCount: number (animation frame counter, starts at 0)

IMPORTANT: You are rendering to a REDUCED RESOLUTION buffer (50% of full screen size) that gets automatically stretched to fill the full screen. This means you work with fewer pixels but achieve the same visual coverage with significantly better performance.

To set a pixel at (x, y) to color (r, g, b, a):
const index = (y * screen.width + x) * 4;
screen.pixels[index] = r;     // Red (0-255)
screen.pixels[index + 1] = g; // Green (0-255)
screen.pixels[index + 2] = b; // Blue (0-255)
screen.pixels[index + 3] = a; // Alpha (0-255)

EXAMPLE CODE STRUCTURE:
// Clear screen with opaque background (recommended)
for (let i = 0; i < screen.pixels.length; i += 4) {
  screen.pixels[i] = 0;     // R
  screen.pixels[i + 1] = 0; // G  
  screen.pixels[i + 2] = 0; // B
  screen.pixels[i + 3] = 255; // A (always 255 for opaque)
}

// Draw animation using frameCount
for (let y = 0; y < screen.height; y++) {
  for (let x = 0; x < screen.width; x++) {
    const index = (y * screen.width + x) * 4;
    // Your animation logic here with alpha = 255
  }
}

Create animated graphics showing: `,
    // user input  
    after: `

REQUIREMENTS:
- Generate ONLY pure JavaScript code - NO comments, NO explanations, NO markdown formatting
- Do NOT wrap code in \`\`\`javascript\`\`\` or \`\`\` blocks - output raw executable JavaScript only
- Do NOT include any markdown syntax, explanations, or descriptive text
- Use proper variable declarations (let, const, var)
- All code must be syntactically correct
- Use frameCount for animation timing
- Stay within screen bounds (0 to screen.width-1, 0 to screen.height-1)
- Use Math functions for interesting patterns (Math.sin, Math.cos, etc.)
- Create smooth looping animations
- ALWAYS set alpha channel to 255 for fully opaque pixels - no transparency
- ALWAYS clear/fill the entire screen buffer to prevent visual artifacts
- Start by clearing the screen with an opaque background before drawing
- NO graphics library functions available - only direct pixel array manipulation
- Inline ALL drawing algorithms (lines, circles, etc.) - implement Bresenham or other algorithms directly
- No external functions - everything must be self-contained within the generated code

PERFORMANCE REQUIREMENTS (CPU SOFTWARE RENDERING):
- OPTIMIZE FOR PERFORMANCE - this is CPU-based software rendering, every operation counts
- Minimize expensive operations like Math.sqrt, Math.pow, division - use bit operations when possible
- Cache calculations outside loops - precompute values that don't change per pixel
- Use efficient algorithms - avoid nested loops when possible, prefer linear iterations
- Use integer arithmetic instead of floating point when possible
- Minimize array lookups - cache frequently accessed values in local variables
- Use fast approximations for expensive functions (sin/cos lookup tables, etc.)
- Avoid redundant calculations - store intermediate results
- Consider memory access patterns - process pixels in optimal order
- Use bitwise operations for color manipulation when appropriate

STREAMING-FRIENDLY CODING STYLE:
- Write code that compiles and shows visual results incrementally as each line is added
- Start with simple pixel operations that immediately show output
- Build complexity gradually - begin with basic patterns before adding sophistication
- Use smaller, focused code blocks rather than large nested loops
- Prefer direct pixel manipulation over complex setup code
- Write pixels early and often - avoid long setup phases before visual output
- Structure code so partial execution produces meaningful visual results
- Consider reading existing pixel values when building upon previous frames
- Use efficient algorithms that can start showing results before completion
- Prioritize immediate visual feedback while maintaining high performance
- Cache expensive calculations outside of pixel loops for optimal CPU performance

CODING STYLE REQUIREMENTS:
- Use VERY SHORT variable names (single letters preferred: x, y, i, j, r, g, b, w, h, t, s, c, d, etc.)
- Write TERSE, POETIC code - minimize verbosity, maximize elegance
- Use classic demoscene/oldschool tricks and patterns
- Default to FULL SCREEN effects unless user specifies otherwise (tunnel effects, plasma, fractals, etc.)
- Embrace mathematical beauty - use trigonometry, iteration, recursion creatively
- Create hypnotic, screen-filling animations that captivate the entire display
- Think like classic computer graphics demos - maximize visual impact with minimal code
- Use nested loops efficiently to create complex patterns across the entire screen
- Prefer mathematical formulas over conditional logic where possible

VISUAL REQUIREMENTS:
- ALWAYS USE VIBRANT, DYNAMIC COLOR - never monochrome unless explicitly requested
- FILL THE ENTIRE SCREEN with visual variation - treat each pixel as part of a larger world
- Think like a PROJECTED FRACTAL or WORLD VIEW - as if looking through a portal into infinite mathematical space
- Use color gradients, shifts, and transitions to create depth and movement across the full display
- Vary colors spatially and temporally - different regions should have different hues/intensities
- Create the illusion of looking into vast mathematical landscapes, tunnels, or cosmic phenomena
- Use the full RGB spectrum creatively - mix colors mathematically for rich, evolving palettes
- Make every frame feel like a window into an infinite, colorful mathematical universe
- AVOID REPETITIVE SPIRAL PATTERNS - explore diverse visual forms: waves, grids, cellular automata, noise fields, interference patterns, geometric tessellations, flowing liquids, crystalline structures
- ELIMINATE BLACK/DEAD SPACE - every pixel should contribute meaningful color and pattern variation
- Use mathematical functions that naturally fill space: sine waves, noise functions, modular arithmetic, distance fields, polar coordinates

Generate ONLY executable JavaScript code optimized for incremental compilation, immediate visual results, maximum CPU performance, and poetic terseness:`,
  };

  conversation = new Conversation(store, slug);
  await conversation.retrieve();

  abort = conversation.ask(
    { 
      prompt: params.join(" ") || "swirling colorful plasma effect", 
      program, 
      hint: "code:claude-sonnet-4-20250514" // Use Claude Sonnet 4
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
    }
  );
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, screen, write, painting, paste }) {
  frameCount++; // Increment frame counter for animations

  // Create a custom screen buffer for the animation
  let animationBuffer = null;
  
  // Execute the animation code in a custom buffer (if available)
  if (executeCode) {
    try {
      // Create custom buffer at reduced resolution for performance
      const bufferWidth = Math.floor(screen.width * RENDER_SCALE);
      const bufferHeight = Math.floor(screen.height * RENDER_SCALE);
      
      animationBuffer = painting(bufferWidth, bufferHeight, ({ wipe, screen: bufferScreen }) => {
        // Execute the AI-generated animation code on the custom buffer
        // Ensure the buffer screen has the expected interface
        const screenInterface = {
          pixels: bufferScreen.pixels,
          width: bufferScreen.width,
          height: bufferScreen.height
        };
        executeCode(screenInterface, frameCount);
      });
    } catch (err) {
      if (DEBUG_MODE) console.log("❌ Animation execution error:", err);
      
      // Create error buffer with black background at reduced resolution
      const bufferWidth = Math.floor(screen.width * RENDER_SCALE);
      const bufferHeight = Math.floor(screen.height * RENDER_SCALE);

      animationBuffer = painting(bufferWidth, bufferHeight, ({ wipe }) => {
        wipe("black");
      });
      
      // If we have a fallback, try to use it
      if (lastValidCode && lastValidCode !== currentCode) {
        try {
          executeCode = new Function("screen", "frameCount", lastValidCode);
          if (DEBUG_MODE) console.log("🔄 Attempting to recover with last valid code");
        } catch (fallbackErr) {
          if (DEBUG_MODE) console.log("❌ Recovery failed, disabling execution");
          executeCode = null; // Disable execution to prevent crash loop
        }
      } else {
        executeCode = null; // Disable execution to prevent crash loop
      }
    }
  }

  // If we have an animation buffer, paste it to the main screen (stretched to fit)
  if (animationBuffer) {
    // Use transform object to specify exact target dimensions
    paste(animationBuffer, 0, 0, {
      scale: 1 / RENDER_SCALE // Scale to upscale the reduced resolution buffer back to full size
    });
  } else {
    // No animation yet, show purple background
    wipe("purple");
  }

  // Show status while loading or if no valid code to execute
  if (fullCode && (!executeCode || fullCode.startsWith("COMPILATION ERROR:") || fullCode === "NETWORK FAILURE")) {
    if (fullCode.startsWith("COMPILATION ERROR:") || fullCode === "NETWORK FAILURE") {
      // Show error message
      ink("red").write(fullCode, { x: 20, y: 50 });
    } else if (fullCode !== "PROCESSING...") {
      // Show the generated code while it's being received (overlay on animation if present)
      if (!executeCode) {
        // wipe("black"); // Only wipe if no animation is running
      }
      
      // Display scrolling code buffer
      displayScrollingCodeBuffer(screen, ink, fullCode);
    }
    // Note: No center message for "PROCESSING..." - rely on bottom corner indicator
  }

  // Always show code preview on top of animation (if code exists and is executing)
  if (executeCode && fullCode && !fullCode.startsWith("COMPILATION ERROR:") && fullCode !== "NETWORK FAILURE" && fullCode !== "PROCESSING...") {
    // Display scrolling code buffer with live evaluation highlighting
    displayScrollingCodeBuffer(screen, ink, fullCode, true);
  }
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave() {
  abort?.(); // Cancel any existing `ask` which halts the server.
}

export { boot, paint, leave };

// 📚 Library (Useful functions used throughout the piece)

// Display a scrolling code buffer with auto-scroll and live evaluation highlighting
function displayScrollingCodeBuffer(screen, ink, code, showEvaluation = false) {
  if (!code || !code.trim()) return;
  
  const codeLines = code.trim().split("\n");
  const maxLines = Math.floor((screen.height - 40) / LINE_HEIGHT); // Calculate how many lines fit
  const maxCharsPerLine = Math.floor(screen.width * 2 / TEXT_SCALE) - 20; // Account for scale
  
  // Auto-scroll: show the most recent lines that fit on screen
  const startLine = Math.max(0, codeLines.length - maxLines);
  const visibleLines = codeLines.slice(startLine, startLine + maxLines);
  
  // Calculate fade out effect after completion
  const timeSinceCompletion = completionTime > 0 ? frameCount - completionTime : 0;
  const fadeOutDuration = 120; // 2 seconds at 60fps
  const fadeAlpha = streaming ? 1.0 : Math.max(0, 1.0 - (timeSinceCompletion / fadeOutDuration));
  
  // Only show code overlay if still fading or still streaming
  if (fadeAlpha <= 0) return;
  
  // Create semi-transparent background for the entire code area
  const codeBg = { r: 0, g: 0, b: 0, a: Math.round(180 * fadeAlpha) };
  const bgHeight = visibleLines.length * LINE_HEIGHT + 10;
  drawCodeBackground(screen, 5, 15, screen.width - 10, bgHeight, codeBg);
  
  // Display each visible line
  visibleLines.forEach((line, row) => {
    const actualLineNumber = startLine + row;
    const y = 20 + row * LINE_HEIGHT;
    
    // Truncate line if too long
    const displayLine = line.substring(0, maxCharsPerLine);
    
    // Highlight current evaluation line if showing evaluation
    if (showEvaluation && actualLineNumber === currentEvalLine && streaming) {
      // Draw animated highlight background for current evaluation line (only when streaming)
      const pulseIntensity = Math.sin(frameCount * 0.2) * 20 + 40; // Pulsing effect
      const highlightBg = { 
        r: 60 + pulseIntensity, 
        g: 100 + pulseIntensity, 
        b: 60 + pulseIntensity, 
        a: Math.round(150 * fadeAlpha) 
      };
      drawCodeBackground(screen, 5, y - 2, screen.width - 10, LINE_HEIGHT + 2, highlightBg);
      
      // Draw arrow indicator and code separately for proper alignment
      const textAlpha = Math.round(255 * fadeAlpha);
      ink(255, 255, 0, textAlpha).write("→", { x: 8, y, size: TEXT_SCALE }); // Yellow arrow
      ink(255, 255, 255, textAlpha).write(displayLine, { x: 6, y, size: TEXT_SCALE }); // Code aligned left
    } else {
      // Normal code line with subtle fade-in effect for new lines
      const isRecentLine = actualLineNumber >= codeLines.length - 5;
      const baseOpacity = isRecentLine ? 200 : 150;
      const textAlpha = Math.round(baseOpacity * fadeAlpha);
      ink(180, 180, 180, textAlpha).write(displayLine, { x: 6, y, size: TEXT_SCALE }); // All code aligned left
    }
  });
  
  // Show scroll indicator and streaming status (only when visible)
  if (fadeAlpha > 0.3) {
    if (codeLines.length > maxLines) {
      const scrollPercent = Math.round((startLine / (codeLines.length - maxLines)) * 100);
      const indicatorAlpha = Math.round(255 * fadeAlpha);
      ink(255, 255, 0, indicatorAlpha).write(`↓ ${scrollPercent}% (${startLine + 1}-${startLine + visibleLines.length}/${codeLines.length} lines)`, 
        { x: screen.width - 200, y: screen.height - 15, size: 0.4 });
    }
    
    // Show streaming indicator (only when actively streaming)
    if (streaming && showEvaluation && currentEvalLine >= 0) {
      const dots = ".".repeat((Math.floor(frameCount / 10) % 3) + 1);
      const streamAlpha = Math.round(255 * fadeAlpha);
      ink(0, 255, 0, streamAlpha).write(`◉ making${dots}`, { x: 10, y: screen.height - 15, size: 1.0 });
    } else if (!streaming && fadeAlpha > 0.5) {
      // Show completion indicator briefly
      const completeAlpha = Math.round(200 * fadeAlpha);
      ink(0, 255, 255, completeAlpha).write("✓ COMPLETE", { x: 10, y: screen.height - 15, size: 1.0 });
    }
  }
}

// Draw background for code display area
function drawCodeBackground(screen, x, y, width, height, bgColor) {
  for (let py = y; py < y + height && py < screen.height; py++) {
    for (let px = x; px < x + width && px < screen.width; px++) {
      if (px >= 0 && py >= 0) {
        const index = (py * screen.width + px) * 4;
        // Blend with existing pixel using alpha
        const alpha = bgColor.a / 255;
        screen.pixels[index] = Math.round(screen.pixels[index] * (1 - alpha) + bgColor.r * alpha);
        screen.pixels[index + 1] = Math.round(screen.pixels[index + 1] * (1 - alpha) + bgColor.g * alpha);
        screen.pixels[index + 2] = Math.round(screen.pixels[index + 2] * (1 - alpha) + bgColor.b * alpha);
        screen.pixels[index + 3] = 255; // Keep full opacity
      }
    }
  }
}

function tryExecuteCurrentCode(isFinal = false) {
  if (!currentCode || currentCode.trim().length < 10) return;
  
  try {
    // Clean up the code - remove any markdown formatting
    let cleanCode = currentCode.trim();
    
    // Remove all types of markdown code blocks - enhanced filtering
    cleanCode = cleanCode.replace(/^```[a-zA-Z]*\n?/gm, '').replace(/\n?```$/gm, '');
    
    // Remove any remaining ``` markers that might appear inline
    cleanCode = cleanCode.replace(/```/g, '');
    
    // Remove specific javascript block markers that might appear early
    cleanCode = cleanCode.replace(/^javascript\s*\n/gm, '');
    
    // Don't try to execute if it has obvious incomplete syntax
    if (!isFinal && hasIncompleteStructures(cleanCode)) {
      return;
    }
    
    // Update current evaluation line for highlighting
    const codeLines = cleanCode.split('\n');
    currentEvalLine = codeLines.length - 1; // Highlight the last (most recent) line
    
    // Try to compile the code
    const testFunction = new Function("screen", "frameCount", cleanCode);
    
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
          executeCode = new Function("screen", "frameCount", lastValidCode);
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
  if (Math.abs(openBraces - closeBraces) > 5 || Math.abs(openParens - closeParens) > 5) {
    return true;
  }
  
  // Only check for the most obviously incomplete statements
  // Remove overly strict checks that prevent streaming execution
  const trimmedCode = code.trim();
  
  // Check for incomplete function declarations only if they're at the very end
  if (trimmedCode.endsWith('function ') || trimmedCode.endsWith('function(')) return true;
  
  // Allow partial for/if statements - they might be building up
  // if (code.includes('for (') && !code.includes(') {')) return true;
  // if (code.includes('if (') && !code.includes(') {')) return true;
  
  return false;
}
