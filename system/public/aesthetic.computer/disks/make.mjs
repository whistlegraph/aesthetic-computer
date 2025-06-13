// Make, 25.06.05.00.01 - Simplified Architecture
// Creates animated art from text prompts with sequential parameter highlighting

// Configuration flags (must be defined before any variable declarations that use them)
const ADAPTIVE_RESOLUTION_ENABLED = true; // Re-enabled for dynamic performance scaling
const DEBUG_MODE = true; // Set to true for detailed logging

// Dynamic resolution scaling constants (moved early for initialization order)
const MAX_RENDER_SCALE = 1.0; // Maximum full resolution

/* #region 📓 TODO
  + Now
  - [🟠] Show last generated piece when the same prompt loads / regeneration occurs.
    - [] Cache the existing source.
    - [] On boot, check if it's in the cache and run it while generating.
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
  completionTime = 0, // Track when streaming completed
  originalLabel = "", // Store the original HUD label text
  originalParams = [], // Store the original user params
  userPromptText = "", // Store the user prompt text for console logging
  atParams = [], // Store @-prefixed versions of params for direct matching
  currentParamIndex = 0, // Index of the currently highlighted param
  lastWordChangeTime = 0, // Track when the current word was last changed (in frameCount)
  processedWords = new Set(), // Track words that have already been processed
  foundParameters = new Set(), // Track which parameter indices have been found
  streamingEndTime = 0, // Track when streaming ended for pause duration
  labelRestored = false, // Track if we've restored the original label
  lastParameterFoundFrame = 0, // Track when the last parameter was found
  characterBuffer = "", // Buffer for incoming code chunks
  displayedCode = "", // Code that has been displayed character by character
  lastCharacterDisplayFrame = 0, // Last frame when we displayed a character
  scrollingDisplayBuffer = "", // Rolling buffer for scrolling text display
  charactersPerFrame = 1, // How many characters to show per frame (adjustable speed, up to 4 when catching up)
  state = {}, // Persistent state object that survives between frames
  finalCountdownStartTime = null, // Track when final countdown timer starts (UTC timestamp)
  finalCountdownDuration = 12000, // 12 seconds in milliseconds
  canRegenerate = false, // Flag to allow regeneration
  countdownHitZeroTime = null, // Track when countdown first hit 0 (UTC timestamp)
  regenerationTriggeredTime = 0, // Track when regeneration was triggered
  makingStartTime = 0, // Track when making (streaming) starts
  makingProgress = 0.0, // Current making progress (0.0 to 1.0)
  // Dynamic resolution scaling variables
  currentRenderScale = ADAPTIVE_RESOLUTION_ENABLED ? 0.25 : MAX_RENDER_SCALE, // Start at 25% resolution or max if disabled
  targetRenderScale = ADAPTIVE_RESOLUTION_ENABLED ? 0.25 : MAX_RENDER_SCALE, // Target scale for smooth interpolation
  frameTimings = [], // Array to store recent frame times
  lastFrameTime = 0, // Track last frame time for FPS calculation
  lastScaleAdjustment = 0, // Track when we last adjusted scale
  averageFPS = 30, // Running average FPS
  // Paste optimization cache
  lastCachedScale = -1, // Track last scale for cache invalidation
  cachedPasteParams = null, // Cache for paste calculations
  // Relative coordinate system for resolution-independent state persistence
  relativeStateStore = new Map(), // Internal storage for relative coordinates
  screenDimensions = { width: 0, height: 0 }; // Track current screen dimensions

const TEXT_SCALE = 1.0; // Normal text scale for better readability
const LINE_HEIGHT = 12; // Normal line height for scale 1.0
const PAUSE_DURATION = 0; // 4 seconds at 60fps to give enough time to read the final code
const MIN_CODE_DISPLAY_TIME = 120; // Minimum time to show code before cleanup (2 seconds)

// Dynamic resolution scaling - Enhanced for ultra-responsive performance adaptation
const TARGET_FPS = 30;
const MIN_RENDER_SCALE = 0.03; // Minimum 3% resolution - even more aggressive for extreme cases
const SCALE_ADJUSTMENT = 0.015; // Smaller base adjustments for smoother scaling
const SMOOTH_INTERPOLATION_SPEED = 0.12; // How fast to interpolate towards target (0.12 = 12% per frame)
const FPS_SAMPLE_SIZE = 6; // Smaller sample size for faster response
const SCALE_COOLDOWN = 30; // Faster normal cooldown
const CRITICAL_FPS_COOLDOWN = 6; // Ultra-fast critical response
const EMERGENCY_FPS_COOLDOWN = 3; // Nearly instant emergency response
const SEVERE_FPS_COOLDOWN = 1; // Immediate response for severe drops

// Styled console logging utility for consistent source code display
function logStyledSource(title, code, isError = false, userPrompt = "") {
  const codeStyle = isError
    ? "background: yellow; color: red; font-weight: normal; margin: 4px 0; padding: 4px; font-family: monospace; border-radius: 2px; white-space: pre-wrap; font-size: 8px; line-height: 1.0; border: 1px solid rgba(255, 0, 0, 0.5);"
    : "background: purple; color: rgba(255, 255, 0, 1); font-weight: bold; margin: 4px 0; padding: 4px; font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace; border-radius: 2px; white-space: pre-wrap; font-size: 8px; line-height: 1.2; border: 1px solid rgba(255, 255, 255, 0.5);";

  // Get timestamp
  const timestamp = new Date().toLocaleTimeString();

  // Create title comment
  const titleComment =
    userPrompt && userPrompt.trim() ? `made ${userPrompt}` : `Generated Code`;

  // Add title comment at the beginning and timestamp comment at the end
  const codeWithComments = `/* ${titleComment} */\n${code}\n/* ${timestamp} */`;

  console.log(`%c${codeWithComments}`, codeStyle);
}

// 🥾 Boot (Runs once before first paint and sim)
function boot({ glaze, params, store, slug, hud: { label, currentLabel } }) {
  // glaze({ on: true });
  // Store the original label text and label function
  originalLabel = currentLabel().text || "";

  lastValidCode = store["make-last-code"] || ""; // Load last valid code from store

  // Store the original params for simple highlighting
  originalParams = [...params];
  userPromptText = params.join(" "); // Store the full user prompt for console logging
  atParams = params.map((param) => `/* @${param} */`); // Create multiline comment versions
  currentParamIndex = -1; // Start with "make" highlighted (-1 means no parameter, just "make")
  lastWordChangeTime = 0;
  processedWords.clear(); // Reset processed words
  foundParameters.clear(); // Reset found parameters
  streamingEndTime = 0;
  labelRestored = false; // Reset label restoration flag
  lastParameterFoundFrame = 0; // Reset last parameter found frame
  characterBuffer = ""; // Reset character buffer
  displayedCode = ""; // Reset displayed code
  lastCharacterDisplayFrame = 0;
  state = {}; // Reset persistent state for new prompt
  finalCountdownStartTime = null; // Reset countdown timer
  canRegenerate = false; // Reset regeneration flag
  countdownHitZeroTime = null; // Reset countdown hit zero time
  regenerationTriggeredTime = 0; // Reset regeneration triggered time
  makingStartTime = 0; // Reset making start time
  makingProgress = 0.0; // Reset making progress
  // Reset dynamic scaling variables
  currentRenderScale = ADAPTIVE_RESOLUTION_ENABLED ? 0.25 : MAX_RENDER_SCALE; // Reset to 25% or max if disabled
  targetRenderScale = ADAPTIVE_RESOLUTION_ENABLED ? 0.25 : MAX_RENDER_SCALE; // Reset target scale too
  frameTimings = []; // Clear frame timing history
  lastFrameTime = 0; // Reset frame time tracking
  lastScaleAdjustment = 0; // Reset scale adjustment tracking
  averageFPS = 30; // Reset average FPS
  // Reset paste optimization cache
  lastCachedScale = -1; // Invalidate cache
  cachedPasteParams = null; // Clear cached parameters

  // Load the prompt template from the .prompt file synchronously
  let programSource;
  let promptLoaded = false;

  if (lastValidCode.length > 0) {
    currentCode = lastValidCode;
    tryExecuteCurrentCode(false, label, store);
  }

  fetch("../prompts/make.prompt")
    .then((response) => response.text())
    .then((text) => {
      programSource = text;
      promptLoaded = true;
      initializeConversation();
    })
    .catch((error) => {
      console.error("Failed to load make.prompt:", error);
      programSource = `You are a code poet that makes art. Create JavaScript for: "{{{ USER INPUT }}}"`;
      promptLoaded = true;
      initializeConversation();
    });

  function initializeConversation() {
    const program = {
      before: programSource.split("{{{ USER INPUT }}}")[0] || "",
      after: programSource.split("{{{ USER INPUT }}}")[1] || "",
    };

    conversation = new Conversation(store, slug);
    conversation.retrieve().then(() => {
      // Extract just the prompt part (remove "make" command) for the AI
      const promptForAI = params.join(" ");

      abort = conversation.ask(
        {
          prompt: promptForAI,
          program,
          hint: "code:claude-sonnet-4-20250514",
          // hint: "claude-opus-4-20250514",
          // hint: "code",
          // hint: "code:", // Use Claude Sonnet 4
          temperature: 1,
        },
        function and(msg) {
          if (fullCode === "PROCESSING...") {
            fullCode = ``; // Clear any waiting message.
            currentCode = ``; // Clear the current code
            // Don't clear executeCode here if we have lastValidCode - let it keep running
            // executeCode = null; // Clear the old execution function so new code can take over
            streaming = true; // Mark that we're actively streaming
            makingStartTime = frameCount; // Track when making starts
            makingProgress = 0.0; // Reset making progress
            label(); // Hide the HUD label while processing
          }

          // Check if the incoming message is a compilation error from token limit
          if (msg.startsWith("COMPILATION ERROR: Maximum tokens reached")) {
            // Handle token limit error specially - just show ERROR
            streaming = false; // Mark streaming as complete
            makingProgress = 1.0; // Mark making as complete

            // Only log the generated code if we have any
            if (fullCode && fullCode.trim()) {
              logStyledSource("", fullCode, true, userPromptText);
              console.log("Maximum tokens reached - response truncated");
            }

            // Set simple error message for display
            fullCode = "ERROR";
            currentCode = "ERROR";
            characterBuffer = ""; // Clear character buffer
            displayedCode = "ERROR"; // Set displayed code to just ERROR
            streamingEndTime = frameCount; // Start cleanup timer
            return; // Don't process as normal code
          }

          fullCode += msg;
          currentCode += msg;
          chunkCount++;

          // Add incoming message to character buffer for smooth character-by-character display
          characterBuffer += msg;

          // Try to evaluate code less frequently for safer streaming (was every 3, now every 8)
          // This reduces the chance of trying to execute incomplete structures
          // Only try to execute during streaming if we don't have valid code already running
          if (chunkCount % 8 === 0 && (!executeCode || !lastValidCode)) {
            tryExecuteCurrentCode(undefined, label, store);
          }
        },
        function done() {
          // Mark streaming as complete
          streaming = false;
          makingProgress = 1.0; // Mark making as complete
          completionTime = frameCount;
          // Don't set streamingEndTime here - let the character buffer finish first
          // streamingEndTime = frameCount; // This will be set automatically when character buffer is empty

          // Don't reset param tracking here - keep the last highlighted parameter visible
          // currentParamIndex = 0;
          // processedWords.clear();

          // Don't restore the original HUD label here - wait for pause to expire
          // label(originalLabel);

          if (fullCode && fullCode.trim()) {
            tryExecuteCurrentCode(true, label, store); // Final attempt with full cleanup
          }
        },
        function fail() {
          fullCode = "NETWORK FAILURE";
          streaming = false;
          makingProgress = 1.0; // Mark making as complete even on failure
          // Don't set streamingEndTime here either - let character buffer finish
          // streamingEndTime = frameCount; // Track when streaming ended

          // Don't reset param tracking on failure either - keep last state
          // currentParamIndex = 0;
          // processedWords.clear();

          // Don't restore the original HUD label on failure here - wait for pause to expire
          // label(originalLabel);
        },
      );
    });
  }
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
  typeface,
  clock,
  hud: { label, currentLabel },
}) {
  frameCount++;

  // Track FPS and adjust render scale dynamically
  const currentTime = clock.time();
  if (currentTime && lastFrameTime > 0) {
    const frameTime = currentTime.getTime() - lastFrameTime;
    frameTimings.push(frameTime);

    // Keep only recent frame timings
    if (frameTimings.length > FPS_SAMPLE_SIZE) {
      frameTimings.shift();
    }

    // Calculate average FPS over recent frames
    if (frameTimings.length >= 3) {
      // Need even fewer samples for ultra-fast response
      const averageFrameTime =
        frameTimings.reduce((sum, time) => sum + time, 0) / frameTimings.length;
      averageFPS = 1000 / averageFrameTime; // Convert to FPS

      // Adaptive resolution scaling with smooth interpolation
      if (ADAPTIVE_RESOLUTION_ENABLED) {
        // Enhanced dynamic scaling logic with ultra-responsive multi-tier system
        const timeSinceLastAdjustment = frameCount - lastScaleAdjustment;

        // Determine urgency level and required cooldown with more granular tiers
        let requiredCooldown;
        let scalingMode = "";

        if (averageFPS < 5) {
          // Severe: Catastrophic FPS, immediate action
          requiredCooldown = SEVERE_FPS_COOLDOWN;
          scalingMode = "SEVERE";
        } else if (averageFPS < 10) {
          // Emergency: Very low FPS, almost immediate scaling
          requiredCooldown = EMERGENCY_FPS_COOLDOWN;
          scalingMode = "EMERGENCY";
        } else if (averageFPS < 20) {
          // Critical: Low FPS, fast scaling
          requiredCooldown = CRITICAL_FPS_COOLDOWN;
          scalingMode = "CRITICAL";
        } else if (averageFPS < 25) {
          // Poor: Below target, normal scaling
          requiredCooldown = SCALE_COOLDOWN;
          scalingMode = "POOR";
        } else {
          // Normal or good FPS
          requiredCooldown = SCALE_COOLDOWN;
          scalingMode = "NORMAL";
        }

        if (timeSinceLastAdjustment > requiredCooldown) {
          let targetScaleChanged = false;

          // Determine target scale based on FPS performance
          if (averageFPS < 5 && targetRenderScale > MIN_RENDER_SCALE) {
            // Severe: Catastrophic FPS - large reduction to target
            const reductionAmount = Math.max(0.08, targetRenderScale * 0.4); // 40% reduction or 8% minimum
            targetRenderScale = Math.max(MIN_RENDER_SCALE, targetRenderScale - reductionAmount);
            targetScaleChanged = true;
            if (DEBUG_MODE) {
              console.log(`💀 SEVERE: Target scale reduced to ${Math.ceil(targetRenderScale * 100)}% (FPS: ${averageFPS.toFixed(1)})`);
            }
          } else if (averageFPS < 10 && targetRenderScale > MIN_RENDER_SCALE) {
            // Emergency: Very low FPS - significant reduction
            const reductionAmount = Math.max(0.05, targetRenderScale * 0.25); // 25% reduction or 5% minimum
            targetRenderScale = Math.max(MIN_RENDER_SCALE, targetRenderScale - reductionAmount);
            targetScaleChanged = true;
            if (DEBUG_MODE) {
              console.log(`🚨 EMERGENCY: Target scale reduced to ${Math.ceil(targetRenderScale * 100)}% (FPS: ${averageFPS.toFixed(1)})`);
            }
          } else if (averageFPS < 20 && targetRenderScale > MIN_RENDER_SCALE) {
            // Critical: Low FPS - moderate reduction
            const reductionAmount = averageFPS < 12 ? SCALE_ADJUSTMENT * 3 : 
                                   averageFPS < 15 ? SCALE_ADJUSTMENT * 2 : 
                                   SCALE_ADJUSTMENT * 1.5;
            targetRenderScale = Math.max(MIN_RENDER_SCALE, targetRenderScale - reductionAmount);
            targetScaleChanged = true;
          } else if (averageFPS < 25 && targetRenderScale > MIN_RENDER_SCALE) {
            // Poor: Below target - gentle reduction
            const reductionAmount = averageFPS < 22 ? SCALE_ADJUSTMENT * 1.2 : SCALE_ADJUSTMENT;
            targetRenderScale = Math.max(MIN_RENDER_SCALE, targetRenderScale - reductionAmount);
            targetScaleChanged = true;
          } else if (averageFPS > 35 && targetRenderScale < MAX_RENDER_SCALE) {
            // Good FPS: Increase target scale
            let increaseAmount;

            if (averageFPS > 50) {
              // Excellent FPS: Fast increases
              increaseAmount = SCALE_ADJUSTMENT * 3;
            } else if (averageFPS > 42) {
              // Very good FPS: Moderate increases
              increaseAmount = SCALE_ADJUSTMENT * 2;
            } else if (averageFPS > 38) {
              // Good FPS: Normal increases
              increaseAmount = SCALE_ADJUSTMENT * 1.5;
            } else {
              // Stable FPS: Gradual increases
              increaseAmount = SCALE_ADJUSTMENT;
            }

            targetRenderScale = Math.min(MAX_RENDER_SCALE, targetRenderScale + increaseAmount);
            targetScaleChanged = true;
          }

          if (targetScaleChanged) {
            lastScaleAdjustment = frameCount;
          }
        }

        // Smooth interpolation towards target scale every frame
        if (Math.abs(targetRenderScale - currentRenderScale) > 0.001) {
          const scaleDifference = targetRenderScale - currentRenderScale;
          const interpolationStep = scaleDifference * SMOOTH_INTERPOLATION_SPEED;
          
          // Ensure we don't overshoot the target
          if (Math.abs(interpolationStep) < Math.abs(scaleDifference)) {
            currentRenderScale += interpolationStep;
          } else {
            currentRenderScale = targetRenderScale;
          }
          
          // Clamp to valid range
          currentRenderScale = Math.max(MIN_RENDER_SCALE, Math.min(MAX_RENDER_SCALE, currentRenderScale));
        }
      }
    }
  }
  if (currentTime) {
    lastFrameTime = currentTime.getTime();
  }

  // Update making progress based on parameter highlighting progress
  if (streaming && originalParams) {
    const totalSections = 1 + originalParams.length;
    const sectionsCompleted = Math.max(0, currentParamIndex + 1);
    makingProgress = sectionsCompleted / totalSections;
  }

  // Process character buffer for smooth streaming effect
  processCharacterBuffer();

  // Create a custom screen buffer for the animation
  let animationBuffer = null;

  // Execute the animation code in a custom buffer (if available)
  if (executeCode) {
    try {
      // Create custom buffer at dynamically scaled resolution for performance
      const bufferWidth = Math.ceil(screen.width * currentRenderScale);
      const bufferHeight = Math.ceil(screen.height * currentRenderScale);

      // Update screen dimensions for coordinate system
      screenDimensions.width = screen.width;
      screenDimensions.height = screen.height;

      animationBuffer = painting(
        bufferWidth,
        bufferHeight,
        ({ wipe, screen: bufferScreen }) => {
          // Execute the AI-generated animation code on the custom buffer
          const screenInterface = {
            pixels: bufferScreen.pixels,
            width: bufferScreen.width,
            height: bufferScreen.height,
          };

          // Create resolution-aware pen with both scaled and relative coordinates
          let finalPen = createScaledPen(
            pen,
            screen.width,
            screen.height,
            currentRenderScale,
          );

          // Auto-animate drawing actions when pen is not active
          if (!pen || (!pen.drawing && !pen.x && !pen.y)) {
            // Create automatic pen simulation using real time instead of frame count
            const currentTime = clock.time();
            const autoTime = currentTime
              ? (currentTime.getTime() % 60000) / 1000
              : 0; // 60 second cycle in seconds
            const centerX = Math.floor(bufferWidth / 2);
            const centerY = Math.floor(bufferHeight / 2);

            // Create different movement patterns based on time
            const patternIndex = Math.floor(autoTime / 10) % 4; // Change pattern every 10 seconds
            let autoX, autoY;

            switch (patternIndex) {
              case 0: // Circular motion
                const radius1 = Math.min(centerX, centerY) * 0.3;
                autoX = centerX + Math.cos(autoTime) * radius1;
                autoY = centerY + Math.sin(autoTime * 0.7) * radius1;
                break;
              case 1: // Figure-8 pattern
                const scale = Math.min(centerX, centerY) * 0.25;
                autoX = centerX + Math.sin(autoTime) * scale;
                autoY = centerY + Math.sin(autoTime * 2) * scale;
                break;
              case 2: // Linear sweep
                const sweepProgress = (autoTime % 6) / 6; // 6-second cycle
                autoX = centerX * 0.3 + sweepProgress * centerX * 1.4;
                autoY =
                  centerY +
                  Math.sin(sweepProgress * Math.PI * 2) * centerY * 0.2;
                break;
              case 3: // Random walk
                const walkSpeed = 0.1;
                autoX =
                  centerX +
                  Math.sin(autoTime * walkSpeed) * centerX * 0.4 +
                  Math.cos(autoTime * walkSpeed * 1.3) * centerX * 0.2;
                autoY =
                  centerY +
                  Math.cos(autoTime * walkSpeed * 0.7) * centerY * 0.4 +
                  Math.sin(autoTime * walkSpeed * 1.7) * centerY * 0.2;
                break;
              default:
                autoX = centerX;
                autoY = centerY;
            }

            // Simulate drawing with varied patterns - drawing ~50% of the time with pulses
            const drawingCycle =
              Math.sin(autoTime * 0.3) * Math.cos(autoTime * 0.17); // Complex wave
            const autoDrawing = drawingCycle > 0.2; // Drawing when wave is above threshold

            finalPen = {
              x: Math.floor(autoX),
              y: Math.floor(autoY),
              drawing: autoDrawing,
              relativeX: autoX / bufferWidth,
              relativeY: autoY / bufferHeight,
              originalX: autoX / currentRenderScale,
              originalY: autoY / currentRenderScale,
            };
          }

          // Create resolution-aware state proxy that handles coordinate conversion
          const relativeState = createRelativeStateProxy(
            state,
            screen.width,
            screen.height,
          );

          // Calculate time-based animation parameter for generated code
          const currentTime = clock.time();
          const animationTime = currentTime
            ? (currentTime.getTime() % 60000) / 1000
            : frameCount * 0.016; // Fallback to ~60fps timing

          executeCode(
            screenInterface,
            frameCount,
            finalPen,
            relativeState,
            animationTime,
          );
        },
      );
    } catch (err) {
      // Create error buffer with black background at dynamically scaled resolution
      const bufferWidth = Math.ceil(screen.width * currentRenderScale);
      const bufferHeight = Math.ceil(screen.height * currentRenderScale);

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
            "state",
            "animationTime",
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

  // If we have an animation buffer, paste it to the main screen (optimized, full coverage)
  if (animationBuffer) {
    // Optimization: Check if we're at full resolution for direct paste
    if (Math.abs(currentRenderScale - 1.0) < 0.001) {
      // Full resolution - direct paste without scaling (fastest path)
      paste(animationBuffer, 0, 0);
    } else {
      // Scaled resolution - paste at full screen coverage (no centering)
      const scaleThreshold = 0.001; // Only recalculate if scale changed significantly
      
      if (Math.abs(currentRenderScale - lastCachedScale) > scaleThreshold || !cachedPasteParams) {
        // Recalculate paste parameters for center-based scaling
        const bufferWidth = animationBuffer.width;
        const bufferHeight = animationBuffer.height;
        
        // Calculate scale factors needed to cover full screen dimensions
        const scaleX = screen.width / bufferWidth;
        const scaleY = screen.height / bufferHeight;
        
        // Use the larger scale factor and add aggressive safety margin to guarantee complete coverage
        const baseScale = Math.max(scaleX, scaleY);
        const upscaleFactor = baseScale * 1.05; // Increase to 5% safety margin for better edge coverage
        
        // Calculate scaled dimensions with final scale
        const scaledWidth = bufferWidth * upscaleFactor;
        const scaledHeight = bufferHeight * upscaleFactor;
        
        // Calculate center offsets but ensure they never create gaps
        let offsetX = (screen.width - scaledWidth) / 2;
        let offsetY = (screen.height - scaledHeight) / 2;
        
        // Clamp offsets to ensure scaled content always covers entire screen
        // If offset is positive, it means scaled content is smaller than screen (shouldn't happen with safety margin)
        // If offset is negative, ensure it's negative enough to cover the screen completely
        if (offsetX > 0) {
          offsetX = 0; // Force to cover from left edge
        } else if (offsetX + scaledWidth < screen.width) {
          offsetX = screen.width - scaledWidth; // Ensure right edge is covered
        }
        
        if (offsetY > 0) {
          offsetY = 0; // Force to cover from top edge
        } else if (offsetY + scaledHeight < screen.height) {
          offsetY = screen.height - scaledHeight; // Ensure bottom edge is covered
        }
        
        // Cache the calculated parameters
        cachedPasteParams = {
          upscaleFactor,
          offsetX,
          offsetY
        };
        lastCachedScale = currentRenderScale;
        
        // Debug logging to understand scaling behavior
        // if (DEBUG_MODE && Math.abs(baseScale - upscaleFactor) > 0.001) {
        //   console.log(`🔍 Coverage scaling: ${baseScale.toFixed(3)} → ${upscaleFactor.toFixed(3)} (buffer: ${bufferWidth}x${bufferHeight}, screen: ${screen.width}x${screen.height}, offset: ${offsetX.toFixed(1)},${offsetY.toFixed(1)}, coverage: ${scaledWidth.toFixed(1)}x${scaledHeight.toFixed(1)})`);
        // }
      }
      
      // Use cached parameters for paste operation - guaranteed full coverage with center scaling
      paste(animationBuffer, cachedPasteParams.offsetX, cachedPasteParams.offsetY, {
        scale: cachedPasteParams.upscaleFactor,
      });
    }
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
      fullCode === "NETWORK FAILURE" ||
      fullCode === "ERROR"
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
        displayedCode, // Use character-by-character displayed code
        false,
        { currentLabel },
        text,
        typeface,
        label, // Pass label function for cleanup
        clock, // Pass clock for UTC timing
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
      displayedCode, // Use character-by-character displayed code
      true,
      { currentLabel },
      text,
      typeface,
      label, // Pass label function for cleanup
      clock, // Pass clock for UTC timing
    );
  }

  // Handle making progress bar (green, fills during streaming)
  if (streaming) {
    // Draw green progress bar at bottom of screen divided into sections
    const barWidth = screen.width - 12; // 6px margin on each side
    const barHeight = 4;
    const barX = 6;
    const barY = screen.height - 12; // Match the remaking bar position

    // Calculate total sections: 1 for "make" + originalParams.length
    const totalSections = 1 + (originalParams ? originalParams.length : 0);
    const sectionWidth = barWidth / totalSections;

    // Background (dark gray)
    ink(40, 40, 40).box(barX, barY, barWidth, barHeight);

    // Calculate discrete section-based progress (only advance when sections are completed)
    let totalProgressWidth = 0;

    // Fill completed sections fully (only sections that are fully done)
    if (currentParamIndex >= -1) {
      const completedSections = currentParamIndex + 1; // -1 becomes 0, 0 becomes 1, etc.
      totalProgressWidth = Math.floor(completedSections * sectionWidth);
    }

    // No partial progress within sections - bar only advances when each section is completed
    // Ensure we don't exceed the bar width
    totalProgressWidth = Math.min(totalProgressWidth, barWidth);

    // Fill the progress bar continuously from left to right
    if (totalProgressWidth > 0) {
      ink(60, 255, 60).box(barX, barY, totalProgressWidth, barHeight);
    }

    // Show making text with shadow
    const currentTime = clock.time();
    const timeMs = currentTime ? currentTime.getMilliseconds() : 0;
    const dots = ".".repeat((Math.floor(timeMs / 333) % 3) + 1); // 333ms cycle for ~3 dots per second
    // Show "Remaking..." if there was already stored code, otherwise "Making..."
    const isRemaking = lastValidCode && lastValidCode.length > 0;
    const makingText = `${isRemaking ? "Remaking" : "Making"}${dots}`;
    // Draw shadow (black text offset by 1px down and right)
    ink(0, 0, 0).write(makingText, {
      x: barX + 1,
      y: barY - 15 + 1,
      size: 1,
    });
    // Draw main text
    ink(255, 255, 255).write(makingText, {
      x: barX,
      y: barY - 15,
      size: 1,
    });
  }

  // Handle final countdown timer and regeneration
  if (finalCountdownStartTime !== null) {
    const currentTime = clock.time();
    if (currentTime) {
      const elapsedMs = currentTime.getTime() - finalCountdownStartTime;
      const progress = Math.min(elapsedMs / finalCountdownDuration, 1.0);

      // Draw red progress bar at bottom of screen
      const barWidth = screen.width - 12; // 6px margin on each side
      const barHeight = 4;
      const barX = 6;
      const barY = screen.height - 12;

      // Background (dark gray)
      ink(40, 40, 40).box(barX, barY, barWidth, barHeight);

      // Decreasing red bar: start full and shrink from right to left
      const remainingProgress = 1.0 - progress; // Invert: 1.0 -> 0.0
      const fillWidth = Math.floor(barWidth * remainingProgress);
      if (fillWidth > 0) {
        // Draw from the left side, shrinking from the right
        ink(255, 60, 60).box(barX, barY, fillWidth, barHeight);
      }

      // Show countdown text
      const remainingSeconds = Math.max(
        0,
        Math.ceil((finalCountdownDuration - elapsedMs) / 1000),
      );

      if (remainingSeconds > 0) {
        const remakingText = `Remaking in ${remainingSeconds}s`;
        // Draw shadow (black text offset by 1px down and right)
        ink(0, 0, 0).write(remakingText, {
          x: barX + 1,
          y: barY - 15 + 1,
          size: 1,
        });
        // Draw main text
        ink(255, 255, 255).write(remakingText, {
          x: barX,
          y: barY - 15,
          size: 1,
        });
      } else {
        // Track when countdown first hit zero
        if (countdownHitZeroTime === null) {
          countdownHitZeroTime = clock.time()?.getTime() || 0;
        }

        // Hold "Remaking in 0s" for quarter second (250ms) before showing regeneration prompt
        const currentTime = clock.time();
        const msAtZero = currentTime
          ? currentTime.getTime() - countdownHitZeroTime
          : 0;
        if (msAtZero < 250) {
          // Quarter second delay (250ms)
          const remakingZeroText = `Remaking in 0s`;
          // Draw shadow (black text offset by 1px down and right)
          ink(0, 0, 0).write(remakingZeroText, {
            x: barX + 1,
            y: barY - 15 + 1,
            size: 1,
          });
          // Draw main text
          ink(255, 255, 255).write(remakingZeroText, {
            x: barX,
            y: barY - 15,
            size: 1,
          });
        } else {
          // After delay, allow regeneration and show prompt
          if (!canRegenerate) {
            canRegenerate = true;
          }
          const remakingZeroText = "Remaking in 0s";
          // Draw shadow (black text offset by 1px down and right)
          ink(0, 0, 0).write(remakingZeroText, {
            x: barX + 1,
            y: barY - 15 + 1,
            size: 1,
          });
          // Draw main text
          ink(255, 255, 255).write(remakingZeroText, {
            x: barX,
            y: barY - 15,
            size: 1,
          });
        }
      }

      // When countdown completes, allow regeneration (but only after the delay)
      const msAtZero =
        currentTime && countdownHitZeroTime
          ? currentTime.getTime() - countdownHitZeroTime
          : 0;
      if (
        progress >= 1.0 &&
        countdownHitZeroTime !== null &&
        msAtZero >= 250 &&
        !canRegenerate
      ) {
        canRegenerate = true;
      }
    }
  }

  // Show FPS display (bottom-right corner)
  if (DEBUG_MODE) {
    const fpsText = `${averageFPS.toFixed(1)}fps`;
    // Draw shadow (black text offset by 1px down and right)
    ink(0, 0, 0).write(fpsText, {
      x: screen.width - fpsText.length * 6 - 5,
      y: screen.height - 25,
      size: 1,
    });
    // Draw main text
    ink(255, 255, 255).write(fpsText, {
      x: screen.width - fpsText.length * 6 - 6,
      y: screen.height - 26,
      size: 1,
    });
  }
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave({ store }) {
  abort?.(); // Cancel any existing `ask` which halts the server.
}

// 🧮 Sim (Runs once per logic frame (120fps locked))
function sim({ event, jump, reload }) {
  // Handle regeneration when countdown is complete
  if (canRegenerate) {
    // Hide progress bars immediately to prevent flash
    finalCountdownStartTime = null;
    canRegenerate = false;
    regenerationTriggeredTime = frameCount; // Track when regeneration was triggered

    // Reset to fresh making state
    makingStartTime = 0;
    makingProgress = 0.0;
    streaming = false;

    // Use piece-reload to only reload the current piece, not the entire system
    reload({ piece: "*piece-reload*" });
  }

  // Safety timeout: if regeneration was triggered but piece hasn't reloaded after 3 seconds, cancel the timer
  if (
    regenerationTriggeredTime > 0 &&
    frameCount - regenerationTriggeredTime > 180
  ) {
    // 3 seconds at 60fps
    // Cancel the timer and reset state
    finalCountdownStartTime = null;
    canRegenerate = false;
    regenerationTriggeredTime = 0;
    if (DEBUG_MODE) {
      console.log("⚠️ Regeneration timeout - canceling timer");
    }
  }
}

export { boot, paint, sim, leave };

// 📚 Library (Useful functions used throughout the piece)

// Process character buffer to display characters one by one for smooth streaming effect
function processCharacterBuffer() {
  if (characterBuffer.length === 0) return;

  // Only process characters if enough frames have passed (controls speed)
  if (frameCount - lastCharacterDisplayFrame < 1) return; // 1 frame per character

  // Adaptive speed: show more characters per frame when buffer is building up
  const adaptiveSpeed = Math.min(
    16,
    Math.max(1, Math.floor(characterBuffer.length / 20)),
  ); // 1-16 chars per frame
  const charactersToAdd = Math.min(adaptiveSpeed, characterBuffer.length);
  const newCharacters = characterBuffer.substring(0, charactersToAdd);

  const previousLength = displayedCode.length;

  // Add new characters to the display buffer first
  displayedCode += newCharacters;
  characterBuffer = characterBuffer.substring(charactersToAdd);
  lastCharacterDisplayFrame = frameCount;

  // Keep the displayed code as-is without minification

  if (DEBUG_MODE && charactersToAdd > 1) {
    // console.log(`⚡ Adaptive speed: showing ${charactersToAdd} characters (buffer: ${characterBuffer.length})`);
  }

  // Check for the next expected parameter in sequence using fullCode (not minified displayedCode)
  // Find the next unprocessed parameter index
  let nextExpectedIndex = -1;
  for (let i = 0; i < atParams.length; i++) {
    if (!foundParameters.has(i)) {
      nextExpectedIndex = i;
      break;
    }
  }

  // Only check for the next expected parameter in the original fullCode
  if (nextExpectedIndex !== -1) {
    const atParam = atParams[nextExpectedIndex];

    // Check if this parameter appears anywhere in the full code that we've received so far
    if (fullCode.includes(atParam)) {
      // console.log(`🟡 Next expected parameter detected: ${atParam} at index ${nextExpectedIndex}`);
      foundParameters.add(nextExpectedIndex);
      processedWords.add(atParam + "_" + nextExpectedIndex);

      // Immediately advance to this parameter (stream-driven)
      currentParamIndex = nextExpectedIndex;
      lastWordChangeTime = frameCount;

      // If this was the last parameter, record when it was found
      if (nextExpectedIndex === originalParams.length - 1) {
        lastParameterFoundFrame = frameCount;
      }
    }
  }
}

// Check for direct @parameter matches in code
function checkForParamMatches(code, fromFullCode = false) {
  if (!code || !atParams.length) return;

  if (DEBUG_MODE) {
    const currentParam =
      currentParamIndex < originalParams.length
        ? originalParams[currentParamIndex]
        : "DONE";
    console.log(
      `🔍 Checking for @params. Current index: ${currentParamIndex}, current param: "${currentParam}"`,
    );
  }

  // Check ALL parameters, not just from currentParamIndex
  for (let paramIndex = 0; paramIndex < atParams.length; paramIndex++) {
    const atParam = atParams[paramIndex];

    // Skip if we've already processed this specific parameter index when scanning full code
    if (fromFullCode && foundParameters.has(paramIndex)) {
      continue;
    }

    // Direct string match - much simpler!
    if (code.includes(atParam)) {
      // For duplicate words, only process if this specific parameter index hasn't been processed yet
      if (!foundParameters.has(paramIndex)) {
        if (DEBUG_MODE) {
          console.log(
            `🎯 FOUND! ${atParam} matches param "${originalParams[paramIndex]}" at index ${paramIndex}`,
          );
        }

        // Mark this specific parameter index as processed and found
        processedWords.add(atParam + "_" + paramIndex); // Use unique key for duplicates
        foundParameters.add(paramIndex);

        // Stream-driven: immediately advance to this parameter if it's the next expected one
        if (!fromFullCode && paramIndex === currentParamIndex + 1) {
          currentParamIndex = paramIndex;
          lastWordChangeTime = frameCount;
          if (DEBUG_MODE) {
            console.log(
              `🎯 Stream-driven parameter advance: Now highlighting "${originalParams[paramIndex]}" (index ${paramIndex})`,
            );
          }
        }

        // Continue checking other parameters - don't break on first match
        // This allows processing multiple parameters in the same chunk
      }
    }
  }
}

// Render the prompt with highlighting for the current active parameter only
function renderPromptWithHighlighting(
  screen,
  ink,
  text,
  params,
  currentIndex,
  typeface,
  shouldShowCode = false, // New parameter to sync with code display
  displayState = {}, // Additional display state for fine-grained control
  clock, // Add clock parameter for time-based animations
) {
  // Only show highlighting when shouldShowCode is true
  // This ensures perfect synchronization with code display
  const shouldShowHighlighting = shouldShowCode;

  if (!params || !params.length || !shouldShowHighlighting)
    return { highlights: [] };

  const words = ["make", ...params]; // Include "make" at the beginning
  const startX = 6;
  const startY = 6;
  const bounds = screen.width - typeface.blockWidth;

  let currentX = startX;
  let currentY = startY;
  const highlights = []; // Track highlight positions for connection lines

  words.forEach((word, index) => {
    if (word.trim()) {
      // Check if this is a parameter (not "make")
      const paramIndex = index - 1; // Convert word index to param index (skip "make")
      const isParameter = paramIndex >= 0;

      let color = [128, 128, 128]; // Default gray
      let isHighlighted = false;

      if (isParameter) {
        const isCurrent = paramIndex === currentIndex;

        // SIMPLIFIED: Only highlight when code is showing, this is current param, and highlighting not complete
        if (
          shouldShowCode &&
          isCurrent &&
          currentIndex < originalParams.length
        ) {
          color = [255, 255, 0];
          isHighlighted = true;
        } else {
          // Default gray for non-active parameters
          color = [128, 128, 128];
          isHighlighted = false;
        }
      } else {
        // This is "make" - only highlight when showing code and currentParamIndex is -1 (before any parameters)
        if (shouldShowCode && currentParamIndex === -1) {
          color = [255, 255, 0];
          isHighlighted = true;
        } else {
          // Default gray for "make" when not active
          color = [128, 128, 128];
          isHighlighted = false;
        }
      }

      // Check if word would exceed line bounds
      const wordWidth = word.length * typeface.blockWidth;
      if (currentX + wordWidth > bounds && currentX > startX) {
        currentY += LINE_HEIGHT;
        currentX = startX;
      }

      // Track highlight positions for connection lines
      if (isHighlighted && isParameter) {
        highlights.push({
          paramIndex,
          x: currentX,
          y: currentY,
          width: wordWidth,
          word: word,
          atParam: `/* @${word} */`,
        });
      }

      // ENHANCED WIGGLE LOGIC: Tie wiggle directly to text display state
      if (isHighlighted) {
        // Only wiggle when text is actively being displayed
        const shouldWiggle =
          displayState.hasActiveText &&
          (displayState.isCharacterStreaming ||
            displayState.streaming ||
            displayState.isHighlightingInProgress);

        if (shouldWiggle) {
          // Calculate wiggle offset using real time for frame-rate independence
          const currentTime = clock.time();
          const timeMs = currentTime
            ? currentTime.getMilliseconds() + currentTime.getSeconds() * 1000
            : 0;
          const wiggleSpeed = 0.003; // Adjusted for millisecond timing (was 0.15 for frame timing)
          const wiggleAmount = 1; // 1 pixel wiggle
          const wiggleX = Math.round(
            Math.sin(timeMs * wiggleSpeed) * wiggleAmount,
          );
          const wiggleY = Math.round(
            Math.cos(timeMs * wiggleSpeed * 0.7) * wiggleAmount,
          );

          ink(color[0], color[1], color[2]).write(word, {
            x: currentX + wiggleX,
            y: currentY + wiggleY,
            size: 1,
          });
        } else {
          // Highlighted but no wiggle - render static yellow text
          ink(color[0], color[1], color[2]).write(word, {
            x: currentX,
            y: currentY,
            size: 1,
          });
        }
      } else {
        ink(color[0], color[1], color[2]).write(word, {
          x: currentX,
          y: currentY,
          size: 1,
        });
      }

      // Move cursor position for next word
      currentX += wordWidth + typeface.blockWidth; // Add space after word

      // Add space between words (except last word)
      if (index < words.length - 1) {
        ink(128, 128, 128).write(" ", {
          x: currentX - typeface.blockWidth,
          y: currentY,
          size: 1,
        });
      }
    }
  });

  return { highlights };
}

// Display the entire source code as it grows with scrolling and character wrapping
function displayScrollingCodeBuffer(
  screen,
  ink,
  code,
  showEvaluation = false,
  hud,
  text,
  typeface,
  label, // Add label function parameter
  clock, // Add clock parameter for UTC timing
) {
  if (!code || !code.trim()) return;

  // Determine vertical offset to start right under the corner label
  let codeStartY = hud.currentLabel().btn.box.h + 12;

  // Check if we're still highlighting parameters
  const isCharacterStreaming = characterBuffer.length > 0;
  const hasParametersToHighlight = originalParams && originalParams.length > 0;

  // CRITICAL: Handle completion logic FIRST, before calculating any display states

  // If streaming has finished and no parameters were found, mark highlighting as complete
  if (
    !streaming &&
    !isCharacterStreaming &&
    hasParametersToHighlight &&
    foundParameters.size === 0 &&
    currentParamIndex < originalParams.length
  ) {
    currentParamIndex = originalParams.length; // Signal completion immediately
    if (DEBUG_MODE) {
      console.log(
        `✅ No @parameter comments detected after streaming - marking highlighting complete immediately.`,
      );
    }
  }

  // If all parameters have been found, mark highlighting as complete after the delay
  if (
    !streaming &&
    !isCharacterStreaming &&
    hasParametersToHighlight &&
    foundParameters.size === originalParams.length &&
    currentParamIndex < originalParams.length
  ) {
    // Give the last parameter a brief moment to be visible (60 frames = 1 second)
    if (
      lastParameterFoundFrame > 0 &&
      frameCount - lastParameterFoundFrame >= 60
    ) {
      currentParamIndex = originalParams.length; // Signal completion
      if (DEBUG_MODE) {
        console.log(
          `✅ All ${foundParameters.size} @parameter comments found - marking highlighting complete after ${frameCount - lastParameterFoundFrame} frames. Frame: ${frameCount}`,
        );
      }
    }
  }

  // Calculate isHighlightingInProgress AFTER all completion logic is handled
  const isHighlightingInProgress =
    hasParametersToHighlight && currentParamIndex < originalParams.length;

  // Show code overlay while:
  // 1. Network streaming is active, OR
  // 2. Character buffer has content to display, OR
  // 3. Parameter highlighting is still in progress, OR
  // 4. Brief pause after everything completes (if PAUSE_DURATION > 0), OR
  // 5. There's an error condition to display
  const shouldShowCode =
    streaming ||
    isCharacterStreaming ||
    isHighlightingInProgress ||
    (streamingEndTime > 0 && frameCount - streamingEndTime < PAUSE_DURATION) ||
    (fullCode && fullCode.startsWith("COMPILATION ERROR:")) ||
    fullCode === "ERROR";

  // Start cleanup timer only when ALL phases are complete:
  // - Network streaming is done
  // - Character buffer is empty
  // - Parameter highlighting is complete (or no parameters to highlight)
  if (
    !streaming &&
    !isCharacterStreaming &&
    !isHighlightingInProgress &&
    streamingEndTime === 0
  ) {
    streamingEndTime = frameCount;
  }

  // Perform cleanup after pause (or immediately if PAUSE_DURATION = 0)
  if (streamingEndTime > 0 && frameCount - streamingEndTime >= PAUSE_DURATION) {
    // Print the final results to console for debugging/analysis
    if (code && code.trim()) {
      // Use logStyledSource for consistent styling
      const isCompilationError = code.startsWith("COMPILATION ERROR:");
      logStyledSource("", code, isCompilationError, userPromptText);
    }

    // Perform complete cleanup to ensure clean state for next generation
    streamingEndTime = 0;
    currentParamIndex = -1; // Reset to "make" highlighted
    processedWords.clear();
    foundParameters.clear();
    lastParameterFoundFrame = 0;
    characterBuffer = "";
    displayedCode = "";
    lastCharacterDisplayFrame = 1;

    // Reset label restoration flag for next run
    labelRestored = false;

    // Start the final countdown timer for regeneration
    if (finalCountdownStartTime === null) {
      // Only start if not already started
      const currentTime = clock.time();
      finalCountdownStartTime = currentTime
        ? currentTime.getTime()
        : Date.now();
      canRegenerate = false; // Will be set to true when countdown completes
    }
  }

  // Restore the original HUD label after code display has finished
  if (!shouldShowCode && !labelRestored && label && originalLabel) {
    // Don't actually restore the label - let it stay empty so prompt shows
    label(originalLabel); // DISABLED - this was causing the flickering
    // label
    labelRestored = true;
  }

  // Always render the prompt with current param highlighting - but tie it to shouldShowCode
  if (!labelRestored) {
    renderPromptWithHighlighting(
      screen,
      ink,
      text,
      originalParams,
      currentParamIndex,
      typeface,
      shouldShowCode, // Pass shouldShowCode to synchronize highlighting
      {
        isCharacterStreaming,
        isHighlightingInProgress,
        streaming,
        hasActiveText:
          shouldShowCode && displayedCode && displayedCode.trim().length > 0,
      },
      clock, // Pass clock for time-based animations
    );
  }

  if (!shouldShowCode) return;

  // Expanded typewriter effect - fill available screen space
  const textAlpha = 200;

  // Calculate how much screen space we can use for text display
  // Progress bar is at screen.height - 12, with text 15px above it, so leave room for that
  const progressBarAreaStart = screen.height - 45; // Progress bar text starts here (increased margin to match renderCodeWithHighlighting)
  const availableHeight = Math.max(0, progressBarAreaStart - codeStartY); // Leave room for progress bar, ensure positive
  const calculatedMaxLines = Math.floor(availableHeight / LINE_HEIGHT); // Use available height
  const maxLines = Math.min(calculatedMaxLines, 6); // Hard limit to 6 lines maximum
  const charWidth = typeface ? typeface.blockWidth : 6; // Use actual character width from typeface
  const charsPerLine = Math.floor((screen.width - 16) / charWidth); // Chars per line with margins
  const maxDisplayLength = maxLines * charsPerLine; // Total characters we can display

  // Use the character-by-character displayed code directly for streaming effect
  // Don't flatten it - preserve the streaming text as-is
  let displayCode = code;

  // Apply scrolling only if the code would be too long to display
  if (code.length > maxDisplayLength) {
    // Show the most recent characters that fit
    displayCode = code.substring(code.length - maxDisplayLength);

    // Try to start from a word boundary to avoid cutting words
    const spaceIndex = displayCode.indexOf(" ");
    if (spaceIndex > 0 && spaceIndex < 20) {
      displayCode = displayCode.substring(spaceIndex + 1);
    }
  }

  // Show only the code segment for the currently highlighted parameter
  // This displays only the relevant code for the current @word parameter
  if (displayCode && displayCode.trim()) {
    renderCodeWithHighlighting(
      ink,
      displayCode, // Use the character-by-character displayed code
      {
        x: 6,
        y: codeStartY - 8,
        size: TEXT_SCALE,
      },
      screen.width - 12,
      textAlpha,
      text,
      screen, // Pass screen for height calculations
    );
  }

  // Show status indicators
  // (Removed old making indicator - now shown on progress bar)

  // Draw connection lines between prompt highlights and code highlights
  // drawHighlightConnections(
  //   screen,
  //   ink,
  //   promptHighlightResult,
  //   { x: 8, y: codeStartY, size: TEXT_SCALE },
  //   bounds,
  //   displayCode,
  //   text
  // );
}

// Render code text with @word highlighting for only the current active parameter
function renderCodeWithHighlighting(
  ink,
  code,
  position,
  bounds,
  textAlpha,
  text,
  screen, // Add screen parameter for height calculations
) {
  if (!code) {
    return;
  }

  // Calculate the segment boundaries for the current parameter
  let segmentStart = 0;
  let segmentEnd = code.length;

  if (atParams.length > 0 && currentParamIndex < atParams.length) {
    const highlightAtParam = atParams[currentParamIndex];

    if (highlightAtParam) {
      // Find the code that comes after this @word comment
      const commentIndex = code.indexOf(highlightAtParam);
      if (commentIndex !== -1) {
        // Start after this comment
        segmentStart = commentIndex + highlightAtParam.length;

        // Find the next @word comment to know where to stop
        const afterComment = code.substring(segmentStart);
        let nextCommentIndex = afterComment.length;
        for (let i = 0; i < atParams.length; i++) {
          const nextComment = atParams[i];
          const nextIndex = afterComment.indexOf(nextComment);
          if (nextIndex !== -1 && nextIndex < nextCommentIndex) {
            nextCommentIndex = nextIndex;
          }
        }
        segmentEnd = segmentStart + nextCommentIndex;

        // If there's no meaningful code after this comment, show the code before it instead
        const afterSegment = code.substring(segmentStart, segmentEnd).trim();
        if (!afterSegment || afterSegment.length === 0) {
          // Find the previous @word comment to know where to start
          segmentEnd = commentIndex;
          segmentStart = 0;
          for (let i = 0; i < atParams.length; i++) {
            if (i === currentParamIndex) continue; // Skip current param
            const prevComment = atParams[i];
            const prevIndex = code.lastIndexOf(prevComment, commentIndex);
            if (prevIndex !== -1) {
              segmentStart = Math.max(
                segmentStart,
                prevIndex + prevComment.length,
              );
            }
          }
        }
      }
    }
  } else if (currentParamIndex === -1) {
    // Show initial code before any @word comments appear (when "make" is highlighted)
    segmentStart = 0;
    segmentEnd = code.length;
    for (let i = 0; i < atParams.length; i++) {
      const atParam = atParams[i];
      const commentIndex = code.indexOf(atParam);
      if (commentIndex !== -1 && commentIndex < segmentEnd) {
        segmentEnd = commentIndex;
      }
    }
  }

  // Extract only the visible characters within the segment boundaries
  // This ensures we only show the streaming characters that belong to the current segment
  let visibleSegment = code.substring(segmentStart, segmentEnd);

  // Remove /* @word */ comments from the visible segment before displaying
  if (visibleSegment && atParams.length > 0) {
    for (let i = 0; i < atParams.length; i++) {
      const atParam = atParams[i];
      // Remove all instances of this @word comment from the visible segment
      visibleSegment = visibleSegment.replace(
        new RegExp(atParam.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), "g"),
        "",
      );
    }
    // Clean up any extra whitespace left by comment removal
    visibleSegment = visibleSegment.replace(/\s+/g, " ").trim();
  }

  // Calculate available vertical space to prevent overflow
  if (screen) {
    const progressBarAreaStart = screen.height - 45; // Progress bar area start (increased margin)
    const availableHeight = Math.max(0, progressBarAreaStart - position.y); // Available height from current position
    const calculatedMaxLines = Math.floor(availableHeight / LINE_HEIGHT); // Maximum lines that can fit
    const maxLines = Math.min(calculatedMaxLines, 6); // Hard limit to 6 lines maximum

    if (maxLines > 0) {
      // Calculate characters per line (use actual typeface width if available)
      const charWidth = 6; // Default character width
      const charsPerLine = Math.floor(bounds / charWidth);
      const maxChars = Math.max(1, maxLines * charsPerLine); // Ensure at least 1 character

      // Truncate the visible segment if it would exceed available space
      if (visibleSegment.length > maxChars) {
        visibleSegment = visibleSegment.substring(0, maxChars);
        // Try to end at a word boundary to avoid cutting words
        const lastSpace = visibleSegment.lastIndexOf(" ");
        if (lastSpace > maxChars * 0.8) {
          // Only use word boundary if it's not too far back
          visibleSegment = visibleSegment.substring(0, lastSpace);
        }
      }
    } else {
      // No space available, don't render anything
      return;
    }
  }

  // Only render if we have text to display
  if (visibleSegment && visibleSegment.trim()) {
    // Always render in yellow - this is the streaming preview text
    ink(255, 255, 0, textAlpha).write(
      visibleSegment.trim(),
      position,
      undefined,
      bounds,
    );
  }
}

function tryExecuteCurrentCode(isFinal = false, label, store) {
  if (!currentCode || currentCode.trim().length < 10) return;

  // Use the raw code as-is without any cleaning or formatting
  const codeToExecute = currentCode;

  try {
    // Update display buffers for consistent presentation
    if (isFinal) {
      // On final pass, update all buffers and force completion
      fullCode = codeToExecute;
      currentCode = codeToExecute;
      displayedCode = codeToExecute;
      characterBuffer = ""; // Clear the character buffer since we're replacing the displayed code

      // Force completion of parameter highlighting
      currentParamIndex = originalParams.length; // Signal completion immediately
      foundParameters.clear(); // Clear found parameters since they're no longer relevant
      streamingEndTime = frameCount; // Start the cleanup timer immediately
    }

    // Don't try to execute if it has obvious incomplete syntax
    if (!isFinal && hasIncompleteStructures(codeToExecute)) {
      return;
    }

    // Update current evaluation line for highlighting
    const codeLines = codeToExecute.split("\n");
    currentEvalLine = codeLines.length - 1; // Highlight the last (most recent) line

    // Try to compile the code - now with pen, state, and animationTime parameters
    const testFunction = new Function(
      "screen",
      "frameCount",
      "pen",
      "state",
      "animationTime",
      codeToExecute,
    );

    // If compilation succeeds, update our execute function
    executeCode = testFunction;
    lastValidCode = codeToExecute;

    if (isFinal) {
      currentEvalLine = -1; // Clear highlighting when done
      streaming = false; // Ensure streaming is marked as complete
      if (completionTime === 0) completionTime = frameCount; // Set completion time if not already set

      // If no parameters were found in the final code, mark highlighting as complete immediately
      if (
        foundParameters.size === 0 &&
        originalParams &&
        originalParams.length > 0
      ) {
        currentParamIndex = originalParams.length; // Signal completion
      }

      // Don't reset param tracking here - let the pause logic handle it after the pause expires
      // currentParamIndex = 0;
      // processedWords.clear();
      // foundParameters.clear();

      // Restore the original HUD label when final execution is complete
      // label(originalLabel);
      store["make-last-code"] = lastValidCode; // Save the last generated code to store
    }
  } catch (err) {
    // Silently ignore compilation errors during streaming
    // unless it's the final attempt
    if (isFinal) {
      currentEvalLine = -1; // Clear highlighting on error
      streaming = false; // Mark as complete even on error
      if (completionTime === 0) completionTime = frameCount;

      // Don't reset param tracking here - let the pause logic handle it after the pause expires
      // currentParamIndex = 0;
      // processedWords.clear();
      // foundParameters.clear();

      // Restore the original HUD label on final attempt error
      // label(originalLabel);

      // Try to fall back to the last valid code
      if (lastValidCode) {
        try {
          executeCode = new Function(
            "screen",
            "frameCount",
            "pen",
            "state",
            "animationTime",
            lastValidCode,
          );
        } catch (fallbackErr) {
          fullCode = "ERROR";
        }
      } else {
        fullCode = "ERROR";

        // Simple error logging
        logStyledSource("", codeToExecute, true, userPromptText);
        console.log(err.message);
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
  const openBrackets = (code.match(/\[/g) || []).length;
  const closeBrackets = (code.match(/\]/g) || []).length;

  // Allow moderate imbalance during streaming, but be stricter with arrays
  if (
    Math.abs(openBraces - closeBraces) > 3 ||
    Math.abs(openParens - closeParens) > 3 ||
    Math.abs(openBrackets - closeBrackets) > 2 // Be stricter with arrays
  ) {
    return true;
  }

  const trimmedCode = code.trim();

  // Check for incomplete function declarations only if they're at the very end
  if (trimmedCode.endsWith("function ") || trimmedCode.endsWith("function("))
    return true;

  // Check for incomplete array literals at the end
  if (trimmedCode.endsWith("[") || trimmedCode.endsWith(", [")) return true;

  // Check for incomplete object literals at the end
  if (trimmedCode.endsWith("{") || trimmedCode.endsWith(", {")) return true;

  // Check for incomplete variable declarations that end with assignment
  if (
    trimmedCode.match(/\blet\s+\w+\s*=\s*$/) ||
    trimmedCode.match(/\bconst\s+\w+\s*=\s*$/)
  )
    return true;

  // Check for incomplete for loops
  if (trimmedCode.match(/\bfor\s*\(\s*[^)]*$/) || trimmedCode.endsWith("for ("))
    return true;

  // Check for incomplete multi-dimensional array literals
  if (trimmedCode.match(/=\s*\[\s*\[\s*[^\]]*$/)) return true;

  return false;
}

// Draw connection lines between prompt highlights and code highlights
function drawHighlightConnections(
  screen,
  ink,
  promptHighlights,
  codePosition,
  bounds,
  code,
  text,
) {
  if (
    !promptHighlights ||
    !promptHighlights.highlights ||
    !promptHighlights.highlights.length
  )
    return;

  // Get text bounds for the code to find @word positions
  const textBounds = text.box(code, codePosition, bounds);

  // For each highlighted prompt word, find its @word in the code and draw a connection line
  promptHighlights.highlights.forEach((highlight) => {
    const atParam = highlight.atParam;

    // Find the position of this @word in the code
    const codeIndex = code.indexOf(atParam);
    if (codeIndex === -1) return; // Not found in visible code

    // Calculate approximate position of the @word in the rendered text
    // This is a rough approximation - getting exact text positions is complex
    const codeLines = code.substring(0, codeIndex).split("\n");
    const lineIndex = codeLines.length - 1;
    const charIndex = codeLines[lineIndex].length;

    // Approximate position in the code display
    const codeHighlightX = codePosition.x + charIndex * 6; // Rough character width
    const codeHighlightY = codePosition.y + lineIndex * LINE_HEIGHT;

    // Only draw line if code highlight is within visible area
    if (
      codeHighlightY >= codePosition.y &&
      codeHighlightY <= codePosition.y + textBounds.box.height
    ) {
      // Draw a subtle yellow line connecting the highlights
      const startX = highlight.x + highlight.width / 2;
      const startY = highlight.y + 6; // Bottom of prompt text
      const endX = Math.max(
        codePosition.x,
        Math.min(codeHighlightX, screen.width - 10),
      );
      const endY = codeHighlightY + 3; // Middle of code text

      // Draw line with low alpha for subtlety
      ink(255, 255, 0, 80).line(startX, startY, endX, endY);
    }
  });
}

// Dynamic resolution scaling variables
(currentRenderScale = 0.5), // Start at 50% resolution
  (frameTimings = []), // Array to store recent frame times
  (lastFrameTime = 0), // Track last frame time for FPS calculation
  (lastScaleAdjustment = 0), // Track when we last adjusted scale
  (averageFPS = 30), // Running average FPS
  // Relative coordinate system for resolution-independent state persistence
  (relativeStateStore = new Map()), // Internal storage for relative coordinates
  (screenDimensions = { width: 0, height: 0 }); // Track current screen dimensions

// Coordinate conversion utilities
function toRelative(absValue, dimension) {
  // Convert absolute pixel value to relative proportion (0.0-1.0)
  if (typeof absValue !== "number" || !isFinite(absValue)) return absValue;
  return dimension > 0 ? Math.max(0, Math.min(1, absValue / dimension)) : 0;
}

function toAbsolute(relValue, dimension) {
  // Convert relative proportion (0.0-1.0) to absolute pixel value
  if (typeof relValue !== "number" || !isFinite(relValue)) return relValue;
  return Math.round(relValue * dimension);
}

function isCoordinateLike(key, value) {
  // Detect if a property might be a coordinate based on key name and value
  if (typeof value !== "number" || !isFinite(value)) return false;
  const coordinateKeys =
    /^(x|y|pos|position|left|top|right|bottom|width|height|w|h|centerX|centerY|startX|startY|endX|endY|targetX|targetY|prevX|prevY|lastX|lastY|deltaX|deltaY|offsetX|offsetY)$/i;
  return coordinateKeys.test(key);
}

function isXCoordinate(key) {
  return /^(x|left|right|width|w|centerX|startX|endX|targetX|prevX|lastX|deltaX|offsetX)$/i.test(
    key,
  );
}

function isYCoordinate(key) {
  return /^(y|top|bottom|height|h|centerY|startY|endY|targetY|prevY|lastY|deltaY|offsetY)$/i.test(
    key,
  );
}

// Enhanced state proxy that automatically converts between relative and absolute coordinates
function createRelativeStateProxy(baseState, screenWidth, screenHeight) {
  // Store a unique ID for this proxy instance to avoid conflicts
  const proxyId = Math.random().toString(36).substr(2, 9);

  return new Proxy(baseState, {
    get(target, prop) {
      const key = String(prop);

      // Handle array-like access for coordinate arrays
      if (Array.isArray(target[prop])) {
        return target[prop].map((item, index) => {
          if (typeof item === "object" && item !== null) {
            return createRelativeStateProxy(item, screenWidth, screenHeight);
          }
          return item;
        });
      }

      // Handle nested objects
      if (
        typeof target[prop] === "object" &&
        target[prop] !== null &&
        !Array.isArray(target[prop])
      ) {
        return createRelativeStateProxy(
          target[prop],
          screenWidth,
          screenHeight,
        );
      }

      // Check if this property is stored as relative coordinate
      const relativeKey = `${proxyId}_${prop}_rel`;
      if (relativeStateStore.has(relativeKey)) {
        const relativeValue = relativeStateStore.get(relativeKey);
        if (isXCoordinate(key)) {
          return toAbsolute(relativeValue, screenWidth);
        } else if (isYCoordinate(key)) {
          return toAbsolute(relativeValue, screenHeight);
        }
        // For non-coordinate values that are stored as relative, return as-is
        return relativeValue;
      }

      return target[prop];
    },

    set(target, prop, value) {
      const key = String(prop);

      // If this looks like a coordinate, store as relative coordinate internally
      if (isCoordinateLike(key, value)) {
        const relativeKey = `${proxyId}_${prop}_rel`;
        if (isXCoordinate(key)) {
          // Store relative value (0.0-1.0) internally
          relativeStateStore.set(relativeKey, toRelative(value, screenWidth));
        } else if (isYCoordinate(key)) {
          // Store relative value (0.0-1.0) internally
          relativeStateStore.set(relativeKey, toRelative(value, screenHeight));
        }

        // Don't store the absolute value in the target object
        // The proxy will convert back to absolute when accessed
        return true;
      }

      // For non-coordinate values, store normally
      target[prop] = value;
      return true;
    },
  });
}

// Create resolution-aware pen object that provides coordinates scaled to current buffer
function createScaledPen(basePen, screenWidth, screenHeight, renderScale) {
  if (!basePen) return { x: 0, y: 0, drawing: false };

  // Create a proxy that stores pen coordinates as relative values internally
  const penProxyId = Math.random().toString(36).substr(2, 9);

  // Store the current pen coordinates as relative values
  const xRelativeKey = `${penProxyId}_x_rel`;
  const yRelativeKey = `${penProxyId}_y_rel`;
  relativeStateStore.set(xRelativeKey, toRelative(basePen.x, screenWidth));
  relativeStateStore.set(yRelativeKey, toRelative(basePen.y, screenHeight));

  return {
    // Scale pen coordinates to match the current render buffer size
    get x() {
      const relativeX = relativeStateStore.get(xRelativeKey) || 0;
      return Math.floor(toAbsolute(relativeX, screenWidth) * renderScale);
    },
    get y() {
      const relativeY = relativeStateStore.get(yRelativeKey) || 0;
      return Math.floor(toAbsolute(relativeY, screenHeight) * renderScale);
    },
    drawing: basePen.drawing || false,

    // Provide relative coordinates (0.0-1.0) for absolute positioning
    get relativeX() {
      return relativeStateStore.get(xRelativeKey) || 0;
    },
    get relativeY() {
      return relativeStateStore.get(yRelativeKey) || 0;
    },

    // Original coordinates for reference
    originalX: basePen.x,
    originalY: basePen.y,

    // Method to update pen position (stores as relative coordinates)
    setPosition(newX, newY) {
      relativeStateStore.set(xRelativeKey, toRelative(newX, screenWidth));
      relativeStateStore.set(yRelativeKey, toRelative(newY, screenHeight));
    },
  };
}
