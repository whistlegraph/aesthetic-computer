// Make, 25.06.05.00.01 - Simplified Architecture
// Creates animated art from text prompts with sequential parameter highlighting

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
  completionTime = 0, // Track when streaming completed
  originalLabel = "", // Store the original HUD label text
  originalParams = [], // Store the original user params
  atParams = [], // Store @-prefixed versions of params for direct matching
  currentParamIndex = 0, // Index of the currently highlighted param
  lastWordChangeTime = 0, // Track when the current word was last changed (in frameCount)
  processedWords = new Set(), // Track words that have already been processed
  foundParameters = new Set(), // Track which parameter indices have been found
  streamingEndTime = 0, // Track when streaming ended for pause duration
  labelRestored = false, // Track if we've restored the original label
  parameterQueue = [], // Queue for parameters found in chunks: [{index, foundFrame}]
  currentQueueIndex = 0, // Current position in the queue
  lastQueueAdvanceFrame = 0, // Last frame when we advanced the queue
  characterBuffer = "", // Buffer for incoming code chunks
  displayedCode = "", // Code that has been displayed character by character
  lastCharacterDisplayFrame = 0, // Last frame when we displayed a character
  charactersPerFrame = 1; // How many characters to show per frame (adjustable speed, up to 4 when catching up)

const DEBUG_MODE = false; // Set to true for detailed logging
const TEXT_SCALE = 1.0; // Normal text scale for better readability
const LINE_HEIGHT = 12; // Normal line height for scale 1.0
const RENDER_SCALE = 0.5; // Render at 50% resolution for performance (0.25 for quarter resolution)
const HIGHLIGHT_DURATION_FRAMES = 120; // How long each parameter highlight lasts (2 seconds at 60fps)
const MIN_PARAM_DISPLAY_FRAMES = 6; // Minimum frames to show each parameter (0.1 seconds at 60fps)
const PAUSE_DURATION = 120; // 2 seconds at 60fps to give enough time for last parameter (was 30)

// 🥾 Boot (Runs once before first paint and sim)
function boot({ params, store, slug, hud: { label, currentLabel } }) {
  // Store the original label text and label function
  originalLabel = currentLabel().text || "";

  console.log("🔴 PARAMS:", params, "🔵 Original Label:", originalLabel);

  // Store the original params for simple highlighting
  originalParams = [...params];
  atParams = params.map(param => `/* @${param} */`); // Create multiline comment versions
  currentParamIndex = 0;
  lastWordChangeTime = 0;
  processedWords.clear(); // Reset processed words
  foundParameters.clear(); // Reset found parameters
  streamingEndTime = 0;
  labelRestored = false; // Reset label restoration flag
  parameterQueue = []; // Reset parameter queue
  currentQueueIndex = 0;
  lastQueueAdvanceFrame = 0;
  characterBuffer = ""; // Reset character buffer
  displayedCode = ""; // Reset displayed code
  lastCharacterDisplayFrame = 0;

  // Load the prompt template from the .prompt file synchronously
  let programSource;
  let promptLoaded = false;
  
  fetch("../prompts/make.prompt")
    .then(response => response.text())
    .then(text => {
      programSource = text;
      promptLoaded = true;
      initializeConversation();
    })
    .catch(error => {
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
          hint: "code:claude-sonnet-4-20250514", // Use Claude Sonnet 4
          temperature: 0.5, // TODO: Support this custom temperature setting in ask.js netlify function.
        },
        function and(msg) {
          if (fullCode === "PROCESSING...") {
            fullCode = ``; // Clear any waiting message.
            streaming = true; // Mark that we're actively streaming
            label(); // Hide the HUD label while processing
          }
          fullCode += msg;
          currentCode += msg;
          chunkCount++;

          // Add incoming message to character buffer for smooth character-by-character display
          characterBuffer += msg;

          // Try to evaluate code more frequently for better streaming feedback (was every 5, now every 3)
          if (chunkCount % 3 === 0) {
            tryExecuteCurrentCode(undefined, label);
          }
        },
        function done() {
          // Mark streaming as complete
          streaming = false;
          completionTime = frameCount;
          // Don't set streamingEndTime here - let the character buffer finish first
          // streamingEndTime = frameCount; // This will be set automatically when character buffer is empty

          // Don't reset param tracking here - keep the last highlighted parameter visible
          // currentParamIndex = 0;
          // processedWords.clear();

          // Don't restore the original HUD label here - wait for pause to expire
          // label(originalLabel);

          if (fullCode && fullCode.trim()) {
            tryExecuteCurrentCode(true, label); // Final attempt with full cleanup
          }
        },
        function fail() {
          fullCode = "NETWORK FAILURE";
          streaming = false;
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
  hud: { label, currentLabel },
}) {
  frameCount++; // Increment frame counter for animations

  // Process character buffer for smooth streaming effect
  processCharacterBuffer();

  // Process the parameter queue to advance highlighting
  processParameterQueue();

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
        displayedCode, // Use character-by-character displayed code
        false,
        { currentLabel },
        text,
        typeface,
        label, // Pass label function for cleanup
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
    );
  }
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave() {
  abort?.(); // Cancel any existing `ask` which halts the server.
}

export { boot, paint, leave };

// 📚 Library (Useful functions used throughout the piece)

// Process character buffer to display characters one by one for smooth streaming effect
function processCharacterBuffer() {
  if (characterBuffer.length === 0) return;
  
  // Only process characters if enough frames have passed (controls speed)
  if (frameCount - lastCharacterDisplayFrame < 1) return; // 1 frame per character
  
  // Adaptive speed: show more characters per frame when buffer is building up
  const adaptiveSpeed = Math.min(16, Math.max(1, Math.floor(characterBuffer.length / 20))); // 1-16 chars per frame
  const charactersToAdd = Math.min(adaptiveSpeed, characterBuffer.length);
  const newCharacters = characterBuffer.substring(0, charactersToAdd);
  
  const previousLength = displayedCode.length;
  displayedCode += newCharacters;
  characterBuffer = characterBuffer.substring(charactersToAdd);
  lastCharacterDisplayFrame = frameCount;
  
  if (DEBUG_MODE && charactersToAdd > 1) {
    // console.log(`⚡ Adaptive speed: showing ${charactersToAdd} characters (buffer: ${characterBuffer.length})`);
  }
  
  // Check for the next expected parameter in sequence
  // Find the next unprocessed parameter index
  let nextExpectedIndex = -1;
  for (let i = 0; i < atParams.length; i++) {
    if (!foundParameters.has(i)) {
      nextExpectedIndex = i;
      break;
    }
  }
  
  // Only check for the next expected parameter
  if (nextExpectedIndex !== -1) {
    const atParam = atParams[nextExpectedIndex];
    const lastOccurrence = displayedCode.lastIndexOf(atParam);
    
    if (lastOccurrence !== -1 && lastOccurrence + atParam.length >= previousLength) {
      console.log(`🟡 Next expected parameter detected: ${atParam} at index ${nextExpectedIndex}`);
      foundParameters.add(nextExpectedIndex);
      processedWords.add(atParam + "_" + nextExpectedIndex);
      
      if (!parameterQueue.some(p => p.index === nextExpectedIndex)) {
        parameterQueue.push({ 
          index: nextExpectedIndex, 
          foundFrame: frameCount 
        });
        
        // If this is the first item in the queue, start processing immediately
        if (parameterQueue.length === 1) {
          currentQueueIndex = 0;
          lastQueueAdvanceFrame = frameCount;
          currentParamIndex = nextExpectedIndex;
          lastWordChangeTime = frameCount;
        }
        
        console.log(`📋 Added to queue: ${atParam} (index ${nextExpectedIndex}). Queue length: ${parameterQueue.length}`);
      }
    }
  }
}

// Process the parameter queue to show each parameter for minimum duration
function processParameterQueue() {
  if (parameterQueue.length === 0) return;
  
  // Check if we need to advance to the next parameter in the queue
  const framesSinceLastAdvance = frameCount - lastQueueAdvanceFrame;
  
  if (currentQueueIndex < parameterQueue.length - 1 && framesSinceLastAdvance >= MIN_PARAM_DISPLAY_FRAMES) {
    currentQueueIndex++;
    lastQueueAdvanceFrame = frameCount;
    
    if (DEBUG_MODE) {
      const queueItem = parameterQueue[currentQueueIndex];
      console.log(`⏭️  Queue advanced to index ${currentQueueIndex}, showing param ${queueItem.index}: "${originalParams[queueItem.index]}"`);
    }
  }
  
  // Update currentParamIndex based on current queue position
  if (currentQueueIndex < parameterQueue.length) {
    const queueItem = parameterQueue[currentQueueIndex];
    currentParamIndex = queueItem.index;
    
    if (DEBUG_MODE && frameCount % 30 === 0) {
      console.log(`📍 Current param index: ${currentParamIndex}, queue index: ${currentQueueIndex}/${parameterQueue.length}, param: "${originalParams[currentParamIndex] || 'UNKNOWN'}"`);
    }
  }
}

// Check for direct @parameter matches in code
function checkForParamMatches(code, fromFullCode = false) {
  if (!code || !atParams.length) return;

  if (DEBUG_MODE) {
    const currentParam = currentParamIndex < originalParams.length ? originalParams[currentParamIndex] : "DONE";
    console.log(`🔍 Checking for @params. Current index: ${currentParamIndex}, current param: "${currentParam}"`);
  }

  let foundInThisChunk = [];

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
          console.log(`🎯 FOUND! ${atParam} matches param "${originalParams[paramIndex]}" at index ${paramIndex}`);
        }
        
        // Mark this specific parameter index as processed and found
        processedWords.add(atParam + "_" + paramIndex); // Use unique key for duplicates
        foundParameters.add(paramIndex);
        
        // Add to queue if not already in queue and not from full code scan
        if (!fromFullCode && !parameterQueue.some(item => item.index === paramIndex)) {
          foundInThisChunk.push({ index: paramIndex, foundFrame: frameCount });
        }
        
        // Continue checking other parameters - don't break on first match
        // This allows processing multiple parameters in the same chunk
      }
    }
  }
  
  // Add all found parameters to the queue in order
  if (foundInThisChunk.length > 0) {
    // Sort by parameter index to maintain logical order
    foundInThisChunk.sort((a, b) => a.index - b.index);
    
    parameterQueue.push(...foundInThisChunk);
    
    // If this is the first time we're adding to the queue, start processing immediately
    if (parameterQueue.length === foundInThisChunk.length) {
      currentQueueIndex = 0;
      lastQueueAdvanceFrame = frameCount;
      currentParamIndex = parameterQueue[0].index;
      lastWordChangeTime = frameCount;
    }
    
    if (DEBUG_MODE) {
      console.log(`📥 Added ${foundInThisChunk.length} parameters to queue:`, foundInThisChunk.map(item => `${item.index}:${originalParams[item.index]}`));
      console.log(`📋 Queue now has ${parameterQueue.length} items`);
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
) {
  // Show highlighting during streaming AND during the pause period after streaming ends
  // Also show the final prompt state after everything is complete (if any parameters were found)
  const isPauseActive = streamingEndTime > 0 && frameCount - streamingEndTime < PAUSE_DURATION;
  const shouldShowHighlighting = streaming || isPauseActive || foundParameters.size > 0;
  
  if (DEBUG_MODE && frameCount % 30 === 0) {
    console.log(`🔍 Highlighting debug: streaming=${streaming}, streamingEndTime=${streamingEndTime}, isPauseActive=${isPauseActive}, shouldShow=${shouldShowHighlighting}, foundParams=${foundParameters.size}, currentIndex=${currentIndex}`);
  }
  
  if (!params || !params.length || !shouldShowHighlighting) return { highlights: [] };

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
        const hasBeenFound = foundParameters.has(paramIndex);
        
        if (DEBUG_MODE && frameCount % 60 === 0 && paramIndex === originalParams.length - 1) {
          console.log(`🔍 Last param debug: paramIndex=${paramIndex}, word="${word}", isCurrent=${isCurrent}, hasBeenFound=${hasBeenFound}, currentIndex=${currentIndex}`);
        }
        
        // Show highlighting in different states:
        if (streaming || isPauseActive) {
          // During streaming and pause: only highlight current parameter
          if (isCurrent && hasBeenFound) {
            color = [255, 255, 0];
            isHighlighted = true;
          }
        } else {
          // After streaming is complete: show all found parameters in gray, no wiggling
          if (hasBeenFound) {
            color = [200, 200, 200]; // Light gray for completed parameters
            isHighlighted = false; // No wiggling
          }
        }
      } else {
        // This is "make" - highlight it when no parameters have been found yet
        if (foundParameters.size === 0) {
          color = [255, 255, 0];
          isHighlighted = true;
        } else if (!streaming && !isPauseActive) {
          // After completion, show "make" in light gray
          color = [200, 200, 200];
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
          atParam: `/* @${word} */`
        });
      }

      // Render the word with appropriate color and wiggle if highlighted
      if (isHighlighted) {
        // Calculate wiggle offset for animated movement
        const wiggleSpeed = 0.15;
        const wiggleAmount = 1; // 1 pixel wiggle
        const wiggleX = Math.round(Math.sin(frameCount * wiggleSpeed) * wiggleAmount);
        const wiggleY = Math.round(Math.cos(frameCount * wiggleSpeed * 0.7) * wiggleAmount);
        
        ink(color[0], color[1], color[2]).write(word, { 
          x: currentX + wiggleX, 
          y: currentY + wiggleY, 
          size: 1 
        });
      } else {
        ink(color[0], color[1], color[2]).write(word, { x: currentX, y: currentY, size: 1 });
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
) {
  if (!code || !code.trim()) return;

  // Determine vertical offset to start right under the corner label
  let codeStartY = hud.currentLabel().btn.box.h + 12;

  // Only show code overlay while streaming OR character buffer is not empty OR for a brief period after streaming ends
  const isCharacterStreaming = characterBuffer.length > 0; // Still characters to display
  const shouldShowCode = streaming || isCharacterStreaming || (streamingEndTime > 0 && frameCount - streamingEndTime < PAUSE_DURATION);
  
  // Only start the pause timer when both network streaming is done AND character buffer is empty
  if (!streaming && !isCharacterStreaming && streamingEndTime === 0) {
    streamingEndTime = frameCount; // Start pause timer
    if (DEBUG_MODE) {
      console.log(`⏱️  PAUSE STARTED: Frame ${frameCount}, will end at frame ${frameCount + PAUSE_DURATION}`);
    }
  }
  
  // Reset streamingEndTime and parameter tracking after the pause period to clean up
  if (streamingEndTime > 0 && frameCount - streamingEndTime >= PAUSE_DURATION) {
    // Print the final source code right before the pause ends
    if (DEBUG_MODE && code && code.trim()) {
      console.log("🎬 FINAL SOURCE CODE (before transition):");
      console.log("=" .repeat(60));
      console.log(code);
      console.log("=" .repeat(60));
    }
    
    if (DEBUG_MODE) {
      console.log(`🧹 CLEANUP: Pause expired after ${frameCount - streamingEndTime} frames. Cleaning up all state.`);
    }
    
    // Atomic cleanup - do everything at once to prevent visual glitches
    streamingEndTime = 0;
    currentParamIndex = 0;
    processedWords.clear();
    foundParameters.clear();
    parameterQueue = [];
    currentQueueIndex = 0;
    lastQueueAdvanceFrame = 0;
    characterBuffer = "";
    displayedCode = "";
    lastCharacterDisplayFrame = 0;
    
    // At this point, shouldShowCode will become false and the prompt will disappear cleanly
  }
  
  // Restore the original HUD label after code display has finished
  if (!shouldShowCode && !labelRestored && label && originalLabel) {
    // Don't actually restore the label - let it stay empty so prompt shows
    // label(originalLabel); // DISABLED - this was causing the flickering
    labelRestored = true;
    if (DEBUG_MODE) {
      console.log(`🏷️  Label restoration skipped to keep prompt visible: "${originalLabel}"`);
    }
  }
  
  if (!shouldShowCode) return;

  // Expanded typewriter effect - fill available screen space
  const textAlpha = 200;
  
  // Calculate how much screen space we can use
  const availableHeight = screen.height - codeStartY - 30; // Leave bottom margin
  const maxLines = Math.floor(availableHeight / LINE_HEIGHT); // How many lines fit
  const charsPerLine = Math.floor((screen.width - 16) / 6); // Chars per line (6px char width)
  const maxDisplayLength = maxLines * charsPerLine; // Total characters we can display
  
  // Flatten the code into one continuous line for the typewriter effect
  const flatCode = code.trim().replace(/\n/g, ' ').replace(/\s+/g, ' ');
  
  // Determine how much of the flattened code to show
  let displayCode = "";
  
  if (flatCode.length <= maxDisplayLength) {
    // Show everything if it fits
    displayCode = flatCode;
  } else {
    // Show the most recent code that fits in our display area
    displayCode = flatCode.substring(flatCode.length - maxDisplayLength);
    
    // Trim to word boundary if possible to avoid cutting words in half
    const spaceIndex = displayCode.indexOf(' ');
    if (spaceIndex > 0 && spaceIndex < 20) {
      displayCode = displayCode.substring(spaceIndex + 1);
    }
  }

  // Render the code with text wrapping to fill the available space
  renderCodeWithHighlighting(
    ink,
    displayCode,
    { x: 6, y: codeStartY - 8, size: TEXT_SCALE }, // 6px from left edge, 8px higher than codeStartY
    screen.width - 6, // Wrap to screen width minus 6px
    textAlpha,
    text
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

  // Render the prompt with current param highlighting (show during streaming and after until final execution)
  // Show the most recently found parameter directly
  const promptHighlightResult = renderPromptWithHighlighting(
    screen,
    ink,
    text,
    originalParams,
    currentParamIndex,
    typeface,
  );
  
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
function renderCodeWithHighlighting(ink, code, position, bounds, textAlpha, text) {
  if (!code || !atParams.length) {
    // No highlighting needed, render normally
    ink(180, 180, 180, textAlpha).write(code, position, undefined, bounds);
    return;
  }

  // Only highlight the current active parameter (the most recently found one)
  const highlightAtParam = currentParamIndex < atParams.length ? atParams[currentParamIndex] : null;
  
  if (DEBUG_MODE && frameCount % 60 === 0) {
    console.log(`🎨 Render debug: currentParamIndex=${currentParamIndex}, highlightAtParam="${highlightAtParam}", atParams.length=${atParams.length}`);
  }
  
  if (highlightAtParam) {
    // Extract just the word from the comment format /* @word */
    const wordMatch = highlightAtParam.match(/\/\* @(\w+) \*\//);
    if (wordMatch) {
      const justTheWord = wordMatch[1];
      
      if (DEBUG_MODE && frameCount % 60 === 0) {
        console.log(`🎨 Showing code after "${highlightAtParam}" for word "${justTheWord}"`);
      }
      
      // Find the code that comes after this @word comment
      const commentIndex = code.indexOf(highlightAtParam);
      if (commentIndex !== -1) {
        const afterComment = code.substring(commentIndex + highlightAtParam.length);
        
        // Find the next @word comment to know where to stop
        let nextCommentIndex = afterComment.length; // Default to end of code
        for (let i = 0; i < atParams.length; i++) {
          const nextComment = atParams[i];
          const nextIndex = afterComment.indexOf(nextComment);
          if (nextIndex !== -1 && nextIndex < nextCommentIndex) {
            nextCommentIndex = nextIndex;
          }
        }
        
        // Extract just the code between this @word and the next one
        const codeSegment = afterComment.substring(0, nextCommentIndex).trim();
        
        if (codeSegment) {
          // Show the code segment directly without "Implementing:" prefix - always yellow
          ink(255, 255, 0, textAlpha).write(codeSegment, position, undefined, bounds);
          
          if (DEBUG_MODE && frameCount % 60 === 0) {
            console.log(`🎨 Code segment for "${justTheWord}": "${codeSegment.substring(0, 50)}..."`);
          }
        }
      }
    }
  } else if (foundParameters.size === 0 && code) {
    // Show initial code before any @word comments appear (when "make" is highlighted)
    // Find the first @word comment position
    let firstCommentIndex = code.length; // Default to entire code
    for (let i = 0; i < atParams.length; i++) {
      const atParam = atParams[i];
      const commentIndex = code.indexOf(atParam);
      if (commentIndex !== -1 && commentIndex < firstCommentIndex) {
        firstCommentIndex = commentIndex;
      }
    }
    
    // Show code before the first @word comment
    const initialCode = code.substring(0, firstCommentIndex).trim();
    if (initialCode) {
      ink(255, 255, 0, textAlpha).write(initialCode, position, undefined, bounds);
      
      if (DEBUG_MODE && frameCount % 60 === 0) {
        console.log(`🎨 Showing initial code: "${initialCode.substring(0, 50)}..."`);
      }
    }
  } else {
    // No highlighting, render normally - always yellow
    ink(255, 255, 0, textAlpha).write(code, position, undefined, bounds);
  }
}

function tryExecuteCurrentCode(isFinal = false, label) {
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

    // Print the full source before execution
    if (DEBUG_MODE) {
      // console.log("📄 Executing Code:\n", cleanCode);
    }

    // Try to compile the code - now with pen parameter
    const testFunction = new Function("screen", "frameCount", "pen", cleanCode);

    // If compilation succeeds, update our execute function
    executeCode = testFunction;
    lastValidCode = cleanCode;

    if (isFinal) {
      currentEvalLine = -1; // Clear highlighting when done
      streaming = false; // Ensure streaming is marked as complete
      if (completionTime === 0) completionTime = frameCount; // Set completion time if not already set

      // Don't reset param tracking here - let the pause logic handle it after the pause expires
      // currentParamIndex = 0;
      // processedWords.clear();
      // foundParameters.clear();

      // Restore the original HUD label when final execution is complete
      // label(originalLabel);
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

// Draw connection lines between prompt highlights and code highlights
function drawHighlightConnections(screen, ink, promptHighlights, codePosition, bounds, code, text) {
  if (!promptHighlights || !promptHighlights.highlights || !promptHighlights.highlights.length) return;
  
  // Get text bounds for the code to find @word positions
  const textBounds = text.box(code, codePosition, bounds);
  
  // For each highlighted prompt word, find its @word in the code and draw a connection line
  promptHighlights.highlights.forEach(highlight => {
    const atParam = highlight.atParam;
    
    // Find the position of this @word in the code
    const codeIndex = code.indexOf(atParam);
    if (codeIndex === -1) return; // Not found in visible code
    
    // Calculate approximate position of the @word in the rendered text
    // This is a rough approximation - getting exact text positions is complex
    const codeLines = code.substring(0, codeIndex).split('\n');
    const lineIndex = codeLines.length - 1;
    const charIndex = codeLines[lineIndex].length;
    
    // Approximate position in the code display
    const codeHighlightX = codePosition.x + (charIndex * 6); // Rough character width
    const codeHighlightY = codePosition.y + (lineIndex * LINE_HEIGHT);
    
    // Only draw line if code highlight is within visible area
    if (codeHighlightY >= codePosition.y && codeHighlightY <= codePosition.y + textBounds.box.height) {
      // Draw a subtle yellow line connecting the highlights
      const startX = highlight.x + highlight.width / 2;
      const startY = highlight.y + 6; // Bottom of prompt text
      const endX = Math.max(codePosition.x, Math.min(codeHighlightX, screen.width - 10));
      const endY = codeHighlightY + 3; // Middle of code text
      
      // Draw line with low alpha for subtlety
      ink(255, 255, 0, 80).line(startX, startY, endX, endY);
    }
  });
}