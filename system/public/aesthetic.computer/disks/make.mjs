// Make, 2025.9.2.15.30 - KidLisp Generator
// Creates animated art from text prompts using KidLisp language

/* 📝 Engineering Notes
  This piece generates KidLisp code from natural language prompts.
  Features sophisticated UI with parameter highlighting and streaming display.
  Integrates with the KidLisp interpreter for immediate execution.
*/

/* #region 📓 TODO
  - [x] Incorporate UI/UX features from `oldmake.mjs`
  - [x] Adapt for KidLisp code generation instead of JavaScript
  - [x] Add KidLisp syntax highlighting
  - [x] Add matrix tiny font for code display
  - [ ] Test with various prompts
  - [ ] Add error handling for invalid KidLisp
#endregion */

import { Conversation } from "../lib/ask.mjs";
import { getSyntaxHighlightingColors, KidLisp } from "../lib/kidlisp.mjs";
import { MatrixChunky8 } from "./common/fonts.mjs";

const DEBUG_MODE = true;
const TEXT_SCALE = 1.0;
const LINE_HEIGHT = 12;
const PAUSE_DURATION = 120; // 2 seconds to read final code

let conversation,
  executeKidlisp,
  fullCode = "PROCESSING...",
  currentCode = "",
  abort,
  frameCount = 0,
  lastValidCode = "",
  scrollOffset = 0,
  streaming = false,
  completionTime = 0,
  originalLabel = "",
  originalParams = [],
  userPromptText = "",
  atParams = [],
  currentParamIndex = 0,
  lastWordChangeTime = 0,
  processedWords = new Set(),
  foundParameters = new Set(),
  streamingEndTime = 0,
  labelRestored = false,
  lastParameterFoundFrame = 0,
  characterBuffer = "",
  displayedCode = "",
  totalCharactersProcessed = 0, // Track total characters processed for shaking animation
  lastCharacterDisplayFrame = 0,
  state = {},
  makingProgress = 0.0,
  kidlispInstance = null; // Store KidLisp instance for execution

// 🥾 Boot (Runs once before first paint and sim)
function boot({ params, system: { painting }, store, slug, lisp, hud: { label, currentLabel } }) {
  // Clear MatrixChunky8 font cache to pick up any changes to advance values
  if (typeof window !== 'undefined' && window.typefaceCache) {
    const beforeSize = Object.keys(window.typefaceCache).length;
    delete window.typefaceCache["MatrixChunky8"];
    const afterSize = Object.keys(window.typefaceCache).length;
    console.log("🔄 Font cache before:", beforeSize, "after:", afterSize);
    console.log("🔄 Cache keys:", Object.keys(window.typefaceCache));
  } else {
    console.log("❌ No typefaceCache found on window");
  }
  
  // Store original label and parameters
  originalLabel = currentLabel().text || "";
  originalParams = [...params];
  userPromptText = params.join(" ");
  atParams = params.map((param) => `/* @${param} */`);
  currentParamIndex = -1;
  
  // Reset all state
  lastValidCode = store["make-last-kidlisp-code"] || "";
  processedWords.clear();
  foundParameters.clear();
  streamingEndTime = 0;
  labelRestored = false;
  characterBuffer = "";
  displayedCode = "";
  totalCharactersProcessed = 0;
  state = {};
  makingProgress = 0.0;
  kidlispInstance = lisp; // Store lisp instance

  // Load KidLisp prompt template
  let promptLoaded = false;

  // Show last valid code while loading if available
  if (lastValidCode.length > 0) {
    fullCode = lastValidCode;
    executeKidlisp = createKidlispExecutor(lastValidCode);
    console.log("🔄 Showing cached KidLisp while generating new code...");
  }

  fetch("../prompts/kidlisp-make.prompt")
    .then((response) => {
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
      return response.text();
    })
    .then((text) => {
      // Check if we got HTML instead of the prompt (indicates wrong path)
      if (text.includes('<!doctype html>')) {
        throw new Error("Received HTML instead of prompt file - incorrect path");
      }
      
      const prompt = text.replace("{{USER_PROMPT}}", userPromptText || "colorful abstract art");
      console.log("📋 KidLisp Prompt loaded successfully");
      promptLoaded = true;
      initializeConversation(prompt);
    })
    .catch((error) => {
      console.error("❌ Failed to load prompt:", error);
      
      // Fallback to inline prompt if file loading fails
      const fallbackPrompt = `Generate creative KidLisp code for: "${userPromptText || "colorful abstract art"}"

KidLisp uses S-expressions like (function arg1 arg2). 
Key functions: (wipe color), (ink color), (line x1 y1 x2 y2), (box x y w h), (circle x y r), (wiggle n)
Always start with (wipe color) and use (ink color) before drawing.
Generate animated, creative code only - no explanations.`;
      
      console.log("📋 Using fallback prompt");
      promptLoaded = true;
      initializeConversation(fallbackPrompt);
    });

  function initializeConversation(promptText) {
    conversation = new Conversation(store, slug);
    conversation.retrieve().then(() => {
      console.log("🚀 Starting KidLisp generation for:", userPromptText);
      console.log("📋 Using prompt text length:", promptText.length);
      console.log("🔧 Conversation config:", { store: !!store, slug });
      
      streaming = true;
      makingProgress = 0.0;
      
      // Create the program structure expected by ask.mjs
      const program = {
        before: promptText, // The KidLisp generation instructions
        after: "" // No additional instructions needed
      };
      
      console.log("📡 Sending API request...");
      
      abort = conversation.ask(
        { 
          prompt: userPromptText || "colorful abstract art", // The user's actual prompt
          program: program,
          hint: "code:claude-sonnet-4-20250514", // Use Claude Sonnet 4 (same as oldmake)
          temperature: 1
        },
        function onChunk(msg) {
          console.log("📥 Received chunk:", msg.length, "chars");
          characterBuffer += msg;
        },
        function onComplete(finalMsg) {
          streaming = false;
          completionTime = frameCount;
          console.log("✅ KidLisp generation complete");
          console.log("📜 Final generated code:", displayedCode + characterBuffer);
          
          // DON'T try to execute - just display the final code
          // tryExecuteKidlispCode(displayedCode + characterBuffer, true);
        },
        function onError(error) {
          streaming = false;
          console.error("❌ Network failure during generation:", error);
          console.log("🔍 Error details:", JSON.stringify(error, null, 2));
          
          // Provide a fallback KidLisp example based on the prompt
          const fallbackCode = generateFallbackKidlisp(userPromptText);
          console.log("🔄 Using fallback KidLisp code:", fallbackCode);
          
          // DON'T execute fallback either - just display it
          characterBuffer = fallbackCode;
          fullCode = fallbackCode;
          console.log("📜 Fallback code:", fallbackCode);
          // tryExecuteKidlispCode(fallbackCode, true);
        }
      );
    }).catch((retrieveError) => {
      console.error("❌ Failed to retrieve conversation:", retrieveError);
      
      // Even if conversation retrieval fails, provide fallback
      const fallbackCode = generateFallbackKidlisp(userPromptText);
      console.log("🔄 Using fallback due to conversation retrieval failure:", fallbackCode);
      characterBuffer = fallbackCode;
      fullCode = fallbackCode;
      tryExecuteKidlispCode(fallbackCode, true);
    });
  }
}

// 🧮 Sim (Runs once per logic frame)
function sim({ event, jump, reload, simCount, sound }) {
  // Process character buffer with typing sound
  if (characterBuffer.length && simCount % 2n === 0n) {
    const char = characterBuffer[0];
    displayedCode += char;
    characterBuffer = characterBuffer.slice(1);
    totalCharactersProcessed++; // Increment counter for shaking animation

    // Typing sound effect
    sound.synth({
      type: "sine",
      tone: 700 + Math.random() * 200,
      duration: 0.005,
      attack: 0,
      decay: 0,
      volume: 0.15,
    });

    // Check for parameter matches in the growing code
    checkForParamMatches(displayedCode);
    
    // DON'T try to execute until streaming is complete - just display
    // This prevents errors from incomplete code execution
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
  paintCount,
  hud: { label, currentLabel },
  store,
  kidlisp,
  box
}) {
  frameCount = Number(paintCount);
  
  // Update making progress
  if (streaming && originalParams) {
    makingProgress = Math.min(0.95, foundParameters.size / Math.max(1, originalParams.length));
  } else if (!streaming && !characterBuffer.length) {
    makingProgress = 1.0;
  }

  // First: Clear to dark gray background
  wipe([64, 64, 64]); // Dark gray RGB

  // Second: Execute KidLisp code in bottom right corner as preview (BACKGROUND LAYER)
  const fullCode = displayedCode + characterBuffer;
  if (fullCode && fullCode.trim()) {
    // Small preview window in bottom right
    const previewSize = 120;
    const margin = 10;
    const previewX = screen.width - previewSize - margin;
    const previewY = screen.height - previewSize - margin;
    
    // Force synchronous KidLisp execution by creating a painting buffer
    const kidlispPainting = painting(previewSize, previewSize, (api) => {
      try {
        // Check for special first line handling (rainbow, zebra, or single colors)
        const firstLine = fullCode.trim().split('\n')[0].trim();
        const specialEffects = ['rainbow', 'zebra'];
        const colorWords = ['red', 'blue', 'green', 'yellow', 'orange', 'purple', 'cyan', 'magenta', 'pink', 'lime', 'navy', 'black', 'white', 'gray', 'brown', 'darkred', 'darkblue', 'darkgreen'];
        
        let needsPersistentBg = false;
        if (colorWords.includes(firstLine.toLowerCase()) || specialEffects.includes(firstLine.toLowerCase())) {
          needsPersistentBg = true;
        }
        
        // Only wipe with dark background if NOT a persistent background case
        if (!needsPersistentBg) {
          api.wipe([32, 32, 32]);
        }
        // For persistent backgrounds, don't wipe - let KidLisp handle the background
        
        // Extend the painting API with missing functions that KidLisp needs
        if (!api.time) {
          api.time = performance.now() * 0.001; // Convert to seconds like main API
        }
        if (!api.typeface) {
          api.typeface = function(font) {
            // Simple fallback - painting API might not support full font changes
            console.log("📝 KidLisp typeface:", font);
          };
        }
        if (!api.size) {
          api.size = function(width, height) {
            // Size function for changing canvas dimensions
            console.log("📐 KidLisp size:", width, height);
          };
        }
        if (!api.width) {
          api.width = screen.width;
        }
        if (!api.height) {
          api.height = screen.height;
        }
        if (!api.w) {
          api.w = previewSize;
        }
        if (!api.h) {
          api.h = previewSize;
        }
        
        // Try to access the global KidLisp instance properly
        let kl = null;
        
        // Check globalThis first (works in all environments)
        if (globalThis.globalKidLispInstance) {
          kl = globalThis.globalKidLispInstance;
        } else if (typeof window !== 'undefined' && window.globalKidLispInstance) {
          kl = window.globalKidLispInstance;
        } else if (typeof global !== 'undefined' && global.globalKidLispInstance) {
          kl = global.globalKidLispInstance;
        }
        
        if (kl && kl.setAPI && kl.parse && kl.evaluate) {
          // Save original state
          const originalInEmbedPhase = kl.inEmbedPhase;
          const originalIsNestedInstance = kl.isNestedInstance;
          const originalEmbeddedLayers = kl.embeddedLayers;
          
          // Force immediate execution mode
          kl.inEmbedPhase = true;
          kl.isNestedInstance = true;
          kl.embeddedLayers = null;
          
          // Execute KidLisp
          kl.setAPI(api);
          kl.firstLineColor = null;
          kl.parse(fullCode);
          if (kl.ast && kl.ast.length > 0) {
            kl.evaluate(kl.ast, api, kl.localEnv || {});
          }
          
          // Restore original state
          kl.inEmbedPhase = originalInEmbedPhase;
          kl.isNestedInstance = originalIsNestedInstance;
          kl.embeddedLayers = originalEmbeddedLayers;
        } else {
          // Create an isolated KidLisp instance for synchronous execution to prevent state contamination
          try {
            if (kidlispInstance && kidlispInstance.KidLisp) {
              // Create a new isolated KidLisp instance
              const isolatedKidLisp = new kidlispInstance.KidLisp();
              
              // Set up the isolated instance with the painting API
              isolatedKidLisp.setAPI(api);
              isolatedKidLisp.frameCount = 0; // Reset frame count for isolation
              
              // Clear any inherited state that might cause contamination
              isolatedKidLisp.localEnv = {}; // Reset local environment
              isolatedKidLisp.variables = {}; // Reset variables
              isolatedKidLisp.firstLineColor = null; // Reset color state
              
              // Parse and evaluate in the isolated instance
              isolatedKidLisp.parse(fullCode);
              
              if (isolatedKidLisp.ast && isolatedKidLisp.ast.length > 0) {
                // Validate AST for common errors before evaluation
                let hasIncompleteExpressions = false;
                const validateExpression = (expr) => {
                  if (Array.isArray(expr)) {
                    if (expr[0] === 'def' && expr.length !== 3) {
                      hasIncompleteExpressions = true;
                      console.warn(`❌ Incomplete def statement:`, expr);
                      return false;
                    }
                    // Recursively validate nested expressions
                    for (let i = 1; i < expr.length; i++) {
                      if (!validateExpression(expr[i])) return false;
                    }
                  }
                  return true;
                };
                
                // Validate each top-level expression
                for (const expr of isolatedKidLisp.ast) {
                  if (!validateExpression(expr)) {
                    hasIncompleteExpressions = true;
                    break;
                  }
                }
                
                if (!hasIncompleteExpressions) {
                  const result = isolatedKidLisp.evaluate(isolatedKidLisp.ast, api, isolatedKidLisp.localEnv || {});
                  console.log("✅ KidLisp executed in isolated instance:", result);
                } else {
                  console.warn("⚠️ Skipping execution due to incomplete expressions");
                  // Show warning state
                  api.ink(128, 128, 64);
                  api.box(0, 0, previewSize, previewSize);
                }
              } else {
                console.log("⚠️ No AST generated from code");
                // Show empty state
                api.ink(100, 100, 100);
                api.box(0, 0, previewSize, previewSize);
              }
            } else {
              console.warn("⚠️ kidlispInstance.KidLisp constructor not available");
              // Fallback: show a blue background
              api.ink(64, 64, 128);
              api.box(0, 0, previewSize, previewSize);
            }
          } catch (error) {
            console.error("❌ KidLisp isolated execution error:", error);
            // Error fallback: show red background
            api.ink(128, 64, 64);
            api.box(0, 0, previewSize, previewSize);
            api.ink("white");
            if (api.write) {
              api.write("Error", 10, 25);
            } else if (api.text) {
              api.text("Error", 10, 25);
            }
          }
        }
      } catch (error) {
        console.error("Synchronous KidLisp execution error:", error);
        api.ink("orange");
        api.box(5, 5, previewSize - 10, previewSize - 10);
      }
    });
    
    // Paste the synchronously rendered KidLisp output as background
    paste(kidlispPainting, previewX, previewY);
    
    // Then draw border around the preview window on top of the KidLisp output
    ink("white");
    // Top border
    box(previewX - 1, previewY - 1, previewSize + 2, 1);
    // Bottom border  
    box(previewX - 1, previewY + previewSize, previewSize + 2, 1);
    // Left border
    box(previewX - 1, previewY, 1, previewSize);
    // Right border
    box(previewX + previewSize, previewY, 1, previewSize);
  }

  // Third: Always show the prompt with highlighting and shaking animation (FOREGROUND LAYER)
  let promptBounds = { bottomY: 6 };
  if (originalParams && originalParams.length > 0) {
    // Hide HUD label and show our custom prompt
    label();
    promptBounds = renderPromptWithHighlighting(screen, ink, write, originalParams, currentParamIndex, typeface);
  }

  // Fourth: Show code overlay during generation (positioned below prompt, FOREGROUND LAYER)
  if (shouldShowCode()) {
    displayCodeOverlay(screen, ink, write, typeface, label, promptBounds.bottomY);
  }

  // Show progress bar during generation
  // if (streaming || characterBuffer.length) {
  //   displayProgressBar(screen, ink, write, typeface);
  // }
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave({ store }) {
  abort?.(); // Cancel any existing generation
  
  // Save the last valid KidLisp code
  if (lastValidCode) {
    store["make-last-kidlisp-code"] = lastValidCode;
  }
}

// 📚 Library Functions

function shouldShowCode() {
  const isCharacterStreaming = characterBuffer.length > 0;
  const hasParametersToHighlight = originalParams && originalParams.length > 0;
  const isHighlightingInProgress = hasParametersToHighlight && currentParamIndex < originalParams.length;
  
  return streaming || 
         isCharacterStreaming || 
         isHighlightingInProgress ||
         (streamingEndTime > 0 && frameCount - streamingEndTime < PAUSE_DURATION) ||
         (fullCode && fullCode.startsWith("ERROR"));
}

function displayCodeOverlay(screen, ink, write, typeface, label, codeStartY = 15) {
  if (!displayedCode && !characterBuffer.length) return;
  
  // Display the KidLisp code with proper line-by-line rendering
  const lineHeight = 12; // MatrixChunky8 line height
  
  if (displayedCode && displayedCode.trim()) {
    try {
      // Try with existing instance first, then create new one if needed
      let syntaxInstance = kidlispInstance;
      if (!syntaxInstance || typeof syntaxInstance.initializeSyntaxHighlighting !== 'function') {
        syntaxInstance = new KidLisp();
      }
      
      if (syntaxInstance && typeof syntaxInstance.initializeSyntaxHighlighting === 'function') {
        syntaxInstance.initializeSyntaxHighlighting(displayedCode);
        const coloredString = syntaxInstance.buildColoredKidlispString();
        
        if (coloredString && coloredString.trim()) {
          // Add dark shadow by writing plain text (no color codes) offset by 1 pixel
          // Use dark gray shadow since most syntax colors are bright
          ink([32, 32, 32]); // Dark gray RGB
          // Strip color codes for shadow by using the original displayedCode
          write(displayedCode, { x: 7, y: codeStartY + 3 }, undefined, screen.width - 16, true, "MatrixChunky8");
          // Then write the main colored text
          write(coloredString, { x: 6, y: codeStartY + 2 }, undefined, screen.width - 16, true, "MatrixChunky8");
        } else {
          // Fallback to simple green text with line-by-line rendering
          renderLinesWithColor(displayedCode, "lime", codeStartY, lineHeight, screen, ink, write);
        }
      } else {
        // Simple approach: render lines with green text
        renderLinesWithColor(displayedCode, "lime", codeStartY, lineHeight, screen, ink, write);
      }
    } catch (error) {
      console.warn("🎨 Syntax highlighting error:", error);
      // Fallback to simple green text with line-by-line rendering
      renderLinesWithColor(displayedCode, "lime", codeStartY, lineHeight, screen, ink, write);
    }
  }
}

// Helper function to render multi-line text with color and shadow
function renderLinesWithColor(text, color, startY, lineHeight, screen, ink, write) {
  const lines = text.split('\n');
  let currentY = startY;
  
  for (let i = 0; i < lines.length && currentY < screen.height - 50; i++) {
    const line = lines[i];
    if (line.trim()) {
      // Draw dark shadow first (offset by 1 pixel) - use dark gray for bright syntax colors
      ink([32, 32, 32]); // Dark gray RGB
      write(line, { x: 9, y: currentY + 1 }, undefined, screen.width - 16, false, "MatrixChunky8");
      // Draw main text
      ink(color);
      write(line, { x: 8, y: currentY }, undefined, screen.width - 16, false, "MatrixChunky8");
    }
    currentY += lineHeight;
  }
}

// Helper function to manually parse and render KidLisp colored string
function renderColoredKidlispString(coloredString, startY, lineHeight, screen, ink, write) {
  const lines = coloredString.split('\n');
  let currentY = startY;
  
  for (let lineIndex = 0; lineIndex < lines.length && currentY < screen.height - 50; lineIndex++) {
    const line = lines[lineIndex];
    if (!line.trim()) {
      currentY += lineHeight;
      continue;
    }
    
    // For now, let's try using the write function with the colored line directly
    // The write function might actually support color codes, let's test this approach
    write(line, { x: 8, y: currentY }, undefined, screen.width - 16, false, "MatrixChunky8");
    
    currentY += lineHeight;
  }
}

function displayProgressBar(screen, ink, write, typeface) {
  const barY = screen.height - 25;
  const barWidth = screen.width - 20;
  const barHeight = 6;
  
  // Background using matrix font
  ink("black", 100);
  const matrixCharWidth = 4;
  const fillChars = Math.floor(barWidth / matrixCharWidth);
  write("█".repeat(fillChars), { x: 10, y: barY }, undefined, undefined, false, "MatrixChunky8");
  
  // Progress fill
  const fillWidth = Math.floor(fillChars * makingProgress);
  ink("lime");
  write("█".repeat(fillWidth), { x: 10, y: barY }, undefined, undefined, false, "MatrixChunky8");
  
  // Progress text using matrix font
  const progressText = `Making: ${Math.floor(makingProgress * 100)}%`;
  ink("white");
  write(progressText, { x: 10, y: barY - 12 }, undefined, undefined, false, "MatrixChunky8");
}

function renderPromptWithHighlighting(screen, ink, write, params, currentIndex, typeface) {
  if (!params || !params.length) return { bottomY: 6 };
  
  const words = ["make", ...params];
  const startX = 6;
  let startY = 6;
  let currentX = startX;
  let maxY = startY;
  
  // Calculate current character based on total characters processed (one letter per chunk)
  const promptText = params.join(' '); // Only the user's prompt, not "make"
  const totalChars = promptText.length + Math.max(0, params.length - 1); // Include spaces between params
  const currentShakeChar = Math.min(totalCharactersProcessed, totalChars); // One char per chunk processed
  let promptCharIndex = 0; // Index within the user's prompt only
  
  words.forEach((word, index) => {
    const isHighlighted = index === currentIndex + 1; // +1 because "make" is at index 0
    const isFound = foundParameters.has(index - 1); // -1 because "make" doesn't count as a parameter
    const isMakeWord = index === 0;
    
    // Render each character individually
    for (let charIndex = 0; charIndex < word.length; charIndex++) {
      const char = word[charIndex];
      
      let shakeX = 0, shakeY = 0;
      
      if (!isMakeWord && promptCharIndex === currentShakeChar && (streaming || characterBuffer.length)) {
        // Wiggle the current character during generation (one letter per chunk)
        shakeX = (Math.random() - 0.5) * 4;
        shakeY = (Math.random() - 0.5) * 4;
        ink("cyan");
      } else if (!streaming && !characterBuffer.length) {
        // When generation is complete, make ALL words lime (including "make")
        ink("lime");
      } else if (isMakeWord) {
        // "make" is always white and never shakes
        ink("white");
      } else if (isHighlighted) {
        ink("yellow");
      } else if (isFound) {
        ink("lime");
      } else {
        ink("white", 150);
      }
      
      // Add shadow for the prompt text
      ink([32, 32, 32]); // Dark gray shadow
      write(char, { x: currentX + shakeX + 1, y: startY + shakeY + 1 }, undefined, undefined, false);
      
      // Set the main color again (since we just used shadow color)
      if (!isMakeWord && promptCharIndex === currentShakeChar && (streaming || characterBuffer.length)) {
        ink("cyan");
      } else if (!streaming && !characterBuffer.length) {
        ink("lime");
      } else if (isMakeWord) {
        ink("white");
      } else if (isHighlighted) {
        ink("yellow");
      } else if (isFound) {
        ink("lime");
      } else {
        ink("white", 150);
      }
      
      // Use original font (not MatrixChunky8) for the prompt
      write(char, { x: currentX + shakeX, y: startY + shakeY }, undefined, undefined, false);
      currentX += typeface.blockWidth; // Move to next character position
      
      // Track the maximum Y position
      maxY = Math.max(maxY, startY + typeface.blockHeight);
      
      // Only increment prompt char index for non-"make" words
      if (!isMakeWord) {
        promptCharIndex++;
      }
    }
    
    // Add space between words
    currentX += typeface.blockWidth;
    // Only count space in prompt char index if we're past "make" and not on the last word
    if (!isMakeWord && index < words.length - 1) {
      promptCharIndex++;
    }
    
    if (currentX > screen.width - 50) {
      currentX = startX;
      startY += typeface.blockHeight; // Use typeface line height
      maxY = Math.max(maxY, startY + typeface.blockHeight);
    }
  });
  
  // Return the bounding box info
  return { bottomY: maxY + 4 }; // Add small margin
}

function checkForParamMatches(code) {
  if (!code || !atParams.length) return;
  
  for (let paramIndex = 0; paramIndex < atParams.length; paramIndex++) {
    if (foundParameters.has(paramIndex)) continue; // Skip already found parameters
    
    const paramPattern = atParams[paramIndex];
    const paramWord = originalParams[paramIndex];
    
    // Check if the parameter word appears in the code (case insensitive)
    if (code.toLowerCase().includes(paramWord.toLowerCase()) && 
        paramIndex === currentParamIndex + 1) { // Sequential highlighting
      
      foundParameters.add(paramIndex);
      currentParamIndex = paramIndex;
      lastParameterFoundFrame = frameCount;
      
      if (DEBUG_MODE) {
        console.log(`✅ Found parameter ${paramIndex + 1}/${originalParams.length}: "${paramWord}"`);
      }
      break; // Only highlight one parameter at a time
    }
  }
}

function createKidlispExecutor(code) {
  if (!code || !kidlispInstance) return null;
  
  try {
    // Clean up the code - remove any markdown formatting that might have slipped through
    let cleanCode = code.trim();
    if (cleanCode.startsWith('```')) {
      cleanCode = cleanCode.replace(/```[\w]*\n?/g, '').replace(/```$/g, '').trim();
    }
    
    // Parse the KidLisp code
    const parsed = kidlispInstance.parse(cleanCode);
    console.log("🎨 KidLisp parsed successfully");
    
    // Return an executor function
    return function(api) {
      try {
        kidlispInstance.evaluate(parsed, api);
      } catch (evalError) {
        console.error("❌ KidLisp evaluation error:", evalError);
        // Don't throw - just log and continue with last working version
      }
    };
  } catch (parseError) {
    console.error("❌ KidLisp parse error:", parseError);
    console.log("Raw code that failed to parse:", code);
    return null;
  }
}

function tryExecuteKidlispCode(code, isFinal = false) {
  if (!code || !code.trim()) return;
  
  const newExecutor = createKidlispExecutor(code);
  if (newExecutor) {
    executeKidlisp = newExecutor;
    lastValidCode = code;
    
    if (isFinal) {
      fullCode = code;
      console.log("🎨 Final KidLisp code compiled successfully");
      
      // Log the generated code for debugging
      console.log("%cGenerated KidLisp Code:", 
        "background: purple; color: lime; font-weight: bold; padding: 4px; font-family: monospace;");
      console.log("%c" + code, 
        "background: black; color: lime; padding: 8px; font-family: monospace; white-space: pre;");
    }
  } else if (isFinal) {
    fullCode = "COMPILATION ERROR: " + code;
    console.error("❌ Failed to compile final KidLisp code");
  }
}

export { boot, paint, sim, leave };

// 📚 Library Functions - Matrix Font and Syntax Highlighting

function renderMatrixText(write, text, position, font, maxWidth, lineHeight) {
  // Simple matrix font rendering - write handles font automatically
  write(text, position, font, maxWidth);
}

function renderSyntaxHighlightedCode(screen, ink, write, code, syntaxColors, startY, charWidth, lineHeight, font) {
  const lines = code.split('\n');
  let currentY = startY;
  let colorIndex = 0;
  
  for (let lineIndex = 0; lineIndex < lines.length && currentY < screen.height - 50; lineIndex++) {
    const line = lines[lineIndex];
    let currentX = 8;
    
    // Render each character with its syntax color
    for (let charIndex = 0; charIndex < line.length; charIndex++) {
      const char = line[charIndex];
      
      // Get color for this character position (if available)
      if (colorIndex < syntaxColors.length) {
        const colorInfo = syntaxColors[colorIndex];
        if (colorInfo && colorInfo.color) {
          // Parse color and set ink
          if (typeof colorInfo.color === 'string') {
            ink(colorInfo.color);
          } else if (Array.isArray(colorInfo.color) && colorInfo.color.length >= 3) {
            ink(colorInfo.color[0], colorInfo.color[1], colorInfo.color[2]);
          } else {
            ink("lime"); // fallback
          }
        } else {
          ink("lime"); // fallback
        }
      } else {
        ink("lime"); // fallback
      }
      
      // Render single character
      write(char, { x: currentX, y: currentY }, font);
      
      // Advance position using MatrixChunky8 character widths
      const advance = font.advances && font.advances[char] ? font.advances[char] : charWidth;
      currentX += advance;
      colorIndex++;
      
      // Wrap to next line if needed
      if (currentX > screen.width - 20) {
        break;
      }
    }
    
    // Add newline character to color index
    if (lineIndex < lines.length - 1) {
      colorIndex++; // Account for newline character
    }
    
    currentY += lineHeight;
  }
}

function getColorFromSyntaxInfo(colorInfo) {
  if (!colorInfo || !colorInfo.color) return "lime";
  
  if (typeof colorInfo.color === 'string') {
    return colorInfo.color;
  } else if (Array.isArray(colorInfo.color) && colorInfo.color.length >= 3) {
    return colorInfo.color; // RGB array
  }
  
  return "lime"; // fallback
}

function formatKidlispCode(code) {
  // Basic formatting for better readability
  if (!code) return "";
  
  // Split into lines and clean up
  let lines = code.split('\n').map(line => line.trim()).filter(line => line.length > 0);
  
  // Add some basic indentation for nested expressions
  let formatted = [];
  let indentLevel = 0;
  
  for (let line of lines) {
    // Count opening and closing parens to determine indentation
    let openParens = (line.match(/\(/g) || []).length;
    let closeParens = (line.match(/\)/g) || []).length;
    
    // Decrease indent for closing parens at start of line
    if (line.startsWith(')')) {
      indentLevel = Math.max(0, indentLevel - 1);
    }
    
    // Add current line with indentation
    let indent = '  '.repeat(Math.max(0, indentLevel));
    formatted.push(indent + line);
    
    // Increase indent for opening parens
    indentLevel += openParens - closeParens;
    indentLevel = Math.max(0, indentLevel); // Don't go negative
  }
  
  return formatted.join('\n');
}

function generateFallbackKidlisp(prompt) {
  // Generate basic KidLisp code based on prompt keywords
  const lower = prompt.toLowerCase();
  let code = "";
  
  // Background color based on prompt
  if (lower.includes("dark") || lower.includes("night") || lower.includes("black")) {
    code += "(wipe black)\n";
  } else if (lower.includes("blue") || lower.includes("ocean") || lower.includes("water")) {
    code += "(wipe navy)\n";
  } else if (lower.includes("red") || lower.includes("fire") || lower.includes("warm")) {
    code += "(wipe darkred)\n";
  } else {
    code += "(wipe purple)\n";
  }
  
  // Generate patterns based on keywords
  if (lower.includes("stripe") || lower.includes("line")) {
    code += "(ink white)\n";
    code += "(line 0 50 width 50)\n";
    code += "(ink yellow)\n";
    code += "(line 0 100 width 100)\n";
    code += "(ink cyan)\n";
    code += "(line 0 150 width 150)\n";
    code += "(ink lime)\n";
    code += "(line 0 200 width 200)\n";
  } else if (lower.includes("circle") || lower.includes("dot") || lower.includes("bubble")) {
    code += "(ink yellow)\n";
    code += "(circle (+ 64 (wiggle 20)) (+ 64 (wiggle 20)) 30)\n";
    code += "(ink cyan)\n";
    code += "(circle (+ 192 (wiggle 20)) (+ 64 (wiggle 20)) 25)\n";
    code += "(ink lime)\n";
    code += "(circle (+ 128 (wiggle 20)) (+ 192 (wiggle 20)) 35)\n";
  } else if (lower.includes("square") || lower.includes("box") || lower.includes("rect")) {
    code += "(ink red)\n";
    code += "(box (+ 50 (wiggle 10)) (+ 50 (wiggle 10)) 50 50)\n";
    code += "(ink blue)\n";
    code += "(box (+ 150 (wiggle 10)) (+ 50 (wiggle 10)) 60 60)\n";
    code += "(ink green)\n";
    code += "(box (+ 100 (wiggle 10)) (+ 150 (wiggle 10)) 40 40)\n";
  } else {
    // Default abstract pattern
    code += "(ink yellow)\n";
    code += "(circle (+ 128 (wiggle 50)) (+ 128 (wiggle 50)) (+ 20 (wiggle 10)))\n";
    code += "(ink cyan 150)\n";
    code += "(box (+ 64 (wiggle 30)) (+ 64 (wiggle 30)) (+ 40 (wiggle 20)) (+ 40 (wiggle 20)))\n";
    code += "(ink lime)\n";
    code += "1s... (plot (wiggle width) (wiggle height))\n";
  }
  
  return code;
}
