// 🎨 Chat Message Syntax Highlighting
// Shared utilities for parsing and highlighting chat messages with @handles, URLs, 'prompts', and KidLisp code.

import { isKidlispSource, tokenize, KidLisp } from "./kidlisp.mjs";

// Parse prompts, URLs, @handles, and other special elements from a message string
// Returns an array of elements with {type, text, start, end, color?}
export function parseMessageElements(message) {
  const elements = [];

  // Parse prompts (text within single quotes) - ignore contractions like "I'll" or "you'll"
  // Require word boundary or whitespace before the opening quote
  const promptRegex = /(?:^|[\s\(\[\{])'([^']*)'/g;
  let match;
  while ((match = promptRegex.exec(message)) !== null) {
    // Adjust index if we matched a preceding whitespace/boundary character
    const actualStart = match[0].startsWith("'") ? match.index : match.index + 1;
    const promptText = match[1]; // Captured group without quotes
    
    // Check if this prompt is kidlisp code
    if (isKidlispSource(promptText)) {
      // Add green opening quote
      elements.push({
        type: "prompt",
        text: "'",
        start: actualStart,
        end: actualStart + 1,
      });
      
      // Parse as kidlisp and create elements for each token with syntax highlighting
      const tokens = tokenize(promptText);
      const kidlispInstance = new KidLisp();
      
      let charOffset = actualStart + 1; // +1 to skip opening quote
      
      for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];
        const tokenStart = promptText.indexOf(token, charOffset - (actualStart + 1));
        
        if (tokenStart >= 0) {
          elements.push({
            type: "kidlisp-token",
            text: token,
            start: actualStart + 1 + tokenStart,
            end: actualStart + 1 + tokenStart + token.length,
            color: kidlispInstance.getTokenColor(token, tokens, i),
          });
          charOffset = actualStart + 1 + tokenStart + token.length;
        }
      }
      
      // Add green closing quote
      elements.push({
        type: "prompt",
        text: "'",
        start: actualStart + 1 + promptText.length,
        end: actualStart + 1 + promptText.length + 1,
      });
    } else {
      // Not kidlisp, regular prompt - separate quotes from content
      // Opening quote
      elements.push({
        type: "prompt",
        text: "'",
        start: actualStart,
        end: actualStart + 1,
      });
      
      // Inner content (different color)
      elements.push({
        type: "prompt-content",
        text: promptText,
        start: actualStart + 1,
        end: actualStart + 1 + promptText.length,
      });
      
      // Closing quote
      elements.push({
        type: "prompt",
        text: "'",
        start: actualStart + 1 + promptText.length,
        end: actualStart + 1 + promptText.length + 1,
      });
    }
  }

  // Parse URLs (starting with http://, https://, or www.)
  const urlRegex = /(https?:\/\/[^\s]+|www\.[^\s]+)/g;
  const urlRanges = []; // Track URL positions to avoid matching hashtags inside them
  while ((match = urlRegex.exec(message)) !== null) {
    elements.push({
      type: "url",
      text: match[0],
      start: match.index,
      end: match.index + match[0].length,
    });
    urlRanges.push({ start: match.index, end: match.index + match[0].length });
  }

  // Parse @handles
  // Handles can only contain a-z, 0-9, underscores and periods (not at start/end)
  // They should stop at quotes, parentheses, and other special characters
  const handleRegex = /@[a-z0-9]+([._][a-z0-9]+)*/gi;
  while ((match = handleRegex.exec(message)) !== null) {
    elements.push({
      type: "handle",
      text: match[0],
      start: match.index,
      end: match.index + match[0].length,
    });
  }

  // Parse #painting codes (hashtags followed by alphanumeric characters)
  // Painting codes are typically 3 characters but can vary (e.g., #k3d, #WDv, #abc)
  // Skip hashtags that are inside URLs (e.g., https://example.com#section)
  const hashtagRegex = /#[a-z0-9]+/gi;
  while ((match = hashtagRegex.exec(message)) !== null) {
    // Check if this hashtag is inside a URL
    const isInsideUrl = urlRanges.some(range => 
      match.index >= range.start && match.index < range.end
    );
    
    if (!isInsideUrl) {
      elements.push({
        type: "painting",
        text: match[0],
        start: match.index,
        end: match.index + match[0].length,
      });
    }
  }

  // Parse $kidlisp inline references (dollar sign followed by alphanumeric)
  const kidlispRefRegex = /\$[a-z0-9]+/gi;
  while ((match = kidlispRefRegex.exec(message)) !== null) {
    elements.push({
      type: "kidlisp",
      text: match[0],
      start: match.index,
      end: match.index + match[0].length,
    });
  }

  // Parse !tape URLs (exclamation followed by alphanumeric)
  const tapeRegex = /![a-z0-9]+/gi;
  while ((match = tapeRegex.exec(message)) !== null) {
    elements.push({
      type: "url",
      text: match[0],
      start: match.index,
      end: match.index + match[0].length,
    });
  }

  // Sort elements by start position to ensure proper ordering
  elements.sort((a, b) => a.start - b.start);

  // Remove elements that are completely inside quoted sections
  // (to avoid double-highlighting things like '#abc' when it's inside 'run #abc')
  const quotedRanges = elements
    .filter(el => el.type === "prompt" || el.type === "kidlisp-token" || el.type === "prompt-content")
    .map(el => ({ start: el.start, end: el.end }));
  
  // Merge overlapping quoted ranges
  const mergedQuotedRanges = [];
  for (const range of quotedRanges) {
    if (mergedQuotedRanges.length === 0) {
      mergedQuotedRanges.push(range);
    } else {
      const last = mergedQuotedRanges[mergedQuotedRanges.length - 1];
      if (range.start <= last.end) {
        last.end = Math.max(last.end, range.end);
      } else {
        mergedQuotedRanges.push(range);
      }
    }
  }
  
  // Filter out elements that are inside quotes (except quote-related elements)
  const filteredElements = elements.filter(el => {
    if (el.type === "prompt" || el.type === "kidlisp-token" || el.type === "prompt-content") {
      return true; // Keep quote-related elements
    }
    
    // Check if this element is inside any quoted range
    for (const range of mergedQuotedRanges) {
      if (el.start >= range.start && el.end <= range.end) {
        return false; // Remove it - it's inside quotes
      }
    }
    return true; // Keep it
  });

  return filteredElements;
}

// Apply color codes to a message string based on parsed elements
// Returns a string with embedded \color\ codes for rendering
// colorMap should map element types to color strings/arrays: { handle: "pink", url: "cyan", ... }
export function applyColorCodes(message, elements, colorMap, defaultColor = [255, 255, 255]) {
  if (!elements || elements.length === 0) {
    return message; // No highlighting needed
  }

  // Build the color-coded string by processing elements in reverse order (to maintain indices)
  let colorCodedMessage = message;
  const sortedElements = [...elements].sort((a, b) => b.start - a.start);

  for (const element of sortedElements) {
    // Get color for this element type
    let color;
    if (element.type === "kidlisp-token" && element.color) {
      color = element.color; // Use token-specific color
    } else {
      color = colorMap[element.type] || colorMap[element.type.replace("-", "")]; // Try with/without hyphen
    }

    if (color) {
      const colorStr = Array.isArray(color) ? color.join(',') : color;
      const defaultColorStr = Array.isArray(defaultColor) ? defaultColor.join(',') : defaultColor;
      
      const textBefore = colorCodedMessage.substring(0, element.start);
      const elementText = colorCodedMessage.substring(element.start, element.end);
      const textAfter = colorCodedMessage.substring(element.end);
      
      colorCodedMessage = `${textBefore}\\${colorStr}\\${elementText}\\${defaultColorStr}\\${textAfter}`;
    }
  }

  return colorCodedMessage;
}

// Default color theme for chat highlighting
export const defaultColorTheme = {
  handle: "pink",
  url: "cyan",
  prompt: "lime",
  promptcontent: "cyan", // Note: type is "prompt-content" but map key has no hyphen
  painting: "orange",
  kidlisp: "magenta",
};
