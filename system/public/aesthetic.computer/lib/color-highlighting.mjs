// 🎨 Color Highlighting System
// Shared color highlighting utilities for both kidlisp and nopaint systems
// Extracted from kidlisp.mjs to ensure uniform highlighting across the platform

import { cssColors, staticColorMap } from "./num.mjs";

/**
 * Determine the highlight color for a specific color token
 * This replicates kidlisp's getTokenColor logic for color names
 * @param {string} token - The color token to highlight
 * @returns {string} Color value for highlighting (CSS color name or RGB)
 */
export function getColorTokenHighlight(token) {
  if (!token) return "white";

  // Handle quoted strings by removing quotes
  const cleanToken = token.startsWith('"') && token.endsWith('"') 
    ? token.slice(1, -1) 
    : token;

  // Check for valid CSS color name
  if (cssColors && cssColors[cleanToken]) {
    // Return the actual color value for CSS colors like "red", "blue", etc.
    const colorValue = cssColors[cleanToken];
    if (Array.isArray(colorValue) && colorValue.length >= 3) {
      const rgbColor = `${colorValue[0]},${colorValue[1]},${colorValue[2]}`;
      return rgbColor;
    }
  }
  
  // Check if this is a color code like "c0", "c1", etc.
  if (cleanToken.match(/^c\d+$/)) {
    const index = parseInt(cleanToken.substring(1));
    if (staticColorMap && staticColorMap[index]) {
      const colorValue = staticColorMap[index];
      if (Array.isArray(colorValue) && colorValue.length >= 3) {
        const rgbColor = `${colorValue[0]},${colorValue[1]},${colorValue[2]}`;
        return rgbColor;
      }
    }
  }

  // Special case for "rainbow" - return special marker for rainbow coloring
  if (cleanToken === "rainbow") {
    return "RAINBOW";
  }
  
  // Special case for "zebra" - return special marker for zebra coloring
  if (cleanToken === "zebra") {
    return "ZEBRA";
  }
  
  // Check if this is a fade expression like "fade:red-blue-yellow"
  if (cleanToken.startsWith("fade:")) {
    return "mediumseagreen"; // Give fade expressions a distinct emerald color
  }

  // Default for unrecognized colors
  return "orange";
}

/**
 * Apply color highlighting to a color word, including special cases
 * @param {string} colorName - The color name to highlight
 * @returns {string} Color-escaped string ready for HUD display
 */
export function colorizeColorName(colorName) {
  if (!colorName) return colorName;

  // Remove quotes if present
  const cleanName = colorName.startsWith('"') && colorName.endsWith('"') 
    ? colorName.slice(1, -1) 
    : colorName;

  const color = getColorTokenHighlight(cleanName);

  // Special handling for rainbow coloring
  if (color === "RAINBOW" && cleanName === "rainbow") {
    // ROYGBIV rainbow colors for each character
    const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
    let result = "";
    for (let charIndex = 0; charIndex < cleanName.length; charIndex++) {
      const charColor = rainbowColors[charIndex % rainbowColors.length];
      result += `\\${charColor}\\${cleanName[charIndex]}`;
    }
    return result;
  }

  // Special handling for zebra coloring
  if (color === "ZEBRA" && cleanName === "zebra") {
    // Alternating black/white colors for each character
    const zebraColors = ["black", "white"];
    let result = "";
    for (let charIndex = 0; charIndex < cleanName.length; charIndex++) {
      const charColor = zebraColors[charIndex % zebraColors.length];
      result += `\\${charColor}\\${cleanName[charIndex]}`;
    }
    return result;
  }

  // Special handling for fade expressions like "fade:red-blue-yellow"
  if (cleanName.startsWith("fade:") && color === "mediumseagreen") {
    return colorFadeExpression(cleanName);
  }

  // Default coloring
  return `\\${color}\\${cleanName}`;
}

/**
 * Color a fade expression like "fade:red-blue-yellow" with each color in its own color
 * This is extracted from kidlisp's colorFadeExpression method
 * @param {string} fadeToken - The fade expression token
 * @returns {string} Color-escaped string for the fade expression
 */
export function colorFadeExpression(fadeToken) {
  if (!fadeToken.startsWith("fade:")) {
    return fadeToken; // Fallback to original token
  }

  const parts = fadeToken.split(":");
  if (parts.length < 2) {
    return fadeToken; // Invalid fade syntax
  }
  
  // Check for neat modifier and handle different positions
  let isNeat = false;
  let colorPart = parts[1];
  let direction = parts[2];
  
  if (parts[1] === "neat" && parts[2]) {
    // Format: fade:neat:red-blue or fade:neat:red-blue:vertical
    isNeat = true;
    colorPart = parts[2];
    direction = parts[3];
  } else if (parts[2] === "neat") {
    // Format: fade:red-blue:neat
    isNeat = true;
    direction = undefined;
  } else if (parts[3] === "neat") {
    // Format: fade:red-blue:vertical:neat
    isNeat = true;
    direction = parts[2];
  } else if (parts.includes("neat")) {
    // Handle neat anywhere in the string
    isNeat = true;
    // Remove neat from parts and reconstruct
    const filteredParts = parts.filter(p => p !== "neat");
    if (filteredParts.length >= 2) {
      colorPart = filteredParts[1];
      direction = filteredParts[2];
    }
  }
  
  const colorNames = colorPart.split("-");
  
  let result = "\\mediumseagreen\\fade\\lime\\:"; // "fade" in emerald, ":" in green
  
  // Add neat modifier if present
  if (isNeat) {
    result += "\\cyan\\neat\\lime\\:"; // "neat" in cyan, ":" in green
  }
  
  for (let i = 0; i < colorNames.length; i++) {
    const colorName = colorNames[i];
    
    // Get the color for this color name
    let colorValue = "white"; // Default fallback
    
    if (cssColors && cssColors[colorName]) {
      // Use the actual color if it's a valid CSS color
      const rgbColor = cssColors[colorName];
      if (Array.isArray(rgbColor) && rgbColor.length >= 3) {
        colorValue = `${rgbColor[0]},${rgbColor[1]},${rgbColor[2]}`;
      }
    } else if (colorName.match(/^c\d+$/)) {
      // Handle color codes like c0, c1, etc.
      const index = parseInt(colorName.substring(1));
      if (staticColorMap && staticColorMap[index]) {
        const rgbColor = staticColorMap[index];
        if (Array.isArray(rgbColor) && rgbColor.length >= 3) {
          colorValue = `${rgbColor[0]},${rgbColor[1]},${rgbColor[2]}`;
        }
      }
    } else if (colorName === "rainbow") {
      colorValue = "RAINBOW"; // Special case for rainbow
    } else if (colorName === "zebra") {
      colorValue = "ZEBRA"; // Special case for zebra
    }
    
    // Add the colored color name
    if (colorValue === "RAINBOW") {
      // Special rainbow handling
      const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
      for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
        const charColor = rainbowColors[charIndex % rainbowColors.length];
        result += `\\${charColor}\\${colorName[charIndex]}`;
      }
    } else if (colorValue === "ZEBRA") {
      // Special zebra handling (alternating black/white)
      const zebraColors = ["black", "white"];
      for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
        const charColor = zebraColors[charIndex % zebraColors.length];
        result += `\\${charColor}\\${colorName[charIndex]}`;
      }
    } else {
      result += `\\${colorValue}\\${colorName}`;
    }
    
    // Add the dash separator in emerald (except for the last color)
    if (i < colorNames.length - 1) {
      result += "\\mediumseagreen\\-";
    }
  }
  
  // Add direction part if present
  if (direction) {
    result += "\\lime\\:"; // ":" in green
    
    // Check if direction is numeric (angle)
    const numericAngle = parseFloat(direction);
    if (!isNaN(numericAngle)) {
      result += `\\yellow\\${direction}`; // Numeric angle in yellow
    } else {
      result += `\\cyan\\${direction}`; // Named direction in cyan
    }
  }
  
  return result;
}

/**
 * Generate a colored HUD label for nopaint brush commands
 * @param {string} brushName - The brush name (e.g., "rect", "line")
 * @param {Array} colorParams - The parsed color parameters
 * @param {Array} rawParams - The raw parameter strings from user input
 * @param {string} modifiers - Any brush modifiers (e.g., ":o:c" for outline center)
 * @returns {string} Color-escaped string ready for HUD display
 */
export function generateNopaintHUDLabel(brushName, colorParams, rawParams, modifiers = "") {
  if (!rawParams || rawParams.length === 0) {
    return `${brushName}${modifiers}`;
  }

  let result = `\\cyan\\${brushName}`;
  
  if (modifiers) {
    result += `\\gray\\${modifiers}`;
  }
  
  result += " ";

  // Handle the first parameter which should be a color
  const firstParam = rawParams[0];
  if (firstParam) {
    result += colorizeColorName(firstParam);
  }

  // Add any additional parameters (like alpha values)
  for (let i = 1; i < rawParams.length; i++) {
    result += ` \\orange\\${rawParams[i]}`;
  }

  return result;
}

/**
 * Check if a color string is valid (CSS color, color code, rainbow, zebra, or fade)
 * @param {string} colorStr - The color string to validate
 * @returns {boolean} True if the color is valid
 */
export function isValidColorString(colorStr) {
  if (!colorStr) return false;

  const cleanStr = colorStr.startsWith('"') && colorStr.endsWith('"') 
    ? colorStr.slice(1, -1) 
    : colorStr;

  // Check CSS colors
  if (cssColors && cssColors[cleanStr]) return true;
  
  // Check color codes (c0, c1, etc.)
  if (cleanStr.match(/^c\d+$/)) {
    const index = parseInt(cleanStr.substring(1));
    return staticColorMap && staticColorMap[index] !== undefined;
  }
  
  // Check if it's rainbow or zebra
  if (cleanStr === "rainbow" || cleanStr === "zebra") return true;
  
  // Check if it's a fade expression
  if (cleanStr.startsWith("fade:")) return true;
  
  return false;
}

/**
 * Extract color information from fade string
 * @param {string} fadeString - Fade string like "fade:red-blue" or "fade:red-blue:vertical"
 * @returns {Object|null} Fade information or null if invalid
 */
export function parseFadeString(fadeString) {
  if (!fadeString || !fadeString.startsWith("fade:")) return null;
  
  const parts = fadeString.split(":");
  if (parts.length < 2) return null;
  
  // Extract colors part (skip "fade" and optional direction)
  const colorPart = parts[1]; // The part after "fade:"
  const direction = parts[2]; // Optional direction part
  const colorNames = colorPart.split("-");
  
  if (colorNames.length < 2) return null;
  
  const validColors = [];
  
  // Validate each color in the fade
  for (const colorName of colorNames) {
    if (isValidColorString(colorName)) {
      // Get the actual RGB values
      if (cssColors && cssColors[colorName]) {
        validColors.push(cssColors[colorName]);
      } else if (colorName.match(/^c\d+$/)) {
        const index = parseInt(colorName.substring(1));
        if (staticColorMap && staticColorMap[index]) {
          validColors.push(staticColorMap[index]);
        }
      } else if (colorName === "rainbow") {
        validColors.push([255, 0, 0]); // Use red as representative
      } else if (colorName === "zebra") {
        validColors.push([0, 0, 0]); // Use black as representative
      }
    } else {
      return null; // Invalid color
    }
  }
  
  return validColors.length >= 2 ? {
    colors: validColors,
    colorNames,
    direction: direction || "horizontal",
    originalString: fadeString
  } : null;
}
