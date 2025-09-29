// 🎨 Color Highlighting System
// Shared color highlighting utilities for both kidlisp and nopaint systems
// Extracted from kidlisp.mjs to ensure uniform highlighting across the platform

import { cssColors, staticColorMap, hexToRgb, clamp } from "./num.mjs";

const CHANNEL_COLORS = ["red", "lime", "deepskyblue"];
const GRAYSCALE_COLOR = "silver";
const NUMERIC_TOKEN_REGEX = /^-?\d+(?:\.\d+)?$/;
const RANGE_TOKEN_REGEX = /^-?\d+\s*-\s*-?\d+$/;

function isWildcardToken(token) {
  return token === "?";
}

function isNumericLikeToken(token) {
  if (!token && token !== 0) return false;
  const trimmed = `${token}`.trim();
  if (!trimmed) return false;
  return (
    NUMERIC_TOKEN_REGEX.test(trimmed) ||
    RANGE_TOKEN_REGEX.test(trimmed) ||
    isWildcardToken(trimmed)
  );
}

function isHexColorToken(token) {
  if (!token && token !== 0) return false;
  const trimmed = `${token}`.trim();
  if (!trimmed) return false;

  if (trimmed.startsWith("#")) {
    const body = trimmed.slice(1);
    return /^[0-9a-fA-F]{3,8}$/.test(body);
  }

  if (/^0x[0-9a-fA-F]{3,8}$/i.test(trimmed)) {
    return true;
  }

  if (/^[0-9a-fA-F]{3,8}$/.test(trimmed) && /[a-fA-F]/.test(trimmed)) {
    return true;
  }

  return false;
}

function expandShortHex(body) {
  return body
    .split("")
    .map((char) => char + char)
    .join("");
}

function clampChannelValue(value) {
  if (value === undefined || value === null || Number.isNaN(value)) return null;
  return clamp(Math.round(value), 0, 255);
}

function channelColorString(index, value, { grayscale = false } = {}) {
  const clamped = clampChannelValue(value);
  if (clamped === null) {
    return CHANNEL_COLORS[index] || "orange";
  }

  if (grayscale) {
    return `${clamped},${clamped},${clamped}`;
  }

  if (index === 0) return `${clamped},0,0`;
  if (index === 1) return `0,${clamped},0`;
  if (index === 2) return `0,0,${clamped}`;
  return `${clamped},${clamped},${clamped}`;
}

function parseHexChannelValue(channelHex) {
  if (!channelHex) return 0;
  const normalized = channelHex.length === 1 ? channelHex.repeat(2) : channelHex;
  return parseInt(normalized, 16);
}

function parseAlphaFromToken(token) {
  if (token === undefined || token === null) return null;
  const trimmed = `${token}`.trim();
  if (!trimmed || trimmed === "?" || trimmed.includes("-")) return null;

  const value = Number(trimmed);
  if (Number.isNaN(value)) return null;

  if (value <= 1 && trimmed.includes(".")) {
    return clamp(Math.round(value * 255), 0, 255);
  }

  return clamp(Math.round(value), 0, 255);
}

function parseHexColorToken(token) {
  if (!token && token !== 0) return null;
  const original = `${token}`.trim();
  if (!original) return null;

  let body = original;
  let prefix = "";
  if (body.startsWith("#")) {
    body = body.slice(1);
    prefix = "#";
  } else if (body.startsWith("0x") || body.startsWith("0X")) {
    prefix = body.slice(0, 2);
    body = body.slice(2);
  }

  if (!/^[0-9a-fA-F]+$/.test(body)) return null;
  if (![3, 4, 6, 8].includes(body.length)) return null;

  const uppercaseBody = body.toUpperCase();
  const bodyLength = uppercaseBody.length;

  let rgbHex = uppercaseBody;
  let alphaHex = null;

  if (bodyLength === 3) {
    rgbHex = expandShortHex(uppercaseBody);
  } else if (bodyLength === 4) {
    rgbHex = expandShortHex(uppercaseBody.slice(0, 3));
    alphaHex = `${uppercaseBody[3]}${uppercaseBody[3]}`;
  } else if (bodyLength === 6) {
    rgbHex = uppercaseBody.slice(0, 6);
  } else if (bodyLength === 8) {
    rgbHex = uppercaseBody.slice(0, 6);
    alphaHex = uppercaseBody.slice(6, 8);
  }

  rgbHex = rgbHex.toUpperCase();

  let rgb;
  try {
    rgb = hexToRgb(`#${rgbHex}`);
  } catch (error) {
    return null;
  }

  const alpha = alphaHex !== null ? parseInt(alphaHex, 16) : null;
  const channelDisplays = [];
  let alphaDisplay = null;

  if (bodyLength === 3 || bodyLength === 4) {
    channelDisplays.push(...uppercaseBody.slice(0, 3).split(""));
    if (bodyLength === 4) {
      alphaDisplay = uppercaseBody[3];
    }
  } else {
    const segments = uppercaseBody.slice(0, 6).match(/.{1,2}/g) || [];
    channelDisplays.push(...segments);
    if (bodyLength === 8) {
      alphaDisplay = uppercaseBody.slice(6, 8);
    }
  }

  return {
    rgb,
    alpha,
    alphaHex,
    prefix,
    channelDisplays,
    alphaDisplay
  };
}

function formatAlphaToken(rawToken, parsedAlpha) {
  if (parsedAlpha === undefined || parsedAlpha === null || Number.isNaN(parsedAlpha)) {
    const tokenText = rawToken !== undefined ? rawToken : "?";
    return `\\magenta\\${tokenText}`;
  }

  const alphaInt = clampChannelValue(parsedAlpha) ?? 0;
  return `\\magenta\\${alphaInt}`;
}

function colorizeHexToken(token) {
  const parsed = parseHexColorToken(token);
  if (!parsed || !parsed.rgb) {
    return { text: `\\orange\\${token}`, consumed: 1, brushColor: null, alphaIncluded: false };
  }

  const [r, g, b] = parsed.rgb;
  const colorValue = `${r},${g},${b}`;
  let block = `\\${colorValue}\\${parsed.prefix || "#"}`;

  parsed.channelDisplays.forEach((channelHex, index) => {
    const channelValue = parseHexChannelValue(channelHex);
    const channelColor = channelColorString(index, channelValue);
    block += `\\${channelColor}\\${channelHex}`;
  });

  let alphaIncluded = false;
  if (parsed.alpha !== null && !Number.isNaN(parsed.alpha)) {
    const alphaSegment = formatAlphaToken(parsed.alphaDisplay, parsed.alpha);
    if (alphaSegment) {
      block += ` ${alphaSegment}`;
      alphaIncluded = true;
    }
  }

  return { text: block, consumed: 1, brushColor: colorValue, alphaIncluded };
}

function colorizeNumericChannels(rawTokens, parsedArray) {
  if (!Array.isArray(parsedArray) || parsedArray.length === 0 || rawTokens.length === 0) {
    return { text: "", consumed: 0, brushColor: null, alphaIncluded: false };
  }

  const available = Math.min(rawTokens.length, parsedArray.length);
  const channelCount = Math.min(3, available);
  const segments = [];
  let brushColor = null;
  let alphaIncluded = false;

  if (channelCount === 1) {
    const value = clampChannelValue(parsedArray[0]);
    const colorString = value === null ? GRAYSCALE_COLOR : channelColorString(0, value, { grayscale: true });
    segments.push(`\\${colorString}\\${rawTokens[0]}`);
    brushColor = value === null ? null : `${value},${value},${value}`;

    if (available >= 2) {
      const alphaValue = parsedArray[1];
      segments.push(formatAlphaToken(rawTokens[1], alphaValue));
      alphaIncluded = true;
      return {
        text: segments.join(" "),
        consumed: 2,
        brushColor,
        alphaIncluded
      };
    }

    return {
      text: segments.join(" "),
      consumed: 1,
      brushColor,
      alphaIncluded
    };
  }

  const channelValues = [];
  for (let i = 0; i < channelCount; i++) {
    const value = clampChannelValue(parsedArray[i]);
    channelValues.push(value === null ? 0 : value);
    const channelString = channelColorString(i, value);
    segments.push(`\\${channelString}\\${rawTokens[i]}`);
  }

  let consumed = channelCount;

  if (channelValues.length === 3) {
    brushColor = `${channelValues[0]},${channelValues[1]},${channelValues[2]}`;
  } else if (channelValues.length === 2) {
    brushColor = `${channelValues[0]},${channelValues[1]},0`;
  }

  if (available > channelCount) {
    const alphaValue = parsedArray[channelCount];
    const alphaToken = rawTokens[channelCount];
    segments.push(formatAlphaToken(alphaToken, alphaValue));
    consumed += 1;
    alphaIncluded = true;
  }

  return {
    text: segments.join(" "),
    consumed,
    brushColor,
    alphaIncluded
  };
}

function isFadeColorObject(colorParams) {
  return (
    colorParams &&
    typeof colorParams === "object" &&
    !Array.isArray(colorParams) &&
    colorParams.type === "fade"
  );
}

function highlightColorTokens(colorParams, rawParams = []) {
  if (!rawParams || rawParams.length === 0) {
    return { text: "", brushColor: null };
  }

  const remaining = [...rawParams];
  const segments = [];
  let brushColor = null;
  let alphaHandled = false;

  const pushSegment = (text, { spaceBefore = true } = {}) => {
    if (!text) return;
    segments.push({ text, spaceBefore });
  };

  if (isFadeColorObject(colorParams)) {
    const first = remaining.shift();
    if (first) {
      pushSegment(colorizeColorName(first), { spaceBefore: false });
    }
    if (typeof colorParams.alpha === "number" && remaining.length > 0) {
      pushSegment(formatAlphaToken(remaining.shift(), colorParams.alpha));
      alphaHandled = true;
    }
  } else if (isHexColorToken(remaining[0])) {
    const { text, consumed, brushColor: hexColor, alphaIncluded } = colorizeHexToken(remaining[0]);
    pushSegment(text, { spaceBefore: false });
    remaining.splice(0, consumed);
    brushColor = hexColor;
    alphaHandled = alphaIncluded;
  } else if (Array.isArray(colorParams) && colorParams.length > 0 && isNumericLikeToken(remaining[0])) {
    const { text, consumed, brushColor: numericColor, alphaIncluded } = colorizeNumericChannels(remaining, colorParams);
    pushSegment(text, { spaceBefore: false });
    remaining.splice(0, consumed);
    brushColor = numericColor;
    alphaHandled = alphaIncluded;
  } else {
    const first = remaining.shift();
    if (first) {
      pushSegment(colorizeColorName(first), { spaceBefore: false });
    }
  }

  if (!alphaHandled && remaining.length > 0 && isNumericLikeToken(remaining[0])) {
    const alphaToken = remaining.shift();
    const alphaValue = parseAlphaFromToken(alphaToken);
    pushSegment(formatAlphaToken(alphaToken, alphaValue));
    alphaHandled = true;
  }

  remaining.forEach((token) => {
    pushSegment(`\\orange\\${token}`);
  });

  const text = segments.reduce((acc, seg, index) => {
    if (index === 0 || seg.spaceBefore === false) {
      return acc + seg.text;
    }
    return `${acc} ${seg.text}`;
  }, "");

  return { text, brushColor };
}

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

  if (isHexColorToken(cleanName)) {
    const parsedHex = parseHexColorToken(cleanName);
    if (parsedHex && parsedHex.rgb) {
      const [r, g, b] = parsedHex.rgb;
      const colorValue = `${r},${g},${b}`;
      return `\\${colorValue}\\${parsedHex.display}`;
    }
  }

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
  const { text: colorSection, brushColor } = highlightColorTokens(colorParams, rawParams);
  const brushColorCode = brushColor || "white";

  let label = `\\${brushColorCode}\\${brushName}`;

  if (modifiers) {
    label += ` \\gray\\${modifiers}`;
  }

  if (colorSection) {
    label += ` ${colorSection}`;
  } else if (rawParams && rawParams.length > 0) {
    const fallback = rawParams.map((token) => `\\orange\\${token}`).join(" ");
    label += ` ${fallback}`;
  }

  return label;
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
