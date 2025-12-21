// 🔗 Hotlink - Interactive Text Links
// A high-level API for adding clickable links to any AC piece.
// Detects @handles, URLs, 'prompts', #paintings, $kidlisp, and r8dio.

import {
  parseMessageElements,
  applyColorCodes,
  calculateElementPosition,
  isClickInsideElement,
  getElementAction,
  defaultColorTheme,
  isInteractiveType,
} from "./chat-highlighting.mjs";

// Re-export utilities for direct use
export {
  parseMessageElements,
  applyColorCodes,
  defaultColorTheme,
  isInteractiveType,
};

/**
 * HotlinkText - Wrapper for text with interactive hotlinks
 * 
 * Usage:
 *   const hotlink = new HotlinkText("Check @jeffrey's art at https://aesthetic.computer");
 *   
 *   // In paint():
 *   ink().write(hotlink.getColoredText("white"), { x: 10, y: 10 });
 *   
 *   // In act():
 *   if (e.is("move")) hotlink.updateHover(e.x - textX, e.y - textY, text, rowHeight, font);
 *   if (e.is("lift")) {
 *     const action = hotlink.getClickedAction(e.x - textX, e.y - textY, text, rowHeight, font);
 *     if (action) jump(action.jumpTarget);
 *   }
 */
export class HotlinkText {
  constructor(text, options = {}) {
    this.text = text;
    this.elements = parseMessageElements(text);
    this.theme = { ...defaultColorTheme, ...options.theme };
    this.hoveredElements = new Set();
    this.textLines = null; // Set externally after text.box() wrapping
    this.rowHeight = options.rowHeight || 12;
    this.typefaceName = options.typefaceName || undefined;
  }

  /**
   * Set the wrapped text lines (from text.box().lines)
   * Must be called before hover/click detection works
   */
  setTextLines(lines) {
    this.textLines = lines;
  }

  /**
   * Check if this text has any interactive elements
   */
  hasInteractiveElements() {
    return this.elements.some(el => isInteractiveType(el.type));
  }

  /**
   * Get color-coded text string for rendering with ink().write()
   * @param {string|array} defaultColor - Color for non-highlighted text
   * @param {boolean} applyHoverColors - Whether to apply hover highlight colors
   */
  getColoredText(defaultColor = "white", applyHoverColors = true) {
    if (!this.elements || this.elements.length === 0) {
      return this.text;
    }

    // Build color map with hover states
    const colorMap = {};
    for (const element of this.elements) {
      const isHovered = applyHoverColors && this._isElementHovered(element);
      const baseType = element.type.replace("-", "");
      
      if (element.type === "kidlisp-token" && element.color) {
        // KidLisp tokens have their own syntax colors
        colorMap[`kidlisp-token-${element.start}`] = isHovered ? this.theme.kidlispHover : element.color;
      } else {
        const hoverKey = baseType + "Hover";
        colorMap[baseType] = isHovered && this.theme[hoverKey] ? this.theme[hoverKey] : this.theme[baseType];
      }
    }

    return applyColorCodes(this.text, this.elements, colorMap, defaultColor);
  }

  /**
   * Update hover state based on cursor position
   * @returns {boolean} true if hover state changed (needs repaint)
   */
  updateHover(relativeX, relativeY, textApi) {
    if (!this.textLines || !textApi) return false;

    const previousSize = this.hoveredElements.size;
    const previousElements = new Set(this.hoveredElements);
    this.hoveredElements.clear();

    for (const element of this.elements) {
      if (!isInteractiveType(element.type)) continue;

      const positions = calculateElementPosition(
        element,
        this.text,
        this.textLines,
        textApi,
        this.rowHeight,
        this.typefaceName
      );

      if (positions && isClickInsideElement(relativeX, relativeY, positions, textApi, this.typefaceName)) {
        this.hoveredElements.add(element);
        break; // Only hover one element at a time
      }
    }

    // Check if hover state changed
    if (previousSize !== this.hoveredElements.size) return true;
    for (const el of this.hoveredElements) {
      const found = [...previousElements].some(prev => 
        prev.start === el.start && prev.end === el.end && prev.type === el.type
      );
      if (!found) return true;
    }
    return false;
  }

  /**
   * Check if cursor is over any interactive element
   */
  isHovering() {
    return this.hoveredElements.size > 0;
  }

  /**
   * Get the action for a click at the given position
   * @returns {object|null} { type, text, displayText, description, jumpTarget }
   */
  getClickedAction(relativeX, relativeY, textApi) {
    if (!this.textLines || !textApi) return null;

    for (const element of this.elements) {
      if (!isInteractiveType(element.type)) continue;

      const positions = calculateElementPosition(
        element,
        this.text,
        this.textLines,
        textApi,
        this.rowHeight,
        this.typefaceName
      );

      if (positions && isClickInsideElement(relativeX, relativeY, positions, textApi, this.typefaceName)) {
        return getElementAction(element, this.text, this.elements);
      }
    }

    return null;
  }

  /**
   * Get action for currently hovered element (useful after updateHover)
   */
  getHoveredAction() {
    if (this.hoveredElements.size === 0) return null;
    const element = [...this.hoveredElements][0];
    return getElementAction(element, this.text, this.elements);
  }

  /**
   * Clear all hover states
   */
  clearHover() {
    this.hoveredElements.clear();
  }

  // Private: check if a specific element is hovered
  _isElementHovered(element) {
    return [...this.hoveredElements].some(hovered =>
      hovered.start === element.start &&
      hovered.end === element.end &&
      hovered.type === element.type
    );
  }
}

/**
 * Quick helper to just colorize text without interactivity
 * @param {string} text - The text to colorize
 * @param {string|array} defaultColor - Color for non-highlighted text
 * @param {object} theme - Optional custom theme
 */
export function colorizeText(text, defaultColor = "white", theme = defaultColorTheme) {
  const elements = parseMessageElements(text);
  return applyColorCodes(text, elements, theme, defaultColor);
}

/**
 * Check if text contains any hotlinkable content
 */
export function hasHotlinks(text) {
  const elements = parseMessageElements(text);
  return elements.some(el => isInteractiveType(el.type));
}
