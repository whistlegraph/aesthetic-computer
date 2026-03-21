// Monaco KidLisp Syntax Highlighting
// Shared module for applying KidLisp syntax highlighting to Monaco editors
// Used by both index.html and device.html

import { tokenize, KidLisp } from 'https://aesthetic.computer/aesthetic.computer/lib/kidlisp.mjs';

/**
 * Create a MonacoKidLispHighlighter instance
 * @param {monaco.editor.IStandaloneCodeEditor} editor - Monaco editor instance
 * @param {object} options - Configuration options
 * @param {boolean} options.enableTimingBlinks - Enable animated timing blinks (default: true)
 * @param {boolean} options.lightModeHighContrast - Use high-contrast colors for light mode
 * @param {KidLisp} options.kidlispInstance - Use an existing KidLisp instance instead of creating a new one
 */
export class MonacoKidLispHighlighter {
  constructor(editor, options = {}) {
    this.editor = editor;
    this.kidlisp = options.kidlispInstance || new KidLisp();
    this.currentDecorations = [];
    this.isUpdating = false;
    this.updateLoopRunning = false;

    // Options
    this.enableTimingBlinks = options.enableTimingBlinks !== false;
    this.lightModeHighContrast = options.lightModeHighContrast ?? false;

    // Detect light mode if not explicitly set
    if (options.lightModeHighContrast === undefined) {
      this.lightModeHighContrast = !window.matchMedia('(prefers-color-scheme: dark)').matches;

      // Listen for theme changes
      window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
        this.lightModeHighContrast = !e.matches;
      });
    }

    // CSS color map
    this.cssColorMap = {
      aliceblue: [240, 248, 255], antiquewhite: [250, 235, 215], aqua: [0, 255, 255],
      aquamarine: [127, 255, 212], azure: [240, 255, 255], beige: [245, 245, 220],
      bisque: [255, 228, 196], black: [0, 0, 0], blanchedalmond: [255, 235, 205],
      blue: [0, 0, 255], blueviolet: [138, 43, 226], brown: [165, 42, 42],
      burlywood: [222, 184, 135], cadetblue: [95, 158, 160], chartreuse: [127, 255, 0],
      chocolate: [210, 105, 30], coral: [255, 127, 80], cornflowerblue: [100, 149, 237],
      cornsilk: [255, 248, 220], crimson: [220, 20, 60], cyan: [0, 255, 255],
      darkblue: [0, 0, 139], darkcyan: [0, 139, 139], darkgoldenrod: [184, 134, 11],
      darkgray: [169, 169, 169], darkgrey: [169, 169, 169], darkgreen: [0, 100, 0],
      darkkhaki: [189, 183, 107], darkmagenta: [139, 0, 139], darkolivegreen: [85, 107, 47],
      darkorange: [255, 140, 0], darkorchid: [153, 50, 204], darkred: [139, 0, 0],
      darksalmon: [233, 150, 122], darkseagreen: [143, 188, 143], darkslateblue: [72, 61, 139],
      darkslategray: [47, 79, 79], darkslategrey: [47, 79, 79], darkturquoise: [0, 206, 209],
      darkviolet: [148, 0, 211], deeppink: [255, 20, 147], deepskyblue: [0, 191, 255],
      dimgray: [105, 105, 105], dimgrey: [105, 105, 105], dodgerblue: [30, 144, 255],
      firebrick: [178, 34, 34], floralwhite: [255, 250, 240], forestgreen: [34, 139, 34],
      fuchsia: [255, 0, 255], gainsboro: [220, 220, 220], ghostwhite: [248, 248, 255],
      gold: [255, 215, 0], goldenrod: [218, 165, 32], gray: [128, 128, 128],
      grey: [128, 128, 128], green: [0, 128, 0], greenyellow: [173, 255, 47],
      honeydew: [240, 255, 240], hotpink: [255, 105, 180], indianred: [205, 92, 92],
      indigo: [75, 0, 130], ivory: [255, 255, 240], khaki: [240, 230, 140],
      lavender: [230, 230, 250], lavenderblush: [255, 240, 245], lawngreen: [124, 252, 0],
      lemonchiffon: [255, 250, 205], lightblue: [173, 216, 230], lightcoral: [240, 128, 128],
      lightcyan: [224, 255, 255], lightgoldenrodyellow: [250, 250, 210], lightgray: [211, 211, 211],
      lightgrey: [211, 211, 211], lightgreen: [144, 238, 144], lightpink: [255, 182, 193],
      lightsalmon: [255, 160, 122], lightseagreen: [32, 178, 170], lightskyblue: [135, 206, 250],
      lightslategray: [119, 136, 153], lightslategrey: [119, 136, 153], lightsteelblue: [176, 196, 222],
      lightyellow: [255, 255, 224], lime: [0, 255, 0], limegreen: [50, 205, 50],
      linen: [250, 240, 230], magenta: [255, 0, 255], maroon: [128, 0, 0],
      mediumaquamarine: [102, 205, 170], mediumblue: [0, 0, 205], mediumorchid: [186, 85, 211],
      mediumpurple: [147, 112, 219], mediumseagreen: [60, 179, 113], mediumslateblue: [123, 104, 238],
      mediumspringgreen: [0, 250, 154], mediumturquoise: [72, 209, 204], mediumvioletred: [199, 21, 133],
      midnightblue: [25, 25, 112], mintcream: [245, 255, 250], mistyrose: [255, 228, 225],
      moccasin: [255, 228, 181], navajowhite: [255, 222, 173], navy: [0, 0, 128],
      oldlace: [253, 245, 230], olive: [128, 128, 0], olivedrab: [107, 142, 35],
      orange: [255, 165, 0], orangered: [255, 69, 0], orchid: [218, 112, 214],
      palegoldenrod: [238, 232, 170], palegreen: [152, 251, 152], paleturquoise: [175, 238, 238],
      palevioletred: [219, 112, 147], papayawhip: [255, 239, 213], peachpuff: [255, 218, 185],
      peru: [205, 133, 63], pink: [255, 192, 203], plum: [221, 160, 221],
      powderblue: [176, 224, 230], purple: [128, 0, 128], rebeccapurple: [102, 51, 153],
      red: [255, 0, 0], rosybrown: [188, 143, 143], royalblue: [65, 105, 225],
      saddlebrown: [139, 69, 19], salmon: [250, 128, 114], sandybrown: [244, 164, 96],
      seagreen: [46, 139, 87], seashell: [255, 245, 238], sienna: [160, 82, 45],
      silver: [192, 192, 192], skyblue: [135, 206, 235], slateblue: [106, 90, 205],
      slategray: [112, 128, 144], slategrey: [112, 128, 144], snow: [255, 250, 250],
      springgreen: [0, 255, 127], steelblue: [70, 130, 180], tan: [210, 180, 140],
      teal: [0, 128, 128], thistle: [216, 191, 216], tomato: [255, 99, 71],
      turquoise: [64, 224, 208], violet: [238, 130, 238], wheat: [245, 222, 179],
      white: [255, 255, 255], whitesmoke: [245, 245, 245], yellow: [255, 255, 0],
      yellowgreen: [154, 205, 50]
    };
  }

  /**
   * Apply syntax highlighting decorations to the editor
   * @param {boolean} forceUpdate - Force update even if already updating
   */
  applyDecorations(forceUpdate = false) {
    if (!forceUpdate && this.isUpdating) return;
    if (!this.editor) return;

    const model = this.editor.getModel();
    if (!model) return;

    this.isUpdating = true;
    const decorations = [];
    const code = model.getValue();

    if (!code.trim()) {
      this.currentDecorations = this.editor.deltaDecorations(this.currentDecorations, []);
      this.isUpdating = false;
      return;
    }

    try {
      // Use KidLisp's tokenizer
      const tokens = tokenize(code);
      this.kidlisp.initializeSyntaxHighlighting(code);

      // Track position in code
      let searchPos = 0;

      tokens.forEach((token, index) => {
        // Get the color from KidLisp's getTokenColor method
        let colorName = this.kidlisp.getTokenColor(token, tokens, index);

        // Find the token's position in the code starting from searchPos
        const tokenPos = code.indexOf(token, searchPos);
        if (tokenPos === -1) return;

        // Calculate line and column from position
        const beforeToken = code.substring(0, tokenPos);
        const lines = beforeToken.split('\n');
        const lineNumber = lines.length;
        const columnNumber = lines[lines.length - 1].length + 1;
        const endColumn = columnNumber + token.length;

        // Validate range
        if (lineNumber < 1 || columnNumber < 1 || endColumn < columnNumber) {
          searchPos = tokenPos + token.length;
          return;
        }

        // Handle special coloring cases
        this._handleTokenDecoration(token, colorName, decorations, lineNumber, columnNumber, endColumn);

        searchPos = tokenPos + token.length;
      });

    } catch (error) {
      console.warn('KidLisp highlighting error:', error.message);
    }

    // Apply decorations
    try {
      if (this.editor && this.editor.getModel()) {
        this.currentDecorations = this.editor.deltaDecorations(this.currentDecorations, decorations);
      }
    } catch (error) {
      console.warn('Failed to apply decorations:', error.message);
      this.currentDecorations = [];
    }

    this.isUpdating = false;
  }

  /**
   * Handle token decoration based on color type
   * @private
   */
  _handleTokenDecoration(token, colorName, decorations, lineNumber, columnNumber, endColumn) {
    // Handle RAINBOW special marker
    if (colorName === 'RAINBOW') {
      const rainbowColors = ['#ff0000', '#ff7f00', '#ffff00', '#00ff00', '#0000ff', '#4b0082', '#9400d3'];
      for (let i = 0; i < token.length; i++) {
        const charColor = rainbowColors[i % rainbowColors.length];
        const cssClass = this._getOrCreateCssClass(charColor);
        decorations.push({
          range: new window.monaco.Range(lineNumber, columnNumber + i, lineNumber, columnNumber + i + 1),
          options: { inlineClassName: cssClass }
        });
      }
      return;
    }

    // Handle ZEBRA special marker
    if (colorName === 'ZEBRA') {
      const zebraColors = ['#000000', '#ffffff'];
      for (let i = 0; i < token.length; i++) {
        const charColor = zebraColors[i % zebraColors.length];
        const cssClass = this._getOrCreateCssClass(charColor);
        decorations.push({
          range: new window.monaco.Range(lineNumber, columnNumber + i, lineNumber, columnNumber + i + 1),
          options: { inlineClassName: cssClass }
        });
      }
      return;
    }

    // Handle fade: expressions with multi-color highlighting
    if (token.startsWith('fade:')) {
      const coloredFadeString = this.kidlisp.colorFadeExpression(token);
      const segments = coloredFadeString.split('\\').filter(s => s);

      let charOffset = 0;
      for (let i = 0; i < segments.length; i += 2) {
        const segmentColor = segments[i];
        const segmentText = segments[i + 1] || '';

        const cssClass = this._getOrCreateCssClass(segmentColor);

        // Apply color to each character in the segment
        for (let charIdx = 0; charIdx < segmentText.length; charIdx++) {
          decorations.push({
            range: new window.monaco.Range(lineNumber, columnNumber + charOffset, lineNumber, columnNumber + charOffset + 1),
            options: { inlineClassName: cssClass }
          });
          charOffset++;
        }
      }
      return;
    }

    // Handle COMPOUND: colors (for $codes, #codes, !codes)
    if (colorName && colorName.startsWith('COMPOUND:')) {
      const parts = colorName.split(':');
      const prefixColor = parts[1];
      const identifierColor = parts[2];

      // Apply prefix color to first character
      const prefixClass = this._getOrCreateCssClass(prefixColor);
      decorations.push({
        range: new window.monaco.Range(lineNumber, columnNumber, lineNumber, columnNumber + 1),
        options: { inlineClassName: prefixClass }
      });

      // Apply identifier color to rest
      if (token.length > 1) {
        const idClass = this._getOrCreateCssClass(identifierColor);
        decorations.push({
          range: new window.monaco.Range(lineNumber, columnNumber + 1, lineNumber, endColumn),
          options: { inlineClassName: idClass }
        });
      }
      return;
    }

    // Regular color
    if (colorName) {
      const cssClass = this._getOrCreateCssClass(colorName);
      decorations.push({
        range: new window.monaco.Range(lineNumber, columnNumber, lineNumber, endColumn),
        options: { inlineClassName: cssClass }
      });
    }
  }

  /**
   * Get or create CSS class for a color
   * @private
   */
  _getOrCreateCssClass(colorName) {
    // Apply high-contrast colors for light mode
    let cssColor = colorName;
    if (this.lightModeHighContrast) {
      const lightModeColorMap = {
        'yellow': '#cc9900',
        'cyan': '#0099cc',
        'lime': '#00aa00',
        'pink': '#cc0066',
        'orange': '#cc6600',
        'magenta': '#cc00cc',
        'mediumseagreen': '#2e8b57',
        'gray': '#666666',
        'lightgray': '#999999',
        'white': '#cccccc',
      };
      cssColor = lightModeColorMap[colorName] || colorName;
    }

    let cssClass;
    if (cssColor.includes(',')) {
      // RGB format
      cssClass = `kidlisp-color-rgb-${cssColor.replace(/,/g, '-').replace(/ /g, '')}`;
      cssColor = `rgb(${cssColor})`;
    } else {
      // Named color
      cssClass = `kidlisp-color-${cssColor.replace(/ /g, '-').replace(/#/g, 'hex')}`;
    }

    // Create style if it doesn't exist
    if (!document.getElementById(cssClass)) {
      const style = document.createElement('style');
      style.id = cssClass;

      // Add text-shadow for dark colors in dark mode
      const shadow = this._getShadowForColor(cssColor);
      const shadowStyle = shadow ? ` text-shadow: ${shadow};` : '';

      style.textContent = `.monaco-editor .view-line > span > span.${cssClass} { color: ${cssColor}; font-weight: bold;${shadowStyle} }`;
      document.head.appendChild(style);
    }

    return cssClass;
  }

  /**
   * Get text shadow for dark colors
   * @private
   */
  _getShadowForColor(colorStr) {
    const isDarkMode = document.documentElement.getAttribute('data-theme') === 'dark' ||
                     (!document.documentElement.getAttribute('data-theme') &&
                      window.matchMedia('(prefers-color-scheme: dark)').matches);

    if (!isDarkMode) return null;

    // Parse RGB
    let rgb;
    if (colorStr.includes(',')) {
      const parts = colorStr.replace(/rgb\(|\)/g, '').split(',').map(v => parseInt(v.trim()));
      rgb = parts;
    } else if (colorStr.startsWith('rgb')) {
      return null;
    } else {
      const darkColors = {
        'black': [0, 0, 0],
        'darkblue': [0, 0, 139],
        'darkgreen': [0, 100, 0],
        'darkred': [139, 0, 0],
        'navy': [0, 0, 128],
        'maroon': [128, 0, 0],
        'purple': [128, 0, 128],
        'indigo': [75, 0, 130]
      };
      rgb = darkColors[colorStr.toLowerCase()];
    }

    if (!rgb || rgb.length < 3) return null;

    const [r, g, b] = rgb;
    const luminance = (0.299 * r + 0.587 * g + 0.114 * b);

    if (luminance < 50) {
      return '1px 1px 0px rgba(220, 220, 220, 0.8)';
    } else if (luminance < 100) {
      return '1px 1px 0px rgba(200, 200, 200, 0.6)';
    }

    return null;
  }

  /**
   * Start continuous update loop for timing blinks
   * Requires setting isEditMode on the KidLisp instance
   * @param {function} shouldAnimate - Function that returns true when timing should blink
   */
  startTimingBlinksLoop(shouldAnimate = () => true) {
    if (this.updateLoopRunning) return;
    this.updateLoopRunning = true;

    let lastAnimating = false;
    let lastCode = '';

    const scheduleUpdate = () => {
      if (!this.updateLoopRunning) return;
      if (!this.isUpdating && this.editor && this.editor.getModel()) {
        const animating = shouldAnimate();
        if (animating) {
          // Playing — update at 60fps for timing blinks
          requestAnimationFrame(() => {
            this.kidlisp.isEditMode = true;
            this.applyDecorations(true);
            if (window.perfCounters) window.perfCounters.decorations++;
            lastAnimating = true;
            setTimeout(scheduleUpdate, 16);
          });
        } else {
          // Not playing — only redecorate when code changes or transitioning from playing
          const currentCode = this.editor.getModel().getValue();
          if (lastAnimating || currentCode !== lastCode) {
            this.kidlisp.isEditMode = false;
            this.applyDecorations(true);
            if (window.perfCounters) window.perfCounters.decorations++;
            lastCode = currentCode;
            lastAnimating = false;
          }
          // Poll at low rate (~4fps) to catch code edits
          setTimeout(scheduleUpdate, 250);
        }
      } else {
        setTimeout(scheduleUpdate, 250);
      }
    };

    scheduleUpdate();
  }

  /**
   * Stop the timing blinks update loop
   */
  stopTimingBlinksLoop() {
    this.updateLoopRunning = false;
  }

  /**
   * Clean up and remove all decorations
   */
  destroy() {
    this.stopTimingBlinksLoop();
    if (this.editor && this.editor.getModel()) {
      this.currentDecorations = this.editor.deltaDecorations(this.currentDecorations, []);
    }
    this.editor = null;
    this.kidlisp = null;
  }
}

/**
 * Helper function to quickly set up highlighting on an editor
 * @param {monaco.editor.IStandaloneCodeEditor} editor
 * @param {object} options
 * @returns {MonacoKidLispHighlighter}
 */
export function setupKidLispHighlighting(editor, options = {}) {
  const highlighter = new MonacoKidLispHighlighter(editor, options);
  highlighter.applyDecorations();

  // Auto-update on content changes
  editor.onDidChangeModelContent(() => {
    highlighter.applyDecorations();
  });

  return highlighter;
}
