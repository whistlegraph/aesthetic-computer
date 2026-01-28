// 🌈 KidLisp Syntax Highlighting
// Shared syntax highlighting module for KidLisp code
// Used by: VS Code extension, Monaco editor (kidlisp.com), and other editors
// 
// This module provides consistent syntax highlighting across all KidLisp editors
// including support for:
//   - Rainbow and zebra color words
//   - Animated timing token blinking (1s, 2s..., 3f, etc.)
//   - Color codes (c0, c1, etc.)
//   - Pattern codes (p0=rainbow, p1=zebra)
//   - $code embedded layer references
//   - #code painting references
//   - !code tape references
//   - CSS color names with actual color values
//   - fade: expressions with gradient coloring
//   - RGB channel highlighting (255 0 0 -> red, green, blue)
//   - Rainbow nested parentheses
//   - Function/keyword categorization

// CSS Colors from num.mjs (subset for syntax highlighting)
export const cssColors = {
  aliceblue: [240, 248, 255],
  antiquewhite: [250, 235, 215],
  aqua: [0, 255, 255],
  aquamarine: [127, 255, 212],
  azure: [240, 255, 255],
  beige: [245, 245, 220],
  bisque: [255, 228, 196],
  black: [0, 0, 0],
  blanchedalmond: [255, 235, 205],
  blue: [0, 0, 255],
  blueviolet: [138, 43, 226],
  brown: [165, 42, 42],
  burlywood: [222, 184, 135],
  cadetblue: [95, 158, 160],
  chartreuse: [127, 255, 0],
  chocolate: [210, 105, 30],
  coral: [255, 127, 80],
  cornflowerblue: [100, 149, 237],
  cornsilk: [255, 248, 220],
  crimson: [220, 20, 60],
  cyan: [0, 255, 255],
  darkblue: [0, 0, 139],
  darkcyan: [0, 139, 139],
  darkgoldenrod: [184, 134, 11],
  darkgray: [169, 169, 169],
  darkgreen: [0, 100, 0],
  darkgrey: [169, 169, 169],
  darkkhaki: [189, 183, 107],
  darkmagenta: [139, 0, 139],
  darkolivegreen: [85, 107, 47],
  darkorange: [255, 140, 0],
  darkorchid: [153, 50, 204],
  darkred: [139, 0, 0],
  darksalmon: [233, 150, 122],
  darkseagreen: [143, 188, 143],
  darkslateblue: [72, 61, 139],
  darkslategray: [47, 79, 79],
  darkslategrey: [47, 79, 79],
  darkturquoise: [0, 206, 209],
  darkviolet: [148, 0, 211],
  deeppink: [255, 20, 147],
  deepskyblue: [0, 191, 255],
  dimgray: [105, 105, 105],
  dimgrey: [105, 105, 105],
  dodgerblue: [30, 144, 255],
  firebrick: [178, 34, 34],
  floralwhite: [255, 250, 240],
  forestgreen: [34, 139, 34],
  fuchsia: [255, 0, 255],
  gainsboro: [220, 220, 220],
  ghostwhite: [248, 248, 255],
  gold: [255, 215, 0],
  goldenrod: [218, 165, 32],
  gray: [128, 128, 128],
  green: [0, 128, 0],
  greenyellow: [173, 255, 47],
  grey: [128, 128, 128],
  honeydew: [240, 255, 240],
  hotpink: [255, 105, 180],
  indianred: [205, 92, 92],
  indigo: [75, 0, 130],
  ivory: [255, 255, 240],
  khaki: [240, 230, 140],
  lavender: [230, 230, 250],
  lavenderblush: [255, 240, 245],
  lawngreen: [124, 252, 0],
  lemonchiffon: [255, 250, 205],
  lightblue: [173, 216, 230],
  lightcoral: [240, 128, 128],
  lightcyan: [224, 255, 255],
  lightgoldenrodyellow: [250, 250, 210],
  lightgray: [211, 211, 211],
  lightgreen: [144, 238, 144],
  lightgrey: [211, 211, 211],
  lightpink: [255, 182, 193],
  lightsalmon: [255, 160, 122],
  lightseagreen: [32, 178, 170],
  lightskyblue: [135, 206, 250],
  lightslategray: [119, 136, 153],
  lightslategrey: [119, 136, 153],
  lightsteelblue: [176, 196, 222],
  lightyellow: [255, 255, 224],
  lime: [0, 255, 0],
  limegreen: [50, 205, 50],
  linen: [250, 240, 230],
  magenta: [255, 0, 255],
  maroon: [128, 0, 0],
  mediumaquamarine: [102, 205, 170],
  mediumblue: [0, 0, 205],
  mediumorchid: [186, 85, 211],
  mediumpurple: [147, 112, 219],
  mediumseagreen: [60, 179, 113],
  mediumslateblue: [123, 104, 238],
  mediumspringgreen: [0, 250, 154],
  mediumturquoise: [72, 209, 204],
  mediumvioletred: [199, 21, 133],
  midnightblue: [25, 25, 112],
  mintcream: [245, 255, 250],
  mistyrose: [255, 228, 225],
  moccasin: [255, 228, 181],
  navajowhite: [255, 222, 173],
  navy: [0, 0, 128],
  oldlace: [253, 245, 230],
  olive: [128, 128, 0],
  olivedrab: [107, 142, 35],
  orange: [255, 165, 0],
  orangered: [255, 69, 0],
  orchid: [218, 112, 214],
  palegoldenrod: [238, 232, 170],
  palegreen: [152, 251, 152],
  paleturquoise: [175, 238, 238],
  palevioletred: [219, 112, 147],
  papayawhip: [255, 239, 213],
  peachpuff: [255, 218, 185],
  peru: [205, 133, 63],
  pink: [255, 192, 203],
  plum: [221, 160, 221],
  powderblue: [176, 224, 230],
  purple: [128, 0, 128],
  rebeccapurple: [102, 51, 153],
  red: [255, 0, 0],
  rosybrown: [188, 143, 143],
  royalblue: [65, 105, 225],
  saddlebrown: [139, 69, 19],
  salmon: [250, 128, 114],
  sandybrown: [244, 164, 96],
  seagreen: [46, 139, 87],
  seashell: [255, 245, 238],
  sienna: [160, 82, 45],
  silver: [192, 192, 192],
  skyblue: [135, 206, 235],
  slateblue: [106, 90, 205],
  slategray: [112, 128, 144],
  slategrey: [112, 128, 144],
  snow: [255, 250, 250],
  springgreen: [0, 255, 127],
  steelblue: [70, 130, 180],
  tan: [210, 180, 140],
  teal: [0, 128, 128],
  thistle: [216, 191, 216],
  tomato: [255, 99, 71],
  turquoise: [64, 224, 208],
  violet: [238, 130, 238],
  wheat: [245, 222, 179],
  white: [255, 255, 255],
  whitesmoke: [245, 245, 245],
  yellow: [255, 255, 0],
  yellowgreen: [154, 205, 50],
};

// Static color map (c0, c1, etc.)
export const staticColorMap = {
  0: [255, 0, 0],       // c0 = red
  1: [255, 165, 0],     // c1 = orange
  2: [255, 255, 0],     // c2 = yellow
  3: [0, 255, 0],       // c3 = lime/green
  4: [0, 0, 255],       // c4 = blue
  5: [75, 0, 130],      // c5 = indigo
  6: [238, 130, 238],   // c6 = violet
  7: [255, 192, 203],   // c7 = pink
  8: [0, 255, 255],     // c8 = cyan
  9: [255, 0, 255],     // c9 = magenta
  10: [128, 128, 128],  // c10 = gray
  11: [0, 0, 0],        // c11 = black
  12: [255, 255, 255],  // c12 = white
  13: [165, 42, 42],    // c13 = brown
  14: [0, 128, 0],      // c14 = darkgreen
  15: [128, 0, 0],      // c15 = maroon
};

// Rainbow colors for character-by-character coloring
export const RAINBOW_COLORS = ['#ff0000', '#ff7f00', '#ffff00', '#00ff00', '#0000ff', '#4b0082', '#9400d3'];
export const RAINBOW_NAMES = ['red', 'orange', 'yellow', 'lime', 'blue', 'purple', 'magenta'];

// Zebra colors
export const ZEBRA_COLORS = ['#000000', '#ffffff'];

// Rainbow colors for nested parentheses
export const PAREN_RAINBOW_COLORS = [
  [255, 100, 100],   // 0: red-ish
  [255, 180, 100],   // 1: orange-ish
  [255, 255, 100],   // 2: yellow-ish  
  [100, 255, 100],   // 3: green-ish
  [100, 200, 255],   // 4: cyan-ish
  [150, 150, 255],   // 5: blue-ish
  [200, 100, 255],   // 6: purple-ish
  [255, 100, 200],   // 7: magenta-ish
];

// Known function categories
export const FUNCTIONS = {
  graphics: ['wipe', 'ink', 'line', 'box', 'flood', 'circle', 'write', 'paste', 'stamp', 'point', 'poly', 'embed', 'grid', 'noise', 'qr', 'tri', 'plot', 'draw', 'shape'],
  control: ['def', 'now', 'later', 'repeat', 'bunch', 'once', 'bake', 'jump', 'page', 'tap', 'drag', 'lift', 'key', 'act', 'leave'],
  math: ['abs', 'floor', 'ceil', 'round', 'sqrt', 'pow', 'sin', 'cos', 'tan', 'min', 'max', 'random', 'wiggle', 'lerp', 'map', 'clamp', 'mod'],
  logic: ['if', 'cond', 'and', 'or', 'not', 'eq', 'neq', 'lt', 'gt', 'lte', 'gte', 'choose'],
  list: ['list', 'first', 'rest', 'nth', 'len', 'push', 'pop', 'concat', 'reverse', 'sort', 'filter', 'range'],
  sound: ['sound', 'synth', 'tone', 'play', 'stop', 'bpm', 'beat', 'note', 'chord', 'melody', 'sample', 'overtone', 'mic', 'amplitude', 'speaker'],
  text: ['write', 'text', 'font', 'print', 'say'],
  mode: ['fill', 'outline', 'stroke', 'nofill', 'nostroke'],
  transform: ['pan', 'unpan', 'zoom', 'spin', 'scroll', 'resolution', 'blur', 'contrast', 'mask', 'unmask'],
  utility: ['debug', 'log', 'die', 'steal', 'putback', 'label'],
};

// All known functions flattened
export const ALL_FUNCTIONS = Object.values(FUNCTIONS).flat();

// Math operators
export const MATH_OPERATORS = ['+', '-', '*', '/', '%', 'mod', '=', '>', '<', '>=', '<='];

// Special forms and control flow keywords
export const SPECIAL_FORMS = ['def', 'if', 'cond', 'later', 'once', 'lambda', 'let', 'do'];

// Token types for semantic highlighting
export const TOKEN_TYPES = {
  COMMENT: 'comment',
  STRING: 'string',
  NUMBER: 'number',
  TIMING: 'timing',
  TIMING_CYCLE: 'timing-cycle',
  COLOR_NAME: 'color-name',
  COLOR_CODE: 'color-code',
  PATTERN_CODE: 'pattern-code',
  RAINBOW: 'rainbow',
  ZEBRA: 'zebra',
  FADE: 'fade',
  EMBEDDED_CODE: 'embedded-code',
  PAINTING_REF: 'painting-ref',
  TAPE_REF: 'tape-ref',
  FUNCTION: 'function',
  SPECIAL_FORM: 'special-form',
  MATH_OPERATOR: 'math-operator',
  PAREN_OPEN: 'paren-open',
  PAREN_CLOSE: 'paren-close',
  IDENTIFIER: 'identifier',
  RGB_CHANNEL: 'rgb-channel',
  UNKNOWN: 'unknown',
};

/**
 * Tokenize KidLisp source code
 * @param {string} input - The KidLisp source code
 * @returns {string[]} Array of tokens
 */
export function tokenize(input) {
  const regex = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s()";',]+)/g;
  const tokens = [];
  let match;
  while ((match = regex.exec(input)) !== null) {
    const token = match[1];
    // Include comments for syntax highlighting (they need to be colored gray)
    tokens.push(token);
  }
  return tokens;
}

/**
 * Tokenize with position information for editor decorations
 * @param {string} input - The KidLisp source code
 * @returns {Array<{value: string, pos: number}>} Tokens with positions
 */
export function tokenizeWithPositions(input) {
  const regex = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s()";',]+)/g;
  const tokens = [];
  let match;
  while ((match = regex.exec(input)) !== null) {
    const token = match[1];
    const tokenStart = match.index + match[0].indexOf(token);
    tokens.push({ value: token, pos: tokenStart });
  }
  return tokens;
}

/**
 * Get the token type for a given token
 * @param {string} token - The token to classify
 * @param {string[]} tokens - All tokens (for context)
 * @param {number} index - Index of the token in the tokens array
 * @returns {string} The token type from TOKEN_TYPES
 */
export function getTokenType(token, tokens, index) {
  // Comments
  if (token.startsWith(';')) return TOKEN_TYPES.COMMENT;
  
  // Strings
  if ((token.startsWith('"') && token.endsWith('"')) ||
      (token.startsWith("'") && token.endsWith("'"))) {
    return TOKEN_TYPES.STRING;
  }
  
  // Timing patterns - cycle (3s..., 5f...)
  if (/^\d*\.?\d+[sf]\.\.\.?$/.test(token)) return TOKEN_TYPES.TIMING_CYCLE;
  
  // Timing patterns - delay (1.25s, 0.5s, 3f, 1s!)
  if (/^\d*\.?\d+[sf]!?$/.test(token)) return TOKEN_TYPES.TIMING;
  
  // Rainbow keyword
  if (token === 'rainbow') return TOKEN_TYPES.RAINBOW;
  
  // Zebra keyword
  if (token === 'zebra') return TOKEN_TYPES.ZEBRA;
  
  // Fade expressions
  if (token.startsWith('fade:')) return TOKEN_TYPES.FADE;
  
  // $code references
  if (token.startsWith('$') && token.length > 1 && /^[$][0-9A-Za-z]+$/.test(token)) {
    return TOKEN_TYPES.EMBEDDED_CODE;
  }
  
  // #code painting references (not hex colors)
  if (token.startsWith('#') && token.length > 1 && /^#[0-9A-Za-z]{1,8}$/.test(token)) {
    const codepart = token.substring(1);
    // Check if it's NOT a pure hex color
    const isHexColor = /^[0-9A-Fa-f]{3}$|^[0-9A-Fa-f]{4}$|^[0-9A-Fa-f]{6}$|^[0-9A-Fa-f]{8}$/.test(codepart);
    if (!isHexColor) return TOKEN_TYPES.PAINTING_REF;
  }
  
  // !code tape references
  if (token.startsWith('!') && token.length > 1 && /^![0-9A-Za-z]+$/.test(token)) {
    return TOKEN_TYPES.TAPE_REF;
  }
  
  // Color codes (c0, c1, etc.)
  if (/^c\d+$/i.test(token)) return TOKEN_TYPES.COLOR_CODE;
  
  // Pattern codes (p0, p1, etc.)
  if (/^p\d+$/i.test(token)) return TOKEN_TYPES.PATTERN_CODE;
  
  // CSS color names
  if (cssColors[token.toLowerCase()]) return TOKEN_TYPES.COLOR_NAME;
  
  // Parentheses
  if (token === '(') return TOKEN_TYPES.PAREN_OPEN;
  if (token === ')') return TOKEN_TYPES.PAREN_CLOSE;
  
  // Math operators
  if (MATH_OPERATORS.includes(token)) return TOKEN_TYPES.MATH_OPERATOR;
  
  // Check for function call (first token after opening paren)
  if (index > 0 && tokens[index - 1] === '(') {
    if (SPECIAL_FORMS.includes(token)) return TOKEN_TYPES.SPECIAL_FORM;
    if (ALL_FUNCTIONS.includes(token)) return TOKEN_TYPES.FUNCTION;
    // Unknown function
    return TOKEN_TYPES.FUNCTION;
  }
  
  // Numbers (check for RGB channel context)
  if (/^-?\d+(\.\d+)?$/.test(token)) {
    const isNumeric = (t) => t && /^-?\d+(\.\d+)?$/.test(t);
    const prevToken = index > 0 ? tokens[index - 1] : null;
    const nextToken = index < tokens.length - 1 ? tokens[index + 1] : null;
    const next2Token = index < tokens.length - 2 ? tokens[index + 2] : null;
    const prev2Token = index > 1 ? tokens[index - 2] : null;
    
    // Check if part of RGB sequence
    if ((isNumeric(nextToken) && isNumeric(next2Token)) ||
        (isNumeric(prevToken) && isNumeric(nextToken)) ||
        (isNumeric(prev2Token) && isNumeric(prevToken))) {
      return TOKEN_TYPES.RGB_CHANNEL;
    }
    return TOKEN_TYPES.NUMBER;
  }
  
  // Special forms at any position
  if (SPECIAL_FORMS.includes(token)) return TOKEN_TYPES.SPECIAL_FORM;
  
  // Known functions at any position
  if (ALL_FUNCTIONS.includes(token)) return TOKEN_TYPES.FUNCTION;
  
  return TOKEN_TYPES.IDENTIFIER;
}

/**
 * Get the color for a token (returns CSS color string or special markers)
 * @param {string} token - The token
 * @param {string[]} tokens - All tokens
 * @param {number} index - Token index
 * @param {Object} options - Options for coloring
 * @param {boolean} options.isEditMode - Whether in edit mode (for timing animation)
 * @param {boolean} options.lightMode - Whether in light mode (higher contrast)
 * @returns {string} Color value (CSS color, RGB string, or special marker like RAINBOW, ZEBRA, COMPOUND:x:y)
 */
export function getTokenColor(token, tokens, index, options = {}) {
  const { isEditMode = false, lightMode = false } = options;
  const tokenType = getTokenType(token, tokens, index);
  
  switch (tokenType) {
    case TOKEN_TYPES.COMMENT:
      return 'gray';
      
    case TOKEN_TYPES.STRING:
      return 'yellow';
      
    case TOKEN_TYPES.TIMING:
    case TOKEN_TYPES.TIMING_CYCLE:
      // In edit mode, simulate blinking
      if (isEditMode) {
        const match = token.match(/^(\d*\.?\d+)([sf])/);
        if (match) {
          const value = parseFloat(match[1]);
          const unit = match[2];
          const now = typeof performance !== 'undefined' ? performance.now() : Date.now();
          const blinkDuration = 200;
          
          let duration;
          if (unit === 's') {
            duration = value * 1000;
          } else {
            duration = (value / 60) * 1000; // Frame-based at 60fps
          }
          
          const cyclePhase = now % duration;
          
          if (cyclePhase < blinkDuration) {
            return tokenType === TOKEN_TYPES.TIMING_CYCLE ? 'lime' : 'red';
          }
        }
      }
      return 'yellow';
      
    case TOKEN_TYPES.RAINBOW:
      return 'RAINBOW';
      
    case TOKEN_TYPES.ZEBRA:
      return 'ZEBRA';
      
    case TOKEN_TYPES.FADE:
      return 'mediumseagreen';
      
    case TOKEN_TYPES.EMBEDDED_CODE:
      return 'COMPOUND:limegreen:lime';
      
    case TOKEN_TYPES.PAINTING_REF:
      return 'COMPOUND:magenta:orange';
      
    case TOKEN_TYPES.TAPE_REF:
      return 'COMPOUND:cyan:teal';
      
    case TOKEN_TYPES.COLOR_CODE: {
      const colorIndex = parseInt(token.substring(1));
      if (staticColorMap[colorIndex]) {
        const [r, g, b] = staticColorMap[colorIndex];
        return `${r},${g},${b}`;
      }
      return 'orange';
    }
      
    case TOKEN_TYPES.PATTERN_CODE: {
      const patternIndex = parseInt(token.substring(1));
      if (patternIndex === 0) return 'RAINBOW';
      if (patternIndex === 1) return 'ZEBRA';
      return 'orange';
    }
      
    case TOKEN_TYPES.COLOR_NAME: {
      const colorValue = cssColors[token.toLowerCase()];
      if (colorValue) {
        return `${colorValue[0]},${colorValue[1]},${colorValue[2]}`;
      }
      return 'orange';
    }
      
    case TOKEN_TYPES.PAREN_OPEN:
    case TOKEN_TYPES.PAREN_CLOSE:
      return getParenColor(tokens, index);
      
    case TOKEN_TYPES.MATH_OPERATOR:
      return 'lime';
      
    case TOKEN_TYPES.SPECIAL_FORM:
      return token === 'repeat' ? 'magenta' : 'purple';
      
    case TOKEN_TYPES.FUNCTION:
      return 'cyan';
      
    case TOKEN_TYPES.RGB_CHANNEL: {
      const value = Math.max(0, Math.min(255, parseFloat(token)));
      const isNumeric = (t) => t && /^-?\d+(\.\d+)?$/.test(t);
      const prevToken = index > 0 ? tokens[index - 1] : null;
      const nextToken = index < tokens.length - 1 ? tokens[index + 1] : null;
      const next2Token = index < tokens.length - 2 ? tokens[index + 2] : null;
      const prev2Token = index > 1 ? tokens[index - 2] : null;
      
      // Determine R, G, or B channel
      if (isNumeric(nextToken) && isNumeric(next2Token)) {
        // First of three (Red)
        return `${value},0,0`;
      } else if (isNumeric(prevToken) && isNumeric(nextToken)) {
        // Middle of three (Green)
        return `0,${value},0`;
      } else if (isNumeric(prev2Token) && isNumeric(prevToken)) {
        // Last of three (Blue) - deepskyblue hue
        const greenComponent = Math.round(value * 0.75);
        return `0,${greenComponent},${value}`;
      }
      return 'pink';
    }
      
    case TOKEN_TYPES.NUMBER:
      return 'pink';
      
    case TOKEN_TYPES.IDENTIFIER:
    default:
      return 'orange';
  }
}

/**
 * Get rainbow color for parentheses based on nesting depth
 * @param {string[]} tokens - All tokens
 * @param {number} index - Index of the parenthesis
 * @returns {string} RGB color string
 */
export function getParenColor(tokens, index) {
  let depth = 0;
  for (let i = 0; i < index; i++) {
    if (tokens[i] === '(') depth++;
    else if (tokens[i] === ')') depth--;
  }
  
  // Adjust for closing paren
  if (tokens[index] === ')') {
    depth = Math.max(0, depth - 1);
  }
  
  const colorIndex = depth % PAREN_RAINBOW_COLORS.length;
  const [r, g, b] = PAREN_RAINBOW_COLORS[colorIndex];
  return `${r},${g},${b}`;
}

/**
 * Color a fade expression for display
 * @param {string} fadeToken - The fade token like "fade:red-blue-yellow"
 * @returns {Array<{text: string, color: string}>} Segments with colors
 */
export function colorFadeExpression(fadeToken) {
  if (!fadeToken.startsWith('fade:')) {
    return [{ text: fadeToken, color: 'white' }];
  }
  
  const parts = fadeToken.split(':');
  if (parts.length < 2) {
    return [{ text: fadeToken, color: 'white' }];
  }
  
  const segments = [];
  
  // "fade" in emerald
  segments.push({ text: 'fade', color: 'mediumseagreen' });
  segments.push({ text: ':', color: 'lime' });
  
  // Handle neat/dirty modifiers
  let colorPart = parts[1];
  let direction = parts[2];
  
  if (parts[1] === 'neat' || parts[1] === 'dirty') {
    segments.push({ text: parts[1], color: parts[1] === 'neat' ? 'cyan' : 'orange' });
    segments.push({ text: ':', color: 'lime' });
    colorPart = parts[2] || '';
    direction = parts[3];
  } else if (parts[2] === 'neat' || parts[2] === 'dirty') {
    direction = undefined;
  }
  
  // Color each color name in the gradient
  const colorNames = colorPart.split('-');
  for (let i = 0; i < colorNames.length; i++) {
    const colorName = colorNames[i];
    
    // Get the actual color for this color name
    let color = 'white';
    if (cssColors[colorName]) {
      const [r, g, b] = cssColors[colorName];
      color = `${r},${g},${b}`;
    } else if (colorName.match(/^c\d+$/)) {
      const idx = parseInt(colorName.substring(1));
      if (staticColorMap[idx]) {
        const [r, g, b] = staticColorMap[idx];
        color = `${r},${g},${b}`;
      }
    } else if (colorName === 'rainbow') {
      // Rainbow word gets per-character coloring
      for (let j = 0; j < colorName.length; j++) {
        segments.push({ text: colorName[j], color: RAINBOW_NAMES[j % RAINBOW_NAMES.length] });
      }
      if (i < colorNames.length - 1) {
        segments.push({ text: '-', color: 'mediumseagreen' });
      }
      continue;
    } else if (colorName === 'zebra') {
      // Zebra word gets alternating black/white
      for (let j = 0; j < colorName.length; j++) {
        segments.push({ text: colorName[j], color: j % 2 === 0 ? 'black' : 'white' });
      }
      if (i < colorNames.length - 1) {
        segments.push({ text: '-', color: 'mediumseagreen' });
      }
      continue;
    }
    
    segments.push({ text: colorName, color });
    
    // Add dash separator (except for last)
    if (i < colorNames.length - 1) {
      segments.push({ text: '-', color: 'mediumseagreen' });
    }
  }
  
  // Add direction if present
  if (direction && direction !== 'neat' && direction !== 'dirty') {
    segments.push({ text: ':', color: 'lime' });
    const numericAngle = parseFloat(direction);
    segments.push({ 
      text: direction, 
      color: !isNaN(numericAngle) ? 'yellow' : 'cyan' 
    });
  }
  
  return segments;
}

/**
 * Apply rainbow coloring to a word (returns segments for each character)
 * @param {string} word - The word to color
 * @returns {Array<{text: string, color: string}>} Character segments with colors
 */
export function colorRainbow(word) {
  return word.split('').map((char, i) => ({
    text: char,
    color: RAINBOW_COLORS[i % RAINBOW_COLORS.length],
  }));
}

/**
 * Apply zebra coloring to a word (returns segments for each character)
 * @param {string} word - The word to color
 * @returns {Array<{text: string, color: string}>} Character segments with colors
 */
export function colorZebra(word) {
  return word.split('').map((char, i) => ({
    text: char,
    color: ZEBRA_COLORS[i % ZEBRA_COLORS.length],
  }));
}

/**
 * High contrast color map for light mode
 */
export const LIGHT_MODE_COLOR_MAP = {
  'yellow': '#cc9900',
  'cyan': '#0099cc',
  'lime': '#00aa00',
  'pink': '#cc0066',
  'orange': '#cc6600',
  'magenta': '#cc00cc',
  'gray': '#666666',
  'lightgray': '#999999',
  'white': '#cccccc',
  'mediumseagreen': '#2e8b57',
};

/**
 * Get high-contrast version of a color for light mode
 * @param {string} color - The color name or RGB value
 * @returns {string} High contrast color for light backgrounds
 */
export function getLightModeColor(color) {
  return LIGHT_MODE_COLOR_MAP[color.toLowerCase()] || color;
}

/**
 * Convert RGB string to hex color
 * @param {string} rgb - RGB string like "255,128,64"
 * @returns {string} Hex color like "#ff8040"
 */
export function rgbToHex(rgb) {
  if (!rgb.includes(',')) return rgb; // Already a color name
  const [r, g, b] = rgb.split(',').map(n => parseInt(n.trim()));
  return `#${((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1)}`;
}

/**
 * Convert hex color to RGB array
 * @param {string} hex - Hex color like "#ff8040"
 * @returns {number[]} RGB array like [255, 128, 64]
 */
export function hexToRgb(hex) {
  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
  return result ? [
    parseInt(result[1], 16),
    parseInt(result[2], 16),
    parseInt(result[3], 16),
  ] : null;
}

/**
 * Build a highlighted representation of KidLisp code
 * Returns an array of segments with text and color information
 * @param {string} source - KidLisp source code
 * @param {Object} options - Options
 * @returns {Array<{text: string, color: string}>} Highlighted segments
 */
export function highlightKidLisp(source, options = {}) {
  const tokens = tokenize(source);
  const segments = [];
  let lastPos = 0;
  
  // We need to track positions to preserve whitespace
  let searchPos = 0;
  
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    const tokenPos = source.indexOf(token, searchPos);
    
    if (tokenPos === -1) continue;
    
    // Add whitespace before token
    if (tokenPos > searchPos) {
      segments.push({ text: source.substring(searchPos, tokenPos), color: null });
    }
    
    const color = getTokenColor(token, tokens, i, options);
    
    // Handle special color markers
    if (color === 'RAINBOW') {
      segments.push(...colorRainbow(token));
    } else if (color === 'ZEBRA') {
      segments.push(...colorZebra(token));
    } else if (color.startsWith('COMPOUND:')) {
      const [, prefixColor, identifierColor] = color.split(':');
      segments.push({ text: token[0], color: prefixColor });
      segments.push({ text: token.substring(1), color: identifierColor });
    } else if (token.startsWith('fade:')) {
      segments.push(...colorFadeExpression(token));
    } else {
      segments.push({ text: token, color });
    }
    
    searchPos = tokenPos + token.length;
  }
  
  // Add any trailing whitespace
  if (searchPos < source.length) {
    segments.push({ text: source.substring(searchPos), color: null });
  }
  
  return segments;
}

// Export default for convenience
export default {
  tokenize,
  tokenizeWithPositions,
  getTokenType,
  getTokenColor,
  getParenColor,
  colorFadeExpression,
  colorRainbow,
  colorZebra,
  highlightKidLisp,
  getLightModeColor,
  rgbToHex,
  hexToRgb,
  cssColors,
  staticColorMap,
  TOKEN_TYPES,
  RAINBOW_COLORS,
  RAINBOW_NAMES,
  ZEBRA_COLORS,
  PAREN_RAINBOW_COLORS,
  FUNCTIONS,
  ALL_FUNCTIONS,
  MATH_OPERATORS,
  SPECIAL_FORMS,
  LIGHT_MODE_COLOR_MAP,
};
