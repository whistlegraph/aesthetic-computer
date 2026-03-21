// 🌈 KidLisp Syntax Highlighting for VS Code
// Extracted from shared/kidlisp-syntax.mjs for VS Code extension bundling
// Provides Monaco-parity highlighting including:
//   - Rainbow and zebra color words
//   - Animated timing token blinking (1s, 2s..., 3f, etc.)
//   - Color codes (c0, c1, etc.) and CSS color names
//   - $code, #code, !code references
//   - fade: expressions with gradient coloring
//   - RGB channel highlighting
//   - Rainbow nested parentheses

// CSS Colors (subset for syntax highlighting)
export const cssColors: Record<string, [number, number, number]> = {
  aliceblue: [240, 248, 255],
  aqua: [0, 255, 255],
  aquamarine: [127, 255, 212],
  black: [0, 0, 0],
  blue: [0, 0, 255],
  blueviolet: [138, 43, 226],
  brown: [165, 42, 42],
  chartreuse: [127, 255, 0],
  coral: [255, 127, 80],
  crimson: [220, 20, 60],
  cyan: [0, 255, 255],
  darkblue: [0, 0, 139],
  darkcyan: [0, 139, 139],
  darkgray: [169, 169, 169],
  darkgreen: [0, 100, 0],
  darkmagenta: [139, 0, 139],
  darkorange: [255, 140, 0],
  darkred: [139, 0, 0],
  darkviolet: [148, 0, 211],
  deeppink: [255, 20, 147],
  deepskyblue: [0, 191, 255],
  dodgerblue: [30, 144, 255],
  fuchsia: [255, 0, 255],
  gold: [255, 215, 0],
  gray: [128, 128, 128],
  green: [0, 128, 0],
  greenyellow: [173, 255, 47],
  grey: [128, 128, 128],
  hotpink: [255, 105, 180],
  indigo: [75, 0, 130],
  khaki: [240, 230, 140],
  lavender: [230, 230, 250],
  lawngreen: [124, 252, 0],
  lightblue: [173, 216, 230],
  lightcoral: [240, 128, 128],
  lightcyan: [224, 255, 255],
  lightgray: [211, 211, 211],
  lightgreen: [144, 238, 144],
  lightpink: [255, 182, 193],
  lightyellow: [255, 255, 224],
  lime: [0, 255, 0],
  limegreen: [50, 205, 50],
  magenta: [255, 0, 255],
  maroon: [128, 0, 0],
  mediumaquamarine: [102, 205, 170],
  mediumblue: [0, 0, 205],
  mediumseagreen: [60, 179, 113],
  mediumspringgreen: [0, 250, 154],
  midnightblue: [25, 25, 112],
  navy: [0, 0, 128],
  olive: [128, 128, 0],
  orange: [255, 165, 0],
  orangered: [255, 69, 0],
  orchid: [218, 112, 214],
  palegreen: [152, 251, 152],
  pink: [255, 192, 203],
  plum: [221, 160, 221],
  purple: [128, 0, 128],
  red: [255, 0, 0],
  royalblue: [65, 105, 225],
  salmon: [250, 128, 114],
  seagreen: [46, 139, 87],
  silver: [192, 192, 192],
  skyblue: [135, 206, 235],
  slateblue: [106, 90, 205],
  springgreen: [0, 255, 127],
  steelblue: [70, 130, 180],
  tan: [210, 180, 140],
  teal: [0, 128, 128],
  tomato: [255, 99, 71],
  turquoise: [64, 224, 208],
  violet: [238, 130, 238],
  wheat: [245, 222, 179],
  white: [255, 255, 255],
  yellow: [255, 255, 0],
  yellowgreen: [154, 205, 50],
};

// Static color map (c0, c1, etc.)
export const staticColorMap: Record<number, [number, number, number]> = {
  0: [255, 0, 0],       // c0 = red
  1: [255, 165, 0],     // c1 = orange
  2: [255, 255, 0],     // c2 = yellow
  3: [0, 255, 0],       // c3 = lime
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
export const PAREN_RAINBOW_COLORS: [number, number, number][] = [
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
} as const;

export type TokenType = typeof TOKEN_TYPES[keyof typeof TOKEN_TYPES];

/**
 * Tokenize KidLisp source code
 */
export function tokenize(input: string): string[] {
  const regex = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s()";',]+)/g;
  const tokens: string[] = [];
  let match;
  while ((match = regex.exec(input)) !== null) {
    const token = match[1];
    tokens.push(token);
  }
  return tokens;
}

/**
 * Tokenize with position information for editor decorations
 */
export function tokenizeWithPositions(input: string): Array<{value: string, pos: number, line: number, col: number}> {
  const regex = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s()";',]+)/g;
  const tokens: Array<{value: string, pos: number, line: number, col: number}> = [];
  let match;
  
  // Pre-compute line starts for position mapping
  const lineStarts: number[] = [0];
  for (let i = 0; i < input.length; i++) {
    if (input[i] === '\n') {
      lineStarts.push(i + 1);
    }
  }
  
  while ((match = regex.exec(input)) !== null) {
    const token = match[1];
    const tokenStart = match.index + match[0].indexOf(token);
    
    // Find line and column
    let line = 0;
    for (let i = lineStarts.length - 1; i >= 0; i--) {
      if (tokenStart >= lineStarts[i]) {
        line = i;
        break;
      }
    }
    const col = tokenStart - lineStarts[line];
    
    tokens.push({ value: token, pos: tokenStart, line, col });
  }
  return tokens;
}

/**
 * Get the token type for a given token
 */
export function getTokenType(token: string, tokens: string[], index: number): TokenType {
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
    return TOKEN_TYPES.FUNCTION;
  }
  
  // Numbers (check for RGB channel context)
  if (/^-?\d+(\.\d+)?$/.test(token)) {
    const isNumeric = (t: string | undefined) => t && /^-?\d+(\.\d+)?$/.test(t);
    const prevToken = index > 0 ? tokens[index - 1] : undefined;
    const nextToken = index < tokens.length - 1 ? tokens[index + 1] : undefined;
    const next2Token = index < tokens.length - 2 ? tokens[index + 2] : undefined;
    const prev2Token = index > 1 ? tokens[index - 2] : undefined;
    
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

export interface ColorOptions {
  isEditMode?: boolean;
  lightMode?: boolean;
}

/**
 * Get the color for a token (returns CSS color string or special markers)
 */
export function getTokenColor(token: string, tokens: string[], index: number, options: ColorOptions = {}): string {
  const { isEditMode = false } = options;
  const tokenType = getTokenType(token, tokens, index);
  
  switch (tokenType) {
    case TOKEN_TYPES.COMMENT:
      return 'gray';
      
    case TOKEN_TYPES.STRING:
      return 'yellow';
      
    case TOKEN_TYPES.TIMING:
    case TOKEN_TYPES.TIMING_CYCLE:
      if (isEditMode) {
        const match = token.match(/^(\d*\.?\d+)([sf])/);
        if (match) {
          const value = parseFloat(match[1]);
          const unit = match[2];
          const now = typeof performance !== 'undefined' ? performance.now() : Date.now();
          const blinkDuration = 200;
          
          let duration: number;
          if (unit === 's') {
            duration = value * 1000;
          } else {
            duration = (value / 60) * 1000;
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
      const isNumeric = (t: string | undefined) => t && /^-?\d+(\.\d+)?$/.test(t);
      const prevToken = index > 0 ? tokens[index - 1] : undefined;
      const nextToken = index < tokens.length - 1 ? tokens[index + 1] : undefined;
      const next2Token = index < tokens.length - 2 ? tokens[index + 2] : undefined;
      const prev2Token = index > 1 ? tokens[index - 2] : undefined;
      
      if (isNumeric(nextToken) && isNumeric(next2Token)) {
        return `${value},0,0`;
      } else if (isNumeric(prevToken) && isNumeric(nextToken)) {
        return `0,${value},0`;
      } else if (isNumeric(prev2Token) && isNumeric(prevToken)) {
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
 */
export function getParenColor(tokens: string[], index: number): string {
  let depth = 0;
  for (let i = 0; i < index; i++) {
    if (tokens[i] === '(') depth++;
    else if (tokens[i] === ')') depth--;
  }
  
  if (tokens[index] === ')') {
    depth = Math.max(0, depth - 1);
  }
  
  const colorIndex = depth % PAREN_RAINBOW_COLORS.length;
  const [r, g, b] = PAREN_RAINBOW_COLORS[colorIndex];
  return `${r},${g},${b}`;
}

/**
 * Convert color string to hex format for VS Code decorations
 */
export function colorToHex(color: string): string {
  // Already hex
  if (color.startsWith('#')) return color;
  
  // Named color - look up or use as-is
  if (!color.includes(',')) {
    const mapped = cssColors[color.toLowerCase()];
    if (mapped) {
      const [r, g, b] = mapped;
      return `#${((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1)}`;
    }
    return color; // Return named color for CSS
  }
  
  // RGB string like "255,128,64"
  const parts = color.split(',').map(n => parseInt(n.trim()));
  if (parts.length >= 3) {
    const [r, g, b] = parts;
    return `#${((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1)}`;
  }
  
  return color;
}

/**
 * High contrast color map for light mode
 */
export const LIGHT_MODE_COLOR_MAP: Record<string, string> = {
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
 */
export function getLightModeColor(color: string): string {
  return LIGHT_MODE_COLOR_MAP[color.toLowerCase()] || color;
}
