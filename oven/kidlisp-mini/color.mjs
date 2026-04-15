// KidLisp Mini Color Resolver
// CSS color names, RGB, hex, alpha, rainbow
// ~80 lines, produces ~1 KB minified

const CSS_COLORS = {
  'red': [255, 0, 0],
  'green': [0, 128, 0],
  'blue': [0, 0, 255],
  'white': [255, 255, 255],
  'black': [0, 0, 0],
  'gray': [128, 128, 128],
  'grey': [128, 128, 128],
  'cyan': [0, 255, 255],
  'magenta': [255, 0, 255],
  'yellow': [255, 255, 0],
  'orange': [255, 165, 0],
  'purple': [128, 0, 128],
  'pink': [255, 192, 203],
  'lime': [0, 255, 0],
  'brown': [165, 42, 42],
  'navy': [0, 0, 128],
  'teal': [0, 128, 128],
  'olive': [128, 128, 0],
  'maroon': [128, 0, 0],
  'aqua': [0, 255, 255],
  'silver': [192, 192, 192],
  'gold': [255, 215, 0],
  'indigo': [75, 0, 130],
  'violet': [238, 130, 238],
  'turquoise': [64, 224, 208],
  'khaki': [240, 230, 140],
  'salmon': [250, 128, 114],
  'coral': [255, 127, 80],
  'ivory': [255, 255, 240],
  'snow': [255, 250, 250],
};

function resolveColor(arg) {
  // Already an array [r, g, b, a]
  if (Array.isArray(arg)) {
    return arg;
  }

  // Number: grayscale
  if (typeof arg === 'number') {
    const v = Math.round(arg) & 0xFF;
    return [v, v, v, 255];
  }

  // String
  if (typeof arg === 'string') {
    // CSS color name
    if (CSS_COLORS[arg.toLowerCase()]) {
      const [r, g, b] = CSS_COLORS[arg.toLowerCase()];
      return [r, g, b, 255];
    }

    // Hex color
    if (arg.startsWith('#')) {
      return parseHex(arg);
    }

    // Named special: transparent/none
    if (arg === '0' || arg === 'transparent' || arg === 'none') {
      return [0, 0, 0, 0];
    }

    // Unknown string: return black
    return [0, 0, 0, 255];
  }

  // Object: special types
  if (typeof arg === 'object' && arg.type === 'rainbow') {
    return rainbowColor(arg.frame || 0);
  }

  return [255, 255, 255, 255]; // default white
}

function parseHex(hex) {
  let h = hex.replace(/^#/, '');

  // #RGB → #RRGGBB
  if (h.length === 3) {
    h = h.split('').map(c => c + c).join('');
  }

  // #RRGGBB
  if (h.length === 6) {
    const num = parseInt(h, 16);
    return [
      (num >> 16) & 0xFF,
      (num >> 8) & 0xFF,
      num & 0xFF,
      255
    ];
  }

  return [255, 255, 255, 255];
}

function rainbowColor(frame = 0) {
  // Cycle through hue based on frame
  const hue = (frame / 60) % 1; // 60 frames = full cycle
  const [r, g, b] = hslToRgb(hue, 1, 0.5);
  return [Math.round(r), Math.round(g), Math.round(b), 255];
}

function hslToRgb(h, s, l) {
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs((h * 6) % 2 - 1));
  const m = l - c / 2;

  let r, g, b;
  if (h < 1 / 6) [r, g, b] = [c, x, 0];
  else if (h < 2 / 6) [r, g, b] = [x, c, 0];
  else if (h < 3 / 6) [r, g, b] = [0, c, x];
  else if (h < 4 / 6) [r, g, b] = [0, x, c];
  else if (h < 5 / 6) [r, g, b] = [x, 0, c];
  else [r, g, b] = [c, 0, x];

  return [(r + m) * 255, (g + m) * 255, (b + m) * 255];
}

export { resolveColor, rainbowColor, hslToRgb, CSS_COLORS };
