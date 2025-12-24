#!/usr/bin/env node

/**
 * Rosary Bead Visualizer for KidLisp - Sixel Output
 * 
 * Parses KidLisp code and renders it as a rosary bead diagram.
 * Each expression becomes a bead on a string, loops form circles.
 * 
 * Usage:
 *   ./rosary-sixel.mjs "red, ink blue, line 0 0 100 100"
 *   ./rosary-sixel.mjs "$mtz"  # fetch from AC
 *   echo "wipe, ink, box" | ./rosary-sixel.mjs
 */

const WIDTH = 400;
const HEIGHT = 300;

// Bead types with colors (Calder-inspired palette)
const BEAD_STYLES = {
  state:   { color: [220, 60, 60],   symbol: '○' },   // red - wipe, ink, def
  draw:    { color: [60, 100, 220],  symbol: '●' },   // blue - line, box, circle, point
  effect:  { color: [220, 180, 40],  symbol: '◆' },   // yellow - scroll, blur, zoom
  timing:  { color: [180, 80, 180],  symbol: '◇' },   // purple - 1s, 0.5s, later
  branch:  { color: [40, 180, 120],  symbol: '◈' },   // green - if, ?
  loop:    { color: [255, 140, 40],  symbol: '⬡' },   // orange - repeat
  atom:    { color: [150, 150, 150], symbol: '·' },   // gray - numbers, strings
};

// Command classification
const COMMAND_TYPES = {
  // State changes
  'wipe': 'state', 'ink': 'state', 'def': 'state', 'let': 'state',
  // Drawing
  'line': 'draw', 'box': 'draw', 'circle': 'draw', 'point': 'draw',
  'write': 'draw', 'oval': 'draw', 'tri': 'draw', 'draw': 'draw',
  // Effects
  'scroll': 'effect', 'blur': 'effect', 'zoom': 'effect', 'pan': 'effect',
  'coat': 'effect', 'fade': 'effect', 'noise': 'effect',
  // Timing
  '1s': 'timing', '0.5s': 'timing', '0.3s': 'timing', '2s': 'timing',
  'later': 'timing', 'every': 'timing',
  // Branching
  'if': 'branch', '?': 'branch', 'cond': 'branch',
  // Loops
  'repeat': 'loop', 'loop': 'loop', 'for': 'loop',
};

// Color names to RGB
const COLOR_NAMES = {
  'red': [255, 80, 80], 'blue': [80, 80, 255], 'green': [80, 200, 80],
  'yellow': [255, 255, 80], 'purple': [180, 80, 180], 'orange': [255, 160, 80],
  'cyan': [80, 255, 255], 'magenta': [255, 80, 255], 'white': [255, 255, 255],
  'black': [40, 40, 40], 'gray': [150, 150, 150], 'navy': [40, 40, 120],
  'brown': [140, 90, 60], 'pink': [255, 180, 200],
};

class SixelCanvas {
  constructor(width, height) {
    this.width = width;
    this.height = height;
    this.buffer = new Uint8Array(width * height * 3);
    this.clear(20, 20, 30); // Dark background
  }

  clear(r, g, b) {
    for (let i = 0; i < this.buffer.length; i += 3) {
      this.buffer[i] = r;
      this.buffer[i + 1] = g;
      this.buffer[i + 2] = b;
    }
  }

  setPixel(x, y, r, g, b) {
    x = Math.round(x);
    y = Math.round(y);
    if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
      const idx = (y * this.width + x) * 3;
      this.buffer[idx] = r;
      this.buffer[idx + 1] = g;
      this.buffer[idx + 2] = b;
    }
  }

  // Draw a filled circle (bead)
  fillCircle(cx, cy, r, color) {
    const [cr, cg, cb] = color;
    for (let y = -r; y <= r; y++) {
      for (let x = -r; x <= r; x++) {
        if (x * x + y * y <= r * r) {
          this.setPixel(cx + x, cy + y, cr, cg, cb);
        }
      }
    }
  }

  // Draw circle outline
  strokeCircle(cx, cy, r, color, thickness = 1) {
    const [cr, cg, cb] = color;
    for (let y = -r - thickness; y <= r + thickness; y++) {
      for (let x = -r - thickness; x <= r + thickness; x++) {
        const dist = Math.sqrt(x * x + y * y);
        if (dist >= r - thickness / 2 && dist <= r + thickness / 2) {
          this.setPixel(cx + x, cy + y, cr, cg, cb);
        }
      }
    }
  }

  // Draw a line (string between beads)
  line(x0, y0, x1, y1, color, thickness = 2) {
    const [r, g, b] = color;
    const dx = Math.abs(x1 - x0);
    const dy = Math.abs(y1 - y0);
    const steps = Math.max(dx, dy) * 2;
    
    for (let i = 0; i <= steps; i++) {
      const t = i / steps;
      const x = x0 + (x1 - x0) * t;
      const y = y0 + (y1 - y0) * t;
      
      // Thicken the line
      for (let ox = -thickness / 2; ox <= thickness / 2; ox++) {
        for (let oy = -thickness / 2; oy <= thickness / 2; oy++) {
          this.setPixel(x + ox, y + oy, r, g, b);
        }
      }
    }
  }

  // Draw curved arc (for loops)
  arc(cx, cy, r, startAngle, endAngle, color, thickness = 2) {
    const [cr, cg, cb] = color;
    const steps = Math.abs(endAngle - startAngle) * r;
    
    for (let i = 0; i <= steps; i++) {
      const t = i / steps;
      const angle = startAngle + (endAngle - startAngle) * t;
      const x = cx + Math.cos(angle) * r;
      const y = cy + Math.sin(angle) * r;
      
      for (let ox = -thickness / 2; ox <= thickness / 2; ox++) {
        for (let oy = -thickness / 2; oy <= thickness / 2; oy++) {
          this.setPixel(x + ox, y + oy, cr, cg, cb);
        }
      }
    }
  }

  toSixel() {
    let output = '\x1bPq';
    const colors = new Map();
    let colorIndex = 0;
    
    for (let band = 0; band < Math.ceil(this.height / 6); band++) {
      const bandArrays = new Map();
      
      for (let x = 0; x < this.width; x++) {
        for (let dy = 0; dy < 6; dy++) {
          const y = band * 6 + dy;
          if (y >= this.height) break;
          
          const idx = (y * this.width + x) * 3;
          const r = this.buffer[idx];
          const g = this.buffer[idx + 1];
          const b = this.buffer[idx + 2];
          
          const colorKey = (r << 16) | (g << 8) | b;
          
          if (!colors.has(colorKey)) {
            colors.set(colorKey, colorIndex++);
            output += `#${colors.get(colorKey)};2;${Math.round(r * 100 / 255)};${Math.round(g * 100 / 255)};${Math.round(b * 100 / 255)}`;
          }
          
          const color = colors.get(colorKey);
          if (!bandArrays.has(color)) {
            bandArrays.set(color, new Array(this.width).fill(0));
          }
          bandArrays.get(color)[x] |= (1 << dy);
        }
      }
      
      for (const [color, pixels] of bandArrays) {
        output += `#${color}`;
        for (const pixel of pixels) {
          output += String.fromCharCode(63 + pixel);
        }
        output += '$';
      }
      output += '-';
    }
    
    return output + '\x1b\\';
  }
}

// Simple KidLisp tokenizer (handles commas and parens)
// Splits on commas and newlines, keeps paren expressions together
function tokenize(code) {
  const tokens = [];
  let current = '';
  let inString = false;
  let parenDepth = 0;
  
  for (let i = 0; i < code.length; i++) {
    const char = code[i];
    
    if (char === '"') {
      inString = !inString;
      current += char;
    } else if (inString) {
      current += char;
    } else if (char === '(') {
      if (parenDepth === 0 && current.trim()) {
        tokens.push(current.trim());
        current = '';
      }
      parenDepth++;
      current += char;
    } else if (char === ')') {
      parenDepth--;
      current += char;
      if (parenDepth === 0) {
        tokens.push(current.trim());
        current = '';
      }
    } else if (parenDepth > 0) {
      current += char;
    } else if (char === ',' || char === '\n') {
      if (current.trim()) tokens.push(current.trim());
      current = '';
    } else {
      current += char;
    }
  }
  
  if (current.trim()) tokens.push(current.trim());
  return tokens;
}

// Parse a token into a bead
function parseBead(token) {
  token = token.trim();
  
  // Handle parenthesized expressions
  if (token.startsWith('(') && token.endsWith(')')) {
    const inner = token.slice(1, -1).trim();
    const parts = inner.split(/\s+/);
    const head = parts[0];
    
    // Check for timing like (1s ...)
    if (/^\d+(\.\d+)?s$/.test(head)) {
      return {
        type: 'timing',
        name: head,
        children: parts.slice(1).map(p => parseBead(p)),
        style: BEAD_STYLES.timing
      };
    }
    
    // Check for known commands
    const type = COMMAND_TYPES[head] || 'state';
    return {
      type,
      name: head,
      args: parts.slice(1),
      children: type === 'loop' ? parts.slice(2).map(p => parseBead(p)) : [],
      style: BEAD_STYLES[type]
    };
  }
  
  // Handle simple commands like "red" or "ink blue"
  const parts = token.split(/\s+/);
  const head = parts[0];
  
  // Check if it's a color name (acts as wipe)
  if (COLOR_NAMES[head] && parts.length === 1) {
    return {
      type: 'state',
      name: head,
      args: [],
      style: { ...BEAD_STYLES.state, color: COLOR_NAMES[head] }
    };
  }
  
  const type = COMMAND_TYPES[head] || 'state';
  return {
    type,
    name: head,
    args: parts.slice(1),
    style: BEAD_STYLES[type]
  };
}

// Layout beads in a rosary pattern
function layoutRosary(beads, width, height) {
  const positions = [];
  const n = beads.length;
  
  if (n === 0) return positions;
  
  const centerX = width / 2;
  const centerY = height / 2;
  const radius = Math.min(width, height) * 0.35;
  const beadRadius = Math.max(8, Math.min(20, 200 / n));
  
  // Arrange in a circle
  for (let i = 0; i < n; i++) {
    const angle = (i / n) * Math.PI * 2 - Math.PI / 2; // Start from top
    positions.push({
      x: centerX + Math.cos(angle) * radius,
      y: centerY + Math.sin(angle) * radius,
      radius: beadRadius,
      bead: beads[i],
      angle
    });
  }
  
  return positions;
}

// Draw the rosary
function drawRosary(canvas, positions) {
  const stringColor = [100, 100, 120];
  
  // Draw the string (connect all beads in a loop)
  for (let i = 0; i < positions.length; i++) {
    const curr = positions[i];
    const next = positions[(i + 1) % positions.length];
    canvas.line(curr.x, curr.y, next.x, next.y, stringColor, 2);
  }
  
  // Draw beads
  for (const pos of positions) {
    const { x, y, radius, bead } = pos;
    const color = bead.style.color;
    
    // Draw bead shadow
    canvas.fillCircle(x + 2, y + 2, radius, [20, 20, 30]);
    
    // Draw bead
    canvas.fillCircle(x, y, radius, color);
    
    // Highlight
    canvas.fillCircle(x - radius * 0.3, y - radius * 0.3, radius * 0.3, 
      [Math.min(255, color[0] + 60), Math.min(255, color[1] + 60), Math.min(255, color[2] + 60)]);
  }
  
  // Draw legend
  drawLegend(canvas, positions);
}

function drawLegend(canvas, positions) {
  const startY = 20;
  const startX = 10;
  let y = startY;
  
  // Draw title
  // (Would need font rendering - skip for now)
  
  // Draw bead labels with lines pointing to beads
  const labelColor = [180, 180, 200];
  
  // Just draw type indicators in corner
  const types = ['state', 'draw', 'effect', 'timing', 'loop'];
  types.forEach((type, i) => {
    const style = BEAD_STYLES[type];
    canvas.fillCircle(startX + 8, y + i * 18, 6, style.color);
  });
}

// Fetch code from AC
async function fetchCode(slug) {
  if (!slug.startsWith('$')) slug = '$' + slug;
  
  try {
    const response = await fetch(`https://aesthetic.computer/api/code/${slug.slice(1)}`);
    if (response.ok) {
      const data = await response.json();
      return data.code || data.text || '';
    }
  } catch (e) {
    // Ignore
  }
  return null;
}

async function main() {
  let code = process.argv[2];
  const asciiMode = process.argv.includes('--ascii');
  
  // Check if it's a slug
  if (code && (code.startsWith('$') || /^[a-z0-9]{3}$/i.test(code))) {
    const fetched = await fetchCode(code);
    if (fetched) {
      code = fetched;
      console.error(`Fetched: ${code.substring(0, 50)}...`);
    }
  }
  
  // Or read from stdin
  if (!code) {
    const chunks = [];
    for await (const chunk of process.stdin) {
      chunks.push(chunk);
    }
    code = Buffer.concat(chunks).toString().trim();
  }
  
  if (!code) {
    console.error('Usage: rosary-sixel.mjs "code" or rosary-sixel.mjs $slug');
    console.error('       rosary-sixel.mjs "code" --ascii  (text mode)');
    console.error('Example: rosary-sixel.mjs "red, ink blue, line 0 0 100 100"');
    process.exit(1);
  }
  
  // Parse and render
  const tokens = tokenize(code);
  const beads = tokens.map(parseBead);
  
  console.error(`Parsed ${beads.length} beads: ${beads.map(b => b.name).join(' → ')}`);
  
  if (asciiMode) {
    // ASCII rosary
    renderAsciiRosary(beads);
  } else {
    // Sixel rosary
    const canvas = new SixelCanvas(WIDTH, HEIGHT);
    const positions = layoutRosary(beads, WIDTH, HEIGHT);
    drawRosary(canvas, positions);
    
    process.stdout.write('\x1b[H\x1b[2J'); // Clear screen
    process.stdout.write(canvas.toSixel());
    process.stdout.write('\n');
  }
}

// ASCII rosary renderer
function renderAsciiRosary(beads) {
  const n = beads.length;
  if (n === 0) {
    console.log('(empty rosary)');
    return;
  }
  
  // Type symbols
  const symbols = {
    state: '○', draw: '●', effect: '◆', timing: '◇', branch: '◈', loop: '⬡', atom: '·'
  };
  
  // ANSI colors
  const colors = {
    state: '\x1b[91m',    // red
    draw: '\x1b[94m',     // blue
    effect: '\x1b[93m',   // yellow
    timing: '\x1b[95m',   // magenta
    branch: '\x1b[92m',   // green
    loop: '\x1b[33m',     // orange-ish
    atom: '\x1b[90m',     // gray
  };
  const reset = '\x1b[0m';
  const dim = '\x1b[2m';
  
  // Circular layout for larger programs
  if (n > 4) {
    renderCircularRosary(beads, symbols, colors, reset, dim);
  } else {
    // Linear for small programs
    console.log();
    console.log('  ┌' + '─'.repeat(n * 4 + 2) + '┐');
    console.log('  │ ' + dim + 'LOOP' + reset + ' '.repeat(n * 4 - 2) + '│');
    console.log('  └─┬' + '─'.repeat(n * 4) + '┬┘');
    console.log('    │' + ' '.repeat(n * 4) + '│');
    
    let beadLine = '    ';
    let nameLine = '    ';
    
    for (let i = 0; i < n; i++) {
      const b = beads[i];
      const sym = symbols[b.type] || '○';
      const col = colors[b.type] || '';
      
      beadLine += col + sym + reset + ' ── ';
      const name = b.name.substring(0, 5).padEnd(5);
      nameLine += name;
    }
    
    beadLine = beadLine.slice(0, -4) + dim + ' ↩' + reset;
    
    console.log(beadLine);
    console.log(dim + nameLine + reset);
  }
  
  console.log();
  console.log(dim + '  Legend: ' + reset +
    colors.state + '○' + reset + ' state  ' +
    colors.draw + '●' + reset + ' draw  ' +
    colors.effect + '◆' + reset + ' effect  ' +
    colors.timing + '◇' + reset + ' timing  ' +
    colors.loop + '⬡' + reset + ' loop');
  console.log();
}

// Render circular rosary for larger programs
function renderCircularRosary(beads, symbols, colors, reset, dim) {
  const n = beads.length;
  const width = 50;
  const height = 20;
  const cx = width / 2;
  const cy = height / 2;
  const rx = 18; // x radius
  const ry = 7;  // y radius (compressed for terminal)
  
  // Create grid
  const grid = Array(height).fill(null).map(() => Array(width).fill(' '));
  const colorGrid = Array(height).fill(null).map(() => Array(width).fill(''));
  
  // Place beads around ellipse
  const positions = [];
  for (let i = 0; i < n; i++) {
    const angle = (i / n) * Math.PI * 2 - Math.PI / 2;
    const x = Math.round(cx + Math.cos(angle) * rx);
    const y = Math.round(cy + Math.sin(angle) * ry);
    positions.push({ x, y, bead: beads[i], index: i });
  }
  
  // Draw connecting arcs (simplified as lines)
  for (let i = 0; i < n; i++) {
    const curr = positions[i];
    const next = positions[(i + 1) % n];
    drawLine(grid, curr.x, curr.y, next.x, next.y, dim + '·' + reset);
  }
  
  // Place beads (overwrite lines)
  for (const pos of positions) {
    const b = pos.bead;
    const sym = symbols[b.type] || '○';
    const col = colors[b.type] || '';
    
    if (pos.y >= 0 && pos.y < height && pos.x >= 0 && pos.x < width) {
      grid[pos.y][pos.x] = sym;
      colorGrid[pos.y][pos.x] = col;
    }
  }
  
  // Render
  console.log();
  for (let y = 0; y < height; y++) {
    let line = '  ';
    for (let x = 0; x < width; x++) {
      const col = colorGrid[y][x];
      const char = grid[y][x];
      if (col) {
        line += col + char + reset;
      } else {
        line += char;
      }
    }
    console.log(line);
  }
  
  // Labels below
  console.log();
  console.log(dim + '  Beads: ' + reset + beads.map((b, i) => {
    const col = colors[b.type] || '';
    return col + (i + 1) + '.' + b.name + reset;
  }).join(' → '));
}

// Bresenham line for ASCII
function drawLine(grid, x0, y0, x1, y1, char) {
  const dx = Math.abs(x1 - x0);
  const dy = Math.abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;
  
  let x = x0, y = y0;
  while (true) {
    if (y >= 0 && y < grid.length && x >= 0 && x < grid[0].length) {
      if (grid[y][x] === ' ') grid[y][x] = char;
    }
    
    if (x === x1 && y === y1) break;
    
    const e2 = 2 * err;
    if (e2 > -dy) { err -= dy; x += sx; }
    if (e2 < dx) { err += dx; y += sy; }
  }
}

main().catch(console.error);
