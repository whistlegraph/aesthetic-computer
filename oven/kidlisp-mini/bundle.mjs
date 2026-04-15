// KidLisp Mini Bundler
// Assembles minimal interpreter into a self-contained HTML file
// ~150 lines

import { promises as fs } from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { MongoClient } from 'mongodb';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function fetchPieceSource(pieceName) {
  // Try to fetch from MongoDB
  const mongoUri = process.env.MONGODB_CONNECTION_STRING;
  const mongoDb = process.env.MONGODB_NAME;

  if (!mongoUri || !mongoDb) {
    throw new Error('MongoDB not configured (MONGODB_CONNECTION_STRING, MONGODB_NAME)');
  }

  const client = new MongoClient(mongoUri);
  try {
    await client.connect();
    const db = client.db(mongoDb);
    const kidlispCollection = db.collection('kidlisp');

    // Look up by code field (handle $code style names)
    const cleanName = pieceName.replace(/^\$/, '');
    const piece = await kidlispCollection.findOne({
      code: cleanName
    });

    if (!piece || !piece.source) {
      throw new Error(`Piece not found: ${pieceName}`);
    }

    return piece.source;
  } finally {
    await client.close();
  }
}

// Fix KidLisp source format: wrap unparenthesized commands in parentheses
function fixKidLispSourceFormat(source) {
  const lines = source.split('\n');
  const fixed = lines.map(line => {
    line = line.trim();
    if (!line) return '';

    // Skip lines that already have parens or special prefixes
    if (line.startsWith('(') || line.startsWith('fade:')) {
      return line;
    }

    // Wrap atoms followed by other expressions in parens
    // This handles: "ink (? rainbow white 0) (1s... 24 64)" → "(ink (? rainbow white 0) (1s... 24 64))"
    return '(' + line + ')';
  });

  return fixed.join('\n');
}

async function bundleMini(source, seed = null) {
  // Fix source format: wrap unparenthesized commands in parentheses
  source = fixKidLispSourceFormat(source);

  // Read all module files
  const evalCode = await fs.readFile(path.join(__dirname, 'eval.mjs'), 'utf-8');
  const renderCode = await fs.readFile(path.join(__dirname, 'render.mjs'), 'utf-8');
  const colorCode = await fs.readFile(path.join(__dirname, 'color.mjs'), 'utf-8');
  const transformsCode = await fs.readFile(path.join(__dirname, 'transforms.mjs'), 'utf-8');
  const timingCode = await fs.readFile(path.join(__dirname, 'timing.mjs'), 'utf-8');

  // Concatenate all modules (removing imports/exports for bundling)
  const bundledCode = stripModuleSyntax(
    colorCode + '\n' +
    timingCode + '\n' +
    transformsCode + '\n' +
    renderCode + '\n' +
    evalCode
  );

  // Minify
  const minified = await minifyCode(bundledCode);

  // Escape source for inline JavaScript
  const escapedSource = source
    .replace(/\\/g, '\\\\')
    .replace(/`/g, '\\`')
    .replace(/\$/g, '\\$');

  // Generate HTML
  const html = generateHTML(minified, escapedSource, seed);

  return html;
}

function stripModuleSyntax(code) {
  // Remove import statements
  code = code.replace(/import\s+.*?from\s+['"].*?['"];?/g, '');

  // Remove export statements, keep the content
  code = code.replace(/export\s+\{[^}]*\}/g, '');
  code = code.replace(/export\s+function\s+/g, 'function ');
  code = code.replace(/export\s+class\s+/g, 'class ');
  code = code.replace(/export\s+const\s+/g, 'const ');

  return code;
}

async function minifyCode(code) {
  // Simple minification: remove comments and excess whitespace
  // Not as good as SWC but doesn't require external dependencies
  return code
    .replace(/\/\*[\s\S]*?\*\//g, '')  // Remove /* */ comments
    .replace(/\/\/.*$/gm, '')           // Remove // comments
    .replace(/\n\s*\n/g, '\n')          // Remove blank lines
    .split('\n')
    .map(line => line.trim())
    .filter(line => line && !line.startsWith('//'))
    .join('\n');
}

function generateHTML(minifiedCode, source, seed) {
  const seedValue = seed !== null ? seed : Math.floor(Math.random() * 0xFFFFFFFF);

  return `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>KidLisp Mini</title>
<style>
* { margin: 0; padding: 0; }
body { overflow: hidden; background: #000; }
canvas { display: block; width: 100vw; height: 100vh; }
</style>
</head>
<body>
<canvas id="c"></canvas>
<script>
// Bundled KidLisp Mini Interpreter
${minifiedCode}

// Piece source
const SOURCE = \`${source}\`;

// Seed from bootloader.art, fxhash, etc.
window.seed = window.seed || ${seedValue};
const SEED = window.seed;

// Bootstrap
const canvas = document.getElementById('c');
const ctx = canvas.getContext('2d', { alpha: false });
const W = canvas.width = window.innerWidth;
const H = canvas.height = window.innerHeight;

// Create interpreter and renderer
const kidlisp = new KidLispMini();
const renderer = makeRenderer(W, H);
const timing = new TimingEngine(SEED);

// Create API object that directly calls renderer methods
const api = {
  screen: { width: W, height: H },
  wipe: (r, g, b, a = 255) => renderer.wipe(r, g, b, a),
  ink: (r, g, b, a = 255) => renderer.setColor(r, g, b, a),
  line: (x0, y0, x1, y1) => renderer.line(x0, y0, x1, y1),
  box: (x, y, w, h, mode = 'fill') => renderer.box(x, y, w, h, mode),
  circle: (cx, cy, r, mode = 'fill') => renderer.circle(cx, cy, r, mode),
  plot: (x, y) => renderer.plot(x, y),
  write: (text, x, y) => renderer.write(text, x, y),
  scroll: (dx, dy) => renderer.scroll(dx, dy),
  spin: (angle) => renderer.spin(angle),
  zoom: (factor) => renderer.zoom(factor),
  contrast: (factor) => renderer.contrast(factor),
  fadeBackground: (colors, frame) => renderer.fadeBackground(colors, frame),
};

// Pass API to the interpreter
kidlisp.setApi(api);

// Parse piece
let ast;
try {
  ast = parse(SOURCE);
} catch (e) {
  console.error('Parse error:', e);
  ast = [];
}

// Main animation loop
let frameCount = 0;
function animate() {
  // Clear frame (will be overwritten by wipe if piece calls it)
  renderer.wipe(0, 0, 0, 255);

  // Debug: log on first frame
  if (frameCount === 0) {
    console.log('🎬 Frame 0 - AST length:', ast.length);
    console.log('🎬 AST:', ast);
    console.log('🎬 Renderer methods:', Object.keys(renderer).filter(k => typeof renderer[k] === 'function'));
  }

  // Evaluate the piece - functions will call api methods directly
  for (let i = 0; i < ast.length; i++) {
    const expr = ast[i];
    if (frameCount === 0) {
      console.log('Evaluating expr', i, ':', expr);
    }
    const result = kidlisp.evalExpr(expr, kidlisp.globalEnv, api, frameCount);
    if (frameCount === 0) {
      console.log('Result', i, ':', result);
    }
  }

  // Update display
  renderer.present(ctx);

  // Tick
  kidlisp.tick();
  timing.tick();
  frameCount++;

  requestAnimationFrame(animate);
}


// Start animation
requestAnimationFrame(animate);
</script>
</body>
</html>`;
}

export { bundleMini, fetchPieceSource };
