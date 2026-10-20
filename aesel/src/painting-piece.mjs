// painting-piece — a finished painting becomes a piece: the painting is cut
// into sprite regions, a hosted model writes a piece that loads the painting
// by its code, crops those regions and sets them moving, and the answer is
// checked before anything is published. notepaint.art's "Make piece" button
// runs this on lith (nopaint-piece.mjs); the terminal can run the same path.
//
// Nothing here touches the network or the disk: callers bring the pixels and
// the model, so the same functions run on the server and in tests.
import {parse} from './vendor/acorn.mjs';
import {sourceChecks} from './edit-contract.mjs';

export const PIECE_MODEL = 'anthropic/claude-sonnet-5.5';
export const PIECE_MAX_TOKENS = 6000;
export const PIECE_MAX_LINES = 160;

const quant = (v) => v >> 4; // 16 levels a channel: enough to find a background
const distance = (a, b) => Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[2] - b[2]);
const hex = ([r, g, b]) => '#' + [r, g, b].map((v) => v.toString(16).padStart(2, '0')).join('');

// The painting's ground: the most common colour along its border, averaged.
export function backgroundOf({width, height, pixels}) {
  const bins = new Map();
  const add = (i) => {
    const key = `${quant(pixels[i])},${quant(pixels[i + 1])},${quant(pixels[i + 2])}`;
    const bin = bins.get(key) || {n: 0, r: 0, g: 0, b: 0};
    bin.n++; bin.r += pixels[i]; bin.g += pixels[i + 1]; bin.b += pixels[i + 2];
    bins.set(key, bin);
  };
  for (let x = 0; x < width; x++) { add(x * 4); add(((height - 1) * width + x) * 4); }
  for (let y = 1; y < height - 1; y++) { add(y * width * 4); add((y * width + width - 1) * 4); }
  const top = [...bins.values()].sort((a, b) => b.n - a.n)[0];
  return [Math.round(top.r / top.n), Math.round(top.g / top.n), Math.round(top.b / top.n)];
}

// Cut the painting into the things on it. Cells that differ from the ground
// are grouped into connected blobs; each blob's box is a sprite candidate.
// A flat painting yields no regions, and the prompt says so.
export function spriteRegions(image, {cell = 8, max = 6, threshold = 60, minCells = 2} = {}) {
  const {width, height, pixels} = image;
  const background = backgroundOf(image);
  const cols = Math.ceil(width / cell), rows = Math.ceil(height / cell);
  const active = new Uint8Array(cols * rows), sum = [];
  for (let cy = 0; cy < rows; cy++) for (let cx = 0; cx < cols; cx++) {
    let away = 0, n = 0, r = 0, g = 0, b = 0;
    for (let y = cy * cell; y < Math.min(height, (cy + 1) * cell); y++)
      for (let x = cx * cell; x < Math.min(width, (cx + 1) * cell); x++) {
        const i = (y * width + x) * 4, c = [pixels[i], pixels[i + 1], pixels[i + 2]];
        n++; if (distance(c, background) > threshold) { away++; r += c[0]; g += c[1]; b += c[2]; }
      }
    const k = cy * cols + cx;
    active[k] = away / n > 0.25 ? 1 : 0;
    sum[k] = away ? [Math.round(r / away), Math.round(g / away), Math.round(b / away)] : background;
  }
  const seen = new Uint8Array(cols * rows), blobs = [];
  for (let start = 0; start < cols * rows; start++) {
    if (!active[start] || seen[start]) continue;
    const stack = [start], cells = [];
    seen[start] = 1;
    while (stack.length) {
      const k = stack.pop(), cx = k % cols, cy = (k - cx) / cols;
      cells.push(k);
      for (let dy = -1; dy <= 1; dy++) for (let dx = -1; dx <= 1; dx++) {
        const nx = cx + dx, ny = cy + dy, nk = ny * cols + nx;
        if (nx < 0 || ny < 0 || nx >= cols || ny >= rows || seen[nk] || !active[nk]) continue;
        seen[nk] = 1; stack.push(nk);
      }
    }
    blobs.push(cells);
  }
  const regions = blobs.filter((cells) => cells.length >= minCells).map((cells) => {
    let x0 = cols, y0 = rows, x1 = 0, y1 = 0, r = 0, g = 0, b = 0;
    for (const k of cells) {
      const cx = k % cols, cy = (k - cx) / cols;
      x0 = Math.min(x0, cx); y0 = Math.min(y0, cy); x1 = Math.max(x1, cx); y1 = Math.max(y1, cy);
      r += sum[k][0]; g += sum[k][1]; b += sum[k][2];
    }
    const x = x0 * cell, y = y0 * cell;
    const w = Math.min(width, (x1 + 1) * cell) - x, h = Math.min(height, (y1 + 1) * cell) - y;
    return {x, y, w, h, share: Math.round((cells.length / (cols * rows)) * 1000) / 1000,
      color: [Math.round(r / cells.length), Math.round(g / cells.length), Math.round(b / cells.length)]};
  }).sort((a, b) => b.share - a.share).slice(0, max);
  return {background, regions};
}

export const paintingPieceSlug = (code) => String(code).toLowerCase();

const regionLine = (region, i) =>
  `${i + 1}. x ${region.x}, y ${region.y}, ${region.w}×${region.h}, mostly ${hex(region.color)}, ${Math.round(region.share * 100)}% of the picture`;

// The words the model works from. `guides` is the piece context (pieces.md,
// screen.md); the caller decides how much of it to pay for.
export function paintingPieceInstructions({code, handle, width, height, background, regions, guides = ''}) {
  const stage = `wipe(${background.join(', ')})`;
  const cuts = regions.length
    ? ['The painting has been cut into these regions, in pixel coordinates of the painting itself; they are your sprites:', ...regions.map(regionLine)].join('\n')
    : 'Nothing on the painting stands apart from its ground, so treat the whole painting as one sprite and work with its colour and grain.';
  return [
    guides,
    `# Painting → piece

You turn one finished painting from notepaint.art into one Aesthetic Computer piece. The painting is \`#${code}\` by @${handle}, ${width}×${height} pixels; its ground colour is ${hex(background)}. Look at the picture that is attached, then write a piece that is recognisably made of it: the sprites move, repeat, drift, breathe or answer touch in a way that suits what they depict. Keep the painting's palette; the stage is its ground, \`${stage}\`.

${cuts}

How the piece gets its pixels:
- In \`boot({ get, painting })\` call \`get.painting("${code}").by().then(({ img }) => { ... })\`; \`img\` is the whole painting as a buffer with \`width\`, \`height\` and \`pixels\`. Until it arrives, paint the stage and the text \`#${code}\` with \`ink().write\`.
- Cut a sprite with \`painting(w, h, ({ paste }) => paste(img, -x, -y))\` using a region's x, y, w, h. Do this once, in boot, after the image arrives; keep the buffers in module-level variables.
- Draw a sprite with \`paste(sprite, x, y, scale)\` or \`stamp(sprite, x, y, scale, angle)\`, where stamp rotates about the sprite's centre.
- Animate in \`sim()\` and draw in \`paint({ wipe, ink, paste, stamp, screen, num })\`. Scale the composition to \`screen.width\` and \`screen.height\` so it reads on a phone and a desktop.

Rules:
- One complete ES module in a single \`\`\`js fenced block, nothing outside it. No \`import\` statements; everything comes from the lifecycle arguments.
- Export \`boot\` and \`paint\`; export \`sim\` and \`act\` when you use them. At most ${PIECE_MAX_LINES} lines.
- The first line is \`// #${code} by @${handle} → piece v0\`, then one line saying what moves.
- Colours are numbers: \`ink(r, g, b)\`, never CSS strings. Use \`ink(color).box(...)\`, not \`ink.box\`.
- Leave the top-left 6×6 corner clear for the system label, as the guides say.`,
  ].filter(Boolean).join('\n\n---\n\n');
}

// An Anthropic Messages request for /api/easel-inference: the words plus the
// painting as a bounded PNG. A repair round carries the last answer and what
// was wrong with it.
export function paintingPieceRequest({code, handle, width, height, png, analysis, guides, model = PIECE_MODEL, previous = '', problems = []}) {
  const messages = [{role: 'user', content: [
    {type: 'text', text: `Here is #${code}. Write the piece.`},
    {type: 'image', source: {type: 'base64', media_type: 'image/png', data: png}},
  ]}];
  if (previous && problems.length) {
    messages.push({role: 'assistant', content: [{type: 'text', text: '```js\n' + previous + '\n```'}]});
    messages.push({role: 'user', content: [{type: 'text', text:
      ['That piece did not pass the checks:', ...problems.map((p) => `- ${p}`), 'Send the whole corrected piece again, in one ```js block.'].join('\n')}]});
  }
  return {model, max_tokens: PIECE_MAX_TOKENS, messages,
    system: [{type: 'text', text: paintingPieceInstructions({code, handle, width, height, ...analysis, guides})}]};
}

// The piece out of the model's answer: the one fenced block, or a bare module.
export function pieceFromAnswer(text) {
  const answer = String(text || '');
  const fenced = /```(?:js|javascript|mjs)?\s*\n([\s\S]*?)```/.exec(answer);
  const source = (fenced ? fenced[1] : answer).trim();
  if (!source) throw Error('The model sent no piece.');
  return source + '\n';
}

// What would stop this source from being published as the painting's piece.
export function pieceProblems(source, code) {
  const problems = [];
  let tree;
  try { tree = parse(source, {ecmaVersion: 'latest', sourceType: 'module'}); }
  catch (error) { return [`The piece is not a complete JavaScript module: ${error.message}`]; }
  const exported = new Set();
  for (const node of tree.body) {
    if (node.type === 'ImportDeclaration') problems.push('No import statements; a piece takes everything from its lifecycle arguments.');
    if (node.type !== 'ExportNamedDeclaration') continue;
    if (node.declaration?.type === 'FunctionDeclaration') exported.add(node.declaration.id.name);
    if (node.declaration?.type === 'VariableDeclaration') for (const d of node.declaration.declarations) if (d.id.name) exported.add(d.id.name);
    for (const s of node.specifiers || []) exported.add(s.exported.name);
  }
  for (const name of ['boot', 'paint']) if (!exported.has(name)) problems.push(`Export \`${name}\`.`);
  if (!source.includes(`get.painting("${code}")`) && !source.includes(`get.painting('${code}')`))
    problems.push(`Load the painting with get.painting("${code}").by() in boot.`);
  const lines = source.split('\n').length;
  if (lines > PIECE_MAX_LINES) problems.push(`The piece is ${lines} lines; keep it at or under ${PIECE_MAX_LINES}.`);
  for (const finding of sourceChecks(source)) problems.push(finding.message);
  return problems;
}
