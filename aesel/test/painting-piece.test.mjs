import test from 'node:test';import assert from 'node:assert/strict';
import {backgroundOf, spriteRegions, paintingPieceRequest, pieceFromAnswer, pieceProblems, paintingPieceSlug, PIECE_MAX_LINES} from '../src/painting-piece.mjs';

// A flat ground with two blobs on it: a red square and a blue bar.
function picture(width = 64, height = 64, ground = [240, 230, 220]) {
  const pixels = new Uint8ClampedArray(width * height * 4);
  const put = (x, y, [r, g, b]) => { const i = (y * width + x) * 4; pixels[i] = r; pixels[i + 1] = g; pixels[i + 2] = b; pixels[i + 3] = 255; };
  for (let y = 0; y < height; y++) for (let x = 0; x < width; x++) put(x, y, ground);
  for (let y = 8; y < 24; y++) for (let x = 8; x < 24; x++) put(x, y, [200, 30, 30]);
  for (let y = 40; y < 48; y++) for (let x = 16; x < 56; x++) put(x, y, [30, 40, 200]);
  return {width, height, pixels};
}

test('the ground is the border colour and the blobs become regions with their boxes', () => {
  const image = picture();
  assert.deepEqual(backgroundOf(image), [240, 230, 220]);
  const {background, regions} = spriteRegions(image);
  assert.deepEqual(background, [240, 230, 220]);
  assert.equal(regions.length, 2);
  const [bar, square] = regions; // largest first
  assert.deepEqual([bar.x, bar.y, bar.w, bar.h], [16, 40, 40, 8]);
  assert.deepEqual([square.x, square.y, square.w, square.h], [8, 8, 16, 16]);
  assert.deepEqual(bar.color, [30, 40, 200]);
  assert.ok(bar.share > square.share);
});

test('a flat painting has no regions, and the instructions say to use the whole picture', () => {
  const flat = picture(32, 32);
  flat.pixels.fill(120);
  const analysis = spriteRegions(flat);
  assert.deepEqual(analysis.regions, []);
  const request = paintingPieceRequest({code: 'hf2', handle: 'jeffrey', width: 32, height: 32, png: 'AAAA', analysis});
  assert.match(request.system[0].text, /treat the whole painting as one sprite/);
});

test('the request carries the painting as a bounded PNG block and names the code, regions and ground', () => {
  const image = picture();
  const analysis = spriteRegions(image);
  const request = paintingPieceRequest({code: 'hf2', handle: 'jeffrey', width: 64, height: 64, png: 'AAAA', analysis, guides: '# pieces.md\n\nguide text'});
  assert.equal(request.model, 'anthropic/claude-sonnet-5.5');
  assert.equal(request.messages.length, 1);
  assert.deepEqual(request.messages[0].content[1], {type: 'image', source: {type: 'base64', media_type: 'image/png', data: 'AAAA'}});
  const words = request.system[0].text;
  assert.ok(words.startsWith('# pieces.md'));
  assert.match(words, /get\.painting\("hf2"\)\.by\(\)/);
  assert.match(words, /wipe\(240, 230, 220\)/);
  assert.match(words, /1\. x 16, y 40, 40×8, mostly #1e28c8/);
  assert.match(words, /@jeffrey → piece v0/);
});

test('a repair round replays the last answer and lists what was wrong', () => {
  const analysis = spriteRegions(picture());
  const request = paintingPieceRequest({code: 'hf2', handle: 'jeffrey', width: 64, height: 64, png: 'AAAA', analysis,
    previous: 'export function paint() {}', problems: ['Export `boot`.']});
  assert.equal(request.messages.length, 3);
  assert.equal(request.messages[1].role, 'assistant');
  assert.match(request.messages[1].content[0].text, /export function paint/);
  assert.match(request.messages[2].content[0].text, /- Export `boot`\./);
});

test('the piece is read out of the fenced block, or taken bare', () => {
  assert.equal(pieceFromAnswer('Here you go:\n```js\nexport function paint() {}\n```\nEnjoy.'), 'export function paint() {}\n');
  assert.equal(pieceFromAnswer('export function paint() {}'), 'export function paint() {}\n');
  assert.throws(() => pieceFromAnswer('```js\n\n```'), /no piece/);
});

const good = `// #hf2 by @jeffrey → piece v0
// The bar slides.
let img, bar;
export function boot({ get, painting }) {
  get.painting("hf2").by().then((loaded) => { img = loaded.img; bar = painting(40, 8, ({ paste }) => paste(img, -16, -40)); });
}
export function paint({ wipe, paste, screen }) {
  wipe(240, 230, 220);
  if (bar) paste(bar, 10, screen.height / 2);
}
`;

test('a complete piece that loads its painting passes; the usual slips are named', () => {
  assert.deepEqual(pieceProblems(good, 'hf2'), []);
  assert.deepEqual(pieceProblems('export function paint() {', 'hf2').length, 1);
  const problems = pieceProblems('import x from "y";\nexport function paint() { ink.box(1); }', 'hf2');
  assert.ok(problems.some((p) => /No import statements/.test(p)));
  assert.ok(problems.some((p) => p === 'Export `boot`.'));
  assert.ok(problems.some((p) => /get\.painting\("hf2"\)/.test(p)));
  assert.ok(problems.some((p) => /ink\(color\)\.box/.test(p)));
  const long = good + '// filler\n'.repeat(PIECE_MAX_LINES);
  assert.ok(pieceProblems(long, 'hf2').some((p) => /lines/.test(p)));
});

test('the slug is the painting code, lowercased', () => {
  assert.equal(paintingPieceSlug('Hf2'), 'hf2');
});
