#!/usr/bin/env node
// Benchmark pieces in the real AC runtime, headless: frames per second at a
// few screen sizes, plus source size. A piece is a .mjs (JavaScript) or a
// .lisp (KidLisp) file, fed to the runtime the way the Whistlegraph app and
// the TV page feed it (acSEND dropped:piece), so both languages are measured
// by the same renderer under the same loop.
//
//   node kidlisp/tools/bench-pieces.mjs [--site https://aesthetic.computer] [--seconds 8]
//        [--sizes 390x520,1280x720] [--json out.json] piece.mjs piece.lisp …
//
// fps comes from bios's own stats counter (window.acReportFpsToParent), read
// once a second for --seconds after the piece paints. Headless Chrome caps
// requestAnimationFrame at 60, so 60 means "at least 60"; the sizes climb
// until a piece falls under it.
import puppeteer from 'puppeteer';
import {readFileSync, writeFileSync} from 'node:fs';
import {basename, extname, resolve} from 'node:path';

const argv = process.argv.slice(2);
const flag = (name, fallback) => { const i = argv.indexOf('--' + name); if (i < 0) return fallback; const v = argv[i + 1]; argv.splice(i, 2); return v; };
const SITE = flag('site', 'https://aesthetic.computer');
const SECONDS = Number(flag('seconds', 8));
const SIZES = flag('sizes', '390x520,1280x720,1920x1080').split(',').map(s => s.split('x').map(Number));
const JSON_OUT = flag('json', '');
const CHROME = process.env.CHROME_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const files = argv;
if (!files.length) { console.error('usage: bench-pieces.mjs [--site URL] [--seconds N] [--sizes WxH,…] piece.mjs piece.lisp …'); process.exit(1); }

const tokens = text => Math.round(text.length / 3.6);   // a rough code-token estimate; chars are the real measure
const RUNTIME = `${SITE}/wipe?noauth=true&noplot=true&nogap=true&nolabel=true&preview=walkieware`;

async function measure(browser, file, [width, height]) {
  const source = readFileSync(file, 'utf8');
  const isKidLisp = extname(file) === '.lisp';
  const page = await browser.newPage();
  await page.setViewport({width, height, deviceScaleFactor: 1});
  const errors = [];
  page.on('pageerror', e => errors.push(String(e).slice(0, 200)));
  page.on('console', async m => {
    if (!(m.type() === 'error' || /⛔|Unknown KidLisp|Evaluation failure/.test(m.text()))) return;
    const parts = await Promise.all(m.args().map(a => a.evaluate(v => v instanceof Error ? v.message + ' @ ' + String(v.stack || '').split('\n').slice(1, 3).join(' | ') : String(v)).catch(() => '?')));
    errors.push((parts.join(' ') || m.text()).slice(0, 400));
  });
  // The runtime is the page, as the render pool loads it. bios reports fps to
  // a parent window; `parent` is replaceable, so a stand-in parent collects it.
  await page.goto(RUNTIME, {waitUntil: 'domcontentloaded', timeout: 90_000});
  await page.waitForFunction(() => window.preloaded && window.acSEND, {timeout: 90_000, polling: 100});
  await page.evaluate((source, isKidLisp, name) => {
    window.__fps = [];
    window.parent = {postMessage: m => { if (m && m.type === 'ac:fps-report') window.__fps.push(m.fps); }};
    window.acReportFpsToParent = true;
    window.acSEND({type: 'dropped:piece', content: {name, source, search: 'noauth=true&noplot=true&nogap=true&nolabel=true', isKidLisp}});
  }, source, isKidLisp, 'bench-' + basename(file, extname(file)));
  // Let the piece boot and settle before counting.
  await new Promise(r => setTimeout(r, 3000));
  await page.evaluate(() => { window.__fps = []; });
  await new Promise(r => setTimeout(r, SECONDS * 1000));
  const fps = await page.evaluate(() => window.__fps);
  const shot = await page.screenshot({encoding: 'base64'});
  await page.close();
  const sorted = [...fps].sort((a, b) => a - b);
  return {file: basename(file), language: isKidLisp ? 'kidlisp' : 'javascript', width, height, chars: source.length, lines: source.split('\n').length, tokens: tokens(source),
    samples: fps.length, fpsMedian: sorted[Math.floor(sorted.length / 2)] ?? 0, fpsMin: sorted[0] ?? 0, fpsMax: sorted.at(-1) ?? 0, errors, shot};
}

const browser = await puppeteer.launch({headless: true, executablePath: CHROME, args: ['--no-sandbox', '--ignore-certificate-errors', '--disable-background-timer-throttling', '--disable-renderer-backgrounding', '--disable-backgrounding-occluded-windows', '--autoplay-policy=no-user-gesture-required', '--mute-audio']});
const results = [];
try {
  for (const file of files) for (const size of SIZES) {
    const r = await measure(browser, resolve(file), size);
    results.push(r);
    console.log(`${r.file.padEnd(22)} ${r.language.padEnd(10)} ${String(r.width + 'x' + r.height).padEnd(10)} fps median ${String(r.fpsMedian).padStart(3)}  min ${String(r.fpsMin).padStart(3)}  max ${String(r.fpsMax).padStart(3)}  (${r.samples} s)  ${r.chars} chars ~${r.tokens} tokens${r.errors.length ? '  ERRORS ' + r.errors[0] : ''}`);
    if (JSON_OUT) writeFileSync(JSON_OUT.replace(/\.json$/, '') + `-${basename(r.file)}-${r.width}x${r.height}.png`, Buffer.from(r.shot, 'base64'));
  }
} finally { await browser.close(); }
if (JSON_OUT) writeFileSync(JSON_OUT, JSON.stringify(results.map(({shot, ...r}) => r), null, 1));
