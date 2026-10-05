import puppeteer from 'puppeteer';
import sharp from 'sharp';
import gifenc from 'gifenc';
import { teiaIndexHTML } from '../system/backend/whistlegraph-teia.mjs';

// Capture the compiled artifact, never the mutable saved thread or a screenshot
// supplied by the client. The same HTML supplies both animation and thumbnail.
export async function renderWhistlegraphPreview(html, aspect = '2:3', options = {}) {
  if (!['2:3','9:16','1:1','4:3','16:9'].includes(aspect)) throw new Error('Invalid preview aspect');
  const [x,y] = aspect.split(':').map(Number);
  const width = Math.round(480 * x / Math.max(x,y)), height = Math.round(480 * y / Math.max(x,y));
  const count = 48, interval = 125;
  const browser = await puppeteer.launch({ headless:true, executablePath:options.executablePath || process.env.PUPPETEER_EXECUTABLE_PATH,
    args:['--no-sandbox','--disable-dev-shm-usage','--mute-audio','--enable-unsafe-swiftshader'] });
  const timer = setTimeout(() => browser.close().catch(() => browser.process()?.kill()), 90_000);
  try {
    const page = await browser.newPage();
    await page.setViewport({ width, height, deviceScaleFactor:1 });
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.setRequestInterception(true);
    page.on('request', request => {
      if (request.url() === 'https://whistlegraph-preview.invalid/index.html') request.respond({ contentType:'text/html', body:teiaIndexHTML(html) });
      else if (/^(blob:|data:)/.test(request.url())) request.continue();
      else request.abort();
    });
    await page.goto('https://whistlegraph-preview.invalid/index.html', { waitUntil:'load', timeout:30_000 });
    await page.waitForSelector('canvas', { timeout:20_000 });
    await new Promise(r => setTimeout(r, 2500));
    const frames = [];
    let thumbnail;
    for (let i=0; i<count; i++) {
      const started = Date.now();
      const png = Buffer.from(await page.screenshot({ type:'png' }));
      if (i === 0) thumbnail = await sharp(png).resize({ width:350, height:350, fit:'inside' }).png().toBuffer();
      frames.push(await sharp(png).ensureAlpha().raw().toBuffer());
      if (errors.length) throw new Error('Packed artwork failed during preview');
      await new Promise(r => setTimeout(r, Math.max(0, interval - (Date.now()-started))));
    }
    const encoder = gifenc.GIFEncoder();
    for (const rgba of frames) {
      const palette = gifenc.quantize(rgba, 128);
      const indexed = gifenc.applyPalette(rgba, palette);
      encoder.writeFrame(indexed, width, height, { palette, delay:interval, repeat:0 });
    }
    encoder.finish();
    const gif = Buffer.from(encoder.bytes());
    const info = await sharp(gif, { animated:true }).metadata();
    if (info.pages < 2) throw new Error('Preview animation is missing frames');
    return { gif, thumbnail, frames:info.pages, width, height, duration:count*interval };
  } finally { clearTimeout(timer); await browser.close(); }
}
