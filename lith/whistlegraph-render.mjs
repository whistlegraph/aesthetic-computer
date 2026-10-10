// Pixels for turns that run off the phone (apple/whistlegraph/TURNS.md, slice 3).
//
// A pool of warm headless Chrome tabs, each holding the same AC runtime the
// phone paints with (/wipe?…preview=walkieware). A render hands a tab a
// candidate's source, waits for the runtime's own painted/invalidated event
// for that exact source hash, and takes four frames over two seconds — the
// evidence shape the picture review already expects. The same tab also
// rasterizes chalk strokes to the PNG the model sees.
//
// This is the proven renderer; a graph.mjs worker-thread isolate is the
// lighter one and can replace it per piece later without touching callers.
import puppeteer from 'puppeteer';
import {existsSync} from 'node:fs';
import {normalizeDrawing} from '../apple/whistlegraph/Resources/Web/drawing-input.mjs';

const PREVIEW = '/wipe?noauth=true&noplot=true&nogap=true&nolabel=true&preview=walkieware';
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const sleep = ms => new Promise(r => setTimeout(r, ms));
const PAGE_SCRIPT = `(() => {
  window.acFORCE_NOGAP = true; window.acPACK_DENSITY = 2; window.acAutoDensityOverride = true;
  const state = window.__wg = {ready: false, events: [], sessionID: 'wg-worker', revision: 0};
  window.addEventListener('aesel-preview', e => { const d = e.detail; if (!d || d.sessionID !== state.sessionID || d.revision !== state.revision) return; state.events.push({...d, atMs: performance.now()}); });
  window.__wgRender = async (source, renderID) => {
    const current = ++state.revision; state.events = [];
    const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(source));
    const hash = [...new Uint8Array(digest)].map(b => b.toString(16).padStart(2, '0')).join('');
    window.AC?.startAudio?.();
    window.acSEND({type: 'dropped:piece', content: {name: 'whistlegraph-preview', source, search: 'noauth=true&noplot=true&nogap=true&nolabel=true', isKidLisp: false,
      aeselPreview: {sessionID: state.sessionID, revision: current, sourceHash: hash, requestID: renderID}}});
    return hash;
  };
  const poll = setInterval(() => { if (window.preloaded && window.acSEND) { clearInterval(poll); state.ready = true; } }, 100);
})();`;

export class RenderPool {
  #browser = null; #tabs = []; #waiting = []; #site; #size; #viewport; #frameSize; #log;
  // The runtime paints at the phone's proportions; the frames the reviewer
  // sees are scaled to fit its 384-pixel limit (deviceScaleFactor < 1).
  constructor({site = 'https://aesthetic.computer', size = 2, viewport = {width: 390, height: 520}, log = () => {}} = {}) {
    const scale = Math.min(1, 384 / Math.max(viewport.width, viewport.height));
    this.#site = site; this.#size = size; this.#log = log;
    this.#viewport = {...viewport, deviceScaleFactor: scale};
    this.#frameSize = {width: Math.round(viewport.width * scale), height: Math.round(viewport.height * scale)};
  }
  async start() {
    let shell = null; try { shell = puppeteer.executablePath({headless: 'shell'}); } catch {}
    const executablePath = process.env.CHROME_PATH || (shell && existsSync(shell) ? shell : existsSync(CHROME) ? CHROME : undefined);
    this.#browser = await puppeteer.launch({headless: shell && !process.env.CHROME_PATH ? 'shell' : true, executablePath,
      // Every tab must keep painting while others are in front: no
      // background throttling, or a pool of three paints only one.
      args: ['--no-sandbox', '--disable-dev-shm-usage', '--autoplay-policy=no-user-gesture-required', '--mute-audio',
        '--disable-background-timer-throttling', '--disable-renderer-backgrounding', '--disable-backgrounding-occluded-windows', '--disable-features=CalculateNativeWinOcclusion',
        `--window-size=${this.#viewport.width},${this.#viewport.height}`]});
    for (let i = 0; i < this.#size; i++) this.#tabs.push(await this.#open(i));
    this.#log('render pool ready', `${this.#size} tabs`, executablePath || 'bundled chrome');
  }
  async #open(index) {
    // Each tab in its own context: its own window, never a background tab of another.
    const context = await this.#browser.createBrowserContext();
    const page = await context.newPage();
    await page.setViewport(this.#viewport);
    await page.evaluateOnNewDocument(PAGE_SCRIPT);
    await page.goto(this.#site + PREVIEW, {waitUntil: 'domcontentloaded', timeout: 90_000});
    return {index, page, busy: false, renders: 0};
  }
  async #acquire() {
    for (;;) {
      const tab = this.#tabs.find(t => !t.busy);
      if (tab) { tab.busy = true; return tab; }
      await new Promise(r => this.#waiting.push(r));
    }
  }
  async #release(tab, broken = false) {
    try {
      if (broken || ++tab.renders >= 40) { await tab.page.browserContext().close().catch(() => {}); const fresh = await this.#open(tab.index); Object.assign(tab, fresh, {renders: 0}); }
    } catch (error) { this.#log('tab recycle failed', error.message); }
    tab.busy = false; this.#waiting.shift()?.();
  }
  // {rendered, sourceHash, renderID, logs, frames, error} — frames only when it painted.
  async render({source, renderID}, {timeoutMs = 20_000, spanMs = 2200, frames = 4} = {}) {
    const tab = await this.#acquire(); let broken = false;
    try {
      await tab.page.waitForFunction('window.__wg && window.__wg.ready', {timeout: 90_000});
      const hash = await tab.page.evaluate((s, id) => window.__wgRender(s, id), source, renderID);
      const logs = []; let painted = false, invalidated = false;
      const drain = async () => { for (const e of await tab.page.evaluate(() => window.__wg.events.splice(0))) {
        if (e.sourceHash !== hash) continue;
        if (e.kind === 'painted') painted = true;
        if (e.kind === 'invalidated') invalidated = true;
        if (e.kind === 'console') logs.push({level: e.event?.level || 'log', text: String(e.event?.message || '').slice(0, 500)});
      } };
      const started = Date.now();
      while (Date.now() - started < timeoutMs) { await drain(); if (painted || invalidated) break; await sleep(100); }
      const out = {rendered: painted && !invalidated, sourceHash: hash, renderID, logs, frames: []};
      if (!painted && !invalidated) out.error = `Nothing painted within ${timeoutMs / 1000} s`;
      if (out.rendered) {
        const first = Date.now();
        for (let i = 0; i < frames; i++) {
          const at = Math.round(i * spanMs / (frames - 1));
          while (Date.now() - first < at) await sleep(20);
          const png = await tab.page.screenshot({encoding: 'base64', type: 'png'});
          out.frames.push({png, atMs: Date.now() - first, width: this.#frameSize.width, height: this.#frameSize.height});
        }
        await drain(); if (invalidated) { out.rendered = false; out.error = 'The piece broke while being watched'; }
      }
      return out;
    } catch (error) { broken = true; return {rendered: false, sourceHash: null, renderID, logs: [], frames: [], error: error.message}; }
    finally { await this.#release(tab, broken); }
  }
  // The chalk as the model sees it: the app's drawingImage, run in a tab's canvas.
  async chalkImage(drawing) {
    const value = normalizeDrawing(drawing);
    const tab = await this.#acquire();
    try {
      const data = await tab.page.evaluate(drawing => {
        const canvas = document.createElement('canvas'), side = 768;
        canvas.width = Math.round(side * Math.min(1, drawing.aspect)); canvas.height = Math.round(side / Math.max(1, drawing.aspect));
        const ctx = canvas.getContext('2d');
        ctx.fillStyle = '#ffffff'; ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.strokeStyle = ctx.fillStyle = '#202020'; ctx.lineWidth = 3; ctx.lineCap = ctx.lineJoin = 'round';
        const point = p => [p[0] / 1000 * canvas.width, p[1] / 1000 * canvas.height];
        for (const stroke of drawing.strokes) {
          ctx.beginPath();
          if (stroke.every(p => p[0] === stroke[0][0] && p[1] === stroke[0][1])) { const [x, y] = point(stroke[0]); ctx.arc(x, y, 1.5, 0, Math.PI * 2); ctx.fill(); }
          else { stroke.forEach((p, i) => ctx[i ? 'lineTo' : 'moveTo'](...point(p))); ctx.stroke(); }
        }
        return canvas.toDataURL('image/png').split(',')[1];
      }, value);
      if (!data || data.length > 700_000) throw Error('Could not encode chalk image');
      return {type: 'image', source: {type: 'base64', media_type: 'image/png', data}};
    } finally { await this.#release(tab); }
  }
  async close() { await this.#browser?.close().catch(() => {}); this.#browser = null; this.#tabs = []; }
}
