// Run with Electron on macOS after building native/credit-label.
// Uses the real title renderer and layout, without starting Aesel or a session.
const { app, BrowserWindow } = require('electron');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const { execFileSync } = require('node:child_process');
const root = path.resolve(__dirname, '..');
const output = process.env.AESEL_TITLE_TEST_OUTPUT || fs.mkdtempSync(path.join(os.tmpdir(), 'aesel-title-'));
fs.mkdirSync(output, { recursive: true });
app.setPath('userData', path.join(output, 'profile'));
const wait = ms => new Promise(resolve => setTimeout(resolve, ms));
const asset = name => pathToFileURL(path.join(root, name)).href;

const timeout=setTimeout(()=>{console.error('Title smoke timed out');app.exit(1)},45000);
app.whenReady().then(async () => {
  const fixture = path.join(output, 'title.html');
  fs.writeFileSync(fixture, `<!doctype html><meta charset="utf-8">
    ${['style.css', 'qr-label.css', 'donkey.css'].map(name => `<link rel="stylesheet" href="${asset(name)}">`).join('')}
    <style>body{background:#463264}#artifact-shell{position:fixed;left:540px;top:14px;width:200px;height:200px}#conversation{display:block}</style>
    <div id="conversation"><div id="qr-label" tabindex="0"></div><div id="notebook-page"><div id="notebook-content"></div></div></div>
    <div id="artifact-shell"></div>
    <script>window.aesel={renderProxTitle:async()=>window.testGlyphs}</script>
    ${['qr-label.js', 'prox-hover.js', 'notebook-layout.js'].map(name => `<script src="${asset(name)}"></script>`).join('')}
    <script>installProxHover(document.getElementById('qr-label'))</script>`);
  // A visible window lets macOS composite the animated image layers in captures.
  const win = new BrowserWindow({ show: true, width: 760, height: 420,
    webPreferences: { backgroundThrottling: false, contextIsolation: true, sandbox: true } });
  const run = source => win.webContents.executeJavaScript(source);
  await win.loadFile(fixture);
  console.log('Title fixture loaded');
  const setTitle = async text => {
    const glyphs = JSON.parse(execFileSync(path.join(root, 'native/credit-label'), [text, '16', '--glyphs'], { encoding: 'utf8' }));
    await run(`window.testGlyphs=${JSON.stringify(glyphs)};updateQrLabel(${JSON.stringify(text)})`);
    await wait(100);
    await run(`Promise.all([...document.querySelectorAll('#qr-label img')].map(img=>img.decode()))`);
    await wait(100);
    assert.equal(await run(`document.getElementById('qr-label').getAttribute('aria-label')`), text);
  };
  const check = async name => {
    const bounds = await run(`(()=>{
      const title=document.getElementById('qr-label'),view=document.getElementById('conversation'),preview=document.getElementById('artifact-shell');
      const right=preview.hidden?view.clientWidth-14:Math.min(preview.getBoundingClientRect().left-16,view.clientWidth-14);
      const images=[...title.querySelectorAll('img')].map(img=>img.getBoundingClientRect());
      return {left:Math.min(...images.map(r=>r.left)),right:Math.max(...images.map(r=>r.right)),limit:right,count:images.length};
    })()`);
    assert.ok(bounds.count > 0, `${name}: no native glyphs`);
    assert.ok(bounds.left >= 0 && bounds.right <= bounds.limit + .5, `${name}: ${JSON.stringify(bounds)}`);
  };
  const freeze = async time => run(`for(const a of document.getAnimations()){a.pause();a.currentTime=${time}}`);
  const checkPixels = async name => {
    await freeze(450);
    await wait(60);
    const rect = { x: 0, y: 0, width: 530, height: 85 };
    const actual = await win.webContents.capturePage(rect);
    // Removing clipping must not reveal missing ink, including the final shadow.
    await run(`window.clipReference=document.createElement('style');clipReference.textContent='#qr-label,#conversation{overflow:visible!important}';document.head.append(clipReference)`);
    await wait(60);
    const reference = await win.webContents.capturePage(rect);
    await run('clipReference.remove()');
    assert.ok(actual.toBitmap().equals(reference.toBitmap()), `${name}: clipped title pixels`);
    fs.writeFileSync(path.join(output, `${name}.png`), actual.toPNG());
  };

  await setTitle('@jeffrey/aesel');
  console.log('Native glyphs ready');
  await check('short title');
  await checkPixels('title');
  await run(`document.getElementById('qr-label').dispatchEvent(new PointerEvent('pointerenter'))`);
  for (const time of [0, 80, 160, 240, 400, 800]) { await freeze(time); await check(`hover ${time}`); }
  await checkPixels('title-hover');
  await setTitle('@jeffrey/aesel-a-long-piece-name-ending-in-W');
  await run(`document.getElementById('artifact-shell').style.left='240px'`);
  await wait(100);
  await check('preview resized while hovered');
  await run(`document.getElementById('qr-label').dispatchEvent(new PointerEvent('pointerleave'))`);
  for (const time of [0, 160, 400, 800]) { await freeze(time); await check(`leave ${time}`); }
  win.setContentSize(360, 420);
  await run(`document.getElementById('artifact-shell').hidden=true`);
  await wait(100);
  await check('narrow window, hidden preview');
  await win.webContents.debugger.attach('1.3');
  await win.webContents.debugger.sendCommand('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-reduced-motion', value: 'reduce' }] });
  await wait(100);
  await check('reduced motion');
  await setTitle('WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW');
  await check('maximum length');
  await setTitle('aesel');
  await check('short title restored');
  console.log(`Title render, hover, resize and reduced-motion checks passed. Captures: ${output}`);
  clearTimeout(timeout);
  app.exit(0);
}).catch(error => { console.error(error); app.exit(1); });
