// Exercise a prompt-hosted entry, stereo WebGL rendering and return to the
// same live Rax session. Only headset tracking and the compositor are faked.
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';

const app = createRaxServer({ guests: true });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = 'http://127.0.0.1:' + app.server.address().port;
const browser = await chromium.launch({ channel: 'chrome', headless: true });
try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', msg => { if (msg.type() === 'error' && msg.text().includes('rax frame')) errors.push(msg.text()); });
  await page.addInitScript(() => {
    const matrix = x => new Float32Array([1,0,0,0, 0,1,0,0, 0,0,1,0, x,0,0,1]);
    const transform = x => ({ matrix: matrix(x), inverse: { matrix: matrix(-x) } });
    const xr = new EventTarget();
    let declined = false;
    xr.isSessionSupported = async () => true;
    xr.requestSession = async () => {
      if (!declined) { declined = true; throw new DOMException('Denied', 'NotAllowedError'); }
      const session = new EventTarget();
      session.visibilityState = 'visible';
      session.inputSources = [{ handedness: 'left', gamepad: { mapping: 'xr-standard', axes: [0,0,0,-0.7] } }];
      session.requestReferenceSpace = async () => new EventTarget();
      session.updateRenderState = () => {};
      const views = [-0.032, 0.032].map(x => ({
        eye: x < 0 ? 'left' : 'right', transform: transform(x),
        projectionMatrix: new Float32Array([1.1,0,0,0, 0,1.1,0,0, Math.sign(x)*0.08,0,-1,-1, 0,0,-0.1,0]),
      }));
      let token;
      session.requestAnimationFrame = cb => { token = requestAnimationFrame(now => {
        cb(now, { getViewerPose: () => ({ transform: transform(0), views }) });
        const { gl, data } = window.xrTarget;
        gl.readPixels(0, 0, 256, 96, gl.RGBA, gl.UNSIGNED_BYTE, data);
        const colors = [new Set(), new Set()];
        let disparity = 0;
        const pixels = new Uint32Array(data.buffer);
        for (let i = 0; i < pixels.length; i++) {
          colors[Number(i % 256 >= 128)].add(pixels[i]);
          if (i % 256 < 128 && pixels[i] !== pixels[i+128]) disparity++;
        }
        window.stereo = { colors: colors.map(s => s.size), disparity, error: gl.getError() };
      }); };
      session.end = async () => { cancelAnimationFrame(token); session.dispatchEvent(new Event('end')); };
      return window.testSession = session;
    };
    Object.defineProperty(navigator, 'xr', { value: xr });
    WebGL2RenderingContext.prototype.makeXRCompatible = async () => {};
    window.XRWebGLLayer = class {
      constructor(session, gl) {
        this.framebuffer = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffer);
        for (const [format, target] of [[gl.RGBA8, gl.COLOR_ATTACHMENT0], [gl.DEPTH_COMPONENT16, gl.DEPTH_ATTACHMENT]]) {
          gl.bindRenderbuffer(gl.RENDERBUFFER, gl.createRenderbuffer());
          gl.renderbufferStorage(gl.RENDERBUFFER, format, 256, 96);
          gl.framebufferRenderbuffer(gl.FRAMEBUFFER, target, gl.RENDERBUFFER, gl.getParameter(gl.RENDERBUFFER_BINDING));
        }
        window.xrTarget = { gl, data: new Uint8Array(256 * 96 * 4) };
      }
      getViewport(v) { return { x: v.eye === 'left' ? 0 : 128, y: 0, width: 128, height: 96 }; }
    };
  });
  await page.route('**/test-host', route => route.fulfill({ contentType: 'text/html', body: '<iframe width="800" height="600" allow="xr-spatial-tracking" src="/?embed=prompt"></iframe>' }));
  await page.goto(base + '/test-host');
  const child = page.frames().find(f => f !== page.mainFrame());
  await child.waitForFunction(() => document.querySelector('canvas')?.raxStats);
  const goggles = child.getByRole('button', { name: 'Enter VR' });
  assert.equal(await goggles.count(), 0);
  const sheet = data => page.evaluate(data => document.querySelector('iframe').contentWindow.postMessage({ type: 'rax:sheet', ...data }, location.origin), data);
  await sheet({ open: true }); await sheet({ phase: 'opened' });
  await child.waitForFunction(() => document.querySelector('#game').raxStats.role === 'player');
  await goggles.click();
  await child.getByText('VR was declined.', {exact:false}).waitFor();
  await goggles.click();
  await child.waitForFunction(() => stereo?.colors.every(n => n > 6) && stereo.disparity > 100);
  const stereo = await child.evaluate(() => window.stereo);
  assert.equal(stereo.error, 0);
  assert.equal(await child.evaluate(() => document.querySelector('#game').raxStats.backend), 'webxr');
  await child.evaluate(() => testSession.end());
  await goggles.waitFor();
  await child.waitForFunction(() => document.querySelector('#game').raxStats.backend !== 'webxr');
  await sheet({ phase: 'closing' });
  await goggles.waitFor({ state: 'hidden' });
  assert.deepEqual(errors, []);
  console.log('Prompt entry, declined permission, stereo, and return passed.', stereo);
} finally { await browser.close(); await app.close(); }
