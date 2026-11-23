// Exercise a prompt-hosted entry, stereo WebGL rendering and return to the
// same live Rax session. Only headset tracking and the compositor are faked.
import assert from 'node:assert/strict';
import { setTimeout as delay } from 'node:timers/promises';
import { kioskPlan } from '../kiosks-plan.mjs';
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
  const errors = [], presses = [];
  page.on('websocket', ws => ws.on('framesent', ({payload}) => {
    const msg = JSON.parse(payload);
    if (msg.type === 'panel') presses.push(msg);
  }));
  async function waitForPress(name, count = 1) {
    const end = Date.now()+5000;
    while (presses.filter(p => p.id === 'yard-0' && p.name === name).length < count) {
      if (Date.now()>end) throw Error('Missing kiosk event ' + name + ' ' + JSON.stringify(presses));
      await delay(25);
    }
  }
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', msg => { if (msg.type() === 'error' && msg.text().includes('rax frame')) errors.push(msg.text()); });
  await page.addInitScript(() => {
    const matrix = x => new Float32Array([1,0,0,0, 0,1,0,0, 0,0,1,0, x,0,0,1]);
    const transform = x => ({ matrix: matrix(x), inverse: { matrix: matrix(-x) } });
    const xr = new EventTarget();
    let declined = false;
    xr.isSessionSupported = async () => true;
    xr.requestSession = async (mode, options) => {
      window.testFeatures = options.optionalFeatures;
      window.testRequired = options.requiredFeatures;
      if (window.noBoundary && options.requiredFeatures.includes('bounded-floor')) {
        throw new DOMException('No bounded tracking', 'NotSupportedError');
      }
      if (!declined) { declined = true; throw new DOMException('Denied', 'NotAllowedError'); }
      const session = new EventTarget();
      session.visibilityState = 'visible';
      session.environmentBlendMode = mode === 'immersive-ar' ? 'alpha-blend' : 'opaque';
      session.inputSources = [{ handedness: 'left', gamepad: { mapping: 'xr-standard', axes: [0,0,0,-0.7] } }];
      session.requestReferenceSpace = async type => {
        window.testSpaceType = type;
        return window.testSpace = Object.assign(new EventTarget(), {boundsGeometry: []});
      };
      session.updateRenderState = ({baseLayer}) => { window.testFoveation = baseLayer.fixedFoveation; };
      const views = [-0.032, 0.032].map(x => ({
        eye: x < 0 ? 'left' : 'right', transform: transform(x),
        projectionMatrix: new Float32Array([1.1,0,0,0, 0,1.1,0,0, Math.sign(x)*0.08,0,-1,-1, 0,0,-0.1,0]),
      }));
      let token;
      session.requestAnimationFrame = cb => { token = requestAnimationFrame(now => {
        cb(now, { getViewerPose: space => {
          if (space !== window.testSpace) throw Error('Viewer tracked in the wrong space');
          return { transform: transform(0), views };
        },
          getJointPose: joint => ({transform:{position: joint}, radius: 0.01}),
          getPose: space => ({transform:{position: space, matrix: matrix(0)}}) });
        const { gl, data } = window.xrTarget;
        gl.readPixels(0, 0, 256, 96, gl.RGBA, gl.UNSIGNED_BYTE, data);
        const colors = [new Set(), new Set()];
        let disparity = 0;
        const pixels = new Uint32Array(data.buffer);
        for (let i = 0; i < pixels.length; i++) {
          colors[Number(i % 256 >= 128)].add(pixels[i]);
          if (i % 256 < 128 && pixels[i] !== pixels[i+128]) disparity++;
        }
        window.stereo = { alpha: [...new Set(pixels.map(p => p >>> 24))], colors: colors.map(s => s.size), disparity, error: gl.getError() };
      }); };
      session.end = async () => { cancelAnimationFrame(token); session.dispatchEvent(new Event('end')); };
      return window.testSession = session;
    };
    Object.defineProperty(navigator, 'xr', { value: xr });
    WebGL2RenderingContext.prototype.makeXRCompatible = async () => {};
    window.XRWebGLLayer = class {
      fixedFoveation = 0;
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
  await page.route('**/test-host', route => route.fulfill({ contentType: 'text/html', body: '<iframe width="800" height="600" allow="xr-spatial-tracking" src="/?embed=prompt&debug"></iframe>' }));
  await page.goto(base + '/test-host');
  const child = page.frames().find(f => f !== page.mainFrame());
  await child.waitForFunction(() => document.querySelector('canvas')?.raxStats);
  const controls = () => child.evaluate(() => document.querySelector('#game').raxStats.headsetRects);
  const clickMode = async mode => {
    await child.waitForFunction(mode => document.querySelector('#game').raxStats.headsetRects.some(r => r.mode === mode), mode);
    const at = await child.evaluate(mode => {
      const c = document.querySelector('#game'), r = c.raxStats.headsetRects.find(r => r.mode === mode);
      return {x:(r.x+r.w/2)*c.clientWidth/c.width, y:(r.y+r.h/2)*c.clientHeight/c.height};
    }, mode);
    await child.locator('#game').click({position:at});
  };
  assert.deepEqual(await controls(), []);
  assert.equal(await child.locator('button').count(), 0);
  const sheet = data => page.evaluate(data => document.querySelector('iframe').contentWindow.postMessage({ type: 'rax:sheet', ...data }, location.origin), data);
  await sheet({ open: true }); await sheet({ phase: 'opened' });
  await child.waitForFunction(() => window.raxDebug?.().player && raxDebug().panels.includes('yard-0'));
  await child.waitForFunction(() => document.querySelector('#game').raxStats.headsetRects.length === 2);
  if (process.env.RAX_XR_CAPTURE) await child.locator('#game').screenshot({path:process.env.RAX_XR_CAPTURE});
  await clickMode('immersive-vr');
  await child.waitForFunction(() => document.querySelector('#game').raxStats.headsetMessage.includes('declined'));
  const start = await child.evaluate(() => raxDebug().player);
  await clickMode('immersive-vr');
  await child.waitForFunction(() => window.stereo?.colors.every(n => n > 6) && stereo.disparity > 100);
  const stereo = await child.evaluate(() => window.stereo);
  assert.equal(stereo.error, 0);
  assert.equal(await child.evaluate(() => testFoveation), 0.5);
  assert.deepEqual(stereo.alpha, [255]);
  assert.equal(await child.evaluate(() => testSpaceType), 'local');
  await child.waitForFunction(start => {
    const p = raxDebug().player;
    return Math.hypot(p.x-start.x, p.z-start.z)>0.15;
  }, start);
  const standing = await child.evaluate(() => {
    const thumb = {x:-0.15, y:-0.11, z:-0.22}, middle = {...thumb, x:-0.14};
    testSession.inputSources = [{handedness:'left', hand:new Map([['thumb-tip', thumb], ['middle-finger-tip', middle]])}];
    return raxDebug().player;
  });
  await child.waitForFunction(p => Math.hypot(raxDebug().player.x-p.x, raxDebug().player.z-p.z)>1, standing);
  await child.evaluate(() => testSession.end());

  // Put the seat within fingertip reach of a real AC kiosk and face it.
  const k = kioskPlan('lobby')[0], body = app.rooms.get('lobby').match.seats.find(Boolean).player;
  Object.assign(body, {x:k.x, y:1.7, z:k.z-0.65, yaw:0, pitch:0, vx:0, vz:0});
  body.life++;
  await child.waitForFunction(k => Math.abs(raxDebug().player.x-k.x)<0.05, k);
  await child.evaluate(() => raxLook({yaw:0,pitch:0}));
  await child.evaluate(() => { window.noBoundary = true; });
  await clickMode('immersive-ar');
  await child.waitForFunction(() => document.querySelector('#game').raxStats.headsetMessage.includes('Roomscale'));
  assert.deepEqual(await child.evaluate(() => testRequired), ['bounded-floor']);
  await child.evaluate(() => { window.noBoundary = false; });
  await clickMode('immersive-ar');
  await child.waitForFunction(() => stereo.alpha.length === 1 && stereo.alpha[0] === 0);
  assert.equal(await child.evaluate(() => testSpaceType), 'bounded-floor');
  await child.evaluate(() => {
    testSpace.boundsGeometry = [[-1,-1], [-1,1], [1,1], [1,-1]].map(([x,z]) => ({ x, y:0, z }));
    const tip = window.tip = {x:0, y:-0.25, z:-0.65};
    testSession.inputSources = [{handedness:'right', hand:new Map([['index-finger-tip',tip]])}];
  });
  await waitForPress('touch');
  assert.deepEqual(await child.evaluate(() => testFeatures), ['hand-tracking']);
  await child.waitForFunction(() => stereo.alpha.some(a => a>=167 && a<=169));
  await child.evaluate(() => {tip.x = 0.12;});
  await waitForPress('draw');
  await child.evaluate(() => {testSession.inputSources=[];});
  await waitForPress('lift');
  await child.evaluate(() => {
    testSession.inputSources = [{handedness:'right', targetRaySpace:{x:0,y:-0.25,z:-0.1},
      gamepad:{mapping:'xr-standard',axes:[0,0,0,0],buttons:[{pressed:true}]}}];
  });
  await waitForPress('touch',2);
  await child.evaluate(() => { testSpace.boundsGeometry = []; });
  await waitForPress('lift',2);
  await child.waitForFunction(() => document.querySelector('#game').raxStats.headsetMessage.includes('Roomscale'));
  await sheet({ phase: 'closing' });
  await child.waitForFunction(() => document.querySelector('#game').raxStats.headsetRects.length === 0);
  assert.deepEqual(errors, []);
  console.log('Canvas entry, stereo VR, stick and pinch movement, room boundary setup, loss and retry, mixed-reality alpha, fingertip drag, controller press and release passed.', stereo);
} finally { await browser.close(); await app.close(); }
