import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import vm from 'node:vm';
const script = readFileSync(new URL('../Resources/preview-continuity.js', import.meta.url), 'utf8');
function harness() {
  const observers = [], colors = [], ticks = [], frames = [];
  const pixels = (fn) => Uint8ClampedArray.from(Array.from({length: 64}, (_, i) => fn(i % 8, Math.floor(i / 8))).flat());
  const makeCanvas = () => {
    const canvas = {dataset: {}, style: {}, width: 160, height: 90, setAttribute() {}, remove() { wrapper.children = wrapper.children.filter(c => c !== this); }};
    let source;
    const context = {clearRect() {}, drawImage(value) { source = value; canvas.copied = value; }, fillRect() { canvas.fill = this.fillStyle; },
      getImageData() { if (source.tainted) throw Error('SecurityError'); return {data: source.pixels}; }};
    canvas.getContext = () => context;
    return canvas;
  };
  const wrapper = {children: [], querySelector(selector) {return this.children.find(c => selector === 'canvas:not([data-type])' ? !c.dataset.type && !c.dataset.aeselContinuity : selector.includes(`data-type="${c.dataset.type}"`));}, insertBefore(child, before) {this.children.splice(this.children.indexOf(before), 0, child);}};
  const window = {webkit: {messageHandlers: {previewBackdrop: {postMessage: color => colors.push(color)}}}};
  const properties = {};
  const document = {createElement: makeCanvas, getElementById: () => wrapper,
    documentElement: {style: {setProperty: (name, value) => properties[name] = value}}, head: {append() {}}};
  class MutationObserver {constructor(callback) {this.callback = callback; observers.push(this);} observe() {} disconnect() {}}
  vm.runInNewContext(script, {window, document, MutationObserver,
    getComputedStyle: canvas => ({display: 'block', visibility: 'visible', opacity: '1', ...canvas.style}),
    requestAnimationFrame: fn => frames.push(fn), setInterval: fn => ticks.push(fn)});
  const notify = records => observers.at(-1).callback(records);
  function show(fn) {
    const freeze = makeCanvas(); freeze.dataset.type = 'freeze'; freeze.pixels = pixels(fn);
    freeze.style = {opacity: '1', transition: 'opacity 50ms ease-out'};
    wrapper.children.push(freeze); notify([{target: wrapper, addedNodes: [freeze]}]); return freeze;
  }
  function live(fn) {const canvas = makeCanvas();canvas.pixels = pixels(fn);wrapper.children.push(canvas);return canvas;}
  return {show, live, tick: () => ticks[0](), frame: () => frames.shift()?.(), properties, notify, colors, wrapper, inspect: () => window.__aeselContinuity.inspect(), backdrop: () => wrapper.children.find(c => c.dataset.aeselContinuity)};
}
test('uniform edges fill the resize gaps with artwork color and follow ready-pixel fade', () => {
  const h = harness(), freeze = h.show(() => [128, 0, 128, 255]);
  assert.equal(h.inspect().mode, 'color'); assert.equal(h.backdrop().fill, 'rgb(128,0,128)');
  assert.deepEqual(Array.from(h.colors[0]), [128, 0, 128]);
  assert.equal(h.backdrop().style.pointerEvents, 'none');
  assert.equal(freeze.style.objectPosition, 'right top');
  assert.equal(h.backdrop().style.objectPosition, 'right top');
  freeze.style.opacity = '0'; h.notify([{target: freeze}]);
  assert.equal(h.backdrop().style.opacity, '0');
  // A new resize during fade must immediately restore the continuity layer.
  freeze.style.opacity = '1'; freeze.style.transition = 'none'; h.notify([{target: freeze}]);
  assert.equal(h.backdrop().style.opacity, '1'); assert.equal(h.backdrop().style.transition, 'none');
  freeze.remove(); h.notify([{target: h.wrapper, removedNodes: [freeze]}]);
  assert.equal(h.inspect().active, false); assert.equal(h.backdrop(), undefined);
});
test('varied edges use a covered blurred copy while the foreground retains its aspect', () => {
  const h = harness(), freeze = h.show((x, y) => [x * 32, y * 32, 50, 255]);
  freeze.style.objectFit = 'contain'; h.notify([{target: freeze}]);
  assert.equal(h.inspect().mode, 'blur'); assert.equal(h.backdrop().copied, freeze);
  assert.equal(h.backdrop().style.objectFit, 'cover'); assert.equal(h.backdrop().style.filter, 'blur(12px)');
  assert.equal(freeze.style.objectFit, 'contain');
  assert.equal(freeze.style.objectPosition, 'right top');
  assert.equal(h.backdrop().style.transform, undefined);
  assert.equal(h.backdrop().style.left, '-36px');
});
test('legitimate black artwork stays black; an unreadable canvas still gets a blurred copy', () => {
  const h = harness(), freeze = h.show(() => [0, 0, 0, 255]);
  assert.equal(h.backdrop().fill, 'rgb(0,0,0)');
  freeze.tainted = true; h.notify([{target: freeze}]);
  assert.equal(h.inspect().mode, 'blur'); assert.equal(h.backdrop().copied, freeze);
});

test('initial rendered wipe seeds every backdrop before any resize, then follows new pixels', () => {
  const h = harness(), live = h.live(() => [128, 0, 128, 255]);
  h.frame();
  assert.equal(h.inspect().active, false);
  assert.deepEqual(Array.from(h.inspect().color), [128, 0, 128]);
  assert.equal(h.properties['--aesel-preview-background'], 'rgb(128,0,128)');
  assert.equal(h.colors.length, 1);
  h.tick(); assert.equal(h.colors.length, 1); // No repeated native messages.
  live.pixels.fill(0); h.tick();
  assert.equal(h.colors.length, 1); // Transparent/cleared frames preserve the fill.
  for(let i=0;i<live.pixels.length;i+=4) live.pixels.set([20,40,80,255],i);
  h.tick(); assert.deepEqual(Array.from(h.colors.at(-1)), [20,40,80]);
});
