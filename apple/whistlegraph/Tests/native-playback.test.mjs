import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {runInNewContext} from 'node:vm';

const script = await readFile(new URL('../Resources/Web/native-playback.js', import.meta.url), 'utf8');
function fixture(active = false) {
  class AudioContext {
    state = 'running';
    listeners = [];
    delayed = null;
    addEventListener(_, listener) { this.listeners.push(listener); }
    change(state) { this.state = state; this.listeners.forEach(listener => listener()); }
    resume() {
      if (this.delayed) return new Promise(resolve => { this.delayed.finish = () => { this.change('running'); resolve(); }; });
      this.change('running'); return Promise.resolve();
    }
    suspend() { this.change('suspended'); return Promise.resolve(); }
    close() { this.change('closed'); return Promise.resolve(); }
  }
  const listeners = {}, forwarded = [];
  const window = {AudioContext, webkitAudioContext: AudioContext, __whistlegraphPlaybackActive: active,
    top: {}, parent: {}, frames: [{postMessage: message => forwarded.push(message)}],
    addEventListener: (name, listener) => { listeners[name] = listener; }};
  runInNewContext(script, {window});
  return {window, listeners, forwarded, set: window.whistlegraphSetPlaybackActive};
}

test('leaving silences existing and newly created contexts, including late resume calls', async () => {
  const {window, set} = fixture(true);
  const before = new window.AudioContext();
  set(false);
  assert.equal(before.state, 'suspended');
  await before.resume(); // Speech completion/render callback after returning to feed.
  const after = new window.webkitAudioContext();
  await after.resume();
  assert.equal(before.state, 'suspended');
  assert.equal(after.state, 'suspended');
  set(true);
  assert.equal(before.state, 'running');
  assert.equal(after.state, 'running');
});

test('a resume already in flight cannot reopen audio after exit', async () => {
  const {window, set} = fixture(true);
  const context = new window.AudioContext();
  await context.suspend();
  context.delayed = {};
  const resume = context.resume();
  set(false);
  context.delayed.finish();
  await resume;
  assert.equal(context.state, 'suspended');
});

test('cold hidden frames are silent and deliberate application pauses survive navigation', async () => {
  const {window, set} = fixture();
  const first = new window.AudioContext();
  assert.equal(first.state, 'suspended');
  await first.resume();
  set(true);
  assert.equal(first.state, 'running');
  await first.suspend();
  set(false); set(true);
  assert.equal(first.state, 'suspended');
  await first.close();
  set(false); set(true);
  assert.equal(first.state, 'closed');
});

test('only the parent frame can relay playback ownership; children inherit it', () => {
  const {window, listeners, forwarded} = fixture();
  const context = new window.AudioContext();
  listeners.message({source: {}, data: {type: 'whistlegraph:playback', active: true}});
  assert.equal(context.state, 'suspended');
  listeners.message({source: window.parent, data: {type: 'whistlegraph:playback', active: true}});
  assert.equal(context.state, 'running');
  assert.equal(forwarded.at(-1).active, true);
  listeners.message({source: window.parent, data: {type: 'whistlegraph:playback', active: false}});
  assert.equal(context.state, 'suspended');
});
