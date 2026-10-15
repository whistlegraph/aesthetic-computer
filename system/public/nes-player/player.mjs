import { inspectROM, MAX_ROM } from './rom.mjs';
import { observeInteraction } from '../aesthetic.computer/lib/frame-interaction.mjs';

observeInteraction();
const params = new URLSearchParams(location.search);
const $ = id => document.getElementById(id);
const canvas = document.querySelector('canvas'), context = canvas.getContext('2d', { alpha: false });
const picture = context.createImageData(256, 240);
const keys = { KeyX: 0, KeyZ: 1, ShiftLeft: 2, ShiftRight: 2, ControlRight: 2, Enter: 3, ArrowUp: 4, ArrowDown: 5, ArrowLeft: 6, ArrowRight: 7 };
const sources = new Map(), buttons = [...document.querySelectorAll('[data-key]')];
let nes, rom, playing = params.get('warm') !== '1', failed = false, frame = 0, previous = 0, debt = 0;
let audio, output, audioPromise, sound = false, audioRequested = false, samples = [], padPulse = 0;
const active = () => !!nes && playing && !document.hidden && !failed;
const report = phase => parent.postMessage({ type: 'mime:interaction', phase }, '*');

function ready() {
  window.__mimeReady = true;
  parent.postMessage({ type: 'mime:ready' }, '*');
}
function soundLabel() {
  const on = sound && audio?.state === 'running';
  $('sound').textContent = on ? 'Sound on' : 'Sound off';
  $('sound').setAttribute('aria-pressed', String(on));
}
async function enableSound() {
  if (!active()) return;
  sound = true; audioRequested = true;
  try {
    if (!audio) {
      audio = new AudioContext({ sampleRate: 44100, latencyHint: 'interactive' });
      nes.opts.sampleRate = nes.papu.sampleRate = audio.sampleRate; nes.setFramerate(60);
      audio.onstatechange = soundLabel;
      audioPromise = audio.audioWorklet.addModule(new URL('./audio-worklet.mjs', import.meta.url)).then(() => {
        output = new AudioWorkletNode(audio, 'mime-nes-audio', { outputChannelCount: [2] });
        output.connect(audio.destination);
      });
    }
    // Resume during the gesture, before awaiting the worklet module on iOS.
    const resumed = audio.resume();
    await Promise.all([resumed, audioPromise]);
    if (!active() || !sound) await audio.suspend();
  } catch {
    sound = false; clearAudio();
    const failedAudio = audio; audio = output = audioPromise = null;
    await failedAudio?.close().catch(() => {});
  }
  soundLabel();
}
function clearAudio() { samples = []; output?.port.postMessage('clear'); audio?.suspend().catch(() => {}); }
function setSource(source, pressed) {
  const before = new Set([...sources.values()].flat());
  if (pressed.length) sources.set(source, pressed); else sources.delete(source);
  const after = new Set([...sources.values()].flat());
  for (let key = 0; key < 8; key++) {
    if (before.has(key) !== after.has(key)) nes?.[after.has(key) ? 'buttonDown' : 'buttonUp'](1, key);
  }
  for (const button of buttons) button.toggleAttribute('data-down', after.has(Number(button.dataset.key)));
}
function release() {
  for (const source of [...sources.keys()]) setSource(source, []);
  report('release');
}
function gamepads(now) {
  let pads;
  try { pads = navigator.getGamepads?.() || []; } catch { return; }
  const pad = [...pads].find(p => p?.connected && p.mapping === 'standard');
  const down = [];
  if (pad) {
    for (const [button, key] of [[0,0], [1,1], [8,2], [9,3], [12,4], [13,5], [14,6], [15,7]]) if (pad.buttons[button]?.pressed) down.push(key);
    if (pad.axes[0] < -.5) down.push(6); if (pad.axes[0] > .5) down.push(7);
    if (pad.axes[1] < -.5) down.push(4); if (pad.axes[1] > .5) down.push(5);
  }
  setSource('gamepad', down);
  if (down.length && now - padPulse > 150) { padPulse = now; report('pulse'); }
}
function draw(buffer) {
  for (let i = 0; i < buffer.length; i++) {
    const color = buffer[i], offset = i * 4;
    picture.data[offset] = color & 255; picture.data[offset + 1] = (color >> 8) & 255;
    picture.data[offset + 2] = (color >> 16) & 255; picture.data[offset + 3] = 255;
  }
  context.putImageData(picture, 0, 0);
}
function tick(now) {
  frame = 0;
  if (!active()) return;
  gamepads(now);
  debt = Math.min(1000 / 15, debt + (previous ? now - previous : 1000 / 60)); previous = now;
  try {
    while (debt >= 1000 / 60) { nes.frame(); debt -= 1000 / 60; }
    if (samples.length) {
      const block = new Float32Array(samples); samples = [];
      output?.port.postMessage(block, [block.buffer]);
    }
  } catch { fail('This ROM could not keep running. Try Restart.'); return; }
  frame = requestAnimationFrame(tick);
}
function settle(value) {
  playing = value === true;
  if (!active()) { cancelAnimationFrame(frame); frame = 0; previous = debt = 0; release(); clearAudio(); }
  else if (!frame) {
    previous = debt = 0; frame = requestAnimationFrame(tick);
    if (sound) audio?.resume().catch(() => {});
  }
}
window.__mimeSettle = window.__mimePlayerSettle = settle;
addEventListener('message', event => {
  if (event.source === parent && event.data?.type === 'mime:playing') settle(event.data.playing);
});
document.addEventListener('visibilitychange', () => settle(playing));
addEventListener('blur', release);
addEventListener('pagehide', () => { settle(false); audio?.close().catch(() => {}); });

for (const button of buttons) {
  button.addEventListener('pointerdown', event => {
    if (!active() || event.button !== 0) return;
    event.preventDefault(); button.setPointerCapture(event.pointerId);
    canvas.focus({ preventScroll: true });
    setSource('pointer:' + event.pointerId, [Number(button.dataset.key)]);
    if (!audioRequested) enableSound();
  });
  for (const type of ['pointerup', 'pointercancel', 'lostpointercapture']) button.addEventListener(type, event => setSource('pointer:' + event.pointerId, []));
  button.addEventListener('click', event => {
    if (event.detail || !active()) return;
    const source = 'accessible:' + button.dataset.key;
    setSource(source, [Number(button.dataset.key)]);
    setTimeout(() => setSource(source, []), 100); if (!audioRequested) enableSound();
  });
}
addEventListener('keydown', event => {
  if (event.target.closest('button') && ['Enter', 'Space'].includes(event.code)) return;
  if (!(event.code in keys) || event.target.closest('input,textarea,select') || event.altKey || event.metaKey || !active()) return;
  event.preventDefault(); setSource('key:' + event.code, [keys[event.code]]);
  if (!audioRequested) enableSound();
});
addEventListener('keyup', event => {
  if (!(event.code in keys)) return;
  setSource('key:' + event.code, []);
});
canvas.addEventListener('pointerdown', () => { canvas.focus(); if (!audioRequested) enableSound(); });
$('sound').onclick = () => { if (sound) { sound = false; clearAudio(); soundLabel(); } else enableSound(); };
$('restart').onclick = () => { if (rom) load(rom); };

function fail(message) {
  failed = true; settle(playing);
  $('notice').textContent = message; $('notice').hidden = false;
  $('retry').hidden = !params.has('file'); ready();
}
function load(bytes) {
  cancelAnimationFrame(frame); frame = 0; release(); clearAudio(); nes = null; failed = false;
  try {
    const info = inspectROM(bytes);
    if (!info.supported) throw new Error(info.console ? 'This NES console variant is not supported yet.' : `NES mapper ${info.mapper} is not supported yet.`);
    rom = bytes;
    nes = new jsnes.NES({ onFrame: draw, sampleRate: audio?.sampleRate || 44100,
      onAudioSample(left, right) { if (sound && output && audio.state === 'running' && active()) samples.push(left, right); } });
    nes.loadROM(bytes); nes.frame(); nes.frame();
    $('notice').hidden = true; $('retry').hidden = true;
    for (const id of ['screen', 'controls', 'tools', 'hint']) $(id).hidden = false;
    $('chooser').hidden = true; ready(); settle(playing);
  } catch (error) { fail(error.message); }
}
$('rom').onchange = async event => {
  const file = event.target.files[0]; if (!file) return;
  if (file.size > MAX_ROM) return fail('ROM over 8 MB.');
  load(new Uint8Array(await file.arrayBuffer()));
};
async function fetchROM(value) {
  const url = new URL(value, location.href);
  if (!['https://mime.ac', 'https://aesthetic.computer', location.origin].includes(url.origin) ||
      !/^\/api\/mime(?:chan)?$/.test(url.pathname) || !url.searchParams.has('file') || url.username || url.password)
    throw new Error('Open a ROM uploaded to MIME.');
  const response = await fetch(url, { credentials: 'omit', redirect: 'error', signal: AbortSignal.timeout(20000) });
  if (!response.ok) throw new Error('Could not load this ROM.');
  if (Number(response.headers.get('content-length')) > MAX_ROM) throw new Error('ROM over 8 MB.');
  const reader = response.body.getReader(), chunks = []; let size = 0;
  try {
    while (true) {
      const { value, done } = await reader.read(); if (done) break;
      size += value.length; if (size > MAX_ROM) throw new Error('ROM over 8 MB.');
      chunks.push(value);
    }
  } finally { await reader.cancel(); }
  const bytes = new Uint8Array(size); let offset = 0;
  for (const chunk of chunks) { bytes.set(chunk, offset); offset += chunk.length; }
  return bytes;
}
async function openPostedROM() {
  $('retry').disabled = true; $('notice').textContent = 'Loading ROM…';
  try { load(await fetchROM(params.get('file'))); }
  catch (error) { fail(error.message); }
  finally { $('retry').disabled = false; }
}
$('retry').onclick = openPostedROM;
if (params.has('file')) openPostedROM();
else { $('notice').hidden = true; $('chooser').hidden = false; ready(); }
