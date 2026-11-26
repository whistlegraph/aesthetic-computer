import { Worker } from 'node:worker_threads';

const id = 'yard-notepat';
export function createCommunalNotepat({ kiosks, send, sendText }) {
  const viewers = new Set(), players = new Map(), keyCounts = new Map();
  let worker, frame, sound, nextPointer = 1, flushed, closing = false, resetAt = -Infinity;
  const broadcast = msg => {
    const text = JSON.stringify(msg);
    for (const ws of viewers) sendText(ws, text);
  };
  function start() {
    if (worker || closing) return;
    const kept = kiosks.of('lobby')[id] || {};
    const instance = new Worker(new URL('./notepat-worker.mjs', import.meta.url), {
      workerData: { settings: kept.settings, events: kept.events },
    });
    worker = instance;
    instance.on('message', msg => {
      if (worker !== instance) return;
      if (msg.type === 'settings') return kiosks.settings('lobby', id, msg.settings);
      if (msg.type === 'flushed') return flushed?.();
      if (msg.type === 'notepat-frame') frame = msg;
      else if (msg.type === 'notepat-sound') sound = msg;
      else return;
      broadcast(msg);
    });
    instance.on('error', err => console.error('Rax Notepat:', err.message));
    instance.on('exit', () => {
      if (worker !== instance) return;
      worker = frame = sound = null;
      players.clear(); keyCounts.clear();
      broadcast({ type: 'notepat-sound', voices: [], at: Date.now() });
      flushed?.();
    });
    instance.postMessage({ type: 'watch', active: viewers.size > 0 });
  }
  function hands(ws) {
    if (!players.has(ws)) players.set(ws, { keys: new Map(), pointers: new Map() });
    return players.get(ws);
  }
  function keyUp(held, code) {
    const e = held.keys.get(code);
    if (!e) return;
    held.keys.delete(code);
    const n = keyCounts.get(e.key) - 1;
    if (n) keyCounts.set(e.key, n);
    else {
      keyCounts.delete(e.key);
      worker?.postMessage({ ...e, type: 'key', way: 'up' });
    }
  }
  function release(ws) {
    const held = players.get(ws);
    if (!held) return;
    for (const code of held.keys.keys()) keyUp(held, code);
    for (const pointer of held.pointers.values())
      worker?.postMessage({ type: 'pointer', pointer, name: 'lift', x: -1, y: -1 });
    players.delete(ws);
  }
  return {
    watch(ws) {
      if (worker && viewers.has(ws)) return;
      const first = !viewers.size;
      viewers.add(ws);
      if (!worker) start();
      else if (first) worker.postMessage({ type: 'watch', active: true });
      if (frame) send(ws, frame);
      if (sound) send(ws, { ...sound, at: Date.now() });
    },
    release,
    leave(ws) {
      release(ws);
      if (viewers.delete(ws) && !viewers.size) worker?.postMessage({ type: 'watch', active: false });
    },
    key(ws, e) {
      if (!viewers.has(ws) || typeof e.code !== 'string' || !/^[A-Za-z0-9]{1,24}$/.test(e.code)) return;
      if (!worker) start();
      const held = hands(ws);
      if (e.way === 'up') return keyUp(held, e.code);
      if (e.way !== 'down' || held.keys.has(e.code) || held.keys.size >= 32 || typeof e.key !== 'string' ||
          !/^(?:[ -~]|Arrow(?:Left|Right|Up|Down)|Shift|Enter|Backspace|Alt|CapsLock|Delete)$/i.test(e.key)) return;
      const key = e.key.toLowerCase(), count = keyCounts.get(key) || 0;
      const event = { code: e.code, key, shiftKey: e.shiftKey === true, altKey: e.altKey === true };
      held.keys.set(e.code, event);
      keyCounts.set(key, count + 1);
      if (!count) worker?.postMessage({ ...event, type: 'key', way: 'down' });
    },
    pointer(ws, name, finger, x, y) {
      if (!worker) start();
      const held = hands(ws);
      if (name === 'touch') held.pointers.set(finger, nextPointer++);
      const pointer = held.pointers.get(finger);
      if (pointer === undefined) return;
      worker?.postMessage({ type: 'pointer', name, pointer, x, y });
      if (name === 'lift') held.pointers.delete(finger);
    },
    reset() {
      if (performance.now() - resetAt < 1000) return false;
      resetAt = performance.now();
      kiosks.reset('lobby', id);
      const old = worker;
      worker = frame = sound = null;
      void old?.terminate();
      players.clear(); keyCounts.clear();
      broadcast({ type: 'notepat-sound', voices: [], at: Date.now() });
      if (viewers.size) start();
      return true;
    },
    async close() {
      closing = true;
      const old = worker;
      if (!old) return;
      await new Promise(resolve => { flushed = resolve; old.postMessage({ type: 'flush' }); });
      await old.terminate();
    },
  };
}
