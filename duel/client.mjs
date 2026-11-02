import { HZ, DT, PROTOCOL, INTERP_TICKS, move, spawn, clamp } from './world.mjs';
import { reconcile, interpolate } from './net.mjs';
import { createView } from './view.mjs';

const canvas = document.getElementById('game'), view = createView(canvas), keys = new Set();
const requestedRoom = new URLSearchParams(location.search).get('room');
const room = requestedRoom && /^[a-z0-9-]{1,32}$/.test(requestedRoom) ? requestedRoom : 'lobby';
let ws, seat = 0, player, snapshot, history = [], pending = [], seq = 0;
let yaw = 0, pitch = 0, third = false, firing = false, latestAt = 0, eventId = 0;
let jumpQueued = false, fireQueued = false, audio, notice = 'CONNECTING';
let last = performance.now(), debt = 0;
const locked = () => document.pointerLockElement === canvas;
const send = data => { if (ws?.readyState === WebSocket.OPEN && ws.bufferedAmount < 32768) ws.send(JSON.stringify(data)); };
const viewTick = () => snapshot ? snapshot.tick + Math.min(6, (performance.now() - latestAt) / 1000 * HZ) - INTERP_TICKS : 0;

function tone(frequency, duration, volume = 0.04) {
  if (!audio || audio.state !== 'running') return;
  const oscillator = audio.createOscillator(), gain = audio.createGain();
  oscillator.type = 'triangle'; oscillator.frequency.value = frequency;
  gain.gain.setValueAtTime(volume, audio.currentTime);
  gain.gain.exponentialRampToValueAtTime(0.001, audio.currentTime + duration);
  oscillator.connect(gain).connect(audio.destination);
  oscillator.start(); oscillator.stop(audio.currentTime + duration);
}

async function capture() {
  if (locked()) return;
  audio ||= new AudioContext();
  await audio.resume();
  if (!ws || ws.readyState === WebSocket.CLOSED) connect();
  try { await canvas.requestPointerLock(); notice = ''; }
  catch { notice = 'CLICK TO CAPTURE MOUSE'; }
}

function connect() {
  const url = new URL('/play', location.href);
  url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  url.search = new URLSearchParams({ room, protocol: PROTOCOL });
  history = []; pending = []; player = null; snapshot = null; seq = 0; eventId = 0; debt = 0;
  notice = 'CONNECTING';
  ws = new WebSocket(url);
  const socket = ws;
  socket.onmessage = e => {
    if (socket !== ws) return;
    const data = JSON.parse(e.data);
    if (data.type === 'welcome') seat = data.seat;
    if (data.type !== 'snapshot' || (snapshot && data.tick <= snapshot.tick)) return;
    const own = data.players[seat];
    if (!own) return;
    const changedLife = !player || own.life !== player.life;
    const result = reconcile(own, pending, data.acks[seat]);
    pending = result.pending;
    player = data.phase === 'playing' || data.phase === 'waiting' ? result.player : { ...own };
    if (changedLife) { yaw = own.yaw; pitch = own.pitch; }
    snapshot = data; latestAt = performance.now(); history.push(data); history = history.slice(-32);
    if (notice === 'CONNECTING') notice = '';
    for (const event of data.events) {
      if (event.id <= eventId) continue;
      eventId = event.id;
      if (data.tick - event.tick > 15) continue;
      view.effect(event);
      if (event.type === 'launch') tone(65, 0.12);
      if (event.type === 'frag') tone(event.attacker === seat ? 600 : 100, 0.2);
    }
  };
  socket.onclose = e => {
    if (socket !== ws) return;
    if (locked()) document.exitPointerLock();
    notice = e.reason || 'DISCONNECTED — CLICK TO REJOIN';
    player = null; snapshot = null; pending = []; history = [];
  };
  socket.onerror = () => { notice = 'SERVER UNAVAILABLE — CLICK TO RETRY'; };
}

canvas.addEventListener('click', capture);
canvas.addEventListener('keydown', e => { if (e.code === 'Enter') capture(); });
function releaseInput() { keys.clear(); firing = jumpQueued = fireQueued = false; }
document.addEventListener('pointerlockchange', releaseInput);
document.addEventListener('mousemove', e => {
  if (!locked()) return;
  yaw = (yaw - e.movementX * 0.12 + 360) % 360;
  pitch = clamp(pitch - e.movementY * 0.12, -85, 85);
});
document.addEventListener('keydown', e => {
  if (!locked()) return;
  if (['KeyW', 'KeyA', 'KeyS', 'KeyD', 'Space', 'KeyV'].includes(e.code)) e.preventDefault();
  keys.add(e.code);
  if (e.code === 'Space' && !e.repeat) jumpQueued = true;
  if (e.code === 'KeyV' && !e.repeat) third = !third;
});
document.addEventListener('keyup', e => keys.delete(e.code));
document.addEventListener('mousedown', e => { if (locked() && e.button === 0) firing = fireQueued = true; });
document.addEventListener('mouseup', () => { firing = false; });
window.addEventListener('blur', releaseInput);
document.addEventListener('visibilitychange', () => { releaseInput(); debt = 0; last = performance.now(); });

function frame(now) {
  debt += Math.min(0.1, (now - last) / 1000); last = now;
  const connected = ws?.readyState === WebSocket.OPEN && player && snapshot;
  while (debt >= DT) {
    debt -= DT;
    if (!connected || pending.length >= 120 || now - latestAt > 1000) continue;
    const c = { seq: ++seq, yaw, pitch,
      fwd: Number(keys.has('KeyW')) - Number(keys.has('KeyS')),
      right: Number(keys.has('KeyD')) - Number(keys.has('KeyA')),
      buttons: keys.has('Space') || jumpQueued ? 1 : 0, fire: (firing || fireQueued) && locked() };
    jumpQueued = fireQueued = false;
    pending.push(c);
    if (snapshot.phase === 'playing' || snapshot.phase === 'waiting') player = move(player, c);
    send({ type: 'input', commands: pending.slice(-3) });
  }
  const seconds = snapshot ? Math.max(1, Math.ceil((snapshot.until - snapshot.tick) / HZ)) : 0;
  const status = !connected ? notice : now - latestAt > 1000 ? 'CONNECTION STALLED' : notice || (
    snapshot.phase === 'countdown' ? `${seconds}` :
    snapshot.phase === 'finished' ? `${snapshot.winner === seat ? 'YOU WIN' : 'OPPONENT WINS'} · ${seconds}` :
    !player.alive ? 'RESPAWNING' : !locked() ? 'CLICK TO PLAY' :
    snapshot.phase === 'waiting' ? 'WAITING FOR OPPONENT' : '');
  view.render({ players: connected ? [0, 1].map(i => interpolate(history, viewTick(), i)) : [],
    me: player ? { ...player, yaw, pitch } : spawn(seat), seat, third,
    rockets: snapshot?.rockets || [], scores: snapshot?.players.map(p => p?.score || 0) || [0, 0],
    status });
  requestAnimationFrame(frame);
}
connect();
requestAnimationFrame(frame);
