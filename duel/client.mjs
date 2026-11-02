import { HZ, DT, PROTOCOL, INTERP_TICKS, move, clamp } from './world.mjs';
import { reconcile, interpolate } from './net.mjs';
import { createView } from './view.mjs';

const $ = id => document.getElementById(id);
const canvas = $('game'), view = createView(canvas), keys = new Set();
const params = new URLSearchParams(location.search);
if (params.has('room') && /^[a-z0-9-]{1,32}$/.test(params.get('room'))) $('room').value = params.get('room');
if (['rocket', 'instagib'].includes(params.get('mode'))) $('mode').value = params.get('mode');
let ws, seat = 0, player, snapshot, history = [], pending = [], seq = 0;
let yaw = 0, pitch = 0, third = false, firing = false, latestAt = 0, eventId = 0;
let jumpQueued = false, fireQueued = false;
let ping = 0, correction = 0, hitUntil = 0, lastPing = 0, audio;
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
  try { await canvas.requestPointerLock(); }
  catch { $('error').textContent = 'Click Enter arena to capture the mouse.'; }
}

$('join').addEventListener('submit', async e => {
  e.preventDefault();
  audio ||= new AudioContext(); await audio.resume();
  $('error').textContent = '';
  if (ws?.readyState === WebSocket.OPEN) { await capture(); return; }
  const url = new URL('/play', location.href);
  url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  url.search = new URLSearchParams({ room: $('room').value, mode: $('mode').value, protocol: PROTOCOL });
  history = []; pending = []; player = null; snapshot = null; seq = 0; eventId = 0; debt = 0;
  ws = new WebSocket(url);
  const socket = ws;
  socket.onopen = () => { $('play').textContent = 'Resume'; $('room').disabled = $('mode').disabled = true; };
  socket.onmessage = e => {
    if (socket !== ws) return;
    const data = JSON.parse(e.data);
    if (data.type === 'welcome') seat = data.seat;
    if (data.type === 'pong') ping = Math.round(performance.now() - data.at);
    if (data.type !== 'snapshot' || (snapshot && data.tick <= snapshot.tick)) return;
    const own = data.players[seat];
    if (!own) return;
    const changedLife = !player || own.life !== player.life;
    const old = player;
    const result = reconcile(own, pending, data.acks[seat]);
    pending = result.pending;
    player = data.phase === 'playing' || data.phase === 'waiting' ? result.player : { ...own };
    if (changedLife) { yaw = own.yaw; pitch = own.pitch; }
    correction = old && !changedLife ? Math.hypot(old.x - player.x, old.y - player.y, old.z - player.z) : 0;
    snapshot = data; latestAt = performance.now(); history.push(data); history = history.slice(-32);
    for (const event of data.events) {
      if (event.id <= eventId) continue;
      eventId = event.id;
      if (data.tick - event.tick > 15) continue;
      view.effect(event);
      if (event.type === 'beam' || event.type === 'launch') tone(event.type === 'beam' ? 170 : 65, 0.12);
      if (event.type === 'beam' && event.hit && event.seat === seat) { hitUntil = performance.now() + 180; tone(900, 0.08); }
      if (event.type === 'frag') tone(event.attacker === seat ? 600 : 100, 0.2);
    }
    $('hud').hidden = false;
  };
  socket.onclose = e => {
    if (socket !== ws) return;
    if (locked()) document.exitPointerLock();
    $('menu').hidden = false; $('crosshair').hidden = true;
    $('error').textContent = e.reason || 'Disconnected. Enter again to rejoin.';
    $('play').textContent = 'Enter arena'; $('room').disabled = $('mode').disabled = false;
    $('status').textContent = 'Disconnected';
    player = null; snapshot = null; pending = []; history = [];
  };
  socket.onerror = () => { $('error').textContent = 'Could not connect to the duel server.'; };
  await capture();
});

$('invite').addEventListener('click', async () => {
  const url = new URL(location.href);
  url.search = new URLSearchParams({ room: $('room').value, mode: $('mode').value });
  try { await navigator.clipboard.writeText(url.href); $('error').textContent = 'Invite copied.'; }
  catch { $('error').textContent = url.href; }
});
document.addEventListener('pointerlockchange', () => {
  keys.clear(); firing = jumpQueued = fireQueued = false;
  $('menu').hidden = locked(); $('crosshair').hidden = !locked();
});
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
window.addEventListener('blur', () => { keys.clear(); firing = jumpQueued = fireQueued = false; });
document.addEventListener('visibilitychange', () => {
  keys.clear(); firing = jumpQueued = fireQueued = false; debt = 0; last = performance.now();
});

function frame(now) {
  debt += Math.min(0.1, (now - last) / 1000); last = now;
  const connected = ws?.readyState === WebSocket.OPEN && player && snapshot;
  while (debt >= DT) {
    debt -= DT;
    if (!connected || pending.length >= 120 || now - latestAt > 1000) continue;
    const c = { seq: ++seq, yaw, pitch, viewTick: viewTick(),
      fwd: Number(keys.has('KeyW')) - Number(keys.has('KeyS')),
      right: Number(keys.has('KeyD')) - Number(keys.has('KeyA')),
      buttons: keys.has('Space') || jumpQueued ? 1 : 0, fire: (firing || fireQueued) && locked() };
    jumpQueued = fireQueued = false;
    pending.push(c);
    if (snapshot.phase === 'playing' || snapshot.phase === 'waiting') player = move(player, c);
    send({ type: 'input', commands: pending.slice(-3) });
  }
  if (connected) {
    const players = [0, 1].map(i => interpolate(history, viewTick(), i));
    const local = { ...player, yaw, pitch };
    view.render({ players, me: local, seat, third, rockets: snapshot.rockets });
    const scores = snapshot.players.map(p => p?.score ?? 0);
    $('score').innerHTML = `<span class="a">${scores[0]}</span> : <span class="b">${scores[1]}</span>`;
    const seconds = Math.max(1, Math.ceil((snapshot.until - snapshot.tick) / HZ));
    $('status').textContent = now - latestAt > 1000 ? 'Connection stalled' :
      snapshot.phase === 'waiting' ? 'Waiting for opponent · warm up' :
      snapshot.phase === 'countdown' ? `${seconds}` :
      snapshot.phase === 'finished' ? `${snapshot.winner === seat ? 'You win' : 'Opponent wins'} · rematch in ${seconds}` :
      !player.alive ? 'Respawning' : '';
    $('health').textContent = `${player.health} HP`;
    $('stats').textContent = `${snapshot.mode} · ${third ? 'third' : 'first'} person · ${seat === 0 ? 'coral' : 'blue'}\n${ping} ms · correction ${correction.toFixed(2)} m`;
    $('hit').style.opacity = now < hitUntil ? '1' : '0';
    if (now - lastPing > 1000) { lastPing = now; send({ type: 'ping', at: now }); }
  } else view.render();
  requestAnimationFrame(frame);
}
requestAnimationFrame(frame);
