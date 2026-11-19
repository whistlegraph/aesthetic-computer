// 🎭 An actor: a scripted player. It joins rax the way a browser does (the
// same socket, the same commands at the same rate) and is told what to do in
// sentences: walk there, face that, say this, ride to a floor.
import WebSocket from 'ws';
import { HZ, PROTOCOL } from '../world.mjs';
import { aim, turn } from './camera.mjs';
import { inCar } from '../elevator.mjs';

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

export class Actor {
  // `base` is the server (http://127.0.0.1:8798); `token` is what the server's
  // verifyIdentity is given, so a film's own server can cast any handle.
  constructor({ base, handle, token = handle }) {
    Object.assign(this, { base, handle, token, room: null, seat: -1, me: null, tower: null, said: [] });
    this.owner = null; // kept across rooms: a ride's passage is bound to it
    this.seq = 0;
    this.sent = [];
    this.control = { fwd: 0, right: 0, yaw: 0, pitch: 0, jump: false };
    this.waiting = new Set();
  }
  // Resolve when a message passes `test` (or reject after `ms`).
  until(test, ms = 8000, what = 'a reply') {
    return new Promise((resolve, reject) => {
      const entry = { test, resolve };
      this.waiting.add(entry);
      setTimeout(() => {
        if (this.waiting.delete(entry)) reject(Error(`@${this.handle} waited ${ms} ms for ${what}`));
      }, ms).unref?.();
    });
  }
  async enter(room = 'lobby', passage = null) {
    this.leave();
    const url = new URL('./play', this.base.endsWith('/') ? this.base : this.base + '/');
    url.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
    url.search = new URLSearchParams({ room, protocol: PROTOCOL, account: '1', ...(passage ? { passage } : {}) });
    const ws = (this.ws = new WebSocket(url));
    Object.assign(this, { room, seat: -1, me: null, seq: 0, sent: [], tower: null });
    ws.on('message', (raw) => {
      if (ws !== this.ws) return;
      const d = JSON.parse(raw);
      if (d.type === 'welcome' || d.type === 'role') this.seat = d.seat;
      if (d.type === 'identity') this.owner = d.token;
      if (d.type === 'tower') this.tower = d;
      if (d.type === 'arrival') { this.control.yaw = d.yaw; this.control.pitch = d.pitch || 0; }
      if (d.type === 'said') this.said.push(d);
      if (d.type === 'snapshot') {
        const p = this.seat >= 0 ? d.players[this.seat] : null;
        if (p) {
          // A fresh body starts its own count of commands.
          if (!this.me) this.seq = d.acks[this.seat] || 0;
          this.me = p;
        }
        return;
      }
      for (const w of [...this.waiting]) if (w.test(d)) { this.waiting.delete(w); w.resolve(d); }
    });
    await new Promise((resolve, reject) => { ws.once('open', resolve); ws.once('error', reject); });
    const seated = this.until((d) => (d.type === 'role' || d.type === 'presence') && this.seat >= 0, 8000, 'a seat');
    ws.send(JSON.stringify({ type: 'identity', token: this.owner || undefined }));
    ws.send(JSON.stringify({ type: 'account', accessToken: this.token }));
    await seated;
    for (let i = 0; i < 200 && !this.me; i++) await sleep(10);
    if (!this.me) throw Error(`@${this.handle} was seated but never appeared`);
    this.control.yaw = this.me.yaw || 0;
    clearInterval(this.timer);
    this.timer = setInterval(() => this.tick(), 1000 / HZ);
    return this;
  }
  leave() {
    clearInterval(this.timer);
    const ws = this.ws;
    this.ws = null;
    if (ws && ws.readyState <= WebSocket.OPEN) ws.close();
  }
  send(data) {
    if (this.ws?.readyState === WebSocket.OPEN) this.ws.send(JSON.stringify(data));
  }
  // One command a tick, the last twelve sent again so a lost message costs nothing.
  tick() {
    if (!this.me || this.ws?.readyState !== WebSocket.OPEN) return;
    const c = this.control;
    this.sent.push({ seq: ++this.seq, fwd: c.fwd, right: c.right, yaw: c.yaw, pitch: c.pitch, buttons: c.jump ? 1 : 0 });
    c.jump = false;
    this.sent = this.sent.slice(-12);
    this.send({ type: 'input', commands: this.sent });
  }
  get at() { return this.me ? { x: this.me.x, y: this.me.y, z: this.me.z } : null; }
  wait(seconds) { return sleep(seconds * 1000); }
  // Turn to a bearing (degrees) or toward a point [x, z], over `seconds`.
  async face(where, seconds = 0.5) {
    const to = Array.isArray(where) ? aim(this.me, { x: where[0], y: this.me.y, z: where[1] }).yaw : where;
    const from = this.control.yaw, sweep = turn(from, to), steps = Math.max(1, Math.round(seconds * 30));
    for (let i = 1; i <= steps; i++) {
      const k = i / steps, e = k < 0.5 ? 2 * k * k : 1 - (-2 * k + 2) ** 2 / 2;
      this.control.yaw = (from + sweep * e + 360) % 360;
      await sleep((seconds * 1000) / steps);
    }
  }
  // Walk to a spot. `pace` is how hard the stick is pushed: 0.35 strolls, 1 runs.
  async walkTo(x, z, { pace = 0.4, within = 0.2, turnFirst = 0.35, patience = 12 } = {}) {
    if (turnFirst) await this.face([x, z], turnFirst);
    const end = Date.now() + patience * 1000;
    for (;;) {
      const dx = x - this.me.x, dz = z - this.me.z, far = Math.hypot(dx, dz);
      if (far <= within) break;
      if (Date.now() > end) { this.control.fwd = 0; throw Error(`@${this.handle} could not reach ${x}, ${z} (at ${this.me.x.toFixed(2)}, ${this.me.z.toFixed(2)})`); }
      this.control.yaw = aim(this.me, { x, y: this.me.y, z }).yaw;
      this.control.fwd = Math.min(pace, Math.max(0.18, far * 0.5)); // ease into the mark
      await sleep(1000 / 30);
    }
    this.control.fwd = 0;
    await sleep(120); // come to rest
  }
  jump() { this.control.jump = true; }
  say(text) { this.send({ type: 'say', text }); }
  // 🏢 Make a floor number this handle's own.
  async take(number) {
    const reply = this.until((d) => d.type === 'elevator-result', 6000, 'the elevator');
    this.send({ type: 'take-floor', number });
    const r = await reply;
    if (r.error) throw Error(`@${this.handle} could not take ${number}: ${r.error}`);
    return r;
  }
  // 🛗 From inside the car: to a number, a handle, 'home', 'g', 'up' or 'down'.
  // Resolves standing in the car on the floor arrived at.
  async ride(to) {
    const reply = this.until((d) => d.type === 'travel' || d.type === 'elevator-result', 6000, 'the elevator');
    this.send({ type: 'elevator', to: String(to) });
    const r = await reply;
    if (r.type !== 'travel') throw Error(`@${this.handle} could not ride to ${to}: ${r.error}`);
    const arrived = this.until((d) => d.type === 'arrival', 8000, 'the doors');
    await this.enter(r.room, r.passage);
    await arrived;
    for (let i = 0; i < 100 && !inCar(this.me); i++) await sleep(10);
    return r.ride;
  }
}
