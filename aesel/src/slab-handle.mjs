// A measured footer slot, leased to Slab only while its native letters exist.
import {mkdirSync, readFileSync, writeFileSync, renameSync, rmSync} from 'node:fs';
import {join} from 'node:path';
import {randomUUID} from 'node:crypto';

export class SlabHandle {
  constructor({directory, sessionId, pid = process.pid, enabled = true, changed = () => {}, now = Date.now}) {
    this.file = join(directory, `${sessionId}.json`);
    this.ack = this.file + '.ack';
    this.sessionId = sessionId; this.pid = pid; this.changed = changed; this.now = now;
    this.layout = null; this.key = ''; this.ready = false; this.writtenAt = 0;
    this.background = null;
    this.hovered = false;
    this.enabled = enabled;
    if (enabled) {
      try { mkdirSync(directory, {recursive:true, mode:0o700}); }
      catch { this.enabled = false; }
    }
    if (this.enabled) { this.timer = setInterval(() => this.poll(), 500); this.timer.unref(); }
  }
  get readyLayout() { return this.ready ? this.layout : null; }
  update(layout) {
    if (!this.enabled) return;
    const key = JSON.stringify(layout);
    if (key === this.key) return;
    this.key = key; this.layout = layout; this.token = randomUUID(); this.setReady(false);
    if (layout) this.write();
    else this.remove();
  }
  setReady(ready) {
    if (this.ready === ready) return;
    this.ready = ready; queueMicrotask(this.changed);
  }
  setHovered(hovered) {
    if (this.hovered === hovered) return;
    this.hovered = hovered;
    if (this.enabled && this.layout) this.write();
  }
  write() {
    const temp = this.file + '.tmp';
    try {
      const at = this.now();
      writeFileSync(temp, JSON.stringify({schema:1, sessionId:this.sessionId, pid:this.pid,
        token:this.token, at, ...this.layout, hovered:this.hovered}), {mode:0o600});
      renameSync(temp, this.file); this.writtenAt = at;
    } catch { this.setReady(false); try { rmSync(temp, {force:true}); } catch {} }
  }
  poll() {
    try {
      const page = JSON.parse(readFileSync(this.file + '.palette', 'utf8'));
      const rgb = page.background;
      if (page.schema === 1 && page.sessionId === this.sessionId && Array.isArray(rgb)
          && rgb.length === 3 && rgb.every(n => Number.isFinite(n) && n >= 0 && n <= 255)
          && JSON.stringify(rgb) !== JSON.stringify(this.background)) {
        this.background = rgb; queueMicrotask(this.changed);
      }
    } catch {}
    if (!this.layout) return;
    if (this.now() - this.writtenAt >= 2000) this.write();
    let ready = false;
    try {
      const text = readFileSync(this.ack, 'utf8');
      if (text.length <= 4096) {
        const ack = JSON.parse(text), age = this.now() - ack.at;
        ready = ack.schema === 1 && ack.sessionId === this.sessionId && ack.token === this.token
          && Number.isFinite(age) && age >= -1000 && age < 3000;
      }
    } catch {}
    this.setReady(ready);
  }
  remove() { for (const file of [this.file, this.ack]) try { rmSync(file, {force:true}); } catch {} }
  close() { clearInterval(this.timer); this.layout = null; this.ready = false; this.remove(); }
}
