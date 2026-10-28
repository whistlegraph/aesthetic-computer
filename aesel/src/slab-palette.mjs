// The status page color Slab reads off Terminal, handed over as a small file
// because Terminal.app does not reliably report live OSC palette changes.
import {mkdirSync, readFileSync} from 'node:fs';
import {join} from 'node:path';

export class SlabPalette {
  constructor({directory, sessionId, enabled = true, changed = () => {}}) {
    this.file = join(directory, `${sessionId}.json.palette`);
    this.sessionId = sessionId; this.changed = changed;
    this.background = null;
    this.enabled = enabled;
    if (enabled) {
      try { mkdirSync(directory, {recursive:true, mode:0o700}); }
      catch { this.enabled = false; }
    }
    if (this.enabled) { this.timer = setInterval(() => this.poll(), 500); this.timer.unref(); }
  }
  poll() {
    try {
      const page = JSON.parse(readFileSync(this.file, 'utf8'));
      const rgb = page.background;
      if (page.schema === 1 && page.sessionId === this.sessionId && Array.isArray(rgb)
          && rgb.length === 3 && rgb.every(n => Number.isFinite(n) && n >= 0 && n <= 255)
          && JSON.stringify(rgb) !== JSON.stringify(this.background)) {
        this.background = rgb; queueMicrotask(this.changed);
      }
    } catch {}
  }
  close() { clearInterval(this.timer); }
}
