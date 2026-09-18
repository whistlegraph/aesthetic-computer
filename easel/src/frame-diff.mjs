// Frames contain full-width, self-colored rows. Move directly to changed rows;
// never clear the display between keystrokes. Reset after any external drawing.
export class FrameDiff {
  constructor({clearOnResize=true}={}) { this.clearOnResize=clearOnResize; }
  reset() { this.lines = null; this.columns = null; }
  update(frame, columns) {
    const lines = frame.split('\n');
    const full = !this.lines || columns !== this.columns || lines.length !== this.lines.length;
    let output = full ? (!this.lines || this.clearOnResize ? '\x1b[H\x1b[2J' : '\x1b[H') : '';
    for (let i = 0; i < lines.length; i++) {
      if (full || lines[i] !== this.lines[i]) output += `\x1b[${i + 1};1H${lines[i]}`;
    }
    this.lines = lines;
    this.columns = columns;
    return output ? `\x1b[?2026h${output}\x1b[?2026l` : '';
  }
}
