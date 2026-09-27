export const MOUSE_ON = "\x1b[?1003h\x1b[?1006h";
export const MOUSE_OFF = "\x1b[?1003l\x1b[?1006l";

export function mouseEvent(token) {
  const match = /^\x1b\[<(\d+);(\d+);(\d+)([Mm])$/.exec(token);
  if (!match) return null;
  const [, code, x, y, end] = match;
  const button = Number(code);
  const held = button & 3;
  return { x: Number(x), y: Number(y), motion: Boolean(button & 32),
    wheel: button & 64 ? (button & 1 ? 1 : -1) : 0,
    click: end === "M" && button === 0,
    // The three moments of a drag: the left button going down, moving while
    // it is down, and coming up — what a selection is made of.
    press: end === "M" && button === 0,
    drag: end === "M" && Boolean(button & 32) && held === 0 && !(button & 64),
    release: end === "m" && (held === 0 || held === 3) && !(button & 64) };
}

// Keep split terminal escape sequences intact between stdin chunks.
export class InputDecoder {
  pending = "";
  push(chunk) {
    this.pending += chunk;
    const tokens = [];
    while (this.pending) {
      if (this.pending.startsWith("\x1b[")) {
        const match = /^\x1b\[[0-?]*[ -/]*[@-~]/.exec(this.pending);
        if (!match) break;
        tokens.push(match[0]);
        this.pending = this.pending.slice(match[0].length);
      } else if (this.pending === "\x1b") {
        break;
      } else {
        const token = String.fromCodePoint(this.pending.codePointAt(0));
        tokens.push(token);
        this.pending = this.pending.slice(token.length);
      }
    }
    return tokens;
  }
  escape() {
    if (this.pending !== "\x1b") return [];
    this.pending = "";
    return ["\x1b"];
  }
}
