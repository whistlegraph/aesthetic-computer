import { palette } from "./onboarding-ui.mjs";
import { PROMPT_TEXT_ORIGIN } from "./prompt-layout.mjs";

const LETTER_MS = 16; // Sixty letters a second, as rax types its own outage.
const LINE_PAUSE = 6; // Letters of quiet between one line and the next.
const LINE_HEIGHT = 12;
const GLYPH_WIDTH = 6;
const FAREWELL_MS = 1600; // How long "back online" stays before the label returns.

// The lines of one outage: what happened, then what the machine is doing.
export function connectionLines({ checking, tries, online }) {
  const lines = [{ text: "connection lost", color: palette.pink }];
  if (online) lines.push({ text: "back online", color: palette.mint });
  else lines.push({
    text: checking ? "retrying" : tries ? `no answer, retry ${tries + 1} soon` : "waiting to retry",
    color: palette.muted,
  });
  return lines;
}

// Owned by disk, so losing the network never replaces, restarts or blocks a
// piece. The notice takes over the corner label's place and types itself
// out; retries are the host's job (connection-status.mjs) and need no button.
export function createConnectionModal(now = () => Date.now()) {
  let offline = false, checking = false, tries = 0;
  let since = 0, farewell = 0;
  let backing;
  const showing = () => offline || now() < farewell;
  return {
    get visible() { return showing(); },
    restore(screen, slug) {
      if (!backing) return false;
      const matches = screen && backing.slug === slug && backing.width === screen.width && backing.height === screen.height;
      if (matches) screen.pixels.set(backing.pixels);
      backing = undefined;
      return !!matches;
    },
    capture(screen, slug) {
      if (showing()) backing = { width: screen.width, height: screen.height, slug, pixels: screen.pixels.slice() };
    },
    update(status) {
      if (status === "online") {
        if (offline) farewell = now() + FAREWELL_MS;
        offline = false; checking = false; tries = 0;
      } else if (status === "offline") {
        if (!offline) { since = now(); farewell = 0; tries = 0; }
        else if (checking) tries++;
        offline = true; checking = false;
      } else if (status === "checking") checking = true;
    },
    // The piece keeps every event: a lost connection is not a question.
    act() { return false; },
    lines() {
      let budget = Math.floor((now() - since) / LETTER_MS);
      const shown = [];
      for (const line of connectionLines({ checking, tries, online: !offline })) {
        if (budget <= 0) break;
        shown.push({ ...line, text: line.text.slice(0, budget), done: budget >= line.text.length });
        budget -= line.text.length + LINE_PAUSE;
      }
      return shown;
    },
    paint($) {
      if (!showing()) return;
      const shown = this.lines();
      const { x, y } = PROMPT_TEXT_ORIGIN;
      const blink = now() % 600 < 400;
      shown.forEach((line, i) => {
        const top = y + i * LINE_HEIGHT, last = i === shown.length - 1;
        const width = line.text.length * GLYPH_WIDTH + (last ? GLYPH_WIDTH : 0);
        // A ground under each line, so it reads over any piece and replaces
        // whatever word was in the corner.
        $.ink(palette.ground).box(x - 2, top - 2, width + 4, LINE_HEIGHT);
        $.ink(line.color).write(line.text, { x, y: top }, undefined, undefined, false, "font_1");
        if (last && blink && offline)
          $.ink(line.color).box(x + line.text.length * GLYPH_WIDTH + 1, top, 4, 8);
      });
    },
  };
}
