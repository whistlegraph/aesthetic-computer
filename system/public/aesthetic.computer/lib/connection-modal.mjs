import { palette } from "./onboarding-ui.mjs";

export function connectionLayout(width, height) {
  const w = Math.min(228, width - 8);
  const h = 62;
  const x = Math.floor((width - w) / 2), y = Math.max(4, Math.floor((height - h) / 2));
  const buttonWidth = Math.floor((w - 24) / 2);
  return { x, y, w, h, buttons: [
    { id: "retry", x: x + 8, y: y + h - 28, w: buttonWidth, h: 22 },
    { id: "keep", x: x + 16 + buttonWidth, y: y + h - 28, w: buttonWidth, h: 22 },
  ] };
}

// Owned by disk, so losing the network never replaces or restarts a piece.
export function createConnectionModal() {
  let offline = false, visible = false, checking = false, focused = 0, pressed;
  let backing;
  const dismiss = () => { visible = false; pressed = undefined; };
  function activate(id, $) {
    if (id === "keep") dismiss();
    else if (!checking) {
      checking = true;
      $.send({ type: "connection:retry" });
    }
    $.needsPaint();
  }
  return {
    get visible() { return visible; },
    restore(screen, slug) {
      if (!backing) return false;
      const matches = screen && backing.slug === slug && backing.width === screen.width && backing.height === screen.height;
      if (matches) screen.pixels.set(backing.pixels);
      backing = undefined;
      return !!matches;
    },
    capture(screen, slug) {
      if (visible) backing = { width: screen.width, height: screen.height, slug, pixels: screen.pixels.slice() };
    },
    update(status) {
      if (status === "online") { offline = false; checking = false; dismiss(); }
      else if (status === "offline") {
        if (!offline) { visible = true; focused = 0; }
        offline = true;
        checking = false;
      } else if (status === "checking") checking = true;
    },
    act($) {
      if (!visible) return false;
      const e = $.event;
      if (e.is("keyboard:down:escape")) { dismiss(); $.needsPaint(); }
      else if (e.is("keyboard:down:tab")) { focused = 1 - focused; $.needsPaint(); }
      else if (e.is("keyboard:down:enter") || e.is("keyboard:down:space")) activate(focused ? "keep" : "retry", $);
      else {
        const hit = connectionLayout($.screen.width, $.screen.height).buttons.find(b =>
          e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h);
        if (e.is("touch")) pressed = hit?.id;
        if (e.is("lift")) {
          if (hit && hit.id === pressed) activate(hit.id, $);
          pressed = undefined;
        }
      }
      return true;
    },
    paint($) {
      if (!visible) return;
      const { x, y, w, h, buttons } = connectionLayout($.screen.width, $.screen.height);
      const write = (s, x, y, color) => $.ink(color).write(s, { x, y }, undefined, undefined, false, "font_1");
      $.ink(0, 0, 0, 100).box(0, 0, $.screen.width, $.screen.height);
      $.ink(palette.ground).box(x, y, w, h);
      $.ink(palette.pink).box(x, y, w, h, "outline");
      write("Connection lost", x + 8, y + 9, palette.pink);
      buttons.forEach((b, i) => {
        $.ink(i === focused ? palette.pink : palette.panel).box(b.x, b.y, b.w, b.h);
        write(i ? (b.w < 80 ? "Stay" : "Keep playing") : checking ? "Trying..." : "Retry", b.x + 4, b.y + 6,
          i === focused ? palette.ground : palette.text);
      });
    },
  };
}
