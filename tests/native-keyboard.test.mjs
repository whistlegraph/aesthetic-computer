import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import vm from "node:vm";

const source = readFileSync(new URL("../system/public/aesthetic.computer/lib/keyboard.mjs", import.meta.url), "utf8")
  .replace(/^import .*;$/m, "")
  .replace("export class Keyboard", "class Keyboard");

function keyboard(native = true) {
  const listeners = {};
  const document = { activeElement: null, addEventListener() {} };
  const context = vm.createContext({
    Aesthetic: native, MacOS: true, MetaBrowser: false, document,
    window: { addEventListener(name, handler) { listeners[name] = handler; } },
  });
  const keys = vm.runInContext(source + "\nnew Keyboard()", context);
  keys.focusHandler = () => false;
  keys.input = {};
  return { keys, listeners, document };
}

function event(extra = {}) {
  return { key: "a", code: "KeyA", target: { closest: () => null },
    preventDefault() { this.defaultPrevented = true; }, ...extra };
}

test("native canvas notes, repeats, and releases reach the piece without native defaults", () => {
  const { keys, listeners } = keyboard();
  for (const repeat of [false, true]) {
    const e = event({ repeat });
    listeners.keydown(e);
    assert.equal(e.defaultPrevented, true);
    assert.equal(keys.events.at(-1).name, "keyboard:down:a");
    assert.equal(keys.events.at(-1).repeat, repeat);
  }
  listeners.keyup(event());
  assert.equal(keys.events.at(-1).name, "keyboard:up:a");
});

test("DOM typing, composition, Command shortcuts, and websites keep native defaults", () => {
  for (const extra of [{ metaKey: true }, { isComposing: true }, { target: { closest: () => ({}) } }]) {
    const { listeners } = keyboard();
    const e = event(extra);
    listeners.keydown(e);
    assert.equal(e.defaultPrevented, undefined);
  }
  const { keys, listeners, document } = keyboard();
  document.activeElement = keys.input;
  const e = event();
  listeners.keydown(e);
  assert.equal(e.defaultPrevented, undefined);
  assert.equal(keys.events.length, 0);
  const site = keyboard(false), siteEvent = event();
  site.listeners.keydown(siteEvent);
  assert.equal(siteEvent.defaultPrevented, undefined);
});
