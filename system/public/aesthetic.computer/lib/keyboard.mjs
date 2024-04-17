// ⌨ Keyboard
import { MetaBrowser } from "./platform.mjs";

// TODO: Add more of these properties as needed:
//       https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

export class Keyboard {
  events = [];
  #lastKeyDown;
  input;
  focusHandler;
  #held = new Set();
  needsImmediateOpen = false;

  constructor() {
    window.addEventListener("keydown", (e) => {
      // console.log("⌨️ Key event:", e);

      this.#held.add(e.key);
      // Firefox "repeat" seems to be broken on linux, so here is
      // some redundancy. 22.07.29.17.43
      const repeat = e.key === this.#lastKeyDown;
      this.#lastKeyDown = e.key;

      const keyboardFocused = this.focusHandler(e); // Focus DOM input field
      //                                               as neded for text entry.

      if (keyboardFocused) return;
      // Skip any initial keyboard event.

      // Skip sending most keyboard events from here if we are using text input
      // which generates a synthetic keyboard event back
      //  in `bios` under `Keyboard`
      if (
        document.activeElement === this.input &&
        // Remaps "Unidentified" to "Backspace" below, avoiding `Enter` code.
        //(MetaBrowser && e.which !== 13) &&
        e.key !== "Unidentified" &&
        e.key !== "Escape" &&
        e.key !== "Delete" &&
        e.key !== "ArrowUp" &&
        e.key !== "ArrowDown" &&
        e.key !== "ArrowLeft" &&
        e.key !== "ArrowRight"
      )
        return;

      // TODO: Debug keyboard prompt on every device again.
      // console.log(piece, e.key, document.activeElement);

      // Remap `Unidentified` to `Backspace` for the Meta Quest Browser. 22.10.24.16.18
      let key = e.key;
      if (e.key === "Unidentified" && e.which === 8) key = "Backspace";
      // if (e.key === "Unidentified" && e.which === 13) key = "Enter";

      // Send a normal keyboard message if we are anywhere else.
      this.events.push({
        name: "keyboard:down:" + parseKey(key),
        key,
        repeat: e.repeat || repeat,
        shift: e.shiftKey,
        alt: e.altKey,
        ctrl: e.ctrlKey,
      });

      if (e.key === "ArrowUp" || e.key === "Tab") e.preventDefault();
    });

    window.addEventListener("keyup", (e) => {
      this.#held.delete(e.key);
      this.events.push({
        name: "keyboard:up:" + parseKey(e.key),
        key: e.key,
      });
      this.#lastKeyDown = null;
    });

    document.addEventListener("visibilitychange", () => {
      if (document.hidden) {
        // Trigger keyup event for each held key
        this.#held.forEach((key) => {
          document.dispatchEvent(
            new KeyboardEvent("keyup", {
              key: key,
              bubbles: true,
              cancelable: true,
            }),
          );
        });
      }
    });
  }
}

function parseKey(key) {
  let parsedKey = key.toLowerCase();
  if (parsedKey === " ") parsedKey = "space";
  return parsedKey;
}
