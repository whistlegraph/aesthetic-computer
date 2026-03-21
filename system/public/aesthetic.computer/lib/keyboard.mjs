// ⌨ Keyboard
import { MetaBrowser } from "./platform.mjs";

// TODO: Add more of these properties as needed:
//       https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent

export class Keyboard {
  events = [];
  #lastKeyCodeDown;
  input;
  focusHandler;
  #held = new Set();
  #rebootSequence = []; // Track Ctrl+R+B sequence for reboot, Ctrl+R+R for quick tape
  #sequenceTimeout = null;
  needsImmediateOpen = false;

  constructor() {
    window.addEventListener("keydown", (e) => {
      // console.log("⌨️ Key event:", e);

      this.#held.add(e.key);
      // Firefox "repeat" seems to be broken on linux, so here is
      // some redundancy. 22.07.29.17.43
      const repeat = e.code === this.#lastKeyCodeDown;
      this.#lastKeyCodeDown = e.code;

      // 🔄 Global Ctrl+R sequences: Ctrl+R+B (reboot), Ctrl+R+R (quick tape)
      if (e.ctrlKey && e.key.toLowerCase() === "r") {
        e.preventDefault(); // Prevent browser reload
        this.#rebootSequence = ["r"];
        // Clear sequence after 1 second if no follow-up
        clearTimeout(this.#sequenceTimeout);
        this.#sequenceTimeout = setTimeout(() => {
          this.#rebootSequence = [];
        }, 1000);
        return; // Don't process this event further
      } else if (this.#rebootSequence.length === 1 && this.#rebootSequence[0] === "r" && e.key.toLowerCase() === "b") {
        e.preventDefault();
        clearTimeout(this.#sequenceTimeout);
        this.#rebootSequence = [];
        console.log("🔄 Reboot sequence triggered: Ctrl+R+B");
        location.reload(); // Reload the page
        return; // Don't process this event further
      } else if (this.#rebootSequence.length === 1 && this.#rebootSequence[0] === "r" && e.key.toLowerCase() === "r") {
        // 📼 Quick tape sequence: Ctrl+R+R
        e.preventDefault();
        clearTimeout(this.#sequenceTimeout);
        this.#rebootSequence = [];
        console.log("📼 Quick tape sequence triggered: Ctrl+R+R");
        // Send a keyboard event that disk.mjs can listen for
        this.events.push({
          name: "keyboard:quick-tape",
          key: "QuickTape",
          ctrl: true,
        });
        return; // Don't process this event further
      } else if (this.#rebootSequence.length > 0 && e.key.toLowerCase() !== "control") {
        // Any other key cancels the sequence (except holding Ctrl)
        clearTimeout(this.#sequenceTimeout);
        this.#rebootSequence = [];
      }

      // Send a parent message to defocus the ac extension.
      if (e.key === "a" && e.ctrlKey && e.altKey) {
        // console.log("Sending defocus...");
        window.parent?.postMessage({ type: "vscode-extension:defocus" }, "*");
      }

      const keyboardFocused = this.focusHandler(e); // Focus DOM input field
      //                                               as neded for text entry.

      if (keyboardFocused) return;
      // Skip any initial keyboard event.

      // Skip sending most keyboard events from here if we are using text input
      // which generates a synthetic keyboard event back
      //  in `bios` under `Keyboard`
      const mediaKey = isMediaKey(e.key);
      if (
        document.activeElement === this.input &&
        // Remaps "Unidentified" to "Backspace" below, avoiding `Enter` code.
        //(MetaBrowser && e.which !== 13) &&
        !mediaKey &&
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
        code: e.code,
        repeat: e.repeat || repeat,
        shift: e.shiftKey,
        alt: e.altKey,
        ctrl: e.ctrlKey,
        velocity: e.velocity ?? 127, // Support velocity from custom events (0-127)
      });

      if (e.key === "ArrowUp" || e.key === "Tab") e.preventDefault();

      // Prevent Alt and Ctrl chrome shortcut.
      if (e.altKey && (e.key.toLowerCase() === "e" || e.key.toLowerCase() === "f" || e.key.toLowerCase() === "d")) e.preventDefault();
      if (e.ctrlKey && (e.key.toLowerCase() === "d" || e.key.toLowerCase() === "n" || e.key.toLowerCase() === "p" || e.key.toLowerCase() === "s")) e.preventDefault();
    });

    window.addEventListener("keyup", (e) => {
      this.#held.delete(e.key);
      this.events.push({
        name: "keyboard:up:" + parseKey(e.key),
        key: e.key,
      });
      this.#lastKeyCodeDown = null;
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

function isMediaKey(key) {
  if (!key) return false;
  return key.startsWith("Media") || key.startsWith("AudioVolume");
}
