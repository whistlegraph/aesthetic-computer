// The NES core: jsnes, with nothing around it. See src/cores.mjs for the
// contract every core keeps and for the host that runs them.
import { createRequire } from "node:module";

const jsnes = createRequire(import.meta.url)("./vendor/jsnes-2.1.0.min.cjs");
const WIDTH = 256, HEIGHT = 240;
const BUTTONS = {
  a: jsnes.Controller.BUTTON_A, b: jsnes.Controller.BUTTON_B,
  select: jsnes.Controller.BUTTON_SELECT, start: jsnes.Controller.BUTTON_START,
  up: jsnes.Controller.BUTTON_UP, down: jsnes.Controller.BUTTON_DOWN,
  left: jsnes.Controller.BUTTON_LEFT, right: jsnes.Controller.BUTTON_RIGHT,
};

export default {
  id: "nes",
  name: "NES",
  engine: "jsnes 2.1.0",
  extensions: ["nes"],
  mime: "application/x-nes-rom",
  width: WIDTH,
  height: HEIGHT,
  fps: 60,
  buttons: Object.keys(BUTTONS),

  boot(bytes) {
    const pixels = new Uint8Array(WIDTH * HEIGHT * 4).fill(255);
    let low = 0, high = 0;
    const nes = new jsnes.NES({
      // jsnes hands over one 0xBBGGRR integer a pixel.
      onFrame(buffer) {
        for (let i = 0, o = 0; i < buffer.length; i++, o += 4) {
          const c = buffer[i];
          pixels[o] = c & 255; pixels[o + 1] = (c >> 8) & 255; pixels[o + 2] = (c >> 16) & 255;
        }
      },
      onAudioSample(left) { if (left < low) low = left; if (left > high) high = left; },
    });
    nes.loadROM(Buffer.from(bytes).toString("binary"));
    return {
      step() { nes.frame(); },
      frame: () => pixels,
      press(button, down) { down ? nes.buttonDown(1, BUTTONS[button]) : nes.buttonUp(1, BUTTONS[button]); },
      // How far the speaker moved since the last asking: 0 is silence.
      loudness() { const swing = high - low; low = high = 0; return swing; },
      save: () => nes.toJSON(),
      restore(state) { nes.fromJSON(state); },
    };
  },
};
