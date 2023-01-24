import { Box } from "./geo.mjs";
import { radians } from "./num.mjs";
const { round } = Math;

// Loading icon.
function spinner(ctx, timePassed) {
  const gap = 20,
    s = 8;

  ctx.save();
  const scaledWidth = ctx.canvas.width / window.devicePixelRatio;
  ctx.translate(scaledWidth - (s + gap), s + gap);

  ctx.rotate(radians(timePassed % 360) * 1);

  ctx.beginPath();
  // \ of the X
  ctx.moveTo(-s, -s); // top left
  ctx.lineTo(s, s); // bottom right
  // / of the X
  //ctx.moveTo(-s, s); // bottom left
  //ctx.lineTo(s, -s); // top right

  ctx.strokeStyle = "rgb(255, 255, 0)";
  ctx.lineWidth = 4;
  ctx.lineCap = "round";
  ctx.stroke();

  ctx.restore();
}

// Paused icon.
function cached(ctx) {
  const gap = 4,
    s = 20;

  ctx.save();
  ctx.translate(round(gap / 2) + 6, round(gap / 2) + 4);

  // Bottom Left option.
  ctx.translate(0, round((ctx.canvas.height / window.devicePixelRatio) - (s * 2) + 2));


  ctx.beginPath();

  ctx.moveTo(gap, gap); // left
  ctx.lineTo(gap, s);
  ctx.moveTo(gap * 3.5, gap); // right
  ctx.lineTo(gap * 3.5, s);

  ctx.strokeStyle = "rgb(0, 255, 255)";
  ctx.lineWidth = 4;
  ctx.lineCap = "round";
  ctx.stroke();

  ctx.restore();
}

class Button {
  box;
  down = false;
  disabled = false;
  icon;

  // (x, y, width, height) or Box
  constructor() {
    if (arguments.length === 1) {
      // Assume we are passing in a box {x,y,w,h} object.
      this.box = Box.copy(arguments[0]);
    } else this.box = new Box(...arguments); // Otherwise: x, y, w, h for a box.
  }

  // For using in a piece's `act` function. Contains callbacks for
  // events that take place inside the button.
  // Usage:  act(e, () => {}); // For 'push' callback only.
  //         act(e, {push: () => {}, down: () => {}, cancel: () => {}, draw() => {}});
  // You can optionally pass in an array of `pens` {x, y} for multi-touch support.
  act(e, callbacks, pens = []) {
    if (this.disabled) return;

    // If only a single function is sent, then assume it's a button push callback.
    if (typeof callbacks === "function") callbacks = { push: callbacks };

    // 1. Down: Enable the button if we touched over it.
    if (e.is("touch:any") && this.box.contains(e) && !this.down) {
      callbacks.down?.();
      this.down = true;
    }

    // 3. Push: Trigger the button if we push it.
    if (e.is("lift:any")) {
      let event;
      if (
        ((pens.length > 0 && this.box.onlyContains(e.pointer - 1, pens)) ||
          this.box.contains(e)) &&
        this.down
      ) {
        event = "push";
        callbacks[event]?.();
        this.down = false;
      } else if (!this.box.contains(e) && this.box.containsNone(pens)) {
        event = "cancel"; // TODO: Is this necessary now that we have rollout? 22.08.29.23.11
        callbacks[event]?.();
        this.down = false;
      }
    }

    // Note: Each piece may use the below to implement custom rolling behavior,
    //       which often differs among use cases such as pianos or general GUIs.

    // 4. Rollover: Run a rollover event if dragged on.
    if (e.is("draw:any") && !this.down && this.box.contains(e)) {
      callbacks.rollover?.();
    }

    // 5. Rollout: Run a rollout event if dragged off.
    if (
      e.is("draw:any") &&
      this.down &&
      !this.box.contains(e) &&
      this.box.containsNone(pens)
    ) {
      callbacks.rollout?.();
    }
  }

  // Draws a callback if the button is not disabled.
  paint(fn) {
    if (!this.disabled) fn(this);
  }

  enableIf(flag) {
    this.disabled = !flag;
  }
}

export { spinner, cached, Button };
