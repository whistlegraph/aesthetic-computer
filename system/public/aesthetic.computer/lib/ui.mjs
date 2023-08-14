import { Box } from "./geo.mjs";
import { radians, p2 } from "./num.mjs";
const { round } = Math;

const spinnerDelayMax = 8; // Skip a few frames of the spinner.
let spinnerDelay = 0;

function spinnerReset() {
  spinnerDelay = 0;
}

// Loading icon.
function spinner(ctx, timePassed) {
  if (spinnerDelay < spinnerDelayMax) {
    spinnerDelay += 1;
    return;
  }

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
  ctx.translate(
    0,
    round(ctx.canvas.height / window.devicePixelRatio - s * 2 + 2)
  );

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
  btn;
  box;
  down = false;
  disabled = false;
  icon;
  dom = false;
  over = false; // Keep track of rollover state.

  // (x, y, width, height) or Box
  constructor() {
    if (arguments.length === 1) {
      // Assume we are passing in a box {x,y,w,h} object.
      this.box = Box.from(arguments[0]);
    } else this.box = new Box(...arguments); // Otherwise: x, y, w, h for a box.
    this.btn = this;
  }

  publishToDom({ send }, label, message) {
    // The only use case for this right now is the Clipboard API. 23.06.16.15.40
    // 📓 Where `message` is used as text to be copied.
    send({
      type: "button:hitbox:add",
      content: { box: this.box, label, message },
    });
  }

  removeFromDom({ send }, label) {
    send({ type: "button:hitbox:remove", content: label });
  }

  // For using in a piece's `act` function. Contains callbacks for
  // events that take place inside the button.
  // Usage:  act(e, () => {}); // For 'push' callback only.
  //         act(e, {push: () => {}, down: () => {}, cancel: () => {}, draw() => {}});
  // You can optionally pass in an array of `pens` {x, y} for multi-touch support.
  act(e, callbacks = () => {}, pens = []) {
    const btn = this.btn;
    if (btn.disabled) return;

    // If only a single function is sent, then assume it's a button push callback.
    if (typeof callbacks === "function") callbacks = { push: callbacks };

    // 1. Down: Enable the button if we touched over it. (Repeatable)
    if (e.is("touch:any") && btn.box.contains(e) /*&& !btn.down*/) {
      callbacks.down?.(btn);
      btn.down = true;
      btn.over = true;
    }

    // 3. Push: Trigger the button if we push it.
    if (e.is("lift:any") && btn.down) {
      if (
        (pens.length > 0 && btn.box.onlyContains(e.pointer - 1, pens)) ||
        btn.box.contains(e)
      ) {
        callbacks.push?.(btn);
        btn.down = false;
        btn.over = false;
      } else if (btn.box.containsNone(pens) || !btn.box.contains(e)) {
        callbacks.cancel?.(btn);
        btn.down = false;
        btn.over = false;
      }
    }

    // Note: Each piece may use the below to implement custom rolling behavior,
    //       which often differs among use cases such as pianos or general GUIs.

    // 4. Rollover: Run a rollover event if dragged on.
    // if (e.is("draw:any") && !this.down && this.box.contains(e)) {
    if (e.is("draw:any") && !btn.over && btn.box.contains(e)) {
      callbacks.rollover?.(btn);
      btn.over = true;
    }

    // 5. Rollout: Run a rollout event if dragged off.
    if (
      e.is("draw:any") &&
      btn.over &&
      !btn.box.contains(e) &&
      btn.box.containsNone(pens)
    ) {
      callbacks.rollout?.(btn);
      btn.over = false;
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

class TextButton {
  txt;
  btn;

  #cw = 6;
  #gap = 4;
  #g2 = this.#gap * 2;
  #offset = { x: this.#gap, y: this.#gap };
  #h = 19;

  constructor(text = "Button", pos = { x: 0, y: 0 }) {
    this.txt = text;
    this.btn = new Button(this.#computePosition(text, pos));
  }

  get act() {
    return this.btn.act;
  }

  set disabled(d) {
    return (this.btn.disabled = d);
  }

  get disabled() {
    return this.btn.disabled;
  }

  get down() {
    return this.btn.down;
  }

  set down(d) {
    return (this.btn.down = d);
  }

  // Compute position for box.
  // pos: {x, y} or { top, left } for positioning.
  // pos: {bottom, right} for bottom right...
  //      { center: "xy", screen } for screen centering.
  #computePosition(text, pos = { x: 0, y: 0 }) {
    pos = { ...pos }; // Make a shallow copy of pos because we will mutate it.
    let x, y;
    const w = text.length * this.#cw + this.#g2;
    const h = this.#h;

    if (pos.center === "xy") {
      return {
        x: pos.screen.width / 2 - w / 2,
        y: pos.screen.height / 2 - h / 2,
        w,
        h,
      };
    }

    if (pos.x !== undefined && pos.y !== undefined) {
      // Position from top left if x and y are set on pos
      x = pos.x;
      y = pos.y;
    } else {
      // Compute "bottom" and "right" properties if they exist.
      if (pos.bottom !== undefined) {
        y = pos.screen.height - pos.bottom - this.#h;
      } else {
        y = pos.top || 0;
      }

      if (pos.right !== undefined) {
        x = pos.screen.width - pos.right - w;
      } else {
        x = pos.left || 0;
      }
    }

    return { x, y, w, h };
  }

  reposition(pos, txt) {
    if (txt) this.txt = txt;
    this.btn.box = Box.from(this.#computePosition(this.txt, pos));
  }

  paint(
    $,
    // scheme = [
    //   [0, 100, 0],
    //   [0, 255, 0, 150],
    //   [0, 200, 0],
    //   [0, 50, 0],
    // ],
    scheme = [0, 255, 255, 0],
    hoverScheme = [255, 0, 0, 255],
    disabledScheme = [64, 127, 127, 64]
  ) {
    let s;
    if (this.btn.disabled) {
      s = disabledScheme;
    } else {
      s = this.btn.down ? hoverScheme : scheme;
    }

    $.ink(s[0])
      .box(this.btn.box, "fill")
      .ink(s[1])
      .box(this.btn.box, "outline")
      .ink(s[2])
      .write(this.txt, p2.add(this.btn.box, this.#offset), s[3]);
  }
}

export { spinner, spinnerReset, cached, Button, TextButton };
