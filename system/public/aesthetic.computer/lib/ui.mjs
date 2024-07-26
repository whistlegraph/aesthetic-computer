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
    round(ctx.canvas.height / window.devicePixelRatio - s * 2 + 2),
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

// An interactive button model.
class Button {
  btn;
  box;
  down = false;
  disabled = false;
  icon;
  dom = false;
  over = false; // Keep track of rollover state.
  multitouch = true; // Toggle to false to make a single touch button2.
  actions; // A held list of callbacks for virtually triggering events.

  get up() {
    return !this.down;
  }

  set up(value) {
    this.down = !value;
  }

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
    btn.actions = callbacks;

    const t = this.multitouch ? "any" : "1";

    // 1. Down: Enable the button if we touched over it. (Repeatable)
    if (e.is(`touch:${t}`) && btn.box.contains(e) && !btn.down) {
      const downed = callbacks.down?.(btn);
      btn.down = downed || downed === undefined ? true : false;
      btn.over = btn.down;
    }

    // 3. Push: Trigger the button if we push it.
    if (e.is(`lift:${t}`) && btn.down) {
      function up() {
        const up = callbacks.up?.(btn);
        if (up === false) {
          btn.down = true;
          btn.over = true;
        }
      }


//        (pens.length > 0 && btn.box.onlyContains(e.pointer - 1, pens)) ||
 //       btn.box.contains(e)

      // console.log("Pens:", pens);

      if (
        (pens.length > 1 &&
          btn.box.containsNone(pens) &&
          btn.box.contains(e)) ||
        //(pens.length > 0 && btn.box.onlyContains(e.pointer - 1, pens)) ||
        ((!pens || pens.length <= 1) && btn.box.contains(e))
      ) {
        // console.log(
        //   "Button up (push):",
        //   btn,
        //   pens,
        //   "onlyContains:",
        //   btn.box.onlyContains(e.pointer - 1, pens),
        //   "boxcontains:",
        //   btn.box.contains(e),
        //   "pointer:",
        //   e.pointer,
        // );
        btn.down = false;
        btn.over = false;
        callbacks.push?.(btn);
        up();
      } else if (
        btn.box.containsNone(pens) ||
        ((!pens || pens.length === 0) && !btn.box.contains(e))
      ) {
        // console.log(
        //   "contains no pens!?",
        //   btn.box.containsNone(pens),
        //   pens,
        //   "contains e:",
        //   btn.box.contains(e),
        // );
        btn.down = false;
        btn.over = false;
        callbacks.cancel?.(btn);
        up();
        //console.log("Button up (cancel):", btn, pens);
      }
    }

    // Note: Each piece may use the below to implement custom rolling behavior,
    //       which often differs among use cases such as pianos or general GUIs.

    // 4. Rollover: Run a rollover event if dragged on.
    // if (e.is("draw:any") && !this.down && this.box.contains(e)) {
    if (e.is(`draw:${t}`) && !btn.over && btn.box.contains(e)) {
      if (callbacks.rollover) {
        callbacks.rollover(btn);
      } else {
        callbacks.over?.(btn);
      }
      btn.over = true;
    }

    // 5. Rollout: Run a rollout event if dragged off.
    if (
      e.is(`draw:${t}`) &&
      btn.over &&
      !btn.box.contains(e) &&
      btn.box.containsNone(pens)
    ) {
      if (callbacks.rollout) {
        callbacks.rollout(btn);
      } else {
        callbacks.out?.(btn);
      }
      console.log("Button out (rollout):", btn, pens);
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
    this.btn = new Button(this.#computePosition(text, { ...pos }));
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

  get width() {
    return this.txt.length * this.#cw + this.#gap * 2;
  }

  get height() {
    return this.#h;
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

    if (pos.screen) {
      pos.screen.x = pos.screen.x || 0;
      pos.screen.y = pos.screen.y || 0;
    }

    if (pos.center === "xy") {
      return {
        x: pos.screen.x + (pos.x || 0) + pos.screen.width / 2 - w / 2,
        y: pos.screen.y + (pos.y || 0) + pos.screen.height / 2 - h / 2,
        w,
        h,
      };
    }

    // Position from top left if x and y are set on pos
    x = (pos.screen?.x || 0) + (pos.x || 0);
    y = (pos.screen?.y || 0) + (pos.y || 0);

    // Compute "bottom" and "right" properties if they exist.
    if (pos.bottom !== undefined) {
      y += pos.screen.height - pos.bottom - this.#h;
    } else {
      y += pos.top || 0;
    }

    if (pos.right !== undefined) {
      x += pos.screen.width - pos.right - w;
    } else {
      x += pos.left || 0;
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
    disabledScheme = [64, 127, 127, 64],
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
