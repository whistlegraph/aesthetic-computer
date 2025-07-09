import { Box } from "./geo.mjs";
import { radians, p2 } from "./num.mjs";
const { round } = Math;

const spinnerDelayMax = 8; // Skip a few frames of the spinner.
let spinnerDelay = 0;

let TYPEFACE_UI; // A default `Typeface` instance carried over from `disk`.

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

// Global tracking for drag-between-buttons behavior
const activeButtons = new Set(); // Track which buttons are currently active

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
  downPointer; // Keep track of what original pointer downed the button.
  actions; // A held list of callbacks for virtually triggering events.
  noEdgeDetection = false; // Set to true to opt out of global edge detection cancellation
  noRolloverActivation = false; // Set to true to prevent activation via rollover from other buttons
  stickyScrubbing = false; // Set to true to prevent rollover activation when scrubbing from another button
  offScreenScrubbing = false; // Set to true to allow scrubbing to continue off-screen but still allow horizontal rollover

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
    if (btn.disabled) {
      return;
    }

    // If only a single function is sent, then assume it's a button push callback.
    if (typeof callbacks === "function") callbacks = { push: callbacks };
    btn.actions = callbacks;

    // Handle global edge detection cancellation
    if (e.is("ui:cancel-interactions") && !btn.noEdgeDetection) {
      // Only cancel if the button is still down (not already cancelled by other means)
      // For offScreenScrubbing buttons, don't cancel if cursor is horizontally within bounds
      // For stickyScrubbing buttons, always cancel when mouse leaves screen to prevent sounds getting stuck
      const shouldCancel = btn.down && (!btn.offScreenScrubbing || 
        (btn.offScreenScrubbing && (e.x < btn.box.x || e.x >= btn.box.x + btn.box.w)));
      
      if (shouldCancel) {
        btn.down = false;
        btn.over = false;
        btn.downPointer = undefined;
        activeButtons.delete(btn); // Remove from activeButtons Set
        callbacks.cancel?.(btn);
      }
      return;
    }

    const t = this.multitouch ? "any" : "1";

    // 1. Down: Enable the button if we touched over it. (Repeatable)
    if (e.is(`touch:${t}`) && btn.box.contains(e) && !btn.down) {
      // Prevent immediate re-activation if this button was just processed
      if (btn._justProcessed) {
        return;
      }
      
      const downed = callbacks.down?.(btn);
      btn.down = downed || downed === undefined ? true : false;
      if (btn.down && btn.downPointer === undefined)
        btn.downPointer = e.pointer || 0;
      btn.over = btn.down;
      // Add to active buttons set
      if (btn.down) {
        activeButtons.add(btn);
      }
    }

    // 3. Push: Trigger the button if we push it.
    if (e.is(`lift:${t}`) && btn.down) {
      function up() {
        const up = callbacks.up?.(btn);
        if (up === false) {
          btn.down = true;
          btn.over = true;
        } else {
          btn.down = false;
          btn.over = false;
          btn.downPointer = undefined;
          activeButtons.delete(btn);
        }
      }

      // Check if this is a valid button push or should be cancelled
      const isValidPush = 
        // Multi-touch case: all pens are outside box but lift event is inside
        (pens.length > 1 &&
          btn.box.containsNone(pens) &&
          btn.box.contains(e)) ||
        // Single touch case: lift event is inside box
        ((!pens || pens.length <= 1) && btn.box.contains(e));

      if (isValidPush) {
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
        activeButtons.delete(btn);
        callbacks.push?.(btn);
        
        // Mark as just processed to prevent immediate re-activation
        btn._justProcessed = true;
        btn._lastProcessedTime = Date.now();
        setTimeout(() => {
          btn._justProcessed = false;
        }, 250); // Increase to 250ms to ensure lift events are handled
        
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
        activeButtons.delete(btn);
        callbacks.cancel?.(btn);
        up();
        //console.log("Button up (cancel):", btn, pens);
      }
    }

    // Note: Each piece may use the below to implement custom rolling behavior,
    //       which often differs among use cases such as pianos or general GUIs.

    // 4. Rollover: Run a rollover event if dragged on.
    // if (e.is("draw:any") && !this.down && this.box.contains(e)) {

    // For offScreenScrubbing, check if cursor is horizontally within bounds
    const horizontallyWithin =
      btn.offScreenScrubbing && e.x >= btn.box.x && e.x < btn.box.x + btn.box.w;

    if (
      e.is(`draw:${t}`) &&
      !btn.over &&
      (btn.box.contains(e) || horizontallyWithin)
    ) {
      // Check if we're dragging from another button
      const isDraggingFromOtherButton = activeButtons.size > 0 && !btn.down;

      // Check if any active button has sticky scrubbing enabled
      const hasStickyButton = Array.from(activeButtons).some(
        (activeBtn) => activeBtn.stickyScrubbing,
      );

      // Only prevent rollover activation if:
      // 1. We're dragging from another button AND
      // 2. That other button has sticky scrubbing enabled AND
      // 3. This button doesn't allow rollover activation
      const shouldPreventRollover = isDraggingFromOtherButton && 
        hasStickyButton && 
        btn.noRolloverActivation;

      if (
        isDraggingFromOtherButton &&
        !shouldPreventRollover &&
        (btn.box.contains(e) || horizontallyWithin)
      ) {
        // Only allow rollover activation if conditions are met
        // Deactivate all other buttons first (except sticky ones that are actively being used)
        for (const otherBtn of activeButtons) {
          if (otherBtn !== btn && !otherBtn.stickyScrubbing) {
            otherBtn.down = false;
            otherBtn.over = false;
            otherBtn.actions?.up?.(otherBtn);
            activeButtons.delete(otherBtn);
          }
        }

        // Only activate this button if no sticky button conflicts
        if (!hasStickyButton || btn.stickyScrubbing) {
          btn.down = true;
          btn.downPointer = e.pointer || 0;
          activeButtons.add(btn);
          callbacks.down?.(btn);
        }
      }

      // Always allow rollover callbacks for visual feedback
      if (
        !shouldPreventRollover ||
        horizontallyWithin
      ) {
        if (callbacks.rollover) {
          callbacks.rollover(btn);
        } else {
          callbacks.over?.(btn);
        }
        btn.over = true;
      }
    }

    // Scrub condition: allow if drag is within bounds OR if button is active from rollover
    // For sticky scrubbing, allow scrubbing even when dragging outside bounds
    // For offScreenScrubbing, allow scrubbing when horizontally within bounds even if vertically off-screen
    const containsDrag = btn.box.contains(e.drag);
    const inActiveButtons = activeButtons.has(btn);
    const horizontallyWithinForOffScreen =
      btn.offScreenScrubbing &&
      e.drag &&
      e.drag.x >= btn.box.x &&
      e.drag.x < btn.box.x + btn.box.w;
    const allowScrub =
      containsDrag ||
      (inActiveButtons && !btn.stickyScrubbing) ||
      (btn.stickyScrubbing && btn.down) ||
      (btn.offScreenScrubbing && btn.down && horizontallyWithinForOffScreen);

    if (e.is(`draw:${t}`) && btn.down && allowScrub) {
      // Allow scrubbing if pointers match OR if this button was activated via rollover
      // For sticky scrubbing, always allow if the button is down regardless of pointer position
      // For offScreenScrubbing, allow if horizontally within bounds
      if (
        e.pointer === btn.downPointer ||
        (inActiveButtons && !btn.stickyScrubbing) ||
        (btn.stickyScrubbing && btn.down) ||
        (btn.offScreenScrubbing && btn.down && horizontallyWithinForOffScreen)
      ) {
        callbacks.scrub?.(btn);
      }
    }

    // 5. Rollout: Run a rollout event if dragged off.
    if (
      e.is(`draw:${t}`) &&
      btn.over &&
      !btn.box.contains(e) &&
      btn.box.containsNone(pens)
    ) {
      // For offScreenScrubbing buttons, only trigger rollout if we're also outside horizontal bounds
      const shouldRollout =
        !btn.offScreenScrubbing ||
        (btn.offScreenScrubbing &&
          (e.x < btn.box.x || e.x >= btn.box.x + btn.box.w));

      if (shouldRollout) {
        // Only truly deactivate if we're not dragging to another button
        // The rollover on the new button will handle the transition
        if (callbacks.rollout) {
          callbacks.rollout(btn);
        } else {
          callbacks.out?.(btn);
        }
        // console.log("Button out (rollout):", btn, pens);
        btn.over = false;
      }
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

  #gap = 4;
  #cw = 6; // Character width in pixels. Set from `typeface`.
  #g2 = this.#gap * 2;
  #h = 12 + this.#g2; // 19; //
  #offset = { x: this.#gap, y: this.#gap };

  constructor(text = "Button", pos = { x: 0, y: 0 }, typeface = TYPEFACE_UI) {
    this.#cw = typeface.blockWidth;
    this.#h = typeface.blockHeight + this.#gap * 2;

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

  get noEdgeDetection() {
    return this.btn.noEdgeDetection;
  }

  set noEdgeDetection(value) {
    this.btn.noEdgeDetection = value;
  }

  get stickyScrubbing() {
    return this.btn.stickyScrubbing;
  }

  set stickyScrubbing(value) {
    this.btn.stickyScrubbing = value;
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

function setTypeface(tf) {
  TYPEFACE_UI = tf;
}

export { spinner, spinnerReset, cached, Button, TextButton, setTypeface };
