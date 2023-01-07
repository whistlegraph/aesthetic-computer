// ✍️ Pen

// Multi-Touch Story
// Use a concept of 'primary' through nth-pointers.
// The 'primary' behavior is already defined below.
// And now `nth-pointers` can be tacked on.

// TODO
// - [-] Add mouse support for handling multiple mouse button presses:
//      See also: https://github.com/testing-library/user-event/issues/803p
// - [] Add game controller / axis support.

import { Point } from "./geo.mjs";

const { assign } = Object;
const { round } = Math;
const debug = window.acDEBUG;

class Pointer {
  x;
  y;
  px;
  py;
  delta;
  pressure;
  device;
  pointerId;
  pointerIndex;
  isPrimary;
  button;
  untransformedPosition;
  drawing = false;
  penDragStartPos;
  dragBox;
  // These are only used to calculate the liminal event delta.
  lastEventX;
  lastEventY;
}

export class Pen {
  // Global data for the overall pen system.
  point; // + Used globally (transform to screen space fn)
  changedInPiece = false; // + Used globally.
  events = []; // + Used globally to hold all events.

  #lastP; // + Used globally, in the renderer.
  cursorCode; // + Used globally, in the renderer.
  penCursor = false; // + Used globally, in the renderer.

  pointers = {}; // Stores an object of `Pointers` to keep track of each gesture.

  get pointerCount() {
    return Object.keys(this.pointers).length;
  }

  // `point` is a transform function for projecting coordinates from screen
  // space to virtual screen space.
  constructor(point) {
    this.point = point;

    // Add pointer events.
    const pen = this;

    // Prevent double-tap delay: https://stackoverflow.com/a/71025095
    window.addEventListener(
      "touchend" || "dblclick",
      (event) => {
        // Only prevent double tap to Zoom if native-cursor is enabled.
        if (document.body.classList.contains("native-cursor") === false) {
          event.preventDefault();
          event.stopImmediatePropagation();
        }
      },
      {
        passive: false,
      }
    );

    // Prevent context click.
    window.addEventListener("contextmenu", function (e) {
      e.preventDefault();
    });

    // Mouse only...
    window.addEventListener("mousedown", (e) => {
      const pointer = pen.pointers[1];
      if (!pointer) return;

      if (pointer.button !== e.button) {
        if (!pointer.buttons.includes(e.button)) pointer.buttons.push(e.button);
        const newPointer = { ...pointer };
        newPointer.button = e.button;
        pen.#event("touch", newPointer);
      }
    });

    window.addEventListener("mouseup", (e) => {
      const pointer = pen.pointers[1];
      if (!pointer) return;

      const buttonIndex = pointer.buttons.indexOf(e.button);
      if (buttonIndex > -1) pointer.buttons.splice(buttonIndex, 1);

      if (pointer.button !== e.button) {
        const newPointer = { ...pointer };
        newPointer.button = e.button;
        pen.#event("lift", newPointer);
      }
    });

    // ***Touch***
    window.addEventListener("pointerdown", (e) => {
      // Make sure the pointer we are using is already being tracked.
      let pointer = pen.pointers[e.pointerId];

      // If it doesn't exist, then make a new pointer and push to pointers.
      if (!pointer) {
        // Create a new `Pointer` to track an individual gesture.
        pointer = new Pointer();

        // Assign data to individual pointer.
        assign(pointer, point(e.x, e.y));

        pointer.px = pointer.x;
        pointer.py = pointer.y;

        pointer.untransformedPosition = { x: e.x, y: e.y };
        pointer.pressure = reportPressure(e);
        //pointer.down = true;
        //console.log("pointerdown", pointer.down);
        pointer.drawing = true;
        pointer.button = e.button; // Should this be deprecated? 22.11.07.22.13
        pointer.buttons = [e.button];

        pointer.penDragStartPos = { x: pointer.x, y: pointer.y };

        pointer.device = e.pointerType;
        pointer.pointerId = e.pointerId;
        pointer.isPrimary = e.isPrimary;
        pointer.pointerIndex = this.pointerCount;

        pen.pointers[e.pointerId] = pointer;
      } else {
        pointer.button = e.button; // Should this be deprecated? 22.11.07.22.13
        pointer.buttons = [e.button];
        pointer.drawing = true;
        pointer.penDragStartPos = { x: pointer.x, y: pointer.y };
      }

      // Set `pen` globals.
      pen.penCursor = true;
      if (e.device !== "mouse") pen.penCursor = false;
      pen.#event("touch", pointer);
    });

    // ***Move (Hover) and Draw (Drag)***
    window.addEventListener("pointermove", (e) => {
      // Make sure the pointer we are using is already being tracked.
      let pointer = pen.pointers[e.pointerId];

      // If it doesn't exist, then make a new pointer and push to pointers.
      if (!pointer) {
        pointer = new Pointer();
        assign(pointer, point(e.x, e.y));
        pointer.px = pointer.x;
        pointer.py = pointer.y;
        pointer.untransformedPosition = { x: e.x, y: e.y };
        pointer.pressure = reportPressure(e);
        pointer.button = e.button; // Should this be deprecated? 22.11.07.22.13
        pointer.buttons = [e.button];
        pointer.device = e.pointerType;
        pointer.pointerId = e.pointerId;
        pointer.isPrimary = e.isPrimary;
        pointer.pointerIndex = this.pointerCount;
        pen.pointers[e.pointerId] = pointer;
      }

      // console.log("BXY:", e.x, e.y);

      assign(pointer, point(e.x, e.y));

      // console.log("AXY:", pointer.x, pointer.y);

      pointer.untransformedPosition = { x: e.x, y: e.y };
      pointer.pressure = reportPressure(e);

      // const delta = {
      //   x: pointer.x - pointer.px || 0,
      //   y: pointer.y - pointer.py || 0,
      // };

      // pointer.delta = delta;

      if (pointer.drawing) {
        const penDragAmount = {
          x: pointer.x - pointer.penDragStartPos.x,
          y: pointer.y - pointer.penDragStartPos.y,
        };

        pointer.dragBox = {
          x: pointer.penDragStartPos.x,
          y: pointer.penDragStartPos.y,
          w: penDragAmount.x,
          h: penDragAmount.y,
        };
        // Only send an event if the new point differs from the last.
        pointerMoveEvent("draw", pointer);
      } else {
        pointerMoveEvent("move", pointer);
      }

      // TODO: This could be renamed? 22.09.30.10.56
      pen.changedInPiece = true; //delta.x !== 0 || delta.y !== 0;

      // Set `pen` globals.
      pen.penCursor = true;
      if (e.pointerType !== "mouse") pen.penCursor = false;
    });

    function pointerMoveEvent(type, pointer) {
      if (!Point.equals(pointer, { x: pointer.px, y: pointer.py })) {
        pen.#event(type, pointer);
      }
    }

    // ***Lift***
    window.addEventListener("pointerup", (e) => {
      const pointer = pen.pointers[e.pointerId];
      if (!pointer) return;

      if (pointer.drawing) pen.#event("lift", pointer);

      pointer.drawing = false;

      pen.penCursor = true;
      if (e.pointerType !== "mouse") pen.penCursor = false;

      // Delete pointer only if we are using touch.
      if (e.pointerType === "touch") delete pen.pointers[e.pointerId];

      if (debug)
        console.log("Removed pointer by ID:", e.pointerId, this.pointers);
    });

    // Mousewheel
    // https://developer.mozilla.org/en-US/docs/Web/API/Element/wheel_event
    window.addEventListener("wheel", (e) => {
      // Get the wheel direction... probably from the mouse.
      const dir = Math.sign(e.deltaY);
      pen.events.push({ name: "wheel", dir, device: "n/a", isPrimary: true });
    });

    // Pressure Detection
    let forceTouchPressure = 0;
    let forceTouchEnabled = false;

    // MacBook Trackpad Pressure (in Safari)
    // TODO: When shipping natively for macOS:
    //       - Report or re-report actual pen events for:
    //         https://developer.mozilla.org/en-US/docs/Web/API/Force_Touch_events
    // When webkitForce > 2 the button is held down quickly,
    // so we don't report anything. (It's a separate gesture)
    // Otherwise, normalize the pressure from 0-1.
    // Note: e.webkitForce reports from 1-3 by default.
    window.addEventListener("webkitmouseforcechanged", (e) => {
      forceTouchEnabled = true;
      if (e.webkitForce >= 2) {
        forceTouchPressure = 0;
      } else {
        forceTouchPressure = Math.max(0, e.webkitForce - 1);
      }
    });

    function reportPressure(e) {
      let pressure;
      // If the device is a trackpad (probably on a MacBook and in Safari)
      if (forceTouchEnabled) {
        pressure = forceTouchPressure;
      } else {
        // If pressure sensitivity doesn't exist then force it to be 1.
        pressure = e.pressure || 1;
        // Unless the device type is a pen, then make it 0. This assumes all pens
        // have pressure sensitivity.
        if (pen.device === "pen" && pressure === 1) {
          pressure = 0;
        }
        // If the device is a mouse, then set it to 1.
        if (pen.device === "mouse") pressure = 1;
      }
      return pressure;
    }

    return pen;
  }

  // TODO: Fix pointer delta issues.
  updatePastPositions() {
    for (const pointer in this.pointers) {
      this.pointers[pointer].px = this.pointers[pointer].x;
      this.pointers[pointer].py = this.pointers[pointer].y;
    }
  }

  retransformPosition() {
    assign(
      this,
      this.point(this.untransformedPosition?.x, this.untransformedPosition?.y)
    );
  }

  normalizedPosition(rect) {
    if (this.untransformedPosition) {
      return {
        x: (this.untransformedPosition.x - rect.x) / rect.width,
        y: (this.untransformedPosition.y - rect.y) / rect.height,
      };
    } else {
      return { x: undefined, y: undefined };
    }
  }

  // TODO: Merge this logic into the above events & consolidate class properties.
  // Check the hardware for any changes.
  #event(name, pointer) {
    const pen = this;

    // 💁 Calculate pointer delta based on the pointer's lastEvent so it
    //    can be cleared for each subsequent event.
    //    This is different from px and py, which do not reset and keep
    //    track of the last position.
    pointer.delta = {
      x: pointer.x - pointer.lastEventX || 0,
      y: pointer.y - pointer.lastEventY || 0,
    };

    pen.events.push({
      name,
      device: pointer.device,
      id: pointer.pointerId,
      isPrimary: pointer.isPrimary,
      // index: pointer.pointerIndex, // 0 based index of pointers.
      pointer: pointer.pointerIndex + 1, // 1 based index of pointers.
      button: pointer.button,
      buttons: pointer.buttons,
      x: pointer.x,
      y: pointer.y,
      px: pointer.px,
      py: pointer.py,
      delta: pointer.delta,
      pressure: pointer.pressure,
      drag: pointer.dragBox,
      penChanged: this.changedInPiece,
    });

    // Assign data to individual pointer.
    pointer.lastEventX = pointer.x;
    pointer.lastEventY = pointer.y;
  }

  render(ctx, bouRect) {
    // TODO: How to get the primary pointer from pointers?
    const pointer = this.pointers[1];

    if (!pointer) return;

    const p = pointer.untransformedPosition;

    const s = 10 + 4,
      r = bouRect;

    // Erase the last cursor that was drawn.
    if (!this.#lastP) this.#lastP = { x: p.x, y: p.y };
    else
      ctx.clearRect(
        this.#lastP.x - r.x - s,
        this.#lastP.y - r.y - s,
        s * 2,
        s * 2
      );

    assign(this.#lastP, p);

    // Remove native cursor if it was turned off.
    if (this.cursorCode != "native") {
      if (document.body.classList.contains("native-cursor")) {
        document.body.classList.remove("native-cursor");
      }
    }

    if (!this.cursorCode || this.cursorCode === "precise") {
      // 🎯 Precise
      ctx.lineCap = "round";

      ctx.save();
      ctx.translate(round(p.x - r.x), round(p.y - r.y));

      // A. Make circle in center.
      const radius = 2;
      ctx.beginPath();
      ctx.arc(0, 0, radius, 0, 2 * Math.PI);

      ctx.fillStyle = "white";
      ctx.fill();

      const gap = 7.5,
        to = 10;

      ctx.beginPath();
      ctx.moveTo(0, -gap); // Over
      ctx.lineTo(0, -to);
      ctx.moveTo(0, gap); // Under
      ctx.lineTo(0, to);
      ctx.moveTo(-gap, 0); // Left
      ctx.lineTo(-to, 0);
      ctx.moveTo(gap, 0); // Right
      ctx.lineTo(to, 0);

      ctx.strokeStyle = "rgb(0, 255, 255)";
      ctx.lineWidth = 4;
      ctx.stroke();
      ctx.restore();
    } else if (this.cursorCode === "tiny") {
      // 🦐 Tiny
      const l = 4;
      ctx.save();
      ctx.translate(round(p.x - r.x), round(p.y - r.y));

      ctx.beginPath();
      ctx.moveTo(0, -l); // Over
      ctx.lineTo(0, -l);
      ctx.moveTo(0, l); // Under
      ctx.lineTo(0, l);
      ctx.moveTo(-l, 0); // Left
      ctx.lineTo(-l, 0);
      ctx.moveTo(l, 0); // Right
      ctx.lineTo(l, 0);

      ctx.strokeStyle = "rgba(255, 255, 0, 0.75)";
      ctx.lineWidth = 4;
      ctx.stroke();
      ctx.restore();
    } else if (this.cursorCode === "dot") {
      ctx.save();
      ctx.translate(round(p.x - r.x), round(p.y - r.y));
      ctx.beginPath();
      ctx.lineTo(0, 0); // bottom right

      ctx.strokeStyle = "rgba(255, 0, 0, 0.9)";
      ctx.lineWidth = 4;
      ctx.stroke();
      ctx.restore();
    } else if (this.cursorCode === "none") {
      // ...
    } else if (this.cursorCode === "native") {
      if (document.body.classList.contains("native-cursor") === false) {
        document.body.classList.add("native-cursor");
      }
    }
  }

  setCursorCode(code) {
    this.cursorCode = code;
  }
}
