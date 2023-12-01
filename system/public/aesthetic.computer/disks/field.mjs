// Field, 2023.11.30.16.05.21.050
// An open place to walk around.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Get multi-user networking online. 
  - [] Add a "special" command 😉. 
  - [] Make the world scrollable.
  - [] Move common functionality to a `world.mjs` library file.
  + Done
  - [x] Add an overhead chat display.
  - [x] Wire up tappable character button to activate the text input.
  - [x] Enter button should close empty prompt.
  - [x] Escape key should close prompt no matter what.
  - [x] Don't snap the cursor all the way back after hitting return
        / keep it at its position.
  - [x] Paste button does not appear when going back to the prompt
        from another piece after entering a single key.
  - [x] `Enter` button appears and disappears at weird times.
#endregion */

// 🧒
class Kid {
  pos = { x: 0, y: 0 };
  size = 16;
  leash = { x: 0, y: 0, len: 0, max: 12, deadzone: 8 };
  #keys = { U: false, D: false, L: false, R: false };
  #message;
  #messageDuration;
  #messageProgress = 0;

  constructor(handle, pos = this.pos) {
    console.log("🧒 From:", handle);
    this.pos = pos;
  }

  // Show a message above the kid's head for `time` frames.
  write(text, time = 240) {
    this.#message = text;
    this.#messageDuration = 120;
  }

  // Render the kid.
  paint({ ink, pan, text, typeface }) {
    const leash = this.leash;
    pan(this.pos.x, this.pos.y);
    ink("white").circle(0, 0, this.size);
    ink(leash.len > leash.deadzone ? "yellow" : "red").line(
      0,
      0,
      leash.x,
      leash.y,
    );
    if (this.#message) {
      const blockWidth = typeface.glyphs["0"].resolution[0];
      const tb = text.box(
        this.#message,
        undefined,
        this.#message.length * blockWidth,
      );
      ink("yellow").write(this.#message, {
        x: -tb.box.width / 2,
        y: -this.size - 12,
      });
    }
  }

  // Control the kid.
  act({ event: e, num }) {
    const k = this.#keys;
    const leash = this.leash;
    if (e.is("keyboard:down:w") || e.is("keyboard:down:arrowup")) k.U = true;
    if (e.is("keyboard:down:s") || e.is("keyboard:down:arrowdown")) k.D = true;
    if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft")) k.L = true;
    if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright")) k.R = true;
    if (e.is("keyboard:up:w") || e.is("keyboard:up:arrowup")) k.U = false;
    if (e.is("keyboard:up:s") || e.is("keyboard:up:arrowdown")) k.D = false;
    if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft")) k.L = false;
    if (e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) k.R = false;

    if (e.is("touch:1")) leash.start = { x: e.x, y: e.y };

    if (e.is("draw:1") && leash.start) {
      leash.x = e.x - leash.start.x;
      leash.y = e.y - leash.start.y;
      this.#snapLeash(num);
    }

    if (e.is("lift:1")) leash.start = null;
  }

  // Simulate the kid's movement and time messages.
  sim({ num }) {
    // 🗨️ Message
    if (this.#message) {
      if (this.#messageProgress < this.#messageDuration) {
        this.#messageProgress += 1;
      } else {
        this.#message = null;
        this.#messageProgress = 0;
      }
    }

    // 🏃 Movement
    const k = this.#keys,
      leash = this.leash,
      pos = this.pos;

    if (k.U) leash.y -= 1;
    if (k.D) leash.y += 1;
    if (k.L) leash.x -= 1;
    if (k.R) leash.x += 1;

    this.#snapLeash(num);

    if (!leash.start) {
      leash.y *= 0.97;
      leash.x *= 0.97;
    }

    if (leash.len > leash.deadzone) {
      pos.x = num.lerp(pos.x, pos.x + leash.x, 0.075);
      pos.y = num.lerp(pos.y, pos.y + leash.y, 0.075);
    } else if (leash.len > 1) {
      pos.x = num.lerp(pos.x, pos.x + leash.x, 0.025);
      pos.y = num.lerp(pos.y, pos.y + leash.y, 0.025);
    }
  }

  // Limit the kid's movement leash.
  #snapLeash(num) {
    const leash = this.leash;
    leash.len = num.p2.len(leash);
    if (leash.len > leash.max) {
      const scale = leash.max / leash.len;
      leash.x *= scale;
      leash.y *= scale;
    }
  }

  // Kill all controls.
  off() {
    const k = this.#keys;
    k.U = k.D = k.L = k.R = false;
    this.leash.start = null;
  }

  // Return the screen position of this kid, given a camera and world,
  screenPos(cam, world) {
    return { x: cam.x + this.pos.x, y: cam.y + this.pos.y };
  }
}

// 🌎
class World {
  size = {};
  constructor(width = 192, height = 192) {
    this.size.width = width;
    this.size.height = height;
  }

  paint({ ink }) {
    ink("green").box(0, 0, this.size.width, this.size.height);
  }
}

// 🎥
class Cam {
  x = 0;
  y = 0;

  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
}

let me, world, cam, input, inputBtn;

// 🥾 Boot
function boot({ api, wipe, handle, screen, ui, send }) {
  wipe(0);
  world = new World();
  me = new Kid(handle(), { x: world.size.width / 2, y: world.size.height / 2 });
  cam = new Cam(
    screen.width / 2 - world.size.width / 2,
    screen.height / 2 - world.size.height / 2,
  );

  const scheme = {
    dark: {
      fg: 255,
      bg: [0, 100],
      block: 255,
      blockHi: 0,
      line: 255,
    },
  };

  input = new ui.TextInput(
    api,
    ">",
    async (text) => {
      // Clear the text, hide the cursor block, and close the keyboard.
      me.write(input.text); // Activate a timed display message on 🧒.
      input.text = "";
      input.showBlink = false;
      send({ type: "keyboard:close" });
    },
    {
      // autolock: false,
      // wrap,
      scheme,
      // copied,
      // activated,
      // didReset: () => {
      // messageComplete = true;
      // },
      // gutterMax,
      // lineSpacing,
      closeOnEmptyEnter: true,
    },
  );

  inputBtn = new ui.Button();

  send({ type: "keyboard:soft-lock" });
}

// 🎨 Paint
function paint({ api, wipe, ink, pan, unpan, pen, screen }) {
  wipe(0); // Backdrop
  // 🌎 + 🧒 World & Players
  pan(cam.x, cam.y);
  world.paint(api);
  me.paint(api);
  unpan();

  // 💻 Screen UI
  const l = me.leash;
  if (l.start) {
    if (pen) ink(0, 255, 0, 90).line(l.start.x, l.start.y, pen.x, pen.y);
    ink(l.len > l.deadzone ? "yellow" : "red").line(
      l.start.x,
      l.start.y,
      l.start.x + me.leash.x,
      l.start.y + me.leash.y,
    );
  }

  if (input.canType) {
    input.paint(api, false, {
      x: 0,
      y: 18,
      width: screen.width,
      height: screen.height - 18,
    });
  }

  inputBtn.paint((btn) => {
    ink("yellow", btn.down && btn.over ? 128 : 64).box(btn.box);
  });
}

// 🎪 Act
function act({ event: e, api, send }) {
  if (!input.canType) {
    me.act(api);

    function open() {
      me.off();
      send({ type: "keyboard:open" });
    }

    inputBtn.act(e, { push: () => open() });
    if (e.is("keyboard:down:enter")) open();
  }

  if (
    input.canType &&
    (e.is("keyboard:down:escape") ||
      (input.text.trim().length === 0 &&
        e.is("keyboard:down:enter") &&
        !e.shift))
  ) {
    send({ type: "keyboard:close" });
  }

  if (
    e.is("keyboard:open") ||
    e.is("keyboard:close") ||
    (input.canType && !e.is("keyboard:down:escape"))
  ) {
    if (e.is("keyboard:close")) input.text = "";
    input.act(api);
  }
}

// 🧮 Sim
function sim({ api, geo }) {
  me.sim(api); // 🧒 Movement
  input.sim(api); // 💬 Chat

  const btnPos = me.screenPos(cam, world); // Button to activate prompt.
  inputBtn.box = new geo.Box(
    btnPos.x - me.size,
    btnPos.y - me.size,
    me.size * 2,
  );
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Field",
    desc: "An open place to walk around.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export const system = "world";
export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
