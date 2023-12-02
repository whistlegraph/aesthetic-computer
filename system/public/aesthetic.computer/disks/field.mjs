// Field, 2023.11.30.16.05.21.050
// An open place to walk around.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add a "special", `smile`, `frown` and `meh` command 😉. 
    - [] Add color words to change face.
    - [] Make sure these can be written like: `smile: chat` / using a 
         simple character on the keyboard.
  - [] Make the world scrollable.
  - [] Move common functionality to a `world.mjs` library file.
  + Done
  - [x] Get multi-user networking online. 
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
  sim({ num }, net) {
    // 🗨️ Message
    if (this.#message) {
      if (this.#messageProgress < this.#messageDuration) {
        this.#messageProgress += 1;
      } else {
        this.#message = null;
        this.#messageProgress = 0;
        net?.({ clear: true });
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

    const newPos = { ...pos };
    if (leash.len > leash.deadzone) {
      newPos.x = num.lerp(pos.x, pos.x + leash.x, 0.075);
      newPos.y = num.lerp(pos.y, pos.y + leash.y, 0.075);
    } else if (leash.len > 1) {
      newPos.x = num.lerp(pos.x, pos.x + leash.x, 0.025);
      newPos.y = num.lerp(pos.y, pos.y + leash.y, 0.025);
    }

    // Run the net callback whenever the position changes.
    if (newPos.x !== pos.x || newPos.y !== pos.y) net?.({ pos });

    pos.x = newPos.x;
    pos.y = newPos.y;
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

let me, world, cam, input, inputBtn, server;
const kids = {};

const { keys } = Object;

// 🥾 Boot
function boot({ api, wipe, handle, screen, ui, send, net: { socket } }) {
  // ✨ Initialization & Interface
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
    "...",
    async (text) => {
      me.write(input.text); // Display message on 🧒.
      server.send("field:write", input.text); // Send to server.

      // Clear text, hide cursor block, and close keyboard.
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

  // Socket Networking
  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id);
      delete kids[id];
    }

    if (type === "joined") {
      console.log("️👋 Hello:", id);
    }

    if (type.startsWith("connected")) {
      server.send("field:join", { handle: me.handle, pos: me.pos });
      console.log("🪴 Welcome:", me.handle, `(${id})`);
    }

    if (server.id !== id) {
      if (type === "field:write") {
        const kid = kids[id];
        if (kid) kid.write(content);
      }

      if (type === "field:write:clear") {
        const kid = kids[id];
        if (kid) kid.write(null);
      }

      if (type === "field:join") {
        if (!kids[id]) {
          kids[id] = new Kid(content.handle || id, content.pos);
          server.send("field:join", { handle: me.handle, pos: me.pos });
        }
      }

      if (type === "field:move") {
        const kid = kids[id];
        if (kid) kid.pos = content.pos;
      }
    }
  });
}

// 🎨 Paint
function paint({ api, wipe, ink, pan, unpan, pen, screen }) {
  wipe(0); // 🖼️ Backdrop
  // 🌎 + 🧒 World & Players
  pan(cam.x, cam.y);
  world.paint(api);
  me.paint(api);

  unpan();

  keys(kids).forEach((key) => {
    pan(cam.x, cam.y);
    const kid = kids[key];
    kid.paint(api);
    unpan();
  });

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
  me.sim(api, function net(kid) {
    if (kid.pos) server.send("field:move", kid);
    if (kid.clear) server.send("field:write:clear", kid);
  }); // 🧒 Movement
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
