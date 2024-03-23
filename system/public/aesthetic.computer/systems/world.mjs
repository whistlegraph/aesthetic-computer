// World (System), 23.12.18.22.48
// This module contains all of the world system functionality.
// Worlds are multi-user, interconnected rooms.

/* #region 🏁 TODO 
  - [x] Fix duplicate joins when switching areas.
  - [?] Camera snap after move. 
  + Done
  - [x] While holding arrow keys and then tapping, the elastic line is extended.
       (Play around with the difference.)
#endregion */

let me,
  kids,
  world,
  cam,
  input,
  inputBtn,
  server,
  spectating = false,
  spectatingKid,
  map = false;

const { keys, values } = Object;

async function world_boot(
  {
    api,
    help,
    handle,
    screen,
    ui,
    send,
    net: { socket },
    sound,
    store,
    piece,
    system,
    num: { number },
  },
  worldData,
) {
  // ✨ Initialization & Interface
  kids = {}; // Empty any kids that may be left over from a previous region.
  world = new World(worldData?.width, worldData?.height);

  let pos;
  if (system.world.teleported === false) {
    pos = (await store.retrieve(`world:${piece}:pos`)) || {
      x: undefined,
      y: undefined,
    };
  } else {
    pos = { ...system.world.telepos };
  }

  console.log("🗺️ Loaded position:", pos);

  me = new Kid(
    handle(),
    {
      x: number(pos.x) ? pos.x : world.size.width / 2,
      y: number(pos.y) ? pos.y : world.size.height / 2,
    },
    help.choose("meh", "smile", "frown"),
  );

  // Define the global system.world object that will share data amongst
  // all world-inherited pieces.
  system.world.me = me;
  system.world.size = world.size;

  cam = new Cam(
    screen.width / 2 - me.pos.x,
    screen.height / 2 - me.pos.y,
    me.pos,
  );

  const scheme = {
    dark: {
      text: 255,
      background: [0, 100],
      block: 255,
      highlight: 0,
      guideline: 255,
    },
  };

  input = new ui.TextInput(
    api,
    "...",
    async (text) => {
      if (
        text === "smile" ||
        text === "frown" ||
        text === "sad" ||
        text === "meh"
      ) {
        if (text === "sad") text = "frown";
        me.mood(text);
        server.send(`world:${piece}:mood`, me.face); // Send to server.
      } else if (
        text === "red" ||
        text === "yellow" ||
        text === "orange" ||
        text === "black" ||
        text === "brown" ||
        text === "purple" ||
        text === "pink" ||
        text === "blue" ||
        text === "lime" ||
        text === "white"
      ) {
        me.tint(text);
        server.send(`world:${piece}:tint`, me.color); // Send to server.
      } else if (text === "show") {
        // TODO: Stamp pixels underneath the player, that persist on
        //       the server instance.

        if (system.painting) {
          console.log("🖼️ Showing:", system.painting);

          server.send(
            `world:${piece}:show`,
            help.serializePainting(system.painting),
          );

          me.showing = system.painting;
        } else {
          console.log("❌🖼️ Nothing to show.");
        }

        // } else if (text === "paste") {
        // TODO: Stamp pixels underneath the player, that persist on
        //       the server instance.
      } else if (text === "hide") {
        server.send(`world:${piece}:hide`);
        me.showing = null;
      } else if (text === "map") {
        map = !map;
      } else {
        me.write(text); // Display message on 🧒.
        server.send(`world:${piece}:write`, me.message); // Send to server.
      }

      // Clear text, hide cursor block, and close keyboard.
      input.text = "";
      input.showBlink = false;
      input.mute = true;
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
      hideGutter: true,
      closeOnEmptyEnter: true,
    },
  );

  send({ type: "keyboard:soft-lock" });

  inputBtn = new ui.Button();

  // 🧦 Socket Networking
  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id, kids[id]?.handle);
      delete kids[id];
      return;
    }

    if (type === "joined") {
      console.log("️👋 Hello:", id, type, content);
      // TODO: Potentially reassociate a ghost here?
      return;
    }

    if (type.startsWith("connected")) {
      if (me.handle === "?") me.handle = `nub${id}`;
      server.send(`world:${piece}:join`, {
        handle: me.handle,
        pos: me.pos,
        face: me.face,
        showing: help.serializePainting(me.showing),
        ghost: me.ghost,
      });
      console.log("🪴 Welcome:", me.handle, `(${id})`);
      return;
    }

    if (type === `world:${piece}:ghost`) {
      if (kids[id]) {
        kids[id].ghost = true;
        console.log("👻 Ghosted:", kids[id].handle);
      }
      return;
    }

    // 👻🪦 Remove a dead ghost.
    if (type === `world:${piece}:kick`) {
      delete kids[id];
      return;
    }

    // TODO: How can this be oriented around storing a server list.
    if (type === `world:${piece}:list`) {
      console.log(`🗞️ Got list of all '${piece}' clients...`, content);
      keys(content).forEach((key) => {
        if (!kids[key]) {
          const data = content[key];

          // Drop in to spectating mode if necessary.
          if (
            content[key].handle.startsWith("@") &&
            content[key].handle === me.handle
          ) {
            spectating = true;
            spectatingKid = key;
            console.log("👓 Spectating:", spectatingKid);
          }

          console.log("🧒 Joined:", data.handle || id, data);
          kids[key] = new Kid(
            data.handle || `nub${id}`,
            data.pos,
            data.face,
            true,
          );
          if (data.ghost) kids[key].ghost = data.ghost;
          if (data.showing)
            kids[key].showing = help.deserializePainting(data.showing);
        }
      });
      return;
    }

    if (type === `world:${piece}:join`) {
      console.log("🗺️ Joining world:", type, content);
      if (!kids[id]) {
        if (content.handle.startsWith("@")) {
          if (content.handle === me.handle) {
            // If joining to view oneself, set the joined user as
            // a spectator.
            console.log("👓 Spectator joined:", content);
            return;
          } else {
            // Otherwise iterate through all kids and unghost any.
            keys(kids).forEach((key) => {
              const kid = kids[key];
              // If there is a handle match...
              console.log(kid, content.handle, kid.handle);
              if (content.handle === kid.handle && kid.ghost) {
                console.log("👻 Unghosting:", kid.handle);
                delete kids[key];
              }
            });
          }
        }

        kids[id] = new Kid(
          content.handle || `nub${id}`,
          content.pos,
          content.face,
          true,
        );
        if (content.ghost) kids[id].ghost = true;
        if (content.showing)
          kids[id].showing = help.deserializePainting(content.showing);
      }
      return;
    }

    if (type === `world:${piece}:tint`) {
      const kid = kids[id];
      if (kid) kid.tint(content);
      return;
    }

    if (type === `world:${piece}:mood`) {
      const kid = kids[id];
      if (kid) kid.mood(content);
      return;
    }

    if (type === `world:${piece}:slug`) {
      const kid = kids[id];
      if (kid) kid.slug(content.slug);
      return;
    }

    if (type === `world:${piece}:write`) {
      const kid = kids[id];
      if (kid) {
        kid.write(content);
        sound.synth({
          type: "sine",
          tone: 950,
          attack: 0.1,
          decay: 0.96,
          volume: 0.65,
          duration: 0.015,
        });
      }
      return;
    }

    if (type === `world:${piece}:write:clear`) {
      const kid = kids[id];
      if (kid) kid.write(null);
      return;
    }

    if (type === `world:${piece}:move`) {
      // console.log(type, id, content);
      const kid = kids[id];
      if (kid) kid.netPos = content.pos;
      return;
    }

    if (type === `world:${piece}:show`) {
      const kid = kids[id];
      if (kid) kid.showing = help.deserializePainting(content);
      return;
    }

    if (type === `world:${piece}:hide`) {
      const kid = kids[id];
      if (kid) kid.showing = null;
      return;
    }
  });
}

function world_paint(
  { api, ink, pan, unpan, pen, screen, leaving, hud, typeface },
  paint,
  curtain,
) {
  // 🌎 + 🧒 World & Players
  pan(cam.x, cam.y);

  paint?.(api, world);

  if (!spectating) {
    inputBtn.paint((btn) => {
      ink("white", btn.down && btn.over ? 128 : 64).circle(
        me.pos.x,
        me.pos.y,
        btn.box.w / 2,
        true,
      );
    });
  }

  if (!spectating) me.paint(api);

  unpan();

  // Other kids.
  keys(kids).forEach((key) => {
    pan(cam.x, cam.y);
    const kid = kids[key];
    kid.paint(api);
    unpan();
  });

  // TODO: Make this a generic module for printing user lists? 23.12.04.15.47
  [me, ...values(kids)].forEach((kid, i) => {
    const row = i * 12;
    const handleText = i === 0 ? `you are ${kid.handle}` : kid.handle;

    const kidOnScreen = i !== 0 && onScreen(kid, world, cam, screen);

    const pos = kidOnScreen
      ? {
          x: kid.pos.x - (handleText.length / 2) * typeface.blockWidth,
          y: kid.pos.y + 22,
        }
      : { x: 6, y: 21 + row };

    if (kidOnScreen) pan(cam.x, cam.y);
    ink("black").write(handleText, { x: pos.x + 1, y: pos.y + 1 });
    ink(me === kid ? "yellow" : kid.color).write(handleText, pos);
    if (kidOnScreen) unpan();

    if (i === 0)
      ink(kid.color).write(handleText.replace(kid.handle, "").trim(), {
        x: 6,
        y: 21 + row,
      });
  });

  const statusColor = hud.currentStatusColor();
  if (typeof statusColor === "string" && statusColor !== "lime") {
    ink(statusColor, 128).box(0, 0, screen.width, screen.height);
  }

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

  curtain?.(api); // Paint anything on top of world but under input.

  if (input.canType && !leaving()) {
    input.paint(api, false, {
      x: 0,
      y: 18,
      width: screen.width,
      height: screen.height - 18,
    });
  }

  // 🗺️ Minimap

  if (map) {
    const worldAspect = world.width / world.height;
    const screenAspect = screen.width / screen.height;

    const scale =
      (worldAspect > screenAspect
        ? screen.width / world.width
        : screen.height / world.height) * 0.5;

    const bw = world.width * scale,
      bh = world.height * scale;

    const x = screen.width / 2 - bw / 2,
      y = screen.height / 2 - bh / 2;

    // World
    ink("white", 96).box(x, y, bw, bh);

    // Camera
    ink("red", 64).box(
      x + cam.dolly.x * scale,
      y + cam.dolly.y * scale,
      screen.width * scale,
      screen.height * scale,
      "center",
    );
    // ink("red").box(x + cam.dolly.x * scale - 1, y + cam.dolly.y * scale - 1, 3);

    // Paint dots for all kids.
    [me, ...values(kids)].forEach((kid, i) => {
      ink(kid.color).box(x + kid.pos.x * 0.25 - 1, y + kid.pos.y * 0.25 - 1, 3);
    });

    ink("orange").write(
      `kid x:${me.pos.x.toFixed(1)}  y:${me.pos.y.toFixed(1)}`,
      { x: 6, bottom: 4 + 12 },
    );

    ink("pink").write(
      `cam x:${cam.dolly.x.toFixed(1)}  y:${cam.dolly.y.toFixed(1)}`,
      { x: 6, bottom: 4 },
    );
  }

  // Spectating
  if (spectating) ink("red").write("Spectating", { x: 6, y: 32 });
}

function world_act({ event: e, api, send, jump, hud, piece, screen }) {
  if (e.is("reframed")) {
    cam.x = screen.width / 2 - cam.dolly.x;
    cam.y = screen.height / 2 - cam.dolly.y;
  }

  if (spectating) return; // Don't allow any input if in spectator mode.

  if (!input.canType) {
    me.act(api);

    inputBtn.act(e, {
      down: () => {
        send({ type: "keyboard:soft-unlock" });
      },
      push: () => {
        me.off();
        send({ type: "keyboard:soft-lock" });
      },
      cancel: () => {
        send({ type: "keyboard:soft-lock" });
      },
      rollout: () => {
        send({ type: "keyboard:soft-lock" });
      },
      rollover: () => {
        if (inputBtn.down) send({ type: "keyboard:soft-unlock" });
      },
    });

    if (
      !input.canType &&
      e.is("keyboard:down:enter") // ||
      // e.is("keyboard:down:escape") ||
      // e.is("keyboard:down:`")
    ) {
      send({ type: "keyboard:open" });
      me.off();
    }

    if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) jump("prompt");

    // Backspace back to `prompt`.
    if (e.is("keyboard:down:backspace")) {
      jump(`prompt~${hud.currentLabel.text || piece}`)(() => {
        send({ type: "keyboard:open" });
      });
    }
  }

  if (
    input.canType &&
    (e.is("keyboard:down:`") ||
      e.is("keyboard:down:escape") ||
      (input.text.trim().length === 0 &&
        e.is("keyboard:down:enter") &&
        !e.shift))
  ) {
    send({ type: "keyboard:close" });
  }

  if (input.canType && e.is("lift") && !input.shifting && !input.paste.down) {
    send({ type: "keyboard:close" });
  }

  if (
    e.is("keyboard:open") ||
    e.is("keyboard:close") ||
    (input.canType && !e.is("keyboard:down:escape"))
  ) {
    input.act(api);
  }
}

function world_sim({ api, piece, geo, simCount, screen, num }) {
  keys(kids).forEach((key) => kids[key].sim(api)); // Networked kids.

  if (!spectating) {
    me.sim(api, function net(kid) {
      if (simCount % 4n === 0n) {
        // Send position updates at a rate of 30hz  (120 / 4).
        if (kid.pos) server.send(`world:${piece}:move`, kid);
      }
      if (kid.clear) server.send(`world:${piece}:write:clear`, kid);
    }); // 🧒 Movement

    cam.dolly.x = num.lerp(cam.dolly.x, me.pos.x, 0.035);
    cam.dolly.y = num.lerp(cam.dolly.y, me.pos.y, 0.035);

    cam.x = screen.width / 2 - cam.dolly.x;
    cam.y = screen.height / 2 - cam.dolly.y;

    input.sim(api); // 💬 Chat

    const btnPos = me.screenPos(cam, world); // Button to activate prompt.
    inputBtn.box = new geo.Box(
      btnPos.x - me.size,
      btnPos.y - me.size,
      me.size * 2,
    );
  } else if (spectating && kids[spectatingKid]) {
    const sk = kids[spectatingKid];
    cam.dolly.x = num.lerp(cam.dolly.x, sk.pos.x, 0.035);
    cam.dolly.y = num.lerp(cam.dolly.y, sk.pos.y, 0.035);

    cam.x = screen.width / 2 - cam.dolly.x;
    cam.y = screen.height / 2 - cam.dolly.y;
    // console.log("Spectaing:", cam);
  }
}

// Leaving the world... and logging / saving the position of the user as a ghost.
function world_leave({ system, store, piece }) {
  console.log("🗺️ Leaving world, storing position.");
  server.send(`world:${piece}:persist`, { handle: me.handle, pos: me.pos });
  store[`world:${piece}:pos`] = me.pos; // Persist current position.
  store.persist(`world:${piece}:pos`);
  delete system.world.size; // Clear the system world data set in `boot`.
  delete system.world.me;
}

// Determines whether the world covers the whole screen or not.
// (Used to toggling backdrop.)
function coversScreen(screen) {
  return (
    cam.x <= 0 &&
    cam.y <= 0 &&
    cam.x + world.size.width > screen.width &&
    cam.y + world.size.height > screen.height
  );
}

export {
  world_boot,
  world_paint,
  world_sim,
  world_act,
  world_leave,
  coversScreen,
};

// 🧒
class Kid {
  handle;
  net;
  pos = { x: 0, y: 0 };
  netPos;
  size = 16;
  leash = { x: 0, y: 0, len: 0, max: 12, deadzone: 8 };
  face = "meh";
  color = "white";
  #keys = { U: false, D: false, L: false, R: false };
  message;
  showing; // Contains a buffer to be showing.
  ghost = false;
  #messageDuration;
  #messageProgress = 0;

  constructor(handle = "?", pos = this.pos, face, net = false) {
    this.handle = handle;
    console.log("🧒 *New* kid from:", this.handle, "Feeling:", face);

    this.pos = pos;
    if (net) this.netPos = { ...pos };

    this.face = face || this.face;
    this.net = net; // Is it from the network?
  }

  // Show a message above the kid's head for `time` frames.
  write(text, time = 320) {
    this.message = text;
    this.#messageDuration = time;
    this.#messageProgress = 0; // Reset message progress.
  }

  // Show a message above the kid's head for an unending period.
  // 🐛 Used only if they are `ghosted` for slug updates.
  slug(text) {
    this.write(text, Infinity);
  }

  // Change the mood (face) of the kid.
  mood(face) {
    this.face = face;
  }

  // Change the color of the kid.
  tint(c) {
    this.color = c;
  }

  // Render the kid.
  paint({ ink, line, point, pan, text, typeface, stamp }) {
    const leash = this.leash;
    pan(this.pos.x, this.pos.y);

    if (this.showing) stamp(this.showing); // Show a bitmap.

    if (!this.showing) {
      ink(this.color).circle(0, 0, this.size); // Head

      // Face

      // Eyes
      if (this.ghost) {
        line(-6, -6, -10, -6).line(6, -6, 10, -6);
      } else {
        point(-6, -6).point(6, -6);
      }

      // Mouth
      if (this.face === "smile") {
        ink(this.color).line(0, 6, -6, 3);
        ink(this.color).line(0, 6, 6, 3);
      } else if (this.face === "frown") {
        ink(this.color).line(0, 3, -6, 8);
        ink(this.color).line(0, 3, 6, 8);
      } else if (this.face === "meh") {
        ink(this.color).line(-6, 6, 6, 6);
      }
    }

    ink(leash.len > leash.deadzone ? this.color : [this.color, 128]).line(
      0,
      0,
      leash.x,
      leash.y,
    );

    if (this.message) {
      const blockWidth = typeface.glyphs["0"].resolution[0];
      const tb = text.box(
        this.message,
        undefined,
        this.message.length * blockWidth,
      );
      ink("black").write(this.message, {
        x: -tb.box.width / 2 + 1,
        y: -this.size - 14 + 1,
      });
      ink(this.color).write(this.message, {
        x: -tb.box.width / 2,
        y: -this.size - 14,
      });
    }
  }

  // Control the kid.
  act({ event: e, num }) {
    const k = this.#keys;
    const leash = this.leash;
    if (!this.drag) {
      if (e.is("keyboard:down:w") || e.is("keyboard:down:arrowup")) k.U = true;
      if (e.is("keyboard:down:s") || e.is("keyboard:down:arrowdown"))
        k.D = true;
      if (e.is("keyboard:down:a") || e.is("keyboard:down:arrowleft"))
        k.L = true;
      if (e.is("keyboard:down:d") || e.is("keyboard:down:arrowright"))
        k.R = true;
      if (e.is("keyboard:up:w") || e.is("keyboard:up:arrowup")) k.U = false;
      if (e.is("keyboard:up:s") || e.is("keyboard:up:arrowdown")) k.D = false;
      if (e.is("keyboard:up:a") || e.is("keyboard:up:arrowleft")) k.L = false;
      if (e.is("keyboard:up:d") || e.is("keyboard:up:arrowright")) k.R = false;
    }

    if (e.is("touch:1")) {
      k.U = k.D = k.L = k.R = false;
      leash.start = { x: e.x, y: e.y };
      leash.x = leash.y = 0;
      this.drag = true;
    }

    if (e.is("draw:1") && leash.start) {
      leash.x = e.x - leash.start.x;
      leash.y = e.y - leash.start.y;
      this.#snapLeash(num);
    }

    if (e.is("lift:1")) {
      leash.start = null;
      this.drag = false;
    }
  }

  // Simulate the kid's movement and time messages.
  sim({ num }, net) {
    // 🗼 Network Prediction
    if (this.net && this.netPos) {
      const p2 = num.p2;
      const STEP_SIZE = 1;
      const direction = p2.norm(p2.sub(this.netPos, this.pos));
      const distance = p2.len(p2.sub(this.netPos, this.pos));
      if (distance < STEP_SIZE) {
        this.pos = this.netPos; // Set position to target.
      } else {
        this.pos = p2.inc(this.pos, p2.scl(direction, STEP_SIZE));
      }
      return; // No need to compute movements, locally.
    }

    // Local simulation.

    // 🗨️ Message
    if (this.message) {
      if (this.#messageProgress < this.#messageDuration) {
        this.#messageProgress += 1;
      } else {
        this.message = null;
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

    // When the position has changed...
    if (newPos.x !== pos.x || newPos.y !== pos.y) {
      net?.({ pos }); // Run the net callback.
      this.moved = true;
    }

    pos.x = newPos.x;
    pos.y = newPos.y;

    if (pos.x < 0) pos.x = 0;
    if (pos.x > world.size.width) pos.x = world.size.width;
    if (pos.y < 0) pos.y = 0;
    if (pos.y > world.size.height) pos.y = world.size.height;
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
  get width() {
    return this.size.width;
  }

  get height() {
    return this.size.height;
  }
}

// 🎥
class Cam {
  x = 0;
  y = 0;
  dolly = { x: 0, y: 0 };

  constructor(x, y, dolly) {
    this.x = x;
    this.y = y;
    this.dolly = { ...dolly };
  }
}

function onScreen(obj, world, cam, screen) {
  const halfWidth = screen.width / 2;
  const halfHeight = screen.height / 2;

  const viewport = {
    left: cam.dolly.x - halfWidth,
    right: cam.dolly.x + halfWidth,
    top: cam.dolly.y - halfHeight,
    bottom: cam.dolly.y + halfHeight,
  };

  return (
    obj.pos.x >= viewport.left &&
    obj.pos.x <= viewport.right &&
    obj.pos.y >= viewport.top &&
    obj.pos.y <= viewport.bottom
  );
}
