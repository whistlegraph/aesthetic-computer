// Pond, 2023.9.30.18.17.59.613
// Chat in ripples.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Update `pond` to use world-like user lists.
  - [-] Change title of tab / page to use parameters after a dot or dash?
  - [] Don't allow a user to send marks unless they are connected.
  - [] Better backdrops / make sounds after a gesture is made.
  + Done
  - [x] Add user list.
#endregion */

let server;
const activeMarks = {};
const fadingMarks = [];
const fadeMax = 1200;
const all = {};

const { keys } = Object;

// 🥾 Boot
function boot({ wipe, net: { socket }, handle }) {
  wipe(0);
  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id);
      delete activeMarks[id];
      delete all[id];
    }
    if (type === "joined") {
      console.log("️👋 Hello:", id);
    }
    if (type.startsWith("connected")) {
      console.log("Your ID is:", id);
      all[id] = { handle: handle() || "fish" };
      server.send("pond:join", { ...all[id] });
    }

    if (server.id !== id) {
      if (type === "pond:join") {
        if (!all[id]) {
          all[id] = { ...content };
          server.send("pond:join", { ...all[id] });
        }
        return;
      }

      if (type === "pond:new") {
        //pointers[id] = content;
        activeMarks[id] = { gesture: [content] };
        return;
      }

      if (type === "pond:draw") {
        //pointers[id] = content;
        activeMarks[id]?.gesture.push(content);
        return;
      }

      if (type === "pond:lift") {
        //pointers[id] = content;
        fadingMarks.push({ ...activeMarks[id], fade: fadeMax });
        delete activeMarks[id];
        return;
      }
    }
  });
}

// 🎨 Paint
function paint({ wipe, ink, screen, help }) {
  wipe("darkblue");

  fadingMarks.forEach((mark) => {
    if (mark.gesture) {
      ink(0, 255, 255, 255 * (mark.fade / fadeMax)).poly(
        mark.gesture.map((point) => [
          point[0] * screen.width + help.choose(-1, 0, 1),
          point[1] * screen.height + help.choose(-1, 0, 1),
        ]),
      );
    }
  });

  keys(activeMarks).forEach((m) => {
    const mark = activeMarks[m];
    if (mark.gesture) {
      ink(0, 255, 255).poly(
        mark.gesture.map((point) => [
          point[0] * screen.width,
          point[1] * screen.height,
        ]),
      );
    }
  });

  // TODO: Make this a generic module for printing user lists? 23.12.04.15.47
  keys(all).forEach((k, i) => {
    const row = i * 12;
    ink("black").write(all[k].handle, { x: 7, y: 21 + 1 + row });
    ink("cyan").write(all[k].handle, { x: 6, y: 21 + row });
  });
}

// 🎪 Act
function act({ event: e, screen }) {
  // Send messages to the server.
  if (e.is("touch:1")) {
    const p = [e.x / screen.width, e.y / screen.height];
    activeMarks["me"] = { gesture: [p] };
    server.send("pond:new", p);
  }

  if (e.is("draw:1")) {
    const p = [e.x / screen.width, e.y / screen.height];
    activeMarks["me"]?.gesture.push(p);
    server.send("pond:draw", p);
  }

  if (e.is("lift:1")) {
    fadingMarks.push({ ...activeMarks["me"], fade: fadeMax });
    delete activeMarks["me"];
    server.send("pond:lift");
  }
}

// 🧮 Sim
function sim() {
  Object.keys(fadingMarks).forEach((m) => {
    const mark = fadingMarks[m];
    mark.fade -= 1;
    if (mark.fade === 0) delete fadingMarks[m];
  });
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
    title: "Pond",
    desc: "Chat in ripples.",
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

export { act, boot, paint, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
