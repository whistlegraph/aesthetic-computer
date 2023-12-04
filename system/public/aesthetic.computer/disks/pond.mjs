// Pond, 2023.9.30.18.17.59.613
// Chat in ripples.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Add user list.
  - [] Better backdrops / make sounds after a gesture is made.
#endregion */

let server;
const activeMarks = {};
const fadingMarks = [];
const fadeMax = 1200;

// 🥾 Boot
function boot({ wipe, net: { socket } }) {
  wipe(0);
  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id);
      delete activeMarks[id];
    }
    if (type === "joined") {
      console.log("️👋 Hello:", id);
    }
    if (type.startsWith("connected")) {
      // Respond to: "connected" or "connected:already"
      console.log("Your ID is:", id);
    }

    if (server.id !== id) {
      if (type === "pond:new") {
        //pointers[id] = content;
        activeMarks[id] = { gesture: [content] };
      }

      if (type === "pond:draw") {
        //pointers[id] = content;
        activeMarks[id]?.gesture.push(content);
      }

      if (type === "pond:lift") {
        //pointers[id] = content;
        fadingMarks.push({ ...activeMarks[id], fade: fadeMax });
        delete activeMarks[id];
      }
    }
  });
}

// 🎨 Paint
function paint({ wipe, ink, screen, help }) {
  wipe("darkblue");

  fadingMarks.forEach((mark) => {
    ink(0, 255, 255, 255 * (mark.fade / fadeMax)).poly(
      mark.gesture.map((point) => [
        point[0] * screen.width + help.choose(-1, 0, 1),
        point[1] * screen.height + help.choose(-1, 0, 1),
      ]),
    );
  });

  Object.keys(activeMarks).forEach((m) => {
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
