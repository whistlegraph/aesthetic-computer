// 💅 Nail, 22.12.31
// A multiplayer drawing tool for thumbnailing.

// [] Incorporate 1px line algorithm after smoothing via catmull-rom.
// [] Allow recordings to be made via the action layer.
// [] Set the resolution to a 16x9 situation with a border.
// [] Figure out a storage method on the server
//    for reloading and keeping the board active.

let painting; // A bitmap to draw on.

let server;
const painters = {}; // Instances of Painter stored by client id.
const actions = []; // Actions that have been received by `server`. These get
//                     flushed after every `paint` frame.
//                     TODO: Record all actions in order to replay pictures.

// 🥾 Boot (Runs once before first paint and sim)
function boot({ paste, cursor, painting: p, screen, net, resize, glaze }) {
  // resize(screen.width / 2, screen.height / 2); // TODO: Get screen.nativeWidth.
  cursor("none");

  // Make & display the canvas.
  painting = p(screen.width, screen.height, (gfx) => gfx.wipe(100, 100, 100));
  paste(painting);

  // glaze({ on: true, type: "hello" });

  // Connect to the server and route each message.
  server = net.socket((id, type, content) => {
    // Instantiate painters (clients) based on their `id` attribute.
    painters[id] = painters[id] || new Painter(id);
    // Record the action.
    actions.push({ id, type, content });
  });
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({ pen, ink, abstract: { bresenham }, page, screen, paste }) {
  //paste(painting); // TODO: Optimize this with a dirty rectangle (See `line`).

  if (actions.length) {
    // Process actions and render to the painting.
    page(painting);

    actions.forEach((action) => {
      const painter = painters[action.id];
      painter[action.type](action.content); // Run the action / method in `Painter`.
      painter.paint(action.type, [255, 0, 0, 50], { ink }); // Paint to the canvas.
    });
    actions.length = 0;

    // Paste the updated painting.
    page(screen).paste(painting);

    // Draw the UI.
    for (const p in painters) painters[p].overlay([0, 255, 0, 100], { ink });
    ink(0, 0, 255, 100).plot(pen); // 🔴 Draw cursor.
  } else {
    // Paste the unchanged painting.
    paste(painting);

    // Draw the UI.
    for (const p in painters) painters[p].overlay([0, 255, 0, 100], { ink });
    ink(255, 255, 0, 100).plot(pen); // 🟡 Move (hover) cursor.
  }
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, num: { dist } }) {
  if (e.is("draw") || e.is("touch")) {
    // Extract the necessary fields from the event object.
    // TODO: I could reduce the data into an array here for faster parsing
    //       and a smaller footprint over the network. 22.1.5
    //       And also send it in batches / frames in order to avoid flooding
    //       the server. 2022.02.20.22.14
    const point = (({ x, y, pressure }) => ({ x, y, pressure }))(e);
    server.send("point", point);
  }

  if (e.is("lift")) {
    server.send("stop");
  }
}

// 📚 Library (Useful functions used throughout the program)

import { Mark, pixelPerfect } from "../lib/gesture.mjs";

/**
 * Draws segments of brushes and keeps track of gesture state for each painter.
 */
class Painter {
  id;
  currentMark;
  #paintedMarkOnce = false;

  constructor(id) {
    this.id = id;
  }

  paint(action, color, { ink }) {
    if (!this.currentMark) return; // Nothing to paint if there is no mark.

    const lines = this.currentMark.line();

    lines.forEach((p, i) => {
      if (i < lines.length - 1) {
        ink(color)
          .skip(p)
          .line(p, lines[i + 1])
          .skip(null);
      } else if (this.#paintedMarkOnce === false) {
        this.#paintedMarkOnce = true;
        ink(color).plot(p);
      }
    });

    // TODO: Why would this action behavior be here?
    // TODO: This should go on stop.
    if (action === "stop") {
      // TODO: Paint the rest of previewLine on release.
      this.currentMark.previewLine((pl) =>
        ink(255, 0, 0, 50)
          .skip(pl[0])
          .line(...pl)
          .skip(null)
      );
    }
  }

  // TODO: *** Make a lime-green, pixel-perfect spline preview work!

  overlay(color, { ink }) {
    if (!this.currentMark) return; // Nothing to overlay if there is no mark.

    // Add an interpolated preview from the last point to the current point.
    this.currentMark.previewLine((pl) =>
      //ink(color)
      ink(color)
        .skip(pl[0])
        .line(...pl)
        .skip(null)
    );

    // TODO: Filter by pixel-perfect-ness.
    // TODO: pixelPerfect... how to make "poly" pixel-perfect?

    // Draw a full curve through all the points.
    //ink(255, 255, 0, 128).poly(this.currentMark.spline());

    ink(color).poly(this.currentMark.spline());

    // Plot every spline point.
    this.currentMark.spline().forEach((p) => {
      //ink(0, 0, 0).plot(p);
    });

    // Draw every processed point.
    this.currentMark
      .spots()
      .forEach((spot) => ink(255, 0, 128, 255).plot(spot));
  }

  // Runs on every recorded point.
  point(p) {
    this.currentMark = this.currentMark || new Mark(4);
    this.currentMark.input(p);
  }

  stop() {
    console.log("Stop", this.currentMark.points);
    // TODO: Paint the rest of previewLine on release.
    this.currentMark = null;
    this.#paintedMarkOnce = false;
  }
}

export { boot, paint, act };
