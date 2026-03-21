// Spray, 22.12.31
// A stylus based painting tool for flower pictures.

// - Create a grid for `painting` so it can be displayed
//   and transformed independently of the display.

let painting; // A bitmap to draw on.
const sprays = []; // Points to draw.
let dot = false; // Show preview dot while moving cursor.

let server; // Networking

// 🥾 Boot (Runs once before first paint and sim)
function boot({ paste, cursor, painting: p, screen, net: { socket } }) {
  cursor("none");

  // Make & display the canvas.
  painting = p(screen.width, screen.height, (gfx) => gfx.wipe(140, 50, 20));
  paste(painting);

  // Connect to the server.
  server = socket((id, type, content) => {
    if (type === "point") sprays.push(content);
  });
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({
  pen,
  ink,
  page,
  screen,
  paste,
  geo: { Circle },
  num: { randIntRange: rnd },
  help: { repeat: rep },
}) {
  paste(painting);
  if (sprays.length > 0) {
    page(painting);

    // Spray on the painting within a circle.
    sprays.forEach((s) => {
      const c = new Circle(s.x, s.y, 2);
      const alpha = 255 * s.pressure * s.pressure;

      rep(8 + 32 * s.pressure, () => {
        const point = c.random();
        ink(
          60 + rnd(-40, 80), // R
          255 - rnd(0, 100), // G
          80 + rnd(-40, 80), // B
          alpha + rnd(-20, 20) // A
        ).plot(point);
      });
    });
    sprays.length = 0;
    page(screen);
    paste(painting);
    ink(255, 0, 0).plot(pen); // 🔴 Draw cursor.
  } else if (dot) {
    ink(255, 255, 0).plot(pen); // 🟡 Move (hover) cursor.
    dot = false;
  }
}

// ✒ Act (Runs once per user interaction)
function act({ event: e }) {
  if (e.is("move")) dot = true;
  if (e.is("draw") || e.is("touch")) {
    // Extract the necessary fields from the event object.
    // https://stackoverflow.com/a/39333479
    // TODO: I could reduce the data into an array here for faster parsing
    //       and a smaller footprint over the network. 22.1.5
    const point = (({ x, y, pressure }) => ({ x, y, pressure }))(e);
    sprays.push(point);
    server.send("point", point);
  }
}

// 💗 Beat (Runs once per bpm)
// function beat($api) { // TODO: Play a sound here! }

// 🧮 Simulate (Runs once per logic frame (120fps)).
// function sim($api) { // TODO: Move a ball here! }

// 📚 Library (Useful functions used throughout the program)

export { boot, paint, act };
