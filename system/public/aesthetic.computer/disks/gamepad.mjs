// Gamepad, 2024.11.08.02.41.47.840
// Test your gamepad connectivity.

/* 📝 Notes
 */

const event = [];
const players = {};
const { keys } = Object;
const { abs } = Math;

let picture;

function boot({ wipe, screen, painting }) {
  picture = painting(screen.width, screen.height, ({ wipe }) => wipe("gray"));
}

function paint({ wipe, ink, line, screen, paste, page }) {
  // wipe("black");
  // ink("purple", 64).box(0, 0, screen.width, screen.height);
  paste(picture);

  ink("yellow").write(event[0], { x: 6, y: 20 });
  ink("pink").write(event[1], { x: 6, y: 20 + 12 });

  keys(players).forEach((index) => {
    const player = players[index];
    const len = 8;

    ink(player.color).circle(player.x, player.y, len * 2, "center");

    ink(player.painting ? undefined : "white").line(
      player.x,
      player.y,
      player.x + player.xvel * len,
      player.y + player.yvel * len,
    );

    if (player.painting) {
      page(picture);
      ink(player.color, 16).circle(player.x, player.y, len * 2, "center");
      page(screen);
    }

  });
}

function act({ event: e, screen }) {
  // Respond to user input here.
  if (e.is("gamepad")) {
    [0, 1].forEach((index) => {
      if (e.is(`gamepad:${index}`)) {
        // Instantiate a p1 if they don't exist.
        if (!players[e.gamepad]) {
          players[e.gamepad] = {
            x: screen.width / 2,
            y: screen.height / 2,
            xvel: 0,
            yvel: 0,
            xforce: 0,
            yforce: 0,
            xdec: 0.93,
            ydec: 0.93,
            color: "green",
            painting: false
          };
        }

        const player = players[e.gamepad];

        event[index] = e.name;

        if (e.is(`gamepad:${index}:axis`)) {
          event[index] += ":" + e.value.toFixed(2);
          // Left Stick
          if (e.axis === 0) player.xforce = e.value; // X
          if (e.axis === 1) player.yforce = e.value; // Y
        }

        if (e.is(`gamepad:${index}:button`)) {
          if (e.button === 0 && e.action === "push") player.color = "green"; // A
          if (e.button === 1 && e.action === "push") player.color = "red"; // B
          if (e.button === 2 && e.action === "push") player.color = "blue"; // X
          if (e.button === 3 && e.action === "push") player.color = "yellow"; // Y
          if (e.button === 6) player.painting = e.action === "push";
        }
      }
    });
  }
}

function sim({ screen }) {
  keys(players).forEach((index) => {
    const player = players[index];
    if (abs(player.xforce / 6) > 0.01) player.xvel += player.xforce / 6;
    if (abs(player.yforce / 6) > 0.01) player.yvel += player.yforce / 6;

    player.xvel *= player.xdec;
    player.yvel *= player.ydec;

    player.x += player.xvel;
    player.y += player.yvel;

    if (abs(player.xforce) < 0.35) player.xforce *= player.xdec;
    if (abs(player.yforce) < 0.35) player.yforce *= player.ydec;

    if (player.x < 0) player.x = screen.width + player.x;
    if (player.x > screen.width) player.x = player.x - screen.width;
    if (player.y < 0) player.y = screen.height + player.y;
    if (player.y > screen.height) player.y = player.y - screen.height;
  });
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
