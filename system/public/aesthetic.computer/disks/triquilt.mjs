// Triquilt, 2024.10.14.21.42.15.061
// A half square triangle design tool for quilters.

/* 📝 Notes 
  - [] Add a selector with arrow keys to change the square.
    - [] Use qwas as corners and hjkl as movement keys.
          or qezc
*/

let w, h, size;

let colorA = "brown";
let colorB = "gray";

function boot({ fps }) {
  // Runs once at the start.
  w = 3;
  h = 3;
  size = 48;
  fps(1);
}

function paint({ wipe, ink, line, shape, screen, help }) {
  wipe("black");
  ink("yellow");
  // line(0, 0, screen.width, screen.height);
  // TODO: Paint a grid at w, h, with square size of size, with two triangles for each section of each square.

  const startX = screen.width / 2 - (size * w) / 2;
  const startY = screen.height / 2 - (size * h) / 2;

  // Loop through each grid section
  const directions = ["h", "j", "k", "l"]; // Possible directions
  const offsets = [
    [
      [0, 0],
      [1, 0],
      [0, 1],
    ], // "h"
    [
      [1, 0],
      [1, 1],
      [0, 0],
    ], // "j"
    [
      [1, 1],
      [0, 1],
      [1, 0],
    ], // "k"
    [
      [0, 1],
      [0, 0],
      [1, 1],
    ], // "l"
  ];

  for (let i = 0; i < w; i++) {
    for (let j = 0; j < h; j++) {
      let x = startX + i * size;
      let y = startY + j * size;
      const dir = help.choose(...directions); // Randomly pick a direction
      const idx = directions.indexOf(dir); // Get the index of the direction
      // First triangle
      const t1 = offsets[idx].map(([dx, dy]) => [x + dx * size, y + dy * size]);
      // Second triangle (rotated opposite of the first)
      const t2 = offsets[idx].map(([dx, dy]) => [
        x + (1 - dx) * size,
        y + (1 - dy) * size,
      ]);

      ink(colorA).shape(t1);
      ink(colorB).shape(t2);
    }
  }
}

// 📚 Library

// function act({ event: e }) {
//  // Respond to user input here.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
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
