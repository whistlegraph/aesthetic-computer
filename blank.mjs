// Blank, 2024.6.25.15.39.39.039
// A blank piece.

let flag = false;

function boot({ wipe }) {
 wipe("green");
}

function paint({ wipe, ink, line, screen, pen }) {
  ink();
  line(screen.width / 2, screen.height / 2, pen.x, pen.y);
  if (flag) {
    wipe("blue");
    flag = false;
  }
}

function act({ event }) {
  // Respond to user input here.
  if (event.is("touch")) {
    flag = true;
    console.log(flag);
  }
}

export { boot, paint, act };

// 📚 Library
//   (Useful functions used throughout the piece)
