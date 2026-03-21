// Build up a list of points and then keep drawing them as long as it exists.
let points = [];
let curX, curY;
let firstFrame = true;

// 💗 Beat
// TODO: Make beat unnecessary?
export function beat($api) {
  const { num, help, sound } = $api;
}

// 🧮 Update
export function update($api) {
  const { pen, num, load } = $api;

  if (pen.down) {
    // We are drawing
    curX = pen.x;
    curY = pen.y;

    if (
      pen.x !== points?.[points.length - 1]?.[0] || // If the points are not the same.
      pen.y !== points?.[points.length - 1]?.[1]
    ) {
      if (
        points.length === 0 ||
        num.dist(pen.x, pen.y, ...points[points.length - 1]) > 4 // Check distance b/w the last point if it exists.
      ) {
        points.push([pen.x, pen.y]);
      }
    }
  } else if (points.length > 0) {
    // TODO: Play a sound here.
    // whistle.note(12);
    // whistle.pop(50);
    // If we stopped drawing after points have been added to.
    // points = [];
  }

  if (points.length > 20) {
    load("starfield");
  }
}

// 🎨 Render
const bgColor = [25, 25, 50];

export function render($api) {
  const { clear, color, load, line, pen, plot, num } = $api;

  if (firstFrame) {
    color(...bgColor);
    clear(); // Always clear if the line is changing.
    firstFrame = false;
    return;
  }

  // TODO: THIS SHOULD NOT BE HERE
  //if (pen.changed) {
  color(...bgColor);
  clear();
  //}

  if (points.length >= 1) {
    // Pline
    color(100, 100, 150);
    for (let i = 0; i < points.length - 1; i += 1) {
      line(...points[i], ...points[i + 1]);
    }

    line(...points[points.length - 1], curX, curY);

    // Points
    color(255, 255, 255);
    for (let i = 0; i < points.length; i += 1) {
      const x = points[i][0] + -1 + num.randInt(2);
      const y = points[i][1] + -1 + num.randInt(2);
      plot(x, y);
      //plot(...points[i]);
    }
  } else {
    // return false; // Prevents rendering unchanged frames to the buffer.
  }
}
