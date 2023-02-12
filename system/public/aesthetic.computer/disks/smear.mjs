// Smear, 22.10.12.17.18
// A smear brush CTO'd by rapter.

// TODO: Add smear jiggling (on a ring buffer).
//       (Remember the path behind what the user draws.)
// TODO: Add a delay.

let radius,
  sampled = false;
const samples = [];

function paint({
  ink,
  pen,
  pan,
  unpan,
  params,
  paste,
  page,
  pixel,
  screen,
  system,
  num: { randInt: r, randIntRange: rr },
  geo: { pointFrom },
  help: { repeat },
}) {
  // Pull in the params and set the radius.
  if (!radius) {
    params = params.map((str) => parseInt(str));
    radius = params[0] || 16;
  }

  paste(system.painting);

  if (pen.drawing) {
    // Crop the back buffer to the radius of the circle.
    if (sampled === false) {
      if (samples.length > 64) samples.length = 64; // Chop some samples.
      repeat(128, (i) => {
        const ang = r(360);
        const dst = r(radius);
        const xy = pointFrom(pen.x, pen.y, ang, dst);
        const sample = new Sample(xy[0] - pen.x, xy[1] - pen.y, pixel(...xy));
        samples.push(sample); // Add sample to the list.
      });
      sampled = true;
    }

    page(screen);
    pan(pen.x, pen.y);
    samples.forEach((sample) => ink(sample.c).point(sample.x, sample.y));
    unpan();

    ink(255, 0, 0).circle(pen.x, pen.y, radius); // Circle overlay.

    page(system.painting);
    pan(pen.x, pen.y);
    samples.forEach((sample) => {
      // TODO: Physically drift sample positions based on gesture.
      //sample.x += clamp(sample.x + rr(-1, 1), 0, radius * 2);
      //sample.y += clamp(sample.y + rr(-1, 1), 0, radius * 2);
      sample.x += rr(-1, 1);
      sample.y += rr(-1, 1);
      // TODO: Drift color.
      //sample.c[0] += rr(-5, 5);
      //sample.c[1] += rr(-5, 5);
      //sample.c[2] += rr(-5, 5);
      ink(sample.c).point(sample.x, sample.y);
    });
    unpan();
  }
}

// TODO: How to get this to not override everything?
function act($) {
  $.system.nopaint.act($); // Inherit nopaint's act functionality.
  if ($.event.is("lift")) sampled = false;
}

export { paint, act };

export const system = "nopaint:dont-paint-on-leave";

// 📚 Library (Useful functions used throughout the piece)

class Sample {
  x;
  y;
  c;

  constructor(x, y, c) {
    this.x = x;
    this.y = y;
    this.c = c;
  }
}
