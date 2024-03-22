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
  pan,
  pen,
  unpan,
  params,
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

  const nopaint = system.nopaint;

  if (nopaint.is("painting")) {
    const brush = nopaint.brush;
    // Crop the back buffer to the radius of the circle.
    if (sampled === false) {
      if (samples.length > 64) samples.length = 64; // Chop some samples.
      repeat(128, (i) => {
        const ang = r(360);
        const dst = r(radius);
        const xy = pointFrom(brush.x, brush.y, ang, dst);
        const sample = new Sample(
          xy[0] - brush.x,
          xy[1] - brush.y,
          pixel(...xy, system.painting),
        );
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
    pan(brush.x, brush.y);
    samples.forEach((sample) => {
      // TODO: Physically drift sample positions based on gesture.
      sample.x += rr(-1, 1);
      sample.y += rr(-1, 1);
      // TODO: Drift color.
      //sample.c[0] += rr(-5, 5);
      //sample.c[1] += rr(-5, 5);
      //sample.c[2] += rr(-5, 5);
      ink(sample.c).point(sample.x, sample.y);
    });
    unpan();
    page(screen);
  }
}

// TODO: How to get this to not override everything?
function act($) {
  if ($.event.is("lift")) {
    sampled = false;
    samples.length = 0;
  }
}

export { paint, act };

export const system = "nopaint";

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
