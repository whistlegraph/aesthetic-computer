// i, 22.07.29.16.42
// An environment / game where the player character looks like an i

// TODO: Jeffrey
// - [] Polish off touch based controls for phone.
// - [] Add the ability to have words drawn on the screen.
// - [] Typing in and entering words.
//   - [] Something that reacts, like a `grow` word to get longer?
//   - [] Need word ideas...
//   - []
/*


to add 
camera = x and y center and follow main character, 
mouse controls allow for kloana style camera peaking,
when camera edge reaches level endg collision stop moving and let character approach the collision edge.

world = level drawing and collision, enter and exit indoors/outdoors, mini map/world man?

border collision =
 


*/
// TODO: Resarch
// [] Add 2d physics engine.
// [] Look at old `flower eater` / `mood` engine C source and videos.
// [] Add a portal / doorway to another piece / room for this piece.

const debug = true;

const { abs, sign, min } = Math;

class I {
  dir = { x: 0, y: 0 };
  vel = { x: 0, y: 0 };
  len = 3;
  constructor() {
    this.pos = { x: 15, y: 10 };
  }

  get geometry() {
    return {
      point: [this.pos.x, this.pos.y - this.len - 2], // Head
      line: [
        [this.pos.x, this.pos.y - this.len], // Top of body.
        [this.pos.x, this.pos.y], // Bottom of body.
      ],
    };
  }
}

const i = new I();

// 🥾 Boot (Runs once before first paint and sim)
function boot({ density, resize, cursor, screen }) {
  // TODO: Runs only once!
  cursor("none");
  //resize(screen.width / 2, screen.height / 2);
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, line, point, num: { intersects } }) {
  wipe(255, 50, 0); // Draw a gray background.

  // TODO: How to collide with red?
  // - [] Match them with red line coordinates in a collision calculation.
  // - [] Draw a different color if it's "colliding".
  // - [*] Need to get coordinates of "i"

  // Now that we have the geometry... we can test the point and line
  // inside i against the red line we draw below at [30, 30, 60, 60];

  const redLine = [30, 30, 60, 60];
  const g = i.geometry;

  // Now all we need to do is check if
  //   intersects(redLine, g.line) OR intersects(redLine, g.point);
  // So we need a "line intersects line" function and a "line intersects point" function
  // or "algorithm" - they may be the same one... so let's go hunting!!!

  // These should return `true` if they intersect.
  const didIntersectLine = intersects(...redLine, ...g.line[0], ...g.line[1]);
  const didIntersectPoint = intersects(...redLine, ...g.point, ...g.point); // Just make a line with two `point`s.

  // Doesn't seem to work...
  // Forgot to unpack my geometry data i made properly...

  // Make a "line" in the world.
  ink(255, 0, 0); // Red ink.
  if (didIntersectLine || didIntersectPoint) ink(255, 255, 0); // Yellow ink only if `i` intersects.
  line(...redLine);

  ink(0, 0, 255); // Blue ink.
  line(60, 30, 30, 60);

  // Draw the i character.
  ink(255, 255, 255);

  // A. With the geometry getter in the `i` Class above...
  point(...g.point).line(...g.line[0], ...g.line[1]);

  // B. (or) without the geometry...
  //pan(i.pos.x, i.pos.y - i.len); // Draw i at it's coordinates.
  //point(0, -2);
  //line(0, 0, 0, i.len);
  //unpan();

  // Draw the center point of i in PINK.
  if (debug) ink(255, 0, 255).point(i.pos.x, i.pos.y);

  return false; // Only once.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim({ needsPaint }) {
  if (i.dir.x !== 0) {
    i.pos.x += sign(i.dir.x) * i.vel.x;
    i.vel.x = min(i.vel.x * 1.1, 0.4);
  }

  if (i.dir.y !== 0) {
    i.pos.y += sign(i.dir.y) * i.vel.y;
    i.vel.y = min(i.vel.y * 1.1, 0.4);
  }

  // TODO: Replace with vec2 mag function?
  if (i.dir.x !== 0 || i.dir.y !== 0) needsPaint();
}

// ✒ Act (Runs once per user interaction)
function act({ event: e }) {
  // Pen Controls
  if (e.is("draw")) {
    i.dir.x = sign(e.delta.x);
    i.vel.x = 0.1;

    i.dir.y = sign(e.delta.y);
    i.vel.y = 0.1;
  }

  if (e.is("lift")) {
    i.vel.x = 0;
    i.vel.y = 0;
  }

  // Keyboard Controls
  if (e.is("keyboard:down") && !e.repeat) {
    switch (e.key) {
      case "ArrowRight":
        i.dir.x = 1;
        i.vel.x = 0.1;
        break;
      case "ArrowLeft":
        i.dir.x = -1;
        i.vel.x = 0.1;
        break;
      case "ArrowUp":
        i.dir.y = -1;
        i.vel.y = 0.1;
        break;
      case "ArrowDown":
        i.dir.y = 1;
        i.vel.y = 0.1;
        break;
    }
  }

  if (e.is("keyboard:up")) {
    switch (e.key) {
      case "ArrowRight":
        i.dir.x = 0;
        i.vel.x = 0;
        break;
      case "ArrowLeft":
        i.dir.x = 0;
        i.vel.x = 0;
        break;
      case "ArrowUp":
        i.dir.y = 0;
        i.vel.y = 0;
        break;
      case "ArrowDown":
        i.dir.y = 0;
        i.vel.y = 0;
        break;
    }
  }
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library (Useful functions used throughout the piece)
export { boot, sim, paint, act, beat };
