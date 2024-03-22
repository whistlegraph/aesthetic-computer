// Staka, 2023.6.17.17.01.49
// Stack colors with your hand!

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
+ Now
- [🟠] Ball bounce at different angles related to the plate.
    - [] Prototype this in `balls`.
    - [] This could be a question of using `force` instead of a direct
         angle change.
    - [] Also we could skip some of these phsyics by integrating matter.js
         or bullet or another physics engine.
    - [] How to have the ball bounce off the paddle without sticking or sliding.
- [] Create safe zone boundary that the game takes place in. 
- [] Clean up "dead-zone" edges by tracking some video pixels outside
      of the screen boundery. (For all hand-tracking) 
- [] Title Screen
- [] Game Over
- [] Sound
- [] Rename Staka
+ Done
- [x] Dummy gesture no longer functions.
- [x] Tapping and holding the mouse down when a hand is not present should
      drop the y value of the dummy data of IMO, so the gesture gets
      recognized and the game is playable / testable with no tracking!
- [x] Generate falling shapes.
- [x] Collide falling shapes on the line.
- [x] Recognize shaka gesture based on T and P y values 
- [x] Draw line between T and P 
- [x] Bug for unpan() needs fixing ! :D
- [x] Default hand bouncy line not working
#endregion */

import { HandInput } from "../lib/hand.mjs";
let plate, touching;

const startingSpeed = 1;

const ball = {
  color: Math.floor(Math.random() * 16777215).toString(16),
  speed: startingSpeed,
  angle: 160
};

// 🥾 Boot
let handInput;
function boot({ num, geo, screen }) {
  handInput = new HandInput();
  const radius = 16;
  // ball.circle = new geo.Circle(num.randInt(screen.width), -radius, radius);
  ball.circle = new geo.Circle(screen.width / 2, screen.height / 2 - 100, radius);
}

// 🎨 Paint
function paint($) {
  const {
    wipe,
    ink,
    screen: { width, height },
    num,
    pan,
    unpan,
  } = $;

  wipe(127);

  handInput.paint($, { faded: plate !== undefined }); // Uses calculated points.

  if (plate) {
    if (handInput.dummy) pan(...handInput.dummyPan);
    ink(255, 96).pline(
      [
        { x: plate[0][0], y: plate[0][1] },
        { x: plate[1][0], y: plate[1][1] },
      ],
      12
    );
    ink(255).line(...plate)
    if (handInput.dummy) unpan();
  }

  //Draw Circle
  ink(ball.color).circle(
    ball.circle.x,
    ball.circle.y,
    ball.circle.radius,
    true
  );
  ink(255, 255, 0).circle(ball.circle.x, ball.circle.y, ball.circle.radius);
}

// 🧮 Sim
function sim($) {
  handInput.sim($); // Calculate the hand points.
  // Runs once per logic frame. (120fps locked).

  // Update both ball.circle.x and ball.circle.y by projecting
  // by speed amount, in a given direction. 

  // const travel = $.num.p2.rot(ball.speed, radians); 

  // 🏀 Update the ball.
  const radians = $.num.radians(ball.angle);
  ball.circle.x += ball.speed * Math.cos(radians); 
  ball.circle.y += ball.speed * Math.sin(radians);

  // Check for collision with ball plate.
  if (plate) {
    let vecDistance;
    const A = { x: plate[0][0], y: plate[0][1] };
    const B = { x: plate[1][0], y: plate[1][1] };

    if (handInput.dummy === true) {
      const dp = {x: handInput.dummyPan[0], y: handInput.dummyPan[1] };
      $.num.p2.inc(A, dp); // Add dp to A and B
      $.num.p2.inc(B, dp);
    }

    const C = { x: ball.circle.x, y: ball.circle.y };
    const AC = $.num.p2.sub(C, A);
    const AB = $.num.p2.sub(B, A);
    const D = $.num.p2.add(proj($, AC, AB), A);

    vecDistance = $.num.dist(ball.circle.x, ball.circle.y, D.x, D.y);
    const AD = $.num.p2.sub(D, A);
    const k = Math.abs(AB.x) > Math.abs(AB.y) ? AD.x / AB.x : AD.y / AB.y;

    // Print out the angle of the plate and the angle of the ball in the 
    // console.
    const plateRadians = $.num.p2.angle(A, B);
    const plateDegrees = $.num.degrees(plateRadians);

    // If plate collides with ball.
    if (k > 0.0 && k < 1.0 && vecDistance < ball.circle.radius) {
      // ball.collided = true;
      // setTimeout(() => ball.collided = false, 50);
      ball.angle = ((360 - ball.angle) + plateDegrees); 
      // Move the ball in its new direction by vecDistance?
      const radians = $.num.radians(ball.angle);
      const dist = ball.circle.radius - vecDistance;
      ball.circle.x += dist * Math.cos(radians); 
      ball.circle.y += dist * Math.sin(radians);
    }
  }

  if (ball.circle.y < 0) ball.angle = 360 - ball.angle; // Bounce of the top.
  if (ball.circle.x < 0) ball.angle = 180 - ball.angle; // Left
  if (ball.circle.x > $.screen.width) ball.angle = 180 - ball.angle; // Right

  // Move ball back to the top of the screen and randomize its properties
  // if it leaves the bottom.
  if (ball.circle.y > $.screen.height) {
    ball.circle.y = ball.circle.radius;
    ball.circle.x = $.num.randInt($.screen.width);
    ball.circle.radius = Math.floor(Math.random() * 20) + 10;
    ball.color = Math.floor(Math.random() * 16777215).toString(16);
    ball.speed = genSpeed();
  }

  // 🖐️ Hand
  const timop = handInput.timop;
  const dummyPoints = handInput.dummyPoints;

  if (handInput.dummy) {
    // dummy data
    const t = dummyPoints[4],
      i = dummyPoints[8],
      m = dummyPoints[12],
      o = dummyPoints[16],
      p = dummyPoints[20];

    if (
      t[1] < i[1] &&
      t[1] < m[1] &&
      t[1] < o[1] && // if t is higher than imo
      p[1] < i[1] &&
      p[1] < m[1] &&
      p[1] < o[1] // and p is higher than imo
    ) {
      plate = [dummyPoints[4], dummyPoints[20]];
    } else {
      plate = undefined;
    }
  } else {
    //real data
    const t = timop[0],
      i = timop[1],
      m = timop[2],
      o = timop[3],
      p = timop[4];

    if (
      // gesture recognizer
      t[1] < i[1] &&
      t[1] < m[1] &&
      t[1] < o[1] && // if t is higher than imo
      p[1] < i[1] &&
      p[1] < m[1] &&
      p[1] < o[1] // and p is higher than imo
    ) {
      plate = [timop[0], timop[4]];
    } else {
      plate = undefined;
    }
  }
}

// 🎪 Act
function act($) {
  handInput.act($);
  if ($.event.is("touch")) {
    handInput.dummyGesture =
      handInput.dummyGesture === "shaka" ? "open" : "shaka";
  }
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
    title: "Staka",
    desc: "Stack colors with your hand!",
  };
}

export { boot, act, meta, paint, sim };

// 📚 Library
//   (Useful functions used throughout the piece)

function genSpeed () {
  return Math.random() * 2 + 0.5;
}

function proj($, a, b) {
  const k = $.num.p2.dot(a, b) / $.num.p2.dot(b, b);
  return { x: k * b.x, y: k * b.y };
}