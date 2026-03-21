// Sparkle, 22.09.19.12.44
// A sparkle emitter brush by @maya and @jeffrey.

/* #region 🏁 todo
  + ⏰ Now
  - [] @maya Extract / dump sparkleMode 1 code into `shiny`. 
      (It doesn't have to work)
  - [] @maya think 2-3 possible `params` / try implementing them and using them
       from the `prompt`.
  + Later
  - Session 1
  - [] @jeffrey + maya: code clean-up (rip out / retire code we aren't using) 
  - [] @jeffrey + maya: turtle graphics / directional movement on sparkles
  - Session 2
  - [] @jeffrey + maya: make shadows under the sparkles to show distance
                        from the surface? (psuedo 3d)
  - Session 3
  - [] @jeffrey + maya: read pixels under the existing image to influence
                        sparkle behavior 
  - [] Other symbols / mix of different shapes other than hearts?
  - [] Each symbol uses its own palette.
  - [] Revisit the abandoned brush: `shiny`.
  + Done
  - [x] Find a good general sparkle behavior.
#endregion */

let sparkle;
let sparkleBuffer = [];
let sparkleMode = 0;

export function boot({ hud, net }) {
  hud.label("@maya/sparkle");
  net.rewrite("@maya/sparkle");
}

// painting:reset to reset
// no!
export function paint($api) {
  //debugger;
  let {
    pen,
    ink,
    circle,
    params,
    system,
    paste,
    page,
    screen,
    num: { randInt: r },
  } = $api;
  // console.log($api);

  // maya's color palettes
  let apple = [0x545665, 0xdcc0cf, 0xf4f4f4, 0xfcf3f4, 0xf4f4df];
  let polly = [0xda5748, 0x744864, 0x72242a, 0x5f3f47, 0x302c4d];

  let nussbaum = [
    "#959379",
    "#cbd0c8",
    "#414429",
    "#666d3f",
    "#5e5814",
    "#e2c9d0",
  ];
  let skate = ["#847c7c", "#040404", "#deaebc", "#14140c", "#14141c"];
  let jelly = ["#84649c", "#8c5cb4", "#be81b4", "#7c3464", "#d4cedf"];
  let collector = [
    "#d8d3d1",
    "#efece4",
    "#8c9cb4",
    "#ece4e6",
    "#fcfcf4",
    "#8c9cb4",
  ];
  let tuxedo = ["#c4cccf", "#252a34", "#363539", "#88b8dd", "#fcf4f8"];

  let cardigan = ["#966a54", "#58855c", "#fa91a4", "#fc5484", "#c27c73"];

  let kiko = [
    "#f995d0",
    "#ffffff",
    "#e0cad5",
    "#b98586",
    "#171719",
    "#dec0c5",
    "#6b5e68",
  ];

  let strawberry = ["#efc0c1", "#f5423a", "#bdc9b8", "#ffffff"];

  // let mollyHeartColors = ['#ffa1c8', '#ff0801', '#ffffff', '#03fd00'];
  let mollyHeartColors = ["#ffa1c8", "#ff0801", "#e0cad5", "#ffffff"];
  let mollyStarColors = ["#ce8bac", "#faed93", "#faed93", "#f8dc9c"];

  const mycolors = [
    [150, 106, 84],
    [88, 133, 92],
    [250, 145, 164],
    [252, 84, 132],
    [194, 124, 115],
    [204, 118, 137],
    [84, 86, 101],
    [220, 192, 207],
    [244, 244, 244],
    [252, 243, 244],
    [244, 244, 223],
    [132, 100, 156],
  ];

  if (system.nopaint.is("painting") && pen?.drawing) {
    // @maya/brush 255 16
    // params = params.map((str) => parseInt(str));
    let chosenColorIndex = r(mollyHeartColors.length - 1);
    let chosenColor = mollyHeartColors[chosenColorIndex];

    // chosenColor = '#ffffff';

    if (sparkleMode == 0) {
      sparkleBuffer.push(sparkleFactory(pen, chosenColor, 1));
    } else if (sparkleMode == 1) {
      let chosenColorIndex = r(kiko.length - 1);
      let chosenColor = kiko[chosenColorIndex];
      sparkleBuffer.push(sparkleFactory(pen, chosenColor, 5));
    }

    // Make collection of objects
  }

  // Define the number of points on the star
  const numPoints = 5;

  // Define the radius of the star (distance from center to a vertex)
  const radius = r(10);

  // Define the inner radius (distance from center to a point halfway between two vertices)
  const innerRadius = radius * 0.5;

  // Calculate the angle between each point on the star (360 degrees divided by the number of points)
  const angle = 360 / numPoints;

  let starOffsetRange = 10;

  const calculateCoords = (xPos, yPos, radius, angle) => {
    return {
      x: xPos + radius * Math.cos(((angle - 90) * Math.PI) / 180),
      y: yPos + radius * Math.sin(((angle - 90) * Math.PI) / 180),
    };
  };

  sparkle = (offset = false) => {
    for (let s = 0; s < sparkleBuffer.length; s++) {
      // Loop through the number of points on the star
      let currentSparkle = sparkleBuffer[s];

      if (sparkleMode == 0) {
        let currentX = sparkleBuffer[s].position.x;
        let currentY = sparkleBuffer[s].position.y + currentSparkle.fallState;

        ink(sparkleBuffer[s].color);

        let size = currentSparkle.size;

        if (offset) {
          currentX -= system.nopaint.translation.x;
          currentY -= system.nopaint.translation.y;
        }

        circle(currentX, currentY, size);
        circle(currentX - size * 2, currentY - size * 2, currentSparkle.size);
        circle(currentX + size * 2, currentY - size * 2, currentSparkle.size);
      } else if (sparkleMode == 1) {
        let currentX = sparkleBuffer[s].position.x;
        let currentY = sparkleBuffer[s].position.y;

        if (offset) {
          currentX -= system.nopaint.translation.x;
          currentY -= system.nopaint.translation.y;
        }

        if (sparkleBuffer[s].flash && sparkleBuffer[s].flashing > 0) {
          ink(255);
          let sparkleOffset =
            -2 * currentSparkle.size + Math.random() * currentSparkle.size * 4;
          circle(currentX, currentY + sparkleOffset, 1);
        }

        for (let i = 0; i < numPoints; i++) {
          // Calculate the x and y coordinates of the current point
          const coords = calculateCoords(
            currentX,
            currentY,
            currentSparkle.size,
            angle * i,
          );
          const innerRadius = currentSparkle.size * 0.5;

          // Draw a circle at the current point
          let starCircleSize = 1;
          let starOffset = 10;
          // let starOffset = 10;
          if (sparkleBuffer[s].flash && sparkleBuffer[s].flashing > 0) {
            ink(255);
          } else {
            ink(sparkleBuffer[s].color);
          }
          // ink(sparkleBuffer[s].color)

          circle(coords.x, coords.y, starCircleSize);

          // Calculate the x and y coordinates of the point halfway between the current point and the next point
          const coords2 = calculateCoords(
            currentX,
            currentY,
            innerRadius,
            angle * (i + 0.5),
          );
          // Draw a circle at the midpoint
          // console.log(sparkleBuffer[s].color);
          circle(coords2.x, coords2.y, starCircleSize);
        }
      }
    }
  };

  sparkle();
  system.nopaint.needsPresent = true;
}

export function bake() {
  sparkle?.(true);
}

export function sim() {
  if (sparkleMode == 0) {
    for (let s = 0; s < sparkleBuffer.length; s++) {
      // Loop through the number of points on the star
      let currentSparkle = sparkleBuffer[s];
      let starOffsetX = -0.5 + Math.random() * 1;
      let starOffsetY = -0.5 + Math.random() * 1;

      currentSparkle.position.x += starOffsetX;
      currentSparkle.position.y += starOffsetY;

      if (currentSparkle.state == "active") {
        if (Math.random() > 0.9999) {
          currentSparkle.state = "falling";
        }
      } else if (currentSparkle.state == "falling") {
        currentSparkle.fallState += 1;
      }
    }
  } else if (sparkleMode == 1) {
    for (let s = 0; s < sparkleBuffer.length; s++) {
      // Loop through the number of points on the star
      let currentSparkle = sparkleBuffer[s];
      // let starOffsetX = Math.random();
      // let starOffsetY = Math.random() * 0.05;
      // currentSparkle.position.x += starOffsetX;
      // currentSparkle.position.y += starOffsetY;

      if (currentSparkle.flash && currentSparkle.flashing > -10) {
        currentSparkle.flashing--;
      } else if (currentSparkle.flash && currentSparkle.flashing <= -10) {
        currentSparkle.flashing = 10;
      }

      // console.log(currentSparkle.color);
      // if (currentSparkle.state == 'active') {
      //   if (Math.random() > 0.9999) {
      //     currentSparkle.state = 'falling';
      //   }
      // } else if (currentSparkle.state == 'falling') {
      //   // currentSparkle.fallState += 1;
      // }
    }
  }

  // Cascading series of states...

  if (Math.random() > 0.99) {
    // sparkleBuffer.shift();
    // sparkleBuffer.length - 1, 1
  }
}

function sparkleFactory(pen, color, size) {
  let position = {
    x: pen.x,
    y: pen.y,
  };

  let flash = false;

  if (Math.random() > 0.5) {
    flash = true;
  }

  return {
    position,
    color,
    size,
    flash,
    flashing: 10,
    state: "active",
    fallState: 0,
  };
}

export const system = "nopaint:bake-on-leave";
