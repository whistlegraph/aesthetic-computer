// Brush, 22.09.19.12.44

// ATTENTION:
// Both literal hex values like the ones below... or strings will work now! - Jeffrey

// ðŸŽ¨
// If `params` is empty then ink's RGBA will be randomized.
export function paint($api) {
  //debugger;
  let { pen, params, system, page, screen, num: { randInt: r } } = $api;
  console.log($api);
  console.log('hellooooo');
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

  if (pen.drawing) {
    // @maya/brush 255 16
    // params = params.map((str) => parseInt(str));

    const chosenColor = apple[r(apple.length)];
    // const radius = r(22);

    // // Drawing to the user's painting.
    // page(system.painting)
    //   .ink(chosenColor)
    //   .circle(pen.x, pen.y, radius);

    // // Drawing the same thing to the screen buffer.
    // page(screen)
    //   .ink(chosenColor)
    //   .circle(pen.x, pen.y, radius);

    // Define the number of points on the star
    const numPoints = 5;

    // Define the radius of the star (distance from center to a vertex)
    const radius = r(22);

    // Define the inner radius (distance from center to a point halfway between two vertices)
    const innerRadius = radius * 0.5;

    // Calculate the angle between each point on the star (360 degrees divided by the number of points)
    const angle = 360 / numPoints;

    // Define a function to calculate the x and y coordinates of a point on the star
    const calculateCoords = (radius, angle) => {
      return {
        x: pen.x + radius * Math.cos((angle - 90) * Math.PI / 180),
        y: pen.y + radius * Math.sin((angle - 90) * Math.PI / 180)
      };
    };

    // Loop through the number of points on the star
    for (let i = 0; i < numPoints; i++) {
      // Calculate the x and y coordinates of the current point
      const coords = calculateCoords(radius, angle * i);

      // Draw a circle at the current point
      page(system.painting)
        .ink(chosenColor)
        .circle(coords.x, coords.y, radius * 0.1);
      page(screen)
        .ink(chosenColor)
        .circle(coords.x, coords.y, radius * 0.1);

      // Calculate the x and y coordinates of the point halfway between the current point and the next point
      const coords2 = calculateCoords(innerRadius, angle * (i + 0.5));

      // Draw a circle at the midpoint
      page(system.painting)
        .ink(chosenColor)
        .circle(coords2.x, coords2.y, radius * 0.1);
      page(screen)
        .ink(chosenColor)
        .circle(coords2.x, coords2.y, radius * 0.1);
    }

  }
}

export const system = "nopaint"; // Uses a template for all the other functions.