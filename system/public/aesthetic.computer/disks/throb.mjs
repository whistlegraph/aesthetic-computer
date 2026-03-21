// Miles, 2024.2.20.14.18.14.523
// Miles first piece.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

var blobPoints = [];
var blobRadius = 0.1;
var numBlobPoints = 100;

var pulseAng = 0.0;

var pulseDriver = 0.0;

var growFactor = 1.0;

var growDirection = 1.0;
var maxGrowFactor = 13.0;

// 🥾 Boot
function boot({ api }) {
  console.log("👢 Booting Miles");
  //generate numBlobPoints points around a circle centered at 0.5, 0.5 and radius blobRadius
  for (var i = 0; i < numBlobPoints; i++) {
    var angle = (i / numBlobPoints) * Math.PI * 2;
    var x = Math.cos(angle) * blobRadius;
    var y = Math.sin(angle) * blobRadius;
    blobPoints.push({ x: x, y: y });
  }
}

// 🎨 Paint
function paint({ wipe, ink, api, screen }) {
  wipe("pink");

  let warbleAmp = 0.001 * (Math.sin(pulseAng)*0.5 + 0.5) * growFactor;

  let points = [];

  let numBulges = 8;

  let angInc = Math.PI * 2 / blobPoints.length;

  let strokeWidth = growFactor;

  let blobSize = Math.max(screen.width, screen.height) * 0.5 * growFactor;
  let center = { x: screen.width * 0.5, y: screen.height * 0.5 };

  for(var i = 0; i < blobPoints.length; i++) {
    let point = blobPoints[i];

    let offsetX = Math.sin(Math.pow(Math.max(i * angInc * numBulges + 0.5 * Math.sin(pulseDriver), 0.0), 0.8)) * warbleAmp;
    let offsetY = Math.cos(Math.pow(Math.max(i * angInc * numBulges + 0.5 * Math.sin(pulseDriver), 0.0), 0.8)) * warbleAmp;

    let pointX = (point.x + offsetX) * blobSize + center.x;
    let pointY = (point.y + offsetY) * blobSize + center.y;

    pointX = Math.max(strokeWidth/2, Math.min(screen.width-strokeWidth/2, pointX));
    pointY = Math.max(strokeWidth/2, Math.min(screen.height-strokeWidth/2, pointY));

    points.push([pointX, pointY]);
  }

  //add zeroth point to the end to close the loop
  points.push(points[0]);

  ink(0, 255, 255).pline(points, strokeWidth);
}

// 🎪 Act
// function act({ event: e }) {
//  // Respond to user input here.
// }

// 🧮 Sim
function sim() {
  pulseDriver += 0.1;
  // growFactor = 1.0 + (Math.sin(pulseDriver) * 0.5 + 0.5) * 5.0;

  if(growFactor > maxGrowFactor && growDirection > 0) {
    growDirection = -1;
  }

  else if(growFactor < 1.0 && growDirection < 0) {
    growDirection = 1;
  }

  let growInc = (Math.sin(pulseDriver) + 0.2 * growDirection) * 0.005;

  growFactor += growInc * growFactor;

  // Runs once per logic frame. (120fps locked.)
  pulseAng += 0.3 * (Math.sin(pulseDriver) * 0.5 + 0.5);
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
    title: "throb",
    desc: "A blob that throbs.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { paint, meta, boot, sim };

// 📚 Library
//   (Useful functions used throughout the piece)
