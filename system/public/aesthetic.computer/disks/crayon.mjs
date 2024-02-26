// Crayon, 2024.2.26.13.24.47.880
// Draw with a crayon.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

const randColChangeInterval = 100;

const minNumDots = 30;
const maxNumDots = 100;

const minDotRadius = 1;
const maxDotRadius = 3;

const maxScatterRadius = 7;
const maxColChange = 25;

let colorParams;
let gotColor = false;
let rainbowCrayonCol;
let frameCount = 0;
let randCol;

// 🥾 Boot
function boot({ params, num }) {
  // Runs once at the start.
  colorParams = num.parseColor(params);

  if (colorParams.length > 0) {
    gotColor = true;
  }

  else {
    rainbowCrayonCol = [...num.randIntArr(255, 3), 255];
    randCol = [...num.randIntArr(255, 3), 255];
  }
}

function brush({ ink, num, pen }) {
  frameCount++;

  let baseCol;

  if (gotColor) {
    baseCol = colorParams;
  }
  else {
    if (frameCount % randColChangeInterval == 0) {
      randCol = [...num.randIntArr(255, 3), 255];
    }

    rainbowCrayonCol = rainbowCrayonCol.map((col, index) => num.lerp(col, randCol[index], 0.01));

    baseCol = rainbowCrayonCol;
  }

  let numDots = num.randInt(minNumDots, maxNumDots);

  for (let i = 0; i < numDots; i++) {
    let dotRadius = num.randIntRange(minDotRadius, maxDotRadius);

    let angle = num.rand() * Math.PI * 2;

    let scatterRadius = num.randIntRange(0, maxScatterRadius);

    let x = pen.x + (Math.cos(angle) * scatterRadius);
    let y = pen.y + (Math.sin(angle) * scatterRadius);

    let thisDotCol = baseCol;

    thisDotCol = thisDotCol.map((col, index) => {
      return num.clamp(col + num.randInt(-maxColChange, maxColChange), 0, 255);
    });

    ink(thisDotCol).circle(x, y, dotRadius, true);
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Crayon",
    desc: "Draw with a crayon.",
  };
}

export { boot, brush, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
