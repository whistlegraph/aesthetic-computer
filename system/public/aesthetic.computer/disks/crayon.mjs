// Crayon, 2024.2.26.13.24.47.880
// Draw with a crayon.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Depth based aggregation, waxy feel. 
  - [] Size parameter.
  - [] Easter egg colors like sparkle.
#endregion */

const randColChangeInterval = 100;

const minNumDots = 50;
const maxNumDots = 100;

const minDotRadius = 1;
const maxDotRadius = 2;

const maxScatterRadius = 7;
const maxColChange = 5;

const intervalSpacing = 5;

const minOpacity = 0.05;

const noiseScale = 300.0;
const noisePow = 2.0; //Higher makes the brush softer, lower makes it more harder.

let pPen = { x: -1, y: -1 };

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

  if(pPen.x == -1 && pPen.y == -1) {
    pPen = {x: pen.x, y: pen.y};
  }

  let baseCol;

  if (gotColor) {
    baseCol = colorParams;
  } else {
    if (frameCount % randColChangeInterval == 0) {
      randCol = [...num.randIntArr(255, 3), 255];
    }

    rainbowCrayonCol = rainbowCrayonCol.map((col, index) => num.lerp(col, randCol[index], 0.01));

    baseCol = rainbowCrayonCol;
  }

  const dx = pen.x - pPen.x;
  const dy = pen.y - pPen.y;
  const distance = Math.sqrt(dx * dx + dy * dy);
  
  const numInterpolations = Math.floor(distance / intervalSpacing);
  
  for (let j = 0; j <= numInterpolations; j++) {
    const t = numInterpolations === 0 ? 0 : j / numInterpolations;
    const interpolatedX = pPen.x + dx * t;
    const interpolatedY = pPen.y + dy * t;

    let numDots = num.randInt(minNumDots, maxNumDots);

    for (let i = 0; i < numDots; i++) {
      let dotRadius = num.randIntRange(minDotRadius, maxDotRadius);

      let angle = num.rand() * Math.PI * 2;

      let scatterRadius = num.randIntRange(0, maxScatterRadius);

      let x = interpolatedX + (Math.cos(angle) * scatterRadius);
      let y = interpolatedY + (Math.sin(angle) * scatterRadius);

      let dotOpacity = Math.max(Math.pow(num.perlin(x * noiseScale, y * noiseScale), noisePow), minOpacity);

      let thisDotCol = baseCol;

      for (let index = 0; index < 3; index++) {
        thisDotCol[index] = baseCol[index] + num.randIntRange(-maxColChange, maxColChange);
      }

      thisDotCol[3] = dotOpacity * 255;

      ink(thisDotCol).circle(x, y, dotRadius, true);
    }
  }

  pPen = {x: pen.x, y: pen.y};
}


function act($) {
  if ($.event.is("lift")) {
    pPen = {x: -1, y: -1};
  }
}


function meta() {
  return {
    title: "Crayon",
    desc: "Draw with a crayon.",
  };
}

export { boot, brush, meta, act };

// 📚 Library
//   (Useful functions used throughout the piece)
