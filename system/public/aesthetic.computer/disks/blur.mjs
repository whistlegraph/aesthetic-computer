// Blur, 24.02.23.13.06
// Blur pixels with a given radius.

/* #region ✅ TODO
  + Done
  - [x] @jeffrey: can't switch buffers in `brush` function so defaulted to paint
#endregion */

const blurRad = 2; // 🍃 These params control the form of the blur,
const featherRad = 10;
const minBlurSamples = 64; // 🏃‍♀️ and these affect the speed of the blur.
const sampleScalingFactor = 4;

let radius;

const { random, cos, sin, sqrt, max, min, PI } = Math;

function paint({ ink, pen, hud }) {
  if (pen?.drawing && !hud.currentLabel().btn.down)
    ink("red").circle(pen.x, pen.y, radius); // Red Cursor.
}

function brush({ ink, num, pen, params, pixel, system }) {
  if (!radius) {
    params = params.map((str) => parseInt(str));
    radius = params[0] || 16;
  }
  const numBlurSamples = max(minBlurSamples, radius * sampleScalingFactor);

  for (let i = 0; i < numBlurSamples; i++) {
    const angle = random() * 2 * PI;
    const distance = random() * radius;
    const randomX = cos(angle) * distance;
    const randomY = sin(angle) * distance;
    const xy = [pen.x + randomX, pen.y + randomY];
    const srcColor = pixel(...xy, system.painting);
    const distToCenter = sqrt(randomX * randomX + randomY * randomY);
    const distFromRadius = radius - distToCenter;

    let avgCol = [0, 0, 0, 0],
      numSamples = 0;

    for (let bx = -blurRad; bx < blurRad; bx += 1) {
      for (let by = -blurRad; by < blurRad; by++) {
        if (bx * bx + by * by < blurRad * blurRad) {
          const sampleXY = [
            max(1, min(xy[0] + bx, system.painting.width - 1)),
            max(1, min(xy[1] + by, system.painting.height - 1)),
          ];
          const sampleCol = pixel(...sampleXY, system.painting);
          for (let i = 0; i < 4; i++) avgCol[i] += sampleCol[i];
          numSamples++;
        }
      }
    }

    for (let i = 0; i < 4; i++) avgCol[i] /= numSamples;
    avgCol[3] = 255;
    const lerpAmt = min(1, distFromRadius / featherRad);

    for (let i = 0; i < 4; i++) {
      avgCol[i] = num.lerp(srcColor[i], avgCol[i], lerpAmt);
    }

    ink(...avgCol).point(...xy);
  }
}

export { paint, brush };
