// Blur, 24.02.23.13.06
// Blur pixels with a given radius.

/* #region ✅ TODO
  - [] @jeffrey: can't switch buffers in `brush` function so defaulted to paint
#endregion */

// 📰 Meta
function meta() {
  return {
    title: "Blur",
    desc: "Blur pixels with a given radius.",
  };
}

const blurRad = 2;
const featherRad = 10;
const minBlurSamples = 64;
const sampleScalingFactor = 4;

let radius;

function paint({ ink, num, pen, params, page, pixel, screen, system }) {
  if (!radius) {
    params = params.map((str) => parseInt(str));
    radius = params[0] || 16;
  }
  const nopaint = system.nopaint;

  const numBlurSamples = Math.max(minBlurSamples, radius * sampleScalingFactor);

  if (nopaint.is("painting")) {
    page(screen);
    ink(255, 0, 0).circle(pen.x, pen.y, radius); // Circle overlay.
    page(system.painting);

    const brush = nopaint.brush;

    const paintingWidth = system.painting.width;
    const paintingHeight = system.painting.height;

    for (let i = 0; i < numBlurSamples; i++) {
      const angle = Math.random() * 2 * Math.PI;
      const distance = Math.random() * radius;
      const randomX = Math.cos(angle) * distance;
      const randomY = Math.sin(angle) * distance;
      const xy = [brush.x + randomX, brush.y + randomY];

      const srcColor = pixel(...xy, system.painting);

      const distToCenter = Math.sqrt(randomX * randomX + randomY * randomY);
      const distFromRadius = radius - distToCenter;

      let avgCol = [0, 0, 0, 0];

      let numSamples = 0;

      for (let bx = -blurRad; bx < blurRad; bx++) {
        for (let by = -blurRad; by < blurRad; by++) {
          if (bx * bx + by * by < blurRad * blurRad) {

            const sampleXY = [
              Math.max(1, Math.min(xy[0] + bx, paintingWidth-1)),
              Math.max(1, Math.min(xy[1] + by, paintingHeight-1))
            ];

            const sampleCol = pixel(...sampleXY, system.painting);

            for (let i = 0; i < 4; i++) {
              avgCol[i] += sampleCol[i];
            }
            numSamples++;
          }
        }
      }

      for (let i = 0; i < 4; i++) avgCol[i] /= numSamples;
      
      avgCol[3] = 255;

      const lerpAmt = Math.min(1, distFromRadius / featherRad);

      for (let i = 0; i < 4; i++) {
        avgCol[i] = num.lerp(srcColor[i], avgCol[i], lerpAmt);
      }

      ink(...avgCol).point(...xy);
    }
  }
}

export const system = "nopaint";
export { paint };
