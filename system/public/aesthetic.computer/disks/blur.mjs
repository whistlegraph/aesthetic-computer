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

let radius;

function paint({ ink, pen, params, page, pixel, screen, system }) {
  if (!radius) {
    params = params.map((str) => parseInt(str));
    radius = params[0] || 16;
  }
  const nopaint = system.nopaint;

  const numBlurSamples = Math.max(128, radius * 8);

  if (nopaint.is("painting")) {
    page(screen);
    ink(255, 0, 0).circle(pen.x, pen.y, radius); // Circle overlay.
    page(system.painting);

    const brush = nopaint.brush;
    const blurRad = 2;

    let srcColor = pixel(brush.x, brush.y, system.painting);

    for (let i = 0; i < numBlurSamples; i++) {
      const randomX = Math.floor(Math.random() * (2 * radius)) - radius;
      const randomY = Math.floor(Math.random() * (2 * radius)) - radius;
      const xy = [brush.x + randomX, brush.y + randomY];
      let avgCol = [0, 0, 0, 0];

      let numSamples = 0;

      for (let bx = -blurRad; bx < blurRad; bx++) {
        for (let by = -blurRad; by < blurRad; by++) {
          if (bx * bx + by * by < blurRad * blurRad) {
            const sampleXY = [xy[0] + bx, xy[1] + by];
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
      ink(...avgCol).point(...xy);
    }
  }
}

export const system = "nopaint";
export { paint };
