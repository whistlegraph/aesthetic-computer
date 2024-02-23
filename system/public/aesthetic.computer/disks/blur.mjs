/* #region ✅ TODO
  - [] can't switch buffers in brush

#endregion */

// 📰 Meta
function meta() {
  return {
    title: "Blur",
    desc: "Blur pixels under brush.",
  };
}

let radius;

function paint({
  ink,
  pan,
  pen,
  unpan,
  params,
  page,
  pixel,
  screen,
  system,
  num: { randInt: r, randIntRange: rr },
  geo: { pointFrom },
  help: { repeat },
}) {
  if (!radius) {
    params = params.map((str) => parseInt(str));
    radius = params[0] || 16;
  }
  const nopaint = system.nopaint;

  if (nopaint.is("painting")) {

    page(screen);
    ink(255, 0, 0).circle(pen.x, pen.y, radius); // Circle overlay.
    page(system.painting);


    const brush = nopaint.brush;

    const blurRad = 2;

    for (let x = -radius; x < radius; x++) {
      for (let y = -radius; y < radius; y++) {
        if (x * x + y * y < radius * radius) {

          const xy = [brush.x + x, brush.y + y];

          let pixelCol = pixel(...xy, system.painting);

          let avgCols = [0, 0, 0, 0];

          let numSamples = 0;

          for (let bx = -blurRad; bx < blurRad; bx++) {
            for (let by = -blurRad; by < blurRad; by++) {
              if (bx * bx + by * by < blurRad * blurRad) {
                const sampleXY = [xy[0] + bx, xy[1] + by];
                const sampleCol = pixel(...sampleXY, system.painting);
                for (let i = 0; i < 4; i++) {
                  avgCols[i] += sampleCol[i];
                }

                numSamples++;
              }
            }
          }

          for (let i = 0; i < 4; i++) {
            avgCols[i] /= numSamples;
          }

          avgCols[3] = 255;

          ink(...avgCols).point(...xy);
        }
      }
    }
  }
}

export const system = "nopaint";
export { paint };

