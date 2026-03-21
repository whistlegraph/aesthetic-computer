// Paste, 23.05.07.13.38
// Load an image from an external source.

/* #region 📓 TODO 
  - [❤️‍🔥] Add mobile support.
    - Added... just need to test.
  - [-] Add exact width and height as two parameters.
  - [] Resizable and rotate-able as well?
  - [] Optimize / speed up `paste` function by alot?
  + Later
  - [] Integrate `pan` into `bake`. (Add pan support to bake.)
  + Done
  - [x] Make the image draggable after it loads.
  - [x] Handle file cancel state. (It currently hangs)
  - [x] Get this updating for larger files without frame skips / interaction weirdness.
  - [x] Just load a local file for now.
#endregion */

let img,
  scale = 1,
  x = 0,
  y = 0;

function boot($) {
  request($); // Ask the user to choose an image.
  x = $.screen.width / 2; // Set the starting position to screen center.
  y = $.screen.height / 2;
  scale = parseFloat($.params[0] || 1); // Set scale from first param.
}

function paint({ paste }) {
  if (!img) return;
  paste(img, ...transform(), scale);
}

function bake({ paste, system, unpan }) {
  // TODO: Pan doesn't work here because it needs to respect the
  //       upper transform. 23.05.19.17.07
  const transformed = transform();
  const x = transformed[0] - system.nopaint.translation.x;
  const y = transformed[1] - system.nopaint.translation.y;
  paste(img, x, y, scale);
}

function transform() {
  const half = { w: (img.width / 2) * scale, h: (img.height / 2) * scale };
  return [x - half.w, y - half.w];
}

function act({ api, event: e, store }) {
  if (e.is("touch") && !img) request(api);

  if (e.is("draw") && img) {
    x += e.delta.x;
    y += e.delta.y;
  }
}

export const system = "nopaint:bake-on-leave";
export { boot, paint, bake, act };

// 📚 Library (Useful functions used throughout the piece)

// Ask for a file.
function request({ file }) {
  file()
    .then((f) => (img = f))
    .catch((err) => console.error(err));
}