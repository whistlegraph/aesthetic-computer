// Paste, 23.05.07.13.38
// Load an image from an external source.

/* #region 📓 TODO 
  - [] Add exact width and height scaler to `paste`.
  - [] Optimize / speed up `paste` function by alot?
  + Done
  - [x] Handle file cancel state. (It currently hangs)
  - [x] Get this updating for larger files without frame skips / interaction weirdness.
  - [x] Just load a local file for now.
#endregion */

let needsPaste = false;

function boot($) {
  request($);
}

function bake({ paste, store }) {
  if (needsPaste) {
    paste(store["file:opened"], 0, 0, 1 / 6);
    needsPaste = false;
  }
}

function sim({ system, store }) {
  if (store["paste:ready"]) {
    needsPaste = true;
    system.nopaint.needsBake = true;
    delete store["paste:ready"];
  }
}

function act({ api, event: e, store }) {
  if (e.is("touch") && !store["paste:ready"]) request(api);
}

export const system = "nopaint";
export { boot, bake, sim, act };

// 📚 Library (Useful functions used throughout the piece)

// Ask for a file.
function request({ file, store }) {
  file()
    .then((f) => {
      store["file:opened"] = f;
      store["paste:ready"] = true;
      // The continuation of this is handled inside of `paste` -> `boot`.
    })
    .catch((err) => {
      console.error(err);
    });
}