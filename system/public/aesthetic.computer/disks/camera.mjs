// Camera, 2022.6.19.11.16
// A simple video feedback test.
// TODO: Get this working on iOS (Mobile Safari).

/* #region 🏁 todo
- [🍏] Add this camera to the "nopaint" system!
- [] Camera needs a "title" screen if camera isn't already enabled.
- [] If user doesn't accept camera, then we send the back to prompt with
     an error message of some kind.
- [] Add pixel shaders and or GPU rendered shaders!
  - [] See `bios.mjs:2431` video effects.
+ Done
- [x] Camera needs to take up the whole display by default. 
  - [🟡] Test all browsers...
    - [] Reset video on resize. 
#endregion */

const { floor } = Math;
let vid;

// 🥾 Boot (Runs once before first paint and sim)
//function boot({ screen, video }) {
//}

// 🎨 Paint (Runs once per display refresh rate)
function paint({
  wipe,
  page,
  system,
  paste,
  video,
  screen,
  num: { randIntRange, clamp, rand },
}) {
  if (!vid) {
    vid = video("camera", {
      width: screen.width,
      height: screen.height, //floor(screen.width / (16 / 9)),
    });
  }

  // wipe(15, 20, 0); // Clear the background.
  // Draw the video.
  const frame = vid(function shader({ x, y }, c) {
    if (rand() > 0.1) {
      c[3] = randIntRange(25, 50);
    }

    // Sparkles...
    if (rand() > 0.98) {
      c[0] = clamp(c[0] + randIntRange(50, 150), 0, 255);
      c[1] = clamp(c[1] + randIntRange(50, 150), 0, 255);
      c[2] = clamp(c[2] + randIntRange(50, 150), 0, 255);
    }
  });

  if (frame) {
    paste(frame, 0, 0);
    page(system.painting).paste(frame, 0, 0);
  }

}


export const system = "nopaint";
export { paint };

// 📚 Library (Useful functions used throughout the program)
// ...
