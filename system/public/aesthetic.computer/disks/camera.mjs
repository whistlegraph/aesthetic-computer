// Camera, 2022.6.19.11.16
// A simple piece for taking stills and pasting them to the system painting.

/* #region 🏁 todo
+ Later
  - [] Add filters...
    - [] See `bios.mjs:4247` for GPU style video effects.
  - [] Give combined configurations top-level words, like `hellscape`.
       (Which would be an environment facing camera with a flame effect.)
  - [] Support more weird iOS cameras using parameters.
  - [] Add zoom-a-bility. 
+ Done
  - [x] Fix subtle rotation issues on iOS and potentially Android?
  - [x] Add background painting to camera.
  - [x] `cam:selfie` with a `selfie` alias
  - [x] Pixels fill only the painting frame.
  - [x] Alias `camera` to `cam`
  - [x] Add [Switch] button. (Cycle through all webcams.)
    - [x] Report back the device's camera count after first getting the video.
  - [x] If user doesn't accept camera, then we send the back to prompt with
     an error message of some kind.
  - [x] Switching between Front and Rear cameras / updating constraints.
  - [x] Automatically show the canvas border.
  - [x] Immediately freeze the frame on leaving.
  - [x] Reset / re-frame video on window resize. 
  - [x] Make sure iOS passes video-data in per each frame.
  - [x] Test mobile Safari 
  - [x] Automatically paint the whole buffer on leave by default.
  - [x] Add software shaders.
  - [x] Add this camera to the "nopaint" system!
  - [x] Camera needs to take up the whole display by default. 
#endregion */

let vid, snap;
let swap;
// let advance;
let facing = "environment";
let frame,
  under = false,
  underBuffer,
  capturing = true;

import * as sfx from "./common/sfx.mjs";

// 🥾 Boot
function boot({ ui, params, colon, clonePixels, system }) {
  swap = new ui.TextButton("Swap");
  if (params[0] === "me") facing = "user";
  if (colon[0] === "under" || colon[0] === "u") under = true;
  if (under) underBuffer = clonePixels(system.painting);
}

// 🎨 Paint (Runs once per display refresh rate)
function paint({
  api,
  wipe,
  paste,
  video,
  cameras,
  system,
  screen,
  num: { randIntRange, clamp, rand },
}) {
  // Initialize or update video feed.
  if (!vid) {
    wipe(0);
    vid = video(!vid ? "camera" : "camera:update", {
      width: system.painting.width, // width,
      height: system.painting.height, // height,
      facing,
    });
  }

  // Draw the video on each frame and add an effect.
  if (capturing) {
    frame = vid(function shader({ x, y }, c) {
      // ✨ Sparkles
      if (rand() > 0.98) {
        c[0] = clamp(c[0] + randIntRange(50, 150), 0, 255);
        c[1] = clamp(c[1] + randIntRange(50, 150), 0, 255);
        c[2] = clamp(c[2] + randIntRange(50, 150), 0, 255);
      }

      // Fade
      // if (rand() > 0.1) {
      //   c[3] = randIntRange(0, 5);
      // }
    });
  }

  // Paste the video to the main buffer.
  paste(frame, system.nopaint.translation.x, system.nopaint.translation.y);
  if (under) {
    paste(
      underBuffer,
      system.nopaint.translation.x,
      system.nopaint.translation.y,
    );
  }

  system.nopaint.needsPresent = true;

  snap = () => {
    paste(frame);
    if (under) paste(underBuffer);
    snap = null;
  };

  // User interface
  if (cameras > 1) {
    swap?.reposition({ bottom: 6, right: 6, screen });
    swap?.paint(api);
  }
}

function bake() {
  snap?.();
}

function act({
  event: e,
  jump,
  send,
  video,
  hud,
  cameras,
  sound,
  notice,
  leaving,
}) {
  if (e.is("lift") && !leaving() && swap.btn.down === false) {
    sfx.push(sound);
    jump("prompt")/*(() => send({ type: "keyboard:open" }));*/
  }

  if (cameras > 1) {
    swap?.btn.act(e, {
      down: () => sfx.down(sound),
      push: () => {
        sfx.push(sound);
        swap.btn.disabled = true;
        const faceTo = facing === "user" ? "environment" : "user";
        vid = video("camera:update", { facing: faceTo });
      },
    });
  }

  if (
    e.is("touch") ||
    e.is("keyboard:down:enter") ||
    e.is("keyboard:down:escape") ||
    e.is("keyboard:down:`")
  ) {
    if (!swap.btn.down) {
      capturing = false;
    }

    if (e.is("touch") && !swap?.btn.down && !hud.currentLabel.btn.down) {
      sfx.down(sound);
    }
  }

  if (e.is("camera:mode:user")) {
    facing = "user";
    swap.btn.disabled = false;
    capturing = true;
  }

  if (e.is("camera:mode:environment")) {
    facing = "environment";
    swap.btn.disabled = false;
    capturing = true;
  }

  if (e.is("camera:denied")) {
    notice("DENIED", ["yellow", "red"]);
    jump("prompt");
  }
}

export { boot, paint, bake, act };

export const system = "nopaint:bake-on-leave";

// 📚 Library (Useful functions used throughout the program)
// ...
