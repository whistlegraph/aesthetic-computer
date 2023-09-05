// Painting, 2023.8.17.18.58.39
// View any aesthetic.computer painting.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Automatically go to the `painting` page after a successful upload /
       return the proper code.
       - [] Generally smooth out the `painting:done` and `yes` feedback.
  + Later
  - [] Sound
  - [] Forwards and backwards directionality.
  + Done
  - [x] Add `print` button.
  - [x] Load provisional / non-user paintings.
  - [x] If there is no recording then still load and show the `.png`.
  - [x] Playback all an existing painting's steps in a loop, with
  - [x] Save and load painting recordings with steps 2 remote storage somehow.
  - [x] Ordered text file, combined with bitmaps labeled with their commands and indices.
      the image centered.
#endregion */

const labelFadeSpeed = 80;
const advanceSpeed = 120n;

let painting,
  finalPainting,
  step,
  label,
  labelFade = labelFadeSpeed,
  stepIndex = 0,
  interim = "No recording found.",
  // direction = 1,
  paintingIndex = stepIndex,
  pastRecord; // In case we load a record off the network.

let printBtn, // Sticker button.
  slug; // A url to the loaded image for printing.

const btnBar = 32;
const butBottom = 6;
const butSide = 6;

let mintBtn; // A button to mint.

// 🥾 Boot
function boot({ system, params, get, net, ui, screen }) {
  if (params[0]?.length > 0) {
    interim = "Loading...";

    let handle;
    let imageCode, recordingCode;
    // User string (@user/timestamp)
    if (params[0].startsWith("@")) {
      const [user, timestamp] = params[0].split("/");
      handle = user;
      imageCode = recordingCode = timestamp;
      slug = handle + "/painting/" + imageCode + ".png";
    } else {
      // Assume a guest painting code.
      // Example: Lw2OYs0H:qVlzDcp6;
      //          ^ png    ^ recording (if it exists)
      [imageCode, recordingCode] = params[0].split(":");
      handle = "anon";
      slug = imageCode + ".png";
    }

    net.waitForPreload();
    get
      .painting(imageCode)
      .by(handle)
      .then((out) => {
        finalPainting = out;
        net.preloaded();
      })
      .catch((err) => {
        // console.warn("Could not load painting.", err);
      });
    if (recordingCode) {
      get
        .painting(recordingCode, { record: true })
        .by(handle)
        .then((out) => {
          pastRecord = system.nopaint.record;
          system.nopaint.record = out;
        })
        .catch((err) => {
          // console.warn("Could not load recording.", err);
        });
    }

    printBtn = new ui.TextButton(`Print`, {
      bottom: butBottom,
      right: butSide,
      screen,
    });
    mintBtn = new ui.TextButton(`Mint`, {
      bottom: butBottom,
      left: butSide,
      screen,
    });
  }
  advance(system);
  // if (query.notice === "success") printBtn = null; // Kill button after order.
}

// 🎨 Paint
function paint({ wipe, ink, system, screen, num, paste }) {
  wipe(0);
  ink(0, 127).box(0, 0, screen.width, screen.height);

  function paintUi() {
    ink(64, 127).box(
      0,
      screen.height - btnBar,
      screen.width,
      screen.height - btnBar,
    );
    printBtn?.paint({ ink });
    mintBtn?.paint({ ink });
  }

  // Executes every display frame.
  if (system.nopaint.record?.length > 0) {
    ink().write(label, { size: 2 });

    if (painting) {
      const x = screen.width / 2 - painting.width / 2;
      const y = screen.height / 2 - painting.height / 2;
      paste(painting, x, y - btnBar / 2);
      ink().box(x, y - btnBar / 2, painting.width, painting.height, "outline");
    }

    ink(num.map(labelFade, 0, labelFadeSpeed, 0, 255)).write(
      label,
      { y: 32, center: "x" },
      "black",
    );

    if (system.nopaint.record[stepIndex - 1]?.timestamp) {
      ink(200).write(
        system.nopaint.record[stepIndex - 1]?.timestamp,
        { x: 3, y: screen.height - 13 - btnBar },
        "black",
      );
    }

    // Progress bar.
    ink().box(
      0,
      screen.height - 1 - btnBar,
      screen.width * (stepIndex / system.nopaint.record.length),
      1,
    );

    paintUi();
  } else if (finalPainting) {
    const x = screen.width / 2 - finalPainting.width / 2;
    const y = screen.height / 2 - finalPainting.height / 2;
    paste(finalPainting, x, y - btnBar / 2);
    ink().box(
      x,
      y - btnBar / 2,
      finalPainting.width,
      finalPainting.height,
      "outline",
    );
    paintUi();
  } else {
    ink().write(interim, { center: "xy" });
  }
}

// 🎪 Act
function act({ event: e, screen, print, mint }) {
  printBtn?.act(e, {
    push: async () => {
      printBtn.disabled = true;
      printBtn.reposition(
        { right: butSide, bottom: butBottom, screen },
        "Printing...",
      );
      print(slug);
    },
  });

  mintBtn?.act(e, {
    push: async () => {
      printBtn.disabled = true;
      printBtn.reposition(
        { right: butSide, bottom: butBottom, screen },
        "Minting...",
      );
      mint(slug);
    },
  });

  if (e.is("reframed")) {
    printBtn?.reposition({ right: butSide, bottom: butBottom, screen });
    mintBtn?.reposition({ left: butSide, bottom: butBottom, screen });
  }
}

// 🧮 Sim
function sim({ simCount, system }) {
  if (simCount % advanceSpeed === 0n) advance(system);
  if (labelFade > 0) labelFade--;
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
function leave({ system }) {
  if (pastRecord) system.nopaint.record = pastRecord;
}

// 📰 Meta
function meta({ params }) {
  if (params[0]) {
    const [handle, timestamp] = params[0].split("/");
    return {
      title: `painting · ${handle}/${timestamp}`,
      desc: `A painting by ${handle} from ${timestamp}.`,
    };
  } else {
    return {
      title: "painting",
      desc: "View any aesthetic.computer painting.",
    };
  }
}

// 🖼️ Preview
function preview({ ink, screen, paste, wipe, resolution }) {
  // Render a custom thumbnail image.
  if (finalPainting) {
    resolution(finalPainting.width, finalPainting.height, 0);
    const x = screen.width / 2 - finalPainting.width / 2;
    const y = screen.height / 2 - finalPainting.height / 2;
    paste(finalPainting, x, y);
    ink().box(x, y, finalPainting.width, finalPainting.height, "outline");
  } else {
    wipe();
  }
}

// 🪷 Icon
function icon($) {
  preview($);
}

export { boot, paint, sim, act, leave, meta, preview, icon };

// 📚 Library
//   (Useful functions used throughout the piece)

function advance(system) {
  if (system.nopaint.record.length === 0) return;
  if (stepIndex === system.nopaint.record.length) stepIndex = 0;

  step = system.nopaint.record[stepIndex];
  label = step.label.replaceAll("~", " ");
  labelFade = labelFadeSpeed;

  if (step.painting) {
    painting = step.painting;
    paintingIndex = stepIndex;
  } else {
    if (label === "no") {
      paintingIndex = paintingIndex - 1;
    } else if (label === "yes") {
      paintingIndex = paintingIndex + 1;
    }
    painting = system.nopaint.record[paintingIndex].painting;
  }

  // if (direction > 0) {
  stepIndex = stepIndex + 1;
  //if (stepIndex === system.nopaint.record.length - 1) {
  // stepIndex = system.nopaint.record.length - 1;
  // direction = -1;
  //}
  //} else {
  // stepIndex -= 1;
  // if (stepIndex === 0) direction = 1;
  //}
}
