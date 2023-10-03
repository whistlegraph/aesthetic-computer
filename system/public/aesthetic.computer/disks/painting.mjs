// Painting, 2023.8.17.18.58.39
// View any aesthetic.computer painting.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Later
  - [] Sound
  - [] Forwards and backwards directionality.
  - [] Make right-click / tap to save available just in the "painting" command.
  + Done
  - [x] `done` should take you to the painting page after uploading.
  - [x] Automatically go to the `painting` page after a successful upload /
       return the proper code.
       - [x] Generally smooth out the `painting:done` and `yes` feedback.
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
let advanceCount = 0n;

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

let noadvance = false;

const btnBar = 32;
const butBottom = 6;
const butSide = 6;

let handle;
let imageCode, recordingCode;
let timeout;
let running;

//let mintBtn; // A button to mint.

// 🥾 Boot
function boot({
  system,
  params,
  get,
  net,
  ui,
  screen,
  display,
  dom: { html },
}) {
  if (params[0]?.length > 0) {
    interim = "Loading...";
    genSlug({ params });
    net.waitForPreload();
    get
      .painting(imageCode)
      .by(handle)
      .then((out) => {
        finalPainting = out.img;
        net.preloaded();
        let slug = imageCode + ".png";
        if (handle && handle !== "anon")
          slug = handle + "/painting/" + imageCode + ".png";

        const cssWidth = out.img.width * display.subdivisions;
        const cssHeight = out.img.width * display.subdivisions;

        html`
          <img
            width="${out.img.width}"
            height="${out.img.height}"
            id="hidden-painting"
            crossorigin
            src=${"/api/pixel/2048:conform/" + encodeURI(slug)}
          />
          <style>
            #content {
              z-index: 0 !important;
            }
            #hidden-painting {
              position: absolute;
              top: calc(calc(50% - calc(${cssHeight}px / 2)) - 32px);
              left: calc(50% - calc(${cssWidth}px / 2));
              width: ${cssWidth}px;
              height: ${cssHeight}px;
              background: yellow;
              opacity: 0.25;
              object-fit: contain;
              image-rendering: pixelated;
              -webkit-user-select: all;
              user-select: all;
            }
          </style>
          <script>
            const hp = document.querySelector("#hidden-painting");
            hp.onmousedown = (e) => {};
          </script>
        `;
      })
      .catch((err) => {
        // console.warn("Could not load painting.", err);
      });
    if (recordingCode) {
      get
        .painting(recordingCode, { record: true })
        .by(handle)
        .then((out) => {
          timeout = setTimeout(() => {
            pastRecord = system.nopaint.record;
            system.nopaint.record = out;
            running = true;
            advance(system);
          }, 1500);
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
    // mintBtn = new ui.TextButton(`Mint`, {
    //   bottom: butBottom,
    //   left: butSide,
    //   screen,
    // });
    // mintBtn.disabled = true;
  } else {
    finalPainting = system.painting;
    timeout = setTimeout(() => {
      running = true;
      advance(system);
    }, 1500);
  }
  // if (query.notice === "success") printBtn = null; // Kill button after order.
}

// 🎨 Paint
function paint({ wipe, ink, system, screen, num, paste }) {
  wipe(0);
  ink(0, 127).box(0, 0, screen.width, screen.height);

  function paintUi() {
    ink(32, 127).box(
      0,
      screen.height - btnBar,
      screen.width,
      screen.height - btnBar,
    );
    printBtn?.paint({ ink });
    //mintBtn?.paint({ ink });
  }

  ink(96).box(0, screen.height - 1 - btnBar, screen.width, 1);

  // Executes every display frame.
  if (
    (pastRecord && system.nopaint.record?.length > 0) ||
    (system.nopaint.record?.length > 1 && running)
  ) {
    ink().write(label, { size: 2 });

    if (painting) {
      const x = screen.width / 2 - painting.width / 2;
      const y = screen.height / 2 - painting.height / 2;
      ink(64).box(x, y - btnBar / 2, painting.width, painting.height);
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
    ink(64).box(x, y - btnBar / 2, finalPainting.width, finalPainting.height);
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
function act({ event: e, screen, print, mint, delay }) {
  printBtn?.act(e, {
    push: async () => {
      printBtn.disabled = true;
      printBtn.reposition(
        { right: butSide, bottom: butBottom, screen },
        "Printing...",
      );
      await print(slug);
      delay(() => {
        printBtn.disabled = false;
      }, 0.1);
      printBtn.reposition(
        { right: butSide, bottom: butBottom, screen },
        "Print",
      );
    },
  });

  // mintBtn?.act(e, {
  //   push: async () => {
  //     mintBtn.disabled = true;
  //     mintBtn.reposition(
  //       { right: butSide, bottom: butBottom, screen },
  //       "Minting...",
  //     );
  //     await mint(slug);
  //     mintBtn.disabled = false;
  //     mintBtn.reposition({ right: butSide, bottom: butBottom, screen }, "Mint");
  //   },
  // });

  if (e.is("reframed")) {
    printBtn?.reposition({ right: butSide, bottom: butBottom, screen });
    // mintBtn?.reposition({ left: butSide, bottom: butBottom, screen });
  }
}

// 🧮 Sim
function sim({ simCount, system }) {
  if (running) {
    advanceCount += 1n;
    if (advanceCount % advanceSpeed === 0n) advance(system);
  }
  if (labelFade > 0) labelFade--;
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
function leave({ system }) {
  noadvance = true;
  if (pastRecord) system.nopaint.record = pastRecord;
  clearTimeout(timeout);
}

// 📰 Meta
function meta({ params }) {
  if (params[0]) {
    genSlug({ params });
    return {
      title: `painting · ${slug.replace(".png", "")}`,
      desc: handle
        ? `A pixel painting by ${handle}.`
        : `An anonymous pixel painting.`,
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
  if (noadvance) return;
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

function genSlug({ params }) {
  // User string (@user/timestamp)
  if (params[0].startsWith("@") || params[0].indexOf("@") !== -1) {
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
}
