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
  notice,
  // direction = 1,
  paintingIndex = stepIndex,
  pastRecord; // In case we load a record off the network.

let printBtn, // Sticker button.
  printPixels; // A url to the loaded image for printing.

let noticeTimer;

// 🥾 Boot
function boot({ system, query, params, get, net, ui, screen, gizmo }) {
  if (params[0]?.length > 0) {
    interim = "Loading...";

    let handle;
    let imageCode, recordingCode;
    // User string (@user/timestamp)
    if (params[0].startsWith("@")) {
      const [user, timestamp] = params[0].split("/");
      handle = user;
      imageCode = recordingCode = timestamp;
      printPixels = handle + "/painting/" + imageCode;
    } else {
      // Assume a guest painting code.
      // Example: Lw2OYs0H:qVlzDcp6;
      //          ^ png    ^ recording (if it exists)
      [imageCode, recordingCode] = params[0].split(":");
      handle = "anon";
      printPixels = imageCode;
    }

    console.log("Print pixels:", printPixels);

    net.waitForPreload();
    get
      .painting(imageCode)
      .by(handle)
      .then((out) => {
        finalPainting = out;
        net.preloaded();
      });
    if (recordingCode) {
      get
        .painting(recordingCode, { record: true })
        .by(handle)
        .then((out) => {
          pastRecord = system.nopaint.record;
          system.nopaint.record = out;
        });
    }

    printBtn = new ui.TextButton(`Print`, { bottom: 6, right: 6, screen });
  }
  advance(system);

  // 2 - [] ⚠️
  // TODO: Make this a generic overlay that can be added
  //       on top of any piece?

  const noticeBell = () => {
    noticeTimer = new gizmo.Hourglass(180, {
      completed: () => {
        notice = "";
        noticeTimer = null;
      },
    });
  };

  if (query === "success") {
    notice = "PRINTED";
    noticeBell();
    printBtn = null; // Kill the print button upon success. (Clear signal)
  } else if (query === "cancel") {
    notice = "CANCELLED";
    noticeBell();
  }
}

// 🎨 Paint
function paint({ wipe, ink, system, screen, num, paste }) {
  wipe(0);
  // Executes every display frame.
  if (system.nopaint.record?.length > 0) {
    ink().write(label, { size: 2 });
    ink(0, 127).box(0, 0, screen.width, screen.height);

    if (painting) {
      const x = screen.width / 2 - painting.width / 2;
      const y = screen.height / 2 - painting.height / 2;
      paste(painting, x, y);
      ink().box(x, y, painting.width, painting.height, "outline");
    }

    ink(num.map(labelFade, 0, labelFadeSpeed, 0, 255)).write(
      label,
      { y: 32, center: "x" },
      "black",
    );

    if (system.nopaint.record[stepIndex - 1]?.timestamp) {
      ink(200).write(
        system.nopaint.record[stepIndex - 1]?.timestamp,
        { x: 3, y: screen.height - 13 },
        "black",
      );
    }

    // Progress bar.
    ink().box(
      0,
      screen.height - 1,
      screen.width * (stepIndex / system.nopaint.record.length),
      screen.height,
    );

    printBtn?.paint({ ink });
  } else if (finalPainting) {
    const x = screen.width / 2 - finalPainting.width / 2;
    const y = screen.height / 2 - finalPainting.height / 2;
    paste(finalPainting, x, y);
    ink().box(x, y, finalPainting.width, finalPainting.height, "outline");
    printBtn?.paint({ ink });
  } else {
    ink().write(interim, { center: "xy" });
  }

  if (notice) {
    const c = notice === "CANCELLED";
    ink(c ? "yellow" : "white").write(
      notice,
      { center: "xy", size: 2 },
      c ? "red" : "green",
    );
  }
}

// 🎪 Act
function act({ event: e, screen, print }) {
  printBtn?.act(e, {
    push: async () => {
      // - [] Make the print button appear "held".
      //   - [] Would this require a scheme adjustment?
      printBtn.disabled = true;
      printBtn.reposition({ right: 6, bottom: 6, screen }, "Printing...");
      print("https://aesthetic.computer/api/pixel/1650x1650/" + printPixels);
    },
  });

  if (e.is("reframed")) {
    printBtn?.reposition({ right: 6, bottom: 6, screen });
  }
}

// 🧮 Sim
function sim({ simCount, system }) {
  if (simCount % advanceSpeed === 0n) advance(system);
  if (labelFade > 0) labelFade--;
  noticeTimer?.step();
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
