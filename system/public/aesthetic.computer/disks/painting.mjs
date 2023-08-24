// Painting, 2023.8.17.18.58.39
// View any aesthetic.computer painting.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟠] Playback all an existing painting's steps in a loop, with
       the image centered.
  - [] Save and load painting recordings with steps 2 remote storage somehow.
    - [] Ordered text file, combined with bitmaps labeled with their commands
         and indices.
  + Later
  - [] Get directionality working again.
#endregion */

const labelFadeSpeed = 80;
const advanceSpeed = 120n;

let painting,
  step,
  label,
  labelFade = labelFadeSpeed,
  stepIndex = 0,
  interim = "No recording found.",
  // direction = 1,
  paintingIndex = stepIndex,
  pastRecord; // In case we load a record off the network.

// 🥾 Boot
function boot({ system, params, get }) {
  if (params[0]) {
    const [handle, timestamp] = params[0].split("/");
    interim = "Loading...";
    get
      .painting(timestamp, { record: true })
      .by(handle)
      .then((out) => {
        pastRecord = system.nopaint.record;
        system.nopaint.record = out;
      });
  }
  advance(system);
}

// 🎨 Paint
function paint({ wipe, ink, box, system, screen, num, paste }) {
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
      { y: 12, center: "x" },
      "black"
    );

    if (system.nopaint.record[stepIndex - 1]?.timestamp) {
      ink(200).write(
        system.nopaint.record[stepIndex - 1]?.timestamp,
        { x: 3, y: screen.height - 13 },
        "black"
      );
    }

    // Progress bar.
    ink().box(
      0,
      screen.height - 1,
      screen.width * (stepIndex / system.nopaint.record.length),
      screen.height
    );
  } else {
    ink().write(interim, { center: "xy" });
  }
}

// 🎪 Act
// function act({ event }) {
//  // Respond to user input here.
// }

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
function meta() {
  return {
    title: "Painting",
    desc: "View any aesthetic.computer painting.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, sim, leave, meta };

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

  console.log(step);

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
