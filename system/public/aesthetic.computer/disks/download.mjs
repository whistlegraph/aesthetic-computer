// Download, 23.01.29.22.13
// This piece shows a "screenshottable" / copy + paste-able download screen for
// both local and remote media.

/* #region 📝 TODO 
  + Now
  - [😫] How can I check the expiration date of the file via S3?
    - [] And display it to a user?
  + Later
  - [] Should I check to see if the file exists also?
  + Done
  - [x] Switching on file type / downloading user files vs anon files.
  - [x] Add download button with code. 
  - [x] Use a local download if it's available, otherwise try
       and grab the remote file.
#endregion */

let btn;
let homebtn;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ hud }) {
  // hud.label(); // Hide the label.
}

// 🎨 Paint (Executes every display frame)
function paint({
  ink,
  wipe,
  num,
  params,
  colon,
  ui,
  screen: { width, height },
}) {
  wipe(0, 0, 128); // Background

  const slug = params[0].split("/").pop(); // Remove any username from slug.
  const sw = slug.length * 6 + 12;

  // Buttons
  btn = new ui.Button({
    x: width / 2 - sw / 2,
    y: height / 2 - 10,
    w: sw,
    h: 20,
  });

  ink(0, 200, 0)
    .box(btn.box, "fill")
    .ink(0, 80, 0)
    .write(slug, num.p2.add(btn.box, { x: 6, y: 4 }), [0, 40]);

  let retryText = "GO AGAIN";
  if (colon[0] === "painting") retryText = "RETURN";

  const w = retryText.length * 6 + 12;

  homebtn = new ui.Button({
    x: width - w - 6,
    y: height - 20 - 6,
    w,
    h: 20,
  });

  ink(200, 200, 0)
    .box(homebtn.box, "fill")
    .ink(40, 40, 0)
    .write(retryText, num.p2.add(homebtn.box, { x: 4 + 2, y: 5 }), [0, 20]);

  let text = "YOUR WHISTLEGRAPH IS READY :)";
  if (colon[0] === "painting") text = "YOUR PAINTING IS READY :)";

  // Title
  ink(255, 0, 100, 200).write(text, { center: "x", y: height / 2 - 90 });
  ink(0, 100, 255, 200).write(text, { center: "x", y: height / 2 - 92 });
  ink(100, 255, 0, 200).write(text, { center: "x", y: height / 2 - 91 });

  // Instructions
  ink(255, 200).write("TAP SLUG TO DOWNLOAD", {
    center: "x",
    y: height / 2 - 28,
  });

  const lineHeight = 12;
  const sy = height / 2 + 18;

  ink(255, 200).write("OR", { center: "x", y: sy });

  ink(255, 200).write("SCREENSHOT & TYPE @ PROMPT", {
    center: "x",
    y: sy + lineHeight,
  });

  ink(255, 200).write("/ COPY THIS URL", {
    center: "x",
    y: sy + lineHeight * 2,
  });

  return false;
}

// ✒ Act (Runs once per user interaction)
function act({ event: e, num, download, colon, rec, params, jump, back }) {
  // Respond to user input here.
  btn?.act(e, () => {
    const slug = params[0];

    if (rec.printed) {
      // Local download...
      // Now specific to the whistlegraph video implementation. 23.05.02.00.55
      let filename = `whistlegraph-${num.timestamp()}.mp4`;
      download(filename); // Download the video locally.
    } else if (slug.length > 0) {
      // Network download based on params.
      if (colon[0] === "painting") {
        // Do a network download here...
        download(slug + ".png");
        // TODO: Show an in-app progress bar here. 23.05.09.10.21
      } else {
        download(slug + ".mp4");
      }
    }
  });

  homebtn?.act(e, () => {
    colon[0] === "painting" ? back() : jump("whistlegraph");
  });
}

export { boot, paint, act };

export const nohud = true;
// 📚 Library (Useful functions used throughout the piece)
// ...
