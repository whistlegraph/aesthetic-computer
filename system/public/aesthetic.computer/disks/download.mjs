// Download, 23.01.29.22.13
// This piece shows a "screenshottable" / copy + paste-able download screen for
// both local and remote media.

/* #region 📝 TODO 
  + Now
  - [😫] How can I check the expiration date of the file?
  - [] Should I check to see if the file exists also?
  + Done
  - [-] Add download button with code. 
  - [-] Use a local download if it's available, otherwise try
       and grab the remote file.
  + Later
#endregion */

let btn;
let homebtn;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ ink, wipe, num, params, ui, screen: { width, height } }) {
  wipe(0, 0, 128); // Background

  // Buttons
  btn = new ui.Button({
    x: width / 2 - 40,
    y: height / 2 - 10,
    w: 80,
    h: 20,
  });

  ink(0, 200, 0)
    .box(btn.box, "fill")
    .ink(0, 80, 0)
    .write(params[0], num.p2.add(btn.box, {x: 4 + 12, y: 4}), [0, 40]);

  homebtn = new ui.Button({
    x: (width / 2 - 30) + 55,
    y: height / 2 + 90,
    w: 60,
    h: 20,
  });

  ink(200, 200, 0)
    .box(homebtn.box, "fill")
    .ink(40, 40, 0)
    .write("GO AGAIN", num.p2.add(homebtn.box, {x: 4 + 2, y: 5}), [0, 20]);

  // Title
  ink(255, 0, 100, 200).write("YOUR WHISTLEGRAPH IS READY :)", {center: "x", y: height / 2 - 90});
  ink(0, 100, 255, 200).write("YOUR WHISTLEGRAPH IS READY :", {center: "x", y: height / 2 - 92});
  ink(100, 255, 0, 200).write("YOUR WHISTLEGRAPH IS READY", {center: "x", y: height / 2 - 91});

  // Instructions
  ink(255, 200).write("TAP CODE TO DOWNLOAD", {center: "x", y: height / 2 - 28});
  ink(255, 200).write("SCREENSHOT & ENTER AT PROMPT", {center: "x", y: height / 2 + 20});
  ink(255, 200).write("OR COPY THIS URL", {center: "x", y: height / 2 + 35});
}

// 🎨 Paint (Executes every display frame)
function paint() {}

// ✒ Act (Runs once per user interaction)
function act({ event: e, num, download, rec, params, jump }) {
  // Respond to user input here.
  btn?.act(e, () => {
    if (rec.printed) {
      // Local download...
      let filename = `whistlegraph-${num.timestamp()}.mp4`;
      download(filename); // Download the video locally.
    } else {
      // Network download based on params.
      // TODO: Hardcoded to video for now. 23.01.29.23.53
      if (params[0].length > 0) download(params[0] + ".mp4");
    }
  });

  homebtn?.act(e, () => jump("whistlegraph"));
}

export { boot, paint, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
