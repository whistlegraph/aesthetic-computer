// Download, 23.01.29.22.13
// This piece shows a "screenshottable" / copy + paste-able download screen for
// both local and remote media.

/* #region 📝 TODO 
  + Now
  - [] Add download button with code. 
  - [] Use a local download if it's available, otherwise try
       and grab the remote file.
  - [] How can I check the expiration date of the file?
  - [] Should I check to see if the file exists also?
  + Done
  + Later
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
function boot({ wipe, params }) {
  wipe(0, 0, 128).ink(255).write(`Hello :) ${params[0]}`, { center: "xy" });
}

// 🎨 Paint (Executes every display frame)
function paint() {}

// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

export { boot, paint, act };

// 📚 Library (Useful functions used throughout the piece)
// ...
