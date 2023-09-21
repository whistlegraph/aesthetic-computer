// Booted-by, 2023.9.13.19.43.52.028
// Aesthetic was booted by...

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🧡] overlay with text and buttons
    - [] text jeffrey at xxx-xxx-xxxx to claim spot. urgent, legitimate, exciting 
    - [] 10 tappable bars with decoration.
  - [] `booted-by` - your text and your url
  - [] names / bars go underneath with booted by in top left
  - [] ok button and demo video link within the text that takes you out of app to demo video 
  - [] names and "unclaimed" or "empty" or "open" for open slots
  - [] tapping bar brings back overlay
  + Later?
  - [] Make this a nice payment page
  - [] date of boot after boot completed
  - [] names link to people's profiles, invitation to make profile somewhere
#endregion */

// 🥾 Boot
function boot({ wipe, ink, line }) {
  // Runs once at the start.
  wipe(0);
}

// 🎨 Paint
function paint({ ink }) {
  // Executes every display frame.
  return false; // Uncomment for an animation loop.
}

// 🎪 Act
// function act({ event }) {
//  // Respond to user input here.
// }

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Booted-by",
    desc: "Aesthetic was booted by...",
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

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
