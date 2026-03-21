// iMessage, 2023.12.05.14.08.23.293
// A piece that loads for Apple's iMessage app extension.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Two buttons: [Moods] [Paintings]
  - [] Tapping Moods will show a flashable list of moods.
  - [] Tapping Paintings will show a flashable list of paintings.
  - [] Tapping either a mood or painting will compose the pixels and then
       send them up.
    - [] Paintings and moods should both include a by line with the user's
         handle if one exists, or say `anon` if one doesn't.
#endregion */

let needsWipe = false;

// 🥾 Boot
function boot({ wipe, screen, resolution, jump }) {
  // Runs once at the start.
  wipe("blue"); // Clear's the screen. Can use R, G, B or CSS colors.
}

// 🎨 Paint
function paint({ api, wipe, ink, help: { choose } }) {
  wipe("black");
  ink().write(choose("hello", "goodbye", "i love u"), {
    y: 32,
    center: "x",
    size: 2,
  });
}

// 🎪 Act
function act({ event: e, send, painting, help: { choose } }) {
  if (e.is("touch")) {
    const pixels = painting(128, 128, ({ wipe, ink }) => {
      wipe(0);
      ink().write(choose("hello", "goodbye", "i love u"), {
        center: "xy",
        size: 2,
      });
    });

    send({
      type: "imessage-extension:send",
      content: { type: "tap", body: { pixels } },
    });
  }

  if (e.is("reframed")) needsWipe = true;
}

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
    title: "iMessage",
    desc: "A piece that loads for Apple's iMessage app extension.",
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

export { boot, act, paint, meta };
export const nohud = true;

// 📚 Library
//   (Useful functions used throughout the piece)
