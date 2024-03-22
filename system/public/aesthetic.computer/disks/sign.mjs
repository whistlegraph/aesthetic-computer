// Sign, 2024.1.26.19.17.10.458
// Send an IRL message to someone words at a time, upside down.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
// function boot({ api }) {
// Runs once at the start.
// }

let msg = "";
let flash = false;

// 🎨 Paint
function paint({ wipe, screen, help }) {
  if (flash) {
    wipe("red");
    flash = false;
  } else {
    wipe("black")
      .ink(help.choose("white", "yellow"))
      .write(
        msg,
        { center: "x", y: screen.height * 0.5, size: -3 },
        null,
        screen.width * 0.95,
        true,
      )
      .ink("red")
      .write(
        msg,
        { center: "x", y: screen.height - 40 },
        null,
        screen.width * 0.9,
        false,
      );
  }
}

// 🎪 Act
function act({ event: e, jump }) {
  // Respond to user input here.
  if (e.is("keyboard:down")) {
    let key = e.key.toLowerCase();
    if (key === "backspace" || key === "delete") {
      msg = msg.slice(0, -1);
    } else if (key === "enter") {
      msg = "";
      console.log("❌ Sign cleared!");
      flash = true;
    } else if (key == "escape" || key === "`") {
      jump("prompt");
    } else if (key.length === 1 && e.ctrl === false) {
      msg += e.key;
      console.log("🪧 Sign updated:", msg);
    }
  }
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
    title: "Sign",
    desc: "Send an IRL message to someone words at a time, upside down.",
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

export { paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
