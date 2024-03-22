// LMN-petal, 2023.7.17.18.30.41
// Touch me.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

let message;
let background; 
let panx;
let pany;

// 🥾 Boot
function boot({ help, num }) {
  message = help.choose("He loves me", "He loves me not");
  background = help.choose("blue", "indigo", "lightcyan", "black");
  panx = num.randIntRange(-100, 100);
  pany = num.randIntRange(-100, 100);
}

// 🎨 Paint
function paint({ ink, wipe, oval, write, screen, help, pan, unpan, num }) {
  wipe(background);
  ink("white")
  // function oval(x0, y0, radiusX, radiusY, filled = false, thickness = 1)
  pan(panx, pany);
  oval(screen.width / 2, screen.height / 2, 80, 17, true);
  ink("pink");
  write(message, { center: "xy" })
  unpan();
}

// 🎪 Act
function act({ event, jump }) {
  if (event.is("touch")) {
    jump("lmn-flower");
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
    title: "LMN-petal",
    desc: "Touch me.",
  };
}

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
