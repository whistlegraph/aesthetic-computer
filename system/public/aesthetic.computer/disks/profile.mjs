// Profile, 2023.6.04.16.58.31
// The default profile page for all users.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - ☁️ General thoughts:
    - [] Should @handle eventually be a piece in the system for every user?
    - [] And then they can edit it?
    - [] Or maybe just a custom paint function?
    - [] What happens when you visit there now?
  - 💡 Ideas for content:
    - [] Most recent user painting.
    - [] Activity log
    - [] Consider public facing vs user facing differences.
      - [] Like the ability to set a handle.
    - [] Settings
      - [] Theme
    - [] Globally warn user if they are inauthenticated somehow.
      - [] Or if they are offline... using tiny LEDs?
#endregion */

let profile,
  noprofile = "searching...";

// 📰 Meta
function meta({ piece }) {
  return {
    title: `${piece} • aesthetic.computer`,
    desc: `Welcome to ${piece}'s profile.`,
    // TODO: ^ Replace with user's last status.
  };
}

// 🥾 Boot
function boot({ params, user, handle, slug }) {
  console.log("🤺 Visiting the profile of...", params[0]);
  if (user) console.log("😉 Logged in as...", handle || user?.name);

  // 🎆 Check to see if this user actually exists via a server-side call.
  fetch(`/api/profile/${params[0]}`, {
    headers: { Accept: "application/json" },
  })
    .then(async (response) => {
      const json = await response.json();
      if (response.ok) {
        console.log("🙆 Profile found:", json);
        profile = { handle: params[0] };
      } else {
        console.warn("🙍 Profile not found:", json);
        noprofile = "no profile found";
      }
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

// 🎨 Paint
function paint({ params, wipe, ink, pen }) {
  if (!pen?.drawing) wipe(98);
  ink(127).line();

  if (profile) ink().line().ink().line().ink().line();
  ink(profile ? undefined : 255).write(
    profile?.handle || noprofile,
    { center: "xy" },
    "black"
  );

  // Drawing Boxes (Ida Lesson)
  // let bx = 30;
  // let by = 20;
  // let bc = "blue";
  // const bh = 100;
  // const bw = 130;
  // let boxin = "i am sitting in a box";
  // if (pen) {
  //   bx = pen.x - bw / 2;
  //   by = pen.y - bh / 2;
  // }
  // if (!pen?.drawing) bc = "black";
  // ink(bc);
  // if (pen?.button === 2 && pen?.drawing) ink("red");

  // box(bx, by, bw, bh); // x, y, width, height
  // ink("white");

  // if (!pen?.drawing) {
  // write(boxin, { x: bx, y: by });
  // } else {
  // write("drawing", { x: bx, y: by });
  // }

  // return false;
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

export { boot, paint, meta };

// 📚 Library
//   (Useful functions used throughout the piece)