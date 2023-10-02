// Profile, 2023.6.04.16.58.31
// The default profile page for all users.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🧡] `profile` should be table to <- -> on a user's paintings 
       (tap into lightbox for painting / playback)
  - ☁️ General thoughts:
    - [] Should @handle eventually be a code piece in the system for every user?
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

const RETRIEVING = "retrieving...";
let profile,
  noprofile = RETRIEVING;

let visiting, painting, paintings, paintingIndex;
const { max, min } = Math;

// 📰 Meta
function meta({ piece }) {
  return {
    title: `${piece} • aesthetic.computer`,
    desc: `Welcome to ${piece}'s profile.`,
    // TODO: ^ Replace with user's last status.
  };
}

// 🥾 Boot
async function boot({ params, user, handle, debug, hud, net, get }) {
  // Mask from `profile` if we are logged in.

  visiting = params[0] || handle();

  console.log("Visiting:", visiting);

  if (visiting) {
    hud.label(visiting);
    net.rewrite(visiting);
  }

  console.log("🤺 Visiting the profile of...", visiting);
  if (user) console.log("😉 Logged in as...", handle() || user?.name);

  if (!visiting) {
    noprofile = "sign up to create profile";
    return;
  }
  // 🎆 Check to see if this user's profile actually exists via a server-side call.
  fetch(`/api/profile/${visiting}`, {
    headers: { Accept: "application/json" },
  })
    .then(async (response) => {
      const data = (await response.json()).mood;
      if (response.ok) {
        if (debug) console.log("🙆 Profile found:", data);
        profile = { handle: visiting, mood: data?.mood };
        noprofile = null;
      } else {
        if (debug) console.warn("🙍 Profile not found:", data);
        noprofile = "no profile found";
      }
    })
    .catch((error) => {
      console.error("Error:", error);
      noprofile = "error loading profile";
    });

  if (visiting) {
    // Fetch all of a user's paintings...
    fetch(`/media/${visiting}/painting`)
      .then((res) => res.json())
      .then((data) => {
        paintings = data?.files;
        if (paintings) {
          paintingIndex = paintings.length - 1;
          loadPainting(get, paintingIndex, visiting);
        }
      })
      .catch((err) => {
        console.warn("Could not load painting or fetch media.", err);
      });
  }
}

// 🎨 Paint
function paint({ params, wipe, ink, pen, user, screen, paste }) {
  if (!pen?.drawing) wipe(98);
  ink(127).line();
  if (profile) ink().line().ink().line().ink().line();

  if (profile) ink().write(profile?.mood || "no mood");

  if (painting) {
    const x = screen.width / 2 - painting.width / 2;
    const y = screen.height / 2 - painting.height / 2;
    ink(64).box(x, y, painting.width, painting.height);
    paste(painting, x, y);
    ink().box(x, y, painting.width, painting.height, "outline");
  }

  const retrieving = noprofile === RETRIEVING;
  if (!profile) {
    ink(profile ? undefined : 255).write(
      profile?.handle || noprofile || user?.name,
      { center: "xy" },
      retrieving ? 64 : "black",
    );
  }

  // if (!retrieving && !profile && user?.name) {
  //   ink("yellow").write(
  //     "enter 'handle @urnamehere'",
  //     { center: "x", y: screen.height / 2 + 5 + 24 },
  //     "blue",
  //     screen.width - 8,
  //   );
  // }

  if (profile) {
    ink(255).write(
      profile.mood || "no mood",
      { center: "xy" },
      "black",
      screen.width - 8,
    );
  }
  // return false;
}

// 🎪 Act
function act({ event: e, get }) {
 // Respond to user input here.
 if (e.is("keyboard:down:leftarrow")) {
  console.log("left");
  paintingIndex = max(0, paintingIndex - 1);
  loadPainting(get, paintingIndex, visiting);
 }
 if (e.is("keyboard:down:rightarrow")) {
  paintingIndex = min(paintingIndex + 1, paintings.length - 1);
  loadPainting(get, paintingIndex, visiting);
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

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

// Load a painting from paintings via the index.
async function loadPainting(get, index, from) {
  const code = paintings[index].split("/").pop().replace(".png", "");
  const got = await get.painting(code).by(from);
  painting = got.img;
}