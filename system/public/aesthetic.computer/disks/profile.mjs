// Profile, 2023.6.04.16.58.31
// The default profile page for all users.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
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

let painting;

// 📰 Meta
function meta({ piece }) {
  return {
    title: `${piece} • aesthetic.computer`,
    desc: `Welcome to ${piece}'s profile.`,
    // TODO: ^ Replace with user's last status.
  };
}

// 🥾 Boot
function boot({ params, user, handle, debug, hud, net, get }) {
  // Mask from `profile` if we are logged in.

  const visiting = params[0] || handle;

  if (visiting) {
    hud.label(visiting);
    net.rewrite(visiting);
  }

  console.log("🤺 Visiting the profile of...", visiting);
  if (user) console.log("😉 Logged in as...", handle || user?.name);

  if (!visiting) {
    noprofile = user?.name || "no profile (enter 'hi' at the prompt)";
    return;
  }
  // 🎆 Check to see if this user actually exists via a server-side call.
  fetch(`/api/profile/${visiting}`, {
    headers: { Accept: "application/json" },
  })
    .then(async (response) => {
      const data = (await response.json()).mood;
      if (response.ok) {
        if (debug) console.log("🙆 Profile found:", data);
        profile = { handle: params[0], mood: data?.mood };
        noprofile = null;

        // Fetch all of a user's paintings...
        let paintings;
        try {
          const res = await fetch(`/media/${profile.handle}/painting`);
          paintings = (await res.json())?.files;
        } catch (err) {
          console.warn("Could not fetch media.");
        }

        const lastPainting = paintings?.[paintings?.length - 1];
        console.log("Last painting:", lastPainting);
        const lastPaintingCode = lastPainting
          .split("/")
          .pop()
          .replace(".png", "");
        // net.preload(lastPainting).then((img) => (painting = img)); // This doesn't work because of CORS errors in the Cloudflare worker.
        get
          .painting(lastPaintingCode)
          .by(visiting)
          .then((out) => {
            painting = out;
          })
          .catch((err) => {
            // console.warn("Could not load painting.", err);
          });
      } else {
        if (debug) console.warn("🙍 Profile not found:", data);
        noprofile = "no profile found";
      }
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

// 🎨 Paint
function paint({ params, wipe, ink, pen, user, screen, paste }) {
  if (!pen?.drawing) wipe(98);
  ink(127).line();
  if (profile) ink().line().ink().line().ink().line();

  if (painting) {
    const x = screen.width / 2 - painting.width / 2;
    const y = screen.height / 2 - painting.height / 2;
    ink(64).box(x, y, painting.width, painting.height);
    paste(painting, x, y);
    ink().box(x, y, painting.width, painting.height, "outline");
  }

  const retrieving = noprofile === RETRIEVING;
  ink(profile ? undefined : 255).write(
    profile?.handle || noprofile || user?.name,
    { center: "x", y: screen.height / 2 + 5 - (retrieving ? 0 : 12) },
    retrieving ? 64 : "black",
  );

  if (!retrieving && !profile && user?.name) {
    ink("yellow").write(
      "enter 'handle urnamehere' at the prompt",
      { center: "x", y: screen.height / 2 + 5 + 24 },
      "blue",
    );
  }

  if (profile) {
    ink().write(profile?.mood || "no mood");
    ink(255).write(
      profile.mood || "no mood",
      { center: "x", y: screen.height / 2 + 5 + 12 },
      "black",
    );
  }
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
