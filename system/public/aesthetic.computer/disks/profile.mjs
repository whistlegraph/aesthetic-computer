// Profile, 2023.6.04.16.58.31
// The default profile page for all users.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🧡] `profile` should be table to <- -> on a user's paintings 
       - [x] Tap into lightbox for painting / playback.
       - [] Cache the bitmaps.
       - [] Loading paintings should make a beep.
       - [] Add left and right tap buttons.
       + Done
       - [x] Move mood.
       - [x] Wire up arrow keys.
  - [] Modify `api/profile` request to show a full text response
       if json is not returned.
  + Done
  + Later
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
let debug;
let profile,
  noprofile = RETRIEVING;

let paintingBtn;
let visiting, code, painting, paintings, paintingIndex;
const { max, min } = Math;
import * as sfx from "./common/sfx.mjs";

// 📰 Meta
function meta({ piece }) {
  return {
    title: `${piece} • aesthetic.computer`,
    desc: `Welcome to ${piece}'s profile.`,
    // TODO: ^ Replace with user's last status.
  };
}

// 🥾 Boot
async function boot({ params, user, handle, debug, hud, net, get, debug: d }) {
  // Mask from `profile` if we are logged in.
  debug = d;

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
function paint({ api, wipe, ink, pen, user, screen, ui, text, paste }) {
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

  if (profile) {
    ink(255).write(
      profile.mood || "no mood",
      { center: "x", y: 6 },
      "black",
      screen.width - 8,
    );
  }

  if (profile && !painting && paintings) {
    ink(255).write("retrieving...", { center: "xy" }, "black");
  }

  if (paintings?.length > 0) {
    ink(0).line(0, screen.height - 1, screen.width, screen.height - 1);
    ink("yellow").line(
      0,
      screen.height - 1,
      (paintingIndex / (paintings.length - 1)) * screen.width,
      screen.height - 1,
    );

    const pos = { x: 3, y: screen.height - 13 };

    const box = text.box(code, pos).box;
    const blockWidth = 6;
    box.width -= blockWidth * 2;

    if (!paintingBtn) paintingBtn = new ui.Button(box);
    paintingBtn.paint((btn) => {
      ink(btn.down ? "orange" : 255).write(code, pos);
    });
  }

  // return false;
}

// 🎪 Act
function act({ event: e, get, sound, jump }) {
  paintingBtn?.act(e, () => {
    sfx.push(sound);
    jump(`painting ${visiting}/${code}`);
  });

  // Respond to user input here.
  if (e.is("keyboard:down:arrowleft")) {
    paintingIndex = max(0, paintingIndex - 1);
    loadPainting(get, paintingIndex, visiting);
  }
  if (e.is("keyboard:down:arrowright")) {
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
let controller = null;
async function loadPainting(get, index, from) {
  painting = undefined; // Clear the current picture.

  if (controller) controller.abort(); // Abort any ongoing requests.

  // Create a new controller for the current request.
  controller = new AbortController();
  const signal = controller.signal;

  try {
    code = paintings[index].split("/").pop().replace(".png", "");
    const got = await get.painting(code).by(from, { signal }); // Assuming `get.painting` is based on fetch and can accept a signal
    painting = got.img;
  } catch (err) {
    if (err.name === "AbortError") {
      if (debug) console.log("Request was aborted");
    } else {
      console.error("Painting load failure:", err);
    }
  }
}
