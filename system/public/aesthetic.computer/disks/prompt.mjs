// A text based "access-everything" console.

// Currently the aesthetic.computer home piece!

/* #region 🏁 todo
 - [] Prevent non-printable characters from causing an empty space.
 - [] Generate or pretty print docs (made from the APIs) inside this disk.
      (This would allow people to have a reference while writing disks.)
 + Done
 - [x] Generically "lock" prompt after input before a result returns.
      Show a spinner if too much time has passed?
 + Later
 - [] An iOS app would need a small ESC or arrow overlay button in Swift
      to make this work properly.
#endregion */

import { parse } from "../lib/parse.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { Desktop, MetaBrowser } from "../lib/platform.mjs";
import { TextInput } from "../lib/type.mjs";
import { ordfish } from "./ordfish.mjs";

const { abs } = Math;

const scheme = {
  dark: {
    fg: [255, 100],
    bg: [70, 50, 100],
    block: [200, 30, 100],
    line: [0, 0, 255, 64],
  },
  light: {
    fg: [0, 200],
    bg: [170, 150, 200],
    block: [30, 200, 200],
    line: [0, 0, 0, 128],
  },
};

let input, motd;

// Error / feedback flash on command entry.
let flash;
let flashShow = false;
let flashMessage;
let flashColor = [];
let flashPresent = false;
let uploadProgress = 0; // If not zero, then draw a progress bar.

// 🥾 Boot (Runs once before first paint and sim)
function boot($) {
  const {
    api,
    handle,
    authorize,
    pieceCount,
    glaze,
    load,
    download,
    dark,
    darkMode,
    num,
    store,
    connect,
    bgm,
    needsPaint,
    system,
    screen,
    debug,
    painting,
    net,
    jump,
    user,
    file,
    upload,
  } = $;

  glaze({ on: true }); // TODO: Every glaze triggers `frame` in `disk`, this could be optimized. 2022.04.24.04.25

  makeMotd($); // Generate welcome message.

  input = new TextInput(
    $,
    motd,
    scheme[dark ? "dark" : "light"],
    // 🍎 Process commands...
    async (text) => {
      // Roughly parse out the text (could also do a full `parse` here.)
      const tokens = text.split(" ");
      const slug = tokens[0]; // Note: Includes colon params.
      const params = tokens.slice(1);

      if (text.startsWith("handle")) {
        // Set username handle.
        // TODO: This could eventually be abstracted for more API calls.
        // Something like... await post({handle: "new"});

        // Make sure there is a parameter.
        const handle = text.split(" ")[1];

        // And a handle has been specified.
        if (handle?.length > 0 && validateHandle(handle)) {
          const token = await authorize(); // Get user token.
          if (token) {
            const headers = {
              Authorization: `Bearer ${token}`,
              "Content-Type": "application/json",
            };
            // 😀 Try to set the handle.
            // TODO: Lock the input prompt / put a spinner here...
            try {
              const response = await fetch("/handle", {
                method: "POST",
                headers: headers,
                body: JSON.stringify({ handle }),
              });
              const data = await response.json();
              if (response.status !== 200) {
                flashColor = [255, 0, 0];
                console.error("🧖 Error:", response, data);
              } else {
                flashColor = [0, 128, 0];
                store["handle:updated"] = data.handle;
                console.log("🧖 Handle changed:", data.handle);
                makeFlash($, true, "hi @" + data.handle);
              }
            } catch (error) {
              flashColor = [255, 0, 0]; // Server error.
              makeFlash($);
              console.error("🧖 Error:", error);
            }
          } else {
            flashColor = [255, 0, 0]; // Authorization error.
            makeFlash($);
            console.error("🧖 Not logged in.");
          }
          needsPaint();
        } else {
          flashColor = [255, 0, 0];
          console.warn("🧖 No @handle specified / bad handle design.");
          makeFlash($, true, "HANDLE INVALID");
        }
      } else if ((text === "ul" || text === "upload") && store["painting"]) {
        if (!navigator.onLine) {
          flashColor = [255, 0, 0];
          makeFlash($, true, "OFFLINE");
        } else {
          const filename = `painting-${num.timestamp()}.png`;
          // The first dashed string will get replaced with a slash / media directory filter on the server.
          uploadProgress = -1; // Trigger progress bar rendering.
          upload(filename, store["painting"], (p) => (uploadProgress = p))
            .then((data) => {
              console.log("🪄 Painting uploaded:", filename, data);
              flashColor = [0, 255, 0];
              makeFlash($);
              const slug = user
                ? `${handle || user.email}/painting/${data.slug}`
                : data.slug;
              jump(`download:painting ${slug}`);
            })
            .catch((err) => {
              console.error("🪄 Painting upload failed:", err);
              flashColor = [255, 0, 0];
              makeFlash($);
            });
        }
      } else if (slug === "resize" || slug === "res") {
        // Resize the active painting if one exists, or make one at this
        // size if it doesn't.
        const w = params[0],
          h = params[0] || w;
        if (isNaN(w)) {
          flashColor = [255, 0, 0];
        } else {
          nopaint_adjust(screen, system, painting, store, { w, h });
          flashColor = [0, 255, 0];
        }
        makeFlash($);
      } else if (text.startsWith("dl") || text.startsWith("download")) {
        if (store["painting"]) {
          download(`painting-${num.timestamp()}.png`, store["painting"], {
            scale: abs(parseInt(text.split(" ")[1])) || 6,
            // Read an integer parameter for scale.
            cropToScreen: !(store["painting:resolution-lock"] === true),
            // Only cut the download off at screen-size if user never
            // set a resolution.
          });
          // Show a green flash if we succesfully download the file.
          flashColor = [0, 255, 0];
        } else {
          flashColor = [255, 0, 0]; // Show a red flash otherwise.
        }
        makeFlash($);
      } else if (slug === "login" || slug === "hi") {
        net.login();
        flashColor = [255, 255, 0, 100]; // Yellow
        makeFlash($);
      } else if (text === "logout" || text === "bye") {
        net.logout();
        flashColor = [255, 255, 0, 100]; // Yellow
        makeFlash($);
      } else if (text === "no") {
        system.nopaint.no({ system, store, needsPaint });
        if (system.nopaint.undo.paintings.length > 1) {
          flashColor = [0, 0, 255, 100]; // Blue for succesful undo.
        } else {
          flashColor = [255, 0, 0, 100]; // Red for failed undo.
        }
        makeFlash($);
      } else if (text === "nopan") {
        system.nopaint.resetTransform(api);
        system.nopaint.storeTransform(store, system); // Store the translation after completion.
        input.text = "";
      } else if (text === "painting:reset" || text === "no!") {
        const deleted = system.nopaint.noBang({
          system,
          store,
          screen,
          needsPaint,
        });

        if (deleted) {
          flashColor = [0, 0, 255]; // Blue for succesful deletion.
        } else {
          flashColor = [255, 0, 0]; // Red if delete failed.
        }

        makeFlash($);
        needsPaint();
      } else if (text === "3dline:reset") {
        const deleted = await store.delete("3dline:drawing", "local:db");

        if (deleted) {
          flashColor = [0, 0, 255]; // Blue for succesful deletion.
        } else {
          flashColor = [255, 0, 0]; // Red if delete failed.
        }

        makeFlash($);
        needsPaint();
      } else if (text === "dark" || text === "dark:reset") {
        if (text === "dark:reset") {
          store.delete("dark-mode");
          darkMode("default");
          flashColor = [127, 127, 127]; // Gray for system setting.
        } else {
          let current = await store.retrieve("dark-mode");
          current = current === true ? false : true;
          darkMode(current);
          if (current) {
            flashColor = [0, 0, 0]; // Black for dark mode enabled.
          } else {
            flashColor = [255, 255, 255]; // White for dark mode disabled.
          }
        }
        makeFlash($);
      } else if (text.startsWith("2022")) {
        load(parse("wand~" + text)); // Execute the current command.
      } else if (text === "connect") {
        let identity;
        input.text = "";
        try {
          identity = await connect(); // Web3 connect.
          store["identity"] = identity; // Store the identity.
          store.persist("identity"); // ... and persist it!
          makeFlash($, false);
          flashColor = [0, 255, 0];
        } catch (e) {
          makeFlash($, false);
          flashColor = [255, 0, 0];
        }
      } else if (text === "bgm stop") {
        bgm.stop();
        makeFlash($, false);
        flashColor = [255, 0, 0];
      } else if (text === "help") {
        // Go to the Discord for now if anyone types help.
        jump("https://discord.gg/aesthetic-computer");
      } else if (text === "shillball" || text === "sb") {
        // Shortcuts for Yeche's Shillball game.
        jump("https://galerie-yechelange.baby/ball");
      } else if (text === "prod") {
        jump("https://prompt.ac"); // Visit the live site.
      } else if (text === "local" || text.startsWith("local")) {
        const param = text.replace("local", "").trim().replaceAll(" ", "~");
        const slug = param.length > 0 ? `/${param}` : "";
        console.log(slug);
        jump("https://localhost:8888" + slug); // Go to the local dev server, passing any params as a piece.
      } else if (text.split(" ")[0] === "of") {
        // Ordfish shortcuts.
        jump(`ordfish~${text.split(" ").slice(1).join("~")}`);
      } else if (ordfish[text] || text.split(" ") === "of") {
        jump(`ordfish~${text}`);
      } else {
        // 🟠 Local and remote pieces...
        load(parse(text)); // Execute the current command.
      }
    }
  );

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (pieceCount > 0) {
    if (Desktop) input.canType = true;
    input.text = "";
  }
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($) {
  input?.sim($);

  if ($.store["handle:received"] && input.canType === false) {
    makeMotd($);
    input.text = motd;
    input.canType = false;
    delete $.store["handle:received"];
    $.needsPaint();
  }

  if (flashPresent) flash.step();
}

// 🎨 Paint (Runs once per display refresh rate)
function paint($) {
  let { screen, wipe, ink, history, api, system, store, dark } = $;

  const pal = scheme[dark ? "dark" : "light"];
  if (input) input.pal = pal; // Update text input palette.

  if (store["painting"]) {
    system.nopaint.present(api); // Render the painting.
    ink(...pal.bg, 127).box(screen); // Backdrop
  } else {
    wipe(...pal.bg);
  }

  const glyphsLoaded = input?.paint($); // Paint the text input.

  // Paint last command if needed.
  // TODO: This could be a much shorter call...

  let historyTexts;

  if (history.length === 0) {
    historyTexts = ["aesthetic.computer"];
  } else {
    historyTexts = history.map((h) => h.replaceAll("~", " "));
  }

  historyTexts.reverse().forEach((t, i) => {
    const ii = i + 1;
    const yMargin = i === 0 ? 0 : 2;
    ink(140, 90, 235, 80 / ii).printLine(
      t,
      input?.typeface.glyphs,
      6,
      screen.height - 6 * 3 * ii - 6 - yMargin,
      6,
      2,
      0
    );
  });

  if (uploadProgress > 0 || uploadProgress === -1) {
    ink(0).box(1, 1, screen.width - 2, 2);
    if (uploadProgress > 0) {
      ink(scheme.dark.block).box(1, 1, (screen.width - 2) * uploadProgress, 2);
    }
  }

  // Trigger a red or green screen flash with a timer.
  if (flashShow) {
    ink(flashColor).box(0, 0, screen.width, screen.height);
    if (flashMessage) ink(255).write(flashMessage, { x: 5, y: 4, size: 2 });
  }

  // return glyphsLoaded;
  return false;
}

// ✒ Act (Runs once per user interaction, after boot.)
async function act($) {
  const { event: e, needsPaint, store, screen, system, painting } = $;

  if (e.is("reframed")) {
    nopaint_adjust(screen, system, painting, store);
    needsPaint();
  }

  input?.act($);

  if (e.is("load-error")) {
    makeFlash($);
    flashColor = [255, 0, 0];

    if (MetaBrowser) input.canType = false;
    needsPaint();
  }
}

function meta() {
  return {
    title: "prompt · aesthetic.computer",
    desc: "Type a command to get started.",
  };
}

export { boot, sim, paint, act, meta };

// 📚 Library (Useful classes & functions used throughout the piece)

function makeMotd({ handle, user }) {
  motd = ``;
    // `"chaos in a system"                             ` +
    // `                                                ` +
    // `Try typing:                                     ` +
    // `                                                ` +
    // ` 'of'                                           ` +
    // `  to see an Ordfish                             ` +
    // `                                                ` +
    // ` 'ff'                                           ` +
    // `  to see a Freaky Flower                        ` +
    // `                                                ` +
    // ` 'shape'                                        ` +
    // `  to paint freehand shapes                      ` +
    // `                                                ` +
    // ` 'bleep'                                        ` +
    // `  to play microtones                            ` +
    // `                                                ` +
    // ` 'help'                                         ` +
    // `  to learn more!                                ` +
    // `                                                ` +
    // `mail@aesthetic.computer                         `;

  if (user)
    motd =
      `Welcome, ${handle || user.name}!`.padEnd(48) + " ".padEnd(48) + motd;
}

function makeFlash($, clear = true, message) {
  flash = new $.gizmo.Hourglass($.seconds(message ? 0.35 : 0.1), {
    flipped: () => {
      flashShow = false;
      flashPresent = false;
      flashMessage = undefined;
      flash = undefined;
      $.needsPaint();
    },
    autoFlip: true,
  });

  flashPresent = true;
  flashShow = true;
  flashMessage = message;
  if (clear) input.text = "";
}
