// Prompt, 2023.5.26.21.38.35
// A language based "access-everything" console with LLM fallback.

/* #region 📚 README 
#endregion */

/* #region 🏁 todo
 - [] Lock prompt on handle change.
 - [] Prevent non-printable characters from causing an empty space.
 - [] Generate or pretty print docs (made from the APIs) inside this disk.
      (This would allow people to have a reference while writing disks.)
 + Later
 - [] An iOS app would need a small ESC or arrow overlay button in Swift
      to make this work properly.
  + Done
  - [x] Write an initial prompt program.
  - [x] Generically "lock" prompt after input before a result returns.
        Show a spinner if too much time has passed?
  - [x] Pass the api into `halt`.
  - [x] create a system.prompt.input reference in the api.
  - [x] `export const palette`
  - [x] `export const wrap`
  - [x] `export const autolock`
#endregion */

const before = `
You are playing a character who tries to help me find the command I'm searching for
- The options for the commands are: 'line' 'gargoyle' 'rect' 'smear' 'oval' 'shape' 'word'
If I type 'linr' for example, you say 'how bout typing line instead :P'. If I type 'round' for example, you say 'try typing oval instead ;)'. Choose between emoticons,  ';)' ':)' ':P' ';P' ';*' ';$', at the end of your response.
-  When you suggest the right command, your responses must have only lower case letters.


If I type something like 'tree' or 'dog', which isn't directly related to any of the commands, or if I type random letters, you respond: 'hmm I'm stumped. type list to explore' and make sure all letters are lower case.
`;

// const before = `
// You are playing a character who tries to help me find the command I'm searching for
// - the options for the commands are: 'line' 'rect' 'smear' 'oval' 'shape' 'word'
// If I type 'linr' for example, you say 'do you want the line tool?'
//   - If I type 'round' for example, you say 'do you want the oval tool?'
// If I type something like 'tree' or 'dog', which isn't directly related to any of the commands, you provide a list of all of the possible commands.
//   - You say 'not sure what you're looking for, you can choose between: ...' and list the possible command options
//   - And at the end add 'if none of these are what you need, get more help here'

// If the user types 'yes' in response to your question, go directly to that command's page
// `;

// (requires convo support)

// const before = `
// Please play a game with me. The rules are:
//   - I have typed something into a machine incorrectly.
//   - And you need to suggest a correct message.
//   - You can suggest a correct word based on my incorrect attempt.
//   - The correct words are: 'line', 'rect', 'smear', 'freaky-flower' (ff), 'happy-hands-assembler' (hha), 'bleep', and 'word'.
// Here is what I have typed in:`;

const after = ``;
const forgetful = true;

import { MetaBrowser } from "../lib/platform.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { parse } from "../lib/parse.mjs";
import { ordfish } from "./ordfish.mjs";
const { abs, max } = Math;

// Error / feedback flash on command entry.
let flash;
let flashShow = false;
let flashMessage;
let flashColor = [];
let flashPresent = false;
let uploadProgress = 0; // If not zero, then draw a progress bar.

// 🛑 Intercept input and route it to commands.
async function halt($, text) {
  const {
    api,
    handle,
    authorize,
    load,
    download,
    darkMode,
    num,
    store,
    connect,
    bgm,
    needsPaint,
    system,
    screen,
    painting,
    net,
    jump,
    user,
    upload,
  } = $;
  // Roughly parse out the text (could also do a full `parse` here.)
  const tokens = text.split(" ");
  const slug = tokens[0]; // Note: Includes colon params.
  const params = tokens.slice(1);
  const input = $.system.prompt.input; // Reference to the TextInput.

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
    return true;
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
    return true;
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
    return true;
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
    return true;
  } else if (slug === "gutter") {
    // Change the `TextInput` gutter to a minimum of 5 or a default of 16.
    input.gutter = max(5, parseInt(params[0])) || 16;
    // This will reflow on resize.
    flashColor = [100, 0, 100, 100]; // Dark Magenta
    makeFlash($);
    return true;
  } else if (slug === "login" || slug === "hi") {
    net.login();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    return true;
  } else if (text === "logout" || text === "bye") {
    net.logout();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    return true;
  } else if (text === "no") {
    system.nopaint.no({ system, store, needsPaint });
    if (system.nopaint.undo.paintings.length > 1) {
      flashColor = [0, 0, 255, 100]; // Blue for succesful undo.
    } else {
      flashColor = [255, 0, 0, 100]; // Red for failed undo.
    }
    makeFlash($);
    return true;
  } else if (text === "nopan") {
    system.nopaint.resetTransform(api);
    system.nopaint.storeTransform(store, system); // Store the translation after completion.
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
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
    return true;
  } else if (text === "3dline:reset") {
    const deleted = await store.delete("3dline:drawing", "local:db");

    if (deleted) {
      flashColor = [0, 0, 255]; // Blue for succesful deletion.
    } else {
      flashColor = [255, 0, 0]; // Red if delete failed.
    }

    makeFlash($);
    needsPaint();
    return true;
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
    return true;
  } else if (text.startsWith("2022")) {
    load(parse("wand~" + text)); // Execute the current command.
    return true;
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
    return true;
  } else if (text === "bgm stop") {
    bgm.stop();
    makeFlash($, false);
    flashColor = [255, 0, 0];
    return true;
  } else if (text === "code") {
    jump("https://github.com/digitpain/aesthetic.computer");
    return true;
  } else if (text === "help") {
    // Go to the Discord for now if anyone types help.
    jump("https://discord.gg/aesthetic-computer");
    return true;
  } else if (text === "shillball" || text === "sb") {
    // Shortcuts for Yeche's Shillball game.
    jump("https://galerie-yechelange.baby/ball");
    return true;
  } else if (text === "prod") {
    jump("https://prompt.ac"); // Visit the live site.
    return true;
  } else if (text === "local" || text.startsWith("local")) {
    const param = text.replace("local", "").trim().replaceAll(" ", "~");
    const slug = param.length > 0 ? `/${param}` : "";
    console.log(slug);
    jump("https://localhost:8888" + slug); // Go to the local dev server, passing any params as a piece.
    return true;
  } else if (text.split(" ")[0] === "of") {
    // Ordfish shortcuts.
    jump(`ordfish~${text.split(" ").slice(1).join("~")}`);
    return true;
  } else if (ordfish[text] || text.split(" ") === "of") {
    jump(`ordfish~${text}`);
    return true;
  } else {
    // 🟠 Local and remote pieces...
    const loaded = await load(parse(text)); // Execute the current command.
    return loaded;
  }
}

// 🥾 Boot
function boot({ glaze, api, system, pieceCount, send }) {
  glaze({ on: true });

  if (
    !system.prompt.convo.messages ||
    system.prompt.convo.messages?.length === 0
  ) {
    system.prompt.input.text = makeMotd(api); // Override prompt with motd if
    //                                           no conversation is present.
    system.prompt.input.lastText = system.prompt.input.text;
    system.prompt.input.showButton();
  }

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (pieceCount > 0) {
    system.prompt.input.canType = true;
    system.prompt.input.text = "";
    system.prompt.input.go.btn.disabled = true; // Disable button.
    system.prompt.input.inputStarted = true;

    // 🍫 Create a pleasurable blinking cursor delay.
    system.prompt.input.showBlink = false;
    setTimeout(() => (system.prompt.input.showBlink = true), 100);
    send({ type: "keyboard:unlock" });
  }
}

// 🎨 Paint
function paint($) {
  // 🅰️ Paint below the prom || schemept.
  if ($.store["painting"]) {
    $.system.nopaint.present($); // Render the painting.
    scheme.dark.bg[3] = 127; // Half the opacity of the palette background.
    scheme.light.bg[3] = 127;
  } else {
    $.wipe(scheme.dark.bg);
  }

  $.layer(1); // 🅱️ And above it...

  let historyTexts;
  const { screen, ink, history } = $;
  const input = $.system.prompt.input;

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

  return false;
}

// 🧮 Sim
function sim($) {
  const input = $.system.prompt.input;
  if (
    $.store["handle:received"] &&
    input?.canType === false &&
    $.system.prompt.messages?.length === 0
  ) {
    input.text = makeMotd($);
    input.canType = false;
    delete $.store["handle:received"];
    $.needsPaint();
  }
  if (flashPresent) flash.step();
}

// 🎪 Act
function act({ event: e, api }) {
  const input = $.system.prompt.input;
  if (e.is("load-error")) {
    makeFlash(api);
    flashColor = [255, 0, 0];
    if (MetaBrowser) input.canType = false;
    needsPaint();
  }
}

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
    title: "prompt · aesthetic.computer",
    desc: "Type anything to get started.",
  };
}

export { before, after, forgetful, halt, boot, paint, sim, act, meta };
export const system = "prompt:character"; // or "prompt:code"

// Prompt configuration overrides.
export const wrap = "word";
// export const wrap = "char";
export const scheme = {
  dark: {
    fg: [255, 100],
    bg: [70, 50, 100],
    block: [200, 30, 100],
    blockHi: [255, 100, 0],
    line: [0, 0, 255, 64],
  },
  light: {
    fg: [0, 200],
    bg: [170, 150, 200],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

// 📚 Library
//   (Useful functions used throughout the piece)

function makeMotd({ handle, user }) {
  let motd = ``;
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
  if (user) {
    motd =
      `Welcome, ${handle || user.name}!`.padEnd(48) + " ".padEnd(48) + motd;
  } else motd = "monday    tuesday wednesday";
  return motd;
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
  if (clear) $.system.prompt.input.blank(); // Clear the prompt.
}
