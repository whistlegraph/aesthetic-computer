// Prompt, 2023.5.26.21.38.35
//         2021.11.28.03.13 (Created on)
// A language based "access-everything" console with LLM fallback.

/* #region 📚 README 
#endregion */

/* #region 🏁 todo
  + Later
  - [] Generate or pretty print docs (made from the APIs) inside this disk.
       (This would allow people to have a reference while writing disks.)
  + Done
  - [x] Reset pieceCount on developer reload.
  - [x] Reposition buttons once the frame is resized.
  - [x] Hide buttons after logging in.
  - [x] Hide buttons once starting to type. 
  - [x] Wire up login and sign-up buttons.
  - [x] Make Login Button
    - [x] Get layering working with `write`.
  - [x] Make Sign-Up Button
  - [x] Positioning Login button in center of display.
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

- The following is a data set of all possible options for commands:
  - 'alphapoet', 'bgm', 'bits', 'blank', 'bleep', 'bubble', 'camera', 
  'code-channel', 'decode', 'baktok', 'painting'
  'download', 'encode', 'ff', 'freaky-flowers', 'gargoyle', 'handle',
  'happy-hands-assembler', 'hha', 'liar', 'line', 'login', 
  'logout', 'm2w2', 'melody', 'metronome', 'microphone',
  'no!', 'no', 'oval', 'done', 'paint', 'paste', 'handprint', 
  'plot', 'profile', 'prompt', 'pull', 'rect', 
  'girlfriend', 'boyfriend', 'mom', 'dad', 'scawy-snake', 'scream', 'sfx', 'shape', 'sign', 'sing', 'smear', 
  'song', 'sparkle', 'right', 'left', 'flip', 'flop',
  'staka', 'starfield', 'tone', 'tracker', 'valbear', 'vary', 'video', 'wand', 'wg', 
  'wgr', 'whistle', 'whistlegraph', 'wipe', 'word', 'zoom', 'booted-by'.

- If I type a word that is similar to one of the commands, you only respond "did you mean
(insert correct command)?"
  - for example, if I write "linr", you write "Try typing 'line' instead"
  - you only suggest correct commands that are in the above data set
  - when you suggest a command, always put it in quotes. 
  - if I type "hife" you do not suggest "life" because that is not a command in the data set 
  - you do not respond with any additional information

- If I type a word for which there is no obvious match, you respond "I can't find a match, 
but you can text 1-508-728-4043 for "help". - @jeffrey"

The word I'm entering is:
`;

const after = ``;
const forgetful = true;

import { Android, MetaBrowser, iOS } from "../lib/platform.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { parse } from "../lib/parse.mjs";
import { ordfish } from "./ordfish.mjs";
const { abs, max, min } = Math;

// Error / feedback flash on command entry.
let flash;
let flashShow = false;
let flashColor = [];
let flashPresent = false;

let progressBar = -1; // If not zero, then draw a progress bar.
let progressTrick; // A faux growth period on the progress bar.

let login, // A login button in the center of the display.
  signup, // A Sign-up button.
  profile; // A profile page button.
let ruler = false; // Paint a line down the center of the display.
//                   (for measuring the login / signup centering).
let firstCommandSent = false; // 🏳️
let firstActivation = true; // 🏳️ Used to trigger a startup 🔊🎆

let startupSfx, keyboardSfx;

let tapePromiseResolve, tapePromiseReject;

let handles; // Keep track of total handles set.

import * as starfield from "./starfield.mjs";

// 🥾 Boot
async function boot({
  glaze,
  api,
  net,
  system,
  pieceCount,
  send,
  ui,
  screen,
  user,
  handle,
  params,
}) {
  glaze({ on: true });

  // Fetch handle count.
  fetch("/handle?count=true")
    .then((res) => res.json())
    .then((data) => {
      handles = data.handles;
    })
    .catch((err) => {
      console.warn("Could not get handles:", err);
    });

  // Boot starfield with a clear backdrop.
  starfield.boot();
  starfield.wipe(false);

  // TODO: How could I not keep reloading these sounds?
  //       Are they already cached?
  net.preload("startup").then((sfx) => (startupSfx = sfx)); // Load startup
  net.preload("compkey").then((sfx) => (keyboardSfx = sfx)); // and key sounds.

  // Create login & signup buttons.
  if (!user) {
    login = new ui.TextButton("Log in", { center: "xy", screen });
    signup = new ui.TextButton("I'm new", { center: "xy", screen });
    positionWelcomeButtons(screen);
  }
  if (user)
    profile = new ui.TextButton(handle() || user.name, {
      center: "xy",
      screen,
    });

  // Only if prompt is set to recall conversations.
  if (
    !system.prompt.convo.messages ||
    system.prompt.convo.messages?.length === 0
  ) {
    if (pieceCount === 0) {
      // system.prompt.input.print("aesthetic.computer"); // Set a default empty motd.
      if (!params[0]) makeMotd(api);
    } else {
      firstActivation = false; // Assume we've activated if returning from
      //                          elsewhere.
    }
    system.prompt.input.showButton(api, { nocopy: true, nopaste: true });
  }

  if (params[0]) {
    const text = params[0].replaceAll("~", " ");
    system.prompt.input.text = text;
    system.prompt.input.runnable = true;
    system.prompt.input.addUserText(text);
    system.prompt.input.snap();
  } else {
    system.prompt.input.text = "";
  }

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (pieceCount > 0) {
    activated(api, true);
    system.prompt.input.canType = true;

    // system.prompt.input.enter.btn.disabled = true; // Disable button.
    system.prompt.input.inputStarted = true;

    // 🍫 Create a pleasurable blinking cursor delay.
    // system.prompt.input.showBlink = false;
    // setTimeout(() => (system.prompt.input.showBlink = true), 100);
    send({ type: "keyboard:unlock" });
  }
}

// 🛑 Halt: (Intercept input and route it to commands.)
async function halt($, text) {
  const {
    api,
    broadcast,
    notice,
    handle,
    authorize,
    load,
    download,
    darkMode,
    text: { capitalize },
    num,
    store,
    connect,
    bgm,
    needsPaint,
    system,
    gizmo,
    screen,
    painting,
    net,
    jump,
    user,
    upload,
    code,
    send,
    help,
    zip,
    print,
    mint,
    rec,
    sound,
  } = $;
  motdController?.abort(); // Abort any motd update.

  // Roughly parse out the text (could also do a full `parse` here.)
  const tokens = text.split(" ");
  const slug = tokens[0]; // Note: Includes colon params.
  const params = tokens.slice(1);
  const input = $.system.prompt.input; // Reference to the TextInput.

  // Make a user authorized / signed request to the api.
  // Used both in `motd` and `handle`.
  async function userJSONRequest(method, endpoint, body) {
    try {
      const token = await authorize(); // Get user token.
      if (!token) throw new Error("🧖 Not logged in.");

      const headers = {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      };

      const options = { method, headers };
      if (body) options.body = JSON.stringify(body);
      const response = await fetch(endpoint, options);

      if (response.status === 500) {
        // const text = await response.text();
        // throw new Error(`🚫 Bad response: ${text}`);
        return { message: "error" };
      } else {
        return await response.json(); // Success!
      }
    } catch (error) {
      console.error("🚫 Error:", error);
      return { message: "unauthorized" };
    }
  }
  // 📼 Start taping.
  // Note: Right now, tapes get saved on refresh but can't be concatenated to,
  // and they start over when using `tape`.
  // This could eventually be replaced by a system that makes a new
  // video for every clip and then renders or stitches them together
  // in the end, where `video` can evolve into more of a clip editor.
  // Each of these clips can be stored in indexedDB more easily and played
  // back or be rearranged.
  // 23.09.16.18.01
  if (
    slug === "tape" ||
    slug === "tape:add" ||
    slug === "tape:tt" ||
    slug === "tape:nomic" ||
    slug === "tape:mic" ||
    slug === "tapem"
  ) {
    if (slug !== "tape:add") rec.slate(); // Start a recording over.
    const defaultDuration = 15;
    const tapePromise = new Promise((resolve, reject) => {
      tapePromiseResolve = resolve;
      tapePromiseReject = reject;
    });

    //const nomic = slug === "tape:nomic";
    let nomic;
    if (slug === "tape" || slug === "tape:tt") {
      nomic = iOS || Android ? false : true;
      if (params[0] === "baktok" || params[1] == "baktok") {
        nomic = false;
      } else {
        nomic = true;
      }
    } else if (slug === "tape:nomic") {
      nomic = true;
    } else if (slug === "tape:mic" || slug === "tapem") {
      nomic = false;
    }

    // setTimeout(function () {
    if (!nomic) sound.microphone.connect(); // Connect the mic.
    // }, 500);
    try {
      if (nomic) {
        console.log("📼 Taping...");
        tapePromiseResolve?.();
      }
      await tapePromise;
      let duration = parseFloat(params[0]);

      let jumpTo;

      // Gets picked up on next piece load automatically.
      rec.loadCallback = () => {
        // 😶‍🌫️ Running after the `jump` prevents any flicker and starts
        // the recording at the appropriate time.
        rec.rolling(
          "video" +
            (slug === "tape:tt" || jumpTo === "baktok" ? ":tiktok" : ""),
          (time) => {
            rec.tapeTimerSet(duration || defaultDuration, time);
          },
        ); // Start recording immediately.
      };

      if (isNaN(duration) && params[0]?.length > 0) {
        duration = defaultDuration; //Infinity;
        jumpTo = params[0];
        jump(params.join("~"));
        rec.videoOnLeave = true;
      } else if (params[1]) {
        jumpTo = params[1];
        jump(params.slice(1).join("~"));
      } else {
        jump("prompt");
      }
      flashColor = [0, 255, 0];
    } catch (err) {
      console.log(err);
      flashColor = [255, 0, 0];
    }
    makeFlash($);
    return true;
    // 📼 Cut a tape early.
  } else if (slug === "tape:cut" || slug === "cut") {
    let cutRes, cutRej;
    const cutPromise = new Promise((res, rej) => {
      cutRes = res;
      cutRej = rej;
    });
    setTimeout(cutRej, 250);
    rec.cut(() => {
      cutRes();
    });
    try {
      await cutPromise;
      jump("video");
      flashColor = [0, 255, 0];
    } catch (err) {
      flashColor = [255, 0, 0];
    }
    makeFlash($);
    // TODO: How can I hold the cursor here...
    return true;
  } else if (slug === "me") {
    jump("profile");
    return true;
  } else if (slug === "selfie") {
    jump("camera~me");
    return true;
  } else if (
    slug === "cam" ||
    text.startsWith === "cam " ||
    text.startsWith("cam:")
  ) {
    jump(text.replace("cam", "camera"));
    return true;
  } else if (slug === "camu") {
    jump("camera:under");
    return true;
  } else if (slug === "@maya/sparkle") {
    jump("sparkle");
    return true;
  } else if (slug === "painting:start") {
    system.nopaint.startRecord();
    console.log("🖌️🔴 Now recording:", system.nopaint.record);
    flashColor = [200, 0, 200];
    makeFlash($);
    return true;
  } else if (slug === "print" || slug === "mint") {
    progressBar = 0;

    progressTrick = new gizmo.Hourglass(24, {
      completed: () => (progressBar += min(0.5, progressBar + 0.1)),
      autoFlip: true,
    });

    try {
      if (slug === "print") {
        // 🏚️ Print a sticker.
        await print(system.painting, params[0], (p) => (progressBar = p));
      } else if (slug === "mint") {
        // 🪙 Mint on Zora.
        await mint(
          {
            ...system.painting,
            record: system.nopaint.recording
              ? system.nopaint.record
              : undefined,
          },
          (p) => (progressBar = p),
          params,
        );
      }
      flashColor = [0, 200, 0];
    } catch (err) {
      console.warn(err);
      flashColor = [200, 0, 0];
    }
    progressTrick = null;
    progressBar = 1;
    makeFlash($);
    return true;
  } else if (slug === "painting:done" || slug === "yes!" || slug === "done") {
    let destination = params[0] || "upload"; // or "upload"
    if (destination === "u" || slug === "yes!") destination = "upload";
    //                                  ^ "yes!" is always an upload.
    let filename; // Used in painting upload.
    let recordingSlug;

    if (system.nopaint.recording) {
      console.log("🖌️ Saving recording:", destination);
      const record = system.nopaint.record;
      filename = `painting-${record[record.length - 1].timestamp}.png`;
      // ^ For below, because the record will be cleared.

      if (destination === "upload") {
        progressBar = 0;
        progressTrick = new gizmo.Hourglass(24, {
          completed: () => (progressBar += min(0.5, progressBar + 0.1)),
          autoFlip: true,
        });
      }

      const zipped = await zip({ destination, painting: { record } }, (p) => {
        console.log("🤐 Zip progress:", p);
        progressBar = p;
      });

      progressTrick = null;

      console.log("🤐 Zipped:", zipped);
      recordingSlug = zipped.slug;

      system.nopaint.recording = false;
      system.nopaint.record = [];
      await store.delete("painting:record", "local:db");

      flashColor = [0, 255, 0];
    } else {
      filename = `painting-${num.timestamp()}.png`;
      flashColor = [255, 0, 0];
      console.warn("🖌️ No recording to save!");
    }

    // Always upload a PNG.
    if (destination === "upload") {
      console.log("🖼️ Uploading painting...");
      progressBar = 0; // Trigger progress bar rendering.
      progressTrick = new gizmo.Hourglass(24, {
        completed: () => (progressBar += min(0.5, progressBar + 0.1)),
        autoFlip: true,
      });
      try {
        const data = await upload(filename, store["painting"], (p) => {
          console.log("🖌️ Painting progress:", p);
          progressBar = p;
        });
        console.log("🪄 Painting uploaded:", filename, data);
        progressTrick = null;

        // Jump to the painting page that gets returned.
        if ((handle() || user?.name) && filename.startsWith("painting")) {
          jump(`painting~${handle() || user?.name}/${data.slug}`); // For a user.
        } else {
          jump(
            `painting~${data.slug}${recordingSlug ? ":" + recordingSlug : ""}`,
          ); // Or for a guest.
        }

        flashColor = [0, 255, 0];
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0];
      }
      makeFlash($);
    } else {
      makeFlash($);
    }

    progressBar = -1;
    return true;
  } else if (slug === "flower") {
    jump("lmn-flower");
    return true;
  } else if (slug === "petal") {
    jump("lmn-petal");
    return true;
  } else if (slug === "gf") {
    jump("girlfriend");
    return true;
  } else if (slug === "bf") {
    jump("boyfriend");
    return true;
  } else if (slug === "load") {
    // Load a file via URL.
    // Images:
    if (params[0].startsWith("http")) {
      // Replace painting with loaded image, adding it to the undo stack.
      try {
        const image = await net.preload(params[0]);
        system.nopaint.replace({ system, store, needsPaint }, image);
        flashColor = [0, 0, 255];
        makeFlash($);
        return true;
      } catch (err) {
        console.error("🚫 Could not load:", err);
        flashColor = [255, 0, 0];
        makeFlash($);
        return true;
      }
    } else {
      flashColor = [255, 0, 0];
      makeFlash($);
      return true;
    }
  }
  if (slug === "mood") {
    let res;
    if (params.join(" ").trim().length > 0) {
      res = await userJSONRequest("POST", "/api/mood", {
        mood: params.join(" "),
      });
    }
    console.log("RES", res);

    flashColor = res?.mood ? [0, 255, 0] : [255, 0, 0];
    if (res?.mood) {
      console.log("‍🍼 mood:", res.mood);
      notice(help.choose(":)", ":|", ":(", ":O", ":\\", ":/"));
    } else {
      const message = res?.message;
      let note = "ERROR";
      if (message === "unauthorized") note = "UNAUTHORIZED";
      makeFlash($, true);
      notice(note, ["yellow", "red"]);
      console.error("🍼🚫 Could not set mood.");
    }
    makeFlash($);
    return true;
  } else if (text === "publish") {
    // Publish the last devReload'ed or dragged piece.
    const publishablePiece = store["publishable-piece"];

    if (!publishablePiece) {
      flashColor = [255, 0, 0];
      makeFlash($);
      console.error("🪄 No publishable piece found!");
      return true;
    }

    const { slug, source } = publishablePiece;

    console.log("📥 Now publishing...", slug);

    // Upload to the user's "piece-" directory if possible.
    upload("piece-" + slug + ".mjs", source)
      .then((data) => {
        console.log("🪄 Code uploaded:", data);
        flashColor = [0, 255, 0];
        const route = handle() ? `${handle()}/${data.slug}` : data.slug;
        makeFlash($);
        send({ type: "alert", content: `\`${route}\` was published!` });
        jump(route);
      })
      .catch((err) => {
        console.error("🪄 Code upload failed:", err);
        send({
          type: "alert",
          content: `😥 Piece: \`${slug}\` failed to publish.`,
        });
        flashColor = [255, 0, 0];
        makeFlash($);
      });
    return true;
  } else if (text.startsWith("code-channel")) {
    // Set a `code-channel` for piece writing.
    if (!params[0]) {
      flashColor = [255, 0, 0];
    } else {
      code.channel(params.join(" "));
      flashColor = [0, 255, 0];
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("code")) {
    // Try to grab the piece requested in param[0] or just load blank.
    const piece = params[0] || "blank";
    const { host, path } = parse(piece);

    // Replacement tokens for blank piece.
    const tokens = {
      name: "$NAME",
      timestamp: "$TIMESTAMP",
      desc: "$THIS_IS_A_TEMPLATE_FOR_MAKING_NEW_PIECES",
    };

    // Inject the Blank template with some starting data.
    function inject(body, desc) {
      return body
        .replaceAll(tokens.name, capitalize(piece))
        .replaceAll(tokens.timestamp, num.timestamp())
        .replaceAll(tokens.desc, desc);
    }

    try {
      const result = await fetch(`https://${host}/${path}.mjs`);
      if (result.status === 404) throw new Error("📄🚫 Piece not found.");
      let body = await result.text();
      if (piece === "blank") body = inject(body, `A blank piece.`);
      download(`${piece}.mjs`, body);
      flashColor = [0, 0, 255];
    } catch (err) {
      console.error("📄🤯", err);
      // Download the blank piece if the piece was not found,
      // and replace it.
      console.log("📄📥 Downloading the blank piece.");
      const { host, path } = parse("blank");
      const result = await fetch(`https://${host}/${path}.mjs`);
      let body = await result.text();
      let desc = params.slice(1).join(" ");
      if (desc.length === 0) desc = `A piece called \`${piece}\`.`;
      body = inject(body, desc);
      download(`${piece}.mjs`, body);
      flashColor = [0, 0, 255];
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("email")) {
    // Set user email.
    const email = text.split(" ")[1];
    if (email) {
      const res = await userJSONRequest("POST", "/api/email", { email });
      console.log(res);
      if (res.email) {
        flashColor = [0, 255, 0];
        notice("CHECK " + res.email);
      } else {
        flashColor = [255, 0, 0];
        console.warn(res.message);
        notice("TAKEN?", ["yellow", "red"]);
      }
    } else {
      flashColor = [255, 0, 0];
    }
    makeFlash($, true);
    return true;
  } else if (text.startsWith("handle")) {
    // Set username handle.
    // TODO: This could eventually be abstracted for more API calls.
    // Something like... await post({handle: "new"});

    // Make sure there is a parameter.
    let handle = text.split(" ")[1];

    if (!handle) {
      flashColor = [0, 0, 128];
      makeFlash($);
      notice("EMPTY", ["cyan", "blue"]);
      return true;
    }

    if (handle[0] === "@") handle = handle.slice(1); // Strip off any leading "@" sign to help with validation.

    // And a handle has been specified.
    if (handle?.length > 0 && validateHandle(handle)) {
      const res = await userJSONRequest("POST", "/handle", { handle });
      const handleChanged = res?.handle;
      flashColor = handleChanged ? [0, 255, 0] : [255, 0, 0];
      if (handleChanged) {
        broadcast("handle:updated:" + res.handle);
        console.log("🧖 Handle changed:", res.handle);
        makeFlash($, true);
        notice("@" + res.handle);
      } else {
        const message = res?.message;
        let note = "TAKEN";
        if (message === "unauthorized") note = "UNAUTHORIZED";
        if (message === "unverified") note = "EMAIL UNVERIFIED";
        makeFlash($, true);
        notice(note, ["yellow", "red"]);
      }
      needsPaint();
    } else {
      flashColor = [255, 0, 0];
      console.warn("🧖 No @handle specified / bad handle design.");
      notice("INVALID!", ["yellow", "red"]);
    }
    return true;
  } else if ((text === "ul" || text === "upload") && store["painting"]) {
    if (!navigator.onLine) {
      flashColor = [255, 0, 0];
      notice("OFFLINE", ["yellow", "red"]);
    } else {
      const filename = `painting-${num.timestamp()}.png`;
      // The first dashed string will get replaced with a slash / media directory filter on the server.
      progressBar = 0; // Trigger progress bar rendering.
      try {
        const data = await upload(
          filename,
          store["painting"],
          (p) => (progressBar = p),
        );
        console.log("🪄 Painting uploaded:", filename, data);
        flashColor = [0, 255, 0, 128];
        makeFlash($);
        const slug = user
          ? `${handle() || user.email}/painting/${data.slug}`
          : data.slug;
        jump(`download:painting ${slug}`);
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0, 127];
        makeFlash($);
      }
    }
    return true;
  } else if (slug === "flip" || slug === "flop") {
    const vertical = slug === "flip"; // `flop` is lateral
    const w = system.painting.width,
      h = system.painting.height;
    // Invert the scale of the painting, pasting it into a new one of the
    // same size.
    const scale = vertical ? { x: 1, y: -1 } : { x: -1, y: 1 };
    system.painting = painting(w, h, (p) => {
      p.wipe(64).paste(system.painting, 0, 0, { scale });
    });

    // Persis the painting.
    store["painting"] = system.painting;
    store.persist("painting", "local:db"); // Also persist the painting.
    system.nopaint.addUndoPainting(system.painting, slug);
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "right" || slug === "left") {
    // Turn the canvas to the right or left.
    const angle = slug === "right" ? 90 : -90;
    const width = system.painting.height;
    const height = system.painting.width;

    let x = 0,
      y = 0;

    // Create a new painting with swapped width and height parameters.
    system.painting = painting(width, height, (p) => {
      // Then wipe, rotate and paste.
      // Paste the original painting, rotated by 90 degrees.
      if (angle === 90) {
        x += system.painting.height;
      } else if (angle === -90) {
        y += system.painting.width;
      }

      p.paste(system.painting, x, y, {
        scale: { x: 1, y: 1 },
        angle,
        anchor: { x: 0, y: 0 },
      });
    });

    // Move the painting to the center of the screen.
    system.nopaint.resetTransform({ system, screen });
    system.nopaint.storeTransform(store, system);

    // Persist the painting and lock the resolution.
    store["painting"] = system.painting;
    store.persist("painting", "local:db"); // Also persist the painting.
    system.nopaint.addUndoPainting(system.painting, slug);
    store["painting:resolution-lock"] = true; // Set resolution lock.
    store.persist("painting:resolution-lock", "local:db");

    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "resize" || slug === "res") {
    // Resize the active painting if one exists, or make one at this
    // size if it doesn't.
    const w = params[0],
      h = params[1] || w;

    let fullText = slug;
    if (params.length > 0) fullText += "~" + params.join("~");

    if (w === undefined) {
      flashColor = [255, 0, 0];
    } else {
      const result = nopaint_adjust(
        screen,
        system,
        painting,
        store,
        { w, h, scale: true },
        fullText,
      );
      flashColor = result ? "lime" : "red";
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
    store["gutter:lock"] = input.columns;
    // This will reflow on resize.
    flashColor = [100, 0, 100, 100]; // Dark Magenta
    makeFlash($);
    return true;
  } else if (slug === "login") {
    net.login();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    return true;
  } else if (slug === "hi") {
    net.login();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    return true;
  } else if (slug === "signup" || slug === "imnew") {
    net.signup();
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
      flashColor = [0, 0, 255, 100]; // Blue for successful undo.
    } else {
      flashColor = [255, 0, 0, 100]; // Red for failed undo.
    }
    makeFlash($);
    return true;
  } else if (text === "yes") {
    system.nopaint.no({ system, store, needsPaint }, true);
    if (system.nopaint.undo.paintings.length > 1) {
      flashColor = [0, 0, 255, 100]; // Blue for success.
    } else {
      flashColor = [255, 0, 0, 100]; // Red for fail.
    }
    makeFlash($);
    return true;
  } else if (text === "nopan") {
    system.nopaint.resetTransform(api);
    system.nopaint.storeTransform(store, system); // Store the translation after completion.
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "new") {
    // Combines "no!" and "painting:start";
    const w = parseInt(params[0]),
      h = parseInt(params[1]) || w;

    let size;
    if (!isNaN(w) && !isNaN(h)) size = { w, h };
    await system.nopaint.noBang(
      {
        system,
        store,
        screen,
        needsPaint,
        painting,
      },
      size, // Set a custom resolution to start.
    );
    let fullText = slug;
    if (params.length > 0) fullText += "~" + params.join("~");
    nopaint_adjust(screen, system, painting, store, size, fullText);
    system.nopaint.startRecord(fullText); // Start recording paintings.
    flashColor = [200, 0, 200];
    makeFlash($);
    return true;
  } else if (text === "painting:reset" || text === "no!") {
    const deleted = await system.nopaint.noBang({
      system,
      store,
      screen,
      needsPaint,
      painting,
    });

    system.nopaint.startRecord("new"); // Start recording paintings.

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
  } else if (text === "github" || text === "gh") {
    jump("https://github.com/digitpain/aesthetic.computer");
    return true;
  } else if (text === "browserstack" || text === "bs") {
    jump("https://live.browserstack.com");
    return true;
  } else if (text === "gpt") {
    jump("https://chat.openai.com");
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
    return loaded ? { left: true } : loaded;
  }
}

// 🎨 Paint
function paint($) {
  // 🅰️ Paint below the prompt || scheme.
  if ($.store["painting"]) {
    $.wipe(scheme.dark.bg); // Render the backdrop.
    $.system.nopaint.present($); // Render the painting.
    scheme.dark.bg[3] = 176; // Half semi-opaque palette background.
    scheme.light.bg[3] = 176;
  } else {
    $.wipe(scheme.dark.bg);
  }

  $.layer(1); // 🅱️ And above it...

  const { screen, ink, history } = $;

  if ($.system.prompt.input.canType) {
    let historyTexts =
      history.length === 0 ? [] : history.map((h) => h.replaceAll("~", " "));

    historyTexts.reverse().forEach((t, i) => {
      const ii = i + 1;
      ink(140, 90, 235, 80 / ii).write(t, { x: 6, y: 18 + 12 * i });
    });
  }

  if (progressBar >= 0) {
    ink(255, 180, 0, 120).box(0, 0, screen.width, screen.height, "inline");
    ink(0).box(1, 1, screen.width - 2, 1);
    if (progressBar > 0) {
      ink(scheme.dark.block).box(1, 1, (screen.width - 2) * progressBar, 1);
    }
  }

  if (!login?.btn.disabled || (profile && !profile.btn.disabled)) {
    starfield.paint($, { alpha: 0.4, color: [255, 0, 200] });
    if (handles && screen.height > 200)
      ink(255, 0, 255, 128).write(
        `${handles} HANDLES SET`,
        {
          center: "x",
          y: screen.height / 2 + screen.height / 2.75 - 11,
        },
        [255, 50, 200, 24],
      );
  }

  // Paint UI Buttons
  if (!login?.btn.disabled) login?.paint($, [[0, 0, 64], 255, 255, [0, 0, 64]]);
  if (!signup?.btn.disabled)
    signup?.paint($, [[0, 64, 0], 255, 255, [0, 64, 0]]);
  if (!profile?.btn.disabled) profile?.paint($);

  // 📏 Paint a measurement line in the center of the display.
  if (ruler) {
    $.ink(255, 0, 255, 127).line(
      screen.width / 2,
      0,
      screen.width / 2,
      screen.height,
    );
    if (screen.width % 2 === 0) {
      $.ink(255, 0, 255, 127).line(
        screen.width / 2 - 1,
        0,
        screen.width / 2 - 1,
        screen.height,
      );
    }
  }

  // Trigger a red or green screen flash with a timer.
  if (flashShow) {
    let color = firstActivation ? scheme.dark.block : flashColor;
    ink(color).box(0, 0, screen.width, screen.height);
    if (firstActivation) return true;
  }

  $.layer(0); // Return to the bottom layer.
  return false;
}

// 🧮 Sim
function sim($) {
  const input = $.system.prompt.input;
  progressTrick?.step();
  if (!login?.btn.disabled || !profile?.btn.disabled) {
    starfield.sim($);
    $.needsPaint();
  }

  if (
    $.store["handle:received"] // &&
    //input?.canType === false &&
    //(!$.system.prompt.messages || $.system.prompt.messages.length === 0)
  ) {
    profile = new $.ui.TextButton($.handle(), {
      center: "xy",
      screen: $.screen,
    });
    // if (firstCommandSent === true) profile.btn.disabled = true;
    delete $.store["handle:received"];
    $.needsPaint();
  }
  if (flashPresent) flash.step();
}

// 🎪 Act
function act({
  event: e,
  api,
  needsPaint,
  net,
  screen,
  num,
  jump,
  system,
  sound: { play, synth },
  rec,
  user,
  send,
  handle,
}) {
  // 📼 Taping
  if (e.is("microphone:connect:success")) {
    console.log("📼 Taping...");
    tapePromiseResolve?.();
  }

  if (e.is("microphone:connect:failure")) {
    console.warn("📼 🟡 Microphone failed to connect. Not taping.");
    // TODO: How to re-approve permission here in a cross-browser way?
    tapePromiseReject?.();
  }

  // 🔘 Buttons
  const downSound = () => {
    synth({
      type: "sine",
      tone: 600,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.001,
    });
  };

  const pushSound = () => {
    synth({
      type: "sine",
      tone: 800,
      attack: 0.1,
      decay: 0.99,
      volume: 0.75,
      duration: 0.005,
    });
  };

  login?.btn.act(e, {
    down: () => downSound(),
    push: () => {
      pushSound();
      net.login();
    },
  });

  signup?.btn.act(e, {
    down: () => downSound(),
    push: () => {
      pushSound();
      net.signup();
    },
  });

  profile?.btn.act(e, {
    down: () => downSound(),
    push: () => {
      pushSound();
      jump(handle() || "profile");
    },
  });

  // Rollover keyboard locking.
  // TODO: ^ Move the below events, above to rollover events.
  if (
    e.is("draw") &&
    ((login?.btn.disabled === false && login?.btn.box.contains(e)) ||
      (signup?.btn.disabled === false && signup?.btn.box.contains(e)) ||
      (profile?.btn.disabled === false && profile?.btn.box.contains(e)))
  ) {
    send({ type: "keyboard:lock" });
  }
  if (
    (e.is("touch") || e.is("lift")) &&
    ((login?.btn.disabled === false && login?.btn.box.contains(e)) ||
      (signup?.btn.disabled === false && signup?.btn.box.contains(e)) ||
      (profile?.btn.disabled === false && profile?.btn.box.contains(e)))
  ) {
    system.prompt.input.backdropTouchOff = true;
    send({ type: "keyboard:lock" });
  }

  if (e.is("lift") || e.is("touch")) needsPaint(); // Get button changes to
  //                                           ^      paint on-demand.
  // 🚨 Idea: It would be nice to pass     ----^
  //          what needs to be painted
  //          so the knowledge can be
  //          used in the `paint` function
  //          to allow for manual optimizations. 23.06.20.00.30

  // 🖥️ Screen
  if (e.is("reframed")) positionWelcomeButtons(screen);

  // ⌨️ Keyboard (Skip startup sound if a key is pressed or text is pasted.)
  if (e.is("keyboard:open") && firstActivation && e.method !== "pointer") {
    firstActivation = false;
  }

  if (e.is("pasted:text")) firstActivation = false;

  if (e.is("keyboard:down") && e.key !== "Enter") {
    play(keyboardSfx, { volume: 0.2 + (num.randInt(100) / 100) * 0.4 });
  }

  // 💾 Piece / disk loading
  if (e.is("load-error")) {
    makeFlash(api, false);
    flashColor = [255, 0, 0];
    if (MetaBrowser) api.system.prompt.input.canType = false;
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

// 🖥️ Run When the Prompt is activated.
function activated($, state) {
  if (firstActivation) {
    $.sound.play(startupSfx); // Play startup sound...
    flashColor = scheme.dark.block; // Trigger startup animation...
    makeFlash($, !$.params[0]); // Always sets firstActivation flag to false.
  }
  // console.log(state, firstCommandSent)
  // if (state === false && firstCommandSent) return;
  if (login) login.btn.disabled = state;
  if (signup) signup.btn.disabled = state;
  if (profile) profile.btn.disabled = state;
}

// 💬 Receive each response in full.
function reply(text) {
  firstCommandSent = true;
  // console.log("😀 Replied with:", text || "Halted?");
}

// 📰 Meta
function meta() {
  return {
    title: "prompt · aesthetic.computer",
    desc: "Type anything to get started.",
  };
}

// 👋 Leave
function leave() {
  motdController?.abort(); // Abort any motd update.
}

export {
  before,
  after,
  forgetful,
  halt,
  boot,
  paint,
  sim,
  act,
  activated,
  reply,
  meta,
  leave,
};
export const system = "prompt:character"; // or "prompt:code"

// Prompt configuration overrides.
export const wrap = "word"; // or "char"
export const scheme = {
  dark: {
    fg: [255, 100],
    fgu: [200, 30, 100, 200],
    bg: [70, 50, 100],
    block: [200, 30, 100],
    blockHi: [255, 100, 0],
    line: [0, 0, 255, 64],
  },
  light: {
    fg: [0, 200],
    fgu: [100, 200],
    bg: [170, 150, 200],
    block: [30, 200, 200],
    blockHi: [200, 200, 30],
    line: [0, 0, 0, 128],
  },
};

// 📚 Library
//   (Useful functions used throughout the piece)

let motdController;

async function makeMotd({ system, needsPaint, handle, user, net, api }) {
  let motd = "aesthetic.computer"; // Fallback motd.
  motdController = new AbortController();
  const res = await fetch("/api/mood/@jeffrey", {
    signal: motdController.signal,
  });
  if (res.status === 200) {
    motd = (await res.json()).mood;
    system.prompt.input.latentFirstPrint(motd);
    needsPaint();
  } else {
    console.warn("😢 No mood found.");
  }
  return motd;
}

function makeFlash($, clear = true) {
  flash = new $.gizmo.Hourglass($.seconds(0.1), {
    flipped: () => {
      progressBar = -1;
      flashShow = false;
      flashPresent = false;
      flash = undefined;
      firstActivation = false;
      $.needsPaint();
    },
    autoFlip: true,
  });

  flashPresent = true;
  flashShow = true;
  if (clear) {
    $.system.prompt.input.blank(); // Clear the prompt.
  }
}

function positionWelcomeButtons(screen) {
  if (login && signup) {
    login.reposition({ center: "xy", screen });
    signup.reposition({ center: "xy", screen });
    // Nudge signup and login by half their width.
    let offset = 5; // With a fixed pixel offset.
    signup.btn.box.x += signup.btn.box.w / 2 + offset;
    login.btn.box.x -= login.btn.box.w / 2 + offset;
    if (screen.width % 2 !== 0) login.btn.box.x += 1; // Nudge odd display width.
  }

  if (profile) profile.reposition({ center: "xy", screen });
}
