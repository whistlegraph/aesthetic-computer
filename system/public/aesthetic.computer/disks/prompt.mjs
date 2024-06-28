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
  - 'bgm', 'bits', 'blank', 'bleep', 'bubble', 'camera',
  'channel', 'decode', 'baktok', 'painting'
  'download', 'encode', 'ff', 'freaky-flowers', 'gargoyle', 'handle',
  'happy-hands-assembler', 'hha', 'liar', 'line', 'login',
  'logout', 'm2w2', 'melody', 'metronome', 'microphone',
  'no!', 'no', 'oval', 'done', 'paint', 'paste', 'handprint',
  'plot', 'profile', 'prompt', 'pull', 'rect',
  'girlfriend', 'boyfriend', 'mom', 'dad', 'husband', 'wife', 'kid', 'brother', 'sister', 'scawy-snake', 'scream', 'sfx', 'shape', 'sign', 'sing', 'smear',
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

If the user asks to delete their account or enters "delete" or "deactivate", you tell them to enter "delete-erase-and-forget-me" to delete their account.

If the user enters 'goodiepal' please reply: Yes, but people on the Faro islands call me Pruttipal, so enter 'prutti' instead.

The word I'm entering is:`;

const after = ``;
const forgetful = true;

const TYPO_REPLY = `
Use Aesthetic Computer by entering a correct word.\n\nEnter "list" for available words.\n\nEnter "chat" for help.\n\n - @jeffrey`.trim();

import { Android, MetaBrowser, iOS } from "../lib/platform.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { parse } from "../lib/parse.mjs";
import { ordfish } from "./ordfish.mjs";
const { abs, max, min } = Math;
const { keys } = Object;

// Error / feedback flash on command entry.
let flash;
let flashShow = false;
let flashColor = [];
let flashPresent = false;

let progressBar = -1; // If not zero, then draw a progress bar.
let progressTrick; // A faux growth period on the progress bar.

let login, // A login button in the center of the display.
  signup, // A Sign-up button.
  profile, // A profile page button.
  profileAction;
let resendVerificationText;
let ellipsisTicker;
let ruler = false; // Paint a line down the center of the display.
//                   (for measuring the login / signup centering).
// let firstCommandSent = false; // 🏳️
let firstActivation = true; // 🏳️ Used to trigger a startup 🔊🎆

let startupSfx, keyboardSfx;

let tapePromiseResolve, tapePromiseReject;

let handles; // Keep track of total handles set.

let defaultDownloadScale = 6;

import * as starfield from "./starfield.mjs";

let server;

let darkModeOn;
let pal;
// let schemeAll;

let autocompletions = {};
const activeCompletions = [];

// 🥾 Boot
async function boot({
  glaze,
  api,
  gizmo,
  net,
  system,
  pieceCount,
  send,
  ui,
  screen,
  user,
  handle,
  params,
  notice,
  dark,
  store,
  // code,
  net: { socket },
}) {
  if (dark) glaze({ on: true });

  net.requestDocs().then((d) => {
    autocompletions = { ...d.pieces, ...d.prompts };
    // Remove hidden autocompleteions.
    keys(autocompletions).forEach((key) => {
      if (autocompletions[key].hidden) delete autocompletions[key];
    });
    // console.log("✍️ Autocompletions built:", autocompletions);
  });

  server = socket((id, type, content) => {
    // console.log("🧦 Got message:", id, type, content);
  });

  // Fetch handle count.
  fetch("/handle?count=true")
    .then((res) => res.json())
    .then((data) => {
      handles = data.handles;
    })
    .catch((err) => {
      console.warn("💁 Could not get handle count.");
    });

  // Boot starfield with a clear backdrop.
  starfield.boot(api, { stars: 128 });
  starfield.wipe(false);

  // TODO: How could I not keep reloading these sounds?
  //       Are they already cached?
  net
    .preload("startup")
    .then((sfx) => (startupSfx = sfx))
    .catch((err) => console.warn(err)); // Load startup
  net
    .preload("compkey")
    .then((sfx) => (keyboardSfx = sfx))
    .catch((err) => console.warn(err)); // and key sounds.

  // Create login & signup buttons.
  if (!user) {
    login = new ui.TextButton("Log in", { center: "xy", screen });
    signup = new ui.TextButton("I'm new", { center: "xy", screen });
    positionWelcomeButtons(screen, net.iframe);
  }

  if (user) {
    // console.log("User:", user);
    const hand = handle();
    const btnPos = { center: "xy", screen };
    if (hand) {
      profileAction = "profile";
      profile = new ui.TextButton(hand, btnPos);
    } else if (!user.email_verified) {
      profile = new ui.TextButton("Resend email", { center: "xy", screen });
      profileAction = "resend-verification";
      ellipsisTicker = new gizmo.EllipsisTicker();
      // Start fetching to continuously check for verification here...
      function fetchUser() {
        fetch(`/user?from=${encodeURIComponent(user.email)}`)
          .then((res) => res.json())
          .then((u) => {
            if (u.email_verified) {
              flashColor = "lime";
              makeFlash(api, false, true);
              profileAction = "set-handle";
              profile = new ui.TextButton("Create handle", btnPos);
            } else setTimeout(fetchUser, 1000);
          })
          .catch((err) => setTimeout(fetchUser, 1000));
      }
      fetchUser();
    } else if (user.email_verified) {
      profileAction = "set-handle";
      profile = new ui.TextButton("Create handle", btnPos);
    }
  }

  // Only if prompt is set to recall conversations.
  if (
    !system.prompt.convo.messages ||
    system.prompt.convo.messages?.length === 0
  ) {
    if (pieceCount === 0 || store["prompt:splash"] === true) {
      // system.prompt.input.print("aesthetic.computer"); // Set a default empty motd.
      if (!params[0]) makeMotd(api);
    } else {
      firstActivation = false; // Assume we've activated if returning from
      //                          elsewhere.
    }
    system.prompt.input.showButton(api, {
      nocopy: true,
      nopaste: pieceCount === 0,
    });
  }

  if (params[0]) {
    const text = params.join(" ");
    // const text = params[0].replaceAll("~", " ");
    system.prompt.input.text = text;
    system.prompt.input.runnable = true;
    system.prompt.input.addUserText(text);
    system.prompt.input.snap();
    send({ type: "keyboard:text:replace", content: { text } });
  } else {
    system.prompt.input.text = "";
  }

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (pieceCount > 0 && !store["prompt:splash"]) {
    activated(api, true);
    system.prompt.input.canType = true;

    // system.prompt.input.enter.btn.disabled = true; // Disable button.
    //system.prompt.input.inputStarted = true;

    // 🍫 Create a pleasurable blinking cursor delay.
    // system.prompt.input.showBlink = false;
    // setTimeout(() => (system.prompt.input.showBlink = true), 100);
    send({ type: "keyboard:unlock" });
    // send({ type: "keyboard:open" });
  }

  delete store["prompt:splash"];
}

// 🛑 Halt: (Intercept input and route it to commands.)
async function halt($, text) {
  const {
    api,
    broadcast,
    notice,
    handle,
    load,
    download,
    darkMode,
    text: { capitalize },
    num,
    store,
    connect,
    bgm,
    needsPaint,
    leaving,
    system,
    gizmo,
    screen,
    painting,
    net,
    jump,
    beep,
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
    canShare,
    debug,
  } = $;
  activeCompletions.length = 0; // Reset activeCompletions on every halt.
  motdController?.abort(); // Abort any motd update.

  // Roughly parse out the text (could also do a full `parse` here.)
  const tokens = text.split(" ");
  const slug = tokens[0]; // Note: Includes colon params.
  const params = tokens.slice(1);
  const input = $.system.prompt.input; // Reference to the TextInput.

  // 🕸️ Custom URL routing.
  if (slug.startsWith("/")) {
    jump(`https://${debug ? "localhost:8888" : "aesthetic.computer"}${slug}`);
    return true;
  } else if (
    // 📼 Start taping.
    // Note: Right now, tapes get saved on refresh but can't be concatenated to,
    // and they start over when using `tape`.
    // This could eventually be replaced by a system that makes a new
    // video for every clip and then renders or stitches them together
    // in the end, where `video` can evolve into more of a clip editor.
    // Each of these clips can be stored in indexedDB more easily and played
    // back or be rearranged.
    // 23.09.16.18.01
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

    if (!nomic) sound.microphone.connect(); // Connect the mic.
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
  } else if (slug === "me" || slug === "profile") {
    console.log("Logged in?", user);
    if (user) {
      jump("profile");
      return true;
    } else {
      notice("LOG IN OR SIGN UP", ["yellow", "red"]);
      $.system.prompt.input.blank(); // Clear the prompt.
      send({ type: "keyboard:close" });
      return true; //"dont-unlock";
    }
  } else if (slug === "scream") {
    // TODO: Scream additions. 23.12.11.12.53
    // - [] Vocalize all screams / make a sound?
    // - [] Smartly time-synchronize that message for all users by looking ahead?
    // console.log("😱 Screaming...");
    server?.send("scream", params.join(" ") || "Ahh!");
    flashColor = [255, 0, 0];
    makeFlash($);
    return true;
  } else if (slug === "nonotifs") {
    send({
      type: "ios:send",
      content: { type: "notifications", body: false },
    });
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "notifs") {
    send({
      type: "ios:send",
      content: { type: "notifications", body: true },
    });
    flashColor = [0, 0, 255];
    makeFlash($);
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
  } else if (slug === "@maya" + "/sparkle") {
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

      // TODO: Don't delete painting record unless `new` is entered. 23.10.03.01.51
      // system.nopaint.recording = false;
      // system.nopaint.record = [];
      // await store.delete("painting:record", "local:db");

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
  } else if (slug === "bro") {
    jump("brother");
    return true;
  } else if (slug === "sis") {
    jump("sister");
    return true;
  } else if (slug === "gf") {
    jump("girlfriend");
    return true;
  } else if (slug === "bf") {
    jump("boyfriend");
    return true;
  } else if (slug === "bb") {
    jump("booted-by");
    return true;
  } else if (slug === "p" || slug === "pain") {
    jump("painting");
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
  } else if (slug === "mood:nuke" || slug === "mood:denuke") {
    const nuke = slug === "mood:nuke";
    const label = slug === "mood:nuke" ? "NUKE" : "DENUKE";
    const res = await net.userRequest("POST", "/api/mood", { nuke });
    flashColor = res?.deleted ? [0, 255, 0] : [255, 0, 0];
    if (res?.altered >= 0) {
      notice(`${label}D MOODS`);
    } else {
      notice(`${label} FAILED`, ["yellow", "red"]);
    }
    makeFlash($, true);
    return true;
  } else if (slug === "mood") {
    let res;
    if (params.join(" ").trim().length > 0) {
      res = await net.userRequest("POST", "/api/mood", {
        mood: params.join(" "),
      });
    }

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
  } else if (text.startsWith("publish")) {
    const publishablePiece = store["publishable-piece"];
    if (!publishablePiece) {
      flashColor = [255, 0, 0];
      makeFlash($);
      notice("No piece found!", ["yellow", "red"]);
      console.error("🪄 No publishable piece found!");
      return true;
    }

    // 🐍
    // TODO: May need to detect (...) here and give the piece
    //       a random name if one is not specified.
    let publishSlug = params[0] || publishablePiece.slug;
    if (publishSlug === "(...)") {
      publishSlug = "sketch";
    }

    // Check if publishSlug contains only valid filename characters.
    if (!/^[a-zA-Z0-9_-]+$/.test(publishSlug)) {
      notice("BAD NAME", ["yellow", "red"]);
      // throw new Error("publishSlug contains invalid characters.");
    } else {
      await publishPiece(
        { api, send, jump, handle, upload },
        publishSlug,
        publishablePiece.source,
        publishablePiece.ext,
      );
    }

    return true;
  } else if (text.startsWith("channel") || text.startsWith("code-channel")) {
    // Set a `code-channel` for piece writing.
    let newChannel = params.join(" ") || "";
    let isNone = false;
    console.log("new channel:", newChannel);
    if (newChannel === "") {
      const currentChannel = await store.retrieve("code-channel");
      isNone = true;
      const text = currentChannel || "no channel";
      notice(
        text,
        text === "no channel" ? ["yellow", "red"] : ["white", "blue"],
        { wrap: "char" },
      );
    } else if (newChannel === "none") {
      newChannel = "";
      isNone = true;
      notice("no channel", ["yellow", "red"]);
      code.channel(newChannel);
    } else {
      notice(newChannel, ["white", "blue"]);
      code.channel(newChannel);
    }

    if (isNone) {
      flashColor = [255, 0, 0];
    } else {
      flashColor = [0, 0, 255];
    }
    makeFlash($);
    return true;
  } else if (text === "run") {
    send({ type: "post-to-parent", content: { type: "runPiece" } });
    makeFlash($);
    return true;
  } else if (text === "docs") {
    if (net.iframe) {
      send({ type: "post-to-parent", content: { type: "openDocs" } });
    } else {
      jump("out:/docs");
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("edit")) {
    jump(
      "out:https://vscode.dev/github/digitpain/aesthetic.computer-code/blob/main/blank.mjs",
    );
    makeFlash($);
    return true;
  } else if (text === "+") {
    jump(`out:${location.origin}`);
    makeFlash($);
    return true;
  } else if (text.startsWith("google")) {
    const query = text.substring(7).trim(); // Extract the search query
    jump(`https://www.google.com/search?q=${encodeURIComponent(query)}`);
    makeFlash($);
    return true;
  } else if (text.startsWith("source")) {
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
      const fullUrl = `https://${host}/${path}.mjs`;
      console.log("📥 Attempting to load source from url:", fullUrl);
      let result = await fetch(fullUrl);

      if (result.status === 404) {
        const anonUrl =
          "https://art.aesthetic.computer/" + path.split("/").pop() + ".mjs";
        console.log("🧑‍🤝‍🧑 Attempting to load piece from anon url:", anonUrl);
        result = await fetch(anonUrl);
        if (result.status === 404) {
          throw new Error("📄🚫 Piece not found.");
        }
      }

      let body = await result.text();
      if (piece === "blank") body = inject(body, `A blank piece.`);

      if (!net.iframe) {
        download(`${piece}.mjs`, body);
      } else {
        send({
          type: "post-to-parent",
          content: { type: "openSource", title: `${piece}.mjs`, source: body },
        });
      }

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
    let clear = true;
    if (email) {
      const res = await net.userRequest("POST", "/api/email", {
        email,
        name: email,
      });
      console.log("Request:", res);
      if (res.email) {
        flashColor = [0, 255, 0];
        notice("Check " + res.email);
        send({ type: "keyboard:close" });
      } else {
        flashColor = [255, 0, 0];
        console.warn(res.message);
        notice("NETWORK ERROR", ["yellow", "red"]);
        clear = false;
      }
    } else {
      flashColor = [255, 0, 0];
    }
    makeFlash($, clear);
    return true;
  } else if (slug.startsWith("admin:migrate-")) {
    // Usage: `admin:migrate-painting`
    //        `admin:migrate-piece`
    const res = await net.userRequest(
      "GET",
      `/api/admin?migrate=${slug.split("-")[1]}`,
    );
    flashColor = res && res.status === 202 ? [0, 255, 0] : [255, 0, 0];
    if (res && res.status === 202) {
      notice("MIGRATION STARTED ;)");
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("handle") && !text.startsWith("handles")) {
    // Set username handle.
    // TODO: This could eventually be abstracted for more API calls.
    // Something like... await post({handle: "new"});

    // Make sure there is a parameter.
    let newHandle = text.split(" ")[1];

    if (!newHandle) {
      flashColor = [0, 0, 128];
      makeFlash($);
      notice("EMPTY", ["cyan", "blue"]);
      return true;
    }

    if (newHandle[0] === "@") newHandle = newHandle.slice(1); // Strip off any leading "@" sign to help with validation.

    // And a handle has been specified.
    const validated = validateHandle(newHandle);
    if (newHandle?.length > 0 && validated === "valid") {
      const res = await net.userRequest("POST", "/handle", {
        handle: newHandle,
      });
      const handleChanged = res?.handle;
      flashColor = handleChanged ? [0, 255, 0] : [255, 0, 0];
      if (handleChanged) {
        const previousHandle = handle();
        broadcast("handle:updated:" + res.handle);
        console.log("🧖 Handle changed:", res.handle);
        makeFlash($, true);
        if (previousHandle) notice("@" + res.handle);
        profileAction = "profile";
        if (!previousHandle) {
          jump("chat");
          beep();
        }
        store["handle"] = res.handle;
        store.persist("handle");
      } else {
        const note = res?.message || "error";
        console.log("Response:", res);
        makeFlash($, true);
        notice(note.toUpperCase(), ["yellow", "red"]);
      }
      needsPaint();
    } else {
      console.warn("🧖 No @handle specified / bad handle design:", validated);
      notice(validated.toUpperCase(), ["yellow", "red"]);
    }
    return true;
  } else if (text.startsWith("admin:handle:strip")) {
    const handleToStrip = text.split(" ")[1];
    //  🩹️ Strip the handle from a user.

    if (handleToStrip) {
      console.log("🩹 Stripping handle:", handleToStrip);
      const res = await net.userRequest("POST", "/handle", {
        handle: handleToStrip,
        action: "strip",
      });

      console.log("🩹 Strip result:", res);
      notice(
        res.message.toUpperCase(),
        res.status === 200 ? undefined : ["yellow", "red"],
      );
      flashColor = res.status === 200 ? "lime" : "red";
      makeFlash($, true);
      // If the handle was stripped then somehow broadcast it
      // and update the chat.
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
      if (!canShare) {
        downloadPainting(
          api,
          abs(parseInt(text.split(" ")[1])) || defaultDownloadScale,
        );
      }
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
    // if (net.iframe) jump("login-wait");
    return true;
  } else if (slug === "hi") {
    net.login();
    flashColor = [255, 255, 0, 100]; // Yellow
    makeFlash($);
    // if (net.iframe) jump("login-wait");
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
    // Combines "no!" and "painting:start" in a graphics context;
    // Or jumps to the creation of a new piece of code given a textual parameter.

    // ❓ How could this UX be improved for a better in-editor experience? 24.02.23.19.12

    if (
      ["piece", "bot", "brush", "fps", "space", "stamp"].includes(params[0])
    ) {
      try {
        const response = await fetch(
          `https://raw.githubusercontent.com/digitpain/aesthetic.computer-code/main/${params[0]}.mjs`,
        );
        let body = await response.text();

        const lines = body.split("\n"); // Split the body into lines.
        if (
          params[1] &&
          lines.length >= 2 && // Check if the first two lines are comments...
          lines[0].startsWith("//") &&
          lines[1].startsWith("//")
        ) {
          lines[0] = `// ${capitalize(params[1]) || ""}, ${num.timestamp()}`;
          const desc = params.slice(2).join(" ");
          if (desc) lines[1] = `// ${desc}`;
          body = lines.join("\n");
        }

        const name = params[1] || params[0];
        if (!net.iframe) {
          download(`${name}.mjs`, body);
        } else {
          send({
            type: "post-to-parent",
            content: {
              type: "openSource",
              title: `${name}.mjs`,
              source: body,
            },
          });
          flashColor = [0, 0, 255];
          makeFlash($);
          return true;
        }
      } catch (error) {
        console.error("Error fetching source:", error);
        flashColor = [255, 0, 0];
        makeFlash($);
        return true;
      }
    }

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
  } else if (text === "dark" || text === "light") {
    if (text === "light") {
      store.delete("dark-mode");
      darkMode(false);
      flashColor = [255, 255, 255];
    } else {
      flashColor = [0, 0, 0];
      darkMode(true);
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
  } else if (text.toLowerCase() === "sotce-net") {
    jump(
      debug ? "https://" + location.host + "/sotce-net" : "https://sotce.net",
    );
    flashColor = "pink";
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "github" || text === "gh") {
    jump("https://github.com/digitpain/aesthetic.computer");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "ucla-syllabus") {
    jump("out:https://docs.google.com/document/d/1foiOLdvJeTdPHQKMIWzKBoGcszGPJS5bRcY-Rg3ou6A/edit?usp=sharing");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "app" || text === "ios") {
    jump("https://apps.apple.com/app/aesthetic-computer/id6450940883");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "pp") {
    jump("https://aesthetic.computer/privacy-policy");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "support") {
    jump("https://aesthetic.computer/support");
    makeFlash($);
    return true;
  } else if (text === "browserstack" || text === "bs") {
    jump("https://live.browserstack.com");
    makeFlash($);
    return true;
  } else if (text === "gpt") {
    jump("https://chat.openai.com");
    makeFlash($);
    return true;
  } else if (text === "help") {
    // Go to the Discord for now if anyone types help.
    jump("out:https://discord.gg/aesthetic-computer");
    makeFlash($);
    return true;
  } else if (text === "shillball" || text === "sb") {
    // Shortcuts for Yeche's Shillball game.
    jump("https://galerie-yechelange.baby/ball");
    makeFlash($);
    return true;
  } else if (text === "prod") {
    jump("https://prompt.ac"); // Visit the live site.
    makeFlash($);
    return true;
  } else if (text === "local" || text.startsWith("local")) {
    const param = text.replace("local", "").trim().replaceAll(" ", "~");
    const slug = param.length > 0 ? `/${param}` : "";
    jump("https://localhost:8888" + slug); // Go to the local dev server, passing any params as a piece.
    makeFlash($);
    return true;
  } else if (text.split(" ")[0] === "of") {
    // Ordfish shortcuts.
    jump(`ordfish~${text.split(" ").slice(1).join("~")}`);
    return true;
  } else if (ordfish[text] || text.split(" ") === "of") {
    jump(`ordfish~${text}`);
    return true;
  } else if (text.startsWith("hiccup")) {
    // Disconnect from socket server, chat, and udp in 5 seconds...
    net.hiccup();
    return true;
  } else {
    console.log("🟢 Attempting a load!");
    // 🟠 Local and remote pieces...

    // Theory: Is `load` actually similar to eval?
    //         (Whereas this is eval/apply at the program level.)

    let body, loaded;
    const trimmed = text.trim();
    if (trimmed.startsWith("(") || trimmed.startsWith(";")) {
      body = { name: "(...)", source: trimmed };
      loaded = await load(body, false, false, true); // Force `devReload` flag.
      //                                                (in case of publish)
    } else {
      body = parse(trimmed);
      loaded = await load(body); // Execute the current command.
    }

    // console.log("Loaded:", loaded);

    if (!loaded) {
      leaving(false);
      if (/*text.indexOf(" ") === -1 &&*/ text !== "goodiepal") {
        system.prompt.input.text = TYPO_REPLY;
        system.prompt.input.replied($); // Set the UI state back to normal.
        loaded = { replied: true };
      }
    } else {
      loaded = { left: true };
    }
    return loaded;
  }
}

// 🎨 Paint
function paint($) {
  // 🅰️ Paint below the prompt || scheme.
  if ($.store["painting"]) {
    $.wipe($.dark ? scheme.dark.background : scheme.light.background);
    $.system.nopaint.present($); // Render the painting.
    pal = $.system.prompt.input.pal;
    scheme.dark.background[3] = 176; // Half semi-opaque palette background.
    scheme.light.background[3] = 190;
  } else {
    $.wipe($.dark ? scheme.dark.background : scheme.light.background);
  }

  $.layer(1); // 🅱️ And above it...

  const { screen, ink, history, net, help } = $;

  if ($.system.prompt.input.canType) {
    if (activeCompletions.length === 0) {
      // History
      let historyTexts =
        history.length === 0 ? [] : history.map((h) => h.replaceAll("~", " "));

      historyTexts.reverse().forEach((t, i) => {
        const ii = i + 1;
        ink(140, 90, 235, 80 / ii).write(t, { x: 6, y: 18 + 12 * i });
      });
    }

    // Autocompetions
    if (activeCompletions.length > 0)
      activeCompletions.forEach((completion, i) => {
        $.system.prompt.input.text;
        const diff =
          completion.length -
          (completion.length - $.system.prompt.input.text.length);
        let text = completion;
        if (i === 0) {
          text = completion.replace(
            $.system.prompt.input.text,
            " ".repeat(diff),
          );
        }
        ink($.dark ? "white" : "red", 32).write(text, {
          x: 6,
          y: 6 + i * 12,
        });
      });
  }

  if (progressBar >= 0) {
    ink(255, 180, 0, 120).box(0, 0, screen.width, screen.height, "inline");
    ink(0).box(1, 1, screen.width - 2, 1);
    if (progressBar > 0) {
      ink(scheme.dark.block).box(1, 1, (screen.width - 2) * progressBar, 1);
    }
  }

  if (
    (!login?.btn.disabled && !profile) ||
    (!login && !profile?.btn.disabled)
  ) {
    // Paint current status color.
    // if (!$.system.prompt.input.canType) {
    starfield.paint($, {
      alpha: $.dark ? 0.3 : 0.9,
      color: $.hud.currentStatusColor() || [255, 0, 200],
    });

    // 📊 Stats / Analytics

    if ($.chat.messages.length > 0) {
      const msg = $.chat.messages[$.chat.messages.length - 1];
      const fullText = msg.from + ": " + msg.text;
      ink("teal", 128).write(
        fullText,
        {
          center: "x",
          y: screen.height / 2 + 38,
        },
        undefined,
        screen.width - 8,
      );
    }

    if (handles && screen.height > 200)
      ink(pal.handleColor).write(
        `${handles} HANDLES SET`,
        {
          center: "x",
          y: screen.height / 2 + screen.height / 3.25 - 11,
        },
        [255, 50, 200, 24],
        screen.width - 18,
      );
  }

  // Paint UI Buttons
  //if (!net.iframe) {

  //what we actually want for login and signup is pal.signup / pal.login
  //currently gives an error, I think because of paint (works fine with ink)
  if (!net.sandboxed) {
    if (!login?.btn.disabled) {
      login?.paint($, $.dark ? scheme.dark.login : scheme.light.login);
    }
  }

  if (!net.iframe) {
    if (!signup?.btn.disabled) {
      signup?.paint($, $.dark ? scheme.dark.signup : scheme.light.signup);
    }
  }
  if (!profile?.btn.disabled) {
    if (
      profileAction === "resend-verification" ||
      profileAction === "set-handle"
    ) {
      const verified = profileAction === "set-handle";
      const message = verified
        ? "Email verified!"
        : "Awaiting email verification" + ellipsisTicker.text(help.repeat);

      ink("black", 128).write(message, {
        center: "x",
        x: screen.width / 2 + 1,
        y: screen.height / 2 - 48 + 1,
      });

      ink(verified ? "lime" : "yellow").write(
        message,
        { center: "x", y: screen.height / 2 - 48 },
        undefined,
        screen.width - 16,
      );
    }
    profile?.paint($);
  }
  //}

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
  ellipsisTicker?.sim();
  progressTrick?.step();
  if (!login?.btn.disabled || !profile?.btn.disabled) {
    starfield.sim($);
    $.needsPaint();
  }

  if ($.store["handle:received"]) {
    profile = new $.ui.TextButton($.handle(), {
      center: "xy",
      screen: $.screen,
    });
    if (login) login.btn.disabled = true;
    if (signup) signup.btn.disabled = true;
    delete $.store["handle:received"];
    profileAction = "profile";
    $.needsPaint();
  }

  if ($.store["handle:failed"]) {
    delete $.store["handle:failed"];
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
  user,
  store,
  sound: { play, synth },
  send,
  handle,
  glaze,
  canShare,
  notice,
  ui,
}) {
  // Checks to clear prefilled 'email user@email.com' message
  // on signup.
  if (
    e.is("keyboard:close") &&
    resendVerificationText &&
    system.prompt.input.text === resendVerificationText
  ) {
    system.prompt.input.blank(); // Clear the prompt.
    resendVerificationText = undefined;
  }

  // Light and dark mode glaze shift.
  if (e.is("dark-mode")) glaze({ on: true });
  if (e.is("light-mode")) glaze({ on: false });

  // 👱 Handle Callback
  if (e.is("handle:request:completed")) {
    console.log("Handle request completed:", profile);
    profile.btn.disabled = false;
  }

  //if (e.is("session:updated")) {
  //console.log("SESSION UPDATED!", user);
  //   if (user === null) {
  //     login = new ui.TextButton("Log in", { center: "xy", screen });
  //     signup = new ui.TextButton("I'm new", { center: "xy", screen });
  //     positionWelcomeButtons(screen, net.iframe);
  //   }
  // }

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

  if (!net.sandboxed) {
    login?.btn.act(e, {
      down: () => downSound(),
      push: () => {
        pushSound();
        net.login();
        // if (net.iframe) jump("login-wait");
      },
    });
  }

  if (!net.iframe) {
    signup?.btn.act(e, {
      down: () => downSound(),
      push: () => {
        pushSound();
        net.signup();
      },
    });
  }

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
    //system.prompt.input.backdropTouchOff === false &&
    (e.is("touch") || e.is("lift")) &&
    ((login?.btn.disabled === false && login?.btn.box.contains(e)) ||
      (signup?.btn.disabled === false && signup?.btn.box.contains(e)) ||
      (profile?.btn.disabled === false &&
        profile?.btn.box.contains(e) &&
        profileAction === "profile"))
  ) {
    send({ type: "keyboard:lock" });
    system.prompt.input.backdropTouchOff = true;
  }

  if (e.is("lift") || e.is("touch")) needsPaint(); // Get button changes to
  //                                           ^      paint on-demand.
  // 🚨 Idea: It would be nice to pass     ----^
  //          what needs to be painted
  //          so the knowledge can be
  //          used in the `paint` function
  //          to allow for manual optimizations. 23.06.20.00.30

  profile?.btn.act(e, {
    down: () => {
      downSound();
      if (profileAction !== "profile") {
        //send({ type: "keyboard:enabled" }); // Enable keyboard flag.
        // send({ type: "keyboard:unlock" });
      }
    },
    push: () => {
      pushSound();
      if (profileAction === "resend-verification") {
        // notice("RESEND EMAIL?", ["yellow", "blue"]);
        const text = "email " + user.email;
        resendVerificationText = text;
        system.prompt.input.text = text;
        system.prompt.input.snap();
        system.prompt.input.runnable = true;
        firstActivation = false;
        send({ type: "keyboard:text:replace", content: { text } });
        // send({ type: "keyboard:unlock" });
        // send({ type: "keyboard:open" });
      } else if (profileAction === "profile") {
        jump(handle() || "profile");
      } else if (profileAction === "set-handle") {
        notice("ENTER HANDLE", ["yellow", "blue"]);
        const text = "handle ";
        system.prompt.input.text = text;
        system.prompt.input.snap();
        system.prompt.input.runnable = true;
        firstActivation = false;
        send({ type: "keyboard:text:replace", content: { text } });
        // send({ type: "keyboard:unlock" });
        // send({ type: "keyboard:open" });
      }
    },
    cancel: () => {
      if (profileAction !== "profile") {
        // send({ type: "keyboard:disabled" }); // Disable keyboard flag.
        send({ type: "keyboard:lock" });
        system.prompt.input.backdropTouchOff = true;
      }
    },
  });

  // 🖥️ Screen
  if (e.is("reframed")) positionWelcomeButtons(screen, net.iframe);

  // ⌨️ Keyboard (Skip startup sound if a key is pressed or text is pasted.)
  if (e.is("keyboard:open") && firstActivation && e.method !== "pointer") {
    firstActivation = false;
    console.log("⌨️ First keyboard activation completed!");
  }

  // if (e.is("pasted:text")) firstActivation = false;

  // Whenever the text input is edited.
  if (
    e.is("prompt:text:replace") &&
    !firstActivation &&
    system.prompt.input.canType
  ) {
    if (!e.mute) {
      play(keyboardSfx, { volume: 0.2 + (num.randInt(100) / 100) * 0.4 });
    }

    // Compute autocompletions...
    activeCompletions.length = 0;
    if (e.text.length > 0) {
      keys(autocompletions).forEach((key) => {
        if (key.startsWith(e.text)) activeCompletions.push(key);
      });
      // oif (activeCompletions.length > 0)
      //  console.log("✍️ Completions:", activeCompletions);
    }

    if (
      (e.text === "dl" || e.text === "download") &&
      canShare &&
      store["painting"]
    ) {
      downloadPainting(api, defaultDownloadScale, true); // Trigger early download response, before the user enters.
    }
  }

  if (e.is("keyboard:down:tab") && e.key === "Tab" && activeCompletions[0]) {
    console.log("Tab completing:", activeCompletions[0]);
    // TODO: The text input object needs to be updated here also...
    system.prompt.input.text = activeCompletions[0];
    system.prompt.input.snap();
    send({
      type: "keyboard:text:replace",
      content: { text: system.prompt.input.text },
    });
  }

  function autocompleteChar() {
    const text = system.prompt.input.text;
    const completion = activeCompletions[0];
    if (completion && text !== completion) {
      const cursorX = system.prompt.input.prompt.cursor.x;
      system.prompt.input.text = completion.slice(0, cursorX + 1);
      system.prompt.input.snap();
      send({
        type: "keyboard:text:replace",
        content: { text: system.prompt.input.text },
      });
    }
  }

  if (e.is("keyboard:down:arrowright")) {
    if (system.prompt.input.prompt.textPos() === undefined) autocompleteChar();
  }

  if (e.is("textinput:shift-right:empty")) autocompleteChar();

  // if (e.is("keyboard:down") && e.key !== "Enter") {
  // console.log("down key...");
  // play(keyboardSfx, { volume: 0.2 + (num.randInt(100) / 100) * 0.4 });
  // }

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
    makeFlash($ /*, $.params[0]*/); // Always sets firstActivation flag to false.
  }
  // console.log(state, firstCommandSent)
  // if (state === false && firstCommandSent) return;
  if (login) login.btn.disabled = state;
  if (signup) signup.btn.disabled = state;
  if (profile) profile.btn.disabled = state;
}

// 💬 Receive each response in full.
function reply(text) {
  // firstCommandSent = true;
  // console.log("😀 Replied with:", text || "Halted?");
}

// 📰 Meta
function meta() {
  return {
    title: "prompt · Aesthetic Computer",
    desc: "Type anything to get started.",
  };
}

// 👋 Leave
function leave() {
  motdController?.abort(); // Abort any motd update.
}

export const nohud = true;

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
    text: [255, 100],
    background: [70, 50, 100],
    prompt: [200, 30, 100, 200],
    block: [200, 30, 100],
    highlight: [255, 100, 0],
    guideline: [0, 0, 255, 64],
    login: [[0, 0, 64], 255, 255, [0, 0, 64]],
    signup: [[0, 64, 0], 255, 255, [0, 64, 0]],
    handleColor: [255, 0, 255, 128],
    auto: "white",
    statusColor: "lime",
    focusOutline: "brown",
  },
  light: {
    text: [255, 90, 90],
    background: [255, 255, 0],
    prompt: [255, 128, 128],
    block: [56, 122, 223],
    highlight: [246, 253, 195],
    guideline: [255, 207, 105],
    // login: [255, [0, 0, 64], [0, 0, 64], 255],
    login: [[0, 0, 128], 255, 255, [0, 0, 128]],
    // signup: [255, [0, 64, 0], [0, 64, 0], 255],
    signup: [[0, 128, 0], 255, 255, [0, 128, 0]],
    handleColor: [0, 0, 255, 128],
    auto: "red",
    statusColor: "darkgreen",
    focusOutline: "aqua",
  },
};

// 📚 Library
//   (Useful functions used throughout the piece)

let motdController;

async function makeMotd({ system, needsPaint, handle, user, net, api }) {
  let motd = "aesthetic.computer"; // Fallback motd.
  motdController = new AbortController();
  try {
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
  } catch (err) {
    // console.warn("🙁 System `mood` fetch aborted.");
  }
}

function makeFlash($, clear = true, beep = false) {
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
  if (clear === true) {
    $.system.prompt.input.blank(); // Clear the prompt.
  } else if (typeof clear === "string") {
    $.system.prompt.input.text = clear;
    $.system.prompt.input.snap();
    $.send({
      type: "keyboard:text:replace",
      content: { text: $.system.prompt.input.text },
    });
  }

  if (beep) $.beep();
}

function positionWelcomeButtons(screen, iframe) {
  if (login && signup) {
    login.reposition({ center: "xy", screen });
    signup.reposition({ center: "xy", screen });
    // Nudge signup and login by half their width.
    if (iframe) return; // But not if embedded in an iframe (where only login appears)
    let offset = 5; // With a fixed pixel offset.
    signup.btn.box.x += signup.btn.box.w / 2 + offset;
    login.btn.box.x -= login.btn.box.w / 2 + offset;
    if (screen.width % 2 !== 0) login.btn.box.x += 1; // Nudge odd display width.
  }

  if (profile) profile.reposition({ center: "xy", screen });
}

function downloadPainting({ download, num, store }, scale, sharing = false) {
  download(`painting-${num.timestamp()}.png`, store["painting"], {
    scale,
    // Read an integer parameter for scale.
    cropToScreen: !(store["painting:resolution-lock"] === true),
    // Only cut the download off at screen-size if user never
    // set a resolution.
    sharing,
  });
}

async function publishPiece(
  { api, send, jump, handle, upload },
  slug,
  source,
  ext = "mjs",
) {
  progressBar = 0; // Trigger progress bar rendering.
  try {
    const data = await upload("piece-" + slug + "." + ext, source, (p) => {
      console.log("🎁️ Publishing progress:", p);
      progressBar = p;
    });
    console.log("🪄 Code uploaded:", data);
    flashColor = [0, 255, 0];
    const route = handle() ? `${handle()}/${data.slug}` : data.slug;
    makeFlash(api, route);
    console.log(`\`${route}\` was published!`);
    jump(route);
  } catch (err) {
    console.error("🪄 Code upload failed:", err);
    send({
      type: "alert",
      content: `😥 Piece: \`${slug}\` failed to publish.`,
    });
    flashColor = [255, 0, 0];
    makeFlash(api);
  }
}
