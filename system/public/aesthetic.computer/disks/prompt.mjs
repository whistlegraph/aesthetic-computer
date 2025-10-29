// Prompt, 2023.5.26.21.38.35
//         2021.11.28.03.13 (Created on)
// A language based "access-everything" console with LLM fallback.

/* #region 📚 README
  🎄 Merry Pipeline System
  - Chain pieces together with configurable durations
  - Syntax: `merry piece1 piece2 piece3` (default 5 seconds each)
  - Custom: `merry tone:3 clock:5 wand:2` or `merry 3-tone 5-clock 2-wand`
  - Loop forever: `merryo 0.25-tone` (use `stop` to exit)
  - Stop early: `merry:stop` or `stop`
  - Example: `merry tone:3 clock:5` plays tone for 3s, then clock for 5s, then returns to prompt
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
  - when you suggest a command, always put it in "quotes."
  - if I type "hife" you do not suggest "life" because that is not a command in the data set
  - you do not respond with any additional information

If the user asks to delete their account or enters "delete" or "deactivate", you tell them to enter "delete-erase-and-forget-me" to delete their account.

If the user enters 'goodiepal' please reply: Yes, but people on the Faro islands call me Pruttipal, so enter 'prutti' instead.

The word I'm entering is:`;

const after = ``;
const forgetful = true;

const TYPO_REPLY = `
Use Aesthetic Computer by entering a correct word.\n\nEnter "list" for some available words.\n\nEnter "chat" for help.\n\n - @jeffrey`.trim();

import { Android, MetaBrowser, iOS } from "../lib/platform.mjs";
import { validateHandle } from "../lib/text.mjs";
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { parse } from "../lib/parse.mjs";
import { signed as shop } from "../lib/shop.mjs";
import { ordfish } from "./ordfish.mjs";
import {
  isPromptInKidlispMode,
  isActualKidLisp,
  decodeKidlispFromUrl,
  encodeKidlispForUrl,
  isKidlispSource,
} from "../lib/kidlisp.mjs";
const { abs, max, min } = Math;
const { keys } = Object;

// Error / feedback flash on command entry.
let flash;
let flashShow = false;
let flashColor = [];
let flashPresent = false;

let progressBar = -1; // If not zero, then draw a progress bar.
let progressTrick; // A faux growth period on the progress bar.
let cachedGizmo; // Reference to gizmo for use in act() function
let progressPhase = ""; // Current phase of upload (e.g., "ZIPPING", "UPLOADING IMAGE")
let progressPercentage = 0; // 0-100

let login, // A login button in the center of the display.
  signup, // A Sign-up button.
  profile, // A profile page button.
  profileAction;
let resendVerificationText;
let ellipsisTicker;
let chatTicker; // Ticker instance for chat messages
let chatTickerButton; // Button for chat ticker hover interaction
let contentTicker; // Ticker instance for mixed $kidlisp, #painting, !tape content
let contentTickerButton; // Button for content ticker hover interaction
let contentItems = []; // Store fetched content: {type: 'kidlisp'|'painting'|'tape', code: string}
let ruler = false; // Paint a line down the center of the display.
//                   (for measuring the login / signup centering).
// let firstCommandSent = false; // 🏳️
let firstActivation = true; // 🏳️ Used to trigger a startup 🔊🎆

let startupSfx, keyboardSfx;

// 📚 Book button
let bookButton;
let bookImage;
let bookImageScaled; // Cached scaled version of the book image
let bookRotation = 0; // Rotation angle for oscillation

// 🎆 Corner particles
let cornerParticles = [];

let tapePromiseResolve, tapePromiseReject;

let handles; // Keep track of total handles set.
let motd; // Store the mood of the day text
let motdFrame = 0; // Animation frame counter for MOTD effects
let previousKidlispMode = false; // Track previous KidLisp mode state for sound triggers

let defaultDownloadScale = 6;

import * as starfield from "./starfield.mjs";

let server;

let darkModeOn;
let pal;

let autocompletions = {};
const activeCompletions = [];
let fetchingUser = false,
  fetchUserAPI;

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
  vscode,
}) {
  cachedGizmo = gizmo; // Cache gizmo for use in act() function
  if (dark) glaze({ on: true });
  // if (vscode) console.log("🟣 Running `prompt` in the VSCode extension.");

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

  // Load book cover image
  // 📚 Second Product (Current)
  net
    .preload("https://shop.aesthetic.computer/cdn/shop/files/IMG-1176.png?v=1761673860&width=1646")
    .then((img) => {
      bookImage = img;
      
      // Pre-scale the image for caching
      const bookScale = 0.05; // Bigger for better visibility
      const scaledW = Math.floor(img.img.width * bookScale);
      const scaledH = Math.floor(img.img.height * bookScale);
      
      // Create a scaled bitmap using painting API from api object
      bookImageScaled = api.painting(scaledW, scaledH, (p) => {
        p.paste(img.img, 0, 0, bookScale);
      });
    })
    .catch((err) => console.warn("📚 Could not load book image:", err));
    
  // 📚 First Product (Deprecated - SOLD)
  // net
  //   .preload("https://shop.aesthetic.computer/cdn/shop/files/IMG-1098.png?v=1760737988&width=600")
  //   .then((img) => {
  //     bookImage = img;
  //     const bookScale = 0.1;
  //     const scaledW = Math.floor(img.img.width * bookScale);
  //     const scaledH = Math.floor(img.img.height * bookScale);
  //     bookImageScaled = api.painting(scaledW, scaledH, (p) => {
  //       p.paste(img.img, 0, 0, bookScale);
  //     });
  //   })
  //   .catch((err) => console.warn("📚 Could not load book image:", err));

  // Create login & signup buttons.
  if (!user) {
    login = new ui.TextButton("Log in", { center: "xy", screen });
    login.stickyScrubbing = true; // Prevent drag-between-button behavior
    signup = new ui.TextButton("I'm new", { center: "xy", screen });
    signup.stickyScrubbing = true; // Prevent drag-between-button behavior
    positionWelcomeButtons(screen, net.iframe);
  }

  if (user) {
    // console.log("User:", user);
    const hand = handle();
    const btnPos = { center: "xy", screen };
    if (hand) {
      profileAction = "profile";
      profile = new ui.TextButton(hand, btnPos);
      profile.stickyScrubbing = true; // Prevent drag-between-button behavior
    } else if (!user.email_verified) {
      profile = new ui.TextButton("Resend email", btnPos);
      profile.stickyScrubbing = true; // Prevent drag-between-button behavior
      profileAction = "resend-verification";
      ellipsisTicker = new gizmo.EllipsisTicker();
      fetchUserAPI = api;
      fetchUser();
    } else if (user.email_verified) {
      profileAction = "set-handle";
      profile = new ui.TextButton("Create handle", btnPos);
      profile.stickyScrubbing = true; // Prevent drag-between-button behavior
    }
  }

  // Only if prompt is set to recall conversations.
  if (
    !system.prompt.convo.messages ||
    system.prompt.convo.messages?.length === 0
  ) {
    // Check if we'll be activating the prompt later to avoid showing MOTD when focused
    const willActivate =
      (pieceCount > 0 && !store["prompt:splash"] && !net.devReload) ||
      (vscode && pieceCount === 0);

    // Fetch the MOTD to display above login/signup buttons
    if (!params[0]) makeMotd({ ...api, notice });
    
    // Fetch all content (kidlisp, painting, tape) for content ticker
    if (!params[0]) {
      fetchContentItems(api);
    }

    if (pieceCount === 0 || store["prompt:splash"] === true) {
      // Initial boot setup
    } else {
      firstActivation = false; // Assume we've activated if returning from
      //                          elsewhere.
    }
    system.prompt.input.showButton(api, {
      nocopy: true,
      nopaste: pieceCount === 0,
    });
  }
  // Handle params - content is already decoded by parse.mjs
  if (params[0]) {
    const text = params.join(" "); // Already decoded, just join if multiple params

    // Set the text and user text first before activating
    system.prompt.input.text = text;
    system.prompt.input.runnable = true;
    system.prompt.input.addUserText(text);
    system.prompt.input.snap();
    send({ type: "keyboard:text:replace", content: { text } });
    
    activated({ ...api, params }, true);
    system.prompt.input.canType = true;
    send({ type: "keyboard:unlock" });
    send({ type: "keyboard:open" }); // Necessary for desktop.
    
    // Ensure text and cursor position persist after any system initialization
    setTimeout(() => {
      if (system.prompt.input.text !== text) {
        system.prompt.input.text = text;
        send({ type: "keyboard:text:replace", content: { text } });
      }
      // Always ensure cursor is at the end
      system.prompt.input.snap();
    }, 100);
  } else {
    system.prompt.input.text = "";
  }

  // Activate and reset input text if returning to the prompt from elsewhere.
  if (
    (pieceCount > 0 && !store["prompt:splash"] && !net.devReload) ||
    (vscode && pieceCount === 0)
  ) {
    if (vscode && pieceCount === 0) firstActivation = false;
    // system.prompt.input.enter.btn.disabled = true; // Disable button.
    // system.prompt.input.inputStarted = true;
    // 🍫 Create a pleasurable blinking cursor delay.
    // system.prompt.input.showBlink = false;
    // setTimeout(() => (system.prompt.input.showBlink = true), 100);

    // Clear any latent text before activating to prevent MOTD showing when focused
    // but only if we don't have params (which means we're not coming from backspace navigation)
    if (!params[0]) {
      system.prompt.input.text = "";
    }

    activated({ ...api, params }, true);
    system.prompt.input.canType = true;
    send({ type: "keyboard:unlock" });
    send({ type: "keyboard:open" }); // Necessary for desktop.
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
    ui,
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

  const stopMerryPipeline = ({
    reason = "manual",
    jumpAfter = true,
    jumpTarget = "prompt",
    cutTape = true,
  } = {}) => {
    const merryState = system.merry;
    if (!merryState) {
      return { stopped: false, wasTaping: false };
    }

    console.log(`🎄 Merry stop requested (${reason})`);
    merryState.running = false;

    if (merryState.paintInterval != null) {
      clearInterval(merryState.paintInterval);
      merryState.paintInterval = null;
    }
    const wasTaping = merryState.isTaping;

  delete system.merry;

    if (wasTaping && cutTape) {
      rec.cut(() => {
        try {
          rec.present();
        } catch (err) {
          console.warn("🎄📼 Unable to present recording after merry stop", err);
        }

        if (jumpAfter && jumpTarget) {
          rec.videoOnLeave = false;
          jump(jumpTarget);
        }
      });
    } else if (jumpAfter && jumpTarget) {
      jump(jumpTarget);
    }

    return { stopped: true, wasTaping };
  };

  system.stopMerryPipeline = stopMerryPipeline;

  const activateMerry = (
    pieceParams,
    { markAsTaping = false, flashOnSuccess = true, loop = false, originalCommand = "" } = {}
  ) => {
    if (!pieceParams || pieceParams.length === 0) {
      flashColor = [255, 0, 0];
      makeFlash($);
      notice("MERRY NEEDS PIECES", ["yellow", "red"]);
      return false;
    }

    stopMerryPipeline({ reason: "restart", jumpAfter: false, cutTape: false });

    const defaultDuration = 5;
    const pipeline = [];

    pieceParams.forEach((raw) => {
      const param = raw.trim();
      if (!param) return;

      let piece = param;
      let duration = defaultDuration;

      const colonParts = param.split(":");
      if (colonParts.length > 1) {
        const parsed = parseFloat(colonParts[1]);
        if (!isNaN(parsed) && parsed > 0) {
          duration = parsed;
        }
        piece = colonParts[0];
      } else {
        const hyphenIndex = param.indexOf("-");
        if (hyphenIndex > 0) {
          const prefix = param.slice(0, hyphenIndex);
          const remainder = param.slice(hyphenIndex + 1);
          const parsed = parseFloat(prefix);
          if (!isNaN(parsed) && parsed > 0 && remainder) {
            duration = parsed;
            piece = remainder;
          }
        }
      }

      if (piece && !isNaN(duration) && duration > 0) {
        pipeline.push({ piece, duration });
      }
    });

    if (pipeline.length === 0) {
      flashColor = [255, 0, 0];
      makeFlash($);
      notice("MERRY NEEDS PIECES", ["yellow", "red"]);
      return false;
    }

    const totalDuration = pipeline.reduce((sum, p) => sum + p.duration, 0);

    system.merry = {
      pipeline,
      currentIndex: 0,
      running: true,
      totalDuration,
      elapsedTime: 0,
      progress: 0,
      pieceProgress: 0,
      startTime: null,
      currentPieceStart: null,
      isTaping: markAsTaping,
      paintInterval: null,
      loop,
      cycleCount: 0,
      originalCommand, // Store for backspace editing
    };

    console.log("🎄 Merry pipeline:", pipeline, `total: ${totalDuration}s`);

    const merryToneSequence = [392, 494, 523, 587, 659, 784, 880];

    const startMerryPaintTicker = () => {
      const merryState = system.merry;
      if (!merryState || !merryState.running) {
        return;
      }

      // Clear any existing ticker to avoid duplicates
      if (merryState.paintInterval != null) {
        clearInterval(merryState.paintInterval);
        merryState.paintInterval = null;
      }

      const tick = () => {
        const active = system.merry;
        if (!active || !active.running) {
          if (merryState.paintInterval != null) {
            clearInterval(merryState.paintInterval);
            merryState.paintInterval = null;
          }
          return;
        }
        needsPaint();
      };

      // Trigger an immediate paint so progress appears without delay
      needsPaint();

      const intervalHandle = setInterval(tick, 1000 / 30); // ~33ms cadence
      merryState.paintInterval = intervalHandle;
    };

    const startMerryPiece = (index) => {
      if (!system.merry || !system.merry.running) {
        return;
      }

      if (index >= pipeline.length) {
        const merryState = system.merry;
        if (merryState?.loop && merryState.running) {
          merryState.cycleCount = (merryState.cycleCount || 0) + 1;
          merryState.elapsedTime = 0;
          merryState.progress = 0;
          merryState.pieceProgress = 0;
          merryState.currentPieceStart = null;
          merryState.startTime = Date.now();
          needsPaint();
          startMerryPiece(0);
          return;
        }

        console.log("🎄 Merry pipeline complete!");
        const wasTaping = merryState?.isTaping;
        stopMerryPipeline({
          reason: "complete",
          jumpAfter: true,
          jumpTarget: wasTaping ? "video" : "prompt",
          cutTape: Boolean(wasTaping),
        });
        return;
      }

      const { piece, duration } = pipeline[index];
      console.log(`🎄 Merry: Playing ${piece} for ${duration}s (${index + 1}/${pipeline.length})`);

      if (system.merry) {
        system.merry.currentPieceStart = Date.now();
        system.merry.pieceProgress = 0;
        system.merry.currentIndex = index;
        if (index === 0) {
          system.merry.startTime = Date.now();
        }
      }

      startMerryPaintTicker();

      // � Trigger a visual flash on the progress bar when transitioning (instead of sound)
      if (system.merry) {
        system.merry.transitionFlash = {
          active: true,
          startTime: Date.now(),
          duration: 150, // Flash duration in ms
        };
      }

      setTimeout(() => {
        if (system.merry && system.merry.running) {
          system.merry.elapsedTime += duration;
          startMerryPiece(index + 1);
        }
      }, duration * 1000);

      jump(piece);
    };

    startMerryPiece(0);

    if (flashOnSuccess) {
      flashColor = [0, 255, 0];
      makeFlash($);
    }

    return true;
  };
  activeCompletions.length = 0; // Reset activeCompletions on every halt.
  motdController?.abort(); // Abort any motd update.

  // Roughly parse out the text (could also do a full `parse` here.)
  const tokens = text.split(" ");
  const slug = tokens[0]; // Note: Includes colon params.
  const params = tokens.slice(1);
  const input = $.system.prompt.input; // Reference to the TextInput.

  const openExternalFromIframe = (url) => {
    if (!net.iframe) return false;
    send({ type: "post-to-parent", content: { type: "openExternal", url } });
    return true;
  };

  const siteBase = `https://${debug ? "localhost:8888" : "aesthetic.computer"}`;

  const toAbsoluteSiteUrl = (pathOrUrl) => {
    if (/^https?:\/\//.test(pathOrUrl)) return pathOrUrl;
    const normalized = pathOrUrl.startsWith("/") ? pathOrUrl : `/${pathOrUrl}`;
    return `${siteBase}${normalized}`;
  };

  // 🕸️ Custom URL routing.
  if (slug.startsWith("/")) {
    jump(`https://${debug ? "localhost:8888" : "aesthetic.computer"}${slug}`);
    return true;
  } else if (slug === "shop") {
    console.log(slug);

    const openShopPath = (path) => {
      if (openExternalFromIframe(toAbsoluteSiteUrl(path))) return;
      jump(path);
    };

    if (params.length > 0) {
      if (shop.indexOf(params[0]) > -1) {
        openShopPath("/" + params[0]);
      } else {
        openShopPath("/shop/" + params.join("/"));
      }
    } else {
      openShopPath("/shop");
    }
    return true;
  } else if (slug.startsWith("shop")) {
    const target = params[0] ? "/" + params[0] : "/shop";
    if (openExternalFromIframe(toAbsoluteSiteUrl(target))) return true;
    jump(target);
    return true;
  } else if (shop.indexOf(slug) > -1) {
    if (openExternalFromIframe(toAbsoluteSiteUrl("/" + slug))) return true; // Matches a product so jump to a new page / redirect.
    jump("/" + slug); // Matches a product so jump to a new page / redirect.
    return true;
  } else if (slug === "at") {
    // Jump to ATProto user pages landing
    jump(`https://at.aesthetic.computer`);
    return true;
  } else if (slug === "merry" || slug === "merryo") {
    const loop = slug === "merryo";
    console.log(`🎄 ${slug.toUpperCase()} command received with params:`, params);
    activateMerry(params, {
      markAsTaping: false,
      flashOnSuccess: true,
      loop,
      originalCommand: text, // Store the full original text (already has proper format)
    });
    return true;
  } else if (slug.startsWith("!") && slug.length > 1) {
    console.log("📼 Tape code detected:", slug, "params:", params);
    // Route to video piece to handle tape playback
    jump("video " + text);
    return true;
  } else if (
    slug === "tape" ||
    slug === "tape:add" ||
    slug === "tape:tt" ||
    slug === "tape:nomic" ||
    slug === "tape:mic" ||
    slug === "tape:neat" ||
    slug === "tapem"
  ) {
    const playbackParam = params[0];

    // 📼 Check if this is a playback command (e.g., "tape !JyK")
    if (playbackParam && playbackParam.startsWith('!')) {
      console.log("📼 Tape playback mode detected, routing to video piece");
      jump("video " + params.join(' '));
      return true;
    }
    
    // 📼 Start taping (recording mode).
    // Note: Right now, tapes get saved on refresh but can't be concatenated to,
    // and they start over when using `tape`.
    // This could eventually be replaced by a system that makes a new
    // video for every clip and then renders or stitches them together
    // in the end, where `video` can evolve into more of a clip editor.
    // Each of these clips can be stored in indexedDB more easily and played
    // back or be rearranged.
    // 23.09.16.18.01
    if (slug !== "tape:add") rec.slate(); // Start a recording over.
    const defaultDuration = 7;
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
    } else if (slug === "tape:nomic" || slug === "tape:neat") {
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
      let frameMode = false;
  let isTapingMerry = false; // Flag to track if we're recording a merry pipeline
  let jumpTo;
  let merryPieceParams = null;
      
      // Check if the first parameter ends with 'f' for frame-based recording
      if (params[0] && typeof params[0] === 'string' && params[0].toLowerCase().endsWith('f')) {
        frameMode = true;
        duration = parseFloat(params[0].slice(0, -1)); // Remove the 'f' suffix
        console.log(`🎬 Frame-based recording requested: ${duration} frames`);
      }
      
      // 🎄 Check if we're taping a merry pipeline anywhere in the params: "tape merry tone:3 clock:5"
      const merryTokenIndex = params.findIndex(
        (param) => param === "merry" || param === "merryo"
      );
      if (merryTokenIndex !== -1) {
        isTapingMerry = true;
        console.log("🎄📼 Taping a merry pipeline detected!");
        
        const merryToken = params[merryTokenIndex];
        const loopRequested = merryToken === "merryo";

        if (loopRequested) {
          params[merryTokenIndex] = "merry";
          flashColor = [255, 165, 0];
          makeFlash($);
          notice("MERRYO DISABLED WHILE TAPING", ["yellow", "red"]);
        }

        merryPieceParams = params.slice(merryTokenIndex + 1);
        if (!merryPieceParams.length) {
          flashColor = [255, 0, 0];
          makeFlash($);
          notice("MERRY NEEDS PIECES", ["yellow", "red"]);
          return true;
        }

        const defaultMerryDuration = 5;
        let totalMerryDuration = 0;
        merryPieceParams.forEach((param) => {
          const parts = param.split(":");
          const pieceDuration = parts[1] ? parseFloat(parts[1]) : defaultMerryDuration;
          if (!isNaN(pieceDuration) && pieceDuration > 0) {
            totalMerryDuration += pieceDuration;
          }
        });

        if (totalMerryDuration <= 0) {
          totalMerryDuration = defaultMerryDuration * merryPieceParams.length;
        }

        console.log(`🎄📼 Calculated merry total duration: ${totalMerryDuration}s`);
        duration = totalMerryDuration; // Set tape duration to match merry duration

  jumpTo = "merry";
      }

      // Gets picked up on next piece load automatically.
      rec.loadCallback = () => {
        // Capture the KidLisp FPS if available (set by fps function)
        const kidlispFps = (typeof window !== 'undefined' && window.currentKidlispFps) || null;
        console.log(`🎬 Captured KidLisp FPS for recording: ${kidlispFps}`);
        
        // 🎄 If we're taping a merry, mark it in the merry system
        if (isTapingMerry && system.merry) {
          system.merry.isTaping = true;
          console.log("🎄📼 Marked merry pipeline as being taped");
        }
        
        // 😶‍🌫️ Running after the `jump` prevents any flicker and starts
        // the recording at the appropriate time.
        rec.rolling(
          {
            type: "video" + (slug === "tape:tt" || jumpTo === "baktok" ? ":tiktok" : ""),
            pieceName: (jumpTo && jumpTo.startsWith("$")) ? "$code" : (jumpTo || "tape"),
            pieceParams: (() => {
              if (jumpTo === "merry") {
                return (merryPieceParams || []).join("~");
              }

              // Exclude the piece name from params to avoid duplication in filename
              const startIndex = isNaN(duration) ? 0 : 1;
              const relevantParams = params.slice(startIndex);
              if (relevantParams.length > 0 && relevantParams[0] === jumpTo) {
                return relevantParams.slice(1).join("~");
              }
              return relevantParams.join("~");
            })(),
            originalCommand: text,
            intendedDuration: isNaN(duration) ? null : duration,
            frameMode: frameMode,
            frameCount: frameMode ? (isNaN(duration) ? 8 : duration) : null,
            kidlispFps: kidlispFps, // Pass the KidLisp framerate
            cleanMode: slug === "tape:neat", // Enable clean mode (no overlays, no progress bar)
            // showTezosStamp: true, // Enable Tezos stamp by default for GIF recordings (DISABLED)
            showTezosStamp: false, // Tezos stamp disabled - set to true to re-enable
            mystery: false
          },
          (time) => {
            if (frameMode) {
              rec.tapeTimerSet(duration || 8, time, true); // Default to 8 frames if no duration specified
            } else {
              rec.tapeTimerSet(duration || defaultDuration, time, false);
            }
          },
        ); // Start recording immediately.
      };

      if (isTapingMerry && merryPieceParams) {
        const merryStarted = activateMerry(merryPieceParams, {
          markAsTaping: true,
          flashOnSuccess: false,
          loop: false,
        });
        if (!merryStarted) {
          return true;
        }
        rec.videoOnLeave = true;
      } else if ((isNaN(duration) || duration === 0) && params[0]?.length > 0) {
        // Handle cases like "tape $code" or "tape f" or "tape 0f"
        if (frameMode && (isNaN(duration) || duration === 0)) {
          duration = 8; // Default to 8 frames for "tape f" or "tape 0f"
        } else if (!frameMode) {
          duration = defaultDuration; // Default to 7 seconds for "tape $code"
        }
        
        if (!frameMode || !params[0].toLowerCase().endsWith('f')) {
          // Only jump to piece if it's not just a frame count
          // Reconstruct the original kidlisp content without adding tildes
          const originalContent = text.slice(text.indexOf(params[0])); // Get everything after "tape "
          jumpTo = params[0];
          jump(originalContent);
          rec.videoOnLeave = true;
        } else {
          // For "tape f" or "tape 0f", just record the prompt
          jump("prompt");
        }
      } else if (!isTapingMerry) {
        // Find the first non-empty param after duration (params[0])
        const pieceParam = params.slice(1).find(p => p && p.length > 0);
        if (pieceParam) {
          jumpTo = pieceParam;
          // Reconstruct the original content for kidlisp preservation
          const originalContent = text.slice(text.indexOf(pieceParam)); // Get everything after duration
          jump(originalContent);
        } else {
          jump("prompt");
        }
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
  } else if (slug === "merry:stop" || slug === "stop") {
    // 🎄 Stop the merry pipeline early
    if (system.merry && system.merry.running) {
      console.log("🎄 Merry pipeline stopped!");
      const wasTaping = system.merry.isTaping;
      stopMerryPipeline({
        reason: "manual",
        jumpAfter: true,
        jumpTarget: wasTaping ? "video" : "prompt",
        cutTape: wasTaping,
      });
      flashColor = [0, 255, 0];
    } else {
      flashColor = [255, 0, 0];
      notice("NO MERRY RUNNING", ["yellow", "red"]);
    }
    makeFlash($);
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
    send({ type: "notifications:web", content: { enable: false } });
    flashColor = [0, 0, 255];
    makeFlash($);
    return true;
  } else if (slug === "notifs") {
    send({
      type: "ios:send",
      content: { type: "notifications", body: true },
    });
    send({ type: "notifications:web", content: { enable: true } });
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
        progressPhase = "PREPARING";
        progressPercentage = 0;
        progressTrick = new gizmo.Hourglass(24, {
          completed: () => (progressBar += min(0.5, progressBar + 0.1)),
          autoFlip: true,
        });
      }

      progressPhase = "ZIPPING RECORDING";
      const zipped = await zip({ destination, painting: { record } }, (p) => {
        console.log("🤐 Zip progress:", p);
        progressBar = p * 0.3; // Zip is 0-30% of total progress
        progressPercentage = Math.floor(p * 30);
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
      
      try {
        progressPhase = "FINISHING...";
        console.log(`📞 Calling upload with recordingSlug: ${recordingSlug}`);
        const data = await upload(filename, store["painting"], (p) => {
          console.log("🖌️ Painting progress:", p);
          if (p < 1.0) {
            // During S3 upload: 30-80%
            progressBar = 0.3 + (p * 0.5);
            progressPercentage = Math.floor(30 + (p * 50));
          } else {
            // S3 complete, database processing: show indeterminate state
            progressPhase = "PROCESSING...";
            progressBar = -2; // Special value for pulsing animation
            progressPercentage = -1; // Hide percentage
          }
          needsPaint(); // Update display during upload
        }, undefined, recordingSlug); // Pass bucket as undefined (use auth), recordingSlug as 5th param
        console.log("🪄 Painting uploaded:", filename, data);
        
        // The upload function includes the database call, so this happens after everything
        progressPhase = "COMPLETE";
        progressBar = 1.0; // 100%
        progressPercentage = 100;
        needsPaint(); // Force paint to show completion
        
        // Brief delay to show completed state
        await new Promise(resolve => setTimeout(resolve, 300));
        
        progressBar = -1; // Hide progress bar
        progressPhase = "";
        progressPercentage = 0;

        // For anonymous paintings with recordings, the slug is already combined in MongoDB
        // No need to update it separately
        
        // Jump to the painting using its code
        if (data.code) {
          console.log(`🎨 Your painting code: #${data.code}`);
          jump(`painting#${data.code}`);
        } else {
          // Fallback if no code (shouldn't happen but be safe)
          notice(`Saved!`, ["lime"]);
        }

        flashColor = [0, 255, 0];
        makeFlash($);
        return true; // Prevent default - we handled the upload
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0];
        progressBar = -1;
        progressPhase = "";
        progressPercentage = 0;
        makeFlash($);
        return true;
      }
    } else {
      makeFlash($);
      return true;
    }

    /*
    // 🧪 DEBUG: Simple spinner test with dummy await
    console.log("🧪 Testing spinner for 'done' command");
    
    progressBar = 0; // Show spinner
    progressTrick = new gizmo.Hourglass(24, {
      completed: () => (progressBar += min(0.5, progressBar + 0.1)),
      autoFlip: true,
    });
    needsPaint(); // Request paint
    
    // Dummy async work to test spinner visibility
    await new Promise(resolve => setTimeout(resolve, 2000)); // 2 second delay
    
    progressBar = 1.0; // Complete
    needsPaint();
    await new Promise(resolve => setTimeout(resolve, 300)); // Show completion
    
    progressBar = -1; // Hide
    progressTrick = null;
    
    console.log("🧪 Spinner test complete");
    makeFlash($, false);
    return true;
    
    /*
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
      
      // Only initialize progress bar if not already started (by act() pre-emptively)
      if (progressBar < 0) {
        progressBar = 0; // Trigger progress bar rendering.
        progressTrick = new gizmo.Hourglass(24, {
          completed: () => (progressBar += min(0.5, progressBar + 0.1)),
          autoFlip: true,
        });
        needsPaint(); // Force a repaint to show the progress bar
        
        // Yield to the event loop so the progress bar can render
        await new Promise(resolve => setTimeout(resolve, 0));
      }
      
      try {
        console.log(`📞 Calling upload with recordingSlug: ${recordingSlug}`);
        const data = await upload(filename, store["painting"], (p) => {
          console.log("🖌️ Painting progress:", p);
          progressBar = p;
        }, undefined, recordingSlug); // Pass bucket as undefined (use auth), recordingSlug as 5th param
        console.log("🪄 Painting uploaded:", filename, data);
        
        // Show completed progress bar briefly before jumping
        progressBar = 1.0; // Full bar
        needsPaint(); // Force paint
        progressTrick?.stop?.(); // Stop the hourglass animation
        progressTrick = null;
        
        // Brief delay to show completed state
        await new Promise(resolve => setTimeout(resolve, 300));
        
        progressBar = -1; // Hide progress bar

        // For anonymous paintings with recordings, the slug is already combined in MongoDB
        // No need to update it separately
        
        // Jump to the painting using its code
        if (data.code) {
          console.log(`🎨 Your painting code: #${data.code}`);
          jump(`painting#${data.code}`);
        } else {
          // Fallback if no code (shouldn't happen but be safe)
          notice(`Saved!`, ["lime"]);
        }

        flashColor = [0, 255, 0];
        makeFlash($);
        return true; // Prevent default - we handled the upload
      } catch (err) {
        console.error("🪄 Painting upload failed:", err);
        flashColor = [255, 0, 0];
        progressBar = -1;
        makeFlash($);
        return true;
      }
    } else {
      makeFlash($);
      return true;
    }
    */

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
    const moodInput = params.join(" ").trim();
    if (moodInput.length > 0) {
      res = await net.userRequest("POST", "/api/mood", {
        mood: moodInput,
      });
    }

    if (moodInput.length === 0) {
      flashColor = [0, 255, 255]; // Cyan color for flash
      notice("EMPTY", ["cyan", "blue"]);
    } else {
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
      "out:https://marketplace.visualstudio.com/items?itemName=aesthetic-computer.aesthetic-computer-code",
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
        profile = new ui.TextButton("Resend email", { center: "xy", screen });
        profile.stickyScrubbing = true; // Prevent drag-between-button behavior
        profileAction = "resend-verification";
        ellipsisTicker = new gizmo.EllipsisTicker();
        user.email = res.email; // Update the global `user` object for this session.
        user.name = res.email;
        // store["aesthetic:refresh-user"] = true;
        // store.persist("aesthetic:refresh-user");
        fetchUserAPI = api;
        fetchUser();
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

      // Optionally, you could automatically jump to the new location if desired
      // For example, if migrating to a new piece, you might want to jump there directly
      // const newSlug = "new-piece-slug"; // Replace with actual logic to determine new slug
      // jump(newSlug);
    }
    makeFlash($);
    return true;
  } else if (text.startsWith("handle") && !text.startsWith("handles")) {
    // Set user handle.

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
        store["handle"] = res.handle;
        if (!previousHandle) {
          jump("chat");
          beep();
        }
        // store.persist("handle");
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
  } else if (text.startsWith("admin:chat-system:mute")) {
    const userToMute = text.split(" ")[1];
    const res = await net.userRequest("POST", "/handle", {
      handle: userToMute, // could be a handle, sub, or email
      action: "chat-system:mute",
    });
    // console.log("🦻 Mute result:", res);
    notice(
      res.message.toUpperCase(),
      res.status === 200 ? undefined : ["yellow", "red"],
    );
    flashColor = res.status === 200 ? "lime" : "red";
    makeFlash($, true);
    return true;
  } else if (text.startsWith("admin:chat-system:unmute")) {
    const userToMute = text.split(" ")[1];
    const res = await net.userRequest("POST", "/handle", {
      handle: userToMute, // could be a handle, sub, or email
      action: "chat-system:unmute",
    });
    // console.log("🦻 Unmute result:", res);
    notice(
      res.message.toUpperCase(),
      res.status === 200 ? undefined : ["yellow", "red"],
    );
    flashColor = res.status === 200 ? "lime" : "red";
    makeFlash($, true);
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
    store["painting"] = {
      width: system.painting.width,
      height: system.painting.height,
      pixels: system.painting.pixels,
    }; // system.painting;
    store.persist("painting", "local:db"); // Also persist the painting.
    
    // 🎨 Broadcast painting flip/flop to other tabs
    if (typeof $commonApi !== 'undefined' && $commonApi.broadcastPaintingUpdate) {
      $commonApi.broadcastPaintingUpdate("updated", {
        source: "transform",
        operation: slug,
        vertical: vertical
      });
    }
    
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
    store["painting"] = {
      width: system.painting.width,
      height: system.painting.height,
      pixels: system.painting.pixels,
    }; // system.painting;
    store.persist("painting", "local:db"); // Also persist the painting.
    
    // 🎨 Broadcast painting rotation to other tabs
    if (typeof $commonApi !== 'undefined' && $commonApi.broadcastPaintingUpdate) {
      $commonApi.broadcastPaintingUpdate("updated", {
        source: "rotate",
        direction: slug,
        angle: angle
      });
    }
    
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
        api,
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
    if (!isNaN(w) && !isNaN(h)) {
      size = { w, h };
    } else {
      // If no params or invalid params, use full screen dimensions
      size = { w: screen.width, h: screen.height };
    }
    
    // Clear storage and reset state (without creating a painting yet)
    await store.delete("painting", "local:db");
    await store.delete("painting:resolution-lock", "local:db");
    await store.delete("painting:transform", "local:db");
    await store.delete("painting:record", "local:db");
    
    // Also clear from memory to ensure nopaint_adjust doesn't see stale values
    delete store["painting"];
    delete store["painting:resolution-lock"];
    delete store["painting:transform"];
    delete store["painting:record"];
    
    system.nopaint.undo.paintings.length = 0; // Reset undo stack.
    system.painting = null;
    system.nopaint.resetTransform({ system, screen }); // Reset transform.
    
    if (system.nopaint.recording) {
      system.nopaint.recording = false;
      system.nopaint.record.length = 0;
    }
    
    let fullText = slug;
    if (params.length > 0) fullText += "~" + params.join("~");
    
    // Now create the new painting at the specified size
    nopaint_adjust(api, size, fullText);
    
    system.nopaint.startRecord(fullText); // Start recording paintings.
    
    needsPaint();
    
    flashColor = [200, 0, 200];
    makeFlash($);
    return true;
  } else if (text === "painting:reset" || text === "no!") {
    const deleted = await system.nopaint.noBang(api); //{
    //   system,
    //   store,
    //   screen,
    //   needsPaint,
    //   painting,
    // });

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
    let url = debug
      ? "https://" + location.host + "/sotce-net"
      : "https://sotce.net";
    if (net.iframe) url += "?session-sotce=retrieve";
    jump(url);
    flashColor = "pink";
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "github" || text === "gh") {
    const githubUrl = "https://github.com/digitpain/aesthetic.computer";
    if (!openExternalFromIframe(githubUrl)) jump(githubUrl);
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "gmail") {
    jump("https://gmail.com");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "ucla-syllabus") {
    jump(
      "out:https://docs.google.com/document/d/1foiOLdvJeTdPHQKMIWzKBoGcszGPJS5bRcY-Rg3ou6A/edit?usp=sharing",
    );
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "app" || text === "ios") {
    jump("https://apps.apple.com/app/aesthetic-computer/id6450940883");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "pp") {
    const prefix = !net.iframe ? "out:" : "";
    jump(prefix + "/privacy-policy");
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "direct") {
    const prefix = !net.iframe ? "out:" : "";
    jump(
      debug
        ? prefix + "/aesthetic-direct"
        : prefix + "https://aesthetic.direct",
    );
    makeFlash($);
    return true;
  } else if (text.toLowerCase() === "kidlisp") {
    const kidlispUrl = debug
      ? toAbsoluteSiteUrl("/kidlisp-com")
      : "https://kidlisp.com";
    if (!openExternalFromIframe(kidlispUrl)) {
      const prefix = "out:";
      jump(debug ? prefix + "/kidlisp-com" : prefix + kidlispUrl);
    }
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
  } else if (text === "gpt" || text === "chatgpt") {
    jump("https://chat.openai.com");
    makeFlash($);
    return true;
  } else if (text === "help") {
    // Go to the Discord for now if anyone types help.
    makeFlash($);
    jump("chat");
    return true;
  } else if (text === "shillball" || text === "sb") {
    // Shortcuts for Yeche's Shillball game.
    jump("https://galerie-yechelange.baby/ball");
    makeFlash($);
    return true;
  } else if (text === "ssl") {
    jump("/aesthetic.crt"); // Download the local CRT file.
    // TODO: Is there a way to detect this? 24.07.27.02.48
    makeFlash($);
    return true;
  } else if (text === "prod") {
    jump("https://prompt.ac"); // Visit the live site.
    makeFlash($);
    return true;
  } else if (text === "local" || text.startsWith("local")) {
    const param = text.replace("local", "").trim().replaceAll(" ", "~");
    const slug = param.length > 0 ? `/${param}` : "";
    jump("https://local.aesthetic.computer" + slug); // Go to the ngrok dev server, passing any params as a piece.
    // jump("https://localhost:8888" + slug); // Go to the local dev server, passing any params as a piece.
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
  } else if (text.startsWith("#")) {
    // Handle painting short codes like #k3d, #WDv
    const code = text.slice(1).trim();
    if (code.length > 0) {
      console.log(`🎨 Looking up painting by code: #${code}`);

      if (progressTrick) progressTrick = null;
      const promptInput = system?.prompt?.input;
      let spinnerActive = true;
      const spinnerStart = Date.now();

      const enforceSpinnerDelay = (fn, minimum = 160) => {
        const elapsed = Date.now() - spinnerStart;
        const wait = Math.max(0, minimum - elapsed);
        if (wait > 0) {
          setTimeout(fn, wait);
        } else {
          fn();
        }
      };

      if (promptInput) {
        promptInput.lock = true;
        needsPaint();
        setTimeout(() => {
          if (spinnerActive && promptInput && !promptInput.lock) {
            promptInput.lock = true;
            needsPaint();
          }
        }, 0);
      }

      const clearLookupSpinner = (delay = 0) => {
        progressTrick = null;
        progressBar = -1;
        if (promptInput) {
          const unlock = () => {
            spinnerActive = false;
            promptInput.lock = false;
            needsPaint();
          };
          if (delay > 0) {
            setTimeout(unlock, delay);
          } else {
            unlock();
          }
        }
      };

      const cacheKey = `painting-code:${code}`;
      const setLocationHash = (displayCode) => {
        if (typeof window === "undefined") return;
        const normalizedHash = displayCode.startsWith("#")
          ? displayCode
          : `#${displayCode}`;
        try {
          if (window.location.hash !== normalizedHash) {
            window.location.hash = normalizedHash;
          }
          window.acSTARTING_HASH = normalizedHash.slice(1);
        } catch (err) {
          console.warn("⚠️ Unable to update window hash for painting code", normalizedHash, err);
        }
      };

      const routeToPainting = (metadata) => {
        if (!metadata || !metadata.slug || !metadata.handle) {
          console.error(`❌ Incomplete metadata for painting code: #${code}`, metadata);
          notice(`Painting #${code} not found`, ["red"]);
          clearLookupSpinner();
          return;
        }

        const canonicalCode = (metadata.code || code || "").replace(/^#/, "");
        const normalizedHandle = (metadata.handle || "").replace(/^@+/, "");
        const record = {
          slug: metadata.slug,
          handle: normalizedHandle,
          code: canonicalCode,
        };

        // Cache under both the canonical code and the raw code that was requested
        store[cacheKey] = record;
        store[`painting-code:${canonicalCode}`] = record;
        if (normalizedHandle) {
          store[`painting-slug:${normalizedHandle}/${metadata.slug}`] = canonicalCode;
        }
        if (metadata.handle && metadata.handle !== normalizedHandle) {
          store[`painting-slug:${metadata.handle}/${metadata.slug}`] = canonicalCode;
        }

        enforceSpinnerDelay(() => {
          clearLookupSpinner();
          setLocationHash(canonicalCode);
          jump(`painting#${canonicalCode}`);
        });
      };

      const cached = store[cacheKey];

      if (cached) {
        console.log(`✅ Found cached painting: ${cached.handle}/painting/${cached.slug}`);
        routeToPainting(cached);
        return true;
      }

      fetch(`/api/painting-code?code=${code}`)
        .then((response) => {
          if (!response.ok) {
            throw new Error(`HTTP ${response.status}`);
          }
          return response.json();
        })
        .then((data) => {
          if (data?.slug && data?.handle) {
            console.log(`✅ Found painting: ${data.handle}/painting/${data.slug}`);
            routeToPainting({ ...data, code: data.code || code });
          } else {
            console.error(`❌ Painting not found for code: #${code}`);
            notice(`Painting #${code} not found`, ["red"]);
            clearLookupSpinner(0);
          }
        })
        .catch((err) => {
          console.error(`❌ Error looking up painting code #${code}:`, err);
          notice(`Error loading #${code}`, ["red"]);
          clearLookupSpinner(0);
        });

      return true;
    }
  } else {
    // console.log("🟢 Attempting a load!");    // 🟠 Local and remote pieces...

    // Theory: Is `load` actually similar to eval?
    //         (Whereas this is eval/apply at the program level.)
    let body, loaded;
    const trimmed = text.trim();
    // 🍏 Detect if we are in kidlisp mode and pass that flag through to 'load'
    const isKidlisp =
      trimmed.startsWith("(") ||
      trimmed.startsWith(";") ||
      isKidlispSource(trimmed);
    if (isKidlisp) {
      body = { name: trimmed, source: trimmed };
      loaded = await load(body, false, false, true, undefined, true); // Force kidlisp
      //                                        ^^^^ devReload  ^^^^^ forceKidlisp
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
  if (fetchingUser) fetchUserAPI = $.api;

  // Ensure pal is always defined with a fallback
  pal = $.dark ? scheme.dark : scheme.light;

  // 🅰️ Paint below the prompt || scheme.
  if ($.store["painting"]) {
    $.wipe($.dark ? scheme.dark.background : scheme.light.background);
    $.system.nopaint.present($); // Render the painting.
    // Override pal if available from nopaint
    if ($.system.prompt.input.pal) {
      pal = $.system.prompt.input.pal;
    }
    scheme.dark.background[3] = 176; // Half semi-opaque palette background.
    scheme.light.background[3] = 190;
  } else {
    $.wipe($.dark ? scheme.dark.background : scheme.light.background);
  }

  $.layer(1); // 🅱️ And above it...

  const { screen, ink, history, net, help } = $;
  
  // Make prompt text semi-transparent when curtain is up
  const showLoginCurtain = (!login?.btn.disabled && !profile) || (!login && !profile?.btn.disabled);
  if (showLoginCurtain && $.system.prompt.input.canType) {
    // Add opacity to text colors when curtain is up
    const originalDarkText = [...scheme.dark.text];
    const originalLightText = [...scheme.light.text];
    scheme.dark.text = [...originalDarkText.slice(0, 3), 128]; // 50% opacity
    scheme.light.text = [...originalLightText.slice(0, 3), 128];
  }
  
  if ($.system.prompt.input.canType) {
    const currentInputText = $.system.prompt.input.text;

    // 🤖 Check if we're in kidlisp mode (for syntax highlighting)
    const inKidlispMode = isPromptInKidlispMode(currentInputText);
    
    // 🟢 Check if this is ACTUAL KidLisp code (for cursor color, not just nopaint)
    const isActualKidLispCode = isActualKidLisp(currentInputText);

    // Store kidlisp mode state for other parts of the prompt to use
    $.system.prompt.kidlispMode = inKidlispMode;
    $.system.prompt.actualKidlisp = isActualKidLispCode;

    // 🔊 Play sound when entering or leaving KidLisp mode
    if (isActualKidLispCode !== previousKidlispMode) {
      if (isActualKidLispCode) {
        // Entering KidLisp mode - ascending synth sound
        $.sound.synth({
          type: "sine",
          tone: 440, // A4
          duration: 0.08,
          volume: 0.15,
          attack: 0.02,
          decay: 0.03,
          release: 0.03,
        });
        // Add a second harmonic for richness
        setTimeout(() => {
          $.sound.synth({
            type: "sine",
            tone: 660, // E5 (perfect fifth above)
            duration: 0.06,
            volume: 0.1,
            attack: 0.01,
            decay: 0.02,
            release: 0.03,
          });
        }, 40);
      } else {
        // Leaving KidLisp mode - descending synth sound
        $.sound.synth({
          type: "sine",
          tone: 440, // A4
          duration: 0.08,
          volume: 0.15,
          attack: 0.02,
          decay: 0.03,
          release: 0.03,
        });
        // Add a lower harmonic for contrast
        setTimeout(() => {
          $.sound.synth({
            type: "sine",
            tone: 293.66, // D4 (perfect fifth below)
            duration: 0.06,
            volume: 0.1,
            attack: 0.01,
            decay: 0.02,
            release: 0.03,
          });
        }, 40);
      }
      previousKidlispMode = isActualKidLispCode;
    }

    // If activeCompletions is currently empty, but the input text itself
    // is a valid, non-hidden command, it's likely due to a Tab completion
    // that cleared the active suggestions. In this case, we want to treat
    // the current input text as the single active completion to show its description.
    if (
      activeCompletions.length === 0 &&
      currentInputText && // Ensure text is not empty
      autocompletions[currentInputText] &&
      !autocompletions[currentInputText].hidden
    ) {
      activeCompletions.push(currentInputText);
      // This modification allows the description rendering logic below to pick up
      // the tab-completed command. activeCompletions will be naturally reset
      // by other parts of the system (e.g., in halt() or by TextInput updates).
    }

    // Hide history and autocomplete when in KidLisp mode
    if (!inKidlispMode) {
      if (activeCompletions.length === 0) {
        // History
        let historyTexts =
          history.length === 0 ? [] : history.map((h) => h.replaceAll("~", " "));

        historyTexts.reverse().forEach((t, i) => {
          const ii = i + 1;
          ink(140, 90, 235, 80 / ii).write(t, {
            x: 6,
            y: 6 + $.system.prompt.input.typeface.blockHeight * ii,
          });
        });
      }

      // Autocompetions
      if (activeCompletions.length > 0) {
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
            y: 6 + i * $.system.prompt.input.typeface.blockHeight,
          });
        });
      }

      if (activeCompletions.length === 1) {
        // console.log("has completions!");
        ink(
          $.dark ? "white" : "red",
          $.system.prompt.input.text !== activeCompletions[0] ? 64 : 255,
        ).write(
          autocompletions[activeCompletions[0]].desc,
          { center: "xy" },
          null,
          screen.width - 8,
        );
      }
    }
  }

  if (progressBar >= 0 || progressBar === -2) {
    // Draw orange semi-transparent overlay
    ink(255, 180, 0, 120).box(0, 0, screen.width, screen.height, "inline");
    
    // Draw progress bar line at the top
    if (progressBar > 0 && progressBar <= 1) {
      // Normal progress bar (0-100%)
      const barWidth = Math.floor((screen.width - 2) * progressBar);
      ink(255, 180, 0).box(1, 1, barWidth, 1);
    } else if (progressBar === -2) {
      // Pulsing indeterminate progress (for backend processing)
      const pulse = (Math.sin(Date.now() / 200) + 1) / 2; // 0-1 sine wave
      const barWidth = Math.floor((screen.width - 2) * (0.3 + pulse * 0.4)); // 30-70% width
      const alpha = Math.floor(150 + pulse * 105); // 150-255 alpha
      ink(255, 180, 0, alpha).box(1, 1, barWidth, 1);
      $.system.nopaint.needsRender = true; // Keep animating
    }
    
    // Show progress text in center
    if (progressPhase) {
      // Animated dots
      const dots = Math.floor((Date.now() / 250) % 4);
      const text = progressPhase + ".".repeat(dots);
      
      // Background for text
      const textWidth = text.length * 6 + 16;
      const textHeight = 20;
      const x = (screen.width - textWidth) / 2;
      const y = screen.height / 2 - 10;
      
      ink(0, 200).box(x, y, textWidth, textHeight);
      ink(255, 180, 0).box(x, y, textWidth, textHeight, "outline");
      
      // Progress text
      ink(255, 255, 255).write(text, { center: "x", y: y + 6 });
      
      // Percentage below (only show if not in indeterminate state)
      if (progressPercentage > 0) {
        ink(255, 255, 255).write(`${progressPercentage}%`, { 
          center: "x", 
          y: y + textHeight + 8 
        });
      }
    }
  }

  // Calculate MOTD offset (do this before book rendering so it's always available)
  let motdXOffset = 0;
  
  // 📚 Paint book button (only on login curtain, in top-right area)
  if (showLoginCurtain && bookImageScaled) {
    // Use pre-scaled cached image
    const bookW = bookImageScaled.width;
    const bookH = bookImageScaled.height;
    
    // 📚 Second Product (Current)
    const titleText = "What is Landscape"; // No ? - MatrixChunky8 glyph not preloaded, causes huge fallback
    const authorText = "by John R. Stilgoe";
    const priceText = "$60 USD";
    
    // 📚 First Product (Deprecated - SOLD)
    // const titleText = "The Art of Seeing";
    // const authorText = "by Aldous Huxley";
    // const priceText = "$60 USD";
    
    const titleW = titleText.length * 4; // 4px per char for MatrixChunky8
    const authorW = authorText.length * 4; // 4px per char for MatrixChunky8
    const textH = 8; // 8px height for MatrixChunky8
    const lineSpacing = 1; // Tighter spacing between lines
    
    // Position book image in top-right with tight corner layout
    const rightEdge = screen.width - 6; // Right edge position
    
    // Title above the book image
    const titleX = rightEdge - titleW;
    const titleY = 6; // Start at top
    
    // Book below title
    const bookX = rightEdge - bookW;
    const bookY = titleY + textH + lineSpacing + 2; // Small gap after title
    
    // Author text below book image
    const authorX = rightEdge - authorW;
    const authorY = bookY + bookH + lineSpacing + 2;
    
    // Price below author
    const priceW = priceText.length * 4;
    const priceY = authorY + textH + lineSpacing + 2;
    
    // Calculate book ad bounding box (with some drift padding)
    const bookAdBox = {
      x: Math.min(titleX, bookX, authorX) - 10, // Left edge with padding
      y: titleY - 4, // Top edge with padding
      w: Math.max(titleW, bookW, authorW) + 14, // Width with padding
      h: priceY + textH + 4 - titleY // Height from title top to price bottom
    };
    
    // Check for actual geometric overlap with login/signup buttons
    let wouldOverlap = false;
    
    // Check overlap with login button
    if (login && !login.btn.disabled && login.btn.box) {
      const loginBox = login.btn.box;
      wouldOverlap = wouldOverlap || (
        bookAdBox.x < loginBox.x + loginBox.w &&
        bookAdBox.x + bookAdBox.w > loginBox.x &&
        bookAdBox.y < loginBox.y + loginBox.h &&
        bookAdBox.y + bookAdBox.h > loginBox.y
      );
    }
    
    // Check overlap with signup button
    if (signup && !signup.btn.disabled && signup.btn.box) {
      const signupBox = signup.btn.box;
      wouldOverlap = wouldOverlap || (
        bookAdBox.x < signupBox.x + signupBox.w &&
        bookAdBox.x + bookAdBox.w > signupBox.x &&
        bookAdBox.y < signupBox.y + signupBox.h &&
        bookAdBox.y + bookAdBox.h > signupBox.y
      );
    }
    
    // Check overlap with enter button
    if ($.system.prompt.input?.enter && !$.system.prompt.input.enter.btn.disabled && $.system.prompt.input.enter.btn.box) {
      const enterBox = $.system.prompt.input.enter.btn.box;
      wouldOverlap = wouldOverlap || (
        bookAdBox.x < enterBox.x + enterBox.w &&
        bookAdBox.x + bookAdBox.w > enterBox.x &&
        bookAdBox.y < enterBox.y + enterBox.h &&
        bookAdBox.y + bookAdBox.h > enterBox.y
      );
    }
    
    // Check overlap with paste button
    if ($.system.prompt.input?.paste && !$.system.prompt.input.paste.btn.disabled && $.system.prompt.input.paste.btn.box) {
      const pasteBox = $.system.prompt.input.paste.btn.box;
      wouldOverlap = wouldOverlap || (
        bookAdBox.x < pasteBox.x + pasteBox.w &&
        bookAdBox.x + bookAdBox.w > pasteBox.x &&
        bookAdBox.y < pasteBox.y + pasteBox.h &&
        bookAdBox.y + bookAdBox.h > pasteBox.y
      );
    }
    
    // Check overlap with chat ticker
    if (chatTickerButton && !chatTickerButton.disabled && chatTickerButton.box) {
      const tickerBox = chatTickerButton.box;
      wouldOverlap = wouldOverlap || (
        bookAdBox.x < tickerBox.x + tickerBox.w &&
        bookAdBox.x + bookAdBox.w > tickerBox.x &&
        bookAdBox.y < tickerBox.y + tickerBox.h &&
        bookAdBox.y + bookAdBox.h > tickerBox.y
      );
    }
    
    // Check overlap with MOTD (if present) and calculate offset if needed
    if (motd && screen.height >= 180) {
      // Calculate approximate MOTD bounding box with more aggressive wrapping
      const motdMaxWidth = Math.min(screen.width - 18, 150); // Cap at 150px for much tighter wrapping
      const motdY = screen.height / 2 - 48;
      const motdCharWidth = 6; // Default font char width
      const motdLineHeight = 10; // Default font line height
      const motdLines = Math.ceil((motd.length * motdCharWidth) / motdMaxWidth);
      const motdHeight = motdLines * motdLineHeight;
      const motdWidth = Math.min(motd.length * motdCharWidth, motdMaxWidth);
      
      // First, check if centered MOTD would overlap with book
      const centeredMotdBox = {
        x: (screen.width - motdWidth) / 2,
        y: motdY - 4,
        w: motdWidth,
        h: motdHeight + 8
      };
      
      const motdWouldOverlap = (
        bookAdBox.x < centeredMotdBox.x + centeredMotdBox.w &&
        bookAdBox.x + bookAdBox.w > centeredMotdBox.x &&
        bookAdBox.y < centeredMotdBox.y + centeredMotdBox.h &&
        bookAdBox.y + bookAdBox.h > centeredMotdBox.y
      );
      
      // Apply offset if there would be overlap with centered MOTD
      if (motdWouldOverlap) {
        // Apply graduated offset based on screen width
        if (screen.width >= 400) {
          motdXOffset = -60;
        } else if (screen.width >= 280) {
          // For smaller screens, shift more aggressively
          motdXOffset = -80;
        } else {
          // Very small screens - shift even more
          motdXOffset = -100;
        }
        
        // Recalculate MOTD box based on how it will ACTUALLY be positioned
        let actualMotdBox;
        const leftMargin = 9;
        
        if (screen.width < 400) {
          // Will be left-aligned on narrow screens
          actualMotdBox = {
            x: leftMargin,
            y: motdY - 4,
            w: motdWidth,
            h: motdHeight + 8
          };
        } else {
          // Will be offset from center on wider screens
          const offsetMotdCenterX = screen.width / 2 + motdXOffset;
          actualMotdBox = {
            x: offsetMotdCenterX - (motdWidth / 2),
            y: motdY - 4,
            w: motdWidth,
            h: motdHeight + 8
          };
        }
        
        // Check if actual position still overlaps with book
        const stillOverlaps = (
          bookAdBox.x < actualMotdBox.x + actualMotdBox.w &&
          bookAdBox.x + actualMotdBox.w > actualMotdBox.x &&
          bookAdBox.y < actualMotdBox.y + actualMotdBox.h &&
          bookAdBox.y + actualMotdBox.h > actualMotdBox.y
        );
        
        // Only mark as overlapping if the shift didn't help
        if (stillOverlaps) {
          wouldOverlap = true;
        }
      }
    }
    
    // Hide book if screen is too narrow (less than 220px wide to allow more room)
    const screenTooNarrow = screen.width < 220;
    
    // Only show book if it won't overlap with buttons/MOTD and screen is wide enough
    const shouldShowBook = !wouldOverlap && !screenTooNarrow;
    
    if (shouldShowBook) {
    
    // Theme-sensitive colors
    const isDark = $.dark;
    // const textColor = isDark ? [255, 255, 255] : [0, 0, 0]; // White in dark, black in light
    
    // 📚 Second Product (Current) - New colors
    const titleColor = isDark ? [150, 200, 255] : [50, 100, 200]; // Blue-ish tint for title
    const titleHighlightColor = isDark ? [100, 200, 255] : [0, 150, 255]; // Brighter blue when highlighted
    // const authorColor = isDark ? [255, 200, 150] : [140, 80, 50]; // Warm/orange-ish byline
    // const authorHighlightColor = [255, 255, 0]; // Yellow highlight for byline when pressed
    
    // 📚 First Product (Deprecated - SOLD) - Old colors
    // const titleColor = isDark ? [255, 200, 150] : [200, 100, 50]; // Orange/warm tint for title
    // const titleHighlightColor = isDark ? [255, 255, 100] : [255, 200, 0]; // Yellow tint when highlighted
    // const authorColor = isDark ? [200, 200, 255] : [80, 80, 140]; // Tinted byline (blue-ish)
    // const authorHighlightColor = [255, 255, 0]; // Yellow highlight for byline when pressed
    
    const shadowColor = isDark ? [0, 0, 0] : [255, 255, 255]; // Black shadow in dark, white in light
    // const priceNormalColor = isDark ? [0, 255, 0] : [0, 180, 0]; // Bright green in dark, darker in light
    // const priceHoverColor = isDark ? [100, 255, 100] : [0, 255, 0]; // Brighter greens
    // const priceDownColor = [255, 255, 0]; // Yellow when pressed (same for both)
    
    // Calculate drift/shake offset (a few pixels in x and y) - faster shaking
    const driftX = Math.floor(Math.sin(bookRotation * 0.06) * 2); // Faster drift, 2px range
    const driftY = Math.floor(Math.cos(bookRotation * 0.08) * 2); // Faster speed for more active feel
    
    // Text sway (subtle side-to-side movement)
    const textSwayX = Math.floor(Math.sin(bookRotation * 0.03) * 1.5); // Gentle sway, 1.5px range
    
    // Make button box around the image area (adjusted for drift)
    const totalW = bookW + 4; // Add padding for drift
    const totalH = bookH + 4;
    
    // Create or update button
    if (!bookButton) {
      bookButton = new $.ui.Button(bookX - 2, bookY - 2, totalW, totalH);
      bookButton.stickyScrubbing = true;
    } else {
      bookButton.disabled = false; // Re-enable when curtain is shown
      bookButton.box.x = bookX - 2;
      bookButton.box.y = bookY - 2;
      bookButton.box.w = totalW;
      bookButton.box.h = totalH;
    }
    
    // Determine highlight state (hover or down)
    const isHighlighted = bookButton.over || bookButton.down;
    
    // Scale effect when pressing down
    const imageScale = bookButton.down ? 1.1 : 1;
    const scaledBookW = Math.floor(bookW * imageScale);
    const scaledBookH = Math.floor(bookH * imageScale);
    const scaleOffsetX = Math.floor((scaledBookW - bookW) / 2);
    const scaleOffsetY = Math.floor((scaledBookH - bookH) / 2);
    
    // Draw shadow behind book (offset to bottom-right, scaled)
    $.ink(0, 0, 0, isDark ? 80 : 40) // Darker shadow in dark mode
      .box(Math.floor(bookX + driftX + 2 - scaleOffsetX), Math.floor(bookY + driftY + 2 - scaleOffsetY), scaledBookW, scaledBookH);
    
    // Draw book cover with drift/shake using paste (no rotation), with scale
    if (bookButton.down && imageScale !== 1) {
      // Use paste with scale object for custom dimensions
      $.paste(
        bookImageScaled, 
        Math.floor(bookX + driftX - scaleOffsetX), 
        Math.floor(bookY + driftY - scaleOffsetY),
        { scale: imageScale, width: scaledBookW, height: scaledBookH }
      );
    } else {
      $.paste(bookImageScaled, Math.floor(bookX + driftX), Math.floor(bookY + driftY));
    }
    
    // Apply brightness overlay when highlighted (always use scaled dimensions)
    if (isHighlighted) {
      const overlayX = bookButton.down ? Math.floor(bookX + driftX - scaleOffsetX) : Math.floor(bookX + driftX);
      const overlayY = bookButton.down ? Math.floor(bookY + driftY - scaleOffsetY) : Math.floor(bookY + driftY);
      const overlayW = bookButton.down ? scaledBookW : bookW;
      const overlayH = bookButton.down ? scaledBookH : bookH;
      $.ink(255, 255, 255, bookButton.down ? 60 : 30) // Brighter when down
        .box(overlayX, overlayY, overlayW, overlayH);
    }
    
    // Determine text colors based on state - faster blinking when down
    const blinkSpeed = bookButton.down ? 0.3 : 0.15; // Faster blink when pressed
    const blinkPhase = Math.sin(bookRotation * blinkSpeed) > 0; // Boolean blink
    const shouldBlink = bookButton.down && blinkPhase;
    const finalTitleColor = shouldBlink ? titleHighlightColor : (isHighlighted ? titleHighlightColor : titleColor);
    
    // Draw title text (with sway effect and highlight)
    // Shadow
    ink(shadowColor[0], shadowColor[1], shadowColor[2]).write(titleText, { x: titleX + textSwayX + 1, y: titleY + 1 }, undefined, undefined, false, "MatrixChunky8");
    // Main text
    ink(finalTitleColor[0], finalTitleColor[1], finalTitleColor[2]).write(titleText, { x: titleX + textSwayX, y: titleY }, undefined, undefined, false, "MatrixChunky8");
    // Main text
    // ink(0).write(titleText, {x: 0, y: 0}, 255, 20, true, "MatrixChunky8");

    // // Draw author text (with sway effect, tinted and slightly different color, with blink on down)
    // const finalAuthorColor = shouldBlink ? authorHighlightColor : (bookButton.down ? authorHighlightColor : authorColor);
    // ink(...shadowColor)
    //   .write(authorText, { x: authorX + textSwayX + 1, y: authorY + 1 }, undefined, undefined, false, "MatrixChunky8");
    // ink(...finalAuthorColor)
    //   .write(authorText, { x: authorX + textSwayX, y: authorY }, undefined, undefined, false, "MatrixChunky8");
    
    // // Price text below author byline, scale 1 with solid background and drift
    // const priceFont = 'MatrixChunky8';

    // // Price drift (side-to-side only, slower)
    // const priceDriftX = Math.floor(Math.sin(bookRotation * 0.05) * 3); // Slower, 3px horizontal range only

    // // Price position: below author, with horizontal drift only
    // const priceScale = 1; // Normal size
    // const priceW = priceText.length * 4 * priceScale;
    // const priceH = 8 * priceScale;
    // const padding = 2;
    // const priceTextX = rightEdge - priceW - padding * 2 + priceDriftX;
    // const priceTextY = authorY + textH + lineSpacing + 2; // Below author (no vertical drift)

    // // Solid background box for price (theme-sensitive)
    // const priceBg = isDark ? [0, 0, 0, 255] : [255, 255, 255, 255];
    // ink(...priceBg).box(priceTextX - padding, priceTextY - padding, priceW + padding * 2, priceH + padding * 2);

    // // Subtle dark shadow (not blinking) - tighter offset
    // ink(0, 0, 0, isDark ? 80 : 50).write(priceText, { x: priceTextX + 0.5, y: priceTextY + 0.5, size: priceScale }, undefined, undefined, false, priceFont);

    // // Price text - normal green (not dimmed, since this product is available)
    // const priceFinalColor = bookButton.down ? priceDownColor : (bookButton.over ? priceHoverColor : priceNormalColor);
    // ink(...priceFinalColor).write(priceText, { x: priceTextX, y: priceTextY, size: priceScale }, undefined, undefined, false, priceFont);
    
    // 📚 First Product (Deprecated - SOLD) - SOLD banner removed for second product
    // // Draw "SOLD" banner centered on the book with unique animation
    // const soldText = "SOLD";
    // const soldTextWidth = soldText.length * 6; // Default font is 6px wide per character
    // const soldTextHeight = 8; // Default font height
    // const soldPadding = 4;
    // 
    // // Center position on book (with slower side-to-side sway)
    // const soldSway = Math.sin(bookRotation * 0.05) * 2; // Slower side-to-side, 2px range
    // const soldX = bookX + (bookW / 2) - (soldTextWidth / 2) + driftX + soldSway;
    // const soldY = bookY + (bookH / 2) - (soldTextHeight / 2) + driftY;
    // 
    // // Red banner background with pulsing opacity
    // const soldBgAlpha = Math.abs(Math.sin(bookRotation * 0.08)) * 80 + 160; // Pulse between 160-240
    // ink(200, 0, 0, soldBgAlpha).box(soldX - soldPadding, soldY - soldPadding, soldTextWidth + soldPadding * 2, soldTextHeight + soldPadding * 2);
    // 
    // // SOLD text blinking between yellow and red
    // const soldBlink = Math.sin(bookRotation * 0.12) > 0; // Boolean blink
    // const soldColor = soldBlink ? [255, 255, 0] : [255, 50, 50]; // Yellow or bright red
    // 
    // // Shadow for SOLD text
    // ink(0, 0, 0, 180).write(soldText, { x: soldX + 1, y: soldY + 1 });
    // ink(...soldColor).write(soldText, { x: soldX, y: soldY });
    } else if (bookButton) {
      // Hide book button if screen too small or would overlap
      bookButton.disabled = true;
    }
  } else if (bookButton) {
    // Disable button when not on login curtain
    bookButton.disabled = true;
  }

  // 🟢 KidLisp mode border effect (full window border with scrolling dots)
  // Only show when prompt is focused (canType is true)
  const isKidlispMode = $.system?.prompt?.actualKidlisp;
  if (isKidlispMode && $.system.prompt.input.canType) {
    // Animated offset for scrolling effect
    const scrollOffset = Math.floor(bookRotation * 0.5) % 4; // Scroll 4 pixel cycle (matches spacing)
    
    // Cycle through vibrant colors
    const colorPhase = (bookRotation * 0.1) % 6;
    const colors = [
      [100, 255, 100], // Bright green (primary for kidlisp)
      [100, 255, 255], // Cyan
      [100, 200, 255], // Sky blue
      [200, 100, 255], // Purple
      [255, 100, 200], // Pink
      [255, 255, 100], // Yellow
    ];
    
    const currentColorIndex = Math.floor(colorPhase);
    const nextColorIndex = (currentColorIndex + 1) % colors.length;
    const blend = colorPhase - currentColorIndex;
    
    // Blend between current and next color
    const currentColor = colors[currentColorIndex];
    const nextColor = colors[nextColorIndex];
    const blendedColor = [
      Math.floor(currentColor[0] * (1 - blend) + nextColor[0] * blend),
      Math.floor(currentColor[1] * (1 - blend) + nextColor[1] * blend),
      Math.floor(currentColor[2] * (1 - blend) + nextColor[2] * blend),
    ];
    
    // Pulsing alpha
    const pulseAlpha = Math.floor(Math.sin(bookRotation * 0.12) * 60 + 140); // 80-200 range
    
    // Draw solid dotted border around entire screen (all dots filled)
    const dotSpacing = 4; // Pixels between dots
    const dotSize = 1; // 1 pixel dots
    
    // Top border
    for (let x = 0; x < screen.width; x += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box((x + scrollOffset) % screen.width, 0, dotSize, dotSize);
    }
    
    // Bottom border
    for (let x = 0; x < screen.width; x += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box((x + scrollOffset) % screen.width, screen.height - dotSize, dotSize, dotSize);
    }
    
    // Left border
    for (let y = 0; y < screen.height; y += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box(0, (y + scrollOffset) % screen.height, dotSize, dotSize);
    }
    
    // Right border
    for (let y = 0; y < screen.height; y += dotSpacing) {
      ink(...blendedColor, pulseAlpha).box(screen.width - dotSize, (y + scrollOffset) % screen.height, dotSize, dotSize);
    }
    
    // Keep animating
    $.needsPaint();
  }

  // 🎰 Polychrome border effect pointing to top-left corner (on login curtain)
  if (showLoginCurtain) {
    // Cycle through pink, purple, green phases
    const colorPhase = (bookRotation * 0.08) % 3;
    let primaryColor, secondaryColor, tertiaryColor;
    
    if (colorPhase < 1) {
      primaryColor = [255, 100, 200]; // Pink
      secondaryColor = [200, 100, 255]; // Purple
      tertiaryColor = [100, 255, 150]; // Green
    } else if (colorPhase < 2) {
      primaryColor = [200, 100, 255]; // Purple
      secondaryColor = [100, 255, 150]; // Green
      tertiaryColor = [255, 100, 200]; // Pink
    } else {
      primaryColor = [100, 255, 150]; // Green
      secondaryColor = [255, 100, 200]; // Pink
      tertiaryColor = [200, 100, 255]; // Purple
    }
    
    // Pulsing effect for base intensity
    const pulseBase = Math.sin(bookRotation * 0.15);
    
    // Border extends to 2/3rds of screen
    const borderWidth = (screen.width / 3) * 2;
    const borderHeight = (screen.height / 3) * 2;
    const borderThickness = 1; // Fixed 1 pixel wide
    
    // Draw top border with dithered/striped pattern (1 pixel tall)
    for (let x = 0; x < borderWidth; x++) {
      // Intensity increases as we get closer to the corner (left edge)
      const intensityRatio = 1 - (x / borderWidth); // 1.0 at corner, 0.0 at edge
      const alpha = Math.floor(intensityRatio * 100 + 30 + pulseBase * 30); // 30-160 range
      
      // Dither pattern: show pixels based on intensity ratio
      const patternValue = (x + Math.floor(bookRotation / 4)) % 4; // 0-3 pattern
      const threshold = (1 - intensityRatio) * 4; // 0-4 threshold
      
      if (patternValue >= threshold) {
        ink(...primaryColor, alpha).box(x, 0, 1, borderThickness);
      }
    }
    
    // Draw left border with dithered/striped pattern (1 pixel wide)
    for (let y = 0; y < borderHeight; y++) {
      // Intensity increases as we get closer to the corner (top edge)
      const intensityRatio = 1 - (y / borderHeight); // 1.0 at corner, 0.0 at edge
      const alpha = Math.floor(intensityRatio * 100 + 30 + pulseBase * 30); // 30-160 range
      
      // Dither pattern: show pixels based on intensity ratio
      const patternValue = (y + Math.floor(bookRotation / 4)) % 4; // 0-3 pattern
      const threshold = (1 - intensityRatio) * 4; // 0-4 threshold
      
      if (patternValue >= threshold) {
        ink(...primaryColor, alpha).box(0, y, borderThickness, 1);
      }
    }
    
    // 🎆 Spawn particles from the cursor position and draw cursor overlay
    const input = $.system.prompt.input;
    if (input?.prompt) {
      const cursorPos = input.prompt.pos(undefined, true);
      
      if (cursorPos && cursorPos.x !== undefined && cursorPos.y !== undefined) {
        // Spawn particles 30% of the time
        if (Math.random() < 0.3) {
          // Match cursor color - green for kidlisp, theme color otherwise
          const isDark = $.dark;
          const isKidlisp = $.system?.prompt?.actualKidlisp;
          let particleColor;
          
          if (isKidlisp) {
            // Green for kidlisp
            particleColor = isDark ? [100, 255, 100] : [0, 150, 0];
          } else {
            // Use palette block color or fallback to theme text color
            const blockColor = input.pal?.block || (isDark ? [255, 255, 255] : [0, 0, 0]);
            particleColor = Array.isArray(blockColor) ? blockColor.slice(0, 3) : (isDark ? [255, 255, 255] : [0, 0, 0]);
          }
          
          cornerParticles.push({
            x: cursorPos.x + Math.random() * cursorPos.w, // Spawn across bottom of cursor box
            y: cursorPos.y + cursorPos.h, // Start at bottom of cursor
            vx: (Math.random() - 0.5) * 0.5, // Minimal horizontal drift
            vy: Math.random() * 1.5 + 0.5, // Longer vertical fall (0.5-2 pixels per frame)
            life: 1.0, // Full life
            color: particleColor,
            size: 1, // Always 1 pixel
          });
        }
      }
    }
    
    // Update and draw particles
    cornerParticles = cornerParticles.filter(p => {
      // Update position
      p.x += p.vx;
      p.y += p.vy;
      p.life -= 0.02; // Fade out
      
      // Draw particle
      if (p.life > 0) {
        const alpha = Math.floor(p.life * 200);
        ink(...p.color, alpha).box(p.x, p.y, p.size, p.size);
      }
      
      // Keep particle if still alive
      return p.life > 0;
    });
    
    // 💡 Draw semi-transparent ghost cursor when curtain is up
    if (input?.prompt) {
      const cursorPos = input.prompt.pos(undefined, true);
      
      if (cursorPos && cursorPos.x !== undefined && cursorPos.y !== undefined) {
        const isDark = $.dark;
        // Match the actual cursor color but with reduced opacity
        const isKidlisp = $.system?.prompt?.actualKidlisp;
        let fillColor;
        
        if (isKidlisp) {
          // Green for kidlisp (matching the actual cursor)
          fillColor = isDark ? [100, 255, 100, 100] : [0, 150, 0, 100];
        } else {
          // Use the palette block color (default cursor color)
          const blockColor = input.pal?.block || (isDark ? [255, 255, 255] : [0, 0, 0]);
          fillColor = [...blockColor, 100]; // Add alpha 100 for subtle transparency
        }
        
        // Draw on layer 3 to ensure it's above everything else on the curtain
        $.layer(3);
        
        // Draw filled cursor box with subtle transparency (no outline)
        ink(...fillColor).box(cursorPos.x, cursorPos.y, cursorPos.w, cursorPos.h);
        
        // Reset to default layer
        $.layer(1);
      }
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

    // 📊 Stats / Analytics - Unified Ticker System
    
    const loginY = screen.height / 2;
    
    // Calculate dynamic positioning to prevent overlap
    const tickerHeight = 14; // Text height plus padding
    const tickerSpacing = 0; // No space between tickers for tight stripes
    const tickerPadding = 3; // Padding around text
    
    let currentTickerY = loginY + 44; // Start 44px below login (moved down from 30px)
    
    // 1. CHAT TICKER (top priority)
    if ($.chat.messages.length > 0 && screen.height >= 180) {
      const msg = $.chat.messages[$.chat.messages.length - 1];
      const fullText = msg.from + ": " + msg.text;
      const chatTickerY = currentTickerY;
      
      // Create or update ticker instance
      if (!chatTicker) {
        chatTicker = new $.gizmo.Ticker(fullText, {
          speed: 1, // 1px per frame
          separator: " - ",
          reverse: false, // Left to right
        });
        chatTicker.paused = false;
        chatTicker.offset = 0; // No offset for chat
      } else {
        chatTicker.setText(fullText);
      }

      // Update ticker animation only if not paused
      if (!chatTicker.paused) {
        chatTicker.update($);
      }

      // Create or update invisible button over ticker area
      if (!chatTickerButton) {
        chatTickerButton = new $.ui.Button({
          x: 0,
          y: chatTickerY - tickerPadding,
          w: screen.width,
          h: tickerHeight + (tickerPadding * 2),
        });
        chatTickerButton.noEdgeDetection = true;
        chatTickerButton.noRolloverActivation = true;
        chatTickerButton.stickyScrubbing = true;
      } else {
        chatTickerButton.box.x = 0;
        chatTickerButton.box.y = chatTickerY - tickerPadding;
        chatTickerButton.box.w = screen.width;
        chatTickerButton.box.h = tickerHeight + (tickerPadding * 2);
      }

      // Paint background and borders
      if (!chatTickerButton.disabled) {
        const boxHeight = tickerHeight + (tickerPadding * 2);
        const boxY = chatTickerY - tickerPadding;
        
        // Dark background for high contrast
        const bgAlpha = chatTickerButton.down ? 120 : 80;
        ink([0, 80, 80, bgAlpha]).box(0, boxY, screen.width, boxHeight - 1);
        
        // Top border (1px)
        ink([0, 200, 200, 255]).line(0, boxY, screen.width, boxY);
        
        // Bottom border (1px, non-overlapping)
        ink([0, 200, 200, 255]).line(0, boxY + boxHeight - 1, screen.width, boxY + boxHeight - 1);
        
        // Bright text for high contrast - adjusted for vertical centering
        const tickerAlpha = chatTickerButton.down ? 255 : 220;
        const textY = chatTickerY + 1; // Add 1px to center text better
        chatTicker.paint($, 0, textY, {
          color: [0, 255, 255], // Bright cyan
          alpha: tickerAlpha,
          width: screen.width,
        });
      }
      
      // Move down for next ticker
      currentTickerY += tickerHeight + (tickerPadding * 2) + tickerSpacing;
    } else {
      chatTicker = null;
      chatTickerButton = null;
    }
    
    // 2. CONTENT TICKER (combined $kidlisp, #painting, !tape)
    if (contentItems.length > 0 && screen.height >= 220) {
      const contentTickerY = currentTickerY;
      
      // Build text with prefixes: $ for kidlisp, # for painting, ! for tape
      const fullText = contentItems.map(item => {
        const prefix = item.type === 'kidlisp' ? '$' : item.type === 'painting' ? '#' : '!';
        return `${prefix}${item.code}`;
      }).join(" · ");
      
      // Create or update content ticker instance
      if (!contentTicker) {
        contentTicker = new $.gizmo.Ticker(fullText, {
          speed: 1, // 1px per frame
          separator: " · ",
          reverse: true, // Right to left (opposite of chat)
        });
        contentTicker.paused = false;
        contentTicker.offset = 100; // Offset by 100px for stagger
      } else {
        contentTicker.setText(fullText);
      }
      
      // Update ticker animation
      if (!contentTicker.paused) {
        contentTicker.update($);
      }
      
      // Create or update invisible button over ticker area
      if (!contentTickerButton) {
        contentTickerButton = new $.ui.Button({
          x: 0,
          y: contentTickerY - tickerPadding,
          w: screen.width,
          h: tickerHeight + (tickerPadding * 2),
        });
        contentTickerButton.noEdgeDetection = true;
        contentTickerButton.noRolloverActivation = true;
        contentTickerButton.stickyScrubbing = true;
      } else {
        contentTickerButton.box.x = 0;
        contentTickerButton.box.y = contentTickerY - tickerPadding;
        contentTickerButton.box.w = screen.width;
        contentTickerButton.box.h = tickerHeight + (tickerPadding * 2);
      }
      
      // Paint background and border
      if (!contentTickerButton.disabled) {
        const boxHeight = tickerHeight + (tickerPadding * 2);
        const boxY = contentTickerY - tickerPadding;
        
        // Dark background for high contrast
        const bgAlpha = contentTickerButton.down ? 120 : 80;
        ink([30, 30, 30, bgAlpha]).box(0, boxY, screen.width, boxHeight - 1);
        
        // Bottom border (1px, non-overlapping)
        ink([200, 200, 200, 255]).line(0, boxY + boxHeight - 1, screen.width, boxY + boxHeight - 1);
        
        // Now render the text with color-coded prefixes
        // We need to manually render each item with its own color
        const textY = contentTickerY + 1;
        const contentTickerAlpha = contentTickerButton.down ? 255 : 220;
        
        // Calculate positions for each item manually
        let currentX = -contentTicker.getOffset();
        const displayWidth = screen.width;
        const separator = " · ";
        
        // Render items multiple times to fill the screen (cycling)
        const cycleWidth = contentTicker.getCycleWidth();
        const numCycles = Math.ceil((displayWidth + cycleWidth) / cycleWidth) + 1;
        
        for (let cycle = 0; cycle < numCycles; cycle++) {
          contentItems.forEach((item, idx) => {
            const prefix = item.type === 'kidlisp' ? '$' : item.type === 'painting' ? '#' : '!';
            const text = `${prefix}${item.code}`;
            
            // Color-code by type
            let color;
            if (item.type === 'kidlisp') {
              color = [150, 255, 150]; // Bright lime green
            } else if (item.type === 'painting') {
              color = [255, 150, 255]; // Bright magenta
            } else { // tape
              color = [255, 200, 100]; // Bright orange/yellow
            }
            
            // Only render if visible
            if (currentX > -100 && currentX < displayWidth + 100) {
              ink(color, contentTickerAlpha).write(text, { x: currentX, y: textY });
            }
            
            // Move to next position
            const textWidth = $.text.box(text).box.width;
            currentX += textWidth;
            
            // Add separator after each item (except potentially the last in a cycle)
            if (idx < contentItems.length - 1 || cycle < numCycles - 1) {
              if (currentX > -100 && currentX < displayWidth + 100) {
                ink([150, 150, 150], contentTickerAlpha).write(separator, { x: currentX, y: textY });
              }
              const sepWidth = $.text.box(separator).box.width;
              currentX += sepWidth;
            }
          });
        }
      }
      
      // Move down for next ticker (if we add more)
      currentTickerY += tickerHeight + (tickerPadding * 2) + tickerSpacing;
    } else {
      contentTicker = null;
      contentTickerButton = null;
    }

    // Handle Stats - positioned directly under login button
    if (handles && screen.height >= 100) {
      // Position directly under the login button
      let handlesY = screen.height - 16; // Default bottom position
      
      if (login && !login.btn.disabled && login.btn.box) {
        // Position directly under login button with extra spacing
        handlesY = login.btn.box.y + login.btn.box.h + 8; // 8px gap below button (moved down)
      }
      
      // Use MatrixChunky8 font for more compact display, centered
      const handlesText = `${handles.toLocaleString()} HANDLES SET`;
      
      // Shadow color (black in dark mode, white in light mode)
      const handlesShadowColor = $.dark ? [0, 0, 0] : [255, 255, 255];
      
      // Draw shadow first (offset by 1px)
      ink(...handlesShadowColor).write(
        handlesText,
        {
          center: "x",
          y: handlesY + 1,
        },
        undefined,
        undefined,
        false,
        "MatrixChunky8"
      );
      
      // Cyberpunk-style flicker effect - build colored text string with per-character colors
      const baseHandleColor = pal.handleColor;
      let coloredHandlesText = "";
      
      // Build text with individual character colors for flicker effect
      for (let i = 0; i < handlesText.length; i++) {
        const char = handlesText[i];
        
        // Each character has a chance to flicker independently
        const flickerIntensity = Math.random() < 0.15 ? Math.random() * 0.6 : 0; // 15% chance of flicker
        const r = Math.floor(Math.max(0, baseHandleColor[0] * (1 - flickerIntensity)));
        const g = Math.floor(Math.max(0, baseHandleColor[1] * (1 - flickerIntensity * 0.8)));
        const b = Math.floor(Math.max(0, baseHandleColor[2] * (1 - flickerIntensity * 0.5)));
        
        coloredHandlesText += `\\${r},${g},${b}\\${char}`;
      }
      
      // Draw main text with per-character flicker
      ink(pal.handleColor).write(
        coloredHandlesText,
        {
          center: "x",
          y: handlesY,
        },
        undefined,
        undefined,
        false,
        "MatrixChunky8"
      );
    }

    // MOTD (Mood of the Day) - show above login/signup buttons with animation
    if (motd && screen.height >= 180) {
      motdFrame += 1; // Increment animation frame
      
      // Subtle sway (up and down)
      const swayY = Math.sin(motdFrame * 0.05) * 2; // 2 pixel sway range
      const motdY = screen.height / 2 - 48 + swayY; // Above the login button with sway
      
      // Create colorful blinking letters
      let coloredText = "";
      const colors = ["pink", "cyan", "yellow", "lime", "orange", "magenta"];
      
      for (let i = 0; i < motd.length; i++) {
        // Each letter gets a color that changes over time based on its position
        const colorIndex = Math.floor((i + motdFrame * 0.1) % colors.length);
        const color = colors[colorIndex];
        coloredText += `\\${color}\\${motd[i]}`;
      }
      
      // Use more aggressive text wrapping (max 150px) to keep MOTD compact
      const motdMaxWidth = Math.min(screen.width - 18, 150);
      
      // Determine alignment: left-align when book needs room on narrow screens
      let writePos;
      const leftMargin = 9;
      
      if (motdXOffset < 0 && screen.width < 400) {
        // Left-align on narrow screens when overlapping with book
        writePos = { x: leftMargin, y: Math.floor(motdY) };
      } else {
        // Center by default
        writePos = { center: "x", y: Math.floor(motdY) };
      }
      
      ink(pal.handleColor).write(
        coloredText,
        writePos,
        [255, 50, 200, 24],
        motdMaxWidth,
      );
      
      $.needsPaint();
    }
  }

  // Paint UI Buttons
  //if (!net.iframe) {

  // what we actually want for login and signup is pal.signup / pal.login
  // currently gives an error, I think because of paint (works fine with ink)
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

      ink("black", 128).write(
        message,
        {
          center: "x",
          x: screen.width / 2 + 1,
          y: screen.height / 2 - 48 + 1,
        },
        undefined,
        screen.width - 16,
      );

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
  
  // Oscillate book rotation for drift animation
  const showLoginCurtain = 
    (!login?.btn.disabled && !profile) || 
    (!login && !profile?.btn.disabled);
  if (showLoginCurtain && bookImage) {
    bookRotation += 1.0; // Faster oscillation for more active shaking
    $.needsPaint();
  }

  if ($.store["handle:received"]) {
    profile = new $.ui.TextButton($.handle(), {
      center: "xy",
      screen: $.screen,
    });
    profile.stickyScrubbing = true; // Prevent drag-between-button behavior
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
  painting,
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

  // Via vscode extension.
  if (e.is("aesthetic-parent:focused")) {
    // console.log("🔭 Focusing in on `prompt`...");
    // Clear any latent text when gaining focus to prevent MOTD showing when focused
    system.prompt.input.text = "";
    // activated(api, true);
    // system.prompt.input.canType = true;
    send({ type: "keyboard:unlock" });
    send({ type: "keyboard:open" }); // Necessary for desktop.
  }

  // 👱 Handle Callback
  if (e.is("handle:request:completed")) {
    console.log("Handle request completed:", profile);
    profile.btn.disabled = false;
  }

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

  const cancelSound = () => {
    synth({
      tone: 200,
      beats: 0.1,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
  };

  if (!net.sandboxed) {
    if (login && !login.btn.disabled) {
      login.btn.act(e, {
        down: () => downSound(),
        push: () => {
          pushSound();
          net.login();
          // if (net.iframe) jump("login-wait");
        },
        cancel: () => cancelSound(),
      });
    }
  }

  if (!net.iframe) {
    if (signup && !signup.btn.disabled) {
      signup.btn.act(e, {
        down: () => downSound(),
        push: () => {
          pushSound();
          net.signup();
        },
        cancel: () => cancelSound(),
      });
    }
  }

  // 📚 Book button interaction (only on login curtain, same as login/signup buttons)
  const showLoginCurtainAct = 
    (!login?.btn.disabled && !profile) || 
    (!login && !profile?.btn.disabled);
    
  if (bookButton && showLoginCurtainAct && !bookButton.disabled) {
    bookButton.disabled = false; // Re-enable if on login curtain
    bookButton.act(e, {
      over: () => {
        // Rollover/hover effect - trigger repaint for visual feedback
        $.needsPaint();
      },
      down: () => downSound(),
      push: () => {
        pushSound();
        // 📚 Second Product (Current)
        const bookUrl = "https://shop.aesthetic.computer/products/books_what-is-landscape-by-john-r-stilgoe_25-4-8-21-08";
        // 📚 First Product (Deprecated - SOLD)
        // const bookUrl = "https://shop.aesthetic.computer/products/books_the-art-of-seeing-by-aldous-huxley_25-4-8-21-0";
        if (net.iframe) {
          send({ type: "post-to-parent", content: { type: "openExternal", url: bookUrl } });
        } else {
          jump(bookUrl);
        }
        flashColor = [0, 255, 0];
        makeFlash({ api, needsPaint, net, screen, num, jump, system, user, store, send, handle, glaze, canShare, notice, ui });
      },
      cancel: () => cancelSound(),
    });
  } else if (bookButton && !showLoginCurtainAct) {
    bookButton.disabled = true;
  }

  // Chat ticker button (invisible, just for click interaction)
  if (chatTickerButton && !chatTickerButton.disabled) {
    chatTickerButton.act(e, {
      down: () => {
        // Sound feedback on tap down
        downSound();

        // Stop ticker animation and store initial scrub position
        if (chatTicker) {
          chatTicker.paused = true;
          chatTickerButton.scrubStartX = e.x;
          chatTickerButton.scrubInitialOffset = chatTicker.getOffset();
          chatTickerButton.hasScrubbed = false;
        }
        needsPaint();
      },
      scrub: (btn) => {
        // Manual scrubbing - move content left/right using current position vs start
        if (chatTicker && e.x !== undefined && e.y !== undefined) {
          const scrubDelta = e.x - chatTickerButton.scrubStartX;

          // Calculate the raw offset
          let newOffset = chatTickerButton.scrubInitialOffset - scrubDelta;

          // Add elastic bounce effect for negative values
          if (newOffset < 0) {
            // Apply diminishing returns for negative movement (elastic effect)
            // The further negative, the less responsive it becomes
            newOffset = newOffset * 0.3; // Scale down negative movement to 30%
          }

          chatTicker.setOffset(newOffset);

          chatTickerButton.hasScrubbed = Math.abs(scrubDelta) > 5;

          // Play softer, shorter tick sound during scrubbing
          synth({
            type: "sine",
            tone: 1200 + Math.abs(scrubDelta) * 2, // Pitch varies with scrub distance
            attack: 0.005,
            decay: 0.9,
            volume: 0.08,
            duration: 0.01,
          });

          needsPaint();
        }
      },
      push: () => {
        // Sound feedback on successful action
        if (!chatTickerButton.hasScrubbed) {
          pushSound();
        }

        // Only jump to chat if we didn't scrub
        if (!chatTickerButton.hasScrubbed) {
          // Keep ticker stopped when jumping to chat
          jump("chat");
        } else {
          // Resume animation if we scrubbed
          if (chatTicker) {
            chatTicker.paused = false;
          }
        }
      },
      cancel: () => {
        // Cancel sound
        cancelSound();

        // Resume animation on cancel
        if (chatTicker) {
          chatTicker.paused = false;
        }
      },
    });
  }

  // Content ticker button (invisible, just for click interaction)
  if (contentTickerButton && !contentTickerButton.disabled) {
    contentTickerButton.act(e, {
      down: () => {
        downSound();
        if (contentTicker) {
          contentTicker.paused = true;
          contentTickerButton.scrubStartX = e.x;
          contentTickerButton.scrubInitialOffset = contentTicker.getOffset();
          contentTickerButton.hasScrubbed = false;
        }
        needsPaint();
      },
      scrub: (btn) => {
        if (contentTicker && e.x !== undefined && e.y !== undefined) {
          const scrubDelta = e.x - contentTickerButton.scrubStartX;
          let newOffset = contentTickerButton.scrubInitialOffset - scrubDelta;
          
          if (newOffset < 0) {
            newOffset = newOffset * 0.3; // Elastic effect
          }
          
          contentTicker.setOffset(newOffset);
          contentTickerButton.hasScrubbed = Math.abs(scrubDelta) > 5;
          
          synth({
            type: "sine",
            tone: 1200 + Math.abs(scrubDelta) * 2,
            attack: 0.005,
            decay: 0.9,
            volume: 0.08,
            duration: 0.01,
          });
          
          needsPaint();
        }
      },
      push: () => {
        if (!contentTickerButton.hasScrubbed) {
          pushSound();
        }
        
        if (!contentTickerButton.hasScrubbed && contentItems.length > 0) {
          // Jump to first content item
          const item = contentItems[0];
          const prefix = item.type === 'kidlisp' ? '$' : item.type === 'painting' ? '#' : '!';
          jump(`${prefix}${item.code}`);
        } else {
          if (contentTicker) {
            contentTicker.paused = false;
          }
        }
      },
      cancel: () => {
        cancelSound();
        if (contentTicker) {
          contentTicker.paused = false;
        }
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
      (bookButton?.disabled === false && bookButton?.box.contains(e)) ||
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

  if (profile && !profile.btn.disabled) {
    profile.btn.act(e, {
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
      cancelSound();

      if (profileAction !== "profile") {
        // send({ type: "keyboard:disabled" }); // Disable keyboard flag.
        send({ type: "keyboard:lock" });
        system.prompt.input.backdropTouchOff = true;
      }
    },
  });
  }

  // 🖥️ Screen
  if (e.is("reframed")) {
    positionWelcomeButtons(screen, net.iframe);
    nopaint_adjust(api);
    system.nopaint.present(api);
  }

  // ⌨️ Keyboard (Skip startup sound if a key is pressed or text is pasted.)
  if (e.is("keyboard:open") && firstActivation && e.method !== "pointer") {
    firstActivation = false;
    // console.log("⌨️ First keyboard activation completed!");
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
      //  if (activeCompletions.length > 0)
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
    // console.log("Tab completing:", activeCompletions[0]);
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
  // Clear any latent text when activating to prevent MOTD showing when focused
  // but only if we don't have params (which means we're not coming from backspace navigation)
  if (state === true && !$.params[0]) {
    $.system.prompt.input.text = "";
  }

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
  if (chatTickerButton) chatTickerButton.disabled = state;
}

// 💬 Receive each response in full.
function reply(text) {
  // firstCommandSent = true;
  // console.log("😀 Replied with:", text || "Halted?");
}

// 📰 Meta
function meta() {
  return {
    title: "Prompt",
    desc: "Enter anything to get started.",
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

async function makeMotd({ system, needsPaint, handle, user, net, api, notice }) {
  motd = "aesthetic.computer"; // Fallback motd.
  motdController = new AbortController();
  try {
    const res = await fetch("/api/mood/@jeffrey", {
      signal: motdController.signal,
    });
    if (res.status === 200) {
      motd = (await res.json()).mood;
      needsPaint();
    } else {
      console.warn("😢 No mood found.");
    }
  } catch (err) {
    // console.warn("🙁 System `mood` fetch aborted.");
  }
}

// Fetch all content (kidlisp, painting, tape) and combine into a single shuffled array
async function fetchContentItems(api) {
  try {
    // Fetch all types in one request
    const res = await fetch("/api/tv?types=kidlisp,painting,tape&limit=60");
    if (res.status === 200) {
      const data = await res.json();
      const items = [];
      
      // Collect kidlisp items
      if (data.media?.kidlisp && Array.isArray(data.media.kidlisp)) {
        data.media.kidlisp.forEach(item => {
          if (item.code) items.push({ type: 'kidlisp', code: item.code });
        });
      }
      
      // Collect painting items
      if (data.media?.paintings && Array.isArray(data.media.paintings)) {
        data.media.paintings.forEach(item => {
          if (item.code) items.push({ type: 'painting', code: item.code });
        });
      }
      
      // Collect tape items
      if (data.media?.tapes && Array.isArray(data.media.tapes)) {
        data.media.tapes.forEach(item => {
          if (item.code) items.push({ type: 'tape', code: item.code });
        });
      }
      
      // Shuffle the combined array for variety
      contentItems = items.sort(() => Math.random() - 0.5);
      console.log("✅ Content items loaded:", contentItems.length, 
                  `(${items.filter(i => i.type === 'kidlisp').length} kidlisp, ` +
                  `${items.filter(i => i.type === 'painting').length} paintings, ` +
                  `${items.filter(i => i.type === 'tape').length} tapes)`);
      api.needsPaint();
    } else {
      console.warn("⚠️ Could not fetch content items. Status:", res.status);
    }
  } catch (err) {
    console.warn("⚠️ Content fetch error:", err);
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

function fetchUser() {
  const { api, ui, user, handle, screen, store, jump, beep, broadcast } =
    fetchUserAPI;
  fetchingUser = true;
  fetch(`/user?from=${encodeURIComponent(user.email)}&withHandle=true`)
    .then((res) => res.json())
    .then((u) => {
      if (u.email_verified) {
        const previousHandle = handle();
        // console.log("🟪 User:", u, "Previous Handle:", previousHandle);
        if (previousHandle) {
          profileAction = "profile";
          profile = new ui.TextButton(previousHandle, { center: "xy", screen });
          profile.stickyScrubbing = true; // Prevent drag-between-button behavior
        } else if (u.handle) {
          broadcast("handle:updated:" + u.handle);
          store["handle"] = u.handle;
          // Announce the handle change...
          jump("chat");
          beep();
        } else {
          profileAction = "set-handle";
          profile = new ui.TextButton("Create handle", {
            center: "xy",
            screen,
          });
          profile.stickyScrubbing = true; // Prevent drag-between-button behavior
          user.email_verified = true; // Set verified on global 'user' object.
          // store["aesthetic:refresh-user"] = true;
          // store.persist("aesthetic:refresh-user");
        }
        fetchingUser = false;
        flashColor = "lime";
        makeFlash(api, true, true);
      } else setTimeout(() => fetchUser(), 1000);
    })
    .catch((err) => setTimeout(() => fetchUser(), 1000));
}
