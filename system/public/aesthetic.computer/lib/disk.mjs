// Manages a piece and the transitions between pieces like a
// hypervisor or shell.

/* #region 🏁 todo
#endregion */

import * as graph from "./graph.mjs";
import * as num from "./num.mjs";
import * as text from "./text.mjs";
import * as geo from "./geo.mjs";
import * as gizmo from "./gizmo.mjs";
import * as ui from "./ui.mjs";
import * as help from "./help.mjs";
import * as platform from "./platform.mjs";
import { parse, metadata } from "./parse.mjs";
import { Socket } from "./socket.mjs"; // TODO: Eventually expand to `net.Socket`
import { notArray, defaultTemplateStringProcessor } from "./helpers.mjs";
const { round, sin, random, max, floor } = Math;
const { keys } = Object;
import { nopaint_boot, nopaint_act, nopaint_is } from "../systems/nopaint.mjs";
import * as prompt from "../systems/prompt-system.mjs";
import * as world from "../systems/world.mjs";
import { headers } from "./headers.mjs";
import { logs } from "./logs.mjs";
import { soundWhitelist } from "./sound/sound-whitelist.mjs";

import { TextInput, Typeface } from "../lib/type.mjs";
let tf; // Active typeface global.

export const noWorker = { onMessage: undefined, postMessage: undefined };

let ROOT_PIECE = "prompt"; // This gets set straight from the host html file for the ac.
let USER; // A holder for the logged in user. (Defined in `boot`)
let LAN_HOST; // The IP address of the hosting machine on the local network.
let SHARE_SUPPORTED; // Whether navigator.share is supported. (For `dl`)
//let IFRAME; // Flag if this aesthetic client is hosted in an iframe.
// (For development and IRL workshops)
let debug = false; // This can be overwritten on boot.
let visible = true; // Is aesthetic.computer visibly rendering or not?

const projectionMode = location.search.indexOf("nolabel") > -1; // Skip loading noise.

import { setDebug } from "../disks/common/debug.mjs";
import { nanoid } from "../dep/nanoid/nanoid.js";

const defaults = {
  boot: ({ cursor, screen: { width, height }, resolution, api }) => {
    if (location.host.indexOf("botce") > -1) resolution(width, height, 0);
    if (platform.AestheticExtension) resolution(width, height, 0);
    cursor("native");
  }, // aka Setup
  sim: () => false, // A framerate independent of rendering.
  paint: ({ noise16Aesthetic, noise16Sotce, slug, wipe, ink, screen, net }) => {
    // TODO: Make this a boot choice via the index.html file?
    if (!projectionMode) {
      if (slug?.indexOf("botce") > -1) {
        noise16Sotce();
      } else {
        noise16Aesthetic();
        if (net.loadFailureText) {
          ink("white").write(
            net.loadFailureText,
            { x: 6, y: 6 },
            [64, 64],
            screen.width - 6,
          );
        }
      }
    }
  },
  beat: () => false, // Runs every bpm.
  act: () => false, // All user interaction.
  leave: () => false, // Before unload.
  preview: ({ wipe, slug }) => {
    wipe(64).ink(255).write(slug, { center: "xy", size: 1 });
  },
  icon: ({ glaze, wipe }) => {
    glaze({ on: false });
    wipe(70, 50, 100)
      .ink(200, 30, 100)
      .box(screen.width / 2, screen.height / 2, 48, 72, "*center");
  },
};

let loadAfterPreamble = null;
let hotSwap = null;

// let jumpDelay; // a setInterval for jumping between disks or pages.

// let showHUD = true;

// 🔎 NoPaint
// Inheritable via `export const system = "nopaint"` from any piece.
// Boilerplate for a distributed raster editor.
const nopaint = {
  leave: function leave($) {
    const { store, system, page, screen, flatten } = $;
    if (NPnoOnLeave === false) {
      // ^ This is set when reloading a brush without storing changes. (`N` key)

      if (system.nopaint.bakeOnLeave) {
        // Add bake commands.
        page(system.painting);
        //$activePaintApi = $; // In case of recursive paint functions like `write`.
        bake($);
        // page(screen); // TODO: This should work but it doesnt...
        //                        Seems to have something to do with
        //                        the $activePaintAPI being set
        //                        and it's specific to the `write`
        //                        implementation.
        flatten(); // Force-run the above painting commands.
      }
      // Bake any changes into the existing painting using the screen buffer.
      // Note... this may not include the full API that `paint` has...
      // Which could lead to inconsistencies in `bake` calls.  23.05.04.13.01

      //  const p = system.nopaint.translation; // Offset by the pan position.
      //  page(system.painting).paste(screen, -p.x, -p.y); // TODO: Why the + 1 offset here...
      //  painting.paint(); // TODO: Why is this here?
      //  page(screen);
      //}

      addUndoPainting(system.painting, $.slug);

      // Idea: Check to see if anything actually got painted by doing a diff on
      //       the pixels?

      store["painting"] = system.painting; // Remember the painting data.
      store.persist("painting", "local:db");

      // And its transform.
      store["painting:transform"] = {
        translation: system.nopaint.translation,
        zoom: system.nopaint.zoomLevel,
      };
      store.persist("painting:transform", "local:db");
    } else {
      // Restore a painting if `no`ing.
      const paintings = system.nopaint.undo.paintings;
      page(system.painting)
        .paste(paintings[paintings.length - 1])
        .page(screen);
    }

    NPnoOnLeave = false;
  },
};

const undoPaintings = []; // Stores the last two paintings.
let undoPosition = 0;

function addUndoPainting(painting, step = "unspecified") {
  if (!painting) return; // If there is no painting present, silently pass.
  const op = painting.pixels;
  const pixels = new Uint8ClampedArray(op.length);
  pixels.set(op);

  if (undoPaintings.length > undoPosition + 1) {
    undoPaintings.length = undoPosition + 1;
  }

  if (undoPaintings.length > 0) {
    const lastPainting = undoPaintings[undoPaintings.length - 1];

    // Check for equality in the two states.
    // TODO: How long does this take?
    const eq =
      painting.width === lastPainting.width &&
      painting.height === lastPainting.height &&
      pixels.every((value, index) => value === lastPainting.pixels[index]);

    if (eq) {
      console.log("💩 The undo stack was not changed:", undoPaintings.length);
      return;
    }
  }

  undoPaintings.push({
    pixels,
    width: painting.width,
    height: painting.height,
  });

  if ($commonApi.system.nopaint.recording) {
    $commonApi.system.nopaint.addToRecord({
      label: step,
      painting: {
        pixels,
        width: painting.width,
        height: painting.height,
      },
    });
  }

  undoPosition = undoPaintings.length - 1;

  // Note: This could be extended to increase the size of the
  //       undo stack, and images could be diffed? 23.01.31.01.30
  const maxUndoSteps = 32;
  if (undoPaintings.length > maxUndoSteps) undoPaintings.shift();

  if (debug && logs.painting)
    console.log("💩 Added undo painting...", undoPaintings.length);
}

let system = null; // Used to add built-in templated behaviors like `nopaint`.
let boot = defaults.boot;
let sim = defaults.sim;
let paint = defaults.paint;
let beat = defaults.beat;
let act = defaults.act;
let leave = defaults.leave;
let preview = defaults.preview;
let icon = defaults.icon;
let bake; // Currently only used by the `nopaint` system.

let leaving = false; // Set to true on first piece.
let leaveLoad; // A callback for loading the next disk after leaving.

let previewMode = false; // Detects ?preview on a piece and yields its
//                          preview function if it exists.
let firstPreviewOrIcon = true;
let iconMode = false; // Detects ?icon on a piece and yields its
//                          icon function if it exists.
let hideLabel = false;

let module, loadedModule; // Currently loaded piece module code with an extra reference for `hotSwap`.
let currentPath,
  currentHost,
  currentSearch,
  currentColon,
  currentParams,
  currentHash,
  currentText,
  currentCode,
  currentHUDTxt,
  currentHUDTextColor,
  currentHUDStatusColor = "red",
  currentHUDButton,
  currentHUDOffset;
//currentPromptButton;

function updateHUDStatus() {
  if (udp.connected && socket?.connected) {
    currentHUDStatusColor = "lime";
  } else if (currentHUDStatusColor === "lime") {
    currentHUDStatusColor = "red";
  }
}

let loading = false;
let reframe;

let keyboardOpen = false; // A flag that gets flipped

const sfxProgressReceivers = {};

const signals = []; // Easy messages from embedded DOM content.
const actAlerts = []; // Messages that get put into act and cleared after
// every frame.
let reframed = false;

let screen;
let currentDisplay; // TODO: Remove this? 22.09.29.11.38
let cursorCode;
let pieceHistoryIndex = -1; // Gets incremented to 0 when first piece loads.
let paintCount = 0n;
let simCount = 0n;
let booted = false;
// let initialSim = true;
let noPaint = false;
let labelBack = false;

let storeRetrievalResolution, storeDeletionResolution;

let socket, socketStartDelay;
let udp = {
    send: (type, content) => {
      send({ type: "udp:send", content: { type, content } });
    },
    receive: ({ type, content }) => {
      // console.log("🩰 Received `piece` message from UDP:", content);

      // 🧚 Ambient cursor (fairies) support.
      if (type === "fairy:point" /*&& socket?.id !== id*/ && visible) {
        fairies.push({ x: content.x, y: content.y });
        return;
      }
      udpReceive?.(content);
    },
    kill: () => {
      udp.connected = false;
      send({ type: "udp:disconnect" });
    },
    connected: false,
  },
  udpReceive = undefined;
let scream = null; // 😱 Allow priviledged users to send alerts to everyone.
//                       (A great end<->end socket + redis test.)
let screaming = false;
let screamingTimer; // Keep track of scream duration.

const fairies = []; // Render cursor points of other active users,
//                              dumped each frame.

let glazeEnabled = false; // Keep track of whether glaze is on or off.

// *** Dark Mode ***
// Pass `true` or `false` to override or `default` to the system setting.
function darkMode(enabled = !$commonApi.dark) {
  if (enabled === "default") {
    // default
    store.delete("dark-mode");
    console.log("🌜 Dark mode:", $commonApi.dark);
    return $commonApi.dark;
  } else {
    // true or false
    store["dark-mode"] = enabled;
    store.persist("dark-mode");
    $commonApi.dark = enabled;
    console.log("🌜 Dark mode:", $commonApi.dark);
    return enabled;
  }
}

// *** Store ***
// This object is used to store and retrieve data across disks
// The `local` method encodes everything as a JSON string.

// Note: It strangely also contains an API which could be redefined
//       unintentionally. 22.10.15.01.57
// 22.11.25.11.07: The methods should be refactored out of the storage object / not share keys.
const store = {
  persist: function (key, method = "local") {
    // Send data over the thread through a key in this object.
    send({
      type: "store:persist",
      content: {
        key,
        data: this[key],
        method,
      },
    });
    // TODO: Turn the existing key into a retrieval function / promise?
  },
  retrieve: function (key, method = "local") {
    const promise = new Promise((resolve) => {
      storeRetrievalResolution = resolve;
    });

    send({
      type: "store:retrieve",
      content: {
        key,
        method,
      },
    });

    return promise;
  },
  delete: function (key, method = "local") {
    // Remove the key from the ram store, no matter what the method.
    delete store[key];

    const promise = new Promise((resolve) => {
      storeDeletionResolution = resolve;
    });

    send({
      type: "store:delete",
      content: {
        key,
        method,
      },
    });
    return promise;
  },
};

// Promise based API calls (through `bios` and back)
let fileImport;
let serverUpload, serverUploadProgressReporter;
let zipCreation;
let authorizationRequest;
let fileOpenRequest;
let fileEncodeRequest;
let gpuResponse;
let web3Response;

// Other
let activeVideo; // TODO: Eventually this can be a bank to store video textures.
let videoDeviceCount = 0;
let lastActiveVideo;
let videoSwitching = false;
let preloadPromises = {};
let inFocus;
let loadFailure;

// 1. ✔ API

// TODO: Eventually add a wiggle bank so all wiggles are indexed
//       and start at random angles.
// let wiggler = 0;
let wiggleAngle = 0;

// TODO; Change this to true and update all brushes.
// let NPdontPaintOnLeave = false;
let NPnoOnLeave = false;

// 🔴 Recorder (Singleton)
class Recorder {
  printProgress = 0;
  presentProgress = 0;
  printing = false; // Set by a callback from `bios`.
  printed = false; // "
  recording = false; // "
  rollingCallback;
  recorded = false; // "
  presenting = false; // "
  playing = false; // "
  cutCallback;
  printCallback;
  loadCallback;

  tapeTimerStart;
  tapeProgress = 0;
  tapeTimerDuration;

  videoOnLeave = false;

  constructor() {}

  tapeTimerSet(seconds, time) {
    this.tapeTimerStart = time;
    this.tapeTimerDuration = seconds;
  }

  tapeTimerStep({ needsPaint, sound: { time } }) {
    if (!this.tapeTimerDuration) return;
    this.tapeProgress = (time - this.tapeTimerStart) / this.tapeTimerDuration;
    needsPaint();
    const secondsOver =
      this.tapeProgress * this.tapeTimerDuration - this.tapeTimerDuration;
    // Run for an extra 150 milliseconds.
    if (this.tapeProgress >= 1 && secondsOver > 0.15) {
      this.tapeProgress = 0;
      this.tapeTimerStart = null;
      this.tapeTimerDuration = null;
      this.cut(() => $commonApi.jump("video"));
    }
  }

  slate() {
    send({ type: "recorder:slate" }); // Kill the MediaRecorder instance.
    // TODO: Should printing and playing also be set to false?
    //$commonApi.rec.printing = false; // "
    $commonApi.rec.recording = false; // Reset this singleton.
    $commonApi.rec.recorded = false; //
    $commonApi.rec.printed = false; // "
    $commonApi.rec.printProgress = 0; // "
  }

  rolling(opts, cb) {
    send({ type: "recorder:rolling", content: opts });
    this.rollingCallback = cb;
  }

  cut(cb) {
    $commonApi.rec.cutCallback = cb;
    this.tapeProgress = 0;
    this.tapeTimerStart = null;
    this.tapeTimerDuration = null;
    send({ type: "recorder:cut" });
  }

  print(cb) {
    $commonApi.rec.printCallback = cb;
    send({ type: "recorder:print" });
  }

  present(noplay = false) {
    this.presentProgress = 0;
    send({ type: "recorder:present", content: { noplay } });
  }

  unpresent() {
    send({ type: "recorder:unpresent" });
  }

  play() {
    send({ type: "recorder:present:play" });
  }

  pause() {
    send({ type: "recorder:present:pause" });
  }
}

let cachedAPI; // 🪢 This is a bit hacky. 23.04.21.14.59

const hourGlasses = [];

async function uploadPainting(picture, progress, handle, filename) {
  if (typeof picture === "string") {
    return { url: picture }; // Assume a URL and just return it.
  } else {
    // Assume picture is a buffer with `{ pixels, width, height }`
    // Upload it to the temporary bucket.
    filename ||= `painting-${num.timestamp()}.png`;

    try {
      const data = await $commonApi.upload(
        filename,
        picture,
        (p) => {
          console.log("Painting upload progress:", p);
          progress?.(p);
        },
        !handle ? "art" : undefined, // Store in temporary if no HANDLE.
      );
      console.log("🪄 Painting uploaded:", data.slug, data.ext, data.url);
      return data;
    } catch (err) {
      console.error("🪄 Painting upload failed:", err);
    }
  }
}

// Returns whether leaving a piece and also can overwrite the value.
function isLeaving(set) {
  if (set === true || set === false) leaving = set;
  return leaving;
}

// For every function to access.
const $commonApi = {
  // A wrapper for `load(parse(...))`
  // Make it `ahistorical` to prevent a url change.
  // Make it an `alias` to prevent a metadata change for writing landing or
  // router pieces such as `freaky-flowers` -> `wand`. 22.11.23.16.29
  // Jump delay...
  jump: function jump(to, ahistorical = false, alias = false) {
    // let url;
    if (leaving) {
      console.log("🚪🙅 Jump cancelled, already leaving...");
      return;
    }
    const jumpOut =
      to.startsWith("out:") || (to.startsWith("http") && platform.Aesthetic);

    if ((to.startsWith("http") && !to.endsWith(".mjs")) || jumpOut) {
      to = to.replace("out:", "");
      try {
        // url = new URL(to);
        $commonApi.net.web(to, jumpOut);
        return;
      } catch (e) {
        // Could not construct a valid url from the jump, so we will be
        // running a local aesthetic.computer piece.
        return;
      }
    } else {
      leaving = true;
    }

    function loadLine() {
      load(parse(to), ahistorical, alias, false, callback);
    }

    let callback;
    leaveLoad = () => {
      // Intercept returns to the prompt when taping from a piece directly.
      if ($commonApi.rec.videoOnLeave && to.split("~")[0] === "prompt") {
        to = "video";
        $commonApi.rec.videoOnLeave = false;
        $commonApi.rec.cut(loadLine);
      } else {
        loadLine(); // Or just load normally.
      }
    };
    return (cb) => (callback = cb);
  },
  canShare: false, // Whether navigator.share is enabled for mobile devices.
  leaving: isLeaving,
  handle: () => {
    return HANDLE;
  },
  notice: (msg, color = ["white", "green"]) => {
    notice = msg;
    noticeColor = color;
    const sound = {};
    if (color[0] === "yellow" && color[1] === "red") sound.tone = 300;
    noticeBell(cachedAPI, sound);
  },
  // ⌛ Delay a function by `time` number of sim steps.
  delay: (fun, time) => {
    hourGlasses.push(new gizmo.Hourglass(time, { completed: () => fun() }));
  },
  // Different syntax than `delay` but the same with looped behavior.
  blink: (time, fun) => {
    hourGlasses.push(
      new gizmo.Hourglass(time, { completed: () => fun(), autoFlip: true }),
    );
  },
  // 🎟️ Open a ticketed paywall on the page.
  // TODO: Get confirmation or cancellation of payment. 23.10.26.20.57
  ticket: (content) => {
    send({ type: "ticket-wall", content });
  },
  // 🪙 Mint a url or the `pixels` that get passed into the argument to a
  // network of choice.
  mint: async (picture, progress, params) => {
    console.log("🪙 Minting...", picture);
    // Determine if picture is a string or an object.

    // A record will be attached if one exists via the prompt and the user is
    // logged in.
    let filename;
    let zipped;
    if (picture.record && HANDLE) {
      const record = picture.record;
      filename = `painting-${record[record.length - 1].timestamp}.png`;
      // Set filename based on record.

      zipped = await $commonApi.zip(
        { destination: "upload", painting: { record } },
        (p) => {
          console.log("🤐 Zip progress:", p);
          progress?.(p);
        },
      );

      console.log("🤐 Zipped:", zipped);
    }

    const data = await uploadPainting(picture, progress, HANDLE, filename);

    let description;

    if (picture.width && picture.height) {
      description = `A ${picture.width}x${picture.height} pixel painting made on [aesthetic computer](https://aesthetic.computer).`;
    } else {
      description = `A painting made on [aesthetic computer](https://aesthetic.computer).`;
    }

    if (data) {
      // Redirect to Zora.
      if (HANDLE && zipped) {
        description = `[\` ${HANDLE}/${data.slug}\`](https://aesthetic.computer/painting~${HANDLE}/${data.slug})\n\n${description}`;
      }

      if (HANDLE) data.slug = `${HANDLE}/painting/${data.slug}`;

      const pixels = `https://aesthetic.computer/api/pixel/2048:contain/${data.slug}.${data.ext}`;

      // TODO: Also add the `zip` here?
      // Look into writing custom chunks into PNGS: https://chat.openai.com/c/0ed77e57-c5d2-4827-a733-c024da4bebd7

      $commonApi.jump(
        encodeURI(
          `https://zora.co/create/single-edition?image=${pixels}&name=${
            params[0] || "Untitled Painting"
          }&symbol=$${data.slug}&description=${description}`,
        ),
      );
    }
  },
  // 🖨️ Print either a url or the `pixels` that get passed into
  // the argument, with N quantity.
  print: async (picture, quantity = 1, progress) => {
    console.log("🖨️ Printing:", picture, "Quantity:", quantity);
    const data = await uploadPainting(picture, progress);
    let pixels;
    if (data && data.slug) {
      pixels = `${data.slug}.${data.ext}`;
    } else if (data) {
      pixels = data.url;
    }

    try {
      const headers = { "Content-Type": "application/json" };
      try {
        // Include authorization token if logged in.
        const token = await $commonApi.authorize(); // Get user token.
        if (token) headers.Authorization = `Bearer ${token}`;
      } catch (err) {} // Handled up-stream.

      const res = await fetch(`/api/print?new=true&pixels=${pixels}`, {
        method: "POST",
        headers,
        body: JSON.stringify({ quantity, slug: $commonApi.slug }),
        // TODO: Add order info here. ^
      });

      const data = await res.json();
      if (!res.ok)
        throw new Error(
          `🖨️ Print: HTTP error! Status: ${JSON.stringify(data)}`,
        );
      console.log("🖨️ Print order:", data);
      $commonApi.jump(data.location); // Redirect to checkout.
    } catch (error) {
      console.error("🖨️ Print order error:", error);
    }
  },
  // Create a zip file of specified content. (Used for storing painting data.)
  zip: (content, progress) => {
    const prom = new Promise((resolve, reject) => {
      zipCreation = { resolve, reject };
    });

    if (content.destination === "upload") {
      serverUploadProgressReporter = progress;
      serverUploadProgressReporter?.(0);
    }

    send({ type: "zip", content });
    return prom;
  },
  // Track device motion.
  motion: {
    start: () => {
      send({ type: "motion:start" });
    },
    stop: () => {
      send({ type: "motion:stop" });
      // TODO: Automatically stop when changing a disk?
    },
    current: {}, // Will get replaced by an update event.
  },
  speak: (utterance, voice, mode, opts) => {
    send({ type: "speak", content: { utterance, voice, mode, opts } });
  },
  // Broadcast an event through the entire act system.
  act: (event, data = {}) => {
    // console.log("Acted:", event); Show the synthetic event.
    data.is = (e) => e === event;
    cachedAPI.event = data;
    try {
      act(cachedAPI);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
  },
  // 🚥 `Get` api
  // Retrieve media assets from a user account.
  get: {
    painting: (code, opts) => {
      return {
        by: async function (handle = "anon", byOpts) {
          // Add support for pulling paintings from the `art` bucket...
          const extension = opts?.record ? "zip" : "png";
          if (handle === "anon") {
            return $commonApi.net.preload(
              encodeURI(`https://art.aesthetic.computer/${code}.${extension}`),
              true,
              undefined,
              byOpts,
            );
          } else {
            return $commonApi.net.preload(
              `/media/${handle}/painting/${code}.${extension}`,
              true,
              undefined,
              byOpts,
            );
          }
        },
      };
    },
  },
  // ***Actually*** upload a file to the server.
  // 📓 The file name can have `media-` which will sort it on the server into
  // a directory via `presigned-url.js`.
  upload: async (filename, data, progress, bucket) => {
    const prom = new Promise((resolve, reject) => {
      serverUpload = { resolve, reject };
    });
    serverUploadProgressReporter = progress;
    serverUploadProgressReporter?.(0);
    send({ type: "upload", content: { filename, data, bucket } });
    return prom;
  },
  code: {
    channel: (chan) => {
      codeChannel = chan; // Set the current `codeChannel`.
      store["code-channel"] = codeChannel; // Store and keep it in the browser.
      store.persist("code-channel");
      if (!codeChannel || codeChannel?.length === 0) {
        console.log("📭 Code channel cleared!");
      } else {
        console.log("📬 Code channel set to:", codeChannel);
      }
      socket.send("code-channel:sub", codeChannel);
      // Tell any parent iframes that the channel has been updated.
      send({
        type: "post-to-parent",
        content: { type: "setCode", value: codeChannel },
      });
      // ❤️‍🔥
      // TODO: Should return a promise here, and wait for a `code-channel:subbed`
      //       event, that way users get better confirmation if the socket
      //       doesn't go through / there is a server issue. 23.07.04.18.01
    },
  },
  // File should be { type, data } where type is "png", "webp", "jef", etc.
  encode: async (file) => {
    const prom = new Promise((resolve, reject) => {
      fileEncodeRequest = { resolve, reject };
    });
    send({ type: "file-encode:request", content: file });
    return prom;
  },
  file: async () => {
    const prom = new Promise((resolve, reject) => {
      fileOpenRequest = { resolve, reject };
    });
    send({ type: "file-open:request" });
    return prom;
  },
  // Authorize a user.
  authorize: async () => {
    // TODO: This should always fail while running user code.
    const prom = new Promise((resolve, reject) => {
      authorizationRequest = { resolve, reject };
    });
    send({ type: "authorization:request" });
    return prom;
  }, // Get a token for a logged in user.
  hand: { mediapipe: { screen: [], world: [], hand: "None" } }, // Hand-tracking. 23.04.27.10.19 TODO: Move eventually.
  hud: {
    label: (text, color, offset) => {
      currentHUDTxt = text;
      currentHUDTextColor = color;
      currentHUDOffset = offset;
    },
    currentStatusColor: () => currentHUDStatusColor,
    currentLabel: () => ({ text: currentHUDTxt, btn: currentHUDButton }),
    labelBack: () => {
      labelBack = true;
    },
  },
  send,
  platform,
  history: [], // Populated when a disk loads and sets the former piece.
  // Trigger background music.
  // Eventually add an "@" style parameter similar to what a stamp system would have.
  bgm: {
    set: function (trackNumber, volume) {
      send({ type: "bgm-change", content: { trackNumber, volume } });
    },
    stop: () => send({ type: "bgm-stop" }),
    data: {},
  },
  system: {
    // prompt: { input: undefined }, Gets set in `prompt_boot`.
    world: {
      // Populated in `world_boot` of `world.mjs`.
      teleported: false,
      telepos: undefined,
      teleport: (to, telepos) => {
        $commonApi.system.world.teleported = true;
        $commonApi.system.world.telepos = telepos;
        $commonApi.jump(to);
      },
    },
    nopaint: {
      //boot: nopaint_boot, // TODO: Why are these in the commonApi? 23.02.12.14.26
      // act: nopaint_act,
      buffer: null, // An overlapping brush buffer that gets drawn on top of the
      //              painting.
      recording: false,
      record: [], // Store a recording here.
      gestureRecord: [], // Store the active gesture.
      startRecord: function (fullText) {
        const sys = $commonApi.system;
        sys.nopaint.record = []; // Clear any existing recording.
        sys.nopaint.recording = true;
        sys.nopaint.addToRecord({
          label: fullText || "start",
          painting: {
            pixels: new Uint8ClampedArray(sys.painting.pixels),
            width: sys.painting.width,
            height: sys.painting.height,
          },
        });
        console.log("🖌️🔴 Now recording:", sys.nopaint.record);
      },
      addToRecord: function (record) {
        record.timestamp = num.timestamp(); // Insert the timestamp data.
        record.gesture = $commonApi.system.nopaint.gestureRecord.slice();
        if (record.gesture.length === 0) delete record.gesture;
        $commonApi.system.nopaint.gestureRecord = [];
        $commonApi.system.nopaint.record.push(record);
        store["painting:record"] = $commonApi.system.nopaint.record;
        store.persist("painting:record", "local:db");
        console.log("🖌️🟠 Recorded a step:", record.label);
      },
      is: nopaint_is,
      undo: { paintings: undoPaintings },
      needsBake: false,
      needsPresent: false,
      bakeOnLeave: false,
      addUndoPainting,
      // Regresses the system painting to a previous state.
      // Or the reverse... ("yes")
      no: ({ system, store, needsPaint }, yes = false) => {
        const paintings = system.nopaint.undo.paintings;

        let dontRecord = false;

        if (yes) {
          // ⏩ Fast-forward mode.
          undoPosition += 1;
          if (undoPosition > paintings.length - 1) {
            undoPosition = paintings.length - 1;
            dontRecord = true;
          }
        } else {
          // ⏪ Rewind mode.
          undoPosition -= 1;
          if (undoPosition < 0) {
            undoPosition = 0;
            dontRecord = true;
          }
        }

        if (paintings.length > 1) {
          // Copy over the old picture here...
          const p = paintings[undoPosition];

          const op = p.pixels;
          const pixels = new Uint8ClampedArray(op.length);
          pixels.set(op);

          store["painting"] = {
            width: p.width,
            height: p.height,
            pixels,
          };

          const resolutionChange =
            paintings[0].width !== paintings[1].width ||
            paintings[0].height !== paintings[1].height;

          // 🦢 Swap mode.
          // 'no' should swap...
          // const temp = paintings[0];
          // paintings[0] = paintings[1];
          // paintings[1] = temp;

          store.persist("painting", "local:db");

          system.painting = store["painting"];

          if (system.nopaint.recording && dontRecord === false) {
            const label = yes ? "yes" : "no";
            system.nopaint.addToRecord({
              label, //,
              // painting: {
              //   width: system.painting.width,
              //   height: system.painting.height,
              //   pixels: new Uint8Array(system.painting.pixels),
              // },
            });
          }

          if (resolutionChange) {
            system.nopaint.resetTransform({ system });
            system.nopaint.storeTransform(store, system);
          }

          needsPaint();
        }
      },
      // Center the picture within the screen / default translation.
      resetTransform: ({ system: sys }) => {
        sys.nopaint.zoomLevel = 1;

        if (!sys.painting) {
          sys.nopaint.translation = { x: 0, y: 0 };
          return;
        }

        sys.nopaint.translation.x = floor(
          screen.width / 2 - sys.painting.width / 2,
        );
        sys.nopaint.translation.y = floor(
          screen.height / 2 - sys.painting.height / 2,
        );
      },
      storeTransform: (store, sys) => {
        store["painting:transform"] = {
          translation: sys.nopaint.translation,
          zoom: sys.nopaint.zoomLevel,
        };
        store.persist("painting:transform", "local:db");
      },
      translation: { x: 0, y: 0 },
      zoomLevel: 1,
      translate: ({ system }, x, y) => {
        system.nopaint.translation.x += x;
        system.nopaint.translation.y += y;
      },
      // zoom: ({ system }, dir) => {
      //   system.nopaint.zoomLevel += dir === "in" ? 1 : -1;
      //   console.log("🔭 Zoom level:", system.nopaint.zoomLevel);
      //   if (system.nopaint.zoomLevel <= 0) system.nopaint.zoomLevel = 1;
      //   // TODO: Adjust the translation based on system.nopaint.brush.x and y
      //   //       Which would serve as the zoom origin point.
      // },
      zoom: ({ system }, dir, cursor) => {
        // Store old zoom level
        const oldZoomLevel = system.nopaint.zoomLevel;

        // Adjust the zoom level
        system.nopaint.zoomLevel += dir === "in" ? 1 : -1;

        // Ensure zoom level is at least 1
        if (system.nopaint.zoomLevel <= 0) system.nopaint.zoomLevel = 1;

        // Calculate the scaling factor based on the change in zoom levels
        const scale = system.nopaint.zoomLevel / oldZoomLevel;

        // Adjust the translation based on the scaling factor and the cursor's position
        system.nopaint.translation.x = floor(
          cursor.x + (system.nopaint.translation.x - cursor.x) * scale,
        );
        system.nopaint.translation.y = floor(
          cursor.y + (system.nopaint.translation.y - cursor.y) * scale,
        );
      },
      brush: { x: 0, y: 0, dragBox: undefined },
      transform: (p) => {
        return {
          x: (p.x - nopaintAPI.translation.x) / nopaintAPI.zoomLevel,
          y: (p.y - nopaintAPI.translation.y) / nopaintAPI.zoomLevel,
        };
      },
      // Similar to `updateBrush` but for arbitrary points,
      // with no change to `system.nopaint.brush`.
      pointToPainting: ({ system, pen }) => {
        const zoom = system.nopaint.zoomLevel;
        const x = Math.floor(
          ((pen?.x || 0) - system.nopaint.translation.x) / zoom,
        );
        const y = Math.floor(
          ((pen?.y || 0) - system.nopaint.translation.y) / zoom,
        );

        return { x, y };
      },
      updateBrush: ({ pen, system }, act) => {
        // TODO: Use `pointToPainting` above. 23.10.11.08.49
        // let { x, y } = system.nopaint.pointToPainting({ system });
        const zoom = system.nopaint.zoomLevel;
        const x = Math.floor(
          ((pen?.x || 0) - system.nopaint.translation.x) / zoom,
        );
        const y = Math.floor(
          ((pen?.y || 0) - system.nopaint.translation.y) / zoom,
        );

        if (act === "touch") system.nopaint.startDrag = { x, y };

        const dragBox = new geo.Box(
          system.nopaint.startDrag.x,
          system.nopaint.startDrag.y,
          x -
            system.nopaint.startDrag.x +
            (x >= system.nopaint.startDrag.x ? 1 : -1),
          y -
            system.nopaint.startDrag.y +
            (y >= system.nopaint.startDrag.y ? 1 : -1),
        );

        system.nopaint.brush = { x, y, dragBox };
      },

      // Helper to display the existing painting on the screen, with an
      // optional pan amount, that returns an adjusted pen pointer as `brush`.

      // TODO: - [] Add Zoom
      //       - [] And Rotation!

      present: ({ system, screen, wipe, paste, ink, slug }, tx, ty) => {
        system.nopaint.needsPresent = false;

        const x = tx || system.nopaint.translation.x;
        const y = ty || system.nopaint.translation.y;

        system.nopaint.translation = { x, y };

        const fullbleed =
          x === 0 &&
          y === 0 &&
          screen.width <= system.painting.width &&
          screen.height <= system.painting.height;

        if (fullbleed) {
          // If we are not panned and the painting fills the screen.
          paste(system.painting).paste(system.nopaint.buffer);
        } else {
          // If we are panned or the painting is a custom resolution.

          wipe(32)
            .paste(system.painting, x, y, system.nopaint.zoomLevel)
            .paste(system.nopaint.buffer, x, y, system.nopaint.zoomLevel)
            .ink(128)
            .box(
              x,
              y,
              system.painting.width * system.nopaint.zoomLevel,
              system.painting.height * system.nopaint.zoomLevel,
              "outline",
            );
        }

        // Graph `zoomLevel`
        if (system.nopaint.zoomLevel !== 1 && slug !== "prompt")
          ink(255, 127).write(`${system.nopaint.zoomLevel}x`, { x: 6, y: 18 });

        return {
          x,
          y, //,
          //brush: { x: (pen?.x || 0) - x, y: (pen?.y || 0) - y },
        };
      },
      // Kill an existing painting.
      noBang: async (
        { system, store, needsPaint, painting },
        res = { w: screen.width, h: screen.height },
      ) => {
        const deleted = await store.delete("painting", "local:db");
        await store.delete("painting:resolution-lock", "local:db");
        await store.delete("painting:transform", "local:db");
        system.nopaint.undo.paintings.length = 0; // Reset undo stack.
        system.painting = null;
        system.nopaint.resetTransform({ system, screen }); // Reset transform.
        needsPaint();

        // Make a blank painting.
        // I don't like that these getters will not re-associate.
        system.painting = painting(res.w, res.h, ($) => {
          $.wipe(64);
        });
        store["painting"] = $commonApi.system.painting;

        // Clear any existing painting recording in RAM and
        // storage.
        await store.delete("painting:record", "local:db");
        if (system.nopaint.recording) {
          system.nopaint.recording = false;
          system.nopaint.record.length = 0;
          console.log("🖌️🛑 Recording cleared.");
        }

        return deleted;
      },
      // Replace a painting entirely, remembering the last one.
      // (This will always enable fixed resolution mode.)
      replace: (
        { system, screen, store, needsPaint },
        painting,
        message = "(replace)",
      ) => {
        system.painting = painting; // Update references.
        store["painting"] = system.painting;
        store.persist("painting", "local:db"); // Persist to storage.
        store["painting:resolution-lock"] = true;
        store.persist("painting:resolution-lock", "local:db");
        system.nopaint.resetTransform({ system, screen }); // Reset transform.
        system.nopaint.storeTransform(store, system);
        system.nopaint.addUndoPainting(system.painting, message);
        system.nopaint.needsPresent = true;
        needsPaint();
      },
      abort: () => (NPnoOnLeave = true),
    },
  },
  // Paint all queued rendering commands immediately.
  flatten: () => {
    return painting.paint(true);
  },
  connect: () => {
    const p = new Promise((resolve, reject) => {
      web3Response = { resolve, reject };
    });
    send({ type: "web3-connect" });
    return p;
  },
  wiggle: function (n, level = 0.2, speed = 6) {
    wiggleAngle = (wiggleAngle + 1 * speed) % 360;
    const osc = sin(num.radians(wiggleAngle));
    return n + n * level * osc;
  },
  dark: true, // Dark mode. (Gets set on startup and on any change.)
  darkMode, // Toggle dark mode or set to `true` or `false`.
  // content: added programmatically: see Content class
  gpuReady: false,
  gpu: {
    message: (content) => {
      const p = new Promise((resolve, reject) => {
        gpuResponse = { resolve, reject };
      });
      send({ type: "gpu-event", content });
      return p;
    },
  },
  // Deprecated in favor of `bios` -> `hitboxes`. (To support iOS)
  // clipboard: {
  //   copy: (text) => {
  //     send({ type: "copy", content: text });
  //   },
  // },
  text: {
    capitalize: text.capitalize,
    box: (text, pos = { x: 0, y: 0 }, bounds, scale = 1) => {
      pos = { ...pos }; // Copy pos because it gets mutated.
      let run = 0;
      const blockWidth = 6 * scale; // TODO: Replace this `6`. 23.09.13.15.31
      const words = text.split(" ");
      const lines = [[]];
      let line = 0;

      if (bounds === undefined) bounds = (text.length + 2) * blockWidth;

      function newLine() {
        run = 0;
        line += 1;
        lines[line] = [];
      }

      // Word-wrapping with new line support.
      words.forEach((word) => {
        const wordLen = (word.length + 1) * blockWidth;
        if (word.includes("\n")) {
          const segs = word.split("\n");
          segs.forEach((seg, i) => {
            const segLen = (seg.length + 1) * blockWidth;
            if (run + segLen >= bounds || i > 0) newLine();
            lines[line].push(seg);
            run += segLen;
          });
        } else {
          if (run + wordLen >= bounds) newLine();
          lines[line].push(word);
          run += wordLen;
        }
      });

      const blockHeight = 11 * scale; // TODO: Replace `11`. 23.09.13.15.31

      if (lines.length >= 1 && pos.center && pos.center.indexOf("y") !== -1) {
        pos.y =
          $activePaintApi.screen.height / 2 -
          (lines.length * blockHeight) / 2 +
          blockHeight / 2 +
          (pos.y || 0);
      }

      const height = lines.length * blockHeight;
      const box = { x: pos.x, y: pos.y, width: bounds, height };

      return { pos, box, lines };
    },
  },
  num: {
    add: num.add,
    wrap: num.wrap,
    even: num.even,
    odd: num.odd,
    clamp: num.clamp,
    rand: num.rand,
    randInt: num.randInt,
    randInd: num.randInd,
    randIntArr: num.randIntArr,
    randIntRange: num.randIntRange,
    rangedInts: num.rangedInts,
    multiply: num.multiply,
    dist: num.dist,
    dist3d: num.dist3d,
    radians: num.radians,
    degrees: num.degrees,
    lerp: num.lerp,
    map: num.map,
    arrMax: num.arrMax,
    arrCompress: num.arrCompress,
    Track: num.Track,
    timestamp: num.timestamp,
    p2: num.p2,
    midp: num.midp,
    number: num.number,
    intersects: num.intersects,
    signedCeil: num.signedCeil,
    signedFloor: num.signedFloor,
    vec2: num.vec2,
    vec3: num.vec3,
    vec4: num.vec4,
    mat3: num.mat3,
    mat4: num.mat4,
    quat: num.quat,
    parseColor: num.parseColor,
    findColor: num.findColor,
    saturate: num.saturate,
    desaturate: num.desaturate,
    shiftRGB: num.shiftRGB,
    rgbToHexStr: num.rgbToHexStr,
    hexToRgb: num.hexToRgb,
    blend: num.blend,
    rgbToHsl: num.rgbToHsl,
    rainbow: num.rainbow,
  },
  geo: {
    Box: geo.Box,
    DirtyBox: geo.DirtyBox,
    Grid: geo.Grid,
    Circle: geo.Circle,
    linePointsFromAngle: geo.linePointsFromAngle,
    pointFrom: geo.pointFrom,
    Race: geo.Race,
    Quantizer: geo.Quantizer,
  },
  ui: {
    Button: ui.Button,
    TextButton: ui.TextButton,
    TextInput: TextInput,
  },
  help: {
    choose: help.choose,
    flip: help.flip,
    repeat: help.repeat,
    every: help.every,
    any: help.any,
    anyIndex: help.anyIndex,
    anyKey: help.anyKey,
    each: help.each,
    shuffleInPlace: help.shuffleInPlace,
  },
  gizmo: { Hourglass: gizmo.Hourglass, EllipsisTicker: gizmo.EllipsisTicker },
  rec: new Recorder(),
  net: {
    signup: () => {
      send({ type: "signup" });
    },
    login: () => {
      send({ type: "login" });
    }, // { email }
    logout: () => send({ type: "logout" }),
    pieces: `${location.protocol}//${location.host}/aesthetic.computer/disks`,
    parse, // Parse a piece slug.
    // lan: // Set dynamically.
    // host: // Set dynamically.
    // loadFailureText: // Set dynamically.
    // Make a user authorized / signed request to the api.
    // Used both in `motd` and `handle`.
    userRequest: async (method, endpoint, body) => {
      try {
        const token = await $commonApi.authorize(); // Get user token.
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
          const clonedResponse = response.clone();
          try {
            return {
              ...(await clonedResponse.json()),
              status: response.status,
            };
          } catch {
            return { status: response.status, body: await response.text() };
          }
        }
      } catch (error) {
        console.error("🚫 Error:", error);
        return { message: "unauthorized" };
      }
    },
    // Loosely connect the UDP receiver.
    udp: (receive) => {
      udpReceive = receive;
      return udp;
    },
  },
  needsPaint: () => {
    noPaint = false;
    if (system === "nopaint") $commonApi.system.nopaint.needsPresent = true;
  }, // TODO: Does "paint" needs this?
  store,
  pieceCount: -1, // Incs to 0 when the first piece (usually the prompt) loads.
  //                 Increments by 1 each time a new piece loads.
  debug,
};

const nopaintAPI = $commonApi.system.nopaint;

// Broadcast to other tabs in the same origin.
const channel = new BroadcastChannel("aesthetic.computer");

channel.onmessage = (event) => {
  console.log(`🗼 Got broadcast: ${event.data}`);
  processMessage(event.data);
};

function processMessage(msg) {
  console.log(`🗼 Processing broadcast: ${msg}`);
  if (msg.startsWith("handle:updated")) {
    // 👰‍♀️ Update the user handle if it changed.
    HANDLE = "@" + msg.split(":").pop();
    send({ type: "handle", content: HANDLE });
    store["handle:received"] = true;
  }
}

$commonApi.broadcast = (msg) => {
  processMessage(msg); // Process locally.
  channel.postMessage(msg);
};

// Spawn a session backend for a piece.
async function session(slug, forceProduction = false, service) {
  let endPoint = "/session/" + slug;
  const params = { service };
  if (forceProduction) params.forceProduction = 1;
  endPoint += "?" + new URLSearchParams(params);

  const req = await fetch(endPoint);

  let session;
  if (req.status === 200) {
    session = await req.text().then((text) => {
      try {
        return JSON.parse(text);
      } catch (e) {
        return text;
      }
    });
  } else {
    session = await req.text();
  }

  if (typeof session === "string") return session;

  //if (debug && logs.session) {
  console.log(
    `🐕‍🦺 Session: ${slug} - ${session.backend || session.name || session.url}`,
  );
  //}
  // Return the active session if the server knows it's "Ready", otherwise
  // wait for the one we requested to spin up.
  // (And in debug mode we just get a local url from "/session" so no need
  // to check that.)
  if (session.state === "Ready" || (debug && !forceProduction)) {
    return session;
  } else {
    let eventSource = new EventSource(
      `https://api.jamsocket.com/backend/${session.name}/status/stream`,
      // See also: https://docs.jamsocket.com/api-docs/#get-a-backends-status-stream
    );

    return new Promise((resolve, reject) => {
      eventSource.onmessage = (event) => {
        const update = JSON.parse(event.data);
        const colors = {
          Ready: "🟢",
          Loading: "🟠",
          Starting: "🟡",
        };
        const color = colors[update.state] || "🔵";
        console.log(color + " Backend:", update.state);

        if (update.state === "Loading") {
          currentHUDStatusColor = "red";
        } else if (update.state === "Ready") {
          currentHUDStatusColor = "yellow";
        } else if (update.state === "Starting") {
          currentHUDStatusColor = "orange";
        } else {
          currentHUDStatusColor = "brown";
        }

        $commonApi.needsPaint(); // Make sure the label gets updated.

        if (update.state === "Ready") {
          eventSource.close(); // Close the event stream handler.
          resolve(session);
        } else {
          if (update.state !== "Loading" && update.state !== "Starting") {
            eventSource.close(); // Close the event stream handler.
          }
        }
      };
    });
  }
}

// Just for "update".
const $updateApi = {};

// 🖼 Painting

// Pre-fab models:
const QUAD = {
  type: "quad",
  positions: [
    // Triangle 1 (Left Side)
    [-1, -1, 0, 1], // Bottom Left
    [-1, 1, 0, 1], // Top Left
    [1, 1, 0, 1], // Top Right
    // Triangle 2 (Right Side)
    [-1, -1, 0, 1], // Bottom Left
    [1, -1, 0, 1], // Bottom Right
    [1, 1, 0, 1], // Top Right
  ],
  indices: [
    // These are not re-used for now.
    // One
    0, 1, 2,
    //Two
    3, 4, 5,
  ],
};

// A cube of lines.
const CUBEL = {
  type: "line",
  positions: [
    // Back
    [-0.5, -0.5, 0.5, 1], // Down
    [-0.5, 0.5, 0.5, 1],

    [-0.5, 0.5, 0.5, 1], // Across
    [0.5, 0.5, 0.5, 1],

    [0.5, 0.5, 0.5, 1], // Up
    [0.5, -0.5, 0.5, 1],

    [0.5, -0.5, 0.5, 1], // Back
    [-0.5, -0.5, 0.5, 1],
    // Front
    [-0.5, -0.5, -0.5, 1], // Down
    [-0.5, 0.5, -0.5, 1],

    [-0.5, 0.5, -0.5, 1], // Across
    [0.5, 0.5, -0.5, 1],

    [0.5, 0.5, -0.5, 1], // Up
    [0.5, -0.5, -0.5, 1],

    [0.5, -0.5, -0.5, 1], // Back
    [-0.5, -0.5, -0.5, 1],
    // Bars (back to front)
    [-0.5, -0.5, 0.5, 1], // Top Left
    [-0.5, -0.5, -0.5, 1],

    [-0.5, 0.5, 0.5, 1], // Bottom Left
    [-0.5, 0.5, -0.5, 1],

    [0.5, 0.5, 0.5, 1], // Up
    [0.5, 0.5, -0.5, 1],

    [0.5, -0.5, 0.5, 1], // Back
    [0.5, -0.5, -0.5, 1],
  ],
};

const ORIGIN = {
  type: "line",
  positions: [
    [-0.5, 0, 0, 1], // Horizontal X
    [0.5, 0, 0, 1],
    [0, 0, -0.5, 1], // Horizontal Z
    [0, 0, 2, 1],
    [0, -0.5, 0, 1], // Vertical
    [0, 0.5, 0, 1],
  ],
  colors: [
    [255, 0, 0, 255],
    [255, 0, 0, 255],
    [0, 255, 0, 255],
    [0, 255, 0, 255],
    [0, 0, 255, 255],
    [0, 0, 255, 255],
  ],
};

const TRI = {
  type: "triangle",
  positions: [
    [-1, -1, 0, 1], // Bottom Left
    [0, 1, 0, 1], // Top Left
    [1, -1, 0, 1], // Top Right
    // Triangle 2 (Right Side)
  ],
  indices: [0, 1, 2],
};

const LINE = {
  type: "line",
  positions: [
    [0, 0, 0, 1], // Bottom
    [0, 1, 0, 1], // Top
  ],
  indices: [0, 1],
};

let SCREEN;

// Inputs: (r, g, b), (r, g, b, a) or an array of those.
//         (rgb) for grayscale or (rgb, a) for grayscale with alpha.
//         Or hex with "#000000" or "0x000000" or 0x000000.
// TODO: Add `erase` anc all css color alpha support. 23.07.20.14.45
// TODO: Add transparency and short hex to hex support.
// TODO: Add better hex support via: https://stackoverflow.com/a/53936623/8146077

function ink() {
  return graph.color(...graph.findColor(...arguments));
}

// 🔎 PAINTAPI
const $paintApi = {
  // 1. Composite functions (that use $activePaintApi)
  //    (Must be unwrapped)

  // Prints a line of text using the default / current global font.
  // Argument options:
  // text, pos: {x, y, center}, bg (optional)
  write: function (text, pos, bg, bounds) {
    if (!text || !tf) return $activePaintApi; // Fail silently if no text.
    text = text.toString();

    // 🎁
    // See if the text length is greater than the bounds, and if it is then
    // print on a new line.
    const scale = pos?.size || 1;

    if (bounds) {
      const tb = $commonApi.text.box(text, pos, bounds, scale);
      // TODO: Get the current ink color, memoize it, and make it static here.
      //       23.10.12.22.04
      tb.lines.forEach((line, index) => {
        tf?.print($activePaintApi, tb.pos, index, line.join(" "), bg);
      });
    } else {
      tf?.print($activePaintApi, pos, 0, text, bg);
    }

    return $activePaintApi;
  },
  // 2. Image Utilities
  clonePixels: graph.cloneBuffer,
  colorsMatch: graph.colorsMatch,
  color: graph.findColor,
  resize: graph.resize,
  // 3. 3D Classes & Objects
  Camera: graph.Camera,
  Form: graph.Form,
  Dolly: graph.Dolly,
  TRI,
  QUAD,
  LINE,
  CUBEL,
  ORIGIN,
};

// This is where I map the API functions that anyone can use, to the internal
// code that represents them...

// Rendering of 3D forms.

const formsToClear = [];
let backgroundColor3D = [0, 0, 0, 255];
let formsSent = {}; // TODO: This should be cleared more often...

// `cpu: true` enabled software rendering
function form(
  forms,
  cam,
  { cpu, background } = {
    cpu: false,
    keep: true,
    background: backgroundColor3D,
  },
) {
  // Exit silently if no forms are present.
  if (forms === undefined || forms?.length === 0) return;

  if (cpu === true) {
    if (Array.isArray(forms))
      forms.filter(Boolean).forEach((form) => form.graph(cam));
    else forms.graph(cam);
  } else {
    // GPU forms.
    if (!Array.isArray(forms)) forms = [forms];

    // Clear out any forms that need deleting.
    formsToClear.forEach((id) => delete formsSent[id]);
    formsToClear.length = 0;

    // Build a list of forms to send, ignoring already sent ones by UID.
    const formsToSend = [];

    forms.filter(Boolean).forEach((form) => {
      // Clear out any trash in `formsSent` that do not have IDs left in forms.
      //if (formsSent[forms.uid])

      // A. If the form has not been sent yet...
      if (formsSent[form.uid] === undefined && form.vertices.length > 0) {
        // Set the form to expire automatically if keep is false.
        formsToSend.push(form);
        formsSent[form.uid] = form;
        //console.log("Forms sent:", Object.keys(formsSent).length);
        form.gpuVerticesSent = form.vertices.length;
        form.gpuReset = false;
      } else {
        // B. If the form has been sent, but the form has changed and
        //    needs a partial state update or is simply being redrawn.
        let msgCount = 0;

        if (form.gpuRecolored === true) {
          formsToSend.push({
            update: "form:color",
            uid: form.uid,
            color: form.color,
          });
          msgCount += 1;
          form.gpuRecolored = false;
        }

        // Transform the geometry.
        if (form.gpuTransformed === true) {
          formsToSend.push({
            update: "form:transform",
            uid: form.uid,
            rotation: form.rotation,
            position: form.position,
            scale: form.scale,
          });
          form.gpuTransformed = false;
          msgCount += 1;
        }

        if (form.vertices.length > form.gpuVerticesSent || form.gpuReset) {
          // Add vertices to buffered forms.
          formsToSend.push({
            update: "form:buffered:add-vertices",
            uid: form.uid,
            reset: form.gpuReset,
            vertices: form.vertices.slice(form.gpuVerticesSent),
            length: form.vertices.length, // TODO: These aren't being used anymore / they are generated from the GPU.
            pastLength: form.gpuVerticesSent,
          });

          // Update form state now that we are sending the message.
          // TODO: Put these both under a "gpu" object in form.
          //console.log(form.gpuReset);
          form.gpuReset = false;
          form.gpuVerticesSent = form.vertices.length;
          msgCount += 1;
        }

        if (msgCount === 0) {
          // Simply tell the system we are still drawing the form... otherwise
          // it will get cleared.
          formsToSend.push({
            update: "form:touch",
            uid: form.uid,
          });
        }
      }
    });

    if (formsToSend.length === 0) return;

    // console.log("Sending form...", performance.now())

    // Only send a background update if the value changed.
    if (background !== backgroundColor3D) {
      send({
        type: "gpu-event",
        content: {
          type: "background-change",
          content: background,
        },
      });
      backgroundColor3D = background;
    }

    send({
      type: "forms",
      content: {
        forms: formsToSend,
        cam: {
          position: cam.position,
          rotation: cam.rotation,
          scale: cam.scale,
          fov: cam.fov,
          near: cam.near,
          far: cam.far,
        },
        color: graph.color(),
      },
    });

    // paintFormsResolution?.();
    // return new Promise((resolve) => {
    // paintFormsResolution = resolve;
    // });
  }
}

const $paintApiUnwrapped = {
  // Shortcuts
  // l: graph.line,
  // i: ink,
  // Defaults
  blend: graph.blendMode,
  page: graph.setBuffer,
  edit: graph.changePixels, // Edit pixels by pasing a callback.
  ink: function () {
    const out = ink(...arguments);
    twoDCommands.push(["ink", ...out]);
  }, // Color
  // inkrn: () => graph.c.slice(), // Get current inkColor.
  // 2D
  wipe: function () {
    ink(...arguments);
    graph.clear();
  },
  copy: graph.copy,
  paste: graph.paste,
  pixel: graph.pixel,
  plot: function () {
    if (arguments.length === 1) {
      graph.plot(arguments[0].x, arguments[0].y);
    } else {
      graph.plot(...arguments);
    }
  }, // TODO: Should this be renamed to set?
  flood: graph.flood,
  point: graph.point,
  line: function () {
    const out = graph.line(...arguments);
    // console.log(out);
    if (out) {
      twoDCommands.push(["line", ...out]);
    }
  },
  lineAngle: graph.lineAngle,
  pline: graph.pline,
  pppline: graph.pixelPerfectPolyline,
  oval: graph.oval,
  circle: graph.circle,
  poly: graph.poly,
  box: graph.box,
  shape: graph.shape,
  grid: graph.grid,
  draw: graph.draw,
  printLine: graph.printLine, // TODO: This is kind of ugly and I need a state machine for type.
  form,
  pan: graph.pan,
  unpan: graph.unpan,
  skip: graph.skip,
  noise16: graph.noise16,
  noise16DIGITPAIN: graph.noise16DIGITPAIN,
  noise16Aesthetic: graph.noise16Aesthetic,
  noise16Sotce: graph.noise16Sotce,
  noiseTinted: graph.noiseTinted,
  // glaze: ...
};

// TODO: Eventually restructure this a bit. 2021.12.16.16.0
//       Should global state like color and transform be stored here?

let $activePaintApi;

let paintingAPIid = 0n;

const twoDCommands = [];

class Painting {
  #layers = [];
  #layer = 0;
  api = {};
  inkrn;

  constructor() {
    Object.assign(this.api, $paintApi);
    const p = this;

    p.api.index = paintingAPIid;
    paintingAPIid += 1n;

    p.inkrn = graph.c.slice(); // Init global state machine read-outs.

    // Filter for and then wrap every rendering behavior of $paintApi into a
    // system to be deferred in groups, using layer.
    // ⛓️ This wrapper also makes the paint API chainable.

    function globals(k, args) {
      if (k === "ink") p.inkrn = [...args].flat();
      // TODO: 😅 Add other state globals like line thickness? 23.1.25
    }

    for (const k in $paintApiUnwrapped) {
      if (typeof $paintApiUnwrapped[k] === "function") {
        // Wrap and then transfer to #api.
        p.api[k] = function () {
          globals(k, arguments); // Keep track of global state, like ink, via `inkrn`.
          // Create layer if necessary.
          if (notArray(p.#layers[p.#layer])) p.#layers[p.#layer] = [];
          // Add each deferred paint api function to the layer, to be run
          // all at once in `paint` on each frame update.
          p.#layers[p.#layer].push([
            k,
            () => {
              globals(k, arguments); // Update globals again on chainable calls.
              $paintApiUnwrapped[k](...arguments);
            },
          ]);
          return p.api;
        };
      }
    }

    // Allows grouping & composing painting order using an AofA (Array of Arrays).
    // n: 0-n (Cannot be negative.)
    // fun: A callback that contains $paintApi commands or any other code.
    this.api.layer = function (n) {
      p.#layer = n;
      // TODO: ❤️‍🔥 Current layer needs to be set on each API state...!
      return p.api;
    };

    // Creates a new pixel buffer with its own layering wrapper / context
    // on top of the base painting API.
    this.api.painting = function () {
      const oldActivePaintApi = $activePaintApi;
      const painting = new Painting();
      $activePaintApi = painting.api;
      // Mock out the screen here using the arguments.
      $activePaintApi.screen = {
        width: arguments[0],
        height: arguments[1],
      };
      const pix = graph.makeBuffer(...arguments, painting, $activePaintApi);
      $activePaintApi = oldActivePaintApi;
      return pix;
    };

    this.api.pixel = function () {
      return graph.pixel(...arguments);
    };

    this.api.inkrn = () => this.inkrn; // Return current ink color.

    // This links to abstract, solitary graph functions that do not need
    // to be wrapped or deferred for rendering.
    // TODO: Maybe these functions should be under a graphics algorithms label?
    this.api.abstract = { bresenham: graph.bresenham };
  }

  // Paints every layer.
  //async paint(immediate = false) {
  paint(immediate = false) {
    for (let layer of this.#layers) {
      layer ||= []; // Evaporate empty layers.
      for (const paint of layer) {
        // if (immediate) {
        // console.log("Label:", paint[0]);
        paint[1]();
        // } else {
        // await paint();
        // }
      }
    }
    this.#layers.length = 0;
    this.#layer = 0;
  }
}

const painting = new Painting();

let glazeAfterReframe;

// *** Resolution ***
// Accepts width, height and gap either as numbers or as
// an object with those keys.
//
// Usage: resolution(64);
//        resolution(320, 240);
//        resolution(display); // "display" is a global object whose width
//                                 and height matches the hardware display
//                                 hosting aesthetic.computer.
let lastGap = 8;
$commonApi.resolution = function (width, height = width, gap = 8) {
  if (typeof width === "object") {
    const props = width;
    height = props.height;
    width = props.width || props.height;
    gap = props.gap === 0 ? 0 : props.gap || 8;
  }

  if (typeof width === "number" && typeof height === "number") {
    width = round(width);
    height = round(height);
  }

  // Don't do anything if there is no change and no gap update.
  if (screen.width === width && screen.height === height && gap === lastGap)
    return;

  lastGap = gap;

  // width = width || currentDisplay.innerWidth;
  // height = height || currentDisplay.innerHeight;

  // TODO: Paint anything that needs to be painted before resizing...
  // TODO: Does this even work right now?
  painting.paint();

  if (width === undefined && height === undefined) {
    // 1. Generate a new width and height.
    width = round(currentDisplay.width / currentDisplay.subdivisions);
    height = round(currentDisplay.height / currentDisplay.subdivisions);
    // Build a reframe request that will be sent to the main thread, mirroring this.
    reframe = {
      width: undefined,
      height: undefined,
      gap,
    };
  } else {
    // 2. Manually set the width and height.
    reframe = { width, height, gap };
  }

  console.log(
    "🖼 Reframe to:",
    width,
    height,
    "from",
    screen.width,
    screen.height,
  );

  // 3. Assign the generated or manual width and height.
  const oldScreen = {
    width: screen.width,
    height: screen.height,
    pixels: screen.pixels,
  };

  screen.width = width;
  screen.height = height;

  // Reset / recreate the depth buffer. (This is only used for the 3D software renderer in `graph`)
  // graph.depthBuffer.length = screen.width * screen.height;
  // graph.depthBuffer.fill(Number.MAX_VALUE);
  // graph.writeBuffer.length = screen.width * screen.height;
  // graph.writeBuffer.fill(Number.MAX_VALUE);

  screen.pixels = new Uint8ClampedArray(screen.width * screen.height * 4);
  screen.pixels.fill(255);

  graph.setBuffer(screen);
  graph.paste({
    painting: oldScreen,
    crop: new geo.Box(0, 0, oldScreen.width, oldScreen.height),
  });
};

// Add new content to the DOM.
// (Requires `send`)
class Content {
  nodes = [];
  #id = 0;

  constructor() {}

  // Make a request to add new content to the DOM.
  add(content) {
    // if (debug) console.log("📃 Adding content:", content);
    this.nodes.push({ id: this.#id });
    this.#id = this.nodes.length - 1;
    send({ type: "content-create", content: { id: this.#id, content } });
    return this.nodes[this.nodes.length - 1];
  }

  remove() {
    send({ type: "content-remove" });
    this.nodes = [];
    this.#id = 0;
  }

  receive({ id, response }) {
    this.nodes[id].response = response;
  }

  //update({ id, msg }) {
  //  send({ type: "content-update", content: { id, msg } });
  //}
}

// 🔈 Sound

// Microphone State (Audio Input)
class Microphone {
  amplitude = 0;
  waveform = [];
  pitch = 0;
  connected = false; // Flips to true on a callback message from `bios`.
  recordingPromise;

  // Note: can send `{monitor: true}` in `options` for audio feedback.
  connect(options) {
    send({ type: "microphone", content: options });
    return this;
  }

  disconnect() {
    send({ type: "microphone", content: { detach: true } });
  }

  poll() {
    send({ type: "get-microphone-amplitude" });
    send({ type: "get-microphone-waveform" });
    send({ type: "get-microphone-pitch" });
  }

  // Start recording.
  rec() {
    send({ type: "microphone:record" });
  }

  // Stop recording.
  cut() {
    const prom = new Promise((resolve, reject) => {
      this.recordingPromise = { resolve, reject };
    });
    send({ type: "microphone:cut" });
    return prom;
  }
}

class Speaker {
  waveforms = { left: [], right: [] };
  amplitudes = { left: 0, right: 0 };

  poll() {
    send({ type: "get-speaker-waveforms" });
    send({ type: "get-speaker-amplitudes" });
  }
}

let sound,
  soundClear, // Used by receivedBeat and defined in first frame update.
  soundId = 0n, // Increment each sound / give it an id in the `bios`.
  soundTime; // Used by `$sound.synth` for global timing.

sound = {
  bpm: undefined,
  sounds: [],
  bubbles: [],
  kills: [],
};

const speaker = new Speaker();
const microphone = new Microphone();

// 2. ✔ Loading the disk.
let originalHost;
let firstLoad = true;

let notice, noticeTimer, noticeColor; // Renders a full-screen notice on piece-load if present.

async function load(
  parsed, // If parsed is not an object, then assume it's source code.
  fromHistory = false,
  alias = false,
  devReload = false,
  loadedCallback,
) {
  let fullUrl, source;
  let params,
    search,
    colon,
    hash,
    path,
    host = originalHost,
    slug;

  if (loading === false) {
    loading = true;
  } else {
    // TODO: If the piece is different, then there should be a way to abort
    //       or ignore a current load.
    console.warn(
      "Coudn't load:",
      parsed.path || parsed.name,
      "(Already loading.)",
    );
    return true;
  }

  // 🕸️ Loading over the network from a parsed path object with no source code.
  if (!parsed.source) {
    params = parsed.params;
    path = parsed.path;
    search = parsed.search;
    colon = parsed.colon;
    hash = parsed.hash;
    host = parsed.host;
    slug = parsed.text;

    // 👱 Route to the `profile` piece if we are just hitting an empty
    // username.
    if (slug.startsWith("@") && slug.indexOf("/") === -1) {
      params = [slug, ...params]; // Rewrite all params for `@user` slug urls.
      //slug = "profile"; // Go to `profile` instead of the `@user`.
      const hiddenSlug = "profile";
      // Rewrite path to `profile`.
      console.log("Profile Path:", path);
      path = [...path.split("/").slice(0, -1), hiddenSlug].join("/");
    }

    if (debug) console.log(debug ? "🟡 Development" : "🟢 Production");
    if (host === "") host = originalHost;
    loadFailure = undefined;
    host = host.replace(/\/$/, ""); // Remove any trailing slash from host.
    //                                 Note: This fixes a preview bug on teia.art. 2022.04.07.03.00

    if (path === "") path = ROOT_PIECE; // Set bare path to what "/" maps to.
    // if (path === firstPiece && params.length === 0) params = firstParams;

    fullUrl =
      location.protocol + "//" + host + "/" + path + ".mjs" + "#" + Date.now();
    // The hash `time` parameter busts the cache so that the environment is
    // reset if a disk is re-entered while the system is running.
    // Why a hash? See also: https://github.com/denoland/deno/issues/6946#issuecomment-668230727
    if (debug) console.log("🕸", fullUrl);
  } else {
    // 📃 Loading with provided local source code.
    //    Check to see if we are subscribed to thr right codeChannel only
    //    on devReload (coming from the server)
    if (
      devReload === true &&
      (parsed.codeChannel === undefined || parsed.codeChannel !== codeChannel)
    ) {
      console.warn(
        "🙅 Not reloading, code signal invalid:",
        codeChannel || "N/A",
      );
      return;
    }

    source = parsed.source;
    slug = parsed.name;
    path = "aesthetic.computer/disks/" + slug;
    // 📓 Might need to fill in hash, path, or slug here. 23.06.24.18.49

    if (devReload) {
      // Remember the source and slug for the `publish` command.
      store["publishable-piece"] = { source, slug };
    }
  }

  // 🅱️ Load the piece.
  // const moduleLoadTime = performance.now();
  let blobUrl, sourceCode;
  try {
    // If this is a reload (with no source change) then just create a new
    // blobURL off the old source.
    if (
      slug.split("~")[0] === currentText?.split("~")[0] &&
      sourceCode == currentCode &&
      !devReload
    ) {
      const blob = new Blob([currentCode], { type: "application/javascript" });
      blobUrl = URL.createObjectURL(blob);
      sourceCode = currentCode;
    } else {
      let response, sourceToRun;
      if (fullUrl) {
        // console.log("Attempting to load from local url:", fullUrl);
        response = await fetch(fullUrl);
        if (response.status === 404) {
          throw new Error("404");
        }
        sourceToRun = await response.text();
      } else {
        sourceToRun = source;
      }

      /*
      if (sourceToRun.startsWith("// 404")) {
        let found = false;
        try {
          // Piece not found... try the guest server...
          const fullUrl = `https://art.aesthetic.computer/${
            path.split("/").slice(-1)[0]
          }.mjs#${Date.now()}`;
          console.warn("Local load failed. Attempting to run from: ", fullUrl);
          response = await fetch(fullUrl);
          sourceToRun = await response.text();
          found = true; // Found a piece on the guest server!
        } catch (err) {
          console.warn("😢 No guest piece found.");
        }

        if (!found) {
          if (!firstLoad) {
            throw new Error("📄 Piece not found.");
          } else {
            console.log("📄🚫 Piece not found:", slug);
          }
        }
      }
      */

      // Automatically replace relative imports with absolute ones.
      const twoDots =
        /^(import|export) {([^{}]*?)} from ["'](\.\.\/|\.\.|\.\/)(.*?)["'];?/gm;
      const oneDot =
        /^(import|export) \* as ([^ ]+) from ["']\.?\/(.*?)["'];?/gm;

      let updatedCode = sourceToRun.replace(
        twoDots,
        (match, p1, p2, p3, p4) => {
          let url = `${location.protocol}//${host}/aesthetic.computer${
            p3 === "./" ? "/disks" : ""
          }/${p4.replace(/\.\.\//g, "")}`;
          return `${p1} { ${p2} } from "${url}";`;
        },
      );

      updatedCode = updatedCode.replace(oneDot, (match, p1, p2, p3) => {
        let url = `${location.protocol}//${host}/aesthetic.computer${
          p3.startsWith("disks/") ? "" : "/disks"
        }/${p3.replace(/^disks\//, "")}`;
        return `${p1} * as ${p2} from "${url}";`;
      });

      // 💉 Constant Injection (for pieces to use)
      // Inject the DEBUG constant into the updatedCode
      updatedCode = `const DEBUG = ${debug};\n${updatedCode}`;

      const blob = new Blob([updatedCode], { type: "application/javascript" });
      blobUrl = URL.createObjectURL(blob);
      sourceCode = updatedCode;
    }

    loadedModule = await import(blobUrl);
  } catch (err) {
    // 🧨 Continue with current module if one has already loaded.
    console.error(
      `😡 "${path}" load failure:`,
      err,
      "💾 First load:",
      firstLoad,
    );
    loadFailure = err;
    $commonApi.net.loadFailureText = err.message + "\n" + sourceCode;
    loading = false;
    // Only return a 404 if the error type is correct.
    if (firstLoad && err.message === "404") {
      $commonApi.jump(`404~${slug}`);
    } else {
      $commonApi.notice(":(", ["red", "yellow"]);
    }
    return false;
  }

  // console.log("Module load time:", performance.now() - moduleLoadTime, module);

  // 🧨 Fail out if no module is found.
  if (loadedModule === undefined) {
    loading = false;
    leaving = false;
    return false;
  }

  // 🧩 Piece code has been loaded...
  //    Now we can instantiate the piece.

  pieceHistoryIndex += fromHistory === true ? 0 : 1; // Adjust the history.

  if (!debug && !firstLoad) {
    // console.clear();
    headers(); // Clear console and re-print headers if we are in production.
  }

  console.log("🧩", path, "🌐", host);

  // Add debug to the common api.
  $commonApi.debug = debug;

  // Add reload to the common api.
  $commonApi.reload = ({ piece, name, source, codeChannel } = {}) => {
    if (loading) {
      console.log("🟡 A piece is already loading.");
      return;
    }

    if (piece === "*refresh*") {
      console.log("💥️ Restarting system...");
      send({ type: "refresh" }); // Refresh the browser.
    } else if (name && source) {
      // TODO: Check for existence of `name` and `source` is hacky. 23.06.24.19.27
      // Note: This is used for live development via the socket server.
      $commonApi.load({ source, name, codeChannel }, false, false, true); // Load source code.
    } else if (piece === "*" || piece === undefined || currentText === piece) {
      console.log("💾️ Reloading current piece...", piece);
      const devReload = true;
      $commonApi.pieceCount = -1; // Reset pieceCount on developer reload.
      //                             (This can be disabled while testing pieces
      //                              that rely on pieceCount increments)
      $commonApi.load(
        {
          path: currentPath,
          host: currentHost,
          search: currentSearch,
          colon: currentColon,
          params: currentParams,
          hash: currentHash,
          text: currentText,
        },
        // Use the existing contextual values when live-reloading in debug mode.
        true, // (fromHistory) ... never add any reload to the history stack
        alias,
        devReload,
      );
    } else if (piece !== undefined) {
      // TODO: Make this happen...
      console.log(piece, currentText);
      if (debug) console.log("⚠️ Could jump instantly to another piece here in development...");
    }
  };

  // Start the socket server
  // TODO: Before we load the disk, in case of needing to reload remotely on failure? 23.01.27.12.48
  let receiver; // Handles incoming messages from the socket.
  const forceProd = false; // For testing prod socket servers in development.
  // TOOD: Hoist this to an environment variable?

  // Requests a session-backend and connects via websockets.
  function startSocket() {
    if (
      parsed.search?.startsWith("preview") ||
      parsed.search?.startsWith("icon")
    ) {
      console.log("🧦 Sockets disabled, just grabbing screenshots. 😃");
      return;
    }
    // Never open socket server in icon / preview mode.
    if (debug && logs.session) console.log("🫂 Finding session server...");
    socket = new Socket(debug, send); // Then redefine and make a new socket.

    const monolith = undefined; // "monolith"; // or `undefined` for horizontal scaling via
    // jamstack

    session(slug, forceProd, monolith)
      .then((sesh) => {
        if (typeof sesh === "string") throw new Error(sesh); // Cancel if error.
        const url = new URL(sesh.url); // Parse the url.
        const udpUrl = new URL(sesh.udp); // Parse the udp url.

        // 🩰 UDP... (via `bios`)
        send({
          type: "udp:connect",
          content: {
            url: `https://${udpUrl.hostname}`,
            port: debug && !forceProd ? 8889 : 443,
          },
        });

        // 🕸️ Web Sockets
        socket?.connect(
          url.host,
          (id, type, content) => {
            // Globally receivable messages...
            // (There are also some messages handled in `Socket`)
            // 😱 Scream at everyone who is connected!
            if (type === "scream" && socket?.id !== id) {
              console.log("😱 Scream:", content, "❗");
              scream = content;
              // return;
            }

            // 🧚 Ambient cursor (fairies) support.
            // if (type === "ambient-pen:point" && socket?.id !== id && visible) {
            // fairies.push({ x: content.x, y: content.y });
            // return;
            // }

            // 🧩 Pieces get all other messages not caught in `Socket`.
            receiver?.(id, type, content); // Run the piece receiver.
          },
          $commonApi.reload,
          "wss",
          () => {
            // Post-connection logic.
            if (codeChannel) socket?.send("code-channel:sub", codeChannel);
            updateHUDStatus();
            $commonApi.needsPaint();
            codeChannelAutoLoader?.();
            // setTimeout(function () {
            //   currentHUDStatusColor = undefined;
            // }, 250);
          },
          () => {
            // Post-disconnection logic.
            updateHUDStatus();
          },
        );
      })
      .catch((err) => {
        console.error("Session connection:", {
          Error: JSON.parse(err?.message) || err,
        });
      });
  }

  // End the socket connection before switching pieces if one exists.
  // socket?.kill();
  // udp?.kill();
  // socket = undefined;

  // Delay session server by .75 seconds in order to prevent redundant
  //  connections being opened as pieces are quickly re-routing and jumping.
  clearTimeout(socketStartDelay);
  socket?.kill(); // Kill any already open socket from a previous disk.
  udp?.kill();
  socket = undefined;
  socketStartDelay = setTimeout(() => startSocket(), 250);

  $commonApi.net.socket = function (receive) {
    receiver = receive || (() => {});
    if (!socket) {
      // Just in case we init. in a `boot` before the timeout fires above.
      clearTimeout(socketStartDelay);
      startSocket();
    } else {
      // Return the server then send an already connected message.
      setTimeout(() => {
        if (socket?.id) receiver(socket.id, "connected:already");
      }, 10);
    }
    return socket;
  };

  // ***Client Metadata Fields***
  // Set default metadata fields for SEO and sharing,
  // (requires serverside prerendering, also via `index.js`).
  let meta;

  if (alias === false) {
    // Parse any special piece metadata.
    const { title, desc, ogImage, twitterImage, icon } = metadata(
      location.host, // "aesthetic.computer",
      slug,
      // Adding the num API here is a little hacky, but needed for Freaky Flowers random metadata generation. 22.12.27
      loadedModule.meta?.({
        ...parsed,
        num: $commonApi.num,
        store: $commonApi.store,
      }),
    );

    meta = {
      title,
      desc, // Note: This doesn't auto-update externally hosted module descriptions, and may never need to? 22.07.19.06.00
      img: {
        og: ogImage,
        twitter: twitterImage,
        icon,
      },
      url: "https://aesthetic.computer/" + slug,
    };
  }

  // Add meta to the common api so the data can be overridden as needed.
  $commonApi.meta = (data) => send({ type: "meta", content: data });

  $commonApi.gap = function (newGap) {
    console.log("🟡 Gap has been deprecated. Use `resize` instead.");
  };

  // TODO: Eventually remove this deprecation notice. 22.09.29.11.07
  $commonApi.density = function (newDensity) {
    console.log("Density has been deprecated. Use `resize` instead.");
  };

  // Rewrite a new URL / parameter path without affecting the history.
  $commonApi.net.rewrite = (path, historical = false) => {
    if (historical) $commonApi.history.push(path);
    send({ type: "rewrite-url-path", content: { path, historical } }); // Jump the browser to a new url.
  };

  // Add host to the networking api.
  $commonApi.net.host = host;

  // Jump the browser to a new url.
  $commonApi.net.web = (url, jumpOut) => {
    send({ type: "web", content: { url, blank: jumpOut } });
  };

  $commonApi.net.refresh = () => {
    send({ type: "refresh" });
  };

  $commonApi.net.waitForPreload = () => {
    send({ type: "wait-for-preload", content: true }); // Tell the browser to wait until preloading is finished before painting.
  };

  $commonApi.net.preloaded = () => {
    send({ type: "preload-ready", content: true }); // Tell the browser that all preloading is done.
  };

  $commonApi.content = new Content();

  $commonApi.dom = {};

  $commonApi.dom.html = (strings, ...vars) => {
    const processed = defaultTemplateStringProcessor(strings, ...vars);
    $commonApi.content.add(processed);
  };

  $commonApi.dom.css = (strings, ...vars) => {
    const processed = defaultTemplateStringProcessor(strings, ...vars);
    $commonApi.content.add(`<style>${processed}</style>`);
  };

  $commonApi.dom.javascript = (strings, ...vars) => {
    const processed = defaultTemplateStringProcessor(strings, ...vars);
    $commonApi.content.add(`<script>${processed}</script>`);
  };
  // 💾 Uploading + Downloading
  // Add download event to trigger a file download from the main thread.
  $commonApi.download = (filename, data, modifiers) => {
    send({ type: "download", content: { filename, data, modifiers } });
  };

  // * Preload *
  // Add preload to the boot api.
  // Accepts paths local to the original disk server, full urls, and demos.
  // Usage:   preload("demo:drawings/2021.12.12.17.28.16.json") // pre-included
  //          preload("https://myserver.com/test.json") // remote
  //          preload("drawings/default.json") // hosted with disk
  // Results: preload().then((r) => ...).catch((e) => ...) // via promise

  $commonApi.net.preload = async function (
    path,
    parseJSON = true,
    progressReport,
    options = {},
  ) {
    let extension;

    const rejection = (reject) => {
      reject(new DOMException("Aborted", "AbortError"));
    };

    if (soundWhitelist.includes(path)) {
      extension = "m4a";
    } else {
      if (typeof path === "object") {
        extension = path.extension;
        path = path.path;
      } else {
        extension = path.split(".").pop().split("?")[0];
      }

      if (path.indexOf("/") === 0) path = path.slice(1);

      try {
        const url = new URL(path);
        if (url.protocol === "demo:") {
          path = `/demo/${url.pathname}`;
        } else if (url.protocol !== "https:") {
        }
      } catch {
        path = `${location.protocol}//${location.host}/${path}`;
      }
    }

    if (extension === "json") {
      return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();

        options.signal?.addEventListener("abort", () => {
          xhr.abort();
          rejection(reject);
        });

        xhr.open("GET", path, true);
        xhr.onprogress = function (event) {
          const progress = Math.min(event.loaded / event.total, 1);
          if (debug && logs.download) {
            console.log(`💈 JSON Download: ${progress * 100}%`);
          }
          progressReport?.(progress);
        };
        xhr.onload = function () {
          if (xhr.status === 200) {
            resolve(parseJSON ? JSON.parse(xhr.response) : xhr.response);
          } else {
            reject(xhr.status);
          }
        };
        xhr.onerror = reject;
        xhr.send();
      });
    } else if (
      extension === "webp" ||
      extension === "jpg" ||
      extension === "gif" ||
      extension === "jpeg" ||
      extension === "png"
    ) {
      return new Promise((resolve, reject) => {
        if (options.signal?.aborted) {
          rejection(reject);
          return;
        }

        send({ type: "load-bitmap", content: path });
        preloadPromises[path] = { resolve, reject };

        options.signal?.addEventListener("abort", () => {
          send({ type: "load:abort", content: path });
          rejection(reject);
        });
      });
    } else if (
      extension === "m4a" ||
      extension === "wav" ||
      extension === "mp3"
    ) {
      return new Promise((resolve, reject) => {
        if (options.signal?.aborted) {
          rejection(reject);
          return;
        }

        send({ type: "sfx:load", content: path });
        preloadPromises[path] = { resolve, reject };

        options.signal?.addEventListener("abort", () => {
          rejection(reject);
        });
      });
    } else if (extension === "zip") {
      return new Promise((resolve, reject) => {
        if (options.signal?.aborted) {
          rejection(reject);
          return;
        }

        send({ type: "zip:load", content: path });
        preloadPromises[path] = { resolve, reject };

        options.signal?.addEventListener("abort", () => {
          rejection(reject);
        });
      });
    }
  };

  $commonApi.slug = slug;
  $commonApi.piece = slug.split("~")[0];
  $commonApi.query = Object.fromEntries(new URLSearchParams(search));
  $commonApi.params = params || [];
  $commonApi.colon = colon;

  $commonApi.load = async function () {
    // Load a piece, wrapping it in a leave function so a final frame
    // plays back.
    leaving = true;

    return new Promise((resolve) => {
      leaveLoad = async () => {
        const loaded = await load(...arguments);
        resolve(loaded); // Resolve with `true` or `false`.
      };
    });
  };

  // 💡 Eventually this could merge with net.web so there is one command
  //    to either go to a piece within the system if one loads... or an entirely
  //    different url somehow! 23.02.07.21.21

  $commonApi.alias = function alias(name, colon, params) {
    $commonApi.jump(
      name +
        colon.map((c) => `:` + c).join("") +
        params.map((p) => `~` + p).join(""),
      true,
      true,
    );
  };

  // Go back to the previous piece, or to the prompt if there is no history.
  $commonApi.back = () => {
    if (pieceHistoryIndex > 0) {
      send({ type: "back-to-piece" });
    } else {
      $commonApi.jump("prompt");
    }
  };

  if (!alias) $commonApi.pieceCount += 1; // Don't bump pieceCount if aliased.

  // Load typeface if it hasn't been yet.
  // (This only has to happen when the first piece loads.)
  if (!tf) tf = await new Typeface().load($commonApi.net.preload);
  $commonApi.typeface = tf; // Expose a preloaded typeface globally.

  /**
   * @function video
   * @descrption Make a live video feed. Returns an object that links to current frame.
   * @param {string} type - "camera" or "camera-update" or see below. 💡
   * @param {object} options - *unimplemented* { src, width, height }
   */

  let videoTimeout;

  $commonApi.video = function (type, options) {
    // TODO: ❤️‍🔥 Prevent fast multiple taps while camera is updating...

    // TODO: Options could eventually be { src, width, height }
    // const vid = video("youtube-link");
    // const vid = video("tiktok:@whistlegraph");
    // https://codepen.io/oceangermanique/pen/LqaPgO

    if (videoSwitching === false) {
      if (type === "camera:update") {
        lastActiveVideo = activeVideo || lastActiveVideo;
        activeVideo = null;
      }

      clearTimeout(videoTimeout);
      videoTimeout = setTimeout(() => {
        send({ type: "video", content: { type, options } });
      }, 50);

      if (type === "camera:update") videoSwitching = true;
    }

    // Return an object that can grab whatever the most recent frame of
    // video was.
    return videoFrame;
  };

  function videoFrame(shader) {
    if (activeVideo) {
      const { width, pixels } = activeVideo;

      if (shader) {
        for (let i = 0; i < pixels.length; i += 4) {
          const c = pixels.subarray(i, i + 4);
          const p = { x: (i / 4) % width, y: floor(i / 4 / width) };
          shader(p, c);
        }
      }
    } else if (lastActiveVideo) {
      // Make it all red...
      const { pixels } = lastActiveVideo;
      for (let i = 0; i < pixels.length; i += 4) {
        pixels[i] = 255;
        pixels[i + 1] = 0;
        pixels[i + 2] = 0;
        pixels[i + 3] = 255;
      }
    }
    return activeVideo || lastActiveVideo;
  }

  // This function actually hotSwaps out the piece via a callback from `bios` once fully loaded via the `loading-complete` message.
  hotSwap = () => {
    loadedCallback?.(); // Run the optional load callback. (See also: `jump`)

    $commonApi.rec.loadCallback?.(); // Start any queued tape.
    $commonApi.rec.loadCallback = null;

    module = loadedModule;

    if (!module.system?.startsWith("world"))
      $commonApi.system.world.teleported = false;

    if (module.system?.startsWith("nopaint")) {
      // If there is no painting is in ram, then grab it from the local store,
      // or generate one.

      $commonApi.system.nopaint.bakeOnLeave =
        module.system.split(":")[1] === "bake-on-leave"; // The default is to `bake` at the end of each gesture aka `bake-on-lift`.

      boot = module.boot || nopaint_boot;
      sim = module.sim || defaults.sim;
      paint = module.paint || (() => undefined);
      beat = module.beat || defaults.beat;
      act = ($) => {
        nopaint_act($); // Inherit base functionality.
        if (module.act) {
          return module.act($);
        } else {
          return defaults.act($);
        }
      };
      leave = ($) => {
        module.leave?.($); // Run the custom leave.
        nopaint.leave($); // And the inherited default leave from nopaint.
      };
      bake = module.bake || nopaint.bake;
      system = "nopaint";
    } else if (module.system?.startsWith("prompt")) {
      // Default wrap to "word" if using `prompt:character`.
      const wrap =
        module.wrap ||
        (module.system.indexOf("character") > -1 ? "word" : undefined);

      boot = async ($) => {
        await prompt.prompt_boot(
          $,
          {
            prompt: module.prompt,
            program: {
              before: module.before,
              after: module.after,
            },
            hint: module.system.split(":").slice(1).join(":"), // See `ask.ts`.
            forgetful: module.forgetful || false,
            memory: module.memory || Infinity,
            gutterMax: module.gutterMax,
            lineSpacing: module.lineSpacing,
          },
          module.reply,
          module.halt,
          module.scheme,
          wrap,
          module.copied,
          module.activated,
        );
        await module.boot?.($);
      };

      sim = ($) => {
        module.sim?.($);
        prompt.prompt_sim($);
      };

      paint = ($) => {
        let noPaint = module.paint?.($); // Carry the return.
        noPaint = noPaint || prompt.prompt_paint($);
        return noPaint;
      };

      beat = module.beat || defaults.beat;

      act = ($) => {
        module.act?.($);
        prompt.prompt_act($);
      };

      leave = ($) => {
        module.leave?.($);
        prompt.prompt_leave($);
      };

      system = "prompt";
    } else if (module.system?.startsWith("world")) {
      boot = async ($) => {
        await world.world_boot($, module.world);
        await module.boot?.($);
      };

      sim = ($) => {
        world.world_sim($);
        module.sim?.($);
      };

      paint = ($) => {
        if (!world.coversScreen($.screen)) module.background?.($);
        world.world_paint($, module.paint, module.curtain);
      };

      beat = module.beat || defaults.beat;

      act = ($) => {
        world.world_act($);
        module.act?.($);
      };

      leave = ($) => {
        world.world_leave($);
        module.leave?.($);
      };

      system = "world";
    } else {
      boot = module.boot || defaults.boot;
      sim = module.sim || defaults.sim;
      paint = module.paint || defaults.paint;
      beat = module.beat || defaults.beat;
      act = module.act || defaults.act;
      leave = module.leave || defaults.leave;
      system = module.system || null;

      // delete $commonApi.system.name; // No system in use.
    }

    preview = module.preview || defaults.preview; // Set preview method.
    icon = module.icon || defaults.icon; // Set preview method.

    // ♻️ Reset global state for this piece.
    paintCount = 0n;
    paintingAPIid = 0n;
    simCount = 0n;
    booted = false;
    // initialSim = true;
    activeVideo = null;
    videoSwitching = false;
    lastActiveVideo = null;
    keys(preloadPromises).forEach((key) => preloadPromises[key].reject(key));
    preloadPromises = {};
    noPaint = false;
    formsSent = {}; // Clear 3D list for GPU.
    currentPath = path;
    currentHost = host;
    currentSearch = search;
    // console.log("Set currentSearch to:", search);
    firstPreviewOrIcon = true;
    hideLabel = parsed.search?.startsWith("nolabel") || false;
    currentColon = colon;
    currentParams = params;
    currentHash = hash;
    // sound = null;
    glazeEnabled = null;
    soundClear = null;
    hourGlasses.length = 0;
    labelBack = false;
    previewMode = parsed.search?.startsWith("preview") || false;
    iconMode = parsed.search?.startsWith("icon") || false;

    // 🪧 See if notice needs to be shown.
    if ($commonApi.query.notice === "success") {
      notice = "PRINTED!";
      noticeColor = ["white", "green"];
      noticeBell(cachedAPI);
    } else if ($commonApi.query.notice === "cancel") {
      notice = "CANCELLED";
      noticeColor = ["yellow", "red"];
      noticeBell(cachedAPI, { tone: 300 });
    } else if ($commonApi.query.notice?.length > 0) {
      notice = $commonApi.query.notice;
      noticeColor = ["white", "green"];
      noticeBell(cachedAPI, { tone: 300 });
    } else {
      // Clear any existing notice on disk change.
      // notice = noticeTimer = undefined;
    }

    if (!alias) currentHUDTxt = slug; // Update hud if this is not an alias.
    if (module.nohud) currentHUDTxt = undefined;
    currentHUDOffset = undefined; // Always reset these to the defaults.
    currentHUDTextColor = undefined;
    currentHUDStatusColor = "red"; //undefined;
    currentHUDButton = undefined;
    //currentPromptButton = undefined;

    // Push last piece to a history list, skipping prompt and repeats.
    if (
      !fromHistory &&
      currentText &&
      currentText !== "prompt" &&
      currentText !== $commonApi.history[$commonApi.history.length - 1]
    ) {
      $commonApi.history.push(currentText);
    }

    currentText = slug;
    currentCode = sourceCode;

    if (screen) screen.created = true; // Reset screen to created if it exists.

    cursorCode = "precise"; // Set default cursor.

    if (firstLoad === true) {
      firstLoad = false;
      // firstPiece = path;
      // firstParams = params;
      // firstSearch = search;
    }
  };

  send({
    type: "disk-loaded",
    content: {
      path,
      host,
      search,
      params,
      hash,
      text: slug,
      pieceCount: $commonApi.pieceCount,
      pieceHasSound: true, // TODO: Make this an export flag for pieces that don't want to enable the sound engine. 23.07.01.16.40
      // 📓 Could also disable the sound engine if the flag is false on a subsequent piece, but that would never really make practical sense?
      fromHistory,
      alias,
      meta,
      taping: $commonApi.rec.loadCallback !== null || $commonApi.rec.recording, // 🎏 Hacky flag. 23.09.17.05.09
      // noBeat: beat === defaults.beat,
    },
  });

  return true; // Loaded successfully.
}

const isWorker = typeof importScripts === "function";

// ***Bootstrap***
// Start by responding to a load message, then change
// the message response to makeFrame.
if (isWorker) {
  onmessage = makeFrame;
} else {
  noWorker.onMessage = (d) => makeFrame({ data: d });
}

// The main messaging function to comumunicate back with the main thread.
function send(data, shared = []) {
  if (isWorker) {
    if (shared[0] === undefined) shared = [];
    postMessage(data, shared);
  } else {
    noWorker.postMessage({ data });
  }
}

// Used to subscribe to live coding / development reloads.
let codeChannel, codeChannelAutoLoader;

// 4. ✔ Respond to incoming messages, and probably produce a frame.
// Boot procedure:
// First `paint` happens after `boot`, then any `act` and `sim`s each frame
// before `paint`ing occurs. One `sim` always happens after `boot` and before
// any `act`. `paint` can return false to stop drawing every display frame,
// then, it must be manually restarted via `needsPaint();`).  2022.01.19.01.08
// 🔥
// TODO: makeFrame is no longer a great name for this function, which actually
//       receives every message from the main thread, one of which renders a
//       frame.
// TODO: Make simple needsPaint example.
// TODO: Try to remove as many API calls from here as possible.

async function makeFrame({ data: { type, content } }) {
  // Runs once on boot.
  if (type === "init-from-bios") {
    debug = content.debug;
    setDebug(content.debug);
    ROOT_PIECE = content.rootPiece;
    USER = content.user;
    LAN_HOST = content.lanHost;
    SHARE_SUPPORTED = content.shareSupported;
    // IFRAME = content.iframe;
    $commonApi.canShare = SHARE_SUPPORTED;
    $commonApi.net.lan = LAN_HOST;
    $commonApi.user = USER;
    $commonApi.net.iframe = content.iframe;

    codeChannelAutoLoader = null;
    codeChannel = await store.retrieve("code-channel");
    if (!codeChannel || codeChannel?.length === 0) {
      codeChannel = nanoid();
      codeChannelAutoLoader = () => {
        send({
          type: "post-to-parent",
          content: { type: "setCode", value: codeChannel },
        });
        codeChannelAutoLoader = null;
      };
    }

    console.log("💻 Code channel:", codeChannel);

    await handle(); // Get the user's handle.
    originalHost = content.parsed.host;
    loadAfterPreamble = () => {
      loadAfterPreamble = null;
      load(content.parsed); // Load after some of the default frames run.
    };

    send({ type: "disk-defaults-loaded" });
    return;
  }

  // Update the logged in user after initialization.
  if (type === "session:update") {
    console.log("🤩 Session being updated!", content);
    USER = content.user;
    $commonApi.user = USER;
    return;
  }

  // Capture the browser scroll wheel / scroll effect.
  if (type === "scroll") {
    const $api = cachedAPI;
    const data = { ...content };
    Object.assign(data, {
      device: "wheel",
      is: (e) => e === type,
    });
    $api.event = data;
    try {
      act($api);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
    return;
  }

  // Jump to any piece slug from the bios.
  if (type === "jump") {
    console.log("🏃 Jumping to:", content);
    let ahistorical, alias;
    if (content.ahistorical === undefined) {
      ahistorical = true;
    } else ahistorical = content.ahistorical;
    if (content.alias === undefined) {
      alias = true;
    } else alias = content.alias;
    $commonApi.jump(content.piece, ahistorical, alias);
    return;
  }

  // Create a notice.
  if (type === "notice") {
    $commonApi.notice(content, ["white", "maroon"]);
    return;
  }

  if (type === "loading-complete") {
    leaving = false;
    hotSwap?.(); // Actually swap out the piece functions and reset the state.
    loading = false;
    return;
  }

  if (type === "udp:receive") {
    udp.receive(content);
    return;
  }

  if (type === "udp:connected") {
    udp.connected = true;
    updateHUDStatus();
    $commonApi.needsPaint();
    return;
  }

  if (type === "udp:disconnected") {
    udp.connected = false;
    updateHUDStatus();
    $commonApi.needsPaint();
    return;
  }

  if (type === "microphone:disconnect") {
    microphone.connected = false;
    return;
  }

  // if (type === "hand-tracking-data") {
  // $commonApi.hand = { mediapipe: content };
  // return;
  // }

  // Load the source code for a dropped `.mjs` file.
  if (type === "dropped:piece") {
    load(content, false, false, true);
    return;
  }

  if (type === "dropped:bitmap") {
    if (currentPath === "aesthetic.computer/disks/prompt") {
      $commonApi.system.nopaint.replace(
        { system: $commonApi.system, store, needsPaint: $commonApi.needsPaint },
        content.source,
      );
    } else {
      const $api = cachedAPI;
      const data = { name: content.name, painting: content.source };
      Object.assign(data, {
        device: "none",
        is: (e) => e === type,
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ Act failure...", e);
      }
    }
    return;
  }

  // 🗣️ An act that fires when an utterance has ended in the Web Speech API.
  if (type === "speech:completed") {
    actAlerts.push("speech:completed");
    return;
  }

  // When inputting text into the prompt.
  if (
    type === "prompt:text:replace" ||
    type === "prompt:text:select" ||
    type === "prompt:text:cursor"
  ) {
    const $api = cachedAPI;
    const data = { ...content };
    Object.assign(data, {
      device: "none",
      is: (e) => e === type,
    });
    $api.event = data;
    try {
      act($api);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
    return;
  }

  // Handles: clipboard:paste:pasted, clipboard:paste:pasted:empty
  if (type.startsWith("paste:pasted")) {
    actAlerts.push("clipboard:" + type);
    return;
  }

  if (type === "paste:failed") {
    actAlerts.push("clipboard:paste:failed");
    return;
  }

  if (type === "copy:copied") {
    actAlerts.push("clipboard:copy:copied");
    return;
  }

  if (type === "copy:failed") {
    actAlerts.push("clipboard:copy:failed");
    return;
  }

  if (type === "upload:progress") {
    serverUploadProgressReporter?.(content); // Report file upload progress if needed.
    return;
  }

  if (type === "focus-change") {
    if (!cachedAPI) return; // Hacky... 23.04.21.14.59
    const $api = cachedAPI;
    if (content !== inFocus) {
      inFocus = content;
      const data = {};
      Object.assign(data, {
        device: "none",
        is: (e) => e === (inFocus === true ? "focus" : "defocus"),
      });
      $api.event = data;
      try {
        act($api);
      } catch (e) {
        console.warn("️ ✒ Act failure...", e);
      }
    }
  }

  if (type === "visibility-change") {
    // 🧨 Just in case of a regression... 23.06.02.21.12
    //    Because the `bios` focus event changed from visibility behavior.
    // if (!lastActAPI) return; // Hacky... 23.04.21.14.59
    // const $api = lastActAPI; // Focus change events have an empty API.
    // if (content !== inFocus) {
    //   inFocus = content;
    //   const data = {};
    //   Object.assign(data, {
    //     device: "none",
    //     is: (e) => e === (inFocus === true ? "focus" : "defocus"),
    //   });
    //   $api.event = data;
    //   try {
    //     act($api);
    //   } catch (e) {
    //     console.warn("️ ✒ Act failure...", e);
    //   }
    // }
    visible = content;
  }

  if (type === "before-unload") {
    // This has to be synchronous (no workers) to work, and is also often unreliable.
    // I should not design around using this event, other than perhaps
    // sending a beacon at the end. 22.11.03.14.53
    // See also: https://developer.mozilla.org/en-US/docs/Web/API/Navigator/sendBeacon

    /*
    try {
      leave({ store, screen, system: $commonApi.system }); // Trigger leave.
    } catch (e) {
      console.warn("👋 Leave failure...", e);
    }
    */
    return;
  }

  // Get the updated device motion.
  if (type === "motion:update") {
    $commonApi.motion.current = content;
    return;
  }

  if (type === "gpu-rendered-once") {
    $commonApi.gpuReady = true;
    return;
  }

  if (type === "gpu-forms-removed") {
    // Delete forms from the sent list that have been removed from the GPU scene.
    content.forEach((id) => {
      formsToClear.push(id);
    });
    return;
  }

  if (type === "dark-mode") {
    const current = await store.retrieve("dark-mode");
    // console.log("Dark mode:", "now:", current, "to:", content);
    if (current !== null && current !== undefined) {
      darkMode(current);
    } else {
      darkMode(content.enabled);
    }
    return;
  }

  if (type === "forms:baked") {
    //console.log("🍞 Forms baked:", content);
    //noPaint = false;

    // if (content.pixels) {
    //  graph.paste(content, 0, 0, 1, true);
    // }

    // paintFormsResolution?.();
    return;
  }

  // Media Recorder Events

  if (type === "recorder:transcode-progress") {
    if (debug) console.log("📼 Recorder: Transcoding", content);
    $commonApi.rec.printProgress = content;
    if (content === 1) {
      send({ type: "signal", content: "recorder:transcoding-done" });
      // TODO: Is this the best place for this signal to be sent?
      //       Maybe it should go back in the BIOS? 22.08.19.13.44
    }
    return;
  }

  if (
    type === "recorder:rolling:started" ||
    type === "recorder:rolling:resumed"
  ) {
    $commonApi.rec.recording = true;
    $commonApi.rec.rollingCallback?.(content.time);
    return;
  }

  if (type === "recorder:rolling:ended") {
    $commonApi.rec.recording = false;
    $commonApi.rec.recorded = true; // Also cleared when a recording "slates".
    $commonApi.rec.cutCallback?.();
    return;
  }

  if (type === "recorder:printing:started") {
    return;
  }

  if (type === "recorder:printed") {
    $commonApi.rec.printed = true;
    $commonApi.rec.printCallback?.(content);
    return;
  }

  if (type === "recorder:presented") {
    $commonApi.rec.presenting = true;
    return;
  }

  if (type === "recorder:presented:failure") {
    $commonApi.rec.presenting = false;
    return;
  }

  if (type === "recorder:present-progress") {
    $commonApi.rec.presentProgress = content;
    return;
  }

  if (type === "recorder:present-playing") {
    $commonApi.rec.playing = true;
    return;
  }

  if (type === "recorder:present-paused") {
    $commonApi.rec.playing = false;
    return;
  }

  if (type === "recorder:unpresented") {
    $commonApi.rec.presenting = false;
    return;
  }

  if (type === "signal") {
    signals.push(content);
    return;
  }

  if (type === "store:retrieved") {
    storeRetrievalResolution?.(content);
    return;
  }

  if (type === "store:deleted") {
    storeDeletionResolution?.(content);
    return;
  }

  if (type === "content-created") {
    $commonApi.content.receive(content);
    return;
  }

  if (type === "leave") {
    //const $api = {};
    console.log("🏃‍♂️ Leave:", content);
    return;
  }

  if (type === "sfx:progress:report") {
    sfxProgressReceivers[content.id]?.(content); // Resolve the progress report.
    return;
  }

  if (type === "microphone-amplitude") {
    microphone.amplitude = content;
    return;
  }

  if (type === "microphone-waveform") {
    microphone.waveform = content;
    return;
  }

  if (type === "speaker-waveforms") {
    speaker.waveforms = content;
    return;
  }

  if (type === "speaker-amplitudes") {
    speaker.amplitudes = content;
    return;
  }

  if (type === "microphone-pitch") {
    microphone.pitch = content;
    return;
  }

  if (type === "microphone:recording:complete") {
    microphone.recordingPromise?.resolve(content);
    return;
  }

  if (type === "microphone:connect:success") {
    microphone.connected = true;
    actAlerts.push("microphone:connect:success");
    return;
  }

  if (type === "microphone:connect:failure") {
    microphone.connected = false;
    actAlerts.push("microphone:connect:failure");
    return;
  }

  // 1a. Import // One send (returns afterwards)
  // Here we are receiving file data from main thread that was requested
  // by $api.upload 😱. We check to see if the upload promise exists and then
  // use it and/or throw it away.
  if (type === "import" && fileImport) {
    if (content.result === "success") {
      fileImport?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("File failed to load:", content.data);
      fileImport?.reject(content.data);
    }
    fileImport = undefined;
    return;
  }

  // Resolve a web3 connection message.
  if (type === "web3-connect-response" && web3Response) {
    if (content.result === "success") {
      web3Response?.resolve(content.id);
    } else if (content.result === "error") {
      web3Response?.reject("error");
    }
    web3Response = undefined;
    return;
  }

  // Resolve a gpu message
  if (type === "gpu-response" && gpuResponse) {
    if (content.result === "success") {
      gpuResponse?.resolve(content.data);
    } else if (content.result === "error") {
      gpuResponse?.reject(content.data);
    }
    gpuResponse = undefined;
    return;
  }

  // Resolve a server uploaded file.
  if (type === "upload" && serverUpload) {
    if (content.result === "success") {
      serverUpload?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("File failed to load:", content);
      serverUpload?.reject(content.data);
    }
    serverUpload = undefined;
    return;
  }

  if (type === "zipped" && zipCreation) {
    if (content.result === "success") {
      zipCreation.resolve(content.data);
    } else if (content.result === "error") {
      console.error("Zip failed to be created:", content);
      zipCreation?.reject(content.data);
    }
    zipCreation = undefined;
    return;
  }

  // Run when a painting record ZIP is succesfully parsed after being
  // dragged into the A.C window.
  if (type === "painting:record:dropped") {
    // Replace the active nopaint record with the loaded one.
    // $commonApi.system.nopaint.recording = true;
    $commonApi.system.nopaint.record = content;
    if ($commonApi.slug !== "painting") $commonApi.jump("painting");
    return;
  }

  // Resolve a locally requested file.
  if (type === "file-open:response" && fileOpenRequest) {
    if (content.result === "success") {
      fileOpenRequest?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("Failed to open file.", content);
      fileOpenRequest?.reject(content.data);
    }
    fileOpenRequest = undefined;
    return;
  }

  // Resolve a file encoding request.
  if (type === "file-encode:response" && fileEncodeRequest) {
    if (content.result === "success") {
      fileEncodeRequest?.resolve(content.data);
    } else if (content.result === "error") {
      console.error("Failed to encode file.", content);
      fileEncodeRequest?.reject(content.data);
    }
    fileEncodeRequest = undefined;
    return;
  }

  // Resolve an authorization request.
  if (type === "authorization:response" && authorizationRequest) {
    if (content.result === "success") {
      authorizationRequest?.resolve(content.data);
    } else if (content.result === "error") {
      console.warn("Failed to authenticate.", content);
      authorizationRequest?.reject(content.data);
    }
    authorizationRequest = undefined;
    return;
  }

  // 1b. Video frames.
  if (type === "video-frame") {
    if (!videoSwitching) activeVideo = content;
    return;
  }

  if (type === "video-devices") {
    videoDeviceCount = content;
    $commonApi.cameras = videoDeviceCount;
    return;
  }

  if (type === "camera:updated") {
    videoSwitching = false;
    actAlerts.push("camera:mode:" + content);
    return;
  }

  if (type === "camera:denied") {
    actAlerts.push("camera:denied");
    return;
  }

  // 1c. Loading from History
  if (type === "history-load") {
    if (debug) console.log("⏳ History:", content);
    $commonApi.load(content, true);
    return;
  }

  // 1d. Loading Bitmaps
  if (type === "loaded-bitmap-success") {
    if (debug) console.log("🖼️ Bitmap loaded:", content);
    preloadPromises[content.url]?.resolve(content);
    delete preloadPromises[content];
    return;
  }

  if (type === "loaded-bitmap-rejection") {
    if (debug) console.error("🖼️ Bitmap load failure:", content);
    preloadPromises[content.url]?.reject(content.url);
    delete preloadPromises[content.url];
    return;
  }

  // 1e. Loading Sound Effects
  if (type === "loaded-sfx-success") {
    if (debug && logs.sound) console.log("Sound load success:", content);
    preloadPromises[content.sfx]?.resolve(content.sfx);
    delete preloadPromises[content];
    return;
  }

  if (type === "loaded-sfx-rejection") {
    if (debug && logs.sound) console.error("Sound load failure:", content);
    preloadPromises[content.sfx]?.reject(content.sfx);
    delete preloadPromises[content.sfx];
    return;
  }

  // 1f. Loading ZIP files.
  if (type === "loaded-zip-success") {
    if (debug) console.log("🤐 Zip load success:", content.url);
    preloadPromises[content.url]?.resolve(content.data);
    delete preloadPromises[content.url];
    return;
  }

  if (type === "loaded-zip-rejection") {
    if (debug) console.warn("🤐 Zip load failure:", content.url);
    preloadPromises[content.url]?.reject(content.url);
    delete preloadPromises[content.url];
    return;
  }

  // Request a repaint (runs when the window is resized.)
  if (type === "needs-paint") {
    noPaint = false;
    return;
  }

  if (type === "reframed") {
    // Always update the currentDisplay settings for synchronous
    // screen buffer updates.
    currentDisplay = {
      width: content.innerWidth,
      height: content.innerHeight,
      subdivisions: content.subdivisions,
    };
    $commonApi.display = currentDisplay;

    // Only trigger a reframe event if we have already passed `boot` (painted
    // at least once)
    if (booted) reframed = true;
    return;
  }

  // 1. Beat
  if (type === "beat") {
    if (!sound) return; // Just in case no `frame` has been sent yet.
    try {
      beat($activePaintApi);
    } catch (e) {
      console.warn(" 💗 Beat failure...", e);
    }

    send({ type: "beat", content: sound });
    soundClear?.();
    return;
  }

  // 2. Frame
  // Where each piece action (boot, sim, paint, etc...) is run.
  if (type === "frame") {
    // Take hold of a previously worker transferrable screen buffer
    // and re-assign it.
    let pixels;
    if (content.pixels) {
      pixels = new Uint8ClampedArray(content.pixels);
      if (screen) screen.pixels = pixels;
    }

    // 🌟 Global Keyboard Shortcuts (these could also be seen via `act`)
    content.keyboard.forEach((data) => {
      if (data.name === "keyboard:open") {
        keyboardOpen = true;
        return;
      }

      if (data.name === "keyboard:close") {
        keyboardOpen = false;
        return;
      }

      if (currentText && currentText.indexOf("botce") > -1) return; // No global keys on `botce`. 23.11.12.23.38
      if (data.name.indexOf("keyboard:down") === 0) {
        // [Escape] (Deprecated on 23.05.22.19.33)
        // If not on prompt, then move backwards through the history of
        // previously loaded pieces in a session.
        // if (
        //   data.key === "Escape" &&
        //   currentPath !== "aesthetic.computer/disks/prompt"
        // ) {
        //   if (pieceHistoryIndex > 0) {
        //     send({ type: "back-to-piece" });
        //   } else {
        //     // Load the prompt automatically.
        //     // $api.load("prompt"); Disabled on 2022.05.07.03.45
        //   }
        // }

        if (data.key === "$" || data.key === "Home") {
          if (data.ctrl || data.alt) {
            const sys = $commonApi.system;
            // Make it a painting.
            sys.nopaint.replace(
              cachedAPI,
              graph.cloneBuffer(screen),
              "$creenshot",
            );
            $commonApi.jump("prompt");
          } else {
            downloadScreenshot(); // 🖼️ Take a screenshot.
          }
          $commonApi.sound.synth({
            tone: 800,
            duration: 0.02,
            attack: 0.01,
            decay: 0.5,
            volume: 0.25,
          });
        }

        // ⛈️ Jump back to the `prompt` from anywhere..
        if (
          (data.key === "`" ||
            data.key === "Enter" ||
            data.key === "Backspace" ||
            data.key === "Escape") &&
          system !== "prompt" &&
          system !== "world" &&
          // !keyboardOpen &&
          // system === "world" &&
          // data.key === "Enter" &&
          currentPath !== "aesthetic.computer/disks/prompt"
        ) {
          $commonApi.sound.synth({
            tone: data.key === "Backspace" ? 400 : 600,
            beats: 0.1,
            attack: 0.01,
            decay: 0.5,
            volume: 0.15,
          });

          send({ type: "keyboard:unlock" });

          if (!labelBack || data.key === "Backspace") {
            let promptSlug = "prompt";
            if (data.key === "Backspace")
              promptSlug += "~" + (currentHUDTxt || currentText);
            $commonApi.jump(promptSlug)(() => {
              send({ type: "keyboard:open" });
            });
          } else {
            if ($commonApi.history.length > 0) {
              send({ type: "back-to-piece" });
              // $commonApi.jump(
              //   $commonApi.history[$commonApi.history.length - 1],
              // );
            } else {
              $commonApi.jump(promptSlug)(() => {
                send({ type: "keyboard:open" });
              });
            }
          }
        }

        // [Ctrl + X]
        // Enter and exit fullscreen mode.
        if (data.key === "x" && data.ctrl) {
          send({ type: "fullscreen-enable" });
        }
      }
    });

    // Add 'loading' status to $commonApi.
    $commonApi.loading = loading; // Let the piece know if we are already
    //                               loading another piece.

    // Globalize any background music data, retrievable via bgm.data
    $commonApi.bgm.data = {
      amplitude: content.audioMusicAmplitude,
      sample: content.audioMusicSampleData,
    };

    // Hand-tracking
    if (content.hand) $commonApi.hand = { mediapipe: content.hand };

    // Pens
    if (content.pen) {
      const primaryPointer = help.findKeyAndValue(
        content.pen.pointers,
        "isPrimary",
        true,
      );

      // Returns all [pens] if n is undefined, or can return a specific pen by 1 based index.
      // [pens] are sorted by `pointerIndex`

      // TODO: Including "help.findKeyAndValue" seems to bring a lot of
      //       allocation here because it keeps the whole API around?
      //       Re-test this when pointers is not empty! 22.11.12.20.02
      const pointers = content.pen.pointers;
      const pointersValues = Object.values(pointers);

      // Make all available dragBoxes into `Box` instances.
      pointersValues.forEach((p) => {
        if (p.dragBox) p.dragBox = geo.Box.from(p.dragBox);
      });

      const pens = pointersValues.reduce((arr, value) => {
        arr[value.pointerIndex] = value;
        return arr;
      }, []);

      // if (pens.length > 0 && debug)
      //   console.log("Pens:", pens, content.pen.events);

      $commonApi.pens = function (n) {
        if (n === undefined) return pens;
        return help.findKeyAndValue(pointers, "pointerIndex", n - 1) || {};
      };

      if (pointersValues.length > 1 && primaryPointer)
        primaryPointer.multipen = true; // Set a flag for multipen activity on main pen API object.

      $commonApi.pen = primaryPointer;

      if (
        screen &&
        primaryPointer &&
        (primaryPointer.delta?.x !== 0 || primaryPointer.delta?.y !== 0)
      ) {
        //socket?.send("ambient-pen:point", {
        udp?.send("fairy:point", {
          x: primaryPointer.x / screen.width,
          y: primaryPointer.y / screen.height,
        });
      }
    }

    // 🕶️ VR Pen
    $commonApi.pen3d = content.pen3d?.pen;

    // Add upload event to allow the main thread to open a file chooser.
    // type: Accepts N mimetypes or file extensions as comma separated string.
    // Usage: upload(".jpg").then((data) => ( ... )).catch((err) => ( ... ));
    $commonApi.sideload = (type) => {
      const prom = new Promise((resolve, reject) => {
        fileImport = { resolve, reject };
      });
      send({ type: "import", content: type });
      return prom;
    };

    // 🤖 Sim // no send
    $commonApi.seconds = function (s) {
      return s * 120; // TODO: Get 120 dynamically from the Loop setting. 2022.01.13.23.28
    };

    // 🔈 Sound
    // TODO: Most of $sound doesn't need to be generated per
    //       frame. 24.01.14.15.19

    // For reference in `freq` below.
    const noteFrequencies = {
      c: 16.35,
      "c#": 17.32,
      db: 17.32,
      d: 18.35,
      "d#": 19.45,
      eb: 19.45,
      e: 20.6,
      f: 21.83,
      "f#": 23.12,
      gb: 23.12,
      g: 24.5,
      "g#": 25.96,
      ab: 25.96,
      a: 27.5,
      "a#": 29.14,
      bb: 29.14,
      b: 30.87,
    };

    const $sound = {
      time: content.audioTime,
      // Get the bpm with bpm() or set the bpm with bpm(newBPM).
      bpm: function (newBPM) {
        if (newBPM) sound.bpm = newBPM;
        return sound.bpm;
      },
      // Calculate the frequency of a musical note.
      freq: function (noteString) {
        let octave;
        let note;

        // Downcase everything.
        if (typeof noteString === "string")
          noteString = noteString.toLowerCase();

        // Check if the last character is a digit to determine if an octave is provided
        if (!isNaN(noteString.charAt(noteString.length - 1))) {
          // The last character is the octave
          octave = parseInt(noteString.charAt(noteString.length - 1), 10);
          note = noteString.substring(0, noteString.length - 1);
        } else {
          // If no octave is provided, default to octave 4
          octave = 4;
          note = noteString;
        }

        // Replace 's' with '#' and trailing 'f' with 'b', but only for note strings of length 2
        if (note.length === 2) {
          note = note.replace("s", "#").replace(/f$/, "b");
        }

        const frequency = noteFrequencies[note]; // Look up freq for the note.
        if (!frequency) throw new Error("Note not found in the list");

        // Calculate the frequency for the given octave
        const finalFreq = frequency * Math.pow(2, octave);
        return finalFreq;
      },
    };

    $sound.microphone = microphone;
    $sound.speaker = speaker;

    // TODO: Generalize square and bubble calls.
    // TODO: Move this stuff to a "sound" module.
    sound.bpm = content.audioBpm;

    // Clear synchronized audio triggers.
    soundClear = () => {
      sound.sounds.length = 0;
      sound.bubbles.length = 0;
      sound.kills.length = 0;
    };

    // Trigger a named audio sample to playback in the `bios`.
    // options: { volume: 0-n, pan: 0-2?, loop: Bool, ...(there is more) }
    $sound.play = function (sfx, options) {
      const id = sfx + "_" + performance.now(); // A *unique id for this sample.

      send({ type: "sfx:play", content: { sfx, id, options } });

      return {
        kill: (/*fade = false*/) => {
          // if (!fade) {
          send({ type: "sfx:kill", content: { id } });
          // } else {
          // send({ type: "sfx:kill-fade", content: { fade } });
          // }
        },
        progress: async () => {
          const prom = new Promise((resolve, reject) => {
            sfxProgressReceivers[id] = resolve;
            return { resolve, reject };
          });
          send({ type: "sfx:progress", content: { id } });
          return prom;
        },
      };
    };

    soundTime = content.audioTime;

    $sound.synth = function ({
      type = "square",
      tone = 440, // TODO: Make random.
      beats = random(), // Wow, default func. params can be random!
      duration = undefined, // In seconds... (where beats is a shortcut)
      attack = 0,
      decay = 0,
      volume = 1,
      pan = 0,
    } = {}) {
      const id = soundId;
      if (duration !== undefined) beats = (duration * sound.bpm) / 60;
      sound.sounds.push({ id, type, tone, beats, attack, decay, volume, pan });

      soundId += 1n;

      let seconds;
      if (duration !== undefined) seconds = duration;
      else seconds = (60 / sound.bpm) * beats;
      const end = soundTime + seconds;

      return {
        id,
        kill: function () {
          sound.kills.push(id);
        },
        progress: function (time) {
          return 1 - max(0, end - time) / seconds;
        },
        update: function (properties) {
          // Property updates happen outside of beat timing.
          send({
            type: "beat:update",
            content: { id, properties },
          });
        },
      };
    };

    $sound.bubble = function ({ radius, rise, volume = 1, pan = 0 } = {}) {
      sound.bubbles.push({ radius: radius, rise, volume, pan });
    };

    $sound.kill = function (id) {
      sound.kills.push(id);
    };

    $commonApi.sound = $sound;

    // Act & Sim (Occurs after first boot and paint, `boot` occurs below.)
    if (booted && paintCount > 0n /*&& !leaving*/) {
      const $api = {};
      keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      keys($updateApi).forEach((key) => ($api[key] = $updateApi[key]));
      keys(painting.api).forEach((key) => ($api[key] = painting.api[key]));
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      $api.inFocus = inFocus;

      $api.screen = {
        width: content.width,
        height: content.height,
        pixels: screen.pixels,
      };

      $api.cursor = (code) => (cursorCode = code);

      // 📻 Signaling
      $api.signal = (content) => {
        send({ type: "signal", content });
      };

      // Deprecated on 23.07.01.15.31 (Remove later if no regressions.)
      // if (initialSim) {
      //   console.log("initial", initialSim, content.updateCount, 'paintcount', paintCount);
      //   simCount += 1n;
      //   $api.simCount = simCount;
      //   try {
      //     sim($api);
      //   } catch (e) {
      //     console.warn("🧮 Sim failure...", e);
      //   }
      //   initialSim = false;
      // } else

      if (content.updateCount > 0 && paintCount > 0n) {
        // Run `sim` the number of times as requested from `bios`.
        for (let i = content.updateCount; i--; ) {
          simCount += 1n;
          $api.simCount = simCount;
          try {
            sim($api);
            noticeTimer?.step(); // Globally tick the noticeTimer if it exists.
            // ⌛ Run through all the global hourglass timers.
            for (let i = hourGlasses.length - 1; i >= 0; i--) {
              hourGlasses[i].step();
              if (hourGlasses[i].complete && !hourGlasses[i].autoFlip)
                hourGlasses.splice(i, 1);
            }
            $api.rec.tapeTimerStep($api);
          } catch (e) {
            console.warn("🧮 Sim failure...", e);
          }
        }
      }

      // 🌟 Act
      // *Device Event Handling*

      // TODO: Shouldn't all these events come in as part of one array to
      //       keep their order of execution across devices?
      // TODO: Could "device" be removed in favor of "device:event" strings and
      //       if needed, a device method?

      // Window Events

      // Reframing the piece... (resizing the window).
      if (reframed === true) {
        $api.event = {
          device: "none",
          is: (e) => e === "reframed",
        };
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
        reframed = false;

        // Global reframings.
        // currentPromptButton?.reposition({
        //   left: 6,
        //   bottom: 6,
        //   screen: $api.screen,
        // });
      }

      // If a disk failed to load, then notify the disk that loaded it
      // by checking to see if loadFailure has anything set.
      if (loadFailure) {
        $api.event = {
          error: loadFailure,
          is: (e) => e === "load-error",
        };
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
        send({ type: "load-failure" });
        loadFailure = undefined;
      }

      // Signaling
      if (signals.length) {
        const data = { signal: signals };
        Object.assign(data, {
          device: "none",
          is: (e) => e === "signal",
        });
        $api.event = data;
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
        signals.length = 0;
      }

      // Keyboard Paste Event
      // if (content.clipboardText) {
      //   const data = { text: content.clipboardText };
      //   Object.assign(data, {
      //     device: "none",
      //     is: (e) => e === "pasted:text",
      //   });
      //   $api.event = data;
      //   try {
      //     act($api);
      //   } catch (e) {
      //     console.warn("️ ✒ Act failure...", e);
      //   }
      // }

      // *** Pen Events ***
      // Ingest all pen input events by running act for each event.
      // TODO: I could also be transforming pen coordinates here...
      // TODO: Keep track of lastPen to see if it changed.
      content.pen?.events.forEach((data) => {
        Object.assign(data, {
          device: data.device,
          is: (e) => {
            let [name, pointer] = e.split(":");
            if (pointer) {
              if (pointer === "any") {
                return name === data.name;
              } else {
                return name === data.name && data.pointer === parseInt(pointer);
              }
            } else {
              return name === data.name && data.isPrimary === true;
            }
          },
        });
        //console.log(data)
        $api.event = data;
        // 🌐🖋️️ Global pen events.
        try {
          // Always check to see if there was a tap on the corner.
          const { event: e, jump, send, sound, system } = $api;
          let originalColor;

          let masked = false;

          if (e.is("touch:5")) {
            sound.synth({
              tone: 800,
              duration: 0.02,
              attack: 0.01,
              decay: 0.5,
              volume: 0.25,
            });
            system.nopaint.replace(
              cachedAPI,
              graph.cloneBuffer(screen),
              "$creenshot",
            );
            jump("prompt");
          }

          // Corner prompt button.
          currentHUDButton?.act(e, {
            down: () => {
              originalColor = currentHUDTextColor;
              currentHUDTextColor = [0, 255, 0];
              send({ type: "keyboard:enabled" }); // Enable keyboard flag.
              send({ type: "keyboard:unlock" });
              $api.needsPaint();

              // Mask unless we are in the camera.
              if ($api.slug !== "camera") masked = true;

              $api.sound.synth({
                tone: 300,
                beats: 0.1,
                attack: 0.01,
                decay: 0.5,
                volume: 0.25,
              });
            },
            push: () => {
              $api.sound.synth({
                tone: 600,
                beats: 0.1,
                attack: 0.01,
                decay: 0.5,
                volume: 0.15,
              });
              // send({ type: "keyboard:open" });
              // send({ type: "keyboard:open" });

              if (!labelBack) {
                jump("prompt");
              } else {
                if ($commonApi.history.length > 0) {
                  send({ type: "back-to-piece" });
                  // jump($commonApi.history[$commonApi.history.length - 1]);
                } else {
                  jump("prompt");
                }
              }

              // pieceHistoryIndex > 0
              //   ? send({ type: "back-to-piece" })
              //   : jump("prompt");
              $api.needsPaint();
              masked = true;
            },
            cancel: () => {
              currentHUDTextColor = originalColor;
              // TODO: This might break on pieces where the keyboard is already
              //       open.
              send({ type: "keyboard:disabled" }); // Disable keyboard flag.
              send({ type: "keyboard:lock" });
              $api.needsPaint();
            },
            rollover: (btn) => {
              if (btn) send({ type: "keyboard:unlock" });
            },
            rollout: () => {
              send({ type: "keyboard:lock" });
            },
          });

          // currentPromptButton?.act(e, {
          //   down: () => {
          //     send({ type: "keyboard:enabled" }); // Enable keyboard flag.
          //     send({ type: "keyboard:unlock" });
          //     $api.needsPaint();
          //     masked = true;
          //     $api.sound.synth({
          //       type: "sine",
          //       tone: 600,
          //       attack: 0.1,
          //       decay: 0.99,
          //       volume: 0.75,
          //       duration: 0.001,
          //     });
          //   },
          //   push: () => {
          //     $api.sound.synth({
          //       type: "sine",
          //       tone: 800,
          //       attack: 0.1,
          //       decay: 0.99,
          //       volume: 0.75,
          //       duration: 0.005,
          //     });
          //     send({ type: "keyboard:open" });
          //     jump("prompt");
          //     $api.needsPaint();
          //     masked = true;
          //   },
          //   cancel: () => {
          //     // TODO: This might break on pieces where the keyboard is already
          //     //       open.
          //     send({ type: "keyboard:disabled" }); // Disable keyboard flag.
          //     send({ type: "keyboard:lock" });
          //     $api.needsPaint();
          //   },
          //   rollover: (btn) => {
          //     if (btn) send({ type: "keyboard:unlock" });
          //   },
          //   rollout: () => {
          //     send({ type: "keyboard:lock" });
          //   },
          // });

          if (!masked) act($api); // Run the act function for all pen events.
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });

      // *** 3D Pen Events ***
      content.pen3d?.events?.forEach((data) => {
        Object.assign(data, {
          is: (e) => {
            let [prefix, event, pointer] = e.split(":");
            if (
              prefix === "3d" &&
              event === data.name &&
              (pointer === undefined || parseInt(pointer) === data.pointer)
            )
              return true;
          },
        });
        $api.event = data;
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });

      // Ingest all keyboard input events by running act for each event.
      content.keyboard?.forEach((data) => {
        Object.assign(data, {
          device: "keyboard",
          is: (e) => {
            const parts = e.split(":");
            if (parts.length > 2) {
              // Check for an exact match if `keyboard:action:?`
              return data.name === e;
            } else {
              // Or a subtring match if `keyboard:action`
              return data.name.indexOf(e) === 0;
            }
          },
        });
        $api.event = data;
        try {
          act($api); // Execute piece shortcut.
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });

      // *** Act Alerts *** (Custom events defined in here.)
      actAlerts.forEach((name) => {
        const data = {
          name,
          is: (e) => e === name,
          of: (e) => name.startsWith(e),
        };
        $api.event = data;
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      });
      actAlerts.length = 0; // Clear act alerts.
    }

    // 🖼 Paint
    if (content.needsRender) {
      const $api = {};
      keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      keys(painting.api).forEach((key) => ($api[key] = painting.api[key]));
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      // Object.assign($api, $commonApi);
      // Object.assign($api, painting.api);

      $api.paintCount = Number(paintCount);

      $api.inFocus = content.inFocus;

      $api.glaze = function (content) {
        if (glazeEnabled === content.on) return; // Prevent glaze from being fired twice...
        glazeEnabled = content.on;
        glazeAfterReframe = { type: "glaze", content };
      };

      // Make a screen buffer or resize it automatically if it doesn't exist.

      if (
        !screen ||
        screen.width !== content.width ||
        screen.height !== content.height
      ) {
        const hasScreen = screen !== undefined;

        screen = {
          pixels:
            pixels || new Uint8ClampedArray(content.width * content.height * 4),
          width: content.width,
          height: content.height,
          load: function load(name) {
            if (store[name]?.pixels) {
              this.pixels = new Uint8ClampedArray(store[name].pixels);
              this.width = store[name].width;
              this.height = store[name].height;
              $commonApi.resize(this.width, this.height);
              return true;
            } else {
              return false;
            }
          },
          save: function save(name) {
            store[name] = {
              pixels: new Uint8ClampedArray(this.pixels),
              width: this.width,
              height: this.height,
            };
          },
        };

        screen[hasScreen ? "resized" : "created"] = true; // Screen change type.

        // TODO: Add the depth buffer back here.
        // Reset the depth buffer.
        // TODO: I feel like this is causing a memory leak...
        // graph.depthBuffer.length = screen.width * screen.height;
        // graph.depthBuffer.fill(Number.MAX_VALUE);

        graph.writeBuffer.length = 0; //screen.width * screen.height;
        // graph.writeBuffer.fill(0);
      }

      // TODO: Disable the depth buffer for now... it doesn't need to be
      //       regenerated on every frame.
      // graph.depthBuffer.fill(Number.MAX_VALUE); // Clear depthbuffer.
      graph.writeBuffer.length = 0; //fill(0); // Clear writebuffer.

      $api.screen = screen;
      $api.screen.center = { x: screen.width / 2, y: screen.height / 2 };

      SCREEN = screen;

      $api.fps = function (newFps) {
        send({ type: "fps-change", content: newFps });
      };

      $api.cursor = (code) => (cursorCode = code);

      graph.setBuffer(screen);

      // API Stops being modified here...
      $activePaintApi = $api;

      // TODO: Set bpm from boot.
      /*
      $api.sound = {
        time: content.time,
        bpm: function (newBPM) {
          if (newBPM) {
            content.bpm[0] = newBPM;
          }
          return content.bpm[0];
        },
      };
       */

      // TODO: Boot's painting is currently bound by whatever dirtyBox gets
      //       set to at the end of `paint`.

      // Run boot only once before painting for the first time.
      if (paintCount === 0n && loading === false) {
        const dark = await store.retrieve("dark-mode"); // Read dark mode.
        if (dark === true || dark === false) $commonApi.dark = dark;

        // System specific preloaders.
        //if ($commonApi?.system?.name === "nopaint" || currentText === "prompt") {

        // Create a new painting if one doesn't already exist.
        if (!store["painting"]) {
          store["painting"] =
            (await store.retrieve("painting", "local:db")) ||
            painting.api.painting(screen.width, screen.height, ($) => {
              $.wipe(64);
            });

          store["painting:resolution-lock"] = await store.retrieve(
            "painting:resolution-lock",
            "local:db",
          );

          store["painting:transform"] = await store.retrieve(
            "painting:transform",
            "local:db",
          );

          addUndoPainting(store["painting"]);
        }

        const sys = $commonApi.system;
        sys.painting = store["painting"];

        // Set the painting record if one is in storage.
        if (!sys.nopaint.recording) {
          sys.nopaint.record =
            (await store.retrieve("painting:record", "local:db")) || [];

          if (sys.nopaint.record.length === 0) {
            $commonApi.system.nopaint.startRecord("new");
          }

          sys.nopaint.recording = sys.nopaint.record.length > 0;
        }

        sys.nopaint.translation =
          store["painting:transform"]?.translation || sys.nopaint.translation;
        sys.nopaint.zoomLevel =
          store["painting:transform"]?.zoom || sys.nopaint.zoomLevel;

        try {
          if (system === "nopaint") nopaint_boot($api);
          await boot($api);
          booted = true;
        } catch (e) {
          console.warn("🥾 Boot failure...", e);
        }
        send({ type: "disk-loaded-and-booted" });
      }

      // Paint a frame, which can return false to enable caching via noPaint and by
      // default returns undefined (assume a repaint).
      // Once paint returns false and noPaint is marked true, `needsPaint` must be called.
      // Note: Always marked false on a disk's first frame.

      let painted = false;
      let dirtyBox;

      // Render a thumbnail instead of the piece.
      if (previewMode) {
        try {
          // Assign a default resolution on first preview,
          // which can be over-ridden using `resolution` inside the
          // `preview` function.
          if (firstPreviewOrIcon) {
            if (currentSearch === "preview") {
              $api.resolution(1200 / 8, 630 / 8, 0);
            } else {
              $api.resolution(
                ...currentSearch
                  .split("=")[1]
                  .split("x")
                  .map((n) => floor(parseInt(n) / 8)),
                0,
              );
            }
            firstPreviewOrIcon = false;
          }

          preview($api);
          painting.paint(true);
          painted = true;
          paintCount += 1n;
        } catch (err) {
          console.warn("🖼️ Preview failure...", err);
          previewMode = false;
        }
      } else if (iconMode) {
        // Render a favicon instead of the piece.
        try {
          if (firstPreviewOrIcon) {
            $api.resolution(128, 128, 0);
            if (currentSearch === "icon") {
              $api.resolution(128, 128, 0);
            } else {
              console.log("Current:", currentSearch);
              $api.resolution(
                ...currentSearch
                  .split("=")[1]
                  .split("x")
                  .map((n) => parseInt(n)),
                0,
              );
            }
            firstPreviewOrIcon = false;
          }
          icon($api);
          painting.paint(true);
          painted = true;
          paintCount += 1n;
        } catch (err) {
          console.warn("🪷 Icon failure...", err);
          iconMode = false;
        }
      }

      // Attempt a paint.
      if (
        previewMode === false &&
        iconMode === false &&
        (noPaint === false || scream || fairies.length > 0) &&
        booted
      ) {
        let paintOut;

        try {
          // 📓 Bake any painting from the nopaint system before anything else.
          if (system === "nopaint") {
            const np = $api.system.nopaint;
            // No Paint: baking

            if (np.needsBake === true && bake) {
              $api.page($api.system.painting);
              bake($api);
              $api.page($api.screen);
              np.present($api);
              np.needsBake = false;
            } else if (np.is("painting") || np.needsPresent) {
              np.present($api); // No Paint: prepaint
            }
          }

          // All: Paint
          paintOut = paint($api); // Returns `undefined`, `false`, or `DirtyBox`.
        } catch (e) {
          console.warn("🎨 Paint failure...", e);
        }

        // `DirtyBox` and `undefined` always set `noPaint` to `true`.
        noPaint =
          paintOut === false || (paintOut !== undefined && paintOut !== true);

        // Run everything that was queued to be painted, then devour paintLayers.
        //await painting.paint();

        // Upper layer.
        const { page, layer, ink, needsPaint, pieceCount } = $api;
        page($api.screen); // Make sure we're on the right bufer.
        layer(1000); // Always make sure this stuff draws on top.

        // const piece = $api.slug?.split("~")[0];
        // if (
        //   !previewMode &&
        //   !iconMode &&
        //   !hideLabel &&
        //   system !== "prompt" &&
        //   piece !== "textfence" &&
        //   piece !== "bleep" &&
        //   piece !== undefined &&
        //   piece.length > 0 &&
        //   piece !== "painting" &&
        //   pieceCount > 0
        // ) {
        // currentPromptButton =
        //   currentPromptButton ||
        //   new $api.ui.TextButton("Back", {
        //     left: 6,
        //     bottom: 6,
        //     screen: $api.screen,
        //   });
        // currentPromptButton.paint($api);
        // }

        // 😱 Scream - Paint a scream if it exists.
        // TODO: Should this overlay after the fact and not force a paint? 23.05.23.19.21
        //       Yes probably, because of layering issues?
        if (scream || screaming) {
          ink("yellow").write(scream, { x: 6, y: 18 }, "red");
          //ink("red").write(scream, { x: 6 + 1, y: 18 + 1 });

          /*
          ink(255)
            .wipe(255, 0, 0)
            .write(
              scream,
              { center: "xy", size: 3, thickness: 1 },
              undefined,
              $api.screen.width - 8,
              needsPaint(),
            );
          */
          if (!screaming) {
            screaming = true;
            clearTimeout(screamingTimer);
            screamingTimer = setTimeout(() => {
              screaming = false;
              scream = null;
            }, 1000);
          }
        }

        // 🧚 Ambient Pen Points - Paint if they exist.
        fairies.forEach(({ x, y }) => {
          ink().point(x * screen.width, y * screen.height);
        });
        if (fairies.length > 0) {
          needsPaint();
          // if (system === "nopaint") $api.system.nopaint.needsPresent = true;
        }
        fairies.length = 0;

        // 🔴 Show a cross-piece "Recording" indicator.
        //    Currently only implemented for `painting:record`. 23.08.20.21.36
        if (
          $api.system.nopaint.recording &&
          !hideLabel &&
          pieceHistoryIndex > -1 &&
          !loading
        ) {
          // ink("red").box(screen.width - 3, 1, 2);
        }

        // Show a notice if necessary.
        if (notice) {
          ink(noticeColor[0])
            //.pan(help.choose(-1, 0, 1), help.choose(-1, 0, 1))
            .write(
              notice,
              { center: "xy", size: 2 },
              // { center: "x", y: 32, size: 2 },
              noticeColor[1],
              $api.screen.width - 8,
            );
          //.unpan();
        }

        layer(0);

        painting.paint(true);
        painted = true;
        paintCount = paintCount + 1n;

        if (paintOut) dirtyBox = paintOut;

        delete screen.resized; // Remove status from screen after painting.
        delete screen.created;

        //console.log("bake")
        //send({ type: "3d-bake" });
      }

      // Draw any Global UI / HUD in an overlay buffer that will get
      // composited by the other thread.

      // TODO: ❤️‍🔥 Why is this being composited by a different thread?
      //       Also... where do I put a scream?

      // System info label (addressability).
      let label;
      const piece = currentHUDTxt?.split("~")[0];
      const defo = 6; // Default offset

      if (
        !previewMode &&
        !iconMode &&
        !hideLabel &&
        piece !== undefined &&
        piece.length > 0 &&
        piece !== "download:painting" &&
        piece !== "prompt" &&
        piece !== "play" &&
        piece !== "gargoyle" &&
        piece !== "girlfriend" &&
        piece !== "textfence" &&
        piece !== "boyfriend" &&
        piece.indexOf("botce") === -1 &&
        piece !== "angel" &&
        piece !== "dad" &&
        piece !== "kid" &&
        piece !== "decode" &&
        piece !== "liar" &&
        piece !== "mom" &&
        piece !== "encode" &&
        piece !== "alphapoet" &&
        piece !== "sing" // &&
      ) {
        let w = currentHUDTxt.length * 6;
        const h = 11;
        if (piece === "video") w = screen.width;

        label = $api.painting(w, h, ($) => {
          let c;
          if (currentHUDTextColor) {
            c = num.shiftRGB(currentHUDTextColor, [255, 255, 255], 0.75);
          } else if (currentHUDStatusColor) {
            c = currentHUDStatusColor;
          } else {
            c = [255, 200, 240];
          }
          if (piece !== "video") {
            let text = currentHUDTxt;
            if (currentHUDTxt.split(" ")[1]?.indexOf("http") !== 0) {
              text = currentHUDTxt?.replaceAll("~", " ");
            }
            $.ink(0).write(text, { x: 1, y: 1 });
            $.ink(c).write(text, { x: 0, y: 0 });
          } else {
            $.ink(0).line(1, 1, 1, h - 1);
            $.ink(c).line(0, 0, 0, h - 2);
          }
        });

        if (piece === "video") currentHUDOffset = { x: 0, y: 6 };
        if (!currentHUDOffset) currentHUDOffset = { x: defo, y: defo };

        currentHUDButton =
          currentHUDButton ||
          new $api.ui.Button({
            x: 0,
            y: 0,
            w: w + currentHUDOffset.x,
            h: h + currentHUDOffset.y,
          });
        $commonApi.hud.currentLabel = {
          text: currentHUDTxt,
          btn: currentHUDButton,
        };
      }

      // Return frame data back to the main thread.
      let sendData = { width: screen.width, height: screen.height };

      // Tack on the tape progress bar pixel buffer if necessary.
      if ($api.rec.tapeProgress) {
        const tapeProgressBar = $api.painting($api.screen.width, 1, ($) => {
          $.ink(0).box(0, 0, $api.screen.width, 1);
          $.ink("red").box(
            0,
            0,
            $api.screen.width * (1 - $api.rec.tapeProgress),
            1,
          );
        });

        if (tapeProgressBar)
          sendData.tapeProgressBar = {
            x: 0,
            y: 0, // screen.height - 1,
            img: tapeProgressBar,
          };
      }

      maybeLeave();

      // TODO: Write this up to the data in `painting`.
      sendData.TwoD = {
        code: twoDCommands,
        // code: [
        //   ["ink", 1.0, 0, 0, 1.0],
        //   ["ink2", 0.0, 1.0, 0, 1.0],
        //   ["line", 0, 0, 30, 30],
        //   //     x1, y1, x2, y2, r1, g1, b1, a1, r2, g2, b2, a2
        // ],
      };

      // Attach a label buffer if necessary.
      if (label)
        sendData.label = {
          x: currentHUDOffset.x,
          y: currentHUDOffset.y,
          img: label,
        };

      let transferredPixels;

      // Check to see if we have a dirtyBox to render from.
      const croppedBox = dirtyBox?.croppedBox?.(screen);

      if (croppedBox?.w > 0 && croppedBox?.h > 0) {
        transferredPixels = dirtyBox.crop(screen);
        sendData.pixels = transferredPixels;
        sendData.dirtyBox = croppedBox;
      } else if (painted === true) {
        // TODO: Toggling this causes a flicker in `line`... but helps prompt. 2022.01.29.13.21
        // Otherwise render everything if we drew anything!
        transferredPixels = screen.pixels;
        sendData.pixels = transferredPixels;
      }

      // Optional messages to send.
      if (painted === true) sendData.paintChanged = true;
      if (loading === true) sendData.loading = true;

      // These fields are one time `signals`.
      if (reframe || glazeAfterReframe) {
        sendData.reframe = reframe || glazeAfterReframe !== undefined;
        if (glazeAfterReframe) {
          send(glazeAfterReframe);
          glazeAfterReframe = undefined;
        }
      }

      if (cursorCode) sendData.cursorCode = cursorCode;

      // Note: transferredPixels will be undefined when sendData === {}.
      if (sendData.pixels) {
        sendData.pixels = sendData.pixels.buffer;
      } else {
        sendData.pixels = content.pixels;
      }

      // new hud

      if (sendData.pixels?.byteLength === 0) sendData.pixels = undefined;

      let transferredObjects = [sendData.pixels];
      if (sendData.label)
        transferredObjects.push(sendData.label?.img.pixels.buffer);

      sendData.sound = sound;

      send({ type: "render", content: sendData }, transferredObjects);

      twoDCommands.length = 0; // Empty the 2D GPU command buffer.

      // Flush the `signals` after sending.
      if (reframe) reframe = undefined;
      if (cursorCode) cursorCode = undefined;
    } else {
      // Send update (sim).
      maybeLeave();
      // TODO: How necessary is this - does any info ever need to actually
      //       get sent? 23.01.06.16.02
      send(
        {
          type: "update",
          content: {
            didntRender: true,
            loading,
            pixels: pixels?.buffer,
            width: content.width,
            height: content.height,
            sound,
          },
        },
        [pixels?.buffer],
      );
    }

    // Wait 8 frames of the default piece before loading the next piece.
    if (paintCount > 8n) loadAfterPreamble?.(); // Start loading after the first disk if necessary.

    soundClear?.();

    // ***Frame State Reset***
    // Reset video transcoding / print progress.

    //console.log(performance.now() - frameTime, "ms");

    //performance.mark("b");

    //performance.measure("a", "b");
    //console.log("Frame perf:", performance.getEntriesByType("measure")[0].duration);
    //performance.clearMarks();
    //performance.clearMeasures();
  }
}

// 📚 Utilities

// Get the active user's handle from the server if one exists, updating
// $commonApi.handle
let HANDLE;
async function handle() {
  if (USER) {
    try {
      const response = await fetch(`/handle?for=${USER.sub}`);
      if (response.status === 200) {
        const data = await response.json();
        const newHandle = "@" + data.handle;
        if (newHandle !== $commonApi.handle()) {
          HANDLE = "@" + data.handle;
          send({ type: "handle", content: HANDLE });
          store["handle:received"] = true;
        }
      } else {
        console.warn(await response.text());
      }
    } catch (error) {
      console.error(error);
    }
  }
}

// Tell the `bios` to pull a screenshot of the next frame.
function downloadScreenshot() {
  send({
    type: "$creenshot",
    content: {
      filename: `$creenshot-${num.timestamp()}.png`,
      modifiers: { scale: 6 },
    },
  });
}

// Run the piece's "leave" function which will trigger
// a new load before sending off the final frame.
function maybeLeave() {
  // 🚪 Leave (Skips act and sim and paint...)
  if (leaving && leaveLoad) {
    try {
      leave({ ...painting.api, screen, ...$commonApi }); // Trigger leave.
    } catch (e) {
      console.warn("👋 Leave failure...", e);
    }
    leaveLoad();
    leaveLoad = null;
  }
}

const noticeBell = (api, { tone } = { tone: 600 }) => {
  api.sound.synth({
    tone,
    beats: 0.1,
    attack: 0.01,
    decay: 0.5,
    volume: 0.25,
  });

  noticeTimer = new gizmo.Hourglass(160, {
    completed: () => {
      notice = "";
      noticeTimer = null;
      $commonApi.needsPaint();
    },
    // every: () => $commonApi.needsPaint(),
  });
};
