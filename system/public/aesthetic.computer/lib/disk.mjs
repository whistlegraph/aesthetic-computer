// 💾 Disk (Piece)
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
// import { UDP } from "./udp.mjs"; // TODO: Eventually expand to `net.Socket`
import { notArray } from "./helpers.mjs";
const { round } = Math;
import { nopaint_boot, nopaint_act, nopaint_is } from "../systems/nopaint.mjs";
import * as prompt from "../systems/prompt-system.mjs";
import { headers } from "./console-headers.mjs";
import { logs } from "./logs.mjs";
import { soundWhitelist } from "./sound/sound-whitelist.mjs";

import { Typeface } from "../lib/type.mjs";
let tf; // Typeface global.

export const noWorker = { onMessage: undefined, postMessage: undefined };

const { sin, floor } = Math;

let ROOT_PIECE = "prompt"; // This gets set straight from the host html file for the ac.
let USER; // A holder for the logged in user. (Defined in `boot`)
let debug = false; // This can be overwritten on boot.
import { setDebug } from "../disks/common/debug.mjs";

const defaults = {
  boot: ({ resize, cursor, screen: { width, height } }) => {
    // resize(width / 2, height / 2);
    cursor("native");
  }, // aka Setup
  sim: () => false, // A framerate independent of rendering.
  paint: ({ noise16Aesthetic }) => {
    // TODO: Make this a boot choice via the index.html file?
    noise16Aesthetic();
    //noiseTinted([20, 20, 20], 0.8, 0.7);
  },
  beat: () => false, // Runs every bpm.
  act: () => false, // All user interaction.
  leave: () => false, // Before unload.
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

      addUndoPainting(system.painting);

      // Idea: Check to see if anything actually got painted by doing a diff on
      //       the pixels?

      store["painting"] = system.painting; // Remember the painting data.
      store.persist("painting", "local:db");

      // And its transform.
      store["painting:transform"] = { translation: system.nopaint.translation };
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

function addUndoPainting(painting) {
  if (!painting) return; // If there is no painting present, silently pass.
  const op = painting.pixels;
  const pixels = new Uint8ClampedArray(op.length);
  pixels.set(op);

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
let bake; // Currently only used by the `nopaint` system.

let leaving = false; // Set to true on first piece.
let leaveLoad; // A callback for loading the next disk after leaving.

let module; // Currently loaded piece module code.
let currentPath,
  currentHost,
  currentSearch,
  currentColon,
  currentParams,
  currentHash,
  currentText,
  currentCode,
  currentHUDText,
  currentHUDTextColor,
  currentHUDButton,
  currentHUDOffset;
let loading = false;
let reframe;

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

let sound,
  soundClear, // Used by receivedBeat and defined in first frame update.
  soundId = 0n; // Increment each sound / give it an id in the `bios`.

let storeRetrievalResolution, storeDeletionResolution;

let socket, socketStartDelay;
let scream = null; // 😱 Allow priviledged users to send alerts to everyone.
//                       (A great end<->end socket + redis test.)
let screaming = false;
let screamingTimer; // Keep track of scream duration.

const ambientPenPoints = []; // Render cursor points of other active users,
//                              dumped each frame.

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
let authorizationRequest;
let fileOpenRequest;
let fileEncodeRequest;
let gpuResponse;
let web3Response;

// Other
let activeVideo; // TODO: Eventually this can be a bank to store video textures.
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
  recorded = false; // "
  presenting = false; // "
  playing = false; // "
  cutCallback;
  printCallback;

  constructor() {}

  slate() {
    send({ type: "recorder:slate" }); // Kill the MediaRecorder instance.
    // TODO: Should printing and playing also be set to false?
    //$commonApi.rec.printing = false; // "

    $commonApi.rec.recording = false; // Reset this singleton.
    $commonApi.rec.recorded = false; //
    $commonApi.rec.printed = false; // "
    $commonApi.rec.printProgress = 0; // "
  }

  rolling(opts) {
    send({ type: "recorder:rolling", content: opts });
  }

  cut(cb) {
    $commonApi.rec.cutCallback = cb;
    send({ type: "recorder:cut" });
  }

  print(cb) {
    // $commonApi.rec.printing = true; // Set this to `true` right away.
    $commonApi.rec.printCallback = cb;
    send({ type: "recorder:print" });
  }

  present() {
    send({ type: "recorder:present" });
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

// For every function to access.
const $commonApi = {
  // Broadcast an event through the entire act system.
  act: (event, data = {}) => {
    data.is = (e) => e === event;
    cachedAPI.event = data;
    try {
      act(cachedAPI);
    } catch (e) {
      console.warn("️ ✒ Act failure...", e);
    }
  },
  // `Get` api
  // Retrieve assets from a user account.
  get: {
    painting: (code) => {
      return {
        by: (handle) =>
          $commonApi.net.preload(`/media/${handle}/painting/${code}.png`),
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
    send({ type: "upload", content: { filename, data, bucket } });
    return prom;
  },
  code: {
    channel: (chan) => {
      codeChannel = chan; // Set the current `codeChannel`.
      store["code-channel"] = codeChannel; // Store and keep it in the browser.
      store.persist("code-channel");
      console.log("💻 Code channel set to:", codeChannel);
      socket.send("code-channel:sub", codeChannel);
      //       ❤️‍🔥
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
      currentHUDText = text;
      currentHUDTextColor = color;
      currentHUDOffset = offset;
    },
    currentLabel: () => ({ text: currentHUDText, btn: currentHUDButton }),
  },
  send,
  platform,
  history: [], // Populated when a disk loads and sets the former piece.
  // Trigger background music.
  // Eventually add an "@" style parameter similar to what a stamp system would have.
  bgm: {
    set: function (trackNumber) {
      send({ type: "bgm-change", content: { trackNumber } });
    },
    stop: () => send({ type: "bgm-stop" }),
    data: {},
  },
  system: {
    // prompt: { input: undefined }, Gets set in `prompt_boot`.
    nopaint: {
      //boot: nopaint_boot, // TODO: Why are these in the commonApi? 23.02.12.14.26
      // act: nopaint_act,
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

        if (yes) {
          // ⏩ Fast-forward mode.
          console.log("YES:", undoPosition);
          undoPosition += 1;
          if (undoPosition > paintings.length - 1)
            undoPosition = paintings.length - 1;
        } else {
          // ⏪ Rewind mode.
            undoPosition -= 1;
            if (undoPosition < 0) undoPosition = 0;
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

          if (resolutionChange) {
            system.nopaint.resetTransform({ system });
            system.nopaint.storeTransform(store, system);
          }

          needsPaint();
        }
      },
      // Center the picture within the screen / default translation.
      resetTransform: ({ system: sys }) => {
        if (!sys.painting) {
          sys.nopaint.translation = { x: 0, y: 0 };
          return;
        }

        sys.nopaint.translation.x = floor(
          screen.width / 2 - sys.painting.width / 2
        );
        sys.nopaint.translation.y = floor(
          screen.height / 2 - sys.painting.height / 2
        );
      },
      storeTransform: (store, sys) => {
        store["painting:transform"] = { translation: sys.nopaint.translation };
        store.persist("painting:transform", "local:db");
      },
      translation: { x: 0, y: 0 },
      translate: ({ system }, x, y) => {
        system.nopaint.translation.x += x;
        system.nopaint.translation.y += y;
      },
      brush: { x: 0, y: 0 },
      transform: (p) => {
        return {
          x: p.x - nopaintAPI.translation.x,
          y: p.y - nopaintAPI.translation.y,
        };
      },
      updateBrush: ({ pen, system }) => {
        const { x, y } = system.nopaint.translation;

        const pos = { x: (pen?.x || 0) - x, y: (pen?.y || 0) - y };

        // Transform the original dragBox
        const dragBox = new geo.Box(
          pen?.dragBox?.x - x,
          pen?.dragBox?.y - y,
          pen?.dragBox?.w,
          pen?.dragBox?.h
        );

        system.nopaint.brush = { x: pos.x, y: pos.y, dragBox };
      },
      // Helper to display the existing painting on the screen, with an
      // optional pan amount, that returns an adjusted pen pointer as `brush`.

      // TODO: - [] Add Zoom
      //       - [] And Rotation!

      present: ({ system, screen, wipe, paste }, tx, ty) => {
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
          paste(system.painting);
        } else {
          // If we are panned or the painting is a custom resolution.
          wipe(32)
            .paste(system.painting, x, y)
            .ink(128)
            .box(
              x,
              y,
              system.painting.width,
              system.painting.height,
              "outline"
            );
        }

        return {
          x,
          y, //,
          //brush: { x: (pen?.x || 0) - x, y: (pen?.y || 0) - y },
        };
      },
      // Kill an existing painting.
      noBang: async ({ system, store, needsPaint }) => {
        const deleted = await store.delete("painting", "local:db");
        await store.delete("painting:resolution-lock", "local:db");
        await store.delete("painting:transform", "local:db");
        system.nopaint.undo.paintings.length = 0; // Reset undo stack.
        system.painting = null;
        system.nopaint.resetTransform({ system, screen }); // Reset transform.
        needsPaint();
        return deleted;
      },
      // Replace a painting entirely, remembering the last one.
      // (This will always enable fixed resolution mode.)
      replace: ({ system, store, needsPaint }, painting) => {
        system.painting = painting; // Update references.
        store["painting"] = system.painting;
        store.persist("painting", "local:db"); // Persist to storage.
        store["painting:resolution-lock"] = true;
        store.persist("painting:resolution-lock", "local:db");
        system.nopaint.addUndoPainting(system.painting);
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
  },
  num: {
    wrap: num.wrap,
    even: num.even,
    odd: num.odd,
    clamp: num.clamp,
    rand: num.rand,
    randInt: num.randInt,
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
    Track: num.Track,
    timestamp: num.timestamp,
    p2: num.p2,
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
  },
  help: {
    choose: help.choose,
    flip: help.flip,
    repeat: help.repeat,
    every: help.every,
    any: help.any,
    anyKey: help.anyKey,
    each: help.each,
  },
  gizmo: { Hourglass: gizmo.Hourglass },
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
  },
  needsPaint: () => (noPaint = false), // TODO: Does "paint" needs this?
  store,
  pieceCount: -1, // Incs to 0 when the first piece (usually the prompt) loads.
  //                 Increments by 1 each time a new piece loads.
  debug,
};

const nopaintAPI = $commonApi.system.nopaint;

// Spawn a session backend for a piece.
async function session(slug, forceProduction = false, service) {
  let endPoint = "/session/" + slug;
  const params = { service };
  if (forceProduction) params.forceProduction = 1;
  endPoint += "?" + new URLSearchParams(params);

  const req = await fetch(endPoint);

  const session = await req.json();

  if (debug && logs.session)
    console.log(
      `🐕‍🦺 Session: ${slug} - ${session.backend || session.name || session.url}`
    );
  // Return the active session if the server knows it's "Ready", otherwise
  // wait for the one we requested to spin up.
  // (And in debug mode we just get a local url from "/session" so no need
  // to check that.)
  if (session.state === "Ready" || (debug && !forceProduction)) {
    return session;
  } else {
    let eventSource = new EventSource(
      `https://api.jamsocket.com/backend/${session.name}/status/stream`
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
        if (update.state === "Ready") {
          resolve(session);
        } else {
          if (update.state !== "Loading" && update.state !== "Starting") {
            eventSource = null; // Clears the event stream handler.
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

// Inputs: (r, g, b), (r, g, b, a) or an array of those.
//         (rgb) for grayscale or (rgb, a) for grayscale with alpha.
//         Or hex with "#000000" or "0x000000" or 0x000000.
// TODO: Add `erase` anc all css color alpha support. 23.07.20.14.45
// TODO: Add transparency and short hex to hex support.
// TODO: Add better hex support via: https://stackoverflow.com/a/53936623/8146077
function color() {
  let args = [...arguments];

  if (args.length === 1 && args[0] !== undefined) {
    const isNumber = () => typeof args[0] === "number";
    const isArray = () => Array.isArray(args[0]);
    const isString = () => typeof args[0] === "string";

    // If it's not a Number or Array or String, then assume it's an object,
    // randomly pick a key & re-run.
    if (!isNumber() && !isArray() && !isString())
      return color(help.any(args[0]));

    // Single number argument.
    if (isNumber()) {
      // Treat as raw hex if we hit a certain limit.
      if (args[0] > 255) {
        args = num.hexToRgb(args[0]);
      } else {
        // Otherwise, replicate the first number across all three fields.
        args = Array.from(args);
        args.push(args[0], args[0]);
      }
    } else if (isArray()) {
      // Or if it's an array, then spread it out and re-ink.
      // args = args[0];
      return color(...args[0]);
    } else if (isString()) {
      // See if it's a hex.
      const cleanedHex = args[0].replace("#", "").replace("0x", "");
      if (num.isHexString(cleanedHex) === true) {
        args = num.hexToRgb(cleanedHex);
      } else if (args[0] === "erase") {
        args = [-1, -1, -1];
        // if (args[1]) alpha = parseFloat(args[1]);
      } else {
        args = num.cssColors[args[0]]; // Try to match it to a table.
      }

      // TODO: Add an error message here. 22.08.29.13.03
    }
  } else if (args.length === 2) {
    // rgb, a
    args = [arguments[0], arguments[0], arguments[0], arguments[1]];
  } else if (
    args.length === 0 ||
    (args.length === 1 && args[0] === undefined)
  ) {
    args = num.randIntArr(255, 3);
    args.push(255); // Generate random values here, always leave alpha 255.
  }

  if (args.length === 3) args = [...args, 255]; // Always be sure we have alpha.

  // Randomized any undefined or null values across all 4 arguments.
  args.forEach((a, i) => {
    if (isNaN(args[i])) args[i] = num.randInt(255);
  });

  return args;
}

function ink() {
  return graph.color(...color(...arguments));
}

// 🔎 PAINTAPI (for searching)
const $paintApi = {
  // 1. Composite functions (that use $activePaintApi)
  //    (Must be unwrapped)

  // Prints a line of text using the default / current global font.
  // Argument options:
  // text, pos: {x, y, center}, bg (optional)
  write: function (text, pos, bg) {
    if (!text) return; // Fail silently if no text.
    tf?.print($activePaintApi, pos, 0, text.toString(), bg); // Fail on preamble.
    return $activePaintApi;
  },
  // 2. Image Utilities
  clonePixels: graph.cloneBuffer,
  color,
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
  }
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
  plot: function () {
    if (arguments.length === 1) {
      graph.plot(arguments[0].x, arguments[0].y);
    } else {
      graph.plot(...arguments);
    }
  }, // TODO: Should this be renamed to set?
  point: graph.point,
  line: function () {
    const out = graph.line(...arguments);
    twoDCommands.push(["line", ...out]);
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
      const pix = graph.makeBuffer(...arguments, painting, painting.api);

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
  if (screen.width === width && screen.height === height && gap === undefined)
    return;

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
    screen.height
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

// Microphone State (Audio Input)
class Microphone {
  amplitude = 0;
  waveform = [];
  pitch = 0;
  connected = false; // Flips to true on a callback message from `bios`.

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
}

class Speaker {
  waveforms = { left: [], right: [] };
  amplitudes = { left: 0, right: 0 };

  poll() {
    send({ type: "get-speaker-waveforms" });
    send({ type: "get-speaker-amplitudes" });
  }
}

const speaker = new Speaker();
const microphone = new Microphone();

// 2. ✔ Loading the disk.
let originalHost;
let firstLoad = true;

async function load(
  parsed, // If parsed is not an object, then assume it's source code.
  fromHistory = false,
  alias = false,
  devReload = false,
  loadedCallback
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
      "(Already loading.)"
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
      console.log("Path:", path);
      path = [...path.split("/").slice(0, -1), hiddenSlug].join("/");
    }

    // Update the user handle if it changed between pieces.
    // TODO: This is not an optimal spot for this. 23.07.01.22.38
    if (store["handle:updated"]) {
      $commonApi.handle = "@" + store["handle:updated"];
      delete store["handle:updated"];
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
        codeChannel || "N/A"
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
    if (slug.split("~")[0] === currentText?.split("~")[0] && !devReload) {
      const blob = new Blob([currentCode], { type: "application/javascript" });
      blobUrl = URL.createObjectURL(blob);
      sourceCode = currentCode;
    } else {
      let response, sourceToRun;
      if (fullUrl) {
        console.log("Attempting to load from local url:", fullUrl);
        response = await fetch(fullUrl);
        sourceToRun = await response.text();
      } else {
        sourceToRun = source;
      }

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
        }
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

    module = await import(blobUrl);
  } catch (err) {
    // 🧨 Continue with current module if one has already loaded.
    console.error(`😡 "${path}" load failure:`, err);
    loadFailure = err;
    loading = false;
    return false;
  }
  // console.log("Module load time:", performance.now() - moduleLoadTime, module);

  // 🧨 Fail out if no module is found.
  if (module === undefined) {
    loading = false;
    return false;
  }

  // 🧩 Piece code has been loaded...
  //    Now we can instantiate the piece.

  pieceHistoryIndex += fromHistory === true ? 0 : 1; // Adjust the history.

  if (!debug && !firstLoad) {
    console.clear();
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
      console.log("💾️ Reloading piece...", piece);
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
        devReload
      );
    }
  };

  // Start the socket server
  // TODO: Before we load the disk, in case of needing to reload remotely on failure? 23.01.27.12.48
  let receiver; // Handles incoming messages from the socket.
  const forceProd = false; // For testing prod socket servers in development.

  // Requests a session-backend and connects via websockets.
  function startSocket() {
    if (debug) console.log("🧦 Initializing socket server...");
    socket?.kill(); // Kill any already open socket from a previous disk.
    socket = new Socket(debug); // Then redefine and make a new socket.

    const monolith = "monolith"; // or undefined for horizontal scaling.

    session(slug, forceProd, monolith)
      .then((sesh) => {
        socket.connect(
          new URL(sesh.url).host,
          (id, type, content) => {
            // Globally receivable messages...
            // (There are also some messages handled in `Socket`)
            // 😱 Scream at everyone who is connected!
            if (type === "scream") {
              console.log("😱 Scream:", content, "❗");
              scream = content;
              return;
            }

            // 🧚 Ambient cursor support.
            if (type === "ambient-pen:point" && socket.id !== id) {
              console.log(socket, id);
              ambientPenPoints.push({ x: content.x, y: content.y });
              return;
            }

            // 🧩 Pieces get all other messages not caught in `Socket`.
            receiver?.(id, type, content); // Run the piece receiver.
          },
          $commonApi.reload,
          "wss",
          () => {
            // Post-connection logic.
            if (codeChannel) socket.send("code-channel:sub", codeChannel);
          }
        );
      })
      .catch((err) => {
        console.error("Socket connection error:", err);
      });
  }

  // Delay session server by .75 seconds in order to prevent redundant
  //  connections being opened as pieces are quickly re-routing and jumping.
  clearTimeout(socketStartDelay);
  socketStartDelay = setTimeout(() => startSocket(), 250);

  $commonApi.net.socket = function (receive) {
    receiver = receive || (() => {});
    if (!socket) {
      // Just in case we init. in a `boot` before the timeout fires above.
      clearTimeout(socketStartDelay);
      startSocket();
    }
    return socket;
  };

  // This would also get the source code, in case meta-programming is needed.
  // const source = await (await fetch(fullUrl)).text();

  if (!alias) currentHUDText = slug; // Update hud text if this is not an alias.
  if (module.nohud) currentHUDText = undefined; // Don't use hud text if needed.
  currentHUDOffset = undefined; // Always reset these to the defaults.
  currentHUDTextColor = undefined;
  currentHUDButton = undefined;

  // ***Client Metadata Fields***
  // Set default metadata fields for SEO and sharing,
  // (requires serverside prerendering, also via `index.js`).
  let meta;

  if (alias === false) {
    // Parse any special piece metadata.
    const { title, desc, ogImage, twitterImage } = metadata(
      "aesthetic.computer",
      slug,
      // Adding the num API here is a little hacky, but needed for Freaky Flowers random metadata generation. 22.12.27
      module.meta?.({ ...parsed, num: $commonApi.num, store: $commonApi.store })
    );

    meta = {
      title,
      desc, // Note: This doesn't auto-update externally hosted module descriptions, and may never need to? 22.07.19.06.00
      img: {
        og: ogImage,
        twitter: twitterImage,
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
  $commonApi.net.rewrite = (path) => {
    send({ type: "rewrite-url-path", content: { path } }); // Jump the browser to a new url.
  };

  // Add host to the networking api.
  $commonApi.net.host = host;

  // Add web to the networking api.
  $commonApi.net.web = (url) => {
    send({ type: "web", content: url }); // Jump the browser to a new url.
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
    progressReport
  ) {
    let extension;
    if (soundWhitelist.includes(path)) {
      // Use shortnames for system sounds that are in the `preloadWhitelist`.
      extension = "m4a";
    } else {
      // Overload path with an object that can set a custom extension.
      // Implemented in `ordfish`. 23.05.08.14.07
      if (typeof path === "object") {
        extension = path.extension; // Custom extension.
        path = path.path; // Remap path reference to a string.
      } else {
        // Assume path is a string with a file extension,
        // filtering out any query parameters.
        extension = path.split(".").pop().split("?")[0];
      }

      // Remove any prepending "/" because it's already relative to root.
      if (path.indexOf("/") === 0) path = path.slice(1);

      // This is a hack for now. The only thing that should be encoded is the file slug.
      if (!path.startsWith("https://")) path = encodeURIComponent(path);

      try {
        const url = new URL(path);
        if (url.protocol === "demo:") {
          // Load from aesthetic.computer host.
          path = `/demo/${url.pathname}`;
        } else if (url.protocol === "https:") {
          // No need to change path because an original URL was specified.
        }
      } catch {
        // Not a valid URL so assume local file on disk server.
        path = `${location.protocol}//${$commonApi.net.host}/${path}`;
      }
    }

    // If we are loading a .json file then we can parse or not parse it here.
    if (extension === "json") {
      return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        xhr.open("GET", path, true);
        xhr.onprogress = function (event) {
          const progress = event.loaded / event.total;
          if (debug && logs.download)
            console.log(`💈 JSON Download: ${progress * 100}%`);
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
      // 🖼️ Image files.
      extension === "webp" ||
      extension === "jpg" ||
      extension === "png"
    ) {
      return new Promise((resolve, reject) => {
        send({ type: "load-bitmap", content: path });
        preloadPromises[path] = { resolve, reject };
      });
    } else if (extension === "m4a") {
      // 🔈 Audio files
      return new Promise((resolve, reject) => {
        send({ type: "sfx:load", content: path });
        preloadPromises[path] = { resolve, reject };
      });
    }
  };

  $commonApi.slug = slug;
  $commonApi.query = search;
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

  // A wrapper for `load(parse(...))`
  // Make it `ahistorical` to prevent a url change.
  // Make it an `alias` to prevent a metadata change for writing landing or
  // router pieces such as `freaky-flowers` -> `wand`. 22.11.23.16.29
  // Jump delay...
  $commonApi.jump = function jump(to, ahistorical = false, alias = false) {
    leaving = true;

    let url;
    if (to.startsWith("http")) {
      try {
        url = new URL(to);
      } catch (e) {
        // Could not construct a valid url from the jump, so we will be
        // running a local aesthetic.computer piece.
      }
    }

    let callback;
    leaveLoad = url
      ? () => $commonApi.net.web(to)
      : () => load(parse(to), ahistorical, alias, false, callback);
    return (cb) => (callback = cb);
  };

  $commonApi.alias = function alias(name, colon, params) {
    $commonApi.jump(
      name +
        colon.map((c) => `:` + c).join("") +
        params.map((p) => `~` + p).join(""),
      true,
      false
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

  $commonApi.pieceCount += 1;

  // Load typeface if it hasn't been yet.
  // (This only has to happen when the first piece loads.)
  if (!tf) tf = await new Typeface().load($commonApi.net.preload);
  $commonApi.typeface = tf; // Expose a preloaded typeface globally.

  // This function actually hotSwaps out the piece via a callback from `bios` once fully loaded via the `loading-complete` message.
  hotSwap = () => {
    loadedCallback?.(); // Run the optional load callback. (See also: `jump`)

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
            hint: module.system.split(":")[1], // See `ask.ts`.
            forgetful: module.forgetful || false,
          },
          module.reply,
          module.halt,
          module.scheme,
          wrap,
          module.copied,
          module.activated
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
    } else {
      boot = module.boot || defaults.boot;
      sim = module.sim || defaults.sim;
      paint = module.paint || defaults.paint;
      beat = module.beat || defaults.beat;
      act = module.act || defaults.act;
      leave = module.leave || defaults.leave;
      system = null;

      // delete $commonApi.system.name; // No system in use.
    }

    // ♻️ Reset global state for this piece.
    paintCount = 0n;
    paintingAPIid = 0n;
    simCount = 0n;
    booted = false;
    // initialSim = true;
    activeVideo = null;
    preloadPromises = {};
    noPaint = false;
    formsSent = {}; // Clear 3D list for GPU.
    currentPath = path;
    currentHost = host;
    currentSearch = search;
    currentColon = colon;
    currentParams = params;
    currentHash = hash;
    sound = null;
    soundClear = null;

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
      // noBeat: beat === defaults.beat,
    },
  });

  return true; // Loaded succesfully.
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
let codeChannel = await store.retrieve("code-channel");
if (codeChannel?.length > 0) console.log("💻 Code channel:", codeChannel);

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
    graph.setDebug(content.debug);
    ROOT_PIECE = content.rootPiece;
    USER = content.user;
    $commonApi.user = USER;
    handle();
    originalHost = content.parsed.host;
    loadAfterPreamble = () => {
      loadAfterPreamble = null;
      load(content.parsed); // Load after some of the default frames run.
    };
    send({ type: "disk-defaults-loaded" });
    return;
  }

  if (type === "loading-complete") {
    loading = false;
    leaving = false;
    hotSwap?.(); // Actually swap out the piece functions and reset the state.
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
        content.source
      );
    } else {
      console.warn("🖼️ Dropped images only function in the `prompt`.");
    }
    return;
  }

  if (type === "paste:pasted") {
    actAlerts.push("clipboard:paste:pasted");
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
    if (current !== null) {
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
      $commonApi.rec.printing = false;
      // TODO: Is this the best place for this signal to be sent?
      //       Maybe it should go back in the BIOS? 22.08.19.13.44
    }
    return;
  }

  if (type === "recorder:rolling:started") {
    $commonApi.rec.recording = true;
    return;
  }

  if (type === "recorder:rolling:ended") {
    $commonApi.rec.recording = false;
    $commonApi.rec.recorded = true; // Also cleared when a recording "slates".
    $commonApi.rec.cutCallback?.();
    return;
  }

  if (type === "recorder:printing:started") {
    $commonApi.rec.printing = true;
    return;
  }

  if (type === "recorder:printed") {
    $commonApi.rec.printed = true;
    $commonApi.rec.printCallback?.();
    $commonApi.rec.printing = false;
    return;
  }

  if (type === "recorder:presented") {
    $commonApi.rec.presenting = true;
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

  if (type === "microphone-connect:success") {
    microphone.connected = true;
    actAlerts.push("microphone-connect:success");
    return;
  }

  if (type === "microphone-connect:failure") {
    microphone.connected = false;
    actAlerts.push("microphone-connect:failure");
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
    serverUploadProgressReporter?.(0);
    serverUpload = undefined;
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
      console.error("Failed to authenticate.", content);
      authorizationRequest?.reject(content.data);
    }
    authorizationRequest = undefined;
    return;
  }

  // 1b. Video frames.
  if (type === "video-frame") {
    activeVideo = content;
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
    if (debug) console.log("Bitmap load success:", content);
    preloadPromises[content.url].resolve(content.img);
    delete preloadPromises[content];
    return;
  }

  if (type === "loaded-bitmap-rejection") {
    if (debug) console.error("Bitmap load failure:", content);
    preloadPromises[content.url].reject(content.url);
    delete preloadPromises[content.url];
    return;
  }

  // 1e. Loading Sound Effects
  if (type === "loaded-sfx-success") {
    if (debug && logs.sound) console.log("Sound load success:", content);
    preloadPromises[content.sfx].resolve(content.sfx);
    delete preloadPromises[content];
    return;
  }

  if (type === "loaded-sfx-rejection") {
    if (debug) console.error("Sound load failure:", content);
    preloadPromises[content.sfx].reject(content.sfx);
    delete preloadPromises[content.sfx];
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
    soundClear();
    return;
  }

  // 2. Frame
  // Where each piece action (boot, sim, paint, etc...) is run.
  if (type === "frame") {
    // 🌟 Global Keyboard Shortcuts (these could also be seen via `act`)
    content.keyboard.forEach((data) => {
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

        if (
          data.key === "`" &&
          currentPath !== "aesthetic.computer/disks/prompt"
        ) {
          // $api.send({ type: "keyboard:enabled" }); // Enable keyboard flag.
          // $api.send({ type: "keyboard:unlock" });
          // Jump to prompt if the backtic is pressed.
          $commonApi.jump("prompt")(() => {
            send({ type: "keyboard:open" });
          });
        }

        // [Ctrl + X]
        // Enter and exit fullscreen mode.
        if (data.key === "x" && data.ctrl) {
          send({ type: "fullscreen-enable" });
        }
      }
    });

    // Take hold of a previously worker transferrable screen buffer
    // and re-assign it.
    let pixels;
    if (content.pixels) {
      pixels = new Uint8ClampedArray(content.pixels);
      if (screen) screen.pixels = pixels;
    }

    // Add 'loading' status to $commonApi.
    $commonApi.loading = loading; // Let the piece know if we are already
    //                               loading another piece.
    $commonApi.leaving = () => leaving; // Set a flag to tell whether we are leaving.

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
        true
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

      $commonApi.pen = primaryPointer; // || { x: undefined, y: undefined };

      if (
        primaryPointer &&
        (primaryPointer.delta?.x !== 0 || primaryPointer.delta?.y !== 0)
      ) {
        socket?.send("ambient-pen:point", {
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
    const $sound = {
      time: content.audioTime,
      // Get the bpm with bpm() or set the bpm with bpm(newBPM).
      bpm: function (newBPM) {
        if (newBPM) sound.bpm = newBPM;
        return sound.bpm;
      },
    };

    $sound.microphone = microphone;
    $sound.speaker = speaker;

    // TODO: Generalize square and bubble calls.
    // TODO: Move this stuff to a "sound" module.
    sound = {
      bpm: content.audioBpm,
      sounds: [],
      bubbles: [],
      kills: [],
    };

    // Clear synchronized audio triggers.
    soundClear = () => {
      sound.sounds.length = 0;
      sound.bubbles.length = 0;
      sound.kills.length = 0;
    };

    // Trigger a named audio sample to playback in the `bios`.
    // options: { volume: 0-n }
    $sound.play = async function (sfx, options) {
      send({ type: "sfx:play", content: { sfx, options } });
    };

    $sound.synth = function ({
      type = "square",
      tone = 440, // TODO: Make random.
      beats = Math.random(), // Wow, default func. params can be random!
      attack = 0,
      decay = 0,
      volume = 1,
      pan = 0,
    } = {}) {
      const id = soundId;
      sound.sounds.push({ id, type, tone, beats, attack, decay, volume, pan });
      soundId += 1n;

      // Return a progress function so it can be used by rendering.
      const seconds = (60 / content.audioBpm) * beats;
      const end = content.audioTime + seconds;

      return {
        id,
        kill: function () {
          sound.kills.push(id);
        },
        progress: function (time) {
          return 1 - Math.max(0, end - time) / seconds;
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
      Object.keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      Object.keys($updateApi).forEach((key) => ($api[key] = $updateApi[key]));
      Object.keys(painting.api).forEach(
        (key) => ($api[key] = painting.api[key])
      );
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      $api.inFocus = inFocus;

      // Don't pass pixels to updates.
      $api.screen = {
        width: content.width,
        height: content.height,
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
      if (content.clipboardText) {
        const data = { text: content.clipboardText };
        Object.assign(data, {
          device: "none",
          is: (e) => e === "pasted:text",
        });
        $api.event = data;
        try {
          act($api);
        } catch (e) {
          console.warn("️ ✒ Act failure...", e);
        }
      }

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
        try {
          act($api);

          // Always check to see if there was a tap on the corner.
          const { event: e, jump, send } = $api;
          let originalColor;

          // TODO: Show keyboard immediately when returning to prompt.
          currentHUDButton?.act(e, {
            down: () => {
              originalColor = currentHUDTextColor;
              currentHUDTextColor = [0, 255, 0];
              send({ type: "keyboard:enabled" }); // Enable keyboard flag.
              send({ type: "keyboard:unlock" });
              $api.needsPaint();
            },
            push: () => {
              // send({ type: "keyboard:open" });
              jump("prompt");
              // pieceHistoryIndex > 0
              //   ? send({ type: "back-to-piece" })
              //   : jump("prompt");
              $api.needsPaint();
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
              // if (btn) send({ type: "keyboard:unlock" });
            },
            rollout: () => {
              // send({ type: "keyboard:lock" });
            },
          });
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
      Object.keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      Object.keys(painting.api).forEach(
        (key) => ($api[key] = painting.api[key])
      );
      $api.api = $api; // Add a reference to the whole API.

      cachedAPI = $api; // Remember this API for any other acts outside
      // of this loop, like a focus change or custom act broadcast.

      // Object.assign($api, $commonApi);
      // Object.assign($api, painting.api);

      $api.paintCount = Number(paintCount);

      $api.inFocus = content.inFocus;

      $api.glaze = function (content) {
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

        graph.writeBuffer.length = screen.width * screen.height;

        graph.writeBuffer.fill(0);
      }

      // TODO: Disable the depth buffer for now... it doesn't need to be
      //       regenerated on every frame.
      // graph.depthBuffer.fill(Number.MAX_VALUE); // Clear depthbuffer.
      graph.writeBuffer.fill(0); // Clear writebuffer.

      $api.screen = screen;
      $api.screen.center = { x: screen.width / 2, y: screen.height / 2 };

      $api.fps = function (newFps) {
        send({ type: "fps-change", content: newFps });
      };

      $api.cursor = (code) => (cursorCode = code);

      /**
       * @function video
       * @descrption Make a live video feed. Returns an object that links to current frame.
       * @param {string} type - "camera" or "camera-update" or see below. 💡
       * @param {object} options - *unimplemented* { src, width, height }
       */
      $api.video = function (type, options) {
        // TODO: Options could eventually be { src, width, height }
        // const vid = video("youtube-link");
        // const vid = video("tiktok:@whistlegraph");
        // https://codepen.io/oceangermanique/pen/LqaPgO
        if (type === "camera:update") activeVideo = null;

        send({ type: "video", content: { type, options } });

        // Return an object that can grab whatever the most recent frame of
        // video was.
        return function videoFrame(shader) {
          if (activeVideo) {
            const { width, pixels } = activeVideo;

            if (shader) {
              for (let i = 0; i < pixels.length; i += 4) {
                const c = pixels.subarray(i, i + 4);
                const p = { x: (i / 4) % width, y: floor(i / 4 / width) };
                shader(p, c);
              }
            }
          }

          return activeVideo;
        };
      };

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
            "local:db"
          );

          store["painting:transform"] = await store.retrieve(
            "painting:transform",
            "local:db"
          );

          addUndoPainting(store["painting"]);
        }

        const sys = $commonApi.system;
        sys.painting = store["painting"];

        sys.nopaint.translation =
          store["painting:transform"]?.translation || sys.nopaint.translation;

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

      // Attempt a paint.
      //if (noPaint === false && booted && loading === false) {
      if (
        (noPaint === false || scream || ambientPenPoints.length > 0) &&
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

        // 😱 Scream - Paint a scream if it exists.
        // TODO: Should this overlay after the fact and not force a paint? 23.05.23.19.21
        //       Yes probably, because of layering issues?
        if (scream || screaming) {
          const { ink, needsPaint } = $api;
          ink(255)
            .wipe(255, 0, 0)
            .write(scream, { center: "xy", size: 3, thickness: 1 });
          needsPaint();
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
        const { ink, needsPaint } = $api;
        ambientPenPoints.forEach(({ x, y }) => {
          ink().point(x * screen.width, y * screen.height);
        });
        if (ambientPenPoints.length > 0) {
          needsPaint();
          if (system === "nopaint") $api.system.nopaint.needsPresent = true;
        }
        ambientPenPoints.length = 0;

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

      // TODO: Why is this being composited by a different thread?
      //       Also... where do I put a scream?

      // System info.
      let label;
      const piece = currentHUDText?.split("~")[0];
      const defo = 6; // Default offset

      if (
        piece !== undefined &&
        piece.length > 0 &&
        piece !== "prompt" &&
        piece !== "play" &&
        piece !== "gargoyle" &&
        piece !== "girlfriend" &&
        piece !== "boyfriend" &&
        piece !== "botce" &&
        piece !== "dad" &&
        piece !== "decode" &&
        piece !== "liar" &&
        piece !== "mom" &&
        piece !== "encode" &&
        piece !== "alphapoet" &&
        piece !== "sing" &&
        piece !== "neoprompt" &&
        piece !== "video"
      ) {
        const w = currentHUDText.length * 6;
        const h = 11;
        label = $api.painting(w, h, ($) => {
          // $activePaintApi = $;
          $.ink(0).write(currentHUDText?.replaceAll("~", " "), { x: 1, y: 1 });
          let c;
          if (currentHUDTextColor) {
            c = num.shiftRGB(currentHUDTextColor, [255, 255, 255], 0.75);
          } else {
            c = [255, 200, 240];
          }
          $.ink(c).write(currentHUDText?.replaceAll("~", " "), { x: 0, y: 0 });
        });

        currentHUDButton =
          currentHUDButton ||
          new $api.ui.Button({
            x: 0,
            y: 0,
            w: w + (currentHUDOffset?.x || defo),
            h: h + (currentHUDOffset?.y || defo),
          });
        $commonApi.hud.currentLabel = {
          text: currentHUDText,
          btn: currentHUDButton,
        };
      }

      maybeLeave();

      // Return frame data back to the main thread.
      let sendData = { width: screen.width, height: screen.height };

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
          x: currentHUDOffset?.x || defo,
          y: currentHUDOffset?.y || defo,
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
        [pixels?.buffer]
      );
    }

    // Wait 8 frames of the default piece before loading the next piece.
    if (paintCount > 8n) loadAfterPreamble?.(); // Start loading after the first disk if necessary.

    soundClear();

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
async function handle() {
  if (USER) {
    try {
      const response = await fetch(`/handle?for=${USER.sub}`);
      if (response.status === 200) {
        const data = await response.json();
        const newHandle = "@" + data.handle;
        if (newHandle !== $commonApi.handle) {
          $commonApi.handle = "@" + data.handle;
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

// Run the piece's "leave" function which will trigger
// a new load before sending off the final frame.
function maybeLeave() {
  // 🚪 Leave (Skips act and sim and paint...)
  if (leaving) {
    // End the socket connection before switching pieces if one exists.
    socket?.kill();
    socket = undefined;

    try {
      leave({ ...painting.api, screen, ...$commonApi }); // Trigger leave.
    } catch (e) {
      console.warn("👋 Leave failure...", e);
    }
    leaving = false;
    leaveLoad?.();
  }
}

// Default template string behavior: https://stackoverflow.com/a/64298689/8146077
function defaultTemplateStringProcessor(strings, ...vars) {
  let result = "";
  strings.forEach((str, i) => {
    result += `${str}${i === strings.length - 1 ? "" : vars[i]}`;
  });
  return result;
}
