// 👩‍💻 Disk (Jockey) aka the tape / piece player?
import * as graph from "./graph.mjs";
import * as num from "./num.mjs";
import * as geo from "./geo.mjs";
import * as gizmo from "./gizmo.mjs";
import * as ui from "./ui.mjs";
import * as help from "./help.mjs";
import { parse, metadata } from "./parse.mjs";
import { Socket } from "./socket.mjs"; // TODO: Eventually expand to `net.Socket`
//import { UDP } from "./udp.mjs"; // TODO: Eventually expand to `net.Socket`
import { notArray } from "./helpers.mjs";
const { round } = Math;
import { nopaint_adjust } from "../systems/nopaint.mjs";
import { headers } from "./console-headers.mjs";

export const noWorker = { onMessage: undefined, postMessage: undefined };

const { abs, cos, sin } = Math;

let ROOT_PIECE = "prompt"; // This gets set straight from the host html file for the ac.
let debug = false; // This can be overwritten on boot.

const defaults = {
  boot: ($) => {
    $.cursor("native");
  }, // aka Setup
  sim: () => false, // A framerate independent of rendering.
  paint: ($) => {
    // TODO: Make this a boot choice via the index.html file?
    //$.noise16DIGITPAIN();
    //$.noiseTinted([20, 20, 20], 0.8, 0.7);
    //$.wipe(0, 0, 0);
  },
  beat: () => false, // Runs every bpm.
  act: () => false, // All user interaction.
  leave: () => false, // Before unload.
};

// 🔎 NoPaint
// Inheritable via `export const system = "nopaint"` from any piece.
// Boilerplate for a distributed raster editor.
const nopaint = {
  boot: function boot({ paste, painting, store, screen, system: sys }) {
    //if (!screen.load("painting")) wipe(64); // Load painting or wipe to gray.
    nopaint_adjust(screen, sys, painting, store);
    paste(sys.painting);
  },
  act: function act({
    event: e,
    system: sys,
    painting,
    download,
    paste,
    num,
    screen,
    store,
  }) {
    if (e.is("keyboard:down:enter")) {
      download(`painting-${num.timestamp()}.png`, sys.painting, { scale: 4 });
    }

    if (e.is("reframed")) {
      nopaint_adjust(screen, sys, painting, store);
      paste(sys.painting);
    }
  },
  leave: function leave({ store, system }) {
    store["painting"] = system.painting;
    store.persist("painting", "local:db");
  },
};

let boot = defaults.boot;
let sim = defaults.sim;
let paint = defaults.paint;
let beat = defaults.beat;
let act = defaults.act;
let leave = defaults.leave;

let currentPath,
  currentHost,
  currentSearch,
  currentParams,
  currentHash,
  currentText;
let loading = false;
let reframe;
let screen;
let currentDisplay; // TODO: Remove this? 22.09.29.11.38
let cursorCode;
let pieceHistoryIndex = -1; // Gets incremented to 0 when first piece loads.
let paintCount = 0n;
let simCount = 0n;
let booted = false;
let initialSim = true;
let noPaint = false;

let storeRetrievalResolution, storeDeletionResolution;

let socket;
let pen = {};

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

// Promises
let fileImport;
let serverUpload;
let gpuResponse;
let web3Response;

// Other
let activeVideo; // TODO: Eventually this can be a bank to store video textures.
let bitmapPromises = {};
let inFocus;
let loadFailure;

// 1. ✔ API

// TODO: Eventually add a wiggle bank so all wiggles are indexed
//       and start at random angles.
let wiggler = 0;
let wiggleAngle = 0;

// For every function to access.
const $commonApi = {
  // Trigger background music.
  // Eventually add an "@" style parameter similar to what a stamp system would have.
  send,
  bgm: {
    set: function (trackNumber) {
      send({ type: "bgm-change", content: { trackNumber } });
    },
    stop: () => send({ type: "bgm-stop" }),
    data: {},
  },
  system: {},
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
  num: {
    even: num.even,
    odd: num.odd,
    clamp: num.clamp,
    randInt: num.randInt,
    randIntArr: num.randIntArr,
    randIntRange: num.randIntRange,
    multiply: num.multiply,
    dist: num.dist,
    dist3d: num.dist3d,
    radians: num.radians,
    degrees: num.degrees,
    lerp: num.lerp,
    map: num.map,
    Track: num.Track,
    timestamp: num.timestamp,
    vec2: num.vec2,
    vec3: num.vec3,
    vec4: num.vec4,
    mat3: num.mat3,
    mat4: num.mat4,
    quat: num.quat,
    saturate: num.saturate,
    desaturate: num.desaturate,
    shiftRGB: num.shiftRGB,
    rgbToHexStr: num.rgbToHexStr,
    hexToRgb: num.hexToRgb,
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
  },
  help: {
    choose: help.choose,
    repeat: help.repeat,
    every: help.every,
    any: help.any,
    each: help.each,
  },
  gizmo: { Hourglass: gizmo.Hourglass },
  rec: {
    rolling: (opts) => send({ type: "recorder-rolling", content: opts }),
    cut: () => send({ type: "recorder-cut" }),
    print: () => send({ type: "recorder-print" }),
    printProgress: 0,
  },
  net: {},
  needsPaint: () => (noPaint = false), // TODO: Does "paint" needs this?
  store,

  pieceCount: -1, // Incs to 0 when the first piece (usually the prompt) loads.
  //                 Increments by 1 each time a new piece loads.
  debug,
};

// Spawn a session backend for a piece.
async function session(slug, forceProduction = false) {
  let endPoint = "/session/" + slug;

  if (forceProduction)
    endPoint += "?" + new URLSearchParams({ forceProduction: 1 });

  const req = await fetch(endPoint);

  const session = await req.json();

  if (debug) console.log("🐕‍🦺 Session:", session.backend);

  // Return the active session if the server knows it's "Ready", otherwise
  // wait for the one we requested to spin up before doing anything else.
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
      } else {
        // Try to match it to a table.
        const colors = {
          red: [255, 0, 0],
          green: [0, 255, 0],
          blue: [0, 0, 255],
        };
        args = colors[args[0]];
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
  return args;
}

function ink() {
  graph.color(...color(...arguments));
}

// 🔎 PAINTAPI (for searching)
const $paintApi = {
  // Image Utilities
  clonePixels: graph.cloneBuffer,
  color,
  // 3D Classes & Objects
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
  l: graph.line,
  i: ink,
  // Defaults
  page: graph.setBuffer,
  edit: graph.changePixels, // Edit pixels by pasing a callback.
  ink, // Color
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
  line: graph.line,
  lineAngle: graph.lineAngle,
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
  noiseTinted: graph.noiseTinted,
  // glaze: ...
};

let formsSent = {}; // TODO: This should be cleared more often...
let paintFormsResolution;

// TODO: Eventually restructure this a bit. 2021.12.16.16.0
//       Should global state like color and transform be stored here?
class Painting {
  #layers = [];
  #layer = 0;
  api = {};

  constructor() {
    Object.assign(this.api, $paintApi);
    const p = this;

    // Filter for and then wrap every rendering behavior of $paintApi into a
    // system to be deferred in groups, using layer.
    // ⛓️ This wrapper also makes the paint API chainable.
    for (const k in $paintApiUnwrapped) {
      if (typeof $paintApiUnwrapped[k] === "function") {
        // Wrap and then transfer to #api.
        p.api[k] = function () {
          if (notArray(p.#layers[p.#layer])) p.#layers[p.#layer] = [];
          // deferred action called as paint() below.
          p.#layers[p.#layer].push(() => {
            $paintApiUnwrapped[k](...arguments);
          });
          // p.#layers[p.#layer].push(async () => {
          //   await $paintApiUnwrapped[k](...arguments);
          // });
          return p.api;
        };
      }
    }

    // Creates a new pixel buffer with its own layering wrapper / context
    // on top of the base painting API.
    this.api.painting = function () {
      return graph.makeBuffer(...arguments, new Painting());
    };

    this.api.pixel = function () {
      return graph.pixel(...arguments);
    };

    // Allows grouping & composing painting order using an AofA (Array of Arrays).
    // n: 0-n (Cannot be negative.)
    // fun: A callback that contains $paintApi commands or any other code.
    this.api.layer = function (n) {
      p.#layer = n;
      return p.api;
    };

    // This links to abstract, solitary graph functions that do not need
    // to be wrapped or deferred for rendering.
    // TODO: Maybe these functions should be under a graphics algorithms label?
    this.api.abstract = { bresenham: graph.bresenham };
  }

  // Paints every layer.
  //async paint(immediate = false) {
  paint(immediate = false) {
    /*
    this.#layers.forEach((layer) => {
      //layer.forEach(async (paint) => await paint());
      for (const paint of layer) {
        console.log(paint);
        await paint();
      }
    });
    */

    for (const layer of this.#layers) {
      for (const paint of layer) {
        //        if (immediate) {
        paint();
        //       } else {
        //        await paint();
        //      }
      }
    }

    this.#layers.length = 0;
    this.#layer = 0;
  }
}

const painting = new Painting();

$commonApi.flatten = function () {
  return painting.paint();
};

let glazeAfterReframe;

// Microphone State (Audio Input)
class Microphone {
  amplitude = 0;
  waveform = [];

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
  }
}

const microphone = new Microphone();

// 2. ✔ Loading the disk.
let originalHost;
let lastHost; // = "disks.aesthetic.computer"; TODO: Add default host here.
let firstLoad = true;
let firstPiece, firstParams, firstSearch;

async function load(parsed, fromHistory = false, alias = false) {
  let { path, host, search, params, hash, text: slug } = parsed;

  send({ type: "alert-popup:instagram", content: JSON.stringify(parsed) });

  if (loading === false) {
    loading = true;
  } else {
    // TODO: Implement some kind of loading screen system here?
    console.warn("Already loading another disk:", path);
    return;
  }

  if (path)
    if (debug) {
      console.log("🟡 Development");
    } else {
      // console.log("🟢 Production");
    }

  if (host === "") {
    host = originalHost;
  }

  loadFailure = undefined;
  host = host.replace(/\/$/, ""); // Remove any trailing slash from host. Note: This fixes a preview bug on teia.art. 2022.04.07.03.00
  lastHost = host; // Memoize the host.
  pieceHistoryIndex += fromHistory === true ? -1 : 1;

  // Kill any existing socket that has remained open from a previous disk.
  socket?.kill();

  // Set the empty path to whatever the "/" route piece was.
  if (path === "") path = ROOT_PIECE;
  if (path === firstPiece && params.length === 0) params = firstParams;

  if (!debug && !firstLoad) {
    console.clear();
    headers(); // Clear console and re-print headers if we are in production.
  }

  console.log("🧩", path, "🌐", host);

  let fullUrl = location.protocol + "//" + host + "/" + path + ".mjs";

  // let fullUrl = "https://" + host + "/" + path + ".js";
  // The hash `time` parameter busts the cache so that the environment is
  // reset if a disk is re-entered while the system is running.
  // Why a hash? See also: https://github.com/denoland/deno/issues/6946#issuecomment-668230727
  fullUrl += "#" + Date.now();

  if (debug) console.log("🕸", fullUrl);

  // 🅰️ Start the socket server before we load the disk, in case of needing
  // to reload remotely on failure.
  let receiver; // Handles incoming messages from the socket.

  // Add debug to the common api.
  $commonApi.debug = debug;

  // Add reload to the common api.
  $commonApi.reload = ({ piece }) => {
    if (piece === "*refresh*") {
      console.log("💥️ Restarting system...");
      send({ type: "refresh" }); // Refresh the browser.
    } else if (piece === "*" || piece === undefined || currentText === piece) {
      console.log("💾️ Reloading piece...", piece);
      // Reload the disk.
      load(
        {
          path: currentPath,
          host: currentHost,
          search: currentSearch,
          params: currentParams,
          hash: currentHash,
          text: currentText,
        },
        // Use the existing contextual values when live-reloading in debug mode.
        fromHistory,
        alias
      );
    }
  };

  // 🧨

  // 🅱️ Load the piece.
  // TODO: What happens if source is undefined?
  // const moduleLoadTime = performance.now();
  const module = await import(fullUrl).catch((err) => {
    console.error(`😡 "${path}" load failure:`, err);
    send({ type: "alert-popup:instagram", content: JSON.stringify(err) });
    loadFailure = err;
  });
  // console.log(performance.now() - moduleLoadTime, module);

  if (module === undefined) {
    loading = false;
    return;
  }

 send({ type: "alert-popup:instagram", content: fullUrl });

  const forceProd = false; // For testing production servers in development.

  socket = new Socket(debug);

  // // Requests a session-backend and connects via websockets.
  async function startSocket() {
    const sesh = await session(slug, forceProd); // Grab a session backend for this piece.
    socket.connect(
      new URL(sesh.url).host,
      (id, type, content) => receiver?.(id, type, content),
      $commonApi.reload,
      debug === true && !forceProd ? "ws" : "wss"
    );
  }

  startSocket();

  $commonApi.net.socket = function (receive) {
    //console.log("📡 Mapping receiver.");
    receiver = receive;
    return socket;
  };

  // This would also get the source code, in case meta-programming is needed.
  // const source = await (await fetch(fullUrl)).text();

  // ***Client Metadata Fields***
  // Set default metadata fields for SEO and sharing,
  // (requires serverside prerendering, also via `index.js`).
  let meta;

  if (alias === false) {
    // Parse any special piece metadata.
    const { title, desc, ogImage, twitterImage } = metadata(
      "aesthetic.computer",
      slug,
      module.meta?.(parsed)
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

  // *** Resize ***
  // Accepts width, height and gap either as numbers or as
  // an object with those keys.
  //
  // Usage: resize(64);
  //        resize(320, 240);
  //        resize(display); // "display" is a global object whose width
  //                             and height matches the hardware display
  //                             hosting aesthetic.computer.
  $commonApi.resize = function (width, height = width, gap = 8) {
    if (typeof width === "object") {
      const props = width;
      height = props.height;
      width = props.width || props.height;
      gap = props.gap === 0 ? 0 : props.gap || 8;
    }

    // Don't do anything if there is no change and no gap update.
    if (screen.width === width && screen.height === height && gap === undefined)
      return;

    // width = width || currentDisplay.innerWidth;
    // height = height || currentDisplay.innerHeight;

    // TODO: Paint anything that needs to be painted before resizing...
    // TODO: Does this even work right now?
    //debugger;
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

    screen.pixels = new Uint8ClampedArray(screen.width * screen.height * 4);
    screen.pixels.fill(255);

    graph.setBuffer(screen);
    graph.paste({
      painting: oldScreen,
      crop: new geo.Box(0, 0, oldScreen.width, oldScreen.height),
    });
  };

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

  // TODO: Add the rest of the $api to "leave" ... refactor API. 22.08.22.07.34
  if (firstLoad === false) {
    try {
      leave({ store, screen, system: $commonApi.system }); // Trigger leave.
    } catch (e) {
      console.warn("👋 Leave failure...", e);
    }
  }

  // Artificially imposed loading by at least 1/4 sec.
  // Redefine the default event functions if they exist in the module.
  if (module.system === "nopaint") {
    // If there is no painting is in ram, then grab it from the local store,
    // or generate one.

    boot = module.boot || nopaint.boot;
    sim = module.sim || defaults.sim;
    paint = module.paint || defaults.paint;
    beat = module.beat || defaults.beat;
    act = module.act || nopaint.act;
    leave = module.leave || nopaint.leave;
  } else {
    boot = module.boot || defaults.boot;
    sim = module.sim || defaults.sim;
    paint = module.paint || defaults.paint;
    beat = module.beat || defaults.beat;
    act = module.act || defaults.act;
    leave = module.leave || defaults.leave;



    delete $commonApi.system.name; // No system in use.
  }

  paintCount = 0n;
  simCount = 0n;
  booted = false;
  initialSim = true;
  activeVideo = null; // reset activeVideo
  bitmapPromises = {};
  noPaint = false;
  formsSent = {}; // Clear 3D list for GPU.
  currentPath = path;
  currentHost = host;
  currentSearch = search;
  currentParams = params;
  currentHash = hash;
  currentText = slug;

  $commonApi.slug = slug;
  $commonApi.query = search;
  $commonApi.params = params || [];
  $commonApi.load = load;

  // A wrapper for `load(parse(...))`
  // Make it `ahistorical` to prevent a url change.
  // Make it an `alias` to prevent a metadata change for writing landing or
  // router pieces such as `freaky-flowers` -> `wand`. 22.11.23.16.29
  $commonApi.jump = function jump(to, ahistorical = false, alias = false) {
    load(parse(to), ahistorical, alias);
  };

  $commonApi.pieceCount += 1;
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
  $commonApi.download = (filename, data, modifiers) =>
    send({ type: "download", content: { filename, data, modifiers } });

  // * Preload *
  // Add preload to the boot api.
  // Accepts paths local to the original disk server, full urls, and demos.
  // Usage:   preload("demo:drawings/2021.12.12.17.28.16.json") // pre-included
  //          preload("https://myserver.com/test.json") // remote
  //          preload("drawings/default.json") // hosted with disk
  // Results: preload().then((r) => ...).catch((e) => ...) // via promise

  // TODO: Add support for files other than .json and .png / .jpeg 2022.04.06.21.42

  // TODO: How to know when every preload finishes? 2021.12.16.18.55

  // TODO: Preload multiple files and load them into an assets folder with
  //       a complete handler. 2021.12.12.22.24
  $commonApi.net.preload = function (path, parseJSON = true) {
    // console.log("Preload path:", path);

    const extension = path.split(".").pop();

    //if (extension === "json") {
    // This is a hack for now. The only thing that should be encoded is the file slug.
    if (!path.startsWith("https://")) {
      path = encodeURIComponent(path);
    }
    //}

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

    // If we are loading a .json file then we can parse or not parse it here.
    if (extension === "json") {
      return new Promise((resolve, reject) => {
        fetch(path)
          .then(async (response) => {
            if (!response.ok) {
              reject(response.status);
            } else return parseJSON ? response.json() : response.text();
          })
          .then((response) => resolve(response))
          .catch(reject);
      });
    } else if (
      extension === "webp" ||
      extension === "jpg" ||
      extension === "png"
    ) {
      // Other-wise we should drop into the other thread and wait...
      return new Promise((resolve, reject) => {
        send({ type: "load-bitmap", content: path });
        bitmapPromises[path] = { resolve, reject };
      });
    }
  };

  cursorCode = "precise";

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
      fromHistory,
      alias,
      meta,
      // noBeat: beat === defaults.beat,
    },
  });

  if (firstLoad === true) {
    firstLoad = false;
    firstPiece = path;
    firstParams = params;
    firstSearch = search;
  }

 send({ type: "alert-popup:instagram", content: boot });

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

// 3. ✔ Add any APIs that require send.
//      Just the `content` API for now.
//      TODO: Move others from makeFrame into here.
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
const signals = [];
let reframed = false;
async function makeFrame({ data: { type, content } }) {
  if (type === "init-from-bios") {
    debug = content.debug;
    graph.setDebug(content.debug);
    ROOT_PIECE = content.rootPiece;
    originalHost = content.parsed.host;
    load(content.parsed);
    return;
  }

  if (type === "loading-complete") {
    loading = false;
    return;
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

  if (type === "transcode-progress") {
    if (debug) console.log("📼 Recorder: Transcoding", content);
    $commonApi.rec.printProgress = content;
    if (content === 1)
      send({ type: "signal", content: "recorder:transcoding-done" });
    // TODO: Is this the best place for this signal to be sent?
    //       Maybe it should go back in the BIOS? 22.08.19.13.44
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

  // 1. Beat // One send (returns afterwards)
  if (type === "beat") {
    const $api = {};
    Object.assign($api, $commonApi);
    $api.graph = painting.api; // TODO: Should this eventually be removed?

    $api.sound = {
      time: content.time,
      // Get the bpm with bpm() or set the bpm with bpm(newBPM).
      bpm: function (newBPM) {
        if (newBPM) content.bpm = newBPM;
        return content.bpm;
      },
    };

    $api.sound.microphone = microphone;
    // Attach the microphone.
    /*
    $api.sound.microphone = function (options) {
      send({ type: "microphone", content: options });
      return {
        amplitude: (cb) => {
          send({ type: "get-microphone-amplitude" });
        },
      };
    };
    */

    // TODO: Generalize square and bubble calls.
    // TODO: Move this stuff to a "sound" module.
    const squares = [];
    const bubbles = [];

    $api.sound.square = function ({
      tone = 440, // TODO: Make random.
      beats = Math.random(), // Wow, default func. params can be random!
      attack = 0,
      decay = 0,
      volume = 1,
      pan = 0,
    } = {}) {
      squares.push({ tone, beats, attack, decay, volume, pan });

      // Return a progress function so it can be used by rendering.
      const seconds = (60 / content.bpm) * beats;
      const end = content.time + seconds;
      return {
        progress: function (time) {
          return 1 - Math.max(0, end - time) / seconds;
        },
      };
    };

    $api.sound.bubble = function ({ radius, rise, volume = 1, pan = 0 } = {}) {
      bubbles.push({ radius: radius, rise, volume, pan });

      // Return a progress function so it can be used by rendering.
      /*
      const seconds = (60 / content.bpm) * beats;
      const end = content.time + seconds;
      return {
        progress: function (time) {
          return 1 - Math.max(0, end - time) / seconds;
        },
      };
      */
    };

    try {
      beat($api);
    } catch (e) {
      console.warn(" 💗 Beat failure...", e);
    }

    send(
      { type: "beat", content: { bpm: content.bpm, squares, bubbles } } //,
      //[content.bpm]
    );

    squares.length = 0;
    bubbles.length = 0;

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
      console.error("File failed to load:", content.data);
      serverUpload?.reject(content.data);
    }
    serverUpload = undefined;
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
    // console.log("Bitmap load success:", content);
    bitmapPromises[content.url].resolve(content.img);
    delete bitmapPromises[content];
    return;
  }

  if (type === "loaded-bitmap-rejection") {
    console.error("Bitmap load failure:", content);
    bitmapPromises[content.url].reject(content.url);
    delete bitmapPromises[content.url];
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

  // 2. Frame
  // This is where each...
  if (type === "frame") {
    // Take hold of a previously transferred screen buffer.
    let pixels;
    if (content.pixels) {
      //console.log(screen, content.pixels)
      pixels = new Uint8ClampedArray(content.pixels);
      if (screen) screen.pixels = pixels;
    }

    // Globalize any background music data, retrievable via bgm.data
    $commonApi.bgm.data = {
      amplitude: content.audioMusicAmplitude,
      sample: content.audioMusicSampleData,
    };

    // Act & Sim (Occurs after first boot and paint.)
    if (booted && paintCount > 0n) {
      const $api = {};
      Object.keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      Object.keys($updateApi).forEach((key) => ($api[key] = $updateApi[key]));
      Object.keys(painting.api).forEach(
        (key) => ($api[key] = painting.api[key])
      );

      //Object.assign($api, $commonApi);
      //Object.assign($api, $updateApi);
      //Object.assign($api, painting.api);

      $api.inFocus = content.inFocus;

      $api.sound = { time: content.audioTime, bpm: content.audioBpm };

      // Don't pass pixels to updates.
      $api.screen = {
        width: content.width,
        height: content.height,
      };

      $api.cursor = (code) => (cursorCode = code);

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

        if (content.pen.pointers.length > 0) {
          $commonApi.pens = function (n) {
            if (n === undefined) {
              return Object.values(pointers).reduce((arr, value) => {
                arr[value.pointerIndex] = value;
                return arr;
              }, []);
            }
            return help.findKeyAndValue(pointers, "pointerIndex", n - 1) || {};
          };
        }

        $commonApi.pen = primaryPointer; // || { x: undefined, y: undefined };
      }

      // 🤖 Sim // no send
      $api.seconds = function (s) {
        return s * 120; // TODO: Get 120 dynamically from the Loop setting. 2022.01.13.23.28
      };

      // 🕶️ VR Pen
      $commonApi.pen3d = content.pen3d?.pen;

      // TODO: A booted check could be higher up the chain here?
      // Or this could just move. 22.10.11.01.31
      if (loading === false && booted) {
        if (initialSim) {
          simCount += 1n;
          $api.simCount = simCount;
          try {
            sim($api);
          } catch (e) {
            console.warn("🧮 Sim failure...", e);
          }
          initialSim = false;
        } else if (content.updateCount > 0 && paintCount > 0n) {
          // Update the number of times that are needed.
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
      }

      // 📻 Signalling
      $api.signal = (content) => {
        send({ type: "signal", content });
      };

      // Add upload event to allow the main thread to open a file chooser.
      // type: Accepts N mimetypes or file extensions as comma separated string.
      // Usage: upload(".jpg").then((data) => ( ... )).catch((err) => ( ... ));
      $api.upload = (type) => {
        send({ type: "import", content: type });
        return new Promise((resolve, reject) => {
          fileImport = { resolve, reject };
        });
      };

      // ***Actually*** upload a file to the server.
      $api.serverUpload = (filename, data, bucket) => {
        send({ type: "upload", content: { filename, data, bucket } });
        return new Promise((resolve, reject) => {
          serverUpload = { resolve, reject };
        });
      };

      // 🌟 Act
      // *Device Event Handling*

      // TODO: Shouldn't all these events come in as part of one array to
      //       keep their order of execution across devices?
      // TODO: Could "device" be removed in favor of "device:event" strings and
      //       if needed, a device method?

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

      // Window Events
      if (content.inFocus !== inFocus) {
        inFocus = content.inFocus;
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

        // 🌟 Global Keyboard Shortcuts
        if (data.name.indexOf("keyboard:down") === 0) {
          // [Escape]
          // If not on prompt, then move backwards through the history of
          // previously loaded pieces in a session.
          if (
            data.key === "Escape" &&
            currentPath !== "aesthetic.computer/disks/prompt"
          ) {
            if (pieceHistoryIndex > 0) {
              send({ type: "back-to-piece" });
            } else {
              // Load the prompt automatically.
              // $api.load("prompt"); Disabled on 2022.05.07.03.45
            }
          }

          if (
            data.key === "`" &&
            currentPath !== "aesthetic.computer/disks/prompt"
          ) {
            // Load prompt if the backtic is pressed.
            $api.load(parse("prompt"));
            /*
            $api.load({
              host: location.hostname,
              path: "aesthetic.computer/disks/prompt",
              params: [],
              search: "",
              hash: "",
              text: "/",
            });
            */
          }

          // [Ctrl + X]
          // Enter and exit fullscreen mode.
          if (data.key === "x" && data.ctrl) {
            send({ type: "fullscreen-toggle" });
          }
        }
      });
    }

    // 🖼 Render // Two sends (Move one send up eventually? -- 2021.11.27.17.20)
    if (content.needsRender) {
      const $api = {};
      Object.keys($commonApi).forEach((key) => ($api[key] = $commonApi[key]));
      Object.keys(painting.api).forEach(
        (key) => ($api[key] = painting.api[key])
      );
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

        // TODO: Add the depth buffer back here.
        // Reset the depth buffer.
        // TODO: I feel like this is causing a memory leak...
        graph.depthBuffer.length = screen.width * screen.height;
        graph.depthBuffer.fill(Number.MAX_VALUE);
      }

      // TODO: Disable the depth buffer for now... it doesn't need to be
      //       regenerated on every frame.
      graph.depthBuffer.fill(Number.MAX_VALUE); // Clear depthbuffer.

      $api.screen = screen;
      $api.screen.center = [screen.width / 2, screen.height / 2];

      $api.fps = function (newFps) {
        send({ type: "fps-change", content: newFps });
      };

      $api.cursor = (code) => (cursorCode = code);

      /**
       * @function video
       * @descrption Make a live video feed. Returns an object that links to current frame.
       * @param {string} type
       * @param {object} options - *unimplemented* { src, width, height }
       */
      $api.video = function (type, options) {
        // Options could eventually be { src, width, height }
        send({ type: "video", content: { type, options } });

        // Return an object that can grab whatever the most recent frame of
        // video was.
        return function videoFrame() {
          return activeVideo;
        };
      };

      graph.setBuffer(screen);

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

      // Run boot only once before painting for the first time.
      // TODO: Why is boot running twice? 22.07.17.17.26

      // TODO: Boot's painting is currently bound by whatever dirtyBox gets
      //       set to at the end of `paint`.

      // How can this be solved intelligently?
      // Right now, in `line` there is a paintCount check to work around this.
      // 22.09.19.20.45

      if (paintCount === 0n && loading === false) {
        inFocus = content.inFocus; // Inherit our starting focus from host window.
        // Read current dark mode.

        const dark = await store.retrieve("dark-mode");
        if (dark === true || dark === false) $commonApi.dark = dark;

        // System specific preloaders.
        //if ($commonApi?.system?.name === "nopaint" || currentText === "prompt") {
        store["painting"] =
          store["painting"] ||
          (await store.retrieve("painting", "local:db")) ||
          painting.api.painting(screen.width, screen.height, ($) => {
            $.wipe(64);
          });

        $commonApi.system.painting = store["painting"];

        try {
          boot($api);
        } catch (e) {
          console.warn("🥾 Boot failure...", e);
        }
        booted = true;
        send({ type: "disk-loaded-and-booted" });
      }

      //console.log(paintCount, "booted:", booted, "loading:", loading);

      // Paint a frame, which can return false to enable caching via noPaint and by
      // default returns undefined (assume a repaint).
      // Once paint returns false and noPaint is marked true, `needsPaint` must be called.
      // Note: Always marked false on a disk's first frame.
      let painted = false;
      let dirtyBox;

      // Attempt a paint.
      if (noPaint === false && booted && loading === false) {
        let paintOut;

        try {
          paintOut = paint($api); // Returns `undefined`, `false`, or `DirtyBox`.
        } catch (e) {
          console.warn("🎨 Paint failure...", e);
        }

        // `DirtyBox` and `undefined` always set `noPaint` to `true`.
        noPaint =
          paintOut === false || (paintOut !== undefined && paintOut !== true);

        // Run everything that was queued to be painted, then devour paintLayers.
        //await painting.paint();
        painting.paint(true);
        painted = true;
        paintCount = paintCount + 1n;

        // console.log("!! Painted", paintCount, performance.now());

        if (paintOut) dirtyBox = paintOut;

        //console.log("bake")
        //send({ type: "3d-bake" });
      }

      // Return frame data back to the main thread.
      let sendData = {};
      let transferredPixels;

      // Check to see if we have a dirtyBox to render from.
      const croppedBox = dirtyBox?.croppedBox?.(screen);

      if (croppedBox?.w > 0 && croppedBox?.h > 0) {
        transferredPixels = dirtyBox.crop(screen);
        sendData = {
          pixels: transferredPixels,
          dirtyBox: croppedBox,
        };
      } else if (painted === true) {
        // TODO: Toggling this causes a flicker in `line`... but helps prompt. 2022.01.29.13.21
        // Otherwise render everything if we drew anything!
        transferredPixels = screen.pixels;
        sendData = { pixels: transferredPixels };
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

      if (sendData.pixels?.byteLength === 0) sendData.pixels = undefined;

      send({ type: "render", content: sendData }, [sendData.pixels]);

      // Flush the `signals` after sending.
      if (reframe) reframe = undefined;
      if (cursorCode) cursorCode = undefined;
    } else {
      // Send update (sim).
      // TODO: How necessary is this - does any info ever need to actually
      //       get sent?
      send(
        {
          type: "update",
          content: { didntRender: true, loading, pixels: pixels?.buffer },
        },
        [pixels?.buffer]
      );
    }

    // ***Frame State Reset***
    // Reset video transcoding / print progress.
    if ($commonApi.rec.printProgress === 1) $commonApi.rec.printProgress = 0;

    //console.log(performance.now() - frameTime, "ms");

    //performance.mark("b");

    //performance.measure("a", "b");
    //console.log("Frame perf:", performance.getEntriesByType("measure")[0].duration);
    //performance.clearMarks();
    //performance.clearMeasures();
  }
}

// 📚 Utilities

// Default template string behavior: https://stackoverflow.com/a/64298689/8146077
function defaultTemplateStringProcessor(strings, ...vars) {
  let result = "";
  strings.forEach((str, i) => {
    result += `${str}${i === strings.length - 1 ? "" : vars[i]}`;
  });
  return result;
}
