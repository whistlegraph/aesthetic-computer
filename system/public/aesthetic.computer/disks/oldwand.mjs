// Wand, 22.10.29.01.50 https://wand.ac
//       22.10.05.11.01

// TODO
// - [] Add Camdoll
//      import { CamDoll } from "../lib/cam-doll.mjs";

import { font1 } from "./common/fonts.mjs"; // This import will probably break now. 23.02.18.13.00 
import { MetaBrowser, iOS } from "../lib/platform.mjs";

const { entries, keys } = Object;
const glyphs = {};

let cam, dolly; // Camera system.
let wand; // Player / line geometry.
let floor, cross, tri; // Room Geometry.
let lookCursor = false;
let client; // Network.
let colorParams = [255, 255, 255, 255];
let W, S, A, D, UP, DOWN, LEFT, RIGHT; // Controls
let wandDepth2D = 0.3; //iOS ? 0.1 : 0.3; // The distance from the near plane for drawing on screens.
let showWandCount = true;
let showWandCountDuration = 300;
let showWandCountProgress = 0;
const wandLength = 4096; // The maximum number of points per each wand.
const remoteWands = {}; // A container for wands that come off the network.

let lastDevice;
let sentWand = false;

const beeps = [];
let beatCount = 0n;

// 🥾 Boot
async function boot({
  painting: p,
  Camera,
  Dolly,
  Form,
  QUAD,
  TRI,
  color,
  net: { preload },
  geo: { Race, Quantizer },
  num: { dist3d, map, randIntRange },
  help: { choose },
  params,
  store,
  net,
  cursor,
  wipe,
}) {
  colorParams = params.map((str) => parseInt(str)); // Set params for color.
  if (colorParams.filter(Boolean).length === 0) {
    colorParams = undefined;
  } // Set up empty params for randomization.

  entries(font1).forEach(([glyph, location]) => {
    preload(`aesthetic.computer/disks/drawings/font-1/${location}.json`).then(
      (res) => {
        glyphs[glyph] = res;
      }
    );
  });

  // Clear the screen to black and remove the cursor.
  cursor("none");
  wipe(0);

  // Connect to the network.
  client = net.socket(async (id, type, content) => {
    // Make a new wand for another user.
    if (type === "create") {
      if (client.id === id) return;
      // TODO: This is hacky / bad design... so many create messages are being sent... (need server state?)
      if (remoteWands[id] !== undefined) return; // Don't instantiate anyone's wand more than once.

      remoteWands[id] = await new Wand(
        {
          Form,
          color,
          Quantizer,
          Race,
          store,
          dist3d,
          client,
          map,
          randIntRange,
          choose,
        },
        "line",
        wandLength
      ).init({
        preload: false,
        color: content.color,
        segments: content.segments,
        segmentMarkers: content.segmentMarkers,
        totalLength: content.totalLength,
      });

      if (content.points?.length > 0) {
        remoteWands[id].drawing.addPoints({
          positions: content.points,
          colors: content.colors,
        });
      }

      // Instantiate our wand for the client who sent us their wand instantiation request.
      client.send("create", {
        color: wand.color,
        segments: wand.segments, // I really don't have to be passing in both these values here... 22.11.01.22.17
        segmentMarkers: wand.segmentMarkers,
        totalLength: wand.totalLength,
        points: wand.drawing.vertices.map((v) => [...v.pos]),
        colors: wand.drawing.vertices.map((v) => [...v.color]),
      });

      showWandCount = true;
      showWandCountProgress = 0;
      beeps[0] = { event: "join" };
    }

    if (type === "start" && client.id !== id) {
      remoteWands[id]?.start(content.target, content.vr, true);
    }

    if (type === "goto" && client.id !== id) {
      remoteWands[id]?.goto(content.target, true);
    }

    if (type === "move") {
      if (client.id !== id) {
        const rw = remoteWands[id];
        if (rw === undefined) return;
        const form = rw.form;
        rw.position = content.position;
        rw.rotation = content.rotation;
      }
    }

    if (type === "stop" && client.id !== id) remoteWands[id]?.stop(true);

    // Directly points to wand... (like when loading from start)
    if (type === "add" && client.id !== id) {
      remoteWands[id]?.drawing.addPoints(content); // Add points locally.
    }

    // Clear wand.
    if (type === "clear" && client.id !== id) {
      remoteWands[id]?.clear(
        content.color,
        content.segments,
        content.segmentMarkers,
        content.totalLength,
        true
      );
    }

    if (type === "left" && client.id !== id && remoteWands[content.id]) {
      delete remoteWands[content.id];
      showWandCount = true;
      showWandCountProgress = 0;
      beeps[0] = { event: "left" };
    }
  });

  cam = new Camera(80, { z: 0, y: 1.8, scale: [1, 1, 1] }); // camera with fov
  dolly = new Dolly(cam); // moves the camera

  floor = new Form(
    QUAD,
    { tex: p(2, 2, (g) => g.wipe(0, 0, 100)) },
    { pos: [0, 0, 0], rot: [-90, 0, 0], scale: [8, 8, 8] }
  );

  const cs = 128;
  cross = new Form(
    QUAD,
    // { tex: p(4, 4, (g) => g.noise16DIGITPAIN()) },
    {
      tex: p(cs, cs, (g) =>
        g.ink(0, 0, 150).line(0, 0, cs, cs).line(cs, 0, 0, cs)
      ),
    },
    { pos: [0, 0, 0], rot: [-90, 0, 0], scale: [0.5, 0.5, 0.5] }
  );

  tri = new Form(
    TRI,
    { tex: p(1, 8, (g) => g.noise16DIGITPAIN()), alpha: 0.75 },
    { pos: [0, 1, 0], scale: [0.5, 0.5, 0.5] }
  );

  // 1. Create a wand for the user.
  // Wand type, how many samples allowed, and a geometry.
  wand = await new Wand(
    {
      Form,
      color,
      Quantizer,
      Race,
      store,
      dist3d,
      client,
      map,
      randIntRange,
      choose,
    },
    "line",
    wandLength
  ).init({
    preload: false,
    color: colorParams,
  });

  /*
  client.send("create", {
    color: wand.color,
    segments: wand.segments, // I really don't have to be passing in both these values here... 22.11.01.22.17
    segmentMarkers: wand.segmentMarkers,
    totalLength: wand.totalLength,
  }); // Tell anyone whose around to make a wand for us.
  */
}

// 🎨 Paint (Executes every display frame)
function paint({
  ink,
  pen,
  gpuReady,
  pen3d,
  wipe,
  help,
  screen,
  Form,
  form,
  num: { randIntRange },
}) {
  if (wand && client.id !== undefined && sentWand === false) {
    client.send("create", {
      color: wand.color,
      segments: wand.segments, // I really don't have to be passing in both these values here... 22.11.01.22.17
      segmentMarkers: wand.segmentMarkers,
      totalLength: wand.totalLength,
    }); // Tell anyone whose around to make a wand for us.
    sentWand = true;
  }

  // Only draw a wand / a wand's drawing if it exists.

  // The lines & the furnitue and our wand.
  wand?.generateForm();

  if ((pen?.device !== "touch" && lastDevice !== "touch") || MetaBrowser) {
    form([floor, wand?.form, wand?.drawing], cam);
  } else {
    form([floor, wand?.drawing], cam);
  }

  if (pen && pen.device) lastDevice = pen.device;

  // Everyone else's wands...
  help.each(remoteWands, (w) => {
    w.generateForm();
    form([w.form, w.drawing], cam);
  });

  // Draw cursors for 2D screens.
  if (gpuReady === false) {
    wipe(0);
  } else {
    wipe(10, 0);
  }

  if (lookCursor) ink(127).circle(pen.x, pen.y, 8);
  else if (pen && wand && pen?.device !== "touch") {
    const shadow = 30 * (1 - wand.progress);
    ink(127, 127).line(pen.x, pen.y, pen.x + shadow, pen.y + shadow);
  }

  // Wait until all text is loaded before printing.
  if (Object.keys(glyphs).length === Object.keys(font1).length) {
    if (wand?.started === false) {
      // Print title.
      const title = "Wand";
      const scale = 2;
      const spacing = 6;
      const width = title.length * (scale * spacing);
      const height = 10 * scale;
      const hheight = height / 2;
      const hwidth = width / 2;

      ink(...wand.currentSegment.color.slice(0, 3), 180).printLine(
        title,
        glyphs,
        screen.width / 2 - hwidth,
        screen.height / 2 - hheight,
        spacing,
        scale,
        0
      );
    }

    if (showWandCount && wand) {
      // Print wand count.
      const wandCount = keys(remoteWands).length + 1;
      const hd = showWandCountDuration / 2;
      const color = [...wand?.currentSegment.color.slice(0, 3), 180];
      if (showWandCountProgress > hd) {
        // Fade after half the duration passed.
        color[3] = 180 * (1 - (showWandCountProgress - hd) / hd);
      }
      ink(color).printLine(wandCount, glyphs, 4, 4, 6, 2, 0);
    }
  }

  // Show wandLine
  if (wand) {
    const y = screen.height - 1;
    let length = 0;
    wand.segments.forEach((seg) => {
      const start = length;
      const end = length + (seg.length / wand.totalLength) * screen.width;
      if (end - start > 1) {
        ink(...seg.color.slice(0, 3), 200).line(start, y, end - 1, y);
      }
      length = end;
    });
  }

  // Draw current wand's tail.
  if (wand?.tail) {
    //ink(0, 255, 0).form(
    ink(wand.currentColor).form(
      new Form(
        { type: "line", positions: wand.tail, keep: false },
        { alpha: 1 }
      ),
      cam
    );
  }

  if (wand?.tail2) {
    ink().form(
      //ink(wand.currentColor).form(
      new Form(
        { type: "line", positions: wand.tail2, keep: false },
        { alpha: 1 }
      ),
      cam
    );
  }

  // Draw the tails for any remote wands.

  help.each(remoteWands, (w) => {
    if (w.tail) {
      ink(w.currentColor).form(
        new Form(
          { type: "line", positions: w.tail, keep: false },
          { alpha: 1 }
        ),
        cam
      );
    }

    if (w.tail2) {
      ink(w.currentColor).form(
        new Form(
          { type: "line", positions: w.tail2, keep: false },
          { alpha: 1 }
        ),
        cam
      );
    }
  });
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim({ pen, pen3d, screen: { width, height }, num: { degrees: deg } }) {
  // 🔫 FPS style camera movement.
  let forward = 0,
    strafe = 0;
  if (W) forward = -0.002;
  if (S) forward = 0.002;
  if (A) strafe = -0.002;
  if (D) strafe = 0.002;
  if (W || S || A || D) dolly.push({ x: strafe, z: forward });
  if (UP) cam.rotX += 1;
  if (DOWN) cam.rotX -= 1;
  if (LEFT) cam.rotY += 1;
  if (RIGHT) cam.rotY -= 1;
  dolly.sim();

  // 🌴 Scenery adjustments.
  tri.turn({ y: -0.25 });

  // 🪄 Wand
  if (wand) {
    // Position wand.
    if (pen3d) {
      wand.form.turn({ x: 0.0, y: 0.0, z: 0.0 }); // Why does this make everything work?
      wand.position = [pen3d.pos.x, pen3d.pos.y, pen3d.pos.z, 0];
      wand.rotation = [deg(pen3d.rot._x), deg(pen3d.rot._y), deg(pen3d.rot._z)];
      wand.form.gpuTransformed = true; // Is this still needed?

      client.send("move", {
        position: wand.position,
        rotation: wand.rotation,
      });
    } else if (pen) {
      //wand.form.turn({ x: 0.0, y: 0.0, z: 0.0 }); // Why does this make everything work?
      wand.position = cam.ray(pen.x, pen.y, wandDepth2D);
      wand.rotation = cam.rotation;

      client.send("move", {
        position: [...wand.position],
        rotation: wand.rotation,
      });
    }

    // Calculate current gesture.
    if (wand.waving) {
      if (wand.vr) wand.goto([pen3d.pos.x, pen3d.pos.y, pen3d.pos.z, 0]);
      else wand.goto(cam.ray(pen.x, pen.y, wandDepth2D));
    }

    // Timing for showWandCount duration.
    if (showWandCount) {
      showWandCountProgress += 1;
      if (showWandCountProgress === showWandCountDuration) {
        showWandCount = false;
        showWandCountProgress = 0;
      }
    }
  }
}

// ✒ Act
function act({ event: e, color, gpu, screen, download, num: { timestamp } }) {

  // 👋 Right Hand
  if (e.is("3d:touch:2")) wand.start([e.pos.x, e.pos.y, e.pos.z, 0]); // ✏️
  if (e.is("3d:lift:2")) wand.stop(); // 🚩

  // 🖱️ Mouse
  if (e.device === "mouse" && e.button === 0) {
    if (e.is("touch")) {
      const point = cam.ray(e.x, e.y, wandDepth2D);
      wand.start(point, false); // ✏️ Start a mark.
      gpu.message({uid: wand.drawing.uid, to: point });
    } 
    if (e.is("lift")) { wand.stop(); } // 🚩 End a mark.
  }

  // 👀 Look around if 2nd mouse button is held.
  if (e.is("draw") && e.button === 2) {
    cam.rotX -= e.delta.y / 3.5;
    cam.rotY -= e.delta.x / 3.5;
  }

  // Enable look mode / cursor.
  if (e.is("touch") && e.button === 2) lookCursor = true;
  if (e.is("lift") && e.button === 2) lookCursor = false;

  // 🖖 Touch
  // Two fingers for move forward.
  //if (e.is("touch:2")) W = true;
  //if (e.is("lift:2")) W = false;

  // Three fingers for moving backward.
  //if (e.is("touch:3")) S = true;
  //if (e.is("lift:3")) S = false;

  // One finger to draw.
  if (!MetaBrowser) {
    console.log();
    if (e.device === "touch") {
      if (e.is("touch:1")) wand.start(cam.ray(e.x, e.y, wandDepth2D), false); // ✏️ Start a mark.
      if (e.is("lift:1")) wand.stop(); // 🚩 End a mark.
    }
  }

  // 👀 Look around if 2nd mouse button is held.
  // if (e.is("draw") && e.device === "touch") {
  //  cam.rotX -= e.delta.y / 3.5;
  //  cam.rotY -= e.delta.x / 3.5;
  // }

  // ✖️ 🪄 Clear the wand if
  // Shift button is pressed on keyboard.
  // Left hand trigger is clicked in VR.
  // Fourth finger is down on a phone.
  if (e.is("keyboard:down:shift") || e.is("3d:touch:1") || e.is("touch:4")) {
    wand?.clear();
  }

  // 💻️ Keyboard: WASD for movement, arrows for looking.
  if (e.is("keyboard:down:w")) W = true;
  if (e.is("keyboard:down:s")) S = true;
  if (e.is("keyboard:down:a")) A = true;
  if (e.is("keyboard:down:d")) D = true;

  if (e.is("keyboard:up:w")) W = false;
  if (e.is("keyboard:up:s")) S = false;
  if (e.is("keyboard:up:a")) A = false;
  if (e.is("keyboard:up:d")) D = false;

  if (e.is("keyboard:down:arrowup")) UP = true;
  if (e.is("keyboard:down:arrowdown")) DOWN = true;
  if (e.is("keyboard:down:arrowleft")) LEFT = true;
  if (e.is("keyboard:down:arrowright")) RIGHT = true;

  if (e.is("keyboard:up:arrowup")) UP = false;
  if (e.is("keyboard:up:arrowdown")) DOWN = false;
  if (e.is("keyboard:up:arrowleft")) LEFT = false;
  if (e.is("keyboard:up:arrowright")) RIGHT = false;

  if (e.is("keyboard:down:k")) cursorSize += 0.1;
  if (e.is("keyboard:down:j")) cursorSize -= 0.1;

  // 💾 Saving
  if (e.is("keyboard:down:enter") && !e.repeat) {
    // 🅰️ Pack up and download the vertices in an array of arrays.
    /*
    download(`sculpture-${timestamp()}.json`, JSON.stringify(
      drawing.vertices.map(v => [
        [v.pos["0"], v.pos["1"], v.pos["2"]],
        [...v.color]
      ]))
    );
    */

    // 🅱️ Pack up and download the vertices according to an unreal engine
    //    friendly JSON import format for Data Tables..
    download(
      `sculpture-${timestamp()}.json`,
      JSON.stringify(
        wand.drawing.vertices.map((v, i) => {
          return {
            Name: "NewRow_" + i,
            Vertices: [{ X: v.pos["0"], Y: v.pos["1"], Z: v.pos["2"] }],
            Color: [
              {
                R: v.color["0"],
                G: v.color["1"],
                B: v.color["2"],
                A: v.color["3"],
              },
            ],
          };
        })
      )
    );
  }
}

// 💗 Beat
function beat({ sound: { bpm, synth } }) {
  if (beatCount === 0n) bpm(1800); // Set bpm to 1800 ~ 30fps }
  beatCount += 1n; // TODO: This should go into the main API. 22.11.01.17.43
  if (beatCount < 8n * 2n) {
    beeps.length = 0;
    return;
  } // Wait ~ 2 sec to prevent mass bleeps.
  // TODO: ^ This is a little janky and should be tied to an event.

  beeps.forEach((beep) => {
    synth({
      tone: beep.event === "join" ? 200 : 50,
      beats: 0.5,
      attack: 0.1,
      decay: 0.9,
      volume: 0.15,
    });
  });

  if (wand.beep) {
    synth({
      tone: 300,
      beats: 0.7,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
    wand.beep = false;
  }

  if (wand.bop) {
    synth({
      tone: 600,
      beats: 0.7,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
    wand.bop = false;
  }

  beeps.length = 0;
}

// 👋 Leave
function leave({ store }) {
  if (wand?.drawing) {
    store["3dline:drawing"] = wand.drawing;
    store.persist("3dline:drawing", "local:db");
  }
}

export { boot, sim, paint, act, beat, leave };

// 📚 Library

const wandPalettes = {
  rgb: [
    [255, 0, 0, 255],
    [0, 255, 0, 255],
    [0, 0, 255, 255],
  ],
  rgb2: [
    [255, 0, 0, 255],
    [0, 255, 0, 255],
    [0, 0, 255, 255],
  ],
  rgb3: [
    [255, 0, 0, 255],
    [0, 255, 0, 255],
    [0, 0, 255, 255],
  ],
  zebra: [
    [200, 200, 200, 255],
    [100, 100, 100, 255],
    [200, 200, 200, 255],
    [100, 100, 100, 255],
  ],
  cmy: [
    [0, 255, 255, 255],
    [255, 0, 255, 255],
    [255, 255, 0, 255],
  ],
  roygbiv: [
    [255, 0, 0, 255],
    [255, 127, 0, 255],
    [255, 255, 0, 255],
    [0, 200, 0, 255],
    [0, 0, 255, 255],
    [200, 0, 100, 255],
    [255, 0, 200, 255],
  ],
  roygbiv2: [
    [255, 0, 0, 255],
    [255, 127, 0, 255],
    [255, 255, 0, 255],
    [0, 200, 0, 255],
    [0, 0, 255, 255],
    [200, 0, 100, 255],
    [255, 0, 200, 255],
  ],
  red: [[255, 0, 0, 255]],
  blue: [[0, 0, 255, 255]],
  green: [[0, 255, 0, 255]],
  yellow: [[255, 255, 0, 255]],
};

class Wand {
  cleared = false;
  started = false;

  api;
  type;
  waving = false;
  form;
  color;
  randomColor = false;

  position = [0, 0, 0];
  rotation = [0, 0, 0];

  currentSegment;
  segments;
  segmentMarkers;
  currentLength;
  totalLength;

  lastPalette;

  // The below fields are mostly for the "line" type, but could also be shared
  // by many wand types, depending on the design.
  drawing; // Geometry.
  race; // Lazy line control.
  lastTarget; // Prevents sending the same points over and over in `goto`.
  tail; // Preview / prediction lines.
  tail2;

  beep = false; // Triggers a beep.
  bop = false; // Triggers a bop.

  constructor(api, type, length) {
    this.api = api;
    this.type = type;
  }

  // TODO: Pass in segments instead of color if necessary.
  async init({ preload, color, segments, segmentMarkers, totalLength }) {
    // TODO: Over-ride segments if a color has been defined?
    // Set the color or choose a random one.
    if (color === undefined) {
      this.randomColor = true;
      // Pick a color for this wand.
      let r = Math.random() > 0.5 ? 0 : 255;
      let g = Math.random() > 0.5 ? 0 : 255;
      let b = Math.random() > 0.5 ? 0 : 255;

      if (r === 0 && g === 0 && b === 0) {
        r = 255;
        g = 255;
        b = 255;
      }

      this.color = [r, g, b, 255];
    } else {
      this.color = color;
    }

    // Define the 3D renderable for this wand.
    // Generate or load segments, formatted like below.
    /*
    [
      { length: 0.025, color: [255, 0, 0, 255] },
      { length: 0.05, color: [0, 255, 0, 255] },
      { length: 0.025, color: [0, 0, 255, 255] }
    ];
    */
    if (segments) {
      this.segments = segments;
      this.segmentMarkers = segmentMarkers;
      this.totalLength = totalLength;
      this.generateForm();
    } else {
      // Generate segments...
      const rr = this.api.randIntRange;
      const ch = this.api.choose;

      const genSegs = [];

      const k = keys(wandPalettes);
      let palette = wandPalettes[k[this.api.randIntRange(0, k.length - 1)]];

      while (palette === this.lastPalette) {
        palette = wandPalettes[k[this.api.randIntRange(0, k.length - 1)]];
      }

      this.lastPalette = palette;

      for (let i = 0; i < palette.length; i += 1) {
        let len;
        if (palette.length === 1) len = 0.1;
        else if (palette === wandPalettes.roygbiv) len = 0.02;
        else len = ch(0.04, 0.02, 0.03, 0.05);
        genSegs.push({ length: len, color: palette[i], originalLength: len });
      }

      this.segments = genSegs;
      this.generateForm();
      this.totalLength = this.currentLength;

      let p = 0;
      this.segmentMarkers =
        segmentMarkers ||
        this.segments.map((s, i) => {
          return i === 0 ? s.length : (p += s.length);
        });
    }

    // Calculate the current segment whether generated or loaded.
    for (let i = 0; i < this.segments.length; i += 1) {
      if (this.segments[i].length > 0) {
        this.currentSegment = this.segments[i];
        break;
      }
    }

    if (this.type === "line") {
      let stored;
      if (preload)
        stored = this.api.store["3dline:drawing"] =
          this.api.store["3dline:drawing"] ||
          (await this.api.store.retrieve("3dline:drawing", "local:db"));

      this.drawing = new this.api.Form(
        {
          type: "line:buffered",
          vertices: stored?.vertices,
          indices: stored?.indices,
        } //,
        //{ color: this.color }
      );

      this.drawing.MAX_POINTS = this.totalLength * 2 * 32000; // Must be a multiple of two for "line".
    } else {
      // TODO: Implement other geometries.
      // drawing.MAX_POINTS = length;
    }

    this.lastTarget = undefined; // Reset the target tracker in `goto`.
    return this;
  }

  get currentColor() {
    return this.currentSegment?.color;
  }

  generateForm() {
    const positions = [];
    const colors = [];
    let length = 0;

    this.segments.forEach((segment) => {
      const nextLength = length + segment.length;
      positions.push([0, 0, length, 1], [0, 0, nextLength, 1]);
      length = nextLength;
      colors.push(segment.color, segment.color);
    });

    this.currentLength = length;

    // Segmented
    this.form = new this.api.Form(
      {
        type: "line",
        positions,
        colors,
        keep: false,
      },
      { color: this.color },
      { pos: this.position, rot: this.rotation, scale: [1, 1, 1] }
    );
  }

  get progress() {
    if (!this.drawing) return 0;
    return this.drawing.vertices.length / this.drawing.MAX_POINTS;
  }

  start(target, vr = true, remote) {
    this.started = true;
    this.vr = vr;
    // *** Markmaking Configuration ***
    const smoothing = true; // Use a lazy moving cursor, or normal quantized lines.
    const quantizedSmoothing = true; // Regulate all segments while still smoothing.
    //const step = 0.001;
    const step = 0.12;
    const speed = 5;
    //const speed = iOS ? 40 : 20;

    this.race =
      smoothing === true
        ? new this.api.Race({ step, speed, quantized: quantizedSmoothing })
        : new this.api.Quantizer({ step });
    this.race.start(target);
    this.waving = true;
    if (!remote) this.api.client.send("start", { target, vr });
  }

  goto(target, remote = false) {

    /*
    if (
      this.lastTarget !== undefined &&
      this.lastTarget[0] === target[0] &&
      this.lastTarget[1] === target[1] &&
      this.lastTarget[2] === target[2]
    ) {
      return;
    }
    */

    this.lastTarget = target;
    if (this.drawing === null) return;
    if (!this.waving) return;
    const path = this.race.to(target);
    if (!path) return;

    let maxedOut;
    let needsStop = false;

    if (path.out?.length > 0) {
      const colors = [];

      path.out.forEach(() => {
        const vertexColor = this.api.color(this.currentSegment.color);
        colors.push(vertexColor, vertexColor);
      });

      // Add points locally.
      maxedOut = this.drawing.addPoints({ positions: path.out, colors });

      if (!maxedOut) {
        const totalSegmentLength = this.totalLength;
        const segments = this.segments;
        const progressThisTick =
          (path.out.length / this.drawing.MAX_POINTS) * totalSegmentLength;

        for (let i = 0; i < segments.length; i += 1) {
          if (segments[i].length > 0) {
            segments[i].length -= progressThisTick;
            this.currentSegment = segments[i];
            if (segments[i].length <= 0) {
              segments[i].length = 0;
              if (segments[i + 1]) {
                this.currentSegment = segments[i + 1];
              } else { // Remove the else to enable segment stops.
                needsStop = true;
              }
            }
            break;
          }
        }
      }
    }

    // Previews from wand tip.
    if (this.api.dist3d(path.last, path.current))
      this.tail = [path.last, path.current];
    if (this.api.dist3d(path.current, target))
      this.tail2 = [path.current, target];

    if (!remote) this.api.client.send("goto", { target: [...target] });

    if (needsStop) {
      if (!remote) {
        this.beep = true;
      }
      this.stop(remote);
    }

    if (maxedOut)
      this.clear(
        !this.randomColor ? this.color : undefined,
        undefined,
        undefined,
        undefined,
        remote
      );
    return maxedOut;
  }

  stop(remote = false) {
    // Draw the second tail if it exists, then clear both.
    const start = this.tail?.[0] || this.tail2?.[0];
    const end = this.tail2?.[1];

    let maxedOut;

    // Interpolate / add end of tail.
    if (
      start &&
      end &&
      this.api.dist3d(start, end) < 0.05 &&
      this.currentSegment.length !== this.currentSegment.originalLength
    ) {

      const q = new this.api.Quantizer({ step: this.race.step });

      q.start(start);
      const path = q.to(end);

      if (path.out.length > 0) {
        const colors = [];
        path.out.forEach(() => {
          const vertexColor = this.api.color(this.currentSegment.color);
          colors.push(vertexColor, vertexColor);
        });

        maxedOut = this.drawing.addPoints({ positions: path.out, colors });
      }
    }

    this.race = null;
    this.waving = false;
    this.tail = this.tail2 = undefined;
    if (maxedOut)
      this.clear(
        !this.randomColor ? this.color : undefined,
        undefined,
        undefined,
        undefined,
        remote
      );
    if (!remote) this.api.client.send("stop");
    return maxedOut;
  }

  // Remove the drawing and the wand.
  clear(color, segments, segmentMarkers, totalLength, remote = false) {
    this.stop(remote);
    if (!remote) this.bop = true;
    this.init({ preload: false, color, segments, segmentMarkers, totalLength }); // Reload this wand.

    if (!remote) {
      this.api.client.send("clear", {
        color: this.color,
        segments: this.segments,
        segmentMarkers: this.segmentMarkers,
        totalLength: this.totalLength,
      }); // Send a clear event to the server.
    }
  }
}