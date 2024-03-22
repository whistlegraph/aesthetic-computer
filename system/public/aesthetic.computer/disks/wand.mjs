// ️🪄 Wand, 22.11.19.04.40
// ️🪄 Cadwand, 22.11.05.00.30 ⚙️

/* #region 📓 versions
// v2: (Wand) Now will be used as the official wand program.
// v1: (Cadwand) A laboratory & development piece for designing the geometry in `wand`.
#endregion */

/* #region 🏁 todo
+ Now
  - [🟡] Decide on a feature set for the next version / iteration.
*** General Interest ***
  - [] Map sound to color. (Add better sound)
  - [] Performance testing / allocation test for demo playback,
       especially in VR.
  - [] Add glaze / lighting / mastering.
  - [] Add background music if it isn't already playing.
  - [] Lazy wand follow / rotate camera to wand situation.
    - Touch Screen Only
    - [] Two finger pinch to zoom and slow rotation on a touch screen.
      - [] Progress bar doesn't always seem to hit 100%.
      - [] Let go to snap back. 
    - [] [WANDER] button that shows keyboard shortcut overlays /
        touch instructions. (Add chat here?)
  - [] Resize while reconstructing flower yields pixel buffer bugs in the console.
  - [] Red lines of measurement cube sometimes start as black or faded.
  - [] Create a "full playback" mode to run all the pieces.
  - [] Add arrows to top right to move between slides?
*** Asset Generation ***
  - [] Automate the rendering of turn-around animation GIFs.
     - [] Could this be done on a server?
*** Behavior ***
  - [] Decide on how to best end a demo playback / don't show wand when
       demo finishes on a purely playback url? (Keep player alive?) 
  - [] Make instant demo playback much, much faster.
  - [] Add demo scrubbing.
  - [] Prevent users from viewing or moving outside a certain range.
  - [] Record a "wand:gesture:start" event.
  + Later
  - [] Auto jump from piece to piece / show an option?
*** Networking ***
  - [] Integrate `oldwand` multiplayer architecture.
    - [] Read both pieces side by side.
    - [] Model each wand as a single skinny tube (with colored stripes).
        (Bring Tube geometry into Wand)
*** UI ***
  - [] Implement final keyboard controls / desktop controls / also make a UI.
  - [] Implement final mobile controls.
*** Metadata ***
  - [-] Re-enable Twitter player? Check `index` and `bios` "twitter";
    - [-] https://developer. twitter.com/en/docs/twitter-for-websites/cards/overview/player-card
  - [x] Proofread all titles and descriptions.
  - [x] Add metadata / attributes and tags from the Google Sheet to the JSON.
    - [x] Ask about the Google Sheet / json generation flow.
*** Camera ***
  - [] Reload last camera position on refresh... make sense for users too?
  + Later / Bonus / Next Release
  - [] What would happen if I just randomly started removing or vibrating sets of vertices after
      a piece loaded?
  - [] There should be an "inner" and "outer" triangulation option.
        - [] Inner ONLY for complexity 1 and 2.
        - [] Optional elsewhere.
  - [] Add a generic `turn` function to `Spider` for fun procedural stuff.
  - [] Make sure no demo can record beyond the alotted geometry MAX. See `Tube` MAX.
  - [] Upgrade demo format to support overloaded color parameters for
        special / animated type colors.
  - [] Make a discord bot that pings the jeffrey channel for updated wand sculpture URLs /
    - keep an automated list somewhere... maybe look at the S3 shell scripts?
  - [] Should each stroke be its own Tube in order to support different
        geometry types? Yes if I wanna easily select and delete.
    - Do we need that for demo recording? No...
      - Would it be nice for the export data? Yes!
      - Could it be added subsequently after the drawings are complete? Yes.
      - (The files would load the same way.)
  - [] Change the top wand cursor position to something better. 
  - [] Re-enable thin line drawing.
*** (Wand Future) ***
  - [] Add / design / consider better touch controls. [F] [B] [W/L]
  - [] Add circular buffer to wand lines (buffer-geometry) / infinite
      wand with dissolving trail? Spectator mode.
  - [] Add audio to demo recording and playback.
  - [] Shrink three.js filesize: https://github.com/mrdoob/three.js/issues/19148
  - [] Fix keyboard hold and drag to symbol in Meta Quest software keyboard.
  - [] Add user connected messages.
  - [] Better mouse and keyboard controls.
  - [] Automatically reload the socket server?
    - [] Try and get web sockets working locally with SSL enabled (in general).
    - [] Try to get websockets server locally connectable in Meta Quest browser.
         https://github.com/nekocode/Websocket-Adb-Forward/blob/master/example-py-client/client.py
  - [] Does each player bring their lines with them?
       (This would make their presence very important.)
  - [] Add a "room" system.
    - [] Just start with one room but add an abstraction layer for it soon.
    - [] Save buffered lines via store["remote"] to the remote room.
         (Delete everything once all clients disconnect for a certain amount
          of time.)
      - [] Perhaps this can just use the system ram, then dump to S3 after
           clients disconnect?
  - [] How to hear someone else's audio stream? https://stackoverflow.com/a/61438244/8146077
  - [] Jiggle every two vertices the same amount so each segment
       drifts as one and maintains its integrity / add jiggle or break-apart
       as a toggle.
  - [] Add ability to delete lines / limit lines.
  - [] 🎖️ Pass the Whistlegraph accessibility/latency threshold.
    (Must work on a wide range of devices with various connections, and
     be fast enough or communicable enough to work.)
    - [] Sockets can always be a fallback.
    - [] Will require UDP over WebRTC.
    - [] Experiment with http/3 transport as well.
  - [] Draw in 2D draw with the mouse and project out the lines.
       - How to switch to an orthographic camera for this?
  - [] Free camera mode / better camera controls on keyboard and mouse.
  - [] Bring `microphone` amplitude into `3dline`. 
       (Draw a linear wave form spatially)
  - [] Bring in font rendering from the plotted system font.
  - [] Touch controls.
  - [] Add other tools other than line.
  - [] Add a 3d prompt so you can change tools / be more active without going 2d.
  - [] Updates on the 3D object pipeline.
    - [] Also be able to delete stuff from the GPU scene.
+ Done
  - [x] See `wand` todo section for pre-launch Freaky Flower items.
  - [x] Line thickness.
    - [x] https://codepen.io/garciahurtado/pen/WNVjog?editors=0010
    - [x] Work on implementing: https://github.com/mrdoob/three.js/blob/master/examples/webgl_lines_fat.html
    - [x] Tube geometry?
    - [] Billboarded MeshLine?
  - [x] Add CSV unreal exporter that follows this pattern:
      ---,Vertices,Color
      NewRow,"((X=10.000000,Y=20.000000,Z=-30.000000))","((B=0,G=0,R=201,A=0))"
      NewRow_0,"((X=10.000000,Y=20.000000,Z=-30.000000))","((B=0,G=0,R=201,A=0))"
  - [x] Replace Gl.lines with something cooler.
  - [x] Clear console on new disk load. (Only in production.) 
  - [x] Make workers optional / turn them back on. (but not for Meta Quest Browser).
  - [x] Make buffers transferrable.
  - [x] Reduce the latency of the pen information.
  - [x] Retrieve the floor / head height information.
  - [x] Make a "wand" / stick to draw with that comes off the VR controller. 
  - [x] Draw a line in webXR.
  - [x] Better output for Unreal Engine.
  - [x] Android keyboard...
  - [x] Save the buffered lines to indexedDB... and/or to the remote room?
    - [x] Store and reload from indexedDB.
    - [x] Save and load with ram storage.
  - [🌞] Add WebXR session.
    - [x] Use these demos.
      - https://github.com/immersive-web/webxr-samples/blob/main/vr-barebones.html
      - https://github.com/mrdoob/three.js/blob/master/examples/webxr_vr_paint.html
      - https://github.com/mrdoob/three.js/blob/master/examples/js/misc/TubePainter.js
    - [x] Future specs: https://immersive-web.github.io/raw-camera-access
  - [no] Can the keyboard automatically open when returning back to the prompt? 
    - (If it's not the first disk.) (All mobile platforms)
  - [x] Don't kill the keyboard if the disk fails to load. (Meta Quest 2)
  - [x] Get 3dline working on Firefox too.
  - [x] Auto-expand buffer at the start of prompt.
  - [x] Quickly going back and forth between 3dline and itself / prompt
       will cause errors. 
  - [x] Export an array of camera data from user, in addition to,
       where each step / frame is not duplicated and has a time delay.
  - [x] Add websocket support for other players and their lines.
  - [x] Should `Race` lines be more quantized / regulated?
  - [x] Synchronization error on 144hz displays with keep: false.
    (Notes)
    - The sim rate is locked at 120fps so on a 144hz display, we are gonna be
      rendering multiple times per sim which means things like `keep: false`
      will fail on Forms that are defined in sim. 
    - Garbage cannot be collected after each render unless
      the next sim has occurred... or we'll see flickering.
    - And it cannot be collected after each sim, because the next render
      may need it and sometimes sims only run once per frame, always before
      a render.
    - We can know if a render is first or a repeat since each sim.
    - If a render is first, then we can collect any garbage from the last
      frame before rendering.
  - [x] Add final red tail preview to point on `lift`.
  - [x] Add color and transparency to line.
    - [x] Use default params to change the color on load.
  - [x] Clean up smooth sim code.
  - [x] Draw with left mouse button while looking with right. (via `pen`)
  - [x] Density field idea / jiggle lines.
  - [x] Add spline / smooth drawing support: https://threejs.org/docs/#api/en/extras/curves/CatmullRomCurve3
    - [x] Add smooth sim. 
    - [x] Tried and cancelled in favor of doing a lazy nezumi style physical sim in 3D.
        (Better to filter the data on input)
  - [x] Disable ThreeD when leaving a piece that loaded it.
  - [x] Fix the deallocator.
  - [x] Fix "tri's" turn update.
  - [x] Fix "lines" no longer rendering.
  - [x] Break line into separate marks.
  - [x] Draw a line from the 3d cursor in 3d space.
    - [x] Stream numbers to the GPU / don't send copies of vertices every frame?
      - [x] Add identifiers to each Form.
      - [x] Keep track of what identifiers got uploaded to the GPU so far
           after each form call.
      - [x] Only send new identifiers to the GPU.
  - [x] Clean up the whole code and optimize the API for readability.
  - [x] Make a good wasd / mouse fps camera.
    - [x] Refactor existing controls to look nicer.
    - [x] And basic touch-only controls.
    - [x] Also add keyboard-only controls.
  - [x] Add an X on the ground... and maybe a horizon?
  - [x] Add triangle clipping.
  - [x] Add line clipping.
  - [x] Optimize screenMatrix.
  - [x] Basic FPS Camera.
  - [x] Make it all way faster?
  - [x] Draw a bresenham line in 3d.
  - [x] Switch shift and space in camdoll (space should move up).
  - [x] Show a full preview cursor while running a demo for demoWand? (Need a `demoWandCapForm`)
  - [x] Master the main materials and lights in the scene.
    - [x] Use G key to toggle lighting.
  - [x] Generate final model assets for Unreal.
  - [x] Create a "view wand timestamp" player that loads the model as a GLTF.
  - [x] Try out different export formats. (Using glb)
  - [x] Plug metadata into preview links.
  - [x] Hook up screenshots as og:images in `thumbnail` / make a special case in index.js for freaky-flowers and ff urls...
    - [-] Parse thumbnail parameters better / make it way faster? Eh...
  - [x‍] Take Final PNG screenshots of each work.
  - [x] Advancing a "freaky-flowers" token should advance its URL parameters.
  - [x] Generate a new GLB and JSON demo for every piece to replace each one,
      prefixing them with their token ID.
    - [x] Visit each piece in order, exporting a new (prefixed) demo from each.
  - [x] Produce much more accurate colors everywhere.
  - [x] Add screenshot button that works via WebGL for FF stills.
    - [x] Orthographic camera?
  - [x] Pick final URL structure for FF.
  - [x] Get demos working.
  - [x] Send spec for Jens
  - [x] Send examples of drawings and file formats for barry.
  - [x] Disable any lame "test" keyboard controls.
  - [x] Push the viewer to the server.
  - [x] Add parameter support with a copy+paste timestamp shortcut to the prompt
      for viewing the work.
  - [x] Add loading of JSON files via parameters in cadwand.
  - [x] Add a special line to the top of each demo file.
  - [x] Write final background color into the filename.
  - [x] Make 3 multiple properly oriented test sculptures without refreshing,
  - [x] Then check everything!
    - [x] Playback
    - [x] Check JSON
    - [x] Filesize
  - [x] Orientation
  - [x] Color
  - [x] Add color palettes.
  - [x] What would it be like if I tried to rotate the form around it's center slowly...
      in orthographic mode?
#endregion */

/* #region 🐕‍🦺 docs
  Pipe uploaded sculptures into vim: `aws s3api list-objects-v2 --bucket "wand.aesthetic.computer" --endpoint-url "https://sfo3.digitaloceanspaces.com" --query 'Contents[?LastModified>`2022-11-19`].Key' | nvim`
*/

// #region 🗺️ global
import { CamDoll } from "../lib/cam-doll.mjs";
import { MetaBrowser } from "../lib/platform.mjs";

const { min, abs, max, cos, sin, tan, floor, round } = Math;

let label;
let debug; // Set in boot.
let unreal = false; // Flip to true in case meshes need to be rendered for Unreal engine. Basically just prevents duplicate / two sided triangles.
let camdoll, stage; // Camera and stage.
let stageOn = false;
const rulers = false; // Whether to render arch. guidelines for development.
let measuringCube; // A toggled cube for conforming a sculpture to a scale.
let origin; // Some crossing lines to check the center of a sculpture.
let measuringCubeOn = true;
let cubeHeight = 1.45; // head of jeffrey
let orthographic = false; // Only in GPU for now.
let orthoZoom = 1; // Sent every frame when the camera is in orthographic mode.
let orthoZoomDir = 0; // Sent every frame when the camera is in orthographic mode.
let spin = true; // Interact in spin mode before a key is pressed.
let autoRotate = false; // Automatically rotate the Tube model.
let autoRotateDir = 1; // Automatically rotate the Tube model.
// let cubeHeight = 0.7; // floor of jeffrey-(6'1") Note: Store user's heights? 23.02.09.13.47
// let cubeHeight = 1.5; // head of jeffrey-(6'1")
let originOn = true;
let background; // Background color for the 3D environment.
let waving = false; // Whether we are making tubes or not.
let geometry = "triangles"; // "triangles" for surfaces or "lines" for wireframes
let race,
  speed = 18; //9; // Race after the cursor quickly.
let spi, // Follow it in even increments.
  color; // The current spider color read by the tube..
let tube, // Circumscribe the spider's path with a form.
  sides = 1, // Number of tube sides. 1 or 2 means flat.
  radius = 0.004, // The width of the tube.
  minRadius = 0.001,
  maxSides = 12, // Max and minimum sides alotted to tube geo complexity.
  minSides = 1,
  stepRel = () => radius * 1.01,
  step = stepRel(), // The length of each tube segment.
  capColor, // [255, 255, 255, 255] The currently selected tube end cap color.
  capVary = 0, // 2; How much to drift the colors for the cap.
  tubeVary = 0, // 50; How much to drift the colors for the tube.
  rayDist = 0.1, // How far away to draw the tube on non-spatial devices.
  graphPreview = false; // Whether to render pieces of a tube smaller or greater
//                         than `step` at the start of end of a stroke.
let wandForm; // A live cursor.

let demoWandTube, demoWandTubeData; // Also used for demoWandTube.

let demo, player; // For recording a session and rewatching it in realtime.
let playerProgLast = 0;
let playerBarSegs = [];
let lastPlayedFrames; // Keeps track of recently run through Player frames / demo frames.
//                       (Useful for re-dumping / modifying demo data.)
let noact = false;

let loadDemoSwitch,
  loadDemoProgress = 0,
  loadingDemoFile = false,
  progressBarDelay = 0,
  progressBarDelayMax = 12, // Frames to delay progress bar rendering by.
  demoLabel;
let needsWipe = null; // Flag that clears the buffer once on next paint.
let reconstructingWipe;

let lastWandPosition, lastWandRotation;

let beep, bop; // For making sounds when pieces begin and end.
let ping, pong; // For making sounds when pieces upload or fail to upload.
let bap; // For randomPalette. 🌈
let bip; // For demo playback.
let beatCount = 0n; // TODO: This should really go into the main API at this point... 22.11.15.05.22

let flashDuration = 10; // Screen flashes.
let flashCount = 0;

// 🏳️‍🌈Colors
let palettes; // Assigned at boot, for dynamics.
let saturationStep = 0.2;
let brightnessStep = 0.2;

// A special feature to generate random colors and
// backgrounds outside of the set palette system.
let randomPalette = false;
let randomPaletteCount = 0;
const randomPaletteCountMax = 18;

const racePoints = [], // Extra stuff for CAD-style drawing.
  diffPrevPoints = [],
  diffPrevColors = [],
  diffPoints = [],
  diffColors = [];
let trackerPoints = [],
  trackerColors = [];
let spiderForm, trackerForm;
let limiter = 0;

const flashes = [];
let currentFlash;
let cachedBackground;

// Numeric API hooks for helpers, assigned at `boot` and used in `library`.
let clamp, rr;

// This is just for convenience. The server URL is also replicated elsewhere
// right now.
const baseURL = "https://wand.aesthetic.computer";
const bucket = "wand";

let ellipsisTicker,
  ellipsisDots = 3; // Downloading... & Reconstructing...

// #endregion

function boot({
  Camera,
  Dolly,
  Form,
  QUAD,
  ORIGIN,
  CUBEL,
  num,
  geo,
  params,
  gizmo,
  debug: dbg,
  cursor,
  noise16DIGITPAIN,
  wipe,
  screen: { width, height },
}) {
  debug = dbg; // Set a global debug flag.
  // Assign some globals from the api.
  clamp = num.clamp;
  rr = num.randIntRange;

  color = almostWhite(); // Set starting colors.
  capColor = color;
  background = almostBlack();

  // Animated ellipsis for "Downloading..." & "Reconstructing..."
  ellipsisTicker = new gizmo.Hourglass(30, {
    completed: () => {
      ellipsisDots = (ellipsisDots + 1) % 4;
    },
    autoFlip: true,
  });

  palettes = {
    rgbwcmyk: [
      { fg: barely([255, 0, 0, 255]) }, // Barely-red on barely dim red.
      { fg: barely([0, 255, 0, 255]) }, // Barely-green on barely dim green.
      { fg: barely([0, 0, 255, 255]) }, // Barely-blue on barely dim blue.
      // {
      // fg: () => [rr(245, 255), rr(245, 255), rr(245, 255), 255],
      // }, // Not-quite-white on barely-grey.
      //{ fg: barely([0, 255, 255, 255]) }, // Barely-cyan on barely dim cyan.
      //{ fg: barely([255, 0, 255, 255]) }, // Barely-magenta on barely dim magenta.
      //{ fg: barely([255, 255, 0, 255]) }, // Barely-yellow on barely dim yellow
      //{ fg: almostBlack }, // Not-quite-black on barely grey.
    ],
    binary: [
      { fg: almostWhite, bg: almostBlack }, // Almost-white on almost-black.
      { fg: almostBlack, bg: almostWhite }, // Almost-black on almost-white.
    ],
    blackredyellow: [
      { fg: barely([20, 180, 40, 255]) }, // Almost-black on almost-white.
      { fg: barely([140, 90, 70, 255]) }, // Almost-black on almost-white.
    ],
    roygbiv: [
      { fg: barely([255, 0, 0, 255]) }, // Barely-red on barely mid-grey
      { fg: barely([255, 127, 0, 255]) }, // Barely-orange on barely mid-grey.
      { fg: barely([255, 255, 0, 255]) }, // Barely-yellow on barely mid-grey.
      { fg: barely([0, 255, 0, 255]) }, // Barely-blue on barely mid-grey.
      { fg: barely([0, 0, 255, 255]) }, // Barely-green on barely mid-grey.
      { fg: barely([75, 0, 30, 255]) }, // Barely-indigo on barely mid-grey.
      { fg: barely([148, 0, 211, 255]) }, // Barely-violet on barely mid-grey.
    ],
    // Keep track of which palette color we are on for the picker.
    indices: {
      rgbwcmyk: 0,
      blackredyellow: 0,
      binary: 0,
      roygbiv: 0,
    },
  };

  const aspectRatio = width / height;
  const cubeSize = 0.7;
  let fov = 80;
  let z;

  const shortSide = aspectRatio > 1 ? height : width;
  const h = cubeSize / 2 / Math.tan((fov / 2) * (Math.PI / 180));
  const w = h * aspectRatio;

  if (aspectRatio >= 1) {
    z = shortSide >= w ? h : w / aspectRatio;
  } else {
    z = shortSide >= h ? h / aspectRatio : w;
  }

  z += (cubeSize / 2) * 1.1;

  camdoll = new CamDoll(Camera, Dolly, {
    fov,
    z,
    y: cubeHeight,
    sensitivity: 0.00045 / 2,
  });

  stage = new Form(
    QUAD,
    { color: [2, 2, 2, 255] },
    /* { tex: painting(16, 16, (g) => g.noise16DIGITPAIN()) }, */
    { pos: [0, 0, 0], rot: [-90, 0, 0], scale: [1, 1, 1] },
  );

  measuringCube = new Form(
    CUBEL,
    { color: [255, 0, 0, 255] },
    { pos: [0, cubeHeight, 0], rot: [0, 0, 0], scale: cubeSize },
  );

  origin = new Form(ORIGIN, {
    pos: [0, cubeHeight, 0],
    rot: [0, 0, 0],
    scale: [0.01, 0.01, 0.01],
  });

  race = new geo.Race({ speed });

  // Load and play a demo file instantly from parameter 0... if it exists.
  if (params[0]) {
    let speed = parseInt(params[1]);
    if (speed < 0 || isNaN(speed)) speed = 5; // Make it instant if speed is <= 0 or undefined.
    const handle = "digitpain";
    const recordingSlug = `${params[0]}-recording-${handle}`;
    loadDemoSwitch = { slug: `${baseURL}/${recordingSlug}.json`, speed };
    stageOn = false;
    measuringCubeOn = false;
    originOn = false;
    cursor("native"); // Use a native cursor while the demo is loading.
    //wipe(0, 0, 0, 255); // Write a black background while loading.
  }

  // Start a demo recording (unless we are coming to watch only).
  if (!params[0]) {
    demo = new Demo(); // Start logging user interaction on demo frame 0.
    demo?.rec("room:color", background); // Record the starting bg color in case the default ever changes.
    demo?.rec("wand:color", color);
    wipe(0, 0); // Clear the software buffer to make sure we see the gpu layer.
  } else {
    noise16DIGITPAIN(); // Show a smoke-screen to prepare for loading.
  }

  tube = new Tube({ Form, num }, radius, sides, step, geometry);
}

function sim({
  pen,
  pen3d,
  Form,
  simCount,
  num,
  debug,
  gpuReady,
  gpu,
  meta,
  store,
  params,
  cursor,
  hud,
  help,
  net: { rewrite, preload, waitForPreload },
  num: { vec3, randIntRange: rr, dist3d, quat, vec4, mat3 },
}) {
  if (loadingDemoFile) ellipsisTicker.step(); // Animate loading ellipsis.

  // 😵‍💫 Spinning a sculpture around via `Tube`.
  if (autoRotate) tube.form.rotation[1] += 0.02 * autoRotateDir;

  if (autoRotate || spin) {
    tube.triCapForm.rotation[1] = tube.capForm.rotation[1] =
      tube.form.rotation[1];

    tube.form.gpuTransformed =
      tube.capForm.gpuTransformed =
      tube.triCapForm.gpuTransformed =
        true;
  }

  // 📽️ Loading and triggering a demo once the graphics system is ready.
  if (gpuReady && loadDemoSwitch) {
    const { speed, slug } = loadDemoSwitch;
    loadDemoSwitch = null;
    loadingDemoFile = true;
    noact = true;
    progressBarDelay = 0;
    loadDemoProgress = 0;
    cursor("native");

    preload(slug, false, (p) => {
      loadDemoProgress = p;
    }).then((data) => {
      tube.clear(); // Reset the tube geometry...

      const frames = parseDemoFrames(data);
      console.log("🎞️ Loaded a wand file:", frames.length);
      // Play all frames back.
      player = new Player(frames, speed, waitForPreload);
      player.paused = true; // Pause to wait for progress bar to render out.

      // Check to see if we have inherited any metadata from `freaky-flowers`.
      // And then use that data in order to
      if (store["freaky-flowers"]) {
        const ff = store["freaky-flowers"];
        const ffmeta = ff.meta({ params: [ff.tokenID], num });
        const path =
          `${ff.hook}~${ff.tokenID}` +
          params
            .slice(1)
            .map((p) => "~" + p)
            .join("");
        rewrite(path);
        meta(ffmeta); // Update page metadata.
        ff.headers(ff.tokenID); // Print any series headers.

        // const label = `${ffmeta.title} (ff ${ff.tokenID})`;
        label = `ff ${ff.tokenID}`; // Set the label globally.

        // Update label depending on the speed...
        if (speed === 0) {
          demoLabel = () => {
            needsWipe = [0, 0];
            hud.label(label);
            cursor("precise");
            demoLabel = undefined;
          }; // On `demo:complete`
        } else {
          demoLabel = () => {
            hud.label(label); // Update label.
            cursor("precise");
            demoLabel = undefined;
          };
        }

        autoRotate = false; // Rotate sculpture automatically.
        autoRotateDir = help.choose(-1, 1);
      }
    });
  }

  camdoll.sim(); // Update the camera + dolly.

  // 🌈 Rainbow Colors
  if (randomPalette) {
    if (randomPaletteCount === randomPaletteCountMax) {
      const fg = [rr(0, 255), rr(0, 255), rr(0, 255), 255];
      const bg = [rr(0, 255), rr(0, 255), rr(0, 255), 255];
      //const average = (fg[0] + fg[1] + fg[2]) / 3;
      // Generate a background either way above or way below the average.
      // (A light <-> dark / fg <-> bg relationship.
      // let bg;
      // if (average >= 128) {
      //   bg = [rr(0, 96), rr(0, 96), rr(0, 96), 255];
      // } else {
      //   bg = [rr(160, 255), rr(160, 255), rr(160, 255), 255];
      // }
      processNewColor(fg, bg);
      bap = true;
      randomPaletteCount = 0;
    } else {
      randomPaletteCount += 1;
    }
  }

  // Orthographic Camera Zoom
  if (orthographic) {
    orthoZoom += orthoZoomDir * 0.001;
    gpu.message({ type: "camera:ortho-zoom", content: orthoZoom });
  }

  // 🐭️ Live Cursor: generated from the controller position and direction.
  let position, lastPosition, rotation, controllerRotation, newNormal;

  if (pen3d && !player) {
    position = [pen3d.pos.x, pen3d.pos.y, pen3d.pos.z, 1];
    lastPosition = [pen3d.lastPos.x, pen3d.lastPos.y, pen3d.lastPos.z, 1];
    const dir = [pen3d.direction.x, pen3d.direction.y, pen3d.direction.z];

    // Measure number of vertices left.
    const length = (1 - tube.progress()) * 0.3;

    wandForm = new Form(
      {
        type: "line",
        positions: [
          position,
          vec3.add(
            vec3.create(),
            position,
            vec3.scale(vec3.create(), dir, length),
          ),
        ],
        colors: [color, color],
        keep: false,
      },
      { pos: [0, 0, 0] },
    );

    rotation = quat.fromValues(
      pen3d.rotation._x,
      pen3d.rotation._y,
      pen3d.rotation._z,
      pen3d.rotation._w,
    );

    controllerRotation = quat.fromValues(
      pen3d.rotation._x,
      pen3d.rotation._y,
      pen3d.rotation._z,
      pen3d.rotation._w,
    );

    if (spi === undefined) {
      spi = new Spider(
        { num, debug },
        [...position],
        lastPosition,
        dir,
        rotation,
        color,
      );
      race.start(spi.state.position);
    }

    // Record current wand position into the active demo if it has changed.
    // Format: wand, PX, PY, PZ, QX, QY, QZ
    // TODO: Generalize this with the below. 22.11.14.22.47
    if (
      (lastWandPosition !== undefined &&
        lastWandRotation !== undefined &&
        (lastWandPosition.x !== pen3d.pos.x ||
          lastWandPosition.y !== pen3d.pos.y ||
          lastWandPosition.z !== pen3d.pos.z ||
          lastWandRotation._x !== pen3d.rotation._x ||
          lastWandRotation._y !== pen3d.rotation._y ||
          lastWandRotation._z !== pen3d.rotation._z ||
          lastWandRotation._w !== pen3d.rotation._w)) ||
      (lastWandPosition === undefined && lastWandRotation === undefined)
    ) {
      demo?.rec("wand", [
        pen3d.pos.x,
        pen3d.pos.y - cubeHeight,
        pen3d.pos.z,
        pen3d.rotation._x,
        pen3d.rotation._y,
        pen3d.rotation._z,
        pen3d.rotation._w,
      ]);
      lastWandPosition = pen3d.pos;
      lastWandRotation = pen3d.rotation;
    }

    // Show a live preview of what will be drawn, with measurement lines
    // as needed.
    {
      // This block is very similar to `crawlTowards` in `Spider`,
      // Except customized to make a preview. The functionality could
      // probably be merged, otherwise they have to be kept in sync.

      let lpos, pos, tpos, rot;

      lpos = spi.state.position;
      pos = race.pos;
      tpos = position;

      if (tube.gesture.length === 0) {
        rot = rotation;
      } else {
        rot = spi.state.rotation;
      }

      const firstTangent = vec3.normalize(
        vec3.create(),
        vec3.sub(vec3.create(), pos, lpos),
      );

      const nextTangent = vec3.normalize(
        vec3.create(),
        vec3.sub(vec3.create(), tpos, lpos),
      );

      let lastNormal = vec4.transformQuat(vec4.create(), [0, 1, 0, 1], rot);

      const helper = vec3.normalize(
        vec3.create(),
        vec3.cross(vec3.create(), lastNormal, firstTangent),
      );

      lastNormal = vec3.normalize(
        vec3.create(),
        vec3.cross(vec3.create(), firstTangent, helper),
      );

      if (waving) {
        diffPrevPoints.push(
          // For debugging.
          pos,
          vec3.add(
            vec3.create(),
            pos,
            vec3.scale(vec3.create(), lastNormal, 0.15),
          ),
          pos,
          vec3.add(vec3.create(), pos, vec3.scale(vec3.create(), helper, 0.15)),
        );
        diffPrevColors.push(
          [255, 255, 100, 255],
          [255, 255, 100, 255],
          [50, 255, 100, 255],
          [50, 255, 100, 255],
        );
      }

      // 🥢 Measurement of normals for rotation.
      let bitangent = vec3.cross(vec3.create(), firstTangent, nextTangent);

      if (vec3.length(bitangent) === 0) {
        newNormal = lastNormal;
      } else {
        // Get angle between first and next tangent.
        let rq = quat.normalize(
          quat.create(),
          quat.rotationTo(quat.create(), firstTangent, nextTangent),
        );
        newNormal = vec3.transformQuat(vec3.create(), lastNormal, rq);
        spi.lastNormal = newNormal; // hmm... 22.11.18.03.57
      }

      bitangent = vec3.normalize(
        vec3.create(),
        vec3.cross(vec3.create(), newNormal, nextTangent),
      );

      if (waving) {
        diffPrevPoints.push(
          // For debugging.
          pos,
          vec3.add(
            vec3.create(),
            pos,
            vec3.scale(vec3.create(), newNormal, 0.15),
          ),
          pos,
          vec3.add(
            vec3.create(),
            pos,
            vec3.scale(vec3.create(), bitangent, 0.15),
          ),
        );
        diffPrevColors.push(
          [255, 0, 0, 255],
          [255, 0, 0, 255],
          [50, 0, 255, 255],
          [50, 0, 255, 255],
        );
      }

      // Build a rotation transform.
      const qua = quat.normalize(
        quat.create(),
        quat.fromMat3(
          quat.create(),
          mat3.fromValues(...bitangent, ...newNormal, ...nextTangent),
        ),
      );

      rotation = qua;

      // Create separate preview segments for different edge cases.
      if (waving) {
        if (tube.gesture.length === 0) {
          spi.rotation = rotation;
          tube.preview(spi.state, {
            position,
            rotation,
            color,
          });
        } else {
          if (tube.sides === 2) {
            // Use original rotation for ribbons.  🎀
            tube.preview(spi.state, {
              position,
              rotation: controllerRotation,
              color,
            });
          } else {
            tube.preview(spi.state, { position, rotation, color });
          }
        }
      } else {
        tube.preview({
          position: position,
          rotation: controllerRotation,
          color: color,
        });
      }
      // tube.preview({ position, rotation, color: [255, 0, 0, 255] });
    }
    // Default / non-waving tube preview.
    //}
  } else if (pen) {
    // TODO: Also generate a full live cursor here... using the above code
    //       for spatial devices.
    position = camdoll.cam.ray(pen.x, pen.y, rayDist, true);
    rotation = quat.fromEuler(quat.create(), ...camdoll.cam.rot);

    // 🔴 Record current wand position into the active demo if it has changed.
    // Format: wand, PX, PY, PZ, QX, QY, QZ
    // TODO: Generalize this with the above. 22.11.14.22.46
    if (
      (lastWandPosition !== undefined &&
        lastWandRotation !== undefined &&
        (lastWandPosition[0] !== position[0] ||
          lastWandPosition[1] !== position[1] ||
          lastWandPosition[2] !== position[2] ||
          lastWandRotation[0] !== rotation[0] ||
          lastWandRotation[1] !== rotation[1] ||
          lastWandRotation[2] !== rotation[2] ||
          lastWandRotation[3] !== rotation[3])) ||
      (lastWandPosition === undefined && lastWandRotation === undefined)
    ) {
      const demoPos = position.slice(0, 3);
      demoPos[1] -= cubeHeight;
      demo?.rec("wand", [...demoPos, ...rotation]);
      lastWandPosition = position;
      lastWandRotation = rotation;
    }

    //tube.preview({ position, rotation, color: [255, 0, 0, 255] });
  }
  lastWandPosition = position;

  // 🏁 Move the race finish line to the current cursor position.
  // Compute an in-progress gesture.
  if (
    pen3d &&
    race.pos &&
    pen3d.pos.x !== 0 &&
    pen3d.pos.y !== 0 &&
    pen3d.pos.z !== 0
  ) {
    race.to([pen3d.pos.x, pen3d.pos.y, pen3d.pos.z, 1]);
  } else if (race.pos && pen && pen.x && pen.y) {
    race.to(camdoll.cam.ray(pen.x, pen.y, rayDist / 4, true));
  }

  if (waving && !player) {
    let d = dist3d(spi.state.position, race.pos);
    // 🕷️ Spider Jump 🕷️
    const step = tube.step;

    if (tube.gesture.length === 0 && d > step) {
      // Populate first tube start with the preview state.
      if (pen3d) {
        tube.start(spi.state, radius, sides, step);
        // Move manually.
        const direction = vec4.transformQuat(
          vec4.create(),
          [0, 0, step, 1],
          rotation,
        );

        const lastPosition = spi.position;
        spi.position = vec3.add(vec3.create(), spi.position, direction);
        const position = spi.position;

        tube.goto(spi.state, undefined, false, false);

        spi = new Spider(
          { num, debug },
          position,
          lastPosition,
          direction,
          rotation,
          color,
        );

        spi.lastNormal = newNormal;
      } else {
        // 🖱️ Planar
        //tube.start(spi.state, radius, sides, step);
        //spi.crawlTowards(race.pos, step, 1); // <- last param is a tightness fit
        //tube.goto(spi.state); // 2. Knots the tube.
      }
    } else if (tube.gesture.length > 0) {
      if (pen3d) {
        // 🥽 VR
        if (tube.sides === 1) {
          // ♾️ Curvy / cut corner loops.
          if (d > step) {
            const repeats = floor(d / step);
            for (let i = 0; i < repeats; i += 1) {
              spi.crawlTowards(race.pos, step, (i + 1) / repeats); // <- last parm is a tightness fit
            }
            tube.goto(spi.state, undefined, false, false); // Knot the tube just once.
          }
        } else if (tube.sides === 2) {
          // 🎀 Ribbons (Manual Translation)
          if (d > step) {
            const repeats = floor(d / step);
            for (let i = 0; i < repeats; i += 1) {
              spi.crawlTowards(race.pos, step, 1); // <- last param is a tightness fit
              spi.rotation = controllerRotation; // What would crawlTowards actually be doing here? 22.11.17.11.43
              tube.goto(spi.state, undefined, false, false); // Knot the tube just once.
            }
          }
        } else if (tube.sides > 2) {
          // ♾️ Curvy / cut corner loops.
          // This is basically magic. 🪄
          if (d > step) {
            let repeats = min(6, floor(d / step));
            const spiToRace = vec3.normalize(
              vec3.create(),
              vec3.sub(vec3.create(), race.pos, spi.state.position),
            );
            let dot = vec3.dot(spiToRace, spi.state.direction);
            let divisor = max(3, round(sides / 2));
            if (abs(dot) > 0.55) {
              divisor = 1;
            }
            let tightness = 1 / divisor;
            const increments = step / divisor;

            for (let r = 0; r < repeats; r += 1) {
              for (let i = 0; i < divisor; i += 1) {
                spi.crawlTowards(race.pos, increments, tightness); // <- last param is a tightness fit
              }
              tube.goto(spi.state); // Knot the tube just once per repeat.
            }
          }
        }
      } else if (!pen3d) {
        // 🖱️ Planar
        // TODO: Disable pc for now... 22.11.17.10.34
        //console.log(spi.state);
        //spi.crawlTowards(race.pos, step, 1); // <- last parm is a tightness fit
        //tube.goto(spi.state); // 2. Knots the tube.
      }
    }

    // Add some debug data to show the future direction.
    // const scaledDiff = vec3.scale(vec3.create(), diff, 2);
    // trackerPoints = [
    //   // spi.state.position,
    //   // vec3.add(vec3.create(), spi.state.position, [pen3d.direction.x, pen3d.direction.y, pen3d.direction.z]),
    //   spi.state.position,
    //   vec3.add(vec3.create(), spi.state.position, scaledDiff),
    //   spi.state.position,
    //   vec3.add(vec3.create(), spi.state.position, spi.state.direction),
    // ];
    // if (dot > 0.5) {
    //   // For debugging.
    //   trackerColors = [
    //     // [255, 255, 255, 255],
    //     // [255, 255, 255, 255],
    //     [0, 255, 0, 255],
    //     [0, 255, 0, 255],
    //     [255, 255, 0, 255],
    //     [255, 255, 0, 255],
    //   ];
    // } else {
    //   trackerColors = [
    //     // [255, 255, 255, 255],
    //     // [255, 255, 255, 255],
    //     [255, 0, 0, 255],
    //     [255, 0, 0, 255],
    //     [255, 255, 0, 255],
    //     [255, 255, 0, 255],
    //   ];
    // }
  }

  demo?.sim(simCount); // 🔴 Update the demo frame count.

  player?.sim((frames) => {
    // Parse demo frames and act on them in order.
    // 🟢 Advance forward any player frames.
    frames.forEach((f) => {
      const type = f[1];
      const di = 2;
      if (type === "piece:info") {
        console.log("🟢 Playing back...");
        console.log("🙆 Artist:", f[di + 1]);
        console.log("📆 Timestamp:", f[di]);
      } else if (type === "room:color") {
        // ❔ tick, room:color, R, G, B, A
        background = [f[di], f[di + 1], f[di + 2]];
        bip = true;
      } else if (type === "wand:color") {
        // ❔ tick, wand:color (true / false based on starting light or dark value)
        color = [f[di], f[di + 1], f[di + 2], f[di + 3]];
        capColor = [f[di], f[di + 1], f[di + 2], f[di + 3]];
        bip = true;
        // Skip `true` and `false` values for now.
      } else if (type === "wand") {
        // ❔ tick, wand, PX, PY, PZ, QX, QY, QZ, QW
        const pos = [f[di], f[di + 1] + cubeHeight, f[di + 2], 1];
        const rot = [f[di + 3], f[di + 4], f[di + 5], f[di + 6]];
        const pos2 = vec3.transformQuat(vec3.create(), [0, 0, 0.15], rot);
        const np = [...vec3.add(vec3.create(), pos, pos2), 1];

        demoWandTubeData = {
          positions: [pos, np],
          colors: [color, color],
          rotation: rot,
        };

        if (tube.lastPathP) {
          // Disable showing the wand preview form before the first block
          // of a gesture gets added. (Stay surprising!) 23.02.08.19.28
          const previewPoints = [{ position: pos, rotation: rot, color }];
          previewPoints[0].rotation = tube.lastPathP.rotation;
          previewPoints.unshift({
            position: tube.lastPathP.pos,
            direction: tube.lastPathP.direction,
            rotation: tube.lastPathP.rotation,
            color: tube.lastPathP.color,
          });
          tube.playbackPreview(...previewPoints); // Show a tube preview.
        }
      } else if (type === "tube:start") {
        tube.start(
          {
            position: [f[di], f[di + 1] + cubeHeight, f[di + 2]],
            rotation: [f[di + 3], f[di + 4], f[di + 5], f[di + 6]],
            color,
          },
          radius,
          sides,
          step,
          true,
        );
      } else if (type === "tube:goto") {
        // ❔ tick, tube:goto, PX, PY, PZ, QX, QY, QZ
        tube.goto(
          {
            position: [f[di], f[di + 1] + cubeHeight, f[di + 2]],
            rotation: [f[di + 3], f[di + 4], f[di + 5], f[di + 6]],
            color,
          },
          undefined,
          true,
          true,
        );
      } else if (type === "tube:radius") {
        // ❔ tick, tube:radius N
        tube.update({ radius: f[di] });
        radius = f[di];
      } else if (type === "tube:sides") {
        // ❔ tick, tube:sides N
        tube.update({ sides: f[di] });
        sides = f[di];
      } else if (type === "tube:step") {
        // ❔ tick, tube:step N
        tube.update({ step: f[di] });
        step = f[di];
      } else if (type === "tube:stop") {
        // ❔ tick, tube:stop, (no other data needed)
        tube.stop(true);
      } else if (type === "demo:complete") {
        // A "synthesized frame" with no other information to destroy our player.
        lastPlayedFrames = player.frames;
        player = null;
        playerProgLast = 0;
        playerBarSegs = [];
        setTimeout(() => {
          needsWipe = [0, 0];
          ping = true;
        }, 250); // Wipe after a sec.
        noact = false; // Re-enable acts.
        demoWandTubeData = null;
        demoWandTube = null;

        tube.capForm.clear(); // Clear any preview cursors leftover.
        tube.triCapForm.clear();

        demoLabel?.();

        // #region relic-1
        // 📔 Some old scripts to automate making changes to demos and GLB files.
        //    Leaving them here in case I need to do automation again!

        // Advance to next piece, save json, etc. etc.
        /*
        {
          // GLB
          const tokenID = store["freaky-flowers"].tokenID;
          const ts = store["freaky-flowers"].tokens[tokenID] || timestamp();
          const handle = "digitpain"; // Hardcoded for now.
          const bg = rgbToHexStr(...background.slice(0, 3)).toUpperCase(); // Empty string for no `#` prefix.

          let sculptureSlug = `ff${tokenID}-${ts}-sculpture-${bg}-${handle}`;

          // Prepend "ff#-" if a freakyFlowersToken has been loaded.
          //if (store["freaky-flowers"]?.tokenID >= 0) {
          //  sculptureSlug = `${store["freaky-flowers"].tokenID}-${sculptureSlug}`;
          //}

          setTimeout(function () {
            gpu.message({
              type: "export-scene",
              content: {
                slug: sculptureSlug,
                output: "local",
                sculptureHeight: cubeHeight,
              },
            });
            advanceSeries(store["freaky-flowers"], 1, true);
          }, 500);
        }
        */

        /*
        // DEMOS
        setTimeout(function () {
          {
            const ts = lastPlayedFrames[0][2]; // 0n, "piece:info", timestamp, author
            const handle = lastPlayedFrames[0][3]; // 0n, "piece:info", timestamp, author

            console.log("🎨 Adjusting colors!");
            lastPlayedFrames.forEach((f, i) => {
              if (f[1].endsWith("color")) {
                let c = f.slice(2, 5).map((v) => v / 255);
                let nc = c;
                // Convert everything to sRGB.
                nc = [
                  LinearToSRGB(nc[0]),
                  LinearToSRGB(nc[1]),
                  LinearToSRGB(nc[2]),
                ];

                // And bump down things that aren't black...
                if ((nc[0] + nc[1] + nc[2]) / 3 > 0.08) {
                  nc = shiftRGB(nc, c, 0.2, "lerp", 1);
                }

                f[2] = round(nc[0] * 255);
                f[3] = round(nc[1] * 255);
                f[4] = round(nc[2] * 255);
              }
            });

            // TODO: These params do not reload each time.
            const slug = `ff${store["freaky-flowers"].tokenID}-${ts}-recording-${handle}.json`;
            download(slug, lastPlayedFrames.slice()); // Save modified demo to json.
          }
          advanceSeries(store["freaky-flowers"], 1, true);
        }, 100);
        */
        // #endregion
      }
    });

    if (player?.instant === 0) {
      help.repeat(3, () => ellipsisTicker.step()); // Animate ellipsis.
      let ellipsis = "";
      help.repeat(ellipsisDots, () => (ellipsis += "."));
      ellipsis = ellipsis.padEnd(3, " ");
      hud.label(`Reconstructing${ellipsis}`);
    }
  });
}

function paint({
  form,
  ink,
  hud,
  Form,
  wipe,
  help,
  noiseTinted,
  num,
  screen: { width, height },
}) {
  // Flash the screen sometimes.
  if (flashes.length > 0) {
    if (currentFlash === undefined) currentFlash = 0;

    if (!flashes[currentFlash]) {
      flashes.length = 0;
      cachedBackground = undefined;
    } else {
      // If we are in a current flash... count up.
      background = flashes[currentFlash];
      flashCount += 1;

      if (flashCount === flashDuration) {
        flashCount = 0;
        background = cachedBackground;
        flashes.shift();
        // Remove this flash from the flashes list if it exists.
      }
    }
  }

  // #region 🐛 Debugging drawing.
  // Draw the path of the race.
  /*
  const racePositions = [];
  const raceColors = [];

  for (let r = 0; r < racePoints.length - 1; r += 1) {
    racePositions.push(racePoints[r], racePoints[r + 1]);
    raceColors.push([127, 0, 127, 255], [127, 0, 127, 255]);
  }

  //console.log(racePositions);

  const raceForm = new Form(
    {
      type: "line",
      positions: racePositions,
      colors: raceColors,
      gradients: true,
      keep: false,
    },
    { color: [255, 255, 255, 255] },
    { scale: [1, 1, 1] }
  );

  // The spider's path so far.
  if (spi) {
    const spiderPositions = [];
    const spiderColors = [];

    for (let s = 0; s < spi.path.length - 1; s += 1) {
      spiderPositions.push(spi.path[s].position, spi.path[s + 1].position);
      spiderColors.push([255, 255, 255, 255], [0, 0, 0, 255]);
      //spiderColors.push(spi.path[s + 1].color, spi.path[s + 1].color);
    }

    spiderForm = new Form(
      {
        type: "line",
        positions: spiderPositions,
        colors: spiderColors,
        gradients: true,
        keep: false,
      },
      { color: [255, 255, 255, 255] },
      { scale: [1, 1, 1] }
    );
  }
  */

  // Other measurement lines.
  if (rulers) {
    const diffPrevPositions = [];
    const diffPrevCol = [];

    for (let d = 0; d < diffPrevPoints.length - 1; d += 2) {
      diffPrevPositions.push(
        [...diffPrevPoints[d], 1],
        [...diffPrevPoints[d + 1], 1],
      );
      diffPrevCol.push(diffPrevColors[d], diffPrevColors[d + 1]);
    }

    diffPrevPoints.length = 0;

    const diffPrevForm = new Form(
      {
        type: "line",
        positions: diffPrevPositions,
        colors: diffPrevColors,
        gradients: false,
        keep: false,
      },
      { color: [255, 0, 0, 255] },
      { scale: [1, 1, 1] },
    );

    const diffPositions = [];
    const diffCol = [];

    for (let d = 0; d < diffPoints.length - 1; d += 2) {
      diffPositions.push([...diffPoints[d], 1], [...diffPoints[d + 1], 1]);
      diffCol.push(diffColors[d], diffColors[d + 1]);
    }

    const diffForm = new Form(
      {
        type: "line",
        positions: diffPositions,
        colors: diffColors,
        gradients: false,
        keep: false,
      },
      { color: [255, 0, 0, 255] },
      { scale: [1, 1, 1] },
    );

    // Draw some cursor measurement lines.
    trackerForm = new Form(
      {
        type: "line",
        positions: trackerPoints,
        colors: trackerColors,
        gradients: true,
        keep: false,
      },
      { color: [255, 255, 255, 255] },
      { scale: [1, 1, 1] },
    );

    form([trackerForm, diffPrevForm, diffForm, raceForm], camdoll.cam, {
      cpu: false,
    });
  }
  // #endregion

  // Demo playback / preview cursors.
  if (demoWandTubeData) {
    const tubeSides = 16;
    const tubeRadius = 0.002;

    // Instantiate a Tube for the preview.
    if (!demoWandTube) {
      demoWandTube = new Tube(
        { Form, num },
        tubeRadius,
        tubeSides,
        step,
        geometry,
        undefined,
        true, // isWand
      );
      // demoWandTube.form.tag = "demo-wand"; // Disables wand occlusion.
    } else {
      demoWandTube.clear();
    }

    demoWandTube.start(
      {
        position: demoWandTubeData.positions[0],
        rotation: demoWandTubeData.rotation,
        color: demoWandTubeData.colors[0],
      },
      tubeRadius,
      tubeSides,
      step,
    );

    demoWandTube.goto(
      {
        position: demoWandTubeData.positions[1],
        rotation: demoWandTubeData.rotation,
        color: demoWandTubeData.colors[1],
      },
      undefined,
      false,
      false,
    );
    demoWandTube.stop(false);

    demoWandTube.form.rotation[1] =
      demoWandTube.triCapForm.rotation[1] =
      demoWandTube.capForm.rotation[1] =
        tube.form.rotation[1];

    demoWandTube.form.gpuTransformed =
      demoWandTube.capForm.gpuTransformed =
      demoWandTube.triCapForm.gpuTransformed =
        true;
  }

  // Loading screen.
  if (loadingDemoFile) {
    noiseTinted([127, 127, 127], 0.6, 0.7);
    if (progressBarDelay < progressBarDelayMax) {
      progressBarDelay += 1;
    } else {
      if (loadDemoProgress < 1) {
        let ellipsis = "";
        help.repeat(ellipsisDots, () => (ellipsis += "."));
        ellipsis = ellipsis.padEnd(3, " ");
        if (loadDemoProgress < 1) hud.label(`Downloading${ellipsis}`);
      }
      ink(64).box(0, height - 8, loadDemoProgress * width, height - 8);
      if (loadDemoProgress === 1 && player) {
        player.paused = false;
        loadingDemoFile = false; // Stop loading if we are incrementally playing back the file. Otherwise we will flag this in Player.
        if (player.instant !== 0) {
          demoLabel?.();
          noact = false;
        }
        reconstructingWipe = num.randIntArr(128, 3);
        needsWipe = player.instant === 0 ? reconstructingWipe : [0, 0];
      }
    }
  }

  // TODO: Put other types in here to draw color patterns.
  if (needsWipe) {
    wipe(...needsWipe);
    needsWipe = null;
  }

  // Loading bar for Playback & Reconstruction.
  if (player) {
    const p = player.progress;
    let barColor = color; //player.instant === 0 ? [64, 0, 0] : color;
    const thickness = player.instant === 0 ? 8 : 2;
    const y = player.instant === 0 ? height - thickness : 0;

    if (player.instant !== 0) {
      hud.label(label, barColor, { x: 4, y: 6 });
    }

    ink(0).box(0, y, width, thickness);

    // Push the segment to an array... then redraw everything in the array.
    playerBarSegs.push([barColor, playerProgLast, 0, p, thickness]);

    playerBarSegs.forEach((seg) =>
      ink(seg[0]).box(
        floor(seg[1] * width),
        player.instant === 0 ? height - thickness : seg[2],
        floor((seg[3] - seg[1]) * width + 1),
        seg[4],
      ),
    );

    playerProgLast = p;
  }

  form(
    [
      // raceForm,
      // spiderForm,
      tube.form,
      tube.lineForm,
      tube.capForm,
      tube.triCapForm,
      demoWandTube?.form,
      demoWandTube?.capForm,
      demoWandTube?.triCapForm,
      !player ? wandForm : null,
    ],
    camdoll.cam,
    { background, cpu: false },
  );

  if (stageOn) form(stage, camdoll.cam, { cpu: false });
  if (measuringCubeOn) form(measuringCube, camdoll.cam, { cpu: false });
  if (originOn) form(origin, camdoll.cam, { cpu: false });
}

function act({
  event: e,
  gpu,
  params,
  debug,
  sideload,
  download,
  // serverUpload, // TODO: This has been renamed to upload.
  num,
  store,
}) {
  if (e.is("reframed") && player && player.instant === 0)
    needsWipe = reconstructingWipe; // Keep solid color while reconstructing.

  if (noact) return; // Disable all acts if we are loading or reconstructing...

  const {
    quat,
    timestamp,
    clamp,
    saturate,
    desaturate,
    shiftRGB,
    rgbToHexStr,
  } = num;

  if (!spin) camdoll.act(e); // Wire up FPS style navigation events.
  if (
    spin === true &&
    (e.is("keyboard:down:w") || e.is("keyboard:down:space"))
  ) {
    addFlash([0, 255, 0, 255]);

    bap = true;
    setTimeout(() => {
      bop = true;
      addFlash([0, 150, 0, 255]);
    }, 150);

    spin = false;
    camdoll.act(e);
  }

  // Reset a running player from the start.
  if (player && (e.is("keyboard:down:r") || e.is("3d:rhand-button-b-down"))) {
    console.log("️⏱️ Starting over...");
    tube.clear();
    needsWipe = [0, 0];
    playerBarSegs = [];
    player = new Player(player.frames, player.instant);
  }

  // 🥽 Start a gesture. (Spatial)
  if (e.is("3d:touch:2") && !player) {
    const last = [e.lastPosition.x, e.lastPosition.y, e.lastPosition.z];
    const cur = [e.pos.x, e.pos.y, e.pos.z];
    const rot = quat.fromValues(
      e.rotation._x,
      e.rotation._y,
      e.rotation._z,
      e.rotation._w,
    );

    const dir = [e.direction.x, e.direction.y, e.direction.z]; // ❓ Is this even used? 22.11.13.16.41

    spi = new Spider({ num, debug }, cur, last, dir, rot, color);
    race.start(spi.state.position);

    if (tube.progress() < 1) {
      waving = true;
    } else {
      pong = true;
    }

    // TODO: Record a wand:gesture:start event here. 23.02.04.22.13
  }

  // 🖱️ Start a gesture. (Screen)
  /*
  if (e.is("touch") && e.button === 0 && pen && pen.x && pen.y) {
    const far = camdoll.cam.ray(pen.x, pen.y, rayDist, true);
    const near = camdoll.cam.ray(pen.x, pen.y, rayDist / 2, true);
    const dir = vec3.sub(vec3.create(), near, far);
    const rot = quat.fromEuler(quat.create(), ...camdoll.cam.rot);
    spi = new Spider({ num, debug }, near, far, dir, rot, color);
    race.start(spi.state.position);
    waving = true;
  }
  */

  // 🛑 Stop a gesture.
  if (/*(e.is("lift") && e.button === 0) ||*/ e.is("3d:lift:2") && !player) {
    waving = false;
    if (e.is("3d:lift:2")) {
      tube.stop();
    } else if (spi) {
      tube.stop();
    }
  }

  // 🧊 Toggle cube and origin measurement lines.
  if (e.is("keyboard:down:t") || e.is("3d:rhand-button-a-down")) {
    pong = true;
    measuringCubeOn = !measuringCubeOn;
    originOn = !originOn;
    if (measuringCubeOn && originOn) {
      measuringCube.resetUID();
      origin.resetUID();
    }
  }

  // Switch camera mode.
  if (e.is("keyboard:down:g")) {
    gpu.message({
      type: "scene:lighting-switch",
    });
  }

  // Switch camera mode.
  if (e.is("keyboard:down:o")) {
    orthographic = !orthographic;
    camdoll.cam.type = orthographic ? "orthographic" : "perspective";
    gpu.message({
      type: "camera:mode-switch",
    });
  }

  // Automatic model rotation.
  if (e.is("keyboard:down:1")) autoRotate = !autoRotate;

  // Horizontal model rotation in orthographic camera or "spin" mode.
  if (e.is("draw") && (spin || camdoll.cam.type === "orthographic")) {
    //tube.form.rotation[1] -= e.delta.y / 3.5; // Up and down?
    tube.form.rotation[1] += e.delta.x / 1.5; // Left and right.
    tube.form.gpuTransformed = true;
  }

  // Zoom in and out while in orthographic mode.
  if (e.is("keyboard:down:=") && !e.repeat) orthoZoomDir = -1;
  if (e.is("keyboard:down:-") && !e.repeat) orthoZoomDir = 1;
  if (e.is("keyboard:up:=") || e.is("keyboard:up:-")) orthoZoomDir = 0;

  if (e.shift === false && e.is("keyboard:down:i")) {
    const tokenID = store["freaky-flowers"]?.tokenID;
    let tokenSlug = store["freaky-flowers"]?.tokens[tokenID];
    if (tokenSlug) tokenSlug = `ff${tokenID}-` + tokenSlug;
    const ts = tokenSlug || params[0] || timestamp();
    const handle = "digitpain"; // Hardcoded for now.
    const screenshotSlug = `${ts}-still-${handle}`;
    gpu.message({
      type: "screenshot",
      content: {
        slug: screenshotSlug,
        format: "png",
        output: "local",
        camera: {
          position: camdoll.cam.position,
          rotation: camdoll.cam.rotation,
        },
        // squareThumbnail: e.alt,
        squareThumbnail: false,
      },
    });
  }

  // Left hand controller.

  // Radius
  if (e.is("3d:rhand-axis-y") && !player) {
    if (abs(e.value) > 0.01) {
      radius = clamp(radius + e.value * -0.00025, minRadius, 2);
      step = stepRel();
      tube.update({ radius, step });
      demo?.rec("tube:radius", radius);
      demo?.rec("tube:step", step);
    }
  }

  // Side Count
  if (e.is("3d:rhand-trigger-secondary-down") && !player) {
    if (tube.gesture.length === 0) {
      ping = true;
      sides = min(maxSides, sides + 1);
      tube.update({ sides });
      demo?.rec("tube:sides", sides);
    }
  }

  if (e.is("3d:lhand-trigger-secondary-down") && !player) {
    if (tube.gesture.length === 0) {
      bop = true;
      sides = max(minSides, sides - 1);
      tube.update({ sides });
      demo?.rec("tube:sides", sides);
    }
  }

  // 🗺️ COLOR:CONTROLS (all on left hand) 🎨

  if (e.is("3d:lhand-button-thumb-down") && !player) {
    // Switch between a black and white foreground and background relationship.
    advancePalette("binary");
    beep = true;
  }

  if (e.is("3d:lhand-button-y-down") && !player) {
    advancePalette("rgbwcmyk");
    beep = true;
    // breep = true; // Note: typo... but lol, breep should be a default sound... 😄
  }

  if (e.is("3d:lhand-button-x-down") && !player) {
    advancePalette("roygbiv");
    beep = true;
  }

  // Brightness: increase +
  if (e.is("3d:lhand-axis-y-up") && !player) {
    // Lerp to an almost-white color.
    processNewColor(shiftRGB(color, almostWhite(), brightnessStep, "add"));
    beep = true;
  }

  // Brightness: decrease -
  if (e.is("3d:lhand-axis-y-down") && !player) {
    // Lerp to an almost-black color.
    processNewColor(shiftRGB(color, almostBlack(), brightnessStep, "subtract"));
    beep = true;
  }

  // Saturation: increase +
  if (e.is("3d:lhand-axis-x-right") && !player) {
    // Lerp to the most saturated version of the current color.
    processNewColor(shiftRGB(color, saturate(color, 1), saturationStep));
    beep = true;
  }

  // Saturation: decrease -
  if (e.is("3d:lhand-axis-x-left") && !player) {
    // Lerp to the most desaturated of the current color.
    processNewColor(shiftRGB(color, desaturate(color, 1), saturationStep));
    beep = true;
  }

  // Toggle random color cycling.
  if (e.is("3d:lhand-trigger-down") && !player) randomPalette = true;
  if (e.is("3d:lhand-trigger-up") && !player) randomPalette = false;

  if (e.alt && e.is("keyboard:down:]"))
    advanceSeries(store["freaky-flowers"], 1);
  if (e.alt && e.is("keyboard:down:["))
    advanceSeries(store["freaky-flowers"], -1);

  // Save scene data as a GLB.
  if (e.is("keyboard:down:enter") && !player) {
    const ts = params[0] || timestamp();
    const handle = "digitpain"; // Hardcoded for now.
    const bg = rgbToHexStr(...background.slice(0, 3)).toUpperCase(); // Empty string for no `#` prefix.
    const sculptureSlug = `${ts}-sculpture-${bg}-${handle}`;

    gpu
      .message({
        type: "export-scene",
        content: {
          slug: sculptureSlug,
          output: "local",
          sculptureHeight: cubeHeight,
        },
      })
      .then((data) => {
        console.log("🪄 Sculpture downloaded:", `${sculptureSlug}.glb`, data);
        ping = true;
        addFlash([0, 255, 0, 255]);
      })
      .catch((err) => {
        console.error("🪄 Sculpture download failed:", err);
        pong = true;
        addFlash([255, 0, 0, 255]);
      });
  }

  if (e.is("3d:rhand-button-b-down") && !player) {
    advancePalette("blackredyellow");
    beep = true;
  }

  // Save an already loaded demo back to disk.
  if (e.alt && e.is("keyboard:down:m")) {
    const ts = lastPlayedFrames[0][2]; // 0n, "piece:info", timestamp, author
    const handle = lastPlayedFrames[0][3]; // 0n, "piece:info", timestamp, author

    console.log("🎨 Adjusting colors!");
    lastPlayedFrames.forEach((f, i) => {
      if (f[1].endsWith("color")) {
        let c = f.slice(2, 5).map((v) => v / 255);
        let nc = c;
        // Convert everything to sRGB.
        nc = [LinearToSRGB(nc[0]), LinearToSRGB(nc[1]), LinearToSRGB(nc[2])];

        // And bump down things that aren't black...
        if ((nc[0] + nc[1] + nc[2]) / 3 > 0.08) {
          nc = shiftRGB(nc, c, 0.2, "lerp", 1);
        }

        f[2] = round(nc[0] * 255);
        f[3] = round(nc[1] * 255);
        f[4] = round(nc[2] * 255);
      }
    });

    const slug = `ff${store["freaky-flowers"].tokenID}-${ts}-recording-${handle}.json`;
    download(slug, lastPlayedFrames); // Save modified demo to json.
  }

  // 🛑 Finish a piece.
  if (e.is("3d:rhand-button-thumb-down") && !player) {
    // Don't save empty pieces!
    if (
      tube.form.vertices.length === 0 &&
      tube.lineForm.vertices.length === 0
    ) {
      pong = true;
      addFlash([100, 0, 0, 255]);
      console.log("🪄 No piece to save!");
      return;
    }

    const ts = timestamp();

    console.log(
      `%c🪄 Piece completed: ${ts}`,
      `background-color: rgb(32, 32, 32);
     color: rgb(255, 255, 0);
     padding: 0 0.25em;
     border-left: 0.75px solid rgb(255, 255, 0);
     border-right: 0.75px solid rgb(255, 255, 0);`,
    );

    bop = true;

    // TODO: I probably shouldn't dump demos and instead wait until things
    //       finish uploading in case they fail. 22.11.15.08.52
    //       (In my studio for now they probably won't fail...)

    /*
    // Attempt to upload the piece to the server...
    const handle = "digitpain"; // Hardcoded for now.

    const bg = rgbToHexStr(...background.slice(0, 3)).toUpperCase(); // Empty string for no `#` prefix.
    const recordingSlug = `${ts}-recording-${handle}`; // Backgrounds are already encoded in the file.
    const sculptureSlug = `${ts}-sculpture-${bg}-${handle}`;

    demo?.rec("room:color", background); // Always write a final room color,
    //                                      so we know where to cut off a demo
    //                                      in case the artist sets it.

    // Prefix a metadata frame to the demo.
    demo.frames.unshift([0, "piece:info", ts, handle]);

    // Server saving.
    const saveMode = "server"; // The default for now. 22.11.15.05.32
    if (saveMode === "server") {
      // Save demo JSON.
      serverUpload(`${recordingSlug}.json`, demo.frames, () => (), bucket)
        .then((data) => {
          // console.log("JSON Upload success:", data);
          console.log(
            "🪄 Demo uploaded:",
            `${baseURL}/${recordingSlug}.json`,
            data
          );

          ping = true;
          addFlash([255, 255, 0, 255]);
        })
        .catch((err) => {
          console.error("🪄 Demo upload failed:", err);

          pong = true;
          addFlash([255, 0, 0, 255]);
        });

      // Save scene GLTF.
      gpu
        .message({
          type: "export-scene",
          content: {
            slug: sculptureSlug,
            output: "server",
            handle,
            bucket,
            sculptureHeight: cubeHeight,
          },
        })
        .then((data) => {
          console.log(
            "🪄 Sculpture uploaded:",
            `${baseURL}/${sculptureSlug}.glb`,
            data
          );

          ping = true;
          addFlash([0, 255, 0, 255]);
        })
        .catch((err) => {
          console.error("🪄 Sculpture upload failed:", err);

          pong = true;
          addFlash([255, 0, 0, 255]);
        });
    } else {
      // Local saving. (Assume "local")
      download(`${ts}-recording-${handle}.json`, demo.frames); // Save demo to json.
      // gpu.message({ type: "export-scene", content: { timestamp: ts } }); // Save scene to json.
    }
    */

    demo?.dump(); // Start fresh / clear any existing demo cache.

    // Remove all vertices from the existing tube and reset tube state.
    tube.form.clear();
    tube.capForm.clear();
    tube.triCapForm.clear();
    tube.lineForm.clear();
    tube.lastPathP = undefined;
    tube.gesture = [];

    demo?.rec("room:color", background); // Record the starting bg color in case the default ever changes.
    demo?.rec("wand:color", color);

    demo?.rec("tube:sides", tube.sides);
    demo?.rec("tube:radius", tube.radius); // Grab the state of the current tube.
    demo?.rec("tube:step", tube.step);

    addFlash([255, 0, 0, 255]);

    console.log("🪄 A new piece...");
  }

  // 📥 Load a local wand demo file instantly with `l` or in realtime with `p`.
  if (e.is("keyboard:down:p") || e.is("keyboard:down:l")) {
    sideload(".json")
      .then((data) => {
        const frames = parseDemoFrames(data);
        console.log("🎞️ Loaded a wand file:", frames);

        // (L)oad or (P)lay back the demo file.
        player = new Player(
          frames,
          e.is("keyboard:down:l") ? 0 : 1, // If the "l" key is pressed then load all the demo frames instantly.
        );
      })
      .catch((err) => {
        console.error("JSON load error:", err);
      });
  }

  //#region 🐛 Debugging controls.
  // Adjust tube complexity.
  if (e.is("keyboard:down:k") && !player) {
    sides += 1;
    limiter = 0;
  }

  if (e.is("keyboard:down:j") && !player) {
    sides = max(1, sides - 1);
    limiter = 0;
  }

  if (e.is("wheel") && e.dir > 0 && tube && !player) {
    limiter += 1;
    tube.form.limiter = limiter;
  }

  if (e.is("wheel") && e.dir < 0 && tube && !player) {
    limiter -= 1;
    if (limiter < 0) limiter = tube.form.vertices.length / 2;
    tube.form.limiter = limiter;
  }

  // Wipe ambient pixels. Kinda slow... 23.07.15.18.07
  if (e.is("move") || e.is("draw")) {
    needsWipe = [0, 0];
  }

  // In case we need a wireframe form too.
  // if (e.is("keyboard:down:[")) tubeWire.form.limiter = limiter2;

  // if (e.is("keyboard:down:]")) {
  //   if (limiter2 < 0) limiter2 = tubeWire.form.vertices.length / 2;
  //   tubeWire.form.limiter = limiter2;
  // }
  //#endregion
}

// 💗 Beat
function beat({ num, sound: { bpm, synth } }) {
  if (beatCount === 0n) {
    bap = bip = beep = bop = ping = pong = false; // Clear any existing signals.
    bpm(1800); // Set bpm to 1800 ~ 30fps }
  }
  beatCount += 1n; // TODO: This should go into the main API. 22.11.01.17.43

  if (bap) {
    synth({
      tone: num.randIntRange(100, 800),
      beats: 1.5,
      attack: 0.02,
      decay: 0.97,
      volume: 0.35,
    });
    bap = false;
  }

  if (bip) {
    synth({
      tone: num.randIntRange(50, 1600),
      beats: 1,
      attack: 0.02,
      decay: 0.97,
      volume: 0.1,
    });
    bip = false;
  }

  if (beep) {
    synth({
      tone: 300,
      beats: 0.7,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
    beep = false;
  }

  if (bop) {
    synth({
      tone: 600,
      beats: 0.7,
      attack: 0.01,
      decay: 0.5,
      volume: 0.15,
    });
    bop = false;
  }

  if (ping) {
    synth({
      tone: 1000,
      beats: 1.0,
      attack: 0.01,
      decay: 0.9,
      volume: 0.25,
    });
    ping = false;
  }

  if (pong) {
    synth({
      tone: 100,
      beats: 1.5,
      attack: 0.01,
      decay: 0.5,
      volume: 0.35,
    });
    pong = false;
  }
}

function leave() {
  player = null;
}

export { boot, paint, sim, act, leave, beat };

// #region 📑 library

function advanceSeries(series, dir, stop) {
  if (series) {
    let id = series.tokenID;
    id = (id + dir) % series.tokens.length;
    if (id < 0) id = series.tokens.length + id;
    if (id === 0 && stop) return; // Stop on every ending.
    series.tokenID = id;
    const prefixedTimestamp = "ff" + id + "-" + series.tokens[id];
    if (debug) console.log("⌛ Loading token:", id, prefixedTimestamp);

    function queueDemo(speed = 1) {
      const handle = "digitpain";
      const recordingSlug = `${prefixedTimestamp}-recording-${handle}`;
      loadDemoSwitch = { slug: `${baseURL}/${recordingSlug}.json`, speed };
    }
    queueDemo(0);
  }
}

function addFlash(color) {
  if (cachedBackground == undefined) cachedBackground = background;
  flashes.push(color);
}

function parseDemoFrames(data) {
  return JSON.parse(data).map((f) => {
    // TODO: This may not cover embedded array types.
    f.forEach((v, i) => {
      if (i === 0) {
        f[i] = BigInt(f[i]);
      } else if (f[i] === true) {
        f[i] = true;
      } else if (f[i] === false) {
        f[i] = false;
      } else if (typeof f[i] === "string" && !isNaN(f[i])) {
        f[i] = parseFloat(f[i]);
      }
    });
    return f;
  });
}

// Color helpers.
const almostBlack = () => [rr(1, 10), rr(1, 10), rr(1, 10), 255];
const almostWhite = () => [rr(245, 255), rr(245, 255), rr(245, 255), 255];

// Adds or subtracts up to 10 from any color, clamping from 0->255.
const barely = (color) => {
  return [
    clamp(color[0] + rr(-5, 5), 0, 255),
    clamp(color[1] + rr(-5, 5), 0, 255),
    clamp(color[2] + rr(-5, 5), 0, 255),
    255,
  ];
};

// Process both foreground and background, ensuring that they have changed.
function processNewColor(fg, bg) {
  if (
    fg[0] !== color[0] ||
    fg[1] !== color[1] ||
    fg[2] !== color[2] ||
    fg[3] !== color[3]
  ) {
    // Process foreground.
    color = fg;
    capColor = fg;
    if (spi) spi.color = fg;

    tubeVary = 0; // These could eventually be used for more procedural coloring.
    capVary = 0;

    if (!player) demo?.rec("wand:color", color);
  }

  if (bg === undefined) bg = background;

  if (
    bg[0] !== background[0] ||
    bg[1] !== background[1] ||
    bg[2] !== background[2] ||
    bg[3] !== background[3]
  ) {
    background = bg;
    if (!player) demo?.rec("room:color", bg);
  }

  /*
  console.log(
    `%cForeground`,
    `background-color: black;
     color: rgb(${fg[0]}, ${fg[1]}, ${fg[2]});
     padding: 0 0.25em;
     border-left: 0.75px solid rgb(60, 60, 60);
     border-right: 0.75px solid rgb(60, 60, 60);`,
    fg
  );
  console.log(
    `%cBackground`,
    `background-color: black;
     color: rgb(${bg[0]}, ${bg[1]}, ${bg[2]});
     padding: 0 0.25em;
     border-left: 0.75px solid rgb(60, 60, 60);
     border-right: 0.75px solid rgb(60, 60, 60);`,
    bg
  );
  */
}

function advancePalette(pal) {
  let index = palettes.indices[pal]; // Get palette and index.
  const palette = palettes[pal];

  // Reset any palette index that isn't this one...
  // 🧠 This way I can memorize counts / taps to get to mapped colors. 22.11.18.22.28
  Object.keys(palettes.indices).forEach((key) => {
    if (key !== pal) palettes.indices[key] = 0;
  });

  // Grab our colors as constants or as dynamically generated values.
  let fg, bg;
  fg = palette[index].fg; // Foreground
  bg = palette[index].bg; // Background: could be empty.
  if (typeof fg === "function") fg = fg(); // Compute fg` if it's a function...
  if (typeof bg === "function") bg = bg(); // and same with `bg`.

  processNewColor(fg, bg);

  index = (index + 1) % palette.length; // Advance and wrap palette index.
  palettes.indices[pal] = index; // Update the global index.
}

// Here we build a path out of points, which draws
// tubular segments by adding them to a geometric form.
// It does this by producing a cookie cutter shape that gets
// extruded in a transformed direction according to the path data.
class Tube {
  $; // api
  shape; // This hold the vertices in our cookie cutter shape.
  lastSegmentShape; // Keep track of this for rendering a nice preview.
  gesture = []; // Set up points on a path / gesture. (Resets on each gesture.)
  //               Used for triangulation logic.
  lastPathP; // Keep track of the most recent "path point".
  sides; // Number of sides the tube has. (See the top of this file.)
  step; // Number of steps / segment length.
  radius; // Thickness of the tube.
  form; // Represents the tube form that gets sent to the GPU or rasterizer.
  lineForm; // Represents a single infinitely thin line. (for a side of 1)
  capForm; // " a cursor that presents the cap of the tube.
  triCapForm; // " a tri only cursor that presents just a cap of the tube.
  geometry = "triangles"; // or "lines"
  previewRotation; // Keeps track of the last rotation sent to `preview()`.
  triColor; // Stores the current triangle color.
  varyTriCount = 0; // Counts by 3 to color triangle vertices.
  lineColor; // Stores the current triangle color.
  varyLineCount = 0; // Counts by 2 to color line segment vertices.
  // TODO: ^ Some of these fields could still be privated. 22.11.11.15.50
  demo; // Points to a demo that is being recorded to in start, goto, and stop.

  mat4Ident;

  verticesPerCap; // Used to calculate progress.
  verticesPerSide;

  // ☁️
  // Note: I could eventually add behavioral data into these vertices that
  //       animate things / turn on or off certain low level effects etc.

  constructor(
    $,
    radius,
    sides,
    step,
    geometry,
    demo,
    isWand = false,
    cursorGeometry = "line:buffered",
  ) {
    this.$ = $; // Hold onto the API.
    this.geometry = geometry; // Set the geometry type.
    this.radius = radius;
    this.sides = sides;
    this.step = step;
    this.shape = this.#segmentShape(radius, sides); // Set shape to start.
    this.lastSegmentShape = this.shape;
    this.demo = demo;

    demo?.rec("tube:sides", this.sides);
    demo?.rec("tube:radius", this.radius);
    demo?.rec("tube:step", this.step);

    // Make the buffered geometry form, given the geometry type.,
    // and another to represent a dynamic cursor.

    const formType = [
      {
        type:
          this.geometry === "triangles" ? "triangle:buffered" : "line:buffered",
        keep: true,
        // Toggle this for testing full form updates.
        // this.geometry === "triangles" ? "triangle" : "line",
        // keep: false,
        gradients: false,
      },
      { color: [255, 255, 255, 255] }, // If vertices are passed then this number blends down.
      { scale: [1, 1, 1] },
    ];

    this.form = new $.Form(...formType); // Main form.
    this.form.tag = "sculpture"; // This tells the GPU what to export right now. 22.11.15.09.05

    this.mat4Ident = $.num.mat4.create();

    formType[0].type = "line:buffered"; // Change the formType.
    this.lineForm = new $.Form(...formType); // Single line form. (Sides of 1)
    this.lineForm.tag = "sculpture-line"; // This tells the GPU what to export right now. 22.11.15.09.05

    formType[0].type = cursorGeometry; // Change the formType.
    this.capForm = new $.Form(...formType); // Cursor.

    formType[0].type = "triangle:buffered";
    this.triCapForm = new $.Form(...formType); // Tri cursor.

    this.#setVertexLimits();

    if (isWand === false) {
      this.form.MAX_POINTS = 301000; // Demos can't record beyond this alotted geometry. 22.11.25.11.44
      this.lineForm.MAX_POINTS = 101000;
    } else {
      this.form.MAX_POINTS = 3000; // Give wands a smaller range.
      this.lineForm.MAX_POINTS = 1000;
    }

    this.triCapForm.MAX_POINTS = 512; // TODO: Sometimes this maxes out on demo playback?
    this.capForm.MAX_POINTS = 8192; //extra + verticesPerSide + verticesPerCap * 2; // sides * 6 + 3 * 2; // This should be enough to cover our bases. The 4 is for 2 caps and a shaft.
  }

  playbackPreview(lastP, p) {
    this.capForm.clear();
    this.triCapForm.clear();

    // Cap the wand tip with a wireframe.
    const pathP = this.#transformShape(this.#pathp({ ...p }));
    this.#cap(pathP, this.capForm);

    // Cap the end of the tube with a triangle using the cached shape.
    const cachedSegmentShape = this.shape;
    this.shape = this.lastSegmentShape;
    const lpp = this.#transformShape(this.#pathp({ ...lastP }));
    this.#cap(lpp, this.triCapForm, false);
    this.shape = cachedSegmentShape;

    // Draw the preview segment.
    const cachedLastPathP = this.lastPathP;
    const cachedGesture = this.gesture;
    this.lastPathP = lpp;
    this.gesture = [];
    this.goto(p, this.capForm, true, false); // No preview, no demo.
    this.gesture = cachedGesture;
    this.lastPathP = cachedLastPathP;
  }

  // Produces the `capForm` cursor.
  preview(p, nextPathP) {
    // Don't show anything at a very tiny distance.
    if (nextPathP) {
      const d = this.$.num.dist3d(p.position, nextPathP.position);
      if (d < 0.0005) return; // TODO: Eventually make this preview transition smoother.
    }

    // Replace the current capForm shape values with transformed ones.
    this.previewRotation = p.rotation; // Remember preview rotation for segment cut off end bits.
    this.capForm.clear();
    this.triCapForm.clear();

    const pathP = this.#transformShape(this.#pathp({ ...p }));

    // TODO: This wouldn't affect the demo playback... 23.02.04.21.11
    if (waving && this.gesture.length === 0) this.#cap(pathP, this.capForm);
    if (!waving) this.#cap(pathP, this.capForm);

    if (nextPathP) {
      // Use the former segment's properties to transition to this one
      // if we've already drawn a segment.
      if (this.gesture.length > 0) {
        const npp = this.#transformShape(this.#pathp({ ...nextPathP }));
        this.#cap(npp, this.capForm, false);

        const cachedSegmentShape = this.shape;
        this.shape = this.lastSegmentShape;

        const tPathP = this.#transformShape(this.#pathp({ ...p }));
        this.#cap(tPathP, this.triCapForm, false);

        this.shape = cachedSegmentShape;
      } else {
        const npp = this.#transformShape(this.#pathp({ ...nextPathP }));
        this.#cap(npp, this.capForm, false);
      }
    }

    // Also move towards the next possible position here...
    if (nextPathP) {
      // Cache some state that goto writes to, load it back.
      const cachedLastPathP = this.lastPathP;
      const cachedGesture = this.gesture;

      // Alter the lastPathP shape. (This kind of shows this class structure is a little less flexible than I hoped.) 😱
      const cachedSegmentShape = this.shape;
      if (this.gesture.length > 0) {
        this.shape = this.lastSegmentShape;
      }
      this.#transformShape(pathP);
      this.shape = cachedSegmentShape;

      this.lastPathP = pathP;
      this.gesture = [];

      this.goto(nextPathP, this.capForm, true, false); // No preview, no demo.

      this.lastPathP = cachedLastPathP;
      this.gesture = cachedGesture;
    }
  }

  // Creates an initial position, orientation and end cap geometry.
  start(
    p,
    radius = this.radius,
    sides = this.sides,
    step = this.step,
    fromDemo = false,
  ) {
    this.sides = sides;
    this.radius = radius;
    this.step = step;

    // Create an initial position in the path and generate points in the shape.
    const start = this.#pathp(p);
    this.lastPathP = start; // Store an inital lastPath.

    // Transform the first shape and add an end cap to the form.
    this.#transformShape(start);
    if (this.sides > 1) this.#cap(start, this.form);

    // 🗒️ Note: Eventually this should be on the level of abstraction of a Wand, not a tool like Tube. 22.11.14.23.20
    if (start.color.length === 3) start.color.push(255); // Use RGBA for demo.

    if (fromDemo === false) {
      const demoPos = start.pos.slice(0, 3);
      demoPos[1] -= cubeHeight;
      this.demo?.rec("tube:start", [...demoPos, ...start.rotation]);
    }
  }

  // Adds additonal points as args in [position, rotation, color] format.
  goto(pathPoint, form, fromDemo = false, showCapForm = false) {
    // Add new points to the path.
    // Extrude shape points from and in the direction of each path vertex.
    const pathp = this.#pathp(pathPoint);
    if (!this.lastPathP) this.lastPathP = pathp; // In case it doesn't exist...

    this.#consumePath([this.#transformShape(pathp)], form);

    if (showCapForm) {
      this.triCapForm.clear();
      this.#cap(pathp, this.triCapForm, false);
    }

    // Don't update the last segment shape if we are only previewing.
    if (form !== this.capForm) this.lastSegmentShape = this.shape;

    if (pathp.color.length === 3) pathp.color.push(255); // Use RGBA for demo.

    if (!fromDemo) {
      const demoPos = pathp.pos.slice(0, 3);
      demoPos[1] -= cubeHeight;
      this.demo?.rec("tube:goto", [...demoPos, ...pathp.rotation]);
    }

    if (this.progress() >= 1) {
      if (!fromDemo) waving = false;
      this.stop(fromDemo);
    }
  }

  // Ends a stroke / lift a gesture.
  stop(fromDemo = false) {
    const { dist3d } = this.$.num;

    this.capForm.clear();
    this.triCapForm.clear();

    // Push anything we haven't stepped into onto the path.
    // (Optionally add start or end bits, drawn over or under our step size.)
    if (!fromDemo && graphPreview) {
      const d = dist3d(spi.state.position, race.pos);
      if (d > 0.01) {
        if (this.gesture.length === 0) {
          const alteredState = { ...spi.state };
          alteredState.rotation = this.previewRotation;
          this.start(alteredState, radius, sides);
        }

        spi.crawlTowards(race.pos, d, 1);

        const alteredState = { ...spi.state };
        if (this.gesture.length === 0) {
          alteredState.rotation = this.previewRotation;
        }
        this.goto(alteredState);
      }
    }

    if (this.lastPathP) this.#cap(this.lastPathP, this.form, false);
    //                                                      `false` for no ring

    if (fromDemo === false && this.gesture.length > 0)
      this.demo?.rec("tube:stop");

    this.gesture.length = 0;
    this.lastPathP = undefined;
  }

  progress() {
    // Return the number of "safe" full strokes (segments and caps) leftover,
    // depending on the number of sides.
    this.#setVertexLimits();

    let form = this.sides !== 1 ? this.form : this.lineForm;

    return min(
      1,
      form.vertices.length /
        (form.MAX_POINTS -
          (this.verticesPerSide * this.sides + this.verticesPerCap * 2)),
    );
  }

  // Clears out all the geometry from this Tube.
  clear() {
    this.form.clear();
    this.capForm.clear();
    this.triCapForm.clear();
    this.lineForm.clear();
    this.lastPathP = undefined;
    this.gesture = [];
  }

  update({ sides, radius, step }) {
    this.step = step || this.step;
    if (sides == undefined) {
      this.radius = radius || this.radius;
      this.shape = this.#segmentShape(this.radius, this.sides); // Set shape to start.
    } else {
      if (this.gesture.length > 0) this.stop();
      this.sides = sides || this.sides;
      this.radius = radius || this.radius;
      this.#setVertexLimits();
      this.shape = this.#segmentShape(this.radius, this.sides); // Set shape to start.
      waving = false;
    }
  }

  #setVertexLimits() {
    if (this.form.primitive === "line") {
      this.verticesPerSide = 6;
      this.verticesPerCap = this.sides * 3;
    }

    if (this.form.primitive === "triangle") {
      // Just a line here.
      if (this.sides === 1) {
        this.verticesPerSide = 2; // Double sided.
        this.verticesPerCap = 0; // No caps here.
      }

      if (this.sides === 2) {
        this.verticesPerSide = 6; // Double sided.
        this.verticesPerCap = 0; // No caps here.
      }

      if (this.sides === 3) {
        this.verticesPerSide = 6;
        this.verticesPerCap = 3; // No caps here.
      }

      if (this.sides === 4) {
        this.verticesPerSide = 12;
        this.verticesPerCap = 3; // No caps here.
      }

      if (this.sides > 4) {
        this.verticesPerSide = 6 * this.sides;
        this.verticesPerCap = 3 * this.sides; // No caps here.
      }
    }
  }

  // Takes a starting position, direction and length.
  // Projects out a center core along with from there...
  // Produces a circle yet at low complexity makes
  // - lines, planes, prisms, and boxes.
  #segmentShape(radius, sides) {
    const positions = [];

    // 🅰️ Define a core line for this segment shape, based around the z axis.
    positions.push([0, 0, 0, 1]);

    if (sides === 1) return positions; // No need to generate anything if we are using a line.

    // 🅱️ Circumnavigate points around the center core..
    const PI2 = Math.PI * 2;
    for (var i = 0; i < sides; i += 1) {
      const angle = (i / sides) * PI2;
      positions.push([sin(angle) * radius, cos(angle) * radius, 0, 1]);
    }

    return positions;
  }

  // Generate a path point with room for shape positions.
  // A spatial snapshot of state used around as `spider.state`.
  // 😱 (This method is dumb, it only adds a shape array. 22.11.12.01.28)
  #pathp({ position, direction, rotation, color, shape = [] }) {
    if (position.length === 3) position = [...position, 1];
    return { pos: position, direction, rotation, color, shape };
  }

  // Generate a start or end (where ring === false) cap to the tube.
  // Has a form input that is either `form` or `capForm`.
  #cap(pathP, form, ring = true) {
    const { vec3 } = this.$.num;
    const positions = [];
    const colors = [];
    const normals = [];

    let tris;
    if (form === undefined) {
      tris = this.geometry === "triangles";
    } else {
      tris = form.primitive !== "line";
    }

    const shade = pathP.color;

    // 📐 Triangles
    if (tris) {
      // 3️⃣ Three Sides
      if (this.sides === 3) {
        // Start cap.
        if (ring) {
          positions.push(pathP.shape[1], pathP.shape[2], pathP.shape[3]);
          colors.push(
            this.varyCap(shade),
            this.varyCap(shade),
            this.varyCap(shade),
          );
        } else {
          // End cap.
          positions.push(pathP.shape[2], pathP.shape[1], pathP.shape[3]);
          colors.push(
            this.varyCap(shade),
            this.varyCap(shade),
            this.varyCap(shade),
          );
        }
      }

      // 4️⃣ Four sides.
      if (this.sides === 4) {
        if (!ring) {
          positions.push(
            pathP.shape[2],
            pathP.shape[1],
            pathP.shape[3],
            pathP.shape[4],
            pathP.shape[3],
            pathP.shape[1],
          );
        } else {
          positions.push(
            pathP.shape[1],
            pathP.shape[2],
            pathP.shape[3],
            pathP.shape[3],
            pathP.shape[4],
            pathP.shape[1],
          );
        }

        colors.push(
          this.varyCap(shade),
          this.varyCap(shade),
          this.varyCap(shade),
          this.varyCap(shade),
          this.varyCap(shade),
          this.varyCap(shade),
        );
      }

      // This is a general case now.
      if (this.sides >= 5) {
        const center = pathP.shape[0]; // Hold onto center point.

        // Radiate around each point, plotting a triangle towards the center,
        // between this and the next point, skipping the last.
        for (let i = 1; i < pathP.shape.length - 1; i += 1) {
          if (!ring) {
            // Check the rotation here...
            positions.push(pathP.shape[i], center, pathP.shape[i + 1]);
            colors.push(
              this.varyCap(shade), // Inner color for a gradient.
              this.varyCap(shade),
              this.varyCap(shade),
            );
          } else {
            positions.push(center, pathP.shape[i], pathP.shape[i + 1]);
            colors.push(
              this.varyCap(shade), // Inner color for a gradient.
              this.varyCap(shade),
              this.varyCap(shade),
            );
          }
        }

        if (!ring) {
          // And wrap around to the beginning for the final point.
          positions.push(
            center,
            pathP.shape[1],
            pathP.shape[pathP.shape.length - 1],
          );
          colors.push(
            this.varyCap(shade), // Inner color for a gradient.
            this.varyCap(shade),
            this.varyCap(shade),
          );
        } else {
          positions.push(
            pathP.shape[1],
            center,
            pathP.shape[pathP.shape.length - 1],
          );
          colors.push(
            this.varyCap(shade), // Inner color for a gradient.
            this.varyCap(shade),
            this.varyCap(shade),
          );
        }
      }
    } else {
      // 📈 Lines (TODO: This branch could be simplified and broken down)

      // TODO: I might be overdrawing a few lines here... 22.11.16.04.36

      if (this.sides === 3) {
        positions.push(
          pathP.shape[1],
          pathP.shape[2],
          pathP.shape[2],
          pathP.shape[3],
          pathP.shape[3],
          pathP.shape[1],
        );
        colors.push(
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
        );
      }

      if (this.sides === 4) {
        positions.push(
          pathP.shape[1],
          pathP.shape[2],
          pathP.shape[2],
          pathP.shape[3],
          pathP.shape[3],
          pathP.shape[4],
          pathP.shape[4],
          pathP.shape[1],
          pathP.shape[1],
          pathP.shape[3],
        );
        colors.push(
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
          this.varyCapLine(shade),
        );
      }

      if (this.sides > 4) {
        for (let i = 0; i < pathP.shape.length; i += 1) {
          // Pie: Radiate out from core point
          if (i > 0 && this.sides > 4) {
            positions.push(pathP.shape[0], pathP.shape[i]);
            colors.push(this.varyCapLine(shade), this.varyCapLine(shade));
          }

          // Single diagonal for a quad.
          if (i === 0 && this.sides === 4) {
            positions.push(pathP.shape[1], pathP.shape[3]);
            colors.push(this.varyCapLine(shade), this.varyCapLine(shade));
          }

          // Ring: Skip core point
          if (i > 1 && ring) {
            positions.push(pathP.shape[i - 1], pathP.shape[i]);
            colors.push(this.varyCapLine(shade), this.varyCapLine(shade));
          }
        }
      }

      // Ring: add final point
      if ((this.sides > 4 && ring) || this.sides === 2) {
        positions.push(pathP.shape[1], pathP.shape[pathP.shape.length - 1]);
        colors.push(this.varyCapLine(shade), this.varyCapLine(shade));
      }
    }

    // Generate normals for each triangle in the cap.
    // Note: Technically every normal should be the same for every cap so I only
    //       need to make one from the first triangle and copy it over.
    if (form.primitive === "triangle" && positions.length >= 3) {
      const A = positions[0];
      const B = positions[1];
      const C = positions[2];

      const AB = vec3.sub(vec3.create(), B, A);
      const AC = vec3.sub(vec3.create(), C, A);

      const normal = vec3.cross(vec3.create(), AB, AC);

      for (let i = 0; i < positions.length; i += 1) normals.push(normal);
    }

    if (positions.length > 0) form.addPoints({ positions, colors, normals });
  }

  // Transform the cookie-cutter by the pathP, returning the pathP back.
  #transformShape(pathP) {
    const { quat, mat4, vec4, vec3, radians } = this.$.num;

    const rm = mat4.fromRotationTranslationScaleOrigin(
      this.mat4Ident,
      pathP.rotation,
      pathP.pos,
      [1, 1, 1],
      [0, 0, 0],
    );

    this.shape.forEach((shapePos, i) => {
      const newShapePos = vec4.transformMat4(
        vec4.create(),
        [...shapePos, 1],
        rm,
      );
      pathP.shape[i] = newShapePos;
    });
    return pathP;
  }

  #driftValue(n, amt) {
    return this.$.num.clamp(n + this.$.num.randIntRange(-amt, amt), 0, 255);
  }

  // Shade input colors in `consumePath` and `#cap` for triangles and lines.

  // This method is for triangles and keeps a counter to only
  // change after every 3 vertices.
  vary(color) {
    if (this.varyTriCount === 0) {
      const newValues = [
        this.#driftValue(color[0], tubeVary),
        this.#driftValue(color[1], tubeVary),
        this.#driftValue(color[2], tubeVary),
      ];

      if (this.triColor && tubeVary > 0) {
        const differences = [
          this.triColor[0] - newValues[0],
          this.triColor[1] - newValues[1],
          this.triColor[2] - newValues[2],
        ];

        const averageDifference = abs(differences.reduce((p, c) => p + c) / 3);
        // Recurse if the average of all three color channels is still
        // inside the vary range.
        if (averageDifference < tubeVary / 5) {
          return this.vary(color);
        }

        this.triColor[0] = this.#driftValue(color[0], tubeVary);
        this.triColor[1] = this.#driftValue(color[1], tubeVary);
        this.triColor[2] = this.#driftValue(color[2], tubeVary);
      } else {
        this.triColor = color.slice();
      }
      // Don't do anything with the alpha.
    }
    this.varyTriCount = (this.varyTriCount + 1) % 3;
    color = this.triColor.slice();
    return color;
  }
  // and also the input colors in `#cap`.
  varyCap(color) {
    if (this.varyTriCount === 0) {
      if (!this.triColor) this.triColor = [];
      if (capVary > 0) {
        this.triColor[0] = this.#driftValue(color[0], capVary);
        this.triColor[1] = this.#driftValue(color[1], capVary);
        this.triColor[2] = this.#driftValue(color[2], capVary);
      } else {
        this.triColor = color.slice();
      }
      // Don't do anything with the alpha.
    }
    this.varyTriCount = (this.varyTriCount + 1) % 3;
    color = this.triColor.slice();
    return color;
  }

  // And these are for lines, and keep a counter for every 2 vertices.
  varyLine(color) {
    return color;
  }

  varyCapLine(color) {
    return color;
  }

  // Copy each point in the shape, transforming it by the added path positions
  // and angles to `positions` and `colors` which can get added to the `form`.
  #consumePath(pathPoints, form) {
    const { vec3 } = this.$.num;
    const positions = [];
    const colors = [];
    const normals = [];

    let tris;
    if (form === undefined) {
      tris = this.geometry === "triangles";
    } else {
      tris = form.primitive !== "line";
    }

    // console.log(this.sides, tris, form === this.lineForm, form);

    if (this.sides === 1 && form === undefined) {
      form = this.lineForm;
      tris = false;
    }

    const args = pathPoints;

    for (let pi = 0; pi < args.length; pi += 1) {
      const pathP = args[pi];
      const shade = pathP.color;
      this.gesture.push(pathP);

      // console.log("Number of sections so far:", this.gesture.length);

      // Get normals for each vertex in the triangle based on the
      // direction from the center to the triangle face.
      function calculateTriangleNormal(a, b, c) {
        const e1 = vec3.subtract([], b, a); // The two edges of the triangle.
        const e2 = vec3.subtract([], c, a);
        return vec3.normalize([], vec3.cross([], e1, e2)); // Cross product normal.
      }

      function norm(core, edge) {
        // Get the normal from the rotatedPoint to the core.
        return vec3.normalize([], vec3.sub([], edge, core));
      }

      // ⚠️
      // This is a complicated loop that generates triangulated vertices
      // or line segment vertices and sets all their colors for a given
      // parallel tube section.
      for (let si = 0; si < pathP.shape.length; si += 1) {
        if (!tris && si === 0) {
          // 1. 📉 Line: Core / center path.
          positions.push(this.lastPathP.shape[si], pathP.shape[si]);
          colors.push(this.varyLine(shade), this.varyLine(shade));
          // console.log(positions, form === this.lineForm);
        }

        if (this.sides === 1) {
          break;
        }

        // 2. Vertical
        if (si > 0) {
          // 📐
          if (tris) {
            // Two Sides
            if (this.sides === 2) {
              if (si === 1) {
                if (!unreal) {
                  positions.push(pathP.shape[si + 1]);
                  positions.push(this.lastPathP.shape[si + 1]);
                  positions.push(this.lastPathP.shape[si]);

                  const n = calculateTriangleNormal(...positions.slice(-3));
                  // vec3.negate(n, n);
                  // const n = [0, -1, 0];
                  normals.push(n, n, n);
                  normals.push(n, n, n);
                  // }

                  colors.push(
                    this.vary(shade),
                    this.vary(shade),
                    this.vary(shade),
                  );
                }

                if (!unreal) {
                  positions.push(this.lastPathP.shape[si]);
                  positions.push(pathP.shape[si]);
                  positions.push(pathP.shape[si + 1]);

                  {
                    //  const n = calculateTriangleNormal(...positions.slice(-3));
                    // vec3.negate(n, n);
                    // const n = [0, -1, 0];
                    //normals.push(n, n, n);
                  }

                  colors.push(
                    this.vary(shade),
                    this.vary(shade),
                    this.vary(shade),
                  );
                }

                // Double up the sides here.
                positions.push(pathP.shape[si + 1]);
                positions.push(pathP.shape[si]);
                positions.push(this.lastPathP.shape[si]);

                const n2 = calculateTriangleNormal(...positions.slice(-3));
                //vec3.negate(n, n);
                // const n = [0, -1, 0];
                normals.push(n2, n2, n2);
                normals.push(n2, n2, n2);

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );

                positions.push(this.lastPathP.shape[si]);
                positions.push(this.lastPathP.shape[si + 1]);
                positions.push(pathP.shape[si + 1]);

                // const n = calculateTriangleNormal(...positions.slice(-3));
                // vec3.negate(n, n);
                // const n = [0, -1, 0];
                //normals.push(n2, n2, n2);

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              }
            }
            // Three Sides
            if (this.sides === 3) {
              if (si === 1) {
                positions.push(
                  pathP.shape[si],
                  this.lastPathP.shape[si + 1],
                  this.lastPathP.shape[si],
                );

                // The outward facing normal from pathP.shape[si] would be
                // the direction from pathP.shape[0] to pathP.shape[si]

                // The outward facing normal for each triangle would be
                // the direction from the center of each triangle to
                // the midpoint between pathP and lastPathP.

                // pathP.shape[si] would be
                // the direction from pathP.shape[0] to pathP.shape[si]

                // normals.push(
                //   ...norms(
                //     coreLine,
                //     triCenter(...positions.slice(-3)),
                //     positions.slice(-3)
                //   )
                // );
                normals.push(
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              }
            }
            if (this.sides >= 5) {
              if (si === 1) {
                positions.push(this.lastPathP.shape[si]);
                positions.push(pathP.shape[si]);
                positions.push(this.lastPathP.shape[si + 1]);

                normals.push(
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              } //else {
              //}
            }
          } else {
            // 📈 Lines
            positions.push(this.lastPathP.shape[si], pathP.shape[si]);
            colors.push(this.varyLine(shade), this.varyLine(shade));
          }
        }

        // 3. Across (We skip the first shape points here.)
        if (si > 1) {
          // 📐
          if (tris) {
            if (this.sides === 3) {
              positions.push(
                pathP.shape[si],
                this.lastPathP.shape[si],
                pathP.shape[si - 1],
              );

              // normals.push(
              //   ...norms(
              //     coreLine,
              //     triCenter(...positions.slice(-3)),
              //     positions.slice(-3)
              //   )
              // );

              normals.push(
                norm(pathP.shape[0], pathP.shape[si]),
                norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                norm(pathP.shape[0], pathP.shape[si - 1]),
              );

              colors.push(this.vary(shade), this.vary(shade), this.vary(shade));
            }
            if (this.sides === 4) {
              /*
              if (si === 2) {
                positions.push(pathP.shape[si - 1]);
                // colors.push(this.vary(shade));
                colors.push([0, 0, 255, 255]);
              }
              if (si === 3) {
                positions.push(pathP.shape[si - 1]);
                //colors.push(this.vary(shade));
                colors.push([255, 0, 0, 255]);
              }
              */
            }

            if (this.sides >= 5) {
              positions.push(
                pathP.shape[si],
                this.lastPathP.shape[si],
                pathP.shape[si - 1],
              );

              normals.push(
                norm(pathP.shape[0], pathP.shape[si]),
                norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                norm(pathP.shape[0], pathP.shape[si - 1]),
              );

              //              colors.push(this.vary(shade));
              colors.push(this.vary(shade), this.vary(shade), this.vary(shade));
            }
          } else {
            // 📈 Lines
            //if (!form) {
            // Only add across lines if we are not previewing with capForm. (Hacky)
            positions.push(pathP.shape[si], pathP.shape[si - 1]);
            colors.push(this.varyLine(shade), this.varyLine(shade));
            //}
          }
        }

        // 4. Diagonal
        if (si > 0 && si < pathP.shape.length - 1) {
          // 📐
          if (tris) {
            // 3️⃣
            // Three sided
            if (this.sides === 3) {
              if (si === 2) {
                positions.push(
                  this.lastPathP.shape[si],
                  pathP.shape[si],
                  this.lastPathP.shape[si + 1],
                );

                // normals.push(
                //   ...norms(
                //     coreLine,
                //     triCenter(...positions.slice(-3)),
                //     positions.slice(-3)
                //   )
                // );

                normals.push(
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              }
            }

            // 4️⃣
            // Four sided
            if (this.sides === 4) {
              if (si === 1) {
                // positions.push(this.lastPathP.shape[si + 1]);
                // colors.push(this.vary(shade));
                positions.push(
                  this.lastPathP.shape[si],
                  pathP.shape[si],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );

                positions.push(
                  pathP.shape[si],
                  pathP.shape[si + 1],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si + 1]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
                // colors.push(
                //   this.vary(shade),
                //   this.vary(shade),
                //   this.vary(shade)
                // );
              }

              if (si === 2) {
                positions.push(
                  this.lastPathP.shape[si],
                  pathP.shape[si],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );

                positions.push(
                  pathP.shape[si],
                  pathP.shape[si + 1],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si + 1]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              }

              if (si === 3) {
                positions.push(
                  this.lastPathP.shape[si],
                  pathP.shape[si],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  // [255, 0, 0, 255],
                  // [255, 0, 0, 255],
                  // [255, 0, 0, 255]
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );

                positions.push(
                  pathP.shape[si],
                  pathP.shape[si + 1],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si + 1]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              }
            }

            if (this.sides >= 5) {
              if (si > 1) {
                positions.push(
                  this.lastPathP.shape[si],
                  pathP.shape[si],
                  this.lastPathP.shape[si + 1],
                );

                normals.push(
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si]),
                  norm(pathP.shape[0], pathP.shape[si]),
                  norm(this.lastPathP.shape[0], this.lastPathP.shape[si + 1]),
                );

                colors.push(
                  this.vary(shade),
                  this.vary(shade),
                  this.vary(shade),
                );
              }
            }
          } else {
            // 📈 Lines
            positions.push(this.lastPathP.shape[si + 1], pathP.shape[si]);
            colors.push(this.varyLine(shade), this.varyLine(shade));
          }
        }
      }

      //  5. Final side / diagonal

      // 📐 Triangles
      if (tris) {
        if (this.sides === 3) {
          positions.push(
            pathP.shape[pathP.shape.length - 1],
            this.lastPathP.shape[1],
            this.lastPathP.shape[pathP.shape.length - 1],
          );

          normals.push(
            norm(pathP.shape[0], pathP.shape[pathP.shape.length - 1]),
            norm(this.lastPathP.shape[0], this.lastPathP.shape[1]),
            norm(
              this.lastPathP.shape[0],
              this.lastPathP.shape[pathP.shape.length - 1],
            ),
          );

          colors.push(this.vary(shade), this.vary(shade), this.vary(shade));
          //colors.push([255, 0, 0, 255], [255, 0, 0, 255], [255, 0, 0, 255]);

          positions.push(
            this.lastPathP.shape[1],
            pathP.shape[pathP.shape.length - 1],
            pathP.shape[1],
          );

          // normals.push(
          //   ...norms(
          //     coreLine,
          //     triCenter(...positions.slice(-3)),
          //     positions.slice(-3)
          //   )
          // );

          normals.push(
            norm(this.lastPathP.shape[0], this.lastPathP.shape[1]),
            norm(pathP.shape[0], pathP.shape[pathP.shape.length - 1]),
            norm(pathP.shape[0], pathP.shape[1]),
          );

          colors.push(this.vary(shade), this.vary(shade), this.vary(shade));
          //colors.push([255, 0, 0, 255], [255, 0, 0, 255], [255, 0, 0, 255]);

          // TODO: Given this quad, draw a normal out from the mid point
          //       of the lastpathp and current pathp

          // for (let i = positions.length - 6; i < positions.length; i += 3) {
          //   const A = positions[i];
          //   const B = positions[i + 1];
          //   const C = positions[i + 2];
          //   const AB = vec3.sub(vec3.create(), B, A);
          //   const AC = vec3.sub(vec3.create(), C, A);
          //   const normal = vec3.cross(vec3.create(), AB, AC);
          //   // Copy the normal for each position.
          //   normals.push(normal, normal, normal);
          // }
        }

        if (this.sides === 4) {
          positions.push(
            this.lastPathP.shape[1],
            pathP.shape[pathP.shape.length - 1],
            pathP.shape[1],
          );

          normals.push(
            norm(this.lastPathP.shape[0], this.lastPathP.shape[1]),
            norm(pathP.shape[0], pathP.shape[pathP.shape.length - 1]),
            norm(pathP.shape[0], pathP.shape[1]),
          );

          colors.push(this.vary(shade), this.vary(shade), this.vary(shade));

          // First closer.
          positions.push(
            pathP.shape[pathP.shape.length - 1],
            this.lastPathP.shape[1],
            this.lastPathP.shape[pathP.shape.length - 1],
          );

          normals.push(
            norm(pathP.shape[0], pathP.shape[pathP.shape.length - 1]),
            norm(this.lastPathP.shape[0], this.lastPathP.shape[1]),
            norm(
              this.lastPathP.shape[0],
              this.lastPathP.shape[pathP.shape.length - 1],
            ),
          );

          colors.push(this.vary(shade), this.vary(shade), this.vary(shade));

          // colors.push(
          //   [100, 255, 0, 255],
          //   [100, 255, 0, 255],
          //   [100, 255, 0, 255]
          // );
        }

        if (this.sides >= 5) {
          positions.push(
            this.lastPathP.shape[1],
            this.lastPathP.shape[pathP.shape.length - 1],
            pathP.shape[pathP.shape.length - 1],
          );

          normals.push(
            norm(this.lastPathP.shape[0], this.lastPathP.shape[1]),
            norm(
              this.lastPathP.shape[0],
              this.lastPathP.shape[pathP.shape.length - 1],
            ),
            norm(pathP.shape[0], pathP.shape[pathP.shape.length - 1]),
          );

          colors.push(this.vary(shade), this.vary(shade), this.vary(shade));

          positions.push(
            pathP.shape[pathP.shape.length - 1],
            pathP.shape[1],
            this.lastPathP.shape[1],
          );

          normals.push(
            norm(pathP.shape[0], pathP.shape[pathP.shape.length - 1]),
            norm(pathP.shape[0], pathP.shape[1]),
            norm(this.lastPathP.shape[0], this.lastPathP.shape[1]),
          );

          colors.push(this.vary(shade), this.vary(shade), this.vary(shade));
        }
      } else {
        // 📈 Lines
        if (this.sides === 3) {
          positions.push(
            this.lastPathP.shape[1],
            pathP.shape[pathP.shape.length - 1],
          );
          colors.push(this.varyLine(shade), this.varyLine(shade));

          positions.push(pathP.shape[pathP.shape.length - 1], pathP.shape[1]);
          colors.push(this.varyLine(shade), this.varyLine(shade));
        }

        if (this.sides > 3) {
          positions.push(
            this.lastPathP.shape[1],
            pathP.shape[pathP.shape.length - 1],
          );
          colors.push(this.varyLine(shade), this.varyLine(shade));
          // 6. Final across
          positions.push(pathP.shape[1], pathP.shape[pathP.shape.length - 1]);
          colors.push(this.varyLine(shade), this.varyLine(shade));
        }
      }

      this.lastPathP = pathP;
    }

    form = form || this.form;

    // Generate identical normals for each quad!
    // Note: Technically every 6 positions here should result in
    //       the same normals, so I can compute them at the end now.

    //       It's also a good test because the resulting positions should
    //       always be divisible by 6 here.

    //if (this.sides !== 2) return;
    // console.log(positions.length);

    // Draw a line from the center shape point to the current vertex position.

    if (form.primitive === "triangle") {
      /*
      for (let i = 0; i < positions.length; i += 3) {
        const A = positions[i];
        const B = positions[i + 1];
        const C = positions[i + 2];

        const AB = vec3.sub(vec3.create(), B, A);
        const AC = vec3.sub(vec3.create(), C, A);

        const normal = vec3.cross(vec3.create(), AB, AC);

        // Copy the normal for each position.
        normals.push(normal, normal, normal);
      }
      */
    }

    if (positions.length > 0) form.addPoints({ positions, colors, normals });
  }
}

// Turtle graphics in 3D.
class Spider {
  $;
  position;
  lastPosition;
  direction; // A directional axis-orientation vector around [0, 0, 1].
  color;
  rotation; // A quaternion.
  path = []; // A built up path.
  lastNormal; // Remembered for parallel transport / `rotateTowards`.

  constructor(
    $,
    pos = [0, 0, 0, 1],
    lp = [0, 0, 0, 1],
    dir, // A starting directional vector.
    rot, // A quaternion.
    col = [255, 255, 255, 255],
  ) {
    this.$ = $; // Shorthand any dependencies.
    // const { num: { vec3 } } = $;

    if (pos.length === 3) {
      pos = [...pos, 1];
    }
    this.position = pos;
    this.lastPosition = lp;
    this.color = col;

    // TODO: Does this really matter?
    //this.lastNormal = [0, 1, 0];

    this.direction = dir;
    this.rotation = rot;

    this.path.push(this.state);
  }

  get state() {
    // TODO: Is slicing necessary? (Try a complex path with it off.)
    return {
      direction: this.direction.slice(),
      rotation: this.rotation.slice(),
      position: this.position.slice(),
      color: this.color.slice(),
    };
  }

  // Turn the spider towards a target, with a given tightness 0-1.
  // This uses "parallel transport" of normals to maintain orientation.
  crawlTowards(targetPosition, stepSize, tightness) {
    const {
      num: { mat3, mat4, vec3, vec4, quat },
    } = this.$;

    const firstTangent = vec3.normalize(
      vec3.create(),
      //vec3.sub(vec3.create(), targetPosition, this.lastPosition)
      vec3.sub(vec3.create(), this.position, this.lastPosition),
    );

    const nextTangent = vec3.normalize(
      vec3.create(),
      vec3.sub(vec3.create(), targetPosition, this.position),
    );

    // TODO: Will this only be the first normal?
    let lastNormal;
    if (this.lastNormal === undefined) {
      lastNormal = vec4.transformQuat(
        vec4.create(),
        [0, 1, 0, 1],
        this.rotation,
      );
    } else {
      lastNormal = this.lastNormal;
    }

    const helper = vec3.normalize(
      vec3.create(),
      vec3.cross(vec3.create(), lastNormal, firstTangent),
    );

    lastNormal = vec3.normalize(
      vec3.create(),
      vec3.cross(vec3.create(), firstTangent, helper),
    );

    /*
    diffPoints.push(
      // For debugging.
      this.position,
      vec3.add(
        vec3.create(),
        this.position,
        vec3.scale(vec3.create(), firstTangent, 0.05)
      )
    );
    diffColors.push([0, 255, 255, 255], [0, 255, 255, 255]);
    */

    let newNormal;
    let bitangent = vec3.cross(vec3.create(), firstTangent, nextTangent);

    if (vec3.length(bitangent) === 0) {
      newNormal = lastNormal;
    } else {
      // Get angle between first and next tangent.

      // 🅰️ With matrices...
      // bitangent = vec3.normalize(vec3.create(), bitangent);
      // Rotate around bitangent by `theta` radians.
      // const theta = acos(vec3.dot(firstTangent, nextTangent)) || 0;
      // const mat = mat4.fromRotation(mat4.create(), theta, bitangent);
      // newNormal = vec3.transformMat4(vec3.create(), lastNormal, mat);

      // 🅱️ or a quaternion.
      let rq = quat.rotationTo(quat.create(), firstTangent, nextTangent);
      newNormal = vec3.normalize(
        vec3.create(),
        vec3.transformQuat(vec3.create(), lastNormal, rq),
      );
      this.lastNormal = newNormal;
    }

    bitangent = vec3.normalize(
      vec3.create(),
      vec3.cross(vec3.create(), newNormal, nextTangent),
    );

    /*
    diffPoints.push(
      // For debugging.
      this.position,
      vec3.add(
        vec3.create(),
        this.position,
        vec3.scale(vec3.create(), newNormal, 0.05)
      )
    );
    diffColors.push([0, 255, 0, 255], [0, 255, 0, 255]);

    diffPoints.push(
      // For debugging.
      this.position,
      vec3.add(
        vec3.create(),
        this.position,
        vec3.scale(vec3.create(), bitangent, 0.05)
      )
    );
    diffColors.push([255, 255, 0, 255], [255, 255, 0, 255]);
    */

    // Build a rotation transform.
    const qua = quat.normalize(
      quat.create(),
      quat.fromMat3(
        quat.create(),
        mat3.fromValues(...bitangent, ...newNormal, ...nextTangent),
      ),
    );

    // Interpolate it... only apply a section via `tightness` from 0-1.
    let slerpedRot = quat.slerp(quat.create(), this.rotation, qua, tightness);

    // quat.normalize(slerpedRot, slerpedRot);

    // console.log(slerpedRot.slice(), this.rotation.slice())

    this.rotation = slerpedRot; // Only update the quaternion if it makes sense with the bitangent result.

    // This position stuff should come back together maybe...

    // Get the direction between this position and the target position, then

    // Original direction.
    this.direction = vec4.normalize(
      vec4.create(),
      vec4.transformQuat(vec4.create(), [0, 0, 1, 1], slerpedRot),
    );

    const scaledDir = vec3.scale(vec3.create(), this.direction, stepSize);
    const pos = vec3.add(vec3.create(), this.position, scaledDir);

    this.lastPosition = this.position;
    this.position = [...pos, 1];

    const state = this.state;
    this.path.push(state);
    return state;
  }

  // Set the color.
  // Note: Why not invoke the full `ink` / `color` parser from `disk` here?
  ink() {
    this.color = arguments.length === 3 ? [...arguments, 255] : [...arguments];
  }

  // Imagine a future forward position, turning an optional amount beforehand.
  peek(steps, turn) {
    return this.crawl(steps, true);
  }
}

// 🔴 Record and playback session data.
class Demo {
  startTick;
  currentTick;
  startDelay = 0n;
  frames = [];
  progress = 0n;

  constructor() {
    console.log("🔴 Recording a demo!");
  }

  // Update the demo tick. Should be tied to the end of each data update.
  sim(simCount) {
    if (this.startTick === undefined) {
      this.startTick = simCount;
      this.currentTick = simCount;
      this.progress = 0n;
    } else {
      this.currentTick = simCount;
      this.progress = this.currentTick - this.startTick - this.startDelay;
      // Offset any dead time before first interaction.
    }
  }

  // Save timestamped data to a demo frame.
  rec(label, data) {
    let frame;

    if (this.frames.length === 0) {
      this.startDelay = this.progress;
      this.progress = 0n;
    }

    if (Array.isArray(data)) {
      // Add data to a frame array, cutting any decimals to 6 places.
      frame = [
        this.progress,
        label,
        ...data.map((v) => {
          if (typeof v === "number") return parseFloat(v.toFixed(6));
          return v;
        }),
      ];
      this.frames.push(frame);
    } else {
      // Add data directly unless it's `undefined`.
      if (data === undefined) {
        frame = [this.progress, label]; // Don't send anything if data is empty.
      } else {
        if (typeof data === "number") data = parseFloat(data.toFixed(6));
        frame = [this.progress, label, data]; // Make sure to keep Booleans though.
      }
      this.frames.push(frame);
    }

    // if (label !== "wand")
    //  console.log("🔴 Recording:", frame, this.frames.length);
  }

  // Log the current results.
  print() {
    console.log("🔴 Demo frames:", this.frames);
  }

  // Clear any existing demo state / start a new demo.
  dump() {
    this.startTick = undefined;
    this.currentTick = undefined;
    this.frames = [];
    this.progress = 0n;
    console.log("🔴 Recording a demo!");
  }
}

class Player {
  frames;
  frameIndex = 0;
  simCount = 0n;
  endAtLastIndex;
  startAtFirstIndex;
  collectedFrames = [];
  instant; // Play all frames back instantly after one sim call.
  waitForPreload;
  paused = false;

  #instantChunks = [];
  #instantDelay = 0;
  #instantDelayMax = 1;

  // loop;

  constructor(
    frames,
    instant = MetaBrowser ? 1 : 5, // Slower default in VR.
    waitForPreload,
    goUntilFirst = "tube:start",
    endAtLast = "room:color",
  ) {
    this.frames = frames;
    this.instant = instant;
    this.waitForPreload = waitForPreload;

    // Find index of first item if we can.
    for (let f = 0; f < frames.length - 1; f += 1) {
      if (frames[f][1] === goUntilFirst) {
        this.startAtFirstIndex = f;
        break;
      }
    }

    // Find index of last item if we can.
    for (let e = frames.length - 1; e > 0; e -= 1) {
      if (frames[e][1] === endAtLast) {
        this.endAtLastIndex = e;
        break;
      }
    }

    let thisFrame = this.frames[this.frameIndex];

    while (this.frameIndex < this.startAtFirstIndex) {
      this.collectedFrames.push(thisFrame);
      this.simCount = thisFrame[0];
      this.frameIndex += 1;
      thisFrame = frames[this.frameIndex];
    }
  }

  sim(handler, simCount) {
    if (this.paused) return; // Don't simulate anything while paused.

    let thisFrame = this.frames[this.frameIndex];

    // Finish a demo if there are no frames left.
    if (!thisFrame) {
      console.log("🛑 Demo playback completed:", this.frameIndex);
      // Push a completed message with a negative frameCount to mark an ending.
      handler([[-1, "demo:complete"]]);
      this.waitForPreload?.();
      return;
    }

    // Run through all the frames instantly then finish on next tick.
    if (this.instant === 0) {
      if (this.#instantChunks.length === 0) {
        // Segment frames into temporal chunks that run on each sim tick.
        // (Prevents lock-up)

        // Handle first batch of frames up until startAtFirstIndex.
        handler(this.collectedFrames);
        this.collectedFrames = [];

        for (let f = this.startAtFirstIndex; f <= this.endAtLastIndex; f += 1) {
          this.collectedFrames.push(this.frames[f]);
        }

        const chunkSize = 500; // This seems reasonable. 23.02.05.15.00

        for (let i = 0; i < this.collectedFrames.length; i += chunkSize) {
          const end = i + chunkSize;
          const lastChunk = end >= this.collectedFrames.length;
          const chunk = this.collectedFrames.slice(
            i,
            lastChunk ? this.collectedFrames.length : end,
          );
          this.#instantChunks.push(chunk);
        }

        this.collectedFrames = [];
      }

      if (this.#instantChunks.length > 0) {
        // Give a few frames of speace between each chunk for animations
        // and other things to complete.
        if (this.#instantDelay >= this.#instantDelayMax) {
          const chunk = this.#instantChunks.shift();
          handler(chunk); // Run our action handler for this chunk.
          this.frameIndex += chunk.length;
          this.#instantDelay = 0;
        } else {
          this.#instantDelay += 1;
        }
      } else {
        this.frameIndex = -1; // Call `demo:complete` when no chunks are left.
      }

      return;
    }

    // Or wait on a frame if needed... for realtime.
    if (this.simCount < thisFrame[0]) {
      this.simCount += 1n;
      return;
    }

    let multiplier = 1;
    if (this.instant > 0) multiplier = this.instant;

    for (let i = 0; i < multiplier; i += 1) {
      // And push current frame plus others following it on the same tick.
      while (thisFrame && thisFrame[0] === this.simCount) {
        this.collectedFrames.push(thisFrame);
        if (thisFrame === this.frames[this.endAtLastIndex]) {
          this.frameIndex = -1; // Hit last frame.
          break;
        }

        if (this.frameIndex === -1) break;

        this.frameIndex += 1;
        thisFrame = this.frames[this.frameIndex];
      }

      this.simCount += 1n;
    }

    handler(this.collectedFrames, this.simCount); // Run our action handler.
    this.collectedFrames = [];
  }

  get progress() {
    const p =
      (this.frameIndex - this.startAtFirstIndex) /
      (this.endAtLastIndex - this.startAtFirstIndex - 3);
    // Account for the metadata frames.
    return p < 0 ? 1 : p; // Return 1 if not playing or complete.
  }
}

// Via `three.module.js`
function SRGBToLinear(c) {
  return c < 0.04045
    ? c * 0.0773993808
    : Math.pow(c * 0.9478672986 + 0.0521327014, 2.4);
}

function LinearToSRGB(c) {
  return c < 0.0031308 ? c * 12.92 : 1.055 * Math.pow(c, 0.41666) - 0.055;
}
// #endregion
