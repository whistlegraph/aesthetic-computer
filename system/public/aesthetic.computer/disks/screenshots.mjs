// Screenshots, 2024.3.15.10.33.23.883
// Screenshots of aesthetic computer development.

/* #region 🏁 TODO 
  - [c] Pause autoAdvance timer when image is loading.
  - [c] Dogfood with a new GL renderer.
  + Done
  - [x] Metadata change.
  - [x] Add two big buttons for tapping between slides.
  - [x] Add "Chain" button link to token.
  - [x] Don't automatically change / tint color when the hud label changes.
  - [x] Don't replace all html content on a slide change.
  - [x] Test reflowing of caption text (13)
  - [x] Tap to hold / pause.
  - [x] Audo-advance from 0 if just going to `screenshots`.
  - [x] Re-arrange token numbers based on minting.
  - [x] Remove `june 21 2023` from assets.
  - [x] Load in the /assets/screenshots/metadata.json to parse param
       integers with filenames.
#endregion */

const shots = 90;
let shot, metadata, record;
let chain;
let autoAdvance = false,
  autoAdvanceProgress = 0;
const autoAdvanceSteps = 750;
let currentTint;
const tints = [
  "red",
  "yellow",
  "orange",
  "blue",
  "brown",
  "magenta",
  "pink",
  "green",
  "lime",
];
let nextBtn, prevBtn;

const { max, min } = Math;

async function boot({ api, params, num, debug, help }) {
  shot = parseInt(params[0]) || null;

  const response = await fetch(
    `${
      debug ? "/assets/" : "https://assets.aesthetic.computer"
    }/screenshots/metadata.json`,
  );
  metadata = await response.json();

  autoAdvance = shot === null;
  if (autoAdvance) shot = 1;
  currentTint = help.choose(...tints);
  goToShot(api, shot);
}

function paint({ ink, screen, ui, noiseTinted, geo }) {
  noiseTinted(currentTint, 0.4, 0.7);
  ink(60, 40, 60, 200).box(0, 0, screen.width, screen.height); // Would draw a diagonal line.

  // Prev & Next Buttons
  const prevNextMarg = 32;
  const prevNextWidth = screen.width / 2;

  if (!prevBtn) {
    prevBtn = new ui.Button();
    if (shot <= 1) prevBtn.disabled = true;
  }

  if (!nextBtn) nextBtn = new ui.Button();

  prevBtn.box = new geo.Box(
    0,
    prevNextMarg,
    prevNextWidth,
    screen.height - prevNextMarg * 2,
  );

  nextBtn.box = new geo.Box(
    screen.width - prevNextWidth,
    prevNextMarg,
    screen.width,
    screen.height - prevNextMarg * 2,
  );

  // No need to draw these.
  if (!prevBtn.disabled)
    ink(255, 255, 0, prevBtn.down ? 20 : 0).box(
      0,
      0,
      screen.width / 2,
      screen.height,
    );

  if (!nextBtn.disabled)
    ink(255, 255, 0, nextBtn.down ? 20 : 0).box(
      screen.width / 2,
      0,
      screen.width,
      screen.height,
    );

  // Progress bar
  if (autoAdvance) {
    ink(0)
      .box(0, 0, screen.width, 1)
      .ink(currentTint, 230)
      .box(0, 0, (autoAdvanceProgress / autoAdvanceSteps) * screen.width, 1);
  }

  if (!chain)
    chain = new ui.TextButton("Chain", {
      bottom: 6,
      right: 6,
      screen,
    });

  chain.reposition({ bottom: 6, right: 6, screen });
  chain.paint({ ink }, [0, 255, 255, 0]);
}

function act({ api, event: e, jump, help }) {
  function prev() {
    if (autoAdvance) {
      autoAdvance = false;
      autoAdvanceProgress = 0;
    }
    currentTint = help.choose(...tints);
    goToShot(api, max(shot - 1, 1));
  }

  function next() {
    if (autoAdvance) {
      autoAdvance = false;
      autoAdvanceProgress = 0;
    }
    currentTint = help.choose(...tints);
    goToShot(api, shot === 90 ? 1 : min(shot + 1, shots));
  }

  if (e.is("keyboard:down:arrowright")) next();
  if (e.is("keyboard:down:arrowleft")) prev();

  nextBtn?.act(e, next);
  prevBtn?.act(e, prev);

  chain?.btn.act(e, () => {
    jump(`out:https://highlight.xyz/mint/65f5e86427e22afacb0972e7/t/${shot}`);
    bap = true;
  });
}

function sim({ api, help, pen }) {
  if (autoAdvance && !pen?.drawing) {
    autoAdvanceProgress += 1;
    if (autoAdvanceProgress === autoAdvanceSteps) {
      currentTint = help.choose(...tints);
      autoAdvanceProgress = 0;
      goToShot(api, shot === 90 ? 1 : min(shot + 1, shots));
    }
  }
}

let beatCount = 0n; // TODO: This should REALLY go into the main API at this point... 23.05.08.17.32
let bap, bip;

function beat({ num, sound: { bpm, synth } }) {
  if (beatCount === 0n) {
    bap = bip = false; // Clear any existing signals.
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
      tone: num.randIntRange(1000, 1600),
      beats: 1,
      attack: 0.02,
      decay: 0.97,
      volume: 0.1,
    });
    bip = false;
  }
}

function meta({ params }) {
  let title;
  console.log(params);
  if (params[0] === undefined) {
    title = "Screenshots · Aesthetic Computer";
  } else {
    title = `Screenshots - ${shot || params[0]} · Aesthetic Computer`;
  }
  return {
    title,
    desc:
      getCaption(record) || `Screenshots of aesthetic computer development.`,
  };
}

export { boot, paint, act, sim, beat, meta };

let contentAdded = false;

function getCaption(record) {
  if (!record) return;
  return `${record.name}${
    record.description ? " - " + record.description : ""
  }`;
}

function goToShot(
  {
    meta: updateMetadata,
    dom: { html, clear },
    hud,
    net,
    needsPaint,
    debug,
    signal,
  },
  n,
) {
  shot = n;
  bip = true;
  record = metadata[shot - 1];
  updateMetadata(meta({ params: [shot] }));
  function getSrc(record) {
    return (
      (debug ? "/assets" : "https://assets.aesthetic.computer") +
      `/screenshots/images/${record.imageRef}`
    );
  }

  if (prevBtn) {
    if (shot > 1) {
      prevBtn.disabled = false;
    } else {
      prevBtn.disabled = true;
    }
  }

  if (nextBtn) {
    if (shot === shots) {
      nextBtn.disabled = true;
    } else {
      nextBtn.disabled = false;
    }
  }

  if (contentAdded) {
    signal({
      type: "screenshot:switched",
      content: { src: getSrc(record), caption: getCaption(record) },
    });
  } else if (!contentAdded) {
    html`
      <img id="screenshot" loading="lazy" src="${getSrc(record)}" />
      <h1 id="screenshot-title" class="hidden">${getCaption(record)}</h1>
      <script>
        // Only show the font once it has been loaded.
        const fontName = "YWFTProcessing-Regular";
        const checkInterval = 100; // Interval in milliseconds
        let fontCheckInterval;
        function checkFont() {
          if (document.fonts.check(\`1em "\${fontName}"\`)) {
            clearInterval(fontCheckInterval);
            document.getElementById("screenshot-title").style.visibility =
              "visible";
          }
        }
        fontCheckInterval = setInterval(checkFont, checkInterval);
        document.getElementById("screenshot-title").style.visibility = "hidden";
        checkFont();
        const title = document.getElementById("screenshot-title");
        let screenshot = document.getElementById("screenshot");
        window.when("screenshot:switched", function (record) {
          // console.log("📸 Shot switched:", record);
          const img = new Image();
          img.onload = function () {
            img.id = "screenshot";
            screenshot.parentNode.appendChild(img);
            screenshot.remove();
            screenshot = img;
            title.innerHTML = record.caption;
          };
          img.src = record.src;
        });
      </script>
      <style>
        .hidden {
          visibility: hidden;
        }
        #screenshot-title,
        #screenshot {
          pointer-events: none;
        }
        #screenshot-title {
          font-family: YWFTProcessing-Regular;
          font-weight: normal;
          position: absolute;
          top: 2.5em;
          left: 0.7em;
          padding: 0;
          padding-right: 0.5em;
          margin: 0;
          font-size: 1em;
          color: white;
          opacity: 0.75;
        }
        #screenshot {
          width: 100%;
          height: 100%;
          object-fit: contain;
          padding: 4.5em 1em 1em 1em;
          box-sizing: border-box;
        }
      </style>
    `;
    contentAdded = true;
  }
  hud.label(`screenshots ${shot}`);
  net.rewrite(`screenshots~${shot}`);
  needsPaint();
}
