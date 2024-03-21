// Screenshots, 2024.3.15.10.33.23.883
// Screenshots of aesthetic computer development.

/* #region 🏁 TODO 
  - [] Don't replace all html content on a slide change.
  - [] Don't automatically change / tint color when the hud label changes.
  - [] Add link to token.
  - [] Add big button for tapping between slides.
  - [c] Dogfood with a new GL renderer.
  + Done
  - [x] Test reflowing of caption text (13)
  - [x] Tap to hold / pause.
  - [x] Audo-advance from 0 if just going to `screenshots`.
  - [x] Re-arrange token numbers based on minting.
  - [x] Remove `june 21 2023` from assets.
  - [x] Load in the /assets/screenshots/metadata.json to parse param
       integers with filenames.
#endregion */

const shots = 90;
let shot, metadata;
let autoAdvance = false,
  autoAdvanceProgress = 0;
const autoAdvanceSteps = 700;
let currentTint;
const tints = [
  "red",
  "yellow",
  "orange",
  "blue",
  "brown",
  "purple",
  "pink",
  "green",
  "lime",
];

const { max, min } = Math;

async function boot({ api, params, num, debug, help }) {
  const response = await fetch(
    `${
      debug ? "/assets/" : "https://assets.aesthetic.computer"
    }/screenshots/metadata.json`,
  );
  metadata = await response.json();

  shot = parseInt(params[0]) || null;

  autoAdvance = shot === null;
  if (autoAdvance) shot = 1;
  currentTint = help.choose(...tints);
  goToShot(api, shot);
}

function paint({ ink, screen, noiseTinted }) {
  noiseTinted(currentTint, 0.4, 0.7);
  ink(60, 40, 60, 200).box(0, 0, screen.width, screen.height); // Would draw a diagonal line.
  // return false;
  if (autoAdvance) {
    ink(0)
      .box(0, 0, screen.width, 1)
      .ink(currentTint, 230)
      .box(0, 0, (autoAdvanceProgress / autoAdvanceSteps) * screen.width, 1);
  }
}

function act({ api, event: e, jump, help }) {
  if (e.is("keyboard:down:arrowleft")) {
    if (autoAdvance) autoAdvanceProgress = 0;
    currentTint = help.choose(...tints);
    goToShot(api, max(shot - 1, 1));
  }
  if (e.is("keyboard:down:arrowright")) {
    if (autoAdvance) autoAdvanceProgress = 0;
    currentTint = help.choose(...tints);
    goToShot(api, min(shot + 1, shots));
  }
}

function sim({ api, help, pen }) {
  if (autoAdvance && !pen.drawing) {
    autoAdvanceProgress += 1;
    if (autoAdvanceProgress === autoAdvanceSteps) {
      currentTint = help.choose(...tints);
      autoAdvanceProgress = 0;
      goToShot(api, min(shot + 1, shots));
    }
  }
}

function meta() {
  return {
    title: "Screenshots",
    desc: "Screenshots of aesthetic computer development.",
  };
}

export { boot, paint, act, sim, meta };

let contentAdded = false;

function goToShot(
  { dom: { html, clear }, hud, net, needsPaint, debug, signal },
  n,
) {
  shot = n;
  const record = metadata[shot - 1];
  // clear();
  if (contentAdded) {
    // TODO: Compute the new imageRef here.
    signal({ type: "screenshot:switched", content: "test" });
  } else if (!contentAdded) {
    html`
      <img
        id="screenshot"
        src="${debug
          ? "/assets"
          : "https://assets.aesthetic.computer"}/screenshots/images/${record.imageRef}"
      />
      <h1 id="screenshot-title" class="hidden">
        ${record.name}${record.description ? " - " + record.description : ""}
      </h1>
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
        window.when("screenshot:switched", function (data) {
          console.log("📸 Shot switched:", data);
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
