// Screenshots, 2024.3.15.10.33.23.883
// Screenshots of aesthetic computer development.

/* #region 🏁 TODO 
  - [] Re-arrange token numbers based on minting?
  - [] Test reflowing of caption text (15)
  - [] Add link to token.
  - [] Add big button for tapping between slides.
  - [] Dogfood with a new GL renderer.
  + Done
  - [+] Load in the /assets/screenshots/metadata.json to parse param
       integers with filenames.
#endregion */

const shots = 90;
let shot;
let metadata;
const { max, min } = Math;

async function boot({ api, params, num, debug }) {
  const response = await fetch(
    `${
      debug ? "/assets/" : "https://assets.aesthetic.computer"
    }/screenshots/metadata2.json`,
  );
  metadata = await response.json();

  shot = parseInt(params[0]) || num.randIntRange(1, shots);
  goToShot(api, shot);
}

function paint({ ink, screen, noise16Aesthetic }) {
  noise16Aesthetic();
  ink(60, 40, 60, 200).box(0, 0, screen.width, screen.height); // Would draw a diagonal line.
  // return false;
}

function act({ api, event: e, jump }) {
  if (e.is("keyboard:down:arrowleft")) goToShot(api, max(shot - 1, 1));
  if (e.is("keyboard:down:arrowright")) goToShot(api, min(shot + 1, shots));
}

function goToShot({ dom: { html, clear }, hud, net, needsPaint, debug }, n) {
  shot = n;
  clear();
  const record = metadata[shot - 1];
  html`
    <img
      id="screenshot"
      src="${debug
        ? "/assets"
        : "https://assets.aesthetic.computer"}/screenshots/images/${record.imageRef}"
    />
    <h1 id="screenshot-title">
      ${record.name}${record.description ? " - " + record.description : ""}
    </h1>
    <style>
      #screenshot-title {
        font-family: YWFTProcessing-Regular;
        font-weight: normal;
        position: absolute;
        top: 2.5em;
        left: 0.7em;
        padding: 0;
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
  hud.label(`screenshots ${shot}`);
  net.rewrite(`screenshots~${shot}`);
  needsPaint();
}

function meta() {
  return {
    title: "Screenshots",
    desc: "Screenshots of aesthetic computer development.",
  };
}

export { boot, paint, act, meta };
