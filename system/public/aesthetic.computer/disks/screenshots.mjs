// Screenshots, 2024.3.15.10.33.23.883
// Screenshots of aesthetic computer development.

/* #region 🏁 TODO 
#endregion */

const shots = 90;
let shot;

function boot({ params, dom: { html }, num }) {
  // Runs once at the start.
  console.log(params[0]);
  shot = parseInt(params[0]) || num.randIntRange(1, shots);
  html`
    <img
      id="screenshot"
      src="/assets/screenshots/july-11-2023-at-10-41-am.png"
    />
    <style>
      #screenshot {
        width: 100vw;
        height: 100vh;
        object-fit: contain;
        padding: 1em;
        background: orange;
        outline: 2px solid white;
      }
    </style>
  `;
}

function paint({ wipe }) {
  wipe("purple"); // Would draw a diagonal line.
  return false;
}

function act({ event: e, jump }) {
  if (e.is("keyboard:down:arrowleft")) jump(`ss ${min(shot + 1, shot)}`);
  if (e.is("keyboard:down:arrowright")) jump(`ss ${max(shot - 1, 1)}`);
}

function meta() {
  return {
    title: "Screenshots",
    desc: "Screenshots of aesthetic computer development.",
  };
}

export { boot, paint, act, meta };