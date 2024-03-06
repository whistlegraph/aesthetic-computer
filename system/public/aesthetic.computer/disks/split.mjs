// Split, 2024.3.03.15.46.29.905
// Run two instances of aesthetic computer inside itself, side by side.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Make it so that `split` has an escape of some kind?
  - [] Take care of `gap` spacing.
#endregion */

// export const nohud = true;

function boot({ params, dom: { html }, resolution, screen }) {
  html`
    <style>
      #content {
        display; flex;
        flex-direction: column;
        padding-top: 2.75em;
        box-sizing: border-box;
      }
      iframe {
        box-sizing: border-box;
        width: 100%;
        height: 50%;
        border: none;
        background: black;
      }
    </style>
    <iframe src="/${params[0] || ''}?nogap"></iframe>
    <iframe src="/${params[0] || params[1] || ''}?nogap"></iframe>
  `;
}

// 🎨 Paint
function paint({ wipe }) {
  wipe("gray");
  return false;
}

// 🎪 Act
// function act({ event: e }) {
//  // Respond to user input here.
// }

export { boot, paint };
