// Split, 2024.3.03.15.46.29.905
// Run two instances of aesthetic computer inside itself, side by side.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [] Fix white style flash.
  - [] Add an extra line / separator between the frames.
  - [] Add parameter support.
  + Done
  - [x] Make it so that `split` has an escape of some kind?
  - [x] Take care of `gap` spacing.
#endregion */

// export const nohud = true;

function boot({ params, dom: { html } }) {
  html`
    <style>
      #content {
        display; flex;
        flex-direction: column;
        padding-top: 3em;
        box-sizing: border-box;
      }
      iframe {
        box-sizing: border-box;
        width: 100%;
        height: 50%;
        border: none;
        background: black;
      }
      #split-top {

      }
    </style>
    <iframe id="split-top" src="/${params[0] || ''}?nogap"></iframe>
    <iframe src="/${params[0] || params[1] || ''}?nogap"></iframe>
  `;
}

// 🎨 Paint
function paint({ wipe, dark }) {
  wipe(dark ? "purple" : "yellow");
  return false;
}

// 🎪 Act
function act({ event: e, needsPaint }) {
 // Respond to user input here.
 if (e.is("dark-mode") || e.is("light-mode")) needsPaint();
}

export { boot, paint, act };
