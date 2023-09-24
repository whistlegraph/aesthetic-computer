// Booted-by, 2023.9.13.19.43.52.028
// Aesthetic was booted by...

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🧡] overlay with text and buttons
    - [] text jeffrey at xxx-xxx-xxxx to claim spot. urgent, legitimate, exciting 
    - [] 10 tappable bars with decoration.
  - [] `booted-by` - your text and your url
  - [] names / bars go underneath with booted by in top left
  - [] ok button and demo video link within the text that takes you out of app to demo video 
  - [] names and "unclaimed" or "empty" or "open" for open slots
  - [] tapping bar brings back overlay
  + Later?
  - [] Make this a nice payment page
  - [] date of boot after boot completed
  - [] names link to people's profiles, invitation to make profile somewhere
#endregion */

const copy = `We're at the beginning of a computer age where the advent of AI and text-to-media interfaces means that everyone can be a programmer. Aesthetic computer aims to lead this paradigm shift as an accessible and evolving social platform for art and media creation. We need funding to usher in the new age.`;

const blockWidth = 6;
const blockHeight = 11;
let ok;

// 🥾 Boot
function boot({ ui }) {
  ok = new ui.TextButton("Fund");
}

// 🎨 Paint
function paint({ api, ink, wipe, text, screen }) {
  wipe(0);

  // Overlay
  const marg = 4;
  const bound = screen.width - blockWidth * 3;

  const x = screen.width / 2 - bound / 2,
    y = blockHeight * 2.5;

  // Copy
  const pos = { x, y, screen };
  const tb = text.box(copy, pos, bound, 1);

  tb.box.height += 32;
  pos.y = screen.height / 2 - tb.box.height / 2;
  tb.box.y = pos.y;

  ink(64).write(copy, pos, undefined, bound);
  tb.box.x -= marg;
  tb.box.width += marg * 2;
  tb.box.y -= marg * 2;
  tb.box.height += marg;
  ink(255, 0, 0, 64).box(tb.box);

  // Link
  // Button

  ink(0, 100, 0, 192).box(
    tb.box.x,
    tb.box.y,
    tb.box.width,
    tb.box.height,
    "inline",
  );
  ok?.reposition({ bottom: 6, right: 6, screen: tb.box });
  ok?.paint(api, [[0, 128, 0], 255, 255, [0, 128, 0]]);
}

// 🎪 Act
function act({ event: e }) {
  ok?.btn.act(e, {
    push: () => {
      console.log("Hi");
    },
  });
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Booted by",
    desc: "Aesthetic was booted by...",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
