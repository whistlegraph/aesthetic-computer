// Blank, 22.10.03.15.13 
// Every new piece has to start somewhere...

/* #region 🤝 Read Me 

Dear Piecemaker,

🤸 Welcome to aesthetic.computer!

There aren't really any docs right now,
but you can `console.log($api);` or throw in
a `debugger` statement anywhere in a function
to explore what's available.

But what's even faster than that is to just ask 
me directly how to access and control the features
you're interested in using for a piece.

Run this piece by typing `@piecemaker/blank`
on aesthetic.computer or 4esthetic.com for short!

And debug in the dev console!

Jeffrey (me@jas.life / digitpain#2262 / @digitpain)

#endregion */

// 🥾 Boot (Runs once before first paint and sim)
// function boot({wipe, ink, screen }) {
//   wipe(0);
//   ink(255, 0, 0).line(0, 0, screen.width, screen.height);
// }


function boot({ wipe, ink, screen }) {
  const colors = [
    [255, 0, 0], // Red
    [255, 165, 0], // Orange
    [255, 255, 0], // Yellow
    [0, 128, 0], // Green
    [0, 0, 255], // Blue
    [75, 0, 130], // Indigo
    [238, 130, 238], // Violet
  ];

  const radius = Math.min(screen.width, screen.height) / 4;

  wipe(0);

  for (let i = 0; i < colors.length; i++) {
    const [r, g, b] = colors[i];
    ink(r, g, b).circle(screen.width / 2, screen.height / 2, radius + i * 3);
  }

  // Draw eyes
  ink(255, 255, 255).circle(
    screen.width / 2 - radius / 2,
    screen.height / 2 - radius / 2,
    radius / 5
  );
  ink(255, 255, 255).circle(
    screen.width / 2 + radius / 2,
    screen.height / 2 - radius / 2,
    radius / 5
  );
  ink(0, 0, 0).circle(
    screen.width / 2 - radius / 2,
    screen.height / 2 - radius / 2,
    radius / 10
  );
  ink(0, 0, 0).circle(
    screen.width / 2 + radius / 2,
    screen.height / 2 - radius / 2,
    radius / 10
  );

  // Draw mouth
  ink(255, 255, 255).line(
    screen.width / 2 - radius / 2,
    screen.height / 2 + radius / 2,
    screen.width / 2 + radius / 2,
    screen.height / 2 + radius / 2
  );
}




// 🎨 Paint (Executes every display frame)
function paint($) {
}

/*
// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint }