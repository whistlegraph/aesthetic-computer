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
function boot($) {
  console.log("BOOOOOOOOOOOOOOOOOOT")
  $.wipe(255, 0, 0);
}

// 🎨 Paint (Executes every display frame)
function paint() {
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