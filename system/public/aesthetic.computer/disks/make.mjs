// Make, 2025.6.15.02.38.18.414
// Compose a piece with a prompt.

/* 📝 Engineering Notes
  The `paint` function runs every animation frame.
  `screen.pixels` is a Uint8ClampedArray with direct access.
  `screen.width` and `screen.height` is also available for aspect ratio / limits.
  Special note: `Use screen.pixels / direct pixel array access for any automated drawing.`
*/

/* #region 📓 TODO
  - [x] Incorporate features from `oldmake.mjs`. 
#endregion */

import { Conversation } from "../lib/ask.mjs";

let conversation,
  reply = "",
  charbuff = "",
  code = ``,
  abort;
let lastCharacterDisplayFrame = 0;

// 🥾 Boot (Runs once before first paint and sim)
function boot({ params, system: { painting }, store, slug, lisp }) {
  console.log(lisp.parse("(+ 1 2 3)"));

  // Test newline handling in kidlisp
  const testSource = `line 10 10 50 50
ink red
box 20 20 30 30`;

  console.log("Original test source:", testSource);
  console.log("Parsed test source:", lisp.parse(testSource));

  /*
  const program = {
    before: `{{{`,
    // user input
    after: `}}}`,
  };
  conversation = new Conversation(store, slug);
  conversation.retrieve().then(() => {
    abort = conversation.ask(
      { prompt: params.join(" ") || "a red circle", program, hint: "code" },
      function and(msg) {
        charbuff += msg;
        reply += msg;
      },
      function done(msg) {
        console.log("🟢 Done: ", reply);
      },
      function fail() {
        code = "NETWORK FAILURE";
      },
    );
  });
  */
}

function sim({ event, jump, reload, simCount, sound }) {
  if (charbuff.length /* && simCount % 2n === 0n*/) {
    const char = charbuff[0];
    code += char;
    charbuff = charbuff.slice(1);

    // 🚥 TODO: How to compress a synth call into a single string based expression syntax? 25.06.17.19.53
    sound.synth({
      type: "sine",
      tone: 700,
      duration: 0.005,
      attack: 0,
      decay: 0,
      volume: 0.25,
    });
  }
}

// 🎨 Paint (Executes every display frame)
function paint({ wipe, ink, write, system, screen, typeface, undef }) {
  wipe(0);
  write(
    `\\white\\${code}\\gray\\${reply.substring(code.length)}`,
    { x: 7, y: 24 },
    undef,
    screen.width - typeface.blockWidth,
  );

  // TODO: Try to compile the code as a `kidlisp` function.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave() {
  abort?.(); // Cancel any existing `ask` which halts the server.
}

// 📚 Library (Useful functions used throughout the piece)
