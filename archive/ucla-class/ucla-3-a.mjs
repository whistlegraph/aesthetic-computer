// ucla-3, 24.07.09.07.52
// Essential sonics and data types.

/* 📝 Notes 
  - Today we will be synthesizing sound using primitive wave types, and learning
    about structured data in JavaScript.
      📜 basic data types: `string`, `boolean`, `number`
         composite types: `array`, `object`
      🟪️ `act`, `event`, `synth`
    - Exercises -
    1. [🟡] Writing text and basic math. 
    2. [🟡] Keyboard input as a trigger. 🟩🟦
    3. [🟡] Synthesizing sound. 🎹
    4. [] Growing visually and sonically. 🪱

    ⌨️ Useful `Visual Studio Code` shortcuts.
    🍎 + Shift + P -> Format Document
    🍎 + / -> Toggle Comments
*/

let myNumber = 0;
let myOtherNumber = 0;

function paint({ wipe, ink, write }) {
  wipe(myNumber, 128, 200); // clears the screen to blue
  //               calls the function 'wipe' and sends it a string "blue"
  ink("yellow");
  // Combining strings.
  // ARRAYS ["J", "e", "f", "f"]
  //          0    1    2    3
  // To get "e", you write first[1];
  const first = "Jeffrey";
  //             0123456
  const space = ", ";
  const second = "says...";
  write(first + space + second, 16, 16); // text, x, y
  ink("lime");
  write(`${first} ${second}`, 16, 32);
  //     ${ ... any javascript ... }
  ink("cyan");
  write(first[1], 16, 48); // 2nd char of the 'first' string.
  // 🔪 Slice up from first (0) character (J) to 4th character, (f)
  write(first.slice(0, 4), 16, 64);
  write(first.toUpperCase(), 16, 80); // Transformations on a string.
  ink("red");

  // myNumber = myNumber + 1; // Add one to myNumber.

  let result = myNumber; // Math.min(1, 1, 3, 4, 1, 1, 10) // (13 + 2) * 10;
  // Why is this 33?
  // Multiplation always happens before arithmetic.
  // Change order, using parenthesis.

  write(result, 16, 96); // reference

  ink("lime");
  write(myOtherNumber, 16, 128);
}


// ⭐ CC GG AA G FF EE DD C
//    GG FF EE D
//    GG FF EE D

// Mary had a little lamb. 🦙
// EDCDEEE
// DDD EGG
// EDCD EEEE
// DD EDC

const notes = ["c", "d", "e", "f", "g", "a", "b"];
const tones = [260, 293, 329, 349, 392, 440, 493];
//               0,    1,  2,    3,  4,   5,   6
const sounds = {}; // starts empty...

function act({ event: e, sound, help }) {
  // 🚩 Array Iteration
  notes.forEach((note, index) => {
    if (e.is("keyboard:down:" + note) && e.repeat === false) {
      myNumber = tones[index]; // index will be 0, 1, 2, 3
      const wavetype = help.choose("sine", "square", "triangle"); // 🎲
      myOtherNumber = wavetype;
      sounds[note] = sound.synth({ type: wavetype, tone: myNumber, duration: Infinity });
      // "sine", "square" (default), "triangle", "sawtooth", "noise-white"
      // { c: synth, a: synth, b: synth }
    }

    if (e.is("keyboard:up:" + note)) {
      sounds[note]?.kill(0.2);
    }
  });

  // if (e.is("keyboard:down:c") && e.repeat === false) {
  //   myNumber = 260;
  //   cSound = sound.synth({ tone: myNumber, duration: Infinity });
  // }

  // if (e.is("keyboard:up:c") && cSound) {
  //   // Stop the sound....
  //   myNumber = 0;
  //   cSound.kill(0.1); // Add an optional fade out duration...
  //   cSound = undefined; // Clear out the reference.
  // }

  // if (e.is("keyboard:down:d")) {
  //   sound.synth({ tone: 293 });
  // }

  // if (e.is("keyboard:down:e")) {
  //   sound.synth({ tone: 329 });
  // }

  // if (e.is("keyboard:down:f")) {
  //   sound.synth({ tone: 349 });
  // }

  // if (e.is("keyboard:down:g")) {
  //   sound.synth({ tone: 392 });
  // }

  // if (e.is("keyboard:down:a")) {
  //   sound.synth({ tone: 440 });
  // }

  // if (e.is("keyboard:down:b")) {
  //   sound.synth({ tone: 493 });
  // }
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
