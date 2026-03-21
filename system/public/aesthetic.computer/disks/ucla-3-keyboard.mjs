// ucla-3, 24.07.09.07.52 
// Essential sonics and data types.

/* 📝 Notes 
  - Today we will be synthesizing sound using primitive wave types, and learning
    about structured data in JavaScript.
      🟪️ `act`, `event`, `synth`
      📜 basic data types: `string`, `boolean`, `number`
         composite types: `array`, `object`
    - Exercises -
    1. [] Writing text and basic math. 
    2. [] Keyboard input as a trigger. 🟩🟦
    3. [] Synthesizing sound. 🎹
    4. [] Growing visually and sonically. 🪱

    ⌨️ Useful `Visual Studio Code` shortcuts.
    🍎 + Shift + P -> Format Document
    🍎 + / -> Toggle Comments
*/

let myNumber = 0;
let myOtherNumber = 0;

function paint({ wipe, ink, write }) {
  wipe(myNumber, 273, 283);

  ink("yellow");
  const first = "Brat";
  const space = "  ";
  const second = "summer";
  write(first + space + second, 16, 16);
  ink ("lime");
  write(`${first} ${second}`, 16, 32);
  ink("cyan");
  write(first[4], 16, 56);
  write(first.slice(0,4), 16, 64);
  write(first.toUpperCase(), 16, 80);
  
  ink("red")
  write(1, 16, 96);

  //myNumber = myNumber +1;

  let result = myNumber
  write(result, 16, 110);

  ink("black");
  write (myOtherNumber, 16, 128);
  

}

const notes = ['c', 'd', 'e', 'f', 'g', 'a', 'b'];
const tones = [260, 293, 329, 349, 392, 440, 493];

const sounds = {};

function act({ event: e, sound, help }) {

  notes.forEach((note, index) => {
    if(e.is("keyboard:down:" + note) && e.repeat === false) {
      myNumber = tones[index];
      //const wavetype = help.choose("sine", "square", "triangle");

      sounds[note] = sound.synth({type: "sine", tone: myNumber, duration: Infinity});
    }
  if (e.is("keyboard:up:" + note)){
    sounds[note]?.kill();
  }

});
}
 


 // if(e.is("keyboard:down:" + note) && e.repeat === false) {
   // myNumber = 260;
    //cSound = sound.synth({tone: myNumber, duration: 1});
  //}


//  if (e.is("keyboard:up:c")) {
  //  myNumber = 0;
   // cSound.kill(0.5);
    //cSound = undefined;
  //}
  
  
  
  
 

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function act({ event: e, sound }) {
// Respond to user input here.
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
