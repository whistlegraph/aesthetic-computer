// ucla-4, 24.07.11.19.08 
// Intermediate graphics and modal logic.

/* 📝 Notes 
  - Today we will be exploring interactive graphics through making
    a worm, among other things. 🪱 
      🟪️ `box`, `shape`, `synth`.
      📜 Features of: `array`, `object`, and `function`.
    - Exercises -
    1. [x] 🥁 Drum sound! (`synth` and `act` review) 
    2. [] Rectangle painting and the backbuffer. (graphics review)
    3. [] What is a mode and how to design with modes...
    2. [] Leading and following! 🟢🟥
    4. [] Expanding worm. 🪱
*/

function paint({ wipe, ink, write }) {
  wipe("pink");
  ink("black");
  write("y:" + yPercentage, 6, 20);
  ink("blue");
  write("x:" + xPercentage, 6, 32);
}

let yPercentage;
let xPercentage;

function act({ event: e, sound, screen }) {
// Respond to user input here.
  if(e.is("touch")) {
    yPercentage = e.y / screen.height;

    xPercentage = e.x / screen.width;

    console.log(yPercentage);

    const intensity = 1 - yPercentage; // 1 - 0->1
    const panning = xPercentage * 2 - 1; // -1 ---------------- 1
    //                              0
    //                              ^ center mix of LEFT AND RIGHT

    // How do we take 'xPercentage' which is from 0 -> 1 and map it to -1 -> 1
    // When xPercentage is 0.5 we want panning to be 0.
    // When xPercentage is 1 we want panning to be 1.
    // When xPercentage is 0 we want panning to be -1.

    // RANGE for xPercentage is 1
    // RANGE for pan is 2

    // 🚩 Kick Drum

    // Multi-line edit...
    // Hold CMD and click before the 'v' in each line.
    // Hit escape to leave multi-cursors.

    // High
    sound.synth({pan: panning, volume: 1 * intensity, type: "triangle", tone: 170, duration: 0.1 + 0.05});
    sound.synth({pan: panning, volume: 1 * intensity, type: "triangle", tone: 160, duration: 0.1 - 0.03});
    sound.synth({pan: panning, volume: 1 * intensity, type: "triangle", tone: 130, duration: 0.1 + 0.02});
    sound.synth({pan: panning, volume: 1 * intensity, type: "triangle", tone: 150, duration: 0.1 - 0.01});
    sound.synth({pan: panning, volume: 1 * intensity, type: "triangle", tone: 190, duration: 0.1 + 0.04});

    // Low
    sound.synth({pan: panning, volume: 1 * intensity, type: "square", tone: 70, duration: 0.4, attack: 0, decay: 1});
    sound.synth({pan: panning, volume: 0.5 * intensity, type: "square", tone: 80, duration: 0.2, attack: 0, decay: 1});
    sound.synth({pan: panning, volume: 0.5 * intensity, type: "square", tone: 90, duration: 0.2, attack: 0, decay: 1});
    sound.synth({pan: panning, volume: 1 * intensity, type: "sine", tone: 100, duration: 0.4, attack: 0, decay: 0.4});
    sound.synth({pan: panning, volume: 1 * intensity, type: "sine", tone: 150, duration: 0.2 - 0.05, attack: 0, decay: 0.8});
    sound.synth({pan: panning, volume: 1 * intensity, type: "sine", tone: 130, duration: 0.2 + 0.05, attack: 0, decay: 0.8});

    sound.synth({pan: panning, volume: 1 * intensity, type: "noise-white", duration: 0.05});
    sound.synth({pan: panning, volume: 0.8 * intensity, type: "noise-white", duration: 0.1, attack: 0});
  }
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
