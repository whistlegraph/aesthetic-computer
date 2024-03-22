// Metronome, 2022.01.16.17.59

// TODO: Arrange the below:
//       - Add other wavetypes to square and rename it to tone.
//       - Make tone's default parameters random.
//       - Add and load documentation into this file for 'tone'.
//       - Add a sampler so that arbitrary sounds can be loaded and played back with sound.play("name", pitch).for(3, sound.beats)
//       - Remove potential delay from sound starting up?
//       - Make sound start-up optional?
//       - Clean up audio code & api.
//       - Make an "index" disk that gets booted before any remote disks. (Can just be a timed intro for now.)
//       - Global ESC menu.
//       - Video underlay
//       - Color readable spots in Camera that forces you to move.

let flash = false;
const flashColor = [255, 255, 255];

const melody = ["g4", "c4"]; //, "c4", "c4", "d4", "e4", "c4", "d4", "e4", "f4", "g4"];
let melodyIndex = 0;
let square;
let firstBeat = true;

function boot() {}

// 💗 Beat
function beat({ sound, params, store }) {
  // Set the system metronome using `store`.

  let newBpm;
  if (params[0] === "fast") newBpm = 300;
  else if (params[0] === "medium") newBpm = 120;
  else if (params[0] === "slow") newBpm = 80;
  else newBpm = parseInt(params[0]);

  store["metronome:bpm"] = sound.bpm(newBpm || store["metronome:bpm"] || 200);
  // console.log("🎼 BPM:", sound.bpm(), "Time:", sound.time.toFixed(2));

  square = sound.synth({
    type: "square",
    tone: melody[melodyIndex],
    //beats: 1,
    beats: 1,
    attack: 0.01,
    decay: 0.9,
    volume: 0.2,
    pan: 0,
  });

  sound.synth({
    // TODO: Add a delay here so sounds can be arranged
    //       to start part-way through a beat?
    type: "square",
    tone: 200,
    beats: 1 / 2,
    attack: 0.01,
    decay: 0.9,
    volume: 1,
    pan: 0,
  });

  flash = true;
  flashColor.fill(255);
  firstBeat = false;

  melodyIndex = (melodyIndex + 1) % melody.length;
}

let squareP = 0;

// 🧮 Sim(ulate)
function sim({ sound: { time } }) {
  if (square) {
    // Calculate progress of square.
    const p = square.progress(time);
    squareP = p;
    flashColor.fill(Math.floor((1 - p) * 255) / 4, 0, 3);
    if (p === 1) flash = false; // TODO: This might be skipping 1 frame.
  }
}

// 🎨 Paint
const paint = ({ wipe, ink, line, screen, num: { lerp } }) => {
  wipe(flash ? flashColor : 0);

  const baseAngle = -90;
  const left = baseAngle - 20;
  const right = baseAngle + 20;

  let angle =
    melodyIndex === 0 ? lerp(left, right, squareP) : lerp(right, left, squareP);

  if (firstBeat) angle = left;

  ink(255).lineAngle(
    screen.width / 2,
    screen.height - screen.height / 4,
    screen.height / 2,
    angle,
  );
};

export { boot, beat, sim, paint };

// 📚 Library
// ...
