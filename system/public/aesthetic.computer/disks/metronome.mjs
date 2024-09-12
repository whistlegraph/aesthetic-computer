// Metronome, 2022.01.16.17.59

/* #region 🟢 TODO 
  - [🟠] Show the bpm rate visually and use arrow keys for dynamic adjustment.
  - [] Add a really nice drum sound.
  - [] Decouple the sound length from the animation... maybe by playing a silent
       sound that can be used for timing? Should there be a "silence" wave type?
  - [] Add other wavetypes to square and rename it to tone.
  - [] Add a sampler so that arbitrary sounds can be loaded and played back with sound.play("name", pitch).for(3, sound.beats)
  - [] Remove potential delay from sound starting up?
  - [] Make sound start-up optional?
  - [] Clean up audio code & api.
  - [] Make an "index" disk that gets booted before any remote disks. (Can just be a timed intro for now.)
  - [] Global ESC menu.
  - [] Video underlay
  - [] Color readable spots in Camera that forces you to move.
#endregion */


let flash = false;
const flashColor = [255, 255, 255];

const melody = [200, 300]; //, "c4", "c4", "d4", "e4", "c4", "d4", "e4", "f4", "g4"];
let melodyIndex = 0;
let square;
let firstBeat = true;

function boot({ sound }) {
  sound.skip(); // 💀 Send a signal to skip to the next beat.
}

// 💗 Beat
function beat({ sound, params, store }) {
  // Set the system metronome using `store`.

  let newBpm;
  if (params[0] === "fast") newBpm = 300;
  else if (params[0] === "medium") newBpm = 120;
  else if (params[0] === "slow") newBpm = 80;
  else newBpm = parseInt(params[0]);

  store["metronome:bpm"] = sound.bpm(newBpm || store["metronome:bpm"] || 180);

  // console.log("🎼 BPM:", sound.bpm(), "Time:", sound.time.toFixed(2));

  sound.synth();

  square = sound.synth({
   type: "square",
   tone: melody[melodyIndex],
   beats: 1,
   volume: 0.5,
   pan: 0, // Should I pan left or right on every other beat?
  });

  flash = true;
  flashColor.fill(255);
  firstBeat = false;

  melodyIndex = (melodyIndex + 1) % melody.length;
}

let squareP = 0;

function sim({ sound: { time } }) {
  if (square) {
    const p = square.progress(time);
    squareP = p;
    flashColor.fill(Math.floor((1 - p) * 255) / 4, 0, 3);
    if (p === 1) flash = false; // TODO: This might be skipping 1 frame.
  }
}

function paint({ wipe, ink, line, screen, num: { lerp } }) {
  wipe(0);
  // wipe(flash ? flashColor : 0);

  if (!square) return;

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
}

export { boot, beat, sim, paint };

// 📚 Library
// ...
