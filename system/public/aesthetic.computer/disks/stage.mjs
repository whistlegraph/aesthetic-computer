// Stage, 2021

// TODO
// * See: https://www.notion.so/whistlegraph/9abf76a4437a46199087176bd06657ea?v=98b5d3a8965e47d9a4c177dd5147960d

// Graphics
let cam, arrow;

const camZ = 10;

const blocks = {};
const blocksX = { A: -6, B: -4, C: -2, D: 0, E: 2, F: 4, G: 6 };
const blocksY = { A: 0, B: 0, C: 0, D: 0, E: 0, F: 0, G: 0 };

const blocksColors = {
  A: [255, 0, 0],
  B: [255, 127, 0],
  C: [255, 255, 0],
  D: [0, 200, 0],
  E: [0, 150, 255],
  F: [155, 0, 255],
  G: [255, 0, 127],
};

let arrowTrack;
let arrowSpin = 0;

let wiggle;

let needsFlash = false;
let flashColor;
let flashFrames = 0;
let flashDuration = 3;

let indicatorBlink = 0;
let indicatorBlinkRate = 16;

// let freezeFrame = false;

// Music
let BPM = 80;

let loopSong = false;

let melody = `
GECFDBECAB_C
EEEEDCDCBBAC
EEAEEAG_FE_E
FDBECAGDBC__
GGCGGCBCBA_C
EEEEDCD_FGGC
GGCG_FE_EFCA
DCBC_C___A_C
B_AD_CB_CEEE
EDCDCBBACEEA
EEAG_FE_EFDB
ECABCBA_____
`.replace(/\s/g, "");

const scale = {
  A: "a2",
  B: "b2",
  C: "c3",
  D: "d3",
  E: "e3",
  F: "f3",
  G: "g3",
};

let notes;
let melodyBeatsTotal;
let melodyBeatsPlayed = 0;

let countIn = 4;
let noteI = -countIn;
let instrument;
let instrumentProgress;
let playingLetter;

// General
const { abs } = Math;

export function boot({
  Camera,
  Form,
  painting,
  QUAD,
  TRI,
  screen,
  params,
  help: { each },
}) {
  // Load some initial parameters.

  // Change from default BPM
  BPM = params.bpm || BPM;

  // Change from default melody
  melody = params.melody || melody;
  melody = melody.replace(/\s/g, "");
  notes = parseMelody(melody);
  melodyBeatsTotal = melody.length;

  // Change whether we loop or not.
  if (params.loop === "true") {
    loopSong = true;
  }

  cam = new Camera(80);

  const aspectRatio = screen.width / screen.height;

  if (aspectRatio < 1.1) {
    cam.z = 14 / aspectRatio;
  } else if (aspectRatio < 1.5) {
    cam.z = 12;
  } else {
    cam.z = camZ; // TODO: Currently hardcoded to 16x9 aspect ratio.
  }

  for (const l of "ABCDEFG") // Add each colored block.
    blocks[l] = new Form(
      QUAD,
      {
        texture: painting(32, 32, (p) => {
          p.wipe(...blocksColors[l]);
        }),
      },
      [blocksX[l], 0, 4], // Position
    );

  each(blocks, (b) => (b.alpha = 0.25));

  // Add indicator arrow.
  arrow = new Form(
    TRI,
    { texture: painting(32, 32, (p) => p.wipe(32)) },
    [blocksX[notes[0].letter], 4, 4],
    [0, 0, 180],
    [0.7, 0.7, 1],
  );
}

let wiggleStartTime;

export function sim({ num: { lerp }, sound: { time, bpm }, help: { each } }) {
  //arrowSpin += 3;
  //arrow.rotation[1] = arrowSpin;

  // console.log(bp);

  // Make sure our main instrument is playing and if it's not then return after
  // checking for the introductory wiggle.
  if (!instrument) {
    // TODO: Pre-calc. beatProgress so that it is always available in the API. 2021.11.28.00.02
    const bpmInSec = 60 / bpm;
    const beatProgress = (time - wiggleStartTime) / bpmInSec;
    if (beatProgress >= 0.5) wiggle?.step((beatProgress - 0.5) / 0.5);
    return;
  }

  const p = Math.min(1, instrument.progress(time)); // Progress of current note.
  instrumentProgress = p;

  // Animate all blocks back to resting position based on p.
  each(blocks, (block, letter) => {
    block.position[1] = lerp(blocksY[letter], 0, p);
  });

  // Animate arrow to next position, if one exists.
  arrowTrack?.step(p * 3);

  // Animate any block wiggles, if they exist.
  if (playDurationProgress === 0) {
    if (p >= 0.5) wiggle?.step((p - 0.5) / 0.5);
  }
}

export function paint({
  ink,
  wipe,
  painting,
  screen: { width, height },
  num: { lerp },
  help: { each },
  box,
  form,
}) {
  // if (freezeFrame === true) return false;
  const songFinished = melodyBeatsPlayed === melodyBeatsTotal + 1;

  // 1. Background
  if (needsFlash) {
    const r = lerp(flashColor[0], 32, 0.95);
    const g = lerp(flashColor[1], 32, 0.95);
    const b = lerp(flashColor[2], 32, 0.95);
    ink(r, g, b);

    flashFrames = (flashFrames + 1) % flashDuration;
    if (flashFrames === 0) {
      needsFlash = false;
    }
  } else {
    if (songFinished && noteI > notes.length) {
      ink(32); // Dark backdrop once the song ends.
    } else {
      ink(48); // Grey backdrop as usual.
    }
  }
  wipe(0);

  // 2. Blocks & Arrow
  each(blocks, (block) => form(block, cam, { cpu: true })); // Paint every block.

  // TODO: Fix this for looping.
  if (noteI < 0 && noteI > -countIn) {
    if (instrumentProgress) {
      const originalArrowY = 4;
      arrow.position[1] = lerp(originalArrowY, 2.25, instrumentProgress);
    } else {
      arrow.position[1] = 2.25;
    }

    form(arrow, cam, { cpu: true }); // Paint arrow.
  }

  if (noteI > 0 && songFinished === false) {
    if (playDurationProgress === 0 && noteI < notes.length) {
      arrow.texture = painting(32, 32, ({ wipe }) => {
        const light = [255, 255, 255].map((n) =>
          lerp(0, n, instrumentProgress),
        );
        wipe(...light);
        // const height = Math.floor(lerp(0, 32, (playIndex + 1) / plays.length));
        // for (let y = 0; y < height; y += 1) {
        //   line(0, y, w, y);
        // }
      });

      const originalArrowY = 4;
      const newArrowY = lerp(originalArrowY, 2.25, instrumentProgress);
      arrow.position[1] = newArrowY;
    }
  }

  // 3. Timeline
  const playHeight = Math.max(3, height * 0.02);
  const playY = Math.ceil(height - playHeight);
  const notesHeight = Math.max(3, height * 0.02);
  const notesY = Math.ceil(playY - notesHeight);
  const indicatorHeight = Math.max(3, height * 0.02);

  // Draw black line in the background.
  ink(0).box(0, playY, width, playHeight);

  // 4. Draw progress of current play.
  if (instrumentProgress >= 0 && songFinished === false) {
    const filteredProgress = Math.min(1, instrumentProgress * 1.0);
    const boxWidth = filteredProgress * width;

    // Draw progress line.
    // or, ...blocksColors[playingLetter]
    ink(127).box(0, playY, boxWidth, playHeight);
  }

  // Draw a line with every color of every block.
  const beatUnit = width / melodyBeatsTotal;

  // let startX = 0;

  // notes.forEach((note, index) => {
  //   const plays = note.plays;

  // TODO: Programming proper breaks should fix this!
  // TODO: Fix rendering bug in playback here: CC_CC___C
  // console.log(plays.length);

  // let playCount = 1;

  // plays.forEach((play) => {
  // let currentDuration = 0;

  // while (currentDuration < play.duration) {
  // Draw full colored blocks adjusting for each duration.
  // const shift = blocksColors[note.letter].map((n) =>
  //   lerp(64, n, 1 - currentDuration / play.duration)
  // );
  // color(...shift); // TODO: Re-enable this once the bugs are fixed.

  // if (currentDuration === 0) {
  //   color(...blocksColors[note.letter]);
  // } else {
  //   const breakColor = blocksColors[note.letter].map((n) =>
  //     lerp(n, 96, 1.0)
  //   );
  //   color(...breakColor);
  // }

  // color(255, 255, 255);
  // box(startX + beatUnit * currentDuration, notesY, beatUnit, notesHeight);
  // currentDuration += 1;
  // }

  // const w = beatUnit * play.duration;

  // Draw darker boxes.
  // if (plays.length > 1 && play.duration === 1) {
  //   const dark = blocksColors[note.letter].map((n) => lerp(0, n, 0.5));
  //   color(...dark);
  //   const playProgress = playCount / plays.length;
  //   box(startX, notesY, w, (1 - playProgress) * notesHeight);
  // }

  // playCount += 1;

  // startX += w;
  // });
  // });

  let songProgress = (melodyBeatsPlayed - 1) * beatUnit;

  // Draw song progress, offset by 1 to match the play progress.
  if (noteI >= 0 && songFinished === false && melodyBeatsPlayed > 0) {
    // Light indicator.
    // if (indicatorBlink < indicatorBlinkRate / 2) {
    ink(96, 96, 96);
    // } else {
    // color(255, 255, 255);
    // }

    // indicatorBlink = (indicatorBlink + 1) % indicatorBlinkRate;
    // box(songProgress, notesY, beatUnit - 1, Math.ceil(indicatorHeight));
    box(0, notesY, songProgress + beatUnit, Math.floor(indicatorHeight));

    // Dark covering box.
    // color(32, 32, 32);
    // box(songProgress + 1, notesY, width - (songProgress + 1), notesHeight);
  }

  // freezeFrame = true;
}

let playIndex = 0;
let playDurationProgress = 0;

// 2. Music
export function beat({
  help: { every, each, choose, repeat },
  sound: { bpm, time, synth },
  num: { lerp, Track },
  painting,
}) {
  bpm(BPM);

  // A. Introductory Countdown
  if (noteI < 0) {
    synth({
      tone: 50 - abs(noteI * 10),
      beats: 0.05,
      attack: 0.1,
      decay: 0.1,
      volume: 0.7,
      pan: 0,
    });

    cam.forward(2);
    noteI += 1;

    each(blocks, (b) => {
      b.alpha = lerp(0.25, 1, 1 - abs(noteI) / countIn);
      const scale = lerp(1, 0.85, 1 - abs(noteI) / countIn);
      b.scale[0] = b.scale[1] = scale;
    });

    // Wiggle the first block.
    if (noteI === 0) {
      wiggle = new Track(...wiggleBlock(blocks[notes[noteI].letter], choose));
      wiggleStartTime = time;
    }

    return;
  }

  // B. Melody
  if (noteI < notes.length) {
    // Within the current note.
    const letter = notes[noteI].letter;
    const plays = notes[noteI].plays;
    const play = plays[playIndex];
    playingLetter = letter;

    // If we are looping, then reset melodyBeatsPlayed.
    if (noteI === 0 && playIndex === 0 && playDurationProgress === 0) {
      melodyBeatsPlayed = 0;
    }

    // Within the current play.
    if (playIndex < plays.length) {
      //if (playDurationProgress === 0) {
      // Play a note.

      if (playDurationProgress === 0) {
        instrument = synth({
          tone: scale[letter],
          beats: 1, //play.duration,
          attack: 0.1,
          decay: 0.9,
          volume: 1,
          pan: 0,
        });
      } else {
        instrument = synth({
          tone: scale[letter],
          beats: 1, //play.duration,
          attack: 0.1,
          decay: 0.9,
          volume: 0,
          pan: 0,
        });
      }

      // console.log("Playing:", letter, "Duration:", plays[playIndex]);

      // Reset all blocks.
      every(blocksY, 0);

      // Jump the playing block.

      if (playDurationProgress === 0) {
        blocksY[letter] = 1;
        needsFlash = true;
      }

      // Trigger a screen flash.
      flashColor = blocks[letter].texture.pixels.slice(0, 2);

      // Fill block up with colored lines if it is repeating.
      if (plays.length > 1) {
        blocks[letter].texture = painting(32, 32, ({ wipe, line, width }) => {
          const dark = blocksColors[letter].map((n) => lerp(0, n, 0.5));
          wipe(...dark).ink(...blocksColors[letter]);
          const nh = Math.floor(lerp(0, 32, (playIndex + 1) / plays.length));
          for (let y = 0; y < nh; y += 1) line(0, y, width, y);
        });
      }

      const finalPlay = playIndex === plays.length - 1;

      if (finalPlay) {
        // Start moving the arrow to the next note if this is the final play
        // of a note.
        const nextNote = notes[noteI + 1];
        const nextLetter = nextNote?.letter;
        const nextPlays = nextNote?.plays;
        if (nextLetter) {
          // TODO: Brightly color arrow.
          arrow.position[0] = blocks[nextLetter].position[0];

          // blocks[nextLetter].position[0] += Math.random(0.5);

          // repeat(8, (n) => wiggleValues.push());
          wiggle = new Track(...wiggleBlock(blocks[nextLetter], choose));

          // arrowTrack = new Track(
          //   { from: arrow.position[0], to: blocks[nextLetter].position[0] },
          //   (x) => (arrow.position[0] = x)
          // );

          // Fill block up with a darkened version of its color.
          if (nextPlays.length > 1) {
            blocks[nextLetter].texture = painting(32, 32, ({ wipe }) => {
              const dark = blocksColors[nextLetter].map((n) => lerp(0, n, 0.5));
              wipe(...dark);
            });
          }
        } else {
          // Stop moving arrow if the last note in the melody.
          arrowTrack = undefined;
          wiggle = undefined;
        }
      } else {
        // Stop moving arrow if we are playing the same note
        // more than once.
        arrowTrack = undefined;
      }
      // }

      if (playDurationProgress < play.duration) {
        playDurationProgress += 1;
      }

      if (playDurationProgress === play.duration) {
        playDurationProgress = 0;
        playIndex += 1;
      }
    }

    // Advance to the next note.
    if (playIndex === plays.length) {
      playIndex = 0;
      noteI += 1;
    }

    melodyBeatsPlayed += 1;
  }

  // C: Metronome clicks up till the end of the last note.
  if (noteI < notes.length + 1) {
    synth({
      tone: 10,
      beats: 0.05,
      attack: 0.1,
      decay: 0.1,
      volume: 0.8,
      pan: 0,
    });
  }

  if (noteI === notes.length && loopSong) {
    noteI = 0;

    each(blocks, (b) => (b.alpha = 1));

    // TODO: Brightly color arrow.
    arrow.position[0] = blocksX[notes[0].letter];

    // arrowTrack = new Track(
    //   arrow.position[0],
    //   noteX[notes[0].letter],
    //   (x) => (arrow.position[0] = x)
    // );

    return;
  }

  // D: Ending
  if (noteI === notes.length) {
    noteI += 1;
    return;
  }

  // E: Final Sound
  if (noteI === notes.length + 1) {
    synth({
      tone: 25,
      beats: 0.2,
      attack: 0.01,
      decay: 0.01,
      volume: 1.5,
      pan: 0,
    });
    each(blocks, (b) => (b.alpha = 0));
    noteI += 1;
    melodyBeatsPlayed += 1;
  }
}

export function act({ event: e, net: { web } }) {
  if (e.is("keyboard:down") && e.key === "e") web("/disks");
}

// ⚙️ Utilities

// Parses and builds an array of character sequences with underscores
// marking duration. Leading underscores are ignored!
function parseMelody(notes) {
  const parsedSequence = [];

  let i = 0;
  let play = { duration: 1 };
  let plays = [play];
  let lastLetter;

  while (i < notes.length) {
    const char = notes[i];
    const letter = char !== "_" ? char : undefined;

    if (letter === undefined) {
      play.duration += 1;
    } else if (letter === lastLetter) {
      // If a note repeats then add another play.
      play = { duration: 1 };
      plays.push(play);
    } else {
      if (lastLetter) {
        // Build the note data for the previous letter.
        parsedSequence.push({ letter: lastLetter, plays });
      }

      play = { duration: 1 };
      plays = [play];

      lastLetter = letter;
    }

    i += 1;
  }

  // Build the note data for the last letter of the sequence.
  parsedSequence.push({ letter: lastLetter, plays });

  return parsedSequence;
}

function wiggleBlock({ position }, choose) {
  // TODO: Adding choose as a parameter here feels janky. 2021.11.28.01.28
  const base = position[0];
  return [
    choose([0, -0.1, 0.125, -0.125, 0.1, 0], [0, -0.1, 0.125, -0.125, 0.1, 0]),
    (x) => (position[0] = base + x),
  ];
}
