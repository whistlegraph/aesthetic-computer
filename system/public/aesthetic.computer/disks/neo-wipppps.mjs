// Wipppps, 2025.3.18.04.34.58.007
// Visualizations for `wipppps` musical tracks.

/* 📝 Notes
 */

let sfx, sfxData, progress, playingSfx, comp, xml;

export async function boot({ net: { preload }, sound, params }) {
  /*
  if (params[0] === "zzzZWAP") {
    sfx = await preload(
      "https://assets.aesthetic.computer/wipppps/zzzZWAP.ogg",
    );
  } else if (params[0] === "WHoOSH") {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/WHoOSH.ogg");
  } else if (params[0] === "BLURP" || true) {
    sfx = await preload("https://assets.aesthetic.computer/wipppps/BLURP.ogg");
    xml = await preload("https://assets.aesthetic.computer/wipppps/BLURP");
    console.log("🔴 XML:", xml);
  }
  sound.getSampleData(sfx).then((data) => {
    sfxData = data;
    // console.log("🔴 Sample Data:", sfxData);
  });
  */
}


export function paint({
  api,
  wipe,
  ink,
  line,
  screen,
  box,
  circle,
  pen,
  sound,
  help,
  write,
  num,
}) {
  // console.log(api); // Log the API or enter `docs` (WIP) in `prompt`.
  wipe("blue"); // Clear the background.

  // wipe(sfx.data.amplitude || 127);

  // let x = 0;
  // bgm.data.sample.forEach((smp) => {
  //   ink(smp, 0, 0);
  //   line(x, screen.height, x, screen.height - smp);
  //   x += 1;
  // });

  /*
  const startY = 0;

  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 1),
    0,
    startY,
    screen.width / 4,
    screen.height - startY,
    undefined, // [255, 0, 0, 255],
    { noamp: true, primaryColor: "red", secondaryColor: "blue" },
  );

  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 2),
    (screen.width / 4) * 1,
    startY,
    (screen.width / 4) * 1,
    screen.height - startY,
    undefined, // [255, 0, 0, 255],
    { noamp: true, primaryColor: "blue", secondaryColor: "red" },
    // { noamp: true, primaryColor: "red", secondaryColor: "blue" },
  );

  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 3),
    (screen.width / 4) * 2,
    startY,
    screen.width / 4,
    screen.height - startY,
    undefined, // [255, 0, 0, 255],
    { noamp: true, primaryColor: "red", secondaryColor: "blue" },
  );

  sound.paint.bars(
    api,
    sound.speaker.amplitudes.left,
    help.resampleArray(sound.speaker.waveforms.left, 4),
    (screen.width / 4) * 3,
    startY,
    screen.width / 4,
    screen.height - startY,
    undefined, // [255, 0, 0, 255],
    { noamp: true, primaryColor: "blue", secondaryColor: "red" },
    // { noamp: true, primaryColor: "red", secondaryColor: "blue" },
  );

  if (sfxData) {
    // comp ||= num.arrCompress(sfxData, 256);
    // sound.paint.waveform(
    //   api,
    //   num.arrMax(sfxData),
    //   comp, // 🔴 TODO: This could be made much faster.
    //   0,
    //   0,
    //   screen.width,
    //   screen.height,
    //   [255, 255, 255, 32],
    //   { direction: "left-to-right" },
    // );
  }

  if (progress) {
    const fromX = 0;
    const toX = screen.width;
    const progressX = num.clamp(fromX + progress * (toX - fromX), fromX, toX);
    ink("red").line(progressX, 0, progressX, screen.height);
  }
  */
}

export function sim({ sound }) {
  /*
  sound.speaker?.poll();
  playingSfx?.progress().then((p) => {
    progress = p.progress;
    // console.log("📏 Progress:", progress);
  }); // Get progress data.
  */
}

export function act({ event: e, sound }) {
  // Respond to user input here.
  // if (e.is("touch")) playingSfx = sound.play(sfx, { speed: 1 });
}

export function meta() {
  return {
    title: "wipppps by @oskie"
  };
}

export const nohud = true;

// function beat() {
//   // Runs once per system metronome (BPM) tick.
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
