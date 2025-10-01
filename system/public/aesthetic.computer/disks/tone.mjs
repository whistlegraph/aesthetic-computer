// Tone, 2023.6.20.18.36.12
// Make a single tone in a specified frequency and wave type.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  + Done
  - [x] Make the whole `tone` screen into a single button so it can be
        held down.
#endregion */

let tone, type, toneSound, d;

function boot({ wipe, slug, params, colon, hud, sound: { synth }, num, help }) {
  wipe(0, 0, 0);
  tone = params[0] || num.randIntRange(50, 4000); // TODO: Make the default random and fill out
  //                                the param on top?
  type = colon[0] || "sine"; // help.choose("sine", "triangle", "square", "sawtooth");

  let label = "tone";
  if (colon[0]) label = label + ":" + colon[0];
  label += " " + tone;
  hud.label(label);

  synth({ type, tone, volume: 1.0, duration: 0.25, attack: 0.1, decay: 0.99 });
}

function sim({ colon, simCount, jump, leaving, num, sound }) {
  if (colon[0] === "cycle" && !leaving() && simCount >= 1n)
    jump(`tone:cycle~${num.randIntRange(400, 500)}`, true);

  sound?.speaker?.poll?.();
}

function paint({ wipe, num, api, sound, screen, help }) {
  // Executes every display frame.
  if (toneSound) {
    wipe(255 * d);
  } else {
    wipe(0);
  }

  const speaker = sound?.speaker;
  const paintWaveform = sound?.paint?.waveform;
  if (!speaker || !paintWaveform) return;

  const waveformSource = speaker.waveforms?.left?.length
    ? speaker.waveforms.left
    : speaker.waveforms?.right;

  if (!waveformSource?.length) return;

  const amplitudeSource = speaker.amplitudes?.left?.length
    ? speaker.amplitudes.left
    : speaker.amplitudes?.right;

  let amplitude = 0;
  if (Array.isArray(amplitudeSource) && amplitudeSource.length) {
    amplitude = num?.arrMax ? num.arrMax(amplitudeSource) : 0;
  } else if (typeof amplitudeSource === "number") {
    amplitude = amplitudeSource;
  }

  const scope = Math.min(waveformSource.length, screen.width * 2);
  const samples =
    typeof help?.resampleArray === "function"
      ? help.resampleArray(waveformSource, scope)
      : waveformSource.slice(0, scope);

  const alpha = toneSound ? 176 : 96;
  paintWaveform(
    api,
    amplitude || 0.1,
    samples,
    0,
    0,
    screen.width,
    screen.height,
    [255, 255, 255, alpha],
  );
}

function act({ event: e, sound: { synth }, num, screen }) {
  if (e.is("touch")) {
    // playing
    d = num.map(num.dist(e.x, e.y, screen.width / 2, screen.height / 2), 0, screen.height / 2, 1, 0);
    toneSound = synth({ type, tone, volume: d, duration: "🔁", attack: 0.1, decay: 0.99 });
  } else if (e.is("lift")) {
    // stop playing
    toneSound?.kill(0.25);
    toneSound = null;
    d = 0;
  } else if (e.is('draw') && toneSound) {
    d = num.map(num.dist(e.x, e.y, screen.width / 2, screen.height / 2), 0, screen.height / 2, 1, 0);
    toneSound.update({ volume: d });
  }
}

export { boot, sim, paint, act };
