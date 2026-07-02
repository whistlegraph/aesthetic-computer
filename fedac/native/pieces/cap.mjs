// cap, 2026.07.02
// Camera video recorder — point a webcam, hold space (or touch) to
// record a clip. Clips are MP4 tapes: they land in /mnt/tapes/ with the
// audio output mix as the soundtrack, then auto-upload to your account.
//
// The music-video move (requested by @minanimals): play your song on the
// computer (dj / notepat) so every clip carries the track's audio for
// syncing in an editor — hold space for each shot, end up with a pile of
// pre-synced takes on prompt.ac.
//
// USB cams are plug-and-play: the stream thread rescans until a camera
// appears, survives unplugs, and prefers the newest-plugged device — so
// you can switch out cams per shot. F flips the picture 180° for
// bent-back "monitor mode" (the performers see themselves right-side-up
// and the footage matches, since the tape records the screen).
//
// While recording the piece paints ONLY the camera frame: the runtime
// submits the framebuffer to the recorder right after paint, so any UI
// drawn here would be baked into the footage. The red TAPE overlay you
// see on-screen is composited after the submit and stays out of the file.

const CAM_W = 640; // capture aspect from camera.c (4:3)
const CAM_H = 480;

let recording = false;
let flipped = false;
let clipsThisSession = 0;
let bootFrame = 0;
let sys = null; // latched at boot — leave() doesn't receive the api

function boot({ system }) {
  sys = system;
  system?.cameraStart?.();
}

function sim() {
  bootFrame++;
}

function fitRect(screen) {
  // Contain: largest 4:3 rect centered on screen
  let dw = screen.width;
  let dh = Math.floor((dw * CAM_H) / CAM_W);
  if (dh > screen.height) {
    dh = screen.height;
    dw = Math.floor((dh * CAM_W) / CAM_H);
  }
  return {
    x: Math.floor((screen.width - dw) / 2),
    y: Math.floor((screen.height - dh) / 2),
    w: dw,
    h: dh,
  };
}

function paint({ wipe, ink, screen, system, sound }) {
  const ready = system?.cameraReady?.();

  if (!ready) {
    wipe(0, 0, 0);
    const dots = ".".repeat(1 + (Math.floor(bootFrame / 20) % 3));
    ink(180, 180, 180).write("looking for a camera" + dots, { x: 8, y: 12 });
    ink(130, 130, 130).write("plug in a USB cam and it appears here", {
      x: 8,
      y: 24,
    });
    return;
  }

  recording = sound?.tape?.recording?.() || false;

  wipe(0, 0, 0);
  const r = fitRect(screen);
  system.cameraBlit(r.x, r.y, r.w, r.h, 0, flipped ? 1 : 0);

  // Recording: nothing else — keep the footage clean.
  if (recording) return;

  // Idle chrome
  ink(255, 255, 255).write("cap", { x: 6, y: 6 });
  ink(200, 200, 200).write("hold SPACE to record a clip", {
    x: 6,
    y: screen.height - 32,
  });
  ink(130, 130, 130).write("F flips the picture (monitor mode)", {
    x: 6,
    y: screen.height - 22,
  });
  ink(130, 130, 130).write("play your song first — clips carry its audio", {
    x: 6,
    y: screen.height - 12,
  });
  if (flipped) {
    ink(120, 220, 255).write("flipped", { x: 34, y: 6 });
  }
  if (clipsThisSession > 0) {
    const label = `${clipsThisSession} clip${clipsThisSession === 1 ? "" : "s"} → tapes`;
    ink(120, 220, 255).write(label, { x: screen.width - label.length * 6 - 6, y: 6 });
  }
}

function startClip(system) {
  if (recording) return;
  if (system?.tapeStart?.()) recording = true;
}

function stopClip(system) {
  if (!recording) return;
  if (system?.tapeStop?.()) {
    recording = false;
    clipsThisSession++;
  }
}

function act({ event: e, system }) {
  // Hold space (or hold a touch) to record; release to cut.
  if (e.is("keyboard:down:space") && !e.repeat) startClip(system);
  if (e.is("keyboard:up:space")) stopClip(system);
  if (e.is("touch")) startClip(system);
  if (e.is("lift")) stopClip(system);
  // F rotates the view 180° — bent-back screen becomes a performer monitor.
  if (e.is("keyboard:down:f") && !e.repeat) flipped = !flipped;
}

function leave() {
  if (recording) stopClip(sys);
  sys?.cameraStop?.();
}

export { boot, sim, paint, act, leave };
