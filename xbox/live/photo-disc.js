let previousDown = [];
let lastAdvance = 0;

function boot() {
  telemetry("PHOTO_DISC_BOOT", "scan");
  discScan();
}

function pressed(name, down) {
  return down.includes(name) && !previousDown.includes(name);
}

function sim() {
  const state = disc();
  const down = gamepad().down;
  if (pressed("Y", down)) discScan();
  if (pressed("X", down)) discCopy();
  if (state.count > 0 && state.status !== "scanning") {
    if (pressed("ArrowLeft", down) || pressed("LeftShoulder", down)) {
      discShow(state.index - 1);
      lastAdvance = runtime().monotonicUs;
    } else if (pressed("ArrowRight", down) || pressed("RightShoulder", down) ||
        pressed("A", down)) {
      discShow(state.index + 1);
      lastAdvance = runtime().monotonicUs;
    }
  }
  const now = runtime().monotonicUs;
  if (state.currentReady && state.count > 1 &&
      (lastAdvance === 0 || now - lastAdvance >= 7000000)) {
    discShow(state.index + 1);
    lastAdvance = now;
  }
  previousDown = down.slice();
}

function paint() {
  const state = disc();
  wipe(5, 5, 8);

  if (state.currentReady && state.width > 0 && state.height > 0) {
    const availableWidth = 1840;
    const availableHeight = 930;
    const scale = Math.min(availableWidth / state.width, availableHeight / state.height);
    const width = Math.max(1, state.width * scale);
    const height = Math.max(1, state.height * scale);
    discPhoto((1920 - width) / 2, (990 - height) / 2, width, height);
  } else {
    systemGlyph("Pictures", 850, 330, 220, 80, 90, 115);
    systemWrite(state.status === "scanning" ? "SEARCHING PHOTO CD" :
      state.status === "empty" ? "NO PHOTOS FOUND" :
      state.status.startsWith("error") ? "DISC NOT MOUNTED" :
      state.status.startsWith("decode-error") ? "IMAGE COULD NOT DECODE" :
      "LOADING PHOTO", 640, 590, 46, 235, 235, 242);
  }

  box(0, 990, 1920, 90, 10, 11, 18);
  const position = state.count > 0 ? (state.index + 1) + " / " + state.count : "0 / 0";
  systemWrite(position, 42, 1008, 30, 255, 225, 95);
  systemWrite((state.name || state.status || "PHOTO DISC").slice(0, 72),
    205, 1008, 27, 238, 238, 245);
  const copy = state.copyStatus === "copying" ?
    "COPY " + state.copied + "/" + state.count :
    state.copyStatus === "complete" ? "COPIED " + state.copied :
    "A/NEXT  DPAD/BROWSE  X/COPY  Y/RESCAN";
  write(copy, 1210, 1022, 12, 155, 180, 215);
}

function act(button) {
  telemetry("PHOTO_DISC_BUTTON", button);
}

function leave() {
  telemetry("PHOTO_DISC_LEAVE", "ok");
}
