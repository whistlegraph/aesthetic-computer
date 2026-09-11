// The little guy.
//
// Aesthetic Computer's mark is a period, and oskiewar's fighters are round
// heads on stick bodies, so the harness's mascot is the same creature at
// terminal scale: a pink dot with a body under it. He walks in from the left
// edge when a session opens, waves once he arrives, and then stands in the
// bottom-left corner for the rest of the session — the opposite corner from
// the QR code, which owns the right.
//
// Everything here is pure. A caller asks what the guy looks like at a given
// number of milliseconds and gets back lines; nothing schedules, nothing draws,
// and nothing reads a clock. That keeps him testable frame by frame and keeps
// the decision about how often to repaint with the interface that owns the
// screen.

export const MASCOT_WIDTH = 3;
export const MASCOT_HEIGHT = 3;

// Three rows, three columns, always. A sprite that changes size shifts the
// text beside it, and a mascot that makes the transcript jitter is a bug
// wearing a costume.
const POSES = {
  stand: [" ● ", "╱│╲", "╱ ╲"],
  // Mid-stride: the legs close and the arms swing back, which reads as motion
  // even at three rows because the silhouette changes rather than the position
  // of one pixel.
  step: [" ● ", "╲│╱", " ╽ "],
  wave: ["╲● ", " │╱", "╱ ╲"],
  // A blink is the smallest thing that says the drawing is alive. The head
  // keeps its width so the body does not appear to shift under it.
  blink: [" ▪ ", "╱│╲", "╱ ╲"],
};

// One row, for the footer, where he lives for the rest of the session. Working
// is the only thing worth animating there: it is the one fact the interface
// cannot otherwise tell you without spending a row on it, and a figure that
// moves exactly while the machine is busy needs no label.
const ONE_ROW = {
  stand: "\\●/",
  // Arms down, arms up. Two frames is a dance at this size — the eye reads
  // alternation as effort, and anything more elaborate is noise in a corner.
  work: ["\\●/", "/●\\", "—●—", "/●\\"],
};

export const MASCOT_ROW_WIDTH = 3;

// How fast he dances. Slow enough not to strobe beside text someone is
// reading, fast enough to read as motion rather than a glitch.
const DANCE_MS = 220;

// The one-row guy. `busy` is the only input that changes him: standing when it
// is your turn, dancing while the machine has the floor.
export function mascotRow(elapsed = 0, busy = false) {
  if (!busy) return ONE_ROW.stand;
  const ms = Math.max(0, Number(elapsed) || 0);
  return ONE_ROW.work[Math.floor(ms / DANCE_MS) % ONE_ROW.work.length];
}

// When the footer next needs repainting. Standing costs nothing — the answer
// is null, and the caller can stop asking.
export function mascotRowNextFrameIn(elapsed = 0, busy = false) {
  if (!busy) return null;
  const ms = Math.max(0, Number(elapsed) || 0);
  return DANCE_MS - (ms % DANCE_MS);
}

// Milliseconds. The walk is brisk enough that nobody waits for it and slow
// enough to read as walking; the wave lands after he stops, because waving
// mid-stride looks like falling over.
const STEP_MS = 160;
const WALK_MS = 1120;
const WAVE_MS = 720;
const BLINK_EVERY_MS = 5200;
const BLINK_MS = 140;

// Where he comes to rest, in columns from the left edge of the frame. One
// column of margin so he is not welded to the border.
export const MASCOT_REST_X = 1;

// How far off the left edge he starts. He walks in from outside the frame
// rather than fading in, so the first thing the interface does is move.
const ENTER_X = -MASCOT_WIDTH;

function lerp(from, to, t) {
  return Math.round(from + (to - from) * t);
}

// The whole performance as a function of time. `elapsed` is milliseconds since
// the session opened.
export function mascotAt(elapsed = 0) {
  const ms = Math.max(0, Number(elapsed) || 0);

  if (ms < WALK_MS) {
    const t = ms / WALK_MS;
    return {
      phase: "walking",
      x: lerp(ENTER_X, MASCOT_REST_X, t),
      // Alternating stride. Two poses is enough: the eye fills in the rest.
      lines: Math.floor(ms / STEP_MS) % 2 ? POSES.step : POSES.stand,
    };
  }

  if (ms < WALK_MS + WAVE_MS) {
    const beat = Math.floor((ms - WALK_MS) / (WAVE_MS / 3)) % 2;
    return {
      phase: "waving",
      x: MASCOT_REST_X,
      lines: beat ? POSES.wave : POSES.stand,
    };
  }

  // Standing, with a blink often enough to notice and rarely enough to ignore.
  const since = (ms - WALK_MS - WAVE_MS) % BLINK_EVERY_MS;
  return {
    phase: "idle",
    x: MASCOT_REST_X,
    lines: since < BLINK_MS ? POSES.blink : POSES.stand,
  };
}

// When the entrance is over and he is simply standing there.
export const MASCOT_SETTLED_MS = WALK_MS + WAVE_MS;

// True while the guy is still doing something worth repainting for. Once he is
// standing, the interface only needs to repaint him on the blink, so the caller
// can drop from an animation timer to a lazy one.
export function mascotIsAnimating(elapsed = 0) {
  return (Number(elapsed) || 0) < MASCOT_SETTLED_MS;
}

// When the caller should next repaint, in milliseconds from `elapsed`. During
// the walk that is the next stride; afterwards it is the next blink edge, which
// is seconds away — so an idle session costs two repaints per blink rather than
// a frame timer running forever.
export function mascotNextFrameIn(elapsed = 0) {
  const ms = Math.max(0, Number(elapsed) || 0);
  if (ms < WALK_MS) return STEP_MS - (ms % STEP_MS);
  if (ms < WALK_MS + WAVE_MS) {
    const unit = WAVE_MS / 3;
    return unit - ((ms - WALK_MS) % unit);
  }
  const since = (ms - WALK_MS - WAVE_MS) % BLINK_EVERY_MS;
  return since < BLINK_MS ? BLINK_MS - since : BLINK_EVERY_MS - since;
}

// The sprite as rows of {text, tone} so the renderer can paint the head in the
// mark's pink and the body in the interface's soft purple without this module
// knowing anything about colour.
export function mascotRows(elapsed = 0) {
  const { lines, x, phase } = mascotAt(elapsed);
  return {
    phase,
    x,
    rows: lines.map((text, index) => ({
      text,
      // Row zero carries the head, which is the mark itself.
      tone: index === 0 ? "handle" : "soft",
    })),
  };
}
