// Aesel, the donkey at the easel.
//
// The terminal uses an ASCII version of the pixel-art donkey mascot. He walks in from the left
// edge when a session opens, waves once he arrives, and then stands in the
// bottom-left corner for the rest of the session — the opposite corner from
// the QR code, which owns the right.
//
// Everything here is pure. A caller asks what the guy looks like at a given
// number of milliseconds and gets back lines; nothing schedules, nothing draws,
// and nothing reads a clock. That keeps him testable frame by frame and keeps
// the decision about how often to repaint with the interface that owns the
// screen.

export const MASCOT_WIDTH = 28;
export const MASCOT_HEIGHT = 8;

// Aesel painting at an easel. Fixed cells keep the canvas and hooves planted.
const stand = [
  "   /\\ /\\             /\\",
  "  ( o o )           /  \\",
  "  /  ^  \\    __    /----\\",
  "  \\_____/---/ /-->| *  |",
  "   /|     |/      |____|",
  "  / |_____|         ||",
  "    / / \\ \\        /  \\",
  "   /_/   \\_\\      /____\\",
];
const pose = (changes={}) => stand.map((line,i)=>(changes[i]??line).padEnd(MASCOT_WIDTH));
const POSES = {
  stand: pose(),
  step: pose({6:"     /|  |\\        /  \\",7:"    /_|  |_\\      /____\\"}),
  wave: pose({0:"   /\\ //             /\\",3:"  \\_____/---/ /--> | *  |"}),
  blink: pose({1:"  ( - - )           /  \\"}),
};
const ONE_ROW = {stand:"//o>",work:["//o>","\\\\o>","//o>","//->"]};
export const MASCOT_ROW_WIDTH = 4;

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

// The sprite as rows of {text, tone} so the renderer can paint the ears in
// pink and the body in the interface's soft purple without this module
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
