// runtimes.mjs — the piece languages Aesthetic Computer can run.
//
// A piece is JavaScript, KidLisp, Lua, or a p5.js sketch, chosen by file
// extension. The blank
// templates are the starting point for a session's piece: each one paints a
// flat color on its own so a phone pointed at the QR code has an image
// immediately, before the agent has written anything. The blank does not write
// its own name — the system's corner label already shows it, and a name left
// in the source tends to survive the agent's first edit as clutter.

const randomBackdrop=()=>Array.from({length:3},()=>Math.floor(Math.random()*256));
const randomCSSBackdrop=()=> '#'+randomBackdrop().map(n=>n.toString(16).padStart(2,'0')).join('');

export const RUNTIMES = {
  mjs: {
    id: "mjs",
    label: "javascript",
    extension: ".mjs",
    mime: "application/javascript; charset=utf-8",
    // The handled-piece loader resolves .mjs and .lisp under an @handle.
    routable: true,
    blank: (name) => `// ${name}, ${stamp()}
// A blank Aesthetic Computer piece.

function paint({ wipe }) {
  wipe("${randomCSSBackdrop()}");
  return false; // Painted once; return true to keep painting.
}

export { paint };
`,
  },
  lisp: {
    id: "lisp",
    label: "kidlisp",
    extension: ".lisp",
    mime: "text/x-lisp; charset=utf-8",
    routable: true,
    blank: (name) => `; ${name}, ${stamp()}
(wipe "${randomCSSBackdrop()}")
`,
  },
  l5: {
    id: "l5",
    label: "l5",
    extension: ".l5",
    mime: "text/x-lua; charset=utf-8",
    // L5 is Aesthetic Computer's own Processing, in Lua: a piece here is
    // written in Processing's vocabulary — setup and draw, not paint —
    // against `lib/l5.mjs`.
    //
    // The shape of the blank is load-bearing. A push over the code channel
    // carries only { piece, source, codeChannel }: no extension, no language.
    // The client recognises Lua in that message by reading the source — it must
    // begin with a `--` comment AND declare `function setup(` or `function
    // draw(` — and anything else is compiled as JavaScript and fails. Both are
    // natural in real L5, and this blank keeps them. Published, it answers at
    // @handle/slug: the handled-piece loader falls back to .l5.
    routable: true,
    blank: (name) => `-- ${name}, ${stamp()}
-- A blank L5 piece: Processing, in Lua.

function setup()
  noStroke()
end

function draw()
  background(${randomBackdrop().join(", ")})
end
`,
  },
  p5: {
    id: "p5",
    label: "processing",
    extension: ".p5",
    mime: "application/javascript; charset=utf-8",
    // A `.p5` piece is a global-mode p5.js sketch, run on real p5.js inside
    // the piece worker by `lib/p5-worker.mjs`: it draws into an OffscreenCanvas
    // that AC composites like any other piece. The extension is ours, so what
    // runs a `.p5` can change without renaming anyone's pieces.
    //
    // As with Lua, the shape of the blank is load-bearing. A push carries no
    // extension, so the client takes the source for p5 only when it declares a
    // top-level `function setup(` or `function draw(` and exports nothing — an
    // `export` makes it an AC piece. Published, it answers at @handle/slug:
    // the handled-piece loader falls back to .p5 when no .mjs is there.
    routable: true,
    blank: (name) => `// ${name}, ${stamp()}
// A blank p5.js sketch.

function setup() {
  createCanvas(windowWidth, windowHeight);
  noStroke();
}

function draw() {
  background(${randomBackdrop().join(", ")});
}

function windowResized() {
  resizeCanvas(windowWidth, windowHeight);
}
`,
  },
};

// Some runtimes answer to more than one name. `processing` means p5 — real
// p5.js, the Processing most people already write. L5 is Aesthetic
// Computer's own Processing in Lua, and answers to its own name.
export const RUNTIME_ALIASES = { processing: "p5", p5js: "p5", "p5.js": "p5", lua: "l5", js: "mjs", kidlisp: "lisp" };

export const DEFAULT_RUNTIME = "mjs";

export function stamp(date = new Date()) {
  const pad = (value) => String(value).padStart(2, "0");
  return [
    String(date.getFullYear()).slice(2),
    pad(date.getMonth() + 1),
    pad(date.getDate()),
  ].join(".");
}

export function runtimeIds() {
  return Object.keys(RUNTIMES);
}

// How the runtimes read in the interface: the id to type, named by what it is
// where the two differ. `p5` is the one worth spelling out — people come
// looking for Processing.
export function runtimeMenu() {
  return runtimeIds()
    .map((id) => (RUNTIMES[id].label === id ? id : `${id} (${RUNTIMES[id].label})`))
    .join(", ");
}

export function runtimeFor(id) {
  const wanted = String(id || "").replace(/^\./, "").toLowerCase();
  const runtime = RUNTIMES[RUNTIME_ALIASES[wanted] || wanted];
  if (!runtime) {
    throw new Error(`unknown runtime "${id}" — try ${runtimeIds().join(", ")}`);
  }
  return runtime;
}

export function runtimeForExtension(extension) {
  const wanted = String(extension || "").toLowerCase();
  return Object.values(RUNTIMES).find((runtime) => runtime.extension === wanted) || null;
}
