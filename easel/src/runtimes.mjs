// runtimes.mjs — the piece languages Aesthetic Computer can run.
//
// A piece is JavaScript, KidLisp, or Lua, chosen by file extension. The blank
// templates are the starting point for a session's piece: each one paints
// something on its own so a phone pointed at the QR code has an image
// immediately, before the agent has written anything.

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

function paint({ wipe, ink, screen }) {
  wipe(70, 50, 100);
  ink(255, 100, 255).write("${name}", { center: "xy" });
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
(wipe "purple")
(ink "pink")
(write "${name}" 6 6)
`,
  },
  lua: {
    id: "lua",
    label: "processing",
    extension: ".lua",
    mime: "text/x-lua; charset=utf-8",
    // Lua is the language of L5, Aesthetic Computer's Processing surface, so a
    // piece here is written in Processing's vocabulary — setup and draw, not
    // paint — against `lib/l5.mjs`.
    //
    // The shape of the blank is load-bearing. A push over the code channel
    // carries only { piece, source, codeChannel }: no extension, no language.
    // The client recognises Lua in that message by reading the source — it must
    // begin with a `--` comment AND declare `function setup(` or `function
    // draw(` — and anything else is compiled as JavaScript and fails. Both are
    // natural in real L5, and this blank keeps them.
    routable: false,
    blank: (name) => `-- ${name}, ${stamp()}
-- A blank Aesthetic Computer piece, in Processing (L5).

function setup()
  noStroke()
end

function draw()
  background(70, 50, 100)
  fill(255, 100, 255)
  text("${name}", 6, 16)
end
`,
  },
};

// Some runtimes answer to more than one name. Lua is the language; Processing
// is what it is for.
export const RUNTIME_ALIASES = { processing: "lua", l5: "lua", js: "mjs", kidlisp: "lisp" };

export const DEFAULT_RUNTIME = "mjs";

function stamp(date = new Date()) {
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
// where the two differ. `lua` is the one worth spelling out — nobody comes
// looking for Lua, they come looking for Processing.
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
