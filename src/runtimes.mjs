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
    label: "lua",
    extension: ".lua",
    mime: "text/x-lua; charset=utf-8",
    // Live pushes run, but aesthetic.computer/@handle/<name> only resolves
    // .mjs and .lisp today, so a published .lua has no front door yet.
    routable: false,
    blank: (name) => `-- ${name}, ${stamp()}

function paint()
  wipe(70, 50, 100)
  ink(255, 100, 255)
  write("${name}", 6, 6)
end
`,
  },
};

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

export function runtimeFor(id) {
  const runtime = RUNTIMES[String(id || "").replace(/^\./, "").toLowerCase()];
  if (!runtime) {
    throw new Error(`unknown runtime "${id}" — try ${runtimeIds().join(", ")}`);
  }
  return runtime;
}

export function runtimeForExtension(extension) {
  const wanted = String(extension || "").toLowerCase();
  return Object.values(RUNTIMES).find((runtime) => runtime.extension === wanted) || null;
}
