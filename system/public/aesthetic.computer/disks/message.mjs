// message, 26.05.31
// Shows a single chat message or mood, full screen.
//
// Opened by selecting a chat / mood row in the prompt's universal search — the
// payload is handed off through the shared in-memory store (`message:view`).
// Tap, Esc, or Enter to return to the prompt.

let message = null; // { from, text, source }

function boot({ store, params }) {
  message = store["message:view"] || null;
  // Bare fallback so `message hello world` shows something when linked directly.
  if (!message && params?.length) {
    message = { from: "anon", text: params.join(" "), source: "chat" };
  }
}

function paint({ wipe, ink, write, screen }) {
  // Tint the backdrop by source so chat / clock / mood read distinctly.
  const bg =
    message?.source === "mood"
      ? [18, 28, 26]
      : message?.source === "clock"
        ? [28, 22, 12]
        : [14, 14, 24];
  wipe(bg);

  if (!message) {
    ink(160).write("no message", { center: "xy" });
    return;
  }

  const margin = 12;
  const wrap = screen.width - margin * 2;

  // Author (left) + source tag (right) header.
  ink(150, 200, 255).write(`@${message.from}`, { x: margin, y: margin });
  const tag = message.source || "message";
  ink(110).write(
    tag,
    { x: screen.width - margin - tag.length * 4, y: margin + 1 },
    undefined,
    undefined,
    false,
    "MatrixChunky8",
  );

  // Message body — wrapped beneath the header.
  ink(240).write(message.text, { x: margin, y: margin + 18 }, undefined, wrap);

  // Return hint.
  ink(90).write(
    "tap or esc to go back",
    { center: "x", y: screen.height - 12 },
    undefined,
    undefined,
    false,
    "MatrixChunky8",
  );
}

function act({ event: e, jump }) {
  if (
    e.is("touch") ||
    e.is("keyboard:down:escape") ||
    e.is("keyboard:down:enter")
  ) {
    jump("prompt");
  }
}

function meta() {
  return { title: "message", desc: "A single message, full screen." };
}

export const nohud = true;

export { boot, paint, act, meta };
