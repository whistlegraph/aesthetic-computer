// Pruttivox, 26.09.09.09.29
// Read a reviewed text in Prutti's consented synthetic voice.

const CHUNK_LIMIT = 1100;
const MAX_TEXT_LENGTH = 12000;
const SYNTHETIC_TAG = "Pruttivox. Syntetisk stemme.";

let text = "";
let chunks = [];
let chunkIndex = 0;
let requestId = null;
let status = "idle";

function splitPoint(value, limit) {
  const window = value.slice(0, limit + 1);
  const minimum = Math.floor(limit * 0.4);
  let point = window.lastIndexOf("\n\n");

  const sentenceMatches = [...window.matchAll(/[.!?…]["'”’)]*\s+/g)];
  const sentence = sentenceMatches.at(-1);
  if (sentence && sentence.index + sentence[0].length > point) {
    point = sentence.index + sentence[0].length;
  }

  if (point < minimum) point = window.lastIndexOf("\n");
  if (point < minimum) point = window.lastIndexOf(" ");
  return point >= minimum ? point : limit;
}

function chunkText(value, limit = CHUNK_LIMIT) {
  const result = [];
  let remaining = value.trim();

  while (remaining.length > limit) {
    const point = splitPoint(remaining, limit);
    const chunk = remaining.slice(0, point).trim();
    if (chunk) result.push(chunk);
    remaining = remaining.slice(point).trim();
  }

  if (remaining) result.push(remaining);
  return result;
}

function boot({ params, hud }) {
  text = params.join(" ").trim();
  chunks = text.length <= MAX_TEXT_LENGTH ? chunkText(text) : [];
  hud.label("pruttivox", "magenta");
}

function queueChunk(speak) {
  const queue = [...chunks, SYNTHETIC_TAG];
  speak(queue[chunkIndex], "neutral:0", "cloud", {
    provider: "prutti",
    volume: 1,
    requestId,
    chunkIndex,
    chunkCount: queue.length,
  });
}

function start(speak) {
  if (!text || text.length > MAX_TEXT_LENGTH || status === "speaking") return;
  requestId = globalThis.crypto?.randomUUID?.()
    || `pruttivox-${Date.now()}-${Math.random().toString(36).slice(2)}`;
  chunks = chunkText(text);
  chunkIndex = 0;
  status = "speaking";
  queueChunk(speak);
}

function paint({ wipe, ink, screen }) {
  wipe(24, 12, 30);

  ink(255, 210, 238).write("SYNTETISK STEMME", { center: "x", screen, y: 42 });

  if (!text) {
    ink(190, 160, 185).write("pruttivox <text>", { center: "xy", screen });
  } else if (text.length > MAX_TEXT_LENGTH) {
    ink(255, 110, 110).write(`TEXT IS OVER ${MAX_TEXT_LENGTH} CHARACTERS`, {
      center: "xy",
      screen,
    });
  } else {
    const preview = text.replace(/\s+/g, " ");
    const visible = preview.length > 520 ? `${preview.slice(0, 519)}…` : preview;
    ink(255, 245, 250).write(visible, {
      x: 24,
      y: 84,
    }, undefined, Math.max(120, screen.width - 48), true);
  }

  let footer = "TAP TO SPEAK";
  let color = [190, 160, 185];
  if (status === "speaking") {
    footer = `SPEAKING ${Math.min(chunkIndex + 1, chunks.length + 1)} / ${chunks.length + 1}`;
    color = [190, 255, 100];
  } else if (status === "done") {
    footer = "COMPLETE · TAP TO REPLAY";
    color = [190, 255, 100];
  } else if (status === "error") {
    footer = "UNAVAILABLE · SIGN IN AS AN APPROVED PRODUCER";
    color = [255, 110, 110];
  }
  ink(...color).write(footer, { center: "x", screen, y: screen.height - 24 });
}

function act({ event: e, speak }) {
  if (
    e.is("touch")
    || e.is("keyboard:down:space")
    || e.is("keyboard:down:enter")
  ) {
    start(speak);
  }

  if (e.is("speech:completed") && status === "speaking") {
    chunkIndex += 1;
    if (chunkIndex <= chunks.length) queueChunk(speak);
    else status = "done";
  }

  if (e.is("speech:error") && e.content?.provider === "prutti") {
    status = "error";
  }
}

function meta() {
  return {
    title: "Pruttivox",
    desc: "Read reviewed text in Prutti's consented synthetic voice.",
  };
}

export { act, boot, chunkText, meta, paint };
