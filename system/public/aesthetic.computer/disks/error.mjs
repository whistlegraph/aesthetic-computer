// error, 2026.01.03
// A minimal error/stall screen.

function decodeParam(str) {
  if (!str) return "";
  try {
    return decodeURIComponent(str);
  } catch (e) {
    return String(str);
  }
}

function splitLines(text, maxLen = 44) {
  const raw = String(text || "").split("\n");
  const out = [];
  for (const line of raw) {
    if (line.length <= maxLen) {
      out.push(line);
      continue;
    }
    const words = line.split(/\s+/).filter(Boolean);
    let cur = "";
    for (const word of words) {
      const next = cur ? `${cur} ${word}` : word;
      if (next.length > maxLen) {
        if (cur) out.push(cur);
        cur = word;
      } else {
        cur = next;
      }
    }
    if (cur) out.push(cur);
  }
  return out;
}

function boot({ wipe, ink, screen, params }) {
  wipe("black");

  const msg = decodeParam(params?.[0]) || "An unknown error occurred.";
  const lines = splitLines(msg);

  const margin = 12;
  let y = 14;

  ink([255, 255, 255]).write("ERROR", { x: margin, y }, undefined, undefined, false, "MatrixChunky8");
  y += 16;

  for (let i = 0; i < Math.min(lines.length, 18); i += 1) {
    ink([180, 180, 180]).write(lines[i], { x: margin, y }, undefined, undefined, false, "MatrixChunky8");
    y += 12;
    if (y > screen.height - 14) break;
  }

  if (lines.length > 18) {
    ink([120, 120, 120]).write("…", { x: margin, y }, undefined, undefined, false, "MatrixChunky8");
  }
}

function paint() {
  return false;
}

export { boot, paint };
