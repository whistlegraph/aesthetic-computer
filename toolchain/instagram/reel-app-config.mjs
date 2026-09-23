const DAY_MS = 86_400_000;
const EPOCH = Date.parse("2026-01-01T00:00:00Z");

export const REEL_APPS = {
  whistlegraph: {
    account: "whistlegraph",
    prefix: "WHISTLEGRAPH",
    source: "archive",
    slotsPerDay: 1,
    segments: ["classic", "play", "classic", "archive"],
    tags: ["whistlegraph", "drawing", "graphicscore", "performanceart", "soundart"],
  },
  aesthetic: {
    account: "aesthetic",
    prefix: "AESTHETIC",
    source: "av",
    slotsPerDay: 1,
    // 60 because the shared media gate wants reels authored at 60fps, and the
    // CDP screencast delivers ~60 anyway (30 dropped every other frame).
    // density 2 = each AC pixel is 2×2 video pixels (540×960 of piece);
    // capture-av's default 3 read too chunky.
    capture: { width: 1080, height: 1920, fps: 60, density: 2 },
    // The piece alone: no pals side stamps or title columns, and a cursor
    // small enough to read as a pointer rather than a mascot.
    stamp: { chrome: "none", cursorSize: 56 },
    segments: ["instrument", "draw", "instrument", "code"],
    tags: ["aestheticcomputer", "creativecoding", "digitalart", "generativeart", "webart"],
    recipes: [
      {
        id: "notepat-melody",
        piece: "notepat",
        performance: "notepat-melody",
        duration: 10,
        title: "notepat",
        segment: "instrument",
        line: "A small melody, played on notepat.",
      },
      {
        id: "bubble-taps",
        piece: "bubble",
        performance: "bubble-taps",
        duration: 10,
        title: "bubble",
        segment: "instrument",
        line: "Ten touches turn into a bubble instrument.",
      },
      {
        // The most-hit stored clock (110 hits on /api/store-clock, 2026-09-23):
        // four saw voices; the lead loops every ~7.75 s, so 24 s is three
        // loops. Lane rule: whole loops landing in 20–30 s.
        id: "clock-wibe",
        piece: "*wibe",
        duration: 24,
        title: "clock",
        segment: "code",
        line: "The most-played clock on aesthetic.computer: four saw voices, one tiny score.",
      },
      {
        id: "clock-square",
        piece: "clock:0.5~{square}cegcdfdefgec",
        duration: 12,
        title: "clock",
        segment: "code",
        line: "A tiny score becomes a clockwork instrument.",
      },
      {
        id: "notepat-furelise",
        piece: "notepat",
        performance: "notepat-furelise",
        duration: 25,
        title: "notepat",
        segment: "instrument",
        line: "Für Elise, played on notepat.",
      },
    ],
  },
};

export function daySlot(day, index = 0, slotsPerDay = 1) {
  const at = Date.parse(`${day}T00:00:00Z`);
  if (!Number.isFinite(at)) throw new Error(`invalid day ${day}; expected YYYY-MM-DD`);
  if (!Number.isInteger(index) || index < 0 || index >= slotsPerDay)
    throw new Error(`index ${index} is outside 0..${slotsPerDay - 1}`);
  return Math.floor((at - EPOCH) / DAY_MS) * slotsPerDay + index;
}

export function pickUnposted(candidates, slot, posted = new Set(), allowRepeat = false) {
  if (!candidates.length) return null;
  const first = ((slot % candidates.length) + candidates.length) % candidates.length;
  for (let offset = 0; offset < candidates.length; offset += 1) {
    const candidate = candidates[(first + offset) % candidates.length];
    if (allowRepeat || !posted.has(candidate.id)) return candidate;
  }
  return null;
}

export function cleanCaption(text, max = 760) {
  return String(text || "")
    .replace(/https?:\/\/\S+/g, "")
    .replace(/(^|\s)#[\p{L}\p{N}_]+/gu, " ")
    .replace(/\s+/g, " ")
    .trim()
    .slice(0, max)
    .trim();
}

export function whistlegraphCaption(post, segment, tags) {
  const code = (post.works || [])[0] || "";
  const opening = cleanCaption(post.desc) || "A drawing you sing.";
  let action = code
    ? `Draw and sing [${code}] — whistlegraph.org/${code}`
    : "Explore the archive — whistlegraph.org";
  if (segment === "play") action = "Record your own — aesthetic.computer/whistlegraph";
  if (segment === "archive") action = "More drawings you can sing — whistlegraph.org";
  return [opening, "", action, "", tags.map((tag) => `#${tag}`).join(" ")].join("\n");
}

// The caption is the prompt and nothing else: exactly what you'd type into
// aesthetic.computer to get what the reel shows (`*wibe`, `notepat`). No
// link, no hashtags, no line — the prompt is the whole invitation.
export function aestheticCaption(recipe) {
  return recipe.prompt || recipe.piece;
}
