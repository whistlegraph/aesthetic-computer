// The market table — the only place a reel's audience is decided.
//
// @jeffrey's ask was "targeting of markets", and the point of writing them
// down is that nobody picks one on the day. A slot number goes into
// `segmentForSlot` and a segment comes out; the rotation is a fixed cycle, so
// each market gets a guaranteed share of the grid rather than whatever the
// operator felt like on a Tuesday.
//
// Every segment says the same true things about oskiewar in the dialect its
// audience already speaks. None of the copy claims anything the game does not
// do — the sim really is deterministic, the replays really are addressable,
// the whole thing really is one 264 KB file.
//
// What a segment carries is what gets DRAWN: a hook line and a tail. There is
// no caption and there are no hashtags — @jeffrey, 2026-09-02: "they are
// lame". The reel is the post; the post says nothing under it.

export const segments = {
  fgc: {
    name: "fighting game / FGC",
    why: "a real fighter: hitboxes, hitstun, blockstun, grabs, a ball nobody asked for",
    source: "self-play",
    hooks: ["hitbox on impact", "grab beats shield", "the ball is legal",
      "read the frame", "both bots, no player"],
    tail: "oskiewar.com",
  },
  gamedev: {
    name: "indie gamedev",
    why: "no engine, one file, deterministic, tested headlessly in CI",
    source: "self-play",
    hooks: ["no engine", "one file, 264 KB", "deterministic sim",
      "this is a test run", "60 Hz fixed timestep"],
    tail: "oskiewar.com",
  },
  homebrew: {
    name: "Xbox / console homebrew",
    why: "the same source runs under JavaScriptCore on console, no port",
    source: "self-play",
    hooks: ["same file on console", "runs on Xbox", "no port, no rewrite",
      "JavaScriptCore, natively", "console + browser, one source"],
    tail: "oskiewar.com",
  },
  retro: {
    name: "retro / pixel + arcade",
    why: "hand-drawn stick fighters, comic lettering, arcade clock",
    source: "self-play",
    hooks: ["stick figures, real physics", "drawn by hand", "arcade rules",
      "24 seconds on the clock", "pick a pal, pick a fight"],
    tail: "oskiewar.com",
  },
  gen: {
    name: "generative / computational art",
    why: "the whole match falls out of one seed; the audio shares its offline clock",
    source: "self-play",
    hooks: ["one seed, one fight", "the audio is synthesized offline",
      "named by arithmetic", "no samples", "seeded by the date"],
    tail: "oskiewar.com",
  },
};

export const segmentKeys = Object.keys(segments);

// One accent per market, drawn from the fighters' own palette in `oskiewar.js`
// so a reel never introduces a colour the game does not already own.
export const accents = {
  fgc: "#cd3048", gamedev: "#6fe8d2", homebrew: "#3076cd",
  retro: "#ffe85c", gen: "#d164d8",
};

// The rotation. Ten slots, so a share is readable at a glance: FGC and gamedev
// carry three and two because they are where a fighting game actually lands;
// the other three take one or two each and keep the account from reading as a
// single-note feed. Change the array, change the market mix — nothing else.
export const rotation = ["fgc", "gamedev", "retro", "fgc", "gen",
  "homebrew", "fgc", "gamedev", "retro", "gen"];

export const segmentForSlot = (slot) =>
  rotation[((slot % rotation.length) + rotation.length) % rotation.length];

export function share() {
  const counts = {};
  for (const key of rotation) counts[key] = (counts[key] || 0) + 1;
  return Object.fromEntries(Object.entries(counts).map(([key, count]) =>
    [key, `${count}/${rotation.length}`]));
}

export function dress(segmentKey, pick, facts) {
  const segment = segments[segmentKey];
  const hook = segment.hooks[pick % segment.hooks.length];
  return { hook, tail: segment.tail, caption: "", tags: [],
    lines: { hook, under: facts.under } };
}
