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

export const segments = {
  fgc: {
    name: "fighting game / FGC",
    why: "a real fighter: hitboxes, hitstun, blockstun, grabs, a ball nobody asked for",
    source: "self-play",
    hooks: ["hitbox on impact", "grab beats shield", "the ball is legal",
      "read the frame", "both bots, no player"],
    tail: "oskiewar.com",
    captions: [
      "Hitboxes flash on contact so you can see exactly what hit what. Two bots, no player, no cuts.",
      "Shield stops strikes. It does not stop a grab. That is the whole rock-paper-scissors.",
      "There is a ball in the fighting game and it is fully legal. Kick it at someone.",
    ],
    tags: ["fgc", "fightinggame", "indiefightinggame", "hitbox", "framedata",
      "fgcommunity", "combo", "indiegame", "oskiewar"],
  },
  gamedev: {
    name: "indie gamedev",
    why: "no engine, one file, deterministic, tested headlessly in CI",
    source: "self-play",
    hooks: ["no engine", "one file, 264 KB", "deterministic sim",
      "this is a test run", "60 Hz fixed timestep"],
    tail: "oskiewar.com",
    captions: [
      "No engine. One JavaScript file, a fixed 60 Hz timestep, and a canvas. This clip is the test suite playing itself.",
      "Under a fixed timestep two machines given the same match name agree on a SHA-256 sixty seconds in. That is what makes replays and netcode possible.",
      "Every match records to a replay you can open by name. Nothing here is a video of a video.",
    ],
    tags: ["gamedev", "indiedev", "indiegame", "javascript", "gamedevelopment",
      "creativecoding", "madewithcode", "solodev", "oskiewar"],
  },
  homebrew: {
    name: "Xbox / console homebrew",
    why: "the same source runs under JavaScriptCore on console, no port",
    source: "self-play",
    hooks: ["same file on console", "runs on Xbox", "no port, no rewrite",
      "JavaScriptCore, natively", "console + browser, one source"],
    tail: "oskiewar.com",
    captions: [
      "The exact file in this clip runs on console too — JavaScriptCore natively, no port and no rewrite.",
      "Browser and console from one source. The renderer is the only thing that differs.",
      "Homebrew that is not emulated and not a wrapper. One game, two machines.",
    ],
    tags: ["xbox", "homebrew", "consoledev", "gamedev", "indiegame",
      "xboxseriesx", "devkit", "oskiewar"],
  },
  retro: {
    name: "retro / pixel + arcade",
    why: "hand-drawn stick fighters, comic lettering, arcade clock",
    source: "self-play",
    hooks: ["stick figures, real physics", "drawn by hand", "arcade rules",
      "24 seconds on the clock", "pick a pal, pick a fight"],
    tail: "oskiewar.com",
    captions: [
      "Stick figures with real joints, real weight, and real hitboxes. Round timer, best of three, no menus.",
      "Everything is drawn with lines and circles at runtime. No sprites, no assets, no atlas.",
      "Pick a pal, pick a fight. That is the entire front end.",
    ],
    tags: ["retrogaming", "pixelart", "arcade", "stickfigure", "indiegame",
      "retro", "gamedev", "oskiewar"],
  },
  gen: {
    name: "generative / computational art",
    why: "the whole match falls out of one seed; the audio is synthesized live",
    source: "self-play",
    hooks: ["one seed, one fight", "the audio is synthesized",
      "named by arithmetic", "no samples", "seeded by the date"],
    tail: "oskiewar.com",
    captions: [
      "Today's date is the seed. It names the match, picks the ball and sets the fighters — the whole bout falls out of one string.",
      "Nothing you hear is a sample. Every hit, step and bell is synthesized in the browser at the moment it happens.",
      "A date goes in, a match comes out. The reel you are watching was chosen by arithmetic, not by me.",
    ],
    tags: ["generativeart", "creativecoding", "computationalart", "webaudio",
      "proceduralaudio", "deterministic", "codeart", "oskiewar"],
  },
};

export const segmentKeys = Object.keys(segments);

// One accent per market, drawn from the fighters' own palette in `hello.js`
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

// A caption is one line of copy, one line of address, then the tags. Meta
// allows 2200 characters and 30 hashtags; nine tags and three lines is a post,
// not a keyword dump.
export function dress(segmentKey, pick, facts) {
  const segment = segments[segmentKey];
  const hook = segment.hooks[pick % segment.hooks.length];
  const body = segment.captions[pick % segment.captions.length];
  const caption = [body, "", `▶ play it free in a browser — ${segment.tail}`, "",
    segment.tags.map((tag) => "#" + tag).join(" ")].join("\n");
  return { hook, tail: segment.tail, caption, tags: segment.tags,
    lines: { hook, under: facts.under } };
}
