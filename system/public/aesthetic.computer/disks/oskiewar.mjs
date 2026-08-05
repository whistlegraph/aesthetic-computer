// OSKIEWAR, 26.08.05
// Opens the standalone OSKIEWAR game, live room, or replay suite.

const WORD = "(?:[bdfgklmnprstvz][aeiou]){3}";
const ROUND_NAME = new RegExp(`^(?:${WORD}-${WORD}-${WORD}|[a-z]{4,7}[0-9]{1,3})$`);

function target({ colon = [], params = [] } = {}) {
  const candidate = String(colon[0] || params[0] || "").toLowerCase()
    .replace(/^ow-/, "");
  return ROUND_NAME.test(candidate)
    ? `https://oskiewar.com/${candidate}` : "https://oskiewar.com/";
}

function boot({ colon = [], params = [], jump }) {
  jump(`out:${target({ colon, params })}`);
}

function meta({ params = [], colon = [] } = {}) {
  const url = target({ params, colon });
  const round = url.split("/").filter(Boolean).at(-1);
  return { title: round && round !== "oskiewar.com" ? `OSKIEWAR ${round}` : "OSKIEWAR",
    desc: "Play OSKIEWAR or open a live round and its demo." };
}

export { boot, meta, target };
