// Mention Roster, 2026.11.13
// Who an "@" typed in a chat room most likely means: the people in the room,
// by the names the room actually calls them.

// Nicknames a room uses for a handle. Lowercase, no "@".
const ALIASES = { jas: "jeffrey" };

const clean = (h) => String(h || "").replace(/^@/, "");
const isHandle = (h) => /^[A-Za-z0-9_.-]+$/.test(h) && h !== "log" && h !== "nohandle" && h !== "unknown";

// Room handles: newest speaker first, then anyone present who has not spoken.
// `self` is left out, since nobody mentions themselves.
function roomRoster(messages = [], present = [], self = "") {
  const me = clean(self).toLowerCase();
  const seen = new Set();
  const names = [];
  const add = (h) => {
    const name = clean(h);
    const key = name.toLowerCase();
    if (!isHandle(name) || key === me || seen.has(key)) return;
    seen.add(key);
    names.push(name);
  };
  for (let i = messages.length - 1; i >= 0; i--) add(messages[i]?.from);
  present.forEach(add);
  return names;
}

// How well `query` names `name`: 0 exact · 1 starts with · 2 nickname ·
// 3 inside · 4 its letters in order ("hove" in "HendeFraHoeve") · -1 no.
function matchTier(query, name, aliases = ALIASES) {
  const q = query.toLowerCase();
  const n = name.toLowerCase();
  if (!q) return 1;
  if (n === q) return 0;
  if (n.startsWith(q)) return 1;
  for (const aka in aliases) if (aliases[aka] === n && aka.startsWith(q)) return 2;
  if (n.includes(q)) return 3;
  if (q.length < 2) return -1;
  let at = 0;
  for (const ch of n) if (ch === q[at]) at++;
  return at === q.length ? 4 : -1;
}

// The roster narrowed to `query`, best match first; ties keep roster order
// (so the person who spoke last wins).
function rankRoster(query, names, aliases = ALIASES) {
  return names
    .map((name, order) => ({ name, order, tier: matchTier(query, name, aliases) }))
    .filter((row) => row.tier >= 0)
    .sort((a, b) => a.tier - b.tier || a.order - b.order)
    .map((row) => row.name);
}

export { ALIASES, roomRoster, matchTier, rankRoster };
