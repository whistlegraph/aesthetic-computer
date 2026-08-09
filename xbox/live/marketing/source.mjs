// Stage 1 — decide which match becomes a reel. Mechanically.
//
// Nothing here has an opinion. A date and a slot index turn into an absolute
// slot number; the slot number picks the market segment out of the rotation
// and the seed string; the seed string reproduces the whole fight. The
// operator's only remaining job is to watch what came out.

import { segmentForSlot, segments, dress } from "./segments.mjs";

const DAY = 86400000;
// Slot zero. Fixed forever, because moving it would renumber every past post
// and silently reshuffle which market owns which slot.
export const epoch = Date.UTC(2026, 0, 1);

export const dayNumber = (date) =>
  Math.floor((Date.UTC(date.getUTCFullYear(), date.getUTCMonth(),
    date.getUTCDate()) - epoch) / DAY);

export const slotNumber = (date, index, slotsPerDay) =>
  dayNumber(date) * slotsPerDay + index;

// One in five slots plays a match a real person actually had, so the feed is
// not only robots sparring. Which one is still arithmetic: the store is asked
// for its recent rounds, the unwatchable ones are dropped, and the seed picks
// from what is left.
const replaySlot = (slot) => slot % 5 === 4;

const MIN_TICKS = 420;   // 7 seconds at 60 Hz — shorter is a whiff, not a fight
const store = "https://aesthetic.computer/api/oskiewar-replays";

export async function recentRounds({ limit = 50, fetcher = fetch } = {}) {
  const response = await fetcher(`${store}?limit=${limit}`,
    { headers: { accept: "application/json" } });
  if (!response.ok) throw new Error(`replay store ${response.status}`);
  const body = await response.json();
  return (body.replays || []).filter((round) =>
    round.durationTicks >= MIN_TICKS && round.roundName);
}

// FNV-1a, the same one `render.mjs` seeds the sim with. Keeping one hash means
// a seed string always means the same number everywhere in the factory.
export function seed32(text) {
  let hash = 0x811c9dc5;
  for (const char of String(text)) {
    hash ^= char.charCodeAt(0);
    hash = Math.imul(hash, 0x01000193);
  }
  return hash >>> 0;
}

export async function pickSource({ date = new Date(), index = 0,
  slotsPerDay = 3, cap = 600, allowReplays = true, log = () => {} } = {}) {
  const slot = slotNumber(date, index, slotsPerDay);
  const day = date.toISOString().slice(0, 10);
  const segmentKey = segmentForSlot(slot);
  const seed = `${day}#${index}`;
  const pick = seed32(seed);
  let kind = segments[segmentKey].source;
  let round = null;

  if (allowReplays && replaySlot(slot)) {
    try {
      const rounds = await recentRounds();
      if (rounds.length) {
        round = rounds[pick % rounds.length];
        kind = "replay";
      } else log("  no replay in the store long enough — falling back to self-play");
    } catch (error) {
      log(`  replay store unreachable (${error.message}) — falling back to self-play`);
    }
  }

  const facts = {
    // What the under-line may say is bounded by what the renderer can prove.
    // A seed reproduces the match's identity — its name, its ball, its
    // fighters — but not its frames: the browser advances the sim on the wall
    // clock, so tick alignment jitters between runs.
    under: kind === "replay"
      ? `real round · oskiewar.com/${round.roundName}`
      : `seeded ${seed}`,
    round: round?.roundName || null,
    fighters: round?.fighters || ["BOT 1", "BOT 2"],
    winner: round?.winner || null,
  };
  const copy = dress(segmentKey, pick, facts);

  const spec = { id: `${day}-s${index}-${segmentKey}`, slot, day, index,
    segment: segmentKey, segmentName: segments[segmentKey].name,
    kind, seed, cap, round: round?.roundName || null, facts, ...copy };
  log(`🎯 slot ${slot} · ${day} #${index} · ${segments[segmentKey].name} · ${kind}`);
  log(`   seed "${seed}" · hook "${copy.hook}"`);
  return spec;
}
