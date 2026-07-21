// Provider-neutral region and relay policy. These are logical buckets; adding
// one here does not provision a server.

export const FIGHT_REGIONS = Object.freeze({
  "us-west": { neighbors: ["us-east"], relays: ["us-west"] },
  "us-east": { neighbors: ["us-west", "eu-west"], relays: ["us-east"] },
  "eu-west": { neighbors: ["eu-central", "us-east"], relays: ["eu-west"] },
  "eu-central": { neighbors: ["eu-west"], relays: ["eu-central"] },
});

export const DEFAULT_FIGHT_REGION = "us-west";

export function normalizeFightRegion(region, fallback = DEFAULT_FIGHT_REGION) {
  return FIGHT_REGIONS[region] ? region : fallback;
}

export function regionSearchOrder(region) {
  const first = normalizeFightRegion(region);
  return [first, ...FIGHT_REGIONS[first].neighbors];
}

// Competitive search stays local for ten seconds, tries adjacent regions for
// the next twenty, and only then considers every configured region. It never
// relaxes build/rules compatibility or the network-quality ceiling.
export function allowedOpponentRegions(region, waitedMs = 0) {
  const order = regionSearchOrder(region);
  if (waitedMs < 10_000) return order.slice(0, 1);
  if (waitedMs < 30_000) return order;
  return [...new Set([...order, ...Object.keys(FIGHT_REGIONS)])];
}

function finite(value, min, max) {
  const number = Number(value);
  return Number.isFinite(number) && number >= min && number <= max ? number : null;
}

export function sanitizeProbeSample(sample, expectedNonce) {
  if (!sample || sample.nonce !== expectedNonce) return null;
  const rttMs = finite(sample.rttMs, 0, 2_000);
  const jitterMs = finite(sample.jitterMs, 0, 1_000);
  const loss = finite(sample.loss, 0, 1);
  const samples = finite(sample.samples, 3, 10_000);
  if ([rttMs, jitterMs, loss, samples].includes(null)) return null;
  return { rttMs, jitterMs, loss, samples };
}

export function routeScore(a, b) {
  if (!a || !b) return Infinity;
  return Math.max(a.rttMs, b.rttMs) + (a.jitterMs + b.jitterMs) * 2 +
    Math.max(a.loss, b.loss) * 1000;
}

export function selectFightRoute(reports, {
  directRttMax = 120,
  directJitterMax = 25,
  directLossMax = 0.05,
} = {}) {
  const players = [reports?.[0], reports?.[1]];
  if (players.every(Boolean)) {
    const direct = players.map((report) => report.direct).filter(Boolean);
    if (direct.length === 2 && direct.every((sample) =>
      sample.rttMs <= directRttMax && sample.jitterMs <= directJitterMax &&
      sample.loss <= directLossMax)) {
      return { type: "direct", region: null, score: routeScore(...direct) };
    }
  }

  let best = null;
  for (const region of Object.keys(FIGHT_REGIONS)) {
    const a = players[0]?.relays?.[region];
    const b = players[1]?.relays?.[region];
    const score = routeScore(a, b);
    if (Number.isFinite(score) && (!best || score < best.score)) {
      best = { type: "relay", region, score };
    }
  }
  return best || { type: "unavailable", region: null, score: Infinity };
}

