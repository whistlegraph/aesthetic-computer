const DEFAULT_STATUS = Object.freeze({
  version: 1,
  system: "mediascholar",
  updatedAt: null,
  state: "unavailable",
  phase: "connection",
  headline: "Live status is unavailable",
  detail: "Mediascholar may still be running; the public status link could not be reached.",
  nextCheckAt: null,
  gates: { processor: "unknown", memory: "unknown", disk: "unknown" },
  current: null,
  activity: { runs: 0, providerRuns: 0, candidates: [] },
  safeguards: { autoPublish: false },
});

const clipped = (value, max) => String(value || "").replace(/\s+/g, " ").trim().slice(0, max);
const iso = (value) => Number.isFinite(Date.parse(value)) ? new Date(value).toISOString() : null;

function safeUrl(value) {
  try {
    const url = new URL(value);
    return new Set(["http:", "https:"]).has(url.protocol) ? url.toString() : null;
  } catch {
    return null;
  }
}

function safeTopic(topic) {
  if (!topic || typeof topic !== "object") return null;
  return {
    title: clipped(topic.title, 140),
    question: clipped(topic.question, 500),
    claim: clipped(topic.claim, 700),
    whyNow: clipped(topic.whyNow, 700),
    terms: Array.isArray(topic.terms) ? topic.terms.map((item) => clipped(item, 80)).filter(Boolean).slice(0, 12) : [],
    signals: Array.isArray(topic.signals) ? topic.signals.flatMap((signal) => {
      const url = safeUrl(signal?.url);
      return url ? [{
        title: clipped(signal.title, 240),
        url,
        kind: clipped(signal.kind, 80),
        relevance: clipped(signal.relevance, 500),
      }] : [];
    }).slice(0, 16) : [],
  };
}

export function sanitizeMediascholarStatus(value) {
  if (!value || typeof value !== "object") return { ...DEFAULT_STATUS };
  const state = new Set(["waiting", "working", "idle", "review", "unavailable"]).has(value.state)
    ? value.state : "unavailable";
  const current = value.current && typeof value.current === "object" ? {
    id: clipped(value.current.id, 64),
    status: clipped(value.current.status, 40),
    startedAt: iso(value.current.startedAt),
    completedAt: iso(value.current.completedAt),
    topic: safeTopic(value.current.topic),
  } : null;
  const candidates = Array.isArray(value.activity?.candidates) ? value.activity.candidates.map((candidate) => ({
    id: clipped(candidate.id, 64),
    title: clipped(candidate.title, 140) || "Botted Paper",
    completedAt: iso(candidate.completedAt),
    status: "awaiting review",
  })).slice(0, 12) : [];
  const gate = (name) => new Set(["ready", "waiting", "unknown"]).has(value.gates?.[name])
    ? value.gates[name] : "unknown";
  return {
    version: 1,
    system: "mediascholar",
    updatedAt: iso(value.updatedAt),
    state,
    phase: clipped(value.phase, 40) || "unknown",
    headline: clipped(value.headline, 180) || DEFAULT_STATUS.headline,
    detail: clipped(value.detail, 300) || DEFAULT_STATUS.detail,
    nextCheckAt: iso(value.nextCheckAt),
    gates: { processor: gate("processor"), memory: gate("memory"), disk: gate("disk") },
    current,
    activity: {
      runs: Math.max(0, Math.min(10_000, Number(value.activity?.runs) || 0)),
      providerRuns: Math.max(0, Math.min(10_000, Number(value.activity?.providerRuns) || 0)),
      candidates,
    },
    safeguards: { autoPublish: false },
  };
}

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, OPTIONS",
      "Cache-Control": "public, max-age=5, stale-while-revalidate=15",
      "X-Content-Type-Options": "nosniff",
    },
    body: JSON.stringify(body),
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, {});
  if (event.httpMethod !== "GET") return respond(405, { error: "GET only" });
  const workerUrl = String(process.env.MEDIASCHOLAR_WORKER_URL || "").replace(/\/$/, "");
  if (!workerUrl) return respond(200, DEFAULT_STATUS);
  try {
    const response = await fetch(`${workerUrl}/status/mediascholar`, {
      headers: { accept: "application/json" },
      signal: AbortSignal.timeout(4000),
    });
    if (!response.ok) throw new Error(`worker returned ${response.status}`);
    return respond(200, sanitizeMediascholarStatus(await response.json()));
  } catch (error) {
    console.warn(`mediascholar-status: ${clipped(error.message, 160)}`);
    return respond(200, DEFAULT_STATUS);
  }
}
