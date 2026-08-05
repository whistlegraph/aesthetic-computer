import {
  classifyPostHogFunction,
  permitsPostHogEndpointAggregate,
} from "../shared/posthog-policy.mjs";

const CLOUD_HOSTS = new Set([
  "https://us.i.posthog.com",
  "https://eu.i.posthog.com",
]);

export function browserAnalyticsConfig(env = process.env) {
  const projectToken = env.POSTHOG_PROJECT_TOKEN?.trim();
  const apiHost = env.POSTHOG_API_HOST?.trim() || "https://us.i.posthog.com";
  if (!projectToken?.startsWith("phc_") || !CLOUD_HOSTS.has(apiHost)) {
    return null;
  }
  return {
    projectToken,
    apiHost,
    uiHost: apiHost.startsWith("https://eu.")
      ? "https://eu.posthog.com"
      : "https://us.posthog.com",
  };
}

export function durationBucket(ms) {
  if (ms < 100) return "under_100ms";
  if (ms < 500) return "100_499ms";
  if (ms < 2000) return "500_1999ms";
  return "2000ms_or_more";
}

export function statusClass(status) {
  const numeric = Number(status);
  if (!Number.isFinite(numeric) || numeric < 100 || numeric > 599)
    return "unknown";
  return `${Math.floor(numeric / 100)}xx`;
}

export function endpointAggregate(name, ms, status, method) {
  const policy = classifyPostHogFunction(String(name || "unknown"));
  if (!permitsPostHogEndpointAggregate(policy)) return null;
  return {
    endpoint: String(name || "unknown")
      .replace(/[^a-z0-9-]/gi, "_")
      .slice(0, 80),
    method: String(method || "UNKNOWN")
      .toUpperCase()
      .slice(0, 10),
    status_class: statusClass(status),
    duration_bucket: durationBucket(Number(ms) || 0),
    analytics_class: policy.class,
  };
}

export function lithSurfaceAggregate(pathname, ms, status, method) {
  const path = String(pathname || "");
  const gym = path.match(/^\/api\/(publish|history|rewind)-gym\/?$/);
  const endpoint = gym
    ? `lith-gym-${gym[1]}`
    : /^\/media(?:\/|$)/.test(path)
      ? "lith-media"
      : /^\/frame(?:\/|$)/.test(path)
        ? "lith-frame"
        : null;
  if (!endpoint) return null;
  return {
    endpoint,
    method: String(method || "UNKNOWN")
      .toUpperCase()
      .slice(0, 10),
    status_class: statusClass(status),
    duration_bucket: durationBucket(Number(ms) || 0),
    analytics_class: gym ? "gym" : "content-media",
  };
}

export function createEndpointAnalytics({
  projectToken = process.env.POSTHOG_PROJECT_TOKEN,
  apiHost = process.env.POSTHOG_API_HOST || "https://us.i.posthog.com",
  enabled = process.env.POSTHOG_SERVER_ENDPOINT_EVENTS === "true",
  fetchImpl = globalThis.fetch,
  flushIntervalMs = 10_000,
} = {}) {
  const active = Boolean(
    enabled &&
      projectToken?.startsWith("phc_") &&
      CLOUD_HOSTS.has(apiHost) &&
      typeof fetchImpl === "function",
  );
  let aggregates = new Map();
  let flushing = null;

  function queue(properties) {
    if (!active || !properties) return false;
    const key = JSON.stringify(properties);
    const current = aggregates.get(key);
    aggregates.set(key, { ...properties, count: (current?.count || 0) + 1 });
    if (aggregates.size >= 100) void flush();
    return true;
  }

  async function flush() {
    if (!active || flushing || aggregates.size === 0) return false;
    const pending = aggregates;
    aggregates = new Map();
    flushing = (async () => {
      const batch = [...pending.values()].map(({ count, ...properties }) => ({
        event: "ac endpoint completed",
        distinct_id: "ac-lith-endpoint-aggregate",
        properties: {
          ...properties,
          count,
          $process_person_profile: false,
          $geoip_disable: true,
        },
      }));
      try {
        const response = await fetchImpl(`${apiHost}/batch/`, {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ api_key: projectToken, batch }),
          signal: AbortSignal.timeout(3000),
        });
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return true;
      } catch {
        for (const [key, value] of pending) {
          const current = aggregates.get(key);
          aggregates.set(key, {
            ...value,
            count: value.count + (current?.count || 0),
          });
        }
        return false;
      } finally {
        flushing = null;
      }
    })();
    return flushing;
  }

  function capture(name, ms, status, method) {
    return queue(endpointAggregate(name, ms, status, method));
  }

  function captureSurface(pathname, ms, status, method) {
    return queue(lithSurfaceAggregate(pathname, ms, status, method));
  }

  const timer = active
    ? setInterval(() => void flush(), flushIntervalMs)
    : null;
  timer?.unref?.();

  return {
    active,
    capture,
    captureSurface,
    flush,
    stop() {
      if (timer) clearInterval(timer);
    },
  };
}
