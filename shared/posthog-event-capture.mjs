const CLOUD_HOSTS = new Set([
  "https://us.i.posthog.com",
  "https://eu.i.posthog.com",
]);

export function createPostHogEventCapture({
  projectToken = process.env.POSTHOG_PROJECT_TOKEN,
  apiHost = process.env.POSTHOG_API_HOST || "https://us.i.posthog.com",
  enabled = process.env.POSTHOG_OSKIEWAR_EVENTS === "true",
  fetchImpl = globalThis.fetch,
  distinctId,
  eventFactory,
  flushIntervalMs = 10_000,
} = {}) {
  const active = Boolean(
    enabled &&
    projectToken?.startsWith("phc_") &&
    CLOUD_HOSTS.has(apiHost) &&
    typeof fetchImpl === "function" &&
    typeof eventFactory === "function" &&
    distinctId,
  );
  let aggregates = new Map();
  let flushing = null;

  function capture(action, properties) {
    const item = eventFactory(action, properties);
    if (!active || !item) return false;
    const key = JSON.stringify(item);
    const current = aggregates.get(key);
    aggregates.set(key, { ...item, count: (current?.count || 0) + 1 });
    if (aggregates.size >= 100) void flush();
    return true;
  }

  async function flush() {
    if (!active || flushing || aggregates.size === 0) return false;
    const pending = aggregates;
    aggregates = new Map();
    flushing = (async () => {
      const batch = [...pending.values()].map(
        ({ event, properties, count }) => ({
          event,
          distinct_id: distinctId,
          properties: {
            ...properties,
            count,
            $process_person_profile: false,
            $geoip_disable: true,
          },
        }),
      );
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

  const timer = active
    ? setInterval(() => void flush(), flushIntervalMs)
    : null;
  timer?.unref?.();

  return {
    active,
    capture,
    flush,
    stop() {
      if (timer) clearInterval(timer);
    },
  };
}
