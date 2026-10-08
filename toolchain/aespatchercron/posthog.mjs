import { catalog } from "./collect.mjs";
import { git } from "./io.mjs";
import { classifyPostHogFunction, permitsPostHogEndpointAggregate } from "../../shared/posthog-policy.mjs";

const source = "posthog";
const events = { ac_piece_opened: "piece_opened", ac_piece_interacted: "piece_interacted", ac_prompt_succeeded: "prompt_succeeded" };
const hosts = new Set(["https://us.posthog.com", "https://eu.posthog.com"]);
const empty = (status, reason) => ({ format: "aespatcher.supplement.v1", coverage: [{ source, status, reason }], leads: [], metrics: [], privateReports: [] });
const literal = value => `'${value}'`; // Callers supply validated identifiers or ISO timestamps only.

async function readResponse(fetchImpl, url, options = {}, limit = 1024 * 1024) {
  const response = await fetchImpl(url, { ...options, redirect: "error", signal: AbortSignal.timeout(20000) });
  if (!response.ok) throw new Error("posthog-read-failed");
  const reader = response.body.getReader();
  const chunks = []; let bytes = 0;
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      bytes += value.byteLength;
      if (bytes > limit) throw new Error("posthog-response-too-large");
      chunks.push(value);
    }
    return Buffer.concat(chunks).toString("utf8");
  } finally { await reader.cancel(); }
}

// Bind the read project to the browser token served by AC itself, never an unrelated account's default project.
export async function postHogIdentity(config, key, fetchImpl) {
  const browser = JSON.parse(await readResponse(fetchImpl, "https://aesthetic.computer/api/product-analytics-config"));
  if (!browser || !/^phc_[a-zA-Z0-9]+$/.test(browser.projectToken) || !hosts.has(browser.uiHost)) throw new Error("posthog-production-binding-unavailable");
  const project = JSON.parse(await readResponse(fetchImpl,
    `${browser.uiHost}/api/organizations/${config.organizationId}/projects/${config.projectId}/`,
    { headers: { Authorization: `Bearer ${key}` } }));
  if (String(project.id) !== String(config.projectId) || project.organization !== config.organizationId || project.api_token !== browser.projectToken)
    throw new Error("posthog-project-mismatch");
  return browser.uiHost;
}

export function postHogQueries(routes, endpoints, { hours = 24, minimum = 3, limit = 100, end = new Date().toISOString() } = {}) {
  if (![routes, endpoints].every(list => Array.isArray(list) && list.length && list.every(s => /^[a-z0-9-]+$/.test(s))) ||
      !Number.isInteger(hours) || hours < 1 || hours > 168 || !Number.isInteger(minimum) || minimum < 3 || minimum > 100 ||
      !Number.isInteger(limit) || limit < 1 || limit > 500 || !Number.isFinite(Date.parse(end))) throw new Error("posthog-invalid-bounds");
  const stop = new Date(end), start = new Date(+stop - hours * 3600000);
  const time = date => literal(date.toISOString().slice(0, 19).replace("T", " "));
  const window = `timestamp >= toDateTime(${time(start)}, 'UTC') AND timestamp < toDateTime(${time(stop)}, 'UTC')`;
  return [
    `SELECT toString(properties.piece), event, count() FROM events WHERE ${window} AND event IN (${Object.keys(events).map(literal).join(",")}) AND properties.piece_kind = 'built-in' AND toString(properties.piece) IN (${routes.map(literal).join(",")}) GROUP BY 1, 2 HAVING count() >= ${minimum} ORDER BY 3 DESC, 1, 2 LIMIT ${limit + 1}`,
    `SELECT toString(properties.endpoint), toString(properties.status_class), sum(toFloat(properties.count)) FROM events WHERE ${window} AND event = 'ac endpoint completed' AND toString(properties.endpoint) IN (${endpoints.map(literal).join(",")}) AND properties.status_class IN ('2xx','3xx','4xx','5xx') GROUP BY 1, 2 HAVING sum(toFloat(properties.count)) >= ${minimum} ORDER BY 2 DESC, 3 DESC, 1 LIMIT ${limit + 1}`,
  ];
}

export function postHogSignals(responses, routes, endpoints, { limit = 100, minimum = 3 } = {}) {
  if (!Array.isArray(responses) || responses.length !== 2) throw new Error("posthog-query-unavailable");
  const out = empty("available", "aggregate-product-context");
  let scanned = 0, truncated = false;
  responses.forEach((response, index) => {
    if (!Array.isArray(response.results) || response.results.length > limit + 1) throw new Error("posthog-query-unavailable");
    truncated ||= response.results.length > limit;
    for (const row of response.results.slice(0, limit)) {
      if (!Array.isArray(row) || row.length !== 3) throw new Error("posthog-invalid-aggregate");
      const [target, metric, count] = row;
      if (!Number.isSafeInteger(count) || count < minimum || !(index ? endpoints : routes).includes(target) ||
          !(index ? ["2xx", "3xx", "4xx", "5xx"].includes(metric) : Object.hasOwn(events, metric))) throw new Error("posthog-invalid-aggregate");
      out.metrics.push({ source, metric: index ? `endpoint_${metric}` : events[metric], target, count });
      if (index && metric === "5xx") out.leads.push({ source, kind: "endpoint-error", route: null, count,
        summary: `${target} recorded ${count} server-error responses in PostHog endpoint aggregates; inspect Lith and reproduce before proposing a fix.`, evidenceRefs: [] });
      scanned++;
    }
  });
  out.coverage[0] = { source, status: "available", scanned, truncated };
  return out;
}

export async function collectPostHog(repo, config, { fetchImpl = globalThis.fetch, env = process.env } = {}) {
  const settings = config.posthog || {};
  if (settings.enabled === false) return empty("disabled", "operator-disabled");
  const key = env.AESPATCHER_POSTHOG_READ_KEY;
  if (!key) return empty("unavailable", "missing-read-credential");
  if (!/^\d+$/.test(String(settings.projectId || "")) || !/^[a-f0-9]{8}-[a-f0-9-]{27}$/i.test(settings.organizationId || ""))
    return empty("unavailable", "missing-project-binding");
  try {
    const host = await postHogIdentity(settings, key, fetchImpl);
    const routes = await catalog(repo);
    const prefix = "system/netlify/functions/";
    const endpoints = (await git(repo, "ls-tree", "--name-only", "HEAD", `${prefix}`)).split("\n")
      .map(file => file.slice(prefix.length).replace(/\.(m?js)$/, ""))
      .filter(name => /^[a-z0-9-]+$/.test(name) && permitsPostHogEndpointAggregate(classifyPostHogFunction(name)));
    const queries = postHogQueries(routes, [...new Set(endpoints)], config.collection);
    const responses = [];
    for (const query of queries) responses.push(JSON.parse(await readResponse(fetchImpl,
      `${host}/api/projects/${settings.projectId}/query/`, { method: "POST", headers: { Authorization: `Bearer ${key}`, "content-type": "application/json" },
        body: JSON.stringify({ query: { kind: "HogQLQuery", query } }) })));
    return postHogSignals(responses, routes, endpoints, config.collection);
  } catch (error) {
    const reason = ["posthog-project-mismatch", "posthog-production-binding-unavailable", "posthog-invalid-bounds", "posthog-invalid-aggregate"].includes(error.message)
      ? error.message : "posthog-read-unavailable";
    return empty("unavailable", reason);
  }
}
