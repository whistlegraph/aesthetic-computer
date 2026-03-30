const ROUTE_ALIASES = {
  "verify-password": "verify-builds-password",
  "pack-telemetry": "bundle-telemetry",
  "pack-telemetry-query": "bundle-telemetry-query",
  "track-tape": "track-media",
  "logo.png": "logo",
};

const NESTED_ROUTES = {
  "chat/messages": "chat-messages",
  "chat/heart": "chat-heart",
  "auth/cli-callback": "auth-cli-callback",
  "news/toll": "news-toll",
};

const NEWS_API_ROUTES = new Set([
  "posts",
  "updates",
  "submit",
  "comment",
  "vote",
  "delete",
  "unfurl",
]);

function normalizeRest(rest) {
  if (Array.isArray(rest)) return rest.join("/");
  if (typeof rest === "string") return rest;
  return "";
}

function resolveFunctionName(fn, rest, functions = {}) {
  const normalizedRest = normalizeRest(rest);

  if (normalizedRest) {
    const nested = `${fn}/${normalizedRest}`;
    for (const [pattern, target] of Object.entries(NESTED_ROUTES)) {
      if (nested === pattern || nested.startsWith(`${pattern}/`)) {
        return target;
      }
    }
  }

  if (ROUTE_ALIASES[fn]) return ROUTE_ALIASES[fn];

  if (fn === "news" && normalizedRest) {
    const firstSegment = normalizedRest.split("/")[0];
    return NEWS_API_ROUTES.has(firstSegment) ? "news-api" : "news";
  }

  if (fn === "ff1" && normalizedRest) {
    const subFn = `ff1-${normalizedRest.split("/")[0]}`;
    if (functions[subFn]) return subFn;
  }

  return fn;
}

export { resolveFunctionName };
