const CLOUD_HOSTS = new Set([
  "https://us.i.posthog.com",
  "https://eu.i.posthog.com",
]);

export const POSTHOG_OPTIONS = Object.freeze({
  advanced_disable_flags: true,
  defaults: "2026-05-30",
  autocapture: false,
  capture_pageview: false,
  capture_pageleave: false,
  capture_dead_clicks: false,
  capture_exceptions: false,
  capture_heatmaps: false,
  capture_performance: false,
  disable_session_recording: true,
  disable_surveys: true,
  disable_product_tours: true,
  disable_external_dependency_loading: true,
  enable_recording_console_log: false,
  mask_all_element_attributes: true,
  mask_all_text: true,
  mask_personal_data_properties: true,
  person_profiles: "identified_only",
  property_denylist: [
    "content",
    "email",
    "file",
    "host",
    "hostname",
    "message",
    "name",
    "path",
    "prompt",
    "recipient",
    "source",
    "text",
  ],
  respect_dnt: true,
});

export function safeRoutePath(pathname = "/") {
  const first = String(pathname).split("/").filter(Boolean)[0] || "";
  if (!first) return "/";
  if (first.startsWith("@")) return "/@published";
  if (first.startsWith("$")) return "/$code";
  if (first.startsWith("prompt~")) return "/prompt";
  if (/^[a-z0-9-]{1,64}$/i.test(first)) return `/${first.toLowerCase()}`;
  return "/_other";
}

export function pieceProperties(diskPath, pathname = "/") {
  const builtIn = String(diskPath || "").match(
    /^aesthetic\.computer\/disks\/([a-z0-9-]{1,64})$/i,
  );
  return {
    piece: builtIn?.[1]?.toLowerCase() || null,
    piece_kind: builtIn ? "built-in" : "published-or-code",
    route: safeRoutePath(pathname),
  };
}

export function userIdentity(user) {
  if (!user?.sub) return null;
  return {
    distinctId: String(user.sub),
    properties: user.handle ? { handle: String(user.handle) } : {},
  };
}

function installPostHogStub(win, doc) {
  const posthog = (win.posthog = win.posthog || []);
  if (posthog.__SV) return posthog;
  posthog._i = [];
  posthog.init = function (token, options, name) {
    const script = doc.createElement("script");
    script.type = "text/javascript";
    script.crossOrigin = "anonymous";
    script.async = true;
    script.src =
      options.api_host.replace(".i.posthog.com", "-assets.i.posthog.com") +
      "/static/array.js";
    const firstScript = doc.getElementsByTagName("script")[0];
    firstScript.parentNode.insertBefore(script, firstScript);

    const instance = name ? (posthog[name] = []) : posthog;
    instance.people = instance.people || [];
    const methods = [
      "capture",
      "identify",
      "reset",
      "opt_in_capturing",
      "opt_out_capturing",
      "has_opted_out_capturing",
    ];
    for (const method of methods) {
      instance[method] = function (...args) {
        instance.push([method, ...args]);
      };
    }
    posthog._i.push([token, options, name || "posthog"]);
  };
  posthog.__SV = 1;
  return posthog;
}

let initialized = false;
let lastRoute = null;
let lastIdentity = null;

function captureRoute(win, posthog) {
  const route = safeRoutePath(win.location.pathname);
  if (route === lastRoute) return;
  lastRoute = route;
  posthog.capture("$pageview", {
    $current_url: `${win.location.origin}${route}`,
    ac_route: route,
  });

  const namedPiece = route.match(/^\/([a-z0-9-]{1,64})$/i)?.[1] || null;
  posthog.capture("ac_piece_opened", {
    piece: namedPiece,
    piece_kind: namedPiece ? "built-in" : "published-or-code",
    route,
  });
}

function watchRoutes(win, posthog) {
  for (const method of ["pushState", "replaceState"]) {
    const original = win.history[method];
    win.history[method] = function (...args) {
      const result = original.apply(this, args);
      queueMicrotask(() => captureRoute(win, posthog));
      return result;
    };
  }
  win.addEventListener("popstate", () => captureRoute(win, posthog));
}

function watchIdentity(win, posthog) {
  const sync = () => {
    const identity = userIdentity(win.acUSER);
    if (identity) {
      const key = `${identity.distinctId}:${identity.properties.handle || ""}`;
      if (key !== lastIdentity) {
        posthog.identify(identity.distinctId, identity.properties);
        lastIdentity = key;
      }
      return;
    }
    if (lastIdentity && win.acAuthTiming?.sessionStartedSent) {
      posthog.reset();
      lastIdentity = null;
    }
  };
  sync();
  win.setInterval(sync, 1000);
}

export function initProductAnalytics({ win = window, doc = document } = {}) {
  if (initialized) return win.posthog || null;
  const config = win.acPostHogConfig;
  if (
    !config?.projectToken ||
    !CLOUD_HOSTS.has(config.apiHost) ||
    win !== win.top ||
    win.acPACK_MODE ||
    !["aesthetic.computer", "staging.aesthetic.computer"].includes(
      win.location.hostname,
    )
  ) {
    return null;
  }

  const posthog = installPostHogStub(win, doc);
  posthog.init(config.projectToken, {
    ...POSTHOG_OPTIONS,
    api_host: config.apiHost,
    ui_host: config.uiHost,
    before_send(event) {
      const properties = { ...(event.properties || {}) };
      const route = safeRoutePath(win.location.pathname);
      properties.$current_url = `${win.location.origin}${route}`;
      delete properties.$initial_current_url;
      delete properties.$initial_referrer;
      delete properties.$referrer;
      delete properties.$session_entry_url;
      return { ...event, properties };
    },
  });
  initialized = true;
  captureRoute(win, posthog);
  watchRoutes(win, posthog);
  watchIdentity(win, posthog);
  return posthog;
}

export function capturePieceOpened(diskPath, { win = window } = {}) {
  if (!initialized) initProductAnalytics({ win, doc: win.document });
  win.posthog?.capture?.(
    "ac_piece_opened",
    pieceProperties(diskPath, win.location.pathname),
  );
}

export function identifyUser(user, { win = window } = {}) {
  const identity = userIdentity(user);
  if (!identity) return;
  if (!initialized) initProductAnalytics({ win, doc: win.document });
  win.posthog?.identify?.(identity.distinctId, identity.properties);
}

export function resetProductAnalytics({ win = window } = {}) {
  win.posthog?.reset?.();
}

if (typeof window !== "undefined" && typeof document !== "undefined") {
  initProductAnalytics();
}
