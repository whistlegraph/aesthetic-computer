import { oskiewarEvent } from "./oskiewar-analytics.mjs";

const CLOUD_HOSTS = new Set([
  "https://us.i.posthog.com",
  "https://eu.i.posthog.com",
]);

const PRODUCT_HOSTS = new Set([
  "aesthetic.computer",
  "oskiewar.com",
  "staging.aesthetic.computer",
  "www.oskiewar.com",
]);
const OSKIEWAR_HOSTS = new Set(["oskiewar.com", "www.oskiewar.com"]);
const OSKIEWAR_ROUND = /^(?:[a-z]{4,7}[0-9]{1,3}|(?:[bdfgklmnprstvz][aeiou]){3}-(?:[bdfgklmnprstvz][aeiou]){3}-(?:[bdfgklmnprstvz][aeiou]){3})$/;

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

export function safeAnalyticsRoute(pathname = "/", hostname = "") {
  if (!OSKIEWAR_HOSTS.has(String(hostname).toLowerCase())) {
    return safeRoutePath(pathname);
  }
  const path = String(pathname).replace(/^\/+|\/+$/g, "").toLowerCase();
  if (!path) return "/oskiewar";
  if (OSKIEWAR_ROUND.test(path)) return "/oskiewar/round";
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
let interactedRoute = null;

const PROMPT_ROUTES = new Set(["/", "/prompt"]);
const MEDIA_KINDS = Object.freeze({
  png: "painting",
  mjs: "piece",
  lisp: "kidlisp",
  zip: "tape",
  mp4: "tape",
});

export function routePieceProperties(route) {
  if (PROMPT_ROUTES.has(route) || route === "/_other") return null;
  if (route === "/oskiewar" || route === "/oskiewar/round") {
    return { piece: "oskiewar", piece_kind: "built-in", route };
  }
  const namedPiece = route.match(/^\/([a-z0-9-]{1,64})$/i)?.[1] || null;
  return {
    piece: namedPiece,
    piece_kind: namedPiece ? "built-in" : "published-or-code",
    route,
  };
}

export function mediaCreatedProperties(extension, identified) {
  const mediaKind = MEDIA_KINDS[String(extension || "").toLowerCase()];
  if (!mediaKind) return null;
  return {
    media_kind: mediaKind,
    account_state: identified ? "identified" : "anonymous",
  };
}

function captureRoute(win, posthog) {
  const route = safeAnalyticsRoute(
    win.location.pathname,
    win.location.hostname,
  );
  if (route === lastRoute) return;
  const previousRoute = lastRoute;
  lastRoute = route;
  interactedRoute = null;
  posthog.capture("$pageview", {
    $current_url: `${win.location.origin}${route}`,
    ac_route: route,
  });

  const piece = routePieceProperties(route);
  if (!piece) return;
  posthog.capture("ac_piece_opened", piece);
  if (PROMPT_ROUTES.has(previousRoute)) {
    posthog.capture("ac_prompt_succeeded", piece);
  }
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

function watchInteractions(win, posthog) {
  const capture = (inputKind) => {
    const route = safeAnalyticsRoute(
      win.location.pathname,
      win.location.hostname,
    );
    if (route === interactedRoute) return;
    const piece = routePieceProperties(route);
    if (!piece) return;
    interactedRoute = route;
    posthog.capture("ac_piece_interacted", {
      ...piece,
      input_kind: inputKind,
    });
  };
  win.addEventListener("pointerdown", () => capture("pointer"), true);
  win.addEventListener("touchstart", () => capture("touch"), true);
  win.addEventListener("keydown", () => capture("keyboard"), true);
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
    !PRODUCT_HOSTS.has(win.location.hostname)
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
      const route = safeAnalyticsRoute(
        win.location.pathname,
        win.location.hostname,
      );
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
  watchInteractions(win, posthog);
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

export function captureProductAction(action, { win = window } = {}) {
  const event = { handle_created: "ac_handle_created" }[action];
  if (!event) return false;
  if (!initialized) initProductAnalytics({ win, doc: win.document });
  win.posthog?.capture?.(event);
  return true;
}

export function captureMediaCreated(
  extension,
  identified,
  { win = window } = {},
) {
  const properties = mediaCreatedProperties(extension, identified);
  if (!properties) return false;
  if (!initialized) initProductAnalytics({ win, doc: win.document });
  win.posthog?.capture?.("ac_media_created", properties);
  return true;
}

export function captureOskiewarAction(
  action,
  properties,
  { win = window } = {},
) {
  const item = oskiewarEvent(action, properties);
  if (!item) return false;
  if (!initialized) initProductAnalytics({ win, doc: win.document });
  win.posthog?.capture?.(item.event, item.properties);
  return true;
}

export function resetProductAnalytics({ win = window } = {}) {
  win.posthog?.reset?.();
}

if (typeof window !== "undefined" && typeof document !== "undefined") {
  initProductAnalytics();
}
