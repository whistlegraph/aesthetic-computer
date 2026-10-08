// App registry — every app that reports devices or receives pushes.
// One row per app. Adding an app is adding a row here; the device registry
// (app-devices.mjs), /api/app-device and push routing all read this table.
//
//   tenant  Auth0 tenant that signs the app's users in ("aesthetic" | "sotce").
//           sotce users are stored as "sotce-" + sub, the repo-wide convention.
//   apns    APNs topic (bundle id) for native Apple builds, or null.
//   web     true when the app is a web/PWA surface that registers Web Push.
//   wired   true once the app's client actually reports. Others are ready to
//           hook in; the endpoint accepts them already.
export const APPS = Object.freeze({
  whistlegraph:      { tenant: "aesthetic", apns: "computer.aesthetic.walkieware", web: false, wired: true },
  aestheticcomputer: { tenant: "aesthetic", apns: "aesthetic.computer",            web: true,  wired: false },
  aesel:             { tenant: "aesthetic", apns: "computer.aesthetic.easel",      web: false, wired: true },
  oskiewar:          { tenant: "aesthetic", apns: "computer.aesthetic.oskiewar",   web: true,  wired: false },
  menuband:          { tenant: "aesthetic", apns: "computer.aesthetic.menuband",   web: false, wired: false },
  "sotce-net":       { tenant: "sotce",     apns: null,                            web: true,  wired: false },
});

export const PLATFORMS = Object.freeze(["ios", "ipados", "mac", "web", "windows", "xbox", "linux", "android"]);

export function appConfig(id) {
  return Object.hasOwn(APPS, id) ? APPS[id] : null;
}

// The stored user key for a verified Auth0 subject in this app's tenant.
export function userKey(app, sub) {
  if (!sub) return null;
  return appConfig(app)?.tenant === "sotce" ? `sotce-${sub}` : sub;
}
