export const NOPAINT_SESSION_SEED_KEY = "nopaint:session-seed";

const isNoPaintHost = (hostname) =>
  hostname === "nopaint.art" || hostname === "www.nopaint.art";

export function noPaintStartingPiece(url, navigationType, savedSeed) {
  if (!isNoPaintHost(url.hostname)) return null;
  if (url.pathname !== "/") return "nopaint";
  const code = url.hash.slice(1);
  if (/^[a-zA-Z0-9]{3,12}$/.test(code) && !["debug", "nodebug"].includes(code)) {
    return "painting";
  }
  // Session storage belongs to this tab. A new navigation starts fresh even
  // if the browser copied storage when opening or duplicating a tab.
  return navigationType === "reload" && /^\d+$/.test(savedSeed || "")
    ? `nopaint:${savedSeed}`
    : "nopaint~fresh";
}

export function noPaintHistoryTarget(path, currentURL) {
  const url = new URL(path, currentURL);
  const seed = url.pathname.match(/^\/nopaint:(\d+)$/)?.[1];
  if (!isNoPaintHost(url.hostname) || !seed) return { path, seed: null };
  return { path: `/${url.search}${url.hash}`, seed };
}
