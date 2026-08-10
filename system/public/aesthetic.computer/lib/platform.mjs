let nav;

try {
  nav = navigator;
} catch (e) {
  nav = {};
}

export const iOS = /(iPad|iPhone|iPod)/g.test(nav.userAgent);
export const Safari = /apple/i.test(nav.vendor);
export const Android = /(Android)/g.test(nav.userAgent);
export const MetaBrowser = /(OculusBrowser)/g.test(nav.userAgent);
export const Desktop = !iOS && !Android && !MetaBrowser;
// iPadOS can advertise itself as Macintosh. A touch-capable "Mac" is an iPad,
// so keep the desktop-only display treatment off of it.
export const MacOS = /(Macintosh|Mac OS X)/g.test(nav.userAgent) &&
  (nav.maxTouchPoints || 0) <= 1;
export const Instagram = /(Instagram)/g.test(nav.userAgent);
export const TikTok = /BytedanceWebview/i.test(nav.userAgent);
export const Aesthetic = /Aesthetic/i.test(nav.userAgent);
export const AestheticExtension = /AestheticExtension/i.test(nav.userAgent);
// The native iPhone shell deliberately uses the exact custom UA "Aesthetic",
// which removes the usual iPhone/iPad tokens from WKWebView's user agent.
// Keep this narrower than `Aesthetic`: desktop hosts use that word too.
export function isAestheticIOSAppUserAgent(userAgent) {
  return /^Aesthetic$/i.test(String(userAgent || "").trim());
}
export const AestheticIOSApp = isAestheticIOSAppUserAgent(nav.userAgent);
