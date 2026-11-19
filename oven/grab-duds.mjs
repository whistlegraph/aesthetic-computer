// grab-duds.mjs — telling a failed render from a thumbnail.
//
// Pure helpers for grabber.mjs, kept apart so they can be tested without
// loading Puppeteer, Mongo and the grabber's timers.

/**
 * A uniform-color capture is either a piece that really is one flat color or a
 * piece that failed to render. The two are indistinguishable in the pixels, so
 * a dud is trusted only briefly and then captured again. Records written
 * before the `dud` flag existed are recognized by size: a flat 512px WebP
 * still is ~0.5 KB, where real content runs to several KB.
 */
export const DUD_CACHE_TTL_MS = 10 * 60 * 1000;
const DUD_MAX_BYTES = 1024;
export function isSuspectDud(grab) {
  return !!grab && (grab.dud === true || (grab.size > 0 && grab.size < DUD_MAX_BYTES));
}
export function isStaleDud(grab, now = Date.now()) {
  if (!isSuspectDud(grab)) return false;
  const at = new Date(grab.completedAt ?? grab.startTime ?? 0).getTime();
  return !(now - at < DUD_CACHE_TTL_MS);
}

/**
 * Did this failed response carry the source of the piece being captured?
 * Only a $code piece's own /api/store-kidlisp read counts: embedded pieces and
 * unrelated endpoints may fail without the capture being worthless.
 * @returns {string|null} An error message, or null when the capture can go on.
 */
export function pieceSourceFailure(piece, url, status) {
  if (typeof piece !== 'string' || !piece.startsWith('$') || !(status >= 400)) return null;
  let parsed;
  try { parsed = new URL(url); } catch { return null; }
  if (parsed.pathname !== '/api/store-kidlisp') return null;
  if (parsed.searchParams.get('code') !== piece.slice(1)) return null;
  return status === 404
    ? `KidLisp piece ${piece} not found (HTTP 404 from /api/store-kidlisp)`
    : `KidLisp source for ${piece} failed to load (HTTP ${status} from /api/store-kidlisp)`;
}

/**
 * The page a capture loads. A painting is addressed by fragment (`/#code`), so
 * the capture flags must come before the '#': appended after it they become
 * part of the fragment, the runtime sees neither flags nor painting, and the
 * capture is of the prompt.
 */
export function captureUrl(baseUrl, piece, query) {
  return piece.startsWith('#') ? `${baseUrl}/?${query}${piece}` : `${baseUrl}/${piece}?${query}`;
}

/**
 * A grab is stored under its piece name, and a painting's name starts with
 * '#'. Unescaped in a redirect, the browser reads the rest of the URL as a
 * fragment and asks the CDN for the bare folder (403).
 */
export function cdnHref(url) {
  return typeof url === 'string' ? url.replace(/#/g, '%23') : url;
}
