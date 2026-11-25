// grab-duds.mjs — telling a failed render from a thumbnail.
//
// Pure helpers for grabber.mjs, kept apart so they can be tested without
// loading Puppeteer, Mongo and the grabber's timers (see test-duds.mjs).

/**
 * A uniform-color capture is either a piece that really is one flat color or a
 * piece that failed to render. The two are indistinguishable in the pixels, so
 * a dud is captured again — but each time it comes back a dud for the same
 * source, it waits longer before the next try: 10 min → 1 h → 1 day → never.
 * An edited source (a new `sourceHash`) starts the ladder again.
 *
 * The ladder is OVEN_DUD_BACKOFF, a comma list of durations (`10m`, `1h`,
 * `1d`, `never`, or milliseconds). OVEN_DUD_BACKOFF=10m is the old behaviour
 * (every dud re-baked after ten minutes, forever).
 *
 * New grab records always carry `dud: true|false`. A record with no `dud`
 * field predates the flag (or came from the old frozen path, which never set
 * it); a tiny one of those is suspect once, so it is re-baked a single time
 * and then judged by the flag.
 */
export const DUD_CACHE_TTL_MS = 10 * 60 * 1000;
const LEGACY_DUD_MAX_BYTES = 1024;
const DAY_MS = 24 * 60 * 60 * 1000;

export function parseBackoff(text) {
  const units = { ms: 1, s: 1000, m: 60000, h: 3600000, d: DAY_MS };
  const steps = String(text ?? '').split(',').map((part) => {
    const t = part.trim().toLowerCase();
    if (t === 'never' || t === 'inf' || t === 'infinity') return Infinity;
    const m = t.match(/^(\d+(?:\.\d+)?)(ms|s|m|h|d)?$/);
    return m ? Number(m[1]) * units[m[2] || 'ms'] : NaN;
  });
  return steps.length && steps.every((v) => v > 0) ? steps : null;
}
export const DUD_BACKOFF_MS = parseBackoff(globalThis.process?.env?.OVEN_DUD_BACKOFF) ?? [DUD_CACHE_TTL_MS, 3600000, DAY_MS, Infinity];

/** How long a dud on its `attempt`-th try (1-based) is trusted. Without a
 *  source hash an edit cannot be noticed, so `never` is capped at a day. */
export function dudBackoffMs(attempt, { hashKnown = true, backoff = DUD_BACKOFF_MS } = {}) {
  const i = Math.max(0, Math.min(backoff.length - 1, (Number(attempt) || 1) - 1));
  const ms = backoff[i];
  return ms === Infinity && !hashKnown ? DAY_MS : ms;
}

export function isSuspectDud(grab) {
  if (!grab) return false;
  if (grab.dud === true) return true;
  return grab.dud === undefined && grab.size > 0 && grab.size < LEGACY_DUD_MAX_BYTES;
}

/**
 * Should this cached dud be captured again?
 * @param {object} grab  the newest oven-grabs record for the capture key
 * @param {number} now
 * @param {{ sourceHash?: string|null, headlessVersion?: string|null, backoff?: number[] }} current
 *   sourceHash: the piece's source as it is now (null when not looked up);
 *   headlessVersion: the headless renderer now running, so a dud the
 *   headless precheck called final is re-judged when the renderer changes.
 */
export function isStaleDud(grab, now = Date.now(), { sourceHash = null, headlessVersion = null, backoff = DUD_BACKOFF_MS } = {}) {
  if (!isSuspectDud(grab)) return false;
  if (sourceHash && grab.sourceHash && sourceHash !== grab.sourceHash) return true; // edited
  if (headlessVersion && typeof grab.dudBy === 'string' && grab.dudBy.startsWith('headless-') && grab.dudBy !== `headless-${headlessVersion}`) return true;
  const at = new Date(grab.completedAt ?? grab.startTime ?? 0).getTime();
  const ttl = dudBackoffMs(grab.dudAttempt ?? 1, { hashKnown: !!grab.sourceHash, backoff });
  return !(now - at < ttl);
}

/** The try number for a new dud, given the dud it replaces (if any). */
export function nextDudAttempt(previous, sourceHash = null) {
  if (!isSuspectDud(previous)) return 1;
  if (sourceHash && previous.sourceHash && sourceHash !== previous.sourceHash) return 1;
  return (Number(previous.dudAttempt) || 1) + 1;
}

/** Should a fresh-by-backoff dud have its source looked up for an edit?
 *  Only once it has outlived the first rung (so a cache hit never fetches
 *  for a new dud) and only when there is a hash to compare against. */
export function dudWantsSourceCheck(grab, now = Date.now(), backoff = DUD_BACKOFF_MS) {
  if (!isSuspectDud(grab) || !grab.sourceHash) return false;
  const at = new Date(grab.completedAt ?? grab.startTime ?? 0).getTime();
  return now - at >= backoff[0];
}

/**
 * Is a stack of frames one flat colour? Judged over every pixel of every
 * frame together (flashing between colours is not uniform), by coverage: the
 * commonest colour, ± `tolerance` per channel (lossy-WebP / dither noise),
 * must cover at least `threshold` of all pixels. A thin line or a small mark
 * on a flat ground stays above the line; a smooth gradient never comes close.
 * @param {{ data: Uint8Array|Uint8ClampedArray, channels: number }[]} frames
 *   raw pixels, all the same size (grabber downsizes to 64² first)
 * @returns {{ isUniform: boolean, colour: number[]|null, coverage: number }}
 */
export function uniformCoverage(frames, { tolerance = 8, threshold = 0.998 } = {}) {
  if (!frames?.length) return { isUniform: false, colour: null, coverage: 0 };
  // Commonest colour by 4-bit-per-channel bucket, then its exact mean.
  const counts = new Map();
  let total = 0;
  for (const { data, channels } of frames) {
    for (let i = 0; i < data.length; i += channels) {
      const key = ((data[i] >> 4) << 8) | ((data[i + 1] >> 4) << 4) | (data[i + 2] >> 4);
      counts.set(key, (counts.get(key) || 0) + 1);
      total++;
    }
  }
  let bestKey = -1, best = -1;
  for (const [k, n] of counts) if (n > best) { best = n; bestKey = k; }
  let sr = 0, sg = 0, sb = 0, sn = 0;
  for (const { data, channels } of frames) {
    for (let i = 0; i < data.length; i += channels) {
      const key = ((data[i] >> 4) << 8) | ((data[i + 1] >> 4) << 4) | (data[i + 2] >> 4);
      if (key === bestKey) { sr += data[i]; sg += data[i + 1]; sb += data[i + 2]; sn++; }
    }
  }
  const colour = [Math.round(sr / sn), Math.round(sg / sn), Math.round(sb / sn)];
  let near = 0;
  for (const { data, channels } of frames) {
    for (let i = 0; i < data.length; i += channels) {
      if (Math.abs(data[i] - colour[0]) <= tolerance && Math.abs(data[i + 1] - colour[1]) <= tolerance && Math.abs(data[i + 2] - colour[2]) <= tolerance) near++;
    }
  }
  const coverage = total ? near / total : 0;
  return { isUniform: coverage >= threshold, colour, coverage };
}

/**
 * uniformCoverage over encoded frames (PNG/WebP buffers): up to `maxFrames`
 * evenly spread, each downsized to `size`² by area averaging, so a 1 px line
 * at 512² still moves its 64² pixels well past the tolerance.
 * @param {Buffer[]} frames
 * @param {Function} sharp  the sharp module (injected: this file stays pure)
 */
export async function uniformFromImages(frames, sharp, { size = 64, maxFrames = 32, ...opts } = {}) {
  if (!frames?.length) return { isUniform: false, colour: null, coverage: 0, frames: 0 };
  const raws = [];
  for (const i of sampleIndices(frames.length, maxFrames)) {
    const { data, info } = await sharp(frames[i])
      .removeAlpha()
      .resize(size, size, { fit: 'fill', kernel: 'cubic' })
      .raw()
      .toBuffer({ resolveWithObject: true });
    raws.push({ data, channels: info.channels });
  }
  return { ...uniformCoverage(raws, opts), frames: raws.length };
}

/** Evenly spread indices into a list of `length`, at most `max` of them. */
export function sampleIndices(length, max = 32) {
  if (length <= max) return Array.from({ length }, (_, i) => i);
  return Array.from({ length: max }, (_, i) => Math.round((i * (length - 1)) / (max - 1)));
}

/**
 * What the headless precheck's outcome means for a $code grab.
 *   'dud'     blank / one flat colour: record a final dud, skip Chrome
 *   'missing' the source is not there (HTTP 404): fail without Chrome
 *   'drawn'   headless drew it (its output may stand in for a black Chrome grab)
 *   'chrome'  anything else (unsupported, timeout, boot, painting...): Chrome as before
 */
export function precheckVerdict(outcome) {
  if (!outcome) return 'chrome';
  if (outcome.out) return 'drawn';
  const reason = outcome.error?.reason;
  if (reason === 'blank' || reason === 'uniform') return 'dud';
  // Not found, or saved empty: there is nothing for Chrome to load either.
  if (reason === 'source' && (outcome.error?.status === 404 || /^(No|Empty) KidLisp source/.test(outcome.error?.message ?? ''))) return 'missing';
  return 'chrome';
}

/** The flat colour a blank/uniform HeadlessKidlispError reports. */
export function precheckColour(error) {
  if (Array.isArray(error?.colour) && error.colour.length >= 3) return error.colour.slice(0, 3);
  const m = String(error?.message ?? '').match(/one colou?r \((\d+),(\d+),(\d+)\)/);
  return m ? [Number(m[1]), Number(m[2]), Number(m[3])] : [0, 0, 0];
}

/**
 * Piece names no grab should ever load: path traversal, shell/template
 * punctuation and other scanner noise. AC's own names (`name`, `$code`,
 * `@handle/piece`, `#painting`, `!tape`, `name~a~b`, `name:opt`) pass.
 */
export function isJunkPieceName(piece) {
  if (typeof piece !== 'string' || !piece) return true;
  if (piece.length > 200) return true;
  if (/\.\.[/\\]|^[/\\]|\/etc\/|^-|[<>{}\\|`;\x00-\x1f\x7f]/.test(piece)) return true;
  if (piece.startsWith('$') && !/^\$[A-Za-z0-9][\w~:.-]{1,127}$/.test(piece)) return true;
  return false;
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
