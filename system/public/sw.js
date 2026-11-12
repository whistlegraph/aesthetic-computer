// Aesthetic Computer Service Worker
// Caches JavaScript modules for faster subsequent loads

const CACHE_NAME = 'ac-modules-v10'; // Bump: clear stale capture-session modules after the camera API expansion
const CACHE_DURATION = 60 * 60 * 1000; // 1 hour in ms (dev-friendly)

// Critical modules to precache on install. NOTE: bios.mjs and lib/disk.mjs
// are intentionally absent — they need to ship updates within seconds, so
// they ride the network-first path below (conditional revalidate, cached copy
// only after NETWORK_FIRST_TIMEOUT_MS) rather than a precache.
const PRECACHE_MODULES = [
  '/aesthetic.computer/boot.mjs',
  '/aesthetic.computer/lib/parse.mjs',
  '/aesthetic.computer/lib/graph.mjs',
  '/aesthetic.computer/lib/num.mjs',
  '/aesthetic.computer/lib/help.mjs',
  '/aesthetic.computer/lib/geo.mjs',
  '/aesthetic.computer/lib/text.mjs',
  '/aesthetic.computer/lib/ui.mjs',
  '/aesthetic.computer/lib/platform.mjs',
  '/aesthetic.computer/lib/kidlisp.mjs',
  '/aesthetic.computer/lib/type.mjs',
  '/aesthetic.computer/lib/pen.mjs',
  '/aesthetic.computer/lib/keyboard.mjs',
  '/aesthetic.computer/lib/loop.mjs',
  '/aesthetic.computer/lib/store.mjs',
  '/aesthetic.computer/lib/headers.mjs',
  '/aesthetic.computer/lib/logs.mjs',
  '/aesthetic.computer/lib/helpers.mjs',
  '/aesthetic.computer/style.css',
];

// Patterns to cache (stale-while-revalidate)
const CACHEABLE_PATTERNS = [
  /\/aesthetic\.computer\/.*\.mjs$/,
  /\/aesthetic\.computer\/.*\.js$/,
  /\/aesthetic\.computer\/.*\.css$/,
  /\/aesthetic\.computer\/dep\/.*\.mjs$/,
  /\/aesthetic\.computer\/systems\/.*\.mjs$/,
];

// Network-first (conditional revalidate; cache is only an offline/failure
// fallback). Pieces under /disks/ are never cached, but they import shared
// modules from /lib/ — when a commit changes a piece AND its lib together,
// stale-while-revalidate hands the fresh piece a one-version-old lib and the
// import explodes (e.g. catnom.mjs vs a cached nom.mjs missing an export).
// Keeping piece-importable modules network-first preserves that lockstep;
// the `cache: 'no-cache'` conditional fetch below makes the common case a
// cheap 304. /systems/ rides along for the same reason (disk.mjs, itself
// never cached, imports them).
const NETWORK_FIRST_PATTERNS = [
  /\/aesthetic\.computer\/lib\/.*\.mjs$/,
  /\/aesthetic\.computer\/systems\/.*\.mjs$/,
  // bios.mjs (244 KB gz) was NEVER_CACHE — a full download on every boot,
  // 13–100 s on a lossy link. Network-first keeps it fresh (the origin serves
  // it no-cache, so the conditional fetch is a 304 when unchanged) and hands
  // out the last good copy once the link stalls past the timeout. lib/disk.mjs
  // already matches the lib/ pattern above.
  /\/aesthetic\.computer\/bios\.mjs$/,
  // boot.mjs imports bios; serving it stale-while-revalidate could pair an
  // old boot with a fresh bios for one load after a deploy. Its check rides
  // the same parallel wave as bios's, so it costs no extra round trip.
  /\/aesthetic\.computer\/boot\.mjs$/,
];

// How long a network-first module waits for its conditional fetch before the
// cached copy goes out instead (the fetch still completes and refreshes the
// cache). Boot logs from a Mac on a stalling link (2026-09-04) showed ~1 in 4
// fetches hanging 10–23s and a single boot sitting on dozens of them. A
// healthy link revalidates in well under a second and never hits this. The
// lockstep concern above still holds in the fallback window: a fresh piece
// can meet a lib cached before the last deploy. That trades a 20s hang for a
// rare failed import that a reload fixes.
const NETWORK_FIRST_TIMEOUT_MS = 3000;

// Content-hashed bundles: the hash in the filename is the version (bios.mjs
// reads the current one from disk-worker-manifest.json, fetched no-cache), so
// a cached copy never goes stale — serve it without touching the network.
const IMMUTABLE_PATTERNS = [
  /\/aesthetic\.computer\/lib\/disk\.worker\.[a-f0-9]+\.mjs$/,
];

// Revalidations in flight, by URL. Network-first keeps every module one round
// trip to lith, and a worker's import graph is several waves deep (the prompt's
// is three: ~155 ms each, warm, every boot). When the first module of a graph
// arrives here, its cached dependency closure is read out of the cached bodies
// and revalidated all at once; later imports join these fetches instead of
// starting their own. Every module is still checked against the network, so
// the piece/lib lockstep holds. Entries leave the map shortly after settling.
const inflight = new Map();
const INFLIGHT_LINGER_MS = 5000;
const depsMemo = new Map(); // url → { tag, deps: Promise<urls> }
const PIECE_SOURCE = /^\/aesthetic\.computer\/disks\/.*\.mjs$/;
const STATIC_IMPORT = /\b(?:import|export)\s*(?:[\w*{}\s,$]*?\s*from\s*)?["']([^"'\n]+\.m?js)["']/g;

// Never cache these (always network)
const NEVER_CACHE = [
  /\/api\//,
  /\/disks\/.*\.mjs$/, // User pieces should always be fresh
  /\?v=/, // Cache-busted URLs
  /localhost:8889/, // Session server
];

self.addEventListener('install', (event) => {
  console.log('🔧 SW: Installing...');
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => {
        console.log('🔧 SW: Precaching critical modules');
        return cache.addAll(PRECACHE_MODULES);
      })
      .then(() => self.skipWaiting())
      .catch((err) => {
        console.warn('🔧 SW: Precache failed (non-fatal):', err.message);
        return self.skipWaiting();
      })
  );
});

self.addEventListener('activate', (event) => {
  console.log('🔧 SW: Activating...');
  event.waitUntil(
    caches.keys()
      .then((cacheNames) => {
        return Promise.all(
          cacheNames
            .filter((name) => name !== CACHE_NAME && name !== BUILD_META && !name.startsWith(BUILD_PREFIX))
            .map((name) => {
              console.log('🔧 SW: Deleting old cache:', name);
              return caches.delete(name);
            })
        );
      })
      .then(() => self.clients.claim())
      .then(() => checkForBuild())
  );
});

self.addEventListener('fetch', (event) => {
  const url = new URL(event.request.url);
  
  // Skip non-GET requests
  if (event.request.method !== 'GET') return;

  // A deployed build answers from its own cache first (see BUILD below).
  if (buildHandles(event, url)) return;
  
  // A piece's source names the modules its worker is about to import, wave by
  // wave. The shell fetches it at parse time, long before the worker exists:
  // read its imports on the way through and start revalidating the closure.
  // The piece itself still goes straight to the network, uncached.
  // When the shell's fetch hasn't landed by the time the worker wants the piece,
  // the worker asks again (with a ?v= bust); hand it the shell's response
  // instead of a second round trip.
  if (url.origin === self.location.origin && PIECE_SOURCE.test(url.pathname)) {
    const joined = inflight.get(url.pathname);
    if (joined) {
      event.respondWith(joined.then((r) => r.clone(), () => fetch(event.request)));
      return;
    }
    // Joinable only while in flight: a settled piece is never handed out again.
    const piece = fetch(event.request);
    inflight.set(url.pathname, piece);
    const drop = () => { if (inflight.get(url.pathname) === piece) inflight.delete(url.pathname); };
    piece.then(drop, drop);
    event.respondWith(
      piece.then((r) => r.clone()).then((response) => {
        if (response.ok && !walked.has(url.pathname)) {
          markWalked(url.pathname);
          event.waitUntil(
            response.clone().text()
              .then(async (source) => prefetchFrom(await caches.open(CACHE_NAME), importsOf(source, url.href)))
              .catch(() => {}),
          );
        }
        return response;
      }),
    );
    return;
  }

  // Skip never-cache patterns
  if (NEVER_CACHE.some((pattern) => pattern.test(url.pathname) || pattern.test(url.href))) {
    return;
  }
  
  // Check if this is a cacheable request
  const isCacheable = CACHEABLE_PATTERNS.some((pattern) => pattern.test(url.pathname));

  // Piece-importable modules prefer the network (cache = fallback only)
  const preferNetwork = NETWORK_FIRST_PATTERNS.some((pattern) => pattern.test(url.pathname));
  
  // For core modules, create a clean cache key without query params
  // This ensures disk.mjs?session-aesthetic=... matches cached /aesthetic.computer/lib/disk.mjs
  const isCoreModule = PRECACHE_MODULES.some((m) => url.pathname === m);
  const isImmutable = IMMUTABLE_PATTERNS.some((pattern) => pattern.test(url.pathname));
  const cacheKey = isCoreModule || isImmutable ? new Request(url.origin + url.pathname) : event.request;

  if (isImmutable) {
    // Cache-first, no revalidation: the hash in the name is the version, so a
    // cached copy is the copy. On a miss, fetch once, store it, and drop the
    // previous hashes — each deploy would otherwise leave ~1.7MB behind.
    event.respondWith(
      caches.open(CACHE_NAME).then(async (cache) => {
        const cachedResponse = await cache.match(cacheKey);
        if (cachedResponse) return cachedResponse;
        const networkResponse = await fetch(event.request);
        if (networkResponse.ok && networkResponse.status === 200) {
          const store = (async () => {
            try {
              const responseToCache = networkResponse.clone();
              const body = await responseToCache.clone().text();
              if (!body || body.length === 0) return;
              await cache.put(cacheKey, responseToCache);
              const keys = await cache.keys();
              await Promise.all(
                keys
                  .filter((key) => key.url !== cacheKey.url)
                  .filter((key) => IMMUTABLE_PATTERNS.some((pattern) => pattern.test(new URL(key.url).pathname)))
                  .map((key) => cache.delete(key)),
              );
            } catch (e) {
              // Incomplete download — leave the cache alone; next boot retries.
            }
          })();
          event.waitUntil(store);
        }
        return networkResponse;
      })
    );
    return;
  }

  // The page asked for a cached copy only (the boot canvas reading module
  // source back for display). Honor that: never revalidate, never download.
  // Without a cached copy, pass the request through untouched so the browser
  // HTTP cache applies the same rule (only-if-cached → 504 on a miss).
  const cacheOnly = event.request.cache === 'only-if-cached' || event.request.cache === 'force-cache';

  if (isCacheable) {
    // Stale-while-revalidate strategy with corruption detection
    event.respondWith(
      caches.open(CACHE_NAME).then((cache) => {
        // Use clean cache key for core modules to ignore query params
        return cache.match(cacheKey).then(async (cachedResponse) => {
          if (cacheOnly) return cachedResponse || fetch(event.request);
          if (preferNetwork) {
            // Only a worker's imports arrive in waves: the page's module graph
            // is modulepreloaded flat by the shell.
            if (cachedResponse) {
              self.clients.get(event.clientId).then((client) => {
                if (client?.type === 'worker') prefetchClosure(cache, url.href);
              }, () => {});
            }
            const joined = inflight.get(url.href);
            if (joined) return withTimeout(joined.then((r) => r.clone()), cachedResponse, event);
          }
          const fetchPromise = revalidate(cache, cacheKey, event.request, cachedResponse);
          if (preferNetwork) track(url.href, fetchPromise);

          // Network-first modules wait for the (conditional, usually-304)
          // fetch — its catch already falls back to cachedResponse offline —
          // but only for NETWORK_FIRST_TIMEOUT_MS when a cached copy exists.
          // On a link that stalls (a lost packet's retransmit backoff runs
          // 1s, 2s, 4s, 8s…) a boot used to sit on ~35 of these round trips;
          // now it gets the last good module after the timeout while the
          // fetch finishes in the background and refreshes the cache. A
          // healthy link answers in well under the timeout and sees no change.
          // Everything else returns the cached response immediately (SWR).
          if (preferNetwork) {
            if (!cachedResponse) return fetchPromise.then((r) => r.clone());
            return withTimeout(fetchPromise.then((r) => r.clone()), cachedResponse, event);
          }
          if (cachedResponse) {
            // Keep the worker alive until the background revalidation lands.
            event.waitUntil(fetchPromise.catch(() => {}));
            return cachedResponse;
          }
          return fetchPromise;
        });
      })
    );
  }
});

// Race a network-first fetch against NETWORK_FIRST_TIMEOUT_MS, then hand out
// the cached copy and let the fetch finish in the background.
async function withTimeout(fromNetworkPromise, cachedResponse, event) {
  if (!cachedResponse) return fromNetworkPromise;
  let timer;
  const timeout = new Promise((resolve) => {
    timer = setTimeout(() => resolve(null), NETWORK_FIRST_TIMEOUT_MS);
  });
  const fromNetwork = await Promise.race([
    fromNetworkPromise.then((response) => response, () => null),
    timeout,
  ]);
  clearTimeout(timer);
  if (fromNetwork) return fromNetwork;
  event.waitUntil(fromNetworkPromise.catch(() => {}));
  return cachedResponse;
}

function track(href, promise) {
  inflight.set(href, promise);
  const drop = () => setTimeout(() => {
    if (inflight.get(href) === promise) inflight.delete(href);
  }, INFLIGHT_LINGER_MS);
  promise.then(drop, drop);
}

// Static imports in `source` that are themselves network-first modules.
function importsOf(source, href) {
  const deps = [];
  for (const [, spec] of source.matchAll(STATIC_IMPORT)) {
    if (!spec.startsWith('.') && !spec.startsWith('/')) continue;
    const dep = new URL(spec, href);
    if (dep.origin !== self.location.origin || dep.search) continue;
    if (NETWORK_FIRST_PATTERNS.some((pattern) => pattern.test(dep.pathname))) deps.push(dep.href);
  }
  return deps;
}

// Static imports of a cached module, parsed once per module per service-worker
// lifetime; a copy refreshed with a new ETag (revalidate below) drops its entry.
function cachedDeps(cache, href) {
  if (!depsMemo.has(href)) {
    const entry = { tag: undefined, deps: null };
    depsMemo.set(href, entry);
    entry.deps = (async () => {
      const cached = await cache.match(href);
      if (!cached) return [];
      entry.tag = cached.headers.get('etag');
      return importsOf(await cached.text(), href);
    })().catch(() => []);
  }
  return depsMemo.get(href).deps;
}

// Modules (and piece paths) already walked during this burst are skipped, so a
// boot's dozens of imports cost one walk of the graph, not one each.
const walked = new Set();
function markWalked(key) {
  walked.add(key);
  setTimeout(() => walked.delete(key), INFLIGHT_LINGER_MS);
}

// Start revalidating every cached module reachable from `href`, in parallel.
async function prefetchClosure(cache, href) {
  if (walked.has(href)) return;
  markWalked(href);
  return prefetchFrom(cache, await cachedDeps(cache, href));
}

async function prefetchFrom(cache, roots) {
  try {
    let frontier = roots;
    while (frontier.length) {
      const next = [];
      for (const dep of new Set(frontier)) {
        if (walked.has(dep)) continue;
        markWalked(dep);
        next.push(dep);
        if (inflight.has(dep)) continue;
        const cachedResponse = await cache.match(dep);
        if (cachedResponse) track(dep, revalidate(cache, new Request(dep), new Request(dep), cachedResponse));
      }
      frontier = (await Promise.all(next.map((h) => cachedDeps(cache, h)))).flat();
    }
  } catch (e) {
    // A prefetch is only a head start; the import itself still fetches.
  }
}

// Conditional fetch that refreshes the cache. Resolves to a response that
// callers clone, so several imports can share one fetch.
function revalidate(cache, cacheKey, request, cachedResponse) {
  // Use `cache: 'no-cache'` so the browser sends a conditional request
  // (If-None-Match / If-Modified-Since) instead of serving from the HTTP
  // cache. Origin returns 304 when unchanged (cheap) or 200 with fresh
  // content. Without this, stale-while-revalidate can loop on a stale
  // response for up to max-age even after the origin has updated.
  const revalidateRequest = new Request(request, { cache: 'no-cache' });
  return fetch(revalidateRequest)
    .then(async (networkResponse) => {
      // An unchanged module (a 304 surfaces here as the HTTP cache's 200)
      // is already in the cache; skip re-reading and re-storing its body.
      const etag = networkResponse.headers.get('etag');
      if (etag && cachedResponse?.headers?.get('etag') === etag) return networkResponse;
      // Only cache successful, complete responses
      if (networkResponse.ok && networkResponse.status === 200) {
        try {
          // Clone and verify the response is complete
          const responseToCache = networkResponse.clone();
          // Read the body to verify it's complete (catches ERR_CONTENT_LENGTH_MISMATCH)
          const body = await responseToCache.clone().text();
          if (body && body.length > 0) {
            // Cache with clean key so future requests match
            await cache.put(cacheKey, responseToCache);
            const memo = depsMemo.get(new URL(revalidateRequest.url).href);
            if (memo && memo.tag !== networkResponse.headers.get('etag')) {
              depsMemo.delete(new URL(revalidateRequest.url).href);
            }
          }
        } catch (e) {
          // Response was incomplete/corrupted - use cache if available (silent)
          if (cachedResponse) return (await cache.match(cacheKey)) || networkResponse;
        }
      }
      return networkResponse;
    })
    .catch(async (err) => {
      // Return cached version if network fails (silent). A fresh read:
      // callers share and clone this result, and the copy they were
      // handed earlier may already have been read.
      const fallback = cachedResponse && (await cache.match(cacheKey));
      if (fallback) return fallback;
      // No cache available - retry before giving up
      for (let attempt = 1; attempt <= 2; attempt++) {
        try {
          await new Promise(r => setTimeout(r, 500 * attempt));
          const retryResponse = await fetch(revalidateRequest);
          if (retryResponse.ok) {
            try {
              const clone = retryResponse.clone();
              const body = await clone.clone().text();
              if (body && body.length > 0) {
                await cache.put(cacheKey, clone);
              }
            } catch {}
          }
          return retryResponse;
        } catch {}
      }
      throw err;
    });
}

// 📦 BUILD. A deploy is one build: the page shell, boot, bios, the worker
// bundle, lib/, pieces and fonts of a single commit. Once a build is
// installed, a launch is served entirely from its cache — no round trip
// before the prompt, and it boots offline. Meanwhile /api/build is asked
// which commit is deployed; a new one is downloaded beside the old (files
// whose git hash didn't change are copied, the rest fetched and checked
// against their hash), goes live for the next launch, and open pages hear
// `ac:build-ready`. Each page keeps the build it booted on, so a piece loaded
// late never meets a lib from another commit. Local dev has no deploy
// (/api/build says commit: null) and keeps the network-first path above.

const BUILD_PREFIX = 'ac-build-';
const BUILD_META = 'ac-meta'; // never under BUILD_PREFIX: installs prune that prefix
const BUILD_ACTIVE_KEY = '/__ac/build/active';
const SHELL_KEY = '/__ac/shell'; // + the page's pathname
// What a build owns: the same roots backend/build-ref.mjs lists.
const BUILD_SCOPE = /^\/(?:aesthetic\.computer\/|type\/|manifest\.json$)/;
// The server renders a different shell for these, so they always go out.
const SHELL_PARAMS = ['boot', 'noboot', 'icon', 'preview', 'menuband'];
// Pages the server answers itself rather than with a piece's shell.
const NOT_SHELL = /^\/(?:api|media|\.netlify|session|reload|user|handles?|authorized|run|cdn-cgi|lith|aesthetic\.computer|type)(?:\/|$)/;
// A build goes live only with all of these.
const BUILD_CORE = [
  '/aesthetic.computer/boot.mjs',
  '/aesthetic.computer/bios.mjs',
  '/aesthetic.computer/style.css',
  '/aesthetic.computer/lib/disk-worker-manifest.json',
];
const WORKER_BUNDLE = /^\/aesthetic\.computer\/lib\/disk\.worker\.[a-f0-9]+\.mjs$/;
const SHELLS_KEPT = 12; // visited pages carried into the next build, besides '/'
const BUILD_MAX_BYTES = 8 * 1024 * 1024;
const BUILD_CHECK_MS = 20 * 1000;
const BUILD_CHECK_DELAY_MS = 2000; // let a launch have the link first
const BUILD_WAIT_MS = 5000; // longest a reload waits on a downloading build

let activeBuild = null; // { commit, files: { path: hash }, hashChars }
let buildLoadedYet = false;
const buildLoaded = (async () => {
  try {
    const stored = await (await caches.open(BUILD_META)).match(BUILD_ACTIVE_KEY);
    if (stored) activeBuild = await stored.json();
  } catch {}
  buildLoadedYet = true;
  return activeBuild;
})();
const clientBuilds = new Map(); // clientId → the build that client runs on (null: network)
let lastPageBuild; // the build the newest page booted on; its workers inherit it
let lastBuildCheck = 0;
let buildInstall = null;
const buildMisses = new Map(); // commit + path → in-flight fetch

function buildForClient(clientId) {
  if (clientId && clientBuilds.has(clientId)) return clientBuilds.get(clientId);
  const build = lastPageBuild !== undefined ? lastPageBuild : activeBuild;
  if (clientId) pinClient(clientId, build);
  return build;
}

function pinClient(clientId, build) {
  if (!clientId) return;
  clientBuilds.delete(clientId);
  clientBuilds.set(clientId, build);
  while (clientBuilds.size > 256) clientBuilds.delete(clientBuilds.keys().next().value);
}

// A URL's path as the manifest spells it (git paths are unescaped UTF-8).
function filePath(url) {
  try { return decodeURIComponent(url.pathname); } catch { return url.pathname; }
}

function shellable(url) {
  if (url.origin !== self.location.origin || NOT_SHELL.test(url.pathname)) return false;
  if (/\.[a-z0-9]+$/i.test(url.pathname)) return false; // a file, not a piece
  return !SHELL_PARAMS.some((param) => url.searchParams.has(param));
}

// Synchronous: does the build layer answer this request?
function buildHandles(event, url) {
  const request = event.request;
  if (url.origin !== self.location.origin || request.headers.has('range')) return false;
  if (request.mode === 'navigate') {
    if (!shellable(url)) return false;
    event.respondWith(serveShell(event, url));
    event.waitUntil(new Promise((resolve) => setTimeout(resolve, BUILD_CHECK_DELAY_MS)).then(() => checkForBuild()));
    return true;
  }
  if (!BUILD_SCOPE.test(url.pathname)) return false;
  if (buildLoadedYet) {
    const build = buildForClient(event.clientId);
    if (!build || !build.files[filePath(url)]) return false; // the network-first path
  }
  event.respondWith(serveFromBuild(event, url));
  return true;
}

async function serveShell(event, url) {
  await buildLoaded;
  // A hard reload asks for the newest deploy; any reload waits briefly for a
  // build that is already downloading (the ↑ badge's reload lands on it).
  const hard = event.request.cache === 'reload' || event.request.cache === 'no-cache';
  const pending = hard ? checkForBuild(true) : buildInstall;
  if (pending) await Promise.race([pending, new Promise((r) => setTimeout(r, BUILD_WAIT_MS))]);
  const build = activeBuild;
  const pin = (b) => { pinClient(event.resultingClientId, b); lastPageBuild = b; };
  const cache = build && (await caches.open(BUILD_PREFIX + build.commit));
  if (cache) {
    const cached = await cache.match(SHELL_KEY + url.pathname);
    if (cached) { pin(build); return cached; }
  }
  try {
    const response = await fetch(event.request);
    const commit = response.headers.get('x-ac-commit');
    if (cache && commit === build.commit && response.status === 200 && !response.redirected) {
      event.waitUntil(cache.put(SHELL_KEY + url.pathname, response.clone()).catch(() => {}));
      pin(build);
    } else {
      pin(null); // a shell from another commit runs on the network
    }
    return response;
  } catch (err) {
    // Offline and this page was never visited: the prompt's shell boots it.
    const fallback = cache && (await cache.match(SHELL_KEY + '/'));
    if (fallback) { pin(build); return fallback; }
    throw err;
  }
}

async function serveFromBuild(event, url) {
  await buildLoaded;
  const build = buildForClient(event.clientId);
  const hash = build?.files[filePath(url)];
  if (!hash) return fetch(event.request);
  const cache = await caches.open(BUILD_PREFIX + build.commit);
  const key = self.location.origin + url.pathname;
  const cached = await cache.match(key);
  if (cached) return cached;
  // First time this build is asked for the file: fetch it once, keep it if
  // the bytes are this commit's.
  const id = build.commit + url.pathname;
  let pending = buildMisses.get(id);
  if (!pending) {
    pending = fetch(key, { cache: 'no-cache' }).then(async (response) => {
      if (response.status === 200 && !response.redirected) {
        const body = await response.clone().arrayBuffer();
        if (body.byteLength <= BUILD_MAX_BYTES && (await blobMatches(body, hash, build.hashChars))) {
          await cache.put(key, response.clone());
        } else {
          checkForBuild(); // the server is likely on a newer commit
        }
      }
      return response;
    });
    buildMisses.set(id, pending);
    const drop = () => buildMisses.delete(id);
    pending.then(drop, drop);
  }
  try {
    return (await pending).clone();
  } catch (err) {
    const older = await caches.match(key); // offline: any copy beats none
    if (older) return older;
    throw err;
  }
}

// Does `body` hash to the git blob `hash` (a hex prefix)?
async function blobMatches(body, hash, chars) {
  const header = new TextEncoder().encode(`blob ${body.byteLength}\0`);
  const bytes = new Uint8Array(header.length + body.byteLength);
  bytes.set(header);
  bytes.set(new Uint8Array(body), header.length);
  const digest = new Uint8Array(await crypto.subtle.digest('SHA-1', bytes));
  let hex = '';
  for (const b of digest) hex += b.toString(16).padStart(2, '0');
  return hex.slice(0, chars || hash.length) === hash;
}

async function checkForBuild(force = false) {
  await buildLoaded;
  if (buildInstall) return buildInstall;
  const now = Date.now();
  if (!force && now - lastBuildCheck < BUILD_CHECK_MS) return;
  lastBuildCheck = now;
  let commit = null;
  try {
    ({ commit } = await (await fetch('/api/build', { cache: 'no-store' })).json());
  } catch {
    return; // offline: the build we have is the build we run
  }
  if (!commit || activeBuild?.commit === commit) return;
  buildInstall = installBuild(commit)
    .catch((err) => console.warn('📦 SW: build', commit.slice(0, 7), 'not installed:', err.message))
    .finally(() => { buildInstall = null; });
  return buildInstall;
}

async function installBuild(commit) {
  const started = Date.now();
  const origin = self.location.origin;
  const response = await fetch(`/api/build?commit=${commit}`);
  if (!response.ok) throw new Error(`manifest ${response.status}`);
  const next = await response.json();
  if (next.commit !== commit || !next.files) throw new Error('bad manifest');

  const prev = activeBuild;
  const prevCache = prev && (await caches.open(BUILD_PREFIX + prev.commit));
  const cacheName = BUILD_PREFIX + commit;
  const cache = await caches.open(cacheName);

  // Carry what the last build held, or — the first time — what the module
  // cache and the root shell already know a launch needs.
  const paths = new Set(BUILD_CORE);
  const shells = new Set();
  for (const source of [prevCache || (await caches.open(CACHE_NAME))]) {
    for (const request of await source.keys()) {
      const path = new URL(request.url).pathname;
      if (path.startsWith(SHELL_KEY)) shells.add(path.slice(SHELL_KEY.length) || '/');
      else paths.add(path);
    }
  }
  shells.delete('/');
  const shellPaths = ['/', ...[...shells].slice(-SHELLS_KEPT)];

  // Every shell must be this very commit's; if the root isn't, the server has
  // already moved on and this install is stale.
  const shellResponses = await pool(shellPaths, 4, async (path) => {
    const r = await fetch(origin + path, { cache: 'no-store' });
    if (r.status !== 200 || r.redirected || r.headers.get('x-ac-commit') !== commit) return null;
    return r;
  });
  if (!shellResponses[0]) throw new Error('root shell is not this commit');
  const rootHtml = await shellResponses[0].clone().text();
  for (const [, href] of rootHtml.matchAll(/(?:href|src)="(\/[^"?#]+)/g)) paths.add(href);

  // One worker bundle: the one this commit's manifest names.
  const store = async (encoded) => {
    const path = filePath(new URL(encoded, origin));
    const hash = next.files[path];
    if (!hash) return false;
    if (prev && prev.files[path] === hash) {
      const kept = await prevCache.match(origin + path);
      if (kept) { await cache.put(origin + path, kept); return 'copied'; }
    }
    const r = await fetch(origin + path, { cache: 'no-cache' });
    if (r.status !== 200 || r.redirected) return false;
    const body = await r.clone().arrayBuffer();
    if (body.byteLength > BUILD_MAX_BYTES || !(await blobMatches(body, hash, next.hashChars))) return false;
    await cache.put(origin + path, r);
    return 'fetched';
  };
  const core = await pool(BUILD_CORE, 4, store);
  if (core.some((ok) => !ok)) {
    await caches.delete(cacheName);
    throw new Error('core files missing or not this commit');
  }
  try {
    const { filename } = await (await cache.match(origin + '/aesthetic.computer/lib/disk-worker-manifest.json')).json();
    for (const path of paths) if (WORKER_BUNDLE.test(path)) paths.delete(path);
    paths.add(`/aesthetic.computer/lib/${filename}`);
  } catch {}
  for (const path of BUILD_CORE) paths.delete(path);
  const results = await pool([...paths], 8, store);
  const copied = results.filter((r) => r === 'copied').length + core.filter((r) => r === 'copied').length;
  const fetched = results.filter((r) => r === 'fetched').length + core.filter((r) => r === 'fetched').length;
  await Promise.all(shellPaths.map((path, i) =>
    shellResponses[i] ? cache.put(origin + SHELL_KEY + path, shellResponses[i]) : null));

  // Go live for the next launch; pages already open keep their own build.
  await (await caches.open(BUILD_META)).put(BUILD_ACTIVE_KEY, new Response(JSON.stringify(next), {
    headers: { 'Content-Type': 'application/json' },
  }));
  activeBuild = next;
  const keep = new Set([cacheName, prev && BUILD_PREFIX + prev.commit]);
  for (const name of await caches.keys()) {
    if (name.startsWith(BUILD_PREFIX) && !keep.has(name)) await caches.delete(name);
  }
  const ms = Date.now() - started;
  console.log(`📦 SW: build ${commit.slice(0, 7)} ready (${copied} kept, ${fetched} fetched, ${ms} ms)`);
  for (const client of await self.clients.matchAll({ type: 'window' })) {
    client.postMessage({ type: 'ac:build-ready', commit, previous: prev?.commit || null, copied, fetched, ms });
  }
}

// Run `fn` over `items`, `limit` at a time; a throw counts as a falsy result.
async function pool(items, limit, fn) {
  const out = new Array(items.length);
  let next = 0;
  await Promise.all(Array.from({ length: Math.min(limit, items.length) }, async () => {
    while (next < items.length) {
      const i = next++;
      try { out[i] = await fn(items[i]); } catch { out[i] = null; }
    }
  }));
  return out;
}

// Handle messages from main thread
self.addEventListener('message', (event) => {
  if (event.data === 'skipWaiting') {
    self.skipWaiting();
  }
  if (event.data?.type === 'ac:build-check') {
    event.waitUntil(checkForBuild(event.data.force === true));
  }
  if (event.data === 'clearCache') {
    // The page saw a new deploy (disk.mjs's /api/version poll): fetch its
    // build now, so the ↑ badge's reload opens on it.
    event.waitUntil(checkForBuild(true));
    caches.delete(CACHE_NAME).then(() => {
      console.log('🔧 SW: Cache cleared');
    });
  }
});

// 🔔 Standard Web Push (no Firebase). The server encrypts a JSON payload per
// RFC 8291 (see shared/push.mjs): { title, body, icon, image, data: { piece } }.
self.addEventListener('push', (event) => {
  let note = {};
  try {
    note = event.data?.json() || {};
  } catch {
    note = { body: event.data?.text() };
  }
  event.waitUntil(
    self.registration.showNotification(note.title || 'aesthetic.computer', {
      body: note.body || '',
      icon: note.icon || 'https://aesthetic.computer/api/logo.png',
      image: note.image,
      data: note.data || {},
    })
  );
});

// Tapping a notification jumps to its piece (e.g. data.piece = "chat"),
// focusing an existing AC tab when one is open.
self.addEventListener('notificationclick', (event) => {
  event.notification.close();
  const piece = event.notification.data?.piece || '';
  const url = `${self.location.origin}/${piece}`;
  event.waitUntil(
    clients.matchAll({ type: 'window', includeUncontrolled: true }).then((tabs) => {
      const tab = tabs.find((t) => t.url.startsWith(self.location.origin));
      if (tab) {
        tab.focus();
        return tab.navigate ? tab.navigate(url) : undefined;
      }
      return clients.openWindow(url);
    })
  );
});
