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
            .filter((name) => name !== CACHE_NAME)
            .map((name) => {
              console.log('🔧 SW: Deleting old cache:', name);
              return caches.delete(name);
            })
        );
      })
      .then(() => self.clients.claim())
  );
});

self.addEventListener('fetch', (event) => {
  const url = new URL(event.request.url);
  
  // Skip non-GET requests
  if (event.request.method !== 'GET') return;
  
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

// Handle messages from main thread
self.addEventListener('message', (event) => {
  if (event.data === 'skipWaiting') {
    self.skipWaiting();
  }
  if (event.data === 'clearCache') {
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
