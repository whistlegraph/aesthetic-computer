// Aesthetic Computer Service Worker
// Caches JavaScript modules for faster subsequent loads

const CACHE_NAME = 'ac-modules-v4'; // Bump version to force SW update
const CACHE_DURATION = 60 * 60 * 1000; // 1 hour in ms (dev-friendly)

// Critical modules to precache on install
const PRECACHE_MODULES = [
  '/aesthetic.computer/boot.mjs',
  '/aesthetic.computer/bios.mjs',
  '/aesthetic.computer/lib/parse.mjs',
  '/aesthetic.computer/lib/disk.mjs',
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
  
  // Skip never-cache patterns
  if (NEVER_CACHE.some((pattern) => pattern.test(url.pathname) || pattern.test(url.href))) {
    return;
  }
  
  // Check if this is a cacheable request
  const isCacheable = CACHEABLE_PATTERNS.some((pattern) => pattern.test(url.pathname));
  
  // For core modules, create a clean cache key without query params
  // This ensures disk.mjs?session-aesthetic=... matches cached /aesthetic.computer/lib/disk.mjs
  const isCoreModule = PRECACHE_MODULES.some((m) => url.pathname === m);
  const cacheKey = isCoreModule ? new Request(url.origin + url.pathname) : event.request;
  
  if (isCacheable) {
    // Stale-while-revalidate strategy with corruption detection
    event.respondWith(
      caches.open(CACHE_NAME).then((cache) => {
        // Use clean cache key for core modules to ignore query params
        return cache.match(cacheKey).then((cachedResponse) => {
          // Use `cache: 'no-cache'` so the browser sends a conditional request
          // (If-None-Match / If-Modified-Since) instead of serving from the HTTP
          // cache. Origin returns 304 when unchanged (cheap) or 200 with fresh
          // content. Without this, stale-while-revalidate can loop on a stale
          // response for up to max-age even after the origin has updated.
          const revalidateRequest = new Request(event.request, { cache: 'no-cache' });
          const fetchPromise = fetch(revalidateRequest)
            .then(async (networkResponse) => {
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
                  }
                } catch (e) {
                  // Response was incomplete/corrupted - use cache if available (silent)
                  if (cachedResponse) return cachedResponse;
                }
              }
              return networkResponse;
            })
            .catch(async (err) => {
              // Return cached version if network fails (silent)
              if (cachedResponse) return cachedResponse;
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
          
          // Return cached response immediately, or wait for network
          return cachedResponse || fetchPromise;
        });
      })
    );
  }
});

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
