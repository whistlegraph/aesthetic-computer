// firebase-messaging-sw, retired 26.06.11
// AC moved off Firebase Cloud Messaging to standard Web Push — payload and
// tap handling now live in /sw.js, and delivery comes straight from each
// browser's push service via VAPID (see shared/push.mjs server-side).
// This file stays only so devices that installed the old FCM service worker
// pick up one final update that unregisters it.
self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", (event) => {
  event.waitUntil(self.registration.unregister());
});
