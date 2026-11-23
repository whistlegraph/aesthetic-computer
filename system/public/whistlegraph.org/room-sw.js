self.addEventListener("push", event => {
  const note = event.data?.json() || {};
  event.waitUntil(self.registration.showNotification(note.title || "Whistlegraph", {
    body: note.body || "New chat message",
    icon: "/favicon.png",
    tag: note.data?.messageId || "whistlegraph",
  }));
});

self.addEventListener("notificationclick", event => {
  event.notification.close();
  event.waitUntil((async () => {
    const tabs = await self.clients.matchAll({ type: "window", includeUncontrolled: true });
    const room = tabs.find(tab => new URL(tab.url).pathname === "/" && !new URL(tab.url).searchParams.has("state"));
    if (room) return room.focus();
    return self.clients.openWindow("/");
  })());
});
