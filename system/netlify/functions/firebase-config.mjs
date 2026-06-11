// firebase-config, retired 26.06.11
// AC moved off Firebase Cloud Messaging to standard Web Push (VAPID) and
// direct APNs — see shared/push.mjs. This endpoint stays only so cached
// pre-migration clients get a clean "disabled" answer instead of a 500:
// old boot.mjs treats a non-ok response as "notifications unavailable" and
// the old firebase-messaging service worker treats a null config as a no-op.

export default async (request) => {
  const url = new URL(request.url);
  const format = url.searchParams.get("format") || "json";

  if (format === "sw" || format === "js") {
    return new Response("self.AC_FIREBASE_CONFIG = null;\n", {
      headers: {
        "Content-Type": "application/javascript; charset=utf-8",
        "Cache-Control": "no-store",
      },
    });
  }

  return new Response(null, { status: 410 });
};

export const config = { path: "/api/firebase-config" };
