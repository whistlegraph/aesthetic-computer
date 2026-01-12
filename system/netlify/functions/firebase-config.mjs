// Runtime Firebase client config (served from env vars)
//
// Goal: avoid committing client keys/config into the monorepo while still
// allowing the browser + firebase-messaging service worker to initialize.
//
// Usage:
// - JSON (page JS):   /api/firebase-config
// - JS  (SW import):  /api/firebase-config?format=sw

export default async (request) => {
  const url = new URL(request.url);
  const format = url.searchParams.get("format") || "json";

  const firebaseConfig = {
    apiKey: process.env.AC_FIREBASE_API_KEY || "",
    authDomain:
      process.env.AC_FIREBASE_AUTH_DOMAIN || "aesthetic-computer.firebaseapp.com",
    projectId: process.env.AC_FIREBASE_PROJECT_ID || "aesthetic-computer",
    storageBucket:
      process.env.AC_FIREBASE_STORAGE_BUCKET || "aesthetic-computer.appspot.com",
    messagingSenderId:
      process.env.AC_FIREBASE_MESSAGING_SENDER_ID || "839964586768",
    appId:
      process.env.AC_FIREBASE_APP_ID || "1:839964586768:web:466139ee473df1954ceb95",
  };

  const hasApiKey = Boolean(firebaseConfig.apiKey);

  if (format === "sw" || format === "js") {
    const body = hasApiKey
      ? `self.AC_FIREBASE_CONFIG = ${JSON.stringify(firebaseConfig)};\n`
      : `self.AC_FIREBASE_CONFIG = null;\n`;

    return new Response(body, {
      headers: {
        "Content-Type": "application/javascript; charset=utf-8",
        "Cache-Control": "no-store",
      },
    });
  }

  return new Response(JSON.stringify(hasApiKey ? firebaseConfig : null), {
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      "Cache-Control": "no-store",
    },
  });
};

export const config = { path: "/api/firebase-config" };
