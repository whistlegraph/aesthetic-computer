// Give the service worker access to Firebase Messaging.
// Note that you can only use Firebase Messaging here. Other Firebase libraries
// are not available in the service worker.
importScripts("https://www.gstatic.com/firebasejs/8.10.1/firebase-app.js");
importScripts(
  "https://www.gstatic.com/firebasejs/8.10.1/firebase-messaging.js",
);

// Load runtime Firebase config (served from env vars via Netlify function).
// If config is missing, notifications stay disabled.
try {
  importScripts("/api/firebase-config?format=sw");
} catch (e) {
  // Silent: don't break SW install if endpoint is unavailable.
}

// Initialize the Firebase app in the service worker by passing in
// your app's Firebase config object.
// https://firebase.google.com/docs/web/setup#config-object
if (self.AC_FIREBASE_CONFIG?.apiKey) {
  firebase.initializeApp(self.AC_FIREBASE_CONFIG);
}

// Retrieve an instance of Firebase Messaging so that it can handle background
// messages.
const messaging = self.AC_FIREBASE_CONFIG?.apiKey ? firebase.messaging() : null;

// messaging.onBackgroundMessage((payload) => {
//   console.log("🗨️ Received background message:", payload);
//   // Customize notification here
//   const notificationTitle = payload.notification.title;
//   const notificationOptions = {
//     body: payload.notification.body,
//     icon: "https://aesthetic.computer/api/logo.png",
//   };
//
//   self.registration.showNotification(notificationTitle, notificationOptions);
// });
