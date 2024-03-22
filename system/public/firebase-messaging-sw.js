// Give the service worker access to Firebase Messaging.
// Note that you can only use Firebase Messaging here. Other Firebase libraries
// are not available in the service worker.
importScripts("https://www.gstatic.com/firebasejs/8.10.1/firebase-app.js");
importScripts(
  "https://www.gstatic.com/firebasejs/8.10.1/firebase-messaging.js",
);

// Initialize the Firebase app in the service worker by passing in
// your app's Firebase config object.
// https://firebase.google.com/docs/web/setup#config-object
firebase.initializeApp({
  apiKey: "AIzaSyBZJ4b5KaHUW0q__FDUwHPrDd0NX2umJ3A",
  authDomain: "aesthetic-computer.firebaseapp.com",
  projectId: "aesthetic-computer",
  storageBucket: "aesthetic-computer.appspot.com",
  messagingSenderId: "839964586768",
  appId: "1:839964586768:web:466139ee473df1954ceb95",
});

// Retrieve an instance of Firebase Messaging so that it can handle background
// messages.
const messaging = firebase.messaging();

messaging.onBackgroundMessage((payload) => {
  console.log("🗨️ Received background message:", payload);
  // Customize notification here
  const notificationTitle = payload.notification.title;
  const notificationOptions = {
    body: payload.notification.body,
    icon: "https://aesthetic.computer/api/logo.png",
  };

  self.registration.showNotification(notificationTitle, notificationOptions);
});
