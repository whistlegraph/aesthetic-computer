// `aesthetic.computer` Bootstrap, 23.02.16.19.23

// Alert the parent /  we are ready.
if (window.parent) window.parent.postMessage({ type: "ready" }, "*");

// Included as a <script> tag to boot the system on a webpage. (Loads `bios`)
import { boot } from "./bios.mjs";
import { parse, slug } from "./lib/parse.mjs";

// 0. Environment Configuration

let debug;

window.preloaded = false; // This gets set to true either automatically or
// manually by a disk. It's used by the thumbnail
// system to know when to take screenshots of each
// piece.

// Check for the debug constant in index.html which overrides all defaults.
// (And assumes we are not in a production environment)
if (window.acDEBUG === true || window.acDEBUG === false) {
  debug = window.acDEBUG;
} else if (
  window.location.hostname === "aesthetic.computer" ||
  window.location.hostname.endsWith(".ac") ||
  window.location.hostname === "m2w2.whistlegraph.com"
) {
  debug = false; // Turn debugging off by default in production.
  window.production = true;
} else {
  debug = true; // Turn debuging on by default everywhere else.
  // TODO: This should eventually be upgraded for IPFS exports.
}

// Check to see if we have a "#debug" hash.
if (window.location.hash === "#debug") debug = true;
if (window.location.hash === "#nodebug") debug = false;

window.acDEBUG = debug; // Set window.acDEBUG again just in case any code relies
// on it down the line. Should it need to? 22.07.15.00.21

// Get the current LAN host if it exists...
if (window.acDEBUG) window.acLAN_HOST = document.body.dataset.lanHost;

// 🥾 Boot the aesthetic.computer system.

// Or it can be set by a custom host...
if (location.hostname === "m2w2.whistlegraph.com")
  window.acSTARTING_PIECE = "wg~m2w2";
if (location.hostname === "botce.ac") window.acSTARTING_PIECE = "botce";

if (window.acSTARTING_PIECE === undefined) window.acSTARTING_PIECE = "prompt";
const parsed = parse(slug(location.href) || window.acSTARTING_PIECE);
const bpm = 120; // Set the starting bpm. Is this still necessary?
// Wait for fonts to load before booting.
// if ("fonts" in document) {
//  await document.fonts.load("1em YWFTProcessing-Light");
//  await document.fonts.load("1em YWFTProcessing-Regular");
//  await document.fonts.load("1em Berkeley Mono Variable");
// }

const nogap = location.search.startsWith("?nogap");
boot(parsed, bpm, { gap: nogap ? 0 : undefined }, debug);

let sandboxed = window.origin === "null";

const previewOrIcon =
  location.search.startsWith("?icon=") ||
  location.search.startsWith("?preview=");

// #region 🔐 Auth0: Universal Login & Authentication
function loadAuth0Script() {
  return new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
    script.crossOrigin = "anonymous";
    document.head.appendChild(script);

    script.onload = () => {
      // console.log("🟢 Auth0 loaded.");
      resolve();
    };

    script.onerror = () => {
      console.error("🔴 Error loading Auth0.");
      reject(new Error("Script loading failed"));
    };
  });
}

// Call this function at the desired point in your application
loadAuth0Script()
  .then(async () => {
    if (!sandboxed && window.auth0 && !previewOrIcon) {
      const clientId = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
      const before = performance.now();

      const auth0Client = await window.auth0?.createAuth0Client({
        domain: "https://hi.aesthetic.computer",
        clientId,
        cacheLocation: "localstorage",
        useRefreshTokens: true,
        authorizationParams: { redirect_uri: window.location.origin },
      });

      window.auth0Client = auth0Client;

      if (
        location.search.includes("state=") &&
        (location.search.includes("code=") ||
          location.search.includes("error="))
      ) {
        try {
          console.log("🔐 Handling auth0 redirect...");
          await auth0Client.handleRedirectCallback();
        } catch (e) {
          console.error("🔐", e);
        }
        window.history.replaceState({}, document.title, "/");
      }

      const url = new URL(window.location);
      const params = url.searchParams;
      const sessionParams = params.get("session");
      let encodedSession = sessionParams; // || localStorage.getItem("acSessionParams");
      if (encodedSession === "null") encodedSession = undefined;
      //localStorage.setItem("acSessionParams", sessionParams);
      let pickedUpSession;
      if (encodedSession) {
        const sessionJsonString = atob(decodeURIComponent(encodedSession));
        const session = JSON.parse(sessionJsonString);
        // Use the session information to authenticate, if it exists.
        // console.log("🥀 Session data:", session);
        if (session.accessToken && session.account) {
          window.acTOKEN = session.accessToken; // Only set using this flow.
          window.acUSER = {
            name: session.account.label,
            sub: session.account.id,
          };
          // Will get passed to the first message by the piece runner.
          // console.log("🌻 Picked up session!", window.acTOKEN, window.acUSER);
          window.acDISK_SEND({
            type: "session:update",
            content: { user: window.acUSER },
          });
          pickedUpSession = true;
        }

        if (sessionParams) {
          params.delete("session"); // Remove the 'session' parameter
          // Update the URL without reloading the page
          history.pushState({}, "", url.pathname + "?" + params.toString());
        }
      }

      let isAuthenticated = await auth0Client.isAuthenticated();

      // const iframe = window.self !== window.top;

      window.acLOGIN = async (mode) => {
        const opts = { prompt: "login" }; // Never skip the login screen.
        if (mode === "signup") opts.screen_hint = mode;

        //if (!iframe) {
        auth0Client.loginWithRedirect({ authorizationParams: opts });
        //} else {
        // window.parent.postMessage(
        //   {
        //     type: "externallyAuthenticate",
        //     authUrl: "https://hi.aesthetic.computer",
        //   },
        //   "*",
        // );
        // console.log("🔐 Logging in with popup...");
        // auth0Client
        // .loginWithPopup()
        // .then(() => {
        // console.log("🔐 Logged in with popup");
        // })
        // .catch((error) => {
        // console.error("🔐 Popup login error:", error);
        // });
        //}
      };

      if (location.pathname === "/hi") window.acLOGIN();

      // Redirect to signup with a query parameter.
      if (location.search.startsWith("?signup")) window.acLOGIN("signup");

      window.acLOGOUT = () => {
        if (isAuthenticated) {
          console.log("🔐 Logging out...");
          // TODO: How to send a message here that could refresh the page for
          //       all connected users.
          window.acSEND({
            type: "logout:broadcast:subscribe",
            content: { user: window.acUSER },
          });

          auth0Client.logout({
            logoutParams: { returnTo: window.location.origin },
          });
        } else {
          console.log("🔐 Already logged out!");
        }
      };

      if (isAuthenticated && !pickedUpSession) {
        try {
          await window.auth0Client.getTokenSilently();
          // console.log("🔐 Authorized");
        } catch (error) {
          console.log("🔐️ ❌ Unauthorized", error);
          console.error(
            "Failed to retrieve token silently. Logging out.",
            error,
          );
          // Redirect the user to logout if the token has failed.
          auth0Client.logout();
          isAuthenticated = false;
        }
      }

      if (isAuthenticated && !pickedUpSession) {
        // TODO: How long does this await actually take? 23.07.11.18.55
        let userProfile = await auth0Client.getUser();
        if (userProfile.email_verified === false) {
          try {
            await window.auth0Client.getTokenSilently({ cacheMode: "off" });
            userProfile = await auth0Client.getUser();
          } catch (err) {
            console.warn("🧔 Could not retrieve user from network.");
          }
        }
        // Check if 'email_verified' is false and load from the network again
        // if it is.
        window.acUSER = userProfile; // Will get passed to the first message by the piece runner.
        window.acDISK_SEND({
          type: "session:update",
          content: { user: window.acUSER },
        });
      } else if (!pickedUpSession) {
        // console.log("🗝️ Not authenticated.");
      }

      // const after = performance.now();
      // console.log("🗝️ Auth took:", (after - before) / 1000, "seconds.");
    }
  })
  .catch((error) => {
    console.error("Failed to load Auth0 script:", error);
  });
// #endregion

// ***Incoming Message Responder***

// - At the moment it is just for a work-in-progress figma widget but any
//   window messages to be received here.
// TODO: Finish FigJam Widget with iframe message based input & output.
//         See also: https://www.figma.com/plugin-docs/working-with-images/
function receive(event) {
  // console.log("🌟 Event:", event);
  if (event.data?.type === "figma-image-input") {
    // TODO: Build image with width and height.
    console.log("Bytes:", event.data.bytes.length);
    return;
  }
  if (event.data?.type === "clipboard:copy:confirmation") {
    // Receive a clipboard copy confirmation from a hosted frame.
    // (vscode extension)
    window.acSEND({ type: "copy:copied" });
    return;
  }
  if (event.data?.type === "setSession") {
    // Use the session information to authenticate, if it exists.
    const session = event.data.session;
    // console.log("🥀 Session data:", session);
    if (session.accessToken && session.account) {
      window.acTOKEN = session.accessToken; // Only set using this flow.
      window.acUSER = { name: session.account.label, sub: session.account.id }; // Will get passed to the first message by the piece runner.
      console.log("🌻 Picked up session!", window.acTOKEN, window.acUSER);
      window.acSEND({
        type: "session:update",
        content: { user: window.acUSER },
      });
    }
    return;
  } else if (event.data?.type === "clearSession" && window.acTOKEN) {
    window.location.reload();
    return;
  } else if (event.data?.startsWith?.("docs:")) {
    console.log("Docs link got!");
    window.acSEND({
      type: "docs:link",
      content: event.data.split(":").pop(),
    });
    return;
  }
}
window.addEventListener("message", receive);

// 🔔 Subscribe to web / client notifications.
// TODO: Test this to make sure it's skipped in the native apps,
//       and factor it out. 24.02.26.19.29

import { initializeApp } from "https://www.gstatic.com/firebasejs/10.8.0/firebase-app.js";
import {
  getMessaging,
  onMessage,
  getToken,
} from "https://www.gstatic.com/firebasejs/10.8.0/firebase-messaging.js";

function initNotifications() {
  // Your web app's Firebase configuration
  // For Firebase JS SDK v7.20.0 and later, measurementId is optional
  const firebaseConfig = {
    apiKey: "AIzaSyBZJ4b5KaHUW0q__FDUwHPrDd0NX2umJ3A",
    authDomain: "aesthetic-computer.firebaseapp.com",
    projectId: "aesthetic-computer",
    storageBucket: "aesthetic-computer.appspot.com",
    messagingSenderId: "839964586768",
    appId: "1:839964586768:web:466139ee473df1954ceb95",
  };

  const app = initializeApp(firebaseConfig);
  const messaging = getMessaging(app);

  getToken(messaging, {
    vapidKey:
      "BDEh3JDD1xZo7eQU00TgsYb_o8ENJlpU-ovbZzWoCOu4AOeFJD8PVbZ3pif_7rMEk65Uj00-lwdXgc3qJVLp4ys",
  })
    .then((token) => {
      if (token) {
        // Send the token to your server and update the UI if necessary
        function subscribe(token, topic, then) {
          fetch("/api/subscribe-to-topic", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ token, topic }),
          })
            .then((response) => {
              if (!response.ok) throw new Error("Bad response.");
              return response.json(); // Parse the JSON in the response
            })
            .then((data) => {
              console.log("📶 Subscribed to:", data.topic);
              then?.(); // Subscribe to another topic if necessary.
            })
            .catch((error) => {
              console.error("🚫 Topic subscription error:", error);
            });
        }

        subscribe(token, "scream", () => subscribe(token, "mood"));

        onMessage(messaging, (payload) => {
          console.log(
            "🗨️ Client notification received. ",
            payload.notification,
          );
        });
      } else {
        console.warn("🔔 No registration token available.");
      }
    })
    .catch((err) => {
      // console.warn("🔔 An error occurred while retrieving token.", err);
    });
}

if (!previewOrIcon) initNotifications();
