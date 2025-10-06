// `aesthetic.computer` Bootstrap, 23.02.16.19.23
console.clear();

// Alert the parent /  we are ready.
if (window.parent) window.parent.postMessage({ type: "ready" }, "*");

// List of legitimate query parameters that should be preserved
const LEGITIMATE_PARAMS = [
  'icon', 'preview', 'signup', 'supportSignUp', 'success', 'code', 
  'supportForgotPassword', 'message', 'vscode', 'nogap', 'nolabel', 
  'density', 'zoom', 'duration', 'session-aesthetic', 'session-sotce', 'notice', 'tv', 'highlight'
];

// Auth0 parameters that need to be temporarily processed but then removed
const AUTH0_PARAMS = ['state', 'error', 'error_description'];

// Extract only legitimate query parameters from URL, preserving kidlisp ? characters in path
function extractLegitimateParams(fullUrl) {
  const params = new URLSearchParams();
  
  // Look for legitimate parameters at the end of the URL
  for (const paramName of LEGITIMATE_PARAMS) {
    const regex = new RegExp(`[?&]${paramName}=([^&]*)`);
    const match = fullUrl.match(regex);
    if (match) {
      params.set(paramName, decodeURIComponent(match[1]));
    }
    
    // Also check for boolean parameters (without =value)
    const boolRegex = new RegExp(`[?&]${paramName}(?=[&]|$)`);
    if (boolRegex.test(fullUrl)) {
      params.set(paramName, 'true');
    }
  }
  
  return params;
}

// Remove Auth0 parameters from URL
function cleanAuth0Params(url) {
  const urlObj = new URL(url);
  
  // Always remove these Auth0 parameters
  AUTH0_PARAMS.forEach(param => {
    urlObj.searchParams.delete(param);
  });
  
  // Only remove 'code' if 'state' is also present (indicating Auth0 callback)
  if (urlObj.searchParams.has('state')) {
    urlObj.searchParams.delete('code');
  }
  
  return urlObj.pathname + (urlObj.searchParams.toString() ? '?' + urlObj.searchParams.toString() : '');
}

// Create a clean URL without legitimate query parameters
function createCleanUrl(fullUrl) {
  let cleanUrl = fullUrl;
  
  // Remove legitimate parameters but preserve ? in kidlisp content
  for (const paramName of LEGITIMATE_PARAMS) {
    const regexWithValue = new RegExp(`[?&]${paramName}=([^&]*)`);
    const regexBool = new RegExp(`[?&]${paramName}(?=[&]|$)`);
    cleanUrl = cleanUrl.replace(regexWithValue, '');
    cleanUrl = cleanUrl.replace(regexBool, '');
  }
  
  // Clean up any trailing ? or & that might be left
  cleanUrl = cleanUrl.replace(/[?&]+$/, '');
  
  return cleanUrl;
}

const legitParams = extractLegitimateParams(window.location.href);
const previewOrIcon =
  legitParams.has("icon") ||
  legitParams.has("preview");

window.acPREVIEW_OR_ICON = previewOrIcon;

const url = new URL(location);

function cleanUrlParams(url, params) {
  const queryString = params.toString();
  history.pushState(
    {},
    "",
    url.pathname + (queryString ? "?" + queryString : ""),
  );
}

// Test localStorage access early to detect sandbox restrictions
let localStorageBlocked = false;
try {
  // Test both read and write access
  const testKey = 'ac-sandbox-test';
  localStorage.setItem(testKey, 'test');
  localStorage.getItem(testKey);
  localStorage.removeItem(testKey);
} catch (err) {
  localStorageBlocked = true;
  console.warn('🏜️ localStorage access blocked (sandboxed iframe):', err.message);
}

// Test sessionStorage access
let sessionStorageBlocked = false;
try {
  const testKey = 'ac-session-test';
  sessionStorage.setItem(testKey, 'test');
  sessionStorage.getItem(testKey);
  sessionStorage.removeItem(testKey);
} catch (err) {
  sessionStorageBlocked = true;
  console.warn('🏜️ sessionStorage access blocked (sandboxed iframe):', err.message);
}

// Make storage state globally available
window.acLOCALSTORAGE_BLOCKED = localStorageBlocked;
window.acSESSIONSTORAGE_BLOCKED = sessionStorageBlocked;

// Safe localStorage functions
function safeLocalStorageGet(key) {
  if (localStorageBlocked) return null;
  try {
    return localStorage.getItem(key);
  } catch (err) {
    console.warn('localStorage access failed:', err.message);
    return null;
  }
}

function safeLocalStorageSet(key, value) {
  if (localStorageBlocked) return false;
  try {
    localStorage.setItem(key, value);
    return true;
  } catch (err) {
    console.warn('localStorage write failed:', err.message);
    return false;
  }
}

function safeLocalStorageRemove(key) {
  if (localStorageBlocked) return false;
  try {
    localStorage.removeItem(key);
    return true;
  } catch (err) {
    console.warn('localStorage remove failed:', err.message);
    return false;
  }
}

// Safe sessionStorage functions
function safeSessionStorageGet(key) {
  if (sessionStorageBlocked) return null;
  try {
    return sessionStorage.getItem(key);
  } catch (err) {
    console.warn('sessionStorage access failed:', err.message);
    return null;
  }
}

function safeSessionStorageSet(key, value) {
  if (sessionStorageBlocked) return false;
  try {
    sessionStorage.setItem(key, value);
    return true;
  } catch (err) {
    console.warn('sessionStorage write failed:', err.message);
    return false;
  }
}

function safeSessionStorageRemove(key) {
  if (sessionStorageBlocked) return false;
  try {
    sessionStorage.removeItem(key);
    return true;
  } catch (err) {
    console.warn('sessionStorage remove failed:', err.message);
    return false;
  }
}

// Make safe functions globally available
window.safeLocalStorageGet = safeLocalStorageGet;
window.safeLocalStorageSet = safeLocalStorageSet;
window.safeLocalStorageRemove = safeLocalStorageRemove;
window.safeSessionStorageGet = safeSessionStorageGet;
window.safeSessionStorageSet = safeSessionStorageSet;
window.safeSessionStorageRemove = safeSessionStorageRemove;

// 📧 Check to see if the user clicked an 'email' verified link.
{
  const params = extractLegitimateParams(window.location.href);
  if (
    params.get("supportSignUp") === "true" &&
    params.get("success") === "true" &&
    params.get("code") === "success"
  ) {
    params.delete("supportSignUp");
    params.delete("supportForgotPassword");
    params.delete("message");
    params.delete("success");
    params.delete("code");
    params.set("notice", "email-verified");
    cleanUrlParams(url, params);
  }
}

{
  const params = extractLegitimateParams(window.location.href);
  const vscode =
    params.get("vscode") === "true" ||
    safeLocalStorageGet("vscode") === "true";

  if (vscode) {
    params.delete("vscode");
    cleanUrlParams(url, params);
    window.acVSCODE = true;
    safeLocalStorageSet("vscode", "true");
  }
}

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
  window.location.hostname === "m2w2.whistlegraph.com" ||
  window.acTEIA_MODE // Disable debug mode in TEIA packages
) {
  debug = false; // Turn debugging off by default in production and TEIA mode.
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
if (
  location.hostname === "wipppps.world" ||
  location.hostname === "www.wipppps.world"
) {
  window.acSTARTING_PIECE = "wipppps";
}

if (window.acSTARTING_PIECE === undefined) window.acSTARTING_PIECE = "prompt";

// In TEIA mode, always use acSTARTING_PIECE instead of URL slug
let originalUrl = location.href;
let sluggedUrl = null;

if (!window.acTEIA_MODE) {
  sluggedUrl = slug(originalUrl) || window.acSTARTING_PIECE;
} else {
  sluggedUrl = window.acSTARTING_PIECE;
}

// if (window.acDEBUG || window.acVSCODE) {
//   console.log("🧭 URL pipeline", {
//     originalUrl,
//     sluggedUrl,
//   });
// }

const pieceToLoad = sluggedUrl;
const parsed = parse(pieceToLoad);

// if (window.acDEBUG || window.acVSCODE) {
//   console.log("🧭 Parsed result", {
//     text: parsed?.text,
//     search: parsed?.search,
//     hash: parsed?.hash,
//     params: parsed?.params,
//   });
// }

// Preserve the original search parameters that were stripped by slug()
if (location.search) {
  parsed.search = location.search;
}

const bpm = 120; // Set the starting bpm. Is this still necessary?
// Wait for fonts to load before booting.

// Parse the query string to detect both ?nogap and ?nolabel parameters
const params = extractLegitimateParams(window.location.href);
const nogap = params.has("nogap") || location.search.includes("nogap") || location.host.includes("wipppps.world");

// Check for nolabel parameter (no localStorage persistence)
const nolabel = params.has("nolabel") || location.search.includes("nolabel");

// Check for density parameter with localStorage persistence
const densityParam = params.get("density");
let density;

if (densityParam) {
  // URL parameter provided - use it and save to localStorage
  density = parseFloat(densityParam);
  safeLocalStorageSet("ac-density", density.toString());
} else {
  // No URL parameter - check localStorage for saved density
  const savedDensity = safeLocalStorageGet("ac-density");
  density = savedDensity ? parseFloat(savedDensity) : undefined;
}

// Check for zoom parameter (device/navigator zoom level) with localStorage persistence
const zoomParam = params.get("zoom");
let zoom;

if (zoomParam) {
  // URL parameter provided - use it and save to localStorage
  zoom = parseFloat(zoomParam);
  safeLocalStorageSet("ac-zoom", zoom.toString());
} else {
  // No URL parameter - check localStorage for saved zoom
  const savedZoom = safeLocalStorageGet("ac-zoom");
  zoom = savedZoom ? parseFloat(savedZoom) : undefined;
}

// Check for duration parameter (for timed pieces with progress bars) with localStorage persistence
const durationParam = params.get("duration");
let duration;

if (durationParam) {
  // URL parameter provided - use it and save to localStorage
  duration = parseFloat(durationParam);
  // Duration is in seconds, validate it's a positive number
  if (isNaN(duration) || duration <= 0) {
    duration = undefined;
    safeLocalStorageRemove("ac-duration"); // Remove invalid duration
  } else {
    safeLocalStorageSet("ac-duration", duration.toString());
    // Optional: Remove duration from URL after consumption (uncomment next 2 lines)
    // params.delete("duration");
    // cleanUrlParams(url, params);
  }
} else {
  // No URL parameter - check localStorage for saved duration
  const savedDuration = safeLocalStorageGet("ac-duration");
  if (savedDuration) {
    duration = parseFloat(savedDuration);
    // Validate saved duration
    if (isNaN(duration) || duration <= 0) {
      duration = undefined;
      safeLocalStorageRemove("ac-duration");
    }
  }
}

// Check for tv parameter (disables touch/keyboard input for TV display mode)
const tvParam = params.has("tv") || location.search.includes("tv");
const tv = tvParam === true || tvParam === "true";

// Check for highlight parameter (enables HUD background highlighting with optional color)
const highlightParam = params.get("highlight");
const highlight = highlightParam ? (highlightParam === "true" ? true : highlightParam) : false;

// Note: zoom parameter is available but not automatically applied to avoid text rendering issues
// It's passed to the boot function for selective use

// Apply nogap class immediately to prevent flash
if (nogap) {
  document.body.classList.add("nogap");
}

// Force dark background when in VSCode (like when not embedded)
if (window.acVSCODE) {
  document.body.classList.add("vscode");
}

// Pass the parameters directly without stripping them
boot(parsed, bpm, { gap: nogap ? 0 : undefined, nolabel, density, zoom, duration, tv, highlight }, debug);

let sandboxed = (window.origin === "null" && !window.acVSCODE) || localStorageBlocked || sessionStorageBlocked || window.acTEIA_MODE;

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
if (!sandboxed) {
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
          // console.log("🔐 Handling auth0 redirect...");
          await auth0Client.handleRedirectCallback();
        } catch (e) {
          console.error("🔐", e);
        }
        
        // Ensure Auth0 parameters are completely cleaned from URL
        const cleanUrl = cleanAuth0Params(window.location.href);
        window.history.replaceState({}, document.title, cleanUrl);
      }

      const params = extractLegitimateParams(window.location.href);
      let param = params.get("session-aesthetic");

      {
        // Handle any sotce tenancy from the vscode extension by storing
        // the param in localStorage.
        let sotceParam = params.get("session-sotce");
        if (sotceParam && sotceParam !== "null" && sotceParam !== "retrieve") {
          safeLocalStorageSet("session-sotce", sotceParam);
        }
      }

      if (param === "null") {
        // Returns "null" as a string if logged out.
        safeLocalStorageRemove("session-aesthetic");
      } else if (param === "retrieve" || param === null) {
        // null as a type if a check is needed
        param = safeLocalStorageGet("session-aesthetic");
      }

      const sessionParams = param;
      let encodedSession = sessionParams;
      if (encodedSession === "null") encodedSession = undefined;
      let pickedUpSession;

      let isAuthenticated;
      if (!encodedSession)
        isAuthenticated = await auth0Client.isAuthenticated();

      if (encodedSession) {
        const sessionJsonString = atob(decodeURIComponent(encodedSession));
        const session = JSON.parse(sessionJsonString);
        // Use the session information to authenticate, if it exists.
        // console.log("🥀 Session data:", session);
        if (session.accessToken && session.account) {
          // 🟩 Confirm the session access token and data is live here...
          try {
            const response = await fetch("/api/authorized", {
              method: "GET",
              headers: {
                Authorization: `Bearer ${session.accessToken}`,
              },
            });

            if (response.ok) {
              const result = await response.json();
              if (result.authorized) {
                // console.log("✅ Session is active and token is valid.");
                pickedUpSession = true;
              } else {
                // console.log("⚠️ Token is invalid or expired.");
                pickedUpSession = false;
              }
            } else {
              // console.log(
              //   "⚠️ Token validation request failed with status:",
              //   response.status,
              // );
              pickedUpSession = false;
            }
          } catch (err) {
            // console.error("❌ Error validating token:", err);
            pickedUpSession = false;
          }

          if (!pickedUpSession) {
            if (window.parent)
              window.parent.postMessage({ type: "logout" }, "*");
            return;
          }

          window.acTOKEN = session.accessToken; // Only set using this flow.
          window.acUSER = {
            email: session.account.label,
            sub: session.account.id,
          };
          // Will get passed to the first message by the piece runner.
          // console.log("🌻 Picked up session!", window.acTOKEN, window.acUSER);
          window.acDISK_SEND({
            type: "session:started",
            content: { user: window.acUSER },
          });
        }

        if (sessionParams) {
          safeLocalStorageSet("session-aesthetic", encodedSession);
          params.delete("session-aesthetic"); // Remove the 'session' parameters
          params.delete("session-sotce");
          
          // Also ensure any remaining Auth0 parameters are cleaned
          AUTH0_PARAMS.forEach(param => params.delete(param));
          // Only remove 'code' if this appears to be an Auth0 callback
          if (params.has('state')) {
            params.delete('code');
          }
          
          const cleanUrl = url.pathname + (params.toString() ? "?" + params.toString() : "");
          history.pushState({}, "", cleanUrl);
        }
      }

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
      const legitParams = extractLegitimateParams(window.location.href);
      if (legitParams.has("signup")) window.acLOGIN("signup");

      window.acLOGOUT = () => {
        if (isAuthenticated) {
          console.log("🔐 Logging out...");
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
          // const options = localStorage.getItem("aesthetic:refresh-user")
          // ? { cacheMode: "off" }
          // : undefined;
          await auth0Client.getTokenSilently();
          // if (options) localStorage.removeItem("aesthetic:refresh-user");

          // console.log("🗝️ Got fresh auth token.");
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
        const userExists = await fetch(
          `/user?from=${encodeURIComponent(userProfile.email)}&tenant=aesthetic`,
        );
        const u = await userExists.json();
        if (!u.sub || !userProfile.email_verified) {
          try {
            await window.auth0Client.getTokenSilently({ cacheMode: "off" });
            userProfile = await auth0Client.getUser();
          } catch (err) {
            console.warn("🧔 Could not retrieve user from network.");
          }
        }
        window.acUSER = userProfile; // Will get passed to the first message by the piece runner.
        window.acDISK_SEND({
          type: "session:started",
          content: { user: window.acUSER },
        });
      } else if (!pickedUpSession) {
        // console.log("🗝️ Not authenticated.");
        window.acDISK_SEND({
          type: "session:started",
          content: { user: null },
        });
      }

      // const after = performance.now();
      // console.log("🗝️ Auth took:", (after - before) / 1000, "seconds.");
    }
  })
  .catch((error) => {
    console.error("Failed to load Auth0 script:", error);
  });
} else {
  // Auth0 disabled in TEIA mode
}
// #endregion

// ***Incoming Message Responder***

// - At the moment it is just for a work-in-progress figma widget but any
//   window messages to be received here.
// TODO: Finish FigJam Widget with iframe message based input & output.
//         See also: https://www.figma.com/plugin-docs/working-with-images/

// setTimeout(function() {
//   console.log("attempting a real focus...")
//   document.querySelector("#software-keyboard-input").focus();
// }, 4000);

function receive(event) {
  // console.log("🌟 Event:", event);
  if (event.data?.type === "aesthetic-parent:focused") {
    window.acSEND?.({ type: "aesthetic-parent:focused" });
    return;
  }
  if (event.data?.type === "figma-image-input") {
    // TODO: Build image with width and height.
    console.log("Bytes:", event.data.bytes.length);
    return;
  }
  if (event.data?.type === "clipboard:copy:confirmation") {
    // Receive a clipboard copy confirmation from a hosted frame.
    // (vscode extension)
    window.acSEND?.({ type: "copy:copied" });
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
        type: "session:started",
        content: { user: window.acUSER },
      });
    }
    return;
  } else if (event.data?.type === "clearSession" && window.acTOKEN) {
    window.location.reload();
    return;
  } else if (event.data?.startsWith?.("docs:")) {
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

async function initNotifications() {
try {
  const { initializeApp } = await import(
    "https://www.gstatic.com/firebasejs/10.8.0/firebase-app.js"
  );

  const { getMessaging, onMessage, getToken } = await import(
    "https://www.gstatic.com/firebasejs/10.8.0/firebase-messaging.js"
  );

  function setupNotifications() {
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
                // console.log("Subcribed to:", data.topic);
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

  // Call the setup function  
  setupNotifications();
} catch (err) {
  console.warn("🔥 Could not initialize firebase notifications:", err);
}
}

let notificationsInitialized = false;

window.acRequestNotifications = function requestNotifications() {
  if (notificationsInitialized) return;
  if (previewOrIcon || sandboxed) return;
  notificationsInitialized = true;
  initNotifications();
};
