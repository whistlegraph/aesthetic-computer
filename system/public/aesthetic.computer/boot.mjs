// `aesthetic.computer` Bootstrap, 23.02.16.19.23
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
if (window.acDEBUG === true || window.acDEBUG === false) {
  debug = window.acDEBUG;
} else if (
  window.location.hostname === "aesthetic.computer" ||
  window.location.hostname === "m2w2.whistlegraph.com"
) {
  debug = false; // Turn debugging off by default in production.
} else {
  debug = true; // Turn debuging on by default everywhere else.
  // TODO: This should eventually be upgraded for IPFS exports.
}

// Check to see if we have a "#debug" hash.
if (window.location.hash === "#debug") debug = true;
if (window.location.hash === "#nodebug") debug = false;

window.acDEBUG = debug; // Set window.acDEBUG again just in case any code relies
// on it down the line. Should it need to? 22.07.15.00.21

const sandboxed = window.origin === "null";

if (!sandboxed && window.auth0) {
  // #region 🔐 Auth0: Universal Login & Authentication
  const clientId = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
  const auth0Client = await window.auth0?.createAuth0Client({
    domain: "https://auth0.aesthetic.computer",
    clientId,
    cacheLocation: "localstorage",
    useRefreshTokens: true,
    authorizationParams: {
      redirect_uri: window.location.origin,
    },
  });

  window.auth0Client = auth0Client;

  // Assumes a button with id "login" in the DOM
  if (
    location.search.includes("state=") &&
    (location.search.includes("code=") || location.search.includes("error="))
  ) {
    try {
      await auth0Client.handleRedirectCallback();
    } catch (e) {
      console.error("🔐", e);
    }
    window.history.replaceState({}, document.title, "/");
  }

  const isAuthenticated = await auth0Client.isAuthenticated();

  window.acLOGIN = async () => auth0Client.loginWithRedirect();

  window.acLOGOUT = () => {
    if (isAuthenticated) {
      console.log("🔐 Logging out...");
      auth0Client.logout({
        logoutParams: {
          returnTo: window.location.origin,
        },
      });
    } else {
      console.log("🔐 Already logged out!");
    }
  };

  if (isAuthenticated) {
    const userProfile = await auth0Client.getUser();
    console.log("🔐 Welcome,", userProfile.name, "!");
    console.log("to... \"chaos in a system\"");
    window.acUSER = userProfile; // Will get passed to the first message by the piece runner.
  }
}
// #endregion

// 🥾
// 2. Boot the aesthetic.computer system.

// If IPFS Exporting is revisited, then the below code should be rewritten
// and probably moved into `lib/parse.js`. 22.07.15.00.14

// TODO: This code is leftover from IPFS exporting... but shouldn't be adding on
//       to anything called "host" from this point on.
//       It might need to be revisited / refactored when new static
//       builds become necessary.
/*
if (window.location.pathname.length > 1) {
  const pathSegments = window.location.pathname.split("/");
  if (pathSegments[pathSegments.length - 1].endsWith(".html")) {
    pathSegments.pop();
  }
  host += pathSegments.join("/");
}
*/

// Set first input text to the default starting piece, which can be set in
// index.html via `window.acSTARTING_PIECE` or default to `prompt`.

// Or it can be set by a custom host...
if (location.hostname === "m2w2.whistlegraph.com")
  window.acSTARTING_PIECE = "wg~m2w2";

if (window.acSTARTING_PIECE === undefined) window.acSTARTING_PIECE = "prompt";

// Boot the machine with the specified root piece, or a #piece route if one
// is in the url.
/*
if (debug)
  console.log(
    "Parsed:",
    parse(slug(window.location.href) || window.acSTARTING_PIECE)
  );
*/

const parsed = parse(slug(window.location.href) || window.acSTARTING_PIECE);

const bpm = 120; // Set the starting bpm. Is this still necessary?

// TODO: Add params, search, and hash in here. 22.07.15.00.46

boot(parsed, bpm, undefined, debug);

// ***Incoming Message Responder***

// - At the moment it is just for a work-in-progress figma widget but any
//   window messages to be received here.
// TODO: Finish FigJam Widget with iframe message based input & output.
//         See also: https://www.figma.com/plugin-docs/working-with-images/
function receive(event) {
  // console.log("🌟 Event:", event);
  if (event.data.type === "figma-image-input") {
    // TODO: Build image with width and height.
    console.log("Bytes:", event.data.bytes.length);
  }
}
window.addEventListener("message", receive);

// TODO: Rewrite this snippet.
// Decoding an image can be done by sticking it in an HTML
// canvas, as we can read individual pixels off the canvas.
/*
async function decode(canvas, ctx, bytes) {
  const url = URL.createObjectURL(new Blob([bytes]));
  const image = await new Promise((resolve, reject) => {
    const img = new Image();
    img.onload = () => resolve(img);
    img.onerror = () => reject();
    img.src = url;
  });
  canvas.width = image.width;
  canvas.height = image.height;
  ctx.drawImage(image, 0, 0);
  const imageData = ctx.getImageData(0, 0, image.width, image.height);
  return imageData;
}
*/
