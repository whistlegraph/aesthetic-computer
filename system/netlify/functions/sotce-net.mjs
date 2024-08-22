// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🏁 TODO 
  --- 🏁 pre-launch
  - [-] add handle creation / handle support for sotce-net users
    - [*] there should be a 'create handle' button / (a dedicated handle space)
      - [] where does it go?
    - [] there should be a 'write a page' button
                           'compose' 
                           'write' 
         that appears once a handle has been created
      - [] this should prompt the user to make a handle
        - [] once a handle is made there should also be a way to change the handle
             and visit their profile ala ac; central button style
    - [] make sure handles will also be deleted
    - [] should the sign up page say 'subscribe to make a handle and keep a diary'
         or have intermediate content?
    - [] inherit handles from ac (only if the email is verified)
  - [] The handle system would be shared among ac users.
    - [] Perhaps the subs could be 'sotce' prefixed.
  - [] Allow Amelia's user / @sotce to post a diary, but no other users
       for now.
    - [] read from the database
  - [] Polish the sign up flow.
    - [] Use actual 'Helvetica' for the font? Or choose a special sans-serif.
    - [] Do better with the auth0 flow.
      - [] Re-capitalize login screens effectively.
      - [] Verification email should re-route to a nice page if in a new tab. 
        - [] Also check this on aesthetic.
  - [] Add email notifications for subscribed users.
  - [] Do some local mobile testing.
  - [] Run a production subscription.
  - [] Show number of signed up users so far.
  --- 🚩 post launch / next launch
  - [] How can I do shared reader cursors / co-presence somehow?
  + Done
  - [x] delete all existing subscriptions in stripe and resubscribe with a test
         user
  - [x] tapping the user's email address should allow them to alter / reset
        email even after initial verification...
  - [x] add cookie favicon which switches if the user is logged in...
  - [x] Account deletion.
  - [x] Implement a DOM structure for these layouts.
    - [x] Add a templated layout.
  💈 #wrapper.gate
    // 👣 logged-out 
    [login] [im-new]

    // 👣 unverified 
    signed-in-as: me@jas.life
    "waiting for verification" [resend?]
    [logout] [delete account]

    // 👣 unsubscribed
    signed-in-as: me@jas.life
    [subscribe]
    [logout] [delete account]

  🏡 #wrapper.garden
    // 👣 subscribed
    signed-in-as: me@jas.life
   
    ^^^^^settings^^^^^ (colophon?)
    "you are subscribed to sotce-net!"
    "your monthly subscription will auto-renew on July 7th, 2024 for $5"
    [cancel renewal]
    [delete account]
    _______________
    [logout]
  - [x] add a cancellation button...
    - [x] make sure it returns the right subscribed information below
  - [x] refreshing the page with websockets needs to keep the current session somehow!
  - [x] run a new development subscription
  - [x] try from vscode / cancel as needed? 
  - [x] bring in `respond` helper and replace `statusCode:` handler returns with it.
  - [x] stripe paywall
  - [x] set up subscription payment wall on 'subscribe' button
    - [x] run a test subscription
    - [x] check for subscription status somehow...
    - [x] Make subscription `priceId` on Stripe.
  - [x] bring in necessary env vars for stripe
  - [x] Add session / login support to the Aesthetic Computer VSCode extension /
       switch to an in-editor development flow.
  - [x] Need to be able to login to Aesthetic with VS Code, then switch to
       sotce-net and then back to aesthetic and stay logged in.
  - [️x] Need to be able to log in to sotce-net with VS Code, then switch
       to Aesthetic and back and stay logged in.
  - [x] Disambiguate 'session' query string and
        somehow receive both / capture both on first load.
        or know which one to send for both AC and Sotce-Net.
  - [x] Get rid of the 'user consent' signup field.
         (Only is necessary on localhost)
  - [x] auth0 based auth / registration
    - [x] review existing ac auth implementation and necessary
         keys
    - [x] Set up login pages / logo etc. 
    - [x] not logged in option
    - [x] logged in option
  - [x] Use shadow dom / custom element or a "modal" element for signup.
  - [x] Run through a test sign-up.
  - [x] Set up email sending service via google apps?
  - [x] Developmnt mode.
  - [x] Live reload function.
  - [x] Set up path->method routing in `sotce-net.js`.
#endregion */

/* #region 🤖 Dummy Copy
S is for sotce.
Shavasana brought this to mind. She does love how memories show themselves out in the supine state. She was too young then and wanted to be so old. She chose him because of his careful images and because of how he wrote to her. Sharp cold and eyes all over. Soon after rapid texting she went to see him. Shorter than he said he would be. Small even. Suspicious seeming. Still went with him to his apartment. Scandi style work from home kawaii decor in there. Saw all the figurines and light wood. Saw his work on display. Saw the photos of his open relationship girlfriend.
Striking. Surveyed his cool objects and new money. Sat on the red couch across the room from him. Stared. Slavic like her. Same age as Sean is now. She was interested in eating his food and looking around. Secretly she wanted to become like him too. Sort of dripped out and independent off of art. She wanted to learn how people could be this way. She had never met a man who wasn’t her teacher or her uncle. She wondered if she was smart (special?) enough to talk to him or if he was pretending like she was for sexual aspirations. Sad instant noodles of a vegan variety offered. Slurped them like worms. Something else packaged too that she can’t remember now. Seaweed or cookies. Soon she was nodding off on the couch in the brilliant air conditioning. She felt him touch her hands, arms, shoulders. She would learn later that her body reserved its deepest rest for the company of men. She would learn that sleep came very reverently when she felt guarded by a neurotic seeming someone. She woke up with him fully lying on her, his back crushed into hers. She felt his bird bones digging. Spine to spine. Something was taboo about this, something was incorrect. Suddenly startled, had to get out of there. Stayed too long and had a feeling. Serial killer vibes. Still she felt like she was breaking a promise. She told him she’d come back. She would get comfortable lying like this. She was too young to even have a purse to grab. Slanted gold light on the floor. Stumbling downstairs still stuck in her dream. Sudden fear from him, then anger. Screaming and blurry lines. Some voice caught in his throat sort of. Stream of texts blowing her up like she was the last thing on earth. Subway ride off the island and a quick blocking of his number. She got home and she didn’t tell her roommates. She didn’t know what she could have been to him. Some years later she looked him up. Struggled at first to remember his name. Saw she who had been his girlfriend on Instagram and went from there. She thought he was smart and special in spite of his urgency. She knew that on paper he let her sleep on his couch. She wondered if all men do this for girls. She wondered if all the wayward girls go to phone men to fall asleep. She saw on her phone that he died that night, that night that they met and she left. She left and blocked him after the fast long texts. She saw online that he drove crazy on his motorcycle. Saw he crashed it and died on the night they met.
#endregion */

// ♻️ Environment
const AUTH0_CLIENT_ID_SPA = "3SvAbUDFLIFZCc1lV7e4fAAGKWXwl2B0";
const AUTH0_DOMAIN = "https://hi.sotce.net";

// 💳 Payment
const SOTCE_STRIPE_API_PRIV_KEY = process.env.SOTCE_STRIPE_API_PRIV_KEY;
const SOTCE_STRIPE_API_PUB_KEY = process.env.SOTCE_STRIPE_API_PUB_KEY;
const SOTCE_STRIPE_API_TEST_PRIV_KEY =
  process.env.SOTCE_STRIPE_API_TEST_PRIV_KEY;
const SOTCE_STRIPE_API_TEST_PUB_KEY = process.env.SOTCE_STRIPE_API_TEST_PUB_KEY;
const SOTCE_STRIPE_ENDPOINT_DEV_SECRET =
  process.env.SOTCE_STRIPE_ENDPOINT_DEV_SECRET;
const SOTCE_STRIPE_ENDPOINT_SECRET = process.env.SOTCE_STRIPE_ENDPOINT_SECRET;

const dev = process.env.NETLIFY_DEV;

const priceId = dev
  ? "price_1PcGkMA9SniwoPrCdlOCsFJi"
  : "price_1PcH1BA9SniwoPrCCzLZdvES";
const productId = dev ? "prod_QTDAZAdV2KftJI" : "prod_QTDSAhsHGMRp3z";

import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
import { respond } from "../../backend/http.mjs";
import {
  authorize,
  deleteUser /* hasAdmin, */,
} from "../../backend/authorization.mjs";

import Stripe from "stripe";

export const handler = async (event, context) => {
  // console.log("Event:", event, "Context:", context, "Path:", event.path);
  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net"))
    path = path.replace("/sotce-net", "/").replace("//", "/");

  const key = dev
    ? process.env.SOTCE_STRIPE_API_TEST_PRIV_KEY
    : process.env.SOTCE_STRIPE_API_PRIV_KEY;

  const dateOptions = {
    weekday: "long",
    year: "numeric",
    month: "long",
    day: "numeric",
  };

  // 🏠 Home
  if (path === "/" && method === "get") {
    const assetPath = dev
      ? "/assets/sotce-net/"
      : "https://assets.aesthetic.computer/sotce-net/";

    const body = html`
      <html>
        <head>
          <meta charset="utf-8" />
          <link rel="icon" type="image/png" href="${assetPath}/cookie.png" />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
          />
          <style>
            body {
              font-family: sans-serif;
              background: rgb(255, 230, 225);
              margin: 0;
              width: 100vw;
              height: 100vh;
              -webkit-text-size-adjust: none;
            }
            #wrapper {
              display: flex;
              width: 100%;
              height: 100%;
            }
            #gate-curtain {
              position: absolute;
              width: 100%;
              height: 100%;
              display: flex;
              overflow: hidden;
            }
            #gate {
              margin: auto;
              width: 260px;
              max-width: 80vw;
              /* background: yellow; */
              z-index: 1;
              box-sizing: border-box;
              padding: 0em 0.5em 2.25em 0.5em;
            }
            #gate #cookie {
              max-width: 70%;
              max-height: 100%;
              margin: auto;
              display: block;
              user-select: none;
            }
            #gate #cookie.interactive {
              cursor: pointer;
              transition: 0.2s ease-out transform;
            }
            #gate #cookie.interactive:hover {
              transform: scale(0.99);
            }
            #gate #cookie.interactive:active {
              transform: scale(0.96);
              transition: 0.13s ease-out transform;
            }
            #gate h1 {
              font-weight: normal;
              font-size: 100%;
              margin: 0;
              margin-top: -0.5em;
              padding-bottom: 1em;
              text-align: center;
              user-select: none;
            }
            #gate h2 {
              font-weight: normal;
              font-size: 100%;
              margin: 0;
              text-align: center;
              padding-bottom: 1em;
              user-select: none;
            }
            #gate nav {
              display: flex;
              justify-content: center;
            }
            #gate nav:has(> *:first-child:nth-last-child(2)) {
              justify-content: space-between;
            }
            #gate nav button {
              color: black;
              background: rgb(255, 235, 183);
              padding: 0.35em;
              font-size: 100%;
              border: 0.205em solid rgb(255, 190, 215);
              filter: drop-shadow(-0.055em 0.055em 0.055em rgb(80, 80, 80));
              border-radius: 0.5em;
              cursor: pointer;
              user-select: none;
            }
            #gate nav button:hover {
              background: rgb(255, 240, 185);
            }
            #gate nav button:active {
              filter: drop-shadow(
                -0.035em 0.035em 0.035em rgba(40, 40, 40, 0.8)
              );
              background: rgb(255, 235, 183);
              transform: translate(-1px, 1px);
            }
            #garden {
              padding-left: 1em;
              padding-top: 1em;
            }
            #email {
              color: black;
            }
            #delete-account {
              color: black;
              position: absolute;
              font-size: 80%;
              left: calc(-130% / 8);
              bottom: -65%;
              width: 130%;
            }
            #logout-wrapper {
              position: relative;
            }
            #cookie-menu {
              position: absolute;
              top: 0;
              right: 0;
              width: 90px;
              user-select: none;
              cursor: pointer;
              transition: 0.2s ease-out transform;
            }
            #cookie-menu:hover {
              transform: scale(0.97);
            }
            #cookie-menu:active {
              transform: scale(0.93);
              transition: 0.13s ease-out transform;
            }
            #prompt {
              font-size: 22px;
              font-family: monospace;
              border: none;
              background: none;
              position: absolute;
              top: 16px;
              left: 16px;
              color: black;
              user-select: none;
              cursor: pointer;
            }
            #prompt:hover {
              color: rgb(180, 72, 135);
            }
            .hidden {
              visibility: hidden;
              pointer-events: none;
            }
          </style>
          ${dev ? reloadScript : ""}
          <script
            crossorigin="anonymous"
            src="/aesthetic.computer/dep/auth0-spa-js.production.js"
          ></script>
          <script src="https://js.stripe.com/v3/"></script>
        </head>
        <body>
          <div id="wrapper"></div>
          <script type="module">
            // 🗺️ Environment

            const dev = ${dev};
            const fromAesthetic =
              (document.referrer.indexOf("aesthetic") > -1 ||
                document.referrer.indexOf("localhost") > -1) &&
              document.referrer.indexOf("sotce-net") === -1;
            const embedded = window.self !== window.top;
            const cel = (el) => document.createElement(el); // shorthand

            function adjustFontSize() {
              // const vmin = Math.min(window.innerWidth, window.innerHeight) / 100;
              const fontSizeInPx = 16; //Math.max((window.devicePixelRatio || 1) * 3 * vmin, 16);
              document.body.style.fontSize = fontSizeInPx + "px";
            }

            window.addEventListener("resize", adjustFontSize);
            adjustFontSize();

            function asset(identifier) {
              return "${assetPath}" + identifier;
            }

            function cleanUrlParams(url, params) {
              const queryString = params.toString();
              history.pushState(
                {},
                "",
                url.pathname + (queryString ? "?" + queryString : ""),
              );
            }

            // Reload the page with the gate open in development.
            let GATE_WAS_UP = false;
            if (dev) {
              const url = new URL(window.location);
              const params = url.searchParams;
              const param = params.get("gate");
              GATE_WAS_UP = param === "up";
              params.delete("gate");
              cleanUrlParams(url, params);
            }

            // 🌠 Initialization

            // Send some messages to the VS Code extension.
            window.parent?.postMessage(
              { type: "url:updated", slug: "sotce-net" },
              "*",
            );
            window.parent?.postMessage({ type: "ready" }, "*");

            // Check for the '?notice=' parameter, memorize and clear it.
            const urlParams = new URLSearchParams(window.location.search);
            const notice = urlParams.get("notice");

            if (notice) {
              console.log("🪧 Notice:", notice);
              urlParams.delete("notice");
              const paramsString = urlParams.toString();
              const newUrl =
                window.location.pathname +
                (paramsString ? "?" + paramsString : "");
              window.history.replaceState({}, document.title, newUrl);
            }
            const wrapper = document.getElementById("wrapper");
            const gateElements = {};

            function gate(status, user, subscription) {
              // console.log(
              //   "🌒 Gate:",
              //   status,
              //   "user:",
              //   user,
              //   "subscription:",
              //   subscription,
              // );
              let message,
                buttons = [];

              // TODO: Remove any existing gate dom elements if they exist.
              const existingGate = document.getElementById("gate-curtain");
              existingGate?.remove();

              const g = document.createElement("div");
              const curtain = document.createElement("div");
              curtain.id = "gate-curtain";
              g.id = "gate";
              const img = document.createElement("img");
              img.id = "cookie";
              const h1 = document.createElement("h1");
              const h2 = cel("h2");
              const nav = document.createElement("nav");

              function genSubscribeButton() {
                h2.innerText = "email verified";
                const sb = cel("button");
                sb.onclick = subscribe;
                sb.innerText = "subscribe";
                return sb;
              }

              function genWelcomeMessage() {
                return (
                  "Signed in as <a href='' id='email'>" + user.email + "</a>"
                );
              }

              if (status !== "logged-out") {
                const lo = cel("button");
                lo.onclick = logout;
                lo.innerText = "log out";

                const lowrap = cel("div");
                lowrap.id = "logout-wrapper";
                lowrap.appendChild(lo);

                const del = cel("a");
                del.id = "delete-account";
                del.innerText = "delete account";
                del.href = "";

                del.onclick = function (e) {
                  e.preventDefault();
                  // Show a confirmation dialog before proceeding
                  if (
                    confirm("Are you sure you want to delete your account?") &&
                    confirm("This action cannot be undone!")
                  ) {
                    // Run the delete endpoint if confirmed
                    userRequest("POST", "/sotce-net/delete-account")
                      .then(async (res) => {
                        if (res.status === 200) {
                          alert("Your account has been deleted.");
                          logout();
                        } else {
                          alert(
                            "Your account could not be deleted. Please email hello@sotce.net.",
                          );
                          logout();
                        }
                      })
                      .catch((err) => {
                        console.error("🔴 Account deletion error...", err);
                      });
                  }
                };

                lowrap.appendChild(del);
                buttons.push(lowrap);
              }

              if (status === "logged-out") {
                message = "for your best thoughts";

                const lb = cel("button");
                lb.innerText = "log in";
                lb.onclick = login;
                buttons.push(lb);

                if (!embedded) {
                  const imnew = cel("button");
                  imnew.onclick = signup;
                  imnew.innerText = "i'm new";
                  buttons.push(imnew);
                }
              } else if (status === "unverified") {
                message = genWelcomeMessage();
                h2.innerText = "awaiting email verification...";

                const rs = cel("button");
                rs.onclick = resend;
                rs.innerText = "resend?";
                rs.classList.add("hidden");
                buttons.push(rs);

                fetchUser = function (email) {
                  // console.log("Fetching user...");
                  fetch(
                    "/user?from=" + encodeURIComponent(email) + "&tenant=sotce",
                  )
                    .then((res) => res.json())
                    .then(async (u) => {
                      if (u.email_verified) {
                        // ❤️‍🔥 TODO: Check to see if the user has a subscription here, before rendering a subscribe button.
                        user.email_verified = u.email_verified;
                        const entered = await subscribed();
                        if (entered) {
                          garden(entered, user);
                        } else {
                          if (!embedded) {
                            rs.remove();
                            nav.appendChild(genSubscribeButton());
                          } else {
                            h2.innerText = "please subscribe in your browser";
                          }
                        }
                      } else {
                        h2.innerText = "awaiting email verification...";
                        rs.classList.remove("hidden");
                        verificationTimeout = setTimeout(() => {
                          fetchUser(email);
                        }, 1000);
                      }
                    })
                    .catch(
                      (err) =>
                        (verificationTimeout = setTimeout(() => {
                          fetchUser(email);
                        }, 1000)),
                    );
                };
                fetchUser(user.email);
              } else if (status === "verified") {
                message = genWelcomeMessage();
                buttons.push(genSubscribeButton());
              } else if (status === "subscribed") {
                message = genWelcomeMessage();

                // 🙆 Add 'create handle' button here.
                const hb = cel("button");
                hb.innerText = "handle";
                hb.onclick = function handle() {
                  console.log("Prompt user for handle...");
                  const newHandle = prompt(
                    "Enter your username:",
                    "@urhandlehere",
                  );
                };

                // buttons.push(hb);

                if (subscription.until === "recurring") {
                  h2.innerText =
                    "Your subscription renews on " + subscription.renews; // + ".";
                  const cb = cel("button");
                  cb.innerText = "unsubscribe";
                  cb.onclick = cancel;
                  buttons.push(cb);
                } else {
                  h2.innerText =
                    "Your subscription ends on " +
                    subscription.until.toLowerCase() +
                    ".";
                  const ab = cel("button");
                  ab.innerText = "resubscribe";
                  ab.onclick = subscribe;
                  buttons.push(ab);
                }
              }

              if (!GATE_WAS_UP) {
                curtain.classList.add("hidden");
              } else {
                img.classList.add("interactive");
              }

              img.addEventListener(
                "click",
                () => {
                  if (!img.classList.contains("interactive")) return;
                  curtain.classList.add("hidden");
                  img.classList.remove("interactive");

                  document.querySelector("#garden")?.classList.remove("hidden");
                },
                // { once: true },
              );

              h1.innerHTML = message || "";
              if (buttons.length > 0)
                buttons.forEach((b) => nav.appendChild(b));
              g.appendChild(img);
              g.appendChild(h1);
              g.appendChild(h2);
              g.appendChild(nav);
              curtain.appendChild(g);
              img.onload = function () {
                wrapper.appendChild(curtain);

                const email = document.getElementById("email");
                if (email)
                  email.onclick = (e) =>
                    resend(e, status === "unverified" ? undefined : "change");
              };

              img.src = asset(
                status === "subscribed" ? "cookie-open.png" : "cookie.png",
              );
            }

            // Build a layout for the 🌻 'garden'.
            function garden(subscription, user) {
              gate("subscribed", user, subscription);

              // Change out the favicon url...
              document.querySelector('link[rel="icon"]').href =
                asset("cookie-open.png");

              const g = cel("div");
              g.id = "garden";

              // TODO: Add a blog post here to the garden layout.
              g.innerText = "Hello...";

              const cookie = cel("img");
              cookie.id = "cookie-menu";
              cookie.src = asset("cookie.png");
              g.appendChild(cookie);

              if (GATE_WAS_UP) g.classList.add("hidden");

              cookie.onclick = function () {
                const curtain = document.getElementById("gate-curtain");
                curtain.classList.remove("hidden");
                g.classList.add("hidden");
                const curtainCookie = curtain.querySelector("#cookie");
                curtainCookie.classList.add("interactive");
              };

              cookie.onload = function () {
                wrapper.appendChild(g);
              };
            }

            // 🔐 Authorization
            const clientId = "${AUTH0_CLIENT_ID_SPA}";
            let verificationTimeout;
            let isAuthenticated = false;
            let fetchUser;
            let user;

            async function retrieveUncachedUser() {
              try {
                await auth0Client.getTokenSilently({ cacheMode: "off" });
                user = await auth0Client.getUser();
                // console.log("🎇 New user is...", user);
              } catch (err) {
                console.log("Error retrieving uncached user.");
              }
            }

            const auth0Client = await window.auth0.createAuth0Client({
              domain: "${AUTH0_DOMAIN}",
              clientId,
              cacheLocation: "localstorage",
              useRefreshTokens: true,
              authorizationParams: { redirect_uri: window.location.href },
            });

            if (embedded || fromAesthetic) {
              const prompt = document.createElement("button");
              prompt.id = "prompt";
              prompt.onclick = aesthetic;
              prompt.innerHTML = "sotce-net";
              document.body.appendChild(prompt);
            }

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
              window.history.replaceState(
                {},
                document.title,
                window.location.pathname,
              );
            }

            // 😶‍🌫️ Picking up a session from VS Code / the parent frame.
            let pickedUpSession;
            {
              const url = new URL(window.location);
              const params = url.searchParams;
              let param = params.get("session-sotce");

              if (param === "null") {
                localStorage.removeItem("session-sotce");
              } else if (param === "retrieve") {
                param = localStorage.getItem("session-sotce");
              }

              const sessionParams = param;
              let encodedSession = sessionParams;
              if (encodedSession === "null") encodedSession = undefined;
              if (encodedSession) {
                // console.log("🪷 Sotce Session:", encodedSession);
                const sessionJsonString = atob(
                  decodeURIComponent(encodedSession),
                );
                const session = JSON.parse(sessionJsonString);
                // Use the session information to authenticate, if it exists.
                // console.log("🥀 Session data:", session);
                if (session.accessToken && session.account) {
                  window.sotceTOKEN = session.accessToken; // Only set using this flow.
                  window.sotceUSER = {
                    email: session.account.label,
                    sub: session.account.id,
                  };
                  // console.log(

                  //   "🌻 Picked up sotce session!",
                  //   window.sotceTOKEN,
                  //   window.sotceUSER,
                  // );
                  pickedUpSession = true;
                }

                if (sessionParams) {
                  localStorage.setItem("session-sotce", encodedSession);
                  params.delete("session-sotce"); // Remove the 'session' parameter
                  // Update the URL without reloading the page
                  cleanUrlParams(url, params);
                }
              }
            }

            // Logging in normally.
            if (!pickedUpSession) {
              isAuthenticated = await auth0Client.isAuthenticated();

              // Try to fetch a new token on every refresh.
              // TODO: ❤️‍🔥 Is this necessary?
              if (isAuthenticated) {
                try {
                  await auth0Client.getTokenSilently(/*{ cacheMode: "off" }*/);
                  // console.log("🗝️ Got fresh token.");
                } catch (error) {
                  console.log("🔐️ ❌ Unauthorized", error);
                  console.error(
                    "Failed to retrieve token silently. Logging out.",
                    error,
                  );
                  // Redirect the user to logout if the token has failed.
                  auth0Client.logout({
                    logoutParams: { returnTo: window.location.href },
                  });
                  isAuthenticated = false;
                }
              }
            } else {
              isAuthenticated = true;
            }

            if (!isAuthenticated) {
              gate("logged-out");
            } else {
              // 🅰️ Check / await email verification.
              user = pickedUpSession
                ? window.sotceUSER
                : await auth0Client.getUser();
              // console.log("🪷 Got user:", user);
              if (!user.email_verified) await retrieveUncachedUser();
              if (!user.email_verified) {
                gate("unverified", user);
              } else {
                const entered = await subscribed();
                if (entered?.subscribed) {
                  garden(entered, user);
                } else if (entered !== "error") {
                  gate("verified", user);
                } else {
                  console.log("🔴 Server error.");
                  // gate("error");
                }
              }
            }

            function login(hint = "login") {
              if (embedded) {
                window.parent.postMessage(
                  { type: "login", tenant: "sotce" },
                  "*",
                );
              } else {
                const opts = { prompt: "login" }; // Never skip the login screen.
                opts.screen_hint = hint;
                auth0Client.loginWithRedirect({ authorizationParams: opts });
              }
            }

            function signup() {
              if (embedded) {
                console.log("🟠 Cannot sign up in an embedded view.");
              } else {
                login("signup");
              }
            }

            // Go to the paywalled subscription page.
            async function subscribe() {
              const stripe = Stripe(
                "${dev
                  ? SOTCE_STRIPE_API_TEST_PUB_KEY
                  : SOTCE_STRIPE_API_PUB_KEY}",
              );
              const response = await fetch("/sotce-net/subscribe", {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ email: user.email, sub: user.sub }),
              });
              if (response.ok) {
                const session = await response.json();
                // console.log("💳 Session:", session);
                const result = await stripe.redirectToCheckout({
                  sessionId: session.id,
                });
                if (result.error) console.error(result.error.message);
              } else {
                const error = await response.json();
                console.error("💳", error.message);
              }
            }

            // Check the subscription status of the logged in user.
            async function subscribed() {
              if (!user) return false;
              // console.log("🗞️ Checking subscription status for:", user.email);
              const response = await userRequest(
                "POST",
                "sotce-net/subscribed",
              );

              if (response.status === 200) {
                // console.log("💳 Subscribed:", response);
                if (response.subscribed) {
                  return response;
                } else {
                  return false;
                }
              } else {
                console.error("💳", response);
                return "error";
              }
            }

            // Cancel an existing subscription.
            async function cancel() {
              if (!user) return;

              const confirmation = confirm("Cancel your subscription?");
              if (!confirmation) return;

              try {
                const response = await fetch("/sotce-net/cancel", {
                  method: "POST",
                  headers: {
                    "Content-Type": "application/json",
                    Authorization:
                      "Bearer " +
                      (window.sotceTOKEN ||
                        (await auth0Client.getTokenSilently())),
                  },
                });

                if (response.ok) {
                  const result = await response.json();
                  console.log("Subscription cancelled:", result);
                  alert(result.message);
                  location.reload();
                } else {
                  const error = await response.json();
                  console.error("Cancellation error:", error.message);
                  alert("Failed to cancel subscription: " + error.message);
                }
              } catch (error) {
                console.error("Error:", error);
                alert("An error occurred while cancelling the subscription.");
              }
            }

            function logout() {
              if (isAuthenticated) {
                console.log("🔐 Logging out...", window.location.href);
                if (embedded) {
                  localStorage.removeItem("session-sotce");
                  window.parent.postMessage(
                    { type: "logout", tenant: "sotce" },
                    "*",
                  );
                } else {
                  auth0Client.logout({
                    logoutParams: { returnTo: window.location.href },
                  });
                }
              } else console.log("🔐 Already logged out!");
            }

            function resend(e, type) {
              e?.preventDefault();
              clearTimeout(verificationTimeout);

              const promptText =
                type === "change"
                  ? "Change email to?"
                  : "Resend verification email to?";

              const email = prompt(promptText, user.email);
              if (!email) return;
              console.log("📧 Resending...", email);

              userRequest("POST", "/api/email", {
                name: email,
                email,
                tenant: "sotce",
              })
                .then(async (res) => {
                  if (res.status === 200) {
                    console.log("📧 Email verification sent...", res);

                    if (type !== "change") {
                      alert("📧 Verification email sent!");
                      const emailEl = document.getElementById("email");
                      emailEl.innerHTML = email;
                      await retrieveUncachedUser();
                      fetchUser(email);
                    } else {
                      window.location.reload();
                    }
                  } else {
                    throw new Error(res.code);
                  }
                })
                .catch((err) => {
                  alert("Email verification error.");
                  console.error("🔴 📧 Email verification error...", err);
                });
            }

            // Jump to Aesthetic Computer.
            function aesthetic() {
              let href = dev
                ? "https://localhost:8888"
                : "https://aesthetic.computer";
              if (embedded) {
                window.parent.postMessage(
                  { type: "url:updated", slug: "prompt" },
                  "*",
                );
                href += "?session-aesthetic=retrieve";
              }
              window.location.href = href;
            }

            async function userRequest(method, endpoint, body) {
              try {
                const token =
                  window.sotceTOKEN || (await auth0Client.getTokenSilently());
                if (!token) throw new Error("🧖 Not logged in.");

                const headers = {
                  Authorization: "Bearer " + token,
                  "Content-Type": "application/json",
                };

                const options = { method, headers };
                if (body) options.body = JSON.stringify(body);
                const response = await fetch(endpoint, options);

                if (response.status === 500) {
                  try {
                    const json = await response.json();
                    return { status: response.status, ...json };
                  } catch (e) {
                    return {
                      status: response.status,
                      message: response.statusText,
                    };
                  }
                } else {
                  const clonedResponse = response.clone();
                  try {
                    return {
                      ...(await clonedResponse.json()),
                      status: response.status,
                    };
                  } catch {
                    return {
                      status: response.status,
                      body: await response.text(),
                    };
                  }
                }
              } catch (error) {
                console.error("🚫 Error:", error);
                return { message: "unauthorized" };
              }
            }

            window.login = login;
            window.logout = logout;
            window.cancel = cancel;
            window.signup = signup;
            window.resend = resend;
          </script>
        </body>
      </html>
    `;
    return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
  } else if (path === "/subscribe" && method === "post") {
    try {
      const stripe = Stripe(key);
      // console.log("💳", stripe);

      const redirectPath =
        event.headers.origin === "https://sotce.net" ? "" : "sotce-net";

      console.log(
        "🗺️ Origin:",
        event.headers.origin,
        "redirectPath:",
        redirectPath,
      );

      const { email, sub } = JSON.parse(event.body);

      // Search for the customer by the metadata field 'sub'

      const customers = await stripe.customers.search({
        query: `metadata['sub']:'${sub}'`,
      });

      let customer;

      if (customers.data.length > 0) {
        // Customer found, optionally update metadata
        // customer = await stripe.customers.update(customers.data[0].id, {
        //  metadata: { sub },
        // });
        customer = customers.data[0];
      } else {
        // Customer doesn't exist, create a new one
        customer = await stripe.customers.create({
          email: email,
          metadata: { sub },
        });
      }

      const session = await stripe.checkout.sessions.create({
        payment_method_types: ["card"],
        mode: "subscription",
        line_items: [{ price: priceId, quantity: 1 }],
        // customer_email: email,
        customer: customer.id, // Attach the existing or newly created customer
        success_url: `${event.headers.origin}/${redirectPath}?notice=success`,
        cancel_url: `${event.headers.origin}/${redirectPath}?notice=cancel`,
        // metadata: { sub },
      });

      return respond(200, { id: session.id });
    } catch (error) {
      console.log("⚠️", error);
      return respond(500, { message: `Error: ${error.message}` });
    }
  } else if (path === "/subscribed" && method === "post") {
    // First validate that the user has an active session via auth0.
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized user." });

    const email = user.email;
    const sub = user.sub;
    let subscription;

    // Then, make sure they are subscribed.
    try {
      const stripe = Stripe(key);
      // Fetch customer by email [deprecated in favor of storing user id (sub)]
      // const customers = await stripe.customers.list({ email, limit: 1 });

      // Fetch customer by user ID (sub) from metadata
      const customers = await stripe.customers.search({
        query: "metadata['sub']:'" + sub + "'",
      });

      if (customers.data.length === 0) {
        return respond(200, { subscribed: false });
      }
      const customer = customers.data[0];

      // Fetch subscriptions for the customer
      const subscriptions = await stripe.subscriptions.list({
        customer: customer.id,
        status: "active", // Only find the first active subscription...
        limit: 1,
      });
      subscription = subscriptions.data.find((sub) =>
        sub.items.data.some((item) => item.price.product === productId),
      );
    } catch (err) {
      console.error("Error fetching subscription status:", err);
      return respond(500, { error: "Failed to fetch subscription status" });
    }

    if (subscription && subscription.status === "active") {
      const until = subscription.cancel_at_period_end
        ? new Date(subscription.cancel_at * 1000).toLocaleDateString(
            "en-US",
            dateOptions,
          )
        : "recurring";

      let renews;
      if (until === "recurring") {
        renews = `${new Date(subscription.current_period_end * 1000).toLocaleDateString("en-US", dateOptions)}`;
      }

      return respond(200, { subscribed: true, until, renews });
    } else {
      return respond(200, { subscribed: false });
    }
  } else if (path === "/cancel" && method === "post") {
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized user." });
    const cancelResult = await cancelSubscription(user, key);
    return respond(cancelResult.status, cancelResult.body);
  } else if (path === "/delete-account" && method === "post") {
    // See also the 'delete-erase-and-forget-me.js' function for aesthetic users.
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Authorization failure..." });
    // 1. Unsubscribe the user if they have an active subscription.
    const cancelResult = await cancelSubscription(user, key);
    console.log("❌ Cancelled subscription?", cancelResult.status);
    // TODO: 2. Delete any user data, like posts.
    // TODO: 3. Delete the user's handle if it exists.
    // 3. Delete the user's auth0 account.
    const deleted = await deleteUser(user.sub, "sotce");
    console.log("❌ Deleted user registration:", deleted);
    return respond(200, { result: "Deleted!" }); // Successful account deletion.
  }
};

async function cancelSubscription(user, key) {
  const email = user.email;
  const result = { status: undefined, body: undefined };
  try {
    const stripe = Stripe(key);
    // Fetch customer by email
    const customers = await stripe.customers.list({ email, limit: 1 });
    if (customers.data.length === 0) {
      result.status = 404;
      result.body = { message: "Customer not found." };
      return result;
    }

    const customer = customers.data[0];

    // Fetch subscriptions for the customer
    const subscriptions = await stripe.subscriptions.list({
      customer: customer.id,
      status: "all",
      limit: 10,
    });

    if (subscriptions.data.length === 0) {
      result.status = 404;
      result.body = { message: "Subscription not found." };
      return result;
    }

    // Find the subscription matching the productId
    const subscription = subscriptions.data.find((sub) =>
      sub.items.data.some((item) => item.price.product === productId),
    );

    if (!subscription) {
      result.status = 404;
      result.body = { message: "Subscription not found." };
      return result;
    }

    // Cancel the subscription
    const cancelled = await stripe.subscriptions.update(subscription.id, {
      cancel_at_period_end: true,
    });

    result.status = 200;
    result.body = {
      message: `Your subscription will not be renewed after ${new Date(
        cancelled.cancel_at * 1000,
      ).toLocaleDateString("en-US", dateOptions)}.`,
      subscription: cancelled,
    };
    return result;
  } catch (error) {
    console.error("Error cancelling subscription:", error);
    result.status = 500;
    result.body = { message: `Error: ${error.message}` };
    return result;
  }
}

// Inserted in `dev` mode for live reloading.
const reloadScript = html`
  <script>
    let reconnectInterval = false;
    function connect() {
      clearInterval(reconnectInterval);
      let ws;
      try {
        ws = new WebSocket("wss://localhost:8889");
      } catch {
        console.warn("🧦 Connection failed.");
        return;
      }

      // ws.onopen = (e) => console.log("🧦 Connected:", e);
      let reloadTimeout;

      ws.onmessage = (e) => {
        const msg = JSON.parse(e.data);
        if (msg.type === "reload" && msg.content.piece === "*refresh*") {
          console.log("🧦 ♻️ Reloading...");
          clearTimeout(reloadTimeout);
          reloadTimeout = setTimeout(() => {
            const sessionItem = localStorage.getItem("session-sotce");
            const gateUp = document.querySelector("#gate-curtain:not(.hidden)");
            if (sessionItem) {
              const url = new URL(window.location.href);
              url.searchParams.set("session-sotce", "retrieve");
              if (gateUp) url.searchParams.set("gate", "up");
              window.location.href = url.toString();
            } else {
              const url = new URL(window.location.href);
              if (gateUp) url.searchParams.set("gate", "up");
              window.location.href = url.toString();
            }
          }, 150); // 📓 Could take a sec for the function to reload... 24.08.08.00.53
        }
      };

      ws.onclose = (e) => {
        console.log("🧦 🔴 Closed:", e);
        reconnectInterval = setInterval(connect, 1000);
      };
    }
    connect();
  </script>
`.trim();
