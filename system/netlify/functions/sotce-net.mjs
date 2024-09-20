// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🟢 TODO 

  *** ⭐ Page Composition ***
  - [🟠] Test scaffolded end<->end page creation logic.
  - [] keep draft remotely / have a "published" flag on pages
  - [] show rules / timer under the form
  + Done 
  - [x] add endpoint for submitting a "page"
  - [x] add the 'write a page' button
        whitelisted for admin users
  - [x] show the form, maybe in a modal?

  *** 📟 Page Feed ***
   - [] upscrolling
   - [] editing / modification on a page

  *** 🛂 Page Controls ***
  - [] redaction

  *** 📧 Email Notifications for Pages ***
  - [] email new pages to each subscriber, and include the contents?
    - [] make an 'eblast' endpoint for this
    - [] add the checkbox under the main page

  *** 📄 Page Reactions ***
  - [] Add some kind of handle based reaction for pages? (touch?)

  *** 📊 Statistics ***
  - [] Show number of subscribed users so far - maybe in the closed donut or
       privacy policy? and only for certain whitelisted users?
  - [] Add some form of google analytics.

  *** 🖨️ Typography & Design ***
  - [] Use actual 'Helvetica' for the font? Or try to choose a special sans-serif.
  - [] Test mobile designs locally.

  *** 🛩️ Pre-flight ***
  - [] Test in production.

  --- 🏁 Launch 🏁 ---

  --- ☁️ Post-Launch ☁️ ---
  - [] Automatic Dark Theme
  - [] Patreon linkage?
  - [] print 🖨️ css
  - [] Search / hashtags
  + Done
  - [x] Add 'isAdmin' support for sotce-net subs and add necessary subs.
  - [x] go through all the prompt boxes, including the username entry / too long / inappropriate etc.
  - [x] make a privacy policy for sotce.net (inlined in this file)
    - [x] update ac privacy policy with shared accounting rules
#endregion */

/* #region 🤖 Dummy Copy
S is for sotce.
Shavasana brought this to mind. She does love how memories show themselves out in the supine state. She was too young then and wanted to be so old. She chose him because of his careful images and because of how he wrote to her. Sharp cold and eyes all over. Soon after rapid texting she went to see him. Shorter than he said he would be. Small even. Suspicious seeming. Still went with him to his apartment. Scandi style work from home kawaii decor in there. Saw all the figurines and light wood. Saw his work on display. Saw the photos of his open relationship girlfriend.
Striking. Surveyed his cool objects and new money. Sat on the red couch across the room from him. Stared. Slavic like her. Same age as Sean is now. She was interested in eating his food and looking around. Secretly she wanted to become like him too. Sort of dripped out and independent off of art. She wanted to learn how people could be this way. She had never met a man who wasn’t her teacher or her uncle. She wondered if she was smart (special?) enough to talk to him or if he was pretending like she was for sexual aspirations. Sad instant noodles of a vegan variety offered. Slurped them like worms. Something else packaged too that she can’t remember now. Seaweed or cookies. Soon she was nodding off on the couch in the brilliant air conditioning. She felt him touch her hands, arms, shoulders. She would learn later that her body reserved its deepest rest for the company of men. She would learn that sleep came very reverently when she felt guarded by a neurotic seeming someone. She woke up with him fully lying on her, his back crushed into hers. She felt his bird bones digging. Spine to spine. Something was taboo about this, something was incorrect. Suddenly startled, had to get out of there. Stayed too long and had a feeling. Serial killer vibes. Still she felt like she was breaking a promise. She told him she’d come back. She would get comfortable lying like this. She was too young to even have a purse to grab. Slanted gold light on the floor. Stumbling downstairs still stuck in her dream. Sudden fear from him, then anger. Screaming and blurry lines. Some voice caught in his throat sort of. Stream of texts blowing her up like she was the last thing on earth. Subway ride off the island and a quick blocking of his number. She got home and she didn’t tell her roommates. She didn’t know what she could have been to him. Some years later she looked him up. Struggled at first to remember his name. Saw she who had been his girlfriend on Instagram and went from there. She thought he was smart and special in spite of his urgency. She knew that on paper he let her sleep on his couch. She wondered if all men do this for girls. She wondered if all the wayward girls go to phone men to fall asleep. She saw on her phone that he died that night, that night that they met and she left. She left and blocked him after the fast long texts. She saw online that he drove crazy on his motorcycle. Saw he crashed it and died on the night they met.
#endregion */

// ♻️ Environment
const AUTH0_CLIENT_ID_SPA = "3SvAbUDFLIFZCc1lV7e4fAAGKWXwl2B0";
const AUTH0_DOMAIN = "https://hi.sotce.net";

const dev = process.env.NETLIFY_DEV;

// 💳 Payment
import {
  SOTCE_STRIPE_API_PRIV_KEY,
  SOTCE_STRIPE_API_PUB_KEY,
  SOTCE_STRIPE_API_TEST_PRIV_KEY,
  SOTCE_STRIPE_API_TEST_PUB_KEY,
  SOTCE_STRIPE_ENDPOINT_DEV_SECRET,
  SOTCE_STRIPE_ENDPOINT_SECRET,
  priceId,
  productId,
} from "../../backend/sotce-net-constants.mjs";

import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";
import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";
import { shell } from "../../backend/shell.mjs";

import {
  authorize,
  deleteUser,
  hasAdmin,
  getHandleOrEmail,
  userIDFromEmail,
} from "../../backend/authorization.mjs";
import * as KeyValue from "../../backend/kv.mjs";

import Stripe from "stripe";

const dateOptions = {
  weekday: "long",
  year: "numeric",
  month: "long",
  day: "numeric",
};

export const handler = async (event, context) => {
  // console.log("Event:", event, "Context:", context, "Path:", event.path);
  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net"))
    path = path.replace("/sotce-net", "/").replace("//", "/");

  const key = dev ? SOTCE_STRIPE_API_TEST_PRIV_KEY : SOTCE_STRIPE_API_PRIV_KEY;
  const assetPath = dev
    ? "/assets/sotce-net/"
    : "https://assets.aesthetic.computer/sotce-net/";

  // 🏠 Home
  if (path === "/" && method === "get") {
    const body = html`
      <html>
        <head>
          <meta charset="utf-8" />
          <title>sotce.net</title>
          <link rel="icon" type="image/png" href="${assetPath}/cookie.png" />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
          />
          <style>
            body {
              font-family: sans-serif;
              margin: 0;
              width: 100%;
              height: 100vh;
              -webkit-text-size-adjust: none;
              background: rgb(255, 230, 225);
              user-select: none;
            }
            #wrapper {
              display: flex;
              width: 100%;
              height: 100%;
              /* min-width: 220px; */
              background: rgb(255, 230, 225);
              position: relative;
            }
            #wrapper.reloading {
              filter: blur(2px) saturate(1.25);
              transition: 0.25s filter;
            }
            #wrapper.flash::after {
              content: "";
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              pointer-events: none;
              z-index: 2;
            }
            #wrapper.flash.red::after {
              /* sprinkle matching red */
              background-color: rgb(180, 11, 40, 0.8);
            }
            #spinner,
            #veil div.spinner {
              width: 5vmin;
              height: 5vmin;
              background: rgb(255, 147, 191);
              margin: auto;
              border-radius: 100%;
              filter: blur(4px);
              transition: 0.25s opacity ease-in;
              opacity: 0;
              animation: spinner 1s infinite alternate ease-in-out;
            }
            #veil div.spinner {
              background: white;
            }
            #spinner.showing,
            #veil div.spinner.showing {
              opacity: 1;
              transition: 0.3s opacity ease-in;
            }
            @keyframes spinner {
              0% {
                filter: blur(4px);
                transform: scale(1);
              }
              50% {
                filter: blur(2px);
                transform: scale(1.2);
              }
              100% {
                filter: blur(4px);
                transform: scale(1);
              }
            }
            #full-alert {
              margin: auto;
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
            #gate.coming-soon {
              padding: 0;
            }
            #gate.coming-soon :is(h2, h1, nav) {
              display: none;
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
            #gate #nav-high {
              margin-top: -0.5em;
            }
            #gate :is(#nav-low, #nav-high) {
              display: flex;
              justify-content: center;
            }
            #gate
              :is(#nav-low, #nav-high):has(> *:first-child:nth-last-child(2)) {
              justify-content: space-between;
            }
            #gate nav button,
            #write-a-page {
              color: black;
              background: rgb(255, 235, 183);
              padding: 0.35em;
              font-size: 100%;
              border: 0.205em solid rgb(255, 190, 215);
              filter: drop-shadow(-0.055em 0.055em 0.055em rgb(80, 80, 80));
              border-radius: 0.5em;
              cursor: pointer;
              user-select: none;
              margin-bottom: 1em;
            }
            #write-a-page {
              margin-left: 1em;
              margin-top: 1em;
              z-index: 3;
              position: fixed;
            }
            #gate nav button:hover,
            #write-a-page:hover {
              background: rgb(255, 245, 170);
            }
            #gate nav button:active,
            #write-a-page:active {
              filter: drop-shadow(
                -0.035em 0.035em 0.035em rgba(40, 40, 40, 0.8)
              );
              background: rgb(255, 248, 165);
              transform: translate(-1px, 1px);
            }
            #write-a-page {
              /* background: rgb(240, 240, 240); */
              /* border-color: rgb(40, 40, 200); */
            }
            #gate nav button.positive {
              background: rgb(203, 238, 161);
              border-color: rgb(114, 203, 80);
            }
            #gate nav button.positive:hover {
              background: rgb(199, 252, 136);
            }
            #gate nav button.positive:active {
              background: rgb(210, 252, 146);
            }
            #garden {
              /* padding-left: 1em; */
              /* padding-top: 1em; */
              box-sizing: border-box;
              width: 100%;
            }
            #garden article.page {
              background-color: white;
              border: 0.1em solid black;
              padding: 1em;
              margin-bottom: 1em;
              margin-left: auto;
              margin-right: auto;
              width: 100%;
              aspect-ratio: 4 / 5;
              /*max-width: calc(800px / 2);*/
              /*max-height: calc(1000px / 2);*/
              transform-origin: top left;
              position: relative;
              overflow: hidden;
              box-sizing: border-box;
            }
            #binding {
              background: yellow;
              margin-top: calc(68px + 16px + 16px);
              /* padding-left: 16px; */
              /* padding-right: 16px; */
              margin-left: auto;
              margin-right: auto;
              box-sizing: border-box;
            }
            #garden article.page p {
              /* font-size will be set in javascript
                 badsed on the parent container */
              line-height: 1.5em; /* Maintains a consistent line height relative to font size */
              text-align: left;
              margin: 0;
              padding: 0;
            }

            #email {
              position: relative;
              color: black;
              /* user-select: all; */
            }
            #email.admin::after {
              content: "🪷";
              font-size: 85%;
              position: absolute;
              top: -0.3em;
              right: -1.25em;
              opacity: 0.75;
            }
            #email:hover {
              color: maroon;
            }
            #email:active {
              color: darkgreen;
            }
            #delete-account,
            #privacy-policy {
              color: black;
              position: absolute;
              font-size: 80%;
              bottom: -15%;
              user-select: none;
            }
            #delete-account {
              left: calc(-130% / 8);
              width: 130%;
            }
            #delete-account:hover {
              color: rgb(200, 0, 0);
            }
            #delete-account:active {
              color: red;
            }
            #privacy-policy {
              /* 'width' and 'left' value calculated in js 'genSubscribeButton' */
            }
            #privacy-policy:hover {
              color: rgb(0, 0, 200);
            }
            #privacy-policy:active {
              color: blue;
            }
            #logout-wrapper,
            #secondary-wrapper {
              position: relative;
            }
            #cookie-menu {
              position: fixed;
              top: 0;
              right: 0;
              width: 90px;
              user-select: none;
              cursor: pointer;
              transition: 0.2s ease-out transform;
            }
            @media (max-width: 220px) {
              #cookie-menu {
                position: absolute;
              }
              #write-a-page {
                position: absolute;
              }
              /* #binding {
                margin-top: calc(68px + 16px);
              } */
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
              overflow: hidden;
            }
            .obscured {
              display: none !important;
              pointer-events: none;
            }
            .loading-dots::after {
              content: "\\00a0\\00a0\\00a0"; /* Three non-breaking spaces */
              animation: loading-dots 1s steps(3, end) infinite;
            }
            @keyframes loading-dots {
              0% {
                content: "\\00a0\\00a0\\00a0";
              } /* Empty spaces */
              33% {
                content: ".\\00a0\\00a0";
              } /* One dot, two spaces */
              66% {
                content: "..\\00a0";
              } /* Two dots, one space */
              100% {
                content: "...";
              } /* Three dots */
            }
            #veil {
              position: fixed;
              z-index: 1;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              display: flex;
              background: black;
              opacity: 0.75;
              transition: 0.5s opacity;
            }
            #veil.unveiled {
              opacity: 0;
              pointer-events: none;
            }
            #veil.unveiled-instant {
              transition: none;
              opacity: 0;
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
          <div id="wrapper">
            <div id="spinner"></div>
          </div>
          <div id="veil" class="unveiled"></div>
          <script type="module">
            // 🗺️ Environment
            const dev = ${dev};
            const fromAesthetic =
              (document.referrer.indexOf("aesthetic") > -1 ||
                document.referrer.indexOf("localhost") > -1) &&
              document.referrer.indexOf("sotce-net") === -1;
            const embedded = window.self !== window.top;
            const url = new URL(window.location);
            const cel = (el) => document.createElement(el); // shorthand
            let fullAlert;
            let waitForSubscriptionSuccessThreeTimes = false;
            const { min, max } = Math;

            // 🌠 Initialization

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
              const queryString = params?.toString();
              history.pushState(
                {},
                "",
                url.pathname + (queryString ? "?" + queryString : ""),
              );
            }

            // Reload the page with the gate open in development.
            let GATE_WAS_UP = false;

            if (dev) {
              const params = url.searchParams;
              const param = params.get("gate");
              GATE_WAS_UP = param === "up";
              params.delete("gate");
              cleanUrlParams(url, params);
            }

            // Send some messages to the VS Code extension.
            window.parent?.postMessage(
              { type: "url:updated", slug: "sotce-net" },
              "*",
            );
            window.parent?.postMessage({ type: "ready" }, "*");

            // 🗺️🎏 URL Param Flags
            // Check for the '?notice=' parameter, memorize and clear it.
            {
              const urlParams = new URLSearchParams(window.location.search);
              const notice = urlParams.get("notice");

              if (notice) {
                console.log("🪧 Notice:", notice, urlParams);
                if (notice === "success")
                  waitForSubscriptionSuccessThreeTimes = true;

                urlParams.delete("notice");
                cleanUrlParams(url, urlParams);
              }
            }

            // 📧 Check to see if the user clicked an 'email' verified link.
            {
              const urlParams = new URLSearchParams(window.location.search);
              if (
                urlParams.get("supportSignUp") === "true" &&
                urlParams.get("success") === "true" &&
                urlParams.get("code") === "success"
              ) {
                urlParams.delete("supportSignUp");
                urlParams.delete("supportForgotPassword");
                urlParams.delete("message");
                urlParams.delete("success");
                urlParams.delete("code");
                cleanUrlParams(url, urlParams);
                fullAlert = "Email verified!";
              }
            }

            // Clean any url params if the first param is 'iss'.
            if (window.location.search.startsWith("?iss")) cleanUrlParams(url);

            const wrapper = document.getElementById("wrapper");

            // Reload fading.
            window.addEventListener("beforeunload", (e) => {
              wrapper.classList.add("reloading");
            });

            document.addEventListener("visibilitychange", function () {
              if (!document.hidden) wrapper.classList.remove("reloading");
            });

            const gateElements = {};
            let gating = false;

            // #region 🥀 gate&garden
            async function gate(status, user, subscription) {
              if (gating) return;
              gating = true;
              let message,
                buttons = [],
                buttonsTop = [];

              const g = document.createElement("div");
              const curtain = document.createElement("div");
              curtain.id = "gate-curtain";
              curtain.classList.add("obscured");
              g.id = "gate";
              const img = document.createElement("img");
              img.id = "cookie";
              const h1 = document.createElement("h1");
              const h2 = cel("h2");
              const navLow = document.createElement("nav");
              navLow.id = "nav-low";

              function genSubscribeButton(type) {
                if (!type) {
                  h2.innerText = "Email verified!";
                  h2.classList.remove("loading-dots");
                }

                // Build button.
                const sb = cel("button");

                if (!type) {
                  sb.id = "subscribe";
                  sb.onclick = subscribe;
                  sb.innerText = "subscribe";
                } else if (type === "unsubscribe") {
                  sb.innerText = "unsubscribe";
                  sb.onclick = cancel;
                } else if (type === "resubscribe") {
                  sb.innerText = "resubscribe";
                  sb.onclick = subscribe;
                }

                let out = sb;

                if (type) {
                  // And privacy-policy link.
                  const priv = cel("a");
                  priv.id = "privacy-policy";
                  priv.innerText = "privacy policy";
                  priv.href = "${dev ? "/sotce-net/" : "/"}privacy-policy";

                  if (!type) {
                    // subscribe
                    priv.style.left = "3%";
                    priv.style.width = "91%";
                  } else if (type === "resubscribe") {
                    priv.style.left = "10%";
                    priv.style.width = "78%";
                  } else if (type === "unsubscribe") {
                    priv.style.left = "11%";
                    priv.style.width = "75%";
                  }

                  const secondrap = cel("div");
                  secondrap.id = "secondary-wrapper";
                  secondrap.appendChild(sb);
                  secondrap.appendChild(priv);
                  out = secondrap;
                }

                return out;
              }

              function genWelcomeMessage(subscription) {
                return (
                  "Signed in as <a href='' id='email'" +
                  (subscription?.admin ? "class='admin'" : "") +
                  ">" +
                  user.email +
                  "</a>"
                );
              }

              if (status === "logged-out") {
                message = "for my best thoughts";

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
              } else if (status !== "coming-soon") {
                const lo = cel("button");
                lo.onclick = () => {
                  if (confirm("🐾 See you later?")) logout();
                };
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
                  veil();
                  if (
                    confirm(
                      "🚨 Are you sure you want to delete your account?",
                    ) &&
                    confirm("❌ This action cannot be undone.") &&
                    confirm("🔴 Press OK once more to delete your account.")
                  ) {
                    // Run the delete endpoint if confirmed
                    userRequest("POST", "/sotce-net/delete-account")
                      .then(async (res) => {
                        if (res.status === 200) {
                          unveil({ instant: true });
                          flash("red", { hold: true });
                          setTimeout(
                            () => alert("Your account has been deleted."),
                            25,
                          );
                          logout();
                        } else {
                          setTimeout(
                            () =>
                              alert(
                                "😦 Your account could not be fully deleted. Email \`hello@sotce.net\` for help.",
                              ),
                            25,
                          );
                          unveil({ instant: true });
                          logout();
                        }
                      })
                      .catch((err) => {
                        console.error("🔴 Account deletion error...", err);
                        unveil();
                      });
                  } else {
                    unveil();
                  }
                  // Show a confirmation dialog before proceeding
                };

                lowrap.appendChild(del);
                buttons.push(lowrap);
              }

              if (status === "coming-soon") {
                // 🥠 Leave a blank cookie in production pre-launch. 24.08.23.21.35
                g.classList.add("coming-soon");
              }

              if (status === "unverified") {
                message = genWelcomeMessage();
                h2.innerText = "Awaiting email verification";
                h2.classList.add("loading-dots");

                const rs = cel("button");
                rs.onclick = resend;
                rs.innerText = "resend email";
                buttons.push(rs);

                fetchUser = function (email) {
                  // console.log("💚 Fetching user...");
                  fetch(
                    "/user?from=" + encodeURIComponent(email) + "&tenant=sotce",
                  )
                    .then((res) => res.json())
                    .then(async (u) => {
                      if (u.email_verified) {
                        // Check to see if the user has a subscription here, before rendering a subscribe button.
                        user.email_verified = u.email_verified;
                        const entered = await subscribed();
                        if (entered) {
                          await garden(entered, user, true); // Re-open garden but show the gate first.
                        } else {
                          if (!embedded) {
                            rs.remove();
                            navLow.appendChild(genSubscribeButton());
                          } else {
                            h2.innerText = "please subscribe in your browser";
                            h2.classList.remove("loading-dots");
                          }
                        }
                      } else if (u) {
                        h2.innerText = "Awaiting email verification";
                        h2.classList.add("loading-dots");
                        verificationTimeout = setTimeout(() => {
                          fetchUser(email);
                        }, 1000);
                      } else {
                        console.warn("No user retrieved:", u);
                      }
                    })
                    .catch((err) => {
                      console.error("👨‍🦰 Error:", err);
                      verificationTimeout = setTimeout(() => {
                        fetchUser(email);
                      }, 1000);
                    });
                };
                fetchUser(user.email);
              } else if (status === "verified") {
                message = genWelcomeMessage();
                buttons.push(genSubscribeButton());
              } else if (status === "subscribed") {
                message = genWelcomeMessage(subscription);
                // 🗼 Check to see if a handle already exists...

                let handle;

                try {
                  const response = await fetch("/handle?for=sotce-" + user.sub);
                  if (response.status === 200) {
                    const data = await response.json();
                    const newHandle = "@" + data.handle;
                    handle = newHandle;
                    // console.log("🫅 Handle found:", newHandle);
                  } else {
                    //console.warn(
                    //  "❌ 🫅 Handle not found:",
                    //  await response.json(),
                    //);
                  }
                } catch (error) {
                  console.error("❌ 🫅 Handle error:", error);
                }

                // 🙆 Add 'create handle' button here.
                const hb = cel("button");
                hb.classList.add("positive");
                hb.innerText = handle || "create handle";
                // const hbwrap = cel("div");
                // hbwrap.id = "handle-wrapper";
                // hbwrap.appendChild(hb);
                // if (handle && subscription.admin) hbwrap.classList.add("admin");

                hb.onclick = async function () {
                  const newHandle = prompt(
                    handle
                      ? "👤 Change your handle to?"
                      : "Set your handle to?",
                  );
                  if (!newHandle) return;
                  veil();
                  try {
                    const response = await fetch("/handle", {
                      method: "POST",
                      headers: {
                        "Content-Type": "application/json",
                        Authorization:
                          "Bearer " +
                          (window.sotceTOKEN ||
                            (await auth0Client.getTokenSilently())),
                      },
                      body: JSON.stringify({
                        handle: newHandle,
                        tenant: "sotce",
                      }),
                    });
                    if (response.ok) {
                      const result = await response.json();
                      // console.log("💁‍♀️ Handle set:", result);
                      hb.innerText = "@" + result.handle;
                      unveil();
                    } else {
                      const error = await response.json();
                      console.error("❌ Handle error:", error.message);
                      unveil({ instant: true });
                      const message =
                        error.message === "same" ? "the same" : error.message;
                      setTimeout(
                        () =>
                          alert(
                            "❌ Sorry, " +
                              (message !== "error"
                                ? "that handle is " + message + "."
                                : "there was an error setting your handle."),
                          ),
                        100,
                      );
                    }
                  } catch (error) {
                    console.error("🔴 Error:", error);
                    unveil({ instant: true });
                    setTimeout(
                      () =>
                        alert("An error occurred while creating your handle."),
                      100,
                    );
                  }
                };
                buttonsTop.push(hb /*hbwrap*/);

                if (subscription.until === "recurring") {
                  h2.innerText =
                    "Your subscription renews on " + subscription.renews; // + ".";
                  buttons.push(genSubscribeButton("unsubscribe"));
                } else {
                  h2.innerText =
                    "Your subscription ends on " + subscription.until + ".";
                  buttons.push(genSubscribeButton("resubscribe"));
                }
              }

              if (status === "subscribed") {
                curtain.classList.add("hidden");
                if (GATE_WAS_UP) img.classList.add("interactive");
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
                buttons.forEach((b) => navLow.appendChild(b));
              g.appendChild(img);
              if (buttonsTop.length > 0) {
                const navHigh = cel("nav");
                navHigh.id = "nav-high";
                buttonsTop.forEach((b) => navHigh.appendChild(b));
                g.appendChild(navHigh);
              }
              g.appendChild(h1);
              g.appendChild(h2);
              g.appendChild(navLow);

              curtain.appendChild(g);

              const imageLoadPromise = new Promise((resolve) => {
                img.onload = function () {
                  document.getElementById("gate-curtain")?.remove(); // Rid old curtain.
                  wrapper.appendChild(curtain);
                  const email = document.getElementById("email");
                  if (email) {
                    email.onclick = (e) =>
                      resend(e, status === "unverified" ? undefined : "change");
                  }
                  resolve(); // Resolve the promise once the image is loaded.
                };
              });

              img.src = asset(
                status === "subscribed" ? "cookie-open.png" : "cookie.png",
              );

              await imageLoadPromise; // Wait for the image to load.
              gating = false;
              return curtain;
            }

            // Build a layout for the 🌻 'garden'.
            async function garden(subscription, user, showGate = false) {
              const gateCurtain = await gate("subscribed", user, subscription);
              gateCurtain.classList.remove("obscured");
              if (showGate) gateCurtain.classList.remove("hidden");

              // Swap the favicon url.
              document.querySelector('link[rel="icon"]').href =
                asset("cookie-open.png");

              const g = cel("div");
              g.id = "garden";

              if (!showGate) g.classList.add("obscured");
              if (showGate) g.classList.add("hidden");

              // 🪷 write-a-page - Create compose form.
              const writeButton = cel("button");
              writeButton.id = "write-a-page";
              writeButton.innerText = "write a page"; // or "page" or "prayer";

              // const purposes = ["page", "poem", "prayer"];
              // let currentPurpose = 0;
              // const writeButtonInterval = setInterval(() => {
              //   if (!document.body.contains(writeButton)) {
              //     clearInterval(writeButtonInterval);
              //     return;
              //   }
              //   currentPurpose = (currentPurpose + 1) % purposes.length;
              //   writeButton.innerText = "write a " + purposes[currentPurpose];
              // }, 2000);

              writeButton.onclick = function compose() {
                const editor = cel("dialog");
                editor.setAttribute("open", "");

                const form = cel("form");
                // 🔴 TODO: Add a title field? (Autosuggest via LLM)
                // 🔴 TODO: Word count notice.
                // 🔴 TODO: Auto-drafting via the cloud, at certain
                //          intervals after changes? 24.09.13.00.57

                const text = cel("textarea");
                const submit = cel("input");
                submit.type = "submit";

                form.appendChild(text);
                form.appendChild(submit);
                form.addEventListener("submit", async (e) => {
                  const res = await userRequest(
                    "POST",
                    "sotce-net/write-a-page",
                    { words: text.value },
                  );
                  if (res.status === 200) {
                    console.log("🪧 Written:", res);
                  } else {
                    console.error("🪧 Unwritten:", res);
                  }
                  // 🔴 TODO: The dialogue should close appropriately here.
                });

                editor.appendChild(form);
                g.appendChild(editor);
              };

              if (subscription?.admin) g.appendChild(writeButton);

              if (subscription.pages) {
                const pages = subscription.pages;
                console.log("🗞️ Pages retrieved:", pages);

                const binding = cel("div");
                binding.id = "binding";

                pages.forEach((page) => {
                  const pageEl = cel("article");
                  pageEl.classList.add("page");
                  const wordsEl = cel("p");

                  wordsEl.innerText = page.words;
                  pageEl.appendChild(wordsEl);
                  // 🔴 TODO: Add date and perhaps page number / a special bar?
                  binding.appendChild(pageEl);
                });

                g.appendChild(binding);

                function computeTypeSize() {
                  const bindingWidth = max(220, min(800, window.innerWidth));


                  binding.style.width = bindingWidth + "px";
                  binding.style.fontSize = bindingWidth * 0.03 + "px";
                }

                window.addEventListener("resize", function resizeEvent() {
                  computeTypeSize();
                  if (!document.body.contains(binding)) {
                    window.removeEventListener(resizeEvent);
                  }
                });

                computeTypeSize();
              }

              const cookie = cel("img");
              cookie.id = "cookie-menu";
              cookie.src = asset("cookie-open.png");
              g.appendChild(cookie);

              if (GATE_WAS_UP) g.classList.add("hidden");

              const curtainCookie = gateCurtain.querySelector("#cookie");

              cookie.onclick = function () {
                gateCurtain.classList.remove("hidden");
                g.classList.add("hidden");
                curtainCookie.classList.add("interactive");
              };

              if (showGate) curtainCookie.classList.add("interactive");

              cookie.onload = function () {
                document.getElementById("garden")?.remove(); // Remove old gardens.
                wrapper.appendChild(g);
              };

              return g;
            }
            // #endregion

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
                console.warn("Error retrieving uncached user:", err);
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
                // console.log("🔐 Handling auth0 redirect...");
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
                  // await auth0Client.getTokenSilently(/*{ cacheMode: "off" }*/);
                  // console.log("🗝️ Got silent token.");
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

            const spinner = document.getElementById("spinner");
            const spinnerTO = setTimeout(() => {
              spinner.classList.add("showing");
            }, 250);

            async function spinnerPass(callback) {
              clearTimeout(spinnerTO);
              let page;
              if (spinner.classList.contains("showing")) {
                setTimeout(async () => {
                  spinner.addEventListener(
                    "transitionend",
                    () => {
                      spinner.remove();
                      page?.classList.remove("obscured"); // Show 'gate' / 'garden'
                      if (GATE_WAS_UP)
                        document
                          .getElementById("gate-curtain")
                          ?.classList.remove("hidden");
                    },
                    { once: true },
                  );
                  page = await callback();
                  spinner.classList.remove("showing");
                }, 250);
              } else {
                page = await callback();
                spinner.remove();
                page?.classList.remove("obscured");
                if (GATE_WAS_UP)
                  document
                    .getElementById("gate-curtain")
                    ?.classList.remove("hidden");
              }
            }

            if (!fullAlert) {
              if (!isAuthenticated) {
                await spinnerPass(
                  async () => await gate(!dev ? "coming-soon" : "logged-out"),
                );
              } else {
                user = pickedUpSession
                  ? window.sotceUSER
                  : await auth0Client.getUser();

                const userExists = await fetch(
                  "/user?from=" +
                    encodeURIComponent(user.email) +
                    "&tenant=sotce",
                );

                let u;
                try {
                  u = await userExists.json();
                } catch (err) {
                  console.warn(err);
                }
                if (!u?.sub || !user.email_verified)
                  await retrieveUncachedUser();

                if (!user.email_verified) {
                  await spinnerPass(async () => await gate("unverified", user));
                } else {
                  let entered = await subscribed();
                  let times = 0;

                  while (
                    waitForSubscriptionSuccessThreeTimes &&
                    !entered?.subscribed &&
                    times < 3
                  ) {
                    entered = await subscribed();
                    times += 1;
                  }

                  if (entered?.subscribed) {
                    await spinnerPass(async () => await garden(entered, user));
                  } else if (entered !== "error") {
                    await spinnerPass(async () => await gate("verified", user));
                  } else {
                    console.log("🔴 Server error.");
                    setTimeout(() => window.location.reload(), 2500);
                    // gate("error");
                  }
                }
              }
            } else {
              // 🚨 Full Alert
              await spinnerPass(() => {
                wrapper.innerHTML =
                  "<div id='full-alert'>" + fullAlert + "</div>";
                setTimeout(() => location.reload(), 2500);
              });
            }

            function veil() {
              const el = document.getElementById("veil");
              el.innerHTML = ""; // Clear any extra spinners in the veil.
              const spinner = cel("div");
              spinner.classList.add("spinner", "showing");
              el.appendChild(spinner);
              el.classList.remove("unveiled");
              el.classList.remove("unveiled-instant");
            }

            function unveil(options) {
              const el = document.getElementById("veil");
              if (options?.instant) {
                el.classList.add("unveiled-instant");
                el.querySelector(".spinner")?.remove();
              } else {
                el.classList.add("unveiled");
                el.addEventListener(
                  "transitionend",
                  () => {
                    el.querySelector(".spinner")?.remove();
                  },
                  { once: true },
                );
              }
            }

            function flash(color, options) {
              if (options?.hold) wrapper.classList.add("flash", color);
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
              console.log("🗞️ Checking subscription status for:", user.email);
              const response = await userRequest(
                "POST",
                "sotce-net/subscribed",
                { retrieve: "everything" },
              );

              if (response.status === 200) {
                console.log("💳 Subscribed:", response);
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

              const confirmation = confirm("📆 End your subscription?");
              if (!confirmation) return;
              veil();

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
                  const entered = await subscribed();
                  if (entered?.subscribed) {
                    await garden(entered, user, true); // Open garden and show the gate.
                    unveil({ instant: true });
                    setTimeout(() => alert(result.message), 100);
                  } else {
                    // unveil({ instant: true });
                    flash("red", { hold: true });
                    setTimeout(() => window.location.reload(), 150);
                  }
                } else {
                  const error = await response.json();
                  console.error("Cancellation error:", error.message);
                  unveil({ instant: true });
                  setTimeout(
                    () =>
                      alert("Failed to cancel subscription: " + error.message),
                    100,
                  );
                }
              } catch (error) {
                console.error("Error:", error);
                alert(
                  "A network error occurred while cancelling your subscription. Please try again.",
                );
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

            // resend verification email
            function resend(e, type) {
              e?.preventDefault();
              clearTimeout(verificationTimeout);

              const promptText =
                type === "change"
                  ? "💌 Update your email to?"
                  : "💌 Resend verification email to?";

              const email = prompt(promptText, user.email);
              if (!email) return;

              veil();
              console.log("📧 Resending...", email);

              userRequest("POST", "/api/email", {
                name: email,
                email,
                tenant: "sotce",
              })
                .then(async (res) => {
                  if (res.status === 200) {
                    await retrieveUncachedUser();
                    console.log("📧 Email verification sent...", res);
                    const gateEl = await gate("unverified", user);
                    unveil({ instant: true });
                    gateEl.classList.remove("obscured");
                    setTimeout(
                      () => alert("📧 Check your \`" + email + "\` inbox."),
                      100,
                    );
                  } else {
                    throw new Error(res.code);
                  }
                })
                .catch((err) => {
                  console.error("🔴 📧 Email change error:", err);
                  unveil({ instant: true });
                  setTimeout(
                    () => alert("❌ Sorry, your email could not be changed."),
                    100,
                  );
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

                if (method.toLowerCase() !== "get" && body)
                  options.body = JSON.stringify(body);

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
                  } catch (error) {
                    console.log("🚫 Error:", error);
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

    // TODO: To properly handle rate limits on the `/userinfo` endpoint
    //       I should probably be sending the user info up in the POST
    //       request here and then the `authorize` function should
    //       be rewritten to use a different endpoint which has
    //       a more apt rate limit?
    // Also it's possible that finding the subscription information here
    // via Stripe should / could be cached in redis for faster
    // retrieval later? 24.08.24.19.22

    const body = JSON.parse(event.body); // Make sure we can parse the body.
    const retrieve = body.retrieve || "everything";

    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });
    // shell.log("Subscribing user:", user);

    const email = user.email;
    const sub = user.sub;
    let subscription;

    // Then, make sure they are subscribed.
    try {
      const stripe = Stripe(key);
      // Fetch customer by user ID (sub) from subscription metadata field.
      const customers = await stripe.customers.search({
        query: "metadata['sub']:'" + sub + "'",
      });

      if (!customers.data.length) return respond(200, { subscribed: false });
      const customer = customers.data[0];

      // Fetch subscriptions for the customer
      const subscriptions = await stripe.subscriptions.list({
        customer: customer.id,
        status: "active", // Only find the first active subscription.
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
      // What did we need the subscription for?

      const out = { subscribed: true };

      if (retrieve === "everything") {
        // 📆 Subscription status. (until, renews)
        out.until = subscription.cancel_at_period_end
          ? new Date(subscription.cancel_at * 1000).toLocaleDateString(
              "en-US",
              dateOptions,
            )
          : "recurring";
        if (out.until === "recurring") {
          out.renews = `${new Date(subscription.current_period_end * 1000).toLocaleDateString("en-US", dateOptions)}`;
        }

        // 👸 Administrator status.
        const isAdmin = await hasAdmin(user, "sotce");
        if (isAdmin) out.admin = isAdmin;
        console.log(isAdmin, out);

        // 📓 Recent Pages
        const database = await connect();
        const pages = database.db.collection("pages");
        // Get page content from the database.
        const retrievedPages = await pages
          .aggregate([{ $sort: { when: -1 } }, { $limit: 100 }])
          .toArray();
        out.pages = retrievedPages;
        await database.disconnect();

        // TODO: 👤 'Handled' pages filtered by user..
      }
      return respond(200, out);
    } else {
      return respond(200, { subscribed: false });
    }
  } else if (path === "/cancel" && method === "post") {
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized." });
    shell.log("User authorized. Cancelling subscription...");
    const cancelResult = await cancelSubscription(user, key);
    return respond(cancelResult.status, cancelResult.body);
  } else if (path === "/write-a-page" && method === "post") {
    // 🪧 write-a-page - Submission endpoint.
    const user = await authorize(event.headers, "sotce");
    const isAdmin = await hasAdmin(user, "sotce");
    if (!user || !isAdmin) return respond(401, { message: "Unauthorized." });
    const body = JSON.parse(event.body);
    console.log("🪧 Post so far:", body);
    const words = body.words;
    if (words) {
      // Add page to database.
      const database = await connect();
      const pages = database.db.collection("pages");
      // 💡 TODO: Eventually create a 'books' abstraction so users can have
      // multiple books that capture groupings of pages. 24.09.13.01.41
      await pages.insertOne({ user: user.sub, words, when: new Date() });
      await database.disconnect();
      const page = body;
      return respond(200, { page });
    } else {
      return respond(500, { message: "No words written." });
    }
  } else if (path === "/delete-account" && method === "post") {
    // See also the 'delete-erase-and-forget-me.js' function for aesthetic users.
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Authorization failure..." });
    shell.log("🔴 Deleting user:", user.sub);

    const sub = user.sub;
    const sotceSub = "sotce-" + sub;

    // 1. Unsubscribe the user if they have an active subscription.
    try {
      const cancelResult = await cancelSubscription(user, key);
      shell.log(
        "❌ Cancelled subscription?",
        cancelResult.status,
        cancelResult.body,
      );
    } catch (err) {
      shell.error("🔴 Subscription cancellation error:", err);
    }

    // TODO: 2. Delete any user data, like posts.

    const database = await connect();

    // Remove the user's handle cache from redis.
    const handle = await getHandleOrEmail(sotceSub);
    if (handle?.startsWith("@")) {
      await KeyValue.connect();
      await KeyValue.del("@handles", handle);
      await KeyValue.del("userIDs", sotceSub);
      await KeyValue.disconnect();
    }

    // 3. Delete the user's handle if it exists and the user does not have
    //    an aesthetic computer account, otherwise re-associate the key.
    if (handle) {
      shell.log(
        "📚 Checking for any `aesthetic` user with the same email and handle:",
        handle,
      );
      const bareHandle = handle.slice(1); // Remove the "@" from the handle.
      const idRes = await userIDFromEmail(user.email, "aesthetic");
      if (idRes?.userID && idRes?.email_verified) {
        const handles = database.db.collection("@handles");

        const aestheticSub = idRes.userID;
        // Check if an entry with the same _id already exists
        const existingHandle = await handles.findOne({ _id: aestheticSub });

        if (!existingHandle) {
          // If no existing entry, proceed with deletion and insertion
          await handles.deleteOne({ _id: sotceSub });
          await handles.insertOne({ _id: aestheticSub, handle: bareHandle });
          shell.log(
            "🧔 Changed primary handle key of 'sotce' user:",
            sub,
            "to 'aesthetic' user:",
            aestheticSub,
          );
        } else {
          // If an entry already exists, skip deletion and insertion
          shell.log(
            "🩹 Handle already native to `aesthetic`, skipping reassignment.",
          );
        }
      } else {
        await database.db.collection("@handles").deleteOne({ _id: sotceSub });
        shell.log("🧔 Deleted user handle for:", sotceSub);
      }
    }

    shell.log("❌ Deleted database data.");

    await database.disconnect();

    // 3. Delete the user's auth0 account.
    const deleted = await deleteUser(sub, "sotce");
    shell.log("❌ Deleted user registration:", deleted);
    return respond(200, { result: "Deleted!" }); // Successful account deletion.
  } else if (path === "/privacy-policy" && method === "get") {
    const body = html`
      <html>
        <head>
          <title>Sotce Net's Privacy Policy</title>
          <style>
            body {
              font-family: sans-serif;
              background-color: rgb(255, 251, 234);
              /* background: rgb(255, 230, 225); */
            }
            img {
              filter: grayscale(0.75);
            }
            code {
              font-weight: bold;
            }
            a,
            a:visited {
              color: black;
              text-decoration: none;
            }
            sub {
              line-height: 1.25em;
            }
          </style>
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1.0"
          />
        </head>

        <body>
          <h1>Sotce Net's Privacy Policy</h1>
          <p>
            Sotce Net keeps finished pages on a remote server so they can be shared
            with and viewed by subscribers.
          </p>
          <p>
            Sotce Net allows you to associate an email with a
            <code>@handle</code> to represent your identity.
          </p>
          <p>
            Sotce Net does not sell or exchange any user data with third
            parties.
          </p>
          <p>
            Sotce Net is served by
            <code><a href="https://aesthetic.computer/privacy-policy">Aesthetic Computer</code>.</a>
          </p>
          <p>
            For more information write to <code>mail@sotce.net</code> to
            communicate with the author.
          </p>
          <a href="${dev ? "/sotce-net" : "/"}"><img width="128" src="${assetPath + "cookie.png"}" /></a>
          <br />
          <br />
          <sub>Edited on September 7, 2024</sub>
        </body>
      </html>
    `;
    return respond(200, body, { "Content-Type": "text/html; charset=utf-8" });
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
      message: `Your subscription ends on ${new Date(
        cancelled.cancel_at * 1000,
      ).toLocaleDateString(
        "en-US",
        dateOptions,
      )}. You will not be billed again.`,
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
