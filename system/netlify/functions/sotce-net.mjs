// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🏁 TODO 
  - [🌟] refreshing the page with websockets needs to keep the current session somehow!

  - [🟠] run a new development subscription
  - [] add a cancellation button...
  - [] run a production subscription
  - [] add handle creation / handle support for sotce-net users
  - [] read from the database
  - [] The handle system would be shared among ac users.
    - [] Perhaps the subs could be 'sotce' prefixed.
  - [] add cookie favicon which switches if the user is logged in...
  - [] Allow Amelia's user / @sotce to post a diary, but no other users
       for now.
  - [] Show number of signed up users so far.
  - [] How can I do shared reader cursors / co-presence somehow?
  - [] Account deletion.
  + Done
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
  authorize, //,
  // hasAdmin,
} from "../../backend/authorization.mjs";

import Stripe from "stripe";

export const handler = async (event, context) => {
  // console.log("Event:", event);
  // console.log("Context:", context);
  // console.log("Path:", event.path);

  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net"))
    path = path.replace("/sotce-net", "/").replace("//", "/");

  const key = dev
    ? process.env.SOTCE_STRIPE_API_TEST_PRIV_KEY
    : process.env.SOTCE_STRIPE_API_PRIV_KEY;

  // 🏠 Home
  if (path === "/" && method === "get") {
    const body = html`
      <html>
        <head>
          <meta charset="utf-8" />
          ${dev ? reloadScript : ""}
          <script
            crossorigin="anonymous"
            src="/aesthetic.computer/dep/auth0-spa-js.production.js"
          ></script>
          <script src="https://js.stripe.com/v3/"></script>
          <style>
            body {
              font-family: sans-serif;
              background: gray;
            }
          </style>
        </head>
        <body>
          <div id="wrapper">
            <h1>${path} : ${method}</h1>
          </div>
          <script>
            // 🗺️ Environment
            const dev = ${dev};
            const fromAesthetic =
              (document.referrer.indexOf("aesthetic") > -1 ||
                document.referrer.indexOf("localhost") > -1) &&
              document.referrer.indexOf("sotce-net") === -1;
            const embedded = window.self !== window.top;

            // 🤖 Initialization
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

            // 🔐 Authorization
            (async () => {
              const clientId = "${AUTH0_CLIENT_ID_SPA}";
              let fetchUser, verificationTimeout;

              let isAuthenticated = false;
              let user;

              const auth0Client = await window.auth0.createAuth0Client({
                domain: "${AUTH0_DOMAIN}",
                clientId,
                cacheLocation: "localstorage",
                useRefreshTokens: true,
                authorizationParams: { redirect_uri: window.location.href },
              });

              if (embedded || fromAesthetic) {
                const back = document.createElement("div");
                back.innerHTML =
                  "<button onclick='aesthetic()'>sotce-net</button>";
                document.body.appendChild(back);
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

                console.log("PARAM IS:", param);

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
                    history.pushState(
                      {},
                      "",
                      url.pathname + "?" + params.toString(),
                    );
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
                    console.log("🗝️ Got fresh token.");
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
                    return;
                  }
                }
              } else {
                isAuthenticated = true;
              }

              if (!isAuthenticated) {
                wrapper.innerHTML =
                  'not signed in :( <br><button onclick="login()">log in</button>' +
                  (embedded
                    ? ""
                    : '<button onclick="signup()">i&apos;m new</button>');
              } else {
                // 🅰️ Check / await email verification.
                user = pickedUpSession
                  ? window.sotceUSER
                  : await auth0Client.getUser();

                // console.log("🪷 Got user:", user);
                let verifiedText = "email unverified :(";

                function subscription() {
                  if (embedded) {
                    return "<code id='subscribe'>please subscribe in your browser</code>";
                  } else {
                    return '<button id="subscribe" onclick="subscribe()">subscribe</button>';
                  }
                }

                if (!user.email_verified) {
                  verifiedText =
                    'waiting for email verification... <button onclick="resend()">Resend?</button>';

                  fetchUser = function (email) {
                    fetch(
                      "/user?from=" +
                        encodeURIComponent(email) +
                        "&tenant=sotce",
                    )
                      .then((res) => res.json())
                      .then((u) => {
                        // console.log("Fetched updated user...", u);
                        if (u.email_verified) {
                          // console.log("📧 Email verified!");
                          const verifiedEl =
                            document.getElementById("verified");
                          console.log(verifiedEl.innerHTML);
                          verifiedEl.innerHTML = subscription();
                        } else {
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
                } else {
                  // console.log("📧 Email verified!");
                  verifiedText = subscription();
                  // console.log("verified text:", verifiedText);
                }

                // 🅱️ Check for active subscription.
                // - [] If subscription exists, then show 'enter' button.
                //   - [] And also show 'cancel' button to end a subscription.
                // - [] Else, show a 'subscribe' button to the user with a price.

                wrapper.innerHTML =
                  "signed in as: <span id='email'>" +
                  user.email +
                  '</span>!<br><mark id="verified">' +
                  verifiedText +
                  '</mark><br><a onclick="logout()" id="logout" href="#">logout</a>';
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

              function resend() {
                clearTimeout(verificationTimeout);
                console.log("📧 Resending...");
                const email = prompt(
                  "Resend verification email to?",
                  user.email,
                );

                userRequest("POST", "/api/email", {
                  name: email,
                  email,
                  tenant: "sotce",
                })
                  .then(async (res) => {
                    if (res.status === 200) {
                      console.log("📧 Email verification resent...", res);
                      alert("📧 Verification resent!");
                      // Replace signed in email...
                      const emailEl = document.getElementById("email");
                      emailEl.innerHTML = email;

                      // Clear user cache / token here.
                      try {
                        await auth0Client.getTokenSilently({
                          cacheMode: "off",
                        });
                        user = await auth0Client.getUser();
                        console.log("🎇 New user is...", user);
                      } catch (err) {
                        console.log("Error retrieving uncached user.");
                      }
                    }
                    fetchUser(email);
                  })
                  .catch((err) => {
                    alert("Email verification error.");
                    console.error("🔴 📧 Email verification error...", err);
                  });
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
                  body: JSON.stringify({ email: user.email }),
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
                  // nobody ;)
                );
                if (response.status === 200) {
                  // console.log("💳 Subscribed:", response);
                  if (response.subscribed) {
                    return response.content;
                  } else {
                    return false;
                  }
                } else {
                  console.error("💳", response);
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
                  console.log("Sotce token...", window.sotceTOKEN);
                  const token = window.sotceTOKEN || await auth0Client.getTokenSilently();
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
                      const message = await response.text();
                      return { status: response.status, message };
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
              window.signup = signup;
              window.resend = resend;
              window.subscribe = subscribe;
              window.logout = logout;
              window.aesthetic = aesthetic;
              window.user = user;

              // 🚪 Check for subscription and add content as necessary.
              const entered = await subscribed();
              if (entered) {
                console.log("🚪", entered);
                document.getElementById("subscribe").remove();
                document.getElementById("verified")?.remove();
                wrapper.innerHTML += "<br><b>" + entered + "</b>";
              }
            })();
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

      const { email } = JSON.parse(event.body);

      const session = await stripe.checkout.sessions.create({
        payment_method_types: ["card"],
        mode: "subscription",
        line_items: [{ price: priceId, quantity: 1 }],
        customer_email: email,
        success_url: `${event.headers.origin}/${redirectPath}?notice=success`,
        cancel_url: `${event.headers.origin}/${redirectPath}?notice=cancel`,
      });

      return respond(200, { id: session.id });
    } catch (error) {
      console.log("⚠️", error);
      return respond(500, { message: `Error: ${error.message}` });
    }
  } else if (path === "/subscribed" && method === "post") {
    // First validate that the user has an active session via auth0.
    console.log(event.headers)
    const user = await authorize(event.headers, "sotce");
    if (!user) return respond(401, { message: "Unauthorized user." });

    const email = user.email;
    let isSubscribed = false;

    // Then, make sure they are subscribed.
    try {
      const stripe = Stripe(key);
      // Fetch customer by email
      const customers = await stripe.customers.list({ email, limit: 1 });

      if (customers.data.length === 0) {
        return respond(200, { subscribed: false });
      }

      const customer = customers.data[0];

      // Fetch subscriptions for the customer
      const subscriptions = await stripe.subscriptions.list({
        customer: customer.id,
        status: "all",
        limit: 1,
      });
      // console.log("👗", subscriptions.data?.[0]?.items);
      isSubscribed = subscriptions.data.some((sub) =>
        sub.items.data.some((item) => item.price.product === productId),
      );
    } catch (err) {
      console.error("Error fetching subscription status:", error);
      return respond(500, { error: "Failed to fetch subscription status" });
    }

    // 🟠 TODO: Lastly, show them the paywalled content and include a "cancel" button.

    if (isSubscribed) {
      return respond(200, {
        subscribed: isSubscribed,
        content: "you are subscribed!",
      });
    } else {
      return respond(200, { subscribed: isSubscribed });
    }
  }
};

// Inserted in `dev` mode for live reloading.
const reloadScript = html`
  <script>
    // Live reloading in development.
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

      ws.onopen = (e) => {
        console.log("🧦 Connected:", e);
      };

      ws.onmessage = (e) => {
        const msg = JSON.parse(e.data);
        if (msg.type === "reload" && msg.content.piece === "*refresh*") {
          console.log("🧦 ♻️ Reloading...");
          const sessionItem = localStorage.getItem("session-sotce");
          if (sessionItem) {
            const url = new URL(window.location.href);
            url.searchParams.set("session-sotce", "retrieve");
            window.location.href = url.toString();
          } else {
            location.reload();
          }
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
