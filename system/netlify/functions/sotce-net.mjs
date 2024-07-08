// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🏁 TODO 
  - [🩷] Disambiguate 'session' query string and
        somehow receive both / capture both on first load.
        or know which one to send for both AC and Sotce-Net.
  - [🟠] Add session / login support to the Aesthetic Computer VSCode extension /
       switch to an in-editor development flow.
  - [-] stripe paywall
    - [] bring in necessary env vars for stripe
    - [] set up subscription payment wall on 'subscribe' button
    - [] logged in but unpaid route
    - [] logged in and paid route
      - [] read from the database
  - [] The handle system would be shared among ac users.
    - [] Perhaps the subs could be 'sotce' prefixed.
  - [] Store whether a user is subscribed with an expiration date.
  + Done
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

const dev = process.env.NETLIFY_DEV;

import { defaultTemplateStringProcessor as html } from "../../public/aesthetic.computer/lib/helpers.mjs";

export const handler = async (event, context) => {
  // console.log("Event:", event);
  // console.log("Context:", context);
  // console.log("Path:", event.path);

  // 🚙 Router
  const method = event.httpMethod.toLowerCase();
  let path = event.path;
  if (path.startsWith("/sotce-net"))
    path = path.replace("/sotce-net", "/").replace("//", "/");

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
          <style>
            body {
              background: white;
            }
          </style>
        </head>
        <body>
          <div id="wrapper">
            <h1>${path} : ${method}</h1>
          </div>
          <script>
            const dev = ${dev};
            const fromAesthetic =
              (document.referrer.indexOf("aesthetic") > -1 ||
                document.referrer.indexOf("localhost") > -1) &&
              document.referrer.indexOf("sotce-net") === -1;
            const embedded = window.self !== window.top;

            // Send some messages to the VS Code extension.
            window.parent?.postMessage(
              { type: "url:updated", slug: "sotce-net" },
              "*",
            );
            window.parent?.postMessage({ type: "ready" }, "*");
            // Authorization
            (async () => {
              const clientId = "${AUTH0_CLIENT_ID_SPA}";
              let fetchUser, verificationTimeout;

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

              // 😶‍🌫️ TODO: Picking up sessions from VS Code.
              {
                const url = new URL(window.location);
                const params = url.searchParams;
                const sessionParams = params.get("session");
                console.log("🌻 Sotce Session params:", sessionParams);// sessionParams);
                let encodedSession = sessionParams;
                if (encodedSession === "null") encodedSession = undefined;
                let pickedUpSession;
                if (encodedSession) {
                  const sessionJsonString = atob(
                    decodeURIComponent(encodedSession),
                  );
                  const session = JSON.parse(sessionJsonString);
                  // Use the session information to authenticate, if it exists.
                  // console.log("🥀 Session data:", session);
                  if (session.accessToken && session.account) {
                    window.sotceTOKEN = session.accessToken; // Only set using this flow.
                    window.sotceUSER = {
                      name: session.account.label,
                      sub: session.account.id,
                    };
                    console.log(
                      "🌻 Picked up sotce session!",
                      window.acTOKEN,
                      window.acUSER,
                    );
                    pickedUpSession = true;
                  }

                  if (sessionParams) {
                    params.delete("session"); // Remove the 'session' parameter
                    // Update the URL without reloading the page
                    history.pushState(
                      {},
                      "",
                      url.pathname + "?" + params.toString(),
                    );
                  }
                }
              }

              let isAuthenticated = await auth0Client.isAuthenticated();

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

              const wrapper = document.getElementById("wrapper");
              let user;

              if (!isAuthenticated) {
                wrapper.innerHTML =
                  'not signed in :( <br><button onclick="login()">log in</button>' +
                  (embedded
                    ? ""
                    : '<button onclick="signup()">i&apos;m new</button>');
              } else {
                // 🅰️ Check / await email verification.
                //  - [🟡] If unverified, then show awaiting animation
                //         and long poll for verification.
                user = await auth0Client.getUser();

                console.log(user);

                // else...

                let verifiedText = "email unverified :(";

                function subscription() {
                  return '<button onclick="subscribe()">subscribe</button>';
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
                          console.log("📧 Email verified!");
                          const verifiedEl =
                            document.getElementById("verified");
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
                  console.log("📧 Email verified!");
                  verifiedText = subscription();
                }

                // 🅱️ Check for active subscription.
                // - [] If subscription exists, then show 'enter' button.
                //   - [] And also show 'cancel' button to end a subscription.
                // - [] Else, show a 'subscribe' button to the user with a price.

                {
                  // Send a get fetch request to /api/subscribed?user=sub
                  // Which will hold the user sub.
                }

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

              function subscribe() {
                // ❤️‍🔥 TODO: Go to stripe subscription page...
              }

              function logout() {
                if (isAuthenticated) {
                  console.log("🔐 Logging out...", window.location.href);
                  if (embedded) {
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
                window.location.href = dev
                  ? "https://localhost:8888"
                  : "https://aesthetic.computer";
                if (embedded) {
                  window.parent.postMessage(
                    { type: "url:updated", slug: "prompt" },
                    "*",
                  );
                }
              }

              async function userRequest(method, endpoint, body) {
                try {
                  // const token = await $commonApi.authorize(); // Get user token.
                  const token = await auth0Client.getTokenSilently();
                  // if (!token) throw new Error("🧖 Not logged in.");

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
            })();
          </script>
        </body>
      </html>
    `;

    return {
      body,
      statusCode: 200,
      headers: { "Content-Type": "text/html; charset=utf-8" },
    };
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
          location.reload();
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