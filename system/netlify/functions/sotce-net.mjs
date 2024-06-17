// Sotce Net, 24.06.13.06.38
// A paid diary network, 'handled' by Aesthetic Computer.

/* #region 🏁 TODO 
  - [💚] Use shadow dom / custom element or a "modal" element for signup.
  - [] auth0 based auth / registration
    - [x] review existing ac auth implementation and necessary
         keys
    - [] Set up login pages / logo etc. 
    - [] not logged in route
    - [] logged in route
  - [] stripe paywall
    - [] bring in necessary keys
    - [] logged in but unpaid route
    - [] logged in and paid route
    - [] this should be subscription based
  - [] The handle system would be shared among ac users.
    - [] Perhaps the subs could be 'sotce' prefixed.
  - [] Store whether a user is subscribed with an expiration date.
  + Done
  - [x] Run through a test sign-up.
  - [x] Set up email sending service via google apps?
  - [x] Developmnt mode.
  - [x] Live reload function.
  - [x] Set up path->method routing in `sotce-net.js`.
#endregion */

// ♻️ Environment
// const AUTH0_CLIENT_ID = process.env.SOTCE_AUTH0_M2M_CLIENT_ID;
// const AUTH0_SECRET = process.env.SOTCE_AUTH0_M2M_SECRET;
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
        </head>
        <body>
          <div id="wrapper">
            <h1>${path} : ${method}</h1>
          </div>
          <script>
            (async () => {
              const clientId = "${AUTH0_CLIENT_ID_SPA}";

              const auth0Client = await window.auth0.createAuth0Client({
                domain: "${AUTH0_DOMAIN}",
                clientId,
                cacheLocation: "localstorage",
                useRefreshTokens: true,
                authorizationParams: { redirect_uri: window.location.href },
              });

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

              const isAuthenticated = await auth0Client.isAuthenticated();
              const wrapper = document.getElementById("wrapper");

              if (!isAuthenticated) {
                wrapper.innerHTML = 'not signed in :( <a onclick="login()" href="#">Log in</a> <a onclick="login(\"signup\")" href="#">Im new</a>'; 
              } else {
                wrapper.innerHTML = 'signed in! <a onclick="logout()" id="logout" href="#">logout</a>'; 
              }

              function login(hint = "login") {
                const opts = { prompt: "login" }; // Never skip the login screen.
                opts.screen_hint = "signup";
                auth0Client.loginWithRedirect({ authorizationParams: opts });
              }

              function logout() {
                if (isAuthenticated) {
                  console.log("🔐 Logging out...");
                  auth0Client.logout({
                    logoutParams: { returnTo: window.location.href },
                  });
                } else {
                  console.log("🔐 Already logged out!");
                }
              }

              window.logout = logout;
              window.login = login;
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
